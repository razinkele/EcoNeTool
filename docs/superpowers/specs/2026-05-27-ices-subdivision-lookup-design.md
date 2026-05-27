# ICES Subdivision Lookup — Design

**Date:** 2026-05-27
**Author:** A. Razinkovas-Baziukas (with Claude)
**Status:** Approved — 3-agent spec review incorporated (architecture, GIS
correctness, conventions). GIS findings verified live against the ICES
WFS in R 4.4.1.
**Tracks:** v2-plan item **R1** (PR10 scope-M follow-up; see
`docs/superpowers/plans/2026-05-26-v2-plan-remaining.md`)

---

## 1. Goal & non-goals

### Goal
Add a geometry-based reverse lookup to `R/functions/ices_lookups.R`:
given a `(lon, lat)` point, return the **most specific ICES Statistical Area
code** it falls in. This completes the ICES toolset — `get_ices_area_codes()`
(codes) and `get_ices_area_detail()` (metadata) already exist; R1 adds
**point → code**.

The function is named `lookup_ices_subdivision()` (the R1 / PR10 deliverable
name; Subdivision is the common case in the MARBEFES Baltic study region).
But the returned `Area_Full` code is the most-specific level the layer
carries for that point — which may be a **Division** (`27.4.b`), a
**Subdivision** (`27.3.d.27`), or a **Unit** (`27.3.d.28.2`). The contract
(§3) is explicit about this; callers must not assume a Subdivision.

### Non-goals (YAGNI)
- **No consumer wiring.** Standalone building block, matching the "minimum
  surface needed for downstream code" philosophy `ices_lookups.R` was built
  on. No UI, no `ecopath_import_server.R` / spatial-module changes.
- **No vectorized/batch API.** Scalar single point only. A future consumer
  that needs a bounding box maps over corner/centroid points itself.
- **No descriptive-name join.** The layer carries no name field; callers who
  want the human-readable name pass the returned code to the existing
  `get_ices_area_detail()`.
- **No biomass calibration / DATRAS coupling.** That is R2 (scope-L), gated.

## 2. Data source (verified live)

ICES GIS WFS GeoServer at **gis.ices.dk**, layer
`ices_eg:ICES_AREAS_VISA_SIMPLE_5KM` (alias `ices_areas`), fetched as GeoJSON
(`outputFormat=application/json`).

Verified empirically (live `sf::st_read` in R 4.4.1):

- **66 features total** — tiny; loads at once, **no WFS pagination concern**.
- **Flat leaf partition** — no `Area_Full` value is a prefix of another;
  there are no parent (Division-only) polygons overlapping children. A sea
  point matches **exactly one** feature. (The earlier worry about nested
  parent polygons is a non-issue for this layer.)
- **GeoJSON `crs` member is `null`** → GDAL defaults to OGC:CRS84, so
  `st_read` reports CRS as WGS 84 with **lon/lat** axis order. (The layer's
  GML output instead uses `EPSG::4326` with **lat/lon** axis order — so the
  fetch MUST pin `outputFormat=application/json` and MUST NOT pass an
  `srsName`, or coordinates read axis-swapped and every lookup is silently
  wrong.)
- **13 of 66 polygons are invalid under S2** ("duplicate vertex" — an
  artifact of the 5 km simplification). With S2 on, `st_intersects` **crashes**.
  → see §4 (S2 must be disabled / geometries made valid).
- **bbox** ≈ lon −44…68.5, lat 36…90 — never crosses the antimeridian
  (no 180° wrap handling needed).

Live attribute schema (66 features confirmed; example values illustrate each
field independently, not one shared feature):

| Field | Example | Notes |
|-------|---------|-------|
| `Area_Full` | `27.3.d.27` | **primary return** — most-specific code (Division/Subdivision/Unit) |
| `Major_FA` | `27` | component |
| `SubArea` | `3` | component |
| `Division` | `d` | component |
| `SubDivisio` | `27` | component (empty for Division-level features) |
| `Unit` | `2` | component (empty unless a Unit-level feature) |

The **5 km-simplified** layer is acceptable: point-in-polygon at this
granularity is unaffected except within ~5 km of a boundary (within the
inherent fuzziness of statistical-area assignment). A point exactly on a
simplified boundary may fall in a tiny gap (→ no match) or overlap (→ §4
tie-break).

## 3. API contract

```r
lookup_ices_subdivision(lon, lat, layer_path = NULL, timeout = 60)
```

- `lon`, `lat`: numeric scalars (WGS84 degrees).
- `layer_path`: optional local override (a `.gpkg`/`.geojson` path); default
  NULL = use the in-session/on-disk cache, else download + cache. (Named
  `layer_path`, not `shapefile_path`, because the cache is a GeoPackage.)
- `timeout`: network budget in seconds for the one-time layer fetch. Default
  `60` (heavier than the 30 s vocab calls — a multi-MB layer download).

**Returns** the file's standard structured list:

```r
list(
  success = TRUE/FALSE,
  source  = "ICES_GIS_WFS",
  data    = <1-row data.frame> | NULL,
  error   = NA_character_ | "<reason>"
)
```

On success, `data` has exactly one row, columns in this canonical order
(lower-cased per project snake_case):
`area_full` (e.g. `27.3.d.27`), `major_fa`, `subarea`, `division`,
`subdivision`, `unit`. These are the source fields lower-cased, with the
truncated GeoServer column `SubDivisio` mapped to `subdivision`.
**Component columns may be empty strings** when the matched feature is
coarser than a Unit/Subdivision — callers must tolerate that (see §1).

## 4. Data flow

1. **Input validation** — `lon`/`lat` numeric, **finite** (`is.finite`, not
   just `!is.na` — guards `Inf`), in `[-180,180]` / `[-90,90]`; else
   `success = FALSE`, `error = "lon/lat invalid"`.
2. **Layer acquisition (cached)** — if `.ices_cache$areas_sf` is set, reuse.
   Else load from `layer_path` if given; else load the on-disk cache
   (`cache/spatial/ices_areas.gpkg`) if present; else **two-step download**:
   fetch the WFS GeoJSON bytes with `curl`/`httr` under a real network
   timeout (NOT `with_timeout()` around `st_read` — `setTimeLimit` only
   bounds R-level CPU, not GDAL/curl C-level blocking I/O, so a slow server
   would hang), then `sf::st_read()` the downloaded file. After read:
   - normalise CRS: `if (is.na(sf::st_crs(areas_sf))) sf::st_crs(areas_sf) <- 4326`
     (mirrors `emodnet_habitat_utils.R:157-160`).
   - make geometries valid for the planar query: `sf::st_make_valid()` (the
     5 km layer has S2-invalid loops).
   - `sf::st_write()` the validated layer to `cache/spatial/ices_areas.gpkg`
     (after `dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)`),
     and store the object in `.ices_cache$areas_sf`.
3. **Point-in-polygon** — build an `sf` point in EPSG:4326. Disable S2 around
   the query and restore on exit (mirrors `emodnet_habitat_utils.R:225-227`):
   ```r
   old_s2 <- sf::sf_use_s2(); sf::sf_use_s2(FALSE)
   on.exit(sf::sf_use_s2(old_s2), add = TRUE)
   hits <- sf::st_filter(areas_sf, point, .predicate = sf::st_intersects)
   ```
   (`st_filter` returns the matched rows; bare `st_intersects` returns an
   `sgbp` index list — use `st_filter`.)
4. **Result / tie-break** —
   - `nrow(hits) == 1` → map that row to the `data` columns, `success = TRUE`.
   - `nrow(hits) == 0` → `success = FALSE`,
     `error = "no ICES area at this point (on land or outside ICES Area 27)"`.
   - `nrow(hits) > 1` (rare; only at a simplified shared boundary) →
     deterministic tie-break: pick the row with the **longest `Area_Full`**
     (most specific), and `warning()` noting the boundary ambiguity (so the
     non-determinism is visible, not silent). NEVER rely on WFS feature order.

## 5. Caching

- **In-session:** validated `sf` object in the existing `.ices_cache` env as
  `.ices_cache$areas_sf` (new key, no collision with `area_codes` /
  `area_detail_*` / `datras_*`). Loaded once per R session.
- **On-disk:** the **validated** layer written once to
  `cache/spatial/ices_areas.gpkg` (caching the *validated* sf, not the raw
  GeoJSON, so later sessions skip both the download and the `st_make_valid`).
  `cache/` is gitignored and not deployed; the layer is a few MB.
- **Concurrent first-load:** if two Shiny sessions trigger the download
  simultaneously before the cache is populated, both downloads complete and
  the second write wins — accepted as a one-time cost, no corruption. The
  cached `sf` is read-only shared state; R's copy-on-modify prevents callers
  mutating it.

## 6. Error handling

Matches the file's conventions (PR6 lineage):
- `sf` package missing → `success = FALSE`, `error = "sf package not installed"`
  (`requireNamespace` guard, like the other helpers).
- Network/download failure or timeout → `success = FALSE`, populated `error`,
  **`warning()`** emitted (production preserves no `message()` logs).
- Point outside all polygons → `success = FALSE`, neutral `error` (a
  legitimate "no match" — land OR outside ICES area — NOT a warning/fault).
- **Inside `error = function(e)` closures, mutate the outer `result` with
  `<<-`** (e.g. `result$error <<- conditionMessage(e)`), per project
  convention — plain `<-` mutates only the closure-local copy and the change
  is silently lost (the bug previously duplicated across 10 lookup functions).
- The function never `stop()`s; callers branch on `$success`.

## 7. Testing

- **Unit (no network):** build a synthetic `sf` fixture in EPSG:4326 — 2–3
  hand-built square polygons carrying the **full attribute set** (`Area_Full`,
  `Major_FA`, `SubArea`, `Division`, `SubDivisio`, `Unit`) so the snake_case
  column-mapping path is exercised. Inject it by **pre-seeding
  `.ices_cache$areas_sf`** (the test `source()`s the file `local = TRUE`, per
  `test-ices-lookups.R:14-16`, so the module env is reachable). Assert: a
  point inside polygon A returns A's `area_full`; a point in the gap returns
  `success = FALSE`; a multi-match overlap returns the longest code + warns.
  Plus contract paths: invalid/`Inf` `lon`/`lat`, `sf` missing
  (`skip_if(!requireNamespace("sf"))`), structured-return shape.
- **Live (gated):** real `gis.ices.dk` fetch behind `RUN_LIVE_TESTS=true`,
  wrapped so a slow upstream doesn't hang the run. Point-in-polygon for a
  fixed sea point is **deterministic**, so assert **unconditionally** (NOT the
  `if (success) {...} else {...}` anti-pattern): `expect_true(res$success)` and
  `expect_match(res$data$area_full, "^27\\.3\\.")` for a verified-offshore
  Baltic coordinate. (`(19.0, 57.0)` resolves live to `27.3.d.28.2`; pick a
  coordinate well clear of the ~5 km boundary fuzz, verified before commit.)
- Never gate `expect_*` behind `if (precondition)`; use `skip_if()` with a
  reason (project convention).

## 8. Open items (resolved at implementation)

- Pin the exact WFS `GetFeature` URL for `ICES_AREAS_VISA_SIMPLE_5KM` on the
  `gis.ices.dk` GeoServer `ows` endpoint, **including `outputFormat=application/json`
  and no `srsName`** (verify via WebFetch / the `ices-fish-data` MCP before
  hardcoding the constant).
- Choose and verify the offshore live-test coordinate (§7).

## Appendix: reference patterns

- **GIS mechanics** (S2 toggle, CRS normalisation, `st_filter` point-in-poly):
  `R/functions/emodnet_habitat_utils.R` (S2 at 88-90 / 225-227, CRS at
  157-160, `st_filter` at 216-233). NOTE: that file predates the PR6
  structured-return convention (it uses top-level `library(sf)`, `stop()`,
  and returns `NULL`) — borrow only its **sf idioms**, not its error model.
- **Structured-return / cache-env / timeout conventions:** `ices_lookups.R`
  itself (the three existing helpers) — the authoritative template here.
- **`with_timeout()` signature:** `R/functions/validation_utils.R`.
