# ICES Subdivision Lookup — Design

**Date:** 2026-05-27
**Author:** A. Razinkovas-Baziukas (with Claude)
**Status:** Approved (brainstorming) — pending spec review
**Tracks:** v2-plan item **R1** (PR10 scope-M follow-up; see
`docs/superpowers/plans/2026-05-26-v2-plan-remaining.md`)

---

## 1. Goal & non-goals

### Goal
Add a geometry-based reverse lookup to `R/functions/ices_lookups.R`:
given a `(lon, lat)` point, return the ICES Statistical Area / Subdivision
code it falls in (e.g. `27.3.d.27`). This completes the ICES toolset —
`get_ices_area_codes()` (codes) and `get_ices_area_detail()` (metadata)
already exist; R1 adds **point → code**.

### Non-goals (YAGNI)
- **No consumer wiring.** This is a standalone building block, matching the
  "minimum surface needed for downstream code" philosophy `ices_lookups.R`
  was built on. No UI, no `ecopath_import_server.R` / spatial-module changes.
- **No vectorized/batch API.** Scalar single point only. A future consumer
  that needs a bounding box maps over corner/centroid points itself.
- **No descriptive-name join.** The shapefile carries no name field; callers
  who want the human-readable name pass the returned code to the existing
  `get_ices_area_detail()`.
- **No biomass calibration / DATRAS coupling.** That is R2 (scope-L), gated.

## 2. Data source

ICES GIS WFS GeoServer at **gis.ices.dk**, layer
`ices_eg:ICES_AREAS_VISA_SIMPLE_5KM` (alias `ices_areas`), fetched once as
GeoJSON and read with `sf::st_read()`.

Confirmed live attribute schema (one feature):

| Field | Example | Use |
|-------|---------|-----|
| `Area_Full` | `27.3.d.27` | primary returned code (most specific) |
| `Major_FA` | `27` | component |
| `SubArea` | `3` | component |
| `Division` | `d` | component |
| `SubDivisio` | `27` | component |
| `Area_27` | `3.d.27` | (FAO-area-27-relative) |

The **5 km-simplified** layer is acceptable: point-in-polygon at
Subdivision granularity is unaffected except within ~5 km of a boundary,
which is within the inherent fuzziness of statistical-area assignment.

**Chosen access (per approved Option a):** download once, cache to disk +
in-session, mirroring how `emodnet_habitat_utils.R` handles EUSeaMap layers.
The exact WFS `GetFeature` URL (GeoServer `ows` endpoint) is pinned in a
single module constant and verified against `gis.ices.dk` at implementation
time; the design does not depend on its exact string.

## 3. API contract

```r
lookup_ices_subdivision(lon, lat, shapefile_path = NULL, timeout = 60)
```

- `lon`, `lat`: numeric scalars (WGS84 degrees).
- `shapefile_path`: optional local override; default NULL = use the cached
  copy, else download + cache.
- `timeout`: network budget for the one-time layer fetch (`with_timeout`).

**Returns** the file's standard structured list:

```r
list(
  success = TRUE/FALSE,
  source  = "ICES_areas_shapefile",
  data    = <1-row data.frame> | NULL,
  error   = NA_character_ | "<reason>"
)
```

On success, `data` has one row:
`area_full` (e.g. `27.3.d.27`), `subdivision`, `division`, `subarea`,
`major_fa` (lower-cased column names, per project snake_case convention).

## 4. Data flow

1. **Input validation** — `lon`/`lat` numeric, finite, in `[-180,180]` /
   `[-90,90]`; else `success = FALSE`, `error = "lon/lat invalid"`.
2. **Layer acquisition (cached)** — if `.ices_cache$areas_sf` is set, reuse.
   Else load from `shapefile_path` if given; else load the on-disk cache
   file if present; else download the WFS GeoJSON (bounded by
   `with_timeout`), `sf::st_read()` it, write the on-disk cache, and store
   the `sf` object in `.ices_cache$areas_sf`.
3. **Point-in-polygon** — build an `sf` point in EPSG:4326, transform to the
   layer CRS if they differ, then `sf::st_intersects(point, areas_sf)`
   (the established `emodnet_habitat_utils.R:230` pattern).
4. **Result** — if exactly one (or, at a shared boundary, the first) polygon
   matches, return its attribute row mapped to the `data` columns. If none
   match (point on land / outside ICES areas), `success = FALSE`,
   `error = "point is outside all ICES areas"`.

## 5. Caching

- **In-session:** loaded `sf` object in the existing `.ices_cache` env
  (`.ices_cache$areas_sf`) — loaded once per R session, like the file's
  other cache entries.
- **On-disk:** the fetched layer is written once to a cache file under the
  app cache dir (e.g. `cache/spatial/ices_areas.gpkg`), so subsequent
  sessions/builds skip the network. `cache/` is already gitignored and not
  deployed; the file is a few MB at 5 km simplification.

## 6. Error handling

Matches the file's conventions (PR6 lineage):
- `sf` package missing → `success = FALSE`, `error = "sf package not installed"`.
- Network/download failure or timeout → `success = FALSE`, `error` populated,
  **`warning()`** emitted (production preserves no `message()` logs).
- Point outside all polygons → `success = FALSE`, `error` populated (NOT a
  warning — a legitimate "no match", not a fault).
- The function never `stop()`s; callers branch on `$success`.

## 7. Testing

- **Unit (no network):** point-in-polygon logic against a tiny synthetic
  `sf` fixture (2–3 hand-built square polygons with known codes) — assert a
  point inside polygon A returns A's code, a point in the gap returns
  `success = FALSE`. Plus the contract paths: invalid `lon`/`lat`, `sf`
  missing (skip_if not installed), and the structured-return shape.
- **Live (gated):** real `gis.ices.dk` fetch + a known Baltic point
  (e.g. ~`(19.0, 57.0)` → an `27.3.d.*` Subdivision) behind
  `RUN_LIVE_TESTS=true`, wrapped in `with_timeout` — runs on the nightly
  workflow (which already installs ICES packages; `sf` is a core dep).
- Never gate `expect_*` behind `if (precondition)`; use `skip_if()` with a
  reason (project convention).

## 8. Open items (resolved at implementation)

- Pin the exact WFS `GetFeature` URL for `ICES_AREAS_VISA_SIMPLE_5KM` on the
  `gis.ices.dk` GeoServer `ows` endpoint (verify via WebFetch / the
  `ices-fish-data` MCP before hardcoding the constant).
- Confirm `sf::st_read()` reads the WFS GeoJSON URL directly, or whether a
  two-step (download `.geojson` → read) is needed for the cache write.
