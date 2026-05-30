# R2 — BIAS Acoustic Clupeid Coverage (design spec)

**Date:** 2026-05-30
**Status:** approved (design); pending implementation plan
**Builds on:** `2026-05-30-r2-datras-survey-validation-design.md` (R2 P1 + R2b SAG SSB)

## 1. Problem & motivation

R2 P1 compares an Ecopath model's per-group biomass inputs against ICES survey
*temporal trends* (trend-only; every cross-group biomass-vs-abundance view is
body-weight confounded and was dropped). The demersal path uses ICES DATRAS
**BITS** bottom-trawl indices via `icesDatras::getIndices()`. BITS only computes
indices for cod (126436) / flounder (127141) / plaice (127143), so demersal
coverage is inherently thin.

R2b added the dominant Baltic pelagic groups — herring (126417) and sprat
(126425) — by pulling their assessed **SSB** from ICES SAG (`fetch_sag_ssb`),
because BITS bottom-trawl cannot index pelagics. SAG SSB is an *assessment
output*, not a survey observation.

This phase adds the **BIAS acoustic survey** abundance index series for the same
two clupeids, giving a *survey-based* trend (acoustic) that is methodologically
closer to the DATRAS demersal path (survey-vs-survey rather than
survey-vs-assessment-output).

## 2. Key constraint: BIAS has no clean live API

Verified during brainstorming (2026-05-30):

- BIAS is **not** a DATRAS survey. `lookup_datras_indices()` already documents
  this: BIAS indices live in a different ICES database. `icesDatras` is
  bottom-trawl only.
- BIAS data sits in an Access DB (`BIAS_DB.mdb`) behind the ICES acoustic data
  portal (acoustic.ices.dk) — an export/screening portal, **not** a documented
  REST web service for derived index series.
- WGBIFS produces herring/sprat/cod acoustic abundance index series (WGBFAS
  tuning fleets), but the `ices-eg/wg_WGBIFS` GitHub repo holds allocation
  procedures and plots, not a stable index-series data product.
- No CRAN package is analogous to `icesDatras` for acoustic data.

**Decision:** source the series from a **curated, versioned static CSV**
transcribed from a citeable WGBIFS/WGBFAS report table — mirroring how
`R/config/ewe_group_dictionary.csv` is handled. Reproducible, citeable,
offline-safe; refreshed manually when WGBIFS publishes. Live scraping of the
acoustic portal was rejected (undocumented, unstable, high maintenance).
Dropping the phase was rejected (the user wants the survey-vs-survey comparison
even though SAG SSB partly embeds the acoustic signal).

**Deploy note (post-review):** the CSV lives in `R/config/` — **not** `data/` —
because the documented laguna deploy runs `deploy-windows.ps1 -SkipData`, which
skips `data/`. A file under `data/` would ship dormant (server keeps using the
SAG fallback). `R/config/` is deployed as code, so the BIAS path activates in
production. This matches the existing `R/config/ewe_group_dictionary.csv`.

## 3. Architecture (reuses the source-agnostic pipeline)

`compute_survey_trends()` consumes a uniform
`data.frame(group, year, survey_value, is_ref_year)` regardless of source. The
demersal path produces it via `aggregate_survey_series()` (DATRAS); the clupeid
path via `aggregate_sag_ssb()` (SAG). This phase adds a third producer —
`aggregate_bias_series()` — and changes only the *clupeid routing* in the
observer. The trend math, plot, ref-rank, and exclusions logic are reused
untouched.

### 3.1 Data file — `R/config/bias_indices.csv`

Keyed by AphiaID (matches `SURVEY_TRENDS_CLUPEID_SAG` keying). Columns:

| column            | type    | notes                                              |
|-------------------|---------|----------------------------------------------------|
| `aphia_id`        | integer | 126417 (herring) or 126425 (sprat)                 |
| `stock`           | text    | ICES stock key, e.g. `her.27.25-2932`              |
| `sd`              | text    | subdivision label, e.g. `25-29+32`, `22-32`        |
| `year`            | integer | survey year                                        |
| `abundance_index` | numeric | acoustic abundance index (documentation unit)      |
| `unit`            | text    | e.g. `million_ind` — documentation only, not math  |
| `source`          | text    | per-row citation, e.g. `WGBFAS 2024 ... tuning fleet` |

The reader **sums `abundance_index` per (stock, year)** for the requested
AphiaID's expected stock key — so per-SD rows aggregate like the BITS
area-summing path, but **cross-stock summing is prevented**. AphiaID 126417
(herring) spans several ICES stocks (central Baltic `her.27.25-2932`,
Gulf-of-Riga `her.27.28`, Western Baltic `her.27.20-24`); summing across them
would be a body-of-fish confound. The reader therefore filters on **both
`aphia_id` and the expected `stock` key** (the value already held in
`SURVEY_TRENDS_CLUPEID_SAG`). Trend-only, so `unit` is documentation, never used
in computation.

### 3.2 Pure function — `aggregate_bias_series()`

In `R/functions/rpath/survey_validation.R`, mirroring `aggregate_sag_ssb()`:

```r
aggregate_bias_series(bias_df, aphia_id, stock_key = NULL) -> data.frame(year, survey_value)
```

- Filters `bias_df` to `aphia_id`, and — when `stock_key` is supplied — to that
  single `stock` (prevents cross-stock summing; see §3.1).
- Sums `abundance_index` per year. `na.rm = FALSE` on the per-year sum so one NA
  among a year's SD rows poisons the whole year, which surfaces a `warning()`
  then drops the year (matches `aggregate_survey_series`).
- Distinguishes an **unparseable** `abundance_index` (non-numeric token in the
  CSV) from a genuinely **absent** value: a parse failure emits its own
  `warning()` so a transcription error isn't silently read as "BIAS lacked that
  year".
- Defensive on missing columns: if required columns absent, returns the
  empty `data.frame(year=integer(0), survey_value=numeric(0))`.
- Pure, file-free, fully unit-testable.

### 3.2a Pure routing helper — `select_clupeid_series()`

The BIAS-vs-SAG decision, BIAS windowing, `Class` label, and exclusion text are
the new behaviour the feature exists for. They must not live un-tested in the
observer. Extract them into a pure helper in the same file:

```r
select_clupeid_series(bias_df, aphia_id, stock_key, window_years, ref_year,
                      fetch_sag = fetch_sag_ssb)
  -> list(series = df(year, survey_value, is_ref_year) | NULL,
          class  = "clupeid (BIAS acoustic)" | "clupeid (SAG SSB)",
          excluded_reason = NULL | "no BIAS or SAG data in window")
```

`fetch_sag` is injected so tests pass a fake. The observer calls this, stamps
`group`, and appends to `series_list`. Four branches, each unit-tested:
1. BIAS rows in window → BIAS series, `class = "clupeid (BIAS acoustic)"`.
2. BIAS rows exist but all outside `window_years` → falls through to SAG.
3. BIAS empty + SAG success → SAG series, `class = "clupeid (SAG SSB)"`.
4. BIAS empty + SAG failure/empty → `series = NULL`,
   `excluded_reason = "no BIAS or SAG data in window"`.

### 3.3 Loader — `.survey_trends_bias()` reactive

In `rpath_server.R`, parallel to `.survey_trends_dict()`:

```r
.survey_trends_bias <- reactive({
  f <- "R/config/bias_indices.csv"
  if (file.exists(f)) read.csv(f, stringsAsFactors = FALSE)
  else data.frame(aphia_id = integer(0), stock = character(0), sd = character(0),
                  year = integer(0), abundance_index = numeric(0),
                  unit = character(0), source = character(0))
})
```

Empty frame when the file is absent → BIAS path yields no rows → SAG fallback.
(Re-reads on each Run click, intentionally matching `.survey_trends_dict()`.)

### 3.4 Routing change (the one behavioral decision)

BIAS is the **primary** clupeid source; **SAG SSB is the automatic fallback**
when the CSV has no rows for that stock within the requested window. The
observer's clupeid branch (`as.character(m$aphia_id) %in% names(SURVEY_TRENDS_CLUPEID_SAG)`)
delegates the whole decision to `select_clupeid_series()` (§3.2a):

```r
stock_key <- SURVEY_TRENDS_CLUPEID_SAG[[as.character(m$aphia_id)]]
sel <- select_clupeid_series(bias_df, m$aphia_id, stock_key, window_years, ref_year)
mapping_rows[[length(mapping_rows)]]$Class <- sel$class
if (is.null(sel$series)) <add exclusion row sel$excluded_reason>
else <stamp group (with provenance tag, below) + append to series_list>
```

The `Class` column reports which source fired, so provenance is auditable.

**Per-curve provenance on the plot.** Because the trend is computed per-group and
each series is normalised to its own mean (`rel = value / mean`), mixing BIAS
abundance (numbers) and SAG SSB (tonnes/ratio) across clupeid groups on the same
plot is mathematically valid — each curve is unit-free. But the *scientific
meaning* differs (acoustic abundance vs spawning biomass), and a group can flip
source between runs as the CSV gains data. To stop a reader misattributing a
source artefact to ecology, the **plot group label carries the source tag**: the
series `group` becomes `paste0(g, " [BIAS]")` or `paste0(g, " [SSB]")` so a
mixed-source plot is self-documenting in the legend. (The mapping side-table
keeps the untagged `Group` + full `Class`.)

## 4. Tests (`tests/testthat/test-survey-validation.R`)

- `aggregate_bias_series`: sum per-SD-rows-per-year; **stock filtering** (rows
  for a non-target stock excluded); a multi-SD year with one NA → whole year
  dropped (not partially summed) + warning; empty input; missing-column input;
  filtering to the requested `aphia_id` only; empty-frame column schema.
- `select_clupeid_series` (with an injected fake `fetch_sag`): all four branches
  of §3.2a — BIAS-hit, BIAS-out-of-window→SAG, BIAS-empty→SAG-success,
  both-empty→exclusion — asserting the returned `class`, `series`, and
  `excluded_reason`. This is what makes the routing fork a red-green contract
  rather than untested observer code.
- Schema guard for the shipped CSV: parses `R/config/bias_indices.csv`, asserts
  the seven required columns present. **Then** `skip_if(nrow == 0, ...)` (stub
  state) before value assertions: `stock ∈ names(SURVEY_TRENDS_CLUPEID_SAG)`
  (robust to adding a clupeid — assert against the constant, not literals),
  `aphia_id ∈ names(...)` keys, `year` parseable integer, `abundance_index`
  numeric and `>= 0`, every row's `source` non-empty (mechanical no-fabrication
  guard). Never gates `expect_*` behind `if`.

## 5. Data acquisition (the long pole) & honesty constraint

Accurately transcribing real WGBIFS acoustic numbers is the riskiest task.

- The CSV will be seeded from **one specific, citeable** WGBFAS/WGBIFS table.
  Each row's `source` must name the **report + table** (not just a portal URL),
  and the schema-guard test asserts `source` is non-empty — a mechanical, if
  partial, no-fabrication guard.
- **No fabricated values.** Bound the search to ~2-3 source fetches; if a
  specific citeable table can't be verified, ship the CSV **header-only** (a
  stub) and stop — do not block. In stub state the BIAS path yields no rows and
  the feature degrades to the SAG SSB fallback (identical to current R2b
  behaviour), with schema + loader + pure functions in place for a later drop.
- Before committing **real** values, paste the transcribed table back for review
  (the table is small — one stock, ~10-15 years — so a human can spot-check it).
- **Single-stock / non-overlapping-SD invariant** (correctness depends on it,
  documented in the README): all rows for a given `aphia_id` must belong to one
  ICES stock and to mutually exclusive subdivisions. Specifically: herring SD 28
  means **28.2 only** (open central Baltic) — exclude **28.1 (Gulf of Riga,
  her.27.28)**; sprat SD 22-24 autumn BIAS coverage is partial, so record the
  exact survey+SDs per row in `source`.

## 6. Out of scope (unchanged R2 deferrals)

Editable/persistent mapping; stored model ref-year; age-0/1 filtering;
ordering-within-size-class; absolute calibration; BASS (spring acoustic) — only
BIAS (autumn) herring/sprat in this phase.

## 7. Files touched

- **new** `R/config/bias_indices.csv` (seeded or header-only stub — in `R/config/`
  so the `-SkipData` deploy carries it; see §2 deploy note)
- **new** `R/config/bias_indices.README.md` (schema, single-stock invariant,
  source URLs, refresh instructions)
- `R/functions/rpath/survey_validation.R` — add `aggregate_bias_series()` and the
  pure routing helper `select_clupeid_series()`
- `R/modules/rpath_server.R` — `.survey_trends_bias()` reactive; clupeid branch
  delegates to `select_clupeid_series()`; the "summed all BITS areas"
  notification is gated on a real demersal series having fired (it previously
  mis-fired whenever any series existed — now more visible with BIAS/SAG-only
  models)
- `tests/testthat/test-survey-validation.R` — `aggregate_bias_series`,
  `select_clupeid_series`, and CSV schema-guard tests
