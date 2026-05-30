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
`R/config/ewe_group_dictionary.csv` and `data/external_traits/*.csv` are
handled. Reproducible, citeable, offline-safe; refreshed manually when WGBIFS
publishes. Live scraping of the acoustic portal was rejected (undocumented,
unstable, high maintenance). Dropping the phase was rejected (the user wants the
survey-vs-survey comparison even though SAG SSB partly embeds the acoustic
signal).

## 3. Architecture (reuses the source-agnostic pipeline)

`compute_survey_trends()` consumes a uniform
`data.frame(group, year, survey_value, is_ref_year)` regardless of source. The
demersal path produces it via `aggregate_survey_series()` (DATRAS); the clupeid
path via `aggregate_sag_ssb()` (SAG). This phase adds a third producer —
`aggregate_bias_series()` — and changes only the *clupeid routing* in the
observer. The trend math, plot, ref-rank, and exclusions logic are reused
untouched.

### 3.1 Data file — `data/acoustic/bias_indices.csv`

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

The reader **sums `abundance_index` per (aphia_id, year)** — so per-SD rows are
allowed and aggregate exactly like the BITS area-summing path. Trend-only, so
`unit` is documentation, never used in computation.

### 3.2 Pure function — `aggregate_bias_series()`

In `R/functions/rpath/survey_validation.R`, mirroring `aggregate_sag_ssb()`:

```r
aggregate_bias_series(bias_df, aphia_id) -> data.frame(year, survey_value)
```

- Filters `bias_df` to `aphia_id == aphia_id`.
- Sums `abundance_index` per year (na.rm semantics matching the existing
  series aggregators; missing/NA years surface a `warning()` then drop).
- Defensive on missing columns: if required columns absent, returns the
  empty `data.frame(year=integer(0), survey_value=numeric(0))`.
- Pure, file-free, fully unit-testable.

### 3.3 Loader — `.survey_trends_bias()` reactive

In `rpath_server.R`, parallel to `.survey_trends_dict()`:

```r
.survey_trends_bias <- reactive({
  f <- "data/acoustic/bias_indices.csv"
  if (file.exists(f)) read.csv(f, stringsAsFactors = FALSE)
  else data.frame(aphia_id = integer(0), stock = character(0), sd = character(0),
                  year = integer(0), abundance_index = numeric(0),
                  unit = character(0), source = character(0))
})
```

Empty frame when the file is absent → BIAS path yields no rows → SAG fallback.

### 3.4 Routing change (the one behavioral decision)

BIAS is the **primary** clupeid source; **SAG SSB is the automatic fallback**
when the CSV has no rows for that stock within the requested window. In the
observer's clupeid branch (`as.character(m$aphia_id) %in% names(SURVEY_TRENDS_CLUPEID_SAG)`):

1. `ser <- aggregate_bias_series(bias_df, m$aphia_id)`, then window to
   `window_years` and stamp `group` + `is_ref_year`.
2. If `nrow(ser) > 0` → use it; set `Class = "clupeid (BIAS acoustic)"`.
3. Else → existing SAG SSB path (`fetch_sag_ssb`); set
   `Class = "clupeid (SAG SSB)"`.
4. If both empty → exclusion row (`reason = "no BIAS or SAG data in window"`).

The `Class` column reports which source actually fired, so provenance is
auditable per group. No new UI control.

Because the trend is computed per-group and each series is normalised to its own
mean (`rel = value / mean`), mixing BIAS abundance (numbers) and SAG SSB
(tonnes/ratio) across different clupeid groups on the same plot is valid — each
group's curve is unit-free. The `Class` column documents the per-group source.

## 4. Tests (`tests/testthat/test-survey-validation.R`)

- `aggregate_bias_series`: sum-per-year; per-SD rows summing to one value/year;
  empty input; missing-column input; NA `abundance_index` handling; filtering to
  the requested `aphia_id` only.
- Schema guard for the shipped CSV: parses `data/acoustic/bias_indices.csv`,
  asserts required columns present, `aphia_id ∈ {126417, 126425}`, `year`
  numeric. **Skipped** (`skip_if`) with an actionable reason if the CSV is
  header-only (stub state) — never gates `expect_*` behind `if`.
- Routing is exercised at the existing integration level if a server-level test
  harness exists; otherwise the pure-function + schema tests are the contract.

## 5. Data acquisition (the long pole) & honesty constraint

Accurately transcribing real WGBIFS acoustic numbers is the riskiest task.

- The CSV will be seeded from **one specific, citeable** WGBFAS/WGBIFS table,
  recorded in the `source` column.
- **No fabricated values.** If real numbers cannot be verified during
  implementation, the CSV ships **header-only** (a stub, exactly like
  `data/external_traits/*.csv`) with the source URL in `data/acoustic/README.md`.
  In that state the BIAS path yields no rows and the feature degrades gracefully
  to the SAG SSB fallback — identical to current R2b behaviour, with the schema
  + loader + pure function in place for a future data drop.

## 6. Out of scope (unchanged R2 deferrals)

Editable/persistent mapping; stored model ref-year; age-0/1 filtering;
ordering-within-size-class; absolute calibration; BASS (spring acoustic) — only
BIAS (autumn) herring/sprat in this phase.

## 7. Files touched

- **new** `data/acoustic/bias_indices.csv` (seeded or header-only stub)
- **new** `data/acoustic/README.md` (source URL + refresh instructions)
- `R/functions/rpath/survey_validation.R` — add `aggregate_bias_series()`
- `R/modules/rpath_server.R` — `.survey_trends_bias()` reactive + clupeid routing
- `tests/testthat/test-survey-validation.R` — new tests
