# R2 — Age-filtered abundance for survey trends (design)

**Date:** 2026-06-01
**Status:** approved (brainstorm) → ready for plan
**Scope:** one of the §7 deferred phases of the R2 DATRAS survey-validation feature
(`docs/superpowers/specs/2026-05-30-r2-datras-survey-validation-design.md` §7,
"Age-filtered abundance").

## 1. Problem

The demersal survey trend (R2) is built from `abundance_index`, which
`.datras_reshape_indices()` computes as the **sum across every `Age_*` column**
`icesDatras::getIndices()` returns (`ices_lookups.R:166-177`). That total includes
age-0 and age-1 fish. Those youngest year-classes are recruitment pulses: they
swing wildly year-to-year for reasons (settlement success, gear selectivity at
small sizes) unrelated to the adult biomass dynamics the Ecopath mass-balance
the trend informs. Including them injects recruitment noise into a trend that is
meant to reflect the exploitable/adult stock. The original R2 spec flagged this
as a known caveat (§1.4) and deferred the fix to §7.

## 2. Goal

Sum only ages **≥ 2** (drop age-0 and age-1) for the R2 demersal trend, while
leaving every other consumer of `lookup_datras_indices()` — notably the R3
trait-research panel — on the current all-ages behavior. The cutoff is fixed
(not per-species, not user-selectable) but implemented as a parameter so those
remain trivial future extensions.

### Decisions (resolved during brainstorm)
- **Cutoff = age 2+ (drop 0 and 1).** Matches the deferred-spec's "drop age-0/1"
  and the adult/exploitable intent. Age-1 still carries lagged recruitment signal,
  so removing it as well gives a cleaner adult trend.
- **Fixed, not per-species or UI-selectable.** YAGNI: a maintained per-species
  table or a UI control is disproportionate for a *relative temporal* trend. The
  parameter makes both cheap later.
- **Parameter default preserves current behavior** so R3 and any other caller are
  untouched (backward-compatible, opt-in per caller).

## 3. Architecture

A single `min_age` parameter threaded through the two existing functions, applied
at the one point where ages are already collapsed to a sum. Nothing downstream of
`abundance_index` changes — `aggregate_survey_series()`, `compute_survey_trends()`,
and the plot all consume the (now age-filtered) `abundance_index` unchanged.

```
getIndices (Age_0..Age_N)
   │
   ▼
.datras_reshape_indices(idx, svy, yr, min_age)   ← filter here: keep cols age ≥ min_age
   │  abundance_index = rowSums(kept age cols)
   ▼
lookup_datras_indices(aphia_id, surveys, years, min_age)   ← threads min_age; min_age in cache key
   │
   ├── R2 demersal fetch (rpath_server.R:1970): min_age = 2L   → adult trend
   └── R3 trait panel + any other caller: min_age = 0L (default) → all ages (unchanged)
```

## 4. Component changes (exact sites)

### 4.1 `.datras_reshape_indices(idx, svy, yr, min_age = 0L)` — `R/functions/ices_lookups.R:165`
- Parse the integer age from each kept column name:
  `ages <- suppressWarnings(as.integer(sub("^Age_", "", age_cols)))`.
- Select columns to sum:
  - `min_age == 0L` → **all** `Age_*` columns (current behavior, incl. any whose
    suffix doesn't parse to an integer — preserved exactly).
  - `min_age > 0L` → `age_cols[!is.na(ages) & ages >= min_age]`.
- Compute `abundance_index`:
  - `length(age_cols) == 0` → `NA_real_` (existing behavior, unchanged).
  - `min_age > 0L && length(kept) == 0` (ages present but none ≥ cutoff) →
    **`NA_real_`** for every row. **NA, not 0** — so the year is dropped, never
    recorded as false-zero abundance. This is the critical correctness point.
  - otherwise → `rowSums(age_mat[, kept, drop = FALSE], na.rm = TRUE)`.
- Output schema unchanged: `survey, year, quarter, index_area, abundance_index`.

### 4.2 `lookup_datras_indices(aphia_id, surveys, years, timeout, min_age = 0L)` — `R/functions/ices_lookups.R:215`
- New `@param min_age` (default `0L`, all ages). Update the roxygen description of
  `abundance_index` to note it is the sum across `Age_*` columns **with age ≥
  `min_age`**.
- Pass `min_age` into the reshape call (`ices_lookups.R:307`):
  `.datras_reshape_indices(idx, svy, yr, min_age)`.
- Add `min_age` to `cache_key` (`ices_lookups.R:236-238`) so an R2 (`min_age=2`)
  and an R3 (`min_age=0`) lookup of the same species/survey do **not** collide in
  `.ices_cache`. Form: append `"_a", min_age` to the key.

### 4.3 R2 demersal fetch — `R/modules/rpath_server.R:1970`
- `res <- lookup_datras_indices(m$aphia_id, surveys = "BITS", years = window_years, min_age = 2L)`.
- Already inside the `m$class == "demersal"` branch (guard at line 1965), so the
  clupeid (BIAS/SAG) path is untouched.

### 4.4 Trend banner — `R/modules/rpath_server.R:567`
- Append one sentence to the existing `alert alert-info` banner HTML:
  *"Demersal indices sum ages 2+ (age-0/1 excluded as recruitment-pulse noise)."*
- Static text; matches the fixed cutoff. (If a UI selector is added later, this
  becomes dynamic — out of scope now.)

## 5. Error handling

- The filter is pure column subsetting — no new failure modes, no new `tryCatch`.
- The single trap is `rowSums` over an empty column subset yielding `0`; avoided
  by emitting `NA_real_` when the cutoff removes all ages. **NA, not 0**, so the
  value is never a false zero.
- **NA propagation (verified against the real code):** for a species with no
  age-≥2 data, every affected `(survey, year, quarter)` frame yields `NA`
  `abundance_index`. `aggregate_survey_series()` (`survey_validation.R:57-64`)
  sums per year with `na.rm = FALSE`, so such a year becomes `survey_value = NA`
  **and emits a `warning("[survey_trends] NA abundance in area sum for year …")`**.
  `compute_survey_trends()` then drops NA years up front
  (`s <- s[!is.na(s$survey_value), ]`, `survey_validation.R:264`); a group left
  with too few years falls into the **existing** insufficient-data exclusion and
  is reported in the exclusions footnote. So: no silent loss, no false zero, no
  new plumbing — the NA routes through machinery that already exists.
- **Practical note:** the BITS R2 species (cod, flounder, plaice) all have
  age-≥2 indices, so the sparse guard (and its per-year warning) does not fire in
  normal operation; it is purely a defensive net for a species/survey-year that
  genuinely lacks adult ages. The warning text is benign in that edge case (the
  year is dropped, not silently mis-summed).

## 6. Testing (TDD)

Extend `tests/testthat/test-ices-lookups.R` (already holds reshape tests ~38-70
and a `getIndices` mock idiom ~84-99).

Unit tests on the pure `.datras_reshape_indices`:
1. **Backward-compat:** `min_age = 0` on an Age_0..Age_3 frame → `abundance_index`
   equals the all-ages `rowSums` (identical to current).
2. **Cutoff:** `min_age = 2` on the same frame → `abundance_index` equals
   `Age_2 + Age_3` per row (age-0/1 excluded).
3. **Sparse guard:** `min_age = 2` on a frame with only `Age_0`/`Age_1` →
   `abundance_index` all `NA` (asserts NA, **not** 0).
4. **No age cols:** `min_age = 2` on a frame with no `Age_*` columns → `NA`
   (unchanged from current).
5. **NA within kept cols:** an `NA` value in `Age_2` with a value in `Age_3` →
   `na.rm` sum returns the `Age_3` value (not `NA`).

Mock-level on `lookup_datras_indices` (existing `with_mock`/stub idiom):
6. Same mocked `getIndices` frame, two calls with `min_age = 0` vs `min_age = 2`
   → different `abundance_index` totals **and** distinct cache entries (proves the
   `min_age` cache-key separation; guards against R2/R3 cross-contamination).

Optional (gated live test, `RUN_LIVE_TESTS`): extend the existing BITS cod live
test to also fetch `min_age = 2` and assert its summed index ≤ the all-ages index.

## 7. Scope guard / non-goals

- **R3 untouched:** R3's `lookup_datras_indices` call passes no `min_age` → stays
  all-ages. Confirm by grep for all call sites before finishing; only the
  `rpath_server.R:1970` site gains `min_age = 2L`.
- **Not in scope:** per-species cutoffs, a UI age selector, absolute biomass
  calibration, editable/persistent mapping (separate §7 phases).

## Appendix: reference sites
- `.datras_reshape_indices` — `R/functions/ices_lookups.R:165-187`
- `lookup_datras_indices` (cache key ~236, reshape call ~307) — `:215-309`
- `aggregate_survey_series` (consumes `abundance_index`) — `R/functions/rpath/survey_validation.R:47-65`
- R2 demersal fetch — `R/modules/rpath_server.R:1965-1970`
- Trend banner — `R/modules/rpath_server.R:563-574`
- Test idioms — `tests/testthat/test-ices-lookups.R` (reshape ~38-70, mock ~84-99, live-gate ~195-221)
