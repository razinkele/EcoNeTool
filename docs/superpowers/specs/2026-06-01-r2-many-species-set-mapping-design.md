# R2 — Many-species set mapping for survey trends (design)

**Date:** 2026-06-01
**Status:** approved (brainstorm) → ready for plan
**Scope:** Sub-phase **A** of the R2 §7 "Editable + many-species mapping + persistence"
deferred phase (`docs/superpowers/specs/2026-05-30-r2-datras-survey-validation-design.md`
§7). Sub-phase **B** (editable mapping table) and **C** (persistence) are deferred to
their own later spec → plan → build cycles.

## 1. Problem

A Survey Trends group is auto-mapped to exactly **one** AphiaID
(`.map_group_to_aphia()` requires `length(hit) == 1`). But real Ecopath groups are
often *aggregates* — "Small demersals", "Flatfish", "Other gadoids" — that should map
to a **set** of species. Today such a group either resolves to a single arbitrary
species (if its name happens to hit `wm_name2id`) or is silently excluded as
"unmapped". The result: aggregate groups get no survey trend even though several of
their member species are well surveyed by BITS. This phase lets a group resolve to a
set whose BITS-indexed demersal members are summed into one per-year trend.

## 2. Goal

A group resolves to a **set of distinct AphiaIDs**; its demersal, BITS-indexed members
are fetched (at `min_age = 2L`, per the age-filter phase), tagged per species, and
summed per year into the group's trend. Non-demersal members are reported as excluded
sub-members (never silently dropped). Single-species and single-clupeid groups behave
exactly as today (backward-compatible).

### Decisions (resolved during brainstorm)
- **Reuse the existing flat dictionary, no schema change.** A group maps to the set of
  distinct `aphia_id`s across all its rows in `R/config/ewe_group_dictionary.csv`.
- **Demersal-only sum for mixed sets.** A group's trend is the BITS sum of its
  demersal members; clupeid/pelagic/not_surveyed/unmapped members are listed as
  excluded sub-members. BITS-demersal and BIAS-clupeid series are **never merged** into
  one group trend (body-weight/units confounding the R2 design forbids).
- **One row per (group, member) in the mapping table** for full transparency.
- **Backward-compat:** a set of exactly one clupeid keeps the existing BIAS/SAG
  routing; a single-species demersal group is just a set of size 1.

## 3. Architecture

```
ewe_group_dictionary.csv (group, aphia_id; aggregate groups = multiple rows)
        │
        ▼
.map_group_to_aphia(group, dictionary, pelagic_set, bits_indexed)
        │  returns data.frame(aphia_id, class) — ONE ROW PER MEMBER
        ▼
survey-trends loop (rpath_server.R), per group:
  • mapping_rows  <- one row per (group, member, class)   [table shows the set]
  • if members == a single clupeid -> existing select_clupeid_series() path (unchanged)
  • else:
      demersal_members <- members with class == "demersal"
      for each demersal member: lookup_datras_indices(aphia, min_age = 2L)
                                -> reshape -> aggregate_survey_series -> per-year series
      group trend <- .sum_member_series(per-member per-year series)  [sum per year]
      non-demersal members -> excluded_map sub-rows with their class as reason
        │
        ▼
compute/plot (unchanged): aggregate_survey_series runs PER MEMBER (above); the summed
group series then feeds compute_survey_trends -> plot exactly as a single-species series
does today.
```

## 4. Component changes (exact sites)

### 4.1 `.map_group_to_aphia()` → returns a set — `R/functions/rpath/survey_validation.R:210`
- Change the return from `list(aphia_id, class)` to a **data.frame with one row per
  member**: `data.frame(aphia_id = numeric, class = character, stringsAsFactors = FALSE)`.
- Dictionary hit: `aids <- unique(dictionary$aphia_id[dictionary$group == group_name])`,
  keep `aids[!is.na(aids) & aids > 0]`. Each becomes a row, classified individually
  (the existing per-aphia classification logic: pelagic / not_surveyed / demersal).
- No dictionary hit: fall back to `worrms::wm_name2id(group_name)` exactly as today
  (scalar guard, `warning()` on failure) → a single member row (or one `aphia_id = NA,
  class = "unmapped"` row if it fails).
- Empty/whitespace group name guard retained.

### 4.2 New helper `.sum_member_series()` — `R/functions/rpath/survey_validation.R`
- Signature: `.sum_member_series(member_series_list)` where each element is the
  `data.frame(year, survey_value)` output of `aggregate_survey_series` for one member
  (each member is already collapsed to one value per year; no per-frame species tagging
  is needed because the summation is purely by year).
- Returns `data.frame(year, survey_value)` summed across members per year. A year present
  for some members but not others sums the members that have it (full-join by year,
  treating absent members as contributing nothing — NOT as NA, since a species simply
  not caught that year is a real 0 contribution to the group total). If NO member has any
  finite value, returns a 0-row frame so the group routes to the existing
  insufficient-data exclusion.
- `na.rm` semantics: within a member, `aggregate_survey_series` already produced the
  per-year value (NA years dropped there); `.sum_member_series` sums the present values.
- **Scientific caveat (stated where the science lives):** the group total is a sum of
  raw per-species BITS indices, so it is dominated by the most abundant member. This is
  acceptable because R2 uses only the **normalised temporal trend** of each series (never
  the absolute level and never a cross-group comparison), so a group's trend reflects the
  combined year-to-year movement of its surveyed members. A biomass-weighted combination
  would need per-species mean weights (out of scope; that is the absolute-calibration
  phase). The mapping table + exclusions footnote make the member composition explicit.

### 4.3 Survey-trends loop — `R/modules/rpath_server.R:1936-1992`
- `m <- .map_group_to_aphia(...)` now returns a member data.frame. Build `mapping_rows`
  as **one row per member**: `data.frame(Group = g, AphiaID = m$aphia_id, Class = m$class)`
  (the existing single-row append becomes a multi-row append via `rbind`).
- **Single-clupeid branch (unchanged behaviour):** if `nrow(m) == 1` AND that member's
  aphia is in `SURVEY_TRENDS_CLUPEID_SAG`, run the existing `select_clupeid_series()`
  path verbatim, then `next`.
- **Else (set path):**
  - `dem <- m[m$class == "demersal", ]`. For each demersal member, fetch
    `lookup_datras_indices(aphia, surveys = "BITS", years = window_years, min_age = 2L)`,
    `aggregate_survey_series(...)` → a per-year series; collect non-empty ones.
  - Record every non-demersal member (`class != "demersal"`) in `excluded_map` as a
    sub-row keyed by group + member aphia so the footnote shows which members were
    excluded and why: `data.frame(group = sprintf("%s [member %s]", g, aphia),
    reason = class, stringsAsFactors = FALSE)`.
  - If at least one demersal member produced a series: `grp <- .sum_member_series(list)`;
    if `nrow(grp) > 0 && !all(is.na(grp$survey_value))` → tag `grp$group <- paste0(g,
    " [BITS 2+]")` (the age-filter tag, now representing the summed set), set
    `is_ref_year`, append, `summed_bits <- TRUE`. Else exclude the group with reason
    "no age-2+ survey index (recruitment-only)" (existing reason).
  - If no demersal members at all: exclude the group with reason
    "no BITS-surveyed demersal members".

### 4.4 Mapping table — `R/modules/rpath_server.R` (`output$survey_trends_mapping`)
- Already renders `mapping_df` (Group / AphiaID / Class). With one row per member it now
  shows the full set per group automatically. Add a short caption noting "one row per
  mapped species; aggregate groups list each member." No structural DT change.

## 5. Error handling
- Per-member fetch wrapped so one member failing (`res$success == FALSE`) is recorded as
  an excluded sub-member with a `warning()` (never `message()`), and the remaining
  members still sum. The group is only excluded when **no** demersal member yields data.
- No false zeros: a member with no age-2+ data contributes NA years (dropped by
  `aggregate_survey_series`), consistent with the age-filter phase; `.sum_member_series`
  sums present values only.
- All new `tryCatch(..., error=)` closures `warning()` before returning and use `<<-`
  for any outer-scope mutation (project conventions).

## 6. Testing (TDD)
Unit tests in `tests/testthat/test-survey-validation.R` (which already tests
`.map_group_to_aphia`):
1. **Set return:** a dictionary with three rows for group "Small demersals" (three
   distinct demersal aphia) → `.map_group_to_aphia` returns 3 rows, all class "demersal".
2. **Synonym dedupe:** three rows for "Cod" all `126436` → returns ONE row (distinct).
3. **Mixed-set classification:** a set with one demersal + one pelagic aphia → two rows
   with the correct per-member classes.
4. **Unmapped fallback unchanged:** a name not in the dictionary and not WoRMS-resolvable
   → one row `aphia_id = NA, class = "unmapped"` (mock `wm_name2id` to fail).
5. **Single-species backward-compat:** a one-row dictionary group → one member row
   (same as today's single-aphia semantics, just shaped as a 1-row frame).
6. **`.sum_member_series`:** two member series `(2019:2021, …)` → summed per year; a year
   present in only one member sums that member's value; all-empty input → 0-row frame.

(The loop changes in `rpath_server.R` are Shiny-observer code — verified by parse check +
a scope check that single-species and single-clupeid groups still route as before, not by
a unit test, consistent with the age-filter phase.)

## 7. Scope / non-goals
- **Out of scope (later sub-phases):** editable mapping table (B), persistence (C),
  clupeid-*set* summing (a set of ≥2 clupeids — currently excluded), absolute calibration,
  cross-group ordering.
- The dictionary is **author-curated**: this phase enables sets in the data model and the
  fetch/sum path; populating aggregate-group rows for specific MARBEFES models is a
  data-entry task, not code.

## Appendix: reference sites
- `.map_group_to_aphia` — `R/functions/rpath/survey_validation.R:210-238`
- `aggregate_survey_series` (per-member per-year collapse) — `:47-66`
- Dictionary load — `R/modules/rpath_server.R:1885` (`.survey_trends_dict`)
- Survey-trends loop (map → clupeid branch → demersal fetch) — `R/modules/rpath_server.R:1936-1992`
- Clupeid routing keyed on `SURVEY_TRENDS_CLUPEID_SAG` — `:1946-1966`
- Constants `SURVEY_TRENDS_PELAGIC_APHIA` / `SURVEY_TRENDS_BITS_INDEXED_APHIA` / `SURVEY_TRENDS_CLUPEID_SAG` — `R/config.R` (or the survey-trends config block)
- Dictionary CSV — `R/config/ewe_group_dictionary.csv` (schema `group, aphia_id`)
