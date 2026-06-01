# R2 — Many-species set mapping for survey trends (design)

**Date:** 2026-06-01
**Status:** approved (brainstorm) → revised after 4-angle spec review → ready for plan
**Scope:** Sub-phase **A** of the R2 §7 "Editable + many-species mapping + persistence"
deferred phase (`docs/superpowers/specs/2026-05-30-r2-datras-survey-validation-design.md`
§7). Sub-phase **B** (editable mapping table) and **C** (persistence) are deferred to
their own later spec → plan → build cycles.

## 1. Problem

A Survey Trends group is auto-mapped to exactly **one** AphiaID
(`.map_group_to_aphia()` requires `length(hit) == 1`). But real Ecopath groups are
often *aggregates* — "Small demersals", "Flatfish" — that should map to a **set** of
species. Today such a group resolves to a single arbitrary species (if its name hits
`wm_name2id`) or is silently excluded as "unmapped". Aggregate groups thus get no
survey trend even when several member species are well surveyed by BITS. This phase
lets a group resolve to a set whose demersal, BITS-indexed members are summed into one
per-year trend — **with member-level auditing so a partial set never masquerades as a
complete one.**

## 2. Goal & decisions

A group resolves to a **set of distinct AphiaIDs**; its demersal, BITS-indexed members
are fetched (at `min_age = 2L`), aggregated per member, then summed **over the years
all contributing members cover** into the group trend. Every member that does not
contribute (wrong class, fetch failure, no age-2+ data, no quarter data) is surfaced as
an excluded sub-member. Single-species and single-clupeid groups behave exactly as today.

### Decisions (resolved in brainstorm + spec review)
- **Reuse the existing flat dictionary, no schema change.** A group maps to the set of
  distinct `aphia_id`s across all rows of `R/config/ewe_group_dictionary.csv` whose
  `group` cell **exactly equals** the Ecopath group label (raw `==`, no trimming — a
  stated contract; aggregate rows MUST match the model label byte-for-byte).
  - **Invariant (must be documented in the CSV header/README):** *a repeated `group`
    key = aggregate members; a unique `group` key = a single species (possibly with
    synonym keys elsewhere).* Today every `group` value is unique (synonyms like
    "Cod"/"Atlantic cod"/"Gadus morhua" are **distinct keys** → the feature is purely
    additive and changes nothing for existing rows). Adding an aggregate means adding
    ≥2 rows sharing one `group` key; an author must grep for an existing row with that
    key first, because a pre-existing synonym row would silently become a set member.
- **Demersal-only sum for mixed sets.** A group's trend is the BITS sum of its demersal
  members; clupeid/pelagic/not_surveyed/unmapped members are excluded sub-members.
  BITS-demersal and BIAS-clupeid are **never merged** (body-weight/units confounding).
- **Constant-coverage summation (review fix #1).** The summed series spans only years in
  the **intersection** of the contributing members' covered years; partial-coverage
  years are dropped (not zero-filled) and reported, so a member's coverage starting or
  ending mid-window cannot inject a spurious step into the trend.
- **No silent member drops (review fix #2).** Every demersal member that fails to yield
  a usable series gets its own excluded sub-row + `warning()`. The group is excluded only
  when **zero** members contribute.
- **Dominant-member disclosure (review fix).** Because the sum of raw numbers-per-hour is
  dominated by the most abundant member, the orchestration computes each member's mean
  share of the summed index and, when one member exceeds 70 %, emits a `warning()` and a
  footnote note so the group trend is not silently read as a balanced aggregate.
- **Backward-compat:** a set of exactly one clupeid keeps the existing BIAS/SAG routing;
  a single demersal species is a set of size 1 (same trend as today).

## 3. Architecture

```
ewe_group_dictionary.csv (group, aphia_id; aggregate group = ≥2 rows sharing the key)
        │
        ▼
.map_group_to_aphia(group, dictionary, pelagic_set, bits_indexed)
        │  returns data.frame(aphia_id, class) — ONE ROW PER MEMBER (classified)
        ▼
survey-trends loop (rpath_server.R), per group:
  • mapping_rows  <- one row per (group, member, class)
  • if nrow(members)==1 AND that aphia ∈ SURVEY_TRENDS_CLUPEID_SAG
        -> existing select_clupeid_series() path (UNCHANGED), next
  • else  -> res <- .fetch_group_demersal_series(members, quarter, window_years,
                                                 ref_year, group_label = g)
            res$series   : data.frame(group, year, survey_value, is_ref_year) or NULL
            res$excluded : data.frame(group, reason)  -- one row per dropped member
            append res$series to series_list (if non-NULL); rbind res$excluded into
            excluded_map; if res$series NULL, also add a group-level exclusion row.
        │
        ▼
compute_survey_trends -> plot  (UNCHANGED: one summed series per group, exactly the
contract a single-species series satisfies today).
```

The per-member fetch/aggregate/sum/exclude/dominance logic lives in the **pure,
mockable helper `.fetch_group_demersal_series()`** (review fix: testability) — NOT inline
in the observer. The observer only reads the dictionary, dispatches the clupeid branch,
accumulates lists, and drives the progress bar.

## 4. Component changes (exact sites)

### 4.1 `.map_group_to_aphia()` → returns a member frame — `R/functions/rpath/survey_validation.R:210`
- Return changes from `list(aphia_id, class)` to **`data.frame(aphia_id, class,
  stringsAsFactors = FALSE)` — one row per member.** Update the roxygen `@return`
  (currently `survey_validation.R:207`) to the frame shape.
- **Dictionary-first, fallback-suppressed (review fix):**
  `rows <- dictionary$aphia_id[dictionary$group == group_name]`.
  If `length(rows) >= 1` (any rows match the key): `aids <- unique(rows[!is.na(rows) &
  rows > 0])`. **The WoRMS fallback is suppressed whenever any rows match** — even if all
  matched rows are invalid (then return one `aphia_id = NA, class = "unmapped"` row). This
  prevents an aggregate name with a typo'd aphia from WoRMS-resolving to one wrong species.
  - If `length(aids) >= 1` → one row per aid.
  - If rows matched but no valid aid → single `aphia_id = NA, class = "unmapped"` row.
- If **no** rows match the key → `wm_name2id(group_name)` exactly as today (scalar guard,
  `warning()` on failure) → one member row (or `aphia_id = NA, class = "unmapped"`).
- **Shared classifier:** both the dictionary and WoRMS branches funnel each resolved aid
  through ONE `classify_aphia(aid, pelagic_set, bits_indexed)` (the existing class ladder:
  `NA→unmapped`, `aid ∈ pelagic_set → pelagic`, `bits_indexed` non-NULL and `aid ∉
  bits_indexed → not_surveyed`, else `demersal`) so dictionary and WoRMS rows classify
  identically. Extract this ladder (currently inline at `:226-234`) into the helper.
- Empty/whitespace group-name guard retained.

### 4.2 New helper `.sum_member_series()` — `R/functions/rpath/survey_validation.R`
- `.sum_member_series(member_series_list)` — each element is the
  `data.frame(year, survey_value)` from `aggregate_survey_series` for one member. Returns
  `data.frame(year, survey_value)` (canonical empty form
  `data.frame(year = integer(0), survey_value = numeric(0))`).
- **Constant-coverage rule (review fix #1):** a member "covers" a year iff it has a row
  for that year with a **finite** `survey_value` (an absent year AND a present-but-`NA`
  year both count as *not covered* — these are unified, resolving the na.rm ambiguity).
  The output years are the **intersection** of all members' covered years; each output
  `survey_value` is the sum across members for that year. Years not covered by every
  contributing member are **dropped, not zero-filled.**
- If the intersection is empty → 0-row frame (group routes to the existing
  insufficient-data exclusion).
- The dropped partial-coverage years (covered by some but not all members) are returned
  as an attribute / second list element so the caller can surface them in a coverage note.

### 4.3 New helper `.fetch_group_demersal_series()` — `R/functions/rpath/survey_validation.R`
- Signature: `.fetch_group_demersal_series(members, quarter, window_years, ref_year,
  group_label, fetch_indices = lookup_datras_indices, aggregate = aggregate_survey_series)`
  — `fetch_indices`/`aggregate` injectable for tests.
- Returns `list(series = <df(group, year, survey_value, is_ref_year)> or NULL,
  excluded = <df(group, reason)>, notes = <character>)`.
- Logic:
  1. `dem <- members[members$class == "demersal", ]`; `other <- members[class != "demersal"]`.
     Each `other` member → an `excluded` sub-row keyed `sprintf("%s [member %s]",
     group_label, aphia)`, reason = its class.
  2. For each demersal member: `res <- fetch_indices(aphia, surveys = "BITS",
     years = window_years, min_age = 2L)`; then **branch exactly like the single-species
     path** (`aggregate(res$data, quarter = quarter, areas = NULL)`):
     - `res$success == FALSE` → excluded sub-row reason "fetch failed" + `warning()`.
     - `nrow(ser) == 0` → excluded sub-row reason "no data for selected quarter".
     - `all(is.na(ser$survey_value))` → excluded sub-row reason
       "no age-2+ survey index (recruitment-only)".
     - else → keep `ser` as a contributing member series.
     The `quarter` and `areas = NULL` arguments are mandatory and identical to the
     single-species path (review fix: do not drop `quarter`).
  3. If ≥1 contributing series: `grp <- .sum_member_series(list)`. If `nrow(grp) == 0`
     → `series = NULL`, add reason "no overlapping survey years across members". Else tag
     `grp$group <- paste0(group_label, " [BITS 2+]")`, set `grp$is_ref_year <- grp$year
     == ref_year`, `series = grp`; record dropped partial-coverage years in `notes`.
  4. If 0 contributing series and ≥1 demersal member existed → `series = NULL`, reason
     "no usable age-2+ index for any member".
  5. If 0 demersal members at all → `series = NULL`, reason "no BITS-surveyed demersal
     members" (covers all-pelagic / all-clupeid sets too).
  6. **Dominance:** over the common years, `share_i = mean(member_i) / sum(mean(members))`.
     If `max(share) > 0.70` and >1 contributing member, append to `notes`:
     "trend dominated by member <aphia> (NN% of summed index)" and `warning()` it.
- All `tryCatch(..., error=)` closures `warning()` before returning; outer-scope mutation
  uses `<<-` (project conventions) — but prefer plain returns inside this pure helper.

### 4.4 Survey-trends loop — `R/modules/rpath_server.R:1936-1992`
- `m <- .map_group_to_aphia(...)` now returns a member frame. Build `mapping_rows` as
  **one row per member** (`rbind` of `data.frame(Group=g, AphiaID=m$aphia_id[i],
  Class=m$class[i])`).
- Single-clupeid branch: `if (nrow(m) == 1 && as.character(m$aphia_id[1]) %in%
  names(SURVEY_TRENDS_CLUPEID_SAG))` → existing `select_clupeid_series` path verbatim,
  `next`. (The current scalar `if (!is.na(m$aphia_id) && as.character(m$aphia_id) %in% …)`
  at `:1946` MUST be fully replaced — never evaluated with a length-N vector `m`.)
- Else: call `.fetch_group_demersal_series(...)`; append `res$series` (if non-NULL) to
  `series_list` and set `summed_bits <- TRUE`; `rbind` `res$excluded` into `excluded_map`;
  if `res$series` is NULL add a group-level exclusion row; carry `res$notes` into the
  coverage/dominance footnote.

### 4.5 UI surfacing — `R/modules/rpath_server.R`
- **Mapping table** (`output$survey_trends_mapping`): already renders `mapping_df`
  (Group/AphiaID/Class); one-row-per-member now shows the full set automatically. Add a
  caption: "one row per mapped species; aggregate groups list each member."
- **Exclusions footnote** (`output$survey_trends_exclusions`): now includes per-member
  sub-rows, so its `Excluded (N)` count becomes **member-rows, not groups** — state this
  in the caption ("counts excluded species/members") so the shift is not surprising.
- **Coverage/dominance note:** render `res$notes` (dropped partial-coverage years; a
  dominant-member warning) in the summary block beneath the plot, so the disclosures are
  on-screen, not only in logs.
- **`summed_bits` notice** (`:2003-2009`): keep firing when any member series is added;
  reword the copy to "summed all BITS index areas **per member**" (the cross-stock /
  cross-area double-count caveat is inherited from P1 and is now compounded across a set —
  the wording must reflect per-member summation).

## 5. Error handling (the "no silent loss" guarantee, made real)
- Every member that does not contribute — wrong class, `success==FALSE`, empty quarter,
  recruitment-only — produces an `excluded` sub-row **and** a `warning()` (never
  `message()`; production preserves no `message` logs). The group's trend is therefore
  always auditable down to which of its N members fed it.
- No false zeros: the constant-coverage rule (§4.2) sums only years every contributing
  member covers; partial-coverage years are dropped (NA semantics), never zero-filled —
  consistent with the age-filter phase's reshape guard.
- Group excluded only when **zero** members contribute, with a specific reason (§4.3).

## 6. Testing (TDD)
Unit tests in `tests/testthat/test-survey-validation.R`. **The existing
`.map_group_to_aphia` tests (`test-survey-validation.R:53-115`) are rewritten** to consume
the new frame shape (`m$aphia_id[1]` / `m$class[1]` / `nrow(m)`), not the old scalar list.

`.map_group_to_aphia`:
1. **Set return:** dictionary with 3 rows sharing key "Small demersals" (3 distinct
   demersal aphia) → 3 rows, all class "demersal".
2. **Distinct guard:** 2 rows "X"→126436 (same aphia, same key) → 1 row.
   (Note in the test: real synonyms are *distinct keys*, so this validates the `unique()`
   guard, not an existing-data scenario.)
3. **Mixed-set classification:** set {one demersal, one pelagic} → 2 rows, correct classes.
4. **Fallback suppressed:** key present but all aphia invalid (NA) → 1 row `aphia_id=NA,
   class="unmapped"`, and `wm_name2id` is NOT called (assert via a mock that would error).
5. **WoRMS fallback (no key):** name absent from dict, `wm_name2id` mocked to a valid
   demersal aphia → 1 row class "demersal"; mocked to fail → 1 row "unmapped".
6. **Single-species backward-compat:** 1-row dict group → 1 member row.

`.sum_member_series`:
7. **Constant coverage:** two members both covering 2019-2021 → summed per year.
8. **Partial coverage (the key science test):** member A covers 2016-2021, member B only
   2019-2021 → output is 2019-2021 only (intersection); 2016-2018 are dropped (NOT
   B-zero-filled); dropped years reported. Asserts a coverage change cannot create a step.
9. **Present-but-NA:** a member with an NA value in a year → that year treated as not
   covered (excluded from the intersection).
10. **All-empty:** all members 0-row → 0-row frame.

`.fetch_group_demersal_series` (with mocked `fetch_indices`):
11. **Two good members → summed series, no exclusions.**
12. **One member `success=FALSE` → that member excluded sub-row, the other still sums.**
13. **All members recruitment-only (all-NA) → series NULL, group excluded with the
    recruitment-only reason; each member appears as an excluded sub-row.**
14. **Zero demersal members (all pelagic) → series NULL, reason "no BITS-surveyed
    demersal members", each pelagic member an excluded sub-row.**
15. **Dominance:** two members where one is >70 % of the summed index → a dominance note
    + warning is emitted.

(The thin observer glue in `rpath_server.R` — dictionary read, clupeid dispatch, list
accumulation — is verified by parse check + a scope check that single-species and
single-clupeid groups still route as before; all real logic is in the three unit-tested
helpers.)

## 7. Scope / non-goals
- **Out of scope (later sub-phases):** editable mapping table (B), persistence (C),
  clupeid-*set* summing (a ≥2-clupeid set is excluded via "no BITS-surveyed demersal
  members" — accepted; a clupeid-aware reason is a future nicety), per-member biomass
  weighting (needs mean weights → the absolute-calibration phase), cross-group ordering.
- The dictionary is **author-curated**: this phase enables sets in the data model and the
  fetch/sum path; populating aggregate-group rows for specific MARBEFES models is a
  data-entry task (subject to the §2 invariant), not code.
- This sub-phase unlocks the parent spec §7 "many-species sets (summed)" item, carrying
  the parent's §3.5 "every drop → footnote row + warning" auditing down to member level.

## Appendix: reference sites
- `.map_group_to_aphia` + classification ladder + the three `SURVEY_TRENDS_*` constants
  — `R/functions/rpath/survey_validation.R:12-36` (constants) and `:210-238` (function).
  (NOTE: constants are in `survey_validation.R`, NOT `R/config.R`.)
- `aggregate_survey_series` (per-member per-year collapse, `na.rm=FALSE`) — `:47-66`
- `compute_survey_trends` (loops `unique(series_df$group)`, dedups years) — `:247+`
- Dictionary load — `R/modules/rpath_server.R:1885` (`.survey_trends_dict`)
- Survey-trends loop (map → clupeid branch → demersal fetch) — `R/modules/rpath_server.R:1936-1992`
- Single-species demersal three-way branch (the per-member template) — `:1970-1992`
- Clupeid routing keyed on `SURVEY_TRENDS_CLUPEID_SAG` — `:1946-1966`
- `summed_bits` area-sum notice — `:2002-2010`; status/summary/exclusions/mapping outputs — `:2022-2121`
- Dictionary CSV — `R/config/ewe_group_dictionary.csv` (schema `group, aphia_id`; all 32 keys currently unique)
