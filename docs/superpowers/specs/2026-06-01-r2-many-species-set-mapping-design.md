# R2 — Many-species set mapping for survey trends (design)

**Date:** 2026-06-01 (rev. after two rounds of multi-angle review)
**Status:** approved (brainstorm) → ready for plan
**Scope:** Sub-phase **A** of the R2 §7 "Editable + many-species mapping + persistence"
deferred phase. Sub-phases **B** (editable table) and **C** (persistence) are deferred to
their own cycles.

## 1. Problem

A Survey Trends group is auto-mapped to exactly one AphiaID
(`.map_group_to_aphia()` requires `length(hit) == 1`). Real Ecopath groups are often
*aggregates* ("Small demersals", "Flatfish") that should map to a **set** of species.
Today such a group resolves to a single arbitrary species or is silently excluded as
"unmapped" — so aggregate groups get no survey trend even when member species are well
surveyed by BITS. This phase lets a group resolve to a set and produces one honest,
member-auditable trend for it.

## 2. Goal & decisions

A group resolves to a **set of distinct AphiaIDs**; its demersal, BITS-indexed members are
fetched (at `min_age = 2L`), each aggregated to its own per-year series, then **combined
by averaging each member's mean-normalised trend** into the group trend. Every member that
does not contribute is surfaced as an excluded sub-member. Single-species and
single-clupeid groups behave exactly as today.

### Combination rule — mean of normalised member trends (key decision)
The downstream `compute_survey_trends()` already divides every group series by its own
window-mean (`rel <- s$survey_value / m`, `survey_validation.R:276`), so only the **shape**
of the combined series is ever shown. Therefore the group series is built as the **mean of
each member's mean-normalised series over the union of years**, NOT a raw-numbers sum:

- For each contributing demersal member series `(year, survey_value)`, normalise:
  `n_i(year) = survey_value(year) / mean(survey_value)`.
- For each year in the **union** of members' years, the group value is the average of
  `n_i(year)` over the members that have a finite value that year.

Why this and not a raw-numbers sum (rejected after review):
- **Dominance-proof by construction.** Each member is weighted equally on its own relative
  scale, so the most *numerous* species cannot silently become the group trend. (A raw sum
  is dominated by the most abundant member; abundance ≠ biomass, so that domination is not
  even a valid biomass proxy. Biomass-weighting needs mean weights → the deferred
  absolute-calibration phase.)
- **No raw-magnitude coverage step.** A member whose BITS coverage starts/ends mid-window is
  centred on ~1.0, so its appearance/disappearance cannot inject a large absolute-level step
  the way a raw-numbers sum does — and **no years are dropped** (full union retained),
  removing any need for an intersection/window-shrinking rule.
- **Matches the parent R2 stance** ("remove the confound, don't caveat it";
  `2026-05-30-...-design.md` §1.1).

Residual artifacts (accepted, second-order, both bounded to relative-scale O(1) and far
smaller than a raw-level step): (a) when the *count* of averaged members changes at a year
boundary, the group value can shift slightly (by ≈ half the relative gap between the
entering member and the incumbents), since the per-year mean is over the members present;
(b) a partial-coverage member is normalised by its within-window mean, not its true
long-run mean. Eliminating (a) entirely would require an intersection (constant basket),
which shrinks the window — the worse trade. These residuals are disclosed via the
`survey_trends_summary` note (§4.5), not hidden.

### Other decisions
- **Reuse the existing flat dictionary, no schema change.** A group maps to the distinct
  `aphia_id`s across all rows of `R/config/ewe_group_dictionary.csv` whose `group` cell
  **exactly equals** the Ecopath group label (raw `==`, no trimming — a stated contract;
  aggregate rows MUST match the label byte-for-byte).
  - **Invariant (document in the CSV header/README):** a *repeated* `group` key = aggregate
    members; a *unique* key = a single species. Today every key is unique (synonyms like
    "Cod"/"Atlantic cod"/"Gadus morhua" are **distinct keys**), so the feature is purely
    additive. Adding an aggregate means ≥2 rows sharing one key; an author must grep for an
    existing row with that key first (a pre-existing synonym row would become a set member).
- **Single contributing member → raw passthrough.** If exactly one demersal member
  contributes, its raw series is used unchanged (byte-exact backward-compat with today's
  single-species trend). Normalise-and-average applies only when ≥2 members contribute.
- **Demersal-only.** Clupeid/pelagic/not_surveyed/unmapped members are excluded sub-members;
  BITS-demersal and BIAS-clupeid are never merged.
- **No silent member drops.** Every non-contributing member (wrong class, fetch failure,
  empty quarter, recruitment-only, non-positive index) gets an excluded sub-row +
  `warning()`. Group excluded only when **zero** members contribute.
- **Backward-compat:** a set of exactly one clupeid keeps the existing BIAS/SAG routing.

## 3. Architecture

```
ewe_group_dictionary.csv (group, aphia_id; aggregate = ≥2 rows sharing the key)
        │
        ▼
.map_group_to_aphia(group, dictionary, pelagic_set, bits_indexed)
        │  -> data.frame(aphia_id, class)  ONE ROW PER MEMBER (via shared classify_aphia)
        ▼
survey-trends loop (rpath_server.R), per group:
  • mapping_rows  <- one row per (group, member, class)
  • if nrow(members)==1 AND that aphia ∈ SURVEY_TRENDS_CLUPEID_SAG
        -> existing select_clupeid_series() path (UNCHANGED), next
  • else -> res <- .fetch_group_demersal_series(members, quarter, window_years, ref_year, g)
            res$series   : df(group, year, survey_value, is_ref_year) or NULL
            res$excluded : df(group, reason)  one row per non-contributing member
            res$notes    : character (e.g. "trend = mean of 3 normalised member series")
        │
        ▼
compute_survey_trends -> plot  (UNCHANGED: one series per group; it re-normalises by mean).
```

`.fetch_group_demersal_series` (fetch + per-member branch + combine + exclusions) and
`.combine_member_trends` (the normalise-then-average science core) are **pure, mockable
helpers**, unit-tested directly. The observer is thin glue.

## 4. Component changes (exact sites)

### 4.1 `.map_group_to_aphia()` → member frame — `R/functions/rpath/survey_validation.R:210`
- Return changes from `list(aphia_id, class)` to **`data.frame(aphia_id = numeric, class =
  character, stringsAsFactors = FALSE)` — one row per member.** Update the `@return` roxygen
  (`:207`).
- **Extract the existing class ladder (`:226-234`) into `classify_aphia(aid, pelagic_set,
  bits_indexed)`** (`NA→unmapped`; `aid ∈ pelagic_set → pelagic`; `bits_indexed` non-NULL &
  `aid ∉ bits_indexed → not_surveyed`; else `demersal`). BOTH the dictionary and WoRMS
  branches — and the all-invalid `NA` case — funnel every aid through this one function, so
  classification is identical everywhere (the `unmapped` row is `classify_aphia(NA, …)`, not
  a hard-coded string).
- **Dictionary-first, fallback suppressed:**
  `rows <- dictionary$aphia_id[dictionary$group == group_name]`.
  - `length(rows) >= 1` (key present): `aids <- unique(rows[!is.na(rows) & rows > 0])`. The
    WoRMS fallback is **suppressed whenever any rows match the key** (even if all are
    invalid). `length(aids) >= 1` → one classified row per aid; else → one
    `aphia_id = NA, class = classify_aphia(NA,…) = "unmapped"` row.
  - `length(rows) == 0` (no key): `wm_name2id(group_name)` exactly as today (scalar guard,
    `warning()` on failure) → one classified member row (or `NA → "unmapped"`).
- Empty/whitespace group-name guard retained.

### 4.2 New helper `.combine_member_trends()` — `R/functions/rpath/survey_validation.R`
- `.combine_member_trends(member_series_list)` — each element is a `data.frame(year,
  survey_value)` (one contributing member's `aggregate_survey_series` output, mean > 0
  guaranteed by the caller; see §4.3). Returns `data.frame(year, survey_value)` (canonical
  empty form `data.frame(year = integer(0), survey_value = numeric(0))`).
- For each member, `n_i <- survey_value / mean(survey_value, na.rm = TRUE)` over its finite
  years (a year with `NA`/absent `survey_value` is simply not present for that member).
- Output years = **union** of members' finite years. For each year, `survey_value <-`
  `mean(n_i(year) for members present that year)`. No year is dropped; absent members do
  not contribute to that year's mean (they are not averaged in as 0).
- Pure: no fetch, no I/O. (Caller guarantees every input member has `mean > 0` and ≥1 finite
  year, so no divide-by-zero here.)

### 4.3 New helper `.fetch_group_demersal_series()` — `R/functions/rpath/survey_validation.R`
- Signature: `.fetch_group_demersal_series(members, quarter, window_years, ref_year,
  group_label, fetch_indices = lookup_datras_indices, aggregate = aggregate_survey_series)`
  — `fetch_indices`/`aggregate` injectable for tests (no live network).
- Returns `list(series = <df(group, year, survey_value, is_ref_year)> or NULL,
  excluded = <df(group, reason)>, notes = <character(0+)>)`.
- Logic:
  1. Split `members` by class. Each member with `class != "demersal"` → an `excluded`
     sub-row keyed `sprintf("%s [member %s]", group_label, aphia)`, reason = its class.
  2. For each demersal member: `res <- fetch_indices(aphia, surveys = "BITS",
     years = window_years, min_age = 2L)`; `ser <- aggregate(res$data, quarter = quarter,
     areas = NULL)` (same `quarter`/`areas = NULL` as the single-species path — mandatory).
     Branch:
     - `res$success == FALSE` → excluded sub-row reason "fetch failed" + `warning()`.
     - `nrow(ser) == 0` → excluded sub-row reason "no data for selected quarter".
     - `all(is.na(ser$survey_value))` → excluded sub-row reason "no age-2+ survey index
       (recruitment-only)".
     - `mean(ser$survey_value, na.rm = TRUE) <= 0` or non-finite → excluded sub-row reason
       "no positive survey index" (guards the normalise divide-by-zero).
     - else → a **contributing** member series.
  3. Build the group series from the contributing list:
     - 0 contributing & ≥1 demersal member existed → `series = NULL`, group reason "no
       usable age-2+ index for any member".
     - 0 demersal members at all → `series = NULL`, group reason "no BITS-surveyed demersal
       members" (covers all-pelagic / all-clupeid sets).
     - **exactly 1 contributing** → `grp <- that raw series` (passthrough; backward-compat).
     - **≥2 contributing** → `grp <- .combine_member_trends(list)`; `notes <- sprintf(
       "group trend = mean of %d normalised member series", k)`.
     - For a non-NULL `grp`: `grp$group <- paste0(group_label, " [BITS 2+]")`;
       `grp$is_ref_year <- grp$year == ref_year`; `series <- grp`.
- Closures `warning()` before returning; prefer plain returns over `<<-` in this pure helper.

### 4.4 Survey-trends loop — `R/modules/rpath_server.R` (replace the per-group loop **body** only)
- The loop body that currently runs per group (the clupeid branch + the single-species
  demersal three-way branch, roughly `:1936-2004`, ending at the loop's closing `}` on
  `:2005`) is replaced. **Everything after the loop is preserved:** the
  `mapping_df`/`series_df` assembly (`:2007-2011`), the `summed_bits` area-sum
  `warning()`+`showNotification()` (`:2013-2021`), and `compute_survey_trends` +
  `trends$excluded`/`$mapping`/`$ref_year` (`:2023+`).
- New body per group:
  - `m <- .map_group_to_aphia(...)`; append **one `mapping_rows` row per member** (`rbind`).
  - Single-clupeid: replace the scalar condition at `:1946-1947` with
    `if (nrow(m) == 1 && as.character(m$aphia_id[1]) %in% names(SURVEY_TRENDS_CLUPEID_SAG))`;
    the existing body `:1948-1966` (`select_clupeid_series`, `sel$series$group` tag, `next`)
    is reused **verbatim** inside the new guard.
  - Else: `res <- .fetch_group_demersal_series(m, quarter, window_years, ref_year, g)`.
    `rbind` `res$excluded` into `excluded_map`; collect `res$notes`. If `res$series` non-NULL
    → append to `series_list`, `summed_bits <- TRUE`. If NULL → add the group-level exclusion
    from `res$excluded`'s group reason (do **not** also add a duplicate group row when
    member sub-rows already explain the drop — avoid double counting in the exclusions count).
- Reword the preserved `summed_bits` notice copy from "per species" to **"per member"** (the
  text only; keep the `warning()`+`showNotification` in place). Store notes for the UI:
  `trends$notes <- unlist(notes_list)` alongside `trends$mapping` (`:2023+`).

### 4.5 UI surfacing — `R/modules/rpath_server.R`
- **Mapping table** (`output$survey_trends_mapping`, ~`:2104`): renders one row per member
  automatically; add caption "one row per mapped species; aggregate groups list each member."
- **Exclusions footnote** (`output$survey_trends_exclusions`, ~`:2111`): now includes
  per-member sub-rows, so its `Excluded (N)` counts **species/members, not groups** — say so
  in the caption. (No double count: §4.4 suppresses the redundant group-level row when member
  sub-rows exist.)
- **Combine note** (`output$survey_trends_summary`, ~`:2067-2102`): render `res$notes`
  (stored on `trends$notes`) as an added `tags$p`/`tags$ul` inside the existing summary
  `tags$div`, so "group trend = mean of N normalised member series" is on-screen, telling the
  user the group line is an *average member trajectory*, not a single species.

## 5. Error handling (the "no silent loss" guarantee, made real)
- Every non-contributing member produces an `excluded` sub-row **and** a `warning()` (never
  `message()`). A group trend is always auditable down to which of its N members fed it.
- No false zeros / no spurious steps: §4.2 averages members present per year on a relative
  scale; absent members are not zero-filled, and no years are dropped.
- The normalise divide-by-zero is impossible: §4.3 excludes any member with mean ≤ 0 before
  `.combine_member_trends` runs.
- Group excluded only when zero members contribute, with a specific reason.

## 6. Testing (TDD)
In `tests/testthat/test-survey-validation.R`. **The two existing `.map_group_to_aphia`
`test_that` blocks (`:53-115`) are rewritten** to the frame shape (`m$aphia_id[1]` /
`m$class[1]` / `nrow(m)`), and must retain their existing class-ladder coverage
(`not_surveyed`, and `bits_indexed = NULL → demersal`). All mocking uses
`testthat::local_mocked_bindings(..., .package = "worrms")` for `wm_name2id` and the
`fetch_indices=`/`aggregate=` injection params for `.fetch_group_demersal_series` — **no test
hits the network.**

`.map_group_to_aphia`:
1. **Set:** 3 rows sharing key "Small demersals" (3 distinct demersal aphia) → 3 rows, class "demersal".
2. **Distinct guard:** 2 rows "X"→126436 → 1 row. (Comment: real synonyms are distinct keys.)
3. **Mixed-set classification:** {demersal, pelagic} → 2 rows, correct classes; plus a
   `not_surveyed` case and a `bits_indexed = NULL → demersal` case (retain old coverage).
4. **Fallback suppressed:** key present, all aphia NA → 1 row "unmapped"; `wm_name2id` mock
   set to `stop("must not be called")` to assert non-invocation.
5. **WoRMS fallback (no key):** mock `wm_name2id` → valid demersal aphia → 1 row "demersal";
   mock → failure → 1 row "unmapped".
6. **Single-species backward-compat:** 1-row dict group → 1 member row.

`.combine_member_trends` (the science core):
7. **Equal weight:** two members both 2019-2021 → per-year mean of the two normalised series.
8. **Coverage change retains all years and stays on the relative scale (key test):**
   member A 2016-2021, member B 2019-2021 → output spans 2016-2021 (union, no years lost);
   2016-2018 == A's own normalised values (B not averaged in); 2019-2021 == the per-year
   mean of A's and B's normalised values (NOT a magnitude sum — assert each overlap value
   equals `mean(nA, nB)` and lies on the O(1) relative scale, demonstrating no
   raw-magnitude step from B entering).
9. **Present-but-NA / absent year:** a member with an NA year contributes nothing for that
   year (not 0), and that year still appears if another member covers it.
10. **All-empty:** empty list / all 0-row members → 0-row frame.

`.fetch_group_demersal_series` (mocked `fetch_indices`/`aggregate`):
11. **Two good members → combined series (note "mean of 2 …"), no exclusions.**
12. **One member `success=FALSE` → that member excluded sub-row, the other passes through
    raw (1 contributing → passthrough, no normalise).**
13. **All members recruitment-only → `series = NULL`, group reason set, each member an
    excluded sub-row.**
14. **Zero demersal members (all pelagic) → `series = NULL`, reason "no BITS-surveyed
    demersal members", each member an excluded sub-row.**
15. **Non-positive member (mean 0) → excluded "no positive survey index"; remaining member
    used.**

(The thin observer glue — dictionary read, clupeid dispatch, list accumulation, preserved
`summed_bits` notice — is verified by parse check + a scope check that single-species and
single-clupeid groups route exactly as before; all real logic lives in the three unit-tested
helpers.)

## 7. Scope / non-goals
- **Out of scope (later):** editable mapping table (B), persistence (C), clupeid-*set*
  summing, **biomass-weighted** combination (needs mean weights → absolute-calibration phase),
  cross-group ordering. The mean-of-normalised primitive generalises cleanly to the deferred
  clupeid-set / weighted cases later.
- The dictionary is **author-curated** (subject to the §2 invariant); populating aggregate
  rows for specific models is data entry, not code.

## Appendix: reference sites (locate by content; line numbers approximate)
- `.map_group_to_aphia` + class ladder `:226-234` + constants `SURVEY_TRENDS_PELAGIC_APHIA`
  / `SURVEY_TRENDS_BITS_INDEXED_APHIA` / `SURVEY_TRENDS_CLUPEID_SAG` — all in
  `R/functions/rpath/survey_validation.R` (`:12-36` constants, `:210-238` function).
- `aggregate_survey_series` (per-member per-year collapse, `na.rm=FALSE`) — `:47-66`
- `compute_survey_trends` (loops `unique(series_df$group)`, re-normalises by mean `:276`) — `:247+`
- Dictionary load — `R/modules/rpath_server.R` `.survey_trends_dict` (~`:1885`)
- Survey-trends loop body to replace — `R/modules/rpath_server.R:1936-2004` (loop closes `:2005`)
- Clupeid branch (reuse body verbatim) — `:1946-1966`
- PRESERVE post-loop: assembly `:2007-2011`; `summed_bits` notice `:2013-2021`; trends
  assembly `:2023+` (add `trends$notes`)
- Status/summary/exclusions/mapping outputs — `:2024-2121`
- Dictionary CSV — `R/config/ewe_group_dictionary.csv` (schema `group, aphia_id`; all 32 keys currently unique)
