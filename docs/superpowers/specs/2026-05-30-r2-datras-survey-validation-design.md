# DATRAS Survey Trend Context for Ecopath Biomass — Design

**Date:** 2026-05-30
**Author:** A. Razinkovas-Baziukas (with Claude)
**Status:** Converged after THREE spec-review rounds (10 agent-reviews:
architecture / convention / domain, then test-design / silent-failure /
simplification / skeptic-usability, then domain-re-review / statistics /
type-design / implementation-readiness). Pending user review.
**Tracks:** v2-plan item **R2** (PR10 scope-L).

### Design history — why this is trend-only
Three rounds converged on one conclusion: **every cross-group biomass-vs-
abundance comparison is confounded and must be dropped.**
1. Re-scoped "biomass calibration" → relative validation (absolute needs `q`;
   §7).
2. Cross-group **biomass-share vs abundance-share** dropped: confounded by mean
   body weight `W` (`biomass ∝ N×W`, `survey ∝ N`); invites the wrong action.
3. Its replacement, a **rank-ordering / inversion check**, was then dropped too:
   the domain re-review showed it is the *same* `W`-confound discretised into a
   falsely-confident flag (a correct Baltic model shows cod high in biomass rank
   but low in survey *numbers* vs abundant small dab — a guaranteed "inversion"
   that is correct ecology), and the statistics review showed Spearman ρ is
   meaningless at the typical n=3–8 groups (perfect agreement is non-significant
   at n≤4).

The **only** comparison free of the `W` confound is a group compared to
**itself across years** — the per-group temporal trend (`W`, catchability `q`,
and selectivity cancel year-on-year). That is R2. "Honest but modest" beats
"confidently wrong."

---

## 1. Goal & non-goals

### Goal
For each demersal fish group in a balanced Ecopath model that maps to a
trawl-surveyed species, show the **multi-year DATRAS (BITS) survey abundance
trend** and where the model's snapshot year sits on it — so the modeller can see
whether a single-year `Biomass` input is anchored to an anomalous survey year
and, if so, **consider averaging biomass across years**. A read-only,
on-demand diagnostic in the rpath module. No cross-group comparison.

### 1.1 Why "context", not "calibration" or "verdict"
Absolute biomass needs catchability `q` (unreliable → §7). Cross-group structure
is `W`-confounded (design history). So R2 makes **no** absolute claim and **no**
pass/fail verdict; it shows each group's own survey history as context for one
modelling decision (single-year vs multi-year-averaged biomass input).

### 1.2 Gear caveat & clupeid banner (MARBEFES honesty)
BITS is a bottom-trawl survey; it under-samples pelagics — herring/sprat (the
*dominant* Baltic groups) are assessed from **BIAS acoustic**, not BITS, so their
BITS trends are gear artifacts. R2 therefore covers **demersal** fish only,
excludes a curated pelagic set (herring 126417, sprat 126425, …) by default, and
must **lead with a prominent banner**: *"Cannot yet validate herring/sprat — they
need BIAS acoustic data (planned, §7)."* A BIAS paste-in path is the named next
phase, not this spec.

### Non-goals (YAGNI / dropped by review)
- No absolute biomass; no `q`/swept-area/length-weight machinery (§7).
- **No cross-group comparison of any kind** — no shares, no ordering/rank check,
  no Spearman ρ. (All tried and dropped as `W`-confounded.)
- No automatic verdict; trend is context, the modeller judges.
- No non-demersal/non-fish/pelagic groups (shown as excluded).
- **No mapping persistence, no editable mapping, no many-species sets in P1** —
  single-species auto-map only; the rest is deferred (§7).
- No new survey-fetch logic — reuses `lookup_datras_indices()` (scalar
  `aphia_id`, explicit-`years` multi-year path) and `worrms::wm_name2id()`.

## 2. Architecture & placement
- **Tabs render server-side.** `R/ui/rpath_ui.R` is a shell
  (`uiOutput(ns("rpath_content"))`); real tabs are in `R/modules/rpath_server.R`
  inside `output$rpath_content <- renderUI({ tabBox(tabPanel(...)) })` (~:85). The
  new **"Survey Trends"** `tabPanel` goes **inside that `tabBox` in
  `rpath_server.R`**, not in `rpath_ui.R`.
- rpath **is namespaced** (`ns()`/`moduleServer`) — all new IDs use `ns(...)`.
- Reads the balanced model from `rpath_values$params$model`
  (`Group`, `Type`, `Biomass`).
- **Pure core:** `R/functions/rpath/survey_validation.R` holds
  `compute_survey_trends()` + `aggregate_survey_series()` — unit-testable without
  Shiny/network. TDD target.
- **Read-only:** never writes back to `rpath_values$params$model`.

## 3. Components

### 3.1 Group → species mapping (single-species auto-map; P1)
For each **living group** (`model$Type %in% c(0, 1)`; excludes detritus
`Type==2`, fleets `Type==3`):
- **Seed from a small shipped dictionary** of the actual MARBEFES Baltic EwE
  group names → a **single** AphiaID (stored as a CSV/inline table; contents
  enumerated from the model files in `examples/` before build — a bounded ~15–20
  rows, §8). Covers functional names ("Cod"→126436) that `wm_name2id` misses.
- **Fallback:** `tryCatch(worrms::wm_name2id(Group), error=function(e){warning();
  NA})` with the R3 scalar guard (`length==1 && !is.na && >0`; multi-match → NA).
- **Class:** AphiaID in the curated pelagic set → pelagic, **excluded**; a group
  that resolves to no single species (aggregates) → **unmapped, excluded**.
- **Read-only mapping table** (`Group | species | AphiaID | class | included`)
  shows what auto-map decided; no editing in P1 (deferred §7). The exclusions are
  shown, never silent (§3.5).
- Session-only; re-seeded on mass-balance completion
  (`observeEvent(input$btn_run_ecopath …)` success). No persistence in P1.

### 3.2 Per-group survey fetch & aggregation
- **One region-survey** (dropdown, default **BITS** for the Baltic).
- **Multi-year window fetched once per group** (scalar AphiaID):
  `lookup_datras_indices(aphia_id, surveys = survey, years = window)` (explicit-
  `years` path → all years, no step-back).
- **`aggregate_survey_series(reshaped_df, quarter, areas)`** (explicit, testable
  signature): keep the pinned **quarter** (default Q1 for BITS; selectable),
  keep only `index_area`s in `areas`, sum `abundance_index` per `year`. Returns
  `data.frame(year, survey_value)` for the supplied series — **not** group-keyed;
  the caller stamps `group` and `rbind`s into the group×year frame. `na.rm=FALSE`
  on the per-area sum; a partial-NA (an `index_area` missing for a year) →
  `warning()`, never NA→0. (Note: age-level NA is already collapsed to 0 upstream
  by `.datras_reshape_indices`; this guard catches *area*-level gaps.)
- **Area selection is a confirmed choice.** Defaulting to all of a survey's areas
  can double-count separate stocks (E vs W Baltic cod). When a run uses the
  **unconfirmed default** area set → `warning("[survey_trends] area selection not
  confirmed; summed all <n> areas")` + a visible UI notice; the footnote lists
  exactly which areas were summed.
- **Known caveat (deferred fix):** `abundance_index` sums all ages incl. age-0/1,
  so a strong recruiting year-class adds inter-annual noise to the trend. Visible
  in the plot; age-filtering to adult/exploitable ages is a §7 follow-up.

### 3.3 The trend (pure)
`compute_survey_trends(series_df)` where `series_df` has `group`, `year`,
`survey_value`, and a logical `is_ref_year` (per-group, so the no-common-year
fallback in §3.4 is expressible without a scalar `ref_year`).
- Per group: a **relative series** `survey_value / mean(survey_value over the
  group's years)` for plotting on a common axis.
- **Reference-year position as a rank-of-n, not a continuous percentile** (the
  data is too coarse for a percentile): e.g. "ref year is the 2nd-lowest of 6
  survey years." Honest about granularity (stats review).
- **Direction** glyph `↑/↓/≈` only when the group has **≥5** years, from
  **non-overlapping** windows (`mean(last 3 yr)` vs `mean(earlier yr)`) with an
  explicit dead-band (`|Δ|/mean < 0.10 → ≈`) so it's deterministic. Below 5
  years: plot only, no glyph.
- Returns a structured list (codebase convention):
  `list(success, status, trends = df(group, rel_series-by-year, ref_rank,
  n_years, direction), excluded = df(group, reason))`.
- **Guards (visible, never silent NaN/Inf):** a group whose window-mean is
  `0`/`NA` (all-zero/all-NA series) → **excluded + reason**, not normalised to
  `NaN`. `< 3` years → omitted + reason ("trend omitted: <n> survey years").
  Duplicate `(group, year)` rows (area leak) → de-duped with a `warning()`.
  `< 1` group with a usable trend → `status = "insufficient"`.

### 3.4 Reference year & window
- **Window:** fixed trailing **10 years** ending at the most recent year with
  **complete** indices (labelled; a provisional latest year is not authoritative).
  Not user-selectable (YAGNI).
- **Reference year (model snapshot):** most recent year common to the mapped
  demersal groups; user-selectable from common years. No common year → per-group
  fallback (`is_ref_year` set on each group's own most-recent year) with
  `warning("[survey_trends] reference years differ across groups: …")` + UI notice.

### 3.5 Outputs
- **Clupeid banner** (§1.2) at top — always visible.
- **Trend panel:** per-group relative series plotted (faceted or overlaid),
  ref-year marked, with the ref-year **rank-of-n** label and the `↑/↓/≈` glyph
  (where ≥5 yr). One actionable line per group: "ref year is the lowest of 7 —
  consider averaging biomass across years."
- **Mapping table** (read-only, §3.1).
- **Exclusions footnote:** counts + names + **reason** for every excluded group
  (unmapped, pelagic, non-fish, no-survey-data, all-zero/NA, <3-yr) so coverage
  is fully auditable.

## 4. Data flow
`rpath_values$params$model` → filter `Type %in% c(0,1)` → single-species auto-map
(dictionary + `wm_name2id`; demersal-only) → per mapped group:
`lookup_datras_indices(aphia_id, survey, years=window)` (once) →
`aggregate_survey_series(quarter, areas)` → stamp `group`, `rbind` →
`compute_survey_trends()` → render. "Run validation" button; per-group loop under
`withProgress` honouring `timeout`. Validate mapped-group count **early** (before
the fetch), so the user isn't made to wait only to hit "insufficient".

## 5. Error handling
- Inside `error=function(e)` closures: **`<<-`** to mutate outer state;
  **`warning()` not `message()`** (production keeps no `message()` logs).
- **Every drop → an exclusions-footnote row + `warning()`** — never footnote-only,
  never a silent `NaN`/`Inf`/`0`, never `message()`: unmappable, multi-match→NA,
  pelagic, no-survey-data, all-zero/NA series, <3-yr, area-leak dup.
- **No balanced model** → "Balance an Ecopath model first." (benign UI state, not
  a `warning()`); **`worrms`/`icesDatras` missing** → notice, no crash (benign).
- Never `stop()`.

## 6. Testing
Conventions: never gate `expect_*` behind `if`; `skip_if` with reasons; live
behind `RUN_LIVE_TESTS`.
- **`compute_survey_trends()` (TDD core, no network):** rel-series normalised to
  mean 1; **ref-year rank-of-n** exact (ref = lowest → rank 1 of n; highest →
  n of n; absent from a group's years → reported, not crash); `↑/↓/≈` with the
  dead-band on rising/falling/flat ≥5-yr series, and **no glyph** below 5 yr;
  **all-NA / all-zero series → excluded + reason, NOT `NaN`/`Inf`**; `<3` yr →
  omitted + reason; duplicate `(group,year)` → de-duped + warning; `<1` usable
  group → `status="insufficient"`. Assert the return schema via
  `expect_setequal(names(...), …)` (proves the verdict-free contract).
- **`aggregate_survey_series()`:** synthetic multi-area/multi-quarter frame —
  `quarter=1` drops Q4 before summing; `areas="BS_CodEast"` sums only East,
  `areas=c(East,West)` sums both (the stock double-count guard); partial-NA area
  sum → `warning()`, not NA→0; output is `df(year, survey_value)`, not
  group-keyed.
- **Mapping helper:** dictionary hit → the AphiaID; `wm_name2id` multi-match → NA;
  `aid<=0`/`NA` → NA; pelagic AphiaID → class "pelagic" (excluded).
- **Idiom trap (record):** the group-loop test mocks **our own**
  `lookup_datras_indices` via `local_mocked_bindings(lookup_datras_indices=…)`
  with **no `.package`** (it is not an `icesDatras` binding).
- **Gated integration test (add — nearly free):** `skip_if_no_live_tests()`;
  `wm_name2id("Gadus morhua")` → `lookup_datras_indices(survey="BITS",
  years=<window>)` → `aggregate_survey_series()` → assert a non-empty per-year
  series. Closes the `wm_name2id → fetch → aggregate` seam unit mocks can't.

## 7. Deferred / gated future phases (explicitly out of P1)
- **Editable + many-species mapping + persistence.** Aggregate groups → a SET of
  AphiaIDs (summed), an editable table, and a per-group-keyed persisted store
  (`cache/survey_mappings/`, left-join-by-group on reload so iterative balancing
  doesn't re-map). Each is its own task; the type-design/buildability reviews
  flagged real infra in all three — deferred to keep P1 shippable.
- **Cross-group ordering within a size class.** The only defensible cross-group
  check needs coarse per-group mean-weight binning (small/med/large) so `W`
  roughly cancels within a bin. Weak length-weight dependency; revisit only if a
  real need appears.
- **Clupeid coverage via BIAS acoustic** — a read-only pull or user paste-in for
  the dominant Baltic pelagics. The §1.2 banner is the placeholder.
- **Age-filtered abundance** (drop age-0/1) to remove recruitment-pulse noise from
  the trend — needs per-age access beyond the current `abundance_index` sum.
- **Absolute biomass calibration** — opt-in, user-supplied `q` +
  swept-area/stratified weighting + length-weight, all assumptions shown.

## 8. Open items (resolve at implementation)
- **Dictionary contents (bounded spike, do first):** enumerate the functional
  group names in the actual MARBEFES Baltic EwE files in `examples/`; that list
  *is* the dictionary (~15–20 rows). If the user can't enumerate them, the
  dictionary is unbounded → ship `wm_name2id`-only auto-map and let exclusions
  show the gaps.
- Curated pelagic set (herring/sprat + any others to exclude) — confirm with user.
- Region-survey default (BITS); whether the dropdown offers all three; whether
  non-Baltic models are in scope for v1.
- `lookup_datras_indices` cache keys differ between R2 (single-survey) and R3
  (three-survey default) → no cross-panel cache sharing (accepted; note).

## Appendix: reference patterns
- **Model object & namespaced tabs:** `R/modules/rpath_server.R`
  (`output$rpath_content`/`tabBox` ~:85; `rpath_values$params$model` ~:694;
  `Group`/`Type`/`Biomass` ~:711-713; `btn_run_ecopath`; DT-editable +
  `_cell_edit` pattern ~:832 — for the *deferred* editable phase).
- **Survey fetch + conventions (scalar `aphia_id`, explicit-`years` path):**
  `R/functions/ices_lookups.R` (`lookup_datras_indices` ~:166-303;
  `.datras_reshape_indices` output `survey/year/quarter/index_area/
  abundance_index` ~:179-186 — note: no species column, so a future many-species
  sum must tag per-species frames before aggregation). Test idioms in
  `tests/testthat/test-ices-lookups.R` (reshape ~:38-70; mock ~:84-99; live-gate
  ~:195-221; `expect_setequal` ~:170/:180).
- **`wm_name2id` tryCatch + scalar guard:** `R/modules/trait_research_server.R`
  (R3 `datras_fetch` observer).
- **Error-closure (`warning()`, `<<-`) / test conventions:** project `CLAUDE.md`;
  R1/R3 specs.
