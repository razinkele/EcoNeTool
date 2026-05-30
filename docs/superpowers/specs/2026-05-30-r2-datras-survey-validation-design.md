# DATRAS Survey Validation of Ecopath Biomass — Design

**Date:** 2026-05-30
**Author:** A. Razinkovas-Baziukas (with Claude)
**Status:** Revised after TWO 3/4-agent spec-review rounds
(architecture / convention / domain, then test-design / silent-failure /
simplification / skeptic-usability). Pending user review.
**Tracks:** v2-plan item **R2** (PR10 scope-L).

### Design history (why the shape changed twice)
1. Re-scoped "biomass calibration" → relative **validation** (absolute needs
   catchability `q`; deferred — §7).
2. Round 1 (domain): bottom-trawl BITS under-samples pelagics → **demersal
   only**; per-group **trend** chosen over cross-group shares.
3. Round 2 (skeptic/usability + simplification): a per-group trend is honest
   but **not actionable** (Ecopath's reference year is fixed by data, not
   chosen), and cross-group **shares invite the wrong action**. The one
   *actionable, confound-robust* diagnostic is a **demersal biomass-ordering /
   rank-inversion check** → that is now the **primary** view; trend is demoted
   to context; the shares view is **dropped**. Mapping is made usable
   (dictionary-seeded, many-species, persisted).

---

## 1. Goal & non-goals

### Goal
Help a modeller spot **implausible relative biomass inputs** in a balanced
Ecopath model by comparing the model's demersal-fish biomass **ordering** with
the ICES DATRAS (BITS) survey's abundance ordering, flagging rank
**inversions** that warrant checking an input. A read-only diagnostic in the
rpath module. Per-group survey **trends** provide supporting context (is the
model's snapshot year a survey outlier?).

### 1.1 Why "validation", not "calibration"
Absolute biomass needs `density = catch/(swept_area × q)`; trawl `q` is small,
uncertain, gear/size/species-specific. Seeding Ecopath `Biomass` from an assumed
`q` injects false precision. R2 uses the survey only for **relative** signals;
absolute conversion is a gated future phase (§7).

### 1.2 Currency caveat (biomass vs numbers)
Model `biomass ∝ N × W`; survey `abundance ∝ N`. A **share/level** comparison is
confounded by body weight `W`. **Rank order**, however, is far more robust:
among demersal groups of broadly similar size, the order of biomass and the
order of abundance largely agree, so a *disagreement* is informative. The
ordering check (§3.3) still carries a caveat that an inversion between very
different-sized groups may be size-driven, not an error.

### 1.3 Gear caveat (bottom trawl under-samples pelagics)
BITS is a bottom-trawl survey; herring/sprat are semi-pelagic and BITS
mis-samples them (ICES assesses Baltic clupeids from **BIAS acoustic**). Their
BITS indices are gear artifacts. So R2 restricts comparison to the **demersal
assemblage** (cod, flounder, plaice, dab, brill, turbot, …) and **excludes
known pelagics by default** (curated AphiaID set incl. herring 126417, sprat
126425; user re-include shows a "BITS under-samples pelagics; prefer BIAS"
warning).

### 1.4 Clupeid coverage banner (MARBEFES use-case honesty)
Baltic MARBEFES models are dominated by sprat/herring, which this panel
**cannot** validate from BITS. The panel must **lead with a prominent banner**
saying so: *"Cannot yet validate herring/sprat — they require BIAS acoustic
data (planned, §7)."* Not buried in a footnote. A lightweight BIAS
paste-in/manual-entry path is the **immediate next phase** (§7), not this spec.

### Non-goals (YAGNI)
- No absolute biomass; no `q`/swept-area/length-weight machinery.
- No automatic pass/fail verdict — inversions and trends are **prompts to
  check**, not errors.
- No non-demersal/non-fish/pelagic groups in the comparison (shown as excluded).
- **No cross-group biomass-share vs abundance-share view** (dropped in round 2 —
  confounded and invites the wrong action).
- No new survey-fetch logic — reuses `lookup_datras_indices()` (incl. its
  explicit-`years` multi-year path) and `worrms::wm_name2id()`.

## 2. Architecture & placement
- **Tabs render server-side.** `R/ui/rpath_ui.R` is a shell
  (`uiOutput(ns("rpath_content"))`); the real tabs live in
  `R/modules/rpath_server.R` inside `output$rpath_content <- renderUI({ tabBox(
  tabPanel(...) ) })` (~:85). The new **"Survey Validation"** `tabPanel` goes
  **inside that `tabBox` in `rpath_server.R`**, not in `rpath_ui.R`.
- rpath **is namespaced** (`ns()` / `moduleServer`) — all new IDs use `ns(...)`.
- Reads the balanced model from `rpath_values$params$model`
  (`Group`, `Type`, `Biomass`).
- **Pure core:** `R/functions/rpath/survey_validation.R` holds
  `compute_ordering_check()`, `compute_survey_trends()`, and the aggregation /
  mapping helpers — unit-testable without Shiny/network. TDD target.
- **Read-only:** never writes back to `rpath_values$params$model`.

## 3. Components

### 3.1 Group → species mapping (dictionary-seeded, many-species, persisted)
For each **living group** (`model$Type %in% c(0, 1)`; excludes detritus
`Type == 2` and fleets `Type == 3`):
- **Seed from a shipped dictionary** of common Baltic EwE group names →
  AphiaID(s) (`R/config/` data; e.g. "Cod"→126436, "Demersal fish"→{cod,
  flounder, plaice, dab,…}). This is what makes auto-map actually work on
  *functional* names, not just Linnaean ones.
- **Fallback auto-suggest:** `tryCatch(worrms::wm_name2id(Group), error =
  function(e){ warning(...); NA_real_ })` with R3's scalar guard
  (`length == 1 && !is.na && > 0`; **multi-match → NA**, never silent `[1]`).
- **Many-species groups map to a SET of AphiaIDs** whose indices are summed
  (§3.2) — the demersal-assemblage sum machinery already exists. Aggregate
  groups become *covered*, not "unmapped".
- **Demersal/pelagic class** from the curated set (§1.3); pelagics excluded by
  default.
- **Editable** mapping table (`Group | species/set | AphiaID(s) | class |
  include`) for overrides and set assignment; match the rpath `group_params`
  editable-table widget.
- **Persisted** per model (e.g. keyed by the group-name signature) so a modeller
  re-running during balancing does **not** re-map each time. (Drops the earlier
  session-only non-goal — persistence is the highest-impact usability fix.)
- Re-seed only-missing entries on mass-balance completion
  (`observeEvent(input$btn_run_ecopath …)` success); never clobber user edits.

### 3.2 Per-group survey fetch & aggregation
- **One region-survey** (dropdown, default **BITS** for the Baltic).
- **Multi-year window fetched once:** `lookup_datras_indices(aphia_ids, survey,
  years = window)` (explicit-`years` path → all years, no step-back). Feeds both
  §3.3 and §3.4; reference year filtered downstream, never refetched.
- **`aggregate_survey_series(reshaped_df, quarter, areas, aphia_ids)`** (explicit
  signature so it is testable): filter to the pinned **quarter** (default Q1 for
  BITS; selectable), keep only `index_area`s in `areas`, sum `abundance_index`
  per `year` across the area set and the group's `aphia_ids` (many-species sum)
  → one `survey_value` per group per year. `na.rm = FALSE` on the per-area sum;
  a partially-NA sum emits `warning()` rather than coercing NA→0.
- **Area selection is a confirmed choice, not a silent default.** Defaulting to
  all of a survey's areas can double-count separate stocks (E vs W Baltic cod).
  When a run uses the **unconfirmed default** area set, emit `warning(
  "[survey_validation] area selection not confirmed; summed all <n> areas")` AND
  a visible UI notice; the footnote lists exactly which areas were summed.

### 3.3 PRIMARY — demersal ordering / inversion check (pure)
`compute_ordering_check(groups_df)` where `groups_df` has `group`, `biomass`,
`survey_value` for the **demersal** mapped groups at the reference year.
- Model rank = rank of `biomass`; survey rank = rank of `survey_value`.
- **Inversions:** pairs `(a, b)` where the model and survey disagree on order
  (model says `B_a > B_b` but survey says `N_a < N_b`). Each inversion is a
  prompt: "model ranks <a> above <b>; survey disagrees — check inputs."
- **Concordance:** Spearman `ρ` between the two rank vectors as a single
  headline number ("survey and model orderings agree at ρ=…").
- Returns: per-group `group, biomass, model_rank, survey_value, survey_rank`;
  a list of inversion pairs; scalar `rho`. No verdict.
- **Guards (visible, not silent):** a group with `NA`/`0`/negative biomass or
  `NA`/`0` `survey_value` is **excluded and reported** (footnote + `warning()`),
  never ranked as a real 0. `< 2` rankable groups → structured "insufficient
  groups" signal.

### 3.4 SECONDARY — per-group survey trend (context, pure)
`compute_survey_trends(series_df, ref_year)`: per demersal group, a relative
series (`survey_value / mean(window)`), a simple **direction** glyph
(`↑/↓/≈` from `mean(last 3 yr)` vs `mean(window)` — no Spearman here), and
`ref_year_percentile` = **fraction of window years with `survey_value ≤` the
ref-year value** (pinned estimator: min→0, max→1, median→0.5; `ref_year` absent
from a group's series → `NA`). Purpose: "is the model's snapshot year a survey
outlier? → consider averaging biomass across years."
- **Degenerate guard:** a group whose window-mean is `0`/`NA` (all-zero/all-NA
  series) is **excluded + reported** (footnote + `warning()`), never normalised
  into `NaN`/`Inf`. `< 3` years → trend omitted for that group, **routed to the
  exclusions footnote** ("trend omitted: <n> survey years") + `warning()`.

### 3.5 Reference year & window
- **Window:** fixed trailing **10 years** ending at the most recent year with
  **complete** indices (labelled; a provisional latest year is not treated as
  authoritative). Not user-selectable (YAGNI).
- **Reference year (model snapshot):** most recent year common to the mapped
  demersal groups; user-selectable from common years. No common year → per-group
  fallback with `warning("[survey_validation] reference years differ: …")` +
  UI notice; the ordering table then carries a per-group `survey_year` column so
  the heterogeneity is visible at the point of use.

### 3.6 Outputs
- **Clupeid banner** (§1.4) at the top — always visible.
- **Primary:** the ordering table (model rank vs survey rank per demersal group)
  + an **inversions list** (the actionable prompts) + the `ρ` concordance line,
  under the §1.2 body-size caveat.
- **Secondary (collapsible):** per-group trend lines, ref-year marked, `↑/↓/≈`.
- **Exclusions footnote:** counts + names + **reason** for every excluded group
  (unmapped, pelagic, non-fish, no-survey-data, zero/NA biomass-or-survey,
  <3-yr trend) so coverage is fully auditable — the modeller can reconcile
  "model has N groups; comparison shows M; here's N−M and why."

## 4. Data flow
`rpath_values$params$model` → filter `Type %in% c(0,1)` → mapping (dictionary +
auto + edits + persistence; demersal-only) → per group:
`lookup_datras_indices(aphia_ids, survey, years=window)` (once) →
`aggregate_survey_series(quarter, areas)` → annual `survey_value` →
`compute_ordering_check()` (primary, at ref year) + `compute_survey_trends()`
(secondary) → render. "Run validation" button; per-group loop under
`withProgress` honouring `lookup_datras_indices`'s `timeout`. **Validate group
count early:** if `< 2` mappable demersal groups, say so *before* the fetch, not
after the user waited.

## 5. Error handling
- Inside `error = function(e)` closures: **`<<-`** to mutate outer state;
  **`warning()` not `message()`** (production keeps no `message()` logs).
- **Every drop → an exclusions-footnote row + `warning()`** — never
  footnote-only, never a silent `NaN`/`Inf`/`0`, never a `message()`. This
  covers: unmappable, multi-match→NA, pelagic, no-survey-data, <3-yr trend,
  zero/NA biomass-or-survey, no-value-at-reference-year.
- **No balanced model** → "Balance an Ecopath model first." (benign UI state,
  *not* a `warning()`); **`worrms`/`icesDatras` missing** → notice, no crash
  (benign). These two are deliberately not the `warning()` path; the genuine
  drops above always are.
- Never `stop()`.

## 6. Testing
Conventions: never gate `expect_*` behind `if`; `skip_if` with reasons; live
behind `RUN_LIVE_TESTS`.
- **`compute_ordering_check()` (TDD core):** a known biomass/survey set → exact
  `model_rank`/`survey_rank`; a constructed inversion (model cod>plaice, survey
  plaice>cod) appears in the inversion list; `rho` exact on a small set;
  `survey_value == 0`/`NA`/negative biomass → group **excluded + reason**, not
  ranked; `< 2` groups → "insufficient" signal. Assert output schema via
  `expect_setequal(names(...), …)` (proves the **verdict-free** contract), not a
  single negative.
- **`compute_survey_trends()` (TDD core):** rel-series normalised to mean 1;
  `↑/↓/≈` for rising/falling/flat; `ref_year_percentile` exact (max→1, min→0,
  median→0.5); `ref_year` outside series → `NA`; **all-NA / all-zero series →
  excluded, NOT `NaN`/`Inf`**; `<3` years → omitted + reason.
- **`aggregate_survey_series()`:** synthetic multi-area/multi-quarter frame —
  `quarter = 1` drops Q4 rows before summing; `areas = "BS_CodEast"` sums only
  East, `areas = c(East,West)` sums both (the S1 double-count guard); a
  many-species `aphia_ids` set sums across species; partial-NA sum → `warning()`,
  not NA→0.
- **Mapping helper:** dictionary hit returns the AphiaID(s); `wm_name2id`
  multi-match → `NA`; `aid <= 0`/`NA` → `NA`; pelagic AphiaID → class "pelagic".
- **Idiom trap (record for implementer):** the group-loop test mocks **our own**
  `lookup_datras_indices` via `local_mocked_bindings(lookup_datras_indices = …)`
  with **no `.package`** (it is not an `icesDatras` binding).
- **Gated integration test (add — nearly free):** `skip_if_no_live_tests()`;
  `wm_name2id("Gadus morhua")` → `lookup_datras_indices(survey="BITS")` →
  `aggregate_survey_series()` → assert a non-empty per-year series. Closes the
  `wm_name2id → fetch → aggregate` seam that unit mocks can't.

## 7. Gated future phases (out of R2)
- **Next (R2b): clupeid coverage via BIAS acoustic** — a read-only pull or a
  user paste-in/manual-entry path for the 2 dominant Baltic pelagic indices, so
  the panel covers the groups MARBEFES cares most about. Cheap relative to the
  demersal machinery; the §1.4 banner is the placeholder until then.
- **Later: absolute biomass calibration** — opt-in, requires user-supplied `q`,
  **swept-area/stratified-area weighting** (q alone ≠ density), and length-weight
  params, all assumptions shown. Its own spec.

## 8. Open items (resolve at implementation)
- The shipped Baltic EwE-name → AphiaID(s) dictionary contents + the curated
  pelagic set — confirm with the user; document as heuristics, user-overridable.
- Editable mapping-table widget (DT editable vs `rhandsontable`) — match
  `group_params`.
- Mapping persistence key/store (per model-file vs per session-restore) and
  format.
- Region-survey default (BITS) and whether the dropdown offers all three;
  whether non-Baltic models are in v1 scope.
- `lookup_datras_indices` cache keys differ between R2 (single-survey) and R3
  (three-survey default) → no cross-panel cache sharing (accepted; note).

## Appendix: reference patterns
- **Model object & namespaced tabs:** `R/modules/rpath_server.R`
  (`output$rpath_content`/`tabBox` ~:85; `rpath_values$params$model` ~:694;
  `Group`/`Type`/`Biomass` ~:711-713; `btn_run_ecopath`; editable table ~:832).
- **Survey fetch + conventions (explicit-`years` path):**
  `R/functions/ices_lookups.R` (`lookup_datras_indices` ~:146-303); reshape +
  mock + live-gate + `expect_setequal` idioms in
  `tests/testthat/test-ices-lookups.R` (~:38-70, :84-99, :195-221, :170/:180).
- **`wm_name2id` tryCatch + scalar guard:** `R/modules/trait_research_server.R`
  (R3 `datras_fetch` observer).
- **Error-closure (`warning()`, `<<-`) / test conventions:** project `CLAUDE.md`;
  R1/R3 specs.
