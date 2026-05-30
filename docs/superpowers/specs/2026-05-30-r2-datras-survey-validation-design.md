# DATRAS Survey Validation of Ecopath Biomass — Design

**Date:** 2026-05-30
**Author:** A. Razinkovas-Baziukas (with Claude)
**Status:** Revised after 3-agent spec review (architecture / convention /
domain). Pending user review.
**Tracks:** v2-plan item **R2** (PR10 scope-L). Re-scoped during brainstorm
(2026-05-30) from "DATRAS biomass calibration" to **relative survey
validation**; primary view changed from cross-group shares to **per-group
temporal trend** after the domain review (see §1.4). See §1.1–1.3 for the
science boundaries.

---

## 1. Goal & non-goals

### Goal
Give a modeller an honest, on-demand check of an Ecopath model's biomass
inputs against ICES DATRAS survey data: for the model's **demersal** fish
groups that map to a trawl-surveyed species, show (primary) the **multi-year
survey abundance trend** per group and where the model's snapshot year sits on
it, and (secondary) a heavily-caveated cross-group structure comparison. A
read-only diagnostic inside the rpath module.

### 1.1 Why "validation", not "calibration" (rejected scope)
Absolute calibration was deliberately rejected. Converting DATRAS abundance to
absolute biomass (tons/km²) requires `density = catch / (swept_area × q)`, and
trawl catchability `q` is small, uncertain, and species/size/gear-specific —
surveys are designed as *relative* indices because their absolute scaling is
unreliable. Seeding Ecopath `Biomass` from an assumed `q` injects false
precision into a mass-balance model. R2 uses the survey only for relative
signals. Absolute conversion is a gated future phase (§8).

### 1.2 Currency caveat (biomass vs numbers)
Model biomass and survey abundance are different currencies:
`biomass ∝ N × W` (numbers × mean weight); `survey abundance ∝ N` (numbers).
They agree across groups only if `W` is constant across groups — it is not
(sprat ~10 g, adult cod ~kg+). So any **cross-group** comparison of
biomass-share vs abundance-share is confounded by body-size structure. This is
why cross-group shares are demoted to a secondary, caveated view (§3.5) and
carry no pass/fail verdict.

### 1.3 Gear caveat (bottom trawl under-samples pelagics) — the decisive one
DATRAS `getIndices` here comes from **BITS, a bottom-trawl survey.** Bottom
trawls under-sample **pelagic** species: Baltic herring and sprat are
semi-pelagic and are caught by BITS only opportunistically, so their BITS
indices are **not** valid relative abundance indices — ICES assesses Baltic
clupeids from the **BIAS acoustic** survey instead. A sprat/herring BITS
abundance-share is therefore a *gear artifact*, larger than and independent of
the §1.2 currency effect. Consequently R2:
- restricts all comparisons to the **demersal assemblage** (cod, flounder,
  plaice, dab, brill, turbot, …), where catch efficiency is at least the same
  order of magnitude across groups;
- **flags and excludes known pelagics by default** (a small curated pelagic
  AphiaID set — herring 126417, sprat 126425, and a few others — user can
  re-include with an explicit "BITS under-samples pelagics; prefer BIAS
  acoustic" warning shown);
- never silently pools a pelagic into a demersal denominator.

### 1.4 Why per-group temporal trend is the primary view
The within-group trend over time is the one relative comparison confounded by
**neither** §1.2 nor §1.3: catchability `q` and mean weight `W` are ~constant
for a given group across years, so they cancel in a trend. "Is cod's survey
index rising or falling; is the model's standing-stock year representative or
anomalous?" is defensible. Cross-group single-year shares are confounded by
both effects, so they are secondary (§3.5).

### Non-goals (YAGNI)
- **No absolute biomass.** No `q`, swept-area, or length-weight machinery.
- **No automatic model-error verdict.** Trends and shares are interpretive aids;
  the modeller judges. No ✓/⚠ pass/fail.
- **No non-demersal / non-fish groups.** Aggregates, inverts, plankton, birds,
  mammals, and pelagic fish are excluded from comparison (shown as excluded).
- **No cross-session persistence** of the group→species mapping.
- **No new survey-fetch logic.** Reuses `lookup_datras_indices()` (incl. its
  explicit-`years` multi-year path) and `worrms::wm_name2id()` as-is.

## 2. Architecture & placement
- **UI tab is built server-side.** `R/ui/rpath_ui.R` is only a shell
  (`uiOutput(ns("rpath_content"))`); the actual tabs are rendered in
  `R/modules/rpath_server.R` inside `output$rpath_content <- renderUI({ tabBox(
  tabPanel(...), ... ) })` (~line 85), conditional on Rpath being installed. The
  new **"Survey Validation"** `tabPanel` is added inside **that `tabBox` call in
  `rpath_server.R`** — NOT in `rpath_ui.R` (which would do nothing).
- The rpath module **is namespaced** (`rpathModuleUI(id)` uses `ns()`,
  `rpathModuleServer` uses `moduleServer`). All new inputs/outputs use `ns(...)`.
- Server logic in `rpath_server.R` reads the balanced model from
  `rpath_values$params$model` (`Group`, `Type`, `Biomass`).
- **Pure core:** `R/functions/rpath/survey_validation.R` holds the two pure
  functions (§3.3, §3.5) + helpers — unit-testable without Shiny/network. TDD
  target.
- **Read-only:** nothing writes back to `rpath_values$params$model`. The pure
  functions return new data.frames; no `observeEvent`/proxy edit touches the
  model.

## 3. Components

### 3.1 Group → species mapping (hybrid)
For each **living group** (`model$Type %in% c(0, 1)` — consumer/producer;
explicitly excludes `Type == 2` detritus and `Type == 3` fleets):
- **Auto-suggest:** `tryCatch(worrms::wm_name2id(Group), error = function(e) {
  warning(...); NA_real_ })`. The helper applies R3's guard:
  `length(aid) == 1 && !is.na(aid) && aid > 0` — **a multi-match (`length > 1`)
  yields `NA` (ambiguous), never a silent `[1]`** (so aggregate/genus names
  aren't mistaken for a species).
- **Demersal/pelagic classification:** a resolved AphiaID in the curated
  pelagic set (§1.3) is marked pelagic and **excluded by default**.
- **Manual override:** an editable mapping table — `Group | suggested species |
  AphiaID | class (demersal/pelagic/unmapped) | include`. User confirms,
  overrides (type a species → re-resolve), or toggles inclusion. Match the
  editable-table widget rpath already uses for `group_params`.
- Mapping held in a session `reactiveVal`, (re)seeded with auto-suggestions on
  **mass-balance completion** (`observeEvent(input$btn_run_ecopath …)` success,
  when `rpath_values$params$model` carries balanced values) — NOT on the
  earlier convert step, so the user's mapping isn't silently reset mid-flow.

### 3.2 Per-group survey fetch & aggregation
- **One region-survey** (panel dropdown, default **BITS** for the Baltic study
  region; not multiple — avoids mixing regions, §1.3).
- **Multi-year, fetched once:** `lookup_datras_indices(aphia_id, surveys =
  survey, years = seq(max_year - window, max_year))` using the explicit-`years`
  path (returns all requested years, no step-back). Fetching the full window
  once feeds BOTH the trend (§3.3) and the reference-year share (§3.5); the
  reference year is filtered downstream in the pure functions, not refetched.
- **Quarter pinned:** filter to one quarter (default the survey's main quarter,
  **Q1 for BITS**; panel-selectable) — Q1/Q4 sample different size structure, so
  pooling them is unsound (domain S2).
- **Area within region:** sum `abundance_index` across only the `index_area`s in
  the model's region, NOT blindly across all areas. Default offers the survey's
  areas with the user able to deselect; the footnote lists which areas were
  summed. (E.g. East vs West Baltic cod are separate stocks — summing both
  double-counts unless the model spans both; domain S1.)
- Age aggregation stays `abundance_index` = sum over `Age_*` (the building
  block's output); a fixed caveat notes strong year-classes (age-0/1) add
  numbers-share noise — mitigated by the multi-year trend being primary.

### 3.3 PRIMARY — per-group temporal trend (pure)
`compute_survey_trends(series_df, ref_year)` where `series_df` has `group`,
`year`, `survey_value` (one row per group-year, demersal groups only).
- Per group: normalise to a relative series (`survey_value / mean(survey_value
  over the window)`), the trend direction over the window (sign/slope, e.g.
  Spearman of value vs year), and the **relative position of `ref_year`** on the
  series (is the model's snapshot year high/low/typical vs the window).
- Returns per group: `group, rel_series (per year), trend_direction,
  ref_year_percentile`. No verdict. Guard: a group with `< 3` years → trend
  omitted for that group (noted), not errored.

### 3.4 Reference year & window
- **Window:** trailing N years (default 10) ending at the most recent year with
  **complete** indices — labelled as such; a returned-but-provisional latest
  year is not treated as authoritative (domain S3).
- **Reference year (the model's snapshot):** default = most recent year common
  to the mapped demersal groups; user-selectable from the common years. If
  groups share no common year, fall back per group and emit
  `warning("[survey_validation] reference years differ across groups: …")` **and**
  a UI notice (not UI-only).

### 3.5 SECONDARY — cross-group shares (demersal-only, caveated; pure)
`compute_survey_shares(groups_df)` where `groups_df` has `group`, `biomass`,
`survey_value` for the **demersal** mapped groups at the reference year.
- `model_share = biomass / Σ biomass`; `survey_share = survey_value / Σ
  survey_value`; `log2_ratio = log2(model_share / survey_share)` as an
  **informational sort key, explicitly not an error metric** (§1.2). No flag.
- Returns `group, biomass, model_share, survey_value, survey_share, log2_ratio`.
  Guard: `< 2` rows → structured "insufficient groups" signal.

### 3.6 Outputs
- **Primary panel:** per-group relative trend lines (faceted or overlaid,
  normalised) with the reference year marked; a short per-group "↑/↓/≈ over last
  N yr" summary.
- **Secondary panel (collapsible):** the shares table + paired bar chart
  (model **biomass**-share vs survey **abundance**-share), axis-labelled with
  the two currencies, under a **prominent body-size + gear caveat** (§1.2/§1.3).
- **Exclusions footnote:** counts + names of groups excluded as unmapped,
  pelagic, or non-fish — partial coverage is explicit, never hidden.

## 4. Data flow
`rpath_values$params$model` → filter `Type %in% c(0,1)` → mapping (auto +
overrides, demersal-only by default) → for each included group:
`lookup_datras_indices(survey, years = window)` (once) → filter quarter + region
areas → annual `survey_value` series → `compute_survey_trends()` (primary) and
`compute_survey_shares()` at `ref_year` (secondary) → render. Triggered by a
"Run validation" button. The per-group loop runs under `withProgress`, passing
`lookup_datras_indices`'s `timeout`, so a many-group model can't hang the
session silently; results are the function's own `.ices_cache`-memoised lists.

## 5. Error handling
Project conventions:
- Inside `error = function(e)` closures, use **`<<-`** to mutate any outer-scope
  variable (e.g. `result$error <<- conditionMessage(e)`) — plain `<-` is
  silently discarded (CLAUDE.md).
- Use **`warning()` not `message()`** inside those closures (production keeps no
  `message()` logs).
- **No balanced model** → "Balance an Ecopath model first." (not an error)
- **Group unmappable / `wm_name2id` fails** → `tryCatch(…, error =
  function(e){ warning(…); NA })`; excluded; shown in the exclusions footnote.
- **`lookup_datras_indices` fails / no rows for a group** → `warning()`; group
  shown as "no survey data" and dropped.
- **< 2 demersal groups with data** → "Need ≥2 mappable demersal fish groups."
- **`worrms` / `icesDatras` missing** → notice, no crash. Never `stop()`.

## 6. Testing
Conventions: never gate `expect_*` behind `if`; `skip_if` with reasons; live
tests behind `RUN_LIVE_TESTS`.
- **`compute_survey_trends()` (TDD core, no network):** relative series
  normalised to mean 1; trend direction sign for a rising/falling/flat series;
  `ref_year_percentile` placement; `< 3` years → group omitted, not errored.
- **`compute_survey_shares()` (TDD core, no network):** both shares sum to 1;
  `log2_ratio` correct (model 0.6 / survey 0.3 → +1); **assert no flag/verdict
  column**; zero/NA handled; `< 2` rows → "insufficient" signal.
- **Aggregation helper:** synthetic multi-area/multi-quarter
  `lookup_datras_indices`-shaped frame → correct quarter-pinned, region-area
  sum per year; mocked `lookup_datras_indices` for the group loop.
- **Mapping helper:** auto-suggest → `NA` for an aggregate name AND for a
  multi-match (`length > 1`); a value for a clean single-species name (mocked
  `wm_name2id`); pelagic AphiaID classified pelagic.
- **Known live-coverage gap (recorded, intentional):** R3's gated cod test
  covers `lookup_datras_indices` in isolation; the `wm_name2id → fetch →
  aggregate` pipeline has no live test in v1 — acceptable, gated for a future
  R2 integration test.

## 7. Gated future phase (explicitly out of R2)
Absolute biomass estimation — opt-in, requiring **user-supplied** `q`,
**swept-area / stratified-area weighting** (q alone doesn't convert an index to
density), and length-weight/age-length params, every assumption shown in the
UI. Its own spec, gated on those inputs.

## 8. Open items (resolve at implementation)
- Editable mapping-table widget (DT editable vs `rhandsontable`) — match what
  rpath uses for `group_params`.
- The curated pelagic-AphiaID exclusion set — start with herring/sprat and the
  obvious Baltic pelagics; confirm the list with the user; document it's a
  heuristic, user-overridable.
- Region-survey default (BITS) and whether the dropdown offers all three
  surveys; whether non-Baltic models are in scope for v1 at all.
- Trend statistic (Spearman vs simple recent-vs-window-mean) — pick the simplest
  defensible one.
- `lookup_datras_indices` cache keys differ between R2's single-survey calls and
  R3's three-survey default → no cache sharing across panels (accepted; note).

## Appendix: reference patterns
- **Model object & namespaced tabs:** `R/modules/rpath_server.R`
  (`output$rpath_content`/`tabBox` ~:85; `rpath_values$params$model` ~:694;
  `Group`/`Type`/`Biomass` ~:711-713; balance trigger `btn_run_ecopath`;
  editable params table ~:832).
- **Survey fetch + conventions (incl. explicit-`years` multi-year path):**
  `R/functions/ices_lookups.R` (`lookup_datras_indices` ~:146-303), reused as-is.
- **`wm_name2id` tryCatch + scalar guard idiom:**
  `R/modules/trait_research_server.R` (the R3 panel's `datras_fetch` observer).
- **Error-closure (`warning()`, `<<-`) / test conventions:** project `CLAUDE.md`;
  the R1/R3 specs.
