# Trait-Research DATRAS Abundance Panel — Design

**Date:** 2026-05-29
**Author:** A. Razinkovas-Baziukas (with Claude)
**Status:** Revised after 3-agent spec review (architecture / convention /
domain). **The domain review surfaced — and direct `icesDatras` 1.4.1 testing
confirmed — that the supposedly "already-shipped" building block
`lookup_datras_indices()` is broken and returns zero rows on every call in
production.** R3 therefore has two parts: **(A) fix the building block** and
**(B) add the panel.** See §2.1 for the verified bugs.
**Tracks:** v2-plan item **R3** (the on-demand DATRAS surface in the
trait-research tab; see
`docs/superpowers/plans/2026-05-26-v2-plan-remaining.md`). Approach **A**
(on-demand fetch + table) approved in chat 2026-05-29.

---

## 1. Goal & non-goals

### Goal
After a trait lookup has populated the results table, let the user pick one of
the found species and pull its **ICES DATRAS age-disaggregated abundance
indices** for the MARBEFES study-region trawl surveys — on demand, without
leaving the Trait Research tab.

### Scope reframing (post-review)
This was scoped as a thin presentation layer over a shipped function. It is not:
the function (`lookup_datras_indices()`) has three confirmed DATRAS-domain bugs
(§2.1) that make it return nothing today. R3 must therefore:

- **Part A — fix `lookup_datras_indices()`** so it actually returns indices
  (§2.2). This is real domain logic, not just wiring.
- **Part B — add the on-demand panel** (UI + server wiring, §5–§6).
- **Part C — propagate `aphia_id`** through the orchestrator's API path so the
  panel has a reliable AphiaID source (§3).

### Non-goals (YAGNI)
- **No Ecopath biomass calibration.** Converting survey *abundance* indices into
  mass-balance biomass is the deferred scope-L follow-up. R3 only *displays* the
  raw indices. (Note: the data is **abundance-at-age**, never biomass — see §2.1
  bug 3; the naming throughout R3 must not say "biomass".)
- **No batch / all-species fetch.** One selected species per click.
- **No new caching layer.** `lookup_datras_indices()` memoises in `.ices_cache`;
  the panel adds none. (The fix in Part A must keep that memoisation intact.)
- **No survey/year pickers in v1.** Fixed to the three valid MARBEFES DATRAS
  surveys (§4) and the most-recent year with computed indices.
- **No cross-module transfer.** Indices are shown, not pushed to Food Web
  Construction. The module's `return()` list (server :1045) stays unchanged —
  do **not** reflexively add `datras_result` to it.

## 2. The building block

### 2.1 Verified bugs in the current `lookup_datras_indices()`

Tested directly against `icesDatras` 1.4.1 on 2026-05-29 (`getSurveyList()`,
`getIndices("NS-IBTS", 2023, ...)`):

1. **`quarter = -1` is rejected.** `getIndices(svy, yr, quarter = -1)`
   (ices_lookups.R:210) returns a bare **`FALSE`** with *"Supplied quarter (-1)
   is not available. Available options are: 1 3"*. Unlike `getCPUELength` /
   `getHaulData`, `getIndices` does **not** treat `-1` as an all-quarters
   wildcard. The `FALSE` is then silently swallowed by the non-data.frame guard
   added in `949f9e7` → the year is skipped → **zero rows, every call.** (The
   `949f9e7` guard wasn't catching a rare malformed response; it was masking
   this bug.)
2. **`species` is never passed.** Signature is
   `getIndices(survey, year, quarter, species)`. The function omits `species`,
   so even with a valid quarter it would pull *all* species and rely on a
   client-side `AphiaID` filter (:234-236) — but #1 means it never gets that far.
3. **There is no `Index` column.** `getIndices` output columns (verified, 25
   cols) are `Survey, Year, Quarter, AphiaID, Species, IndexArea, Sex, PlusGr,
   Age_0 … Age_15, DateofCalculation`. The function's
   `biomass_index = if ("Index" %in% names(idx)) idx$Index[1] else NA_real_`
   (:243) → `"Index"` is never present → `biomass_index` is **always `NA`**, and
   the label "biomass" is wrong: the data is **abundance numbers-at-age**.
4. **Row-1 collapse drops `IndexArea` disaggregation (should-fix).** A single
   survey-year-quarter returns **multiple rows, one per `IndexArea`**. Verified:
   NS-IBTS 2023 Q1 cod → `NS_Cod` + `NS_CodCat` (2 rows); BITS splits Baltic cod
   into **East/West** stocks (`BS_CodEast` / `BS_CodWest`) — biologically and
   management distinct. Taking `idx$...[1]` silently reports one and discards the
   rest.

### 2.2 Required fixes (Part A)

Rewrite the inner loop of `lookup_datras_indices()` to:

- **Resolve real quarters** per survey-year instead of `-1`: query
  `icesDatras::getSurveyYearQuarterList(svy, yr)` (under `with_timeout()`) and
  iterate the quarters it returns. (BITS → 1, 4; NS-IBTS → 1, 3; BTS varies.)
- **Pass `species = aphia_id`** into `getIndices(svy, yr, quarter, species = aphia_id)`
  so DATRAS filters server-side. The client-side `AphiaID` filter and its
  "no-op fallback" (:231-237) then become **dead code — remove them.**
- **Reshape the output**: emit **one row per `(survey, year, quarter, index_area)`**
  with the age-at-abundance values. Two acceptable shapes (decide in §8):
  - **(preferred) long per-area row** carrying `index_area` plus a documented
    aggregate `abundance_index` = sum of `Age_0…Age_N` (total numerical
    abundance index, n per hour), with the age vector retained for the detail
    view; **or**
  - the full age columns surfaced as-is.
  Either way: column is named **`abundance_index`** (or `index_area` +
  age columns), **never `biomass_index`**. Update the docblock's `@return`.
- **Keep** the `.ices_cache` memoisation, the `requireNamespace("icesDatras")`
  guard, the `aphia_id` validation, and the `warning()`-on-skip discipline.

### 2.3 Return shape (the panel's input contract, post-fix)

```r
list(
  success = TRUE/FALSE,
  source  = "DATRAS",
  data    = <data.frame: survey, year, quarter, index_area, abundance_index, ...> | NULL,
  error   = NA_character_ | "<reason>"
)
```

`success = FALSE` with a populated `error` is returned both for genuine API
failures **and** for the legitimate "no rows for this species" case (:249) — the
panel must distinguish these (§6c / §7).

## 3. The AphiaID problem (Part C)

`lookup_datras_indices()` needs an **AphiaID**. The trait module surfaces species
by name, and `aphia_id` is **not reliably present** in `rv$trait_results`:

- The **offline-DB** path selects an `aphia_id` column (`orchestrator.R:79`).
- The **WoRMS API** path resolves `aphia_id_for_lookup` (`orchestrator.R:217`)
  and threads it into downstream calls — **but never writes it back onto the
  result data.frame.** Confirmed: no `result$aphia_id <-` assignment exists.
  So for every species resolved via the live API path, the column is absent/NA.

So the column-first read is the *happy case for offline-DB hits only*; the live
path is the common path and currently has no AphiaID. Fixes:

- **Part C fix:** add `result$aphia_id <- aphia_id_for_lookup` at the end of the
  API path in `lookup_species_traits()` so the resolved AphiaID propagates onto
  the result row. This makes the column-first strategy actually reliable.
- **Fallback (kept):** if the column is still absent/`NA` for a given species,
  the fetch observer resolves the name → AphiaID with **`worrms::wm_name2id(sp)`**
  (already a transitive dependency via `database_lookups.R`; a single HTTP call
  returning an integer), wrapped in `tryCatch` with `warning()` + `<<-` on the
  outer `aid` (§6c). If that also fails → structured "no AphiaID" notification,
  no DATRAS call.

## 4. Survey & year scope (design decision)

- **Surveys (corrected):** `c("BITS", "NS-IBTS", "BTS")` — the three valid
  MARBEFES DATRAS surveys. **BIAS is dropped:** it is the Baltic International
  Acoustic Survey (WGBIFS), not a DATRAS trawl survey (confirmed absent from
  `getSurveyList()`); its indices live in the ICES acoustic database, out of
  scope here. Update the `lookup_datras_indices()` default and all "four
  surveys" wording to three.
- **Year:** most-recent year **with computed indices**, not `Sys.Date()`'s year
  and not blindly `max(getSurveyYearList())`. Indices lag the survey by ~6–18
  months (the 2023 Q1 cod index has `DateofCalculation = 2024-04-09`), and the
  year *list* already advertises 2026 today. So: try `max(year list)`, and if it
  yields no index rows, step back (max−1, max−2) until rows appear or a small
  bound is hit. The panel labels results with the actual `year` returned.

## 5. UI design (`R/ui/trait_research_ui.R`)

Add a **collapsible `box()` inside a `fluidRow`**, placed **between the
"Offline Trait Database" `fluidRow` (closes :702) and the `bsModal` Help Modal
(:705)**. Mirrors the existing full-width collapsible boxes (radar :657,
offline-DB :671): `box(status=, solidHeader=TRUE, width=12, collapsible=TRUE,
collapsed=TRUE)`.

Contents (bare, non-namespaced IDs — these modules don't use `NS()`):

```
box("DATRAS Abundance Indices", status = "primary", collapsed = TRUE, ...)
├─ selectInput("datras_species", "Species:", choices = NULL)   # filled server-side
├─ actionButton("datras_fetch", "Fetch DATRAS Indices", icon("water"), btn-info)
├─ uiOutput("datras_status")        # idle / spinner / no-data / error / "n rows, N surveys"
└─ DT::dataTableOutput("datras_table")
```

- `selectInput` choices are NULL in static UI, populated server-side from
  `rv$trait_results$species` (§6a). Empty until a lookup has run.
- Help text under the button names the three surveys, the most-recent-indexed
  year, and states the indices are **raw abundance-at-age, not biomass and not
  calibrated** — surfacing the non-goal to users and matching the corrected
  terminology.

**Why a box, not a 4th tab:** the tabset is the per-lookup results view; DATRAS
is an optional drill-down on one species, a sibling of the radar/offline-DB
boxes that already sit below the tabset.

## 6. Server design (`R/modules/trait_research_server.R`)

Modules take `input/output/session` directly and register outputs with **bare
IDs** (no `NS()`/`moduleServer()` — project convention). New `reactiveValues`
slots (extend the block at :10): **`datras_result = NULL`** and a
**`datras_fetching = FALSE`** in-flight flag.

**a. Populate / clear the species selector.**
- Beside the existing `updateSelectInput(... "trait_research_raw_species" ...)`
  (server :347-352), add one more `updateSelectInput(session, "datras_species",
  choices = rv$trait_results$species)`. Use the **harmonised `species` column**
  (not `names(raw_list)`) so the selector and the row filter
  (`df$species == sp`) operate on the same values.
- In the `trait_research_clear` observer (:388), reset **both** the selector
  (`updateSelectInput(... "datras_species", choices = character(0))`) **and**
  `rv$datras_result <- NULL`, so a cleared session shows no stale table.

**b. AphiaID reactive (pure read — no I/O).**

```r
.datras_aphia_for_species <- function(df, sp) {   # pure helper, unit-testable
  if (is.null(df) || !"aphia_id" %in% names(df)) return(NA_real_)
  row <- df[df$species == sp, , drop = FALSE]
  if (nrow(row) == 0) return(NA_real_)
  aid <- as.numeric(row$aphia_id[1])   # no suppressWarnings — a coercion
                                       # warning here is a useful data signal
  if (length(aid) == 1 && !is.na(aid) && aid > 0) aid else NA_real_
}

datras_aphia_id <- reactive({
  req(input$datras_species)
  .datras_aphia_for_species(rv$trait_results, input$datras_species)
})
```

**c. Fetch observer** (`observeEvent(input$datras_fetch, ...)`):

```r
if (isTRUE(rv$datras_fetching)) {            # double-click guard
  showNotification("DATRAS fetch already in progress…", type = "warning"); return()
}
rv$datras_fetching <- TRUE
on.exit(rv$datras_fetching <- FALSE)         # plain <- : rv is by-reference
rv$datras_result <- NULL                     # clear → status shows spinner

aid <- datras_aphia_id()
if (is.na(aid)) {                            # WoRMS fallback (network I/O)
  aid <- tryCatch(
    worrms::wm_name2id(input$datras_species),
    error = function(e) {
      warning(sprintf("[datras] wm_name2id failed for '%s': %s",
                      input$datras_species, conditionMessage(e)), call. = FALSE)
      NA_real_            # closure-local; assigned to aid via <- below is fine
    }
  )
}
if (is.na(aid) || aid <= 0) {
  showNotification("No AphiaID available for this species", type = "warning"); return()
}

res <- lookup_datras_indices(aid)            # defaults: 3 surveys, most-recent indexed year
rv$datras_result <- res
if (!isTRUE(res$success)) {
  # distinguish legitimate "no data" from a real failure
  is_no_data <- grepl("^no DATRAS rows", res$error %||% "")
  showNotification(paste("DATRAS:", res$error),
                   type = if (is_no_data) "message" else "warning")
}
```

> Note on `<<-`: the convention applies to error closures that mutate an
> **outer-scope variable**. Here the `tryCatch` returns its value and `aid <- `
> captures it at observer-body scope, so plain `<-` is correct. `rv$...<-`
> assignments are also plain `<-` (`rv` is a by-reference `reactiveValues`). Use
> `<<-` only if you instead write the result *inside* the closure onto a
> variable declared before the `tryCatch`.

**d. Outputs.**
- `output$datras_table <- DT::renderDataTable({ req(rv$datras_result$success); ...
  rv$datras_result$data ... })` — show `survey, year, quarter, index_area,
  abundance_index` (+ optional age columns). `escape`/format per the existing
  trait-table idiom (:522).
- `output$datras_status <- renderUI(...)` covering: null+not-fetching → idle;
  `rv$datras_fetching` → spinner (`fa-spin`, mirroring `trait_research_status`
  :421); success → "n rows across N surveys"; no-data → informational; error →
  alert. The "no data" vs "error" split mirrors §6c.

## 7. Error handling

- Branch on `$success`; the building block never `stop()`s.
- The WoRMS fallback closure (§6c) **`warning()`s** before returning `NA`
  (production preserves no `message()` logs).
- `icesDatras` / `worrms` missing → structured error / notification, no crash.
  Confirm `icesDatras` is in the laguna deploy env (§8).
- Zero matching rows is a **`type = "message"`** informational notice routed
  through the status output, not a fault-styled `"warning"`/`"error"` toast.
- Long calls: `lookup_datras_indices` wraps each upstream call in
  `with_timeout()`; the `datras_fetching` spinner gives feedback meanwhile.

## 8. Open items (resolve at implementation)

- **Output shape (§2.2):** choose long-per-area aggregate `abundance_index`
  (preferred) vs full age columns. Decide whether the panel shows the aggregate
  with ages in an expandable detail, or the age matrix directly.
- **Quarter resolution:** `getSurveyYearQuarterList()` per survey-year vs a
  hardcoded per-survey quarter map. Prefer the API call (robust to schedule
  changes) under `with_timeout()`.
- **Year fallback bound:** how many years to step back (max → max−1 → max−2?)
  before giving up when the latest listed year has no computed indices yet.
- **`worrms::wm_name2id` return contract:** verify it returns a scalar positive
  integer for a clean name and errors (not `NULL`/multi) for ambiguous names;
  adjust the guard accordingly.
- **`icesDatras` / `worrms` presence** in the laguna environment — grep the
  installed library before assuming the happy path.
- **`aphia_id` propagation (Part C):** confirm the exact end-of-API-path site in
  `lookup_species_traits()` for `result$aphia_id <- aphia_id_for_lookup`, and
  that it doesn't disturb the `bind_rows()` column union in the module (:338).

## 9. Testing

Conventions: never gate `expect_*` behind `if`; use `skip_if()` with a reason;
live API tests behind `RUN_LIVE_TESTS=true` with `with_timeout` discipline.

- **Unit (no network) — Part A building block:** pre-seed `.ices_cache` (R1
  fixture pattern) and/or stub, asserting the **fixed** behaviour: a valid
  quarter is used (not `-1`); `species` is passed; a synthetic two-`IndexArea`
  `getIndices`-shaped frame yields **two output rows** (not one); the
  `abundance_index` aggregate sums the age columns; `success = FALSE` +
  `"no DATRAS rows"` on an empty frame.
- **Unit (no network) — AphiaID helper:** `.datras_aphia_for_species()` is pure
  and **must** be tested (not "preferred"): present+valid column → returns it;
  missing column → `NA`; `NA`/non-positive value → `NA`; species not in df →
  `NA`.
- **Unit (no network) — result shaping:** feed a success result with an `NA`
  `abundance_index` row through the table/status formatting; assert no error on
  the `NA`.
- **Live (gated `RUN_LIVE_TESTS=true`):** real `lookup_datras_indices(126436)`
  (cod). Follow the R1 live pattern: **`skip_if(!isTRUE(res$success), "DATRAS
  unavailable / no data for cod — check icesDatras + API")`** before the
  assertions (a network call can legitimately fail), then
  `expect_true(nrow(res$data) >= 1)` and `expect_true(is.numeric(res$data$abundance_index))`.
  Pre-verify cod returns data in CI before committing.

## Appendix: reference patterns

- **Real `getIndices` schema / behaviour:** verified live against `icesDatras`
  1.4.1 (see §2.1) — `getSurveyList()`, `getSurveyYearQuarterList()`,
  `getIndices(survey, year, quarter, species)`.
- **Building block & its conventions:** `R/functions/ices_lookups.R`
  (`lookup_datras_indices` :146-259) — the function to fix.
- **`aphia_id` propagation site:** `R/functions/trait_lookup/orchestrator.R`
  (`aphia_id_for_lookup` :217; offline column :79).
- **Selector-population, rv-driven table, status alert idioms:**
  `R/modules/trait_research_server.R` (`updateSelectInput` :347, `rv` :10,
  `DT::renderDataTable` :522, status `renderUI` :421, clear observer :388,
  double-click guard precedent :983-988).
- **Collapsible full-width box idiom:** `R/ui/trait_research_ui.R` (radar :657,
  offline-DB :671).
- **Error-closure (`warning()`, `<<-`), test (`skip_if`), live-test gating
  conventions:** project `CLAUDE.md` "Trait-Pipeline / Concurrency Conventions";
  the R1 spec `2026-05-27-ices-subdivision-lookup-design.md`.
