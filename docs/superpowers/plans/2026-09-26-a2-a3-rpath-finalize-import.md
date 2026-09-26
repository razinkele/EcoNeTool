# A2 + A3: Rpath TL/Diagnostics and Finalize/Import Attributes Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Stop EcoNeTool overwriting Rpath's trophic levels and mis-reporting Rpath diagnostics (A2), and make every import path (metaweb, EcoBase, EwE native, data editor, trait food web, EMODnet) deliver correct per-vertex attributes to `finalize_network()` (A3). Then release 1.5.0.

**Architecture:** Two PRs on two branches. **Part A2** (`fix/a2-rpath-tl`, off master after B0; independent of A1) removes the transposed TL override, adds `.as_balanced_frame()` so the diagnostics accept the class-`Rpath` list, and extracts a pure `build_rpath_diet_frame()` that keeps cannibalism. **Part A3** (`fix/a3-finalize-import`, off master **after A1 is merged**) adds small pure helpers (`ewe_group_biomass()`, `assign_ewe_functional_groups()`, `metaweb_species_to_info()`, `apply_cell_edit()`, `resolve_emodnet_bbox()`) and wires them into the Shiny modules, so each fix has an offline known-answer test. The last two tasks release 1.5.0 and deploy.

**Tech Stack:** R 4.4.1, Shiny/bs4Dash, igraph, DT, testthat 3, RODBC (Windows EwE reader). Rpath is GitHub-only and **not installed locally**.

**Spec:** `docs/superpowers/specs/2026-09-26-fix-a-network-science-correctness-design.md` (sections A2, A3, 5, 6, 7, 8) and `docs/superpowers/specs/2026-09-26-fix-overview.md`.

## Global Constraints

- Merge order: B0 (1.4.5) -> A2 (any time after B0) ; B0 -> A1 -> **A3** -> release **1.5.0**. A3 must not start until A1 is on master (both touch `metaweb_to_igraph()` and `trait_foodweb.R`).
- Edge contract (A1, binding): `A -> B` means B eats A; `adj[prey, predator]`; diet matrices are prey x predator. A3 must preserve prey -> predator orientation.
- Interfaces from A1 used here, exactly: `assert_prey_to_predator(net, prey, predator)` in `tests/testthat/helper-fixtures.R`; `metaweb_to_igraph()` builds edges `prey_id -> predator_id`; `E(net)$diet_prop` set by CSV/native/EcoBase importers; `calculate_trophic_levels()` returns NA + `warning()` for unreachable nodes; bundled metawebs already migrated.
- Error handlers call `warning()`, never `message()`; use `<<-` for outer-scope mutation inside error closures.
- Paths inside function bodies use `app_path()`; no bare relative `source()` outside `load_all.R`/`app.R`.
- Tests: never `if (cond) expect_*()`; use `skip_if()` / `skip_if_not()` / `skip_if_not_installed()` with a reason. Live API work behind `RUN_LIVE_TESTS=true` with `with_timeout()`.
- Style: `<-` assignment, no tabs, no trailing whitespace, max 120-char lines (lintr config in `.lintr`).
- After editing any `.R` file: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "invisible(parse(file='<path>')); cat('OK\n')"`.
- Single test file: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/<file>')"` from the repo root. Full suite: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_dir('tests/testthat')"` (`tests/run_all_tests.R` cannot run locally: it needs `dggridR`).
- No existing test may regress except the expectation updates listed in Task 1.
- Commit messages end with `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`. A2 commits use `fix(rpath): ...`; A3 uses one `fix(import): ...` commit per finding.
- Line numbers below were read on master @ `4457d9d` (2026-09-26). B0 and A1 move lines. Every edit quotes the exact old text; locate it by that text, not by the line number.

## Review Focus

1. **Duplicate `species_name` in a metaweb** (two rows "Clupea harengus"): export must keep both vertices distinct and aligned with their info rows, not collapse or mis-join. Pinned in Task 7 (`make.unique` test).
2. **Interaction ids missing from the species table** (typo in an uploaded CSV): export must still build the graph, drop only the bad links, and say which ids were dropped. `graph_from_data_frame()` would otherwise abort the whole export. Pinned in Task 7.
3. **EwE database without `Area`, or with `Area` = -9999 / 0**: biomass must fall back to "whole area" (x1), and must not become NA or 0. Pinned in Task 5.
4. **Rpath object that is ragged or not an Rpath object** (a partially built model, or `params` passed by mistake): diagnostics must return NULL with a named warning, not produce garbage or crash the plot. Pinned in Task 1.
5. **Data-editor text that does not fit the column** ("abc" in `meanB`, "Krill" in `fg`): the edit must be rejected with a warning and the old value kept, not silently written as NA. Pinned in Task 10.

## File Structure

| File | Part | Responsibility / change |
|---|---|---|
| `R/functions/rpath/rpath_workflows.R` | A2 | `.as_balanced_frame()`, `.require_balanced_model()` now returns the frame, living = `type < 2`, `n_detritus` |
| `R/functions/rpath/rpath_balancing.R` | A2 | delete `calculate_rpath_trophic_levels()` + TL override; lowercase `type` summary |
| `R/functions/rpath/rpath_conversion.R` | A2, A3 | A2: new pure `build_rpath_diet_frame()` + `check_rpath_diet_sums()`, cannibalism kept. A3: Biomass via `ewe_group_biomass()` |
| `R/modules/rpath_server.R` | A2 | diagnostics box shows detritus count |
| `tests/testthat/test-rpath-tl.R` | A2 | **new** |
| `tests/testthat/test-rpath-diagnostics.R` | A2 | six expectation updates (`type < 2`) |
| `R/functions/ecopath/ecopath_group_biomass.R` | A3 | **new**: `ewe_group_biomass()` with the F52 finding in its header |
| `R/functions/ecopath/load_all.R` | A3 | source the new file |
| `R/functions/network_finalize.R` | A3 | F66 fg vector length |
| `R/functions/metaweb_core.R` | A3 | `metaweb_to_igraph()` name-keyed vertices; **new** `metaweb_species_to_info()` |
| `R/modules/metaweb_manager_server.R` | A3 | export through `metaweb_species_to_info()` |
| `R/functions/ecobase_connection.R`, `R/modules/ecobase_server.R` | A3 | drop proxies; finalize |
| `R/functions/functional_group_utils.R` | A3 | detritus regex; **new** `assign_ewe_functional_groups()` |
| `R/modules/ecopath_import_server.R` | A3 | F52 biomass, F49 Type, F51 EMODnet observer |
| `R/functions/data_editor_utils.R` | A3 | **new**: `apply_cell_edit()`, `non_numeric_info_columns()` |
| `app.R` | A3 | source `data_editor_utils.R` |
| `R/modules/dataeditor_inline_server.R` | A3 | use the helpers |
| `R/functions/euseamap_regional_config.R` | A3 | **new** `resolve_emodnet_bbox()` |
| `R/functions/trait_foodweb.R`, `R/modules/foodweb_construction_server.R` | A3 | species-first vertices; observer `tryCatch` |
| `tests/testthat/test-finalize-network.R` | A3 | F66/F67 tests appended |
| `tests/testthat/test-import-attributes.R` | A3 | **new** |
| `VERSION`, `R/config.R`, `app.R`, `README.md`, `CHANGELOG.md` | release | 1.5.0 |

## Verified facts this plan relies on (recorded 2026-09-26)

**F52 inspection, done while writing this plan.** RODBC and the Access driver work on this machine (R x86_64). `parse_ecopath_native_cross_platform()` on both example files returns `group_data` with columns `GroupID, GroupName, Sequence, Type, Biomass, Area, ProdBiom, ConsBiom, EcoEfficiency, ..., Production, Consumption, ...`.

- There is **no** `BiomassAreaInput`-style column. `Biomass` and `Area` are the only biomass/area fields.
- The derived columns `Production` and `Consumption` are stored as 0, so the table holds **inputs only**.
- The EwE basic-input field is "Biomass in habitat area" (t/km²), and `Area` is the habitat-area fraction. Ecopath's total-area biomass is `Biomass x Area` (EwE user guide, Basic input).
- `Coastal model EE 1.ewemdb` (41 groups) has five groups with `Area < 1`. Four macrozoobenthos groups have 0.2; "Macrozoobenthos filtrators" is `Biomass` 14.26000118 x `Area` 0.2000000030. "Polychaetes" is 4.849999905 x 0.1000000015. "Greater sand-eel (adult)" has `Biomass` -9999.
- `LT2022_0.5ST_final7.eweaccdb` (24 groups) has `Area = 1` everywhere. "Herring " (the name has a trailing space in the DB) is 6.130000114 and "Phytoplankton" is 26.700000763.
- Both files carry the metadata bbox `min_lon` 20.7, `max_lon` 21.1, `min_lat` 55.3, `max_lat` 56.1.

**Chosen semantics:** `ewe_group_biomass()` returns `Biomass x Area`, the total-area biomass. The **network path** (`ecopath_import_server.R:132-140`, which multiplies by Area) was correct. The **Rpath path** (`rpath_conversion.R:204`, which does not) was wrong: Rpath has no habitat-area parameter, so it balanced those five Coastal groups on 5-10x their model-area biomass.

As a result, network-tab biomasses do not change, and Rpath balancing of models with `Area < 1` does change. That change goes under "Results changed" (Task 14). Missing biomass (NA, -9999, negative) stays `NA_real_`, so Ecopath can still estimate it. The network caller applies its own default of 1 afterwards.

**Scratch validation.** Every new test in this plan was run against scratch copies of the repo on 2026-09-26:

- On a copy of current code (with A1's two edge flips simulated), all new tests fail.
- On a copy with the code in this plan applied, all pass. That covers `test-rpath-tl.R` (all non-skipped), the updated `test-rpath-diagnostics.R` (8/8), `test-finalize-network.R` (23/23) and `test-import-attributes.R` (29/29).
- The Rpath-dependent and live tests skip locally.

---

# PART A2 - Rpath TL & diagnostics (branch `fix/a2-rpath-tl`)

### Task 0: Branch setup (A2)

- [ ] **Step 1: Branch from up-to-date master (B0 merged)**

```bash
git checkout master && git pull
git log --oneline -5   # confirm the B0 hotfix commit is present
git checkout -b fix/a2-rpath-tl
```

- [ ] **Step 2: Baseline the suite**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_dir('tests/testthat')"`
Expected: record the pass/fail/skip counts. Every later task compares against them.

### Task 1: F44 - diagnostics accept the class-`Rpath` list; living = type < 2

**Files:**
- Modify: `R/functions/rpath/rpath_workflows.R:302-391` (the whole "BALANCED-MODEL DIAGNOSTICS" section)
- Modify: `R/modules/rpath_server.R:1651` (diagnostics summary table)
- Create: `tests/testthat/test-rpath-tl.R`
- Modify: `tests/testthat/test-rpath-diagnostics.R:48-49, 56, 59, 81, 89, 101`

**Interfaces:**
- Consumes: nothing new.
- Produces:
  - `.as_balanced_frame(model, scope) -> data.frame | NULL`
  - `.require_balanced_model(model, scope) -> data.frame | NULL`. It used to return TRUE/FALSE; it now returns the usable frame.
  - `calculate_ecopath_diagnostics(model)` gains the element `n_detritus`.

Context: `rpath_server.R:1615` and `:1670` pass `rpath_values$ecopath_model`, which is the class-`Rpath` **list** returned by `Rpath::rpath()`. `.require_balanced_model()` rejects anything that is not a data.frame, so in production the diagnostics and the pyramid have always shown "unavailable". The fixture in the existing test is a data.frame, which is why the tests never caught this.

- [ ] **Step 1: Write the failing test file**

Create `tests/testthat/test-rpath-tl.R`:

```r
# =============================================================================
# A2: Rpath trophic levels and diagnostics (F40, F44, F41)
# =============================================================================
# F40 - run_ecopath_balance() overwrote Rpath's TL with a recomputation that
#       read DC[i, j] as "prey j of predator i"; Rpath's DC is prey x
#       predator, so the override was transposed.
# F44 - the diagnostics rejected the class-"Rpath" list the server stores and
#       counted detritus (type 2) as living.
# F41 - the converter zeroed cannibalism without renormalising the column.

source_app_dependencies()

local({
  root <- get_app_root()
  source(file.path(root, "R/functions/rpath/rpath_workflows.R"), local = FALSE)
  source(file.path(root, "R/functions/rpath/rpath_conversion.R"), local = FALSE)
})

# What Rpath::rpath() returns: a LIST of per-group vectors, class "Rpath".
rpath_object <- function() {
  structure(
    list(
      NUM_GROUPS = 5,
      Group = c("Phyto", "Det", "Zoo", "Cod", "Fleet"),
      type = c(1, 2, 0, 0, 3),
      TL = c(1, 1, 2, 3, NA),
      Biomass = c(20, 50, 5, 1, NA),
      PB = c(100, 0, 30, 0.5, NA)
    ),
    class = "Rpath"
  )
}

test_that("diagnostics accept a class-Rpath list and exclude detritus from living", {
  d <- calculate_ecopath_diagnostics(rpath_object())

  expect_false(is.null(d))
  expect_equal(d$n_groups, 3L)
  expect_equal(d$mean_trophic_level, (1 + 2 + 3) / 3)
  expect_equal(d$n_detritus, 1L)
  expect_equal(d$total_biomass, 20 + 5 + 1)
  expect_equal(d$primary_production, 20 * 100)
})

test_that("trophic pyramid accepts a class-Rpath list and excludes detritus", {
  bins <- trophic_pyramid_bins(rpath_object())

  expect_false(is.null(bins))
  expect_equal(sum(bins, na.rm = TRUE), 20 + 5 + 1)  # Det's 50 is not living
  expect_equal(unname(bins[1]), 20)                  # only Phyto at TL 1
})

test_that("an Rpath object with ragged vectors warns and returns NULL", {
  bad <- rpath_object()
  bad$TL <- c(1, 2)
  expect_warning(d <- calculate_ecopath_diagnostics(bad), "5 groups but 2 'TL'")
  expect_null(d)
})

test_that("an unsupported model object warns and returns NULL", {
  expect_warning(d <- calculate_ecopath_diagnostics(list(TL = 1)), "unsupported")
  expect_null(d)
})
```

- [ ] **Step 2: Update the six existing expectations in `tests/testthat/test-rpath-diagnostics.R`**

The fixture `balanced_model()` is Phyto (type 1, B 20, TL 1.0), Detritus (2, 50, 1.0), Zoo (0, 5, 2.1), Cod (0, 1, 3.6) and Fleet (3, NA, NA). With living defined as `type < 2`, Detritus drops out. The spec lists only L48-58; lines 81, 89 and 101 change for the same reason, because `trophic_pyramid_bins()` uses the same living filter.

Replace L48-49:
```r
  # Living groups are type < 3: Phyto 1.0, Detritus 1.0, Zoo 2.1, Cod 3.6
  expect_equal(d$mean_trophic_level, mean(c(1.0, 1.0, 2.1, 3.6)))
```
with
```r
  # Living groups are type < 2: Phyto 1.0, Zoo 2.1, Cod 3.6 (detritus excluded)
  expect_equal(d$mean_trophic_level, mean(c(1.0, 2.1, 3.6)))  # 2.233333
```
Replace L56 `  expect_equal(d$n_groups, 4L)      # type < 3` with `  expect_equal(d$n_groups, 3L)      # type < 2`.
Replace L59 `  expect_equal(d$total_biomass, 20 + 50 + 5 + 1)` with `  expect_equal(d$total_biomass, 20 + 5 + 1)`.
Replace L81 `  expect_equal(sum(bins, na.rm = TRUE), 20 + 50 + 5 + 1)` with `  expect_equal(sum(bins, na.rm = TRUE), 20 + 5 + 1)`.
Replace L89 `  expect_equal(unname(bins[1]), 70)  # Phytoplankton 20 + Detritus 50` with `  expect_equal(unname(bins[1]), 20)  # Phytoplankton only; detritus is not living`.
Replace L101 `  expect_equal(sum(bins, na.rm = TRUE), 70)` with `  expect_equal(sum(bins, na.rm = TRUE), 20)`.

- [ ] **Step 3: Run both files and verify that they fail**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-rpath-tl.R')"`
Expected: FAIL. Test 1 has 6 failures and test 2 has 3; the code warns "[diagnostics] no model supplied" and returns NULL. The ragged and unsupported tests fail because no warning matches.
Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-rpath-diagnostics.R')"`
Expected: FAIL in the tests for mean TL, counts, pyramid sum, base bin and the flat pyramid.

- [ ] **Step 4: Replace the diagnostics section of `R/functions/rpath/rpath_workflows.R`**

Keep L302-310 (the section banner and its comment) and append these lines to that comment:

```r
#
# run_ecopath_balance() returns the class-"Rpath" LIST that Rpath::rpath()
# produces, not a data.frame. .as_balanced_frame() flattens it so both helpers
# accept what the server actually stores in rpath_values$ecopath_model.
# Rpath type codes: 0 consumer, 1 producer, 2 detritus, 3 fleet. "Living"
# means type < 2; detritus is reported separately.
```

Then replace everything from `#' Require a balanced model that actually carries trophic levels` (L312) to the end of the file with:

```r
#' Flatten a balanced model into a per-group data frame
#'
#' @param model A class-"Rpath" list from Rpath::rpath() / run_ecopath_balance(),
#'   or an already-flat data frame.
#' @param scope Caller label used in warnings.
#' @return data.frame(Group, type, TL, Biomass, PB, QB, EE) for an Rpath
#'   object, the input unchanged for a data frame, or NULL (with a warning).
#' @keywords internal
.as_balanced_frame <- function(model, scope) {
  if (is.null(model)) {
    warning(sprintf("[%s] no model supplied", scope), call. = FALSE)
    return(NULL)
  }
  if (is.data.frame(model)) {
    return(model)
  }
  if (!inherits(model, "Rpath")) {
    warning(sprintf("[%s] unsupported model object of class '%s'", scope,
                    paste(class(model), collapse = "/")), call. = FALSE)
    return(NULL)
  }

  # Group, not NUM_GROUPS, fixes the length: every per-group vector in an
  # Rpath object (fleets included) has one entry per Group.
  n <- length(model$Group)
  frame <- data.frame(Group = as.character(model$Group), stringsAsFactors = FALSE)
  for (col in c("type", "TL", "Biomass", "PB", "QB", "EE")) {
    v <- model[[col]]
    if (is.null(v)) {
      # type and TL stay absent so .require_balanced_model() names them;
      # the optional rates become NA.
      if (col %in% c("type", "TL")) next
      v <- rep(NA_real_, n)
    }
    if (length(v) != n) {
      warning(sprintf("[%s] Rpath object has %d groups but %d '%s' values",
                      scope, n, length(v), col), call. = FALSE)
      return(NULL)
    }
    frame[[col]] <- as.numeric(unname(v))
  }
  frame
}

#' Require a balanced model that actually carries trophic levels
#'
#' @param model Balanced model: class-"Rpath" list or data frame.
#' @param scope Caller label used in the warning.
#' @return The model as a data frame when usable; NULL (with a warning)
#'   otherwise.
#' @keywords internal
.require_balanced_model <- function(model, scope) {
  model <- .as_balanced_frame(model, scope)
  if (is.null(model)) {
    return(NULL)
  }
  if (nrow(model) == 0) {
    warning(sprintf("[%s] no model supplied", scope), call. = FALSE)
    return(NULL)
  }
  if (!"TL" %in% names(model)) {
    msg <- paste0(
      "[%s] model has no TL column - this looks like the Rpath params ",
      "object rather than the balanced model from run_ecopath_balance()"
    )
    warning(sprintf(msg, scope), call. = FALSE)
    return(NULL)
  }
  if (!"type" %in% names(model)) {
    warning(sprintf("[%s] model has no lowercase 'type' column", scope),
            call. = FALSE)
    return(NULL)
  }
  model
}

#' Summary diagnostics for a balanced Ecopath model
#'
#' @param model Balanced model from run_ecopath_balance() (class "Rpath") or a
#'   data frame with Group, type, TL, Biomass, PB.
#' @return Named list of metrics, or NULL (with a warning) if `model` is not a
#'   balanced model. Living groups are type < 2; detritus (type 2) is counted
#'   in n_detritus only; fleets (type 3) are ignored.
#' @export
calculate_ecopath_diagnostics <- function(model) {
  model <- .require_balanced_model(model, "diagnostics")
  if (is.null(model)) {
    return(NULL)
  }

  living <- which(model$type < 2)
  producers <- which(model$type == 1)

  list(
    total_biomass = sum(model$Biomass[living], na.rm = TRUE),
    mean_trophic_level = mean(model$TL[living], na.rm = TRUE),
    primary_production = sum(model$Biomass[producers] * model$PB[producers],
                             na.rm = TRUE),
    n_groups = length(living),
    n_producers = length(producers),
    n_consumers = sum(model$type == 0, na.rm = TRUE),
    n_detritus = sum(model$type == 2, na.rm = TRUE)
  )
}

#' Biomass aggregated into half-unit trophic level bins
#'
#' @param model Balanced model from run_ecopath_balance() (class "Rpath") or a
#'   data frame with Group, type, TL, Biomass.
#' @return Named numeric vector of biomass per TL bin for the living groups
#'   (type < 2), lowest bin first, or NULL (with a warning) if `model` is not
#'   a balanced model.
#' @export
trophic_pyramid_bins <- function(model) {
  model <- .require_balanced_model(model, "trophic pyramid")
  if (is.null(model)) {
    return(NULL)
  }

  living <- model[which(model$type < 2), , drop = FALSE]
  tl <- living$TL
  if (length(tl) == 0 || all(is.na(tl))) {
    warning("[trophic pyramid] no non-NA trophic levels", call. = FALSE)
    return(NULL)
  }

  # ceiling(max(TL)) can equal the lower bound (every group at TL 1), which
  # makes seq() a single point and cut() unusable. Always leave one full bin.
  upper <- max(ceiling(max(tl, na.rm = TRUE)), 1.5)
  breaks <- seq(1, upper, by = 0.5)

  # include.lowest keeps the primary producers sitting at exactly TL 1.0 -
  # the base of the pyramid - which cut()'s right-closed default discarded.
  bins <- cut(tl, breaks = breaks, include.lowest = TRUE)
  tapply(living$Biomass, bins, sum, na.rm = TRUE)
}
```

- [ ] **Step 5: Show the detritus count in the diagnostics box**

In `R/modules/rpath_server.R` (inside `output$diagnostics_summary`, ~L1651), after the line
```r
            <tr><td><strong>Consumers:</strong></td><td>", diag$n_consumers, "</td></tr>
```
insert
```r
            <tr><td><strong>Detritus groups (not in living totals):</strong></td><td>", diag$n_detritus, "</td></tr>
```

- [ ] **Step 6: Parse-check and run the tests**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "invisible(parse(file='R/functions/rpath/rpath_workflows.R')); invisible(parse(file='R/modules/rpath_server.R')); cat('OK\n')"`
Then run both test files again.
Expected: `test-rpath-tl.R` has 4 tests passing. `test-rpath-diagnostics.R` passes 8 of 8, including "rejects an unbalanced params object" and "warns and returns NULL without a TL column", which still see the TL warning.

- [ ] **Step 7: Commit**

```bash
git add R/functions/rpath/rpath_workflows.R R/modules/rpath_server.R tests/testthat/test-rpath-tl.R tests/testthat/test-rpath-diagnostics.R
git commit -m "fix(rpath): diagnostics accept the Rpath object; living means type < 2

The server stores the class-Rpath list from Rpath::rpath(); the helpers
rejected anything but a data.frame, so diagnostics and the trophic pyramid
were always unavailable. Detritus (type 2) was counted as living.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

### Task 2: F40 - keep Rpath's TL (delete the transposed override)

**Files:**
- Modify: `R/functions/rpath/rpath_balancing.R:1-113` (header + `calculate_rpath_trophic_levels()`), `:266-290` (override + summary)
- Test: `tests/testthat/test-rpath-tl.R` (append)

**Interfaces:**
- Consumes: nothing.
- Produces: `run_ecopath_balance()` returns `Rpath::rpath()`'s object unmodified. `calculate_rpath_trophic_levels()` no longer exists; `grep` confirmed it has no other callers.

- [ ] **Step 1: Append the failing structural and live tests to `tests/testthat/test-rpath-tl.R`**

```r
test_that("rpath_balancing.R no longer recomputes or overwrites Rpath TL (F40)", {
  code <- readLines(app_path("R/functions/rpath/rpath_balancing.R"), warn = FALSE)
  code <- code[!startsWith(trimws(code), "#")]

  expect_false(any(grepl("calculate_rpath_trophic_levels", code, fixed = TRUE)))
  expect_false(any(grepl("model$TL <-", code, fixed = TRUE)))
  # The balanced object's column is lowercase `type`; `model$Type` after the
  # Rpath::rpath() call always counted 0 living groups.
  expect_true(any(grepl("sum(model$type < 2", code, fixed = TRUE)))
})

test_that("live: Rpath balance of Phyto -> Zoo -> Fish keeps TL 1, 2, 3", {
  skip_if_no_live_tests()
  skip_if_not_installed("Rpath")
  source(app_path("R/functions/rpath/rpath_balancing.R"), local = FALSE)
  ecopath_data <- list(
    group_data = data.frame(
      GroupID = 1:4, GroupName = c("Phyto", "Det", "Zoo", "Fish"), Type = c(1, 2, 0, 0),
      Biomass = c(10, 5, 2, 0.5), ProdBiom = c(100, NA, 20, 1),
      ConsBiom = c(NA, NA, 60, 4), EcoEfficiency = c(NA, NA, NA, NA),
      stringsAsFactors = FALSE
    ),
    diet_data = data.frame(PredID = c(3, 4), PreyID = c(1, 3), Diet = c(1, 1))
  )
  params <- suppressWarnings(convert_ecopath_to_rpath(ecopath_data))
  model <- with_timeout(run_ecopath_balance(params), timeout = 60)
  tl <- setNames(as.numeric(model$TL), model$Group)
  expect_equal(unname(tl[c("Phyto", "Zoo", "Fish")]), c(1, 2, 3), tolerance = 1e-6)
})
```

- [ ] **Step 2: Run and verify the failure**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-rpath-tl.R')"`
Expected: the F40 test FAILS with 3 failures, because the function name, `model$TL <-` and `model$Type` are all still present. The live test SKIPS with "Live API tests disabled" locally.

- [ ] **Step 3: Delete the override and the recomputation**

In `R/functions/rpath/rpath_balancing.R`:

1. In the header, replace L7 `#   - Iterative trophic level calculation for Rpath models` with `#   - Trophic levels exactly as Rpath solves them (never recomputed here)`. Replace L12-14:
```r
# Note: This file contains calculate_rpath_trophic_levels() which is specific
#       to Rpath model objects. This is different from calculate_trophic_levels()
#       in R/functions/trophic_levels.R which works on igraph networks.
```
with
```r
# Trophic levels: Rpath::rpath() solves TL as a linear system over the
#       prey x predator diet matrix (loops and cannibalism included). That TL
#       is kept exactly as Rpath returns it; this file never recomputes it.
```
2. Delete L18-114: the "TROPHIC LEVEL CALCULATION FOR RPATH MODELS" banner, the whole `calculate_rpath_trophic_levels <- function(rpath_model) { ... }`, and the blank line after it. The file continues with the `# ECOPATH MASS-BALANCE MODEL` banner.
3. Delete the override block, which runs from `    # CRITICAL FIX: Recalculate trophic levels using proper iterative algorithm` to the closing `    }` of the `if (n_changed > 0) { ... } else { ... }` (old L266-284).
4. Replace the two summary lines
```r
    message("  Living groups: ", sum(model$Type <= 1))
    message("  Detritus: ", sum(model$Type == 2))
```
with
```r
    message("  Living groups: ", sum(model$type < 2, na.rm = TRUE))
    message("  Detritus: ", sum(model$type == 2, na.rm = TRUE))
```

- [ ] **Step 4: Parse-check, then run the tests**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "invisible(parse(file='R/functions/rpath/rpath_balancing.R')); cat('OK\n')"`
Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-rpath-tl.R')"`
Expected: the F40 test PASSES and the live test SKIPS.

- [ ] **Step 5: Commit**

```bash
git add R/functions/rpath/rpath_balancing.R tests/testthat/test-rpath-tl.R
git commit -m "fix(rpath): keep Rpath's trophic levels

calculate_rpath_trophic_levels() read DC[i, j] as prey j of predator i, but
Rpath's DC is prey x predator, and its result overwrote Rpath's correct,
linearly solved TL. Delete it and the override; fix the lowercase type in
the summary line, which always printed 0 living groups.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

### Task 3: F41 - keep cannibalism; warn when a diet column sums to more than 1

**Files:**
- Modify: `R/functions/rpath/rpath_conversion.R` (new helpers after `apply_diet_cell_edit()`, which ends at L58; replace L263-368; delete L459-466)
- Test: `tests/testthat/test-rpath-tl.R` (append)

**Interfaces:**
- Consumes: nothing.
- Produces:
  - `build_rpath_diet_frame(living_groups, diet) -> data.frame(Group, <predator columns>)`
  - `check_rpath_diet_sums(diet_df, tol = 1e-6) -> invisible(character)`
  - Both are pure; neither needs Rpath.

**Deviation (reason):** The spec's test 4 wants `convert_ecopath_to_rpath()` on a cannibal fixture, with only the create-params step skipped when Rpath is missing. That is impossible as written: `convert_ecopath_to_rpath()` calls `check_rpath_installed()` on its first line. The diet-frame construction is therefore extracted into a pure helper, which runs and fails or passes offline. The whole-converter test is kept behind `skip_if_not_installed("Rpath")`. The existing `> 1.01` check (L459-466) is replaced by the stricter `1 + 1e-6` check inside the helper, not duplicated.

- [ ] **Step 1: Append the failing tests to `tests/testthat/test-rpath-tl.R`**

```r
test_that("rpath_conversion.R no longer strips cannibalism (F41)", {
  code <- readLines(app_path("R/functions/rpath/rpath_conversion.R"), warn = FALSE)
  expect_false(any(grepl("Removing cannibalism", code, fixed = TRUE)))
})

# EwE-style tables: Phyto(1), Det(2), Zoo(0), Cod(0), DummyFleet(3).
# Zoo eats Phyto 0.7 + Det 0.3; Cod eats Zoo 0.9 + Cod 0.1 (cannibalism).
ewe_groups <- function() {
  data.frame(
    GroupID = 1:5,
    GroupName = c("Phyto", "Det", "Zoo", "Cod", "DummyFleet"),
    Type = c(1, 2, 0, 0, 3),
    stringsAsFactors = FALSE
  )
}
ewe_diet <- function(cod_on_cod = 0.1) {
  data.frame(
    PredID = c(3, 3, 4, 4),
    PreyID = c(1, 2, 3, 4),
    Diet = c(0.7, 0.3, 1 - cod_on_cod, cod_on_cod)
  )
}

test_that("build_rpath_diet_frame keeps cannibalism and the column sums to 1 (F41)", {
  diet_df <- build_rpath_diet_frame(ewe_groups(), ewe_diet())

  expect_equal(diet_df$Group, c("Phyto", "Det", "Zoo", "Cod", "Import"))
  expect_equal(names(diet_df), c("Group", "Phyto", "Zoo", "Cod"))
  expect_equal(diet_df$Cod[diet_df$Group == "Cod"], 0.1)
  expect_equal(sum(diet_df$Cod), 1)
  expect_equal(sum(diet_df$Zoo), 1)
})

test_that("a diet column summing to more than 1 warns with the group name", {
  over <- ewe_diet()
  over$Diet[over$PredID == 3 & over$PreyID == 1] <- 0.8  # Zoo sums to 1.1
  expect_warning(diet_df <- build_rpath_diet_frame(ewe_groups(), over), "'Zoo' sums to 1.1")
  # The data are reported, never changed.
  expect_equal(diet_df$Zoo[diet_df$Group == "Phyto"], 0.8)
})

test_that("a diet column summing to exactly 1 does not warn", {
  expect_no_warning(build_rpath_diet_frame(ewe_groups(), ewe_diet()))
})

test_that("convert_ecopath_to_rpath passes cannibalism through to params$diet", {
  skip_if_not_installed("Rpath")
  ecopath_data <- list(
    group_data = transform(ewe_groups()[1:4, ],
                           Biomass = c(20, 50, 5, 1), ProdBiom = c(100, NA, 30, 0.5),
                           ConsBiom = c(NA, NA, 100, 3), EcoEfficiency = c(NA, NA, NA, NA)),
    diet_data = ewe_diet()
  )
  params <- suppressWarnings(convert_ecopath_to_rpath(ecopath_data))
  expect_equal(params$diet$Cod[params$diet$Group == "Cod"], 0.1)
  expect_equal(sum(params$diet$Cod), 1)
})
```

- [ ] **Step 2: Run and verify the failure**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-rpath-tl.R')"`
Expected: "no longer strips cannibalism" FAILS. The three `build_rpath_diet_frame` tests ERROR with `could not find function "build_rpath_diet_frame"`. The converter test SKIPS because Rpath is not installed.

- [ ] **Step 3: Add the pure helpers**

In `R/functions/rpath/rpath_conversion.R`, insert after the closing `}` of `apply_diet_cell_edit()` (L58) and before the `# PACKAGE CHECK AND INSTALLATION` banner:

```r
# ==============================================================================
# DIET-MATRIX CONSTRUCTION (pure: no Rpath needed)
# ==============================================================================

#' Build the Rpath diet data frame from EwE group and diet tables
#'
#' Rows are prey groups (Type < 3) plus "Import"; columns are "Group" followed
#' by one column per predator (Type < 2). Cell [prey, predator] is the diet
#' proportion; absent links are 0. Cannibalism (a group eating itself) is
#' KEPT as entered: Rpath solves trophic levels as a linear system, so a
#' self-loop is well defined, and zeroing it without renormalising left the
#' predator's diet summing to less than 1.
#'
#' @param living_groups EwE group table (GroupName, Type, GroupID), fleets and
#'   any dummy fleet already appended.
#' @param diet EwE diet table with PredID, PreyID, Diet (may be NULL/empty).
#' @return data.frame with a "Group" column plus one numeric column per
#'   predator.
#' @export
build_rpath_diet_frame <- function(living_groups, diet) {
  # Get predator groups (Type < 2: consumers and producers)
  predator_groups <- living_groups[!is.na(living_groups$Type) & living_groups$Type < 2, ]

  # Get prey groups (Type < 3: all except fleets)
  prey_groups <- living_groups[!is.na(living_groups$Type) & living_groups$Type < 3, ]

  if (nrow(predator_groups) == 0) {
    stop("No predator groups found (Type < 2). Check that Type column is correctly set.")
  }
  if (nrow(prey_groups) == 0) {
    stop("No prey groups found (Type < 3). Check that Type column is correctly set.")
  }

  # Rows: prey + Import, Columns: Group + predators
  diet_df <- data.frame(Group = c(prey_groups$GroupName, "Import"), stringsAsFactors = FALSE)
  for (pred_name in predator_groups$GroupName) {
    diet_df[[pred_name]] <- NA_real_
  }

  if (!is.null(diet) && nrow(diet) > 0) {
    required_diet_cols <- c("PredID", "PreyID", "Diet")
    missing_diet_cols <- setdiff(required_diet_cols, names(diet))
    if (length(missing_diet_cols) > 0) {
      stop("diet_data missing required columns: ", paste(missing_diet_cols, collapse = ", "),
           "\nAvailable columns: ", paste(names(diet), collapse = ", "))
    }

    for (i in seq_len(nrow(diet))) {
      pred_id <- diet$PredID[i]
      prey_id <- diet$PreyID[i]
      diet_val <- diet$Diet[i]
      if (is.na(pred_id) || is.na(prey_id) || is.na(diet_val)) next

      pred_match <- predator_groups[predator_groups$GroupID == pred_id, ]
      prey_match <- prey_groups[prey_groups$GroupID == prey_id, ]
      if (nrow(pred_match) == 0 || nrow(prey_match) == 0) next

      pred_name <- pred_match$GroupName[1]
      prey_name <- prey_match$GroupName[1]
      prey_row <- which(diet_df$Group == prey_name)

      if (length(prey_row) == 1 && pred_name %in% colnames(diet_df)) {
        diet_df[prey_row, pred_name] <- diet_val
      } else if (length(prey_row) > 1) {
        warning("Multiple matches found for prey: ", prey_name, ". Using first match.",
                call. = FALSE)
        diet_df[prey_row[1], pred_name] <- diet_val
      }
    }
  }

  # Rpath can't handle NA values in the diet matrix: no link = 0.
  for (col in setdiff(colnames(diet_df), "Group")) {
    diet_df[[col]][is.na(diet_df[[col]])] <- 0.0
  }

  check_rpath_diet_sums(diet_df)
  diet_df
}

#' Warn about predator diet columns summing to more than 1
#'
#' The data are not changed: an over-full diet is a data-entry problem the
#' user must fix, and silently rescaling it would hide that.
#'
#' @param diet_df Diet data frame from build_rpath_diet_frame().
#' @param tol Tolerance above 1 before warning (default 1e-6).
#' @return Invisibly, the names of the offending predator columns.
#' @export
check_rpath_diet_sums <- function(diet_df, tol = 1e-6) {
  cols <- setdiff(names(diet_df), "Group")
  sums <- vapply(cols, function(col) sum(diet_df[[col]], na.rm = TRUE), numeric(1))
  over <- cols[sums > 1 + tol]
  for (col in over) {
    warning(sprintf("[rpath conversion] diet of '%s' sums to %.6f (> 1); data left unchanged",
                    col, sums[[col]]), call. = FALSE)
  }
  invisible(over)
}
```

- [ ] **Step 4: Call the helper from `convert_ecopath_to_rpath()` and remove the old code**

Replace everything from `  # Rpath diet format requirements:` (old L263) to the blank line after the `if (cannibalism_found) { ... } else { ... }` block (old L368). Those lines are the predator/prey selection, the fill loop, NA -> 0 and the whole "CRITICAL FIX: Remove cannibalism" section. Put this in their place:

```r
  diet_df <- build_rpath_diet_frame(living_groups, diet)

```

The line `  # Convert to data.table (Rpath requires data.table, not data.frame)` with `params$diet <- data.table::as.data.table(diet_df)` follows unchanged.

Then delete the now-duplicate sum check near the end of the function (old L459-466, plus one of the two blank lines around it):

```r
  # Check diet matrix has valid values
  diet_cols <- names(params$diet)[names(params$diet) != "Group"]
  for (col in diet_cols) {
    diet_sum <- sum(params$diet[[col]], na.rm = TRUE)
    if (diet_sum > 1.01) {
      warning("Predator '", col, "' has diet sum > 1.0 (", round(diet_sum, 3), ")")
    }
  }
```

- [ ] **Step 5: Parse-check and run the tests**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "invisible(parse(file='R/functions/rpath/rpath_conversion.R')); cat('OK\n')"`
Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-rpath-tl.R')"`
Expected: 9 tests PASS and 2 SKIP (the converter needs Rpath; the live test needs RUN_LIVE_TESTS).

- [ ] **Step 6: Commit**

```bash
git add R/functions/rpath/rpath_conversion.R tests/testthat/test-rpath-tl.R
git commit -m "fix(rpath): keep cannibalism; warn on diet columns summing above 1

Self-diet was set to 0 without renormalising, so the predator's diet summed
to less than 1, and the user was told only via message(). Extract the diet
frame into build_rpath_diet_frame() (pure, testable without Rpath), keep the
data as entered, and warn() with the group name when a column exceeds 1+1e-6.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

### Task 4: A2 full suite and PR

- [ ] **Step 1: Full suite**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_dir('tests/testthat')"`
Expected: no new failures against the Task 0 baseline. The only intended changes are the six expectation updates in `test-rpath-diagnostics.R`, plus the new file.

- [ ] **Step 2: Lint the touched files**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "for (f in c('R/functions/rpath/rpath_workflows.R','R/functions/rpath/rpath_balancing.R','R/functions/rpath/rpath_conversion.R','tests/testthat/test-rpath-tl.R')) print(lintr::lint(f))"`
Expected: no new lints on changed lines.

- [ ] **Step 3: Push and open the PR**

```bash
git push -u origin fix/a2-rpath-tl
gh pr create --base master --title "fix(rpath): keep Rpath TL, accept Rpath object, keep cannibalism" --body "$(cat <<'EOF'
Spec A2 (F40, F44, F41) - docs/superpowers/specs/2026-09-26-fix-a-network-science-correctness-design.md

- F40: delete calculate_rpath_trophic_levels() and the TL override; Rpath's TL is kept.
- F44: diagnostics/pyramid accept the class-Rpath list; living = type < 2; n_detritus reported.
- F41: cannibalism kept; warning() when a diet column sums above 1 + 1e-6.

Expectation updates in test-rpath-diagnostics.R (type < 3 -> type < 2): L48-49, 56, 59, 81, 89, 101.
Deviation: diet-frame construction extracted to build_rpath_diet_frame() so the cannibalism test runs without Rpath.

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

---

# PART A3 - Finalize & import (branch `fix/a3-finalize-import`)

### Task 5 (FIRST): F52 - one EwE biomass definition for the network and Rpath paths

**Files:**
- Create: `R/functions/ecopath/ecopath_group_biomass.R`
- Modify: `R/functions/ecopath/load_all.R` (source it; "5 files" -> "6 files")
- Modify: `R/modules/ecopath_import_server.R:98` and `:126-140`
- Modify: `R/functions/rpath/rpath_conversion.R:204` (`params$model$Biomass <- ...`)
- Modify: `tests/testthat/test-rpath-tl.R` (source the helper)
- Create: `tests/testthat/test-import-attributes.R`

**Interfaces:**
- Consumes: `parse_ecopath_native_cross_platform(db_file)` (existing), used only by the example-file test.
- Produces: `ewe_group_biomass(group_table, biomass_col = "Biomass", area_col = "Area") -> numeric`. The result is `Biomass x Area`. Missing or sentinel biomass gives `NA_real_`; missing, sentinel or non-positive Area counts as 1.

- [ ] **Step 1: Branch from master with A1 merged, and re-run the F52 inspection**

```bash
git checkout master && git pull
git log --oneline -15   # confirm the A1 commit "fix(network)!: unify prey->predator edge contract" is present
git checkout -b fix/a3-finalize-import
```

Re-confirm the recorded finding (Windows with RODBC). This is the spec's "A3 step 1":

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "
suppressMessages({source('R/functions/validation_utils.R'); for (f in c('ecopath_windows.R','ecopath_unix.R','ecopath_import.R')) source(file.path('R/functions/ecopath', f))})
for (f in c('examples/Coastal model EE 1.ewemdb', 'examples/LT2022_0.5ST_final7.eweaccdb')) {
  g <- suppressMessages(parse_ecopath_native_cross_platform(f))\$group_data
  cat(f, '\n'); print(grep('area|biom', names(g), ignore.case = TRUE, value = TRUE))
  print(g[g\$Area < 1, c('GroupName', 'Biomass', 'Area')], digits = 10)
  cat('Production/Consumption all zero:', all(g\$Production == 0), all(g\$Consumption == 0), '\n')
}"
```
Expected, matching "Verified facts" above:
- The columns found are `Biomass Area ProdBiom ConsBiom BiomAcc BiomAccRate RespBiom`, with no `BiomassAreaInput`.
- Coastal lists 5 groups with Area 0.2/0.1; LT2022 lists none.
- `Production`/`Consumption` are all zero.

**Branch on the result.** If the output differs, for example if a total-area biomass column exists, stop. Record the new finding in this task, and re-derive the semantics before continuing: use that column directly, with no x Area, in both paths.

- [ ] **Step 2: Write the failing test file (F52 part)**

Create `tests/testthat/test-import-attributes.R`:

```r
# =============================================================================
# A3: import attribute layer (F50, F49, F52, F68, F74, F51)
# =============================================================================

source_app_dependencies()

local({
  root <- get_app_root()
  source(file.path(root, "R/config.R"), local = FALSE)
  source(file.path(root, "R/functions/network_finalize.R"), local = FALSE)
  source(file.path(root, "R/functions/ecopath/ecopath_group_biomass.R"), local = FALSE)
})

# ---------------------------------------------------------------------------
# F52 - one biomass definition for the network and Rpath paths
# ---------------------------------------------------------------------------

test_that("ewe_group_biomass multiplies habitat-area biomass by Area (F52)", {
  groups <- data.frame(
    GroupName = c("Filtrators", "Polychaetes", "Fish", "Unknown B", "No area"),
    Biomass = c(14.26, 4.85, 0.5, -9999, 2),
    Area = c(0.2, 0.1, 1, 1, -9999),
    stringsAsFactors = FALSE
  )
  expect_equal(ewe_group_biomass(groups), c(2.852, 0.485, 0.5, NA, 2))
})

test_that("ewe_group_biomass treats an absent Area column as 1", {
  groups <- data.frame(GroupName = "A", Biomass = 3)
  expect_equal(ewe_group_biomass(groups), 3)
})

test_that("ewe_group_biomass warns on a non-positive habitat area", {
  groups <- data.frame(GroupName = "Zero", Biomass = 3, Area = 0)
  expect_warning(b <- ewe_group_biomass(groups), "Zero")
  expect_equal(b, 3)
})

read_example_groups <- function(file) {
  path <- app_path("examples", file)
  skip_if_not(file.exists(path), paste("example database not found:", file))
  root <- get_app_root()
  for (f in c("ecopath_windows.R", "ecopath_unix.R", "ecopath_import.R")) {
    source(file.path(root, "R/functions/ecopath", f), local = FALSE)
  }
  result <- tryCatch(
    suppressMessages(parse_ecopath_native_cross_platform(path)),
    error = function(e) NULL
  )
  skip_if(is.null(result),
          "no Access reader available (RODBC + Access driver on Windows, mdbtools on Linux)")
  result$group_data
}

test_that("ewe_group_biomass known answers on the Coastal model example (F52)", {
  g <- read_example_groups("Coastal model EE 1.ewemdb")
  b <- setNames(ewe_group_biomass(g), trimws(g$GroupName))

  expect_equal(unname(b["Macrozoobenthos filtrators"]), 14.26 * 0.2, tolerance = 1e-6)
  expect_equal(unname(b["Polychaetes"]), 4.85 * 0.1, tolerance = 1e-6)
  expect_equal(unname(b["Phytoplankton"]), 5.199, tolerance = 1e-6)
  expect_true(is.na(b["Greater sand-eel (adult)"]))  # -9999: left for Ecopath to estimate
})

test_that("ewe_group_biomass known answers on the LT2022 example (F52)", {
  g <- read_example_groups("LT2022_0.5ST_final7.eweaccdb")
  b <- setNames(ewe_group_biomass(g), trimws(g$GroupName))

  expect_equal(unname(b["Herring"]), 6.13, tolerance = 1e-6)  # Area = 1 throughout
  expect_equal(unname(b["Phytoplankton"]), 26.7, tolerance = 1e-6)
})

test_that("both biomass paths call ewe_group_biomass (F52)", {
  server <- readLines(app_path("R/modules/ecopath_import_server.R"), warn = FALSE)
  rpath <- readLines(app_path("R/functions/rpath/rpath_conversion.R"), warn = FALSE)
  expect_true(any(grepl("ewe_group_biomass(", server, fixed = TRUE)))
  expect_true(any(grepl("ewe_group_biomass(living_groups)", rpath, fixed = TRUE)))
  expect_false(any(grepl("biomass_values * area_proportions", server, fixed = TRUE)))
})
```

- [ ] **Step 3: Run and verify the failure**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-import-attributes.R')"`
Expected: FAIL. The file errors at `source(... ecopath_group_biomass.R)` with "cannot open file". Once the file exists and is empty, the helper tests error with `could not find function "ewe_group_biomass"` and the structural test fails 3 expectations.

- [ ] **Step 4: Create `R/functions/ecopath/ecopath_group_biomass.R`**

```r
# ==============================================================================
# ECOPATH GROUP BIOMASS
# ==============================================================================
# One definition of "group biomass" shared by the network import
# (ecopath_import_server.R) and the Rpath conversion (rpath_conversion.R), so
# the two paths agree by construction.
#
# EwE field semantics (verified 2026-09-26 against
# examples/Coastal model EE 1.ewemdb and examples/LT2022_0.5ST_final7.eweaccdb
# read through parse_ecopath_native_cross_platform()):
#   - EcopathGroup carries `Biomass` and `Area`; there is no separate
#     total-area biomass column (no BiomassAreaInput or similar), and the
#     derived columns (`Production`, `Consumption`) are stored as 0, i.e. the
#     table holds the user's INPUTS only.
#   - The EwE basic-input field is "Biomass in habitat area" (t/km2) and
#     `Area` is the habitat-area fraction (0-1) of the model area. Ecopath's
#     total-area biomass is Biomass x Area.
#   - Coastal model EE 1 has five groups with Area < 1 (four macrozoobenthos
#     groups at 0.2, Polychaetes at 0.1); LT2022 has Area = 1 throughout.
# Rpath has no habitat-area parameter, so it must be given the total-area
# biomass. Before this helper the network path multiplied by Area and the
# Rpath path did not, so Rpath balanced those five groups on 5-10x their
# model-area biomass.
# ==============================================================================

#' Total-area biomass of each EwE group
#'
#' @param group_table EwE group table (e.g. `group_data` from
#'   parse_ecopath_native_cross_platform()).
#' @param biomass_col Name of the biomass-in-habitat-area column.
#' @param area_col Name of the habitat-area fraction column; if absent (or
#'   NULL) every group is taken to occupy the whole model area.
#' @return Numeric vector, one value per row: Biomass x Area. Missing biomass
#'   (NA, the EwE -9999 sentinel, or negative) stays NA_real_ so Ecopath can
#'   estimate it; callers apply their own defaults. A missing, sentinel or
#'   non-positive Area counts as 1.
#' @export
ewe_group_biomass <- function(group_table, biomass_col = "Biomass", area_col = "Area") {
  if (is.null(group_table) || !biomass_col %in% names(group_table)) {
    stop(sprintf("ewe_group_biomass(): group table has no '%s' column", biomass_col),
         call. = FALSE)
  }
  biomass <- suppressWarnings(as.numeric(group_table[[biomass_col]]))
  biomass[is.na(biomass) | biomass < 0] <- NA_real_

  area <- if (!is.null(area_col) && area_col %in% names(group_table)) {
    suppressWarnings(as.numeric(group_table[[area_col]]))
  } else {
    rep(1, length(biomass))
  }
  bad_area <- !is.na(area) & area > -9000 & area <= 0
  if (any(bad_area)) {
    names_col <- intersect(c("GroupName", "Group"), names(group_table))
    labels <- if (length(names_col) > 0) group_table[[names_col[1]]][bad_area] else which(bad_area)
    warning(sprintf("[ewe_group_biomass] non-positive habitat area treated as 1 for: %s",
                    paste(labels, collapse = ", ")), call. = FALSE)
  }
  area[is.na(area) | area <= 0] <- 1

  biomass * area
}
```

In `R/functions/ecopath/load_all.R`, after `source("R/functions/ecopath/ecopath_csv.R")`, add:
```r
# Shared EwE group-table helpers (biomass semantics, F52)
source("R/functions/ecopath/ecopath_group_biomass.R")
```
and change the last line to `message("✓ ECOPATH import functions loaded (6 files)")`.

- [ ] **Step 5: Route the network path through the helper (`R/modules/ecopath_import_server.R`)**

Replace L98:
```r
      biomass_values <- if (!is.na(biomass_col)) as.numeric(group_table[[biomass_col]]) else rep(1, length(species_names))
```
with
```r
      # Habitat-area fraction column (EwE "Area"); ewe_group_biomass() turns
      # biomass-in-habitat-area into total-area biomass, exactly as the Rpath
      # conversion does.
      area_col <- which(grepl("^area$|habitat.*prop|^habarea$|^habprop$", col_names))[1]
      biomass_values <- if (!is.na(biomass_col)) {
        ewe_group_biomass(group_table, biomass_col = col_names_orig[biomass_col],
                          area_col = if (!is.na(area_col)) col_names_orig[area_col] else NULL)
      } else {
        rep(1, length(species_names))
      }
```
Then replace the block from `      # Apply habitat area proportion if available` through its closing `      }` (old L129-140, the one containing `biomass_values <- biomass_values * area_proportions`) with:
```r
      if (!is.na(area_col)) {
        message("  Biomass = Biomass in habitat area x ", col_names_orig[area_col], " (ewe_group_biomass)")
      }
```
`biomass_values <- clean_ecopath_value(biomass_values, 1, "Biomass")` stays in place (old L127, directly above that block). It turns the helper's NA into the network default of 1.

- [ ] **Step 6: Route the Rpath path through the helper**

In `R/functions/rpath/rpath_conversion.R` replace
```r
  params$model$Biomass <- clean_ecopath_missing(living_groups$Biomass)
```
with
```r
  # Total-area biomass (Biomass in habitat area x Area); see ewe_group_biomass().
  params$model$Biomass <- ewe_group_biomass(living_groups)
```
In `tests/testthat/test-rpath-tl.R`, inside the top `local({ ... })`, add:
```r
  source(file.path(root, "R/functions/ecopath/ecopath_group_biomass.R"), local = FALSE)
```
This lets the Rpath-gated converter test find the helper.

- [ ] **Step 7: Parse-check and run the tests**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "for (f in c('R/functions/ecopath/ecopath_group_biomass.R','R/functions/ecopath/load_all.R','R/modules/ecopath_import_server.R','R/functions/rpath/rpath_conversion.R')) invisible(parse(file=f)); cat('OK\n')"`
Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-import-attributes.R')"` and the same for `test-rpath-tl.R`.
Expected: all six F52 tests PASS on Windows. On Linux CI without mdbtools the two example-file tests SKIP with the reason shown.

- [ ] **Step 8: Commit**

```bash
git add R/functions/ecopath/ecopath_group_biomass.R R/functions/ecopath/load_all.R R/modules/ecopath_import_server.R R/functions/rpath/rpath_conversion.R tests/testthat/test-import-attributes.R tests/testthat/test-rpath-tl.R
git commit -m "fix(import): one EwE biomass definition for network and Rpath (F52)

EcopathGroup.Biomass is biomass in habitat area (verified on both example
DBs: inputs only, no total-area column). The network path multiplied by
Area; the Rpath path did not, so Rpath balanced Area<1 groups on 5-10x
their model-area biomass. Both now call ewe_group_biomass().

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

### Task 6: F66 - infer fg per vertex in `finalize_network()`

**Files:**
- Modify: `R/functions/network_finalize.R:102`
- Test: `tests/testthat/test-finalize-network.R` (append)

**Interfaces:**
- Consumes: `assign_functional_groups(species_names)` (existing).
- Produces: `finalize_network()`. Its signature is unchanged, but it now infers one fg per vertex.

- [ ] **Step 1: Append the failing test to `tests/testthat/test-finalize-network.R`**

```r

# ---------------------------------------------------------------------------
# F66 - fg inferred per vertex, not recycled from the first one
# ---------------------------------------------------------------------------

test_that("finalize_network infers fg per vertex when info has no fg column (F66)", {
  net <- make_net(c("Gadus morhua", "Calanus finmarchicus", "Diatoma"))
  info <- data.frame(species = c("Gadus morhua", "Calanus finmarchicus", "Diatoma"),
                     meanB = c(1, 2, 3), stringsAsFactors = FALSE)

  out <- finalize_network(net, info)

  expect_equal(as.character(out$info$fg), c("Fish", "Zooplankton", "Phytoplankton"))
  expect_equal(length(unique(out$info$colfg)), 3L)
})
```

- [ ] **Step 2: Run and verify the failure**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-finalize-network.R')"`
Expected: FAIL with 2 failures. Every fg is "Fish": a length-1 `NA_character_` receives the first inferred value and is then recycled.

- [ ] **Step 3: Fix L102**

Replace
```r
  fg_char <- if ("fg" %in% names(aligned)) as.character(aligned$fg) else NA_character_
```
with
```r
  fg_char <- if ("fg" %in% names(aligned)) {
    as.character(aligned$fg)
  } else {
    rep(NA_character_, length(vertex_names))
  }
```

- [ ] **Step 4: Parse-check and run the tests**

Run the parse check on `R/functions/network_finalize.R`, then the test file.
Expected: all PASS; the existing 16 tests are unchanged.

- [ ] **Step 5: Commit**

```bash
git add R/functions/network_finalize.R tests/testthat/test-finalize-network.R
git commit -m "fix(import): finalize_network infers fg per vertex (F66)

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

### Task 7: F67 - metaweb export keeps its real attributes

**Files:**
- Modify: `R/functions/metaweb_core.R:127-156` (`metaweb_to_igraph()`, as left by A1) and add `metaweb_species_to_info()` directly after it
- Modify: `R/modules/metaweb_manager_server.R:456`
- Test: `tests/testthat/test-finalize-network.R` (append)

**Interfaces:**
- Consumes:
  - `assert_prey_to_predator(net, prey, predator)` (A1, `helper-fixtures.R`)
  - `get_functional_group_levels()`
  - `finalize_network(net, info)`
- Produces:
  - `metaweb_to_igraph(metaweb) -> igraph`. Vertex names are `make.unique(species_name)`, `V(g)$species_id` is kept, edges run prey -> predator, and unknown ids are dropped with `warning()`.
  - `metaweb_species_to_info(species) -> data.frame` with columns `species` plus whichever of `fg, meanB, bodymasses, met.types, efficiencies, PB, QB, species_id` exist.

**Deviation (reason):** The spec's column map lists six columns. `pb_ratio -> PB` and `qb_ratio -> QB` are added as well, because the EwE->metaweb writer (`ecopath_import_server.R` ~L1759-1760) stores P/B and Q/B under those names, and A1's MTI uses `info$QB` when present. Without the mapping an EwE-derived metaweb would silently fall back to the biomass proxy.

- [ ] **Step 1: Append the failing tests to `tests/testthat/test-finalize-network.R`**

```r

# ---------------------------------------------------------------------------
# F67 - metaweb export keeps its real attributes
# ---------------------------------------------------------------------------

local({
  root <- get_app_root()
  source(file.path(root, "R/functions/metaweb_core.R"), local = FALSE)
})

template_metaweb <- function() {
  create_metaweb(
    species = data.frame(
      species_id = c("SP001", "SP002", "SP003"),
      species_name = c("Gadus morhua", "Clupea harengus", "Calanus finmarchicus"),
      functional_group = c("Fish", "Fish", "Zooplankton"),
      biomass = c(1, 5, 20),
      stringsAsFactors = FALSE
    ),
    interactions = data.frame(
      predator_id = c("SP001", "SP002"),
      prey_id = c("SP002", "SP003"),
      stringsAsFactors = FALSE
    )
  )
}

test_that("metaweb_to_igraph names vertices by species name, prey -> predator (F67)", {
  g <- metaweb_to_igraph(template_metaweb())

  expect_equal(igraph::V(g)$name, c("Gadus morhua", "Clupea harengus", "Calanus finmarchicus"))
  expect_equal(igraph::V(g)$species_id, c("SP001", "SP002", "SP003"))
  assert_prey_to_predator(g, "Clupea harengus", "Gadus morhua")
  assert_prey_to_predator(g, "Calanus finmarchicus", "Clupea harengus")
})

test_that("metaweb_to_igraph keeps duplicate species names distinct", {
  mw <- template_metaweb()
  mw$species$species_name[3] <- "Clupea harengus"
  g <- metaweb_to_igraph(mw)

  expect_equal(igraph::V(g)$name, c("Gadus morhua", "Clupea harengus", "Clupea harengus.1"))
  expect_equal(igraph::ecount(g), 2L)
  info <- metaweb_species_to_info(mw$species)
  expect_equal(info$species, igraph::V(g)$name)
})

test_that("metaweb_to_igraph drops links to unknown ids with a warning", {
  mw <- template_metaweb()
  mw$interactions <- rbind(mw$interactions,
                           data.frame(predator_id = "SP001", prey_id = "SP999",
                                      quality_code = 1, source = "x"))
  expect_warning(g <- metaweb_to_igraph(mw), "SP999")
  expect_equal(igraph::ecount(g), 2L)
})

test_that("metaweb_species_to_info maps the metaweb columns onto info columns", {
  species <- data.frame(
    species_id = c("SP001", "SP002"), species_name = c("Cod", "Mixed plankton"),
    functional_group = c("Fish", "pelagic"), biomass = c(2.5, 7),
    body_mass = c(1000, 0.001), metabolic_type = c("ectotherm vertebrates", "invertebrates"),
    efficiency = c(0.85, 0.75), stringsAsFactors = FALSE
  )

  info <- metaweb_species_to_info(species)

  expect_equal(info$species, c("Cod", "Mixed plankton"))
  expect_equal(info$fg, c("Fish", NA))  # "pelagic" is not canonical -> inferred later
  expect_equal(info$meanB, c(2.5, 7))
  expect_equal(info$bodymasses, c(1000, 0.001))
  expect_equal(info$met.types, c("ectotherm vertebrates", "invertebrates"))
  expect_equal(info$efficiencies, c(0.85, 0.75))
  expect_equal(info$species_id, c("SP001", "SP002"))
})

test_that("the bundled Baltic metaweb exports with real biomass and several fg (F67)", {
  rds <- app_path("metawebs/baltic/baltic_kortsch2021.rds")
  skip_if_not(file.exists(rds), "Baltic metaweb .rds not found")
  mw <- readRDS(rds)

  net <- metaweb_to_igraph(mw)
  out <- finalize_network(net, metaweb_species_to_info(mw$species))

  expect_equal(out$info$species, make.unique(mw$species$species_name))
  expect_equal(out$info$meanB, as.numeric(mw$species$biomass))
  expect_false(all(out$info$meanB == 1))
  expect_gt(length(unique(as.character(out$info$fg))), 1)
})

test_that("metaweb_manager_server.R exports through metaweb_species_to_info (F67)", {
  code <- readLines(app_path("R/modules/metaweb_manager_server.R"), warn = FALSE)
  code <- code[!startsWith(trimws(code), "#")]
  expect_true(any(grepl("finalize_network(new_net, metaweb_species_to_info(", code, fixed = TRUE)))
})
```

- [ ] **Step 2: Run and verify the failure**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-finalize-network.R')"`
Expected:
- the vertex-name test FAILS because the names are `SP001...`;
- the duplicate-name, unknown-id, `metaweb_species_to_info` and Baltic tests ERROR, either with `could not find function "metaweb_species_to_info"` or because `graph_from_data_frame` aborts on the unknown id;
- the structural test FAILS.

- [ ] **Step 3: Replace `metaweb_to_igraph()` and add `metaweb_species_to_info()`**

In `R/functions/metaweb_core.R`, replace everything from `#' Convert metaweb to igraph network` to the closing `}` of `metaweb_to_igraph` (just before `#' Get link quality description`) with:

```r
#' Convert metaweb to igraph network
#'
#' Edge contract (see R/functions/network_finalize.R): an edge A -> B means B
#' eats A, so edges run prey_id -> predator_id.
#'
#' Vertices are named by species NAME (made unique with make.unique()), so the
#' graph joins by name with an info frame from metaweb_species_to_info() in
#' finalize_network(). The metaweb id is kept as V(g)$species_id. Interaction
#' ids are resolved against species_id first and species_name second; links
#' whose endpoints resolve to neither are dropped with a warning().
#'
#' @param metaweb Metaweb object
#' @return igraph object
#' @export
metaweb_to_igraph <- function(metaweb) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required")
  }

  species <- as.data.frame(metaweb$species, stringsAsFactors = FALSE)
  interactions <- as.data.frame(metaweb$interactions, stringsAsFactors = FALSE)

  species_names <- as.character(species$species_name)
  vertex_names <- make.unique(species_names)
  species_ids <- as.character(species$species_id)

  resolve <- function(x) {
    x <- as.character(x)
    out <- vertex_names[match(x, species_ids)]
    by_name <- is.na(out)
    out[by_name] <- vertex_names[match(x[by_name], species_names)]
    out
  }
  from <- resolve(interactions$prey_id)
  to <- resolve(interactions$predator_id)

  keep <- !is.na(from) & !is.na(to)
  if (any(!keep)) {
    unknown <- unique(c(as.character(interactions$prey_id)[is.na(from)],
                        as.character(interactions$predator_id)[is.na(to)]))
    warning(sprintf(
      "[metaweb_to_igraph] dropped %d interaction(s) with ids not in the species table: %s",
      sum(!keep), paste(utils::head(unknown, 10), collapse = ", ")
    ), call. = FALSE)
  }

  edges <- data.frame(from = from[keep], to = to[keep], stringsAsFactors = FALSE)
  vertices <- data.frame(name = vertex_names,
                         species[, setdiff(names(species), "name"), drop = FALSE],
                         stringsAsFactors = FALSE)

  g <- igraph::graph_from_data_frame(d = edges, directed = TRUE, vertices = vertices)

  # Add edge attributes (only for the links that were kept)
  if ("quality_code" %in% colnames(interactions)) {
    igraph::E(g)$quality_code <- interactions$quality_code[keep]
  }
  if ("source" %in% colnames(interactions)) {
    igraph::E(g)$source <- interactions$source[keep]
  }

  g
}

#' Map a metaweb species table onto the info-frame columns
#'
#' The companion of metaweb_to_igraph(): `species` is make.unique(species_name)
#' exactly as the vertex names are, so finalize_network() matches every row.
#' A functional_group outside get_functional_group_levels() becomes NA so that
#' finalize_network() infers it instead of colouring the node grey.
#'
#' @param species The metaweb species data frame.
#' @return Data frame with `species` plus whichever of fg, meanB, bodymasses,
#'   met.types, efficiencies, PB, QB and species_id the metaweb carries.
#' @export
metaweb_species_to_info <- function(species) {
  species <- as.data.frame(species, stringsAsFactors = FALSE)
  if (!"species_name" %in% names(species)) {
    stop("metaweb_species_to_info(): species table has no 'species_name' column",
         call. = FALSE)
  }

  info <- data.frame(species = make.unique(as.character(species$species_name)),
                     stringsAsFactors = FALSE)

  if ("functional_group" %in% names(species)) {
    fg <- as.character(species$functional_group)
    fg[!fg %in% get_functional_group_levels()] <- NA_character_
    info$fg <- fg
  }

  numeric_map <- c(biomass = "meanB", body_mass = "bodymasses",
                   efficiency = "efficiencies", pb_ratio = "PB", qb_ratio = "QB")
  for (src in names(numeric_map)) {
    if (src %in% names(species)) {
      info[[numeric_map[[src]]]] <- suppressWarnings(as.numeric(species[[src]]))
    }
  }
  if ("metabolic_type" %in% names(species)) {
    info$met.types <- as.character(species$metabolic_type)
  }
  if ("species_id" %in% names(species)) {
    info$species_id <- as.character(species$species_id)
  }

  info
}
```

- [ ] **Step 4: Export through the mapper**

In `R/modules/metaweb_manager_server.R:456`, replace
```r
      finalized <- finalize_network(new_net, current_metaweb()$species)
```
with
```r
      finalized <- finalize_network(new_net, metaweb_species_to_info(current_metaweb()$species))
```
Update the comment above it (L450-455) so its first sentence reads: "metaweb_species_to_info() renames the metaweb columns (species_name, biomass, body_mass, ...) to the info columns, and finalize_network() then joins them to V(net)$name, which is the same make.unique(species_name)."

- [ ] **Step 5: Parse-check and run the tests**

Parse-check `R/functions/metaweb_core.R` and `R/modules/metaweb_manager_server.R`.
Run `test-finalize-network.R`. Expected: all PASS (23 tests in the scratch run).
Also run A1's files, which exercise `metaweb_to_igraph`: `test-edge-contract.R`, `test-trophic-levels.R` and `test-mti-keystoneness.R`.
Expected: PASS. If an A1 test looks up vertices by id (`"SP001"`), change the lookup to the species name. From this task on, vertex names are species names; ids live in `V(g)$species_id`. Note the change in the commit body.
`scripts/initialization/create_example_metaweb.R:109` only prints `vcount`/`ecount`, so it needs no change.

- [ ] **Step 6: Commit**

```bash
git add R/functions/metaweb_core.R R/modules/metaweb_manager_server.R tests/testthat/test-finalize-network.R
git commit -m "fix(import): metaweb export keeps biomass, fg and traits (F67)

Vertices were named SP001..., so finalize_network's name join matched
nothing and every attribute fell back to its default. Name vertices by
make.unique(species_name) (id kept as V(g)\$species_id; edges still
prey -> predator) and map metaweb columns with metaweb_species_to_info().
Links to unknown ids are dropped with a warning instead of aborting.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

### Task 8: F50 - EcoBase: remove the proxies and finalize

**Files:**
- Modify: `R/functions/ecobase_connection.R`. Two `info <- data.frame(...)` blocks: ~L456-467 in `convert_ecobase_to_econetool()` and ~L642-653 in `convert_ecobase_to_econetool_hybrid()`. A1 added a `diet_prop` line above each, so locate them by their text.
- Modify: `R/modules/ecobase_server.R:246-249`
- Test: `tests/testthat/test-import-attributes.R` (append)

**Interfaces:**
- Consumes: `finalize_network(net, info)`; `with_mocked_function(pkg_env, func_name, mock_fn, code)` and `load_fixture(name)` from `helper-fixtures.R`; fixtures `ecobase_model_403_{input,output,metadata}.rds`.
- Produces: EcoBase converters return `info` without `bodymasses`, `efficiencies` and `losses`.

- [ ] **Step 1: Append the failing tests to `tests/testthat/test-import-attributes.R`**

```r

# ---------------------------------------------------------------------------
# F50 - EcoBase: no biomass*100 body mass, met.types filled by finalize
# ---------------------------------------------------------------------------

convert_403_offline <- function() {
  input <- load_fixture("ecobase_model_403_input")
  output <- load_fixture("ecobase_model_403_output")
  meta <- load_fixture("ecobase_model_403_metadata")
  with_mocked_function(globalenv(), "get_ecobase_model_input", function(model_id) input, {
    with_mocked_function(globalenv(), "get_ecobase_model_output", function(model_id) output, {
      with_mocked_function(globalenv(), "get_ecobase_model_metadata", function(model_id) meta, {
        suppressWarnings(suppressMessages(convert_ecobase_to_econetool_hybrid(403)))
      })
    })
  })
}

test_that("EcoBase converter no longer emits proxy body mass / efficiency / losses (F50)", {
  result <- convert_403_offline()
  expect_false(any(c("bodymasses", "efficiencies", "losses") %in% names(result$info)))
})

test_that("EcoBase import finalised carries met.types and a real body-mass estimate (F50)", {
  result <- convert_403_offline()
  out <- finalize_network(result$net, result$info)

  expect_false(anyNA(out$info$met.types))
  expect_false(isTRUE(all.equal(out$info$bodymasses, out$info$meanB * 100)))
  expect_false(all(out$info$efficiencies == 0.8))
})

test_that("ecobase_server.R finalises the EcoBase import (F50)", {
  code <- readLines(app_path("R/modules/ecobase_server.R"), warn = FALSE)
  code <- code[!startsWith(trimws(code), "#")]
  expect_true(any(grepl("finalize_network(ecobase_net, ecobase_info)", code, fixed = TRUE)))
})
```

- [ ] **Step 2: Run and verify the failure**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-import-attributes.R')"`
Expected: the three F50 tests FAIL with 1, 2 and 1 failures (the proxy columns are present, `bodymasses == meanB * 100`, and there is no `finalize_network` call).

- [ ] **Step 3: Remove the proxies from both converters**

In `convert_ecobase_to_econetool()` delete these three lines from `info <- data.frame(...)`:
```r
    bodymasses = biomass_values * 100,  # Rough estimate
    losses = 0.1,  # Default
    efficiencies = 0.8,  # Default
```
In `convert_ecobase_to_econetool_hybrid()` delete:
```r
    bodymasses = biomass_values * 100,
    losses = 0.1,
    efficiencies = 0.8,
```
Both frames then end `EE = ee_values,` followed by `stringsAsFactors = FALSE`. `finalize_network()` only fills NA or absent columns, which is why the proxies have to be removed rather than overwritten.

- [ ] **Step 4: Finalize in `R/modules/ecobase_server.R`**

Replace
```r
      # One derivation, shared with finalize_network() (see
      # R/functions/network_finalize.R). Four copies of this loop existed;
      # two of them drifted into findings #18 and #27.
      ecobase_info$colfg <- fg_to_color(ecobase_info$fg)
```
with
```r
      # finalize_network() aligns info to V(net), canonicalises fg, derives
      # colfg and estimates met.types, bodymasses and efficiencies by fg
      # (EcoBase carries none of them).
      finalized <- finalize_network(ecobase_net, ecobase_info)
      ecobase_net <- finalized$net
      ecobase_info <- finalized$info
```

- [ ] **Step 5: Parse-check and run the tests**

Parse-check both files. Run `test-import-attributes.R` and `test-ecobase-unit.R`.
Expected: F50 tests PASS. `test-ecobase-unit.R` still passes; it reads the pre-recorded `ecobase_model_403_converted.rds`, and `expect_valid_ecobase_model()` does not require `bodymasses`.

- [ ] **Step 6: Commit**

```bash
git add R/functions/ecobase_connection.R R/modules/ecobase_server.R tests/testthat/test-import-attributes.R
git commit -m "fix(import): EcoBase gets met.types and fg-based body mass (F50)

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

### Task 9: F49 - EwE `Type` drives Detritus/producer assignment

**Files:**
- Modify: `R/functions/functional_group_utils.R:40` (detritus regex) and add `assign_ewe_functional_groups()` before `#' Get Functional Group Levels`
- Modify: `R/modules/ecopath_import_server.R`: after `qb_values <- qb_values[valid_idx]` (~L178), `pattern_hint` (~L476-482), after `functional_groups <- taxonomic_report_data$functional_group` (~L627), and the `assign_functional_groups(...)` call (~L661-667)
- Test: `tests/testthat/test-import-attributes.R` (append)

**Interfaces:**
- Consumes: `assign_functional_groups(species_names, pb_values, indegrees, outdegrees, use_topology)` (existing).
- Produces: `assign_ewe_functional_groups(species_names, ewe_type = NULL, pb_values = NULL, indegrees = NULL, outdegrees = NULL, base_fg = NULL) -> character`.

**Deviations (reason):**
1. The regex is `detrit($|[^i])|\\bdet\\b|debris`, not the spec's `detrit|^det\\b|debris`, for two reasons.
   - The anchored `^det` would stop matching names such as "Pelagic det.", which today's `det\\.` catches.
   - A bare `detrit` would turn every "Detritivor..." consumer into Detritus. That would also hit EcoBase and `finalize_network()`, which classify by name only and have no Type code to correct it.

   The chosen pattern matches "Detritus", "Detritu", "Rdetrit", "Pelagic det.", "Debris" and "Detritus (POM)", and does not match "Detritivores" or "Detritivorous fish". This was checked with base-R `grepl()` (TRE engine), which is what the classifier uses.
2. For Type 0 the spec says "never Detritus or Phytoplankton via topology". The plan applies that to the name route too, so "Detritus feeders" with EwE Type 0 is a consumer. Such a group becomes "Benthos" if it has both prey and predators, else "Fish".

Note, not fixed here: "macroalgae" and "Macrophytobenthos" hit the `phyto|algae` pattern before the benthos pattern, so the Type-1 Benthos exception only fires for names like "Benthic macrophytes".

- [ ] **Step 1: Append the failing tests**

```r

# ---------------------------------------------------------------------------
# F49 - EwE Type drives Detritus / producer assignment
# ---------------------------------------------------------------------------

test_that("EwE Type 1/2/0 gives Phytoplankton / Detritus / Fish (F49)", {
  fg <- assign_ewe_functional_groups(c("Phyto", "Rdetrit", "Cod"), ewe_type = c(1, 2, 0))
  expect_equal(fg, c("Phytoplankton", "Detritus", "Fish"))
})

test_that("Type 2 is Detritus even when the name says nothing about detritus", {
  fg <- assign_ewe_functional_groups("Dead organic matter", ewe_type = 2)
  expect_equal(fg, "Detritus")
})

test_that("Type 1 benthic macrophytes stay Benthos", {
  fg <- assign_ewe_functional_groups("Benthic macrophytes", ewe_type = 1)
  expect_equal(fg, "Benthos")
})

test_that("a Type 0 consumer is never made a producer or detritus", {
  # Topology alone (no prey, P/B > 1) would call this Phytoplankton.
  fg <- assign_ewe_functional_groups("Group X", ewe_type = 0, pb_values = 50,
                                     indegrees = 0, outdegrees = 2)
  expect_false(fg %in% c("Phytoplankton", "Detritus"))
  # The name classifier calls this Detritus; EwE says it is a consumer.
  fg2 <- assign_ewe_functional_groups("Detritus feeders", ewe_type = 0,
                                      indegrees = 2, outdegrees = 1)
  expect_equal(fg2, "Benthos")
})

test_that("without a Type column the classifier behaves as before", {
  names_in <- c("Phytoplankton", "Herring", "Group X")
  expect_equal(
    assign_ewe_functional_groups(names_in, ewe_type = NULL, pb_values = c(100, 1, 50),
                                 indegrees = c(0, 1, 0), outdegrees = c(1, 0, 1)),
    unname(assign_functional_groups(names_in, c(100, 1, 50), c(0, 1, 0), c(1, 0, 1),
                                    use_topology = TRUE))
  )
})

test_that("the widened detritus regex catches EwE spellings", {
  expect_equal(assign_functional_group("Detritu"), "Detritus")
  expect_equal(assign_functional_group("Rdetrit"), "Detritus")
  expect_equal(assign_functional_group("Pelagic det."), "Detritus")
  expect_equal(assign_functional_group("Debris"), "Detritus")
  # Detritivores are consumers, not detritus - also for EcoBase, which has no Type.
  expect_equal(assign_functional_group("Detritivorous fish"), "Fish")
  expect_false(assign_functional_group("Benthic detritivores") == "Detritus")
})

test_that("ecopath_import_server.R reads the EwE Type column (F49)", {
  code <- readLines(app_path("R/modules/ecopath_import_server.R"), warn = FALSE)
  code <- code[!startsWith(trimws(code), "#")]
  expect_true(any(grepl("assign_ewe_functional_groups(", code, fixed = TRUE)))
})
```

- [ ] **Step 2: Run and verify the failure**

Expected:
- the five helper tests ERROR with `could not find function "assign_ewe_functional_groups"`;
- the regex test fails 2 expectations ("Detritu" and "Rdetrit" come back as "Fish"); the two detritivore expectations already pass and must keep passing;
- the structural test FAILS.

- [ ] **Step 3: Widen the regex (`R/functions/functional_group_utils.R:40`)**

Replace `  if (grepl("detritus|det\\.|debris", sp_lower)) {` with `  if (grepl("detrit($|[^i])|\\bdet\\b|debris", sp_lower)) {`. In the roxygen block above it (L26), change `Detritus: detritus, det., debris` to `Detritus: detrit* (not detritivor*), det (word), debris`.

- [ ] **Step 4: Add the helper before `#' Get Functional Group Levels`**

```r
#' Assign functional groups to EwE groups, honouring the EwE Type code
#'
#' EwE's Type column is authoritative for what the name classifier can only
#' guess: 2 = detritus, 1 = primary producer, 0 < Type < 1 = mixotroph,
#' 0 = consumer. Rules:
#' \itemize{
#'   \item Type 2 -> "Detritus".
#'   \item Type 1 -> "Phytoplankton", unless the classifier returns "Benthos"
#'         (benthic macrophytes / seagrass).
#'   \item 0 < Type < 1 -> the classifier result without topology.
#'   \item Type 0 -> the classifier result with topology, but never
#'         "Detritus" or "Phytoplankton": a consumer that the heuristics call
#'         basal becomes "Benthos" if it has both prey and predators, else
#'         "Fish".
#'   \item Type NA (column absent) -> assign_functional_groups() with topology,
#'         i.e. the pre-existing behaviour.
#' }
#'
#' @param species_names Character vector of group names.
#' @param ewe_type Numeric vector of EwE Type codes (NULL if the table has none).
#' @param pb_values,indegrees,outdegrees Optional numeric vectors for the
#'   topology heuristics (in-degree = number of prey under the prey -> predator
#'   edge contract).
#' @param base_fg Optional character vector of pre-computed classifications
#'   (e.g. from the taxonomic API); used instead of the topology classifier.
#' @return Character vector of functional groups, one per group.
#' @export
assign_ewe_functional_groups <- function(species_names, ewe_type = NULL, pb_values = NULL,
                                         indegrees = NULL, outdegrees = NULL, base_fg = NULL) {
  n <- length(species_names)
  if (is.null(ewe_type)) ewe_type <- rep(NA_real_, n)
  ewe_type <- suppressWarnings(as.numeric(ewe_type))
  if (length(ewe_type) != n) {
    stop("assign_ewe_functional_groups(): ewe_type must have one value per group", call. = FALSE)
  }
  if (is.null(indegrees)) indegrees <- rep(NA_real_, n)
  if (is.null(outdegrees)) outdegrees <- rep(NA_real_, n)

  fg <- if (is.null(base_fg)) {
    assign_functional_groups(species_names, pb_values, indegrees, outdegrees, use_topology = TRUE)
  } else {
    as.character(base_fg)
  }
  by_name <- assign_functional_groups(species_names, use_topology = FALSE)

  is_det <- !is.na(ewe_type) & ewe_type == 2
  is_prod <- !is.na(ewe_type) & ewe_type == 1
  is_mixo <- !is.na(ewe_type) & ewe_type > 0 & ewe_type < 1
  is_cons <- !is.na(ewe_type) & ewe_type == 0

  fg[is_det] <- "Detritus"
  fg[is_prod] <- ifelse(by_name[is_prod] == "Benthos" | fg[is_prod] == "Benthos",
                        "Benthos", "Phytoplankton")
  if (is.null(base_fg)) fg[is_mixo] <- by_name[is_mixo]

  basal <- is_cons & fg %in% c("Detritus", "Phytoplankton")
  has_both <- !is.na(indegrees) & !is.na(outdegrees) & indegrees > 0 & outdegrees > 0
  fg[basal] <- ifelse(has_both[basal], "Benthos", "Fish")

  unname(fg)
}
```

- [ ] **Step 5: Wire it into the native import (`R/modules/ecopath_import_server.R`)**

(a) After `      qb_values <- qb_values[valid_idx]` insert:
```r

      # EwE Type (0 consumer, 1 producer, 2 detritus, 0-1 mixotroph) drives
      # functional-group assignment where present; NULL keeps name + topology.
      type_col <- which(col_names == "type")[1]
      ewe_types <- if (!is.na(type_col)) {
        suppressWarnings(as.numeric(group_table[[type_col]]))[valid_idx]
      } else {
        NULL
      }
```
(b) In the taxonomic-API loop replace
```r
          pattern_hint <- assign_functional_group(
            sp,
            pb_values[i],
            indegrees[i],
            outdegrees[i],
            use_topology = TRUE
          )
```
with
```r
          pattern_hint <- assign_ewe_functional_groups(
            sp,
            ewe_type = if (is.null(ewe_types)) NULL else ewe_types[i],
            pb_values = pb_values[i],
            indegrees = indegrees[i],
            outdegrees = outdegrees[i]
          )
```
(c) After `        functional_groups <- taxonomic_report_data$functional_group` insert:
```r
        # The API result is re-checked against the EwE Type code: a Type 2
        # group is Detritus and a Type 0 group is never a producer.
        functional_groups <- assign_ewe_functional_groups(
          species_names, ewe_type = ewe_types, pb_values = pb_values,
          indegrees = indegrees, outdegrees = outdegrees, base_fg = functional_groups
        )
        taxonomic_report_data$functional_group <- functional_groups
```
(d) Replace
```r
        functional_groups <- assign_functional_groups(
          species_names,
          pb_values,
          indegrees,
          outdegrees,
          use_topology = TRUE  # Use network topology for ECOPATH imports
        )
```
with
```r
        functional_groups <- assign_ewe_functional_groups(
          species_names,
          ewe_type = ewe_types,
          pb_values = pb_values,
          indegrees = indegrees,
          outdegrees = outdegrees
        )
```

Flagged, not fixed in this task: `bodymass_values_raw` (~L101) is never subset by `valid_idx`, so body masses can shift by one row when "Import"/"Fleet" groups are filtered out. Open a follow-up issue; it is outside spec A.

- [ ] **Step 6: Parse-check and run the tests**

Parse-check both files, then run `test-import-attributes.R`.
Expected: the F49 tests PASS.
Also run `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_dir('tests/testthat', filter = 'layer|finalize|edge')"` to check that the regex change broke no other classification test.

- [ ] **Step 7: Commit**

```bash
git add R/functions/functional_group_utils.R R/modules/ecopath_import_server.R tests/testthat/test-import-attributes.R
git commit -m "fix(import): EwE Type drives detritus/producer groups (F49)

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

### Task 10: F68 - data editor keeps numeric columns numeric

**Files:**
- Create: `R/functions/data_editor_utils.R`
- Modify: `app.R:112` (source it after `network_finalize.R`)
- Modify: `R/modules/dataeditor_inline_server.R:101-107` (edit observer), `:116-120` (save validation)
- Test: `tests/testthat/test-import-attributes.R` (append; add the source line)

**Interfaces:**
- Consumes: `DT::coerceValue(val, old)` (DT 0.34).
- Produces:
  - `apply_cell_edit(df, row, col, value) -> data.frame`. `col` is DT's index with `rownames = TRUE`, where 0 is the row-name column. An invalid edit leaves the frame unchanged and emits a warning.
  - `non_numeric_info_columns(df, cols = c("meanB","bodymasses","efficiencies")) -> character`

- [ ] **Step 1: Add the source line and append the failing tests**

In the top `local({ ... })` of `test-import-attributes.R` add:
```r
  source(file.path(root, "R/functions/data_editor_utils.R"), local = FALSE)
```
Append:
```r

# ---------------------------------------------------------------------------
# F68 - data editor keeps numeric columns numeric
# ---------------------------------------------------------------------------

editor_frame <- function() {
  data.frame(
    meanB = c(1, 2),
    fg = factor(c("Fish", "Benthos"), levels = get_functional_group_levels()),
    met.types = c("invertebrates", "invertebrates"),
    stringsAsFactors = FALSE,
    row.names = c("Cod", "Mussel")
  )
}

test_that("editing a meanB cell with '3.5' keeps meanB numeric (F68)", {
  df <- apply_cell_edit(editor_frame(), row = 1, col = 1, value = "3.5")
  expect_true(is.numeric(df$meanB))
  expect_equal(df$meanB, c(3.5, 2))
})

test_that("editing fg to a valid level keeps the factor", {
  df <- apply_cell_edit(editor_frame(), row = 2, col = 2, value = "Fish")
  expect_true(is.factor(df$fg))
  expect_equal(as.character(df$fg), c("Fish", "Fish"))
})

test_that("an edit on the row-name column is ignored with a warning", {
  expect_warning(df <- apply_cell_edit(editor_frame(), row = 1, col = 0, value = "X"),
                 "outside the data")
  expect_identical(df, editor_frame())
})

test_that("text that is not a number is rejected, not written as NA", {
  expect_warning(df <- apply_cell_edit(editor_frame(), row = 1, col = 1, value = "abc"),
                 "not a valid value for column 'meanB'")
  expect_identical(df, editor_frame())
})

test_that("an fg outside the canonical levels is rejected", {
  expect_warning(df <- apply_cell_edit(editor_frame(), row = 1, col = 2, value = "Krill"),
                 "not a valid value for column 'fg'")
  expect_identical(df, editor_frame())
})

test_that("non_numeric_info_columns names character-typed numeric columns", {
  df <- editor_frame()
  df$bodymasses <- c("1", "2")
  df$efficiencies <- c(0.7, 0.8)
  expect_equal(non_numeric_info_columns(df), "bodymasses")
})

test_that("dataeditor_inline_server.R routes edits through apply_cell_edit (F68)", {
  code <- readLines(app_path("R/modules/dataeditor_inline_server.R"), warn = FALSE)
  code <- code[!startsWith(trimws(code), "#")]
  expect_true(any(grepl("apply_cell_edit(", code, fixed = TRUE)))
  expect_true(any(grepl("non_numeric_info_columns(", code, fixed = TRUE)))
  expect_false(any(grepl("<- info_edit$value", code, fixed = TRUE)))
})
```

- [ ] **Step 2: Run and verify the failure**

Expected: the file errors on the missing `data_editor_utils.R`. With an empty file in place, the helper tests ERROR with `could not find function "apply_cell_edit"` and the structural test fails 3 expectations.

- [ ] **Step 3: Create `R/functions/data_editor_utils.R`**

```r
# =============================================================================
# DATA EDITOR HELPERS
# =============================================================================
# Pure helpers behind dataeditor_inline_server.R, testable without a session.

#' Apply one DT cell edit to the species-info frame
#'
#' DT sends the edited value as a string. Assigning it raw turned the whole
#' numeric column into character (one edit to meanB broke every estimator).
#' DT::coerceValue() converts it to the column's existing class instead; a
#' value that does not fit the column (text in a number column, an fg that is
#' not a canonical level) is rejected rather than written as NA.
#'
#' @param df The species-info data frame shown in the table.
#' @param row 1-based row index from `input$<id>_cell_edit$row`.
#' @param col Column index from `input$<id>_cell_edit$col`. The table renders
#'   with `rownames = TRUE`, so DT's column 0 is the row-name column and
#'   column c is `df[[c]]`.
#' @param value The edited value (character).
#' @return `df` with the cell updated; unchanged (with a warning) when the
#'   edit targets the row-name column, lies outside the frame, or cannot be
#'   coerced to the column's type.
#' @export
apply_cell_edit <- function(df, row, col, value) {
  row <- suppressWarnings(as.integer(row))
  col <- suppressWarnings(as.integer(col))
  if (length(row) != 1 || length(col) != 1 || is.na(row) || is.na(col) ||
        row < 1 || row > nrow(df) || col < 1 || col > ncol(df)) {
    warning(sprintf("[data editor] ignored edit outside the data (row %s, col %s)", row, col),
            call. = FALSE)
    return(df)
  }
  new_value <- suppressWarnings(DT::coerceValue(value, df[[col]]))
  blank <- is.null(value) || is.na(value) || identical(trimws(as.character(value)), "")
  if (is.na(new_value) && !blank) {
    warning(sprintf("[data editor] '%s' is not a valid value for column '%s'; edit ignored",
                    value, names(df)[col]), call. = FALSE)
    return(df)
  }
  df[row, col] <- new_value
  df
}

#' Name the required numeric info columns that are not numeric
#'
#' @param df Species-info data frame.
#' @param cols Columns that must be numeric.
#' @return Character vector of offending column names (empty when all good).
#' @export
non_numeric_info_columns <- function(df, cols = c("meanB", "bodymasses", "efficiencies")) {
  present <- intersect(cols, names(df))
  present[!vapply(present, function(col) is.numeric(df[[col]]), logical(1))]
}
```

In `app.R`, after `source("R/functions/network_finalize.R")        # finalize_network(): align info to V(net), derive colfg` (L112), add:
```r
source("R/functions/data_editor_utils.R")       # apply_cell_edit(): type-safe DT cell edits
```

- [ ] **Step 4: Use the helpers in `R/modules/dataeditor_inline_server.R`**

Replace
```r
  observeEvent(input$species_info_table_cell_edit, {
    species_data_df <- species_data()
    info_edit <- input$species_info_table_cell_edit
    species_data_df[info_edit$row, info_edit$col] <- info_edit$value
    species_data(species_data_df)
  })
```
with
```r
  # DT sends the value as a string; apply_cell_edit() coerces it to the
  # column class so one edit cannot turn a numeric column into character.
  # A rejected edit is reported to the user as well as logged via warning().
  observeEvent(input$species_info_table_cell_edit, {
    info_edit <- input$species_info_table_cell_edit
    updated <- withCallingHandlers(
      apply_cell_edit(species_data(), info_edit$row, info_edit$col, info_edit$value),
      warning = function(w) showNotification(conditionMessage(w), type = "warning", duration = 6)
    )
    species_data(updated)
  })
```
In the save observer, after
```r
      if (length(missing_cols) > 0) {
        stop(paste("Missing required columns:", paste(missing_cols, collapse=", ")))
      }
```
insert
```r

      non_numeric <- non_numeric_info_columns(edited_info)
      if (length(non_numeric) > 0) {
        stop(paste("These columns must be numeric:", paste(non_numeric, collapse = ", ")))
      }
```

- [ ] **Step 5: Parse-check and run the tests**

Parse-check `R/functions/data_editor_utils.R`, `app.R` and `R/modules/dataeditor_inline_server.R`. Run `test-import-attributes.R`.
Expected: the seven F68 tests PASS.

- [ ] **Step 6: Commit**

```bash
git add R/functions/data_editor_utils.R app.R R/modules/dataeditor_inline_server.R tests/testthat/test-import-attributes.R
git commit -m "fix(import): data-editor edits keep column types (F68)

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

### Task 11: F51 - wire EMODnet enrichment to the sampling location

**Files:**
- Modify: `R/functions/euseamap_regional_config.R` (append `resolve_emodnet_bbox()`)
- Modify: `R/modules/ecopath_import_server.R:1586-1648` (the `observeEvent(input$enable_emodnet_habitat, ...)` block)
- Test: `tests/testthat/test-import-attributes.R` (append; add the source line)

**Interfaces:**
- Consumes:
  - `load_regional_euseamap(bbt_name, custom_bbox, path)` (existing)
  - `ecopath_native_metadata()` reactiveVal (same file, ~L901; `$metadata` carries `min_lon..max_lat`)
  - `app_path()`
- Produces: `resolve_emodnet_bbox(lon = NULL, lat = NULL, meta = NULL, pad = 2) -> numeric(4) | NULL`

Note: the UI's `numericInput`s default to Gdansk Bay (54.5189, 18.6466), so in practice a location is almost always present. The metadata fallback and the "no location" warning cover a user who clears the inputs.

- [ ] **Step 1: Add the source line and append the failing tests**

In the top `local({ ... })` add `source(file.path(root, "R/functions/euseamap_regional_config.R"), local = FALSE)`. Append:
```r

# ---------------------------------------------------------------------------
# F51 - EMODnet enrichment wired to the sampling location
# ---------------------------------------------------------------------------

test_that("resolve_emodnet_bbox prefers the sampling point (F51)", {
  expect_equal(resolve_emodnet_bbox(21, 55.5, NULL), c(19, 53.5, 23, 57.5))
})

test_that("resolve_emodnet_bbox falls back to the model metadata box", {
  meta <- list(min_lon = 20.7, max_lon = 21.1, min_lat = 55.3, max_lat = 56.1)
  expect_equal(resolve_emodnet_bbox(NA, NA, meta), c(20.7, 55.3, 21.1, 56.1))
  expect_equal(resolve_emodnet_bbox(NULL, NULL, meta), c(20.7, 55.3, 21.1, 56.1))
})

test_that("resolve_emodnet_bbox returns NULL with no location at all", {
  expect_null(resolve_emodnet_bbox(NA, NA, NULL))
  expect_null(resolve_emodnet_bbox(NA, NA, list(min_lon = 0, max_lon = 0, min_lat = 0, max_lat = 0)))
  expect_null(resolve_emodnet_bbox(NA, NA, list(min_lon = -9999, max_lon = 1, min_lat = 1, max_lat = 2)))
})

test_that("EMODnet observer has no current_network() and an app_path() GDB path (F51)", {
  code <- readLines(app_path("R/modules/ecopath_import_server.R"), warn = FALSE)
  code <- code[!startsWith(trimws(code), "#")]
  expect_false(any(grepl("current_network(", code, fixed = TRUE)))
  expect_true(any(grepl('app_path("data/EUSeaMap_2025/EUSeaMap_2025.gdb")', code, fixed = TRUE)))
  expect_false(any(grepl("custom_bbox <- c(20, 55, 21, 56)", code, fixed = TRUE)))
  expect_true(any(grepl("resolve_emodnet_bbox(", code, fixed = TRUE)))
})
```

- [ ] **Step 2: Run and verify the failure**

Expected: the three `resolve_emodnet_bbox` tests ERROR with "could not find function". The structural test fails 4 expectations.

- [ ] **Step 3: Append the helper to `R/functions/euseamap_regional_config.R`**

```r

#' Bounding box for EMODnet habitat loading
#'
#' @param lon,lat Sampling location (decimal degrees); NULL/NA when unset.
#' @param meta Imported model metadata with min_lon, max_lon, min_lat, max_lat
#'   (EwE EcopathModel MinLon..MaxLat); may be NULL.
#' @param pad Half-width in degrees of the box around a sampling point.
#' @return c(xmin, ymin, xmax, ymax), or NULL when neither a sampling point
#'   nor a valid metadata box is available.
#' @export
resolve_emodnet_bbox <- function(lon = NULL, lat = NULL, meta = NULL, pad = 2) {
  num <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_real_)
    v <- suppressWarnings(as.numeric(x[1]))
    if (!is.finite(v) || v == -9999) NA_real_ else v
  }
  lon <- num(lon)
  lat <- num(lat)
  if (!is.na(lon) && !is.na(lat)) {
    return(c(lon - pad, lat - pad, lon + pad, lat + pad))
  }
  if (!is.null(meta)) {
    box <- c(num(meta$min_lon), num(meta$min_lat), num(meta$max_lon), num(meta$max_lat))
    if (!anyNA(box) && box[1] < box[3] && box[2] < box[4]) {
      return(box)
    }
  }
  NULL
}
```

- [ ] **Step 4: Replace the observer**

In `R/modules/ecopath_import_server.R`, replace everything from `  # Observer to load EUSeaMap data when EMODnet habitat enrichment is enabled` up to (not including) `  # Handle ECOPATH native import when button clicked` with:

```r
  # Observer to load EUSeaMap data when EMODnet habitat enrichment is enabled.
  # The bbox comes from the sampling location the user entered (the same
  # inputs the import step uses), else from the imported model's metadata
  # bounding box. With neither there is nothing sensible to load.
  observeEvent(input$enable_emodnet_habitat, {
    if (!isTRUE(input$enable_emodnet_habitat) || !is.null(euseamap_data())) {
      return()
    }

    custom_bbox <- resolve_emodnet_bbox(
      lon = input$sampling_longitude,
      lat = input$sampling_latitude,
      meta = ecopath_native_metadata()$metadata
    )
    if (is.null(custom_bbox)) {
      showNotification(
        paste("EMODnet habitat enrichment needs a sampling location: enter latitude and",
              "longitude, or import a model whose metadata has a bounding box."),
        type = "warning", duration = 10
      )
      updateCheckboxInput(session, "enable_emodnet_habitat", value = FALSE)
      return()
    }

    showNotification("Loading EUSeaMap habitat data (optimized regional loading)...",
                     type = "message", duration = NULL, id = "emodnet_loading")

    tryCatch({
      euseamap <- load_regional_euseamap(
        bbt_name = NULL,
        custom_bbox = custom_bbox,
        path = app_path("data/EUSeaMap_2025/EUSeaMap_2025.gdb")
      )
      euseamap_data(euseamap)

      region <- attr(euseamap, "region") %||% "custom"

      removeNotification("emodnet_loading")
      showNotification(
        sprintf("✓ EUSeaMap loaded: %d polygons (%s region)", nrow(euseamap), toupper(region)),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      warning(sprintf("[EMODnet] EUSeaMap load failed for bbox %s: %s",
                      paste(round(custom_bbox, 3), collapse = ", "), conditionMessage(e)),
              call. = FALSE)
      removeNotification("emodnet_loading")
      showNotification(
        paste("Failed to load EUSeaMap:", conditionMessage(e),
              "\nPlease ensure EUSeaMap_2025.gdb exists in data/ directory"),
        type = "error",
        duration = 10
      )
      # Disable checkbox if loading failed
      updateCheckboxInput(session, "enable_emodnet_habitat", value = FALSE)
    })
  })

```

- [ ] **Step 5: Parse-check and run the tests**

Parse-check both files, then run `test-import-attributes.R`.
Expected: the F51 tests PASS.

- [ ] **Step 6: Commit**

```bash
git add R/functions/euseamap_regional_config.R R/modules/ecopath_import_server.R tests/testthat/test-import-attributes.R
git commit -m "fix(import): EMODnet enrichment uses the sampling location (F51)

current_network() did not exist, so the observer always fell back to a
hard-coded Baltic box and a wd-relative GDB path. Use the sampling inputs,
else the model metadata bbox, else tell the user; path via app_path().

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

### Task 12: F74 - trait food web with `species` not in the first column

**Files:**
- Modify: `R/functions/trait_foodweb.R` (`trait_foodweb_to_igraph()`, the "# Create igraph" block ~L344-351, after A1's N3 edge flip)
- Modify: `R/modules/foodweb_construction_server.R:367-399`
- Test: `tests/testthat/test-import-attributes.R` (append; add the source line)

**Interfaces:**
- Consumes: A1's edge list in `trait_foodweb_to_igraph()` (`from` = resource, `to` = consumer).
- Produces: `trait_foodweb_to_igraph(species_data, threshold, include_probs)`, whose vertices are keyed on `species_data$species` whatever the column order.

- [ ] **Step 1: Add the source line and append the failing tests**

In the top `local({ ... })` add `source(file.path(root, "R/functions/trait_foodweb.R"), local = FALSE)`. Append:
```r

# ---------------------------------------------------------------------------
# F74 - trait food web with species not in the first column
# ---------------------------------------------------------------------------

test_that("trait_foodweb_to_igraph works when species is not the first column (F74)", {
  path <- app_path("examples/trait_foodweb_simple.csv")
  skip_if_not(file.exists(path), "examples/trait_foodweb_simple.csv not found")
  traits <- utils::read.csv(path, stringsAsFactors = FALSE)
  shuffled <- traits[, c("MS", "species", "FS", "MB", "EP", "PR")]

  expect_no_error(g <- trait_foodweb_to_igraph(shuffled, threshold = 0.05))
  expect_equal(igraph::V(g)$name, shuffled$species)
  expect_no_error(g2 <- trait_foodweb_to_igraph(shuffled, threshold = 0.05, include_probs = FALSE))
  expect_equal(igraph::V(g2)$name, shuffled$species)
})

test_that("the construct observer is wrapped in tryCatch (F74)", {
  code <- readLines(app_path("R/modules/foodweb_construction_server.R"), warn = FALSE)
  start <- grep("observeEvent(input$foodweb_construct_network", code, fixed = TRUE)
  expect_length(start, 1L)
  block <- code[start:min(length(code), start + 60L)]
  expect_true(any(grepl("tryCatch(", block, fixed = TRUE)))
  expect_true(any(grepl("warning(", block, fixed = TRUE)))
})
```

- [ ] **Step 2: Run and verify the failure**

Expected: the first test FAILS with "Some vertex names in edge list are not listed in vertex data frame", because `graph_from_data_frame` keys on `MS`. The observer test fails 2 expectations.

- [ ] **Step 3: Key the vertices on `species`**

In `trait_foodweb_to_igraph()` replace
```r
  # Create igraph
  if (include_probs) {
    g <- igraph::graph_from_data_frame(edge_list, directed = TRUE,
                                       vertices = species_data)
  } else {
    g <- igraph::graph_from_data_frame(edge_list[, 1:2], directed = TRUE,
                                       vertices = species_data)
  }
```
with
```r
  # graph_from_data_frame() keys vertices on the FIRST column, so put
  # `species` first whatever the uploaded column order was.
  vertices <- species_data[, c("species", setdiff(names(species_data), "species")), drop = FALSE]

  # Create igraph
  if (include_probs) {
    g <- igraph::graph_from_data_frame(edge_list, directed = TRUE, vertices = vertices)
  } else {
    g <- igraph::graph_from_data_frame(edge_list[, 1:2], directed = TRUE, vertices = vertices)
  }
```

- [ ] **Step 4: Wrap the construct observer**

In `R/modules/foodweb_construction_server.R`, replace everything from `    withProgress(message = 'Constructing food web...', value = 0, {` through `    showNotification("Food web constructed successfully!", type = "message", duration = 3)` (the lines before the observer's closing `  })`) with:

```r
    tryCatch({
      withProgress(message = 'Constructing food web...', value = 0, {

        incProgress(0.3, detail = "Calculating interaction probabilities")

        # Get probability matrix
        rv$prob_matrix <- construct_trait_foodweb(
          rv$trait_data,
          threshold = 0,
          return_probs = TRUE
        )

        incProgress(0.3, detail = "Creating adjacency matrix")

        # Get adjacency matrix with threshold
        rv$adjacency_matrix <- construct_trait_foodweb(
          rv$trait_data,
          threshold = input$foodweb_threshold,
          return_probs = FALSE
        )

        incProgress(0.2, detail = "Building network graph")

        # Create igraph object
        rv$network_igraph <- trait_foodweb_to_igraph(
          rv$trait_data,
          threshold = input$foodweb_threshold,
          include_probs = TRUE
        )

        incProgress(0.2, detail = "Done")
      })

      showNotification("Food web constructed successfully!", type = "message", duration = 3)
    }, error = function(e) {
      warning(sprintf("[foodweb construction] network construction failed: %s",
                      conditionMessage(e)), call. = FALSE)
      showNotification(paste("Food web construction failed:", conditionMessage(e)),
                       type = "error", duration = 10)
    })
```

- [ ] **Step 5: Parse-check and run the tests**

Parse-check both files. Run `test-import-attributes.R` and A1's `test-edge-contract.R`; the latter still covers the N3 orientation of `trait_foodweb_to_igraph`.
Expected: all PASS.

- [ ] **Step 6: Commit**

```bash
git add R/functions/trait_foodweb.R R/modules/foodweb_construction_server.R tests/testthat/test-import-attributes.R
git commit -m "fix(import): trait food web keys vertices on species; observer tryCatch (F74)

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

### Task 12b: EwE body masses stay with their species when technical groups are filtered

Added 2026-09-26 at the user's request. This was found while writing this plan and is not in the report. In the native import observer, lines ~161-178 of
`R/modules/ecopath_import_server.R` drop Import/Export/empty groups by subsetting
`species_names`, `biomass_values`, `pb_values` and `qb_values` with `valid_idx`, but **not**
`bodymass_values_raw` (read at ~L101). Later (~L681-686) `bodymass_values_clean[i]` is indexed
by the *filtered* position `i`. So when a technical group precedes a real one, every later
species receives the body mass of the row above it. This is the same row-misalignment class as the
July findings.

**Files:**
- Modify: `R/modules/ecopath_import_server.R` (the `valid_idx` subsetting block; locate by the
  text `qb_values <- qb_values[valid_idx]`, since Tasks 5 and 9 move lines in this file)
- Test: `tests/testthat/test-import-attributes.R` (append)

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces: nothing new; the invariant "every per-group vector read from `group_table` is
  subset by `valid_idx`" is pinned by a guard test.

- [ ] **Step 1: Append the failing guard test**

The subsetting lives inside a Shiny observer, so the test is structural. It uses the same style as the
F51 guard, but it checks the invariant rather than one variable name, so a future per-group
column cannot repeat the bug.

```r

# ---------------------------------------------------------------------------
# EwE body masses must be filtered with the other per-group vectors
# ---------------------------------------------------------------------------

test_that("every per-group vector read from group_table is subset by valid_idx", {
  code <- readLines(app_path("R/modules/ecopath_import_server.R"), warn = FALSE)
  # Vectors assigned directly from group_table[[<col>]] (e.g. biomass_values, bodymass_values_raw)
  read_lines <- grep("^\\s*([A-Za-z_.]+)\\s*<-.*group_table\\[\\[", code, value = TRUE)
  vars <- unique(sub("^\\s*([A-Za-z_.]+)\\s*<-.*$", "\\1", read_lines))
  # area_proportions is consumed (biomass_values * area_proportions) BEFORE the valid_idx
  # filter and never used after it, so it needs no subsetting. If Task 5 (F52) removes that
  # read in favour of ewe_group_biomass(), this setdiff is a harmless no-op.
  vars <- setdiff(vars, "area_proportions")
  expect_true("bodymass_values_raw" %in% vars)
  for (v in vars) {
    subset_pat <- paste0(v, "\\s*<-\\s*", v, "\\[valid_idx\\]")
    expect_true(any(grepl(subset_pat, code)),
                info = sprintf("%s is read from group_table but never subset by valid_idx", v))
  }
})
```

- [ ] **Step 2: Run and verify the failure**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-import-attributes.R')"`
Expected: FAIL with "bodymass_values_raw is read from group_table but never subset by valid_idx".
(Dry-run on master 7a96789 on 2026-09-26: the regex finds species_names, biomass_values, pb_values, qb_values,
bodymass_values_raw and area_proportions. Only bodymass_values_raw fails once area_proportions is excluded.)
If any *other* variable is reported, it is a real sibling of this bug: subset it too in Step 3
and name it in the commit body.

- [ ] **Step 3: Subset the body masses with the other vectors**

Directly after the line `qb_values <- qb_values[valid_idx]`, add:

```r
      if (!is.null(bodymass_values_raw)) {
        bodymass_values_raw <- bodymass_values_raw[valid_idx]
      }
```

- [ ] **Step 4: Parse-check and run the tests**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='R/modules/ecopath_import_server.R'); cat('OK\n')"`
then the test file again. Expected: OK, then all PASS.

- [ ] **Step 5: Commit**

```bash
git add R/modules/ecopath_import_server.R tests/testthat/test-import-attributes.R
git commit -m "fix(import): keep EwE body masses aligned when technical groups are filtered

bodymass_values_raw was not subset by valid_idx with the other per-group
vectors, so body masses shifted one row per dropped Import/Export group.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

### Task 13: A3 full suite and PR

- [ ] **Step 1: Full suite + lint**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_dir('tests/testthat')"`
Expected: no regressions against the post-A1 baseline. The new tests pass, apart from the documented skips (Rpath, live, and on Linux the example-file tests).
Run `lintr::lint()` on every file touched in Tasks 5-12. Expected: no new lints on changed lines.

- [ ] **Step 2: Push and open the PR**

```bash
git push -u origin fix/a3-finalize-import
gh pr create --base master --title "fix(import): finalize and import attributes (F66, F67, F50, F49, F52, F68, F51, F74, body masses)" --body "$(cat <<'EOF'
Spec A3 - docs/superpowers/specs/2026-09-26-fix-a-network-science-correctness-design.md

F52 finding (A3 step 1): EcopathGroup.Biomass is biomass in habitat area (both example DBs: inputs only, no total-area column). Network path (x Area) was right; Rpath path was wrong -> both now use ewe_group_biomass(). Rpath results change for models with Area < 1.

Deviations: metaweb_species_to_info() also maps pb_ratio/qb_ratio -> PB/QB; detritus regex is detrit($|[^i])|\bdet\b|debris (keeps "Pelagic det.", excludes detritivores); Type-0 groups are never Detritus/Phytoplankton by name either; apply_cell_edit() rejects uncoercible edits instead of writing NA.
Metaweb vertices are now named by species (ids in V(g)$species_id).

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

### Task 14: Release 1.5.0 (after A3 is merged)

**Files:**
- Modify: `VERSION`, `app.R` (header `# CURRENT VERSION:` line), `README.md` (version marker), all via `scripts/version_bump.R`
- Modify: `R/config.R:294-300` (fallback block; `version_bump.R` does not touch it)
- Modify: `CHANGELOG.md` (regenerate, then insert the hand-written section)

**Interfaces:** none.

- [ ] **Step 1: Branch and bump**

```bash
git checkout master && git pull
git checkout -b release/1.5.0
grep -n "^VERSION=" VERSION   # expect 1.4.5 (B0 aligned it); bump from whatever it reads
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/version_bump.R --version 1.5.0 --name "Network Science Correctness" --dry-run
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/version_bump.R --version 1.5.0 --name "Network Science Correctness"
```
Expected: VERSION reads `VERSION=1.5.0`, `MAJOR=1`, `MINOR=5`, `PATCH=0`. The `app.R` line reads `# CURRENT VERSION: v1.5.0 (2026-..-..)`.

- [ ] **Step 2: Align the `R/config.R` fallback**

In `load_version_info()` set the fallback list to:
```r
    VERSION = "1.5.0",
    VERSION_NAME = "Network Science Correctness",
    RELEASE_DATE = "<today, YYYY-MM-DD>",
    STATUS = "stable",
    MAJOR = 1,
    MINOR = 5,
    PATCH = 0
```
Parse-check `R/config.R`, then run `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-config-paths.R')"`. Expected: PASS.

- [ ] **Step 3: Regenerate the CHANGELOG, then insert the hand-written section**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/generate_changelog.R --version 1.5.0
```
The generator overwrites the whole file. Only **after** it has run, insert the following directly under the `## [1.5.0] - <date>` heading, before the generated `### ...` subsections:

```markdown
### Results changed

These fixes change numerical outputs. Results computed with earlier versions
on the paths listed below are not comparable and should be regenerated.

- **MTI / keystoneness - every source.** Formula replaced (Ulanowicz & Puccia
  1990 MTI, Libralato et al. 2006 keystoneness). Producers can now have
  positive impacts on their consumers. Earlier KS values and Keystone/Dominant
  statuses are not comparable.
- **Trophic levels changed for:**
  - CSV/Excel EwE imports (previously inverted; functional-group heuristics
    also shift);
  - the Kongsfjorden metaweb;
  - user metawebs built from the template or uploaded CSVs (previously
    inverted);
  - per-hexagon meanTL/maxTL computed from those metawebs;
  - Rpath TL after balancing (previously overwritten by a transposed
    recomputation).
- **Unchanged TL:** the Baltic, Barents (boreal and arctic) and North Sea
  bundled metawebs, and metawebs derived from `.ewemdb` imports - two errors
  cancelled for these.
- Nodes with no path from a basal species now show TL `NA` (with a warning)
  instead of about 101.
- Earlier exports (CSV, RDS, GraphML) from the "changed" paths above -
  including trait food-web RDS/GraphML exports, whose edges now run
  resource -> consumer - are inverted and should be regenerated.
- Rpath diagnostics now exclude detritus from "living" totals (mean TL,
  total biomass, group count) and report detritus separately; they also work
  on the balanced Rpath object for the first time.
- **Rpath biomass for groups with habitat area < 1.** EwE stores biomass per
  habitat area; the Rpath conversion now multiplies by the habitat-area
  fraction, as the network import always did. Rpath balancing of models with
  `Area < 1` (e.g. four macrozoobenthos groups and Polychaetes in the Coastal
  example) changes. Network-tab biomasses are unchanged.
- Rpath conversion keeps cannibalism as entered (previously set to 0 without
  renormalising the predator's diet).
- **Imported attributes:** metaweb exports now carry their real biomass, body
  mass, metabolic type, efficiency and functional groups (previously all
  defaults); EcoBase imports get metabolic types and functional-group body-mass
  estimates instead of `biomass x 100`; EwE `Type` now decides detritus and
  producer groups.
```

`generate_changelog.R` rebuilds the file from git history, so every future regeneration (1.6.0 onward) erases this hand-written section. The canonical copy lives in this plan file, Task 14 Step 3. Say so in the release commit body so whoever regenerates next re-inserts it.

- [ ] **Step 4: Commit and PR**

```bash
git add VERSION app.R README.md R/config.R CHANGELOG.md
git commit -m "chore(release): 1.5.0 - network science correctness

The CHANGELOG 'Results changed' section is hand-written and is lost on the
next generate_changelog.R run; its source is
docs/superpowers/plans/2026-09-26-a2-a3-rpath-finalize-import.md (Task 14).

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
git push -u origin release/1.5.0
gh pr create --base master --title "chore(release): 1.5.0" --body "$(cat <<'EOF'
Version 1.5.0 in VERSION, R/config.R, app.R; CHANGELOG regenerated plus hand-written "Results changed" section (spec A section 6).

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```
After merging, tag it: `git checkout master && git pull && git tag v1.5.0 && git push origin v1.5.0`.

### Task 15: Deploy 1.5.0 to laguna.ku.lt (EACH STEP REQUIRES USER CONFIRMATION)

Follow the memory workflow. Do not run any step in this task without the user's explicit go-ahead for that step.

- [ ] **Step 1 (confirm with user): Pre-deploy check, run from inside `deployment/`**

```bash
cd deployment && "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" pre-deploy-check.R; cd ..
```
Expected: no FAIL lines.

- [ ] **Step 2 (confirm with user): Clear staging**

Until B2 lands, `cp -rT` would copy stale files from earlier deploys (spec B, F5):
```bash
ssh razinka@laguna.ku.lt "rm -rf /home/razinka/EcoNeTool_staging && mkdir -p /home/razinka/EcoNeTool_staging"
```

- [ ] **Step 3 (confirm with user): Upload code only**

```bash
powershell ./deploy-windows.ps1 -SkipData -NoSudo
```

- [ ] **Step 4 (confirm with user): Verify the staging listing before copying**

```bash
ssh razinka@laguna.ku.lt "grep -c 'metaweb_species_to_info' /home/razinka/EcoNeTool_staging/R/functions/metaweb_core.R; ls -la /home/razinka/EcoNeTool_staging/metawebs/baltic/ /home/razinka/EcoNeTool_staging/R/functions/ecopath/ecopath_group_biomass.R /home/razinka/EcoNeTool_staging/R/functions/data_editor_utils.R; grep '^VERSION=' /home/razinka/EcoNeTool_staging/VERSION; test -d /home/razinka/EcoNeTool_staging/data && echo 'WARNING: data/ present in staging' || echo 'no data/ in staging (expected)'"
```
Expected:
- a count of at least 1;
- the Baltic `.csv`/`.rds` listed, which proves `metawebs/` shipped despite `-SkipData`;
- both new R files present;
- `VERSION=1.5.0`;
- "no data/ in staging (expected)".

**Deviation (reason):** the spec greps the deployed `metaweb_core.R` for `"prey_id","predator_id"`. After Task 7 that literal no longer exists, because edges are resolved through `resolve(interactions$prey_id)`. The grep uses `metaweb_species_to_info` instead.

- [ ] **Step 5 (confirm with user): Copy into place and reload**

```bash
ssh razinka@laguna.ku.lt "cp -rT /home/razinka/EcoNeTool_staging /srv/shiny-server/EcoNeTool && touch /srv/shiny-server/EcoNeTool/restart.txt"
```
Never use `rm -rf /srv/shiny-server/EcoNeTool/*`: it would delete the live `data/` (~3.1 GB).

- [ ] **Step 6: Verify live**

```bash
ssh razinka@laguna.ku.lt "grep -c 'metaweb_species_to_info' /srv/shiny-server/EcoNeTool/R/functions/metaweb_core.R; grep '^VERSION=' /srv/shiny-server/EcoNeTool/VERSION; stat -c '%y' /srv/shiny-server/EcoNeTool/restart.txt; du -sh /srv/shiny-server/EcoNeTool/data"
curl -s -o /dev/null -w "%{http_code}\n" http://laguna.ku.lt/EcoNeTool/
```
Expected:
- a count of at least 1 and `VERSION=1.5.0`;
- `restart.txt` time is now;
- `data/` still about 3.1 GB;
- HTTP `200`.
Then open the app, export the Baltic metaweb to the active network, and check that the Food Web tab shows several functional-group colours.

---

## Self-review (done while writing)

- **Spec coverage:**
  - F40 is Task 2, F44 Task 1, F41 Task 3.
  - F52 is Task 5, the first task of A3, with the inspection done and recorded.
  - F66 is Task 6, F67 Task 7, F50 Task 8, F49 Task 9, F68 Task 10, F51 Task 11 and F74 Task 12.
  - Release (section 6) is Task 14 and deploy is Task 15.
  - Each section 5 test maps onto the tasks:

    | Spec test | Task |
    |---|---|
    | A2 test 1 | 1 |
    | A2 test 2 | 1, which also covers L81/89/101 |
    | A2 test 3 | 2 and 3 |
    | A2 test 4 | 3, via the pure helper; deviation stated |
    | A2 test 5 | 2 |
    | A3 tests 1-8 | 6, 7, 8, 9, 5, 10, 12 and 11 |

  - Acceptance criteria in section 8 for A2, A3 and the release are covered by Tasks 1-15.
- **Placeholders:** none. `<today, YYYY-MM-DD>` in Task 14 is the release date, set at execution time.
- **Type consistency:**
  - `.require_balanced_model()` now returns a frame, and both of its callers were updated in the same task.
  - `ewe_group_biomass(group_table, biomass_col, area_col)` is called the same way in Task 5 steps 5 and 6.
  - `metaweb_species_to_info()` builds `species` with the same `make.unique()` call as `metaweb_to_igraph()`'s vertex names.
  - `assign_ewe_functional_groups(..., base_fg)` is used with the same argument names in Task 9 steps 4 and 5.
- **Review Focus:** all five items have tests, in Tasks 7 (x2), 5, 1 and 10.
