# A1: Edge Contract, MTI/Keystoneness and Trophic-Level NA Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make every EcoNeTool graph builder, metric and bundled metaweb follow one edge contract (prey -> predator), replace the wrong MTI/keystoneness estimator with Ulanowicz-Puccia + Libralato, and make unreachable or non-converged trophic levels `NA` instead of about 101. All of this ships as one atomic PR.

**Architecture:** The contract is documented once, in the header of `R/functions/network_finalize.R`, and asserted in tests through one helper, `assert_prey_to_predator()`. The work lands in four commits on one branch:

1. Trophic-level `NA` semantics plus the callers that must tolerate `NA`.
2. The MTI/KS rewrite and its UI.
3. The EwE importers (CSV orientation, Detritus, `diet_prop`) and the trait food-web export.
4. The atomic orientation flip: `metaweb_to_igraph`, the local-network builder, the EwE->metaweb writer and the metaweb preview, together with the bundled-metaweb migration and its regenerated data.

Every commit leaves `testthat::test_dir("tests/testthat")` green. Task 4 is the only task whose parts cannot be committed separately.

**Tech Stack:** R 4.4.1, igraph 2.2.1, testthat 3, Shiny/bs4Dash, MASS (`ginv`), lintr.

**Spec:** `docs/superpowers/specs/2026-09-26-fix-a-network-science-correctness-design.md` (section 4 "A1", section 5 "A1" tests, section 6 rollout, section 7 risks, section 8 acceptance), and `docs/superpowers/specs/2026-09-26-fix-overview.md` (binding decisions and merge order).

## Global Constraints

- Branch `fix/a1-edge-contract` is cut from `master` **after B0 (F1/F76 harmonization hotfix) has merged**. A1 depends on B0.
- **A1 is atomic.** All edge flips (F64, F56, F48, N1, N2, N3) and the metaweb migration with its regenerated CSV/.rds land in **one PR**. Any subset leaves either the bundled metawebs or the EwE-derived metawebs inverted.
- Edge contract: `A -> B` means B eats A. The adjacency matrix is `adj[prey, predator]`. Diet matrices are prey x predator, and each predator column sums to 1. `metaweb$interactions` columns mean exactly what their names say.
- MTI/KS follows Ulanowicz-Puccia MTI with Libralato keystoneness. FC uses Q/B x B when present and a biomass proxy otherwise. Classification: KS in the top quartile with p < 0.05 is **Keystone**; KS in the top quartile with p >= 0.05 is **Dominant**; everything else is Other or Undefined. The user approved this rule.
- Trophic levels: non-converged or unreachable nodes return `NA` plus `warning()`.
- No version bump in A1. 1.5.0 is released after A3. The PR description carries the CHANGELOG "Results changed" draft (Task 5).
- Conventions (CLAUDE.md):
  - Use `warning()`, never `message()`, in error handlers, and `<<-` for outer mutation inside error closures.
  - Use `app_path()` for runtime paths, never a wd-relative `source()` inside `R/`.
  - Use `skip_if()` / `skip_if_not()`, never an `if`-gated `expect_*`.
  - Assign with `<-`, keep lines at 120 characters or fewer, and use no tabs and no trailing whitespace.
- Parse-check every edited `.R` file:
  `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='<path>'); cat('OK\n')"`
- Test commands. `tests/run_all_tests.R` cannot run locally (it needs `dggridR`).
  - One file: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/<file>')"`
  - Full suite: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_dir('tests/testthat')"`
- Baseline measured while this plan was written, on `docs/deep-analysis-2026-09-specs` (the same code as `master` @ `7a96789`): **1144 pass / 0 fail / 36 skip**. Re-measure on the fresh branch in Task 0. No existing test may regress.
- Every commit message ends with `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`.
- Pre-commit hooks (`.pre-commit-config.yaml`) are hygiene-only: whitespace, EOF, LF and no `DEBUG:`. `metawebs/` is excluded, and there is no lintr hook, so pre-existing lints in the touched files cannot block a commit.
- Line numbers below were read on `7a96789`. B0 edits only harmonization files, so they should still hold. Every edit is nevertheless given as an exact old -> new block; match on the text, not the number.

## Review Focus

1. **A consumer that eats both a normal prey and an unreachable cannibal-only group.** Example: cod eats herring and a self-feeding group. Expected: the consumer gets a finite TL averaged over its reachable prey only, and the loop group is `NA`. It must not poison the consumer. Pinned in Task 1.
2. **An EwE CSV/Excel diet export with blank cells.** Blank cells are routine in EwE exports. Expected: a blank cell means "not eaten" and the import succeeds. It must not raise an `NA` adjacency error or create phantom links. Pinned in Task 3.
3. **A network where only some edges carry `diet_prop`.** This happens, for example, after edges are added by hand. Expected: MTI falls back to the equal diet split for the whole web and raises one `warning()`, rather than silently dropping those links. Pinned in Task 2.
4. **A species with `NA` biomass in the keystoneness table.** Expected: that species is "Undefined" and every other species is still classified. There must be no error, and the table must not come back empty. Pinned in Task 2.
5. **A spatial hexagon whose local web has no basal species.** An example is a hexagon whose only taxa eat each other. Expected: `meanTL`/`maxTL` are `NA`, not `NaN` or `-Inf`, and there is no error. Pinned in Task 1.

---

## File map

| File | Responsibility | Task |
|---|---|---|
| `R/functions/network_finalize.R` | Edge-contract header comment (documentation only) | 1 |
| `tests/testthat/helper-fixtures.R` | `assert_prey_to_predator()` | 1 |
| `R/functions/trophic_levels.R` | `calculate_trophic_levels()`: `NA` for unreachable / non-converged nodes (F69) | 1 |
| `R/functions/network_visualization.R` | `plotfw()` and `create_foodweb_visnetwork()` tolerate `NA` TL | 1 |
| `R/functions/topological_metrics.R` | `TL` and `nwTL` ignore `NA` TL | 1 |
| `R/functions/spatial_analysis.R` | meanTL/maxTL via `calculate_trophic_levels()` (Task 1); edge flip at `extract_local_network` (Task 4) | 1, 4 |
| `tests/testthat/test-trophic-levels.R` | New | 1 |
| `R/functions/keystoneness.R` | `calculate_mti()`, `.mti_rcond()`, `calculate_keystoneness()` rewrite (F65) | 2 |
| `R/modules/analysis_server.R` | Status names, KS reference line, heatmap diagonal and axis labels | 2 |
| `R/ui/keystoneness_ui.R`, `R/ui/dashboard_ui.R` | Help text | 2 |
| `tests/testthat/test-mti-keystoneness.R` | New | 2 |
| `R/functions/ecopath/ecopath_csv.R` | F48 no transpose, F55 keep Detritus, blank cells, `diet_prop` | 3 |
| `R/functions/ecobase_connection.R` | `diet_prop` in both converters | 3 |
| `R/modules/ecopath_import_server.R` | `diet_prop` on native import (Task 3); N1 writer through the helper (Task 4) | 3, 4 |
| `R/functions/trait_foodweb.R` | N3 export direction | 3 |
| `R/functions/functional_group_utils.R` | Comments only | 3 |
| `tests/testthat/test-edge-contract.R` | New (Task 3 part); extended in Task 4 | 3, 4 |
| `R/functions/metaweb_core.R` | F64 edge order; new `igraph_to_metaweb_interactions()` | 4 |
| `R/modules/metaweb_manager_server.R` | N2 preview arrows | 4 |
| `scripts/initialization/fix_metaweb_orientation.R` | New one-off migration | 4 |
| `metawebs/{baltic,arctic,atlantic}/*_interactions.csv` + `.rds` (4 metawebs) | Regenerated by the script | 4 |
| `R/ui/import_ui.R`, `R/ui/metaweb_ui.R`, `R/ui/dataeditor_ui.R`, `metawebs/README.md` | Column semantics and graph direction in help | 4 |

Legacy tests: `tests/test_phase1_spatial.R`, `tests/test_phase1_backend.R` and the spatial cases in `tests/run_all_tests.R` only `print()` meanTL/maxTL. They assert no TL values; `grep -n "stopifnot\|expect" tests/test_phase1_*.R` shows none on TL. **No legacy expectation needs updating.** The three existing `parse_ecopath_data` tests in `test-deep-analysis-fixes.R` include a Detritus row. They were verified to still pass once Detritus is retained (F55).

---

### Task 0: Branch and baseline

**Files:** none

- [ ] **Step 1: Create the branch from an up-to-date master (B0 merged)**

```bash
cd "/c/Users/arturas.baziukas/OneDrive - ku.lt/HORIZON_EUROPE/MARBEFES/Traits/Networks/EcoNeTool"
git fetch origin
git checkout master && git pull --ff-only
git log --oneline -15 | grep -i "harmoniz\|F1\|B0"   # B0 must be present; stop if not
git checkout -b fix/a1-edge-contract
```

- [ ] **Step 2: Record the baseline**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "r <- testthat::test_dir('tests/testthat', stop_on_failure = FALSE); d <- as.data.frame(r); cat('pass', sum(d$passed), 'fail', sum(d$failed), 'skip', sum(d$skipped), '\n')"`

Expected: 0 failures, with about 1144 passes and 36 skips (B0 may add a few). Write the numbers down. Every later full-suite run must show 0 failures, and at least this many passes plus the new tests.

---

### Task 1: Edge-contract doc, `assert_prey_to_predator()`, trophic-level `NA` semantics (F69) and `NA`-tolerant callers

**Files:**
- Modify: `R/functions/network_finalize.R:18-20` (header comment)
- Modify: `tests/testthat/helper-fixtures.R` (append after line 187)
- Modify: `R/functions/trophic_levels.R:6-92` (roxygen + `calculate_trophic_levels`)
- Modify: `R/functions/network_visualization.R:61-66`, `:79-86`, `:147-148`, `:267-268`, `:288`
- Modify: `R/functions/topological_metrics.R:19-20`, `:93`
- Modify: `R/functions/spatial_analysis.R:404-425` (TL block of `calculate_spatial_metrics`)
- Create: `tests/testthat/test-trophic-levels.R`

**Interfaces:**
- Consumes: nothing from other tasks.
- Produces:
  - `assert_prey_to_predator(net, prey, predator)`: a testthat helper (2 expectations). `prey` and `predator` are vertex names or indices. It expects `are_adjacent(net, prey, predator)` to be TRUE and `are_adjacent(net, predator, prey)` to be FALSE.
  - `calculate_trophic_levels(net, max_iter = 100, convergence = 1e-4)` returns a numeric vector named by `V(net)$name`. It is `NA` for (a) nodes with no path from a basal node (in-degree 0; a self-loop counts as in-degree) and (b) nodes still changing at `max_iter`. Each class raises one `warning()` whose text contains "no path from a basal", "did not converge" or "No basal species" respectively. A consumer averages over prey in the reachable set only.

- [ ] **Step 1: Write the failing test file `tests/testthat/test-trophic-levels.R`**

```r
# =============================================================================
# calculate_trophic_levels(): NA for unreachable / non-converged nodes (F69)
# =============================================================================
# A node with no path from a basal species (e.g. a pure cannibal loop) used to
# be returned as ~101 after 100 iterations with only a warning, and that value
# flowed into mean TL, nwTL, plot layouts and per-hexagon metrics. It is now NA
# plus a warning(), and every caller aggregates with na.rm.

source_app_dependencies()

local({
  root <- get_app_root()
  source(file.path(root, "R/config.R"), local = FALSE)
  source(file.path(root, "R/functions/functional_group_utils.R"), local = FALSE)
  source(file.path(root, "R/functions/network_finalize.R"), local = FALSE)
  source(file.path(root, "R/functions/trophic_levels.R"), local = FALSE)
  source(file.path(root, "R/functions/network_visualization.R"), local = FALSE)
  source(file.path(root, "R/functions/topological_metrics.R"), local = FALSE)
  source(file.path(root, "R/functions/spatial_analysis.R"), local = FALSE)
})
suppressPackageStartupMessages(library(igraph))

# P -> A, A <-> B (A also eats B), C eats only itself.
# A = 1 + mean(P, B), B = 1 + A  =>  A = 4, B = 5.  C is unreachable.
loop_web <- function() {
  make_graph(c("P", "A", "A", "B", "B", "A", "C", "C"), directed = TRUE)
}

test_that("a simple chain gets TL 1, 2, 3 with no warning", {
  net <- make_graph(c("A", "B", "B", "C"), directed = TRUE)
  expect_no_warning(tl <- calculate_trophic_levels(net))
  expect_equal(unname(tl[c("A", "B", "C")]), c(1, 2, 3))
})

test_that("a node with no path from a basal species is NA, with a warning", {
  expect_warning(tl <- calculate_trophic_levels(loop_web()), "no path from a basal")
  expect_equal(unname(tl[c("P", "A", "B")]), c(1, 4, 5), tolerance = 1e-3)
  expect_true(is.na(tl[["C"]]))
})

test_that("a web with no basal species is all NA with exactly one warning", {
  net <- make_graph(c("A", "B", "B", "A"), directed = TRUE)
  warns <- testthat::capture_warnings(tl <- calculate_trophic_levels(net))
  expect_length(warns, 1)
  expect_match(warns, "No basal species")
  expect_true(all(is.na(tl)))
})

test_that("nodes still changing at max_iter are NA, with a warning", {
  net <- make_graph(c("P", "A", "A", "B", "B", "A"), directed = TRUE)
  expect_warning(tl <- calculate_trophic_levels(net, max_iter = 3), "did not converge")
  expect_equal(tl[["P"]], 1)
  expect_true(is.na(tl[["A"]]))
  expect_true(is.na(tl[["B"]]))
})

test_that("topological indicators ignore NA trophic levels", {
  topo <- suppressWarnings(get_topological_indicators(loop_web()))
  # mean over P, A, B only - C (formerly ~101) is excluded.
  expect_equal(topo$TL, mean(c(1, 4, 5)), tolerance = 1e-3)
})

test_that("node-weighted TL renormalises biomass over nodes with a TL", {
  net <- loop_web()
  info <- data.frame(species = V(net)$name, meanB = c(10, 5, 2, 1))
  nw <- suppressWarnings(get_node_weighted_indicators(net, info))
  # (10 * 1 + 5 * 4 + 2 * 5) / (10 + 5 + 2); C's biomass is dropped.
  expect_equal(nw$nwTL, 40 / 17, tolerance = 1e-3)
})

test_that("plotfw draws a web containing NA trophic levels", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  res <- suppressWarnings(plotfw(loop_web()))
  # The NA node sits on its own bottom row below TL 1.
  expect_equal(res["C", "y"], 0.5)
  expect_true(all(res[c("P", "A", "B"), "y"] >= 1))
})

test_that("create_foodweb_visnetwork places NA nodes below the lowest row", {
  skip_if_not(requireNamespace("visNetwork", quietly = TRUE), "visNetwork is not installed")
  net <- loop_web()
  info <- finalize_network(net, data.frame(
    species = V(net)$name, meanB = c(10, 5, 2, 1),
    fg = c("Phytoplankton", "Zooplankton", "Zooplankton", "Fish")
  ))$info
  vis <- suppressWarnings(create_foodweb_visnetwork(net, info))
  y <- stats::setNames(vis$x$nodes$y, vis$x$nodes$label)
  expect_true(all(is.finite(y)))
  expect_gt(y[["C"]], max(y[c("P", "A", "B")]))
})

test_that("a consumer of an unreachable loop averages over reachable prey only", {
  # X eats P and C; C eats only itself, so C has no TL and X ignores it.
  net <- make_graph(c("P", "X", "C", "C", "C", "X"), directed = TRUE)
  expect_warning(tl <- calculate_trophic_levels(net), "no path from a basal")
  expect_equal(tl[["X"]], 2)
  expect_true(is.na(tl[["C"]]))
})

test_that("a hexagon whose web has no basal species gets NA meanTL / maxTL", {
  net <- make_graph(c("A", "B", "B", "A"), directed = TRUE)
  metrics <- suppressWarnings(
    calculate_spatial_metrics(list(H1 = net), metrics = c("meanTL", "maxTL"), progress = FALSE)
  )
  expect_true(is.na(metrics$meanTL))
  expect_true(is.na(metrics$maxTL))
})
```

(`fg` is passed explicitly in the visNetwork test so the test does not depend on F66, a finalize bug that A3 fixes.)

- [ ] **Step 2: Add `assert_prey_to_predator()` to `tests/testthat/helper-fixtures.R`** (append at the end of the file, after `expect_valid_db_lookup_result`)

```r

# ---------------------------------------------------------------------------
# Edge contract (R/functions/network_finalize.R header): A -> B means B eats A
# ---------------------------------------------------------------------------
# Asserts that `predator` eats `prey` in `net` and that the reverse link is
# absent. Vertices are given by name (or index). Every orientation test uses
# this, so a predator->prey regression fails with a readable message.
assert_prey_to_predator <- function(net, prey, predator) {
  testthat::expect_true(
    igraph::are_adjacent(net, prey, predator),
    label = sprintf("edge %s -> %s (prey -> predator) exists", prey, predator)
  )
  testthat::expect_false(
    igraph::are_adjacent(net, predator, prey),
    label = sprintf("reverse edge %s -> %s (predator -> prey) exists", predator, prey)
  )
}
```

- [ ] **Step 3: Run the test to verify it fails**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-trophic-levels.R')"`

Expected: FAIL. Nine of the ten tests fail, and only "a simple chain" passes. This was verified against current code. For example: C is `101` not `NA`; the warning text is "did not converge" instead of "no path from a basal"; `topo$TL` is 27.75 instead of 3.33; plotfw puts C at y = 101; the no-basal hexagon returns finite TL.

- [ ] **Step 4: Replace `calculate_trophic_levels()` and its roxygen block** (`R/functions/trophic_levels.R` lines 6-92, from `#' Calculate Trophic Levels for a Food Web (Iterative Method)` down to the closing `}` before `#' Calculate Trophic Levels Using Shortest-Weighted Path Method`)

```r
#' Calculate Trophic Levels for a Food Web (Iterative Method)
#'
#' Computes trophic levels by fixed-point iteration under the edge contract
#' (prey -> predator, `adj[prey, predator]`; see R/functions/network_finalize.R).
#' Basal species (no incoming edge) get TL = 1; every other species gets
#' TL = 1 + mean(TL of its prey).
#'
#' @param net An igraph object representing the food web (directed graph)
#' @param max_iter Maximum number of iterations (default: 100)
#' @param convergence Convergence threshold (default: 0.0001)
#'
#' @return A named numeric vector, one trophic level per vertex. `NA` marks a
#'   vertex whose TL is undefined:
#'   \itemize{
#'     \item no path from any basal species (e.g. a pure cannibal loop), or
#'     \item still changing by more than `convergence` after `max_iter` passes.
#'   }
#'   Every `NA` is announced with a `warning()` naming the vertices. Callers
#'   must use `na.rm = TRUE` (or equivalent) when aggregating.
#'
#' @details
#' - A self-loop counts as an incoming edge, so a vertex whose only prey is
#'   itself is not basal.
#' - Only vertices reachable from a basal vertex are iterated, and a consumer
#'   averages only over prey in that reachable set.
#' - With no basal vertex at all, every TL is `NA` (one warning).
#'
#' @examples
#' tl <- calculate_trophic_levels(net)
#' mean(tl, na.rm = TRUE)  # Mean trophic level of the food web
#'
#' @references
#' Williams, R. J., & Martinez, N. D. (2004). Limits to trophic levels and
#' omnivory in complex food webs. Proceedings of the Royal Society B, 271(1540), 549-556.
#'
#' @export
calculate_trophic_levels <- function(net, max_iter = 100, convergence = 0.0001) {
  tryCatch({
    validate_network(net, require_directed = TRUE, min_vertices = 1)
    validate_numeric_range(max_iter, "max_iter", min = 1, max = 10000)
    validate_numeric_range(convergence, "convergence", min = 0, max = 1)

    n <- vcount(net)
    node_names <- V(net)$name
    if (is.null(node_names)) node_names <- as.character(seq_len(n))
    .name_list <- function(idx) {
      shown <- head(node_names[idx], 10)
      paste0(paste(shown, collapse = ", "), if (length(idx) > 10) ", ..." else "")
    }

    # adj[prey, predator]: prey of i are the non-zero rows of column i.
    adj <- as.matrix(as_adjacency_matrix(net, sparse = FALSE))
    basal <- which(colSums(adj) == 0)
    tl <- rep(NA_real_, n)

    if (length(basal) == 0) {
      warning(sprintf(
        "No basal species (every node has prey): all %d trophic levels are NA", n
      ), call. = FALSE)
    } else {
      hops <- igraph::distances(net, v = basal, mode = "out")
      reach <- which(colSums(is.finite(hops)) > 0)
      unreachable <- setdiff(seq_len(n), reach)
      if (length(unreachable) > 0) {
        warning(sprintf("%d node(s) have no path from a basal species: %s",
                        length(unreachable), .name_list(unreachable)), call. = FALSE)
      }

      tl[reach] <- 1
      consumers <- setdiff(reach, basal)
      prey_of <- lapply(consumers, function(i) intersect(which(adj[, i] > 0), reach))
      change <- rep(0, n)
      converged <- length(consumers) == 0
      iter <- 0
      while (!converged && iter < max_iter) {
        iter <- iter + 1
        tl_old <- tl
        for (k in seq_along(consumers)) {
          tl[consumers[k]] <- 1 + mean(tl[prey_of[[k]]])
        }
        change <- abs(tl - tl_old)
        change[is.na(change)] <- 0
        converged <- max(change) < convergence
      }

      if (!converged) {
        stuck <- which(change >= convergence)
        tl[stuck] <- NA_real_
        warning(sprintf(
          "Trophic levels of %d node(s) did not converge after %d iterations and are NA: %s",
          length(stuck), max_iter, .name_list(stuck)
        ), call. = FALSE)
      }
    }

    names(tl) <- V(net)$name
    tl
  }, error = function(e) {
    stop(sprintf("Failed to calculate trophic levels: %s", e$message), call. = FALSE)
  })
}
```

- [ ] **Step 5: Make `plotfw()` tolerate `NA` (`R/functions/network_visualization.R`)**

Replace (lines 61-66):

```r
  tl <- calculate_trophic_levels(net)
  dgpred <- tl

  bks <- c(0.9, seq(1.9, max(tl), length.out = nylevel))
  ynod <- cut(tl, breaks = bks, include.lowest = TRUE,
              labels = 1:(length(bks) - 1))
```

with:

```r
  tl <- calculate_trophic_levels(net)
  # NA = no path from a basal species / not converged (see calculate_trophic_levels).
  # Those nodes are drawn on their own "unplaced" bottom row at y = 0.5.
  placed <- !is.na(tl)
  tl_max <- if (any(placed)) max(tl[placed]) else 1

  bks <- c(0.9, seq(1.9, tl_max, length.out = nylevel))
  ynod <- cut(tl, breaks = bks, include.lowest = TRUE,
              labels = 1:(length(bks) - 1))
  ynod <- factor(ifelse(placed, as.character(ynod), as.character(nylevel + 1)),
                 levels = as.character(seq_len(nylevel + 1)))
```

(`dgpred` was never used and is dropped.) Then replace (lines 79-86):

```r
  coo <- cbind(xnod, tl)

  # y axis with 1 and continuous axis from 2 to max TL.
  yax <- c(1, seq(2, max(tl), length.out = ynum - 1))
  labax <- round(yax, 1)
  # rescale xax between -1 and 1
  yax_range <- max(yax) - min(yax)
  laby <- if (yax_range == 0) rep(0, length(yax)) else (yax - min(yax)) / yax_range * 2 - 1
```

with:

```r
  coo <- cbind(xnod, ifelse(placed, tl, 0.5))

  # y axis with 1 and continuous axis from 2 to max TL.
  yax <- c(1, seq(2, tl_max, length.out = ynum - 1))
  labax <- round(yax, 1)
  # plot.igraph rescales the layout's y range to [-1, 1]; map the ticks the same way
  y_lo <- min(coo[, 2])
  y_range <- max(coo[, 2]) - y_lo
  laby <- if (y_range == 0) rep(0, length(yax)) else (yax - y_lo) / y_range * 2 - 1
```

(When no TL is `NA`, the layout's y range equals `[1, max(tl)]`, so the ticks are unchanged.)

- [ ] **Step 6: Make `create_foodweb_visnetwork()` tolerate `NA`** (same file)

Replace (lines 147-148):

```r
  # Set Y positions (highest TL at top, lowest at bottom)
  y_pos <- (max(tl) - tl) * VIS_TROPHIC_LEVEL_SPACING
```

with:

```r
  # Set Y positions (highest TL at top, lowest at bottom). NA-TL nodes (no path
  # from a basal species) go one row below the lowest placed node.
  placed <- !is.na(tl)
  tl_top <- if (any(placed)) max(tl[placed]) else 1
  y_pos <- (tl_top - tl) * VIS_TROPHIC_LEVEL_SPACING
  y_pos[!placed] <- (if (any(placed)) max(y_pos[placed]) else 0) + VIS_TROPHIC_LEVEL_SPACING
  tl_group_key <- ifelse(placed, tl, -1)  # NA nodes share one x-spread group
```

Replace (lines 267-268; without this, `x_positions[i] <- numeric(0)` errors on an `NA` node):

```r
    tl_group <- round(tl[i] * 10) / 10
    nodes_in_group <- which(abs(tl - tl_group) < 0.1)
```

with:

```r
    tl_group <- round(tl_group_key[i] * 10) / 10
    nodes_in_group <- which(abs(tl_group_key - tl_group) < 0.1)
```

Replace (line 288):

```r
                     "Trophic Level: ", round(tl[i], 3), "<br>",
```

with:

```r
                     "Trophic Level: ",
                     if (placed[i]) round(tl[i], 3) else "NA (no path from a basal species)", "<br>",
```

- [ ] **Step 7: Make the topological indicators ignore `NA` (`R/functions/topological_metrics.R`)**

Replace (lines 19-20):

```r
    tlnodes <- calculate_trophic_levels(net)
    TL <- mean(tlnodes)
```

with:

```r
    tlnodes <- calculate_trophic_levels(net)
    # NA = no path from a basal species; excluded from the mean.
    TL <- if (all(is.na(tlnodes))) NA_real_ else mean(tlnodes, na.rm = TRUE)
```

Replace (line 93):

```r
    nwTL <- sum(tlnodes * biomass) / sum(biomass)
```

with:

```r
    # nwTL over nodes with a defined TL only, biomass re-normalised over those nodes.
    has_tl <- !is.na(tlnodes) & !is.na(biomass)
    nwTL <- if (any(has_tl) && sum(biomass[has_tl]) > 0) {
      sum(tlnodes[has_tl] * biomass[has_tl]) / sum(biomass[has_tl])
    } else {
      NA_real_
    }
```

(The omnivory `sd(..., na.rm = TRUE)` at line 24 already tolerates `NA`. `netmatrix * tlnodes` followed by `webtl[webtl == 0] <- NA` works with `NA` entries.)

- [ ] **Step 8: Replace the fixed 10-pass TL loop in `calculate_spatial_metrics()` (`R/functions/spatial_analysis.R`)**

Replace (inside `# Trophic levels`, lines 405-424):

```r
      if (igraph::vcount(net) > 0 && igraph::ecount(net) > 0) {
        # Simple trophic level: 1 + mean trophic level of prey
        # (This is a simplified version - full version needs basal species detection)
        tl <- rep(1, igraph::vcount(net))  # Default = basal

        # Iteratively calculate trophic levels
        for (iter in 1:10) {  # Max 10 iterations
          for (v in igraph::V(net)) {
            prey <- igraph::neighbors(net, v, mode = "in")
            if (length(prey) > 0) {
              tl[v] <- 1 + mean(tl[prey])
            }
          }
        }

        if ("meanTL" %in% metrics) {
          results$meanTL[i] <- mean(tl)
        }
        if ("maxTL" %in% metrics) {
          results$maxTL[i] <- max(tl)
        }
```

with:

```r
      if (igraph::vcount(net) > 0 && igraph::ecount(net) > 0) {
        # Same estimator as the Food Web tab. Under the prey -> predator
        # contract a node's prey are its in-neighbours (mode = "in").
        # NA = no path from a basal species; excluded from both summaries.
        tl <- calculate_trophic_levels(net)
        tl_defined <- !all(is.na(tl))

        if ("meanTL" %in% metrics) {
          results$meanTL[i] <- if (tl_defined) mean(tl, na.rm = TRUE) else NA_real_
        }
        if ("maxTL" %in% metrics) {
          results$maxTL[i] <- if (tl_defined) max(tl, na.rm = TRUE) else NA_real_
        }
```

(`calculate_trophic_levels()` reads prey as `adj[, i]`, the in-neighbours, so this keeps the spec's "keep `mode = "in"`" semantics. The edge at `extract_local_network` is flipped in Task 4, not here.)

- [ ] **Step 9: Add the edge-contract header to `R/functions/network_finalize.R`**

Replace (lines 18-20):

```r
#       then failed its required-columns check.

#' Candidate column names holding the species key
```

with:

```r
#       then failed its required-columns check.
#
# -----------------------------------------------------------------------------
# EDGE CONTRACT (app-wide; every importer, metric and plot relies on it)
# -----------------------------------------------------------------------------
#   * A directed edge A -> B means "A is eaten by B" (B eats A): prey -> predator,
#     the direction of energy flow.
#   * Adjacency matrices are indexed adj[prey, predator]; a node's prey are its
#     in-neighbours (column of adj), its predators its out-neighbours (row).
#   * Diet matrices are prey x predator, as in EwE and Rpath; each predator
#     column sums to 1. Edge attribute E(net)$diet_prop, when present, is
#     diet[prey, predator] for that edge.
#   * metaweb$interactions columns mean exactly what their names say:
#     predator_id eats prey_id, so metaweb_to_igraph() builds prey_id -> predator_id.
# Tests assert it with assert_prey_to_predator() (tests/testthat/helper-fixtures.R).

#' Candidate column names holding the species key
```

- [ ] **Step 10: Parse-check every edited file**

```bash
for f in R/functions/trophic_levels.R R/functions/network_visualization.R R/functions/topological_metrics.R \
         R/functions/spatial_analysis.R R/functions/network_finalize.R tests/testthat/helper-fixtures.R \
         tests/testthat/test-trophic-levels.R; do
  "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='$f'); cat('OK $f\n')"
done
```

Expected: 7 lines of `OK`.

- [ ] **Step 11: Run the new test file, then the full suite**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-trophic-levels.R')"`
Expected: PASS for all 10 tests (verified).

Run the full suite (command in Global Constraints). Expected: 0 failures.

- [ ] **Step 12: Commit**

```bash
git add R/functions/trophic_levels.R R/functions/network_visualization.R R/functions/topological_metrics.R \
        R/functions/spatial_analysis.R R/functions/network_finalize.R \
        tests/testthat/helper-fixtures.R tests/testthat/test-trophic-levels.R
git commit -m "$(cat <<'EOF'
fix(network): trophic levels are NA for unreachable or non-converged nodes (F69)

calculate_trophic_levels() iterates only nodes reachable from a basal node and
returns NA plus warning() for unreachable or non-converged nodes (previously
~101). plotfw, create_foodweb_visnetwork, topological/node-weighted indicators
and per-hexagon meanTL/maxTL tolerate NA; spatial TL now uses the same
estimator as the Food Web tab. Documents the prey -> predator edge contract in
network_finalize.R and adds assert_prey_to_predator() for tests.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 2: MTI / keystoneness rewrite (F65) and its UI

**Files:**
- Modify: `R/functions/keystoneness.R:1-159` (everything above the `# METAWEB MANAGEMENT FUNCTIONS` banner at line 161)
- Modify: `R/modules/analysis_server.R:163`, `:181`, `:197-199`, `:225`, `:240`, `:259-260`, `:282`
- Modify: `R/ui/keystoneness_ui.R:24-37`, `:60`, `:76-77`
- Modify: `R/ui/dashboard_ui.R:125`
- Create: `tests/testthat/test-mti-keystoneness.R`

**Interfaces:**
- Consumes: nothing from Task 1. The validators come from `R/functions/validation_utils.R`.
- Produces:
  - `calculate_mti(net, info)` returns a numeric n x n matrix with `dimnames = list(V(net)$name, V(net)$name)`. `MTI[i, j]` is the impact of i (row, impactor) on j. The diagonal is kept. `info` must have one row per vertex in vertex order, with `meanB` and optionally `QB`. When every edge carries `E(net)$diet_prop`, it is used; otherwise the diet is split equally.
  - `.mti_rcond(A)`: an internal seam (returns `rcond(A)`) that tests mock.
  - `calculate_keystoneness(net, info)` returns a data.frame with columns `species, overall_effect, relative_biomass, keystoneness, keystone_status, ks_rank`, sorted by `keystoneness` descending with `NA` last. `keystone_status` is one of `Keystone | Dominant | Other | Undefined`. `ks_rank` is an integer (`rank(-KS, ties = "min")`), or `NA` when KS is `NA`. `rownames` are `NULL`.

- [ ] **Step 1: Write the failing test file `tests/testthat/test-mti-keystoneness.R`**

```r
# =============================================================================
# calculate_mti() / calculate_keystoneness(): known answers (F65)
# =============================================================================
# The old MTI row-normalised over prey rows and returned -(I - DC)^-1 DC, which
# can never be positive, so producers never "helped" their consumers, and the
# old KS = log(1 + OE) / log(1 + p) was not Libralato's index. The known
# answers below were computed by hand from Ulanowicz & Puccia (1990) and
# Libralato et al. (2006) and re-verified independently (spec A, section 5).

source_app_dependencies()

local({
  root <- get_app_root()
  source(file.path(root, "R/functions/keystoneness.R"), local = FALSE)
})
suppressPackageStartupMessages(library(igraph))

# P -> Z -> F (P eaten by Z, Z eaten by F)
chain <- function() make_graph(c("P", "Z", "Z", "F"), directed = TRUE)
chain_info <- function() data.frame(species = c("P", "Z", "F"), meanB = c(10, 5, 1))

# P -> Z1, P -> Z2 (two grazers competing for one producer)
fan <- function() make_graph(c("P", "Z1", "P", "Z2"), directed = TRUE)
fan_info <- function() data.frame(species = c("P", "Z1", "Z2"), meanB = c(10, 3, 1))

test_that("MTI of a three-level chain matches the known answer", {
  mti <- calculate_mti(chain(), chain_info())
  # Rows = impactor, columns = impacted
  expected_x3 <- rbind(
    P = c(-1, 1, 1),
    Z = c(-1, -2, 1),
    F = c(1, -1, -1)
  )
  colnames(expected_x3) <- c("P", "Z", "F")
  expect_equal(3 * mti, expected_x3, tolerance = 1e-8)
})

test_that("a producer has a positive impact on its consumer", {
  mti <- calculate_mti(chain(), chain_info())
  # The F65 regression: the old formula could never return a positive value.
  expect_gt(mti["P", "Z"], 0)
})

test_that("predation pressure uses Q/B x B when QB is present", {
  mti <- calculate_mti(fan(), fan_info())
  expect_equal(mti["Z1", "Z2"], -0.375, tolerance = 1e-8)
  expect_equal(mti["Z2", "Z1"], -0.125, tolerance = 1e-8)

  info_qb <- fan_info()
  info_qb$QB <- c(0, 1, 9)  # consumption Z1 = 3, Z2 = 9: the shares swap
  mti_qb <- calculate_mti(fan(), info_qb)
  expect_equal(mti_qb["Z1", "Z2"], -0.125, tolerance = 1e-8)
  expect_equal(mti_qb["Z2", "Z1"], -0.375, tolerance = 1e-8)
})

test_that("diet_prop on the edges replaces the equal diet split", {
  # Z eats P1 (80%) and P2 (20%); with an equal split both would be 0.5.
  net <- make_graph(c("P1", "Z", "P2", "Z"), directed = TRUE)
  E(net)$diet_prop <- c(0.8, 0.2)
  info <- data.frame(species = c("P1", "Z", "P2"), meanB = c(1, 1, 1))
  mti <- calculate_mti(net, info)
  # Direct effect of each prey on Z is its diet share (no other paths here).
  expect_equal(mti["P1", "Z"] / mti["P2", "Z"], 4, tolerance = 1e-8)
})

test_that("keystoneness uses Libralato's epsilon and log(eps * (1 - p))", {
  ks <- calculate_keystoneness(chain(), chain_info())
  ks <- ks[match(c("P", "Z", "F"), ks$species), ]

  expect_equal(ks$overall_effect, rep(sqrt(2 / 9), 3), tolerance = 1e-7)
  expect_equal(ks$relative_biomass, c(10, 5, 1) / 16, tolerance = 1e-8)
  expect_equal(ks$keystoneness, c(-1.7328680, -1.1267321, -0.8165772), tolerance = 1e-7)
})

test_that("keystoneness returns the documented columns, statuses and ranks", {
  ks <- calculate_keystoneness(chain(), chain_info())

  expect_named(ks, c("species", "overall_effect", "relative_biomass",
                     "keystoneness", "keystone_status", "ks_rank"))
  expect_true(all(ks$keystone_status %in% c("Keystone", "Dominant", "Other", "Undefined")))
  # Sorted by KS descending; F (-0.817) is the only one in the top quartile
  # and holds 6.25% of biomass, so it is Dominant, not Keystone.
  expect_equal(ks$species, c("F", "Z", "P"))
  expect_equal(ks$ks_rank, 1:3)
  expect_equal(ks$keystone_status, c("Dominant", "Other", "Other"))
})

test_that("a low-biomass top-quartile species is Keystone", {
  # Same chain, but the top predator holds < 5% of the biomass.
  info <- data.frame(species = c("P", "Z", "F"), meanB = c(100, 50, 1))
  ks <- calculate_keystoneness(chain(), info)
  expect_equal(ks$keystone_status[ks$species == "F"], "Keystone")
})

test_that("an isolated node has undefined keystoneness, not an error", {
  net <- make_graph(c("P", "Z"), directed = TRUE) + vertex("X")
  info <- data.frame(species = c("P", "Z", "X"), meanB = c(10, 5, 1))
  ks <- calculate_keystoneness(net, info)
  expect_true(is.na(ks$keystoneness[ks$species == "X"]))
  expect_equal(ks$keystone_status[ks$species == "X"], "Undefined")
  expect_true(is.na(ks$ks_rank[ks$species == "X"]))
})

test_that("a web with no basal species still yields a finite MTI", {
  net <- make_graph(c("A", "B", "B", "A"), directed = TRUE)
  info <- data.frame(species = c("A", "B"), meanB = c(1, 1))
  expect_no_error(mti <- calculate_mti(net, info))
  expect_true(all(is.finite(mti)))
})

test_that("a singular (I - Q) falls back to the pseudo-inverse with a warning", {
  # No small food web makes (I - Q) numerically singular (exhaustive search of
  # every 2- and 3-node digraph), so force the branch through the rcond seam.
  with_mocked_function(globalenv(), ".mti_rcond", function(A) 0, {
    expect_warning(mti <- calculate_mti(chain(), chain_info()), "pseudo-inverse")
  })
  expect_true(all(is.finite(mti)))
  # For an invertible matrix the pseudo-inverse equals the inverse.
  expect_equal(3 * mti["P", "Z"], 1, tolerance = 1e-8)
})

test_that("partially missing diet_prop falls back to the equal split with a warning", {
  net <- make_graph(c("P1", "Z", "P2", "Z"), directed = TRUE)
  info <- data.frame(species = c("P1", "Z", "P2"), meanB = c(1, 1, 1))
  binary <- calculate_mti(net, info)
  E(net)$diet_prop <- c(0.8, NA)
  expect_warning(mixed <- calculate_mti(net, info), "equal diet split")
  expect_equal(mixed, binary, tolerance = 1e-12)
})

test_that("an NA biomass gives an Undefined status, not an error", {
  info <- data.frame(species = c("P", "Z", "F"), meanB = c(10, NA, 1))
  expect_no_error(ks <- calculate_keystoneness(chain(), info))
  expect_equal(ks$keystone_status[ks$species == "Z"], "Undefined")
  expect_true(all(ks$keystone_status[ks$species != "Z"] %in% c("Keystone", "Dominant", "Other")))
})
```

- [ ] **Step 2: Run it to verify it fails**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-mti-keystoneness.R')"`

Expected: FAIL in 10 of 12 tests (verified). Examples: the chain MTI does not match; `mti["P","Z"] <= 0`; the fan values differ; `ks_rank` is missing; the status is "Rare"; no "pseudo-inverse" warning. Two tests already pass on current code ("low-biomass Keystone" and "no basal finite MTI") and act as regression guards.

- [ ] **Step 3: Replace lines 1-159 of `R/functions/keystoneness.R`** (from `calculate_mti <- function(net, info) {` through the closing `}` of `calculate_keystoneness`; keep the blank line 160 and the `# ====... METAWEB MANAGEMENT FUNCTIONS` banner below it untouched)

```r
#' Calculate the Mixed Trophic Impact (MTI) matrix
#'
#' Ulanowicz & Puccia (1990) as used by Ecopath, on the app-wide edge contract
#' (A -> B means B eats A; every matrix below is indexed [prey, predator]).
#'
#' @param net Directed igraph food web. Optional edge attribute `diet_prop`
#'   (diet proportion of the edge's prey in the edge's predator's diet).
#' @param info Data frame aligned to `V(net)` (one row per vertex, same order,
#'   as produced by finalize_network()) with `meanB`; optional `QB`.
#'
#' @return Numeric n x n matrix with dimnames `V(net)$name`. `MTI[i, j]` is the
#'   net (direct + indirect) impact of a small increase of i on j. The diagonal
#'   is kept (self-impact); displays blank it.
#'
#' @details
#' \enumerate{
#'   \item \code{D}: `diet_prop` weighted adjacency when every edge has one,
#'     else the binary adjacency (equal diet split). Columns (predators)
#'     normalised to sum to 1; predators without prey keep a zero column.
#'   \item Consumption \code{cons_k = B_k * QB_k} where `QB_k` is finite and
#'     positive, else \code{cons_k = B_k} (biomass proxy).
#'   \item Flows \code{T[p, k] = D[p, k] * cons_k};
#'     \code{FC = T / rowSums(T)}: share of prey p's total predation taken by k.
#'   \item \code{Q = D - t(FC)}; \code{MTI = solve(I - Q) - I}. If
#'     \code{rcond(I - Q) < 1e-10} the Moore-Penrose inverse (MASS::ginv) is
#'     used and a warning() is raised.
#' }
#'
#' @references
#' Ulanowicz, R. E., & Puccia, C. J. (1990). Mixed trophic impacts in
#' ecosystems. Coenoses, 5(1), 7-16.
#' @export
calculate_mti <- function(net, info) {
  tryCatch({
    validate_network(net, require_directed = TRUE, min_vertices = 1)
    validate_dataframe(info, required_cols = "meanB")

    n <- vcount(net)
    if (nrow(info) != n) {
      stop(sprintf("Number of rows in 'info' (%d) must match number of vertices in 'net' (%d)",
                   nrow(info), n), call. = FALSE)
    }
    sp <- V(net)$name
    if (is.null(sp)) sp <- as.character(seq_len(n))

    # 1. Diet composition D[prey, predator], columns sum to 1
    has_diet <- "diet_prop" %in% igraph::edge_attr_names(net)
    if (has_diet && anyNA(igraph::E(net)$diet_prop)) {
      warning("[calculate_mti] some edges have no diet_prop; using an equal diet split for all",
              call. = FALSE)
      has_diet <- FALSE
    }
    D <- if (has_diet) {
      as.matrix(igraph::as_adjacency_matrix(net, attr = "diet_prop", sparse = FALSE))
    } else {
      as.matrix(igraph::as_adjacency_matrix(net, sparse = FALSE))
    }
    D[!is.finite(D) | D < 0] <- 0
    col_tot <- colSums(D)
    D <- sweep(D, 2, ifelse(col_tot > 0, col_tot, 1), "/")

    # 2. Consumption per predator: B * Q/B where available, else biomass proxy
    B <- as.numeric(info$meanB)
    cons <- B
    if ("QB" %in% names(info)) {
      qb <- suppressWarnings(as.numeric(info$QB))
      use_qb <- is.finite(qb) & qb > 0
      cons[use_qb] <- B[use_qb] * qb[use_qb]
    }
    cons[!is.finite(cons) | cons < 0] <- 0

    # 3. Flows and predation shares FC[prey, predator]
    flows <- sweep(D, 2, cons, "*")
    row_tot <- rowSums(flows)
    FC <- flows / ifelse(row_tot > 0, row_tot, 1)

    # 4. Net impacts
    I <- diag(n)
    A <- I - (D - t(FC))
    if (.mti_rcond(A) < 1e-10) {
      warning("[calculate_mti] (I - Q) is singular or near-singular; using the pseudo-inverse",
              call. = FALSE)
      A_inv <- MASS::ginv(A)
    } else {
      A_inv <- solve(A)
    }
    MTI <- A_inv - I
    dimnames(MTI) <- list(sp, sp)
    MTI
  }, error = function(e) {
    stop(sprintf("Failed to calculate Mixed Trophic Impact (MTI): %s", e$message), call. = FALSE)
  })
}

#' Reciprocal condition number used by calculate_mti()'s singularity check
#'
#' A named seam so tests can force the pseudo-inverse branch.
#' @keywords internal
.mti_rcond <- function(A) rcond(A)

#' Calculate Keystoneness Index
#'
#' Libralato et al. (2006) keystoneness from the MTI matrix.
#'
#' @param net An igraph object representing the food web (see calculate_mti())
#' @param info Data frame aligned to `V(net)` with `meanB` (optional `QB`)
#'
#' @return A data frame sorted by `keystoneness` (descending, `NA` last) with
#'   columns:
#' \describe{
#'   \item{species}{Species name}
#'   \item{overall_effect}{epsilon_i = sqrt(sum over j != i of MTI[i, j]^2)}
#'   \item{relative_biomass}{p_i = B_i / sum(B)}
#'   \item{keystoneness}{KS_i = log(epsilon_i * (1 - p_i)); NA when undefined}
#'   \item{keystone_status}{"Keystone", "Dominant", "Other" or "Undefined"}
#'   \item{ks_rank}{Rank by KS, 1 = highest; NA when KS is NA}
#' }
#'
#' @details
#' Classification (a design choice; Libralato ranks without cut-offs):
#' \itemize{
#'   \item Keystone: KS >= 75th percentile of KS and p < 0.05
#'   \item Dominant: KS >= 75th percentile of KS and p >= 0.05
#'   \item Other: every other species with a finite KS
#'   \item Undefined: KS not finite (e.g. epsilon = 0 or p = 1)
#' }
#'
#' @references
#' Libralato, S., Christensen, V., & Pauly, D. (2006). A method for identifying
#' keystone species in food web models. Ecological Modelling, 195(3-4), 153-171.
#' @export
calculate_keystoneness <- function(net, info) {
  tryCatch({
    validate_network(net, require_directed = TRUE, min_vertices = 1)
    validate_dataframe(info, required_cols = "meanB")

    if (nrow(info) != vcount(net)) {
      stop(sprintf("Number of rows in 'info' (%d) must match number of vertices in 'net' (%d)",
                   nrow(info), vcount(net)), call. = FALSE)
    }

    MTI <- calculate_mti(net, info)

    # epsilon_i: overall effect of impactor i (row), self-impact excluded
    off_diag <- MTI
    diag(off_diag) <- 0
    overall_effect <- sqrt(rowSums(off_diag^2))

    biomass <- as.numeric(info$meanB)
    total_biomass <- sum(biomass, na.rm = TRUE)
    if (!(total_biomass > 0)) {
      stop("Total biomass must be positive to calculate keystoneness", call. = FALSE)
    }
    relative_biomass <- biomass / total_biomass

    keystoneness <- suppressWarnings(log(overall_effect * (1 - relative_biomass)))
    keystoneness[!is.finite(keystoneness)] <- NA_real_

    ks_cut <- if (all(is.na(keystoneness))) {
      NA_real_
    } else {
      stats::quantile(keystoneness, 0.75, na.rm = TRUE, names = FALSE)
    }
    top <- !is.na(keystoneness) & keystoneness >= ks_cut
    keystone_status <- ifelse(
      is.na(keystoneness), "Undefined",
      ifelse(top & relative_biomass < 0.05, "Keystone",
             ifelse(top, "Dominant", "Other"))
    )

    results <- data.frame(
      species = rownames(MTI),
      overall_effect = unname(overall_effect),
      relative_biomass = relative_biomass,
      keystoneness = unname(keystoneness),
      keystone_status = keystone_status,
      ks_rank = as.integer(rank(-keystoneness, ties.method = "min", na.last = "keep")),
      stringsAsFactors = FALSE
    )

    results <- results[order(-results$keystoneness, na.last = TRUE), ]
    rownames(results) <- NULL
    results
  }, error = function(e) {
    stop(sprintf("Failed to calculate keystoneness indices: %s", e$message), call. = FALSE)
  })
}
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-mti-keystoneness.R')"`
Expected: PASS, 12/12 (verified against this exact code).

- [ ] **Step 5: Update `R/modules/analysis_server.R` for the new statuses and matrix orientation**

Seven exact replacements:

1. Line 163. Replace `            c('Keystone', 'Dominant', 'Rare', 'Undefined'),` with `            c('Keystone', 'Dominant', 'Other', 'Undefined'),`.
2. Line 181. Replace `        "Rare" = "#999999",` with `        "Other" = "#999999",`.
3. Lines 197-199. Replace

```r
      # Add reference lines
      abline(h = 1, lty = 2, col = "gray50")
      abline(v = 0.05, lty = 2, col = "gray50")
```

with

```r
      # Reference lines: the Keystone/Dominant cut-offs used by calculate_keystoneness()
      ks_cut <- stats::quantile(ks_results$keystoneness, 0.75, na.rm = TRUE, names = FALSE)
      abline(h = ks_cut, lty = 2, col = "gray50")
      abline(v = 0.05, lty = 2, col = "gray50")
```

4. Line 225. Replace `      text(0.001, 1, "Keystone threshold", pos = 3, cex = 0.7, col = "gray50")` with `      text(0.001, ks_cut, "Top-quartile KS", pos = 3, cex = 0.7, col = "gray50")`.
5. Line 240. Replace `      mti_matrix <- calculate_mti(net, info)` with

```r
      mti_matrix <- calculate_mti(net, info)
      diag(mti_matrix) <- NA  # self-impact is kept in the data, blanked for display
```

6. Lines 259-260 (`stats::heatmap` draws matrix rows on the y axis and columns on the x axis, and rows are now the impactor). Replace

```r
        xlab = "Impacting Species (impactor)",
        ylab = "Impacted Species",
```

with

```r
        xlab = "Impacted Species",
        ylab = "Impacting Species (impactor = row)",
```

7. Line 282. Replace `      cat("Rare species:", sum(ks_results$keystone_status == "Rare", na.rm = TRUE), "\n\n")` with `      cat("Other species:", sum(ks_results$keystone_status == "Other", na.rm = TRUE), "\n\n")`.

(`order = list(list(3, 'desc'))` at line 157 still targets `keystoneness`, which remains the fourth column; `ks_rank` is appended after it. The `bindCache` at line 139 needs no change; see spec section 7.)

- [ ] **Step 6: Update the keystoneness help (`R/ui/keystoneness_ui.R`)**

Replace lines 24-37, from `              <h5>Key Concepts:</h5>` through the `</ul>` that closes the classification list (the line after `<li><strong>Rare:</strong> Low impact, low biomass</li>`), with:

```html
              <h5>Key Concepts:</h5>
              <ul>
                <li><strong>Mixed Trophic Impact (MTI):</strong> net effect (direct + indirect) of a small increase
                of one species on every other (Ulanowicz &amp; Puccia 1990). Producers now show positive impacts
                on their consumers, and predators negative impacts on their prey.</li>
                <li><strong>Overall Effect (&epsilon;):</strong> sqrt of the sum of squared MTI values of a
                species on all others (its own self-impact excluded)</li>
                <li><strong>Keystoneness Index (KS):</strong> log(&epsilon; &times; (1 - p)), where p is the species'
                share of total biomass (Libralato et al. 2006). Higher KS = larger impact for its biomass.</li>
              </ul>

              <h5>Species Classifications:</h5>
              <ul>
                <li><strong>Keystone:</strong> KS in the top quartile of the web and biomass &lt; 5% of total</li>
                <li><strong>Dominant:</strong> KS in the top quartile and biomass &ge; 5% of total</li>
                <li><strong>Other:</strong> every other species with a defined KS</li>
                <li><strong>Undefined:</strong> KS cannot be computed (no impact, or the only biomass in the web)</li>
              </ul>

              <h5>Approximations used when EwE data are missing:</h5>
              <ul>
                <li>Without Q/B (consumption/biomass), predation pressure on a prey is apportioned by predator
                biomass instead of predator consumption (B &times; Q/B).</li>
                <li>Without diet proportions (e.g. binary metawebs and trait-based webs), each predator's diet is
                split equally among its prey.</li>
                <li>Detritus is treated as an ordinary prey; EwE routes it through detritus fate, so impacts on
                and of detritus are indicative only.</li>
              </ul>
```

Replace line 60, `            helpText("Keystone species appear in upper-left (high impact, low biomass)")`, with:

```r
            helpText("Keystone species appear top-left: KS above the dashed top-quartile line, biomass left of 5%")
```

Replace lines 76-77:

```html
                <li>Rows = Impacted species</li>
                <li>Columns = Impacting species (impactor)</li>
```

with:

```html
                <li>Rows = Impacting species (impactor)</li>
                <li>Columns = Impacted species</li>
                <li>The diagonal (self-impact) is left blank</li>
```

- [ ] **Step 7: Update the function list in `R/ui/dashboard_ui.R`**

Replace line 125, `                    <li><code>calculate_keystoneness()</code> - Keystoneness index (impact/biomass ratio)</li>`, with `                    <li><code>calculate_keystoneness()</code> - Keystoneness index (Libralato et al. 2006)</li>`.

- [ ] **Step 8: Parse-check and run the suite**

```bash
for f in R/functions/keystoneness.R R/modules/analysis_server.R R/ui/keystoneness_ui.R R/ui/dashboard_ui.R \
         tests/testthat/test-mti-keystoneness.R; do
  "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='$f'); cat('OK $f\n')"
done
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "print(lintr::lint('R/functions/keystoneness.R'))"
```

Expected: 5 `OK`s. lintr reports 8 lints, all pre-existing style in the untouched metaweb section below line 160 (the old file had 18).

Run the full suite. Expected: 0 failures.

- [ ] **Step 9: Commit**

```bash
git add R/functions/keystoneness.R R/modules/analysis_server.R R/ui/keystoneness_ui.R R/ui/dashboard_ui.R \
        tests/testthat/test-mti-keystoneness.R
git commit -m "$(cat <<'EOF'
fix(network): Ulanowicz-Puccia MTI and Libralato keystoneness (F65)

MTI normalises diet columns (predators), apportions predation by Q/B x B (or
biomass), and computes (I - (D - t(FC)))^-1 - I, so producers now have
positive impacts on consumers. KS = log(eps * (1 - p)); Keystone/Dominant
are the top KS quartile split at p = 0.05, else Other/Undefined, plus
ks_rank. Uses E(net)$diet_prop when every edge has it. UI help documents the
Q/B and equal-diet-split approximations; the heatmap blanks the diagonal.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 3: EwE importers (F48, F55, `diet_prop`) and the trait food-web export (N3)

**Files:**
- Modify: `R/functions/ecopath/ecopath_csv.R:71-72`, `:84-87`, `:106-117`
- Modify: `R/functions/ecobase_connection.R:440-444` and `:626-630` (the same block, twice)
- Modify: `R/modules/ecopath_import_server.R:329-330`
- Modify: `R/functions/trait_foodweb.R:337-342`
- Modify: `R/functions/functional_group_utils.R:12-13`, `:29-31`, `:127-137` (comments only)
- Create: `tests/testthat/test-edge-contract.R` (first part)

**Interfaces:**
- Consumes: `assert_prey_to_predator()` and `calculate_trophic_levels()` from Task 1.
- Produces: `E(net)$diet_prop`, a numeric edge attribute equal to `diet[prey, predator]` for that edge. It is set by `parse_ecopath_data()`, both EcoBase converters and the native `.ewemdb` import. `trait_foodweb_to_igraph()` edges run resource -> consumer.

These changes do not depend on the metaweb flip, so this commit is green by itself. The intermediate state was verified.

- [ ] **Step 1: Write the failing test file `tests/testthat/test-edge-contract.R` (part 1)**

```r
# =============================================================================
# Edge contract: A -> B means "A is eaten by B" (prey -> predator) everywhere
# =============================================================================
# The contract is documented in the header of R/functions/network_finalize.R.
# Before A1 several ingest boundaries built predator -> prey edges (F64, F56,
# F48, N1, N3), and four bundled metaweb CSVs stored their columns swapped so
# that two of those bugs cancelled. Every test here pins one boundary.

source_app_dependencies()

local({
  root <- get_app_root()
  source(file.path(root, "R/config.R"), local = FALSE)
  source(file.path(root, "R/functions/functional_group_utils.R"), local = FALSE)
  source(file.path(root, "R/functions/network_finalize.R"), local = FALSE)
  source(file.path(root, "R/functions/trophic_levels.R"), local = FALSE)
  source(file.path(root, "R/functions/metaweb_core.R"), local = FALSE)
  source(file.path(root, "R/functions/spatial_analysis.R"), local = FALSE)
  source(file.path(root, "R/functions/ecopath/ecopath_csv.R"), local = FALSE)
  source(file.path(root, "R/functions/trait_foodweb.R"), local = FALSE)
})
suppressPackageStartupMessages(library(igraph))

# ---------------------------------------------------------------------------
# F48 / F55 - parse_ecopath_data (CSV/Excel EwE import)
# ---------------------------------------------------------------------------

write_ewe_csv <- function() {
  basic <- tempfile(fileext = ".csv")
  diet <- tempfile(fileext = ".csv")
  writeLines(c(
    "Group,Biomass,PB,QB",
    "Phyto,20,100,0",
    "Detritus,50,0,0",
    "Zoo,5,30,100",
    "Cod,1,0.5,3",
    "Apex,0.2,1.5,4"
  ), basic)
  # EwE layout: rows = prey, columns = predators, columns sum to 1.
  writeLines(c(
    "Prey,Phyto,Detritus,Zoo,Cod,Apex",
    "Phyto,0,0,0.6,0,0",
    "Detritus,0,0,0.4,0,0",
    "Zoo,0,0,0,1,0",
    "Cod,0,0,0,0,1",
    "Apex,0,0,0,0,0"
  ), diet)
  c(basic = basic, diet = diet)
}

test_that("parse_ecopath_data keeps EwE's prey x predator orientation (F48)", {
  files <- write_ewe_csv()
  on.exit(unlink(files), add = TRUE)
  res <- parse_ecopath_data(files[["basic"]], files[["diet"]])

  assert_prey_to_predator(res$net, "Phyto", "Zoo")
  assert_prey_to_predator(res$net, "Zoo", "Cod")
  tl <- calculate_trophic_levels(res$net)
  expect_equal(unname(tl[c("Phyto", "Detritus", "Zoo", "Cod", "Apex")]), c(1, 1, 2, 3, 4))
})

test_that("parse_ecopath_data retains the Detritus group (F55)", {
  files <- write_ewe_csv()
  on.exit(unlink(files), add = TRUE)
  res <- parse_ecopath_data(files[["basic"]], files[["diet"]])

  expect_true("Detritus" %in% V(res$net)$name)
  assert_prey_to_predator(res$net, "Detritus", "Zoo")
})

test_that("the topology heuristic sees a top predator, not a producer (F48)", {
  files <- write_ewe_csv()
  on.exit(unlink(files), add = TRUE)
  res <- parse_ecopath_data(files[["basic"]], files[["diet"]])

  # "Apex" matches no name pattern, so topology decides. With inverted edges
  # it had in-degree 0 and P/B > 1 and was classified Phytoplankton.
  expect_equal(as.character(res$info["Apex", "fg"]), "Fish")
  expect_false(as.character(res$info["Cod", "fg"]) == "Phytoplankton")
})

test_that("blank cells in an EwE diet CSV are read as zero", {
  basic <- tempfile(fileext = ".csv")
  diet <- tempfile(fileext = ".csv")
  on.exit(unlink(c(basic, diet)), add = TRUE)
  writeLines(c("Group,Biomass,PB,QB", "Phyto,20,100,0", "Zoo,5,30,100", "Cod,1,0.5,3"), basic)
  writeLines(c("Prey,Phyto,Zoo,Cod", "Phyto,,1,", "Zoo,,,1", "Cod,,,"), diet)

  expect_no_error(res <- parse_ecopath_data(basic, diet))
  expect_equal(ecount(res$net), 2)
  assert_prey_to_predator(res$net, "Phyto", "Zoo")
})

# ---------------------------------------------------------------------------
# E(net)$diet_prop on the EwE importers
# ---------------------------------------------------------------------------

test_that("CSV-imported edges carry their diet proportion", {
  files <- write_ewe_csv()
  on.exit(unlink(files), add = TRUE)
  res <- parse_ecopath_data(files[["basic"]], files[["diet"]])

  expect_true("diet_prop" %in% edge_attr_names(res$net))
  expect_equal(E(res$net)[.from("Phyto") & .to("Zoo")]$diet_prop, 0.6)
  expect_equal(E(res$net)[.from("Detritus") & .to("Zoo")]$diet_prop, 0.4)
})

test_that("EcoBase-imported edges carry their diet proportion", {
  out <- load_fixture("ecobase_model_403_output")
  inp <- load_fixture("ecobase_model_403_input")
  meta <- load_fixture("ecobase_model_403_metadata")
  res <- with_mocked_function(globalenv(), "get_ecobase_model_output", function(id) out,
    with_mocked_function(globalenv(), "get_ecobase_model_input", function(id) inp,
      with_mocked_function(globalenv(), "get_ecobase_model_metadata", function(id) meta,
        suppressWarnings(suppressMessages(convert_ecobase_to_econetool_hybrid(403)))
      )
    )
  )

  expect_gt(ecount(res$net), 0)
  expect_true("diet_prop" %in% edge_attr_names(res$net))
  expect_true(all(E(res$net)$diet_prop > 0 & E(res$net)$diet_prop <= 1))
})

test_that("the native ECOPATH import keeps diet_prop on its edges", {
  # The .ewemdb parser is a closure inside the Shiny module; guard the line.
  code <- readLines(app_path("R/modules/ecopath_import_server.R"), warn = FALSE)
  code <- code[!startsWith(trimws(code), "#")]
  expect_true(any(grepl("E(net)$diet_prop <-", code, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# N3 - trait_foodweb_to_igraph
# ---------------------------------------------------------------------------

test_that("trait food webs are exported prey -> predator (N3)", {
  # The "simple" example dataset from foodweb_construction_server.R
  simple <- data.frame(
    species = c("Predatory_fish", "Small_fish", "Zooplankton", "Phytoplankton", "Benthic_filter_feeder"),
    MS = c("MS5", "MS3", "MS2", "MS1", "MS3"),
    FS = c("FS1", "FS1", "FS6", "FS0", "FS6"),
    MB = c("MB5", "MB5", "MB4", "MB2", "MB1"),
    EP = c("EP4", "EP4", "EP3", "EP4", "EP2"),
    PR = c("PR0", "PR0", "PR0", "PR0", "PR6"),
    stringsAsFactors = FALSE
  )
  g <- trait_foodweb_to_igraph(simple)
  expect_gt(ecount(g), 0)

  # The primary producer (FS0) eats nothing.
  expect_equal(unname(degree(g, "Phytoplankton", mode = "in")), 0)
  # Every edge ends at a consumer: its head is never the FS0 producer.
  heads <- as_edgelist(g)[, 2]
  expect_false(any(simple$FS[match(heads, simple$species)] == "FS0"))
  assert_prey_to_predator(g, "Phytoplankton", "Benthic_filter_feeder")
})
```

(The trait model gives Zooplankton no prey in this dataset: `calc_interaction_probability` returns 0 for FS6 x MS1. The spec's intended pair is therefore replaced with Phytoplankton -> Benthic_filter_feeder, a link the trait model does produce.)

- [ ] **Step 2: Run it to verify it fails**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-edge-contract.R')"`

Expected: FAIL in all 8 tests (verified). Examples: Phyto->Zoo is reversed; TL comes out Phyto 3 / Apex 1; Detritus is missing; Apex is "Phytoplankton"; the blank cells error; there is no `diet_prop` edge attribute; the Phytoplankton in-degree is 3.

- [ ] **Step 3: Fix `parse_ecopath_data()` (`R/functions/ecopath/ecopath_csv.R`)**

Replace (lines 71-72):

```r
  not_summary <- !grepl("^sum$|^total$|^import$|^export$|^detritus$",
                        tolower(raw_names))
```

with:

```r
  # "Detritus" is a real EwE group (TL 1, eaten by detritivores), not a
  # summary row, so it is NOT in this filter (F55).
  not_summary <- !grepl("^sum$|^total$|^import$|^export$",
                        tolower(raw_names))
```

Replace (lines 84-87):

```r
  # Process Diet Composition matrix
  # First column is prey names, rest are predators
  diet_matrix <- as.matrix(diet_data[, -1])
  rownames(diet_matrix) <- as.character(diet_data[[1]])
```

with:

```r
  # Process Diet Composition matrix
  # First column is prey names, rest are predators: diet_matrix[prey, predator]
  diet_matrix <- as.matrix(diet_data[, -1])
  storage.mode(diet_matrix) <- "numeric"
  diet_matrix[is.na(diet_matrix)] <- 0  # blank EwE cells mean "not eaten"
  rownames(diet_matrix) <- as.character(diet_data[[1]])
```

Replace (lines 106-117):

```r
  # Convert diet proportions to binary adjacency matrix
  # In ECOPATH: columns are predators, rows are prey
  # In our format: rows are predators, columns are prey
  # So we need to transpose
  adjacency_matrix <- t(diet_matrix > 0) * 1

  # Ensure rownames and colnames are preserved after transpose
  rownames(adjacency_matrix) <- colnames(diet_matrix)
  colnames(adjacency_matrix) <- rownames(diet_matrix)

  # Create igraph network
  net <- igraph::graph_from_adjacency_matrix(adjacency_matrix, mode = "directed")
```

with:

```r
  # Convert diet proportions to binary adjacency matrix. EwE's diet matrix is
  # already prey x predator, which is exactly the app's edge contract
  # (adj[prey, predator], edge prey -> predator) - no transpose (F48).
  adjacency_matrix <- (diet_matrix > 0) * 1
  rownames(adjacency_matrix) <- rownames(diet_matrix)
  colnames(adjacency_matrix) <- colnames(diet_matrix)

  # Create igraph network; edge (prey, predator) carries its diet proportion
  net <- igraph::graph_from_adjacency_matrix(adjacency_matrix, mode = "directed")
  igraph::E(net)$diet_prop <- diet_matrix[igraph::as_edgelist(net, names = FALSE)]
```

(`as_edgelist(names = FALSE)` returns the (prey index, predator index) pairs, so indexing `diet_matrix` with it reads `diet[prey, predator]` for every edge, in edge order. The indegree/outdegree topology heuristic at lines 127-138 now receives the correct direction without any code change.)

- [ ] **Step 4: Add `diet_prop` to both EcoBase converters (`R/functions/ecobase_connection.R`)**

The same block occurs twice: at lines 440-444 in `convert_ecobase_to_econetool` and at 626-630 in `convert_ecobase_to_econetool_hybrid`. Replace **both** occurrences of:

```r
  # Create network
  net <- graph_from_adjacency_matrix(adjacency_matrix, mode = "directed")

  # Explicitly set vertex names to ensure they're preserved
  V(net)$name <- species_names
```

with:

```r
  # Create network (edge contract: prey -> predator, adj[prey, predator])
  net <- graph_from_adjacency_matrix(adjacency_matrix, mode = "directed")
  # Edge (prey, predator) carries diet_matrix[prey, predator] for MTI/keystoneness
  E(net)$diet_prop <- diet_matrix[as_edgelist(net, names = FALSE)]

  # Explicitly set vertex names to ensure they're preserved
  V(net)$name <- species_names
```

(Both converters fill `diet_matrix[prey_idx, i]`, where `i` is the predator, so no orientation change is needed.)

- [ ] **Step 5: Add `diet_prop` to the native `.ewemdb` import (`R/modules/ecopath_import_server.R`)**

Replace (lines 329-330):

```r
      net <- igraph::graph_from_adjacency_matrix(adjacency_matrix, mode = "directed")
      net <- igraph::upgrade_graph(net)
```

with:

```r
      net <- igraph::graph_from_adjacency_matrix(adjacency_matrix, mode = "directed")
      net <- igraph::upgrade_graph(net)
      # Keep the diet proportions on the edges for MTI/keystoneness:
      # edge (prey, predator) carries diet_matrix[prey, predator].
      igraph::E(net)$diet_prop <- diet_matrix[igraph::as_edgelist(net, names = FALSE)]
```

- [ ] **Step 6: Flip the trait food-web export (N3, `R/functions/trait_foodweb.R`)**

Replace (lines 337-342):

```r
  # Create edge list from probabilities above threshold
  edges <- which(prob_matrix >= threshold, arr.ind = TRUE)
  edge_list <- data.frame(
    from = rownames(prob_matrix)[edges[, 1]],
    to = colnames(prob_matrix)[edges[, 2]],
    probability = prob_matrix[edges]
  )
```

with:

```r
  # Create edge list from probabilities above threshold.
  # prob_matrix is [consumer, resource]; the graph follows the app-wide edge
  # contract resource (prey) -> consumer (predator), so the columns swap here.
  edges <- which(prob_matrix >= threshold, arr.ind = TRUE)
  edge_list <- data.frame(
    from = colnames(prob_matrix)[edges[, 2]],
    to = rownames(prob_matrix)[edges[, 1]],
    probability = prob_matrix[edges]
  )
```

(`construct_trait_foodweb()` and its heatmap keep rows = consumers and are unchanged.)

- [ ] **Step 7: Correct the comments in `R/functions/functional_group_utils.R`** (the logic is already right for prey -> predator)

Replace lines 12-13:

```r
#' @param indegree Numeric, network in-degree (optional, for topology-based assignment)
#' @param outdegree Numeric, network out-degree (optional, for topology-based assignment)
```

with:

```r
#' @param indegree Numeric, network in-degree = number of prey under the
#'   prey -> predator edge contract (optional, for topology-based assignment)
#' @param outdegree Numeric, network out-degree = number of predators
#'   (optional, for topology-based assignment)
```

Replace lines 29-31:

```r
#'    - No predators + high P/B → Phytoplankton
#'    - Has predators, no prey → Top predator (Fish)
#'    - Has both → Intermediate (Benthos)
```

with:

```r
#'    - No prey (in-degree 0) + high P/B → Phytoplankton
#'    - Has prey, no predators (out-degree 0) → Top predator (Fish)
#'    - Has both → Intermediate (Benthos)
```

Replace lines 127-137:

```r
    # No predators (indegree = 0) + high production rate → Primary producer
    if (indegree == 0 && pb > 1) {
      return("Phytoplankton")
    }

    # Has predators but no prey (top predator) → Fish
    if (indegree > 0 && outdegree == 0) {
      return("Fish")
    }

    # Has both predators and prey (intermediate consumer) → Benthos
```

with:

```r
    # Edge contract prey -> predator: in-degree = number of prey,
    # out-degree = number of predators.
    # No prey (indegree = 0) + high production rate → Primary producer
    if (indegree == 0 && pb > 1) {
      return("Phytoplankton")
    }

    # Has prey but no predators (top predator) → Fish
    if (indegree > 0 && outdegree == 0) {
      return("Fish")
    }

    # Has both prey and predators (intermediate consumer) → Benthos
```

- [ ] **Step 8: Parse-check, run the file, run the suite**

```bash
for f in R/functions/ecopath/ecopath_csv.R R/functions/ecobase_connection.R R/modules/ecopath_import_server.R \
         R/functions/trait_foodweb.R R/functions/functional_group_utils.R tests/testthat/test-edge-contract.R; do
  "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='$f'); cat('OK $f\n')"
done
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-edge-contract.R')"
```

Expected: 6 `OK`s. The test file passes 8/8 (verified). Then run the full suite. Expected: 0 failures, including the three existing `parse_ecopath_data` tests in `test-deep-analysis-fixes.R` (verified).

- [ ] **Step 9: Commit**

```bash
git add R/functions/ecopath/ecopath_csv.R R/functions/ecobase_connection.R R/modules/ecopath_import_server.R \
        R/functions/trait_foodweb.R R/functions/functional_group_utils.R tests/testthat/test-edge-contract.R
git commit -m "$(cat <<'EOF'
fix(import): EwE CSV keeps prey x predator, keeps Detritus, carries diet_prop (F48, F55, N3)

parse_ecopath_data no longer transposes the EwE diet matrix (every CSV/Excel
link was reversed and the FG topology heuristic inverted), keeps the Detritus
group, and reads blank diet cells as 0. CSV, native .ewemdb and both EcoBase
converters set E(net)$diet_prop. trait_foodweb_to_igraph exports
resource -> consumer.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 4 (ATOMIC): Metaweb orientation: F64, F56 edge, N1, N2, migration and regenerated data

**Files:**
- Modify: `R/functions/metaweb_core.R:127-138` (`metaweb_to_igraph`); insert a new function before line 158 (`#' Get link quality description`)
- Modify: `R/functions/spatial_analysis.R:256-259` (`extract_local_network`)
- Modify: `R/modules/ecopath_import_server.R:1804-1812` (N1 writer)
- Modify: `R/modules/metaweb_manager_server.R:162-165` (N2)
- Create: `scripts/initialization/fix_metaweb_orientation.R`
- Regenerate (by running the script): `metawebs/baltic/baltic_kortsch2021_interactions.csv` and `.rds`, `metawebs/arctic/barents_boreal_kortsch2015_interactions.csv` and `.rds`, `metawebs/arctic/barents_arctic_kortsch2015_interactions.csv` and `.rds`, `metawebs/atlantic/north_sea_frelat2022_interactions.csv` and `.rds`
- Modify: `R/ui/import_ui.R:304`, `:311-313`, `:348-350`, `:365-366`, `:375-378`; `R/ui/metaweb_ui.R:112`; `R/ui/dataeditor_ui.R:67`, `:69`; `metawebs/README.md:80-81`
- Modify: `tests/testthat/test-edge-contract.R` (append part 2)

**Interfaces:**
- Consumes: `assert_prey_to_predator()` and `calculate_trophic_levels()` (Task 1), and the part-1 test file (Task 3).
- Produces:
  - `metaweb_to_igraph(metaweb)` builds edges from `interactions[, c("prey_id", "predator_id")]`. Vertices are still named by `species_id`; A3 renames them to `species_name`. Edge attributes `quality_code` and `source` are unchanged.
  - `igraph_to_metaweb_interactions(net, quality_code = 3, source = "unspecified")` returns a data.frame with columns `predator_id, prey_id, quality_code, source`. For every edge A -> B it writes `prey_id = A` and `predator_id = B`. It is the exact inverse of `metaweb_to_igraph`'s edge step.
  - The bundled `.rds` files carry `metadata$orientation_fixed == "2026-09 (A1)"` on the four migrated metawebs.

**Why this is one commit:** on master, the bundled-metaweb test already fails for Kongsfjorden. After F64 alone it fails for the other four; this was verified: 12 failures. After the migration alone, all five are inverted. Only F64 together with the migration makes it pass. N1 without F64 would invert every EwE-derived metaweb, and the F56 flip without the migration would invert per-hexagon TL for the four swapped metawebs. The steps below reach green **before** the commit, so no commit on the branch is red.

- [ ] **Step 1: Append part 2 to `tests/testthat/test-edge-contract.R`** (after the N3 test)

```r

# ---------------------------------------------------------------------------
# F64 - metaweb_to_igraph, and the bundled metawebs it reads
# ---------------------------------------------------------------------------

# Vertex labels by species name. metaweb_to_igraph() names vertices by
# species_id today (A3 renames them to species_name); species_name travels as
# a vertex attribute, so this works either way.
vertex_labels <- function(g) {
  if (!is.null(V(g)$species_name)) V(g)$species_name else V(g)$name
}

BASAL_PATTERN <- "detrit|phyto|diatom|autotroph|macroalgae|microalgae"

test_that("metaweb_to_igraph builds prey -> predator edges (F64)", {
  # Cod eats herring, herring eats Calanus. IDs equal names so the assertion
  # is independent of which column names the vertices.
  sp <- c("Gadus morhua", "Clupea harengus", "Calanus finmarchicus")
  mw <- create_metaweb(
    species = data.frame(species_id = sp, species_name = sp, stringsAsFactors = FALSE),
    interactions = data.frame(predator_id = sp[1:2], prey_id = sp[2:3], stringsAsFactors = FALSE)
  )
  g <- metaweb_to_igraph(mw)

  assert_prey_to_predator(g, "Clupea harengus", "Gadus morhua")
  assert_prey_to_predator(g, "Calanus finmarchicus", "Clupea harengus")
  tl <- calculate_trophic_levels(g)
  expect_equal(unname(tl[sp]), c(3, 2, 1))
})

test_that("every bundled metaweb has detritus and producers at the base", {
  # Guards the migration: fails for Kongsfjorden before F64, for the other
  # four after F64 until scripts/initialization/fix_metaweb_orientation.R runs.
  for (key in names(METAWEB_PATHS)) {
    rds <- app_path(METAWEB_PATHS[[key]])
    expect_true(file.exists(rds), label = paste(key, ".rds exists"))
    g <- metaweb_to_igraph(readRDS(rds))
    basal <- grepl(BASAL_PATTERN, vertex_labels(g), ignore.case = TRUE)
    expect_true(any(basal), label = paste(key, "has basal-named vertices"))

    expect_equal(unname(degree(g, V(g)[basal], mode = "in")), rep(0, sum(basal)),
                 label = paste(key, "basal vertices have no prey"))
    tl <- suppressWarnings(calculate_trophic_levels(g))
    expect_equal(unname(tl[basal]), rep(1, sum(basal)),
                 label = paste(key, "basal TL"))
    expect_lt(mean(tl[basal], na.rm = TRUE), mean(tl[!basal], na.rm = TRUE),
              label = paste(key, "basal mean TL below consumers"))
  }
})

# ---------------------------------------------------------------------------
# F56 - extract_local_network + per-hexagon TL
# ---------------------------------------------------------------------------

test_that("local networks are prey -> predator and get correct per-hexagon TL (F56)", {
  sp <- c("Phyto", "Zoo1", "Zoo2", "Fish")
  mw <- create_metaweb(
    species = data.frame(species_id = sp, species_name = sp, stringsAsFactors = FALSE),
    interactions = data.frame(predator_id = c("Zoo1", "Zoo2", "Fish"),
                              prey_id = c("Phyto", "Phyto", "Zoo1"),
                              stringsAsFactors = FALSE)
  )
  local_net <- extract_local_network(mw, sp, "HEX_1")

  assert_prey_to_predator(local_net, "Phyto", "Zoo1")
  assert_prey_to_predator(local_net, "Zoo1", "Fish")

  metrics <- calculate_spatial_metrics(list(HEX_1 = local_net),
                                       metrics = c("meanTL", "maxTL"), progress = FALSE)
  # TL: Phyto 1, Zoo1 2, Zoo2 2, Fish 3
  expect_equal(metrics$meanTL, 2, tolerance = 1e-6)
  expect_equal(metrics$maxTL, 3, tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# N1 - EwE -> metaweb writer
# ---------------------------------------------------------------------------

test_that("the EwE -> metaweb writer labels prey and predator correctly (N1)", {
  native_net <- make_graph(c("Phyto", "Zoo", "Zoo", "Cod", "Detritus", "Zoo"), directed = TRUE)
  interactions <- igraph_to_metaweb_interactions(native_net, quality_code = 3, source = "test")

  expect_equal(interactions$prey_id[interactions$predator_id == "Cod"], "Zoo")
  expect_setequal(interactions$prey_id[interactions$predator_id == "Zoo"], c("Phyto", "Detritus"))

  sp <- V(native_net)$name
  mw <- create_metaweb(
    species = data.frame(species_id = sp, species_name = sp, stringsAsFactors = FALSE),
    interactions = interactions
  )
  back <- metaweb_to_igraph(mw)
  edge_key <- function(g) sort(apply(as_edgelist(g), 1, paste, collapse = "->"))
  expect_equal(edge_key(back), edge_key(native_net))
})

test_that("the ECOPATH import module writes metawebs through the shared writer (N1)", {
  code <- readLines(app_path("R/modules/ecopath_import_server.R"), warn = FALSE)
  code <- code[!startsWith(trimws(code), "#")]
  expect_true(any(grepl("igraph_to_metaweb_interactions(", code, fixed = TRUE)))
  expect_false(any(grepl("predator_id = edges[,1]", code, fixed = TRUE)))
})

test_that("the metaweb preview draws arrows prey -> predator (N2)", {
  code <- readLines(app_path("R/modules/metaweb_manager_server.R"), warn = FALSE)
  code <- code[!startsWith(trimws(code), "#")]
  expect_true(any(grepl("from = metaweb$interactions$prey_id", code, fixed = TRUE)))
  expect_false(any(grepl("from = metaweb$interactions$predator_id", code, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# Guard: no predator -> prey edge-list construct in the graph builders
# ---------------------------------------------------------------------------

test_that("no graph builder constructs a predator -> prey edge list", {
  # Scoped to the two edge-list constructs that fed graph_from_data_frame();
  # c("predator_id", "prey_id") legitimately appears elsewhere (required
  # columns, duplicate-link detection in merge_metawebs()).
  edge_list_constructs <- c(
    'metaweb$interactions[, c("predator_id", "prey_id")]',
    'local_interactions[, c("predator_id", "prey_id")]'
  )
  for (f in c("R/functions/metaweb_core.R", "R/functions/spatial_analysis.R")) {
    code <- readLines(app_path(f), warn = FALSE)
    code <- code[!startsWith(trimws(code), "#")]
    for (pat in edge_list_constructs) {
      expect_false(any(grepl(pat, code, fixed = TRUE)),
                   label = paste(f, "builds predator -> prey edges via", pat))
    }
  }
})
```

- [ ] **Step 2: Run it to verify the new tests fail**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-edge-contract.R')"`

Expected (verified): the 8 part-1 tests pass. Six of the 7 part-2 tests fail, and the N1 round trip errors with `could not find function "igraph_to_metaweb_interactions"`. The bundled-metaweb test fails only for `kongsfjorden_farage2021`.

- [ ] **Step 3: Flip `metaweb_to_igraph()` and add the writer helper (`R/functions/metaweb_core.R`)**

Replace (lines 127-138):

```r
#' Convert metaweb to igraph network
#'
#' @param metaweb Metaweb object
#' @return igraph object
#' @export
metaweb_to_igraph <- function(metaweb) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required")
  }

  # Create edge list
  edges <- metaweb$interactions[, c("predator_id", "prey_id")]
```

with:

```r
#' Convert metaweb to igraph network
#'
#' Edges follow the app-wide contract (R/functions/network_finalize.R):
#' `prey_id -> predator_id`, i.e. an edge A -> B means B eats A.
#'
#' @param metaweb Metaweb object
#' @return igraph object; vertices are named by `species_id`
#' @export
metaweb_to_igraph <- function(metaweb) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required")
  }

  # Create edge list: prey -> predator
  edges <- metaweb$interactions[, c("prey_id", "predator_id")]
```

Then replace the line `#' Get link quality description` (line 158 on master; about line 161 after the edit above) with:

```r
#' Build a metaweb interactions table from a prey -> predator igraph
#'
#' The inverse of the edge step in metaweb_to_igraph(): for every edge
#' A -> B (B eats A) it writes `prey_id = A`, `predator_id = B`.
#'
#' @param net igraph network following the edge contract, vertices named
#' @param quality_code Link quality code (1-4) for every row
#' @param source Source string for every row
#' @return data.frame with predator_id, prey_id, quality_code, source
#' @export
igraph_to_metaweb_interactions <- function(net, quality_code = 3, source = "unspecified") {
  edges <- igraph::as_edgelist(net, names = TRUE)
  data.frame(
    predator_id = edges[, 2],
    prey_id = edges[, 1],
    quality_code = rep(quality_code, nrow(edges)),
    source = rep(source, nrow(edges)),
    stringsAsFactors = FALSE
  )
}

#' Get link quality description
```

- [ ] **Step 4: Flip the local-network edge (F56, `R/functions/spatial_analysis.R`)**

Replace (lines 256-259):

```r
  # Create igraph network
  if (nrow(local_interactions) > 0) {
    local_net <- igraph::graph_from_data_frame(
      d = local_interactions[, c("predator_id", "prey_id")],
```

with:

```r
  # Create igraph network. Edge contract: prey -> predator (B eats A for A -> B).
  if (nrow(local_interactions) > 0) {
    local_net <- igraph::graph_from_data_frame(
      d = local_interactions[, c("prey_id", "predator_id")],
```

(Per-hexagon TL already reads prey as in-neighbours via `calculate_trophic_levels()` (Task 1). **Do not** also switch to `mode = "out"`; that would re-invert TL. See the spec appendix note on F56.)

- [ ] **Step 5: Route the EwE -> metaweb writer through the helper (N1, `R/modules/ecopath_import_server.R`)**

Replace (lines 1804-1812 on master; about 1807-1815 after Task 3 inserted 3 lines at line 330. `edges` is used only inside this block, which was checked with grep):

```r
        # Create interactions data frame for metaweb (use native_net, not reactive value)
        edges <- as_edgelist(native_net)
        interactions_data <- data.frame(
          predator_id = edges[,1],
          prey_id = edges[,2],
          quality_code = 3,  # ECOPATH data = code 3 (model-derived)
          source = paste0("ECOPATH: ", input$ecopath_native_file$name),
          stringsAsFactors = FALSE
        )
```

with:

```r
        # Create interactions data frame for metaweb (use native_net, not reactive value).
        # native_net is prey -> predator, so edge column 1 is the prey (N1).
        interactions_data <- igraph_to_metaweb_interactions(
          native_net,
          quality_code = 3,  # ECOPATH data = code 3 (model-derived)
          source = paste0("ECOPATH: ", input$ecopath_native_file$name)
        )
```

- [ ] **Step 6: Flip the metaweb preview arrows (N2, `R/modules/metaweb_manager_server.R`)**

Replace (lines 162-165):

```r
    # Create edges
    edges <- data.frame(
      from = metaweb$interactions$predator_id,
      to = metaweb$interactions$prey_id,
```

with:

```r
    # Create edges: prey -> predator (energy flow), as in the Food Web tab
    edges <- data.frame(
      from = metaweb$interactions$prey_id,
      to = metaweb$interactions$predator_id,
```

- [ ] **Step 7: Run the test file. Expect exactly one remaining failure: the bundled data**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-edge-contract.R')"`

Expected (verified): every test passes except "every bundled metaweb has detritus and producers at the base". That test fails 12 expectations: 3 each for `baltic_kortsch2021`, `barents_arctic_kortsch2015`, `barents_boreal_kortsch2015` and `north_sea_frelat2022`. Kongsfjorden now passes. **Do not commit here.** The data migration below is part of the same commit.

- [ ] **Step 8: Create `scripts/initialization/fix_metaweb_orientation.R`**

```r
#!/usr/bin/env Rscript
# =============================================================================
# fix_metaweb_orientation.R - one-off data migration for PR A1
# =============================================================================
# Four bundled metawebs were produced out of tree with their interaction
# columns swapped: the value under `predator_id` is really the prey. Until A1,
# metaweb_to_igraph() built predator -> prey edges, which swapped them back, so
# the two errors cancelled. A1 makes metaweb_to_igraph() follow the edge
# contract (prey_id -> predator_id), so these files must now say what their
# column names mean.
#
# For each listed metaweb this script:
#   1. checks orientation from the columns alone: basal-named species
#      (detritus / producers) must appear only as prey_id;
#   2. if the CSV is inverted, swaps predator_id <-> prey_id and rewrites it;
#   3. rebuilds the .rds from the CSVs with import_metaweb_csv(), keeping the
#      old metadata and adding metadata$orientation_fixed.
# A file that already passes the check is never swapped again, so a second run
# is a no-op. Kongsfjorden (arctic/kongsfjorden_farage2021) is already correct
# and is deliberately not listed.
#
# Usage (from anywhere):  Rscript scripts/initialization/fix_metaweb_orientation.R
# =============================================================================

.find_app_root <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  start <- if (length(file_arg) > 0) dirname(sub("^--file=", "", file_arg[1])) else getwd()
  dir <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(dir, "app.R")) && dir.exists(file.path(dir, "R", "functions"))) {
      return(dir)
    }
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("Cannot locate the EcoNeTool root (app.R + R/functions)")
    dir <- parent
  }
}

options(econetool.app_root = .find_app_root())
source(file.path(getOption("econetool.app_root"), "R", "functions", "validation_utils.R"))
source(app_path("R/functions/metaweb_core.R"))
source(app_path("R/functions/metaweb_io.R"))

SWAPPED_METAWEBS <- c(
  "baltic/baltic_kortsch2021",
  "arctic/barents_boreal_kortsch2015",
  "arctic/barents_arctic_kortsch2015",
  "atlantic/north_sea_frelat2022"
)
BASAL_PATTERN <- "detrit|phyto|diatom|autotroph|macroalgae|microalgae"
ORIENTATION_TAG <- "2026-09 (A1)"

#' TRUE when basal-named species occur only as prey (never as predator_id)
orientation_ok <- function(species, interactions) {
  basal_ids <- species$species_id[grepl(BASAL_PATTERN, species$species_name, ignore.case = TRUE)]
  if (length(basal_ids) == 0) {
    stop("no detritus/producer-named species; orientation cannot be checked")
  }
  !any(interactions$predator_id %in% basal_ids) && any(interactions$prey_id %in% basal_ids)
}

for (stem in SWAPPED_METAWEBS) {
  species_file <- app_path("metawebs", paste0(stem, "_species.csv"))
  interactions_file <- app_path("metawebs", paste0(stem, "_interactions.csv"))
  rds_file <- app_path("metawebs", paste0(stem, ".rds"))

  species <- read.csv(species_file, stringsAsFactors = FALSE)
  interactions <- read.csv(interactions_file, stringsAsFactors = FALSE)
  old <- readRDS(rds_file)

  csv_ok <- orientation_ok(species, interactions)
  rds_ok <- orientation_ok(old$species, old$interactions)
  if (csv_ok && rds_ok) {
    cat(sprintf("SKIP  %s: already prey/predator-correct, not touched\n", stem))
    next
  }

  if (!csv_ok) {
    fixed <- interactions
    fixed$predator_id <- interactions$prey_id
    fixed$prey_id <- interactions$predator_id
    if (!orientation_ok(species, fixed)) {
      stop(sprintf("%s: swapping the columns does not fix the orientation; inspect by hand", stem))
    }
    # Binary connection: keep LF line endings on Windows (repo is eol=lf).
    con <- file(interactions_file, open = "wb")
    write.csv(fixed, con, row.names = FALSE)
    close(con)
  }

  metadata <- old$metadata
  metadata$orientation_fixed <- ORIENTATION_TAG
  rebuilt <- import_metaweb_csv(species_file, interactions_file, metadata = metadata)
  saveRDS(rebuilt, rds_file)
  cat(sprintf("FIXED %s: %d interactions, csv %s, rds rebuilt\n",
              stem, nrow(rebuilt$interactions), if (csv_ok) "unchanged" else "swapped"))
}
```

The bootstrap `source(file.path(getOption(...), ...))` is the one unavoidable non-`app_path` source, because it is what defines `app_path`. The `test-deep-analysis-fixes.R` guard scans `R/` only. The orientation check is column-based on purpose, so it does not depend on which `metaweb_to_igraph()` is loaded. If the CSV is already fixed but the `.rds` is stale (an interrupted run), the script rebuilds only the `.rds`.

- [ ] **Step 9: Run the migration, then run it again to prove idempotence**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/initialization/fix_metaweb_orientation.R
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/initialization/fix_metaweb_orientation.R
git status --short metawebs/
git diff --stat metawebs/
```

Expected. These results come from a verified dry run on a copy.
- First run: four `FIXED ...: csv swapped, rds rebuilt` lines, for Baltic (207 interactions), Barents boreal (1546), Barents arctic (848) and North Sea (1760).
- Second run: four `SKIP` lines.
- `git status`: exactly 8 modified files (4 `_interactions.csv` + 4 `.rds`), with **no** Kongsfjorden file and no `_species.csv`.
- CSV diffs: every data row changes except self-loops. The approximate line counts are 206 / 1519 / 838 / 1724. The header and the quoting style are unchanged: swapping back reproduces the original bytes exactly (verified).

- [ ] **Step 10: Sanity-check the regenerated `.rds`**

```bash
cat > /tmp/a1_rds_check.R <<'EOF'
for (stem in c("baltic/baltic_kortsch2021", "arctic/barents_boreal_kortsch2015",
               "arctic/barents_arctic_kortsch2015", "atlantic/north_sea_frelat2022")) {
  mw <- readRDS(file.path("metawebs", paste0(stem, ".rds")))
  csv <- read.csv(file.path("metawebs", paste0(stem, "_interactions.csv")), stringsAsFactors = FALSE)
  stopifnot(inherits(mw, "metaweb"), identical(mw$metadata$orientation_fixed, "2026-09 (A1)"),
            isTRUE(all.equal(mw$interactions, csv)), !is.null(mw$metadata$citation))
  cat("OK", stem, "\n")
}
EOF
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" /tmp/a1_rds_check.R
```

Expected: 4 `OK` lines. The metadata keeps every old key, for example `citation`, `region` and `url`. The species frames are `all.equal` to the old ones; only a floating-point round trip through CSV differs, in Baltic `biomass`.

- [ ] **Step 11: Document the column semantics in the help** (the spec's "one sentence", plus the two help texts that currently state the **opposite** orientation)

`R/ui/metaweb_ui.R` line 112. Replace:

```r
                    HTML("<p><small>Use template files in <code>metawebs/</code> folder as examples.</small></p>")
```

with:

```r
                    HTML(paste0(
                      "<p><small>Use template files in <code>metawebs/</code> folder as examples. ",
                      "Each interactions row means <code>predator_id</code> eats <code>prey_id</code>; ",
                      "the network drawn from it points prey &rarr; predator (direction of energy flow).",
                      "</small></p>"
                    ))
```

`R/ui/import_ui.R`. The uploaded-matrix help says "Species A eats Species B (row -> column)" and its example makes Fish the prey of Zooplankton. `parse_adjacency_df()` and the data editor both treat rows as prey, so the help is what is wrong. Replace line 304:

```html
              <p><em>Value = 1 means Species A eats Species B (row → column)</em></p>
```

with:

```html
              <p><em>Rows = prey, columns = predators: a 1 in row A, column B means B eats A
              (edge A → B, prey → predator). Here B eats A and C eats B.</em></p>
```

Replace lines 311-313:

```html
                  <tr><td>Species_A</td><td>Fish</td><td>1250.5</td><td>0.12</td><td>0.85</td></tr>
                  <tr><td>Species_B</td><td>Zooplankton</td><td>850.2</td><td>0.08</td><td>0.75</td></tr>
                  <tr><td>Species_C</td><td>Phytoplankton</td><td>2100.0</td><td>0.05</td><td>0.40</td></tr>
```

with:

```html
                  <tr><td>Species_A</td><td>Phytoplankton</td><td>2100.0</td><td>0.05</td><td>0.40</td></tr>
                  <tr><td>Species_B</td><td>Zooplankton</td><td>850.2</td><td>0.08</td><td>0.75</td></tr>
                  <tr><td>Species_C</td><td>Fish</td><td>1250.5</td><td>0.12</td><td>0.85</td></tr>
```

Replace lines 348-350:

```text
Species_A,Fish,1250.5,0.12,0.85
Species_B,Zooplankton,850.2,0.08,0.75
Species_C,Phytoplankton,2100.0,0.05,0.40</pre>
```

with:

```text
Species_A,Phytoplankton,2100.0,0.05,0.40
Species_B,Zooplankton,850.2,0.08,0.75
Species_C,Fish,1250.5,0.12,0.85</pre>
```

Replace lines 365-366:

```r
# Create adjacency matrix
adj_matrix <- matrix(c(0,1,0, 0,0,1, 0,0,0), nrow=3, byrow=TRUE)
```

with:

```r
# Create adjacency matrix: rows = prey, columns = predators (B eats A, C eats B)
adj_matrix <- matrix(c(0,1,0, 0,0,1, 0,0,0), nrow=3, byrow=TRUE)
```

Replace lines 375-378:

```r
  fg = factor(c('Fish', 'Zooplankton', 'Phytoplankton')),
  meanB = c(1250.5, 850.2, 2100.0),
  losses = c(0.12, 0.08, 0.05),
  efficiencies = c(0.85, 0.75, 0.40)
```

with:

```r
  fg = factor(c('Phytoplankton', 'Zooplankton', 'Fish')),
  meanB = c(2100.0, 850.2, 1250.5),
  losses = c(0.05, 0.08, 0.12),
  efficiencies = c(0.40, 0.75, 0.85)
```

`R/ui/dataeditor_ui.R`. The round trip `as_adjacency_matrix` -> `graph_from_adjacency_matrix` is already contract-correct; only the text is wrong. Replace line 67:

```html
                <p>Edit the food web structure. Values should be 0 (no interaction) or 1 (predator eats prey).</p>
```

with:

```html
                <p>Edit the food web structure. Values should be 0 (no interaction) or 1
                (the column species eats the row species).</p>
```

Replace line 69:

```html
                <p><em>Rows = Predators, Columns = Prey. Value of 1 in row i, column j means species i eats species j.</em></p>
```

with:

```html
                <p><em>Rows = Prey, Columns = Predators. Value of 1 in row i, column j means species j eats species i
                (edge i → j, prey → predator).</em></p>
```

`metawebs/README.md` lines 80-81. Replace:

```markdown
   - `predator_id`: ID of predator species
   - `prey_id`: ID of prey species
```

with:

```markdown
   - `predator_id`: ID of the predator (the species that eats)
   - `prey_id`: ID of the prey (the species that is eaten)
   - In the igraph network built from a metaweb, every edge points
     `prey_id -> predator_id` (direction of energy flow), the contract used
     throughout EcoNeTool (see `R/functions/network_finalize.R`).
```

- [ ] **Step 12: Parse-check, lint the new script, run the file and the full suite**

```bash
for f in R/functions/metaweb_core.R R/functions/spatial_analysis.R R/modules/ecopath_import_server.R \
         R/modules/metaweb_manager_server.R R/ui/metaweb_ui.R R/ui/import_ui.R R/ui/dataeditor_ui.R \
         scripts/initialization/fix_metaweb_orientation.R tests/testthat/test-edge-contract.R; do
  "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='$f'); cat('OK $f\n')"
done
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "print(lintr::lint('scripts/initialization/fix_metaweb_orientation.R')); print(lintr::lint('tests/testthat/test-edge-contract.R'))"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-edge-contract.R')"
```

Expected: 9 `OK`s, and "No lints found" twice. The test file passes 15/15 (verified). Then run the full suite. Expected: 0 failures, with the pass count above the baseline by the new tests.

The whole A1 change set was verified on a partial scratch copy of the repo: 1179 passes. The only failures were `test-deploy-preserve.R` and `test-offline-traits.R`, and both come from `deploy.sh` and `data/external_traits/` being absent from that partial copy. The same failures appear on an unmodified copy. No existing test file regressed.

- [ ] **Step 13: Commit (the atomic one)**

```bash
git add R/functions/metaweb_core.R R/functions/spatial_analysis.R R/modules/ecopath_import_server.R \
        R/modules/metaweb_manager_server.R R/ui/metaweb_ui.R R/ui/import_ui.R R/ui/dataeditor_ui.R \
        metawebs/README.md scripts/initialization/fix_metaweb_orientation.R tests/testthat/test-edge-contract.R \
        metawebs/baltic/baltic_kortsch2021_interactions.csv metawebs/baltic/baltic_kortsch2021.rds \
        metawebs/arctic/barents_boreal_kortsch2015_interactions.csv metawebs/arctic/barents_boreal_kortsch2015.rds \
        metawebs/arctic/barents_arctic_kortsch2015_interactions.csv metawebs/arctic/barents_arctic_kortsch2015.rds \
        metawebs/atlantic/north_sea_frelat2022_interactions.csv metawebs/atlantic/north_sea_frelat2022.rds
git status --short   # nothing else staged; WBGIFSV5ISSUE70.pdf stays untracked
git commit -m "$(cat <<'EOF'
fix(network)!: unify prey->predator edge contract

metaweb_to_igraph() and extract_local_network() build prey_id -> predator_id
edges (F64, F56); the EwE -> metaweb writer goes through the new
igraph_to_metaweb_interactions() (N1); the metaweb preview draws arrows
prey -> predator (N2). scripts/initialization/fix_metaweb_orientation.R
swaps the predator_id/prey_id columns of the four bundled metawebs that
stored them inverted (Baltic Kortsch 2021, Barents boreal/arctic Kortsch
2015, North Sea Frelat 2022) and rebuilds their .rds with metadata
preserved plus orientation_fixed; Kongsfjorden was already correct.
Help texts now state rows = prey, columns = predators.

BREAKING CHANGE: metaweb interactions CSVs are read literally (predator_id
eats prey_id). User metawebs built from the template or uploaded CSVs,
the Kongsfjorden metaweb, CSV/Excel EwE imports and trait food-web
RDS/GraphML exports change orientation; earlier exports from those paths
are inverted and should be regenerated.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 5: Branch verification and PR

**Files:** none (no version bump in A1)

- [ ] **Step 1: Full suite and a parse check of every R file touched on the branch**

```bash
git diff --name-only master...HEAD -- '*.R' | while read f; do
  "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='$f'); cat('OK $f\n')"
done
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "r <- testthat::test_dir('tests/testthat', stop_on_failure = FALSE); d <- as.data.frame(r); cat('pass', sum(d$passed), 'fail', sum(d$failed), 'skip', sum(d$skipped), '\n')"
```

Expected: every file prints `OK`, and the suite shows 0 failures. Passes exceed the Task 0 baseline by the expectations of the three new files, which add 37 `test_that` blocks: 10 trophic, 12 MTI and 15 edge contract. The skip count is unchanged, apart from the visNetwork skip if that package is missing.

- [ ] **Step 2: Smoke-check the app still starts** (optional but cheap; `run_app.R` is the entry point)

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "source('app.R'); cat('app.R sourced OK\n')"`
Expected: `app.R sourced OK`, with no error. Startup warnings that already appeared on master are fine.

- [ ] **Step 3: Push and open the PR**

```bash
git push -u origin fix/a1-edge-contract
gh pr create --base master --head fix/a1-edge-contract \
  --title "fix(network)!: unify prey->predator edge contract (A1)" \
  --body "$(cat <<'EOF'
## Summary
Sub-project A, PR A1 (spec `docs/superpowers/specs/2026-09-26-fix-a-network-science-correctness-design.md`).
One atomic PR: every graph builder follows prey -> predator, the four bundled metawebs with swapped
columns are migrated, MTI/keystoneness are replaced with Ulanowicz-Puccia / Libralato, and
unreachable/non-converged trophic levels are NA.

- F69: `calculate_trophic_levels()` returns NA + warning() for unreachable / non-converged nodes; plotfw,
  visNetwork, topological/node-weighted indicators and per-hexagon meanTL/maxTL tolerate NA.
- F65: MTI = (I - (D - t(FC)))^-1 - I with column-normalised diets and Q/B x B (or biomass) predation
  shares; KS = log(eps * (1 - p)); Keystone/Dominant = top KS quartile split at p = 0.05; `ks_rank`.
- F48/F55: EwE CSV/Excel import keeps prey x predator and the Detritus group; `E(net)$diet_prop` on CSV,
  native and EcoBase imports.
- F64/F56/N1/N2/N3: metaweb_to_igraph, extract_local_network, EwE->metaweb writer, metaweb preview arrows
  and trait food-web export all prey -> predator.
- Migration: `scripts/initialization/fix_metaweb_orientation.R` (idempotent) + regenerated CSV/.rds for
  Baltic Kortsch 2021, Barents boreal/arctic Kortsch 2015, North Sea Frelat 2022. Kongsfjorden untouched.
- Legacy `tests/test_phase1_*.R` / `run_all_tests.R` spatial cases only print TL values; no expectation
  needed updating.

## CHANGELOG draft - "Results changed" (to be inserted in the 1.5.0 block after A3)
- **MTI / keystoneness - every source.** Formula replaced (Ulanowicz & Puccia 1990; Libralato et al.
  2006). Producers can now have positive impacts. Earlier KS values and statuses ("Rare" is now "Other")
  are not comparable.
- **Trophic levels changed for:**
  - CSV/Excel EwE imports - previously inverted; functional-group heuristics also shift, and the Detritus
    group is now kept;
  - the Kongsfjorden metaweb;
  - user metawebs built from the template or uploaded CSVs - previously inverted;
  - per-hexagon meanTL/maxTL from those metawebs (and now the same estimator as the Food Web tab).
- **Unchanged TL:** the Baltic, Barents (x2) and North Sea bundled metawebs and metawebs derived from
  `.ewemdb` - two errors cancelled for these.
- Nodes with no path from a basal species now show TL `NA` instead of about 101.
- Earlier exports (CSV, RDS, GraphML) from the "changed" paths above - including trait food-web RDS/GraphML -
  are inverted and should be regenerated.
- Metaweb graph arrows now point prey -> predator, as in the Food Web tab.
(A2 adds the Rpath TL / detritus items; A3 adds the import-attribute items.)

## Test plan
- [ ] `testthat::test_dir("tests/testthat")`: 0 failures
- [ ] new: test-trophic-levels.R (10), test-mti-keystoneness.R (12), test-edge-contract.R (15)
- [ ] migration re-run prints SKIP x4; `git status` shows only the 8 migrated files under metawebs/
- [ ] reviewer: open Keystoneness tab on the Baltic default web - statuses Keystone/Dominant/Other,
      heatmap diagonal blank, help lists the Q/B and diet-split approximations

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

Recommend **squash-merge** with the Task 4 subject line and its `BREAKING CHANGE:` footer as the squash message (spec section 6 commit style).

Deployment is **not** part of A1. A is deployed after A3 (overview, "Deploy after A"). When it is deployed, the verification grep from spec section 6 must be `grep -F 'c("prey_id", "predator_id")' metaweb_core.R`. The implemented line has a space after the comma, so the spec's `"prey_id","predator_id"` would not match. `metawebs/` ships with `-SkipData`.

---

## Interfaces produced (relied on by A2 / A3)

| Interface | Exact contract |
|---|---|
| `assert_prey_to_predator(net, prey, predator)` | In `tests/testthat/helper-fixtures.R`. Two expectations: `igraph::are_adjacent(net, prey, predator)` is TRUE and `igraph::are_adjacent(net, predator, prey)` is FALSE. `prey`/`predator` are vertex names or ids. |
| `metaweb_to_igraph(metaweb)` | Edges come from `metaweb$interactions[, c("prey_id", "predator_id")]`, so an edge runs prey -> predator. Vertices come from `metaweb$species` and are **still named by `species_id`**; the other species columns (including `species_name`) are vertex attributes. Edge attributes: `quality_code`, `source`. A3 renames the vertices to `make.unique(species_name)`; the edge order stays. `test-edge-contract.R` reads labels through `vertex_labels()` (`V(g)$species_name` when present, else `V(g)$name`), so the bundled-metaweb test survives the A3 rename. The F64 unit test uses `species_id == species_name`. |
| `igraph_to_metaweb_interactions(net, quality_code = 3, source = "unspecified")` | New, in `metaweb_core.R`. Returns `data.frame(predator_id = head, prey_id = tail, quality_code, source)`, the inverse of `metaweb_to_igraph`'s edge step. Used by the ECOPATH import module (N1). |
| `E(net)$diet_prop` | A numeric edge attribute equal to `diet[prey, predator]` of that edge (0 < x <= 1). It is set by `parse_ecopath_data()`, the native `.ewemdb` import in `ecopath_import_server.R`, `convert_ecobase_to_econetool()` and `convert_ecobase_to_econetool_hybrid()`. It is absent on metaweb-, trait- and matrix-upload-derived graphs, and on graphs rebuilt by the data editor. |
| `calculate_mti(net, info)` | Returns a numeric n x n matrix with `dimnames = list(V(net)$name, V(net)$name)`. `MTI[i, j]` is the impact of i (row) on j (column). The diagonal is kept. `info` must have one row per vertex in vertex order with `meanB` (QB optional). It uses `diet_prop` only when every edge has a non-NA value; otherwise it warns and uses the equal split. When `.mti_rcond(I - Q) < 1e-10` it uses `MASS::ginv` and raises `warning()`. Errors are wrapped as `"Failed to calculate Mixed Trophic Impact (MTI): ..."`. |
| `calculate_keystoneness(net, info)` | Returns a data.frame with columns `species` (chr), `overall_effect` (num, epsilon), `relative_biomass` (num, p), `keystoneness` (num, `log(eps * (1 - p))`, NA if not finite), `keystone_status` (chr in `Keystone`, `Dominant`, `Other`, `Undefined`) and `ks_rank` (int, `rank(-KS, ties = "min")`, NA when KS is NA). Rows are sorted by KS descending with NA last; `rownames` are NULL. Errors are wrapped as `"Failed to calculate keystoneness indices: ..."`. |
| `calculate_trophic_levels(net, max_iter = 100, convergence = 1e-4)` | Returns a numeric vector named by `V(net)$name`. Basal nodes (in-degree 0; a self-loop counts) get 1. Unreachable nodes get `NA` plus the warning "`<k>` node(s) have no path from a basal species: ...". With no basal node, all values are `NA` with one warning "No basal species ...". Nodes still changing at `max_iter` get `NA` with the warning "... did not converge ...". Consumers average over reachable prey only. Callers must aggregate with `na.rm = TRUE`. |

## Deviations from the spec (with reasons)

1. **N1 writer extracted to `igraph_to_metaweb_interactions()`.** The inline writer lives inside a Shiny observer and cannot be unit-tested. A helper makes the spec's round-trip test (section 5, A1 test 5) executable. The module calls the helper, and a guard test pins that call. The round trip compares sorted edge lists rather than using `identical_graphs`: the two graphs legitimately differ in vertex/edge attributes (`quality_code`, `source`).
2. **Singular-matrix test (section 5, MTI test 5).** An exhaustive search of every 2- and 3-node digraph found no food web where `I - Q` is numerically singular; the minimum `rcond` was 0.077. A 2-cycle gives `FC = D` and therefore `Q = 0`. The test is split in two. (a) A no-basal 2-cycle returns a finite matrix without error. (b) The `ginv` + `warning()` branch is forced through a mockable seam, `.mti_rcond()`.
3. **Spec test 1 and 2 vertex names.** `metaweb_to_igraph` names vertices by `species_id` until A3. The F64 test therefore uses a fixture with `species_id == species_name`, and the bundled test resolves names through the `species_name` vertex attribute.
4. **Spec test 4 fg check.** "Cod fg is not Phytoplankton" cannot fail, because "cod" hits the name regex before topology is consulted. A name-neutral `Apex` group (P/B 1.5, eats Cod) was added, and the test asserts it is "Fish". On current code it is "Phytoplankton"; this was verified.
5. **Spec test 6 (N3).** In the "simple" dataset the trait model gives Zooplankton no prey, so the positive orientation assertion uses Phytoplankton -> Benthic_filter_feeder. The FS0 in-degree and "no edge ends at FS0" checks are exactly as specified.
6. **Guard test pattern (spec test 7).** A blanket ban on `c("predator_id", "prey_id")` would hit legitimate uses: the required-column check in `create_metaweb()` and duplicate detection in `merge_metawebs()`. The guard therefore bans the two concrete edge-list constructs that fed `graph_from_data_frame()`.
7. **Additional help fixes beyond the spec's "one sentence".** `import_ui.R` stated the opposite orientation ("A eats B, row -> column", with Fish as the prey of Zooplankton) and `dataeditor_ui.R` said "Rows = Predators". Both are corrected to rows = prey. Behaviour is unchanged: `parse_adjacency_df()` and the data editor were already contract-correct. `analysis_server.R` status names, the KS reference line and the heatmap axis labels are updated because the new statuses and log-scale KS made them wrong (spec step 4 mentioned only the heatmap diagonal).
8. **Blank EwE diet cells** are coerced to 0 in `parse_ecopath_data()`. This Review Focus item was not in the spec. Without it, the new no-transpose path would pass `NA` into `graph_from_adjacency_matrix()`.

## Risks (from spec section 7, as they apply to A1)

- Detritus in MTI is treated as an ordinary prey. The UI help says impacts on and of detritus are indicative only.
- The top-quartile KS rule is a design choice. Libralato ranks without cut-offs, and `ks_rank` exposes the plain ranking.
- A spatial run over many hexagons can emit one TL warning per hexagon that has an unreachable node. R collapses these into "There were N warnings". This is accepted: warnings are the project's diagnostic channel.
- The trait food-web RDS/GraphML exports change direction (N3), and metaweb arrows flip (N2). Both are in the CHANGELOG draft.
- The `bindCache` on `keystoneness_results` needs no action, because cached values die with the process restart on deploy.
