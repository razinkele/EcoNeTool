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
