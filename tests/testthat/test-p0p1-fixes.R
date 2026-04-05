# Tests for P0/P1 bugfixes from deep review (2026-04-04)
# Run: Rscript -e "testthat::test_file('tests/testthat/test-p0p1-fixes.R')"

library(testthat)
library(igraph)
suppressPackageStartupMessages(library(shiny))  # needed for sourcing data_import_server.R

# Source required files in dependency order
app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
source(file.path(app_root, "R/config.R"))
source(file.path(app_root, "R/functions/validation_utils.R"))
source(file.path(app_root, "R/functions/functional_group_utils.R"))
source(file.path(app_root, "R/functions/flux_calculations.R"))
source(file.path(app_root, "R/functions/trophic_levels.R"))
source(file.path(app_root, "R/functions/topological_metrics.R"))
source(file.path(app_root, "R/modules/data_import_server.R"))
source(file.path(app_root, "R/functions/shark_api_utils.R"))

test_that("endotherm vertebrates get nonzero metabolic losses", {
  info <- data.frame(
    bodymasses = c(100, 5000, 2000),
    met.types = c("invertebrates", "endotherm vertebrates", "endotherm vertebrates"),
    stringsAsFactors = FALSE
  )
  losses <- calculate_losses(info, temp = 10)
  expect_true(all(losses > 0), info = "All metabolic losses must be positive")
  expect_true(losses[2] > losses[1], info = "Endotherm (5kg mammal) should have higher losses than invertebrate (100g)")
})

test_that("fluxind handles zero-flux matrix without NaN/Inf", {
  zero_mat <- matrix(0, nrow = 3, ncol = 3)
  result <- fluxind(zero_mat)
  expect_false(any(is.nan(unlist(result))), info = "No NaN values in result")
  expect_false(any(is.infinite(unlist(result))), info = "No Inf values in result")
})

test_that("fluxind handles single-species matrix", {
  one_mat <- matrix(0, nrow = 1, ncol = 1)
  result <- fluxind(one_mat, loop = FALSE)
  expect_false(any(is.nan(unlist(result))))
  expect_false(any(is.infinite(unlist(result))))
})

test_that("parse_adjacency_df creates prey-to-predator edges for diet matrix", {
  # Diet matrix: rows = prey, columns = predators
  # Prey A is eaten by Predator C (mat[1,2] = 0.5)
  mat_df <- data.frame(
    Species = c("PreyA", "PreyB"),
    PredatorC = c(0.5, 0.3),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  result <- parse_adjacency_df(mat_df)
  net <- result$net

  # Edge should go from PreyA -> PredatorC (energy flow: prey to predator)
  edges <- as_edgelist(net)
  expect_true(any(edges[,1] == "PreyA" & edges[,2] == "PredatorC"),
              info = "Edge direction must be prey -> predator (energy flow)")
})

test_that("get_available_shark_parameters returns empty on error, not fake data", {
  result <- get_available_shark_parameters()
  if (!requireNamespace("SHARK4R", quietly = TRUE)) {
    expect_equal(length(result), 0,
                 info = "Should return empty vector when SHARK4R not installed")
  }
})

test_that("get_shark_datasets returns info df on error, not fake data", {
  result <- get_shark_datasets()
  if (!requireNamespace("SHARK4R", quietly = TRUE)) {
    expect_true(nrow(result) <= 1,
                info = "Should not return fake dataset list (6 rows)")
  }
})

test_that("topological indicators handle single-species network", {
  net <- make_empty_graph(n = 1, directed = TRUE)
  V(net)$name <- "Solo"
  result <- get_topological_indicators(net)
  expect_false(any(is.nan(unlist(result))))
  expect_false(any(is.infinite(unlist(result))))
})
