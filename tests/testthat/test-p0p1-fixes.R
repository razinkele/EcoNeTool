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
