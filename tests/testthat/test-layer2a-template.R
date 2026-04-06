# Tests for Layer 2a: Expanded trait template
library(testthat)

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
source(file.path(app_root, "R/config.R"))
source(file.path(app_root, "R/functions/validation_utils.R"))
source(file.path(app_root, "R/functions/functional_group_utils.R"))
source(file.path(app_root, "R/config/harmonization_config.R"))
source(file.path(app_root, "R/functions/trait_lookup/harmonization.R"))
source(file.path(app_root, "R/functions/trait_lookup/database_lookups.R"))
source(file.path(app_root, "R/functions/trait_lookup/orchestrator.R"))

test_that("orchestrator result template has expanded columns", {
  # Call with a nonexistent species — returns the default NA template
  result <- lookup_species_traits("__test_nonexistent_species_xyz__")
  expected_cols <- c("species", "MS", "FS", "MB", "EP", "PR",
                     "RS", "TT", "ST",
                     "trophic_level", "depth_min", "depth_max", "is_hab",
                     "longevity_years", "growth_rate", "body_shape",
                     "phyto_motility", "phyto_growth_form",
                     "imputation_method")
  for (col in expected_cols) {
    expect_true(col %in% names(result), info = paste("Missing column:", col))
  }
})

test_that("lookup_ptdb_traits extracts is_hab (Harmful flag)", {
  ptdb_file <- file.path(app_root, "data/ptdb_phytoplankton.csv")
  skip_if_not(file.exists(ptdb_file), "PTDB file not available")
  ptdb <- read.csv(ptdb_file, stringsAsFactors = FALSE)
  skip_if(nrow(ptdb) == 0, "PTDB is empty")
  test_species <- ptdb$Species[1]
  result <- lookup_ptdb_traits(test_species, ptdb_file = ptdb_file)
  if (result$success) {
    expect_true("is_hab" %in% names(result$traits),
                info = "PTDB should now extract harmful algae flag")
  }
})

test_that("orchestrator wires extracted traits to result columns", {
  orch_text <- readLines(file.path(app_root, "R/functions/trait_lookup/orchestrator.R"))
  orch_joined <- paste(orch_text, collapse = "\n")
  expect_true(grepl("result\\$trophic_level", orch_joined),
              info = "Orchestrator must wire trophic_level to result")
  expect_true(grepl("result\\$depth_min", orch_joined),
              info = "Orchestrator must wire depth_min to result")
  expect_true(grepl("result\\$is_hab", orch_joined),
              info = "Orchestrator must wire is_hab to result")
  expect_true(grepl("result\\$longevity_years", orch_joined),
              info = "Orchestrator must wire longevity_years to result")
  expect_true(grepl("result\\$body_shape", orch_joined),
              info = "Orchestrator must wire body_shape to result")
})
