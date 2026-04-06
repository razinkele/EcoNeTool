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
