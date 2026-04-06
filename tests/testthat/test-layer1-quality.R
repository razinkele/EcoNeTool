library(testthat)
library(igraph)

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
source(file.path(app_root, "R/config.R"))
source(file.path(app_root, "R/functions/validation_utils.R"))
source(file.path(app_root, "R/functions/functional_group_utils.R"))
source(file.path(app_root, "R/config/harmonization_config.R"))
source(file.path(app_root, "R/functions/trait_foodweb.R"))

test_that("EP codes follow water-column-to-sediment gradient", {
  expect_equal(rownames(EP_MS), c("EP1", "EP2", "EP3", "EP4"))
  expect_true(EP_MS["EP1", "MS2"] > EP_MS["EP4", "MS2"],
              info = "Pelagic consumers (EP1) should access pelagic prey more than infaunal (EP4)")
  expect_true(grepl("Pelagic", TRAIT_DEFINITIONS$EP["EP1"], ignore.case = TRUE))
  expect_true(grepl("Endobenthic|Infaunal", TRAIT_DEFINITIONS$EP["EP4"], ignore.case = TRUE))
})

test_that("EP config patterns match canonical scheme", {
  patterns <- HARMONIZATION_CONFIG$environmental_patterns
  expect_true(grepl("benthopelagic|demersal", names(patterns)[2], ignore.case = TRUE))
  expect_true(grepl("epibenthic", names(patterns)[3], ignore.case = TRUE))
  expect_true(grepl("endobenthic|infaun", names(patterns)[4], ignore.case = TRUE))
})

test_that("FS7 is valid and accepted in food web construction", {
  valid_FS <- paste0("FS", c(0:7))
  expect_true("FS7" %in% valid_FS)
  expect_true("FS7" %in% names(TRAIT_DEFINITIONS$FS))
  expect_equal(TRAIT_DEFINITIONS$FS["FS3"], c(FS3 = "Omnivore"))
})
