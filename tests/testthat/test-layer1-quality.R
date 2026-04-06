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

test_that("all taxonomic rule keys used in harmonization exist in config", {
  rules <- HARMONIZATION_CONFIG$taxonomic_rules
  expected_keys <- c(
    "bivalves_sessile", "cnidarians_sessile", "phytoplankton_pelagic",
    "infaunal_bivalves", "bivalves_hard_shell", "gastropods_hard_shell",
    "crustaceans_exoskeleton", "echinoderms_calcium_plates"
  )
  for (key in expected_keys) {
    expect_true(key %in% names(rules), info = paste("Missing rule:", key))
  }
})

test_that("SHARK is removed from trait routing in orchestrator", {
  orch_text <- readLines(file.path(app_root, "R/functions/trait_lookup/orchestrator.R"))
  shark_assignments <- grep("query_shark\\s*<-\\s*TRUE", orch_text)
  expect_equal(length(shark_assignments), 0,
               info = "All query_shark <- TRUE assignments should be removed from orchestrator")
})

test_that("include_raw=TRUE requires raw_data in query", {
  skip_if_not_installed("RSQLite")
  source(file.path(app_root, "R/functions/cache_sqlite.R"))
  fn_body <- deparse(body(load_species_from_cache))
  fn_text <- paste(fn_body, collapse = "\n")
  # After fix: the include_raw=TRUE branch should have IS NOT NULL
  expect_true(grepl("include_raw.*IS NOT NULL|IS NOT NULL.*include_raw", fn_text, ignore.case = TRUE),
              info = "include_raw=TRUE branch should require raw_data IS NOT NULL")
})
