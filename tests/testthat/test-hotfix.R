library(testthat)
app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")

test_that("API key saving uses JSON, not source()'d R code", {
  server_text <- readLines(file.path(app_root, "R/modules/plugin_server.R"))
  server_joined <- paste(server_text, collapse = "\n")
  expect_true(grepl("write_json", server_joined),
              info = "Must use jsonlite::write_json, not paste0 interpolation")
})

source(file.path(app_root, "R/config.R"))
source(file.path(app_root, "R/functions/validation_utils.R"))
source(file.path(app_root, "R/config/harmonization_config.R"))
source(file.path(app_root, "R/functions/trait_lookup/harmonization.R"))

test_that("EP returns EP1 for pelagic species (phytoplankton)", {
  result <- harmonize_environmental_position(
    habitat_info = NULL, depth_min = NULL, depth_max = NULL,
    taxonomic_info = list(phylum = "Ochrophyta", class = "Bacillariophyceae",
                          feeding_mode = "photosynthesis"))
  expect_equal(result, "EP1")
})

test_that("EP returns EP1 for fish", {
  result <- harmonize_environmental_position(
    habitat_info = NULL, depth_min = NULL, depth_max = NULL,
    taxonomic_info = list(phylum = "Chordata", class = "Actinopteri"))
  expect_equal(result, "EP1")
})

test_that("EP returns EP4 for infaunal bivalves", {
  result <- harmonize_environmental_position(
    habitat_info = NULL, depth_min = NULL, depth_max = NULL,
    taxonomic_info = list(phylum = "Mollusca", class = "Bivalvia"))
  expect_equal(result, "EP4")
})

test_that("EP returns EP4 for shallow burrowers", {
  result <- harmonize_environmental_position(
    habitat_info = c("burrowing"), depth_min = 5, depth_max = 20,
    taxonomic_info = NULL)
  expect_equal(result, "EP4")
})

test_that("CSV lookup functions use simple paths, not sys.frame", {
  csv_text <- readLines(file.path(app_root, "R/functions/trait_lookup/csv_trait_databases.R"))
  csv_joined <- paste(csv_text, collapse = "\n")
  expect_false(grepl("sys.frame", csv_joined),
               info = "Must not use sys.frame(1)$ofile")
})
