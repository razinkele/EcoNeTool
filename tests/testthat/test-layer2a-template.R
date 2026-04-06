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

test_that("harmonization config has RS/TT/ST patterns", {
  expect_true("reproductive_patterns" %in% names(HARMONIZATION_CONFIG))
  expect_true("temperature_patterns" %in% names(HARMONIZATION_CONFIG))
  expect_true("salinity_patterns" %in% names(HARMONIZATION_CONFIG))
  expect_equal(length(HARMONIZATION_CONFIG$reproductive_patterns), 4)
  expect_equal(length(HARMONIZATION_CONFIG$temperature_patterns), 4)
  expect_equal(length(HARMONIZATION_CONFIG$salinity_patterns), 5)
})

test_that("harmonize_reproductive_strategy maps correctly", {
  expect_equal(harmonize_reproductive_strategy("broadcast spawner"), "RS1")
  expect_equal(harmonize_reproductive_strategy("brooding"), "RS2")
  expect_equal(harmonize_reproductive_strategy("budding"), "RS3")
  expect_equal(harmonize_reproductive_strategy("unknown_text"), NA_character_)
})

test_that("harmonize_temperature_tolerance maps correctly", {
  expect_equal(harmonize_temperature_tolerance("arctic species"), "TT1")
  expect_equal(harmonize_temperature_tolerance("boreal"), "TT2")
  expect_equal(harmonize_temperature_tolerance("temperate"), "TT3")
  expect_equal(harmonize_temperature_tolerance("tropical"), "TT4")
  expect_equal(harmonize_temperature_tolerance("unknown"), NA_character_)
})

test_that("harmonize_salinity_tolerance maps correctly", {
  expect_equal(harmonize_salinity_tolerance("freshwater"), "ST1")
  expect_equal(harmonize_salinity_tolerance("oligohaline"), "ST2")
  expect_equal(harmonize_salinity_tolerance("mesohaline"), "ST3")
  expect_equal(harmonize_salinity_tolerance("marine"), "ST5")
  expect_equal(harmonize_salinity_tolerance("unknown"), NA_character_)
})

test_that("schema migration adds new columns without error", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  source(file.path(app_root, "R/functions/cache_sqlite.R"))

  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(dbDisconnect(con))
  dbExecute(con, "CREATE TABLE species_traits (
    species TEXT PRIMARY KEY, MS TEXT, FS TEXT, MB TEXT, EP TEXT, PR TEXT,
    MS_confidence REAL, FS_confidence REAL, MB_confidence REAL,
    EP_confidence REAL, PR_confidence REAL, primary_source TEXT
  )")
  dbExecute(con, "CREATE TABLE metadata (key TEXT PRIMARY KEY, value TEXT)")
  dbExecute(con, "INSERT INTO metadata (key, value) VALUES ('version', '1.3.0')")

  migrate_offline_schema(con)

  cols <- dbListFields(con, "species_traits")
  expect_true("RS" %in% cols)
  expect_true("TT" %in% cols)
  expect_true("ST" %in% cols)
  expect_true("trophic_level" %in% cols)
  expect_true("depth_min" %in% cols)
  expect_true("is_hab" %in% cols)
  expect_true("longevity_years" %in% cols)
  expect_true("imputation_method" %in% cols)
})
