# Tests for Layer 2b: Bundled CSV trait database lookup functions
library(testthat)

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
source(file.path(app_root, "R/config.R"))
source(file.path(app_root, "R/functions/validation_utils.R"))
source(file.path(app_root, "R/functions/functional_group_utils.R"))
source(file.path(app_root, "R/config/harmonization_config.R"))
source(file.path(app_root, "R/functions/trait_lookup/harmonization.R"))
source(file.path(app_root, "R/functions/trait_lookup/csv_trait_databases.R"))

# =============================================================================
# "All 5 functions exist" smoke test
# =============================================================================

test_that("all 5 CSV lookup functions exist", {
  expect_true(exists("lookup_blacksea_traits"),  info = "lookup_blacksea_traits missing")
  expect_true(exists("lookup_arctic_traits"),    info = "lookup_arctic_traits missing")
  expect_true(exists("lookup_cefas_traits"),     info = "lookup_cefas_traits missing")
  expect_true(exists("lookup_coral_traits"),     info = "lookup_coral_traits missing")
  expect_true(exists("lookup_pelagic_traits"),   info = "lookup_pelagic_traits missing")
})

# =============================================================================
# Structure tests — nonexistent species → success = FALSE
# =============================================================================

test_that("lookup_blacksea_traits returns correct structure on missing species", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines("species,feeding_mode,mobility,body_size_mm,reproductive_mode,temperature_affinity,salinity_affinity", tmp)
  result <- lookup_blacksea_traits("__no_such_species__", csv_file = tmp)
  expect_false(result$success)
  expect_equal(result$source, "BlackSea")
  expect_equal(result$species, "__no_such_species__")
  expect_true(is.list(result$traits))
})

test_that("lookup_arctic_traits returns correct structure on missing species", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines("species,feeding_mode,mobility,body_size_mm,reproductive_mode,temperature_preference", tmp)
  result <- lookup_arctic_traits("__no_such_species__", csv_file = tmp)
  expect_false(result$success)
  expect_equal(result$source, "ArcticTraits")
  expect_equal(result$species, "__no_such_species__")
  expect_true(is.list(result$traits))
})

test_that("lookup_cefas_traits returns correct structure on missing species", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines("species,feeding_mode,mobility,body_size,lifespan,larval_development,reproductive_mode,living_habit,bioturbation_mode", tmp)
  result <- lookup_cefas_traits("__no_such_species__", csv_file = tmp)
  expect_false(result$success)
  expect_equal(result$source, "Cefas")
  expect_equal(result$species, "__no_such_species__")
  expect_true(is.list(result$traits))
})

test_that("lookup_coral_traits returns correct structure on missing species", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines("species,growth_form,reproductive_mode,thermal_tolerance_max,depth_lower,depth_upper", tmp)
  result <- lookup_coral_traits("__no_such_species__", csv_file = tmp)
  expect_false(result$success)
  expect_equal(result$source, "CoralTraits")
  expect_equal(result$species, "__no_such_species__")
  expect_true(is.list(result$traits))
})

test_that("lookup_pelagic_traits returns correct structure on missing species", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines("species,habitat_use,morphology,body_length_cm,nutritional_quality,feeding_mode", tmp)
  result <- lookup_pelagic_traits("__no_such_species__", csv_file = tmp)
  expect_false(result$success)
  expect_equal(result$source, "PelagicTraits")
  expect_equal(result$species, "__no_such_species__")
  expect_true(is.list(result$traits))
})

# =============================================================================
# File-not-found returns success = FALSE (not an error)
# =============================================================================

test_that("lookup_blacksea_traits handles missing CSV file gracefully", {
  result <- lookup_blacksea_traits("Mytilus galloprovincialis",
                                    csv_file = "/nonexistent/path/blacksea.csv")
  expect_false(result$success)
  expect_equal(result$source, "BlackSea")
})

# =============================================================================
# Positive-case tests using tempfile with known data
# =============================================================================

test_that("lookup_blacksea_traits extracts traits from real data", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c(
    "species,feeding_mode,mobility,body_size_mm,reproductive_mode,temperature_affinity,salinity_affinity",
    "Mytilus galloprovincialis,filter feeder,sessile,80,broadcast,warm eurythermal,euhaline"
  ), tmp)
  result <- lookup_blacksea_traits("Mytilus galloprovincialis", csv_file = tmp)
  expect_true(result$success)
  expect_equal(result$traits$feeding_mode, "filter feeder")
  expect_equal(result$traits$mobility_info, "sessile")
  expect_equal(result$traits$reproductive_mode, "broadcast")
  expect_equal(result$traits$temperature_affinity, "warm eurythermal")
  expect_equal(result$traits$salinity_affinity, "euhaline")
})

test_that("lookup_arctic_traits extracts traits from real data", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c(
    "species,feeding_mode,mobility,body_size_mm,reproductive_mode,temperature_preference",
    "Boreogadus saida,predator,active,300,broadcast,stenothermal cold"
  ), tmp)
  result <- lookup_arctic_traits("Boreogadus saida", csv_file = tmp)
  expect_true(result$success)
  expect_equal(result$traits$feeding_mode, "predator")
  expect_equal(result$traits$mobility_info, "active")
  expect_equal(result$traits$reproductive_mode, "broadcast")
  expect_equal(result$traits$temperature_preference, "stenothermal cold")
})

test_that("lookup_arctic_traits defaults temperature_preference to 'arctic' when absent", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c(
    "species,feeding_mode,mobility,body_size_mm,reproductive_mode,temperature_preference",
    "Calanus hyperboreus,filter feeder,mobile,5,broadcast,"
  ), tmp)
  result <- lookup_arctic_traits("Calanus hyperboreus", csv_file = tmp)
  expect_true(result$success)
  expect_equal(result$traits$temperature_preference, "arctic")
})

test_that("lookup_cefas_traits extracts traits from real data", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c(
    "species,feeding_mode,mobility,body_size,lifespan,larval_development,reproductive_mode,living_habit,bioturbation_mode",
    "Abra alba,deposit feeder,slow,25,3,planktotrophic,broadcast,infaunal,diffusive mixing"
  ), tmp)
  result <- lookup_cefas_traits("Abra alba", csv_file = tmp)
  expect_true(result$success)
  expect_equal(result$traits$feeding_mode, "deposit feeder")
  expect_equal(result$traits$mobility_info, "slow")
  expect_equal(as.character(result$traits$longevity_years), "3")
  expect_equal(result$traits$larval_development, "planktotrophic")
  expect_equal(result$traits$living_habit, "infaunal")
  expect_equal(result$traits$bioturbation_mode, "diffusive mixing")
})

test_that("lookup_coral_traits extracts traits from real data", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c(
    "species,growth_form,reproductive_mode,thermal_tolerance_max,depth_lower,depth_upper",
    "Acropora millepora,branching,broadcast spawner,32.5,1,30"
  ), tmp)
  result <- lookup_coral_traits("Acropora millepora", csv_file = tmp)
  expect_true(result$success)
  expect_equal(result$traits$growth_form, "branching")
  expect_equal(result$traits$reproductive_mode, "broadcast spawner")
  expect_equal(result$traits$thermal_tolerance, 32.5)
  expect_equal(result$traits$depth_min, 1)
  expect_equal(result$traits$depth_max, 30)
})

test_that("lookup_pelagic_traits extracts traits from real data", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c(
    "species,habitat_use,morphology,body_length_cm,nutritional_quality,feeding_mode",
    "Engraulis encrasicolus,pelagic,fusiform,15.0,high,planktivore"
  ), tmp)
  result <- lookup_pelagic_traits("Engraulis encrasicolus", csv_file = tmp)
  expect_true(result$success)
  expect_equal(result$traits$habitat_use, "pelagic")
  expect_equal(result$traits$morphology, "fusiform")
  expect_equal(result$traits$body_length_cm, 15.0)
  expect_equal(result$traits$nutritional_quality, "high")
  expect_equal(result$traits$feeding_mode, "planktivore")
})

# =============================================================================
# Genus-level matching test
# =============================================================================

test_that("lookup finds species by genus when exact match fails", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c(
    "species,feeding_mode,mobility,body_size_mm,reproductive_mode,temperature_affinity,salinity_affinity",
    "Mytilus edulis,filter feeder,sessile,60,broadcast,cold eurythermal,euryhaline"
  ), tmp)
  # Use a different species in same genus
  result <- lookup_blacksea_traits("Mytilus trossulus", csv_file = tmp)
  expect_true(result$success, info = "Should match via genus fallback")
  expect_equal(result$traits$feeding_mode, "filter feeder")
})

# =============================================================================
# Case-insensitive matching test
# =============================================================================

test_that("lookup is case-insensitive for species names", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c(
    "species,feeding_mode,mobility,body_size_mm,reproductive_mode,temperature_affinity,salinity_affinity",
    "Mytilus galloprovincialis,filter feeder,sessile,80,broadcast,warm eurythermal,euhaline"
  ), tmp)
  result <- lookup_blacksea_traits("MYTILUS GALLOPROVINCIALIS", csv_file = tmp)
  expect_true(result$success, info = "Should match case-insensitively")
})

test_that("orchestrator has CSV database routing flags", {
  orch_text <- readLines(file.path(app_root, "R/functions/trait_lookup/orchestrator.R"))
  orch_joined <- paste(orch_text, collapse = "\n")
  expect_true(grepl("query_blacksea", orch_joined), info = "Missing query_blacksea flag")
  expect_true(grepl("query_arctic", orch_joined), info = "Missing query_arctic flag")
  expect_true(grepl("query_cefas", orch_joined), info = "Missing query_cefas flag")
  expect_true(grepl("query_coral", orch_joined), info = "Missing query_coral flag")
  expect_true(grepl("query_pelagic", orch_joined), info = "Missing query_pelagic flag")
})
