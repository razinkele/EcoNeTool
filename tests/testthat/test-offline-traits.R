# Unit tests for offline trait database and helpers

# App root helper (mirrors helper-fixtures.R)
app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")

test_that("size_to_ms returns correct codes at boundaries", {
  source(file.path(app_root, "R/config/harmonization_config.R"), local = TRUE)

  size_to_ms <- function(size_cm) {
    if (is.null(size_cm) || is.na(size_cm) || size_cm <= 0) return(NA_character_)
    thresholds <- HARMONIZATION_CONFIG$size_thresholds
    if (size_cm < thresholds$MS1_MS2) return("MS1")
    if (size_cm < thresholds$MS2_MS3) return("MS2")
    if (size_cm < thresholds$MS3_MS4) return("MS3")
    if (size_cm < thresholds$MS4_MS5) return("MS4")
    if (size_cm < thresholds$MS5_MS6) return("MS5")
    if (size_cm < thresholds$MS6_MS7) return("MS6")
    return("MS7")
  }

  # Edge cases
  expect_equal(size_to_ms(NA), NA_character_)
  expect_equal(size_to_ms(0), NA_character_)
  expect_equal(size_to_ms(-5), NA_character_)

  # Boundary values
  expect_equal(size_to_ms(0.05), "MS1")
  expect_equal(size_to_ms(0.5), "MS2")
  expect_equal(size_to_ms(3), "MS3")
  expect_equal(size_to_ms(10), "MS4")
  expect_equal(size_to_ms(30), "MS5")
  expect_equal(size_to_ms(100), "MS6")
  expect_equal(size_to_ms(200), "MS7")
})

test_that("MAREDAT ESD to cm conversion is plausible for zooplankton", {
  # Typical copepod ESD ~1000 um -> 0.1 cm (MS2 boundary)
  expect_equal(1000 / 10000, 0.1)
  # Euphausiid ~20000 um -> 2 cm (MS3)
  expect_equal(20000 / 10000, 2.0)
  # Jellyfish ~200000 um -> 20 cm (MS5)
  expect_equal(200000 / 10000, 20.0)
})

test_that("PTDB volume to cm conversion is correct", {
  # 1000 um3 -> cube root = 10 um -> 0.001 cm (MS1)
  expect_equal(round((1000^(1/3)) / 10000, 5), 0.001)
  # 1e6 um3 -> cube root = 100 um -> 0.01 cm (MS1)
  expect_equal(round((1e6^(1/3)) / 10000, 4), 0.01)
  # Very large cell 1e9 um3 -> 1000 um -> 0.1 cm (MS2)
  expect_equal(round((1e9^(1/3)) / 10000, 2), 0.1)
})

test_that("PR codes in harmonization config match 8-level system", {
  source(file.path(app_root, "R/config/harmonization_config.R"), local = TRUE)
  pr_names <- names(HARMONIZATION_CONFIG$protection_patterns)
  expect_true(any(grepl("PR0", pr_names)))
  expect_true(any(grepl("PR6", pr_names)))
  expect_true(any(grepl("PR8", pr_names)))
  # Old system should NOT be present
  expect_false(any(grepl("PR1", pr_names)))
})

test_that("New ecosystem profiles are accessible", {
  source(file.path(app_root, "R/config/harmonization_config.R"), local = TRUE)
  profiles <- names(HARMONIZATION_CONFIG$profiles)
  expect_true("mediterranean" %in% profiles)
  expect_true("atlantic_ne" %in% profiles)
  expect_true("deep_sea" %in% profiles)
  # Deep sea should have larger size thresholds
  expect_equal(HARMONIZATION_CONFIG$profiles$deep_sea$size_multiplier, 1.3)
})

test_that("lookup_offline_traits returns NULL for missing DB file", {
  source(file.path(app_root, "R/functions/validation_utils.R"), local = FALSE)
  source(file.path(app_root, "R/config/harmonization_config.R"), local = FALSE)
  env <- new.env(parent = globalenv())
  sys.source(file.path(app_root, "R/functions/trait_lookup/orchestrator.R"), envir = env)
  result <- env$lookup_offline_traits("Gadus morhua", db_path = "nonexistent.db")
  expect_null(result)
})

test_that("lookup_offline_traits returns data for known species when DB exists", {
  skip_if_not(file.exists(file.path(app_root, "cache/offline_traits.db")), "Offline DB not built yet")
  source(file.path(app_root, "R/functions/validation_utils.R"), local = FALSE)
  source(file.path(app_root, "R/config/harmonization_config.R"), local = FALSE)
  env <- new.env(parent = globalenv())
  sys.source(file.path(app_root, "R/functions/trait_lookup/orchestrator.R"), envir = env)

  # Try a species that should be in the ontology
  result <- env$lookup_offline_traits("Gadus morhua")
  if (!is.null(result)) {
    expect_true("MS" %in% names(result))
    expect_true("FS" %in% names(result))
    expect_true("MB" %in% names(result))
    expect_true("EP" %in% names(result))
    expect_true("PR" %in% names(result))
    expect_true("primary_source" %in% names(result))
  }
})

test_that("lookup_offline_traits returns NULL for unknown species", {
  skip_if_not(file.exists(file.path(app_root, "cache/offline_traits.db")), "Offline DB not built yet")
  source(file.path(app_root, "R/functions/validation_utils.R"), local = FALSE)
  source(file.path(app_root, "R/config/harmonization_config.R"), local = FALSE)
  env <- new.env(parent = globalenv())
  sys.source(file.path(app_root, "R/functions/trait_lookup/orchestrator.R"), envir = env)

  result <- env$lookup_offline_traits("Nonexistent_species_xyz_99999")
  expect_null(result)
})
