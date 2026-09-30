# =============================================================================
# Live Integration Tests: SHARK Data tab (C-6b, F12)
# =============================================================================
# These tests hit the REAL SHARK and WoRMS APIs through SHARK4R.
# Skipped by default. Enable with: Sys.setenv(RUN_LIVE_TESTS = "true")
# =============================================================================

source_shark_live <- function() {
  source(file.path(get_app_root(), "R/functions/validation_utils.R"), local = FALSE)
  source(file.path(get_app_root(), "R/functions/shark_api_utils.R"), local = FALSE)
}

test_that("SHARK still offers the parameters and data types the tab sends (live)", {
  skip_if_no_live_tests()
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark_live()
  options <- with_timeout(SHARK4R::get_shark_options(), timeout = 60, on_timeout = NULL)
  skip_if(is.null(options), "get_shark_options() timed out")
  expect_equal(setdiff(SHARK_ENV_PARAMETERS, options$parameters), character(0))
  expect_true("Physical and Chemical" %in% options$dataTypes)
})

test_that("WoRMS via SHARK4R finds Gadus morhua (live)", {
  skip_if_no_live_tests()
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark_live()
  r <- query_shark_worms("Gadus morhua", use_cache = FALSE)
  expect_equal(r$status, "found")
  expect_identical(r$aphia_id, 126436L)
  expect_equal(query_shark_worms("Xx yy", use_cache = FALSE)$status, "not_found")
})

test_that("a small Kattegat temperature query returns SHARK rows (live)", {
  skip_if_no_live_tests()
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark_live()
  r <- get_shark_environmental_data("Temperature CTD", "2024-06-01", "2024-06-30",
                                    bbox = c(north = 58, south = 57, east = 12, west = 11))
  expect_equal(r$status, "ok")
  expect_true(all(r$data$parameter == "Temperature CTD"))
  expect_true(all(c("sample_date", "sample_latitude_dd", "value", "unit") %in% names(r$data)))
})
