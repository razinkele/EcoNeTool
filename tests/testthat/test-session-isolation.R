# Regression tests for the per-session HARMONIZATION_CONFIG refactor (PR9α).
# Pre-PR9α the slider's writeback at harmonization_settings_server.R:37
# (assign envir = globalenv()) leaked tab A's threshold changes into tab B
# under Shiny Server's app_dir mode. The new accessor get_harm_config()
# routes reads through session$userData; these tests pin that contract.

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")

test_that("get_harm_config falls back to globalenv outside Shiny", {
  source(file.path(app_root, "R/config/harmonization_config.R"), local = FALSE)
  source(file.path(app_root, "R/functions/trait_lookup/harmonization.R"),
         local = FALSE)

  # No Shiny session active -> get_harm_config() returns the global
  cfg <- get_harm_config()
  expect_false(is.null(cfg))
  expect_identical(cfg$size_thresholds$MS1_MS2,
                   HARMONIZATION_CONFIG$size_thresholds$MS1_MS2)
})

test_that("get_harm_config prefers session$userData when available", {
  skip_if_not_installed("shiny")
  source(file.path(app_root, "R/config/harmonization_config.R"), local = FALSE)
  source(file.path(app_root, "R/functions/trait_lookup/harmonization.R"),
         local = FALSE)

  shiny::isolate({
    fake_session <- shiny::MockShinySession$new()
    fake_session$userData$harm_config <- list(
      size_thresholds = list(MS1_MS2 = 999.0,
                             MS2_MS3 = 1.0, MS3_MS4 = 5.0,
                             MS4_MS5 = 20.0, MS5_MS6 = 50.0, MS6_MS7 = 150.0)
    )

    # withReactiveDomain swaps the default reactive domain so
    # getDefaultReactiveDomain() inside get_harm_config() returns the
    # session we set up.
    shiny::withReactiveDomain(fake_session, {
      cfg <- get_harm_config()
      expect_equal(cfg$size_thresholds$MS1_MS2, 999.0,
                   info = "session$userData override should win over globalenv")
    })
  })

  # Outside the withReactiveDomain block, the global default is restored
  cfg_after <- get_harm_config()
  expect_equal(cfg_after$size_thresholds$MS1_MS2,
               HARMONIZATION_CONFIG$size_thresholds$MS1_MS2,
               info = "global must NOT have been mutated by the session override")
})

test_that("harmonize_size_class uses session-local thresholds", {
  skip_if_not_installed("shiny")
  source(file.path(app_root, "R/config/harmonization_config.R"), local = FALSE)
  source(file.path(app_root, "R/functions/trait_lookup/harmonization.R"),
         local = FALSE)

  shiny::isolate({
    # Session A: very tight bins (everything is MS7)
    sess_a <- shiny::MockShinySession$new()
    sess_a$userData$harm_config <- list(
      size_thresholds = list(MS1_MS2 = 0.001, MS2_MS3 = 0.002,
                             MS3_MS4 = 0.003, MS4_MS5 = 0.004,
                             MS5_MS6 = 0.005, MS6_MS7 = 0.006),
      active_profile = "temperate"
    )
    shiny::withReactiveDomain(sess_a, {
      expect_equal(harmonize_size_class(1.0), "MS7")
    })

    # Session B: wide bins (a 1 cm fish is MS3)
    sess_b <- shiny::MockShinySession$new()
    sess_b$userData$harm_config <- list(
      size_thresholds = list(MS1_MS2 = 0.1, MS2_MS3 = 1.0,
                             MS3_MS4 = 5.0, MS4_MS5 = 20.0,
                             MS5_MS6 = 50.0, MS6_MS7 = 150.0),
      active_profile = "temperate"
    )
    shiny::withReactiveDomain(sess_b, {
      expect_equal(harmonize_size_class(1.5), "MS3")
    })
  })

  # Cross-contamination check: globalenv must be untouched after both
  # mock sessions ran. The pre-PR9α bug would have been a globalenv
  # mutation persisting outside the session scope.
  expect_identical(
    HARMONIZATION_CONFIG$size_thresholds$MS1_MS2,
    0.1
  )
})
