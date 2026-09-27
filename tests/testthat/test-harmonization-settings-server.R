# Regression tests for F1/F76 (deep analysis 2026-09-26, spec B section 4.0).
# Pre-B0 the INITIALIZE observe() in harmonization_settings_server.R re-read
# config/harmonization_custom.json and read rv$config reactively, while the
# UPDATE observe() wrote rv$config from the sliders. Each observer
# re-triggered the other, so one slider move pinned the single R process.
# These are the repo's first shiny::testServer() tests; testServer accepts
# the module's plain function(input, output, session) signature directly.

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")

source_harm_module <- function() {
  source(file.path(app_root, "R/functions/validation_utils.R"), local = FALSE)
  source(file.path(app_root, "R/config/harmonization_config.R"), local = FALSE)
  source(file.path(app_root, "R/modules/harmonization_settings_server.R"), local = FALSE)
}

# Point HARMONIZATION_CONFIG_FILE at `path` and wrap load_harmonization_config
# in a counter that stop()s once it has been called more than `max_loads`
# times. The stop turns the pre-fix infinite observer loop into a fast test
# failure instead of a hung R process. Both globals are restored when the
# calling test_that() block exits.
local_harm_loader <- function(path, max_loads = 5L, env = parent.frame()) {
  old_file <- get("HARMONIZATION_CONFIG_FILE", envir = globalenv())
  old_loader <- get("load_harmonization_config", envir = globalenv())
  calls <- new.env()
  calls$n <- 0L
  assign("HARMONIZATION_CONFIG_FILE", path, envir = globalenv())
  assign("load_harmonization_config", function(file = HARMONIZATION_CONFIG_FILE) {
    calls$n <- calls$n + 1L
    if (calls$n > max_loads) {
      stop(sprintf("load_harmonization_config called %d times: observer loop", calls$n))
    }
    old_loader(file)
  }, envir = globalenv())
  withr::defer({
    assign("HARMONIZATION_CONFIG_FILE", old_file, envir = globalenv())
    assign("load_harmonization_config", old_loader, envir = globalenv())
  }, envir = env)
  calls
}

# Write a server-default JSON whose MS3_MS4 differs from the built-in 5.
write_harm_json <- function(ms3_ms4, env = parent.frame()) {
  path <- tempfile("harm_cfg_", fileext = ".json")
  cfg <- HARMONIZATION_CONFIG
  cfg$size_thresholds$MS3_MS4 <- ms3_ms4
  suppressMessages(save_harmonization_config(cfg, path))
  withr::defer(unlink(path), envir = env)
  path
}

set_all_thresholds <- function(session, ms3_ms4) {
  session$setInputs(
    harm_thresh_MS1_MS2 = 0.1, harm_thresh_MS2_MS3 = 1.0,
    harm_thresh_MS3_MS4 = ms3_ms4, harm_thresh_MS4_MS5 = 20.0,
    harm_thresh_MS5_MS6 = 50.0, harm_thresh_MS6_MS7 = 150.0
  )
}

test_that("slider change settles and is not reverted by the server-default file", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  calls <- local_harm_loader(write_harm_json(ms3_ms4 = 7))

  shiny::testServer(harmonization_settings_server, {
    expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4, 7,
                 info = "session config is seeded from the server-default JSON")
    set_all_thresholds(session, ms3_ms4 = 9)
    expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4, 9,
                 info = "slider value must survive; pre-B0 the JSON reload reverted it to 7")
    expect_equal(shiny::isolate(rv$config$size_thresholds$MS3_MS4), 9)
  })

  expect_lte(calls$n, 1L)
})

test_that("repeated slider moves never reload the server-default file", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  calls <- local_harm_loader(write_harm_json(ms3_ms4 = 7))

  shiny::testServer(harmonization_settings_server, {
    for (v in c(8, 9, 10, 7, 12)) set_all_thresholds(session, ms3_ms4 = v)
    expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4, 12)
  })

  expect_equal(calls$n, 1L)
})

test_that("without a server-default file the session starts from the built-in defaults", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  calls <- local_harm_loader(file.path(tempdir(), "no_such_harm_cfg.json"))

  shiny::testServer(harmonization_settings_server, {
    expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4,
                 HARMONIZATION_CONFIG$size_thresholds$MS3_MS4)
    set_all_thresholds(session, ms3_ms4 = 9)
    expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4, 9)
  })

  expect_equal(calls$n, 0L)
})

test_that("an unparseable server-default file warns once and falls back to defaults", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  bad <- tempfile("harm_bad_", fileext = ".json")
  writeLines("{ not valid json", bad)
  withr::defer(unlink(bad))
  calls <- local_harm_loader(bad)

  expect_warning(
    shiny::testServer(harmonization_settings_server, {
      expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4,
                   HARMONIZATION_CONFIG$size_thresholds$MS3_MS4)
      set_all_thresholds(session, ms3_ms4 = 9)
      expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4, 9)
    }),
    "could not parse"
  )
  expect_lte(calls$n, 1L)
})

test_that("browser start-up echo (UI defaults, then the file value) settles on the file value", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  calls <- local_harm_loader(write_harm_json(ms3_ms4 = 7))

  shiny::testServer(harmonization_settings_server, {
    # A real browser first reports the sliderInput() defaults from the UI
    # (MS3_MS4 = 5), then the value pushed by updateSliderInput() (7).
    set_all_thresholds(session, ms3_ms4 = 5)
    set_all_thresholds(session, ms3_ms4 = 7)
    expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4, 7)
  })

  expect_equal(calls$n, 1L)
})

test_that("a server-default file missing a threshold is filled from the defaults at start", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  path <- tempfile("harm_partial_", fileext = ".json")
  cfg <- HARMONIZATION_CONFIG
  cfg$size_thresholds$MS6_MS7 <- NULL
  suppressMessages(save_harmonization_config(cfg, path))
  withr::defer(unlink(path))
  calls <- local_harm_loader(path)

  shiny::testServer(harmonization_settings_server, {
    # B1: load_harmonization_config() now runs the shared validator, which
    # fills missing keys from HARMONIZATION_CONFIG (pre-B1 this was NULL).
    expect_equal(session$userData$harm_config$size_thresholds$MS6_MS7, 150)
    set_all_thresholds(session, ms3_ms4 = 9)
    expect_equal(session$userData$harm_config$size_thresholds$MS6_MS7, 150)
    expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4, 9)
  })

  expect_equal(calls$n, 1L)
})

test_that("a slider move in one session leaves the process-wide default and other sessions alone", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  calls <- local_harm_loader(write_harm_json(ms3_ms4 = 7))
  default_before <- HARMONIZATION_CONFIG

  other_cfg <- NULL
  shiny::testServer(harmonization_settings_server, {
    other_cfg <<- session$userData$harm_config
  })
  shiny::testServer(harmonization_settings_server, {
    set_all_thresholds(session, ms3_ms4 = 9)
    expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4, 9)
  })

  expect_identical(HARMONIZATION_CONFIG, default_before)
  expect_equal(other_cfg$size_thresholds$MS3_MS4, 7)
  expect_equal(calls$n, 2L)
})
