# =============================================================================
# API key file paths: reader and writer must agree, independent of wd
# =============================================================================
# R/config.R READS the API key files at startup; R/modules/plugin_server.R
# WRITES them from the config modal. If the two resolve "config/api_keys.json"
# against different working directories, keys are saved to one place and
# loaded from another - silently, because both sides are guarded by
# file.exists(). Sharing one constant makes that drift impossible.

source_app_dependencies()

local({
  root <- get_app_root()
  # validation_utils.R first: config.R prefers app_path() when it exists.
  source(file.path(root, "R/functions/validation_utils.R"), local = FALSE)
  tryCatch(
    source(file.path(root, "R/config.R"), local = FALSE),
    error = function(e) message("Note: R/config.R side effects skipped: ",
                                conditionMessage(e))
  )
})

# ---------------------------------------------------------------------------
# The constants exist and agree
# ---------------------------------------------------------------------------

test_that("config.R exposes a constant for every API key file it touches", {
  expect_true(exists("API_KEYS_FILE"))
  expect_true(exists("API_KEYS_JSON"))
  expect_true(exists("API_KEYS_TEMPLATE"))
})

test_that("reader and writer resolve to the same config directory", {
  skip_if(!exists("API_KEYS_JSON"), "API_KEYS_JSON not defined yet")

  expect_identical(dirname(API_KEYS_JSON), dirname(API_KEYS_FILE))
  expect_identical(dirname(API_KEYS_TEMPLATE), dirname(API_KEYS_FILE))
  expect_equal(basename(API_KEYS_JSON), "api_keys.json")
  expect_equal(basename(API_KEYS_FILE), "api_keys.R")
})

test_that("the paths are repo-absolute when app_path is available", {
  skip_if(!exists("API_KEYS_JSON"), "API_KEYS_JSON not defined yet")
  skip_if(!exists("app_path", mode = "function"),
          "app_path not loaded; config.R would use its relative fallback")

  # Tests run with wd = tests/testthat, so a wd-relative path would not
  # point into the repo's config/ directory at all.
  expect_identical(dirname(API_KEYS_JSON), app_path("config"))
  expect_true(startsWith(API_KEYS_JSON, app_path()))
})

# ---------------------------------------------------------------------------
# The writer must not reintroduce a literal path
# ---------------------------------------------------------------------------

test_that("plugin_server.R writes through the shared constants", {
  lines <- readLines(app_path("R/modules/plugin_server.R"), warn = FALSE)
  code <- lines[!startsWith(trimws(lines), "#")]

  offenders <- character(0)
  for (needle in c('"config/api_keys.json"', '"config/api_keys.R"',
                   'dir.create("config"')) {
    hits <- which(grepl(needle, code, fixed = TRUE))
    if (length(hits) > 0) {
      offenders <- c(offenders, sprintf("%s x%d", needle, length(hits)))
    }
  }

  expect_equal(offenders, character(0),
               label = "literal config paths in plugin_server.R")
})

# ---------------------------------------------------------------------------
# config.R must remain sourceable on its own, from any wd
# ---------------------------------------------------------------------------

test_that("config.R sources without error when app_path is absent", {
  # Many test files do `source(file.path(app_root, "R/config.R"))` before
  # anything else is loaded. config.R must not require app_path() to exist.
  rscript <- file.path(R.home("bin"), "Rscript.exe")
  if (!file.exists(rscript)) rscript <- file.path(R.home("bin"), "Rscript")
  skip_if(!file.exists(rscript), "Rscript not found; cannot spawn a fresh session")

  script <- tempfile(fileext = ".R")
  on.exit(unlink(script), add = TRUE)
  writeLines(c(
    'root <- commandArgs(trailingOnly = TRUE)[1]',
    'setwd(file.path(root, "tests", "testthat"))',
    'stopifnot(!exists("app_path", mode = "function"))',
    'suppressMessages(suppressWarnings(source(file.path(root, "R/config.R"))))',
    'cat("CONFIG_OK\\n")'
  ), script)

  out <- suppressWarnings(
    system2(rscript, c(shQuote(script), shQuote(app_path())),
            stdout = TRUE, stderr = TRUE)
  )

  expect_true(any(grepl("CONFIG_OK", out, fixed = TRUE)),
              info = paste(utils::tail(out, 6), collapse = " | "))
})
