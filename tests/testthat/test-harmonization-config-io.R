# Spec B section 4.1 (F2, F8): the shared harmonization-config validator,
# JSON import/export, and the server-default loader/saver. Pure functions:
# no Shiny session needed.

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
source(file.path(app_root, "R/functions/validation_utils.R"), local = FALSE)
source(file.path(app_root, "R/config/harmonization_config.R"), local = FALSE)

with_threshold <- function(key, value) {
  cfg <- HARMONIZATION_CONFIG
  cfg$size_thresholds[[key]] <- value
  cfg
}

errors_of <- function(cfg) paste(validate_harmonization_config(cfg)$errors, collapse = " | ")

test_that("the built-in defaults validate", {
  v <- validate_harmonization_config(HARMONIZATION_CONFIG)
  expect_true(v$ok)
  expect_identical(v$errors, character(0))
  expect_identical(v$config$size_thresholds, HARMONIZATION_CONFIG$size_thresholds)
})

test_that("non-increasing, infinite, negative and non-numeric thresholds are rejected", {
  expect_false(validate_harmonization_config(with_threshold("MS3_MS4", 0.5))$ok)
  expect_false(validate_harmonization_config(with_threshold("MS3_MS4", 1.0))$ok) # equals MS2_MS3
  expect_false(validate_harmonization_config(with_threshold("MS6_MS7", Inf))$ok)
  expect_false(validate_harmonization_config(with_threshold("MS1_MS2", -0.1))$ok)
  expect_false(validate_harmonization_config(with_threshold("MS1_MS2", "0.1"))$ok)
  expect_match(errors_of(with_threshold("MS3_MS4", 0.5)), "strictly increasing")
  expect_match(errors_of(with_threshold("MS6_MS7", Inf)), "MS6_MS7")
})

test_that("uncompilable and empty foraging patterns are rejected", {
  cfg <- HARMONIZATION_CONFIG
  cfg$foraging_patterns$FS1_predator <- "("
  expect_false(validate_harmonization_config(cfg)$ok)
  expect_match(errors_of(cfg), "FS1_predator")
  cfg$foraging_patterns$FS1_predator <- ""
  expect_false(validate_harmonization_config(cfg)$ok)
  expect_true(harm_pattern_compiles("predat|hunter"))
  expect_false(harm_pattern_compiles("predat("))
  expect_false(harm_pattern_compiles(c("a", "b")))
})

test_that("an FS0 pattern that matches diet nouns is rejected (the 2026-07-17 inversion)", {
  # The string production held in 2026-09: diet nouns make every herbivore
  # and planktivore a primary producer, because FS0 is tested first.
  stale <- "photosyn|autotrop|producer|plant|algae|phytoplankton|diatom|dinoflagellate"
  cfg <- HARMONIZATION_CONFIG
  cfg$foraging_patterns$FS0_primary_producer <- stale
  v <- validate_harmonization_config(cfg)
  expect_false(v$ok)
  expect_match(paste(v$errors, collapse = " | "), "FS0_primary_producer matches diet nouns")
  expect_match(paste(v$errors, collapse = " | "), "phytoplankton")

  cfg$foraging_patterns$FS0_primary_producer <- "producer|seaweed"
  expect_false(validate_harmonization_config(cfg)$ok)

  # The built-in default names no diet noun and passes.
  expect_identical(HARMONIZATION_CONFIG$foraging_patterns$FS0_primary_producer,
                   "photosyn|autotroph|autotrop|primary.?produc|producer")
  expect_true(validate_harmonization_config(HARMONIZATION_CONFIG)$ok)
})

test_that("a stale server file with a diet-noun FS0 loads as the defaults, with a warning", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  cfg <- HARMONIZATION_CONFIG
  cfg$foraging_patterns$FS0_primary_producer <- "photosyn|autotrop|producer|plant|algae|phytoplankton|diatom|dinoflagellate"
  export_config_json(tmp, cfg)
  expect_warning(got <- load_harmonization_config(tmp), "diet nouns")
  expect_identical(got, HARMONIZATION_CONFIG)
  expect_error(import_config_json(tmp), "diet nouns")
})

test_that("non-logical taxonomic rules and unknown profiles are rejected", {
  cfg <- HARMONIZATION_CONFIG
  cfg$taxonomic_rules$bivalves_sessile <- "yes"
  expect_false(validate_harmonization_config(cfg)$ok)
  expect_match(errors_of(cfg), "bivalves_sessile")

  cfg <- HARMONIZATION_CONFIG
  cfg$active_profile <- "atlantis"
  expect_false(validate_harmonization_config(cfg)$ok)
  expect_match(errors_of(cfg), "atlantis")
})

test_that("unknown top-level keys are dropped and missing keys are filled from the defaults", {
  v <- validate_harmonization_config(list(size_thresholds = list(MS3_MS4 = 6), evil = "x"))
  expect_true(v$ok)
  expect_null(v$config$evil)
  expect_equal(v$config$size_thresholds$MS3_MS4, 6)
  expect_equal(v$config$size_thresholds$MS6_MS7, 150)
  expect_identical(v$config$foraging_patterns, HARMONIZATION_CONFIG$foraging_patterns)
})

test_that("a config that is not a named list is rejected", {
  expect_false(validate_harmonization_config(list(1, 2))$ok)
  expect_false(validate_harmonization_config("x")$ok)
  expect_false(validate_harmonization_config(NULL)$ok)
})

test_that("export then import round-trips and never touches the global default", {
  global_before <- HARMONIZATION_CONFIG
  cfg <- with_threshold("MS3_MS4", 7.5)
  cfg$taxonomic_rules$bivalves_sessile <- FALSE
  cfg$active_profile <- "baltic"
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  expect_true(export_config_json(tmp, cfg))
  got <- import_config_json(tmp)

  expect_equal(got$size_thresholds, cfg$size_thresholds)
  expect_equal(got$foraging_patterns, cfg$foraging_patterns)
  expect_equal(got$taxonomic_rules, cfg$taxonomic_rules)
  expect_identical(got$active_profile, "baltic")
  expect_identical(HARMONIZATION_CONFIG, global_before)
})

test_that("import stops with the validation errors for an invalid file", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  export_config_json(tmp, with_threshold("MS3_MS4", 0.5))
  expect_error(import_config_json(tmp), "strictly increasing")
})

test_that("import stops on unparseable JSON", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("{ nope", tmp)
  expect_error(import_config_json(tmp))
})

test_that("load_harmonization_config warns and returns the defaults for an invalid server file", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  export_config_json(tmp, with_threshold("MS3_MS4", 0.5))
  expect_warning(got <- load_harmonization_config(tmp), "invalid config")
  expect_identical(got, HARMONIZATION_CONFIG)
})

test_that("load_harmonization_config fills a partial server file from the defaults", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  writeLines('{"size_thresholds": {"MS3_MS4": 6}}', tmp)
  got <- load_harmonization_config(tmp)
  expect_equal(got$size_thresholds$MS3_MS4, 6)
  expect_equal(got$size_thresholds$MS6_MS7, 150)
  expect_identical(got$foraging_patterns, HARMONIZATION_CONFIG$foraging_patterns)
})

test_that("save replaces the file through a temp file and leaves nothing behind", {
  dir <- tempfile("harm_save_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  target <- file.path(dir, "harmonization_custom.json")

  suppressMessages(save_harmonization_config(with_threshold("MS3_MS4", 6), target))
  suppressMessages(save_harmonization_config(with_threshold("MS3_MS4", 8), target))

  expect_equal(load_harmonization_config(target)$size_thresholds$MS3_MS4, 8)
  expect_identical(list.files(dir), "harmonization_custom.json")
})
