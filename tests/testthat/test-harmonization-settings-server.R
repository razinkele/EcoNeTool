# Regression tests for F1/F76 (deep analysis 2026-09-26, spec B section 4.0).
# Pre-B0 the INITIALIZE observe() in harmonization_settings_server.R re-read
# config/harmonization_custom.json and read rv$config reactively, while the
# UPDATE observe() wrote rv$config from the sliders. Each observer
# re-triggered the other, so one slider move pinned the single R process.
# These are the repo's first shiny::testServer() tests; testServer accepts
# the module's plain function(input, output, session) signature directly.
#
# B1 (spec B section 4.1) adds: the strict admin gate on the two server-default
# buttons (F2), JSON import/export (F8), the wired FS pattern / rule / profile
# widgets (F75), and the B0 deferred minors (RESET echo, exactly-one warning,
# a real two-live-sessions test, unsaved_changes on page load).

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")

source_harm_module <- function() {
  suppressPackageStartupMessages(library(shiny)) # the UI/module code is unqualified, as in app.R
  source(file.path(app_root, "R/functions/validation_utils.R"), local = FALSE)
  source(file.path(app_root, "R/functions/admin_auth.R"), local = FALSE)
  source(file.path(app_root, "R/config/harmonization_config.R"), local = FALSE)
  source(file.path(app_root, "R/functions/trait_lookup/harmonization.R"), local = FALSE)
  source(file.path(app_root, "R/ui/harmonization_settings_ui.R"), local = FALSE)
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

# A path that does not exist yet, removed again when the test exits.
new_harm_path <- function(env = parent.frame()) {
  path <- tempfile("harm_target_", fileext = ".json")
  withr::defer(unlink(c(path, paste0(path, ".tmp"))), envir = env)
  path
}

set_all_thresholds <- function(session, ms3_ms4, ms2_ms3 = 1.0) {
  session$setInputs(
    harm_thresh_MS1_MS2 = 0.1, harm_thresh_MS2_MS3 = ms2_ms3,
    harm_thresh_MS3_MS4 = ms3_ms4, harm_thresh_MS4_MS5 = 20.0,
    harm_thresh_MS5_MS6 = 50.0, harm_thresh_MS6_MS7 = 150.0
  )
}

# admin_gate_enabled() only checks that the record is non-blank, so any
# non-blank string switches the gate on without needing openssl.
GATE_ON <- "econetool1$12$00$00"

# Start the module in a hand-built MockShinySession. Unlike testServer() this
# records every update*Input() message the module sends (testServer drops
# them) and lets two sessions stay alive at the same time.
start_harm_session <- function(env = parent.frame()) {
  s <- shiny::MockShinySession$new()
  sent <- new.env()
  sent$msgs <- list()
  s$sendInputMessage <- function(inputId, message) sent$msgs[[inputId]] <- message
  shiny::isolate(shiny::withReactiveDomain(s, harmonization_settings_server(s$input, s$output, s)))
  s$flushReact()
  withr::defer(s$close(), envir = env)
  list(session = s, sent = sent)
}

read_json_file <- function(path) jsonlite::fromJSON(path, simplifyVector = FALSE)

# ---------------------------------------------------------------------------
# B0: the observer loop (F1/F76)
# ---------------------------------------------------------------------------

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

test_that("an unparseable server-default file warns exactly once and falls back to defaults", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  bad <- tempfile("harm_bad_", fileext = ".json")
  writeLines("{ not valid json", bad)
  withr::defer(unlink(bad))
  calls <- local_harm_loader(bad)

  seen <- character()
  withCallingHandlers(
    shiny::testServer(harmonization_settings_server, {
      expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4,
                   HARMONIZATION_CONFIG$size_thresholds$MS3_MS4)
      set_all_thresholds(session, ms3_ms4 = 9)
      expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4, 9)
    }),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(seen, 1L)
  expect_match(seen, "could not parse")
  expect_equal(calls$n, 1L)
})

# Final fix wave (I1): production keeps no logs, so the loader's warning alone
# left an admin looking at built-in values with no explanation. The rejection
# must be shown on the tab at session start.
test_that("a rejected server-default file is shown to the user, and the session uses the defaults", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  path <- tempfile("harm_stale_", fileext = ".json")
  withr::defer(unlink(path))
  cfg <- HARMONIZATION_CONFIG
  cfg$size_thresholds$MS3_MS4 <- 7
  cfg$foraging_patterns$FS0_primary_producer <-
    "photosyn|autotrop|producer|plant|algae|phytoplankton|diatom|dinoflagellate"
  export_config_json(path, cfg)
  calls <- local_harm_loader(path)

  seen <- character()
  withCallingHandlers(
    shiny::testServer(harmonization_settings_server, {
      expect_identical(session$userData$harm_config, HARMONIZATION_CONFIG)
      status <- output$harm_status_message$html
      expect_match(status, "Server default file rejected (", fixed = TRUE)
      expect_match(status, "diet nouns", fixed = TRUE)
      expect_match(status, paste("this session uses the built-in defaults.",
                                 "An admin can Save or Reset the server default."), fixed = TRUE)
      expect_match(status, "alert-warning", fixed = TRUE)
      expect_false(shiny::isolate(rv$unsaved_changes))
    }),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(seen, 1L) # the loader's warning still reaches logs/tests, once
  expect_match(seen, "invalid config")
  expect_equal(calls$n, 1L)
})

test_that("an unparseable server-default file is shown to the user as rejected too", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  bad <- tempfile("harm_bad_", fileext = ".json")
  writeLines("{ not valid json", bad)
  withr::defer(unlink(bad))
  local_harm_loader(bad)

  suppressWarnings(shiny::testServer(harmonization_settings_server, {
    expect_match(output$harm_status_message$html, "Server default file rejected (could not parse", fixed = TRUE)
  }))
})

test_that("a valid server-default file shows no rejection status", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  local_harm_loader(write_harm_json(ms3_ms4 = 7))

  shiny::testServer(harmonization_settings_server, {
    expect_error(output$harm_status_message) # never rendered
  })
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

test_that("two live sessions keep independent configs and the global default is untouched", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  calls <- local_harm_loader(write_harm_json(ms3_ms4 = 7))
  default_before <- HARMONIZATION_CONFIG

  a <- start_harm_session()
  b <- start_harm_session()
  set_all_thresholds(b$session, ms3_ms4 = 9)
  set_all_thresholds(a$session, ms3_ms4 = 8)

  # Both sessions are still alive here; each sees only its own move.
  expect_equal(a$session$userData$harm_config$size_thresholds$MS3_MS4, 8)
  expect_equal(b$session$userData$harm_config$size_thresholds$MS3_MS4, 9)
  expect_identical(HARMONIZATION_CONFIG, default_before)
  expect_equal(calls$n, 2L)
})

# ---------------------------------------------------------------------------
# B1 / F2: server-default save and reset are strictly admin-gated
# ---------------------------------------------------------------------------

test_that("save is refused, with a warning and no file, when the admin gate is unset", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = "")
  target <- new_harm_path()
  local_harm_loader(target)

  shiny::testServer(harmonization_settings_server, {
    session$userData$admin_unlocked <- TRUE # even a claimed unlock must not help
    expect_warning(session$setInputs(harm_save_config = 1),
                   "Admin gate not configured on this instance", fixed = TRUE)
    expect_match(output$harm_status_message$html, "server defaults are read-only", fixed = TRUE)
  })
  expect_false(file.exists(target))
})

test_that("save is refused when the gate is set but the session is locked", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = GATE_ON)
  target <- new_harm_path()
  local_harm_loader(target)

  shiny::testServer(harmonization_settings_server, {
    expect_warning(session$setInputs(harm_save_config = 1),
                   "Unlock via Trait Research > Configure API Keys first", fixed = TRUE)
  })
  expect_false(file.exists(target))
})

test_that("save by an unlocked admin writes a file that round-trips", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = GATE_ON)
  target <- new_harm_path()
  calls <- local_harm_loader(target)

  shiny::testServer(harmonization_settings_server, {
    session$userData$admin_unlocked <- TRUE
    set_all_thresholds(session, ms3_ms4 = 9)
    expect_true(shiny::isolate(rv$unsaved_changes))
    suppressMessages(session$setInputs(harm_save_config = 1))
    expect_false(shiny::isolate(rv$unsaved_changes))
  })
  expect_true(file.exists(target))
  expect_false(file.exists(paste0(target, ".tmp")))
  expect_equal(calls$n, 0L)
  expect_equal(load_harmonization_config(target)$size_thresholds$MS3_MS4, 9)
})

test_that("an FS0 pattern with diet nouns is never saved as the server default", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = GATE_ON)
  target <- new_harm_path()
  local_harm_loader(target)

  shiny::testServer(harmonization_settings_server, {
    session$userData$admin_unlocked <- TRUE
    session$setInputs(harm_pattern_FS0_primary_producer = "photosyn|producer|plant|algae")
    session$elapse(600)
    session$setInputs(harm_save_config = 1)
    expect_match(output$harm_status_message$html, "diet nouns", fixed = TRUE)
  })
  expect_false(file.exists(target))
})

test_that("an invalid session config (overlapping sliders) is not saved, even by an admin", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = GATE_ON)
  target <- new_harm_path()
  local_harm_loader(target)

  shiny::testServer(harmonization_settings_server, {
    session$userData$admin_unlocked <- TRUE
    # The MS2/MS3 slider goes up to 5 and the MS3/MS4 slider down to 1, so the
    # UI itself can produce non-increasing boundaries.
    set_all_thresholds(session, ms3_ms4 = 2, ms2_ms3 = 4)
    session$setInputs(harm_save_config = 1)
    expect_match(output$harm_status_message$html, "strictly increasing", fixed = TRUE)
  })
  expect_false(file.exists(target))
})

test_that("a save that cannot write warns and reports the failure", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = GATE_ON)
  blocker <- tempfile("harm_blocker_")
  writeLines("a file, not a directory", blocker)
  withr::defer(unlink(blocker))
  local_harm_loader(file.path(blocker, "harmonization_custom.json"))

  shiny::testServer(harmonization_settings_server, {
    session$userData$admin_unlocked <- TRUE
    expect_warning(suppressMessages(session$setInputs(harm_save_config = 1)),
                   "[harmonization] save failed", fixed = TRUE)
    expect_match(output$harm_status_message$html, "Save failed", fixed = TRUE)
  })
})

test_that("reset server default is gated and, when unlocked, removes the file", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  path <- write_harm_json(ms3_ms4 = 7)
  local_harm_loader(path)

  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = "")
  shiny::testServer(harmonization_settings_server, {
    session$userData$admin_unlocked <- TRUE
    expect_warning(session$setInputs(harm_reset_server_default = 1), "[admin auth]", fixed = TRUE)
  })
  expect_true(file.exists(path))

  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = GATE_ON)
  shiny::testServer(harmonization_settings_server, {
    session$userData$admin_unlocked <- TRUE
    session$setInputs(harm_reset_server_default = 1)
    expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4, 7,
                 info = "resetting the server default does not change this session")
  })
  expect_false(file.exists(path))
})

test_that("one session's unlock does not authorize another live session", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = GATE_ON)
  target <- new_harm_path()
  local_harm_loader(target)

  admin <- start_harm_session()
  other <- start_harm_session()
  admin$session$userData$admin_unlocked <- TRUE

  expect_warning(other$session$setInputs(harm_save_config = 1), "Unlock via Trait Research", fixed = TRUE)
  expect_false(file.exists(target))
  suppressMessages(admin$session$setInputs(harm_save_config = 1))
  expect_true(file.exists(target))
})

# ---------------------------------------------------------------------------
# B1 / F8: JSON import (session-only) and export (anyone)
# ---------------------------------------------------------------------------

test_that("import is session-only and pushes the imported values to the widgets", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  target <- new_harm_path()
  local_harm_loader(target)
  upload <- tempfile("harm_upload_", fileext = ".json")
  withr::defer(unlink(upload))
  cfg <- HARMONIZATION_CONFIG
  cfg$size_thresholds$MS3_MS4 <- 11
  cfg$taxonomic_rules$bivalves_sessile <- FALSE
  export_config_json(upload, cfg)

  h <- start_harm_session()
  h$sent$msgs <- list()
  h$session$setInputs(harm_import_json = list(name = "mine.json", datapath = upload))

  expect_equal(h$session$userData$harm_config$size_thresholds$MS3_MS4, 11)
  expect_false(h$session$userData$harm_config$taxonomic_rules$bivalves_sessile)
  expect_identical(h$sent$msgs$harm_thresh_MS3_MS4$value, "11")
  expect_false(h$sent$msgs$harm_rule_bivalves_sessile$value)
  expect_false(file.exists(target))
  # Outside any session the process-wide default is untouched.
  expect_equal(get_harm_config()$size_thresholds$MS3_MS4, 5)
})

test_that("an invalid import keeps the session config and warns", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  local_harm_loader(new_harm_path())
  upload <- tempfile("harm_upload_", fileext = ".json")
  withr::defer(unlink(upload))
  cfg <- HARMONIZATION_CONFIG
  cfg$size_thresholds$MS3_MS4 <- 0.5 # below MS2_MS3
  export_config_json(upload, cfg)

  shiny::testServer(harmonization_settings_server, {
    expect_warning(session$setInputs(harm_import_json = list(name = "bad.json", datapath = upload)),
                   "[harmonization] import failed", fixed = TRUE)
    expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4, 5)
  })
})

test_that("anyone can download the session config as JSON", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = GATE_ON) # locked session
  local_harm_loader(new_harm_path())

  shiny::testServer(harmonization_settings_server, {
    set_all_thresholds(session, ms3_ms4 = 9)
    downloaded <- output$harm_export_json
    expect_true(file.exists(downloaded))
    expect_equal(import_config_json(downloaded)$size_thresholds$MS3_MS4, 9)
  })
})

# ---------------------------------------------------------------------------
# B1 / F75: every widget reaches the config the harmonize_* code reads
# ---------------------------------------------------------------------------

test_that("the widgets are seeded from the config at start, not from UI literals", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  path <- tempfile("harm_cfg_", fileext = ".json")
  withr::defer(unlink(path))
  cfg <- HARMONIZATION_CONFIG
  cfg$taxonomic_rules$bivalves_sessile <- FALSE
  cfg$active_profile <- "baltic"
  suppressMessages(save_harmonization_config(cfg, path))
  local_harm_loader(path)

  h <- start_harm_session()
  fs0 <- h$sent$msgs$harm_pattern_FS0_primary_producer$value
  expect_identical(fs0, HARMONIZATION_CONFIG$foraging_patterns$FS0_primary_producer)
  expect_false(grepl("algae|plant", fs0))
  expect_identical(h$sent$msgs$harm_pattern_FS7_xylophagous$value,
                   HARMONIZATION_CONFIG$foraging_patterns$FS7_xylophagous)
  expect_false(h$sent$msgs$harm_rule_bivalves_sessile$value)
  expect_true(h$sent$msgs$harm_rule_fish_obligate_swimmers$value)
  expect_identical(h$sent$msgs$harm_active_profile$value, "baltic")

  # The UI literal can never be the source of truth: its FS values are empty.
  html <- as.character(harmonization_settings_ui())
  fs_tags <- regmatches(html, gregexpr('<input id="harm_pattern_[A-Za-z0-9_]+"[^>]*>', html))[[1]]
  expect_length(fs_tags, length(HARM_FS_PATTERN_LABELS))
  expect_true(all(grepl('value=""', fs_tags, fixed = TRUE)))
  expect_false(grepl("algae", html))
})

test_that("a rule checkbox reaches is_rule_enabled() in its own session only", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  local_harm_loader(new_harm_path())

  shiny::testServer(harmonization_settings_server, {
    expect_true(shiny::isolate(is_rule_enabled("fish_obligate_swimmers")))
    session$setInputs(harm_rule_fish_obligate_swimmers = FALSE)
    expect_false(shiny::isolate(is_rule_enabled("fish_obligate_swimmers")))
  })
  expect_true(is_rule_enabled("fish_obligate_swimmers"))
})

test_that("the start-up TRUE echo of a rule checkbox does not re-enable a rule the config disabled", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  path <- tempfile("harm_cfg_", fileext = ".json")
  withr::defer(unlink(path))
  cfg <- HARMONIZATION_CONFIG
  cfg$taxonomic_rules$bivalves_sessile <- FALSE
  suppressMessages(save_harmonization_config(cfg, path))
  local_harm_loader(path)

  shiny::testServer(harmonization_settings_server, {
    # A real browser first reports the checkbox's UI literal (TRUE), before
    # the start-up push of FALSE lands.
    session$setInputs(harm_rule_bivalves_sessile = TRUE)
    expect_false(session$userData$harm_config$taxonomic_rules$bivalves_sessile)
    expect_false(shiny::isolate(is_rule_enabled("bivalves_sessile")))
    expect_false(shiny::isolate(rv$unsaved_changes))
    # Then the pushed value echoes back.
    session$setInputs(harm_rule_bivalves_sessile = FALSE)
    expect_false(session$userData$harm_config$taxonomic_rules$bivalves_sessile)
    expect_false(shiny::isolate(rv$unsaved_changes))
    # A later, real click does apply.
    session$setInputs(harm_rule_bivalves_sessile = TRUE)
    expect_true(session$userData$harm_config$taxonomic_rules$bivalves_sessile)
    expect_true(shiny::isolate(rv$unsaved_changes))
  })
})

test_that("the start-up empty echo of the FS inputs keeps the loaded patterns", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  path <- tempfile("harm_cfg_", fileext = ".json")
  withr::defer(unlink(path))
  cfg <- HARMONIZATION_CONFIG
  cfg$foraging_patterns$FS1_predator <- "predat|hunter|ambush"
  suppressMessages(save_harmonization_config(cfg, path))
  local_harm_loader(path)

  shiny::testServer(harmonization_settings_server, {
    # A real browser reports every FS text input's UI literal ("") first.
    for (key in names(HARM_FS_PATTERN_LABELS)) {
      do.call(session$setInputs, stats::setNames(list(""), paste0("harm_pattern_", key)))
    }
    session$elapse(600)
    expect_identical(session$userData$harm_config$foraging_patterns, cfg$foraging_patterns)
    expect_false(shiny::isolate(rv$unsaved_changes))
    # The loaded (non-default) pattern still drives the consumer, and an empty
    # pattern was never applied as match-everything.
    expect_identical(shiny::isolate(harmonize_foraging_strategy("ambush feeder")), "FS1")
  })
})

test_that("an FS pattern edit reaches the consumer after the debounce; bad input keeps the old value", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  local_harm_loader(new_harm_path())
  old <- HARMONIZATION_CONFIG$foraging_patterns$FS1_predator

  shiny::testServer(harmonization_settings_server, {
    session$setInputs(harm_pattern_FS1_predator = "predat|hunter|ambush")
    session$elapse(600)
    expect_identical(session$userData$harm_config$foraging_patterns$FS1_predator,
                     "predat|hunter|ambush")
    expect_identical(shiny::isolate(harmonize_foraging_strategy("ambush feeder")), "FS1")

    session$setInputs(harm_pattern_FS1_predator = "predat(")
    session$elapse(600)
    expect_identical(session$userData$harm_config$foraging_patterns$FS1_predator,
                     "predat|hunter|ambush")

    # A browser reports the UI's empty value before the push lands; an empty
    # pattern would match every text, so it must be ignored.
    session$setInputs(harm_pattern_FS0_primary_producer = "")
    session$elapse(600)
    expect_identical(session$userData$harm_config$foraging_patterns$FS0_primary_producer,
                     HARMONIZATION_CONFIG$foraging_patterns$FS0_primary_producer)
  })
  expect_identical(HARMONIZATION_CONFIG$foraging_patterns$FS1_predator, old)
})

test_that("the profile select reaches apply_size_adjustment() and the effects panel", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  local_harm_loader(new_harm_path())

  shiny::testServer(harmonization_settings_server, {
    session$setInputs(harm_active_profile = "arctic")
    expect_identical(session$userData$harm_config$active_profile, "arctic")
    expect_equal(shiny::isolate(apply_size_adjustment(10)), 12)
    expect_match(output$harm_profile_effects, "arctic", fixed = TRUE)
    expect_match(output$harm_profile_effects, "1.2", fixed = TRUE)

    session$setInputs(harm_active_profile = "atlantis") # not a profile: ignored
    expect_identical(session$userData$harm_config$active_profile, "arctic")
  })
})

# ---------------------------------------------------------------------------
# B0 deferred minors: RESET echo and unsaved_changes on page load
# ---------------------------------------------------------------------------

test_that("Reset to Defaults is session-only, pushes the widgets and its echo settles", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  path <- write_harm_json(ms3_ms4 = 7)
  calls <- local_harm_loader(path)

  h <- start_harm_session()
  set_all_thresholds(h$session, ms3_ms4 = 9)
  h$sent$msgs <- list()
  h$session$setInputs(harm_reset_defaults = 1)

  expect_equal(h$session$userData$harm_config$size_thresholds$MS3_MS4, 5)
  expect_identical(h$sent$msgs$harm_thresh_MS3_MS4$value, "5")
  # The browser echoes the pushed values back; nothing reloads or re-pushes.
  h$sent$msgs <- list()
  set_all_thresholds(h$session, ms3_ms4 = 5)
  expect_equal(h$session$userData$harm_config$size_thresholds$MS3_MS4, 5)
  expect_length(h$sent$msgs, 0L)
  expect_equal(calls$n, 1L)
  expect_equal(read_json_file(path)$size_thresholds$MS3_MS4, 7) # server file untouched
})

test_that("page load does not flag unsaved changes; a real move does", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  local_harm_loader(write_harm_json(ms3_ms4 = 7))

  shiny::testServer(harmonization_settings_server, {
    set_all_thresholds(session, ms3_ms4 = 5) # UI literal first
    set_all_thresholds(session, ms3_ms4 = 7) # then the pushed file value
    expect_false(shiny::isolate(rv$unsaved_changes))
    set_all_thresholds(session, ms3_ms4 = 9)
    expect_true(shiny::isolate(rv$unsaved_changes))
  })
})
