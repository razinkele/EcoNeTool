# Harmonization Settings Server Module
#
# Everything on this tab is SESSION-ONLY: sliders, FS patterns, rule
# checkboxes, the profile and JSON import change session$userData$harm_config
# (read by the harmonize_* helpers through get_harm_config()) and nothing
# else. Only "Save as server default" and "Reset server default" touch the
# process-wide HARMONIZATION_CONFIG_FILE, and both require
# admin_authorized_strict() (spec B section 4.1, F2).
#
# Loop safety (B0, F1/F76): widget observers read rv$config only under
# isolate() and write it only through set_session_config(). Each returns early
# when the incoming value already equals the config, so the browser's echo of
# a push_config_to_widgets() update settles in one round.

harmonization_settings_server <- function(input, output, session) {

  # Read the server-default JSON ONCE per session, outside any reactive.
  # Pre-B0 (F1/F76) this load lived in an observe() that also read
  # rv$config, while the slider observer below wrote rv$config: the two
  # observers re-triggered each other forever and pinned the R process.
  # A rejected file (unparseable or invalid) makes the loader warn with class
  # "harm_config_rejected" and return the defaults. The handler only records
  # the reasons - it does not muffle - so the warning still reaches logs and
  # tests once; the reasons are shown on the tab below, because production
  # keeps no logs and the admin would otherwise see built-in values unexplained.
  load_rejection <- NULL
  initial_cfg <- if (file.exists(HARMONIZATION_CONFIG_FILE)) {
    withCallingHandlers(
      load_harmonization_config(HARMONIZATION_CONFIG_FILE),
      harm_config_rejected = function(w) load_rejection <<- w$errors
    )
  } else {
    HARMONIZATION_CONFIG
  }

  rv <- reactiveValues(
    config = initial_cfg,
    # Hash of the current server default; unsaved_changes compares with it,
    # so the page-load echo of the widgets does not count as a change.
    saved_hash = harm_config_hash(initial_cfg),
    unsaved_changes = FALSE
  )

  # Seed the per-session harm config so the harmonize_* helpers (which
  # read through get_harm_config() -> session$userData) see the server
  # default from page load. Never write HARMONIZATION_CONFIG to globalenv:
  # that contaminated every concurrent session (pre-PR9α).
  session$userData$harm_config <- initial_cfg

  # The only writer of the session config.
  set_session_config <- function(cfg) {
    rv$config <- cfg
    session$userData$harm_config <- cfg
    rv$unsaved_changes <- !identical(harm_config_hash(cfg), isolate(rv$saved_hash))
  }

  # Drive every widget from a config: sliders, FS pattern inputs, rule
  # checkboxes and the profile select. Called at session start, after import
  # and after Reset to Defaults.
  push_config_to_widgets <- function(cfg) {
    for (key in HARM_THRESHOLD_KEYS) {
      updateSliderInput(session, paste0("harm_thresh_", key), value = cfg$size_thresholds[[key]])
    }
    for (key in names(HARM_FS_PATTERN_LABELS)) {
      updateTextInput(session, paste0("harm_pattern_", key), value = cfg$foraging_patterns[[key]] %||% "")
    }
    for (rule in names(CONSUMED_TAXONOMIC_RULES)) {
      updateCheckboxInput(session, paste0("harm_rule_", rule), value = isTRUE(cfg$taxonomic_rules[[rule]]))
    }
    updateSelectInput(session, "harm_active_profile", selected = cfg$active_profile %||% "temperate")
  }

  show_status <- function(alert_class, icon_name, text) {
    output$harm_status_message <- renderUI({
      div(class = paste("alert", alert_class), icon(icon_name), " ", text)
    })
  }

  if (!is.null(load_rejection)) {
    show_status("alert-warning", "exclamation-triangle", sprintf(paste0(
      "Server default file rejected (%s); this session uses the built-in defaults. ",
      "An admin can Save or Reset the server default."
    ), paste(load_rejection, collapse = "; ")))
  }

  # Strict admin gate for the two server-default buttons. Returns TRUE (and
  # tells the user why) when the action must not run.
  refuse_unless_admin <- function(action) {
    refusal <- admin_strict_refusal(session$userData$admin_unlocked, action)
    if (is.null(refusal)) return(FALSE)
    show_status("alert-warning", "lock", refusal)
    showNotification(refusal, type = "warning", duration = 6)
    TRUE
  }

  # INITIALIZE: push the loaded config to every widget once. No reactive
  # read of rv$config, so nothing can re-trigger this.
  observeEvent(TRUE, {
    push_config_to_widgets(initial_cfg)
  }, once = TRUE)

  # UPDATE CONFIG (sliders). Depends on the six slider inputs only: rv$config
  # is read under isolate() and written once, so the write cannot re-trigger
  # this observer. An echo that matches the config is ignored.
  observe({
    req(input$harm_thresh_MS1_MS2, input$harm_thresh_MS2_MS3,
        input$harm_thresh_MS3_MS4, input$harm_thresh_MS4_MS5,
        input$harm_thresh_MS5_MS6, input$harm_thresh_MS6_MS7)
    cfg <- isolate(rv$config)
    incoming <- list(
      MS1_MS2 = input$harm_thresh_MS1_MS2, MS2_MS3 = input$harm_thresh_MS2_MS3,
      MS3_MS4 = input$harm_thresh_MS3_MS4, MS4_MS5 = input$harm_thresh_MS4_MS5,
      MS5_MS6 = input$harm_thresh_MS5_MS6, MS6_MS7 = input$harm_thresh_MS6_MS7
    )
    unchanged <- vapply(HARM_THRESHOLD_KEYS, function(k) {
      isTRUE(all.equal(cfg$size_thresholds[[k]], incoming[[k]]))
    }, logical(1))
    if (all(unchanged)) return()
    cfg$size_thresholds[HARM_THRESHOLD_KEYS] <- incoming[HARM_THRESHOLD_KEYS]
    set_session_config(cfg)
  })

  # FS PATTERNS. Debounced so a half-typed regex is not applied per keystroke.
  lapply(names(HARM_FS_PATTERN_LABELS), function(key) {
    input_id <- paste0("harm_pattern_", key)
    pattern_in <- debounce(reactive(input[[input_id]]), 500)
    observeEvent(pattern_in(), {
      value <- pattern_in()
      # The browser reports the UI's empty value before the start-up push
      # lands, and an empty pattern would match every text: ignore it.
      if (!is.character(value) || length(value) != 1L || !nzchar(trimws(value))) return()
      cfg <- isolate(rv$config)
      if (identical(cfg$foraging_patterns[[key]], value)) return()
      if (!harm_pattern_compiles(value)) {
        showNotification(sprintf("Invalid pattern for %s - keeping the previous one.", key),
                         type = "error", duration = 5)
        return()
      }
      cfg$foraging_patterns[[key]] <- value
      set_session_config(cfg)
    })
  })

  # TAXONOMIC RULES: one observer per rule the harmonize_* code reads.
  lapply(names(CONSUMED_TAXONOMIC_RULES), function(rule) {
    input_id <- paste0("harm_rule_", rule)
    # The checkboxes render checked, so a browser's FIRST report is the UI
    # literal TRUE, sent before the start-up push lands. Applying it would
    # re-enable a rule the loaded config disabled. A first report of FALSE
    # cannot be the literal, so it applies; later reports always apply.
    # This guard DEPENDS on the checkboxes being bound at session init, i.e.
    # rendered by the static harmonization_settings_ui() (checked = TRUE), so
    # the first report always precedes the push. If they ever move into a
    # renderUI()/insertUI(), the first report can arrive AFTER the push and
    # be a real value; this "skip the first TRUE" rule would then drop a real
    # click, so it must be replaced (e.g. compare with the pushed value).
    seen <- new.env(parent = emptyenv())
    seen$first <- TRUE
    observeEvent(input[[input_id]], {
      value <- isTRUE(input[[input_id]])
      startup_echo <- seen$first && value
      seen$first <- FALSE
      if (startup_echo) return()
      cfg <- isolate(rv$config)
      if (identical(isTRUE(cfg$taxonomic_rules[[rule]]), value)) return()
      cfg$taxonomic_rules[[rule]] <- value
      set_session_config(cfg)
    })
  })

  # ECOSYSTEM PROFILE
  observeEvent(input$harm_active_profile, {
    value <- input$harm_active_profile
    cfg <- isolate(rv$config)
    if (identical(cfg$active_profile, value) || !value %in% names(cfg$profiles)) return()
    cfg$active_profile <- value
    set_session_config(cfg)
  })

  # SAVE AS SERVER DEFAULT (admin only). Pre-PR9α this also did
  #   assign("HARMONIZATION_CONFIG", rv$config, envir = globalenv())
  # which contaminated every concurrent Shiny session. Pre-B1 anyone could
  # overwrite the server default with an unvalidated config (F2).
  observeEvent(input$harm_save_config, {
    if (refuse_unless_admin("save server default")) return()
    checked <- validate_harmonization_config(isolate(rv$config))
    if (!checked$ok) {
      show_status("alert-danger", "exclamation-circle",
                  paste("Not saved:", paste(checked$errors, collapse = "; ")))
      return()
    }
    tryCatch({
      save_harmonization_config(checked$config, HARMONIZATION_CONFIG_FILE)
      rv$saved_hash <- harm_config_hash(checked$config)
      rv$unsaved_changes <- FALSE
      show_status("alert-success", "check-circle", "Saved as the server default for new sessions.")
      showNotification("Server default saved", type = "message", duration = 3)
    }, error = function(e) {
      warning(sprintf("[harmonization] save failed: %s", conditionMessage(e)), call. = FALSE)
      show_status("alert-danger", "exclamation-circle", paste("Save failed:", conditionMessage(e)))
    })
  })

  # RESET SERVER DEFAULT (admin only): delete the file, so new sessions start
  # from the built-in HARMONIZATION_CONFIG. This session keeps its settings.
  observeEvent(input$harm_reset_server_default, {
    if (refuse_unless_admin("reset server default")) return()
    tryCatch({
      if (file.exists(HARMONIZATION_CONFIG_FILE) && !file.remove(HARMONIZATION_CONFIG_FILE)) {
        stop(sprintf("could not remove '%s'", HARMONIZATION_CONFIG_FILE), call. = FALSE)
      }
      rv$saved_hash <- harm_config_hash(HARMONIZATION_CONFIG)
      rv$unsaved_changes <- !identical(harm_config_hash(isolate(rv$config)), rv$saved_hash)
      show_status("alert-success", "check-circle",
                  "Server default reset: new sessions start from the built-in defaults.")
    }, error = function(e) {
      warning(sprintf("[harmonization] reset server default failed: %s", conditionMessage(e)),
              call. = FALSE)
      show_status("alert-danger", "exclamation-circle", paste("Reset failed:", conditionMessage(e)))
    })
  })

  # RESET TO DEFAULTS (session only, ungated). Rolls THIS session back to the
  # built-in HARMONIZATION_CONFIG - not to the server-default file - and
  # pushes every widget. The widgets' echo then equals rv$config and returns
  # early, so the reset settles in one round. The global default is never
  # written (pre-PR9α this re-sourced harmonization_config.R into globalenv).
  observeEvent(input$harm_reset_defaults, {
    set_session_config(HARMONIZATION_CONFIG)
    push_config_to_widgets(HARMONIZATION_CONFIG)
    showNotification("This session now uses the built-in defaults", type = "warning", duration = 3)
  })

  # SIZE DISTRIBUTION PREVIEW
  output$harm_size_distribution_plot <- renderPlot({
    req(input$harm_preview_size)

    isolate({
      cache_dir <- "cache/taxonomy"
      if (!dir.exists(cache_dir)) {
        plot.new()
        text(0.5, 0.5, "No cached data available", cex = 1.5)
        return()
      }

      cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
      sample_files <- sample(cache_files, min(100, length(cache_files)))
      sizes <- numeric()

      for (file in sample_files) {
        data <- tryCatch(readRDS(file), error = function(e) NULL)
        if (!is.null(data) && !is.null(data$max_length_cm)) {
          sizes <- c(sizes, data$max_length_cm)
        }
      }

      hist(log10(sizes + 0.01), breaks = 30, col = "lightblue", border = "white",
           main = paste("Size Distribution (", length(sizes), " species)"),
           xlab = "log10(Size in cm)", ylab = "Frequency")

      abline(v = log10(rv$config$size_thresholds$MS1_MS2), col = "red", lwd = 2, lty = 2)
      abline(v = log10(rv$config$size_thresholds$MS2_MS3), col = "orange", lwd = 2, lty = 2)
      abline(v = log10(rv$config$size_thresholds$MS3_MS4), col = "yellow", lwd = 2, lty = 2)
      abline(v = log10(rv$config$size_thresholds$MS4_MS5), col = "green", lwd = 2, lty = 2)
      abline(v = log10(rv$config$size_thresholds$MS5_MS6), col = "blue", lwd = 2, lty = 2)
      abline(v = log10(rv$config$size_thresholds$MS6_MS7), col = "purple", lwd = 2, lty = 2)
    })
  })

  # ECOSYSTEM PROFILE DETAILS
  output$harm_profile_details <- renderUI({
    req(input$harm_active_profile)
    profile <- rv$config$profiles[[input$harm_active_profile]]
    tagList(
      h5("Description:"),
      p(profile$description),
      h5("Size Multiplier:"),
      p(paste0(profile$size_multiplier, "x"))
    )
  })

  # PROFILE EFFECTS: the length at which each MS boundary bites under the
  # active profile. apply_size_adjustment() multiplies a measured length by
  # the profile multiplier before it is compared with the thresholds, so a
  # boundary at T cm is reached by a measured length of T / multiplier.
  output$harm_profile_effects <- renderText({
    cfg <- rv$config
    mult <- apply_size_adjustment(1)
    thr <- unlist(cfg$size_thresholds[HARM_THRESHOLD_KEYS])
    paste(c(
      sprintf("Profile '%s': measured sizes are multiplied by %s", cfg$active_profile %||% "temperate",
              format(mult)),
      "Measured length at each boundary:",
      sprintf("  %s: %s cm", names(thr), format(signif(thr / mult, 3)))
    ), collapse = "\n")
  })

  # EXPORT: anyone may download their own session config.
  output$harm_export_json <- downloadHandler(
    filename = function() {
      paste0("harmonization_config_", Sys.Date(), ".json")
    },
    content = function(file) {
      export_config_json(file, isolate(rv$config))
    }
  )

  # IMPORT: validated, then applied to this session only.
  observeEvent(input$harm_import_json, {
    req(input$harm_import_json)
    tryCatch({
      imported <- import_config_json(input$harm_import_json$datapath)
      set_session_config(imported)
      push_config_to_widgets(imported)
      show_status("alert-info", "file-import",
                  "Imported into this session only. An admin can save it as the server default.")
      showNotification("Configuration imported for this session", type = "message", duration = 3)
    }, error = function(e) {
      warning(sprintf("[harmonization] import failed: %s", conditionMessage(e)), call. = FALSE)
      showNotification(paste("Import failed:", conditionMessage(e)), type = "error", duration = 8)
    })
  })
}
