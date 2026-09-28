# =============================================================================
# API KEY FORM HELPERS (F6)
# =============================================================================
# At file scope so they are unit-testable without a session.

#' The "API Key Configuration" modal
#'
#' Secrets are never sent to the browser: both secret fields are password
#' inputs that start empty, and leaving one blank keeps the stored value
#' (see merge_api_key_submission()). Only the username is pre-filled.
#'
#' @param stored API_KEYS environment (or a list) holding the current keys.
api_key_modal_dialog <- function(stored) {
  keep <- "(unchanged - leave blank to keep)"
  modalDialog(
    title = "API Key Configuration", size = "m",
    textInput("api_key_algaebase_user", "AlgaeBase Username:",
              value = stored$algaebase_username %||% ""),
    passwordInput("api_key_algaebase_pass", "AlgaeBase Password:", value = "",
                  placeholder = keep),
    hr(),
    passwordInput("api_key_freshwater", "freshwaterecology.info API Key:", value = "",
                  placeholder = keep),
    tags$p(class = "text-muted",
           "Keys saved to config/api_keys.json (gitignored). AlgaeBase: register at algaebase.org."),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("save_api_keys", "Save Keys", class = "btn-primary", icon = icon("save"))
    )
  )
}

#' Merge an API-key form submission into the stored keys
#'
#' @param stored API_KEYS environment (or a list).
#' @param username Submitted username; written as given (blank clears it).
#' @param password,freshwater_key Submitted secrets; NULL, NA, empty or
#'   whitespace-only keeps the stored value.
#' @return list(algaebase_username, algaebase_password, freshwaterecology_key).
merge_api_key_submission <- function(stored, username, password, freshwater_key) {
  keep_if_blank <- function(new, old) {
    if (is.null(new) || length(new) != 1 || is.na(new) || !nzchar(trimws(new))) old %||% "" else new
  }
  list(
    algaebase_username = username %||% "",
    algaebase_password = keep_if_blank(password, stored$algaebase_password),
    freshwaterecology_key = keep_if_blank(freshwater_key, stored$freshwaterecology_key)
  )
}

#' Write the API keys JSON atomically with owner-only permissions
#'
#' Writes a sibling tmp file, chmods it 0600, then renames it over `path`
#' (copy + remove if the rename fails), so a crash never leaves a truncated
#' key file and the secrets are never world-readable. chmod is a no-op on
#' Windows.
#' @param keys_list Named list of keys.
#' @param path Destination (API_KEYS_JSON).
write_api_keys_json <- function(keys_list, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(path, ".tmp.", Sys.getpid())
  jsonlite::write_json(keys_list, tmp, auto_unbox = TRUE, pretty = TRUE)
  Sys.chmod(tmp, mode = "0600")
  if (!isTRUE(suppressWarnings(file.rename(tmp, path)))) {
    ok <- file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
    if (!isTRUE(ok)) stop("could not write ", path, call. = FALSE)
  }
  Sys.chmod(path, mode = "0600")
  invisible(path)
}

#' Plugin Management Server Module
#'
#' Handles plugin settings UI rendering, saving, and resetting.
#' Uses flat input/output pattern (no moduleServer/NS) to avoid breaking UI references.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param plugin_states reactiveVal holding plugin states (plugin_id => enabled TRUE/FALSE)

plugin_server <- function(input, output, session, plugin_states) {

  # Render plugin settings UI
  output$plugin_settings_ui <- renderUI({
    # Add reactive dependency so UI updates when plugin states change
    current_states <- plugin_states()

    all_plugins <- get_all_plugins()
    categories <- c("core", "analysis", "data", "advanced")

    tagList(
      lapply(categories, function(cat) {
        plugins_in_cat <- get_plugins_by_category(cat)

        if (length(plugins_in_cat) == 0) return(NULL)

        tagList(
          h4(toupper(cat), " MODULES",
             style = paste0("margin-top: 20px; padding: 10px; background: ",
                           if (cat == "core") "#e3f2fd" else if (cat == "analysis") "#fff3e0"
                           else if (cat == "data") "#e8f5e9" else "#f3e5f5",
                           ";")),

          lapply(names(plugins_in_cat), function(plugin_id) {
            plugin <- plugins_in_cat[[plugin_id]]

            # Check if packages are available
            packages_ok <- check_plugin_packages(plugin_id)
            can_enable <- packages_ok

            tagList(
              div(
                style = paste0("padding: 15px; margin: 10px 0; border: 1px solid #ddd; ",
                              "border-radius: 5px; background: white;"),

                fluidRow(
                  column(1,
                    icon(plugin$icon, style = "font-size: 24px; color: #007bff;")
                  ),
                  column(8,
                    h5(plugin$name, style = "margin-top: 0;"),
                    p(plugin$description, style = "margin: 5px 0; color: #666; font-size: 13px;"),

                    # Show package requirements if any
                    if (!is.null(plugin$packages)) {
                      tagList(
                        p(
                          tags$small(
                            tags$strong("Requires packages: "),
                            paste(plugin$packages, collapse = ", "),
                            if (!packages_ok) {
                              tags$span(" (NOT INSTALLED)", style = "color: red; font-weight: bold;")
                            } else {
                              tags$span(" (installed)", style = "color: green;")
                            }
                          ),
                          style = "margin: 5px 0;"
                        )
                      )
                    }
                  ),
                  column(3,
                    if (plugin$required) {
                      tags$span(
                        icon("lock"), " REQUIRED",
                        style = "color: #999; font-size: 12px;"
                      )
                    } else {
                      switchInput(
                        inputId = paste0("plugin_", plugin_id),
                        label = NULL,
                        value = current_states[[plugin_id]] %||% FALSE,
                        onLabel = "ON",
                        offLabel = "OFF",
                        onStatus = "success",
                        offStatus = "danger",
                        size = "normal",
                        disabled = !can_enable
                      )
                    }
                  )
                )
              )
            )
          })
        )
      }),

      hr(),
      div(
        style = "text-align: right;",
        actionButton("save_plugin_settings",
                    "Save & Apply",
                    icon = icon("save"),
                    class = "btn-primary"),
        actionButton("reset_plugin_settings",
                    "Reset to Defaults",
                    icon = icon("undo"),
                    class = "btn-secondary")
      )
    )
  })

  # Ensure plugin settings UI is not suspended when hidden
  outputOptions(output, "plugin_settings_ui", suspendWhenHidden = FALSE)

  # Save plugin settings
  observeEvent(input$save_plugin_settings, {
    all_plugins <- get_all_plugins()
    new_states <- list()

    for (plugin_id in names(all_plugins)) {
      plugin <- all_plugins[[plugin_id]]

      if (plugin$required) {
        # Required plugins are always enabled
        new_states[[plugin_id]] <- TRUE
      } else {
        # Get state from switch input
        input_id <- paste0("plugin_", plugin_id)
        new_states[[plugin_id]] <- input[[input_id]] %||% FALSE
      }
    }

    plugin_states(new_states)

    showNotification(
      "\u2713 Plugin settings saved and applied!",
      type = "message",
      duration = 3
    )
  })

  # Reset to defaults
  observeEvent(input$reset_plugin_settings, {
    plugin_states(get_default_plugin_states())

    showNotification(
      "\u2713 Plugin settings reset to defaults and applied!",
      type = "message",
      duration = 3
    )
  })

  # API KEY CONFIGURATION
  #
  # Optionally protected by an admin password (R/functions/admin_auth.R).
  # With ECONETOOL_ADMIN_PASSWORD_HASH unset the gate is inert and this
  # behaves exactly as it did before. The unlock is recorded in
  # session$userData - never a global - so one user's unlock cannot leak to
  # other sessions sharing the same R process.
  max_unlock_attempts <- 5L

  show_api_key_modal <- function() {
    showModal(api_key_modal_dialog(if (exists("API_KEYS")) API_KEYS else list()))
  }

  show_admin_unlock_modal <- function(error_message = NULL) {
    showModal(modalDialog(
      title = "Admin password required", size = "s",
      tags$p("API key configuration is protected on this instance."),
      passwordInput("admin_password", "Password:", value = ""),
      if (!is.null(error_message)) {
        tags$p(class = "text-danger", role = "alert", error_message)
      },
      footer = tagList(
        modalButton("Cancel"),
        actionButton("admin_unlock", "Unlock", class = "btn-primary",
                     icon = icon("unlock"))
      )
    ))
  }

  observeEvent(input$show_api_keys, {
    if (admin_authorized(session$userData$admin_unlocked)) {
      show_api_key_modal()
    } else {
      show_admin_unlock_modal()
    }
  })

  observeEvent(input$admin_unlock, {
    attempts <- session$userData$admin_attempts %||% 0L

    if (attempts >= max_unlock_attempts) {
      warning(sprintf("[admin auth] unlock attempt limit (%d) reached; refusing",
                      max_unlock_attempts), call. = FALSE)
      removeModal()
      showNotification(
        "Too many failed attempts. Reload the page to try again.",
        type = "error", duration = 8
      )
      return()
    }

    if (isTRUE(verify_admin_password(input$admin_password))) {
      session$userData$admin_unlocked <- TRUE
      session$userData$admin_attempts <- 0L
      removeModal()
      show_api_key_modal()
      return()
    }

    attempts <- attempts + 1L
    session$userData$admin_attempts <- attempts
    warning(sprintf("[admin auth] failed unlock attempt %d of %d",
                    attempts, max_unlock_attempts), call. = FALSE)

    remaining <- max_unlock_attempts - attempts
    show_admin_unlock_modal(
      error_message = if (remaining > 0L) {
        sprintf("Incorrect password. %d attempt%s remaining.",
                remaining, if (remaining == 1L) "" else "s")
      } else {
        "Incorrect password. No attempts remaining - reload the page."
      }
    )
  })

  observeEvent(input$save_api_keys, {
    # Authorise the WRITE path independently of the modal. Input IDs are
    # client-controlled, so a caller can fire this without ever opening the
    # dialog the gate protects.
    if (!admin_authorized(session$userData$admin_unlocked)) {
      warning("[admin auth] save_api_keys fired without an unlocked session; refusing",
              call. = FALSE)
      removeModal()
      showNotification("Admin password required to change API keys.",
                       type = "error", duration = 6)
      return()
    }

    # Write through the same constants config.R reads, so the two cannot
    # resolve the same relative path against different working directories.
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      showNotification("jsonlite package required. Install with: install.packages('jsonlite')", type = "error")
      return()
    }
    # Use JSON format to avoid R code injection via source(). F6: a blank
    # secret field means "keep the stored value" - the modal never pre-fills
    # secrets, so an untouched field must not erase them.
    stored <- if (exists("API_KEYS", envir = .GlobalEnv) && is.environment(API_KEYS)) API_KEYS else list()
    keys_list <- merge_api_key_submission(
      stored,
      username = input$api_key_algaebase_user,
      password = input$api_key_algaebase_pass,
      freshwater_key = input$api_key_freshwater
    )
    write_api_keys_json(keys_list, API_KEYS_JSON)

    # Remove old vulnerable .R format if it exists
    old_file <- API_KEYS_FILE
    if (file.exists(old_file)) {
      file.remove(old_file)
      message("Removed legacy config/api_keys.R (replaced by config/api_keys.json)")
    }

    # Update in-memory API_KEYS, only the fields that changed. The env is a
    # process-wide reference type, so direct [[<- mutates in place; <<- was
    # unsafe because it could touch whichever frame bound the symbol first.
    if (is.environment(stored)) {
      for (field in names(keys_list)) {
        if (!identical(stored[[field]], keys_list[[field]])) stored[[field]] <- keys_list[[field]]
      }
    }

    removeModal()
    showNotification("API keys saved successfully!", type = "message")
  })
}
