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
}
