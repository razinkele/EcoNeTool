# =============================================================================
# SAFE RENDERING HELPERS (F3)
# =============================================================================
# At file scope so they are unit-testable without a session. EcoBase fields
# are third-party text: they only ever reach the page through htmltools tag
# builders, which escape them. Never paste them into HTML().

#' Connection-failure message for the EcoBase status panel
#' @param msg Error text (escaped by the tag builder).
ecobase_connection_error_ui <- function(msg) {
  tagList(
    tags$p(style = "color: red;", tags$i(class = "fa fa-times"), " Connection failed: ", msg),
    tags$p(tags$small("Required packages: RCurl, XML, plyr, dplyr"))
  )
}

#' EcoBase model metadata preview
#' @param meta List from extract_ecobase_metadata().
#' @param model_id,model_name Values from the model list table.
#' @return A tagList; all metadata is escaped by construction.
ecobase_metadata_panel <- function(meta, model_id, model_name) {
  has_value <- function(field) {
    !is.null(field) && length(field) > 0 && !all(is.na(field)) && field[1] != "" && field[1] != -9999
  }
  fmt <- function(val) meta_text(val, missing = c("", "Not affiliated"))

  location_parts <- character(0)
  if (has_value(meta$ecosystem_name)) {
    location_parts <- c(location_parts, as.character(meta$ecosystem_name[1]))
  } else if (has_value(meta$model_name)) {
    location_parts <- c(location_parts, as.character(meta$model_name[1]))
  }
  if (has_value(meta$region)) {
    location_parts <- c(location_parts, as.character(meta$region[1]))
  }
  if (has_value(meta$country) && meta$country[1] != "Not affiliated") {
    location_parts <- c(location_parts, as.character(meta$country[1]))
  }
  location_text <- if (length(location_parts) > 0) paste(location_parts, collapse = ", ") else fmt(NA)

  time_period_text <- fmt(NA)
  if (has_value(meta$model_year)) {
    time_period_text <- as.character(meta$model_year[1])
  } else if (has_value(meta$model_period)) {
    time_period_text <- as.character(meta$model_period[1])
  }

  coords_text <- fmt(NA)
  lat <- if (has_value(meta$latitude)) suppressWarnings(as.numeric(meta$latitude[1])) else NA_real_
  lon <- if (has_value(meta$longitude)) suppressWarnings(as.numeric(meta$longitude[1])) else NA_real_
  if (!is.na(lat) && !is.na(lon)) {
    coords_text <- sprintf("%.2f deg N, %.2f deg E", lat, lon)
  }

  area_text <- fmt(meta$area)
  area_num <- if (has_value(meta$area)) suppressWarnings(as.numeric(meta$area[1])) else NA_real_
  if (!is.na(area_num) && area_num > 0) {
    area_text <- paste0(meta$area[1], " km2")
  }

  tagList(
    tags$h5(style = "margin-top: 0;", paste0("EcoBase Model #", model_id)),
    tags$p(style = "font-size: 11px; color: #888;", as.character(model_name)),
    meta_description(meta$description),
    tags$hr(style = "margin: 10px 0;"),
    tags$table(
      style = "width: 100%; font-size: 12px;",
      meta_section_row("GEOGRAPHIC"),
      meta_row("Location:", location_text, label_style = "padding: 2px 0; width: 35%;"),
      meta_row("Ecosystem Type:", fmt(meta$ecosystem_type)),
      meta_row("Area:", area_text),
      meta_row("Coordinates:", coords_text),
      meta_section_row("TEMPORAL"),
      meta_row("Time Period:", time_period_text),
      meta_section_row("ATTRIBUTION"),
      meta_row("Author:", fmt(meta$author)),
      meta_row("Contact:", fmt(meta$contact)),
      if (has_value(meta$institution)) {
        meta_row("Institution:", as.character(meta$institution[1]), value_style = "font-size: 11px;")
      },
      meta_publication_row(
        doi = if (has_value(meta$doi)) meta$doi,
        ref = if (has_value(meta$publication)) meta$publication,
        ref_label = "Publication:"
      )
    ),
    tags$hr(style = "margin: 10px 0;"),
    tags$p(style = "font-size: 12px;",
           "Select parameter type and click 'Import Model' to load into EcoNeTool.")
  )
}

#' EcoBase Connection Server Module
#'
#' Handles connecting to EcoBase, browsing models, viewing model details
#' with metadata preview, and importing models into EcoNeTool format.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param net_reactive ReactiveVal holding the igraph network
#' @param info_reactive ReactiveVal holding the species info data frame
#' @param metaweb_metadata ReactiveVal holding metadata for dashboard
#' @param dashboard_trigger ReactiveVal to trigger dashboard updates
#' @param refresh_data_editor Function to refresh data editor tables
ecobase_server <- function(input, output, session, net_reactive, info_reactive,
                            metaweb_metadata, dashboard_trigger, refresh_data_editor) {

  # Reactive values for EcoBase
  ecobase_models <- reactiveVal(NULL)
  selected_model_id <- reactiveVal(NULL)

  # Load model list from EcoBase
  observeEvent(input$load_ecobase_models, {
    tryCatch({
      output$ecobase_connection_status <- renderUI({
        HTML("<p style='color: blue;'><i class='fa fa-spinner fa-spin'></i> Connecting to EcoBase...</p>")
      })

      models <- get_ecobase_models()
      ecobase_models(models)

      output$ecobase_models_table <- renderDT({
        all_cols <- colnames(models)

        id_col <- grep("model.*number|^id$|modelid", all_cols, ignore.case = TRUE, value = TRUE)[1]
        if (is.na(id_col)) id_col <- all_cols[1]

        name_col <- grep("model.*name|modelname", all_cols, ignore.case = TRUE, value = TRUE)[1]
        if (is.na(name_col)) name_col <- all_cols[2]

        eco_col <- grep("ecosystem", all_cols, ignore.case = TRUE, value = TRUE)[1]
        if (is.na(eco_col)) eco_col <- all_cols[3]

        year_col <- grep("year", all_cols, ignore.case = TRUE, value = TRUE)[1]
        if (is.na(year_col)) year_col <- all_cols[4]

        display_cols <- c(id_col, name_col, eco_col, year_col)
        display_cols <- display_cols[!is.na(display_cols)]

        models_display <- models[, display_cols, drop = FALSE]
        colnames(models_display) <- c("ID", "Model Name", "Ecosystem", "Year")[1:length(display_cols)]

        datatable(
          models_display,
          selection = 'single',
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            dom = 'ftp'
          ),
          rownames = FALSE
        )
      })

      output$ecobase_connection_status <- renderUI({
        HTML(paste0("<p style='color: green;'><i class='fa fa-check'></i> ",
                   "Connected! Found ", nrow(models), " models.</p>"))
      })

    }, error = function(e) {
      warning(sprintf("[ecobase] loading the model list failed: %s", conditionMessage(e)),
              call. = FALSE)
      # F3: the error text can carry server-supplied content; render it as text.
      err_msg <- conditionMessage(e)
      output$ecobase_connection_status <- renderUI({
        ecobase_connection_error_ui(err_msg)
      })
    })
  })

  # Show model details when selected
  observeEvent(input$ecobase_models_table_rows_selected, {
    req(input$ecobase_models_table_rows_selected)
    req(ecobase_models())

    row_selected <- input$ecobase_models_table_rows_selected
    models <- ecobase_models()
    all_cols <- colnames(models)

    id_col <- grep("model.*number|^id$|modelid", all_cols, ignore.case = TRUE, value = TRUE)[1]
    model_id <- if (!is.na(id_col)) models[[id_col]][row_selected] else row_selected

    name_col <- grep("model.*name|modelname", all_cols, ignore.case = TRUE, value = TRUE)[1]
    model_name <- if (!is.na(name_col)) models[[name_col]][row_selected] else "Unknown"

    eco_col <- grep("ecosystem", all_cols, ignore.case = TRUE, value = TRUE)[1]
    ecosystem <- if (!is.na(eco_col)) models[[eco_col]][row_selected] else "Unknown"

    year_col <- grep("year", all_cols, ignore.case = TRUE, value = TRUE)[1]
    year <- if (!is.na(year_col)) models[[year_col]][row_selected] else "Unknown"

    country_col <- grep("country", all_cols, ignore.case = TRUE, value = TRUE)[1]
    country <- if (!is.na(country_col)) models[[country_col]][row_selected] else "Unknown"

    selected_model_id(model_id)

    output$ecobase_model_details <- renderUI({
      meta <- tryCatch({
        extract_ecobase_metadata(model_id)
      }, error = function(e) {
        warning(sprintf("[ecobase] metadata for model %s unavailable: %s",
                        model_id, conditionMessage(e)), call. = FALSE)
        NULL
      })

      if (!is.null(meta)) {
        # F3: every EcoBase field is rendered as escaped text via tag builders.
        ecobase_metadata_panel(meta, model_id, model_name)
      } else {
        tagList(
          h4(model_name),
          tags$table(
            class = "table table-sm",
            tags$tr(tags$td(tags$strong("Model ID:")), tags$td(model_id)),
            tags$tr(tags$td(tags$strong("Ecosystem:")), tags$td(ecosystem)),
            tags$tr(tags$td(tags$strong("Year:")), tags$td(year)),
            tags$tr(tags$td(tags$strong("Country:")), tags$td(country))
          ),
          br(),
          HTML("<p style='font-size: 12px; color: #999;'>Could not load full metadata. Select parameter type and click 'Import Model' to load.</p>")
        )
      }
    })
  })

  # Import selected model
  observeEvent(input$import_ecobase_model, {
    req(selected_model_id())

    model_id <- selected_model_id()
    param_type <- input$ecobase_parameter_type

    tryCatch({
      output$ecobase_import_status <- renderPrint({
        cat("Importing model", model_id, "from EcoBase...\n")
        cat("Parameter type:", switch(param_type,
          "hybrid" = "Hybrid (Best of Both)",
          "output" = "Output (Balanced)",
          "input" = "Input (Original)"
        ), "\n\n")
      })

      result <- if (param_type == "hybrid") {
        convert_ecobase_to_econetool_hybrid(model_id)
      } else {
        use_output <- (param_type == "output")
        convert_ecobase_to_econetool(model_id, use_output = use_output)
      }

      ecobase_net <- result$net
      ecobase_info <- result$info

      ecobase_net <- igraph::upgrade_graph(ecobase_net)

      # finalize_network() aligns info to V(net), canonicalises fg, derives
      # colfg and estimates met.types, bodymasses and efficiencies by fg
      # (EcoBase carries none of them).
      finalized <- finalize_network(ecobase_net, ecobase_info)
      ecobase_net <- finalized$net
      ecobase_info <- finalized$info

      net_reactive(ecobase_net)
      info_reactive(ecobase_info)

      refresh_data_editor()

      location_text <- paste0("EcoBase Model #", model_id)
      time_period_text <- "EcoBase Import"

      if (!is.null(result$metadata)) {
        meta <- result$metadata

        has_val <- function(field) {
          !is.null(field) && length(field) > 0 && !all(is.na(field)) && field[1] != "" && field[1] != -9999
        }

        location_parts <- c()
        if (has_val(meta$model_name)) {
          location_parts <- c(location_parts, meta$model_name)
        }
        if (has_val(meta$country) && meta$country != "Not affiliated") {
          location_parts <- c(location_parts, meta$country)
        }
        if (has_val(meta$ecosystem_type)) {
          location_parts <- c(location_parts, paste0("(", meta$ecosystem_type, ")"))
        }
        if (length(location_parts) > 0) {
          location_text <- paste(location_parts, collapse = ", ")
        }

        if (has_val(meta$model_year) && meta$model_year != "0") {
          time_period_text <- as.character(meta$model_year)
        }
      }

      metaweb_metadata(list(
        location = location_text,
        time_period = time_period_text,
        source = paste0("EcoBase #", model_id)
      ))

      dashboard_trigger(dashboard_trigger() + 1)

      output$ecobase_import_status <- renderPrint({
        cat("SUCCESS: EcoBase model imported!\n\n")
        cat("Model ID:", model_id, "\n")
        cat("Parameter type:", switch(param_type,
          "hybrid" = "Hybrid (Best of Both)",
          "output" = "Output (Balanced)",
          "input" = "Input (Original)"
        ), "\n\n")
        cat("Conversion complete:\n")
        cat("  - Species/groups:", vcount(net_reactive()), "\n")
        cat("  - Trophic links:", ecount(net_reactive()), "\n")
        cat("  - Functional groups:", nlevels(info_reactive()$fg), "\n")

        if (param_type == "hybrid") {
          cat("  - Parameters from: OUTPUT (balanced)\n")
          cat("  - Diet links from: INPUT (complete)\n\n")
        } else {
          cat("\n")
        }

        if (ecount(net_reactive()) == 0 && param_type != "hybrid") {
          cat("WARNING: No trophic links imported!\n")
          cat("This model may not have diet data in",
              switch(param_type, "output" = "Output", "input" = "Input"), "parameters.\n")
          cat("Try importing with 'Hybrid (Best of Both)' instead.\n\n")
        }

        cat("Navigate to other tabs to explore the EcoBase model.\n")
      })

      showNotification(
        paste0("Model #", model_id, " imported successfully!"),
        type = "message",
        duration = 5
      )

    }, error = function(e) {
      output$ecobase_import_status <- renderPrint({
        cat("ERROR importing EcoBase model:\n\n")
        cat(e$message, "\n\n")
        cat("Check console for details.\n")
      })

      showNotification(
        paste0("Import failed: ", e$message),
        type = "error",
        duration = 10
      )
    })
  })
}
