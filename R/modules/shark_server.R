#' SHARK4R Server Module
#'
#' Swedish ocean archives integration (SHARK4R >= 1.2.0): taxonomy search
#' (WoRMS; Dyntaxa and AlgaeBase when their keys are set), physical-chemical
#' data, species records from SHARK, and quality control of SHARK files.
#' Every third-party value is rendered through tag builders or escaped.

# ---------------------------------------------------------------------------
# Pure render helpers (tested directly in test-shark-rewire.R)
# ---------------------------------------------------------------------------

SHARK_SOURCE_TITLES <- c(
  worms = "WoRMS (World Register of Marine Species)",
  dyntaxa = "DYNTAXA (Swedish Taxonomy)",
  algaebase = "ALGAEBASE (Algae Database)"
)

SHARK_SOURCE_CLASSES <- c(worms = "alert alert-info", dyntaxa = "alert alert-success",
                          algaebase = "alert alert-primary")

.shark_value <- function(x) {
  v <- .scalar_chr(x)
  if (is.na(v)) "—" else v
}

.shark_row <- function(label, value) {
  tags$tr(tags$td(tags$strong(label)), tags$td(value))
}

.shark_aphia_link <- function(aphia_id) {
  id <- suppressWarnings(as.integer(.scalar_chr(aphia_id)))
  if (is.na(id)) return("—")
  tags$a(href = paste0("https://www.marinespecies.org/aphia.php?p=taxdetails&id=", id),
         target = "_blank", rel = "noopener noreferrer", as.character(id))
}

#' One result card of the taxonomy search
#'
#' @param source_name "worms", "dyntaxa" or "algaebase".
#' @param result A query_shark_worms() / query_dyntaxa() / query_algaebase() result.
#' @return A shiny tag.
shark_taxonomy_card <- function(source_name, result) {
  title <- tags$h5(tags$strong(SHARK_SOURCE_TITLES[[source_name]]))
  status <- .scalar_chr(result$status)
  if (!identical(status, "found")) {
    text <- switch(
      if (is.na(status)) "error" else status,
      not_found = "No results found in this database",
      no_key = .shark_value(result$message),
      paste("Lookup failed:", .shark_value(result$message))
    )
    return(tags$div(
      class = if (identical(status, "not_found")) "alert alert-secondary" else "alert alert-warning",
      style = "margin-bottom: 15px;",
      title,
      tags$p(icon(if (identical(status, "not_found")) "times-circle" else "exclamation-triangle"), " ", text)
    ))
  }
  rows <- switch(
    source_name,
    worms = list(
      .shark_row("Scientific Name:", .shark_value(result$scientific_name)),
      .shark_row("AphiaID:", .shark_aphia_link(result$aphia_id)),
      .shark_row("Authority:", .shark_value(result$authority)),
      .shark_row("Status:", .shark_value(result$taxon_status)),
      .shark_row("Class:", .shark_value(result$class)),
      .shark_row("Family:", .shark_value(result$family))
    ),
    dyntaxa = list(
      .shark_row("Matched Name:", .shark_value(result$matched_name)),
      .shark_row("Scientific Name:", .shark_value(result$scientific_name)),
      .shark_row("Taxon ID:", .shark_value(result$taxon_id)),
      .shark_row("Author:", .shark_value(result$author))
    ),
    algaebase = list(
      .shark_row("Scientific Name:", .shark_value(result$scientific_name)),
      .shark_row("AlgaeBase ID:", .shark_value(result$algaebase_id)),
      .shark_row("Authority:", .shark_value(result$authority)),
      .shark_row("Phylum:", .shark_value(result$phylum)),
      .shark_row("Class:", .shark_value(result$class))
    )
  )
  tags$div(class = SHARK_SOURCE_CLASSES[[source_name]], style = "margin-bottom: 15px;", title,
           tags$table(class = "table table-sm", rows))
}

#' Status line for a data query result
#'
#' @param res NULL (no query yet) or a get_shark_*() result.
#' @param idle_text Text shown before the first query.
#' @return A shiny tag.
shark_query_status <- function(res, idle_text) {
  if (is.null(res)) {
    return(tags$div(class = "alert alert-info", icon("info-circle"), " ", idle_text))
  }
  switch(
    .shark_value(res$status),
    ok = tags$div(class = "alert alert-success", icon("check-circle"), " ", .shark_value(res$message)),
    empty = tags$div(class = "alert alert-warning", icon("exclamation-triangle"), " ", .shark_value(res$message)),
    tags$div(class = "alert alert-danger", icon("exclamation-triangle"), " ", .shark_value(res$message))
  )
}

#' Leaflet popups for occurrence rows, every field HTML-escaped
#'
#' @param d format_shark_results(..., "occurrence") rows.
#' @return Character vector of popup HTML.
shark_occurrence_popup <- function(d) {
  esc <- function(x) htmltools::htmlEscape(ifelse(is.na(x), "", as.character(x)))
  paste0("<strong>", esc(d$Species), "</strong><br>",
         "Date: ", esc(d$Date), "<br>",
         esc(d$Parameter), ": ", esc(d$Value), " ", esc(d$Unit))
}

#' SHARK4R Server Module
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
shark_server <- function(input, output, session) {
  # shark_ui() shows only an installation hint without a usable SHARK4R.
  if (!shark4r_installed()) return(invisible(NULL))

  shark_data <- reactiveValues(
    taxonomy_results = NULL,
    environmental = NULL,
    occurrence = NULL,
    qc_results = NULL
  )

  # ---------------------------------------------------------------------------
  # TAB 1: Taxonomy Search
  # ---------------------------------------------------------------------------

  observeEvent(input$shark_search_taxonomy, {
    species_name <- trimws(input$shark_species_name %||% "")
    req(nzchar(species_name))
    sources <- intersect(input$shark_taxonomy_sources, names(SHARK_SOURCE_TITLES))
    req(length(sources) > 0)
    fuzzy <- isTRUE(input$shark_fuzzy_search)

    withProgress(message = "Querying taxonomic databases...", value = 0, {
      results <- list()
      for (src in sources) {
        incProgress(1 / length(sources), detail = sprintf("Querying %s...", src))
        results[[src]] <- switch(
          src,
          worms = query_shark_worms(species_name, fuzzy = fuzzy),
          dyntaxa = query_dyntaxa(species_name, fuzzy = fuzzy),
          algaebase = query_algaebase(species_name)
        )
      }
      shark_data$taxonomy_results <- results
    })
  })

  output$shark_taxonomy_results <- renderUI({
    results <- shark_data$taxonomy_results
    req(results)
    do.call(tagList, lapply(names(results), function(src) shark_taxonomy_card(src, results[[src]])))
  })

  # ---------------------------------------------------------------------------
  # TAB 2: Environmental Data
  # ---------------------------------------------------------------------------

  observeEvent(input$shark_query_environmental, {
    req(input$shark_date_range)
    bbox <- c(
      north = input$shark_bbox_north %||% NA,
      south = input$shark_bbox_south %||% NA,
      east = input$shark_bbox_east %||% NA,
      west = input$shark_bbox_west %||% NA
    )
    withProgress(message = "Retrieving environmental data from SHARK...", value = 0.5, {
      shark_data$environmental <- get_shark_environmental_data(
        parameters = input$shark_parameters,
        start_date = input$shark_date_range[1],
        end_date = input$shark_date_range[2],
        bbox = bbox,
        max_records = input$shark_max_env_records
      )
      incProgress(0.5)
    })
  })

  env_table <- reactive({
    res <- shark_data$environmental
    req(identical(res$status, "ok"))
    format_shark_results(res$data, "environmental")
  })

  output$shark_environmental_status <- renderUI({
    shark_query_status(shark_data$environmental,
                       "No data retrieved yet. Configure query parameters and click 'Query Data'.")
  })

  output$shark_environmental_table <- renderDT({
    datatable(env_table(), options = list(pageLength = 25, scrollX = TRUE),
              rownames = FALSE, class = "cell-border stripe")
  })

  output$shark_download_environmental <- downloadHandler(
    filename = function() paste0("shark_environmental_", Sys.Date(), ".csv"),
    content = function(file) utils::write.csv(env_table(), file, row.names = FALSE)
  )

  # ---------------------------------------------------------------------------
  # TAB 3: Species Occurrence
  # ---------------------------------------------------------------------------

  observeEvent(input$shark_query_occurrence, {
    req(input$shark_occurrence_species)
    req(input$shark_occurrence_dates)
    withProgress(message = "Retrieving species records from SHARK...", value = 0.5, {
      shark_data$occurrence <- get_shark_species_occurrence(
        species_name = input$shark_occurrence_species,
        start_date = input$shark_occurrence_dates[1],
        end_date = input$shark_occurrence_dates[2],
        max_records = input$shark_max_occ_records
      )
      incProgress(0.5)
    })
  })

  occ_table <- reactive({
    res <- shark_data$occurrence
    req(identical(res$status, "ok"))
    format_shark_results(res$data, "occurrence")
  })

  output$shark_occurrence_status <- renderUI({
    shark_query_status(shark_data$occurrence,
                       "No records retrieved yet. Enter a scientific name and click 'Get Records'.")
  })

  output$shark_occurrence_table <- renderDT({
    datatable(occ_table(), options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE, class = "cell-border stripe")
  })

  output$shark_occurrence_map <- renderLeaflet({
    d <- occ_table()
    lat <- suppressWarnings(as.numeric(d$Lat))
    lon <- suppressWarnings(as.numeric(d$Lon))
    keep <- is.finite(lat) & is.finite(lon)
    if (!any(keep)) {
      return(leaflet() %>% addTiles() %>% setView(lng = 18, lat = 59, zoom = 5))
    }
    d <- d[keep, , drop = FALSE]
    d$Lat <- lat[keep]
    d$Lon <- lon[keep]
    leaflet(d) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~Lon, lat = ~Lat, popup = shark_occurrence_popup(d), radius = 5,
                       color = "#007bff", fillOpacity = 0.7, stroke = TRUE, weight = 1) %>%
      fitBounds(lng1 = min(d$Lon), lat1 = min(d$Lat), lng2 = max(d$Lon), lat2 = max(d$Lat))
  })

  output$shark_download_occurrence <- downloadHandler(
    filename = function() paste0("shark_occurrence_", Sys.Date(), ".csv"),
    content = function(file) utils::write.csv(occ_table(), file, row.names = FALSE)
  )

  # ---------------------------------------------------------------------------
  # TAB 4: Quality Control
  # ---------------------------------------------------------------------------

  observeEvent(input$shark_run_qc, {
    req(input$shark_qc_file)
    withProgress(message = "Running quality control checks...", value = 0, {
      incProgress(0.2, detail = "Reading file...")
      data <- read_shark_qc_file(input$shark_qc_file$datapath, input$shark_qc_file$name)
      if (is.null(data)) {
        shark_data$qc_results <- list(error = TRUE, message = "Failed to read file. Please check file format.")
        return()
      }
      incProgress(0.3, detail = "Checking...")
      datatype <- resolve_shark_qc_datatype(data, input$shark_qc_datatype %||% "auto")
      shark_data$qc_results <- run_shark_qc(data, input$shark_qc_checks %||% character(0), datatype)
      incProgress(0.5, detail = "Complete!")
    })
  })

  output$shark_qc_status <- renderUI({
    qc <- shark_data$qc_results
    if (is.null(qc)) {
      return(tags$div(class = "alert alert-info", icon("info-circle"),
                      " No quality control run yet. Upload a SHARK format file and click 'Run Quality Control'."))
    }
    if (isTRUE(qc$error)) {
      return(tags$div(class = "alert alert-danger", icon("exclamation-triangle"), paste(" Error:", qc$message)))
    }
    tags$div(class = "alert alert-success", icon("check-circle"),
             sprintf(" Quality control completed - %d rows, %d columns analyzed",
                     qc$data_summary$rows, qc$data_summary$columns))
  })

  output$shark_qc_results <- renderPrint({
    qc <- shark_data$qc_results
    req(qc, !isTRUE(qc$error))
    cat(format_shark_qc_report(qc), sep = "\n")
  })

  output$shark_qc_warnings <- renderUI({
    qc <- shark_data$qc_results
    req(qc, !isTRUE(qc$error))
    items <- c(qc$validation$warnings,
               if (NROW(qc$outliers$outliers) > 0) qc$outliers$message,
               if (isTRUE(qc$coordinates$zero + qc$coordinates$out_of_range > 0)) qc$coordinates$message)
    if (length(items) == 0) {
      return(tags$div(class = "alert alert-success", icon("check"), " No warnings detected"))
    }
    tags$div(class = "alert alert-warning",
             tags$h5(icon("exclamation-triangle"), " Warnings:"),
             tags$ul(lapply(items, tags$li)))
  })

  output$shark_qc_summary <- renderUI({
    qc <- shark_data$qc_results
    req(qc, !isTRUE(qc$error))
    completeness <- qc$quality$completeness
    req(isTRUE(is.finite(completeness)))
    status_class <- if (completeness >= 95) "success" else if (completeness >= 80) "warning" else "danger"
    tags$div(
      class = paste0("alert alert-", status_class),
      tags$h5("Data Quality Summary:"),
      tags$p(sprintf("Overall completeness: %.1f%%", completeness)),
      tags$p(
        if (completeness >= 95) {
          "Data quality is excellent."
        } else if (completeness >= 80) {
          "Data quality is acceptable but some values are missing."
        } else {
          "Data quality needs improvement. Significant missing values detected."
        }
      )
    )
  })
}
