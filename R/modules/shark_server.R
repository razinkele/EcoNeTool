#' SHARK4R Server Module
#'
#' Swedish ocean archives integration for marine environmental data.
#' Handles taxonomy search (Dyntaxa, WoRMS, AlgaeBase), environmental data
#' queries, species occurrence data, and quality control checks.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
shark_server <- function(input, output, session) {

  # Initialize reactive values for SHARK data
  shark_data <- reactiveValues(
    taxonomy_results = NULL,
    environmental_data = NULL,
    occurrence_data = NULL,
    qc_results = NULL
  )

  # ---------------------------------------------------------------------------
  # TAB 1: Taxonomy Search
  # ---------------------------------------------------------------------------

  observeEvent(input$shark_search_taxonomy, {
    req(input$shark_species_name)
    req(length(input$shark_taxonomy_sources) > 0)

    species_name <- trimws(input$shark_species_name)

    withProgress(message = 'Querying taxonomic databases...', value = 0, {
      results <- list()
      sources <- input$shark_taxonomy_sources
      n_sources <- length(sources)

      if ("dyntaxa" %in% sources) {
        incProgress(1/n_sources, detail = "Querying Dyntaxa...")
        results$dyntaxa <- query_dyntaxa(species_name,
                                        fuzzy = input$shark_fuzzy_search)
      }

      if ("worms" %in% sources) {
        incProgress(1/n_sources, detail = "Querying WoRMS...")
        results$worms <- query_shark_worms(species_name,
                                          fuzzy = input$shark_fuzzy_search)
      }

      if ("algaebase" %in% sources) {
        incProgress(1/n_sources, detail = "Querying AlgaeBase...")
        results$algaebase <- query_algaebase(species_name)
      }

      shark_data$taxonomy_results <- results
      incProgress(1, detail = "Complete!")
    })
  })

  # Render taxonomy results
  output$shark_taxonomy_results <- renderUI({
    req(shark_data$taxonomy_results)

    results <- shark_data$taxonomy_results

    if (length(results) == 0) {
      return(tags$div(
        class = "alert alert-warning",
        icon("exclamation-triangle"),
        " No results found. Try a different species name or enable more data sources."
      ))
    }

    result_boxes <- lapply(names(results), function(source_name) {
      result <- results[[source_name]]

      if (is.null(result)) {
        return(tags$div(
          class = "alert alert-secondary",
          style = "margin-bottom: 15px;",
          tags$h5(tags$strong(toupper(source_name))),
          tags$p(icon("times-circle"), " No results found in this database")
        ))
      }

      if (source_name == "dyntaxa") {
        tags$div(
          class = "alert alert-success",
          style = "margin-bottom: 15px;",
          tags$h5(tags$strong("DYNTAXA (Swedish Taxonomy)")),
          tags$table(
            class = "table table-sm",
            tags$tr(
              tags$td(tags$strong("Scientific Name:")),
              tags$td(result$scientific_name)
            ),
            tags$tr(
              tags$td(tags$strong("Swedish Name:")),
              tags$td(ifelse(is.na(result$swedish_name), "\u2014", result$swedish_name))
            ),
            tags$tr(
              tags$td(tags$strong("Taxon ID:")),
              tags$td(result$taxon_id)
            ),
            tags$tr(
              tags$td(tags$strong("Class:")),
              tags$td(ifelse(is.na(result$class), "\u2014", result$class))
            ),
            tags$tr(
              tags$td(tags$strong("Family:")),
              tags$td(ifelse(is.na(result$family), "\u2014", result$family))
            ),
            tags$tr(
              tags$td(tags$strong("Author:")),
              tags$td(ifelse(is.na(result$author), "\u2014", result$author))
            )
          )
        )
      } else if (source_name == "worms") {
        tags$div(
          class = "alert alert-info",
          style = "margin-bottom: 15px;",
          tags$h5(tags$strong("WoRMS (World Register of Marine Species)")),
          tags$table(
            class = "table table-sm",
            tags$tr(
              tags$td(tags$strong("Scientific Name:")),
              tags$td(result$scientific_name)
            ),
            tags$tr(
              tags$td(tags$strong("AphiaID:")),
              tags$td(tags$a(href = paste0("https://www.marinespecies.org/aphia.php?p=taxdetails&id=", result$aphia_id),
                            target = "_blank", result$aphia_id))
            ),
            tags$tr(
              tags$td(tags$strong("Authority:")),
              tags$td(ifelse(is.na(result$authority), "\u2014", result$authority))
            ),
            tags$tr(
              tags$td(tags$strong("Status:")),
              tags$td(ifelse(is.na(result$status), "\u2014", result$status))
            ),
            tags$tr(
              tags$td(tags$strong("Class:")),
              tags$td(ifelse(is.na(result$class), "\u2014", result$class))
            ),
            tags$tr(
              tags$td(tags$strong("Family:")),
              tags$td(ifelse(is.na(result$family), "\u2014", result$family))
            )
          )
        )
      } else if (source_name == "algaebase") {
        tags$div(
          class = "alert alert-primary",
          style = "margin-bottom: 15px;",
          tags$h5(tags$strong("ALGAEBASE (Algae Database)")),
          tags$table(
            class = "table table-sm",
            tags$tr(
              tags$td(tags$strong("Scientific Name:")),
              tags$td(result$scientific_name)
            ),
            tags$tr(
              tags$td(tags$strong("AlgaeBase ID:")),
              tags$td(result$algaebase_id)
            ),
            tags$tr(
              tags$td(tags$strong("Authority:")),
              tags$td(ifelse(is.na(result$authority), "\u2014", result$authority))
            ),
            tags$tr(
              tags$td(tags$strong("Phylum:")),
              tags$td(ifelse(is.na(result$phylum), "\u2014", result$phylum))
            ),
            tags$tr(
              tags$td(tags$strong("Class:")),
              tags$td(ifelse(is.na(result$class), "\u2014", result$class))
            )
          )
        )
      }
    })

    do.call(tagList, result_boxes)
  })

  # ---------------------------------------------------------------------------
  # TAB 2: Environmental Data
  # ---------------------------------------------------------------------------

  observeEvent(input$shark_query_environmental, {
    req(input$shark_date_range)
    req(length(input$shark_parameters) > 0)

    bbox <- NULL
    if (!is.null(input$shark_bbox_north) && !is.null(input$shark_bbox_south) &&
        !is.null(input$shark_bbox_east) && !is.null(input$shark_bbox_west)) {
      bbox <- c(
        north = input$shark_bbox_north,
        south = input$shark_bbox_south,
        east = input$shark_bbox_east,
        west = input$shark_bbox_west
      )
    }

    withProgress(message = 'Retrieving environmental data from SHARK...', value = 0.5, {
      data <- get_shark_environmental_data(
        parameters = input$shark_parameters,
        start_date = input$shark_date_range[1],
        end_date = input$shark_date_range[2],
        bbox = bbox,
        max_records = input$shark_max_env_records
      )

      if (!is.null(data)) {
        shark_data$environmental_data <- format_shark_results(data, "environmental")
      } else {
        shark_data$environmental_data <- NULL
      }

      incProgress(1)
    })
  })

  output$shark_environmental_status <- renderUI({
    if (is.null(shark_data$environmental_data)) {
      return(tags$div(
        class = "alert alert-info",
        icon("info-circle"),
        " No data retrieved yet. Configure query parameters and click 'Query Data'."
      ))
    } else {
      return(tags$div(
        class = "alert alert-success",
        icon("check-circle"),
        sprintf(" Retrieved %d environmental records", nrow(shark_data$environmental_data))
      ))
    }
  })

  output$shark_environmental_table <- renderDT({
    req(shark_data$environmental_data)

    datatable(
      shark_data$environmental_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'Bfrtip'
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    )
  })

  output$shark_download_environmental <- downloadHandler(
    filename = function() {
      paste0("shark_environmental_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(shark_data$environmental_data)
      write.csv(shark_data$environmental_data, file, row.names = FALSE)
    }
  )

  # ---------------------------------------------------------------------------
  # TAB 3: Species Occurrence
  # ---------------------------------------------------------------------------

  observeEvent(input$shark_query_occurrence, {
    req(input$shark_occurrence_species)
    req(input$shark_occurrence_dates)

    species_name <- trimws(input$shark_occurrence_species)

    withProgress(message = 'Retrieving species occurrence data...', value = 0.5, {
      data <- get_shark_species_occurrence(
        species_name = species_name,
        start_date = input$shark_occurrence_dates[1],
        end_date = input$shark_occurrence_dates[2],
        max_records = input$shark_max_occ_records
      )

      if (!is.null(data)) {
        shark_data$occurrence_data <- format_shark_results(data, "occurrence")
      } else {
        shark_data$occurrence_data <- NULL
      }

      incProgress(1)
    })
  })

  output$shark_occurrence_status <- renderUI({
    if (is.null(shark_data$occurrence_data)) {
      return(tags$div(
        class = "alert alert-info",
        icon("info-circle"),
        " No occurrence data retrieved yet. Enter a species name and click 'Get Occurrences'."
      ))
    } else {
      return(tags$div(
        class = "alert alert-success",
        icon("check-circle"),
        sprintf(" Retrieved %d occurrence records", nrow(shark_data$occurrence_data))
      ))
    }
  })

  output$shark_occurrence_table <- renderDT({
    req(shark_data$occurrence_data)

    datatable(
      shark_data$occurrence_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    )
  })

  output$shark_occurrence_map <- renderLeaflet({
    req(shark_data$occurrence_data)
    req("Lat" %in% colnames(shark_data$occurrence_data))
    req("Lon" %in% colnames(shark_data$occurrence_data))

    data_map <- shark_data$occurrence_data[
      !is.na(shark_data$occurrence_data$Lat) &
      !is.na(shark_data$occurrence_data$Lon), ]

    if (nrow(data_map) == 0) {
      return(leaflet() %>%
              addTiles() %>%
              setView(lng = 18, lat = 59, zoom = 5))
    }

    leaflet(data_map) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Lon,
        lat = ~Lat,
        popup = ~paste0("<strong>", Species, "</strong><br>",
                       "Date: ", Date, "<br>",
                       "Abundance: ", Abundance, " ", Unit),
        radius = 5,
        color = "#007bff",
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 1
      ) %>%
      fitBounds(
        lng1 = min(data_map$Lon, na.rm = TRUE),
        lat1 = min(data_map$Lat, na.rm = TRUE),
        lng2 = max(data_map$Lon, na.rm = TRUE),
        lat2 = max(data_map$Lat, na.rm = TRUE)
      )
  })

  output$shark_download_occurrence <- downloadHandler(
    filename = function() {
      paste0("shark_occurrence_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(shark_data$occurrence_data)
      write.csv(shark_data$occurrence_data, file, row.names = FALSE)
    }
  )

  # ---------------------------------------------------------------------------
  # TAB 4: Quality Control
  # ---------------------------------------------------------------------------

  observeEvent(input$shark_run_qc, {
    req(input$shark_qc_file)

    withProgress(message = 'Running quality control checks...', value = 0, {
      incProgress(0.2, detail = "Reading file...")
      data <- tryCatch({
        read.csv(input$shark_qc_file$datapath, stringsAsFactors = FALSE)
      }, error = function(e) {
        NULL
      })

      if (is.null(data)) {
        shark_data$qc_results <- list(
          error = TRUE,
          message = "Failed to read file. Please check file format."
        )
        return()
      }

      incProgress(0.3, detail = "Validating format...")
      validation <- if ("format" %in% input$shark_qc_checks) {
        validate_shark_data(data)
      } else {
        list(valid = TRUE, message = "Format validation skipped")
      }

      incProgress(0.3, detail = "Checking quality...")
      quality <- if ("completeness" %in% input$shark_qc_checks) {
        check_data_quality(data)
      } else {
        list(message = "Quality check skipped")
      }

      shark_data$qc_results <- list(
        error = FALSE,
        validation = validation,
        quality = quality,
        data_summary = list(
          rows = nrow(data),
          columns = ncol(data),
          column_names = colnames(data)
        )
      )

      incProgress(1, detail = "Complete!")
    })
  })

  output$shark_qc_status <- renderUI({
    if (is.null(shark_data$qc_results)) {
      return(tags$div(
        class = "alert alert-info",
        icon("info-circle"),
        " No quality control run yet. Upload a SHARK format file and click 'Run Quality Control'."
      ))
    }

    if (shark_data$qc_results$error) {
      return(tags$div(
        class = "alert alert-danger",
        icon("exclamation-triangle"),
        paste(" Error:", shark_data$qc_results$message)
      ))
    }

    return(tags$div(
      class = "alert alert-success",
      icon("check-circle"),
      sprintf(" Quality control completed - %d rows, %d columns analyzed",
              shark_data$qc_results$data_summary$rows,
              shark_data$qc_results$data_summary$columns)
    ))
  })

  output$shark_qc_results <- renderPrint({
    req(shark_data$qc_results)
    req(!shark_data$qc_results$error)

    cat("================================================================================\n")
    cat("SHARK DATA QUALITY CONTROL REPORT\n")
    cat("================================================================================\n\n")

    cat("DATA SUMMARY\n")
    cat("------------\n")
    cat("Rows:", shark_data$qc_results$data_summary$rows, "\n")
    cat("Columns:", shark_data$qc_results$data_summary$columns, "\n")
    cat("Columns:", paste(shark_data$qc_results$data_summary$column_names, collapse = ", "), "\n\n")

    if (!is.null(shark_data$qc_results$validation)) {
      cat("VALIDATION RESULTS\n")
      cat("------------------\n")
      if (shark_data$qc_results$validation$valid) {
        cat("Format validation: PASSED\n")
      } else {
        cat("Format validation: FAILED\n")
        cat("Message:", shark_data$qc_results$validation$message, "\n")
      }
      cat("\n")
    }

    if (!is.null(shark_data$qc_results$quality)) {
      cat("QUALITY CHECK RESULTS\n")
      cat("---------------------\n")
      if (!is.null(shark_data$qc_results$quality$completeness)) {
        cat(sprintf("Data completeness: %.1f%%\n", shark_data$qc_results$quality$completeness))
      }
      if (!is.null(shark_data$qc_results$quality$record_count)) {
        cat("Record count:", shark_data$qc_results$quality$record_count, "\n")
      }
      cat("\n")
    }

    cat("================================================================================\n")
  })

  output$shark_qc_warnings <- renderUI({
    req(shark_data$qc_results)
    req(!shark_data$qc_results$error)

    warnings <- list()

    if (!is.null(shark_data$qc_results$validation) &&
        !is.null(shark_data$qc_results$validation$warnings) &&
        length(shark_data$qc_results$validation$warnings) > 0) {
      warnings <- c(warnings, shark_data$qc_results$validation$warnings)
    }

    if (length(warnings) == 0) {
      return(tags$div(
        class = "alert alert-success",
        icon("check"), " No warnings detected"
      ))
    }

    warning_items <- lapply(warnings, function(w) {
      tags$li(w)
    })

    tags$div(
      class = "alert alert-warning",
      tags$h5(icon("exclamation-triangle"), " Warnings:"),
      tags$ul(warning_items)
    )
  })

  output$shark_qc_summary <- renderUI({
    req(shark_data$qc_results)
    req(!shark_data$qc_results$error)

    if (!is.null(shark_data$qc_results$quality) &&
        !is.null(shark_data$qc_results$quality$completeness)) {
      completeness <- shark_data$qc_results$quality$completeness

      status_class <- if (completeness >= 95) {
        "success"
      } else if (completeness >= 80) {
        "warning"
      } else {
        "danger"
      }

      tags$div(
        class = paste("alert alert-", status_class, sep = ""),
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
    }
  })
}
