#' Data Import Handler Server Module
#'
#' Handles file upload and data loading for RData, Excel, and CSV formats.
#' Updates net_reactive and info_reactive when new data is loaded.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param net_reactive ReactiveVal holding the igraph network
#' @param info_reactive ReactiveVal holding the species info data frame
#' @param dashboard_trigger ReactiveVal to trigger dashboard updates
#' @param refresh_data_editor Function to refresh data editor tables
data_import_server <- function(input, output, session, net_reactive, info_reactive,
                                dashboard_trigger, refresh_data_editor) {

  # Output status message for data upload
  output$data_upload_status <- renderPrint({
    if (is.null(input$data_file)) {
      cat("No file uploaded yet.\n\n")
      cat("Current dataset: Gulf of Riga (default)\n")
      cat("  - 34 species\n")
      cat("  - 207 trophic links\n")
      cat("  - 5 functional groups\n")
    } else {
      cat("File selected:", input$data_file$name, "\n")
      cat("File size:", round(input$data_file$size / 1024, 2), "KB\n\n")
      cat("Click 'Load Data' button to import.\n")
    }
  })

  # Handle file upload when button clicked
  observeEvent(input$load_data, {
    req(input$data_file)

    tryCatch({
      file_path <- input$data_file$datapath
      file_ext <- tools::file_ext(input$data_file$name)

      # Update status
      output$data_upload_status <- renderPrint({
        cat("Processing file:", input$data_file$name, "\n")
        cat("Format:", toupper(file_ext), "\n\n")
        cat("Loading...")
      })

      # Load based on file type
      if (file_ext %in% c("Rdata", "rda")) {
        # Load RData file into separate environment
        # (to avoid overwriting app functions if RData contains them)
        env <- new.env()
        load(file_path, envir = env)

        # Validate required objects
        if (!exists("net", envir = env)) {
          stop("RData file must contain 'net' object (igraph network)")
        }
        if (!exists("info", envir = env)) {
          stop("RData file must contain 'info' data frame")
        }

        # Extract only data objects (not functions) from loaded environment
        # This ensures we use the app's function definitions, not ones from the RData file
        loaded_net <- env$net
        loaded_info <- env$info

        # Upgrade igraph if needed
        loaded_net <- igraph::upgrade_graph(loaded_net)

        # Ensure vertex names are properly set
        # If vertex names are missing or numeric, try to get them from info$species or rownames
        if (is.null(igraph::V(loaded_net)$name) || all(grepl("^[0-9]+$", igraph::V(loaded_net)$name))) {
          if ("species" %in% colnames(loaded_info)) {
            igraph::V(loaded_net)$name <- as.character(loaded_info$species)
          } else if (!is.null(rownames(loaded_info))) {
            igraph::V(loaded_net)$name <- rownames(loaded_info)
          }
        }

        # Assign colors by matching functional group names to COLOR_SCHEME
        fg_levels <- get_functional_group_levels()
        loaded_info$colfg <- sapply(as.character(loaded_info$fg), function(fg) {
          idx <- which(fg_levels == fg)
          if (length(idx) == 0) return("gray")
          COLOR_SCHEME[idx]
        })

        # Update reactive values for dashboard
        net_reactive(loaded_net)
        info_reactive(loaded_info)

        # Refresh data editor tables
        refresh_data_editor()

        output$data_upload_status <- renderPrint({
          cat("SUCCESS: Data loaded!\n\n")
          cat("Network: ", vcount(net_reactive()), "species,", ecount(net_reactive()), "links\n")
          cat("Species info:", nrow(info_reactive()), "rows\n")
          cat("\nAll analysis tabs now use your uploaded data.\n")
          cat("Navigate to other tabs to explore.\n")
        })

      } else if (file_ext %in% c("xlsx", "xls")) {
        # Excel file - require readxl package
        if (!requireNamespace("readxl", quietly = TRUE)) {
          stop("Package 'readxl' required for Excel files.\nInstall with: install.packages('readxl')")
        }

        output$data_upload_status <- renderPrint({
          cat("ERROR: Excel import not yet implemented.\n\n")
          cat("For now, please use:\n")
          cat("  - RData format (.Rdata), or\n")
          cat("  - Convert your Excel file to CSV\n\n")
          cat("Excel import coming in next version!\n")
        })

      } else if (file_ext == "csv") {
        output$data_upload_status <- renderPrint({
          cat("ERROR: CSV import not yet implemented.\n\n")
          cat("For now, please use RData format (.Rdata)\n\n")
          cat("CSV import coming in next version!\n")
        })

      } else {
        stop("Unsupported file format")
      }

    }, error = function(e) {
      output$data_upload_status <- renderPrint({
        cat("ERROR loading data:\n\n")
        cat(e$message, "\n\n")
        cat("Please check:\n")
        cat("  - File format is correct\n")
        cat("  - Required objects/sheets are present\n")
        cat("  - Data matches expected structure\n")
      })
    })
  })
}
