#' Internal Data Editor Handlers Server Module
#'
#' Manages the species info table and network adjacency matrix editors.
#' Returns the refresh_data_editor function for use by other modules.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param net_reactive ReactiveVal holding the igraph network
#' @param info_reactive ReactiveVal holding the species info data frame
#' @param dashboard_trigger ReactiveVal to trigger dashboard updates
#' @return A list containing the refresh_data_editor function
dataeditor_inline_server <- function(input, output, session, net_reactive, info_reactive,
                                      dashboard_trigger) {

  # Reactive values to store editable data
  # Initialize directly with data
  species_data <- reactiveVal({
    info <- info_reactive()
    info_copy <- info[, !names(info) %in% c("colfg"), drop = FALSE]
    info_copy
  })

  network_matrix_data <- reactiveVal({
    net <- net_reactive()
    adj_matrix <- as.matrix(as_adjacency_matrix(net, sparse = FALSE))
    adj_matrix
  })

  # Function to refresh data editor tables
  refresh_data_editor <- function() {
    tryCatch({
      # Use reactive values, not initial default dataset
      current_info <- info_reactive()
      current_net <- net_reactive()

      info_copy <- current_info[, !names(current_info) %in% c("colfg"), drop = FALSE]
      species_data(info_copy)

      adj_matrix <- as.matrix(as_adjacency_matrix(current_net, sparse = FALSE))
      network_matrix_data(adj_matrix)

      cat("Data editor tables refreshed with", nrow(info_copy), "species\n")
    }, error = function(e) {
      cat("Error refreshing data editor:", e$message, "\n")
    })
  }

  # Render Species Info Table (editable with tooltips)
  output$species_info_table <- DT::renderDataTable({
    species_df <- species_data()

    cat("Rendering species_info_table. Data is:", ifelse(is.null(species_df), "NULL", "present"), "\n")
    if (!is.null(species_df)) {
      cat("  Rows:", nrow(species_df), "Columns:", ncol(species_df), "\n")
    }

    req(species_df)

    # Prepare display data with appropriate decimal precision
    species_display <- species_df
    numeric_cols <- sapply(species_display, is.numeric)

    # Round numeric columns (2 decimals by default, 3 for bodymasses)
    for (col_name in names(species_display)[numeric_cols]) {
      if (col_name == "bodymasses") {
        species_display[[col_name]] <- round(species_display[[col_name]], 3)
      } else {
        species_display[[col_name]] <- round(species_display[[col_name]], 2)
      }
    }

    # Create the datatable
    dt <- DT::datatable(
      species_display,
      editable = TRUE,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        scrollY = "500px",
        dom = 'tp'
      ),
      rownames = TRUE
    )

    # Apply formatting (3 decimals for bodymasses, 2 for others)
    if ("bodymasses" %in% names(species_display)) {
      bodymasses_col_idx <- which(names(species_display) == "bodymasses")
      dt <- dt %>% DT::formatRound(columns = bodymasses_col_idx, digits = 3)
    }

    # Format other numeric columns
    other_numeric_cols <- which(numeric_cols & names(species_display) != "bodymasses")
    if (length(other_numeric_cols) > 0) {
      dt <- dt %>% DT::formatRound(columns = other_numeric_cols, digits = 2)
    }

    dt
  })

  # Handle Species Info Table edits
  observeEvent(input$species_info_table_cell_edit, {
    species_data_df <- species_data()
    info_edit <- input$species_info_table_cell_edit
    species_data_df[info_edit$row, info_edit$col] <- info_edit$value
    species_data(species_data_df)
  })

  # Save Species Info button
  observeEvent(input$save_species_info, {
    tryCatch({
      # Get edited data from reactive
      edited_info <- species_data()

      # Validate required columns exist
      required_cols <- c("meanB", "fg", "bodymasses", "met.types", "efficiencies")
      missing_cols <- setdiff(required_cols, colnames(edited_info))
      if (length(missing_cols) > 0) {
        stop(paste("Missing required columns:", paste(missing_cols, collapse=", ")))
      }

      # Reassign colors by matching functional group names to COLOR_SCHEME
      fg_levels <- get_functional_group_levels()
      edited_info$colfg <- sapply(as.character(edited_info$fg), function(fg) {
        idx <- which(fg_levels == fg)
        if (length(idx) == 0) return("gray")
        COLOR_SCHEME[idx]
      })

      # Update reactive value
      info_reactive(edited_info)

      output$species_info_status <- renderPrint({
        cat("SUCCESS: Species information saved!\n")
        cat("Updated", nrow(info_reactive()), "species records.\n")
        cat("\nNavigate to other tabs to see updated visualizations.\n")
      })
    }, error = function(e) {
      output$species_info_status <- renderPrint({
        cat("ERROR saving species info:\n")
        cat(e$message, "\n")
      })
    })
  })

  # Render Network Adjacency Matrix Table (editable with tooltips)
  output$network_matrix_table <- DT::renderDataTable({
    matrix_df <- network_matrix_data()

    cat("Rendering network_matrix_table. Data is:", ifelse(is.null(matrix_df), "NULL", "present"), "\n")
    if (!is.null(matrix_df)) {
      cat("  Rows:", nrow(matrix_df), "Columns:", ncol(matrix_df), "\n")
    }

    req(matrix_df)

    DT::datatable(
      matrix_df,
      editable = TRUE,
      extensions = 'FixedColumns',
      options = list(
        pageLength = 34,
        scrollX = TRUE,
        scrollY = "500px",
        dom = 't',
        fixedColumns = list(leftColumns = 1)
      ),
      rownames = TRUE
    )
  })

  # Handle Network Matrix Table edits
  observeEvent(input$network_matrix_table_cell_edit, {
    matrix_data <- network_matrix_data()
    matrix_edit <- input$network_matrix_table_cell_edit
    matrix_data[matrix_edit$row, matrix_edit$col] <- as.numeric(matrix_edit$value)
    network_matrix_data(matrix_data)
  })

  # Save Network Matrix button
  observeEvent(input$save_network_matrix, {
    tryCatch({
      output$network_matrix_status <- renderPrint({
        cat("Network matrix saved to memory.\n")
        cat("Click 'Update Network from Matrix' to apply changes to the network object.\n")
      })
    }, error = function(e) {
      output$network_matrix_status <- renderPrint({
        cat("ERROR saving network matrix:\n")
        cat(e$message, "\n")
      })
    })
  })

  # Update Network from Matrix button
  observeEvent(input$update_network, {
    tryCatch({
      # Get edited matrix
      edited_matrix <- network_matrix_data()

      # Validate matrix is square
      if (nrow(edited_matrix) != ncol(edited_matrix)) {
        stop("Adjacency matrix must be square (same number of rows and columns)")
      }

      # Validate matrix contains only 0s and 1s
      if (!all(edited_matrix %in% c(0, 1))) {
        stop("Adjacency matrix must contain only 0 (no link) or 1 (link exists)")
      }

      # Create new network from adjacency matrix
      updated_net <- igraph::graph_from_adjacency_matrix(edited_matrix, mode = "directed")

      # Upgrade if needed
      updated_net <- igraph::upgrade_graph(updated_net)

      # Explicitly set vertex names from rownames to ensure they're preserved
      if (!is.null(rownames(edited_matrix))) {
        igraph::V(updated_net)$name <- rownames(edited_matrix)
      }

      # Update reactive values for dashboard
      net_reactive(updated_net)

      output$network_matrix_status <- renderPrint({
        cat("SUCCESS: Network updated from matrix!\n")
        cat("Network now has:\n")
        cat("  - Species:", vcount(net_reactive()), "\n")
        cat("  - Links:", ecount(net_reactive()), "\n")
        cat("\nAll visualizations will now use the updated network.\n")
        cat("Navigate to other tabs to see the changes.\n")
      })
    }, error = function(e) {
      output$network_matrix_status <- renderPrint({
        cat("ERROR updating network:\n")
        cat(e$message, "\n")
      })
    })
  })

  # Ensure data editor outputs are not suspended when hidden (load by default)
  outputOptions(output, "species_info_table", suspendWhenHidden = FALSE)
  outputOptions(output, "network_matrix_table", suspendWhenHidden = FALSE)

  # Return the refresh function so other modules can use it
  return(refresh_data_editor)
}
