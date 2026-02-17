#' Data Editor Module
#'
#' Shiny module for editing network data:
#' - Species information (biomass, functional groups, traits)
#' - Network adjacency matrix (add/remove trophic links)
#'
#' @name dataeditor_module

#' Data Editor Module Server
#'
#' @param id Module ID
#' @param net_reactive Reactive value for network (igraph object)
#' @param info_reactive Reactive value for species info (data frame)
#'
#' @return List with reactive values:
#'   - species_data: Reactive data frame of species information
#'   - network_matrix: Reactive adjacency matrix
#'
#' @export
dataeditor_server <- function(id, net_reactive, info_reactive) {
  moduleServer(id, function(input, output, session) {

    # ============================================================================
    # REACTIVE VALUES
    # ============================================================================

    # Store editable copies of data
    species_data <- reactiveVal(NULL)
    network_matrix <- reactiveVal(NULL)

    # Initialize data when network/info changes
    observe({
      req(info_reactive())
      species_data(info_reactive())
    })

    observe({
      req(net_reactive())
      adj_matrix <- igraph::as_adjacency_matrix(net_reactive(), sparse = FALSE)
      network_matrix(adj_matrix)
    })

    # ============================================================================
    # SPECIES INFORMATION EDITOR
    # ============================================================================

    output$species_info_table <- DT::renderDataTable({
      req(species_data())

      current_data <- species_data()

      # Select editable columns
      editable_cols <- c("species", "meanB", "fg", "bodymasses",
                         "met.types", "efficiencies")
      display_data <- current_data[, intersect(editable_cols, colnames(current_data)), drop = FALSE]

      DT::datatable(
        display_data,
        editable = list(
          target = "cell",
          disable = list(columns = 0)  # Don't allow editing species names
        ),
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          autoWidth = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = TRUE,
        caption = "Edit species information (click cells to edit)"
      )
    })

    # Handle cell edits
    observeEvent(input$species_info_table_cell_edit, {
      req(species_data())

      info <- input$species_info_table_cell_edit
      current_data <- species_data()

      # Update the edited cell
      current_data[info$row, info$col] <- info$value

      # Update reactive
      species_data(current_data)
    })

    # Save button
    observeEvent(input$save_species_info, {
      tryCatch({
        # Get edited data
        edited_info <- species_data()

        # Validate required columns
        required_cols <- c("meanB", "fg", "bodymasses", "met.types", "efficiencies")
        missing_cols <- setdiff(required_cols, colnames(edited_info))
        if (length(missing_cols) > 0) {
          stop(paste("Missing required columns:", paste(missing_cols, collapse=", ")))
        }

        # Reassign colors
        edited_info$colfg <- COLOR_SCHEME[as.numeric(edited_info$fg)]

        # Update reactive value
        info_reactive(edited_info)

        output$species_info_status <- renderPrint({
          cat("✓ SUCCESS: Species information saved!\n")
          cat("Updated", nrow(edited_info), "species records.\n")
          cat("\nNavigate to other tabs to see updated visualizations.\n")
        })

      }, error = function(e) {
        output$species_info_status <- renderPrint({
          cat("✗ ERROR saving species information:\n")
          cat(e$message, "\n")
        })
      })
    })

    # ============================================================================
    # NETWORK ADJACENCY MATRIX EDITOR
    # ============================================================================

    output$network_matrix_table <- DT::renderDataTable({
      req(network_matrix())

      adj_matrix <- network_matrix()

      DT::datatable(
        adj_matrix,
        editable = list(target = "cell"),
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          scrollY = "400px",
          autoWidth = FALSE,
          dom = 't'
        ),
        caption = "Network adjacency matrix: rows = predators, columns = prey (1 = link exists, 0 = no link)"
      )
    })

    # Handle matrix cell edits
    observeEvent(input$network_matrix_table_cell_edit, {
      req(network_matrix())

      info <- input$network_matrix_table_cell_edit
      current_matrix <- network_matrix()

      # Update cell (convert to numeric)
      current_matrix[info$row, info$col] <- as.numeric(info$value)

      # Update reactive
      network_matrix(current_matrix)
    })

    # Save matrix button
    observeEvent(input$save_network_matrix, {
      output$network_matrix_status <- renderPrint({
        cat("✓ Matrix changes saved to temporary storage.\n")
        cat("Click 'Update Network from Matrix' to apply changes.\n")
      })
    })

    # Update network from matrix button
    observeEvent(input$update_network, {
      tryCatch({
        edited_matrix <- network_matrix()

        # Validate matrix
        if (!is.matrix(edited_matrix)) {
          stop("Invalid matrix format")
        }

        if (!all(edited_matrix %in% c(0, 1))) {
          stop("Adjacency matrix must contain only 0 (no link) or 1 (link exists)")
        }

        # Create new network from adjacency matrix
        updated_net <- igraph::graph_from_adjacency_matrix(edited_matrix, mode = "directed")

        # Upgrade if needed
        updated_net <- igraph::upgrade_graph(updated_net)

        # Set vertex names from rownames
        if (!is.null(rownames(edited_matrix))) {
          igraph::V(updated_net)$name <- rownames(edited_matrix)
        }

        # Update reactive value
        net_reactive(updated_net)

        output$network_matrix_status <- renderPrint({
          cat("✓ SUCCESS: Network updated from matrix!\n")
          cat("Network now has:\n")
          cat("  - Species:", igraph::vcount(updated_net), "\n")
          cat("  - Links:", igraph::ecount(updated_net), "\n")
          cat("\nAll visualizations will now use the updated network.\n")
          cat("Navigate to other tabs to see the changes.\n")
        })

      }, error = function(e) {
        output$network_matrix_status <- renderPrint({
          cat("✗ ERROR updating network:\n")
          cat(e$message, "\n")
        })
      })
    })

    # ============================================================================
    # RETURN VALUES
    # ============================================================================

    return(list(
      species_data = species_data,
      network_matrix = network_matrix
    ))
  })
}
