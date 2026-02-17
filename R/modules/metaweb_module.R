#' Metaweb Module
#'
#' Shiny module for metaweb management:
#' - Load regional metawebs
#' - Import custom metawebs
#' - Edit metawebs (add/remove species and links)
#' - Filter by link quality
#' - Export to active network
#'
#' @name metaweb_module

#' Metaweb Module Server
#'
#' @param id Module ID
#' @param net_reactive Reactive value for network (igraph object)
#' @param info_reactive Reactive value for species info (data frame)
#'
#' @return List with reactive values:
#'   - current_metaweb: Currently loaded metaweb
#'   - metaweb_metadata: Metadata about the metaweb
#'
#' @export
metaweb_server <- function(id, net_reactive, info_reactive) {
  moduleServer(id, function(input, output, session) {

    # ============================================================================
    # REACTIVE VALUES
    # ============================================================================

    current_metaweb <- reactiveVal(NULL)
    metaweb_metadata <- reactiveVal(list(
      location = "No metaweb loaded",
      time_period = "",
      source = ""
    ))

    # ============================================================================
    # LOAD REGIONAL METAWEB
    # ============================================================================

    observeEvent(input$load_regional_btn, {
      req(input$regional_metaweb_select)

      selected_region <- input$regional_metaweb_select

      tryCatch({
        # Load metaweb based on selection
        metaweb_path <- METAWEB_PATHS[[selected_region]]

        if (is.null(metaweb_path) || !file.exists(metaweb_path)) {
          stop(paste("Metaweb file not found for", selected_region))
        }

        # Load metaweb
        metaweb <- load_metaweb(metaweb_path)

        # Update reactive values
        current_metaweb(metaweb)

        # Update metadata based on region
        metadata <- list(
          location = selected_region,
          time_period = if(!is.null(metaweb$time_period)) metaweb$time_period else "Multi-year",
          source = if(!is.null(metaweb$source)) metaweb$source else "Regional Metaweb Database"
        )
        metaweb_metadata(metadata)

        showNotification(
          paste("✓ Loaded", selected_region, "metaweb"),
          type = "message",
          duration = 3
        )

      }, error = function(e) {
        showNotification(
          paste("✗ Error loading metaweb:", e$message),
          type = "error",
          duration = 5
        )
      })
    })

    # ============================================================================
    # IMPORT CUSTOM METAWEB
    # ============================================================================

    observeEvent(input$import_metaweb_btn, {
      req(input$metaweb_file)

      tryCatch({
        metaweb_path <- input$metaweb_file$datapath

        # Load metaweb from file
        metaweb <- load_metaweb(metaweb_path)

        # Update reactive values
        current_metaweb(metaweb)

        # Update metadata
        metadata <- list(
          location = "Custom Import",
          time_period = if(!is.null(metaweb$time_period)) metaweb$time_period else "Unknown",
          source = input$metaweb_file$name
        )
        metaweb_metadata(metadata)

        showNotification(
          "✓ Custom metaweb imported successfully",
          type = "message",
          duration = 3
        )

      }, error = function(e) {
        showNotification(
          paste("✗ Error importing metaweb:", e$message),
          type = "error",
          duration = 5
        )
      })
    })

    # ============================================================================
    # METAWEB SUMMARY
    # ============================================================================

    output$metaweb_summary <- renderPrint({
      if (is.null(current_metaweb())) {
        cat("No metaweb loaded.\n\n")
        cat("Select a regional metaweb or import your own.\n")
        return()
      }

      metaweb <- current_metaweb()
      metadata <- metaweb_metadata()

      cat("═══════════════════════════════════════════════════\n")
      cat("METAWEB SUMMARY\n")
      cat("═══════════════════════════════════════════════════\n\n")

      cat("Region:", metadata$location, "\n")
      cat("Period:", metadata$time_period, "\n")
      cat("Source:", metadata$source, "\n\n")

      cat("Network statistics:\n")
      cat("  Species:", nrow(metaweb$species), "\n")
      cat("  Interactions:", nrow(metaweb$interactions), "\n")

      if ("link_quality" %in% colnames(metaweb$interactions)) {
        cat("\nLink quality distribution:\n")
        quality_table <- table(metaweb$interactions$link_quality)
        for (quality in names(quality_table)) {
          cat("  ", quality, ":", quality_table[quality], "\n")
        }
      }
    })

    # ============================================================================
    # METAWEB VISUALIZATION
    # ============================================================================

    output$metaweb_network <- renderVisNetwork({
      req(current_metaweb())

      tryCatch({
        metaweb <- current_metaweb()

        # Convert to igraph for visualization
        g <- metaweb_to_igraph(metaweb)

        # Create nodes data frame
        nodes <- data.frame(
          id = igraph::V(g)$name,
          label = igraph::V(g)$name,
          title = paste0("<b>", igraph::V(g)$name, "</b>"),
          stringsAsFactors = FALSE
        )

        # Create edges data frame
        edges <- igraph::as_data_frame(g, what = "edges")
        colnames(edges) <- c("from", "to")

        # Add link quality if available
        if ("link_quality" %in% colnames(metaweb$interactions)) {
          edges$title <- metaweb$interactions$link_quality
          edges$color <- ifelse(metaweb$interactions$link_quality == "strong", "#2ecc71",
                                ifelse(metaweb$interactions$link_quality == "medium", "#f39c12", "#e74c3c"))
        }

        # Create visualization
        visNetwork(nodes, edges, width = "100%", height = "600px") %>%
          visEdges(arrows = "to", smooth = FALSE) %>%
          visOptions(
            highlightNearest = list(enabled = TRUE, degree = 1),
            nodesIdSelection = TRUE
          ) %>%
          visPhysics(
            stabilization = TRUE,
            solver = "forceAtlas2Based"
          )

      }, error = function(e) {
        # Return empty network on error
        visNetwork(
          data.frame(id = 1, label = "Error", title = e$message),
          data.frame(from = integer(0), to = integer(0))
        )
      })
    })

    # ============================================================================
    # METAWEB TABLES
    # ============================================================================

    output$metaweb_species_table <- renderDT({
      req(current_metaweb())

      species_df <- current_metaweb()$species

      datatable(
        species_df,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        caption = "Species in metaweb"
      )
    })

    output$metaweb_interactions_table <- renderDT({
      req(current_metaweb())

      interactions_df <- current_metaweb()$interactions

      datatable(
        interactions_df,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        caption = "Trophic interactions in metaweb"
      )
    })

    # ============================================================================
    # METAWEB EDITING
    # ============================================================================

    # Add species
    observeEvent(input$add_species_btn, {
      req(current_metaweb(), input$new_species_name)

      tryCatch({
        metaweb <- current_metaweb()

        new_species <- data.frame(
          species_name = input$new_species_name,
          stringsAsFactors = FALSE
        )

        # Check if species already exists
        if (input$new_species_name %in% metaweb$species$species_name) {
          stop("Species already exists in metaweb")
        }

        # Add to species table
        metaweb$species <- rbind(metaweb$species, new_species)

        # Update reactive
        current_metaweb(metaweb)

        output$metaweb_edit_status <- renderPrint({
          cat("✓ Added species:", input$new_species_name, "\n")
        })

      }, error = function(e) {
        output$metaweb_edit_status <- renderPrint({
          cat("✗ Error adding species:", e$message, "\n")
        })
      })
    })

    # Remove species
    observeEvent(input$remove_species_btn, {
      req(current_metaweb(), input$remove_species_name)

      tryCatch({
        metaweb <- current_metaweb()

        # Remove from species table
        metaweb$species <- metaweb$species[metaweb$species$species_name != input$remove_species_name, ]

        # Remove from interactions
        metaweb$interactions <- metaweb$interactions[
          metaweb$interactions$predator != input$remove_species_name &
            metaweb$interactions$prey != input$remove_species_name,
        ]

        # Update reactive
        current_metaweb(metaweb)

        output$metaweb_edit_status <- renderPrint({
          cat("✓ Removed species:", input$remove_species_name, "\n")
          cat("  (and all associated interactions)\n")
        })

      }, error = function(e) {
        output$metaweb_edit_status <- renderPrint({
          cat("✗ Error removing species:", e$message, "\n")
        })
      })
    })

    # Add link
    observeEvent(input$add_link_btn, {
      req(current_metaweb(), input$link_predator, input$link_prey)

      tryCatch({
        metaweb <- current_metaweb()

        new_link <- data.frame(
          predator = input$link_predator,
          prey = input$link_prey,
          link_quality = if(!is.null(input$link_quality)) input$link_quality else "medium",
          stringsAsFactors = FALSE
        )

        # Check if link already exists
        existing <- metaweb$interactions$predator == input$link_predator &
          metaweb$interactions$prey == input$link_prey

        if (any(existing)) {
          stop("Link already exists")
        }

        # Add to interactions
        metaweb$interactions <- rbind(metaweb$interactions, new_link)

        # Update reactive
        current_metaweb(metaweb)

        output$metaweb_edit_status <- renderPrint({
          cat("✓ Added link:", input$link_predator, "→", input$link_prey, "\n")
        })

      }, error = function(e) {
        output$metaweb_edit_status <- renderPrint({
          cat("✗ Error adding link:", e$message, "\n")
        })
      })
    })

    # Remove link
    observeEvent(input$remove_link_btn, {
      req(current_metaweb(), input$remove_link_predator, input$remove_link_prey)

      tryCatch({
        metaweb <- current_metaweb()

        # Remove from interactions
        metaweb$interactions <- metaweb$interactions[
          !(metaweb$interactions$predator == input$remove_link_predator &
              metaweb$interactions$prey == input$remove_link_prey),
        ]

        # Update reactive
        current_metaweb(metaweb)

        output$metaweb_edit_status <- renderPrint({
          cat("✓ Removed link:", input$remove_link_predator, "→", input$remove_link_prey, "\n")
        })

      }, error = function(e) {
        output$metaweb_edit_status <- renderPrint({
          cat("✗ Error removing link:", e$message, "\n")
        })
      })
    })

    # ============================================================================
    # LINK QUALITY FILTERING
    # ============================================================================

    output$link_quality_plot <- renderPlot({
      req(current_metaweb())

      metaweb <- current_metaweb()

      if (!"link_quality" %in% colnames(metaweb$interactions)) {
        plot.new()
        text(0.5, 0.5, "No link quality data available")
        return()
      }

      quality_counts <- table(metaweb$interactions$link_quality)

      barplot(
        quality_counts,
        main = "Link Quality Distribution",
        xlab = "Quality",
        ylab = "Count",
        col = c("strong" = "#2ecc71", "medium" = "#f39c12", "weak" = "#e74c3c")[names(quality_counts)]
      )
    })

    # ============================================================================
    # EXPORT TO ACTIVE NETWORK
    # ============================================================================

    observeEvent(input$export_to_network_btn, {
      req(current_metaweb())

      tryCatch({
        # Convert metaweb to igraph
        new_net <- metaweb_to_igraph(current_metaweb())

        # Update reactive network
        net_reactive(new_net)

        # Try to update info if available
        if ("species_name" %in% colnames(current_metaweb()$species)) {
          info_df <- current_metaweb()$species
          rownames(info_df) <- info_df$species_name

          # Ensure required columns with defaults
          if (!"meanB" %in% colnames(info_df)) info_df$meanB <- 1
          if (!"fg" %in% colnames(info_df)) info_df$fg <- factor(rep("Other", nrow(info_df)))
          if (!"bodymasses" %in% colnames(info_df)) info_df$bodymasses <- 1
          if (!"met.types" %in% colnames(info_df)) info_df$met.types <- "Other"
          if (!"efficiencies" %in% colnames(info_df)) info_df$efficiencies <- 0.5

          # Update reactive info
          info_reactive(info_df)
        }

        output$export_status <- renderPrint({
          cat("✓ SUCCESS: Metaweb converted to active network!\n")
          cat("Species:", igraph::vcount(new_net), "\n")
          cat("Links:", igraph::ecount(new_net), "\n")
          cat("\nThe network is now available in:\n")
          cat("  • Food Web Network tab\n")
          cat("  • Topological Metrics tab\n")
          cat("  • Biomass Analysis tab (if biomass data available)\n")
          cat("  • Energy Fluxes tab (if trait data available)\n")
          cat("  • Keystoneness Analysis tab (if biomass data available)\n")
        })

      }, error = function(e) {
        output$export_status <- renderPrint({
          cat("✗ ERROR exporting metaweb:\n")
          cat(e$message, "\n")
        })
      })
    })

    # ============================================================================
    # DOWNLOAD METAWEB
    # ============================================================================

    output$download_metaweb <- downloadHandler(
      filename = function() {
        metadata <- metaweb_metadata()
        safe_name <- gsub("[^a-zA-Z0-9]", "_", metadata$location)
        paste0("metaweb_", safe_name, "_", Sys.Date(), ".RData")
      },
      content = function(file) {
        metaweb <- current_metaweb()
        save(metaweb, file = file)
      }
    )

    # ============================================================================
    # RETURN VALUES
    # ============================================================================

    return(list(
      current_metaweb = current_metaweb,
      metaweb_metadata = metaweb_metadata
    ))
  })
}
