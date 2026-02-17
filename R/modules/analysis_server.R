#' Analysis Server Module (Flux + Keystoneness)
#'
#' Handles energy flux analysis and keystoneness analysis including
#' cached reactive computations, flux heatmap, flux network, flux indicators,
#' keystoneness table/plot, MTI heatmap, and keystone summary.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param net_reactive ReactiveVal holding the igraph network
#' @param info_reactive ReactiveVal holding the species info data frame
#' @param trophic_levels_cached Cached reactive for trophic levels
analysis_server <- function(input, output, session, net_reactive, info_reactive,
                             trophic_levels_cached) {

  # ============================================================================
  # FLUX ANALYSIS - CACHED REACTIVE
  # ============================================================================

  # Cache expensive flux calculations to avoid redundant computation
  flux_results <- reactive({
    req(net_reactive(), info_reactive())
    withProgress(message = 'Calculating energy fluxes...', {
      get_fluxweb_results(net_reactive(), info_reactive())
    })
  }) %>% bindCache(net_reactive(), info_reactive())

  # Fluxweb Analysis
  output$flux_heatmap <- renderPlot({
    tryCatch({
      res <- flux_results()
      heatmap(log(res$fluxes + FLUX_LOG_EPSILON), Rowv=NA, Colv=NA, scale="none")
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating flux heatmap:", e$message))
    })
  })

  output$flux_network_plot <- renderVisNetwork({
    tryCatch({
      # Use reactive values to ensure automatic updates
      current_net <- net_reactive()
      current_info <- info_reactive()
      current_tl <- trophic_levels_cached()

      # Get cached flux results
      res <- flux_results()

      # Check if flux network has edges
      if (ecount(res$netLW) == 0) {
        # No edges - show network with all nodes but no connections
        create_foodweb_visnetwork(
          net = current_net,
          info = current_info,
          node_size_method = "fixed",
          edge_network = res$netLW,  # Empty edge network
          edge_data = NULL,
          edge_color_by = "default",
          trophic_levels = current_tl
        )
      } else {
        # Prepare edge data with flux information
        flux_weights <- E(res$netLW)$weight

        # Handle case where all fluxes are zero
        max_flux <- max(flux_weights, na.rm = TRUE)
        if (is.finite(max_flux) && max_flux > 0) {
          edge_widths <- EDGE_WIDTH_MIN + (flux_weights/max_flux * EDGE_WIDTH_SCALE)
        } else {
          edge_widths <- rep(EDGE_WIDTH_MIN, length(flux_weights))
        }

        # Format flux values for display
        flux_display <- sapply(flux_weights, function(x) {
          if (is.na(x) || !is.finite(x)) {
            "0"
          } else if (x >= 0.01) {
            sprintf("%.4f", x)
          } else if (x >= 0.0001) {
            sprintf("%.6f", x)
          } else {
            sprintf("%.2e", x)
          }
        })

        edge_data <- data.frame(
          width = edge_widths,
          value = flux_weights,
          title = paste0("Flux: ", flux_display, " kJ/day/km2"),
          stringsAsFactors = FALSE
        )

        # Use common visualization function with flux network and cached trophic levels
        create_foodweb_visnetwork(
          net = current_net,
          info = current_info,
          node_size_method = "fixed",
          edge_network = res$netLW,
          edge_data = edge_data,
          edge_color_by = "default",
          trophic_levels = current_tl
        )
      }
    }, error = function(e) {
      # Return informative error message
      visNetwork(
        data.frame(id=1, label="Error",
                   title=paste0("<b>Flux Network Error:</b><br>", e$message)),
        data.frame(from=integer(0), to=integer(0))
      )
    })
  })

  output$flux_indicators <- renderPrint({
    tryCatch({
      res <- flux_results()
      print(fluxind(res$fluxes))
    }, error = function(e) {
      cat("Error calculating flux indicators:", e$message)
    })
  })

  # Ensure flux analysis outputs are not suspended when hidden (load by default)
  outputOptions(output, "flux_heatmap", suspendWhenHidden = FALSE)
  outputOptions(output, "flux_network_plot", suspendWhenHidden = FALSE)
  outputOptions(output, "flux_indicators", suspendWhenHidden = FALSE)

  # ============================================================================
  # KEYSTONENESS ANALYSIS - CACHED REACTIVE
  # ============================================================================

  # Cache expensive keystoneness calculations (includes MTI matrix inversion)
  keystoneness_results <- reactive({
    req(net_reactive(), info_reactive())
    withProgress(message = 'Calculating keystoneness indices...', {
      calculate_keystoneness(net_reactive(), info_reactive())
    })
  }) %>% bindCache(net_reactive(), info_reactive())

  # Keystoneness table
  output$keystoneness_table <- DT::renderDataTable({
    tryCatch({
      ks_results <- keystoneness_results()

      # Format for display
      ks_display <- ks_results
      ks_display$overall_effect <- round(ks_display$overall_effect, 4)
      ks_display$relative_biomass <- round(ks_display$relative_biomass, 4)
      ks_display$keystoneness <- round(ks_display$keystoneness, 3)

      DT::datatable(
        ks_display,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          order = list(list(3, 'desc'))  # Sort by keystoneness
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          'keystone_status',
          backgroundColor = DT::styleEqual(
            c('Keystone', 'Dominant', 'Rare', 'Undefined'),
            c('#ffcccc', '#cce5ff', '#e6e6e6', '#fff9cc')
          )
        )
    }, error = function(e) {
      DT::datatable(data.frame(Error = paste("Error calculating keystoneness:", e$message)))
    })
  })

  # Keystoneness vs Biomass plot
  output$keystoneness_plot <- renderPlot({
    tryCatch({
      ks_results <- keystoneness_results()

      # Create color mapping for status
      status_colors <- c(
        "Keystone" = "#ff4444",
        "Dominant" = "#4444ff",
        "Rare" = "#999999",
        "Undefined" = "#ffcc00"
      )

      plot(
        ks_results$relative_biomass,
        ks_results$keystoneness,
        col = status_colors[ks_results$keystone_status],
        pch = 19,
        cex = 1.5,
        xlab = "Relative Biomass (proportion of total)",
        ylab = "Keystoneness Index",
        main = "Keystoneness vs Relative Biomass",
        log = "x"  # Log scale for biomass
      )

      # Add reference lines
      abline(h = 1, lty = 2, col = "gray50")
      abline(v = 0.05, lty = 2, col = "gray50")

      # Add labels for top keystone species
      top_n <- min(5, nrow(ks_results))
      top_species <- ks_results[1:top_n, ]

      text(
        top_species$relative_biomass,
        top_species$keystoneness,
        labels = top_species$species,
        pos = 4,
        cex = 0.7,
        col = "black"
      )

      # Add legend
      legend(
        "topright",
        legend = names(status_colors),
        col = status_colors,
        pch = 19,
        cex = 0.8,
        title = "Status"
      )

      # Add text annotations
      text(0.001, 1, "Keystone threshold", pos = 3, cex = 0.7, col = "gray50")
      text(0.05, max(ks_results$keystoneness, na.rm = TRUE) * 0.9,
           "5% biomass threshold", pos = 4, cex = 0.7, col = "gray50")

    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating plot:", e$message))
    })
  })

  # MTI Heatmap
  output$mti_heatmap <- renderPlot({
    tryCatch({
      net <- net_reactive()
      info <- info_reactive()
      mti_matrix <- calculate_mti(net, info)

      # Create color palette (red = negative, blue = positive)
      colors <- colorRampPalette(c("red", "white", "blue"))(100)

      # Determine symmetric color scale around zero
      max_abs <- max(abs(mti_matrix), na.rm = TRUE)
      breaks <- seq(-max_abs, max_abs, length.out = 101)

      # Create heatmap
      heatmap(
        mti_matrix,
        Rowv = NA,
        Colv = NA,
        scale = "none",
        col = colors,
        breaks = breaks,
        margins = c(8, 8),
        main = "Mixed Trophic Impact Matrix",
        xlab = "Impacting Species (impactor)",
        ylab = "Impacted Species",
        cexRow = 0.7,
        cexCol = 0.7
      )

    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating MTI heatmap:", e$message))
    })
  })

  # Keystone summary
  output$keystone_summary <- renderPrint({
    tryCatch({
      ks_results <- keystoneness_results()

      cat("=== KEYSTONENESS ANALYSIS SUMMARY ===\n\n")

      # Overall statistics
      cat("Total species analyzed:", nrow(ks_results), "\n")
      cat("Keystone species:", sum(ks_results$keystone_status == "Keystone", na.rm = TRUE), "\n")
      cat("Dominant species:", sum(ks_results$keystone_status == "Dominant", na.rm = TRUE), "\n")
      cat("Rare species:", sum(ks_results$keystone_status == "Rare", na.rm = TRUE), "\n\n")

      # Top 5 keystone species
      cat("=== TOP 5 KEYSTONE SPECIES ===\n\n")
      top_5 <- ks_results[1:min(5, nrow(ks_results)), ]

      for (i in 1:nrow(top_5)) {
        cat(sprintf("%d. %s\n", i, top_5$species[i]))
        cat(sprintf("   Keystoneness Index: %.3f\n", top_5$keystoneness[i]))
        cat(sprintf("   Overall Effect: %.4f\n", top_5$overall_effect[i]))
        cat(sprintf("   Relative Biomass: %.4f (%.2f%%)\n",
                    top_5$relative_biomass[i],
                    top_5$relative_biomass[i] * 100))
        cat(sprintf("   Status: %s\n", top_5$keystone_status[i]))
        cat("\n")
      }

      # Interpretation
      cat("=== INTERPRETATION ===\n\n")
      cat("Keystone species have high ecosystem impact relative to their biomass.\n")
      cat("These species play critical roles in maintaining ecosystem structure.\n")
      cat("Their removal could lead to disproportionate ecosystem changes.\n\n")

      cat("MTI values indicate:\n")
      cat("  - Positive: Increase in impactor increases impacted species\n")
      cat("  - Negative: Increase in impactor decreases impacted species\n")
      cat("  - Magnitude: Strength of direct + indirect effects\n")

    }, error = function(e) {
      cat("Error generating keystoneness summary:", e$message)
    })
  })

  # Ensure keystoneness analysis outputs are not suspended when hidden (load by default)
  outputOptions(output, "keystoneness_table", suspendWhenHidden = FALSE)
  outputOptions(output, "keystoneness_plot", suspendWhenHidden = FALSE)
  outputOptions(output, "mti_heatmap", suspendWhenHidden = FALSE)
  outputOptions(output, "keystone_summary", suspendWhenHidden = FALSE)
}
