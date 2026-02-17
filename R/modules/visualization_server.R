#' Visualization Outputs Server Module
#'
#' Renders food web visualizations, basal/top species, adjacency heatmap,
#' topological indicators, biomass plots, and biomass network visualization.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param net_reactive ReactiveVal holding the igraph network
#' @param info_reactive ReactiveVal holding the species info data frame
#' @param trophic_levels_cached Cached reactive for trophic levels
#' @param topological_metrics_cached Cached reactive for topological metrics
visualization_server <- function(input, output, session, net_reactive, info_reactive,
                                  trophic_levels_cached, topological_metrics_cached) {

  # Food Web Visualization (visNetwork)
  output$foodweb_visnet <- renderVisNetwork({
    tryCatch({
      # Use reactive values to ensure automatic updates
      current_net <- net_reactive()
      current_info <- info_reactive()
      current_tl <- trophic_levels_cached()

      # Use common visualization function with fixed node sizes and cached trophic levels
      create_foodweb_visnetwork(
        net = current_net,
        info = current_info,
        node_size_method = "fixed",
        edge_color_by = "default",
        trophic_levels = current_tl
      )
    }, error = function(e) {
      # Return empty network on error
      visNetwork(data.frame(id=1, label="Error", title=e$message),
                 data.frame(from=integer(0), to=integer(0)))
    })
  })

  output$basal_species <- renderPrint({
    tryCatch({
      net <- net_reactive()
      basal <- igraph::V(net)$name[igraph::degree(net, mode="in")==0]
      cat("Basal species:\n", paste(basal, collapse=", "))
    }, error = function(e) {
      cat("Error identifying basal species:", e$message)
    })
  })

  output$top_predators <- renderPrint({
    tryCatch({
      net <- net_reactive()
      top_pred <- igraph::V(net)$name[igraph::degree(net, mode="out")==0]
      cat("Top predators:\n", paste(top_pred, collapse=", "))
    }, error = function(e) {
      cat("Error identifying top predators:", e$message)
    })
  })

  output$adjacency_heatmap <- renderPlot({
    tryCatch({
      net <- net_reactive()
      netmatrix <- as_adjacency_matrix(net, sparse=F)
      heatmap(netmatrix, Rowv=NA, Colv=NA, scale="none")
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating adjacency heatmap:", e$message))
    })
  })

  # Topological Indicators (using cached computation)
  output$topo_indicators <- renderPrint({
    tryCatch({
      ind <- topological_metrics_cached()
      print(ind)
    }, error = function(e) {
      cat("Error calculating topological indicators:", e$message)
    })
  })

  output$node_weighted_indicators <- renderPrint({
    tryCatch({
      net <- net_reactive()
      info <- info_reactive()
      ind <- get_node_weighted_indicators(net, info)
      print(ind)
    }, error = function(e) {
      cat("Error calculating node-weighted indicators:", e$message)
    })
  })

  # Node-weighted Indicators
  output$biomass_boxplot <- renderPlot({
    tryCatch({
      info <- info_reactive()
      boxplot(info$meanB~info$fg, las=2, col=COLOR_SCHEME,
              ylab="Biomass (g/day/km2)", xlab="")
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating biomass boxplot:", e$message))
    })
  })

  output$biomass_barplot <- renderPlot({
    tryCatch({
      info <- info_reactive()
      percB <- tapply(info$meanB, info$fg, sum)/sum(info$meanB)*100
      barplot(as.matrix(percB), col=COLOR_SCHEME, ylab="%")
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating biomass barplot:", e$message))
    })
  })

  # Biomass Network Visualization (visNetwork with hierarchical layout)
  output$foodweb_biomass_visnet <- renderVisNetwork({
    tryCatch({
      # Use reactive values to ensure automatic updates
      current_net <- net_reactive()
      current_info <- info_reactive()
      current_tl <- trophic_levels_cached()

      # Use common visualization function with biomass-scaled nodes and cached trophic levels
      create_foodweb_visnetwork(
        net = current_net,
        info = current_info,
        node_size_method = "biomass_sqrt",
        edge_color_by = "prey",
        trophic_levels = current_tl
      )
    }, error = function(e) {
      # Return empty network on error
      visNetwork(
        data.frame(id = 1, label = "Error", stringsAsFactors = FALSE),
        data.frame(from = numeric(0), to = numeric(0)),
        width = "100%", height = "600px"
      ) %>%
        visNodes(color = "red") %>%
        visInteraction(tooltipDelay = 0) %>%
        visOptions(nodesIdSelection = FALSE)
    })
  })

  # Ensure visualization outputs are not suspended when hidden (load by default)
  outputOptions(output, "foodweb_visnet", suspendWhenHidden = FALSE)
  outputOptions(output, "basal_species", suspendWhenHidden = FALSE)
  outputOptions(output, "top_predators", suspendWhenHidden = FALSE)
  outputOptions(output, "adjacency_heatmap", suspendWhenHidden = FALSE)
  outputOptions(output, "topo_indicators", suspendWhenHidden = FALSE)
  outputOptions(output, "node_weighted_indicators", suspendWhenHidden = FALSE)
  outputOptions(output, "biomass_boxplot", suspendWhenHidden = FALSE)
  outputOptions(output, "biomass_barplot", suspendWhenHidden = FALSE)
  outputOptions(output, "foodweb_biomass_visnet", suspendWhenHidden = FALSE)
}
