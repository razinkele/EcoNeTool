
# ============================================================================
# FOOD WEB VISUALIZATION
# ============================================================================

#' Plot food web with trophic level layout
#'
#' Creates a visual representation of the food web with species arranged
#' by trophic level on the y-axis.
#'
#' @param net An igraph object representing the food web
#' @param col Vector of colors for each node (default: rainbow colors)
#' @param lab Vector of labels for each node (default: vertex names)
#' @param size Vector of node sizes (default: maxsize/2 for all)
#' @param nylevel Number of trophic level bins for y-axis (default: 5)
#' @param maxsize Maximum node size (default: 10)
#' @param labcex Label text size (default: 0.01)
#' @param ynum Number of y-axis tick marks (default: 6)
#' @param ylab Y-axis label (default: "Trophic Level")
#' @param ... Additional arguments passed to plot.igraph()
#'
#' @return Invisibly returns a data frame with node coordinates, sizes, and colors
#'
#' @details
#' - Nodes are positioned horizontally by trophic level
#' - Within each trophic level, nodes are spread evenly
#' - Y-axis shows actual trophic level values
#'
#' @examples
#' plotfw(net, col = info$colfg, size = info$meanB * 10)
plotfw <- function(net, col = NULL, lab = NULL, size = NULL,
                   nylevel = 5, maxsize = 10, labcex = 0.01,
                   ynum = 6, ylab = "Trophic Level", ...) {
  n <- vcount(net)
  if (!is.null(col)) {
    V(net)$color <- col
  } else {
    V(net)$color <- rainbow(vcount(net))
  }
  if (!is.null(lab)) {
    V(net)$name <- lab
  }
  if (!is.null(size)) {
    V(net)$size <- size
  } else {
    V(net)$size <- maxsize / 2
  }

  tl <- calculate_trophic_levels(net)
  dgpred <- tl

  bks <- c(0.9, seq(1.9, max(tl), length.out = nylevel))
  ynod <- cut(tl, breaks = bks, include.lowest = TRUE,
              labels = 1:(length(bks) - 1))
  xnod <- rep(0, n)
  for (i in 1:(length(bks) - 1)) {
    l <- sum(ynod == i)
    xnod[ynod == i] <- seq(-l, l, length.out = l)[order(tl[ynod == i])]
  }
  coo <- cbind(xnod, tl)

  # y axis with 1 and continuous axis from 2 to max TL.
  yax <- c(1, seq(2, max(tl), length.out = ynum - 1))
  labax <- round(yax, 1)
  # rescale xax between -1 and 1
  laby <- (yax - min(yax)) / (max(yax) - min(yax)) * 2 - 1

  plot(net, layout = coo, vertex.label.color = "black",
       vertex.label.cex = labcex, ...)
  axis(2, at = laby, labels = labax)
  mtext(ylab, side = 2, line = 2)
  res <- data.frame(coo, "size" = V(net)$size, "color" = V(net)$color)
  names(res) <- c("x", "y", "size", "color")
  row.names(res) <- V(net)$name
  invisible(res)
}

# ============================================================================
# VISNETWORK VISUALIZATION - COMMON FUNCTION
# ============================================================================

#' Create visNetwork Visualization with Trophic Level Layout
#'
#' Common function for visualizing food web networks with consistent styling,
#' trophic level-based vertical positioning, and optimized horizontal layout.
#'
#' @param net igraph network object
#' @param info data frame with species information (must have: fg, colfg, meanB)
#' @param node_size_method How to size nodes: "biomass_sqrt", "biomass_log", "flux", or "fixed"
#' @param edge_network Optional: alternative igraph network for edges (e.g., flux network)
#' @param edge_data Optional: data frame with edge attributes (width, value, title, color)
#' @param edge_color_by How to color edges: "prey" (source node), "predator" (target node), "fixed", or "default"
#' @param fixed_edge_color Color to use if edge_color_by = "fixed" (default: "#95a5a6")
#' @return visNetwork object
#' @export
create_foodweb_visnetwork <- function(net,
                                      info,
                                      node_size_method = "biomass_sqrt",
                                      edge_network = NULL,
                                      edge_data = NULL,
                                      edge_color_by = "prey",
                                      fixed_edge_color = "#95a5a6",
                                      trophic_levels = NULL) {

  # Validate inputs
  if (missing(net) || is.null(net)) stop("Parameter 'net' is required and cannot be NULL")
  if (!igraph::is_igraph(net)) stop("net must be an igraph object")
  if (missing(info) || is.null(info)) stop("Parameter 'info' is required and cannot be NULL")
  if (!is.data.frame(info)) stop("info must be a data frame")
  required_cols <- c("fg", "colfg", "meanB")
  missing <- setdiff(required_cols, colnames(info))
  if (length(missing) > 0) {
    stop(paste("info missing required columns:", paste(missing, collapse = ", ")))
  }

  # Calculate trophic levels (or use pre-calculated if provided)
  if (is.null(trophic_levels)) {
    tl <- calculate_trophic_levels(net)
  } else {
    tl <- trophic_levels
  }

  # Set Y positions (highest TL at top, lowest at bottom)
  y_pos <- (max(tl) - tl) * VIS_TROPHIC_LEVEL_SPACING

  # Calculate node sizes based on method
  if (node_size_method == "biomass_sqrt") {
    # Square root scaling for biomass
    biomass_values <- info$meanB
    biomass_values[biomass_values <= 0] <- min(biomass_values[biomass_values > 0], na.rm = TRUE) / 10
    biomass_sqrt <- sqrt(biomass_values)
    biomass_sqrt_max <- max(biomass_sqrt, na.rm = TRUE)
    biomass_sqrt_min <- min(biomass_sqrt, na.rm = TRUE)
    node_sizes <- ((biomass_sqrt - biomass_sqrt_min) / (biomass_sqrt_max - biomass_sqrt_min)) * 90 + 10
  } else if (node_size_method == "biomass_log") {
    # Logarithmic scaling for biomass
    biomass_values <- info$meanB
    biomass_values[biomass_values <= 0] <- min(biomass_values[biomass_values > 0], na.rm = TRUE) / 10
    biomass_log <- log10(biomass_values)
    biomass_log_max <- max(biomass_log, na.rm = TRUE)
    biomass_log_min <- min(biomass_log, na.rm = TRUE)
    node_sizes <- ((biomass_log - biomass_log_min) / (biomass_log_max - biomass_log_min)) * 90 + 10
  } else if (node_size_method == "fixed") {
    # Fixed size for all nodes
    node_sizes <- rep(15, vcount(net))
  } else {
    # Default to square root
    node_sizes <- rep(15, vcount(net))
  }

  # Prepare nodes data frame for visNetwork
  # MUST set explicit colors - visNetwork needs this even with visGroups
  nodes <- data.frame(
    id = 1:vcount(net),
    label = V(net)$name,
    group = as.character(info$fg),
    value = node_sizes,
    shape = "dot",
    stringsAsFactors = FALSE
  )

  # Set node colors explicitly using info$colfg (which has correct name-based colors)
  # This MUST be a list-column with background/border/highlight structure
  nodes$color <- I(lapply(info$colfg, function(col) {
    list(
      background = col,
      border = col,
      highlight = list(background = col, border = "black")
    )
  }))

  # Calculate X positions based on trophic level grouping
  x_positions <- numeric(vcount(net))
  for (i in 1:vcount(net)) {
    # Group nodes by similar trophic levels (within 0.1 units)
    tl_group <- round(tl[i] * 10) / 10
    nodes_in_group <- which(abs(tl - tl_group) < 0.1)
    position_in_group <- which(nodes_in_group == i)
    n_in_group <- length(nodes_in_group)

    # Calculate initial X position for horizontal spread
    x_positions[i] <- ((position_in_group - 1) / max(n_in_group - 1, 1) - 0.5) * 1600
  }

  # Add position data
  nodes$x <- x_positions
  nodes$y <- y_pos

  # Create fixed column as list-column (fix Y, allow X physics)
  # This must be a list where each element is a list
  nodes$fixed <- I(lapply(1:nrow(nodes), function(i) list(y = TRUE, x = FALSE)))

  # Add tooltip titles
  nodes$title <- sapply(1:vcount(net), function(i) {
    paste0("<b>", V(net)$name[i], "</b><br>",
           "Functional Group: ", info$fg[i], "<br>",
           "Trophic Level: ", round(tl[i], 3), "<br>",
           "Biomass: ", round(info$meanB[i], 2), " g/km²")
  })

  # Map node names to IDs for edges
  name_to_id <- setNames(nodes$id, nodes$label)

  # Create edges (use alternative network if provided, e.g., for flux)
  edge_net <- if (!is.null(edge_network)) edge_network else net

  # Initialize edge_colors
  edge_colors <- NULL

  # Handle case where edge network has no edges
  if (ecount(edge_net) == 0) {
    edges <- data.frame(from=integer(0), to=integer(0), stringsAsFactors = FALSE)
  } else {
    edgelist_raw <- as_edgelist(edge_net)

    # Validate that all edge names exist in node list
    from_names <- edgelist_raw[, 1]
    to_names <- edgelist_raw[, 2]
    missing_from <- setdiff(from_names, nodes$label)
    missing_to <- setdiff(to_names, nodes$label)

    if (length(missing_from) > 0 || length(missing_to) > 0) {
      warning("Edge network contains vertices not in main network. This may cause visualization issues.")
      message("Missing source vertices: ", paste(missing_from, collapse=", "))
      message("Missing target vertices: ", paste(missing_to, collapse=", "))
    }

    # Color edges by prey node if requested
    if (edge_color_by == "prey") {
      edge_colors <- sapply(edgelist_raw[, 1], function(prey_name) {
        prey_id <- name_to_id[prey_name]
        if (is.na(prey_id)) return("#95a5a6")  # Default gray for missing
        info$colfg[prey_id]
      })
    } else if (edge_color_by == "predator") {
      edge_colors <- sapply(edgelist_raw[, 2], function(pred_name) {
        pred_id <- name_to_id[pred_name]
        if (is.na(pred_id)) return("#95a5a6")  # Default gray for missing
        info$colfg[pred_id]
      })
    } else if (edge_color_by == "fixed") {
      edge_colors <- rep(fixed_edge_color, nrow(edgelist_raw))
    } else {
      edge_colors <- NULL  # Use default
    }

    # Build edge dataframe
    edges <- data.frame(
      from = name_to_id[edgelist_raw[, 1]],
      to = name_to_id[edgelist_raw[, 2]],
      stringsAsFactors = FALSE
    )

    # Remove edges with NA mappings
    valid_edges <- !is.na(edges$from) & !is.na(edges$to)
    if (!all(valid_edges)) {
      warning(sprintf("Removed %d edges with invalid node mappings", sum(!valid_edges)))
      edges <- edges[valid_edges, , drop=FALSE]
      if (!is.null(edge_colors)) {
        edge_colors <- edge_colors[valid_edges]
      }
    }
  }

  # Add edge colors if specified
  if (!is.null(edge_colors) && nrow(edges) > 0) {
    edges$color <- edge_colors
  }

  # Add custom edge data if provided (e.g., flux widths)
  if (!is.null(edge_data) && nrow(edges) > 0) {
    # Validate edge_data has same number of rows as edges
    if (nrow(edge_data) == nrow(edges)) {
      for (col in names(edge_data)) {
        edges[[col]] <- edge_data[[col]]
      }
    } else {
      warning(sprintf("edge_data has %d rows but edges has %d rows. Skipping edge_data.",
                      nrow(edge_data), nrow(edges)))
    }
  }

  # Define explicit colors for each functional group
  # This prevents visNetwork from auto-assigning colors based on groups
  fg_levels <- get_functional_group_levels()

  # Create network visualization with straight edges and no navigation
  vis <- visNetwork::visNetwork(nodes, edges, width = "100%", height = "90vh")

  # Add explicit group color definitions to ensure consistency
  for (i in seq_along(fg_levels)) {
    fg <- fg_levels[i]
    if (fg %in% unique(info$fg)) {
      vis <- vis %>%
        visNetwork::visGroups(
          groupname = fg,
          color = list(
            background = COLOR_SCHEME[i],
            border = COLOR_SCHEME[i],
            highlight = list(background = COLOR_SCHEME[i], border = "black")
          )
        )
    }
  }

  vis <- vis %>%
    visNetwork::visEdges(
      arrows = "to",
      smooth = FALSE  # Always use straight edges
    ) %>%
    visNetwork::visNodes(
      shadow = list(enabled = TRUE, size = 5),
      font = list(
        size = 14,
        face = 'arial',
        color = '#000000',
        strokeWidth = 0,
        strokeColor = '#ffffff'
      )
    ) %>%
    visNetwork::visPhysics(
      solver = "barnesHut",
      barnesHut = list(
        gravitationalConstant = VIS_GRAVITATIONAL_CONSTANT,  # Strong repulsion
        centralGravity = 0.05,                                # Weak central pull
        springLength = VIS_SPRING_LENGTH,                     # Long springs
        springConstant = 0.01,          # Flexible springs
        damping = 0.09,
        avoidOverlap = 1.0
      ),
      stabilization = list(
        enabled = TRUE,
        iterations = 1000,
        updateInterval = 50,
        onlyDynamicEdges = FALSE,
        fit = TRUE
      )
    ) %>%
    visNetwork::visEvents(
      stabilizationIterationsDone = "function() {
        this.setOptions({physics: false});
      }"
    ) %>%
    visNetwork::visLayout(
      randomSeed = 42
    ) %>%
    visNetwork::visInteraction(
      navigationButtons = FALSE,  # Never show navigation buttons
      hover = TRUE,
      tooltipDelay = 100,
      zoomView = TRUE
    ) %>%
    visNetwork::visOptions(
      highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 1),
      nodesIdSelection = list(
        enabled = TRUE,
        main = "Select species",
        style = 'width: 200px; height: 26px;'
      )
    )

  # Add legend - show functional groups in standard order
  # Only include groups that are actually present in the network
  fg_levels <- get_functional_group_levels()
  present_groups <- fg_levels[fg_levels %in% unique(info$fg)]

  vis <- vis %>%
    visNetwork::visLegend(
      addNodes = lapply(seq_along(present_groups), function(i) {
        fg <- present_groups[i]
        fg_color <- COLOR_SCHEME[which(fg_levels == fg)]
        list(
          label = as.character(fg),
          shape = "dot",
          color = list(
            background = fg_color,
            border = fg_color
          ),
          size = 15
        )
      }),
      useGroups = FALSE,
      position = "right",
      width = 0.15
    )

  return(vis)
}
