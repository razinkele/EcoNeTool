#!/usr/bin/env Rscript
# Test script to verify visNetwork labels are properly set

library(igraph)
library(visNetwork)

# Load configuration and functions
source("R/config.R")
source("R/functions/functional_group_utils.R")
source("R/functions/trophic_levels.R")
source("R/functions/network_visualization.R")

# Load data
source("R/data_loading.R")

cat("\n=== VISNETWORK LABEL VERIFICATION ===\n\n")

if (exists("net") && exists("info")) {

  # Ensure colfg column exists
  if (!"colfg" %in% colnames(info)) {
    info$colfg <- COLOR_SCHEME[as.numeric(info$fg)]
  }

  # Test the visualization function
  cat("Creating visNetwork visualization...\n")

  tryCatch({
    vis_obj <- create_foodweb_visnetwork(
      net = net,
      info = info,
      node_size_method = "fixed",
      edge_color_by = "default"
    )

    # Extract nodes data from the visNetwork object
    # visNetwork stores data in vis_obj$x$nodes
    if (!is.null(vis_obj$x$nodes)) {
      nodes_data <- vis_obj$x$nodes

      cat("\n✓ visNetwork object created successfully\n")
      cat("Node data structure type:", class(nodes_data), "\n")
      cat("Number of nodes:", nrow(nodes_data), "\n\n")

      cat("First 10 node labels:\n")
      print(head(nodes_data$label, 10))

      cat("\nFirst 10 node IDs:\n")
      print(head(nodes_data$id, 10))

      # Check if labels are numeric strings
      are_numeric <- all(grepl("^[0-9]+$", nodes_data$label))

      if (are_numeric) {
        cat("\n✗ FAILED: Node labels are NUMERIC (", paste(head(nodes_data$label, 5), collapse=", "), "...)\n")
        cat("The graph will show numbers instead of species names.\n")
      } else {
        cat("\n✓ SUCCESS: Node labels are SPECIES NAMES\n")
        cat("The graph should now correctly display species names!\n")
      }

      # Verify tooltip titles
      cat("\n--- Tooltip Check ---\n")
      cat("First tooltip (should contain species name):\n")
      cat(nodes_data$title[1], "\n")

    } else {
      cat("✗ ERROR: Could not extract nodes data from visNetwork object\n")
    }

  }, error = function(e) {
    cat("✗ ERROR creating visualization:", e$message, "\n")
    cat("\nFull error:\n")
    print(e)
  })

} else {
  cat("ERROR: net or info object not found\n")
}

cat("\n=====================================\n")
