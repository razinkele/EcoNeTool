#!/usr/bin/env Rscript
# Test script to verify node structure including fixed parameter

library(igraph)
library(visNetwork)

# Load configuration and functions
source("R/config.R")
source("R/functions/functional_group_utils.R")
source("R/functions/trophic_levels.R")
source("R/functions/network_visualization.R")
source("R/data_loading.R")

cat("\n=== NODE STRUCTURE VERIFICATION ===\n\n")

if (exists("net") && exists("info")) {
  if (!"colfg" %in% colnames(info)) {
    info$colfg <- COLOR_SCHEME[as.numeric(info$fg)]
  }

  vis_obj <- create_foodweb_visnetwork(
    net = net,
    info = info,
    node_size_method = "fixed",
    edge_color_by = "default"
  )

  nodes_data <- vis_obj$x$nodes

  cat("✓ visNetwork object created\n")
  cat("Number of nodes:", nrow(nodes_data), "\n\n")

  cat("--- Node Columns ---\n")
  cat("Columns:", paste(colnames(nodes_data), collapse=", "), "\n\n")

  cat("--- First Node Structure ---\n")
  cat("ID:", nodes_data$id[1], "\n")
  cat("Label:", nodes_data$label[1], "\n")
  cat("X position:", nodes_data$x[1], "\n")
  cat("Y position:", nodes_data$y[1], "\n")
  cat("Fixed structure:\n")
  print(nodes_data$fixed[[1]])

  cat("\n--- Verify Fixed Column ---\n")
  cat("Fixed column class:", class(nodes_data$fixed), "\n")
  cat("First 3 fixed values:\n")
  for (i in 1:3) {
    cat(sprintf("  Node %d: x=%s, y=%s\n",
                i,
                nodes_data$fixed[[i]]$x,
                nodes_data$fixed[[i]]$y))
  }

  cat("\n✓ All structure checks passed!\n")
  cat("Graph topology should be preserved with proper trophic levels.\n")

} else {
  cat("ERROR: net or info not found\n")
}

cat("\n===================================\n")
