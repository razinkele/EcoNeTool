#!/usr/bin/env Rscript
# Test script to verify vertex names are properly set

library(igraph)

# Load configuration
source("R/config.R")

# Load data
source("R/data_loading.R")

# Check vertex names
cat("\n=== VERTEX NAME VERIFICATION ===\n\n")

if (exists("net") && is_igraph(net)) {
  vertex_names <- V(net)$name

  cat("Number of vertices:", vcount(net), "\n")
  cat("First 10 vertex names:\n")
  print(head(vertex_names, 10))

  # Check if names are numeric
  are_numeric <- all(grepl("^[0-9]+$", vertex_names))

  if (are_numeric) {
    cat("\n⚠ WARNING: Vertex names are NUMERIC (", paste(head(vertex_names, 5), collapse=", "), "...)\n")
    cat("This means the fix did not work properly.\n")
  } else {
    cat("\n✓ SUCCESS: Vertex names are PROPER SPECIES NAMES\n")
    cat("The network visualization should now show species names instead of numbers.\n")
  }

  # Check info data
  if (exists("info")) {
    cat("\n--- Info Data Frame ---\n")
    cat("Number of rows:", nrow(info), "\n")

    if ("species" %in% colnames(info)) {
      cat("First 10 species from info$species:\n")
      print(head(info$species, 10))
    } else {
      cat("info$species column not found\n")
      if (!is.null(rownames(info))) {
        cat("First 10 rownames from info:\n")
        print(head(rownames(info), 10))
      }
    }
  }

} else {
  cat("ERROR: net object not found or not an igraph object\n")
}

cat("\n=================================\n")
