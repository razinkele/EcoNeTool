#!/usr/bin/env Rscript
# ==========================================================================
# Phase 1 Comprehensive Test Suite
# ==========================================================================
# Tests all 19 functions systematically with detailed reporting
# ==========================================================================

cat("========================================\n")
cat("  Phase 1 Comprehensive Test Suite\n")
cat("========================================\n\n")

# Test results tracking
test_results <- list()
test_count <- 0
pass_count <- 0
fail_count <- 0

# Test function
run_test <- function(test_name, test_code) {
  test_count <<- test_count + 1
  cat(sprintf("[%02d] Testing: %s\n", test_count, test_name))

  result <- tryCatch({
    test_code()
    pass_count <<- pass_count + 1
    cat("     ✓ PASS\n")
    list(status = "PASS", error = NULL)
  }, error = function(e) {
    fail_count <<- fail_count + 1
    cat("     ✗ FAIL:", e$message, "\n")
    list(status = "FAIL", error = e$message)
  })

  test_results[[test_name]] <<- result
  return(result$status == "PASS")
}

# ==========================================================================
# SETUP
# ==========================================================================

cat("Setting up test environment...\n")

# Load libraries
suppressPackageStartupMessages({
  library(sf)
  library(dggridR)
  library(leaflet)
  library(mapview)
  library(igraph)
})

# Source functions
source("functions.R")
source("spatial_functions_phase1.R")

# Load metaweb
metaweb <- readRDS("metawebs/baltic/baltic_kortsch2021.rds")

cat("✓ Setup complete\n\n")

# ==========================================================================
# COMPONENT 1.1: HEXAGONAL GRID FRAMEWORK (5 functions)
# ==========================================================================

cat("========================================\n")
cat("Component 1.1: Hexagonal Grid Framework\n")
cat("========================================\n\n")

# Test 1: create_hex_grid (basic)
run_test("create_hex_grid - bounding box", {
  bbox <- c(18.5, 59.5, 19.5, 60.5)
  grid <- create_hex_grid(bbox, cell_size = 2)
  stopifnot(inherits(grid, "hex_grid"))
  stopifnot(grid$cell_size == 2)
  stopifnot(grid$n_cells > 0)
})

# Test 2: create_hex_grid (different sizes)
run_test("create_hex_grid - different cell sizes", {
  bbox <- c(18.5, 59.5, 19.5, 60.5)
  grid1 <- create_hex_grid(bbox, cell_size = 1)
  grid2 <- create_hex_grid(bbox, cell_size = 3)
  stopifnot(grid1$n_cells > grid2$n_cells)  # Smaller cells = more hexagons
})

# Test 3: print.hex_grid
run_test("print.hex_grid", {
  bbox <- c(18.5, 59.5, 19.5, 60.5)
  grid <- create_hex_grid(bbox, cell_size = 2)
  output <- capture.output(print(grid))
  stopifnot(length(output) > 0)
  stopifnot(any(grepl("Hexagonal Grid", output)))
})

# Test 4: validate_hex_grid
run_test("validate_hex_grid", {
  bbox <- c(18.5, 59.5, 19.5, 60.5)
  grid <- create_hex_grid(bbox, cell_size = 2)
  result <- validate_hex_grid(grid, verbose = FALSE)
  stopifnot(result == TRUE)
})

# Test 5: export_hex_grid
run_test("export_hex_grid", {
  bbox <- c(18.5, 59.5, 19.5, 60.5)
  grid <- create_hex_grid(bbox, cell_size = 2)
  export_hex_grid(grid, "output/test_grid_export.geojson", format = "geojson", overwrite = TRUE)
  stopifnot(file.exists("output/test_grid_export.geojson"))
})

# ==========================================================================
# COMPONENT 1.2: SPATIAL DATA MANAGEMENT (4 functions)
# ==========================================================================

cat("\n========================================\n")
cat("Component 1.2: Spatial Data Management\n")
cat("========================================\n\n")

# Test 6: import_spatial_species_data
run_test("import_spatial_species_data - CSV with coordinates", {
  bbox <- c(18.5, 59.5, 19.5, 60.5)
  grid <- create_hex_grid(bbox, cell_size = 2)
  species <- import_spatial_species_data(
    file = "output/test_species_data.csv",
    hex_grid = grid,
    format = "csv_coords",
    species_col = "species",
    lat_col = "latitude",
    lon_col = "longitude"
  )
  stopifnot(inherits(species, "spatial_species"))
  stopifnot(length(species$species_list$species) > 0)
})

# Test 7: print.spatial_species
run_test("print.spatial_species", {
  bbox <- c(18.5, 59.5, 19.5, 60.5)
  grid <- create_hex_grid(bbox, cell_size = 2)
  species <- import_spatial_species_data(
    "output/test_species_data.csv", grid, format = "csv_coords"
  )
  output <- capture.output(print(species))
  stopifnot(length(output) > 0)
  stopifnot(any(grepl("Spatial Species", output)))
})

# Test 8: validate_spatial_coverage
run_test("validate_spatial_coverage", {
  bbox <- c(18.5, 59.5, 19.5, 60.5)
  grid <- create_hex_grid(bbox, cell_size = 2)
  species <- import_spatial_species_data(
    "output/test_species_data.csv", grid, format = "csv_coords"
  )
  result <- validate_spatial_coverage(species, verbose = FALSE)
  stopifnot(is.list(result))
  stopifnot("empty_hexagons" %in% names(result))
})

# Test 9: export_spatial_species_data
run_test("export_spatial_species_data", {
  bbox <- c(18.5, 59.5, 19.5, 60.5)
  grid <- create_hex_grid(bbox, cell_size = 2)
  species <- import_spatial_species_data(
    "output/test_species_data.csv", grid, format = "csv_coords"
  )
  export_spatial_species_data(species, "output/test_species_export2.csv", format = "csv")
  stopifnot(file.exists("output/test_species_export2.csv"))
})

# ==========================================================================
# COMPONENT 1.3: NETWORK EXTRACTION (4 functions)
# ==========================================================================

cat("\n========================================\n")
cat("Component 1.3: Network Extraction\n")
cat("========================================\n\n")

# Test 10: extract_local_networks
run_test("extract_local_networks", {
  bbox <- c(18.5, 59.5, 19.5, 60.5)
  grid <- create_hex_grid(bbox, cell_size = 2)
  species <- import_spatial_species_data(
    "output/test_species_data.csv", grid, format = "csv_coords"
  )
  networks <- extract_local_networks(metaweb, species, min_species = 3, progress = FALSE)
  stopifnot(inherits(networks, "spatial_networks"))
  stopifnot(length(networks$local_networks) > 0)
})

# Test 11: print.spatial_networks
run_test("print.spatial_networks", {
  bbox <- c(18.5, 59.5, 19.5, 60.5)
  grid <- create_hex_grid(bbox, cell_size = 2)
  species <- import_spatial_species_data(
    "output/test_species_data.csv", grid, format = "csv_coords"
  )
  networks <- extract_local_networks(metaweb, species, min_species = 3, progress = FALSE)
  output <- capture.output(print(networks))
  stopifnot(length(output) > 0)
  stopifnot(any(grepl("Spatial Food Web", output)))
})

# Test 12: get_local_network
run_test("get_local_network", {
  bbox <- c(18.5, 59.5, 19.5, 60.5)
  grid <- create_hex_grid(bbox, cell_size = 2)
  species <- import_spatial_species_data(
    "output/test_species_data.csv", grid, format = "csv_coords"
  )
  networks <- extract_local_networks(metaweb, species, min_species = 3, progress = FALSE)
  hex_id <- networks$hex_ids[1]
  net <- get_local_network(networks, hex_id)
  stopifnot(igraph::is_igraph(net))
})

# Test 13: validate_local_networks
run_test("validate_local_networks", {
  bbox <- c(18.5, 59.5, 19.5, 60.5)
  grid <- create_hex_grid(bbox, cell_size = 2)
  species <- import_spatial_species_data(
    "output/test_species_data.csv", grid, format = "csv_coords"
  )
  networks <- extract_local_networks(metaweb, species, min_species = 3, progress = FALSE)
  result <- validate_local_networks(networks, verbose = FALSE)
  stopifnot(is.data.frame(result))
  stopifnot(nrow(result) > 0)
})

# ==========================================================================
# COMPONENT 1.4: METRIC CALCULATION (4 functions)
# ==========================================================================

cat("\n========================================\n")
cat("Component 1.4: Metric Calculation\n")
cat("========================================\n\n")

# Test 14: calculate_spatial_metrics
run_test("calculate_spatial_metrics", {
  bbox <- c(18.5, 59.5, 19.5, 60.5)
  grid <- create_hex_grid(bbox, cell_size = 2)
  species <- import_spatial_species_data(
    "output/test_species_data.csv", grid, format = "csv_coords"
  )
  networks <- extract_local_networks(metaweb, species, min_species = 3, progress = FALSE)
  metrics <- calculate_spatial_metrics(networks, metrics = c("richness", "links"), progress = FALSE)
  stopifnot(inherits(metrics, "spatial_metrics"))
  stopifnot("richness" %in% colnames(metrics$metrics))
})

# Test 15: print.spatial_metrics
run_test("print.spatial_metrics", {
  bbox <- c(18.5, 59.5, 19.5, 60.5)
  grid <- create_hex_grid(bbox, cell_size = 2)
  species <- import_spatial_species_data(
    "output/test_species_data.csv", grid, format = "csv_coords"
  )
  networks <- extract_local_networks(metaweb, species, min_species = 3, progress = FALSE)
  metrics <- calculate_spatial_metrics(networks, metrics = c("richness"), progress = FALSE)
  output <- capture.output(print(metrics))
  stopifnot(length(output) > 0)
  stopifnot(any(grepl("Spatial Metrics", output)))
})

# Test 16: export_spatial_metrics
run_test("export_spatial_metrics - CSV", {
  bbox <- c(18.5, 59.5, 19.5, 60.5)
  grid <- create_hex_grid(bbox, cell_size = 2)
  species <- import_spatial_species_data(
    "output/test_species_data.csv", grid, format = "csv_coords"
  )
  networks <- extract_local_networks(metaweb, species, min_species = 3, progress = FALSE)
  metrics <- calculate_spatial_metrics(networks, metrics = c("richness"), progress = FALSE)
  export_spatial_metrics(metrics, "output/test_metrics_export.csv", format = "csv")
  stopifnot(file.exists("output/test_metrics_export.csv"))
})

# Test 17: compare_spatial_metrics
run_test("compare_spatial_metrics", {
  bbox <- c(18.5, 59.5, 19.5, 60.5)
  grid <- create_hex_grid(bbox, cell_size = 2)
  species <- import_spatial_species_data(
    "output/test_species_data.csv", grid, format = "csv_coords"
  )
  networks <- extract_local_networks(metaweb, species, min_species = 3, progress = FALSE)
  metrics <- calculate_spatial_metrics(networks, metrics = c("richness"), progress = FALSE)
  result <- compare_spatial_metrics(metrics, metric = "richness", test = "none")
  stopifnot(is.list(result))
})

# ==========================================================================
# COMPONENT 1.5: SPATIAL VISUALIZATION (3 functions, 2 testable)
# ==========================================================================

cat("\n========================================\n")
cat("Component 1.5: Spatial Visualization\n")
cat("========================================\n\n")

# Test 18: map_spatial_metric
run_test("map_spatial_metric", {
  bbox <- c(18.5, 59.5, 19.5, 60.5)
  grid <- create_hex_grid(bbox, cell_size = 2)
  species <- import_spatial_species_data(
    "output/test_species_data.csv", grid, format = "csv_coords"
  )
  networks <- extract_local_networks(metaweb, species, min_species = 3, progress = FALSE)
  metrics <- calculate_spatial_metrics(networks, metrics = c("richness"), progress = FALSE)
  map <- map_spatial_metric(metrics, "richness", color_palette = "viridis")
  stopifnot(inherits(map, "leaflet"))
})

# Test 19: map_multiple_metrics
run_test("map_multiple_metrics", {
  bbox <- c(18.5, 59.5, 19.5, 60.5)
  grid <- create_hex_grid(bbox, cell_size = 2)
  species <- import_spatial_species_data(
    "output/test_species_data.csv", grid, format = "csv_coords"
  )
  networks <- extract_local_networks(metaweb, species, min_species = 3, progress = FALSE)
  metrics <- calculate_spatial_metrics(networks, metrics = c("richness", "links"), progress = FALSE)
  map <- map_multiple_metrics(metrics, metrics = c("richness", "links"))
  stopifnot(inherits(map, "leaflet"))
})

# ==========================================================================
# SUMMARY
# ==========================================================================

cat("\n========================================\n")
cat("  Test Summary\n")
cat("========================================\n\n")

cat(sprintf("Total Tests:  %d\n", test_count))
cat(sprintf("Passed:       %d (%.1f%%)\n", pass_count, 100 * pass_count / test_count))
cat(sprintf("Failed:       %d (%.1f%%)\n", fail_count, 100 * fail_count / test_count))
cat("\n")

if (fail_count > 0) {
  cat("Failed Tests:\n")
  for (name in names(test_results)) {
    if (test_results[[name]]$status == "FAIL") {
      cat(sprintf("  - %s: %s\n", name, test_results[[name]]$error))
    }
  }
  cat("\n")
}

# Save results
saveRDS(test_results, "output/test_results.rds")

if (fail_count == 0) {
  cat("========================================\n")
  cat("  ✓ ALL TESTS PASSED!\n")
  cat("========================================\n\n")
  cat("Phase 1 Backend: 100% FUNCTIONAL\n\n")
  quit(status = 0)
} else {
  cat("========================================\n")
  cat("  ⚠ SOME TESTS FAILED\n")
  cat("========================================\n\n")
  quit(status = 1)
}
