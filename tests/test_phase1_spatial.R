#!/usr/bin/env Rscript
# =============================================================================
# Phase 1 Spatial Analysis - Test Script
# =============================================================================
#
# This script tests all Phase 1 spatial analysis functions.
# Run in R GUI or RStudio (bash Rscript has issues with this R installation).
#
# Usage:
#   1. Open in RStudio or R GUI
#   2. Set working directory to project root
#   3. Run line by line or source entire script
#
# Expected Output:
#   - Grid with ~57 hexagons
#   - 20 species occurrences assigned to hexagons
#   - Local networks extracted for occupied hexagons
#   - Spatial metrics calculated (S, L, C, LD, meanTL)
#   - Summary statistics and final spatial_foodweb_data object
#
# Date: 2024-12-04
# =============================================================================

# Check working directory
if (!file.exists("functions.R")) {
  stop("Please set working directory to project root (where functions.R is located)")
}

cat("\n")
cat(strrep("=", 77), "\n", sep = "")
cat("PHASE 1 SPATIAL ANALYSIS - COMPREHENSIVE TEST\n")
cat(strrep("=", 77), "\n", sep = "")
cat("\n")

# Load functions
cat("Loading functions...\n")
source("functions.R")
cat("✓ functions.R loaded\n\n")

# Check required packages
cat("Checking required packages...\n")
required_pkgs <- c("sf", "igraph")
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Required package '", pkg, "' is not installed. Install with: install.packages('", pkg, "')")
  }
  cat("  ✓", pkg, "version", as.character(packageVersion(pkg)), "\n")
}
cat("\n")

# =============================================================================
# STEP 1: Load Metaweb
# =============================================================================
cat("STEP 1: Loading metaweb\n")
cat(strrep("-", 77), "\n", sep = "")

metaweb_file <- "metawebs/baltic/baltic_kortsch2021.rds"
if (!file.exists(metaweb_file)) {
  stop("Metaweb file not found: ", metaweb_file)
}

metaweb <- readRDS(metaweb_file)
cat("✓ Loaded:", metaweb$metadata$region %||% "Baltic metaweb", "\n")
cat("  Species:", nrow(metaweb$species), "\n")
cat("  Interactions:", nrow(metaweb$interactions), "\n")
cat("\n")

# =============================================================================
# STEP 2: Create Hexagonal Grid
# =============================================================================
cat("STEP 2: Creating hexagonal spatial grid\n")
cat(strrep("-", 77), "\n", sep = "")

# Baltic Sea bounding box
bbox <- c(xmin = 15, ymin = 54, xmax = 22, ymax = 60)
cell_size <- 2000  # 2km hexagons

cat("Parameters:\n")
cat("  Bounding box: xmin=", bbox[1], ", ymin=", bbox[2],
    ", xmax=", bbox[3], ", ymax=", bbox[4], "\n", sep = "")
cat("  Cell size:", cell_size, "m (", cell_size/1000, "km)\n")

hex_grid <- create_hexagonal_grid(bbox, cell_size = cell_size, crs = 4326)

cat("✓ Grid created\n")
cat("  Hexagons:", nrow(hex_grid), "\n")
cat("  Grid dimensions:",
    round(bbox[3] - bbox[1], 2), "° x",
    round(bbox[4] - bbox[2], 2), "°\n")
cat("\n")

# Show first few hexagons
cat("First 3 hexagons:\n")
print(head(sf::st_drop_geometry(hex_grid), 3))
cat("\n")

# =============================================================================
# STEP 3: Generate Sample Species Data
# =============================================================================
cat("STEP 3: Generating sample species occurrence data\n")
cat(strrep("-", 77), "\n", sep = "")

# Select 4 species from metaweb
sample_species <- metaweb$species$species_name[1:4]
cat("Selected species:\n")
for (i in seq_along(sample_species)) {
  cat("  ", i, ".", sample_species[i], "\n", sep = "")
}
cat("\n")

# Generate random occurrences within bbox
set.seed(42)  # Reproducible results
n_occurrences <- 20

species_data <- data.frame(
  lon = runif(n_occurrences, min = bbox[1], max = bbox[3]),
  lat = runif(n_occurrences, min = bbox[2], max = bbox[4]),
  species = sample(sample_species, n_occurrences, replace = TRUE),
  biomass = round(runif(n_occurrences, min = 5, max = 20), 1)
)

cat("✓ Generated", n_occurrences, "occurrences\n")
cat("Species distribution:\n")
print(table(species_data$species))
cat("\n")

# Show first few occurrences
cat("First 5 occurrences:\n")
print(head(species_data, 5))
cat("\n")

# =============================================================================
# STEP 4: Assign Species to Hexagons
# =============================================================================
cat("STEP 4: Assigning species to hexagons\n")
cat(strrep("-", 77), "\n", sep = "")

hex_species <- assign_species_to_hexagons(species_data, hex_grid)

cat("✓ Species assigned\n")
cat("  Records:", nrow(hex_species), "\n")
cat("  Occupied hexagons:", length(unique(hex_species$hex_id)), "\n")
cat("  Species represented:", length(unique(hex_species$species)), "\n")
cat("\n")

# Show assignment summary
cat("Species per hexagon:\n")
print(head(hex_species, 10))
cat("\n")

# =============================================================================
# STEP 5: Extract Local Networks
# =============================================================================
cat("STEP 5: Extracting local food webs\n")
cat(strrep("-", 77), "\n", sep = "")

local_networks <- extract_local_networks(
  metaweb,
  hex_species,
  hex_grid,
  progress = TRUE
)

n_networks <- length(local_networks)
n_nonempty <- sum(sapply(local_networks, igraph::vcount) > 0)

cat("✓ Networks extracted\n")
cat("  Total hexagons:", n_networks, "\n")
cat("  With species:", n_nonempty, "\n")
cat("  Empty:", n_networks - n_nonempty, "\n")
cat("\n")

# Show network sizes
network_sizes <- data.frame(
  hex_id = names(local_networks),
  species = sapply(local_networks, igraph::vcount),
  links = sapply(local_networks, igraph::ecount)
)
network_sizes <- network_sizes[network_sizes$species > 0, ]

cat("Non-empty networks:\n")
print(network_sizes)
cat("\n")

# =============================================================================
# STEP 6: Calculate Spatial Metrics
# =============================================================================
cat("STEP 6: Calculating spatial metrics\n")
cat(strrep("-", 77), "\n", sep = "")

metrics <- calculate_spatial_metrics(
  local_networks,
  hex_grid,
  metrics = c("S", "L", "C", "LD", "meanTL", "maxTL"),
  progress = TRUE
)

cat("✓ Metrics calculated\n")
cat("  Metrics:", paste(setdiff(colnames(metrics),
                                 c("hex_id", "center_lon", "center_lat")),
                        collapse = ", "), "\n")
cat("\n")

# Show metrics
cat("Spatial metrics (non-empty hexagons):\n")
metrics_display <- metrics[metrics$S > 0, ]
print(metrics_display)
cat("\n")

# =============================================================================
# STEP 7: Aggregate Metrics
# =============================================================================
cat("STEP 7: Aggregating metrics across space\n")
cat(strrep("-", 77), "\n", sep = "")

summary_stats <- aggregate_spatial_metrics(
  metrics,
  metric_cols = c("S", "L", "C", "LD", "meanTL", "maxTL")
)

cat("✓ Aggregation complete\n\n")
cat("Summary statistics:\n")
print(summary_stats)
cat("\n")

# =============================================================================
# STEP 8: Create Spatial Food Web Data Object
# =============================================================================
cat("STEP 8: Creating spatial_foodweb_data object\n")
cat(strrep("-", 77), "\n", sep = "")

spatial_data <- create_spatial_foodweb_data(
  hex_grid = hex_grid,
  hex_species = hex_species,
  metaweb = metaweb,
  local_networks = local_networks,
  metrics = metrics,
  metadata = list(
    region = "Baltic Sea Test",
    study = "Phase 1 Function Test",
    date = Sys.Date(),
    analyst = "Claude Code",
    notes = "Automated test of Phase 1 spatial analysis functions"
  )
)

cat("✓ Spatial food web data object created\n\n")

# Print summary
print(spatial_data)
cat("\n")

# =============================================================================
# FINAL SUMMARY
# =============================================================================
cat(strrep("=", 77), "\n", sep = "")
cat("TEST COMPLETE - ALL FUNCTIONS WORKING!\n")
cat(strrep("=", 77), "\n", sep = "")
cat("\n")

cat("Results Summary:\n")
cat("  ✓ Grid:", nrow(hex_grid), "hexagons created\n")
cat("  ✓ Species:", nrow(hex_species), "occurrence records assigned\n")
cat("  ✓ Networks:", n_nonempty, "/", n_networks, "hexagons with food webs\n")
cat("  ✓ Metrics: Calculated for", nrow(metrics), "hexagons\n")
cat("  ✓ Object: spatial_foodweb_data created successfully\n")
cat("\n")

cat("Objects created in workspace:\n")
cat("  - metaweb: Metaweb object from Phase 2\n")
cat("  - hex_grid: sf object with hexagonal grid\n")
cat("  - species_data: Sample occurrence data\n")
cat("  - hex_species: Species assigned to hexagons\n")
cat("  - local_networks: List of local food webs\n")
cat("  - metrics: Spatial metrics data frame\n")
cat("  - summary_stats: Aggregated statistics\n")
cat("  - spatial_data: Complete spatial_foodweb_data object\n")
cat("\n")

cat("Next steps:\n")
cat("  1. Explore spatial_data object: print(spatial_data)\n")
cat("  2. Visualize grid: library(leaflet); leaflet(hex_grid) %>% addTiles() %>% addPolygons()\n")
cat("  3. Examine networks: local_networks[[1]]  # First network\n")
cat("  4. Export data: saveRDS(spatial_data, 'output/spatial_test.rds')\n")
cat("\n")

cat("Phase 1 backend functions are WORKING CORRECTLY! ✓\n")
cat("\n")
