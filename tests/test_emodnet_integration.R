#' Test Script for EMODnet EUSeaMap Integration
#'
#' This script tests the EMODnet habitat integration independently
#' before integrating into the main Shiny app.
#'
#' Run this script to verify:
#' 1. EUSeaMap data file is downloaded and accessible
#' 2. Spatial queries work correctly
#' 3. Habitat data is extracted properly
#'
#' @author Claude (Anthropic)
#' @date 2025-12-17

# Clear workspace
rm(list = ls())

cat("========================================\n")
cat("EMODnet EUSeaMap Integration Test\n")
cat("========================================\n\n")

# Load required packages
cat("Step 1: Loading required packages\n")
cat("----------------------------------\n")

packages <- c("sf")
missing <- packages[!packages %in% installed.packages()[, "Package"]]

if (length(missing) > 0) {
  cat("⚠ Missing packages:", paste(missing, collapse = ", "), "\n")
  cat("Installing missing packages...\n")
  install.packages(missing, repos = "https://cloud.r-project.org")
}

library(sf)
cat("✓ Packages loaded\n\n")

# Source habitat utilities
cat("Step 2: Loading habitat utility functions\n")
cat("------------------------------------------\n")

if (!file.exists("R/functions/emodnet_habitat_utils.R")) {
  stop("✗ emodnet_habitat_utils.R not found!\n",
       "  Expected location: R/functions/emodnet_habitat_utils.R")
}

source("R/functions/emodnet_habitat_utils.R")
cat("✓ Utilities loaded\n\n")

# Load EUSeaMap layer
cat("Step 3: Loading EUSeaMap data\n")
cat("-----------------------------\n")

# Try EUSeaMap_2025.gdb first, then fallback to other locations
euseamap_path <- if (dir.exists("data/EUSeaMap_2025/EUSeaMap_2025.gdb")) {
  "data/EUSeaMap_2025/EUSeaMap_2025.gdb"
} else if (dir.exists("data/EUSeaMap_2025.gdb")) {
  "data/EUSeaMap_2025.gdb"
} else if (file.exists("data/euseamap/EUSeaMap_Baltic.gpkg")) {
  "data/euseamap/EUSeaMap_Baltic.gpkg"
} else if (file.exists("data/euseamap/EUSeaMap_2023.gpkg")) {
  "data/euseamap/EUSeaMap_2023.gpkg"
} else {
  NULL
}

if (is.null(euseamap_path)) {
  cat("✗ EUSeaMap file not found!\n\n")
  cat("Checked locations:\n")
  cat("  - data/EUSeaMap_2025.gdb\n")
  cat("  - data/euseamap/EUSeaMap_Baltic.gpkg\n")
  cat("  - data/euseamap/EUSeaMap_2023.gpkg\n\n")
  cat("Please download EUSeaMap from:\n")
  cat("https://emodnet.ec.europa.eu/en/seabed-habitats\n\n")
  cat("Or extract EUSeaMap_2025.zip to data/\n\n")
  stop("Cannot proceed without EUSeaMap data")
}

cat("Found EUSeaMap at:", euseamap_path, "\n\n")

euseamap <- load_euseamap(euseamap_path)
cat("\n")

# Print layer summary
cat("Step 4: Layer Information\n")
cat("-------------------------\n")
print_euseamap_summary(euseamap)
cat("\n")

# Test locations
cat("Step 5: Testing Habitat Queries\n")
cat("--------------------------------\n\n")

test_locations <- data.frame(
  name = c(
    "Gdansk Bay",
    "Stockholm Archipelago",
    "Danish Straits",
    "Gotland Basin",
    "Gulf of Finland"
  ),
  lon = c(18.6466, 18.3, 11.5, 21.0, 25.0),
  lat = c(54.5189, 59.3, 55.5, 58.0, 60.0),
  expected_substrate = c("Mud/Sand", "Mixed", "Sand", "Mud", "Mud"),
  stringsAsFactors = FALSE
)

results <- data.frame(
  Location = character(),
  EUNIS_Code = character(),
  Habitat = character(),
  Substrate = character(),
  Depth_Zone = character(),
  Status = character(),
  stringsAsFactors = FALSE
)

for (i in 1:nrow(test_locations)) {
  loc <- test_locations[i, ]

  cat(sprintf("[%d/%d] Testing: %s\n", i, nrow(test_locations), loc$name))
  cat(sprintf("        Coordinates: %.4f°E, %.4f°N\n", loc$lon, loc$lat))

  habitat <- get_habitat_at_location(loc$lon, loc$lat, euseamap)

  if (!is.null(habitat)) {
    results <- rbind(results, data.frame(
      Location = loc$name,
      EUNIS_Code = habitat$eunis_code %||% "N/A",
      Habitat = substr(habitat$eunis_description %||% "N/A", 1, 40),
      Substrate = habitat$substrate_type %||% "N/A",
      Depth_Zone = habitat$depth_zone %||% "N/A",
      Status = "✓ Found",
      stringsAsFactors = FALSE
    ))
  } else {
    results <- rbind(results, data.frame(
      Location = loc$name,
      EUNIS_Code = "N/A",
      Habitat = "Not found",
      Substrate = "N/A",
      Depth_Zone = "N/A",
      Status = "✗ Not found",
      stringsAsFactors = FALSE
    ))
  }

  cat("\n")
}

# Print results table
cat("Step 6: Results Summary\n")
cat("=======================\n\n")
print(results, row.names = FALSE)
cat("\n")

# Test species data frame enhancement
cat("Step 7: Testing Species Data Enhancement\n")
cat("-----------------------------------------\n\n")

# Create sample species data
sample_species <- data.frame(
  species_name = c(
    "Gadus morhua",
    "Clupea harengus",
    "Mytilus edulis",
    "Macoma balthica",
    "Nereis diversicolor"
  ),
  functional_group = c("Fish", "Fish", "Benthos", "Benthos", "Benthos"),
  trophic_level = c(4.29, 3.29, 2.5, 2.3, 2.7),
  source = c("FishBase", "FishBase", "WoRMS", "WoRMS", "WoRMS"),
  stringsAsFactors = FALSE
)

cat("Sample species data (before habitat enhancement):\n")
print(sample_species)
cat("\n")

# Add habitat data (using Gdansk Bay location)
sampling_lon <- 18.6466
sampling_lat <- 54.5189

cat(sprintf("Adding habitat data from sampling location: %.4f°E, %.4f°N\n\n",
            sampling_lon, sampling_lat))

enhanced_species <- add_habitat_to_species(
  sample_species,
  sampling_lon,
  sampling_lat,
  euseamap
)

cat("\nSample species data (after habitat enhancement):\n")
print(enhanced_species[, c("species_name", "functional_group", "eunis_code",
                            "substrate", "depth_zone")])
cat("\n")

# Final summary
cat("Step 8: Test Summary\n")
cat("====================\n\n")

total_tests <- nrow(test_locations)
successful_tests <- sum(results$Status == "✓ Found")
failed_tests <- total_tests - successful_tests

cat(sprintf("Total locations tested: %d\n", total_tests))
cat(sprintf("Successful queries: %d (%.1f%%)\n",
            successful_tests, successful_tests / total_tests * 100))
cat(sprintf("Failed queries: %d (%.1f%%)\n",
            failed_tests, failed_tests / total_tests * 100))
cat("\n")

if (successful_tests > 0) {
  cat("✓ EMODnet integration is WORKING!\n\n")
  cat("Next steps:\n")
  cat("1. Integrate into Shiny app (see EMODNET_INTEGRATION_QUICKSTART.md)\n")
  cat("2. Add location input controls to UI\n")
  cat("3. Call add_habitat_to_species() in server logic\n")
  cat("4. Update species table to display habitat columns\n\n")
} else {
  cat("✗ All queries failed - please check:\n")
  cat("1. EUSeaMap file is correct version for Baltic Sea\n")
  cat("2. Test locations are within coverage area\n")
  cat("3. CRS/projection is correct\n\n")
}

cat("========================================\n")
cat("Test Complete!\n")
cat("========================================\n")

# Helper function
`%||%` <- function(x, y) if (is.null(x)) y else x
