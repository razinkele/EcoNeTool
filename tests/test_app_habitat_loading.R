#!/usr/bin/env Rscript
#' Test App Habitat Loading Fix
#'
#' Validates that habitat loading works with BBT selection
#' Simulates the app workflow to ensure no geometry errors
#'
#' @author Claude (Anthropic)
#' @date 2025-12-19

library(sf)

cat("\n")
cat("========================================\n")
cat("  App Habitat Loading Test\n")
cat("========================================\n\n")

# Load functions
source("R/functions/emodnet_habitat_utils.R")
source("R/functions/euseamap_regional_config.R")

# Configuration
gdb_path <- "data/EUSeaMap_2025/EUSeaMap_2025.gdb"
bbt_file <- "data/BBT.geojson"

test_passed <- TRUE

# ============================================================================
# TEST: Simulate app workflow
# ============================================================================

cat("Simulating app workflow:\n")
cat("1. User selects BBT from dropdown\n")
cat("2. User enables habitat checkbox\n")
cat("3. Habitat loads with precise bbox\n\n")

tryCatch({
  # Step 1: Simulate BBT selection (what app does in observeEvent)
  cat("Step 1: Loading BBT polygon (Lithuanian)...\n")
  sf::sf_use_s2(FALSE)
  bbt_data <- sf::st_read(bbt_file, quiet = TRUE)
  selected_bbt <- bbt_data[bbt_data$Name == "Lithuanian", ]
  selected_bbt <- sf::st_zm(selected_bbt, drop = TRUE, what = "ZM")
  sf::sf_use_s2(TRUE)

  cat(sprintf("  ✓ BBT loaded: %s\n", selected_bbt$Name))
  cat(sprintf("  ✓ Bbox: [%.2f, %.2f] to [%.2f, %.2f]\n",
              sf::st_bbox(selected_bbt)["xmin"],
              sf::st_bbox(selected_bbt)["ymin"],
              sf::st_bbox(selected_bbt)["xmax"],
              sf::st_bbox(selected_bbt)["ymax"]))

  # Step 2: Simulate habitat loading (what app does when checkbox enabled)
  cat("\nStep 2: Loading habitat with precise bbox...\n")

  start_time <- Sys.time()

  euseamap <- load_regional_euseamap(
    bbt_name = "Lithuanian",
    study_area_sf = selected_bbt,  # ← KEY: Pass the BBT polygon!
    buffer_degrees = 1.0,
    path = gdb_path
  )

  end_time <- Sys.time()
  load_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  cat(sprintf("  ✓ Habitat loaded: %d polygons\n", nrow(euseamap)))
  cat(sprintf("  ✓ Region: %s\n", attr(euseamap, "region")))
  cat(sprintf("  ✓ Load time: %.2f seconds\n", load_time))
  cat(sprintf("  ✓ Memory: %.1f MB\n", as.numeric(object.size(euseamap)) / 1024^2))

  # Validate
  if (nrow(euseamap) > 0 && nrow(euseamap) < 10000) {
    cat("\n✅ SUCCESS: Habitat loaded with precise bbox (no geometry errors!)\n")
  } else if (nrow(euseamap) > 100000) {
    cat("\n⚠️  WARNING: Loaded too many features (", nrow(euseamap), "), may be using regional bbox\n")
    test_passed <- FALSE
  } else {
    cat("\n⚠️  WARNING: No habitat data loaded\n")
    test_passed <- FALSE
  }

}, error = function(e) {
  cat(sprintf("\n❌ ERROR: %s\n", conditionMessage(e)))
  test_passed <<- FALSE
})

# ============================================================================
# TEST: Simulate fallback scenario (no BBT loaded)
# ============================================================================

cat("\n")
cat("========================================\n")
cat("  Fallback Scenario Test\n")
cat("========================================\n\n")

cat("Simulating: User enables habitat without selecting BBT\n\n")

tryCatch({
  # Step 1: No BBT selected, use default bbox
  cat("Step 1: No BBT selected, using default bbox...\n")

  start_time <- Sys.time()

  euseamap <- load_regional_euseamap(
    bbt_name = NULL,
    custom_bbox = c(20, 55, 21, 56),  # Small default bbox
    study_area_sf = NULL,
    path = gdb_path
  )

  end_time <- Sys.time()
  load_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  cat(sprintf("  ✓ Habitat loaded: %d polygons\n", nrow(euseamap)))
  cat(sprintf("  ✓ Load time: %.2f seconds\n", load_time))

  if (nrow(euseamap) > 0 && nrow(euseamap) < 1000) {
    cat("\n✅ SUCCESS: Fallback bbox works (small area loaded)\n")
  } else {
    cat("\n⚠️  WARNING: Unexpected number of features:", nrow(euseamap), "\n")
  }

}, error = function(e) {
  cat(sprintf("\n❌ ERROR: %s\n", conditionMessage(e)))
  test_passed <<- FALSE
})

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n")
cat("========================================\n")
cat("  Test Summary\n")
cat("========================================\n\n")

if (test_passed) {
  cat("✅ ALL TESTS PASSED\n\n")
  cat("App habitat loading should work without errors!\n")
  cat("\nNext steps:\n")
  cat("1. Start the app: Rscript run_app.R\n")
  cat("2. Go to Spatial Analysis tab\n")
  cat("3. Select BBT (e.g., Lithuanian)\n")
  cat("4. Go to Habitat Data tab\n")
  cat("5. Enable habitat checkbox\n")
  cat("6. Should load in < 1 second with no errors\n")
  quit(status = 0)
} else {
  cat("⚠️  SOME TESTS FAILED\n\n")
  cat("Please review the errors above.\n")
  quit(status = 1)
}
