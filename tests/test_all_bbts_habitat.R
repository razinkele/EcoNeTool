#!/usr/bin/env Rscript
#' Test Habitat Loading for All BBTs
#'
#' Tests habitat loading with all available BBT polygons
#' to identify which ones work and which ones fail
#'
#' @author Claude (Anthropic)
#' @date 2025-12-19

library(sf)

cat("\n")
cat("========================================\n")
cat("  Testing All BBTs for Habitat Loading\n")
cat("========================================\n\n")

# Load functions
source("R/functions/emodnet_habitat_utils.R")
source("R/functions/euseamap_regional_config.R")

# Configuration
gdb_path <- "data/EUSeaMap_2025/EUSeaMap_2025.gdb"
bbt_file <- "data/BBT.geojson"

# Load all BBTs
sf::sf_use_s2(FALSE)
bbt_data <- sf::st_read(bbt_file, quiet = TRUE)
sf::sf_use_s2(TRUE)

cat(sprintf("Found %d BBT polygons\n\n", nrow(bbt_data)))

# Test results
results <- list()

# Test each BBT
for (i in 1:nrow(bbt_data)) {
  bbt_name <- bbt_data$Name[i]

  cat(sprintf("\n[%d/%d] Testing BBT: %s\n", i, nrow(bbt_data), bbt_name))
  cat(strrep("-", 50), "\n")

  result <- list(
    bbt = bbt_name,
    success = FALSE,
    buffer_worked = NA,
    features = 0,
    load_time = 0,
    error = NA
  )

  tryCatch({
    # Get BBT polygon
    selected_bbt <- bbt_data[i, ]
    selected_bbt <- sf::st_zm(selected_bbt, drop = TRUE, what = "ZM")

    bbox <- sf::st_bbox(selected_bbt)
    cat(sprintf("  Bbox: [%.2f, %.2f] to [%.2f, %.2f]\n",
                bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"]))

    # Try different buffer sizes
    buffer_sizes <- c(0.3, 0.5, 0.7, 1.0)

    for (buffer in buffer_sizes) {
      cat(sprintf("  Trying %.1f° buffer... ", buffer))

      start_time <- Sys.time()

      load_result <- tryCatch({
        euseamap <- load_regional_euseamap(
          bbt_name = bbt_name,
          study_area_sf = selected_bbt,
          buffer_degrees = buffer,
          path = gdb_path
        )

        end_time <- Sys.time()
        load_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

        list(
          success = TRUE,
          features = nrow(euseamap),
          time = load_time
        )
      }, error = function(e) {
        list(
          success = FALSE,
          error = conditionMessage(e)
        )
      })

      if (load_result$success) {
        cat(sprintf("✓ %d features in %.2fs\n", load_result$features, load_result$time))
        result$success <- TRUE
        result$buffer_worked <- buffer
        result$features <- load_result$features
        result$load_time <- load_result$time
        break
      } else {
        cat(sprintf("✗ %s\n", load_result$error))
      }
    }

    # If all buffers failed, try minimal bbox
    if (!result$success) {
      cat("  Trying minimal bbox... ")

      start_time <- Sys.time()

      minimal_result <- tryCatch({
        # Use center of bbox as minimal area
        center_lon <- mean(c(bbox["xmin"], bbox["xmax"]))
        center_lat <- mean(c(bbox["ymin"], bbox["ymax"]))

        euseamap <- load_regional_euseamap(
          custom_bbox = c(center_lon - 0.5, center_lat - 0.5,
                          center_lon + 0.5, center_lat + 0.5),
          path = gdb_path
        )

        end_time <- Sys.time()
        load_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

        list(
          success = TRUE,
          features = nrow(euseamap),
          time = load_time
        )
      }, error = function(e) {
        list(
          success = FALSE,
          error = conditionMessage(e)
        )
      })

      if (minimal_result$success) {
        cat(sprintf("✓ %d features in %.2fs (minimal bbox)\n",
                    minimal_result$features, minimal_result$time))
        result$success <- TRUE
        result$buffer_worked <- "minimal"
        result$features <- minimal_result$features
        result$load_time <- minimal_result$time
      } else {
        cat(sprintf("✗ %s\n", minimal_result$error))
        result$error <- minimal_result$error
      }
    }

  }, error = function(e) {
    cat(sprintf("  ✗ ERROR: %s\n", conditionMessage(e)))
    result$error <<- conditionMessage(e)
  })

  results[[bbt_name]] <- result
}

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n\n")
cat("========================================\n")
cat("  Summary\n")
cat("========================================\n\n")

# Count successes and failures
n_success <- sum(sapply(results, function(x) x$success))
n_failed <- length(results) - n_success

cat(sprintf("Total BBTs tested: %d\n", length(results)))
cat(sprintf("✓ Successful: %d (%.1f%%)\n", n_success, 100 * n_success / length(results)))
cat(sprintf("✗ Failed: %d (%.1f%%)\n\n", n_failed, 100 * n_failed / length(results)))

# Show successful BBTs
if (n_success > 0) {
  cat("Successful BBTs:\n")
  cat(strrep("-", 70), "\n")
  cat(sprintf("%-20s %-12s %-10s %-10s\n", "BBT", "Buffer", "Features", "Time"))
  cat(strrep("-", 70), "\n")

  for (name in names(results)) {
    r <- results[[name]]
    if (r$success) {
      cat(sprintf("%-20s %-12s %-10d %.2fs\n",
                  substr(name, 1, 20),
                  r$buffer_worked,
                  r$features,
                  r$load_time))
    }
  }
  cat("\n")
}

# Show failed BBTs
if (n_failed > 0) {
  cat("Failed BBTs:\n")
  cat(strrep("-", 70), "\n")
  cat(sprintf("%-20s %s\n", "BBT", "Error"))
  cat(strrep("-", 70), "\n")

  for (name in names(results)) {
    r <- results[[name]]
    if (!r$success) {
      error_msg <- substr(r$error, 1, 45)
      cat(sprintf("%-20s %s\n", substr(name, 1, 20), error_msg))
    }
  }
  cat("\n")
}

# Buffer effectiveness
cat("Buffer Effectiveness:\n")
cat(strrep("-", 50), "\n")

buffer_counts <- table(sapply(results, function(x) {
  if (x$success) as.character(x$buffer_worked) else "failed"
}))

for (b in names(buffer_counts)) {
  cat(sprintf("  %-12s: %d BBTs\n", b, buffer_counts[b]))
}

cat("\n")

# Recommendations
cat("Recommendations:\n")
cat(strrep("-", 50), "\n")

if (n_failed > 0) {
  cat("⚠️  Some BBTs failed to load habitat data\n")
  cat("\nSuggested fixes:\n")
  cat("  1. Use smaller buffer sizes (0.2° - 0.3°)\n")
  cat("  2. Try even smaller minimal bbox (0.3° x 0.3°)\n")
  cat("  3. Check specific regions for data quality issues\n")
  cat("  4. Consider pre-processing problematic areas\n")
} else {
  cat("✅ All BBTs successfully loaded habitat data!\n")
  cat("   Current buffer strategy is working well.\n")
}

cat("\n")

if (n_failed == 0) {
  quit(status = 0)
} else {
  quit(status = 1)
}
