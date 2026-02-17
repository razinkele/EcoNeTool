#!/usr/bin/env Rscript
#' Create Regional EUSeaMap Files
#'
#' Splits the large EUSeaMap_2025.gdb into smaller regional GeoPackage files
#' for faster loading and reduced memory usage.
#'
#' This script:
#' 1. Loads the full EUSeaMap GDB (one-time operation)
#' 2. Splits it into 5 regional subsets (Baltic, North Sea, Atlantic, Arctic, Med)
#' 3. Saves each region as a separate .gpkg file
#' 4. Validates the output files
#'
#' @author Claude (Anthropic)
#' @date 2025-12-18

library(sf)

cat("\n")
cat("========================================\n")
cat("  EUSeaMap Regional File Creator\n")
cat("========================================\n\n")

# Source regional configuration
source("R/functions/euseamap_regional_config.R")

# Configuration
gdb_path <- "data/EUSeaMap_2025/EUSeaMap_2025.gdb"
output_dir <- "data/EUSeaMap_2025/regional"
layer_name <- NULL  # Auto-detect

# Check if GDB exists
if (!dir.exists(gdb_path)) {
  stop(sprintf("EUSeaMap GDB not found at: %s\nPlease ensure the file exists.", gdb_path))
}

# Create output directory
if (!dir.exists(output_dir)) {
  cat("📁 Creating output directory:", output_dir, "\n")
  dir.create(output_dir, recursive = TRUE)
}

# Step 1: Load full dataset
cat("\n🔄 Step 1: Loading full EUSeaMap dataset...\n")
cat("   This may take 30-60 seconds (one-time operation)\n\n")

# Detect layers
available_layers <- sf::st_layers(gdb_path)
if (is.null(layer_name)) {
  layer_name <- available_layers$name[1]
  cat("   Using layer:", layer_name, "\n")
}

# Load full dataset
full_data <- tryCatch({
  sf::st_read(gdb_path, layer = layer_name, quiet = TRUE)
}, error = function(e) {
  stop("Failed to read GDB: ", conditionMessage(e))
})

cat(sprintf("   ✓ Loaded %s polygons\n", format(nrow(full_data), big.mark = ",")))
cat(sprintf("   ✓ CRS: %s\n", sf::st_crs(full_data)$input))
cat(sprintf("   ✓ Size: %.1f MB in memory\n", object.size(full_data) / 1024^2))

# Ensure CRS is set
if (is.na(sf::st_crs(full_data))) {
  warning("No CRS found, assuming EPSG:3035 (LAEA Europe)")
  sf::st_crs(full_data) <- 3035
}

# Step 2: Split by region
cat("\n🔄 Step 2: Splitting into regional subsets...\n\n")

# Disable s2 for geometry operations
old_s2 <- sf::sf_use_s2()
sf::sf_use_s2(FALSE)
on.exit(sf::sf_use_s2(old_s2))

# Get regional bboxes
regions <- c("baltic", "north_sea", "atlantic", "arctic", "mediterranean")
bboxes <- get_regional_bboxes()

# Storage for results
results <- list()

for (region in regions) {
  cat(sprintf("   Processing %s...\n", toupper(region)))

  # Get bbox for this region
  bbox_coords <- bboxes[[region]]

  # Create bbox polygon in WGS84
  bbox_poly <- sf::st_as_sfc(sf::st_bbox(c(
    xmin = bbox_coords[1],
    ymin = bbox_coords[2],
    xmax = bbox_coords[3],
    ymax = bbox_coords[4]
  ), crs = 4326))

  # Transform bbox to match data CRS
  bbox_poly_transformed <- sf::st_transform(bbox_poly, sf::st_crs(full_data))

  # Filter data to bbox
  regional_data <- tryCatch({
    suppressMessages({
      suppressWarnings({
        # Remove invalid geometries before filtering
        valid_data <- full_data[sf::st_is_valid(full_data), ]

        # Filter to bbox
        sf::st_filter(valid_data, bbox_poly_transformed, .predicate = sf::st_intersects)
      })
    })
  }, error = function(e) {
    warning(sprintf("Failed to filter %s: %s", region, conditionMessage(e)))
    return(NULL)
  })

  if (is.null(regional_data) || nrow(regional_data) == 0) {
    cat(sprintf("     ⚠ No data found for %s region\n", toupper(region)))
    next
  }

  # Output file path
  outfile <- file.path(output_dir, sprintf("EUSeaMap_%s.gpkg", region))

  # Write GeoPackage
  tryCatch({
    sf::st_write(regional_data, outfile, delete_dsn = TRUE, quiet = TRUE)

    # Get file size
    file_size_mb <- file.info(outfile)$size / 1024^2

    # Store results
    results[[region]] <- list(
      polygons = nrow(regional_data),
      file_size_mb = file_size_mb,
      path = outfile
    )

    cat(sprintf("     ✓ %s polygons → %s (%.1f MB)\n",
                format(nrow(regional_data), big.mark = ","),
                basename(outfile),
                file_size_mb))

  }, error = function(e) {
    warning(sprintf("Failed to write %s: %s", region, conditionMessage(e)))
  })
}

# Step 3: Validation
cat("\n🔄 Step 3: Validating output files...\n\n")

validation_ok <- TRUE

for (region in names(results)) {
  result <- results[[region]]

  # Check file exists
  if (!file.exists(result$path)) {
    cat(sprintf("   ✗ %s: File not found\n", toupper(region)))
    validation_ok <- FALSE
    next
  }

  # Try to read file
  test_read <- tryCatch({
    sf::st_read(result$path, quiet = TRUE)
  }, error = function(e) {
    cat(sprintf("   ✗ %s: Cannot read file - %s\n", toupper(region), conditionMessage(e)))
    validation_ok <- FALSE
    return(NULL)
  })

  if (!is.null(test_read)) {
    if (nrow(test_read) == result$polygons) {
      cat(sprintf("   ✓ %s: Valid (%s polygons, %.1f MB)\n",
                  toupper(region),
                  format(result$polygons, big.mark = ","),
                  result$file_size_mb))
    } else {
      cat(sprintf("   ✗ %s: Polygon count mismatch\n", toupper(region)))
      validation_ok <- FALSE
    }
  }
}

# Step 4: Summary
cat("\n")
cat("========================================\n")
cat("  Summary\n")
cat("========================================\n\n")

total_size <- sum(sapply(results, function(x) x$file_size_mb))
total_polygons <- sum(sapply(results, function(x) x$polygons))
original_size <- object.size(full_data) / 1024^2

cat(sprintf("Original GDB:      %s polygons, ~%.1f MB (in memory)\n",
            format(nrow(full_data), big.mark = ","),
            original_size))
cat(sprintf("Regional files:    %d files, %.1f MB total (on disk)\n",
            length(results),
            total_size))
cat(sprintf("Total polygons:    %s\n",
            format(total_polygons, big.mark = ",")))
cat("\n")

# Print per-region stats
cat("Regional breakdown:\n")
for (region in c("baltic", "north_sea", "atlantic", "arctic", "mediterranean")) {
  if (region %in% names(results)) {
    result <- results[[region]]
    pct <- (result$polygons / nrow(full_data)) * 100
    cat(sprintf("  %-15s %6s polygons (%4.1f%%), %5.1f MB\n",
                paste0(toupper(region), ":"),
                format(result$polygons, big.mark = ","),
                pct,
                result$file_size_mb))
  } else {
    cat(sprintf("  %-15s No data\n", paste0(toupper(region), ":")))
  }
}

cat("\n")

if (validation_ok) {
  cat("✅ SUCCESS! All regional files created and validated.\n\n")
  cat("Next steps:\n")
  cat("1. The app will automatically use these files for faster loading\n")
  cat("2. Original GDB is still available as backup\n")
  cat("3. You can now delete the GDB if disk space is limited\n")
  cat("\n")
  cat(sprintf("Regional files saved to: %s\n", normalizePath(output_dir)))
} else {
  cat("⚠ WARNING: Some files failed validation. Check errors above.\n")
}

cat("\n")
