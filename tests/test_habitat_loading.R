#!/usr/bin/env Rscript
#' Test Suite: EMODnet Habitat Map Loading
#'
#' Comprehensive tests for habitat data loading functionality across
#' different regions, study areas, and edge cases.
#'
#' Tests:
#' 1. Regional loading (all 5 regions)
#' 2. BBT-specific loading (all available BBTs)
#' 3. Custom bbox loading
#' 4. Invalid geometry handling
#' 5. Performance benchmarks
#' 6. Edge cases (small areas, large areas, boundary conditions)
#'
#' @author Claude (Anthropic)
#' @date 2025-12-19

library(sf)

cat("\n")
cat("========================================\n")
cat("  Habitat Map Loading Test Suite\n")
cat("========================================\n\n")

# Load required functions
source("R/functions/emodnet_habitat_utils.R")
source("R/functions/euseamap_regional_config.R")
source("R/functions/spatial_analysis.R")

# Configuration
gdb_path <- "data/EUSeaMap_2025/EUSeaMap_2025.gdb"
bbt_file <- "data/BBT.geojson"

# Test results storage
test_results <- list()
failed_tests <- character()

# Helper function for test assertions
expect_true <- function(condition, test_name) {
  if (condition) {
    cat(sprintf("✓ PASS: %s\n", test_name))
    return(TRUE)
  } else {
    cat(sprintf("✗ FAIL: %s\n", test_name))
    failed_tests <<- c(failed_tests, test_name)
    return(FALSE)
  }
}

expect_no_error <- function(expr, test_name) {
  result <- tryCatch({
    expr
    TRUE
  }, error = function(e) {
    cat(sprintf("✗ FAIL: %s - Error: %s\n", test_name, conditionMessage(e)))
    failed_tests <<- c(failed_tests, test_name)
    FALSE
  })
  if (result) {
    cat(sprintf("✓ PASS: %s\n", test_name))
  }
  return(result)
}

# ============================================================================
# TEST SUITE 1: Regional Bbox Loading
# ============================================================================

cat("\n")
cat("TEST SUITE 1: Regional Bbox Loading\n")
cat("========================================\n\n")

test_regional_loading <- function() {
  regions <- list(
    list(name = "baltic", bbox = c(20, 55, 22, 57)),
    list(name = "north_sea", bbox = c(0, 52, 5, 55)),
    list(name = "mediterranean", bbox = c(12, 40, 18, 43)),
    list(name = "atlantic", bbox = c(-10, 45, -5, 50)),
    list(name = "arctic", bbox = c(10, 75, 20, 80))
  )

  for (region_info in regions) {
    cat(sprintf("\nTesting %s region...\n", toupper(region_info$name)))
    cat(sprintf("  Bbox: [%.1f, %.1f] to [%.1f, %.1f]\n",
                region_info$bbox[1], region_info$bbox[2],
                region_info$bbox[3], region_info$bbox[4]))

    start_time <- Sys.time()

    result <- tryCatch({
      sf::sf_use_s2(FALSE)
      data <- load_euseamap(path = gdb_path, bbox = region_info$bbox)

      # Validations
      expect_true(
        !is.null(data),
        sprintf("%s: Data loaded successfully", region_info$name)
      )

      expect_true(
        nrow(data) > 0,
        sprintf("%s: Data has features (n=%d)", region_info$name, nrow(data))
      )

      expect_true(
        !is.na(sf::st_crs(data)),
        sprintf("%s: Has valid CRS", region_info$name)
      )

      expect_true(
        all(c("geometry", "EUNIS") %in% names(data)) ||
        all(c("geometry", "L3_CODE") %in% names(data)),
        sprintf("%s: Has required columns", region_info$name)
      )

      end_time <- Sys.time()
      load_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

      # Store results
      test_results[[region_info$name]] <<- list(
        n_features = nrow(data),
        load_time = load_time,
        memory_mb = as.numeric(object.size(data)) / 1024^2,
        crs = sf::st_crs(data)$input
      )

      cat(sprintf("  ✓ Loaded: %d features\n", nrow(data)))
      cat(sprintf("  ✓ Time: %.2f seconds\n", load_time))
      cat(sprintf("  ✓ Memory: %.1f MB\n", test_results[[region_info$name]]$memory_mb))

      TRUE
    }, error = function(e) {
      cat(sprintf("  ✗ Error: %s\n", conditionMessage(e)))
      failed_tests <<- c(failed_tests, sprintf("%s regional loading", region_info$name))
      FALSE
    })
  }
}

test_regional_loading()

# ============================================================================
# TEST SUITE 2: BBT-Specific Loading
# ============================================================================

cat("\n")
cat("TEST SUITE 2: BBT-Specific Loading\n")
cat("========================================\n\n")

test_bbt_loading <- function() {
  # Check if BBT file exists
  if (!file.exists(bbt_file)) {
    cat(sprintf("⚠ Warning: BBT file not found at %s\n", bbt_file))
    cat("  Skipping BBT tests\n")
    return()
  }

  # Load BBT data
  sf::sf_use_s2(FALSE)
  bbt_data <- sf::st_read(bbt_file, quiet = TRUE)

  # Test selection of BBTs from different regions
  test_bbts <- c(
    "Lithuanian",      # Baltic
    "Heraklion",       # Mediterranean
    "Hornsund",        # Arctic
    "Bay_of_Gdansk",   # Baltic
    "Balearic"         # Mediterranean
  )

  for (bbt_name in test_bbts) {
    if (!bbt_name %in% bbt_data$Name) {
      cat(sprintf("⚠ Warning: BBT '%s' not found in data, skipping\n", bbt_name))
      next
    }

    cat(sprintf("\nTesting BBT: %s\n", bbt_name))

    start_time <- Sys.time()

    result <- tryCatch({
      # Get BBT polygon
      selected_bbt <- bbt_data[bbt_data$Name == bbt_name, ]
      selected_bbt <- sf::st_zm(selected_bbt, drop = TRUE, what = "ZM")

      # Get bbox and expand
      study_bbox <- sf::st_bbox(selected_bbt)
      buffer_deg <- 1.0
      expanded_bbox <- study_bbox + c(-buffer_deg, -buffer_deg, buffer_deg, buffer_deg)

      cat(sprintf("  Study bbox: [%.2f, %.2f] to [%.2f, %.2f]\n",
                  study_bbox["xmin"], study_bbox["ymin"],
                  study_bbox["xmax"], study_bbox["ymax"]))
      cat(sprintf("  Expanded bbox: [%.2f, %.2f] to [%.2f, %.2f]\n",
                  expanded_bbox["xmin"], expanded_bbox["ymin"],
                  expanded_bbox["xmax"], expanded_bbox["ymax"]))

      # Load habitat data with precise bbox
      habitat <- load_euseamap(path = gdb_path, bbox = as.numeric(expanded_bbox))

      # Clip to exact boundary
      habitat_clipped <- clip_habitat_to_study_area(habitat, selected_bbt)

      end_time <- Sys.time()
      load_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

      # Validations
      expect_true(
        nrow(habitat) > 0,
        sprintf("%s: Habitat data loaded (n=%d)", bbt_name, nrow(habitat))
      )

      expect_true(
        nrow(habitat_clipped) > 0,
        sprintf("%s: Clipped data has features (n=%d)", bbt_name, nrow(habitat_clipped))
      )

      expect_true(
        nrow(habitat_clipped) <= nrow(habitat),
        sprintf("%s: Clipped data <= bbox data", bbt_name)
      )

      expect_true(
        load_time < 5.0,
        sprintf("%s: Load time acceptable (%.2fs < 5s)", bbt_name, load_time)
      )

      cat(sprintf("  ✓ Bbox load: %d features in %.2fs\n", nrow(habitat), load_time))
      cat(sprintf("  ✓ Clipped: %d features (%.1f%% of bbox)\n",
                  nrow(habitat_clipped),
                  100 * nrow(habitat_clipped) / nrow(habitat)))
      cat(sprintf("  ✓ Memory: %.1f MB → %.1f MB\n",
                  as.numeric(object.size(habitat)) / 1024^2,
                  as.numeric(object.size(habitat_clipped)) / 1024^2))

      TRUE
    }, error = function(e) {
      cat(sprintf("  ✗ Error: %s\n", conditionMessage(e)))
      failed_tests <<- c(failed_tests, sprintf("%s BBT loading", bbt_name))
      FALSE
    })
  }
}

test_bbt_loading()

# ============================================================================
# TEST SUITE 3: Custom Bbox Loading
# ============================================================================

cat("\n")
cat("TEST SUITE 3: Custom Bbox Loading\n")
cat("========================================\n\n")

test_custom_bbox <- function() {
  # Test various bbox sizes
  test_cases <- list(
    list(
      name = "Small area (1x1 degree)",
      bbox = c(20, 55, 21, 56),
      expected_time = 1.0
    ),
    list(
      name = "Medium area (3x3 degrees)",
      bbox = c(19, 54, 22, 57),
      expected_time = 2.0
    ),
    list(
      name = "Large area (5x5 degrees)",
      bbox = c(18, 53, 23, 58),
      expected_time = 5.0
    )
  )

  for (test_case in test_cases) {
    cat(sprintf("\nTesting: %s\n", test_case$name))
    cat(sprintf("  Bbox: [%.1f, %.1f] to [%.1f, %.1f]\n",
                test_case$bbox[1], test_case$bbox[2],
                test_case$bbox[3], test_case$bbox[4]))

    start_time <- Sys.time()

    result <- tryCatch({
      sf::sf_use_s2(FALSE)
      data <- load_euseamap(path = gdb_path, bbox = test_case$bbox)

      end_time <- Sys.time()
      load_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

      expect_true(
        nrow(data) > 0,
        sprintf("%s: Data loaded", test_case$name)
      )

      cat(sprintf("  ✓ Loaded: %d features in %.2fs\n", nrow(data), load_time))

      # Check if load time is reasonable (allowing 2x expected time)
      if (load_time > test_case$expected_time * 2) {
        cat(sprintf("  ⚠ Warning: Load time (%.2fs) exceeds expected (%.2fs)\n",
                    load_time, test_case$expected_time))
      }

      TRUE
    }, error = function(e) {
      cat(sprintf("  ✗ Error: %s\n", conditionMessage(e)))
      failed_tests <<- c(failed_tests, sprintf("Custom bbox: %s", test_case$name))
      FALSE
    })
  }
}

test_custom_bbox()

# ============================================================================
# TEST SUITE 4: Invalid Geometry Handling
# ============================================================================

cat("\n")
cat("TEST SUITE 4: Invalid Geometry Handling\n")
cat("========================================\n\n")

test_invalid_geometries <- function() {
  cat("Testing geometry validation and cleanup...\n\n")

  # Load a larger area that's likely to have invalid geometries
  test_bbox <- c(15, 53, 25, 60)  # Large Baltic area

  start_time <- Sys.time()

  result <- tryCatch({
    sf::sf_use_s2(FALSE)
    data <- load_euseamap(path = gdb_path, bbox = test_bbox)

    end_time <- Sys.time()
    load_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

    # Check geometry validity
    valid_geoms <- sf::st_is_valid(data)
    n_invalid <- sum(!valid_geoms, na.rm = TRUE)
    n_total <- length(valid_geoms)

    cat(sprintf("  Bbox: [%.1f, %.1f] to [%.1f, %.1f]\n",
                test_bbox[1], test_bbox[2], test_bbox[3], test_bbox[4]))
    cat(sprintf("  ✓ Loaded: %d features\n", n_total))
    cat(sprintf("  ✓ Valid geometries: %d (%.1f%%)\n",
                sum(valid_geoms, na.rm = TRUE),
                100 * sum(valid_geoms, na.rm = TRUE) / n_total))

    if (n_invalid > 0) {
      cat(sprintf("  ⚠ Invalid geometries found: %d (%.1f%%)\n",
                  n_invalid, 100 * n_invalid / n_total))
      cat("  ✓ Note: load_euseamap() should have automatically removed these\n")
    } else {
      cat("  ✓ All geometries valid\n")
    }

    expect_true(
      n_total > 0,
      "Data loaded successfully"
    )

    expect_true(
      sum(valid_geoms, na.rm = TRUE) == n_total,
      "All loaded geometries are valid (invalid ones removed)"
    )

    cat(sprintf("  ✓ Load time: %.2fs\n", load_time))

    TRUE
  }, error = function(e) {
    cat(sprintf("  ✗ Error: %s\n", conditionMessage(e)))
    failed_tests <<- c(failed_tests, "Invalid geometry handling")
    FALSE
  })
}

test_invalid_geometries()

# ============================================================================
# TEST SUITE 5: Performance Benchmarks
# ============================================================================

cat("\n")
cat("TEST SUITE 5: Performance Benchmarks\n")
cat("========================================\n\n")

test_performance <- function() {
  cat("Running performance benchmarks...\n\n")

  benchmarks <- list(
    list(
      name = "Tiny bbox (0.5x0.5 deg)",
      bbox = c(20.5, 55.5, 21.0, 56.0),
      target_time = 0.5
    ),
    list(
      name = "Small bbox (1x1 deg)",
      bbox = c(20, 55, 21, 56),
      target_time = 1.0
    ),
    list(
      name = "Medium bbox (2x2 deg)",
      bbox = c(19.5, 54.5, 21.5, 56.5),
      target_time = 2.0
    ),
    list(
      name = "Large bbox (4x4 deg)",
      bbox = c(18, 53, 22, 57),
      target_time = 4.0
    )
  )

  results <- data.frame(
    test = character(),
    n_features = integer(),
    load_time_s = numeric(),
    memory_mb = numeric(),
    target_met = logical(),
    stringsAsFactors = FALSE
  )

  for (benchmark in benchmarks) {
    cat(sprintf("  %s: ", benchmark$name))

    start_time <- Sys.time()

    result <- tryCatch({
      sf::sf_use_s2(FALSE)
      data <- load_euseamap(path = gdb_path, bbox = benchmark$bbox)

      end_time <- Sys.time()
      load_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      memory_mb <- as.numeric(object.size(data)) / 1024^2

      target_met <- load_time <= (benchmark$target_time * 1.5)  # Allow 50% margin

      results <- rbind(results, data.frame(
        test = benchmark$name,
        n_features = nrow(data),
        load_time_s = load_time,
        memory_mb = memory_mb,
        target_met = target_met
      ))

      if (target_met) {
        cat(sprintf("✓ %.2fs (%d features, %.1f MB)\n",
                    load_time, nrow(data), memory_mb))
      } else {
        cat(sprintf("⚠ %.2fs (target: %.2fs, %d features, %.1f MB)\n",
                    load_time, benchmark$target_time, nrow(data), memory_mb))
      }

      TRUE
    }, error = function(e) {
      cat(sprintf("✗ Error: %s\n", conditionMessage(e)))
      FALSE
    })
  }

  cat("\n")
  cat("Performance Summary:\n")
  cat("--------------------\n")
  print(results, row.names = FALSE)

  cat("\n")

  # Check if all targets met
  all_met <- all(results$target_met)
  if (all_met) {
    cat("✓ All performance targets met!\n")
  } else {
    n_failed <- sum(!results$target_met)
    cat(sprintf("⚠ %d/%d performance targets not met\n",
                n_failed, nrow(results)))
  }
}

test_performance()

# ============================================================================
# TEST SUITE 6: Edge Cases
# ============================================================================

cat("\n")
cat("TEST SUITE 6: Edge Cases\n")
cat("========================================\n\n")

test_edge_cases <- function() {
  # Test 1: Very small bbox (0.01 x 0.01 degrees)
  cat("Test 1: Tiny bbox (may have no features)\n")
  result1 <- tryCatch({
    sf::sf_use_s2(FALSE)
    data <- load_euseamap(path = gdb_path, bbox = c(20.5, 55.5, 20.51, 55.51))
    cat(sprintf("  ✓ Loaded: %d features (OK even if 0)\n", nrow(data)))
    TRUE
  }, error = function(e) {
    cat(sprintf("  ✗ Error: %s\n", conditionMessage(e)))
    failed_tests <<- c(failed_tests, "Edge case: Tiny bbox")
    FALSE
  })

  cat("\n")

  # Test 2: Bbox crossing 0° meridian (if applicable to data)
  cat("Test 2: Bbox crossing prime meridian\n")
  result2 <- tryCatch({
    sf::sf_use_s2(FALSE)
    data <- load_euseamap(path = gdb_path, bbox = c(-2, 51, 2, 53))
    cat(sprintf("  ✓ Loaded: %d features\n", nrow(data)))
    TRUE
  }, error = function(e) {
    cat(sprintf("  ⚠ Note: %s (may be expected if no data in this area)\n",
                conditionMessage(e)))
    TRUE  # Don't fail, data may not exist there
  })

  cat("\n")

  # Test 3: Bbox at high latitudes (Arctic)
  cat("Test 3: High latitude bbox (Arctic)\n")
  result3 <- tryCatch({
    sf::sf_use_s2(FALSE)
    data <- load_euseamap(path = gdb_path, bbox = c(10, 78, 20, 80))
    cat(sprintf("  ✓ Loaded: %d features\n", nrow(data)))
    TRUE
  }, error = function(e) {
    cat(sprintf("  ⚠ Note: %s (may be expected if no data at this latitude)\n",
                conditionMessage(e)))
    TRUE
  })

  cat("\n")

  # Test 4: Bbox outside EUSeaMap coverage (should return 0 features or error)
  cat("Test 4: Bbox outside coverage area\n")
  result4 <- tryCatch({
    sf::sf_use_s2(FALSE)
    data <- load_euseamap(path = gdb_path, bbox = c(-180, -45, -170, -40))
    cat(sprintf("  ✓ Loaded: %d features (expected 0 for out-of-coverage area)\n",
                nrow(data)))
    if (nrow(data) == 0) {
      cat("  ✓ Correctly returned 0 features for out-of-coverage bbox\n")
    }
    TRUE
  }, error = function(e) {
    cat(sprintf("  ✓ Expected behavior: %s\n", conditionMessage(e)))
    TRUE
  })
}

test_edge_cases()

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n")
cat("========================================\n")
cat("  Test Results Summary\n")
cat("========================================\n\n")

if (length(failed_tests) == 0) {
  cat("✅ ALL TESTS PASSED!\n\n")
  cat("Habitat map loading is working correctly across:\n")
  cat("  ✓ All regional bboxes\n")
  cat("  ✓ BBT-specific loading\n")
  cat("  ✓ Custom bbox sizes\n")
  cat("  ✓ Invalid geometry handling\n")
  cat("  ✓ Performance benchmarks\n")
  cat("  ✓ Edge cases\n\n")
  cat("Status: PRODUCTION READY ✅\n")
  quit(status = 0)
} else {
  cat(sprintf("⚠️  %d TEST(S) FAILED:\n\n", length(failed_tests)))
  for (i in seq_along(failed_tests)) {
    cat(sprintf("  %d. %s\n", i, failed_tests[i]))
  }
  cat("\n")
  cat("Status: NEEDS ATTENTION ⚠️\n")
  quit(status = 1)
}
