# ==============================================================================
# TEST: Average Individual Weight Extraction from FishBase
# ==============================================================================
# Tests the multi-strategy weight extraction added to query_fishbase()
#
# Strategies tested:
# 1. Population data (popchar table)
# 2. 30% of max weight estimate
# 3. Length-weight relationship (W = a * L^b)
# 4. Generic formula (W ≈ 0.01 * L³)
# ==============================================================================

source("R/functions/taxonomic_api_utils.R")

cat("================================================================================\n")
cat("TEST: Average Individual Weight Extraction\n")
cat("================================================================================\n\n")

# Test cases - common fish species with different data availability
test_cases <- list(
  list(name = "Cod", description = "Large commercial fish (should have extensive data)"),
  list(name = "Herring", description = "Common small pelagic fish"),
  list(name = "Sprat", description = "Small schooling fish"),
  list(name = "Plaice", description = "Flatfish with known length-weight relationship"),
  list(name = "European flounder", description = "Another flatfish for comparison")
)

results <- list()

for (i in 1:length(test_cases)) {
  test_case <- test_cases[[i]]
  cat(sprintf("\nTest %d: '%s'\n", i, test_case$name))
  cat(sprintf("Description: %s\n", test_case$description))
  cat("--------------------------------------------------------------------------------\n")

  result <- query_fishbase(test_case$name)

  if (!is.null(result)) {
    cat(sprintf("✓ FOUND\n"))
    cat(sprintf("  Scientific name: %s\n", result$scientific_name))
    cat(sprintf("  Family: %s\n", result$family))
    cat(sprintf("  Common length: %s cm\n",
                ifelse(is.na(result$common_length_cm), "NA", result$common_length_cm)))
    cat(sprintf("  Max length: %s cm\n",
                ifelse(is.na(result$max_length_cm), "NA", result$max_length_cm)))
    cat(sprintf("  Max weight: %s g\n",
                ifelse(is.na(result$max_weight_g), "NA", sprintf("%.1f", result$max_weight_g))))
    cat(sprintf("  Average weight: %s g\n",
                ifelse(is.na(result$avg_weight_g), "NA", sprintf("%.1f", result$avg_weight_g))))

    # Determine which strategy was used based on the messages
    # (This is inferred from the logging messages in query_fishbase)
    if (!is.na(result$avg_weight_g)) {
      cat(sprintf("  ✓ Weight extraction successful\n"))

      # Store for summary
      results[[test_case$name]] <- list(
        found = TRUE,
        avg_weight_g = result$avg_weight_g,
        max_weight_g = result$max_weight_g
      )
    } else {
      cat(sprintf("  ⚠ No weight data available\n"))
      results[[test_case$name]] <- list(found = TRUE, avg_weight_g = NA, max_weight_g = NA)
    }
  } else {
    cat(sprintf("✗ NOT FOUND\n"))
    results[[test_case$name]] <- list(found = FALSE, avg_weight_g = NA, max_weight_g = NA)
  }
}

# Summary
cat("\n\n")
cat("================================================================================\n")
cat("SUMMARY\n")
cat("================================================================================\n")
cat(sprintf("Total tests: %d\n", length(test_cases)))

found_count <- sum(sapply(results, function(x) x$found))
weight_count <- sum(sapply(results, function(x) x$found && !is.na(x$avg_weight_g)))

cat(sprintf("Species found: %d\n", found_count))
cat(sprintf("Weight data extracted: %d\n", weight_count))

if (weight_count > 0) {
  cat("\nWeight Summary:\n")
  for (name in names(results)) {
    res <- results[[name]]
    if (res$found && !is.na(res$avg_weight_g)) {
      cat(sprintf("  • %s: %.1f g (avg), %.1f g (max)\n",
                  name, res$avg_weight_g,
                  ifelse(is.na(res$max_weight_g), 0, res$max_weight_g)))
    }
  }
}

cat("\n")
if (weight_count == found_count && found_count > 0) {
  cat("✓ Weight extraction is working for all found species!\n")
} else if (weight_count > 0) {
  cat(sprintf("⚠ Weight extraction partial: %d/%d species\n", weight_count, found_count))
} else {
  cat("✗ No weight data could be extracted\n")
}

cat("\n")
cat("Weight Estimation Strategies:\n")
cat("  1. Population data (popchar table) - most accurate\n")
cat("  2. 30% of max weight - rough heuristic\n")
cat("  3. Length-weight relationship (W = a * L^b) - species-specific\n")
cat("  4. Generic formula (W ≈ 0.01 * L³) - fallback\n")
cat("================================================================================\n")
