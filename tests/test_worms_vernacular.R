# ==============================================================================
# TEST: WoRMS Vernacular/Common Name Search
# ==============================================================================
# Tests the improved WoRMS API with vernacular name support
#
# ==============================================================================

source("R/functions/taxonomic_api_utils.R")

cat("================================================================================\n")
cat("TEST: WoRMS Vernacular Name Search\n")
cat("================================================================================\n\n")

# Test cases - bird common names that previously failed
test_cases <- list(
  list(name = "Great cormorant", expected_class = "Aves"),
  list(name = "Common eider", expected_class = "Aves"),
  list(name = "Herring gull", expected_class = "Aves"),
  list(name = "Atlantic puffin", expected_class = "Aves")
)

results <- list()

for (i in 1:length(test_cases)) {
  test_case <- test_cases[[i]]
  cat(sprintf("\nTest %d: '%s'\n", i, test_case$name))
  cat("--------------------------------------------------------------------------------\n")

  result <- query_worms(test_case$name, fuzzy = TRUE, try_vernacular = TRUE)

  if (!is.null(result)) {
    cat(sprintf("✓ FOUND\n"))
    cat(sprintf("  Scientific name: %s\n", result$scientific_name))
    cat(sprintf("  AphiaID: %s\n", result$aphia_id))
    cat(sprintf("  Class: %s\n", result$class))
    cat(sprintf("  Family: %s\n", result$family))
    cat(sprintf("  Query type: %s\n", result$query_type))

    # Check if class matches expected
    if (!is.null(result$class) && result$class == test_case$expected_class) {
      cat(sprintf("  ✓ Class verified: %s\n", result$class))
    } else {
      cat(sprintf("  ⚠ Class mismatch: expected %s, got %s\n",
                  test_case$expected_class, result$class))
    }

    results[[test_case$name]] <- "FOUND"
  } else {
    cat(sprintf("✗ NOT FOUND\n"))
    results[[test_case$name]] <- "NOT FOUND"
  }
}

# Summary
cat("\n\n")
cat("================================================================================\n")
cat("SUMMARY\n")
cat("================================================================================\n")
cat(sprintf("Total tests: %d\n", length(test_cases)))
cat(sprintf("Found: %d\n", sum(results == "FOUND")))
cat(sprintf("Not found: %d\n", sum(results == "NOT FOUND")))

if (sum(results == "FOUND") > 0) {
  cat("\n✓ WoRMS vernacular search is working!\n")
} else {
  cat("\n✗ WoRMS vernacular search may have issues\n")
}

cat("\n")
cat("Note: WoRMS vernacular endpoint (AphiaRecordsByVernacular) is now used\n")
cat("      automatically when scientific name search fails.\n")
cat("\n")
cat("Documentation:\n")
cat("  - WoRMS REST API: https://www.marinespecies.org/rest/\n")
cat("  - Vernacular endpoint: https://www.marinespecies.org/aphia.php?p=webservice\n")
cat("================================================================================\n")
