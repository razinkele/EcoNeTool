#!/usr/bin/env Rscript
# Quick Taxonomic API Test - Tests without API calls
# Version: 1.0.22

source("R/functions/taxonomic_api_utils.R")

cat("\n═══════════════════════════════════════════════════════════\n")
cat("  QUICK TAXONOMIC API TESTS (No API Calls)\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Test counter
passed <- 0
failed <- 0

# ==============================================================================
# TEST 1: Species Name Cleaning
# ==============================================================================

cat("TEST 1: Species Name Cleaning\n")
cat("─────────────────────────────────────────────────────────────\n\n")

test_cases <- list(
  list("adult sprat", "sprat"),
  list("juvenile cod", "cod"),
  list("Three-spined stickleback (juvenile/adult)", "Three-spined stickleback"),
  list("European sprat (larvae)", "European sprat"),
  list("Atlantic cod (juv/ad)", "Atlantic cod"),
  list("adult cod (juvenile)", "cod"),
  list("Gadus morhua", "Gadus morhua"),
  list("Detritus", "Detritus"),
  list("  adult  sprat  ", "sprat"),
  list("cod (note: common name)", "cod")
)

for (i in seq_along(test_cases)) {
  tc <- test_cases[[i]]
  result <- clean_species_name(tc[[1]])

  if (result == tc[[2]]) {
    cat(sprintf("  ✓ Test %2d PASS: '%s' → '%s'\n", i, tc[[1]], result))
    passed <- passed + 1
  } else {
    cat(sprintf("  ✗ Test %2d FAIL: '%s'\n", i, tc[[1]]))
    cat(sprintf("      Expected: '%s'\n", tc[[2]]))
    cat(sprintf("      Got:      '%s'\n", result))
    failed <- failed + 1
  }
}

cat(sprintf("\nSummary: %d/%d passed (%.1f%%)\n\n", passed, passed+failed, (passed/(passed+failed))*100))

# ==============================================================================
# TEST 2: Taxonomy Classification
# ==============================================================================

cat("\nTEST 2: Taxonomy Classification by Class\n")
cat("─────────────────────────────────────────────────────────────\n\n")

passed2 <- 0
failed2 <- 0

tax_tests <- list(
  list(list(class = "Actinopterygii"), "Fish"),
  list(list(class = "Malacostraca"), "Benthos"),
  list(list(class = "Copepoda"), "Zooplankton"),
  list(list(class = "Phaeophyceae"), "Phytoplankton"),
  list(list(class = "Mammalia"), "Mammals"),
  list(list(class = "Aves"), "Birds"),
  list(list(class = "Bivalvia"), "Benthos"),
  list(list(class = "Polychaeta"), "Benthos"),
  list(list(class = "UnknownClass"), "Fish")  # Default
)

for (i in seq_along(tax_tests)) {
  tc <- tax_tests[[i]]
  result <- classify_by_taxonomy(tc[[1]])

  if (result == tc[[2]]) {
    cat(sprintf("  ✓ Test %2d PASS: class '%s' → '%s'\n", i, tc[[1]]$class, result))
    passed2 <- passed2 + 1
  } else {
    cat(sprintf("  ✗ Test %2d FAIL: class '%s'\n", i, tc[[1]]$class))
    cat(sprintf("      Expected: '%s'\n", tc[[2]]))
    cat(sprintf("      Got:      '%s'\n", result))
    failed2 <- failed2 + 1
  }
}

cat(sprintf("\nSummary: %d/%d passed (%.1f%%)\n\n", passed2, passed2+failed2, (passed2/(passed2+failed2))*100))

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

total_passed <- passed + passed2
total_failed <- failed + failed2
total_tests <- total_passed + total_failed

cat("\n═══════════════════════════════════════════════════════════\n")
cat("  QUICK TEST SUITE COMPLETE\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat(sprintf("Total tests:   %d\n", total_tests))
cat(sprintf("Passed:        %d (%.1f%%)\n", total_passed, (total_passed/total_tests)*100))
cat(sprintf("Failed:        %d (%.1f%%)\n", total_failed, (total_failed/total_tests)*100))

if (total_failed == 0) {
  cat("\n✓✓✓ ALL TESTS PASSED! ✓✓✓\n\n")
  cat("Note: This quick test only validates local functions.\n")
  cat("Run test_taxonomic_comprehensive.R for full API tests.\n\n")
  quit(status = 0)
} else {
  cat("\n✗ SOME TESTS FAILED\n\n")
  quit(status = 1)
}
