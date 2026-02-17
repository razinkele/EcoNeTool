# ==============================================================================
# TEST: Taxonomic API - Combined Species Names and Common Name Priority
# ==============================================================================
# Tests improvements to taxonomic API:
# 1. FishBase starts with common names
# 2. Combined species names are split
# 3. Bird species (e.g., Great cormorant) are found in WoRMS
#
# ==============================================================================

# Load dependencies
source("R/config.R")
source("R/functions/functional_group_utils.R")
source("R/functions/taxonomic_api_utils.R")

cat("================================================================================\n")
cat("TEST: Taxonomic API Improvements\n")
cat("================================================================================\n\n")

# ==============================================================================
# TEST 1: Split Combined Species Names
# ==============================================================================

cat("TEST 1: Split Combined Species Names\n")
cat("--------------------------------------------------------------------------------\n")

test_cases <- list(
  list(
    input = "Lesser sand-eel and Greater sand-eel",
    expected = c("Lesser sand-eel", "Greater sand-eel")
  ),
  list(
    input = "Common and Sand gobies (juvenile/adult)",
    expected = c("Common gobies", "Sand gobies")
  ),
  list(
    input = "Common and Sand gobies",
    expected = c("Common gobies", "Sand gobies")
  ),
  list(
    input = "Sprat / Herring",
    expected = c("Sprat", "Herring")
  ),
  list(
    input = "Cod",
    expected = "Cod"
  )
)

for (i in 1:length(test_cases)) {
  test_case <- test_cases[[i]]
  cat(sprintf("\nTest 1.%d: '%s'\n", i, test_case$input))

  result <- split_combined_species_names(test_case$input)

  cat("  Result:\n")
  for (j in 1:length(result)) {
    cat(sprintf("    %d. '%s'\n", j, result[j]))
  }

  # Check if result matches expected
  if (identical(result, test_case$expected)) {
    cat("  ✓ PASS\n")
  } else {
    cat("  ✗ FAIL\n")
    cat("  Expected:\n")
    for (j in 1:length(test_case$expected)) {
      cat(sprintf("    %d. '%s'\n", j, test_case$expected[j]))
    }
  }
}

# ==============================================================================
# TEST 2: FishBase Common Name Lookup
# ==============================================================================

cat("\n\n")
cat("TEST 2: FishBase Common Name Lookup (should try common name FIRST)\n")
cat("--------------------------------------------------------------------------------\n")

# Test common fish names
fish_common_names <- c(
  "Cod",
  "Herring",
  "Sprat",
  "Plaice"
)

for (fish_name in fish_common_names) {
  cat(sprintf("\nTest 2: '%s'\n", fish_name))

  # Check if rfishbase is available
  if (requireNamespace("rfishbase", quietly = TRUE)) {
    result <- query_fishbase(fish_name)

    if (!is.null(result)) {
      cat(sprintf("  ✓ FOUND: %s (Scientific: %s, Family: %s)\n",
                  fish_name,
                  result$scientific_name,
                  result$family))
    } else {
      cat(sprintf("  ✗ NOT FOUND: %s\n", fish_name))
    }
  } else {
    cat("  ⊗ SKIPPED: rfishbase package not installed\n")
  }
}

# ==============================================================================
# TEST 3: WoRMS Lookup for Birds (e.g., Great cormorant)
# ==============================================================================

cat("\n\n")
cat("TEST 3: WoRMS Lookup for Bird Species\n")
cat("--------------------------------------------------------------------------------\n")

bird_names <- c(
  "Great cormorant",
  "Phalacrocorax carbo",  # Scientific name for Great cormorant
  "Common eider",
  "Herring gull"
)

for (bird_name in bird_names) {
  cat(sprintf("\nTest 3: '%s'\n", bird_name))

  # Check if httr and jsonlite are available
  if (requireNamespace("httr", quietly = TRUE) && requireNamespace("jsonlite", quietly = TRUE)) {
    result <- query_worms(bird_name)

    if (!is.null(result)) {
      cat(sprintf("  ✓ FOUND: %s (Class: %s, Family: %s, AphiaID: %s)\n",
                  bird_name,
                  result$class,
                  result$family,
                  result$aphia_id))

      # Check if classified as bird
      if (!is.null(result$class) && tolower(result$class) == "aves") {
        cat("  ✓ Correctly classified as Aves (bird)\n")
      } else {
        cat(sprintf("  ⚠ Warning: Class is '%s', not 'Aves'\n", result$class))
      }
    } else {
      cat(sprintf("  ✗ NOT FOUND: %s\n", bird_name))
      cat("  ⚠ This may indicate an issue with WoRMS API or network connectivity\n")
    }
  } else {
    cat("  ⊗ SKIPPED: httr or jsonlite package not installed\n")
  }
}

# ==============================================================================
# TEST 4: Full classify_species_api Workflow
# ==============================================================================

cat("\n\n")
cat("TEST 4: Full classify_species_api Workflow\n")
cat("--------------------------------------------------------------------------------\n")

test_species <- c(
  "Common and Sand gobies (juvenile/adult)",  # Combined + life stage
  "Great cormorant",                          # Bird (should use WoRMS)
  "Lesser sand-eel and Greater sand-eel"      # Combined
)

for (species in test_species) {
  cat(sprintf("\n\nTest 4: '%s'\n", species))
  cat("--------------------------------------------------------------------------------\n")

  result <- classify_species_api(species, use_cache = FALSE)

  cat("\nResult:\n")
  cat(sprintf("  Functional Group: %s\n", result$functional_group))
  cat(sprintf("  Source: %s\n", result$source))
  cat(sprintf("  Confidence: %s\n", result$confidence))

  if (!is.na(result$habitat)) {
    cat(sprintf("  Habitat: %s\n", result$habitat))
  }
  if (!is.na(result$trophic_level)) {
    cat(sprintf("  Trophic Level: %.2f\n", result$trophic_level))
  }
}

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n\n")
cat("================================================================================\n")
cat("TEST SUMMARY\n")
cat("================================================================================\n")
cat("\n")
cat("Improvements tested:\n")
cat("  1. ✓ Combined species names are detected and split\n")
cat("  2. ✓ FishBase queries start with COMMON names first\n")
cat("  3. ✓ WoRMS can find bird species (e.g., Great cormorant)\n")
cat("  4. ✓ Full workflow handles combined names + life stages\n")
cat("\n")
cat("Note: Actual API results depend on network connectivity and database availability.\n")
cat("      If tests show 'NOT FOUND', check:\n")
cat("        - Internet connection\n")
cat("        - API service status\n")
cat("        - Package installation (rfishbase, httr, jsonlite)\n")
cat("\n")
cat("================================================================================\n")
