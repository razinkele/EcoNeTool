#!/usr/bin/env Rscript
# Comprehensive Taxonomic API Test Suite
# Tests all aspects of taxonomic database queries
# Version: 1.0.22
# Date: 2025-12-17

# Source the functions
source("R/functions/taxonomic_api_utils.R")

# Test tracking
total_tests <- 0
passed_tests <- 0
failed_tests <- 0
skipped_tests <- 0

# Helper function to run a test
run_test <- function(test_name, test_fn) {
  total_tests <<- total_tests + 1
  cat(sprintf("\n[Test %d] %s\n", total_tests, test_name))

  result <- tryCatch({
    test_fn()
  }, error = function(e) {
    cat(sprintf("  ✗ ERROR: %s\n", conditionMessage(e)))
    list(status = "error", message = conditionMessage(e))
  })

  if (is.null(result)) {
    result <- list(status = "skipped")
  }

  if (result$status == "pass") {
    passed_tests <<- passed_tests + 1
    cat(sprintf("  ✓ PASSED\n"))
  } else if (result$status == "fail") {
    failed_tests <<- failed_tests + 1
    cat(sprintf("  ✗ FAILED: %s\n", result$message))
  } else if (result$status == "skipped") {
    skipped_tests <<- skipped_tests + 1
    cat(sprintf("  ⊗ SKIPPED: %s\n", result$message))
  } else if (result$status == "error") {
    failed_tests <<- failed_tests + 1
  }

  return(result)
}

cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("  COMPREHENSIVE TAXONOMIC API TEST SUITE\n")
cat("═══════════════════════════════════════════════════════════════\n")

# ==============================================================================
# SECTION 1: Species Name Cleaning
# ==============================================================================

cat("\n\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("SECTION 1: Species Name Cleaning (40 test cases)\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

cleaning_tests <- list(
  # Prefix life stages
  list("adult sprat", "sprat"),
  list("Adult sprat", "sprat"),
  list("ADULT sprat", "sprat"),
  list("juvenile cod", "cod"),
  list("Juvenile Herring", "Herring"),
  list("young plaice", "plaice"),
  list("old salmon", "salmon"),
  list("immature flounder", "flounder"),
  list("larvae sprat", "sprat"),
  list("larval anchovy", "anchovy"),

  # Parenthetical life stages - simple
  list("cod (juvenile)", "cod"),
  list("herring (adult)", "herring"),
  list("sprat (larvae)", "sprat"),
  list("plaice (juv)", "plaice"),
  list("salmon (ad)", "salmon"),

  # Parenthetical life stages - compound
  list("Three-spined stickleback (juvenile/adult)", "Three-spined stickleback"),
  list("European sprat (juv/ad)", "European sprat"),
  list("Atlantic cod (juvenile/adult)", "Atlantic cod"),
  list("flounder (young/old)", "flounder"),
  list("herring (larvae/juvenile)", "herring"),

  # Combined prefix + parenthetical
  list("adult cod (juv/ad)", "cod"),
  list("juvenile herring (juvenile)", "herring"),
  list("young Three-spined stickleback (juvenile/adult)", "Three-spined stickleback"),

  # Other parenthetical info (should be removed)
  list("Atlantic cod (note: common)", "Atlantic cod"),
  list("European sprat (size: 10cm)", "European sprat"),
  list("herring (2023)", "herring"),

  # Scientific names (should not change)
  list("Gadus morhua", "Gadus morhua"),
  list("Sprattus sprattus", "Sprattus sprattus"),
  list("Clupea harengus", "Clupea harengus"),
  list("Gasterosteus aculeatus", "Gasterosteus aculeatus"),

  # Special groups (should not change)
  list("Detritus", "Detritus"),
  list("Phytoplankton", "Phytoplankton"),
  list("Zooplankton", "Zooplankton"),
  list("Benthos", "Benthos"),

  # Edge cases
  list("  adult  sprat  ", "sprat"),  # Extra whitespace
  list("adult", "adult"),  # Just life stage word
  list("(juvenile)", ""),  # Just parenthetical
  list("", ""),  # Empty string
  list("Adult Juvenile Fish", "Juvenile Fish"),  # Life stage in middle
  list("cod juvenile", "cod juvenile")  # Life stage at end (not removed)
)

for (i in seq_along(cleaning_tests)) {
  tc <- cleaning_tests[[i]]
  run_test(
    sprintf("Clean '%s' → '%s'", tc[[1]], tc[[2]]),
    function() {
      result <- clean_species_name(tc[[1]])
      if (result == tc[[2]]) {
        list(status = "pass")
      } else {
        list(status = "fail", message = sprintf("Expected '%s', got '%s'", tc[[2]], result))
      }
    }
  )
}

# ==============================================================================
# SECTION 2: FishBase Queries - Scientific Names
# ==============================================================================

cat("\n\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("SECTION 2: FishBase Queries - Scientific Names\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

if (!requireNamespace("rfishbase", quietly = TRUE)) {
  cat("\n⊗ SECTION SKIPPED: rfishbase package not installed\n")
  cat("  Install with: install.packages('rfishbase')\n")
  skipped_tests <- skipped_tests + 10
} else {
  fishbase_sci_tests <- list(
    list("Gadus morhua", "Gadidae", TRUE),
    list("Sprattus sprattus", "Clupeidae", TRUE),
    list("Clupea harengus", "Clupeidae", TRUE),
    list("Gasterosteus aculeatus", "Gasterosteidae", TRUE),
    list("Salmo salar", "Salmonidae", TRUE),
    list("Pleuronectes platessa", "Pleuronectidae", TRUE),
    list("Merlangius merlangus", "Gadidae", TRUE),
    list("Limanda limanda", "Pleuronectidae", TRUE),
    list("InvalidSpeciesXYZ123", NA, FALSE),
    list("Not A Real Fish", NA, FALSE)
  )

  for (i in seq_along(fishbase_sci_tests)) {
    tc <- fishbase_sci_tests[[i]]
    run_test(
      sprintf("FishBase scientific: '%s'", tc[[1]]),
      function() {
        result <- query_fishbase(tc[[1]])

        if (tc[[3]]) {
          # Should find
          if (!is.null(result) && !is.na(result$family) && result$family == tc[[2]]) {
            cat(sprintf("    Found: Family=%s, TL=%s\n",
                       result$family,
                       ifelse(is.na(result$trophic_level), "NA", round(result$trophic_level, 2))))
            list(status = "pass")
          } else {
            list(status = "fail", message = sprintf("Expected family '%s', got '%s'",
                                                     tc[[2]],
                                                     ifelse(is.null(result), "NULL", result$family)))
          }
        } else {
          # Should not find
          if (is.null(result)) {
            list(status = "pass")
          } else {
            list(status = "fail", message = "Expected NULL but found result")
          }
        }
      }
    )
  }
}

# ==============================================================================
# SECTION 3: FishBase Queries - Common Names
# ==============================================================================

cat("\n\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("SECTION 3: FishBase Queries - Common Names\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

if (!requireNamespace("rfishbase", quietly = TRUE)) {
  cat("\n⊗ SECTION SKIPPED: rfishbase package not installed\n")
  skipped_tests <- skipped_tests + 10
} else {
  fishbase_common_tests <- list(
    list("Atlantic cod", "Gadus morhua"),
    list("European sprat", "Sprattus sprattus"),
    list("Atlantic herring", "Clupea harengus"),
    list("Three-spined stickleback", "Gasterosteus aculeatus"),
    list("Atlantic salmon", "Salmo salar"),
    list("European plaice", "Pleuronectes platessa"),
    list("whiting", "Merlangius merlangus"),
    list("common dab", "Limanda limanda"),
    list("goldfish", "Carassius auratus"),
    list("NotARealFishNameXYZ", NA)
  )

  for (i in seq_along(fishbase_common_tests)) {
    tc <- fishbase_common_tests[[i]]
    run_test(
      sprintf("FishBase common: '%s'", tc[[1]]),
      function() {
        result <- query_fishbase(tc[[1]])

        if (!is.na(tc[[2]])) {
          # Should find
          if (!is.null(result) && result$scientific_name == tc[[2]]) {
            cat(sprintf("    Found: %s → %s (Family: %s)\n",
                       tc[[1]], result$scientific_name, result$family))
            list(status = "pass")
          } else {
            list(status = "fail", message = sprintf("Expected '%s', got '%s'",
                                                     tc[[2]],
                                                     ifelse(is.null(result), "NULL", result$scientific_name)))
          }
        } else {
          # Should not find
          if (is.null(result)) {
            list(status = "pass")
          } else {
            list(status = "fail", message = "Expected NULL but found result")
          }
        }
      }
    )
  }
}

# ==============================================================================
# SECTION 4: WoRMS Queries
# ==============================================================================

cat("\n\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("SECTION 4: WoRMS Queries\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
  cat("\n⊗ SECTION SKIPPED: httr or jsonlite package not installed\n")
  skipped_tests <- skipped_tests + 10
} else {
  worms_tests <- list(
    list("Mysis mixta", "Malacostraca", TRUE),
    list("Idotea balthica", "Malacostraca", TRUE),
    list("Gammarus oceanicus", "Malacostraca", TRUE),
    list("Mytilus edulis", "Bivalvia", TRUE),
    list("Carcinus maenas", "Malacostraca", TRUE),
    list("Fucus vesiculosus", "Phaeophyceae", TRUE),
    list("Asterias rubens", "Asteroidea", TRUE),
    list("Nereis diversicolor", "Polychaeta", TRUE),
    list("InvalidSpeciesXYZ", NA, FALSE),
    list("NotARealSpecies", NA, FALSE)
  )

  for (i in seq_along(worms_tests)) {
    tc <- worms_tests[[i]]
    run_test(
      sprintf("WoRMS: '%s'", tc[[1]]),
      function() {
        Sys.sleep(0.5)  # Rate limiting
        result <- query_worms(tc[[1]])

        if (tc[[3]]) {
          # Should find
          if (!is.null(result) && !is.null(result$class)) {
            cat(sprintf("    Found: AphiaID=%s, Class=%s, Family=%s\n",
                       result$aphia_id, result$class,
                       ifelse(is.null(result$family), "NA", result$family)))
            list(status = "pass")
          } else {
            list(status = "fail", message = sprintf("Expected class '%s', got NULL", tc[[2]]))
          }
        } else {
          # Should not find
          if (is.null(result)) {
            list(status = "pass")
          } else {
            list(status = "fail", message = "Expected NULL but found result")
          }
        }
      }
    )
  }
}

# ==============================================================================
# SECTION 5: OBIS Queries
# ==============================================================================

cat("\n\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("SECTION 5: OBIS Queries\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
  cat("\n⊗ SECTION SKIPPED: httr or jsonlite package not installed\n")
  skipped_tests <- skipped_tests + 5
} else {
  obis_tests <- list(
    list("Gadus morhua", "Actinopterygii", TRUE),
    list("Mysis mixta", "Malacostraca", TRUE),
    list("Mytilus edulis", "Bivalvia", TRUE),
    list("InvalidSpeciesXYZ", NA, FALSE),
    list("NotARealSpecies", NA, FALSE)
  )

  for (i in seq_along(obis_tests)) {
    tc <- obis_tests[[i]]
    run_test(
      sprintf("OBIS: '%s'", tc[[1]]),
      function() {
        Sys.sleep(0.5)  # Rate limiting
        result <- query_obis(tc[[1]])

        if (tc[[3]]) {
          # Should find
          if (!is.null(result) && !is.null(result$class)) {
            cat(sprintf("    Found: TaxonID=%s, Class=%s, Records=%d\n",
                       result$taxon_id, result$class,
                       ifelse(is.null(result$records), 0, result$records)))
            list(status = "pass")
          } else {
            list(status = "fail", message = sprintf("Expected class '%s', got NULL", tc[[2]]))
          }
        } else {
          # Should not find
          if (is.null(result)) {
            list(status = "pass")
          } else {
            list(status = "fail", message = "Expected NULL but found result")
          }
        }
      }
    )
  }
}

# ==============================================================================
# SECTION 6: Full Integration Tests
# ==============================================================================

cat("\n\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("SECTION 6: Full Integration Tests\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

integration_tests <- list(
  # Fish with life stages
  list("adult cod", "Fish", "Fish", "FishBase"),
  list("juvenile herring", "Fish", "Fish", "FishBase"),

  # Fish with common names + life stages
  list("Three-spined stickleback (juvenile/adult)", "Fish", "Fish", "FishBase"),
  list("Atlantic cod (larvae)", "Fish", "Fish", "FishBase"),

  # Zooplankton (skip FishBase)
  list("Mysis mixta", "Zooplankton", "Benthos", "WoRMS"),
  list("Gammarus oceanicus", "Zooplankton", "Benthos", "WoRMS"),

  # Benthos (skip FishBase)
  list("Mytilus edulis", "Benthos", "Fish", "WoRMS"),

  # Phytoplankton (skip FishBase)
  list("Fucus vesiculosus", "Phytoplankton", "Phytoplankton", "WoRMS"),

  # Detritus (skip all databases)
  list("Detritus", "Detritus", NA, NA)
)

for (i in seq_along(integration_tests)) {
  tc <- integration_tests[[i]]
  run_test(
    sprintf("Integration: '%s' (hint: %s)", tc[[1]], tc[[2]]),
    function() {
      Sys.sleep(0.5)  # Rate limiting
      result <- classify_species_api(tc[[1]], functional_group_hint = tc[[2]], use_cache = FALSE)

      if (is.na(tc[[3]])) {
        # Should not find
        if (result$confidence == "none") {
          list(status = "pass")
        } else {
          list(status = "fail", message = sprintf("Expected not found, got source '%s'", result$source))
        }
      } else {
        # Should find in specific database
        if (!is.na(result$source) && result$source == tc[[4]]) {
          cat(sprintf("    Success: Found in %s → %s (confidence: %s)\n",
                     result$source, result$functional_group, result$confidence))
          list(status = "pass")
        } else {
          list(status = "fail", message = sprintf("Expected source '%s', got '%s'",
                                                   tc[[4]],
                                                   ifelse(is.na(result$source), "NA", result$source)))
        }
      }
    }
  )
}

# ==============================================================================
# SECTION 7: Taxonomy Classification
# ==============================================================================

cat("\n\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("SECTION 7: Taxonomy Classification\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

taxonomy_tests <- list(
  # Fish classes
  list(list(class = "Actinopterygii"), "Fish"),
  list(list(class = "Chondrichthyes"), "Fish"),
  list(list(class = "Myxini"), "Fish"),
  list(list(class = "Elasmobranchii"), "Fish"),

  # Benthos
  list(list(class = "Malacostraca"), "Benthos"),
  list(list(class = "Polychaeta"), "Benthos"),
  list(list(class = "Bivalvia"), "Benthos"),
  list(list(class = "Gastropoda"), "Benthos"),

  # Zooplankton
  list(list(class = "Copepoda"), "Zooplankton"),
  list(list(class = "Branchiopoda"), "Zooplankton"),

  # Phytoplankton
  list(list(class = "Phaeophyceae"), "Phytoplankton"),
  list(list(class = "Bacillariophyceae"), "Phytoplankton"),

  # Mammals
  list(list(class = "Mammalia"), "Mammals"),

  # Birds
  list(list(class = "Aves"), "Birds"),

  # Unknown class (default to Fish)
  list(list(class = "UnknownClassXYZ"), "Fish")
)

for (i in seq_along(taxonomy_tests)) {
  tc <- taxonomy_tests[[i]]
  run_test(
    sprintf("Classify class '%s' → '%s'", tc[[1]]$class, tc[[2]]),
    function() {
      result <- classify_by_taxonomy(tc[[1]])
      if (result == tc[[2]]) {
        list(status = "pass")
      } else {
        list(status = "fail", message = sprintf("Expected '%s', got '%s'", tc[[2]], result))
      }
    }
  )
}

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("  TEST SUITE COMPLETE\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat(sprintf("Total tests:   %d\n", total_tests))
cat(sprintf("Passed:        %d (%.1f%%)\n", passed_tests, (passed_tests/total_tests)*100))
cat(sprintf("Failed:        %d (%.1f%%)\n", failed_tests, (failed_tests/total_tests)*100))
cat(sprintf("Skipped:       %d (%.1f%%)\n", skipped_tests, (skipped_tests/total_tests)*100))

if (failed_tests == 0 && skipped_tests == 0) {
  cat("\n✓✓✓ ALL TESTS PASSED! ✓✓✓\n\n")
  quit(status = 0)
} else if (failed_tests == 0) {
  cat("\n✓ All executed tests passed (some skipped)\n\n")
  quit(status = 0)
} else {
  cat("\n✗ SOME TESTS FAILED\n\n")
  quit(status = 1)
}
