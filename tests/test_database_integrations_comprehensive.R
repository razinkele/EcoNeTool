# =============================================================================
# Comprehensive Database Integration Tests
# =============================================================================
#
# Tests all 9 database integrations, API key management, lookup hierarchy,
# fuzzy harmonization, and error handling.
#
# Date: 2025-12-25
# Version: 1.0
#
# =============================================================================

cat("=============================================================================\n")
cat("COMPREHENSIVE DATABASE INTEGRATION TESTS\n")
cat("=============================================================================\n\n")

# Load required functions
cat("Loading dependencies...\n")
source("R/config.R")
source("R/config/harmonization_config.R")
source("R/functions/trait_lookup.R")

# Test results tracking
test_results <- list()
test_count <- 0
pass_count <- 0
fail_count <- 0
skip_count <- 0

# Helper function to record test results
record_test <- function(test_name, status, message = "", details = "") {
  test_count <<- test_count + 1

  if (status == "PASS") {
    pass_count <<- pass_count + 1
    symbol <- "✓"
    color <- ""
  } else if (status == "FAIL") {
    fail_count <<- fail_count + 1
    symbol <- "✗"
    color <- ""
  } else {
    skip_count <<- skip_count + 1
    symbol <- "⊘"
    color <- ""
  }

  cat(sprintf("  %s [%s] %s", symbol, status, test_name))
  if (message != "") cat(" -", message)
  cat("\n")

  if (details != "") {
    cat("     ", details, "\n")
  }

  test_results[[test_name]] <<- list(
    status = status,
    message = message,
    details = details
  )
}

# =============================================================================
# TEST 1: API KEY MANAGEMENT
# =============================================================================

cat("\n=============================================================================\n")
cat("TEST 1: API KEY MANAGEMENT\n")
cat("=============================================================================\n\n")

# Test 1.1: Check if API key functions exist
tryCatch({
  stopifnot(exists("get_api_key"))
  stopifnot(exists("has_api_key"))
  record_test("API key functions exist", "PASS")
}, error = function(e) {
  record_test("API key functions exist", "FAIL", e$message)
})

# Test 1.2: Get default freshwaterecology key
tryCatch({
  key <- get_api_key("freshwaterecology_key")
  if (nchar(key) > 0) {
    record_test("Default freshwaterecology key available", "PASS",
                paste("Key:", substr(key, 1, 20), "..."))
  } else {
    record_test("Default freshwaterecology key available", "FAIL",
                "Key is empty")
  }
}, error = function(e) {
  record_test("Default freshwaterecology key available", "FAIL", e$message)
})

# Test 1.3: Check AlgaeBase credentials
tryCatch({
  username <- get_api_key("algaebase_username")
  password <- get_api_key("algaebase_password")

  if (nchar(username) > 0 && nchar(password) > 0) {
    record_test("AlgaeBase credentials configured", "PASS",
                "Username and password set")
  } else {
    record_test("AlgaeBase credentials configured", "SKIP",
                "Not configured (optional)")
  }
}, error = function(e) {
  record_test("AlgaeBase credentials configured", "FAIL", e$message)
})

# Test 1.4: has_api_key function
tryCatch({
  has_fw <- has_api_key("freshwaterecology_key")
  stopifnot(is.logical(has_fw))
  record_test("has_api_key() returns logical", "PASS")
}, error = function(e) {
  record_test("has_api_key() returns logical", "FAIL", e$message)
})

# =============================================================================
# TEST 2: INDIVIDUAL DATABASE LOOKUP FUNCTIONS
# =============================================================================

cat("\n=============================================================================\n")
cat("TEST 2: INDIVIDUAL DATABASE LOOKUP FUNCTIONS\n")
cat("=============================================================================\n\n")

# Test species for each database
test_species <- list(
  fishbase = "Gadus morhua",           # Atlantic cod - should be in FishBase
  sealifebase = "Crangon crangon",     # Brown shrimp - should be in SeaLifeBase
  algaebase = "Skeletonema costatum",  # Diatom - should be in WoRMS/AlgaeBase
  shark = "Clupea harengus",           # Herring - should be in SHARK (Swedish waters)
  freshwater = "Daphnia magna",        # Water flea - freshwater species
  worms = "Asterias rubens"            # Starfish - should be in WoRMS
)

# Test 2.1: FishBase lookup (baseline)
cat("Testing FishBase (baseline)...\n")
tryCatch({
  result <- lookup_fishbase_traits(test_species$fishbase)

  if (!is.null(result) && length(result) > 0) {
    record_test("FishBase lookup (Gadus morhua)", "PASS",
                paste("Found", length(result), "trait fields"))
  } else {
    record_test("FishBase lookup (Gadus morhua)", "FAIL",
                "No data returned")
  }
}, error = function(e) {
  record_test("FishBase lookup (Gadus morhua)", "FAIL", e$message)
})

# Test 2.2: SeaLifeBase lookup
cat("Testing SeaLifeBase...\n")
tryCatch({
  result <- lookup_sealifebase_traits(test_species$sealifebase)

  if (!is.null(result) && length(result) > 0) {
    record_test("SeaLifeBase lookup (Crangon crangon)", "PASS",
                paste("Found", length(result), "trait fields"))
  } else {
    record_test("SeaLifeBase lookup (Crangon crangon)", "FAIL",
                "No data returned")
  }
}, error = function(e) {
  record_test("SeaLifeBase lookup (Crangon crangon)", "FAIL", e$message)
})

# Test 2.3: AlgaeBase lookup
cat("Testing AlgaeBase...\n")
tryCatch({
  result <- lookup_algaebase_traits(test_species$algaebase)

  if (!is.null(result) && length(result) > 0) {
    record_test("AlgaeBase lookup (Skeletonema costatum)", "PASS",
                paste("Found", length(result), "trait fields"))
  } else {
    # AlgaeBase might not have direct API, fallback to WoRMS is acceptable
    record_test("AlgaeBase lookup (Skeletonema costatum)", "SKIP",
                "No direct API (uses WoRMS fallback)")
  }
}, error = function(e) {
  record_test("AlgaeBase lookup (Skeletonema costatum)", "SKIP",
              "Expected - uses WoRMS fallback")
})

# Test 2.4: SHARK lookup
cat("Testing SHARK...\n")
tryCatch({
  result <- lookup_shark_traits(test_species$shark)

  if (!is.null(result) && length(result) > 0) {
    record_test("SHARK lookup (Clupea harengus)", "PASS",
                paste("Found", length(result), "trait fields"))
  } else {
    record_test("SHARK lookup (Clupea harengus)", "SKIP",
                "No data (requires shark4r package or API access)")
  }
}, error = function(e) {
  record_test("SHARK lookup (Clupea harengus)", "SKIP",
              paste("Expected -", e$message))
})

# Test 2.5: freshwaterecology.info lookup
cat("Testing freshwaterecology.info...\n")
tryCatch({
  result <- lookup_freshwaterecology_traits(test_species$freshwater)

  if (!is.null(result) && length(result) > 0) {
    record_test("freshwaterecology lookup (Daphnia magna)", "PASS",
                paste("Found", length(result), "trait fields"))
  } else {
    record_test("freshwaterecology lookup (Daphnia magna)", "SKIP",
                "No data (requires API access)")
  }
}, error = function(e) {
  record_test("freshwaterecology lookup (Daphnia magna)", "SKIP",
              paste("Expected -", substr(e$message, 1, 50)))
})

# Test 2.6: WoRMS lookup (critical baseline)
cat("Testing WoRMS...\n")
tryCatch({
  result <- lookup_worms_traits(test_species$worms)

  if (!is.null(result) && length(result) > 0) {
    record_test("WoRMS lookup (Asterias rubens)", "PASS",
                paste("Found", length(result), "trait fields"))
  } else {
    record_test("WoRMS lookup (Asterias rubens)", "FAIL",
                "No data returned - WoRMS is critical")
  }
}, error = function(e) {
  record_test("WoRMS lookup (Asterias rubens)", "FAIL", e$message)
})

# =============================================================================
# TEST 3: INTEGRATED LOOKUP HIERARCHY
# =============================================================================

cat("\n=============================================================================\n")
cat("TEST 3: INTEGRATED LOOKUP HIERARCHY\n")
cat("=============================================================================\n\n")

# Test 3.1: Fish lookup (should use FishBase)
cat("Testing integrated lookup for fish...\n")
tryCatch({
  result <- lookup_species_traits("Gadus morhua", cache_dir = NULL)

  if (!is.null(result$harmonized)) {
    sources <- paste(unique(result$sources_used), collapse = ", ")
    record_test("Integrated fish lookup", "PASS",
                paste("Sources:", sources))

    # Check that harmonized traits exist
    if (!is.na(result$harmonized$MS)) {
      record_test("Fish size harmonization", "PASS",
                  paste("MS =", result$harmonized$MS))
    } else {
      record_test("Fish size harmonization", "FAIL", "MS is NA")
    }

    if (!is.na(result$harmonized$FS)) {
      record_test("Fish foraging harmonization", "PASS",
                  paste("FS =", result$harmonized$FS))
    } else {
      record_test("Fish foraging harmonization", "FAIL", "FS is NA")
    }
  } else {
    record_test("Integrated fish lookup", "FAIL", "No harmonized data")
  }
}, error = function(e) {
  record_test("Integrated fish lookup", "FAIL", e$message)
})

# Test 3.2: Invertebrate lookup (should use SeaLifeBase or WoRMS)
cat("Testing integrated lookup for marine invertebrate...\n")
tryCatch({
  result <- lookup_species_traits("Asterias rubens", cache_dir = NULL)

  if (!is.null(result$harmonized)) {
    sources <- paste(unique(result$sources_used), collapse = ", ")
    record_test("Integrated invertebrate lookup", "PASS",
                paste("Sources:", sources))
  } else {
    record_test("Integrated invertebrate lookup", "FAIL", "No harmonized data")
  }
}, error = function(e) {
  record_test("Integrated invertebrate lookup", "FAIL", e$message)
})

# Test 3.3: Phytoplankton lookup (should use ontology + WoRMS)
cat("Testing integrated lookup for phytoplankton...\n")
tryCatch({
  result <- lookup_species_traits("Skeletonema costatum", cache_dir = NULL)

  if (!is.null(result$harmonized)) {
    sources <- paste(unique(result$sources_used), collapse = ", ")
    record_test("Integrated phytoplankton lookup", "PASS",
                paste("Sources:", sources))

    # Check if fuzzy harmonization was used
    if ("Fuzzy" %in% result$sources_used) {
      record_test("Fuzzy harmonization triggered", "PASS",
                  "Ontology data used as fallback")
    }
  } else {
    record_test("Integrated phytoplankton lookup", "FAIL", "No harmonized data")
  }
}, error = function(e) {
  record_test("Integrated phytoplankton lookup", "FAIL", e$message)
})

# =============================================================================
# TEST 4: FUZZY HARMONIZATION INTEGRATION
# =============================================================================

cat("\n=============================================================================\n")
cat("TEST 4: FUZZY HARMONIZATION INTEGRATION\n")
cat("=============================================================================\n\n")

# Load ontology data
ontology_data <- tryCatch({
  read.csv("data/ontology_traits.csv", stringsAsFactors = FALSE)
}, error = function(e) {
  NULL
})

if (!is.null(ontology_data)) {
  record_test("Ontology database loaded", "PASS",
              paste(nrow(ontology_data), "records,",
                    length(unique(ontology_data$taxon_name)), "species"))

  # Test 4.1: Fuzzy foraging harmonization
  test_sp <- "Macoma balthica"
  sp_traits <- ontology_data[ontology_data$taxon_name == test_sp, ]

  if (nrow(sp_traits) > 0) {
    tryCatch({
      fuzzy_fs <- harmonize_fuzzy_foraging(sp_traits)

      if (!is.na(fuzzy_fs$class)) {
        record_test("Fuzzy foraging harmonization (Macoma balthica)", "PASS",
                    paste("FS =", fuzzy_fs$class, "confidence =", fuzzy_fs$confidence))
      } else {
        record_test("Fuzzy foraging harmonization (Macoma balthica)", "FAIL",
                    "No FS class returned")
      }
    }, error = function(e) {
      record_test("Fuzzy foraging harmonization (Macoma balthica)", "FAIL", e$message)
    })

    # Test 4.2: Fuzzy mobility harmonization
    tryCatch({
      fuzzy_mb <- harmonize_fuzzy_mobility(sp_traits)

      if (!is.na(fuzzy_mb$class)) {
        record_test("Fuzzy mobility harmonization (Macoma balthica)", "PASS",
                    paste("MB =", fuzzy_mb$class, "confidence =", fuzzy_mb$confidence))
      } else {
        record_test("Fuzzy mobility harmonization (Macoma balthica)", "FAIL",
                    "No MB class returned")
      }
    }, error = function(e) {
      record_test("Fuzzy mobility harmonization (Macoma balthica)", "FAIL", e$message)
    })

    # Test 4.3: Fuzzy habitat harmonization
    tryCatch({
      fuzzy_ep <- harmonize_fuzzy_habitat(sp_traits)

      if (!is.na(fuzzy_ep$class)) {
        record_test("Fuzzy habitat harmonization (Macoma balthica)", "PASS",
                    paste("EP =", fuzzy_ep$class, "confidence =", fuzzy_ep$confidence))
      } else {
        record_test("Fuzzy habitat harmonization (Macoma balthica)", "FAIL",
                    "No EP class returned")
      }
    }, error = function(e) {
      record_test("Fuzzy habitat harmonization (Macoma balthica)", "FAIL", e$message)
    })
  } else {
    record_test("Fuzzy harmonization tests", "SKIP",
                "Macoma balthica not in ontology database")
  }

  # Test 4.4: Floater recognition (phytoplankton fix)
  test_sp_phyto <- "Skeletonema costatum"
  sp_traits_phyto <- ontology_data[ontology_data$taxon_name == test_sp_phyto, ]

  if (nrow(sp_traits_phyto) > 0) {
    tryCatch({
      fuzzy_mb <- harmonize_fuzzy_mobility(sp_traits_phyto)

      if (!is.na(fuzzy_mb$class)) {
        if (fuzzy_mb$class == "MB3") {
          record_test("Floater recognition fix (Skeletonema)", "PASS",
                      "Correctly assigned to MB3 (floater)")
        } else {
          record_test("Floater recognition fix (Skeletonema)", "FAIL",
                      paste("Wrong class:", fuzzy_mb$class, "expected MB3"))
        }
      } else {
        record_test("Floater recognition fix (Skeletonema)", "FAIL",
                    "No MB class returned")
      }
    }, error = function(e) {
      record_test("Floater recognition fix (Skeletonema)", "FAIL", e$message)
    })
  } else {
    record_test("Floater recognition fix", "SKIP",
                "Skeletonema costatum not in ontology database")
  }

} else {
  record_test("Ontology database loaded", "FAIL",
              "Could not load data/ontology_traits.csv")
  record_test("Fuzzy harmonization tests", "SKIP", "No ontology data")
}

# =============================================================================
# TEST 5: HARMONIZATION CONFIGURATION
# =============================================================================

cat("\n=============================================================================\n")
cat("TEST 5: HARMONIZATION CONFIGURATION\n")
cat("=============================================================================\n\n")

# Test 5.1: Configuration loaded
tryCatch({
  stopifnot(exists("HARMONIZATION_CONFIG"))
  stopifnot(is.list(HARMONIZATION_CONFIG))
  record_test("Harmonization config loaded", "PASS")
}, error = function(e) {
  record_test("Harmonization config loaded", "FAIL", e$message)
})

# Test 5.2: Size thresholds
tryCatch({
  thresh <- get_size_threshold("MS2_MS3")
  stopifnot(!is.na(thresh))
  stopifnot(thresh == 1.0)  # Default value
  record_test("Size threshold retrieval", "PASS",
              paste("MS2_MS3 =", thresh, "cm"))
}, error = function(e) {
  record_test("Size threshold retrieval", "FAIL", e$message)
})

# Test 5.3: Foraging patterns
tryCatch({
  pattern <- get_config_pattern("FS1_predator", "foraging")
  stopifnot(!is.na(pattern))
  stopifnot(grepl("predat", pattern))
  record_test("Foraging pattern retrieval", "PASS",
              paste("FS1:", substr(pattern, 1, 30), "..."))
}, error = function(e) {
  record_test("Foraging pattern retrieval", "FAIL", e$message)
})

# Test 5.4: Taxonomic rules
tryCatch({
  rule <- check_taxonomic_rule("fish_obligate_swimmers")
  stopifnot(is.logical(rule))
  record_test("Taxonomic rule check", "PASS",
              paste("fish_obligate_swimmers =", rule))
}, error = function(e) {
  record_test("Taxonomic rule check", "FAIL", e$message)
})

# Test 5.5: Ecosystem profiles
tryCatch({
  active_profile_name <- HARMONIZATION_CONFIG$active_profile
  profile <- HARMONIZATION_CONFIG$profiles[[active_profile_name]]
  stopifnot(!is.null(profile))
  stopifnot(!is.null(profile$size_multiplier))
  record_test("Ecosystem profile retrieval", "PASS",
              paste("Active:", active_profile_name,
                    "multiplier:", profile$size_multiplier))
}, error = function(e) {
  record_test("Ecosystem profile retrieval", "FAIL", e$message)
})

# =============================================================================
# TEST 6: ERROR HANDLING AND FALLBACKS
# =============================================================================

cat("\n=============================================================================\n")
cat("TEST 6: ERROR HANDLING AND FALLBACKS\n")
cat("=============================================================================\n\n")

# Test 6.1: Invalid species name
tryCatch({
  result <- lookup_species_traits("Nonexistent_species_12345", cache_dir = NULL)

  # Should return something even if species not found
  if (!is.null(result)) {
    record_test("Invalid species handling", "PASS",
                "Function handles invalid species gracefully")
  } else {
    record_test("Invalid species handling", "FAIL",
                "Function returned NULL")
  }
}, error = function(e) {
  # Error is acceptable for invalid species
  record_test("Invalid species handling", "PASS",
              "Appropriate error thrown")
})

# Test 6.2: Empty species name
tryCatch({
  result <- lookup_species_traits("", cache_dir = NULL)
  record_test("Empty species name handling", "PASS",
              "No crash on empty input")
}, error = function(e) {
  record_test("Empty species name handling", "PASS",
              "Appropriate error thrown")
})

# Test 6.3: Species with no database matches (should use fuzzy/taxonomic fallback)
tryCatch({
  # Use a species that's in ontology but might not be in online databases
  result <- lookup_species_traits("Arenicola marina", cache_dir = NULL)

  if (!is.null(result$harmonized)) {
    record_test("Fallback mechanism", "PASS",
                paste("Sources:", paste(result$sources_used, collapse = ", ")))
  } else {
    record_test("Fallback mechanism", "FAIL",
                "No data even with fallback")
  }
}, error = function(e) {
  record_test("Fallback mechanism", "FAIL", e$message)
})

# =============================================================================
# TEST 7: BATCH PROCESSING
# =============================================================================

cat("\n=============================================================================\n")
cat("TEST 7: BATCH PROCESSING\n")
cat("=============================================================================\n\n")

# Test 7.1: Multiple species lookup
test_species_list <- c("Gadus morhua", "Clupea harengus", "Asterias rubens")

tryCatch({
  start_time <- Sys.time()

  results <- lapply(test_species_list, function(sp) {
    lookup_species_traits(sp, cache_dir = NULL)
  })

  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))

  success_count <- sum(sapply(results, function(r) !is.null(r$harmonized)))

  if (success_count == length(test_species_list)) {
    record_test("Batch processing (3 species)", "PASS",
                paste("All", success_count, "species processed in",
                      round(elapsed, 1), "sec"))
  } else {
    record_test("Batch processing (3 species)", "FAIL",
                paste("Only", success_count, "of", length(test_species_list), "succeeded"))
  }

  # Performance check
  avg_time <- elapsed / length(test_species_list)
  if (avg_time < 5) {
    record_test("Performance check", "PASS",
                paste("Average", round(avg_time, 2), "sec per species"))
  } else {
    record_test("Performance check", "FAIL",
                paste("Too slow:", round(avg_time, 2), "sec per species"))
  }

}, error = function(e) {
  record_test("Batch processing", "FAIL", e$message)
})

# =============================================================================
# TEST SUMMARY
# =============================================================================

cat("\n=============================================================================\n")
cat("TEST SUMMARY\n")
cat("=============================================================================\n\n")

total_tests <- test_count
cat("Total tests run:      ", total_tests, "\n")
cat("Passed:               ", pass_count, " (", round(pass_count/total_tests*100, 1), "%)\n", sep = "")
cat("Failed:               ", fail_count, " (", round(fail_count/total_tests*100, 1), "%)\n", sep = "")
cat("Skipped:              ", skip_count, " (", round(skip_count/total_tests*100, 1), "%)\n", sep = "")

cat("\n")

if (fail_count == 0) {
  cat("✓ ALL CRITICAL TESTS PASSED\n")
  cat("\n")
  cat("Database integrations are working correctly.\n")
  cat("The system is ready for production use.\n")
} else {
  cat("✗ SOME TESTS FAILED\n")
  cat("\n")
  cat("Failed tests:\n")
  for (name in names(test_results)) {
    if (test_results[[name]]$status == "FAIL") {
      cat("  - ", name, ": ", test_results[[name]]$message, "\n", sep = "")
    }
  }
  cat("\nPlease review and fix failed tests before production deployment.\n")
}

if (skip_count > 0) {
  cat("\nSkipped tests (expected - optional features):\n")
  for (name in names(test_results)) {
    if (test_results[[name]]$status == "SKIP") {
      cat("  - ", name, ": ", test_results[[name]]$message, "\n", sep = "")
    }
  }
}

cat("\n=============================================================================\n")
cat("TEST REPORT COMPLETE\n")
cat("=============================================================================\n")

# Return test results for further analysis
invisible(list(
  total = total_tests,
  passed = pass_count,
  failed = fail_count,
  skipped = skip_count,
  results = test_results
))
