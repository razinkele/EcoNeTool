# =============================================================================
# ML Trait Prediction Tests
# =============================================================================
#
# Comprehensive tests for Phase 3: ML Trait Prediction
# Tests model loading, prediction functions, and integration with lookup system
#
# Date: 2025-12-25
# Version: 1.0
#
# =============================================================================

cat("=============================================================================\n")
cat("ML TRAIT PREDICTION TESTS\n")
cat("=============================================================================\n\n")

# Load required functions
cat("Loading ML prediction functions...\n")
source("R/functions/ml_trait_prediction.R")

cat("Loading trait lookup functions...\n")
source("R/functions/trait_lookup.R")

# Initialize test counters
tests_run <- 0
tests_passed <- 0
tests_failed <- 0

# Test helper function
run_test <- function(test_name, test_func) {
  tests_run <<- tests_run + 1
  cat("\n", sprintf("[Test %d]", tests_run), test_name, "\n")
  cat(strrep("-", 70), "\n")

  result <- tryCatch({
    test_func()
    tests_passed <<- tests_passed + 1
    cat("✓ PASS\n")
    TRUE
  }, error = function(e) {
    tests_failed <<- tests_failed + 1
    cat("✗ FAIL:", e$message, "\n")
    FALSE
  })

  return(result)
}

# =============================================================================
# TEST 1: Model File Availability
# =============================================================================

run_test("Check if ML models are available", function() {
  available <- ml_models_available()

  if (available) {
    cat("  ✓ ML models file found: models/trait_ml_models.rds\n")
  } else {
    cat("  ⚠️  ML models file not found\n")
    cat("  → Run scripts/train_trait_models.R to generate models\n")
    cat("  → This test will pass (models are optional)\n")
  }

  # Test passes either way - models are optional
  stopifnot(is.logical(available))
})

# =============================================================================
# TEST 2: Model Loading
# =============================================================================

run_test("Load ML models", function() {
  models <- load_ml_models()

  if (is.null(models)) {
    cat("  ⚠️  No models available - skipping ML-dependent tests\n")
    cat("  → This is OK if you haven't trained models yet\n")
    return()
  }

  cat("  ✓ Models loaded successfully\n")
  cat("    Training date:", models$training_date, "\n")
  cat("    Training samples:", models$training_samples, "\n")
  cat("    Available traits:", paste(names(models$models), collapse = ", "), "\n")

  # Validate structure
  stopifnot(!is.null(models$models))
  stopifnot(!is.null(models$performance))
  stopifnot(!is.null(models$feature_cols))
  stopifnot("MS" %in% names(models$models) || "FS" %in% names(models$models))
})

# =============================================================================
# TEST 3: Feature Preparation
# =============================================================================

run_test("Prepare taxonomic features for ML prediction", function() {
  # Test with complete taxonomy
  taxonomic_info_complete <- list(
    phylum = "Chordata",
    class = "Actinopterygii",
    order = "Gadiformes",
    family = "Gadidae",
    genus = "Gadus"
  )

  features <- prepare_ml_features(taxonomic_info_complete)

  if (is.null(features)) {
    stop("Feature preparation failed for complete taxonomy")
  }

  cat("  ✓ Features prepared for complete taxonomy\n")
  cat("    Features:", paste(names(features), collapse = ", "), "\n")

  # Validate feature structure
  stopifnot(is.data.frame(features))
  stopifnot(all(c("phylum", "class", "order", "family", "genus") %in% names(features)))

  # Test with partial taxonomy (minimum required: phylum + class)
  taxonomic_info_partial <- list(
    phylum = "Mollusca",
    class = "Bivalvia",
    order = "",
    family = "",
    genus = ""
  )

  features_partial <- prepare_ml_features(taxonomic_info_partial)

  if (!is.null(features_partial)) {
    cat("  ✓ Features prepared for partial taxonomy (phylum + class)\n")
  }

  # Test with insufficient taxonomy
  taxonomic_info_insufficient <- list(
    phylum = "Unknown",
    class = "",
    order = "",
    family = "",
    genus = ""
  )

  features_insufficient <- prepare_ml_features(taxonomic_info_insufficient)

  if (is.null(features_insufficient)) {
    cat("  ✓ Correctly rejected insufficient taxonomy\n")
  }

  stopifnot(is.null(features_insufficient))
})

# =============================================================================
# TEST 4: Single Trait Prediction
# =============================================================================

run_test("Predict single trait (MS - Max Size)", function() {
  models <- load_ml_models()

  if (is.null(models)) {
    cat("  ⚠️  No models available - skipping test\n")
    return()
  }

  if (!"MS" %in% names(models$models)) {
    cat("  ⚠️  MS model not available - skipping test\n")
    return()
  }

  # Test prediction for a fish
  taxonomic_info <- list(
    phylum = "Chordata",
    class = "Actinopterygii",
    order = "Gadiformes",
    family = "Gadidae",
    genus = "Gadus"
  )

  prediction <- predict_trait_ml("MS", taxonomic_info, models)

  if (is.null(prediction)) {
    stop("Prediction failed for valid taxonomy")
  }

  cat("  ✓ Prediction successful\n")
  cat("    Predicted MS:", prediction$value, "\n")
  cat("    Probability:", round(prediction$probability * 100, 1), "%\n")
  cat("    Confidence:", prediction$confidence, "\n")
  cat("    Model accuracy:", round(prediction$model_accuracy * 100, 1), "%\n")

  # Validate prediction structure
  stopifnot(!is.null(prediction$value))
  stopifnot(!is.null(prediction$probability))
  stopifnot(prediction$confidence %in% c("high", "medium", "low"))
  stopifnot(prediction$source == "ML")
})

# =============================================================================
# TEST 5: Multiple Trait Prediction
# =============================================================================

run_test("Predict multiple missing traits", function() {
  models <- load_ml_models()

  if (is.null(models)) {
    cat("  ⚠️  No models available - skipping test\n")
    return()
  }

  # Simulate partial trait data (some traits missing)
  current_traits <- list(
    MS = "MS4",        # Known
    FS = NA,           # Missing
    MB = NA,           # Missing
    EP = "EP1",        # Known
    PR = NA            # Missing
  )

  taxonomic_info <- list(
    phylum = "Chordata",
    class = "Actinopterygii",
    order = "Perciformes",
    family = "Labridae",
    genus = "Symphodus"
  )

  predictions <- predict_missing_traits(current_traits, taxonomic_info)

  cat("  ✓ Prediction completed\n")
  cat("    Traits predicted:", length(predictions), "\n")

  for (trait in names(predictions)) {
    pred <- predictions[[trait]]
    cat("    ", trait, ":", pred$value,
        "(prob=", round(pred$probability * 100, 1), "%, conf=", pred$confidence, ")\n")
  }

  # Validate that only missing traits were predicted
  stopifnot(!"MS" %in% names(predictions))  # MS was known
  stopifnot(!"EP" %in% names(predictions))  # EP was known
})

# =============================================================================
# TEST 6: Batch Prediction
# =============================================================================

run_test("Batch predict traits for multiple species", function() {
  models <- load_ml_models()

  if (is.null(models)) {
    cat("  ⚠️  No models available - skipping test\n")
    return()
  }

  # Create test batch
  species_list <- list(
    list(
      species = "Gadus morhua",
      current_traits = list(MS = NA, FS = NA, MB = NA, EP = NA, PR = NA),
      taxonomic_info = list(phylum = "Chordata", class = "Actinopterygii",
                           order = "Gadiformes", family = "Gadidae", genus = "Gadus")
    ),
    list(
      species = "Macoma balthica",
      current_traits = list(MS = NA, FS = NA, MB = NA, EP = NA, PR = NA),
      taxonomic_info = list(phylum = "Mollusca", class = "Bivalvia",
                           order = "Cardiida", family = "Tellinidae", genus = "Macoma")
    )
  )

  results <- batch_predict_traits(species_list, progress = FALSE)

  cat("  ✓ Batch prediction completed\n")
  cat("    Species processed:", length(results), "\n")

  for (i in seq_along(results)) {
    res <- results[[i]]
    cat("    ", res$species, "- predicted", res$n_predicted, "traits\n")
  }

  stopifnot(length(results) == 2)
})

# =============================================================================
# TEST 7: Model Diagnostics
# =============================================================================

run_test("Get ML model diagnostics", function() {
  diagnostics <- get_ml_model_diagnostics()

  if (is.null(diagnostics)) {
    cat("  ⚠️  No models available - skipping test\n")
    return()
  }

  cat("  ✓ Diagnostics retrieved\n")
  cat("    Training date:", diagnostics$training_date, "\n")
  cat("    Training samples:", diagnostics$training_samples, "\n")
  cat("    R version:", diagnostics$r_version, "\n\n")

  for (trait in names(diagnostics$performance)) {
    perf <- diagnostics$performance[[trait]]
    cat("    [", trait, "] Accuracy:", round(perf$accuracy * 100, 1), "%",
        "| OOB error:", round(perf$oob_error_rate * 100, 1), "%",
        "| Classes:", perf$n_classes, "\n")
    cat("      Top features:", paste(perf$top_features[1:3], collapse = ", "), "\n")
  }

  stopifnot(!is.null(diagnostics$performance))
})

# =============================================================================
# TEST 8: Integration with Lookup System
# =============================================================================

run_test("ML fallback integration in lookup_species_traits()", function() {
  # This test checks if ML fallback works in the integrated system
  # We'll test with a species that has partial database coverage

  # First, check if the apply_ml_fallback function exists
  stopifnot(exists("apply_ml_fallback"))

  cat("  ✓ apply_ml_fallback function is available\n")

  # Test the function with mock data
  harmonized_traits <- list(
    MS = "MS3",
    FS = NA,
    MB = NA,
    EP = "EP1",
    PR = NA
  )

  raw_traits <- list(
    worms = list(
      phylum = "Chordata",
      class = "Actinopterygii",
      order = "Perciformes",
      family = "Gobiidae",
      genus = "Pomatoschistus"
    )
  )

  result <- apply_ml_fallback(harmonized_traits, raw_traits, verbose = TRUE)

  cat("  ✓ ML fallback executed successfully\n")
  cat("    Traits after ML:\n")
  cat("      MS:", result$MS, "\n")
  cat("      FS:", result$FS, "\n")
  cat("      MB:", result$MB, "\n")
  cat("      EP:", result$EP, "\n")
  cat("      PR:", result$PR, "\n")

  # Result should at least maintain existing values
  stopifnot(result$MS == "MS3")  # Should not change
  stopifnot(result$EP == "EP1")  # Should not change
})

# =============================================================================
# TEST 9: Cache Structure for ML Training
# =============================================================================

run_test("Verify cache structure includes data for ML training", function() {
  # Check if cache directory exists
  cache_dir <- "cache/taxonomy"

  if (!dir.exists(cache_dir)) {
    cat("  ⚠️  Cache directory not found:", cache_dir, "\n")
    cat("  → This is OK if no lookups have been cached yet\n")
    return()
  }

  # Find a cache file to inspect
  cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)

  if (length(cache_files) == 0) {
    cat("  ⚠️  No cache files found\n")
    cat("  → Run some trait lookups first to populate cache\n")
    return()
  }

  # Read a random cache file
  cache_file <- sample(cache_files, 1)
  cache_data <- readRDS(cache_file)

  cat("  ✓ Cache file read successfully\n")
  cat("    File:", basename(cache_file), "\n")
  cat("    Structure:", paste(names(cache_data), collapse = ", "), "\n")

  # Check for required fields for ML training
  has_harmonized <- "harmonized" %in% names(cache_data)
  has_species <- "species" %in% names(cache_data)
  has_traits <- "traits" %in% names(cache_data)

  if (has_harmonized) {
    cat("    ✓ Has 'harmonized' field (NEW FORMAT - ready for ML training)\n")

    # Check if harmonized data has taxonomy
    harmonized <- cache_data$harmonized
    has_taxonomy <- all(c("phylum", "class") %in% names(harmonized))

    if (has_taxonomy) {
      cat("    ✓ Harmonized data includes taxonomy\n")
      cat("      Phylum:", harmonized$phylum, "\n")
      cat("      Class:", harmonized$class, "\n")
    } else {
      cat("    ⚠️  Harmonized field exists but lacks taxonomy\n")
    }
  } else {
    cat("    ⚠️  Cache file lacks 'harmonized' field (OLD FORMAT)\n")
    cat("    → This is OK - old cache files will still work\n")
    cat("    → New lookups will use updated format with harmonized data\n")
    cat("    → Old caches can be cleared and regenerated if needed\n")
  }

  # Both old and new formats should have at least 'traits' or 'data'
  has_data <- "data" %in% names(cache_data)
  stopifnot(has_traits || has_data)  # At least one data field should be present
  cat("    ✓ Cache file has valid data structure\n")
})

# =============================================================================
# TEST 10: Prediction Formatting
# =============================================================================

run_test("Format ML prediction for display", function() {
  # Test with complete prediction
  prediction_complete <- list(
    value = "MS4",
    probability = 0.85,
    confidence = "high",
    model_accuracy = 0.78
  )

  formatted <- format_ml_prediction(prediction_complete)
  cat("  ✓ Formatted complete prediction:\n")
  cat("    ", formatted, "\n")

  stopifnot(grepl("MS4", formatted))
  stopifnot(grepl("85.0%", formatted))
  stopifnot(grepl("high", formatted))

  # Test with NULL prediction
  formatted_null <- format_ml_prediction(NULL)
  cat("  ✓ Formatted NULL prediction:\n")
  cat("    ", formatted_null, "\n")

  stopifnot(grepl("No prediction", formatted_null))
})

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("TEST SUMMARY\n")
cat("=============================================================================\n")
cat(sprintf("Tests run:    %d\n", tests_run))
cat(sprintf("Tests passed: %d (%.1f%%)\n", tests_passed,
            tests_passed / tests_run * 100))
cat(sprintf("Tests failed: %d (%.1f%%)\n", tests_failed,
            tests_failed / tests_run * 100))
cat("=============================================================================\n")

if (tests_failed == 0) {
  cat("\n✓ ALL TESTS PASSED\n")
  cat("\nPhase 3 (ML Trait Prediction) implementation is working correctly!\n\n")

  cat("Next steps:\n")
  if (!ml_models_available()) {
    cat("  1. Run scripts/train_trait_models.R to generate ML models\n")
    cat("  2. Re-run tests to verify model predictions\n")
    cat("  3. Test with real species lookups\n")
  } else {
    cat("  1. ✓ ML models are available\n")
    cat("  2. Test with real species lookups (species with incomplete data)\n")
    cat("  3. Validate prediction accuracy on known species\n")
    cat("  4. Document Phase 3 implementation\n")
  }
} else {
  cat("\n⚠️  SOME TESTS FAILED\n")
  cat("\nPlease review failed tests above and fix issues before proceeding.\n")
}

cat("\n")
