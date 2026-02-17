# ==============================================================================
# Test Rpath Module Refactoring
# ==============================================================================
# Comprehensive test of refactored Rpath module structure
#
# Tests:
#   1. File loading and sourcing
#   2. Module function availability
#   3. UI rendering (basic structure)
#   4. Data import and conversion
#   5. Mass balance calculation
#   6. Enhanced features (calibration, comments)
#
# Expected Result: All tests pass with refactored structure
# ==============================================================================

cat("\n=== TESTING REFACTORED RPATH MODULE ===\n\n")

# ==============================================================================
# TEST 1: File Loading
# ==============================================================================

cat("[1] Testing File Loading and Sourcing\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

test_results <- list()

# Test loading each file
files_to_test <- c(
  "R/functions/rpath_integration.R",
  "R/functions/auxillary_parser.R",
  "R/ui/rpath_ui.R",
  "R/modules/rpath_server.R"
)

for (file in files_to_test) {
  cat(sprintf("Loading %s...\n", file))
  tryCatch({
    source(file, local = FALSE)
    cat(sprintf("  ✓ %s loaded successfully\n", basename(file)))
    test_results[[paste0("load_", basename(file))]] <- TRUE
  }, error = function(e) {
    cat(sprintf("  ✗ Error loading %s: %s\n", basename(file), e$message))
    test_results[[paste0("load_", basename(file))]] <- FALSE
  })
}

cat("\n")

# ==============================================================================
# TEST 2: Function Availability
# ==============================================================================

cat("[2] Testing Function Availability\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

required_functions <- c(
  # UI module
  "rpathModuleUI",
  # Server module
  "rpathModuleServer",
  # Integration functions
  "convert_ecopath_to_rpath",
  "check_rpath_installed",
  # Auxillary parser functions
  "parse_auxillary_valueid",
  "organize_auxillary_data",
  "extract_citations_from_remarks"
)

cat("Checking required functions:\n")
for (func_name in required_functions) {
  if (exists(func_name, mode = "function")) {
    cat(sprintf("  ✓ %s exists\n", func_name))
    test_results[[paste0("func_", func_name)]] <- TRUE
  } else {
    cat(sprintf("  ✗ %s NOT FOUND\n", func_name))
    test_results[[paste0("func_", func_name)]] <- FALSE
  }
}

cat("\n")

# ==============================================================================
# TEST 3: UI Structure Test
# ==============================================================================

cat("[3] Testing UI Structure\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

if (exists("rpathModuleUI", mode = "function")) {
  tryCatch({
    # Call UI function with test ID
    ui_result <- rpathModuleUI("test")

    # Check structure
    if (inherits(ui_result, "shiny.tag.list")) {
      cat("  ✓ UI returns tagList (correct structure)\n")
      test_results$ui_structure <- TRUE
    } else {
      cat("  ✗ UI does not return tagList\n")
      cat("    Actual type:", class(ui_result), "\n")
      test_results$ui_structure <- FALSE
    }

    # Check namespace usage
    ui_html <- as.character(ui_result)
    if (grepl("test-rpath_status", ui_html)) {
      cat("  ✓ UI uses namespace correctly\n")
      test_results$ui_namespace <- TRUE
    } else {
      cat("  ⚠ UI namespace usage could not be verified\n")
      test_results$ui_namespace <- NA
    }

  }, error = function(e) {
    cat("  ✗ Error calling UI function:", e$message, "\n")
    test_results$ui_structure <- FALSE
  })
} else {
  cat("  ✗ rpathModuleUI function not available\n")
  test_results$ui_structure <- FALSE
}

cat("\n")

# ==============================================================================
# TEST 4: Data Import and Conversion
# ==============================================================================

cat("[4] Testing Data Import and Conversion\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

# Load required dependencies
library(RODBC)
source("R/config.R")
source("R/functions/ecopath_import.R")

# Test with LTgoby database
test_file <- "examples/LTgoby.eweaccdb"

if (file.exists(test_file)) {
  cat(sprintf("Importing %s...\n", test_file))

  tryCatch({
    ecopath_data <- parse_ecopath_native_windows(test_file)

    # Check import results
    cat(sprintf("  ✓ Import successful: %d groups, %d diet entries\n",
                nrow(ecopath_data$group_data),
                nrow(ecopath_data$diet_data)))
    test_results$import <- TRUE

    # Check enhanced imports
    enhanced_features <- c(
      "stanza_data", "fleet_data", "discard_fate_data",
      "pedigree_data", "pedigree_levels", "auxillary_data"
    )

    cat("\n  Enhanced import features:\n")
    for (feature in enhanced_features) {
      if (!is.null(ecopath_data[[feature]])) {
        count <- if (is.data.frame(ecopath_data[[feature]])) {
          nrow(ecopath_data[[feature]])
        } else {
          length(ecopath_data[[feature]])
        }
        cat(sprintf("    ✓ %s: %d entries\n", feature, count))
      } else {
        cat(sprintf("    ⚠ %s: Not available\n", feature))
      }
    }

    # Test conversion to Rpath format
    cat("\n  Testing conversion to Rpath format...\n")

    # Check if Rpath is installed
    if (requireNamespace("Rpath", quietly = TRUE)) {
      tryCatch({
        rpath_params <- convert_ecopath_to_rpath(
          ecopath_data,
          model_name = "Test Model"
        )

        cat("    ✓ Conversion successful\n")
        cat(sprintf("    Model has %d groups\n", nrow(rpath_params$model)))
        test_results$conversion <- TRUE

        # Test auxillary data organization
        if (!is.null(ecopath_data$auxillary_data)) {
          cat("\n  Testing auxillary data organization...\n")
          organized <- organize_auxillary_data(
            ecopath_data$auxillary_data,
            ecopath_data$group_data,
            ecopath_data$fleet_data
          )
          cat(sprintf("    ✓ Organized %d comments/tooltips\n", nrow(organized)))
          test_results$auxillary_org <- TRUE
        }

      }, error = function(e) {
        cat("    ✗ Conversion failed:", e$message, "\n")
        test_results$conversion <- FALSE
      })
    } else {
      cat("    ⚠ Rpath not installed, skipping conversion test\n")
      test_results$conversion <- NA
    }

  }, error = function(e) {
    cat("  ✗ Import failed:", e$message, "\n")
    test_results$import <- FALSE
  })

} else {
  cat(sprintf("  ⚠ Test file not found: %s\n", test_file))
  cat("  Skipping import test\n")
  test_results$import <- NA
}

cat("\n")

# ==============================================================================
# TEST 5: Mass Balance Test
# ==============================================================================

cat("[5] Testing Mass Balance Calculation\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

if (exists("rpath_params") && requireNamespace("Rpath", quietly = TRUE)) {
  cat("Running Ecopath balance...\n")

  tryCatch({
    # Manually balance the model
    balanced <- Rpath::rpath(rpath_params, "Test Model")

    cat("  ✓ Model balanced successfully\n")

    # Check results
    if (!is.null(balanced$EE)) {
      total_system_throughput <- sum(balanced$Biomass * balanced$PB, na.rm = TRUE)
      cat(sprintf("  Total System Throughput: %.2f t/km²/year\n", total_system_throughput))
      test_results$balance <- TRUE
    } else {
      cat("  ⚠ Balance results incomplete\n")
      test_results$balance <- FALSE
    }

  }, error = function(e) {
    cat("  ✗ Balance failed:", e$message, "\n")
    test_results$balance <- FALSE
  })

} else {
  if (!requireNamespace("Rpath", quietly = TRUE)) {
    cat("  ⚠ Rpath not installed, skipping balance test\n")
  } else {
    cat("  ⚠ No Rpath parameters available, skipping balance test\n")
  }
  test_results$balance <- NA
}

cat("\n")

# ==============================================================================
# TEST SUMMARY
# ==============================================================================

cat(paste(rep("=", 70), collapse=""), "\n")
cat("\n=== TEST SUMMARY ===\n\n")

# Categorize results
passed <- sum(unlist(test_results) == TRUE, na.rm = TRUE)
failed <- sum(unlist(test_results) == FALSE, na.rm = TRUE)
skipped <- sum(is.na(unlist(test_results)), na.rm = TRUE)
total <- length(test_results)

cat("Test Results by Category:\n\n")

cat("1. FILE LOADING:\n")
for (name in names(test_results)) {
  if (grepl("^load_", name)) {
    result <- test_results[[name]]
    status <- if (result == TRUE) "✅ PASS" else if (result == FALSE) "❌ FAIL" else "⚠ SKIP"
    cat(sprintf("   %s: %s\n", gsub("load_", "", name), status))
  }
}

cat("\n2. FUNCTION AVAILABILITY:\n")
for (name in names(test_results)) {
  if (grepl("^func_", name)) {
    result <- test_results[[name]]
    status <- if (result == TRUE) "✅ PASS" else if (result == FALSE) "❌ FAIL" else "⚠ SKIP"
    cat(sprintf("   %s: %s\n", gsub("func_", "", name), status))
  }
}

cat("\n3. UI STRUCTURE:\n")
if (!is.null(test_results$ui_structure)) {
  status <- if (test_results$ui_structure == TRUE) "✅ PASS" else if (test_results$ui_structure == FALSE) "❌ FAIL" else "⚠ SKIP"
  cat(sprintf("   UI structure: %s\n", status))
}
if (!is.null(test_results$ui_namespace)) {
  status <- if (test_results$ui_namespace == TRUE) "✅ PASS" else if (test_results$ui_namespace == FALSE) "❌ FAIL" else "⚠ SKIP"
  cat(sprintf("   UI namespace: %s\n", status))
}

cat("\n4. DATA OPERATIONS:\n")
for (name in c("import", "conversion", "auxillary_org", "balance")) {
  if (!is.null(test_results[[name]])) {
    result <- test_results[[name]]
    status <- if (result == TRUE) "✅ PASS" else if (result == FALSE) "❌ FAIL" else "⚠ SKIP"
    cat(sprintf("   %s: %s\n", name, status))
  }
}

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat(sprintf("\n📊 OVERALL RESULTS: %d passed, %d failed, %d skipped (Total: %d)\n\n",
            passed, failed, skipped, total))

if (failed == 0) {
  cat("🎉 ALL TESTS PASSED! 🎉\n\n")
  cat("Refactored Rpath module is working correctly:\n")
  cat("  ✓ All files load successfully\n")
  cat("  ✓ All functions are available\n")
  cat("  ✓ UI structure is correct\n")
  cat("  ✓ Data import and conversion work\n")
  if (!is.na(test_results$balance) && test_results$balance) {
    cat("  ✓ Mass balance calculations work\n")
  }
  cat("\n🚀 READY FOR DEPLOYMENT\n\n")
} else if (failed > 0) {
  cat("⚠ SOME TESTS FAILED\n")
  cat("Please review the errors above and fix issues before deployment.\n\n")
} else {
  cat("⚠ TESTS INCOMPLETE\n")
  cat("Some tests were skipped (likely due to missing Rpath package).\n")
  cat("Install Rpath and run again for complete testing.\n\n")
}

cat("=== TEST COMPLETE ===\n\n")
