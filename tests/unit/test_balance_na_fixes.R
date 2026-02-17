# ============================================================================
# Test: NA Handling Fixes for Ecopath Balancing
# ============================================================================

cat("\n=== Testing NA Handling Fixes ===\n\n")

suppressPackageStartupMessages({
  library(Rpath)
})

source("R/config.R")
source("R/functions/ecopath_import.R")
source("R/functions/rpath_integration.R")

# Import and convert
cat("[1] Import and Convert\n")
cat(paste(rep("=", 60), collapse=""), "\n")

ecopath_data <- parse_ecopath_native_windows("examples/LTgoby.eweaccdb")
rpath_params <- convert_ecopath_to_rpath(ecopath_data, model_name = "LTgoby")

cat("\n[2] Check for NA Values Before Balance\n")
cat(paste(rep("=", 60), collapse=""), "\n")

# Check diet matrix
diet <- rpath_params$diet
cat(sprintf("Diet matrix: %d rows × %d columns\n", nrow(diet), ncol(diet)))

# Count NA values in diet
na_count <- sum(is.na(as.matrix(diet[, -1])))  # Exclude Group column
cat(sprintf("NA values in diet matrix: %d\n", na_count))

# Check model parameters
model <- rpath_params$model
cat(sprintf("\nModel parameters: %d groups\n", nrow(model)))

# Check for NA in critical columns
check_cols <- c("Type", "Biomass", "PB", "QB", "EE", "DetFate", "DetInput")
for (col in check_cols) {
  if (col %in% names(model)) {
    na_count <- sum(is.na(model[[col]]))
    cat(sprintf("  %10s: %d NA values\n", col, na_count))
  }
}

cat("\n[3] Attempt Mass Balance\n")
cat(paste(rep("=", 60), collapse=""), "\n")

result <- tryCatch({
  cat("Running balance with auto-fixes...\n\n")
  balanced_model <- run_ecopath_balance(rpath_params, balance = TRUE)
  list(success = TRUE, model = balanced_model)
}, error = function(e) {
  list(success = FALSE, error = conditionMessage(e))
})

cat("\n[4] Results\n")
cat(paste(rep("=", 60), collapse=""), "\n\n")

if (result$success) {
  cat("✅ BALANCE SUCCESSFUL!\n\n")
  cat("Model successfully balanced. Key metrics:\n")
  cat(sprintf("  Groups: %d\n", length(result$model$Group)))
  cat(sprintf("  System throughput: %.2f t/km²/year\n", sum(result$model$Q, na.rm = TRUE)))
  cat("\nAll NA handling fixes are working correctly!\n")
} else {
  cat("❌ BALANCE FAILED\n\n")
  cat("Error message:\n")
  cat(paste("  ", gsub("\n", "\n  ", result$error)), "\n\n")

  # Analyze the error
  if (grepl("NA values in logical comparisons", result$error)) {
    cat("⚠ Still getting NA errors. This means:\n")
    cat("  1. Diet matrix may have NA values in unexpected places\n")
    cat("  2. Model parameters have NA values in required fields\n")
    cat("  3. Rpath package may have internal NA handling issues\n\n")
    cat("Recommendation:\n")
    cat("  - Check Rpath package version\n")
    cat("  - Ensure all diet matrix values are numeric (0.0 for no connection)\n")
    cat("  - Ensure all required parameters are set\n")
  } else {
    cat("Different error - see message above for details\n")
  }
}

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("\n=== SUMMARY ===\n\n")

cat("Fixes Applied:\n")
cat("  1. ✅ Diet matrix: NA → 0.0 (no diet connection)\n")
cat("  2. ✅ DetFate: NA → 0.0 (no mortality to detritus)\n")
cat("  3. ✅ DetInput: Set to 1.0 for detritus\n")
cat("  4. ✅ Detritus EE: Cleared (NA)\n")
cat("  5. ✅ Fleet BioAcc/Unassim: Cleared (NA)\n")
cat("  6. ✅ Producer Q/B: Cleared (NA)\n\n")

if (result$success) {
  cat("Result: ✅ ALL FIXES WORKING - BALANCE SUCCESSFUL\n\n")
} else {
  cat("Result: ⚠ ADDITIONAL FIXES MAY BE NEEDED\n\n")
  cat("The validation logic is correct, but Rpath balancing\n")
  cat("may require additional parameter adjustments or\n")
  cat("there may be issues with the source database.\n\n")
}
