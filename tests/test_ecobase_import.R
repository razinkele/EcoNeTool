#' Test EcoBase Import Fix
#'
#' Tests the fixed EcoBase connection to ensure atomic vector accessors work
#'
#' This script tests:
#' 1. Connection to EcoBase
#' 2. Retrieving model list
#' 3. Downloading a small model (input parameters)
#' 4. Converting to EcoNeTool format
#' 5. Verifying network structure

cat("\n=== EcoBase Import Test ===\n\n")

# Source the fixed module
source("R/functions/ecobase_connection.R")

# Test 1: Check packages
cat("Step 1: Checking required packages...\n")
tryCatch({
  require_ecobase_packages()
  cat("✓ All required packages available\n\n")
}, error = function(e) {
  cat("✗ Missing packages:", e$message, "\n")
  cat("\nPlease install:\n")
  cat("  install.packages(c('RCurl', 'XML', 'plyr', 'dplyr'))\n\n")
  stop("Cannot proceed without required packages")
})

# Test 2: Connect to EcoBase
cat("Step 2: Connecting to EcoBase...\n")
tryCatch({
  models <- get_ecobase_models()
  cat("✓ Retrieved", nrow(models), "available models\n\n")

  # Show first few models with dynamic column detection
  cat("Sample models:\n")
  all_cols <- colnames(models)
  cat("  Available columns:", paste(head(all_cols, 5), collapse = ", "), "...\n")

  # Try to find relevant columns
  id_col <- grep("model.*number|^id$|modelid", all_cols, ignore.case = TRUE, value = TRUE)[1]
  name_col <- grep("model.*name|modelname", all_cols, ignore.case = TRUE, value = TRUE)[1]
  eco_col <- grep("ecosystem", all_cols, ignore.case = TRUE, value = TRUE)[1]

  # Use found columns or fall back to first few
  display_cols <- c(id_col, name_col, eco_col)
  display_cols <- display_cols[!is.na(display_cols)]

  if (length(display_cols) > 0) {
    print(head(models[, display_cols, drop = FALSE], 3))
  } else {
    # Just show first 3 columns
    print(head(models[, 1:min(3, ncol(models)), drop = FALSE], 3))
  }
  cat("\n")

}, error = function(e) {
  cat("✗ Connection failed:", e$message, "\n\n")
  stop("Cannot proceed without EcoBase connection")
})

# Test 3: Download a small model (try model #403 - North Sea)
cat("Step 3: Downloading model #403 (North Sea example)...\n")
test_model_id <- 403

tryCatch({
  # Try to get model input
  input_data <- get_ecobase_model_input(test_model_id)
  cat("✓ Retrieved input data for model", test_model_id, "\n")
  cat("  Number of groups:", length(input_data), "\n")

  # Show structure of first group to verify accessor fix
  if (length(input_data) > 0) {
    cat("\n  First group structure:\n")
    first_group <- input_data[[1]]
    cat("    Type:", class(first_group), "\n")
    cat("    Length:", length(first_group), "\n")
    if (is.atomic(first_group)) {
      cat("    Names:", paste(head(names(first_group), 5), collapse = ", "), "\n")
      cat("    Sample values:", paste(head(first_group, 3), collapse = ", "), "\n")
    }
  }
  cat("\n")

}, error = function(e) {
  cat("✗ Model download failed:", e$message, "\n\n")
  stop("Cannot test conversion without model data")
})

# Test 4: Convert to EcoNeTool format
cat("Step 4: Converting model to EcoNeTool format...\n")
tryCatch({
  result <- convert_ecobase_to_econetool(test_model_id, use_output = FALSE)

  cat("✓ Conversion successful!\n")
  cat("\nNetwork summary:\n")
  cat("  Species/groups:", igraph::vcount(result$net), "\n")
  cat("  Trophic links:", igraph::ecount(result$net), "\n")
  cat("  Functional groups:", nlevels(result$info$fg), "\n")

  cat("\nSample species:\n")
  print(head(result$info[, c("species", "fg", "meanB", "PB", "QB")], 5))

  cat("\n✓ ALL TESTS PASSED\n")
  cat("\nEcoBase import is working correctly!\n")

}, error = function(e) {
  cat("✗ Conversion failed:", e$message, "\n")
  cat("\nError stack trace:\n")
  print(traceback())
  stop("Conversion test failed")
})

cat("\n=== Test Complete ===\n\n")
