# Compare working minimal model with LTgoby conversion

library(Rpath)

source("R/config.R")
source("R/functions/ecopath_import.R")
source("R/functions/rpath_integration.R")

# Load working example
working <- readRDS("rpath_working_example.rds")

# Load LTgoby
ecopath_data <- parse_ecopath_native_windows("examples/LTgoby.eweaccdb")
ltgoby <- convert_ecopath_to_rpath(ecopath_data, model_name = "LTgoby")

cat("\n=== DETAILED COLUMN COMPARISON ===\n\n")

# Check specific problematic columns
check_cols <- c("Detritus", "Fleet", "Fleet.disc", "DummyFleet", "DummyFleet.disc")

cat("Working model columns:\n")
for (col in check_cols) {
  if (col %in% names(working$model)) {
    na_count <- sum(is.na(working$model[[col]]))
    cat(sprintf("  %-20s: %d NA values\n", col, na_count))
    cat("    Values:", paste(head(working$model[[col]], 5), collapse=", "), "\n")
  } else {
    cat(sprintf("  %-20s: NOT PRESENT\n", col))
  }
}

cat("\nLTgoby model columns:\n")
for (col in check_cols) {
  if (col %in% names(ltgoby$model)) {
    na_count <- sum(is.na(ltgoby$model[[col]]))
    cat(sprintf("  %-20s: %d NA values\n", col, na_count))
    cat("    First 5 values:", paste(head(ltgoby$model[[col]], 5), collapse=", "), "\n")
  } else {
    cat(sprintf("  %-20s: NOT PRESENT\n", col))
  }
}

# Check what the Detritus column should contain
cat("\n=== DETRITUS COLUMN INVESTIGATION ===\n\n")

cat("In working model, 'Detritus' column has group name:", working$model$Group[4], "\n")
cat("Type:", working$model$Type[4], "\n")
cat("Detritus column value:", working$model$Detritus[4], "\n\n")

# Find detritus group in LTgoby
det_idx <- which(ltgoby$model$Type == 2)
cat("LTgoby detritus groups (Type=2):\n")
for (idx in det_idx) {
  cat(sprintf("  Group '%s' (row %d):\n", ltgoby$model$Group[idx], idx))
  cat(sprintf("    Detritus column: %s\n", ltgoby$model$Detritus[idx]))
}

# Try to balance LTgoby
cat("\n=== ATTEMPTING TO BALANCE LTGOBY ===\n\n")

result <- tryCatch({
  balanced <- run_ecopath_balance(ltgoby, balance = TRUE)
  list(success = TRUE, model = balanced)
}, error = function(e) {
  list(success = FALSE, error = conditionMessage(e))
})

if (result$success) {
  cat("✅ SUCCESS!\n")
} else {
  cat("❌ FAILED\n")
  cat("Error:\n", result$error, "\n\n")

  # Try raw rpath() call
  cat("Trying direct Rpath::rpath() call...\n")
  raw_result <- tryCatch({
    raw_model <- Rpath::rpath(ltgoby, eco.name = "LTgoby")
    list(success = TRUE, model = raw_model)
  }, error = function(e) {
    list(success = FALSE, error = conditionMessage(e), trace = traceback())
  })

  if (raw_result$success) {
    cat("✅ Raw rpath() succeeded!\n")
    cat("Issue is in our run_ecopath_balance() function\n")
  } else {
    cat("❌ Raw rpath() also failed\n")
    cat("Error:", raw_result$error, "\n")
  }
}
