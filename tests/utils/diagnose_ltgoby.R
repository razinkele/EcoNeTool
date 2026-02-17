# Diagnose LTgoby data for Rpath conversion issues

cat("Loading import functions...\n")
source("ecopath_windows_import.R")
source("rpath_integration.R")

cat("\n=== Importing LTgoby database ===\n")
data <- parse_ecopath_native_cross_platform('examples/LTgoby.eweaccdb')

cat("\n=== Group Data Summary ===\n")
cat("Number of groups:", nrow(data$group_data), "\n")
cat("Columns:", ncol(data$group_data), "\n\n")

# Check key columns
key_cols <- c('GroupName', 'Biomass', 'ProdBiom', 'ConsBiom', 'EcoEfficiency')
existing_cols <- key_cols[key_cols %in% colnames(data$group_data)]

cat("Key columns present:", paste(existing_cols, collapse = ", "), "\n\n")

# Check for NA values in key columns
cat("=== NA Value Check ===\n")
for (col in existing_cols) {
  n_na <- sum(is.na(data$group_data[[col]]))
  if (n_na > 0) {
    cat(sprintf("%s: %d NA values\n", col, n_na))
  }
}

# Check for -9999 values (ECOPATH missing data indicator)
cat("\n=== ECOPATH Missing Value Indicator (-9999) ===\n")
for (col in c('Biomass', 'ProdBiom', 'ConsBiom', 'EcoEfficiency')) {
  if (col %in% colnames(data$group_data)) {
    n_missing <- sum(data$group_data[[col]] < -9000, na.rm = TRUE)
    if (n_missing > 0) {
      cat(sprintf("%s: %d values < -9000\n", col, n_missing))
    }
  }
}

# Display group data
cat("\n=== Group Data Preview ===\n")
print(data$group_data[, existing_cols])

# Check diet data
cat("\n=== Diet Data Summary ===\n")
cat("Number of diet links:", nrow(data$diet_data), "\n")
cat("Diet columns:", paste(colnames(data$diet_data), collapse = ", "), "\n")

# Check for NA in diet
n_diet_na <- sum(is.na(data$diet_data$Diet))
if (n_diet_na > 0) {
  cat("Warning: ", n_diet_na, " NA values in diet proportions\n")
}

# Try conversion and capture error
cat("\n=== Testing Rpath Conversion ===\n")
result <- tryCatch({
  rpath_params <- convert_ecopath_to_rpath(data, model_name = "LTgoby Test")
  cat("✓ Conversion successful!\n")
  cat("Rpath groups:", nrow(rpath_params$model), "\n")
  rpath_params
}, error = function(e) {
  cat("✗ Conversion failed:\n")
  cat("Error:", e$message, "\n")
  cat("\nDetailed traceback:\n")
  print(traceback())
  NULL
})

cat("\n=== Diagnosis Complete ===\n")
