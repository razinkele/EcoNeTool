# Quick Rpath Module Validation Test
# Fast test to verify basic functionality

cat("\nQuick Rpath Module Test\n")
cat(strrep("=", 50), "\n\n")

# Load files
cat("Loading files...\n")
source("ecopath_windows_import.R")
source("rpath_integration.R")
source("R/modules/rpath_module.R")
cat("✓ Files loaded\n\n")

# Test import
cat("Testing ECOPATH import...\n")
data <- parse_ecopath_native_cross_platform("examples/LTgoby.eweaccdb")
cat("✓ Import successful:", nrow(data$group_data), "groups\n\n")

# Test Rpath availability
cat("Checking Rpath package...\n")
if (requireNamespace("Rpath", quietly = TRUE)) {
  cat("✓ Rpath available\n\n")
  
  # Test conversion
  cat("Testing conversion to Rpath format...\n")
  params <- convert_ecopath_to_rpath(data)
  cat("✓ Conversion successful\n\n")
  
  # Test mass balance
  cat("Testing mass balance...\n")
  model <- Rpath::rpath(params)
  cat("✓ Mass balance complete\n\n")
  
  cat("========================================\n")
  cat("✓ ALL QUICK TESTS PASSED\n")
  cat("========================================\n\n")
} else {
  cat("⚠ Rpath not installed\n")
  cat("Install with: install.packages('Rpath')\n\n")
}
