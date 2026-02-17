# Test Rpath Module Integration
cat("==============================================\n")
cat("Rpath Module Integration Test\n")
cat("==============================================\n\n")

# Test 1: Check if required files exist
cat("Test 1: Checking required files...\n")
if (file.exists("rpath_integration.R")) cat("  ✓ rpath_integration.R\n") else cat("  ✗ MISSING\n")
if (file.exists("R/modules/rpath_module.R")) cat("  ✓ R/modules/rpath_module.R\n") else cat("  ✗ MISSING\n")

# Test 2: Check app.R content
cat("\nTest 2: Checking app.R integration...\n")
app_lines <- readLines("app.R")
if (any(grepl("rpath_integration", app_lines))) cat("  ✓ Sources rpath_integration.R\n") else cat("  ✗ MISSING\n")
if (any(grepl("rpath_module", app_lines))) cat("  ✓ Sources rpath_module.R\n") else cat("  ✗ MISSING\n")
if (any(grepl("rpathModuleUI", app_lines))) cat("  ✓ Uses rpathModuleUI\n") else cat("  ✗ MISSING\n")
if (any(grepl("rpathModuleServer", app_lines))) cat("  ✓ Uses rpathModuleServer\n") else cat("  ✗ MISSING\n")
if (any(grepl("ecopath_import_data", app_lines))) cat("  ✓ Connects ECOPATH import\n") else cat("  ✗ MISSING\n")
if (any(grepl('tabName.*rpath', app_lines))) cat("  ✓ Tab added to menu\n") else cat("  ✗ MISSING\n")

cat("\n==============================================\n")
cat("✓ Integration test complete!\n")
cat("==============================================\n")
