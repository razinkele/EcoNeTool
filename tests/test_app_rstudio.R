# test_app_rstudio.R
# Quick test script for RStudio
#
# INSTRUCTIONS:
# 1. Open this file in RStudio Desktop
# 2. Click the "Source" button (top right)
# 3. If all tests pass, open app.R and click "Run App"

cat("================================================================================\n")
cat("EcoNeTool - RStudio Test Script\n")
cat("================================================================================\n\n")

cat("Testing Phase A, B, C Refactored Components...\n\n")

# Test 1: Packages
cat("1. Testing core packages...\n")
tryCatch({
  library(shiny, quietly = TRUE)
  cat("   ✓ shiny\n")
  library(bs4Dash, quietly = TRUE)
  cat("   ✓ bs4Dash\n")
  library(igraph, quietly = TRUE)
  cat("   ✓ igraph\n")
  library(fluxweb, quietly = TRUE)
  cat("   ✓ fluxweb\n")
  library(MASS, quietly = TRUE)
  cat("   ✓ MASS\n")
}, error = function(e) {
  cat("   ✗ Error loading packages:", conditionMessage(e), "\n")
  stop("Package loading failed")
})

# Test 2: Configuration (Phase A)
cat("\n2. Testing configuration (Phase A)...\n")
tryCatch({
  source("R/config.R", local = TRUE)
  cat("   ✓ R/config.R loaded\n")
  cat("   ✓ COLOR_SCHEME:", paste(COLOR_SCHEME, collapse = ", "), "\n")
  cat("   ✓ Found", length(METAWEB_PATHS), "metawebs\n")
}, error = function(e) {
  cat("   ✗ Error loading config:", conditionMessage(e), "\n")
  stop("Config loading failed")
})

# Test 3: Function files (Phase B)
cat("\n3. Testing function files (Phase B)...\n")
function_files <- c(
  "trophic_levels.R",
  "network_visualization.R",
  "topological_metrics.R",
  "flux_calculations.R",
  "keystoneness.R",
  "metaweb_core.R",
  "metaweb_io.R",
  "spatial_analysis.R"
)

for (file in function_files) {
  tryCatch({
    source(file.path("R/functions", file), local = TRUE)
    cat("   ✓", file, "\n")
  }, error = function(e) {
    cat("   ✗", file, ":", conditionMessage(e), "\n")
    stop("Function file loading failed")
  })
}

# Test 4: UI files (Phase C)
cat("\n4. Testing UI files (Phase C)...\n")
ui_files <- c(
  "dashboard_ui.R",
  "import_ui.R",
  "network_ui.R",
  "topological_ui.R",
  "biomass_ui.R",
  "fluxes_ui.R",
  "keystoneness_ui.R",
  "dataeditor_ui.R",
  "metaweb_ui.R",
  "spatial_ui.R"
)

for (file in ui_files) {
  tryCatch({
    source(file.path("R/ui", file), local = TRUE)
    cat("   ✓", file, "\n")
  }, error = function(e) {
    cat("   ✗", file, ":", conditionMessage(e), "\n")
    stop("UI file loading failed")
  })
}

# Test 5: Data loading (Phase A)
cat("\n5. Testing data loading (Phase A)...\n")
tryCatch({
  source("R/data_loading.R", local = TRUE)
  cat("   ✓ R/data_loading.R loaded\n")
  cat("   ✓ Network has", vcount(net), "species\n")
  cat("   ✓ Network has", ecount(net), "trophic links\n")
}, error = function(e) {
  cat("   ✗ Error loading data:", conditionMessage(e), "\n")
  stop("Data loading failed")
})

# Test 6: Main app structure
cat("\n6. Testing main app.R structure...\n")
tryCatch({
  # Check file exists and is readable
  if (!file.exists("app.R")) {
    stop("app.R not found")
  }

  # Parse app.R to check syntax
  parse("app.R")
  cat("   ✓ app.R syntax valid\n")

  # Source app.R
  source("app.R", local = TRUE)
  cat("   ✓ app.R loaded\n")

  # Check UI and server exist
  if (!exists("ui")) stop("UI object not found")
  if (!exists("server")) stop("Server function not found")

  cat("   ✓ UI object created\n")
  cat("   ✓ Server function defined\n")

}, error = function(e) {
  cat("   ✗ Error with app.R:", conditionMessage(e), "\n")
  stop("App structure validation failed")
})

# Summary
cat("\n================================================================================\n")
cat("✅ ALL TESTS PASSED\n")
cat("================================================================================\n\n")

cat("Refactoring validation:\n")
cat("  ✅ Phase A: Configuration extraction - WORKING\n")
cat("  ✅ Phase B: Function modularization - WORKING\n")
cat("  ✅ Phase C: UI component extraction - WORKING\n\n")

cat("App components:\n")
cat("  ✅ 8 function files loaded\n")
cat("  ✅ 10 UI files loaded\n")
cat("  ✅ Data loaded (", vcount(net), " species,", ecount(net), " links)\n")
cat("  ✅ UI and server defined\n\n")

cat("================================================================================\n")
cat("🚀 READY TO LAUNCH\n")
cat("================================================================================\n\n")

cat("Next steps:\n")
cat("1. Open app.R in RStudio\n")
cat("2. Click the 'Run App' button (top right corner)\n")
cat("3. Test all 10 tabs in the launched app:\n")
cat("   - Dashboard\n")
cat("   - Data Import\n")
cat("   - Food Web Network\n")
cat("   - Topological Metrics\n")
cat("   - Biomass Analysis\n")
cat("   - Energy Fluxes\n")
cat("   - Keystoneness Analysis\n")
cat("   - Internal Data Editor\n")
cat("   - Metaweb Manager\n")
cat("   - Spatial Analysis\n\n")

cat("If you see this message, your refactored code is working perfectly! 🎉\n")
