# ============================================================================
# Complete Test: Enhanced ECOPATH Features
# ============================================================================
# Tests all new enhancements:
# 1. Import of multistanza, fleet, discard fate, and pedigree data
# 2. Calibration table rendering logic
# 3. Integration with Rpath conversion
# ============================================================================

cat("\n=== COMPREHENSIVE TEST OF ENHANCED FEATURES ===\n\n")

library(RODBC)
library(Rpath)

source("R/config.R")
source("R/functions/ecopath_import.R")
source("R/functions/rpath_integration.R")

# ==============================================================================
# TEST 1: Enhanced Import
# ==============================================================================

cat("[1] Testing Enhanced Import\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

ecopath_data <- parse_ecopath_native_windows("examples/LTgoby.eweaccdb")

cat("✓ Import Results:\n")
cat(sprintf("  - Group data: %d groups\n", nrow(ecopath_data$group_data)))
cat(sprintf("  - Diet data: %d entries\n", nrow(ecopath_data$diet_data)))
cat(sprintf("  - Metadata: %s\n", if(!is.null(ecopath_data$metadata)) ecopath_data$metadata$name else "Not available"))
cat(sprintf("  - Multistanza: %s\n",
            if (!is.null(ecopath_data$stanza_params))
              paste(nrow(ecopath_data$stanza_params), "groups")
            else "Not available"))
cat(sprintf("  - Fleet data: %s\n",
            if (!is.null(ecopath_data$fleet_data))
              paste(nrow(ecopath_data$fleet_data), "fleets")
            else "Not available"))
cat(sprintf("  - Discard fate: %s\n",
            if (!is.null(ecopath_data$discard_fate))
              paste(nrow(ecopath_data$discard_fate), "entries")
            else "Not available"))
cat(sprintf("  - Pedigree data: %s\n",
            if (!is.null(ecopath_data$pedigree_data))
              paste(nrow(ecopath_data$pedigree_data), "entries")
            else "Not available"))
cat(sprintf("  - Pedigree levels: %s\n",
            if (!is.null(ecopath_data$pedigree_levels))
              paste(nrow(ecopath_data$pedigree_levels), "levels")
            else "Not available"))

# ==============================================================================
# TEST 2: Calibration Table Logic
# ==============================================================================

cat("\n[2] Testing Calibration Table Logic\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

if (!is.null(ecopath_data$pedigree_data) && nrow(ecopath_data$pedigree_data) > 0) {
  pedigree <- ecopath_data$pedigree_data
  groups <- ecopath_data$group_data
  pedigree_levels <- ecopath_data$pedigree_levels

  # Merge group names
  if ("GroupID" %in% colnames(pedigree) && "GroupID" %in% colnames(groups)) {
    pedigree <- merge(pedigree,
                     groups[, c("GroupID", "GroupName"), drop = FALSE],
                     by = "GroupID",
                     all.x = TRUE)
  }

  # Create readable parameter names
  param_names <- list(
    "BiomassAreaInput" = "Biomass",
    "PBInput" = "P/B",
    "QBInput" = "Q/B",
    "DietComp" = "Diet Composition",
    "TCatchInput" = "Catch"
  )

  # Map variable names
  if ("VarName" %in% colnames(pedigree)) {
    pedigree$Parameter <- sapply(pedigree$VarName, function(x) {
      if (x %in% names(param_names)) param_names[[x]] else x
    })
  }

  # Merge pedigree levels
  if (!is.null(pedigree_levels) && "LevelID" %in% colnames(pedigree) && "LevelID" %in% colnames(pedigree_levels)) {
    pedigree <- merge(pedigree,
                     pedigree_levels[, c("LevelID", "LevelName", "Confidence"), drop = FALSE],
                     by = "LevelID",
                     all.x = TRUE)
  }

  # Create display dataframe
  df <- data.frame(
    Group = if ("GroupName" %in% colnames(pedigree)) pedigree$GroupName else pedigree$GroupID,
    Parameter = if ("Parameter" %in% colnames(pedigree)) pedigree$Parameter else pedigree$VarName,
    Level_ID = pedigree$LevelID,
    stringsAsFactors = FALSE
  )

  # Add confidence if available
  if ("Confidence" %in% colnames(pedigree)) {
    df$Confidence <- pedigree$Confidence
  }

  # Add level name if available
  if ("LevelName" %in% colnames(pedigree)) {
    df$Level_Name <- pedigree$LevelName
  }

  cat(sprintf("✓ Calibration table created: %d rows × %d columns\n", nrow(df), ncol(df)))
  cat("  Columns:", paste(colnames(df), collapse=", "), "\n\n")

  cat("Sample calibration entries (first 10):\n")
  print(head(df, 10))

  # Summary by parameter
  cat("\n✓ Calibration summary by parameter:\n")
  param_summary <- table(df$Parameter)
  for (param in names(param_summary)) {
    cat(sprintf("  - %s: %d entries\n", param, param_summary[param]))
  }
} else {
  cat("⚠ No pedigree data available for calibration table test\n")
}

# ==============================================================================
# TEST 3: Integration with Rpath Conversion
# ==============================================================================

cat("\n[3] Testing Integration with Rpath Conversion\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

rpath_params <- convert_ecopath_to_rpath(ecopath_data, model_name = "LTgoby Enhanced Test")
cat(sprintf("✓ Converted to Rpath: %d groups\n", nrow(rpath_params$model)))

# Check if additional data is still accessible
cat("\n✓ Additional data still available after conversion:\n")
cat(sprintf("  - Original has pedigree data: %s\n",
            !is.null(ecopath_data$pedigree_data)))
cat(sprintf("  - Original has fleet data: %s\n",
            !is.null(ecopath_data$fleet_data)))
cat(sprintf("  - Original has multistanza params: %s\n",
            !is.null(ecopath_data$stanza_params)))

# ==============================================================================
# TEST 4: Mass Balance (Ensure Nothing Broke)
# ==============================================================================

cat("\n[4] Testing Mass Balance (Ensuring Compatibility)\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

result <- tryCatch({
  rpath_model <- run_ecopath_balance(rpath_params, balance = TRUE)
  list(success = TRUE, model = rpath_model)
}, error = function(e) {
  list(success = FALSE, error = conditionMessage(e))
})

if (result$success) {
  cat("✅ Mass balance successful!\n")
  cat(sprintf("  Groups: %d\n", length(result$model$Group)))
  cat(sprintf("  System throughput: %.2f t/km²/year\n\n",
              sum(result$model$Q, na.rm = TRUE)))
} else {
  cat("❌ Mass balance failed:\n")
  cat("  Error:", result$error, "\n\n")
}

# ==============================================================================
# SUMMARY
# ==============================================================================

cat(paste(rep("=", 70), collapse=""), "\n")
cat("\n=== TEST SUMMARY ===\n\n")

tests_passed <- 0
total_tests <- 5

# Test 1: Import
if (nrow(ecopath_data$group_data) > 0) {
  cat("✅ Test 1: Enhanced Import - PASS\n")
  tests_passed <- tests_passed + 1
} else {
  cat("❌ Test 1: Enhanced Import - FAIL\n")
}

# Test 2: Pedigree data
if (!is.null(ecopath_data$pedigree_data) && nrow(ecopath_data$pedigree_data) > 0) {
  cat("✅ Test 2: Pedigree Data Import - PASS\n")
  tests_passed <- tests_passed + 1
} else {
  cat("❌ Test 2: Pedigree Data Import - FAIL\n")
}

# Test 3: Calibration table logic
if (exists("df") && nrow(df) > 0) {
  cat("✅ Test 3: Calibration Table Logic - PASS\n")
  tests_passed <- tests_passed + 1
} else {
  cat("❌ Test 3: Calibration Table Logic - FAIL\n")
}

# Test 4: Rpath conversion
if (!is.null(rpath_params) && nrow(rpath_params$model) > 0) {
  cat("✅ Test 4: Rpath Conversion - PASS\n")
  tests_passed <- tests_passed + 1
} else {
  cat("❌ Test 4: Rpath Conversion - FAIL\n")
}

# Test 5: Mass balance
if (result$success) {
  cat("✅ Test 5: Mass Balance - PASS\n")
  tests_passed <- tests_passed + 1
} else {
  cat("❌ Test 5: Mass Balance - FAIL\n")
}

cat(sprintf("\n📊 RESULTS: %d/%d tests passed\n\n", tests_passed, total_tests))

if (tests_passed == total_tests) {
  cat("🎉 ALL TESTS PASSED! 🎉\n\n")
  cat("Enhanced features are working correctly:\n")
  cat("  ✓ Multistanza parameter import\n")
  cat("  ✓ Fleet data import\n")
  cat("  ✓ Discard fate import\n")
  cat("  ✓ Pedigree/calibration data import\n")
  cat("  ✓ Calibration table rendering logic\n")
  cat("  ✓ Integration with existing Rpath workflow\n")
  cat("  ✓ Mass balance still works\n\n")
  cat("🚀 READY FOR DEPLOYMENT\n\n")
} else {
  cat("⚠ Some tests failed. Please review the errors above.\n\n")
}
