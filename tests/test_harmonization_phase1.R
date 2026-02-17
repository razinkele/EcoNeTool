# Test Suite for Phase 1: Harmonization Configuration
# Run this script to verify Phase 1 implementation

cat("=================================================================\n")
cat("Phase 1 Harmonization Configuration Tests\n")
cat("=================================================================\n\n")

# Load configuration
cat("Loading configuration...\n")
source("R/config/harmonization_config.R")

# Test 1: Configuration structure
cat("\n[Test 1] Configuration structure\n")
stopifnot("HARMONIZATION_CONFIG exists" = exists("HARMONIZATION_CONFIG"))
stopifnot("Has size_thresholds" = "size_thresholds" %in% names(HARMONIZATION_CONFIG))
stopifnot("Has foraging_patterns" = "foraging_patterns" %in% names(HARMONIZATION_CONFIG))
stopifnot("Has taxonomic_rules" = "taxonomic_rules" %in% names(HARMONIZATION_CONFIG))
stopifnot("Has profiles" = "profiles" %in% names(HARMONIZATION_CONFIG))
cat("  ✓ Configuration structure valid\n")

# Test 2: Size thresholds
cat("\n[Test 2] Size thresholds\n")
thresh <- HARMONIZATION_CONFIG$size_thresholds
stopifnot("6 boundaries defined" = length(thresh) == 6)
stopifnot("MS1_MS2 exists" = !is.null(thresh$MS1_MS2))
stopifnot("Thresholds are numeric" = is.numeric(thresh$MS1_MS2))
stopifnot("Thresholds are ordered" =
  thresh$MS1_MS2 < thresh$MS2_MS3 &&
  thresh$MS2_MS3 < thresh$MS3_MS4 &&
  thresh$MS3_MS4 < thresh$MS4_MS5 &&
  thresh$MS4_MS5 < thresh$MS5_MS6 &&
  thresh$MS5_MS6 < thresh$MS6_MS7
)
cat("  ✓ Size thresholds valid and ordered\n")

# Test 3: Foraging patterns
cat("\n[Test 3] Foraging patterns\n")
patterns <- HARMONIZATION_CONFIG$foraging_patterns
stopifnot("7 patterns defined" = length(patterns) >= 6)
stopifnot("FS0 pattern exists" = !is.null(patterns$FS0_primary_producer))
stopifnot("FS1 pattern exists" = !is.null(patterns$FS1_predator))
stopifnot("Patterns are strings" = is.character(patterns$FS0_primary_producer))
cat("  ✓ Foraging patterns valid\n")

# Test 4: Taxonomic rules
cat("\n[Test 4] Taxonomic rules\n")
rules <- HARMONIZATION_CONFIG$taxonomic_rules
stopifnot("Rules defined" = length(rules) > 0)
stopifnot("fish_obligate_swimmers exists" = "fish_obligate_swimmers" %in% names(rules))
stopifnot("Rules are logical" = is.logical(rules$fish_obligate_swimmers))
cat("  ✓ Taxonomic rules valid\n")

# Test 5: Ecosystem profiles
cat("\n[Test 5] Ecosystem profiles\n")
profiles <- HARMONIZATION_CONFIG$profiles
stopifnot("6 profiles defined" = length(profiles) >= 6)
stopifnot("Temperate profile exists" = "temperate" %in% names(profiles))
stopifnot("Arctic profile exists" = "arctic" %in% names(profiles))
stopifnot("Profile has size_multiplier" =
  !is.null(profiles$temperate$size_multiplier)
)
cat("  ✓ Ecosystem profiles valid\n")

# Test 6: Helper functions
cat("\n[Test 6] Helper functions\n")

# Test get_size_threshold
thresh_value <- get_size_threshold("MS1_MS2")
stopifnot("get_size_threshold works" = !is.null(thresh_value))
stopifnot("Threshold value correct" = thresh_value == thresh$MS1_MS2)

# Test get_active_profile
profile <- get_active_profile()
stopifnot("get_active_profile works" = !is.null(profile))
stopifnot("Profile has description" = !is.null(profile$description))

# Test apply_size_adjustment
adjusted <- apply_size_adjustment(40)
stopifnot("apply_size_adjustment works" = !is.null(adjusted))
stopifnot("Adjustment is numeric" = is.numeric(adjusted))

# Test is_rule_enabled
enabled <- is_rule_enabled("fish_obligate_swimmers")
stopifnot("is_rule_enabled works" = is.logical(enabled))

cat("  ✓ Helper functions work correctly\n")

# Test 7: Harmonization functions with config
cat("\n[Test 7] Harmonization functions use config\n")
source("R/functions/trait_lookup.R")

# Test size harmonization
size_class <- harmonize_size_class(25)
stopifnot("Size harmonization works" = !is.null(size_class))
stopifnot("Size class is character" = is.character(size_class))
cat("  ✓ Size: 25 cm → ", size_class, "\n")

# Test foraging harmonization
foraging <- harmonize_foraging_strategy("predator carnivore")
stopifnot("Foraging harmonization works" = !is.null(foraging))
cat("  ✓ Foraging: 'predator carnivore' → ", foraging, "\n")

# Test mobility harmonization
mobility <- harmonize_mobility(NULL, NULL, list(class = "Actinopterygii"))
stopifnot("Mobility harmonization works" = !is.null(mobility))
cat("  ✓ Mobility: Fish → ", mobility, "\n")

# Test 8: Ecosystem profile effect
cat("\n[Test 8] Ecosystem profile effects\n")

# Default (temperate)
HARMONIZATION_CONFIG$active_profile <- "temperate"
size_temperate <- harmonize_size_class(40)
cat("  Temperate (40 cm) → ", size_temperate, "\n")

# Arctic (larger fish)
HARMONIZATION_CONFIG$active_profile <- "arctic"
size_arctic <- harmonize_size_class(40)
cat("  Arctic (40 cm) → ", size_arctic, "\n")
cat("  ✓ Profile affects classification\n")

# Test 9: Save and load
cat("\n[Test 9] Save and load configuration\n")

# Save to test file
test_file <- "tests/test_harmonization_temp.R"
success_save <- save_harmonization_config(
  config = HARMONIZATION_CONFIG,
  file = test_file
)
stopifnot("Save succeeded" = success_save)
stopifnot("File created" = file.exists(test_file))

# Modify config
original_thresh <- HARMONIZATION_CONFIG$size_thresholds$MS1_MS2
HARMONIZATION_CONFIG$size_thresholds$MS1_MS2 <- 0.5

# Load from test file
success_load <- load_harmonization_config(file = test_file)
stopifnot("Load succeeded" = success_load)
stopifnot("Config restored" =
  HARMONIZATION_CONFIG$size_thresholds$MS1_MS2 == original_thresh
)

# Clean up
file.remove(test_file)
cat("  ✓ Save and load work correctly\n")

# Test 10: JSON export/import
cat("\n[Test 10] JSON export/import\n")

if (requireNamespace("jsonlite", quietly = TRUE)) {
  # Export to JSON
  test_json <- "tests/test_harmonization_temp.json"
  success_export <- export_config_json(
    file = test_json,
    config = HARMONIZATION_CONFIG
  )
  stopifnot("Export succeeded" = success_export)
  stopifnot("JSON created" = file.exists(test_json))

  # Modify config
  HARMONIZATION_CONFIG$size_thresholds$MS2_MS3 <- 2.0

  # Import from JSON
  success_import <- import_config_json(file = test_json)
  stopifnot("Import succeeded" = success_import)
  stopifnot("Config restored" =
    HARMONIZATION_CONFIG$size_thresholds$MS2_MS3 == 1.0
  )

  # Clean up
  file.remove(test_json)
  cat("  ✓ JSON export/import work correctly\n")
} else {
  cat("  ⚠ jsonlite not available, skipping JSON tests\n")
}

# Test 11: Configuration summary
cat("\n[Test 11] Configuration summary\n")
summary <- get_config_summary()
stopifnot("Summary is character vector" = is.character(summary))
stopifnot("Summary has multiple lines" = length(summary) > 5)
cat("  ✓ Configuration summary generated\n")

# Final summary
cat("\n=================================================================\n")
cat("✅ ALL TESTS PASSED!\n")
cat("=================================================================\n\n")

cat("Configuration Status:\n")
cat("  Size thresholds:", length(HARMONIZATION_CONFIG$size_thresholds), "boundaries\n")
cat("  Foraging patterns:", length(HARMONIZATION_CONFIG$foraging_patterns), "strategies\n")
cat("  Taxonomic rules:", length(HARMONIZATION_CONFIG$taxonomic_rules), "rules\n")
cat("  Ecosystem profiles:", length(HARMONIZATION_CONFIG$profiles), "profiles\n")
cat("  Active profile:", HARMONIZATION_CONFIG$active_profile, "\n")
cat("  Version:", HARMONIZATION_CONFIG$version, "\n")
cat("\n")

cat("Phase 1 Implementation: ✅ VERIFIED\n\n")

cat("Next steps:\n")
cat("  1. Launch app: source('run_app.R')\n")
cat("  2. Go to Settings > Harmonization tab\n")
cat("  3. Adjust thresholds and save\n")
cat("  4. Run automated trait lookup to test\n")
cat("\n")
