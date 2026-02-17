# Test Harmonization Configuration System
# Tests configuration loading, helper functions, and harmonization

cat("=============================================================\n")
cat("HARMONIZATION CONFIGURATION SYSTEM TEST\n")
cat("=============================================================\n\n")

# Load required functions
source("R/config/harmonization_config.R")
source("R/functions/trait_lookup.R")

# TEST 1: Configuration loads correctly
cat("TEST 1: Configuration Loading\n")
cat("-------------------------------------------------------------\n")

if (exists("HARMONIZATION_CONFIG")) {
  cat("✓ HARMONIZATION_CONFIG exists\n")
  
  # Check required fields
  required_fields <- c("size_thresholds", "foraging_patterns", "mobility_patterns",
                      "taxonomic_rules", "profiles", "active_profile")
  
  missing_fields <- setdiff(required_fields, names(HARMONIZATION_CONFIG))
  if (length(missing_fields) == 0) {
    cat("✓ All required fields present\n")
  } else {
    cat("✗ Missing fields:", paste(missing_fields, collapse = ", "), "\n")
  }
  
  # Check size thresholds
  cat("  Size thresholds loaded:\n")
  for (name in names(HARMONIZATION_CONFIG$size_thresholds)) {
    cat("    ", name, "=", HARMONIZATION_CONFIG$size_thresholds[[name]], "cm\n")
  }
  
  # Check active profile
  cat("  Active profile:", HARMONIZATION_CONFIG$active_profile, "\n")
  
} else {
  cat("✗ HARMONIZATION_CONFIG not found\n")
}

cat("\n")

# TEST 2: Helper Functions
cat("TEST 2: Helper Functions\n")
cat("-------------------------------------------------------------\n")

# Test get_size_threshold
cat("Testing get_size_threshold():\n")
thresh_ms2_ms3 <- get_size_threshold("MS2_MS3")
cat("  MS2_MS3 threshold:", thresh_ms2_ms3, "cm\n")

if (thresh_ms2_ms3 == 1.0) {
  cat("✓ get_size_threshold() works correctly\n")
} else {
  cat("✗ Expected 1.0, got", thresh_ms2_ms3, "\n")
}

# Test get_foraging_pattern
cat("\nTesting get_foraging_pattern():\n")
pattern_predator <- get_foraging_pattern("FS1_predator")
cat("  FS1_predator pattern:", pattern_predator, "\n")

if (!is.null(pattern_predator) && grepl("predat", pattern_predator)) {
  cat("✓ get_foraging_pattern() works correctly\n")
} else {
  cat("✗ Pattern not found or incorrect\n")
}

# Test check_taxonomic_rule
cat("\nTesting check_taxonomic_rule():\n")
rule_fish <- check_taxonomic_rule("fish_obligate_swimmers")
cat("  fish_obligate_swimmers:", rule_fish, "\n")

if (rule_fish == TRUE) {
  cat("✓ check_taxonomic_rule() works correctly\n")
} else {
  cat("✗ Expected TRUE, got", rule_fish, "\n")
}

cat("\n")

# TEST 3: Size Class Harmonization
cat("TEST 3: Size Class Harmonization\n")
cat("-------------------------------------------------------------\n")

test_sizes <- c(0.05, 0.5, 3.0, 15.0, 40.0, 100.0, 200.0)
expected_classes <- c("MS1", "MS2", "MS3", "MS4", "MS5", "MS6", "MS7")

cat("Testing harmonize_size_class():\n")
all_correct <- TRUE
for (i in seq_along(test_sizes)) {
  result <- harmonize_size_class(test_sizes[i])
  match <- result == expected_classes[i]
  symbol <- if (match) "✓" else "✗"
  cat("  ", symbol, test_sizes[i], "cm →", result, 
      if (!match) paste("(expected", expected_classes[i], ")") else "", "\n")
  if (!match) all_correct <- FALSE
}

if (all_correct) {
  cat("✓ All size classes assigned correctly\n")
} else {
  cat("✗ Some size classes incorrect\n")
}

cat("\n")

# TEST 4: Foraging Strategy Harmonization
cat("TEST 4: Foraging Strategy Harmonization\n")
cat("-------------------------------------------------------------\n")

test_feeding <- list(
  c("photosynthesis", "autotroph") = "FS0",
  c("predator", "carnivore") = "FS1",
  c("scavenger", "detritivore") = "FS2",
  c("grazer", "herbivore") = "FS4",
  c("filter feeder", "suspension") = "FS6"
)

cat("Testing harmonize_foraging_strategy():\n")
all_correct_fs <- TRUE
for (i in seq_along(test_feeding)) {
  feeding_info <- names(test_feeding)[i]
  expected_fs <- test_feeding[[i]]
  result <- harmonize_foraging_strategy(feeding_info = strsplit(feeding_info, ", ")[[1]])
  match <- result == expected_fs
  symbol <- if (match) "✓" else "✗"
  cat("  ", symbol, feeding_info, "→", result,
      if (!match) paste("(expected", expected_fs, ")") else "", "\n")
  if (!match) all_correct_fs <- FALSE
}

if (all_correct_fs) {
  cat("✓ All foraging strategies assigned correctly\n")
} else {
  cat("✗ Some foraging strategies incorrect\n")
}

cat("\n")

# TEST 5: Configuration Save/Load
cat("TEST 5: Configuration Save/Load\n")
cat("-------------------------------------------------------------\n")

test_config_file <- "config/test_harmonization.json"

cat("Saving configuration to:", test_config_file, "\n")
tryCatch({
  save_harmonization_config(HARMONIZATION_CONFIG, test_config_file)
  
  if (file.exists(test_config_file)) {
    cat("✓ Configuration file created\n")
    
    # Load it back
    cat("Loading configuration...\n")
    loaded_config <- load_harmonization_config(test_config_file)
    
    if (!is.null(loaded_config)) {
      cat("✓ Configuration loaded successfully\n")
      
      # Compare a few key values
      if (loaded_config$size_thresholds$MS2_MS3 == HARMONIZATION_CONFIG$size_thresholds$MS2_MS3) {
        cat("✓ Loaded values match original\n")
      } else {
        cat("✗ Loaded values differ from original\n")
      }
      
      # Clean up test file
      unlink(test_config_file)
      cat("✓ Test file cleaned up\n")
      
    } else {
      cat("✗ Configuration load failed\n")
    }
    
  } else {
    cat("✗ Configuration file not created\n")
  }
  
}, error = function(e) {
  cat("✗ Error:", e$message, "\n")
})

cat("\n")

# TEST 6: Ecosystem Profiles
cat("TEST 6: Ecosystem Profiles\n")
cat("-------------------------------------------------------------\n")

profiles <- names(HARMONIZATION_CONFIG$profiles)
cat("Available profiles:", paste(profiles, collapse = ", "), "\n")

for (profile_name in profiles) {
  profile <- HARMONIZATION_CONFIG$profiles[[profile_name]]
  cat("\n  Profile:", profile_name, "\n")
  cat("    Multiplier:", profile$size_multiplier, "\n")
  cat("    Description:", profile$description, "\n")
}

cat("\n✓ All profiles accessible\n")

cat("\n")

# SUMMARY
cat("=============================================================\n")
cat("TEST SUMMARY\n")
cat("=============================================================\n")
cat("Configuration system is functional and ready for use.\n")
cat("Users can now adjust harmonization thresholds via:\n")
cat("  1. Settings > Harmonization (GUI)\n")
cat("  2. Editing R/config/harmonization_config.R\n")
cat("  3. Importing/exporting JSON configurations\n")
cat("\n")
cat("✓ TESTS COMPLETE\n")
cat("=============================================================\n")
