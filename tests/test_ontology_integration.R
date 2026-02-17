# Test Ontology Traits Integration
# Tests the integration of ontology lookup into main trait lookup pipeline

cat("=============================================================\n")
cat("ONTOLOGY TRAITS INTEGRATION TEST\n")
cat("=============================================================\n\n")

# Load required functions
source("R/config/harmonization_config.R")
source("R/functions/trait_lookup.R")

# TEST 1: Ontology Lookup Function (Standalone)
cat("TEST 1: Standalone Ontology Lookup\n")
cat("-------------------------------------------------------------\n")

# Test with species that exists in ontology database
test_species <- "Macoma balthica"
cat("Looking up:", test_species, "\n")

ontology_result <- lookup_ontology_traits(species_name = test_species)

if (ontology_result$success) {
  cat("✓ Ontology lookup successful\n")
  cat("  → AphiaID:", ontology_result$aphia_id, "\n")
  cat("  → Trait records:", ontology_result$n_traits, "\n")
  cat("  → Categories:", paste(names(ontology_result$trait_summary), collapse=", "), "\n")

  # Test helper functions
  primary_feeding <- extract_primary_feeding(ontology_result$traits)
  if (!is.na(primary_feeding$modality)) {
    cat("  → Primary feeding:", primary_feeding$modality,
        "(score=", primary_feeding$score, ")\n")
    cat("✓ extract_primary_feeding() works\n")
  } else {
    cat("✗ extract_primary_feeding() failed\n")
  }

  # Test fuzzy profile
  feeding_profile <- get_fuzzy_profile(ontology_result$traits, "feeding")
  if (nrow(feeding_profile) > 0) {
    cat("✓ get_fuzzy_profile() works - found", nrow(feeding_profile), "feeding traits\n")
  } else {
    cat("✗ get_fuzzy_profile() returned empty\n")
  }

} else {
  cat("✗ Ontology lookup failed:", ontology_result$error, "\n")
}

cat("\n")

# TEST 2: Integration with Main Lookup Pipeline
cat("TEST 2: Integration with lookup_species_traits()\n")
cat("-------------------------------------------------------------\n")

# Create temporary cache directory for testing
test_cache_dir <- "cache/test_ontology"
if (dir.exists(test_cache_dir)) {
  unlink(test_cache_dir, recursive = TRUE)
}
dir.create(test_cache_dir, recursive = TRUE)

cat("Testing with:", test_species, "\n\n")

# Run full trait lookup (this should now include ontology)
full_result <- lookup_species_traits(
  species_name = test_species,
  cache_dir = test_cache_dir
)

cat("\n")
cat("Categorical Traits Result:\n")
print(full_result)

# Check cache to see if ontology traits were stored
cache_file <- file.path(test_cache_dir, paste0(gsub(" ", "_", test_species), ".rds"))
if (file.exists(cache_file)) {
  cat("\n✓ Cache file created:", cache_file, "\n")

  cached_data <- readRDS(cache_file)

  # Check structure
  cat("  Cache contains:", paste(names(cached_data), collapse=", "), "\n")

  if ("ontology_traits" %in% names(cached_data)) {
    cat("✓ Ontology traits stored in cache\n")
    cat("  → Ontology records:", nrow(cached_data$ontology_traits), "\n")

    # Show sample ontology traits
    cat("\n  Sample ontology traits:\n")
    sample_traits <- head(cached_data$ontology_traits[, c("trait_category", "trait_modality", "trait_score")], 5)
    print(sample_traits)

  } else {
    cat("✗ Ontology traits NOT found in cache\n")
  }

} else {
  cat("✗ Cache file not created\n")
}

cat("\n")

# TEST 3: Test with species NOT in ontology database
cat("TEST 3: Species Not in Ontology Database\n")
cat("-------------------------------------------------------------\n")

test_species_2 <- "Gadus morhua"  # Cod - not in our 18 species ontology list
cat("Testing with:", test_species_2, "\n\n")

full_result_2 <- lookup_species_traits(
  species_name = test_species_2,
  cache_dir = test_cache_dir
)

cat("\nCategorical Traits Result:\n")
print(full_result_2)

# Check cache
cache_file_2 <- file.path(test_cache_dir, paste0(gsub(" ", "_", test_species_2), ".rds"))
if (file.exists(cache_file_2)) {
  cached_data_2 <- readRDS(cache_file_2)

  if ("ontology_traits" %in% names(cached_data_2)) {
    cat("⚠️  Ontology traits unexpectedly found for species not in database\n")
  } else {
    cat("✓ Ontology traits correctly absent (species not in database)\n")
  }
}

cat("\n")

# TEST 4: Test with AphiaID lookup
cat("TEST 4: Lookup by AphiaID\n")
cat("-------------------------------------------------------------\n")

aphia_id_test <- 141579  # Macoma balthica
cat("Looking up AphiaID:", aphia_id_test, "\n")

ontology_by_id <- lookup_ontology_traits(aphia_id = aphia_id_test)

if (ontology_by_id$success) {
  cat("✓ AphiaID lookup successful\n")
  cat("  → Species:", ontology_by_id$species, "\n")
  cat("  → Trait records:", ontology_by_id$n_traits, "\n")
} else {
  cat("✗ AphiaID lookup failed\n")
}

cat("\n")

# CLEANUP
cat("Cleaning up test cache...\n")
unlink(test_cache_dir, recursive = TRUE)
cat("✓ Test cache removed\n")

cat("\n")

# SUMMARY
cat("=============================================================\n")
cat("INTEGRATION TEST SUMMARY\n")
cat("=============================================================\n")
cat("Ontology lookup functions are integrated into the main\n")
cat("trait lookup pipeline. Ontology traits are now:\n")
cat("  1. Queried as step [2/10] in lookup_species_traits()\n")
cat("  2. Stored in cache alongside categorical traits\n")
cat("  3. Available for fuzzy trait harmonization\n")
cat("  4. Accessible via helper functions\n")
cat("\n")
cat("Next steps:\n")
cat("  - Implement fuzzy harmonization (fuzzy scores → categorical)\n")
cat("  - Add ontology trait viewer to GUI\n")
cat("  - Use ontology traits to improve FS/MB/EP assignments\n")
cat("\n")
cat("✓ INTEGRATION TESTS COMPLETE\n")
cat("=============================================================\n")
