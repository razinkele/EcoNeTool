# Test Taxonomic API Integration
# Demonstrates querying FishBase, WoRMS, and OBIS for species classification

cat("================================================================================\n")
cat("Testing Taxonomic API Integration\n")
cat("================================================================================\n\n")

# Source the module
source("R/functions/taxonomic_api_utils.R")

# Test species from coastal model
test_species <- c(
  "Twaite shad",
  "Alosa fallax",  # Scientific name for Twaite shad
  "Vimba vimba",   # Scientific name for Vimba
  "Ruffe",
  "Gymnocephalus cernua"  # Scientific name for Ruffe
)

cat("================================================================================\n")
cat("Test 1: WoRMS Taxonomic Classification\n")
cat("================================================================================\n\n")

for (species in test_species[1:3]) {
  cat("Querying WoRMS for:", species, "\n")
  result <- query_worms(species)

  if (!is.null(result)) {
    cat("  ✓ Found:", result$scientific_name, "\n")
    cat("    Class:", result$class, "\n")
    cat("    Family:", result$family, "\n")
    cat("    Habitat:", paste(
      if (result$is_marine) "Marine" else NULL,
      if (result$is_brackish) "Brackish" else NULL,
      if (result$is_freshwater) "Freshwater" else NULL,
      collapse = ", "
    ), "\n")

    # Classify based on taxonomy
    fg <- classify_by_taxonomy(result)
    cat("    Functional Group:", fg, "\n\n")
  } else {
    cat("  ✗ Not found in WoRMS\n\n")
  }
}

cat("================================================================================\n")
cat("Test 2: FishBase Fish Data (Requires rfishbase package)\n")
cat("================================================================================\n\n")

if (requireNamespace("rfishbase", quietly = TRUE)) {
  for (species in c("Alosa fallax", "Gymnocephalus cernua")) {
    cat("Querying FishBase for:", species, "\n")
    result <- query_fishbase(species)

    if (!is.null(result)) {
      cat("  ✓ Found:", result$scientific_name, "\n")
      cat("    Common Name:", result$common_name, "\n")
      cat("    Family:", result$family, "\n")
      cat("    Max Length:", result$max_length_cm, "cm\n")
      cat("    Max Weight:", result$max_weight_g, "g\n")
      cat("    Trophic Level:", result$trophic_level, "\n")
      cat("    Functional Group:", result$functional_group, "\n\n")
    } else {
      cat("  ✗ Not found in FishBase\n\n")
    }
  }
} else {
  cat("⚠ rfishbase package not installed\n")
  cat("Install with: install.packages('rfishbase')\n\n")
}

cat("================================================================================\n")
cat("Test 3: Enhanced Classification (Pattern + API)\n")
cat("================================================================================\n\n")

# Test pattern matching first
cat("Pattern Matching Only:\n")
for (species in c("Twaite shad", "Ruffe", "Vimba")) {
  fg_pattern <- assign_functional_group(species)
  cat(sprintf("  %-20s → %s\n", species, fg_pattern))
}

cat("\nEnhanced (Pattern + API verification):\n")
cat("Note: This requires internet connection and packages\n\n")

# Note: This would be slow without caching
cat("To test API enhancement, run:\n")
cat("  fg <- assign_functional_group_enhanced('Twaite shad', use_api = TRUE)\n")
cat("  print(fg)\n\n")

cat("================================================================================\n")
cat("Test 4: Taxonomic Classification Mapping\n")
cat("================================================================================\n\n")

# Test the taxonomy-to-functional-group mapping
test_taxonomies <- list(
  list(class = "Actinopterygii", name = "Ray-finned fishes"),
  list(class = "Aves", name = "Birds"),
  list(class = "Mammalia", name = "Mammals"),
  list(class = "Malacostraca", name = "Crabs/Shrimp"),
  list(class = "Copepoda", name = "Copepods"),
  list(class = "Phaeophyceae", name = "Brown algae")
)

cat("Taxonomic Class → Functional Group Mapping:\n")
for (tax in test_taxonomies) {
  fg <- classify_by_taxonomy(tax)
  cat(sprintf("  %-30s (%-20s) → %s\n", tax$name, tax$class, fg))
}

cat("\n================================================================================\n")
cat("Summary\n")
cat("================================================================================\n\n")

cat("Taxonomic API Integration Benefits:\n")
cat("  ✓ Authoritative classification from FishBase/WoRMS/OBIS\n")
cat("  ✓ Actual body mass and trait data\n")
cat("  ✓ Handles spelling variants and synonyms\n")
cat("  ✓ Automatic caching for performance\n")
cat("  ✓ Graceful fallback to pattern matching\n\n")

cat("Next Steps:\n")
cat("  1. Install packages: install.packages(c('httr', 'jsonlite', 'rfishbase'))\n")
cat("  2. Test with real species: classify_species_api('Twaite shad')\n")
cat("  3. Integrate into app.R import workflow\n")
cat("  4. Add UI checkbox for API verification (optional)\n\n")

cat("================================================================================\n")
cat("Test Complete\n")
cat("================================================================================\n")
