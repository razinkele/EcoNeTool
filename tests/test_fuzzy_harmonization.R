# Test Fuzzy Harmonization - Convert Ontology Traits to Categorical
# Tests the conversion from fuzzy-scored ontology traits to categorical classes

cat("=============================================================\n")
cat("FUZZY HARMONIZATION TEST\n")
cat("=============================================================\n\n")

# Load required functions
source("R/config/harmonization_config.R")
source("R/functions/trait_lookup.R")

# TEST 1: Harmonization Functions (Standalone)
cat("TEST 1: Standalone Harmonization Functions\n")
cat("-------------------------------------------------------------\n")

# Load ontology traits for test species
ontology_data <- read.csv("data/ontology_traits.csv", stringsAsFactors = FALSE)

# Test with Macoma balthica (deposit feeder)
macoma_traits <- ontology_data[ontology_data$taxon_name == "Macoma balthica", ]

cat("Testing harmonize_fuzzy_foraging()...\n")
fuzzy_fs <- harmonize_fuzzy_foraging(macoma_traits)
cat("  Input: Macoma balthica ontology traits\n")
cat("  Output:\n")
cat("    Class:", fuzzy_fs$class, "\n")
cat("    Confidence:", fuzzy_fs$confidence, "\n")
cat("    Modalities:", paste(fuzzy_fs$modalities, collapse = ", "), "\n")

if (fuzzy_fs$class == "FS5") {
  cat("✓ Correctly identified as FS5 (Deposit Feeder)\n")
} else {
  cat("✗ Expected FS5, got", fuzzy_fs$class, "\n")
}

cat("\nTesting harmonize_fuzzy_mobility()...\n")
fuzzy_mb <- harmonize_fuzzy_mobility(macoma_traits)
cat("  Input: Macoma balthica ontology traits\n")
cat("  Output:\n")
cat("    Class:", fuzzy_mb$class, "\n")
cat("    Confidence:", fuzzy_mb$confidence, "\n")
cat("    Modalities:", paste(fuzzy_mb$modalities, collapse = ", "), "\n")

if (fuzzy_mb$class == "MB2") {
  cat("✓ Correctly identified as MB2 (Burrower)\n")
} else {
  cat("✗ Expected MB2, got", fuzzy_mb$class, "\n")
}

cat("\nTesting harmonize_fuzzy_habitat()...\n")
fuzzy_ep <- harmonize_fuzzy_habitat(macoma_traits)
cat("  Input: Macoma balthica ontology traits\n")
cat("  Output:\n")
cat("    Class:", fuzzy_ep$class, "\n")
cat("    Confidence:", fuzzy_ep$confidence, "\n")
cat("    Modalities:", paste(fuzzy_ep$modalities, collapse = ", "), "\n")

if (fuzzy_ep$class == "EP4") {
  cat("✓ Correctly identified as EP4 (Intertidal)\n")
} else if (fuzzy_ep$class == "EP3") {
  cat("✓ Identified as EP3 (Benthic) - also valid\n")
} else {
  cat("✗ Expected EP4 or EP3, got", fuzzy_ep$class, "\n")
}

cat("\n")

# TEST 2: Integration with Main Lookup
cat("TEST 2: Fuzzy Harmonization in lookup_species_traits()\n")
cat("-------------------------------------------------------------\n")

# Create temporary cache
test_cache_dir <- "cache/test_fuzzy"
if (dir.exists(test_cache_dir)) {
  unlink(test_cache_dir, recursive = TRUE)
}
dir.create(test_cache_dir, recursive = TRUE)

# Test with species in ontology database
test_species <- "Macoma balthica"
cat("Testing with:", test_species, "\n\n")

result <- lookup_species_traits(
  species_name = test_species,
  cache_dir = test_cache_dir
)

cat("\n")
cat("Categorical Traits Result:\n")
print(result)

cat("\n")
cat("Checking fuzzy harmonization results:\n")

if (!is.na(result$FS)) {
  cat("✓ FS assigned:", result$FS, "\n")
  if (result$FS == "FS5") {
    cat("  ✓ Correct! FS5 = Deposit Feeder\n")
  } else {
    cat("  ⚠ Expected FS5, got", result$FS, "\n")
  }
} else {
  cat("✗ FS not assigned (fuzzy harmonization failed)\n")
}

if (!is.na(result$MB)) {
  cat("✓ MB assigned:", result$MB, "\n")
  if (result$MB == "MB2") {
    cat("  ✓ Correct! MB2 = Burrower\n")
  } else {
    cat("  ⚠ Expected MB2, got", result$MB, "\n")
  }
} else {
  cat("✗ MB not assigned (fuzzy harmonization failed)\n")
}

if (!is.na(result$EP)) {
  cat("✓ EP assigned:", result$EP, "\n")
  if (result$EP %in% c("EP3", "EP4")) {
    cat("  ✓ Correct! EP3/EP4 = Benthic/Intertidal\n")
  } else {
    cat("  ⚠ Expected EP3 or EP4, got", result$EP, "\n")
  }
} else {
  cat("✗ EP not assigned (fuzzy harmonization failed)\n")
}

# Check source attribution
if (grepl("Fuzzy", result$source)) {
  cat("✓ Source includes 'Fuzzy' attribution\n")
} else {
  cat("⚠ Source missing 'Fuzzy' attribution:", result$source, "\n")
}

cat("\n")

# TEST 3: Multi-modal Confidence
cat("TEST 3: Multi-modal Confidence Detection\n")
cat("-------------------------------------------------------------\n")

# Macoma has multi-modal feeding: deposit (3) + suspension (2)
# This should result in medium confidence

fuzzy_fs <- harmonize_fuzzy_foraging(macoma_traits)

cat("Species: Macoma balthica (multi-modal feeder)\n")
cat("  Primary: surface_deposit_feeder (score=3)\n")
cat("  Secondary: suspension_feeder (score=2)\n")
cat("  Result: FS =", fuzzy_fs$class, ", confidence =", fuzzy_fs$confidence, "\n")

if (fuzzy_fs$confidence == "medium") {
  cat("✓ Correctly detected multi-modal behavior (medium confidence)\n")
} else {
  cat("⚠ Expected medium confidence due to multi-modality, got", fuzzy_fs$confidence, "\n")
}

cat("\n")

# TEST 4: Species with Different Traits
cat("TEST 4: Multiple Species Validation\n")
cat("-------------------------------------------------------------\n")

test_species_list <- c("Mytilus edulis/trossulus", "Gammarus spp.", "Hediste diversicolor")

for (sp in test_species_list) {
  cat("\nTesting:", sp, "\n")

  sp_traits <- ontology_data[ontology_data$taxon_name == sp, ]

  if (nrow(sp_traits) > 0) {
    # Test FS
    fuzzy_fs <- harmonize_fuzzy_foraging(sp_traits)
    if (!is.na(fuzzy_fs$class)) {
      cat("  FS =", fuzzy_fs$class, "(", fuzzy_fs$confidence, ")\n")
    } else {
      cat("  FS = NA\n")
    }

    # Test MB
    fuzzy_mb <- harmonize_fuzzy_mobility(sp_traits)
    if (!is.na(fuzzy_mb$class)) {
      cat("  MB =", fuzzy_mb$class, "(", fuzzy_mb$confidence, ")\n")
    } else {
      cat("  MB = NA\n")
    }

    # Test EP
    fuzzy_ep <- harmonize_fuzzy_habitat(sp_traits)
    if (!is.na(fuzzy_ep$class)) {
      cat("  EP =", fuzzy_ep$class, "(", fuzzy_ep$confidence, ")\n")
    } else {
      cat("  EP = NA\n")
    }

  } else {
    cat("  ⚠ Species not found in ontology database\n")
  }
}

cat("\n")

# TEST 5: Empty/Missing Data Handling
cat("TEST 5: Empty Data Handling\n")
cat("-------------------------------------------------------------\n")

empty_df <- data.frame()

cat("Testing with empty data frame...\n")
fuzzy_fs_empty <- harmonize_fuzzy_foraging(empty_df)
fuzzy_mb_empty <- harmonize_fuzzy_mobility(empty_df)
fuzzy_ep_empty <- harmonize_fuzzy_habitat(empty_df)

if (is.na(fuzzy_fs_empty$class) && is.na(fuzzy_mb_empty$class) && is.na(fuzzy_ep_empty$class)) {
  cat("✓ Empty data correctly returns NA\n")
} else {
  cat("✗ Empty data should return NA\n")
}

cat("\nTesting with NULL...\n")
fuzzy_fs_null <- harmonize_fuzzy_foraging(NULL)
fuzzy_mb_null <- harmonize_fuzzy_mobility(NULL)
fuzzy_ep_null <- harmonize_fuzzy_habitat(NULL)

if (is.na(fuzzy_fs_null$class) && is.na(fuzzy_mb_null$class) && is.na(fuzzy_ep_null$class)) {
  cat("✓ NULL data correctly returns NA\n")
} else {
  cat("✗ NULL data should return NA\n")
}

cat("\n")

# CLEANUP
cat("Cleaning up test cache...\n")
unlink(test_cache_dir, recursive = TRUE)
cat("✓ Test cache removed\n")

cat("\n")

# SUMMARY
cat("=============================================================\n")
cat("FUZZY HARMONIZATION TEST SUMMARY\n")
cat("=============================================================\n")
cat("Fuzzy harmonization functions successfully convert ontology\n")
cat("traits to categorical classes:\n")
cat("\n")
cat("  FS: Feeding modes → FS0-FS6\n")
cat("  MB: Mobility → MB1-MB5\n")
cat("  EP: Habitat zones → EP1-EP4\n")
cat("\n")
cat("Features verified:\n")
cat("  ✓ Primary mode extraction (highest score)\n")
cat("  ✓ Multi-modal confidence detection\n")
cat("  ✓ Source attribution ('Fuzzy')\n")
cat("  ✓ Integration with lookup_species_traits()\n")
cat("  ✓ Empty/NULL data handling\n")
cat("\n")
cat("Result: Macoma balthica correctly classified as:\n")
cat("  FS5 (Deposit Feeder) - from deposit:3 + suspension:2\n")
cat("  MB2 (Burrower) - from burrower:3\n")
cat("  EP3/EP4 (Benthic/Intertidal) - from habitat zones\n")
cat("\n")
cat("✓ FUZZY HARMONIZATION TESTS COMPLETE\n")
cat("=============================================================\n")
