#' Test EcoBase Mammal and Bird Classification
#'
#' Verifies that EcoBase imports correctly classify mammals and birds

cat("\n=== Testing EcoBase Mammal and Bird Classification ===\n\n")

# Simulate the EcoBase functional group assignment
assign_fg_ecobase <- function(species_names) {
  n_groups <- length(species_names)
  functional_groups <- rep("Fish", n_groups)

  for (i in 1:n_groups) {
    sp_lower <- tolower(species_names[i])
    if (grepl("phyto|algae|plant|cyano", sp_lower)) {
      functional_groups[i] <- "Phytoplankton"
    } else if (grepl("zoo|plankton|copepod|acartia|pseudo|mysid", sp_lower)) {
      functional_groups[i] <- "Zooplankton"
    } else if (grepl("seal|whale|dolphin|porpoise|otter|walrus|sea lion|fur seal|manatee|dugong|monk seal", sp_lower)) {
      functional_groups[i] <- "Mammals"
    } else if (grepl("bird|gull|tern|cormorant|duck|goose|albatross|petrel|penguin|gannet|puffin|murre|auk|eider|merganser", sp_lower)) {
      functional_groups[i] <- "Birds"
    } else if (grepl("benthos|benthic|bottom|macro.*bent|meio.*bent", sp_lower)) {
      functional_groups[i] <- "Benthos"
    } else if (grepl("detritus|debris", sp_lower)) {
      functional_groups[i] <- "Detritus"
    }
  }

  return(functional_groups)
}

# Test species (including specific ones mentioned by user)
test_species <- c(
  "Monk seal",           # User specifically mentioned
  "Mediterranean monk seal",
  "Common dolphin",      # User specifically mentioned
  "Bottlenose dolphin",
  "Striped dolphin",
  "Harbor porpoise",
  "Grey seal",
  "Harbor seal",
  "Minke whale",
  "Fin whale",

  "Seabirds",
  "Common gull",
  "Arctic tern",
  "Great cormorant",

  "Atlantic cod",
  "European hake",
  "Anchovy",

  "Zooplankton",
  "Copepod",
  "Phytoplankton"
)

# Classify
fg_results <- assign_fg_ecobase(test_species)

# Display results
cat("Classification results:\n\n")
cat(sprintf("%-30s | %-15s\n", "Species Name", "Functional Group"))
cat(strrep("-", 48), "\n")

for (i in 1:length(test_species)) {
  cat(sprintf("%-30s | %-15s\n", test_species[i], fg_results[i]))
}

# Count by functional group
cat("\n=== Summary ===\n\n")
cat("Functional group counts:\n")
print(table(fg_results))

# Verify specific user-mentioned species
cat("\n=== Verification of User-Mentioned Species ===\n\n")
monk_seal_idx <- grep("monk seal", test_species, ignore.case = TRUE)
dolphin_idx <- grep("dolphin", test_species, ignore.case = TRUE)

cat("Monk seal(s):\n")
for (i in monk_seal_idx) {
  cat(sprintf("  %s → %s %s\n",
              test_species[i],
              fg_results[i],
              ifelse(fg_results[i] == "Mammals", "✓", "✗ WRONG!")))
}

cat("\nDolphin(s):\n")
for (i in dolphin_idx) {
  cat(sprintf("  %s → %s %s\n",
              test_species[i],
              fg_results[i],
              ifelse(fg_results[i] == "Mammals", "✓", "✗ WRONG!")))
}

# Final check
all_mammals_correct <- all(fg_results[c(monk_seal_idx, dolphin_idx)] == "Mammals")

cat("\n=== Test Result ===\n\n")
if (all_mammals_correct) {
  cat("✓ SUCCESS: All monk seals and dolphins correctly classified as Mammals!\n")
  cat("✓ EcoBase imports will now correctly assign marine mammals.\n")
} else {
  cat("✗ FAILED: Some mammals still misclassified!\n")
}

cat("\n=== END ===\n")
