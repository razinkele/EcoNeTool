# Quick Test - Species Expansion Verification
# Tests fuzzy harmonization for newly added species (ontology only)

cat("=============================================================\n")
cat("QUICK SPECIES EXPANSION TEST\n")
cat("=============================================================\n\n")

# Load required functions
source("R/config/harmonization_config.R")
source("R/functions/trait_lookup.R")

# Load ontology data
ontology_data <- read.csv("data/ontology_traits.csv", stringsAsFactors = FALSE)

cat("Database Statistics:\n")
cat("-------------------------------------------------------------\n")
cat("Total records:", nrow(ontology_data), "\n")

unique_species <- unique(ontology_data$taxon_name)
cat("Total species:", length(unique_species), "\n\n")

# Test fuzzy harmonization for representative species from each group
cat("Testing Fuzzy Harmonization (Ontology Only):\n")
cat("=============================================================\n\n")

test_species <- list(
  "Fish" = c("Gadus morhua", "Clupea harengus", "Platichthys flesus"),
  "Phytoplankton" = c("Skeletonema costatum", "Chaetoceros spp.", "Dinophysis spp."),
  "Zooplankton" = c("Acartia spp.", "Temora longicornis", "Calanus finmarchicus"),
  "Invertebrates" = c("Crangon crangon", "Asterias rubens", "Arenicola marina")
)

all_results <- data.frame()

for (group in names(test_species)) {
  cat("\n", group, ":\n")
  cat("-------------------------------------------------------------\n")

  for (sp in test_species[[group]]) {
    # Get ontology traits
    sp_traits <- ontology_data[ontology_data$taxon_name == sp, ]

    if (nrow(sp_traits) > 0) {
      # Test fuzzy harmonization
      fuzzy_fs <- harmonize_fuzzy_foraging(sp_traits)
      fuzzy_mb <- harmonize_fuzzy_mobility(sp_traits)
      fuzzy_ep <- harmonize_fuzzy_habitat(sp_traits)

      fs <- if (!is.na(fuzzy_fs$class)) fuzzy_fs$class else "NA"
      mb <- if (!is.na(fuzzy_mb$class)) fuzzy_mb$class else "NA"
      ep <- if (!is.na(fuzzy_ep$class)) fuzzy_ep$class else "NA"

      n_traits <- sum(!is.na(c(fuzzy_fs$class, fuzzy_mb$class, fuzzy_ep$class)))
      coverage <- round(n_traits / 3 * 100, 0)

      cat(sprintf("  %-25s FS=%-3s MB=%-3s EP=%-3s (%d%% coverage)\n",
                  sp, fs, mb, ep, coverage))

      all_results <- rbind(all_results, data.frame(
        group = group,
        species = sp,
        FS = fs,
        MB = mb,
        EP = ep,
        coverage = coverage,
        stringsAsFactors = FALSE
      ))
    } else {
      cat("  ", sp, "- NOT FOUND\n")
    }
  }
}

cat("\n")
cat("=============================================================\n")
cat("SUMMARY STATISTICS\n")
cat("=============================================================\n")

avg_coverage <- mean(all_results$coverage)
full_coverage <- sum(all_results$coverage == 100)

cat("Tested species:", nrow(all_results), "\n")
cat("Average fuzzy coverage:", round(avg_coverage, 1), "% (FS+MB+EP)\n")
cat("Full fuzzy coverage (100%):", full_coverage, "/", nrow(all_results), "species\n")

cat("\nCoverage by group:\n")
by_group <- aggregate(coverage ~ group, all_results, mean)
for (i in 1:nrow(by_group)) {
  cat("  ", by_group$group[i], ": ", round(by_group$coverage[i], 1), "%\n", sep = "")
}

cat("\n")

# Count trait modalities
cat("Trait Modality Distribution:\n")
cat("-------------------------------------------------------------\n")

fs_modes <- all_results$FS[all_results$FS != "NA"]
mb_modes <- all_results$MB[all_results$MB != "NA"]
ep_modes <- all_results$EP[all_results$EP != "NA"]

cat("FS classes:", paste(sort(unique(fs_modes)), collapse = ", "), "\n")
cat("  FS0 (Producer):", sum(fs_modes == "FS0"), "\n")
cat("  FS1 (Predator):", sum(fs_modes == "FS1"), "\n")
cat("  FS5 (Deposit):", sum(fs_modes == "FS5"), "\n")
cat("  FS6 (Filter):", sum(fs_modes == "FS6"), "\n")

cat("\nMB classes:", paste(sort(unique(mb_modes)), collapse = ", "), "\n")
cat("  MB2 (Burrower):", sum(mb_modes == "MB2"), "\n")
cat("  MB3 (Crawler):", sum(mb_modes == "MB3"), "\n")
cat("  MB4 (Limited Swim):", sum(mb_modes == "MB4"), "\n")
cat("  MB5 (Swimmer):", sum(mb_modes == "MB5"), "\n")

cat("\nEP classes:", paste(sort(unique(ep_modes)), collapse = ", "), "\n")
cat("  EP1 (Pelagic):", sum(ep_modes == "EP1"), "\n")
cat("  EP3 (Benthic):", sum(ep_modes == "EP3"), "\n")
cat("  EP4 (Intertidal):", sum(ep_modes == "EP4"), "\n")

cat("\n")
cat("=============================================================\n")
cat("EXPANSION COMPLETE\n")
cat("=============================================================\n")
cat("Successfully expanded from 18 to", length(unique_species), "species\n")
cat("New species: ", length(unique_species) - 18, " (+", round((length(unique_species) - 18) / 18 * 100, 0), "%)\n", sep = "")
cat("Average fuzzy coverage: ", round(avg_coverage, 1), "%\n", sep = "")
cat("\n")

if (avg_coverage >= 90) {
  cat("✓ EXCELLENT - High coverage across all groups\n")
} else if (avg_coverage >= 70) {
  cat("✓ GOOD - Most species well covered\n")
} else {
  cat("⚠ MODERATE - Some species need more trait data\n")
}

cat("=============================================================\n")
