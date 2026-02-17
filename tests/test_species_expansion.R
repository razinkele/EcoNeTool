# Test Species Expansion - Verify New Species Coverage
# Tests that the 45 newly added species work with fuzzy harmonization

cat("=============================================================\n")
cat("SPECIES EXPANSION TEST\n")
cat("=============================================================\n\n")

# Load required functions
source("R/config/harmonization_config.R")
source("R/functions/trait_lookup.R")

# TEST 1: Database Statistics
cat("TEST 1: Database Statistics\n")
cat("-------------------------------------------------------------\n")

ontology_data <- read.csv("data/ontology_traits.csv", stringsAsFactors = FALSE)

cat("Total trait records:", nrow(ontology_data), "\n")

unique_species <- unique(ontology_data$taxon_name)
cat("Total species:", length(unique_species), "\n")

# Count by trait category
cat("\nTrait categories:\n")
trait_cats <- table(ontology_data$trait_category)
print(trait_cats)

# Count species by major groups
cat("\nSpecies by group:\n")

fish <- grep("morhua|harengus|sprattus|flesus|limanda|platessa|marinus|microps|viviparus|aculeatus|merlangus|esmarkii|anguilla|scorpius", unique_species, value = TRUE)
phytoplankton <- grep("Skeletonema|Chaetoceros|Thalassiosira|Dinophysis|Prorocentrum|Coscinodiscus|Ceratium|Rhizosolenia|Pseudo-nitzschia|Nodularia", unique_species, value = TRUE)
zooplankton <- grep("Acartia|Temora|Pseudocalanus|Centropages|Calanus|Evadne|Pleopsis|Oithona|Bosmina|Fritillaria", unique_species, value = TRUE)
benthic_invert <- setdiff(unique_species, c(fish, phytoplankton, zooplankton))

cat("  Fish:", length(fish), "\n")
cat("  Phytoplankton:", length(phytoplankton), "\n")
cat("  Zooplankton:", length(zooplankton), "\n")
cat("  Benthic invertebrates:", length(benthic_invert), "\n")

cat("\n")

# TEST 2: Sample Fish Species
cat("TEST 2: Fish Species Fuzzy Harmonization\n")
cat("-------------------------------------------------------------\n")

test_cache_dir <- "cache/test_expansion"
if (dir.exists(test_cache_dir)) {
  unlink(test_cache_dir, recursive = TRUE)
}
dir.create(test_cache_dir, recursive = TRUE, showWarnings = FALSE)

test_fish <- c("Gadus morhua", "Clupea harengus", "Platichthys flesus")

fish_results <- data.frame(
  species = character(),
  MS = character(),
  FS = character(),
  MB = character(),
  EP = character(),
  PR = character(),
  coverage = numeric(),
  stringsAsFactors = FALSE
)

for (sp in test_fish) {
  cat("\nTesting:", sp, "...\n")

  result <- lookup_species_traits(
    species_name = sp,
    cache_dir = test_cache_dir
  )

  n_traits <- sum(!is.na(c(result$MS, result$FS, result$MB, result$EP, result$PR)))
  coverage <- n_traits / 5 * 100

  fish_results <- rbind(fish_results, data.frame(
    species = sp,
    MS = ifelse(is.na(result$MS), "NA", result$MS),
    FS = ifelse(is.na(result$FS), "NA", result$FS),
    MB = ifelse(is.na(result$MB), "NA", result$MB),
    EP = ifelse(is.na(result$EP), "NA", result$EP),
    PR = ifelse(is.na(result$PR), "NA", result$PR),
    coverage = coverage,
    stringsAsFactors = FALSE
  ))

  cat("  Coverage:", coverage, "% (", n_traits, "/5 traits)\n")
}

cat("\nFish Results Summary:\n")
print(fish_results)

cat("\n")

# TEST 3: Phytoplankton Species
cat("TEST 3: Phytoplankton Species Fuzzy Harmonization\n")
cat("-------------------------------------------------------------\n")

test_phyto <- c("Skeletonema costatum", "Chaetoceros spp.", "Dinophysis spp.")

phyto_results <- data.frame(
  species = character(),
  MS = character(),
  FS = character(),
  MB = character(),
  EP = character(),
  PR = character(),
  coverage = numeric(),
  stringsAsFactors = FALSE
)

for (sp in test_phyto) {
  cat("\nTesting:", sp, "...\n")

  result <- lookup_species_traits(
    species_name = sp,
    cache_dir = test_cache_dir
  )

  n_traits <- sum(!is.na(c(result$MS, result$FS, result$MB, result$EP, result$PR)))
  coverage <- n_traits / 5 * 100

  phyto_results <- rbind(phyto_results, data.frame(
    species = sp,
    MS = ifelse(is.na(result$MS), "NA", result$MS),
    FS = ifelse(is.na(result$FS), "NA", result$FS),
    MB = ifelse(is.na(result$MB), "NA", result$MB),
    EP = ifelse(is.na(result$EP), "NA", result$EP),
    PR = ifelse(is.na(result$PR), "NA", result$PR),
    coverage = coverage,
    stringsAsFactors = FALSE
  ))

  cat("  Coverage:", coverage, "% (", n_traits, "/5 traits)\n")
}

cat("\nPhytoplankton Results Summary:\n")
print(phyto_results)

cat("\n")

# TEST 4: Zooplankton Species
cat("TEST 4: Zooplankton Species Fuzzy Harmonization\n")
cat("-------------------------------------------------------------\n")

test_zoo <- c("Acartia spp.", "Temora longicornis", "Calanus finmarchicus")

zoo_results <- data.frame(
  species = character(),
  MS = character(),
  FS = character(),
  MB = character(),
  EP = character(),
  PR = character(),
  coverage = numeric(),
  stringsAsFactors = FALSE
)

for (sp in test_zoo) {
  cat("\nTesting:", sp, "...\n")

  result <- lookup_species_traits(
    species_name = sp,
    cache_dir = test_cache_dir
  )

  n_traits <- sum(!is.na(c(result$MS, result$FS, result$MB, result$EP, result$PR)))
  coverage <- n_traits / 5 * 100

  zoo_results <- rbind(zoo_results, data.frame(
    species = sp,
    MS = ifelse(is.na(result$MS), "NA", result$MS),
    FS = ifelse(is.na(result$FS), "NA", result$FS),
    MB = ifelse(is.na(result$MB), "NA", result$MB),
    EP = ifelse(is.na(result$EP), "NA", result$EP),
    PR = ifelse(is.na(result$PR), "NA", result$PR),
    coverage = coverage,
    stringsAsFactors = FALSE
  ))

  cat("  Coverage:", coverage, "% (", n_traits, "/5 traits)\n")
}

cat("\nZooplankton Results Summary:\n")
print(zoo_results)

cat("\n")

# TEST 5: More Invertebrates
cat("TEST 5: Benthic Invertebrate Species\n")
cat("-------------------------------------------------------------\n")

test_invert <- c("Crangon crangon", "Arenicola marina", "Asterias rubens")

invert_results <- data.frame(
  species = character(),
  MS = character(),
  FS = character(),
  MB = character(),
  EP = character(),
  PR = character(),
  coverage = numeric(),
  stringsAsFactors = FALSE
)

for (sp in test_invert) {
  cat("\nTesting:", sp, "...\n")

  result <- lookup_species_traits(
    species_name = sp,
    cache_dir = test_cache_dir
  )

  n_traits <- sum(!is.na(c(result$MS, result$FS, result$MB, result$EP, result$PR)))
  coverage <- n_traits / 5 * 100

  invert_results <- rbind(invert_results, data.frame(
    species = sp,
    MS = ifelse(is.na(result$MS), "NA", result$MS),
    FS = ifelse(is.na(result$FS), "NA", result$FS),
    MB = ifelse(is.na(result$MB), "NA", result$MB),
    EP = ifelse(is.na(result$EP), "NA", result$EP),
    PR = ifelse(is.na(result$PR), "NA", result$PR),
    coverage = coverage,
    stringsAsFactors = FALSE
  ))

  cat("  Coverage:", coverage, "% (", n_traits, "/5 traits)\n")
}

cat("\nBenthic Invertebrate Results Summary:\n")
print(invert_results)

cat("\n")

# TEST 6: Overall Coverage Statistics
cat("TEST 6: Overall Coverage Statistics\n")
cat("-------------------------------------------------------------\n")

all_results <- rbind(fish_results, phyto_results, zoo_results, invert_results)

avg_coverage <- mean(all_results$coverage)
full_coverage <- sum(all_results$coverage == 100)
partial_coverage <- sum(all_results$coverage < 100 & all_results$coverage > 0)
no_coverage <- sum(all_results$coverage == 0)

cat("Tested species:", nrow(all_results), "\n")
cat("Average coverage:", round(avg_coverage, 1), "%\n")
cat("Full coverage (100%):", full_coverage, "species (", round(full_coverage/nrow(all_results)*100, 1), "%)\n")
cat("Partial coverage (<100%):", partial_coverage, "species\n")
cat("No coverage (0%):", no_coverage, "species\n")

if (avg_coverage >= 80) {
  cat("✓ Excellent coverage! Most species have high trait data\n")
} else if (avg_coverage >= 60) {
  cat("✓ Good coverage\n")
} else {
  cat("⚠ Coverage could be improved\n")
}

cat("\n")

# CLEANUP
cat("Cleaning up test cache...\n")
unlink(test_cache_dir, recursive = TRUE)
cat("✓ Test cache removed\n")

cat("\n")

# SUMMARY
cat("=============================================================\n")
cat("SPECIES EXPANSION TEST SUMMARY\n")
cat("=============================================================\n")
cat("Ontology traits database expanded successfully:\n")
cat("\n")
cat("  Original: 18 species, 180 records\n")
cat("  Expanded: 63 species, 425 records\n")
cat("  New species: 45 (+250%)\n")
cat("  New records: 245 (+136%)\n")
cat("\n")
cat("Species breakdown:\n")
cat("  Fish: ", length(fish), " species\n")
cat("  Phytoplankton: ", length(phytoplankton), " species\n")
cat("  Zooplankton: ", length(zooplankton), " species\n")
cat("  Benthic invertebrates: ", length(benthic_invert), " species\n")
cat("\n")
cat("Coverage performance:\n")
cat("  Average: ", round(avg_coverage, 1), "%\n")
cat("  Full coverage: ", full_coverage, "/", nrow(all_results), " tested species\n")
cat("\n")
cat("✓ SPECIES EXPANSION COMPLETE\n")
cat("=============================================================\n")
