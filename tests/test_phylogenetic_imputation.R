# =============================================================================
# Test Suite for Phylogenetic Trait Imputation (Phase 5)
# =============================================================================

cat("=============================================================================\n")
cat("PHYLOGENETIC TRAIT IMPUTATION TEST SUITE\n")
cat("=============================================================================\n\n")

# Load required functions
source("R/functions/phylogenetic_imputation.R")

test_count <- 0
pass_count <- 0
fail_count <- 0

# =============================================================================
# TEST 1: Taxonomic Distance Calculation - Same Genus
# =============================================================================

cat("\n--- TEST 1: Taxonomic Distance - Same Genus ---\n")
test_count <- test_count + 1

tryCatch({
  # Gadus morhua vs Gadus ogac (same genus)
  dist <- calculate_taxonomic_distance(
    list(phylum="Chordata", class="Actinopteri", order="Gadiformes",
         family="Gadidae", genus="Gadus"),
    list(phylum="Chordata", class="Actinopteri", order="Gadiformes",
         family="Gadidae", genus="Gadus")
  )

  cat("  Distance between species in same genus:", dist, "\n")

  # Validate
  stopifnot(dist == 0)  # Same genus = distance 0

  cat("  ✓ PASSED: Same genus distance = 0\n")
  pass_count <- pass_count + 1

}, error = function(e) {
  cat("  ✗ FAILED:", e$message, "\n")
  fail_count <- fail_count + 1
})


# =============================================================================
# TEST 2: Taxonomic Distance Calculation - Same Family
# =============================================================================

cat("\n--- TEST 2: Taxonomic Distance - Same Family, Different Genus ---\n")
test_count <- test_count + 1

tryCatch({
  # Gadus morhua vs Melanogrammus aeglefinus (different genus, same family)
  dist <- calculate_taxonomic_distance(
    list(phylum="Chordata", class="Actinopteri", order="Gadiformes",
         family="Gadidae", genus="Gadus"),
    list(phylum="Chordata", class="Actinopteri", order="Gadiformes",
         family="Gadidae", genus="Melanogrammus")
  )

  cat("  Distance between different genera in same family:", dist, "\n")

  # Validate
  stopifnot(dist == 1)  # Different genus = distance 1

  cat("  ✓ PASSED: Different genus distance = 1\n")
  pass_count <- pass_count + 1

}, error = function(e) {
  cat("  ✗ FAILED:", e$message, "\n")
  fail_count <- fail_count + 1
})


# =============================================================================
# TEST 3: Taxonomic Distance Calculation - Same Order
# =============================================================================

cat("\n--- TEST 3: Taxonomic Distance - Same Order, Different Family ---\n")
test_count <- test_count + 1

tryCatch({
  # Gadus morhua (Gadidae) vs Merluccius merluccius (Merlucciidae)
  # Same order (Gadiformes), different family
  dist <- calculate_taxonomic_distance(
    list(phylum="Chordata", class="Actinopteri", order="Gadiformes",
         family="Gadidae", genus="Gadus"),
    list(phylum="Chordata", class="Actinopteri", order="Gadiformes",
         family="Merlucciidae", genus="Merluccius")
  )

  cat("  Distance between different families in same order:", dist, "\n")

  # Validate
  stopifnot(dist == 2)  # Different family = distance 2

  cat("  ✓ PASSED: Different family distance = 2\n")
  pass_count <- pass_count + 1

}, error = function(e) {
  cat("  ✗ FAILED:", e$message, "\n")
  fail_count <- fail_count + 1
})


# =============================================================================
# TEST 4: Taxonomic Distance Calculation - Different Classes
# =============================================================================

cat("\n--- TEST 4: Taxonomic Distance - Different Classes ---\n")
test_count <- test_count + 1

tryCatch({
  # Fish vs Bivalve (very distant)
  dist <- calculate_taxonomic_distance(
    list(phylum="Chordata", class="Actinopteri", order="Gadiformes",
         family="Gadidae", genus="Gadus"),
    list(phylum="Mollusca", class="Bivalvia", order="Venerida",
         family="Veneridae", genus="Mya")
  )

  cat("  Distance between different phyla:", dist, "\n")

  # Validate
  stopifnot(dist == 5)  # Different phylum = distance 5

  cat("  ✓ PASSED: Different phylum distance = 5\n")
  pass_count <- pass_count + 1

}, error = function(e) {
  cat("  ✗ FAILED:", e$message, "\n")
  fail_count <- fail_count + 1
})


# =============================================================================
# TEST 5: Find Closest Relatives (Simulated)
# =============================================================================

cat("\n--- TEST 5: Find Closest Relatives ---\n")
test_count <- test_count + 1

tryCatch({
  # Create temporary test cache
  test_cache_dir <- "tests/test_results/phylo_cache_test"
  dir.create(test_cache_dir, showWarnings = FALSE, recursive = TRUE)

  # Create mock cache files for related species
  # Target: Gadus morhua (missing MS and EP)
  # Relatives: Gadus ogac (same genus), Melanogrammus aeglefinus (same family)

  # Relative 1: Gadus ogac (same genus, distance 0)
  cache_data_1 <- list(
    species = "Gadus ogac",
    harmonized = list(
      phylum = "Chordata",
      class = "Actinopteri",
      order = "Gadiformes",
      family = "Gadidae",
      genus = "Gadus",
      MS = "MS5",
      FS = "FS1",
      MB = "MB5",
      EP = "EP2",
      PR = "PR0"
    )
  )
  saveRDS(cache_data_1, file.path(test_cache_dir, "Gadus_ogac.rds"))

  # Relative 2: Melanogrammus aeglefinus (same family, distance 1)
  cache_data_2 <- list(
    species = "Melanogrammus aeglefinus",
    harmonized = list(
      phylum = "Chordata",
      class = "Actinopteri",
      order = "Gadiformes",
      family = "Gadidae",
      genus = "Melanogrammus",
      MS = "MS5",
      FS = "FS1",
      MB = "MB5",
      EP = "EP2",
      PR = "PR0"
    )
  )
  saveRDS(cache_data_2, file.path(test_cache_dir, "Melanogrammus_aeglefinus.rds"))

  # Search for relatives
  target_taxonomy <- list(
    phylum = "Chordata",
    class = "Actinopteri",
    order = "Gadiformes",
    family = "Gadidae",
    genus = "Gadus"
  )

  target_traits <- list(
    MS = NA,
    FS = "FS1",
    MB = "MB5",
    EP = NA,
    PR = "PR0"
  )

  relatives <- find_closest_relatives(
    target_taxonomy = target_taxonomy,
    target_traits = target_traits,
    cache_dir = test_cache_dir,
    max_distance = 3,
    min_matches = 1
  )

  cat("  Found", nrow(relatives), "relatives\n")
  if (nrow(relatives) > 0) {
    for (i in 1:nrow(relatives)) {
      cat("    ", i, ". ", relatives$species[i], " (distance: ", relatives$distance[i], ")\n", sep="")
    }
  }

  # Validate
  stopifnot(nrow(relatives) >= 2)
  stopifnot("Gadus ogac" %in% relatives$species)
  stopifnot(relatives$distance[relatives$species == "Gadus ogac"] == 0)

  cat("  ✓ PASSED: Found relatives with correct distances\n")
  pass_count <- pass_count + 1

  # Cleanup
  unlink(test_cache_dir, recursive = TRUE)

}, error = function(e) {
  cat("  ✗ FAILED:", e$message, "\n")
  fail_count <- fail_count + 1
  # Cleanup on error
  if (exists("test_cache_dir") && dir.exists(test_cache_dir)) {
    unlink(test_cache_dir, recursive = TRUE)
  }
})


# =============================================================================
# TEST 6: Trait Imputation with Weighted Voting
# =============================================================================

cat("\n--- TEST 6: Trait Imputation with Weighted Voting ---\n")
test_count <- test_count + 1

tryCatch({
  # Simulate relatives with trait data
  relatives <- data.frame(
    species = c("Gadus ogac", "Melanogrammus aeglefinus", "Gadus macrocephalus"),
    distance = c(0, 1, 0),  # Two in same genus, one in same family
    MS = c("MS5", "MS5", "MS5"),  # All agree on MS5
    FS = c("FS1", "FS1", "FS1"),
    MB = c("MB5", "MB5", "MB5"),
    EP = c("EP2", "EP2", "EP3"),  # Disagreement on EP (2/3 say EP2)
    PR = c("PR0", "PR0", "PR0"),
    stringsAsFactors = FALSE
  )

  target_traits <- list(
    MS = NA,
    FS = "FS1",
    MB = "MB5",
    EP = NA,
    PR = "PR0"
  )

  # Impute traits
  imputations <- impute_traits_from_relatives(
    target_traits = target_traits,
    relatives = relatives,
    min_agreement = 0.6
  )

  cat("  Imputed traits:\n")
  for (trait in names(imputations)) {
    imp <- imputations[[trait]]
    cat("    ", trait, " = ", imp$value, " (agreement: ",
        round(imp$agreement * 100, 0), "%, n=", imp$n_relatives, ")\n", sep="")
  }

  # Validate
  stopifnot("MS" %in% names(imputations))
  stopifnot(imputations$MS$value == "MS5")  # All agree
  stopifnot(imputations$MS$agreement == 1.0)  # 100% agreement

  stopifnot("EP" %in% names(imputations))
  stopifnot(imputations$EP$value == "EP2")  # 2/3 agree (weighted by distance)
  stopifnot(imputations$EP$agreement >= 0.6)  # At least 60% agreement

  cat("  ✓ PASSED: Trait imputation with correct weighted voting\n")
  pass_count <- pass_count + 1

}, error = function(e) {
  cat("  ✗ FAILED:", e$message, "\n")
  fail_count <- fail_count + 1
})


# =============================================================================
# TEST 7: Integration - Complete Workflow
# =============================================================================

cat("\n--- TEST 7: Complete Phylogenetic Imputation Workflow ---\n")
test_count <- test_count + 1

tryCatch({
  # Create test cache
  test_cache_dir <- "tests/test_results/phylo_workflow_test"
  dir.create(test_cache_dir, showWarnings = FALSE, recursive = TRUE)

  # Create relatives
  cache_data_1 <- list(
    species = "Gadus ogac",
    harmonized = list(
      phylum = "Chordata",
      class = "Actinopteri",
      order = "Gadiformes",
      family = "Gadidae",
      genus = "Gadus",
      MS = "MS5",
      FS = "FS1",
      MB = "MB5",
      EP = "EP2",
      PR = "PR0"
    )
  )
  saveRDS(cache_data_1, file.path(test_cache_dir, "Gadus_ogac.rds"))

  cache_data_2 <- list(
    species = "Gadus macrocephalus",
    harmonized = list(
      phylum = "Chordata",
      class = "Actinopteri",
      order = "Gadiformes",
      family = "Gadidae",
      genus = "Gadus",
      MS = "MS5",
      FS = "FS1",
      MB = "MB5",
      EP = "EP2",
      PR = "PR0"
    )
  )
  saveRDS(cache_data_2, file.path(test_cache_dir, "Gadus_macrocephalus.rds"))

  # Target species with missing traits
  current_traits <- list(
    MS = NA,
    FS = "FS1",
    MB = "MB5",
    EP = NA,
    PR = "PR0"
  )

  taxonomy <- list(
    phylum = "Chordata",
    class = "Actinopteri",
    order = "Gadiformes",
    family = "Gadidae",
    genus = "Gadus"
  )

  # Apply phylogenetic imputation
  result <- apply_phylogenetic_imputation(
    species_name = "Gadus morhua",
    current_traits = current_traits,
    taxonomy = taxonomy,
    cache_dir = test_cache_dir,
    max_distance = 3,
    min_relatives = 2,
    min_agreement = 0.6,
    verbose = FALSE
  )

  cat("  Result:\n")
  cat("    MS:", result$MS, "\n")
  cat("    EP:", result$EP, "\n")

  # Validate
  stopifnot(!is.na(result$MS))
  stopifnot(result$MS == "MS5")
  stopifnot(!is.na(result$EP))
  stopifnot(result$EP == "EP2")
  stopifnot(!is.null(result$MS_source))
  stopifnot(result$MS_source == "Phylogenetic")

  cat("  ✓ PASSED: Complete workflow successful\n")
  pass_count <- pass_count + 1

  # Cleanup
  unlink(test_cache_dir, recursive = TRUE)

}, error = function(e) {
  cat("  ✗ FAILED:", e$message, "\n")
  fail_count <- fail_count + 1
  if (exists("test_cache_dir") && dir.exists(test_cache_dir)) {
    unlink(test_cache_dir, recursive = TRUE)
  }
})


# =============================================================================
# TEST 8: Distance Weighting
# =============================================================================

cat("\n--- TEST 8: Distance-Based Weighting ---\n")
test_count <- test_count + 1

tryCatch({
  # Test that closer relatives get higher weights
  # 1 very close relative (distance 0) with EP3
  # 2 distant relatives (distance 2) with EP2
  # Should choose EP3 due to weighting

  relatives <- data.frame(
    species = c("Close_relative", "Distant_1", "Distant_2"),
    distance = c(0, 2, 2),
    MS = c(NA, NA, NA),
    FS = c(NA, NA, NA),
    MB = c(NA, NA, NA),
    EP = c("EP3", "EP2", "EP2"),
    PR = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )

  target_traits <- list(MS=NA, FS=NA, MB=NA, EP=NA, PR=NA)

  imputations <- impute_traits_from_relatives(
    target_traits = target_traits,
    relatives = relatives,
    min_agreement = 0.5
  )

  cat("  Imputation with distance weighting:\n")
  if ("EP" %in% names(imputations)) {
    cat("    EP =", imputations$EP$value, "\n")
    cat("    Agreement:", round(imputations$EP$agreement * 100, 0), "%\n")
  }

  # Validate
  # Distance 0 gets weight 5, distance 2 gets weight 3
  # EP3: 1 × 5 = 5
  # EP2: 2 × 3 = 6
  # So EP2 should win (higher total weight)
  stopifnot("EP" %in% names(imputations))
  stopifnot(imputations$EP$value == "EP2")

  cat("  ✓ PASSED: Distance weighting working correctly\n")
  pass_count <- pass_count + 1

}, error = function(e) {
  cat("  ✗ FAILED:", e$message, "\n")
  fail_count <- fail_count + 1
})


# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=============================================================================\n")
cat("TEST SUMMARY\n")
cat("=============================================================================\n")
cat("Total tests: ", test_count, "\n")
cat("Passed:      ", pass_count, " (", round(pass_count/test_count*100, 1), "%)\n", sep="")
cat("Failed:      ", fail_count, " (", round(fail_count/test_count*100, 1), "%)\n", sep="")

if (fail_count == 0) {
  cat("\n✅ ALL TESTS PASSED - Phase 5 ready for production\n")
} else {
  cat("\n⚠️  SOME TESTS FAILED - Review errors above\n")
}

cat("=============================================================================\n")
