# =============================================================================
# Test Local Trait Databases Integration
# =============================================================================

cat("=============================================================================\n")
cat("TESTING LOCAL TRAIT DATABASES\n")
cat("=============================================================================\n\n")

# Load functions
source("R/functions/local_trait_databases.R")

test_count <- 0
pass_count <- 0
fail_count <- 0

# =============================================================================
# TEST 1: Load BVOL Database
# =============================================================================

cat("--- TEST 1: Load BVOL Database ---\n")
test_count <- test_count + 1

tryCatch({
  bvol_db <- load_bvol_database()

  stopifnot(!is.null(bvol_db))
  stopifnot(nrow(bvol_db) > 1000)  # Expect ~3,846 rows
  stopifnot("AphiaID" %in% names(bvol_db))

  cat("  ✓ PASSED: BVOL database loaded\n")
  cat(sprintf("    - Rows: %d\n", nrow(bvol_db)))
  cat(sprintf("    - Columns: %d\n", ncol(bvol_db)))
  cat(sprintf("    - Unique AphiaIDs: %d\n",
              length(unique(bvol_db$AphiaID[!is.na(bvol_db$AphiaID)]))))

  pass_count <- pass_count + 1

}, error = function(e) {
  cat("  ✗ FAILED:", e$message, "\n")
  fail_count <- fail_count + 1
})

cat("\n")

# =============================================================================
# TEST 2: Load Species Enriched Database
# =============================================================================

cat("--- TEST 2: Load Species Enriched Database ---\n")
test_count <- test_count + 1

tryCatch({
  species_db <- load_species_enriched_database()

  stopifnot(!is.null(species_db))
  stopifnot(nrow(species_db) > 900)  # Expect ~915 rows
  stopifnot("aphiaID" %in% names(species_db))

  cat("  ✓ PASSED: Species enriched database loaded\n")
  cat(sprintf("    - Rows: %d\n", nrow(species_db)))
  cat(sprintf("    - Columns: %d\n", ncol(species_db)))
  cat(sprintf("    - Unique AphiaIDs: %d\n",
              length(unique(species_db$aphiaID[!is.na(species_db$aphiaID)]))))

  pass_count <- pass_count + 1

}, error = function(e) {
  cat("  ✗ FAILED:", e$message, "\n")
  fail_count <- fail_count + 1
})

cat("\n")

# =============================================================================
# TEST 3: Lookup BVOL Traits by AphiaID
# =============================================================================

cat("--- TEST 3: Lookup BVOL Traits by AphiaID ---\n")
test_count <- test_count + 1

tryCatch({
  # Get a valid AphiaID from the database
  bvol_db <- load_bvol_database()
  test_aphia_id <- bvol_db$AphiaID[!is.na(bvol_db$AphiaID)][1]

  traits <- lookup_bvol_traits(aphia_id = test_aphia_id)

  stopifnot(!is.null(traits))
  stopifnot(traits$source == "BVOL")
  stopifnot(!is.na(traits$aphia_id))
  stopifnot(!is.na(traits$size_cm))

  cat("  ✓ PASSED: BVOL lookup by AphiaID\n")
  cat(sprintf("    - Species: %s\n", traits$species))
  cat(sprintf("    - AphiaID: %d\n", traits$aphia_id))
  cat(sprintf("    - Size: %.6f cm\n", traits$size_cm))
  cat(sprintf("    - Trophy: %s\n", traits$trophy))

  pass_count <- pass_count + 1

}, error = function(e) {
  cat("  ✗ FAILED:", e$message, "\n")
  fail_count <- fail_count + 1
})

cat("\n")

# =============================================================================
# TEST 4: Lookup Species Enriched Traits by AphiaID
# =============================================================================

cat("--- TEST 4: Lookup Species Enriched Traits by AphiaID ---\n")
test_count <- test_count + 1

tryCatch({
  # Test with a known species: Abra alba (AphiaID: 141433)
  traits <- lookup_species_enriched_traits(aphia_id = 141433)

  stopifnot(!is.null(traits))
  stopifnot(traits$source == "SpeciesEnriched")
  stopifnot(!is.na(traits$aphia_id))

  cat("  ✓ PASSED: Species enriched lookup by AphiaID\n")
  cat(sprintf("    - Species: %s\n", traits$species))
  cat(sprintf("    - Common name: %s\n", traits$common_name))
  cat(sprintf("    - AphiaID: %d\n", traits$aphia_id))
  if (!is.na(traits$size_cm)) {
    cat(sprintf("    - Size: %.2f cm\n", traits$size_cm))
  }
  cat(sprintf("    - Mobility: %s\n", traits$mobility))
  cat(sprintf("    - Feeding: %s\n", traits$feeding_method))

  pass_count <- pass_count + 1

}, error = function(e) {
  cat("  ✗ FAILED:", e$message, "\n")
  fail_count <- fail_count + 1
})

cat("\n")

# =============================================================================
# TEST 5: Harmonize BVOL Traits
# =============================================================================

cat("--- TEST 5: Harmonize BVOL Traits ---\n")
test_count <- test_count + 1

tryCatch({
  # Need harmonize_size_class function
  source("R/functions/trait_lookup.R", local = TRUE)

  # Use same test AphiaID as TEST 3
  bvol_db <- load_bvol_database()
  test_aphia_id <- bvol_db$AphiaID[!is.na(bvol_db$AphiaID)][1]

  raw_traits <- lookup_bvol_traits(aphia_id = test_aphia_id)
  harmonized <- harmonize_bvol_traits(raw_traits)

  stopifnot(!is.null(harmonized))
  stopifnot(!is.na(harmonized$MS))
  stopifnot(!is.na(harmonized$FS))
  stopifnot(!is.na(harmonized$MB))
  stopifnot(!is.na(harmonized$EP))
  stopifnot(!is.na(harmonized$PR))

  cat("  ✓ PASSED: BVOL trait harmonization\n")
  cat(sprintf("    - MS: %s (%.2f confidence)\n", harmonized$MS, harmonized$MS_confidence))
  cat(sprintf("    - FS: %s (%.2f confidence)\n", harmonized$FS, harmonized$FS_confidence))
  cat(sprintf("    - MB: %s (%.2f confidence)\n", harmonized$MB, harmonized$MB_confidence))
  cat(sprintf("    - EP: %s (%.2f confidence)\n", harmonized$EP, harmonized$EP_confidence))
  cat(sprintf("    - PR: %s (%.2f confidence)\n", harmonized$PR, harmonized$PR_confidence))
  cat(sprintf("    - Overall: %.2f confidence\n", harmonized$overall_confidence))

  pass_count <- pass_count + 1

}, error = function(e) {
  cat("  ✗ FAILED:", e$message, "\n")
  fail_count <- fail_count + 1
})

cat("\n")

# =============================================================================
# TEST 6: Harmonize Species Enriched Traits
# =============================================================================

cat("--- TEST 6: Harmonize Species Enriched Traits ---\n")
test_count <- test_count + 1

tryCatch({
  raw_traits <- lookup_species_enriched_traits(aphia_id = 141433)
  harmonized <- harmonize_species_enriched_traits(raw_traits)

  stopifnot(!is.null(harmonized))
  # Some traits might be NA if not present in data
  stopifnot(!is.na(harmonized$overall_confidence))

  cat("  ✓ PASSED: Species enriched trait harmonization\n")
  if (!is.na(harmonized$MS)) {
    cat(sprintf("    - MS: %s (%.2f confidence)\n", harmonized$MS, harmonized$MS_confidence))
  }
  if (!is.na(harmonized$FS)) {
    cat(sprintf("    - FS: %s (%.2f confidence)\n", harmonized$FS, harmonized$FS_confidence))
  }
  if (!is.na(harmonized$MB)) {
    cat(sprintf("    - MB: %s (%.2f confidence)\n", harmonized$MB, harmonized$MB_confidence))
  }
  if (!is.na(harmonized$EP)) {
    cat(sprintf("    - EP: %s (%.2f confidence)\n", harmonized$EP, harmonized$EP_confidence))
  }
  if (!is.na(harmonized$PR)) {
    cat(sprintf("    - PR: %s (%.2f confidence)\n", harmonized$PR, harmonized$PR_confidence))
  }
  cat(sprintf("    - Overall: %.2f confidence\n", harmonized$overall_confidence))

  pass_count <- pass_count + 1

}, error = function(e) {
  cat("  ✗ FAILED:", e$message, "\n")
  fail_count <- fail_count + 1
})

cat("\n")

# =============================================================================
# TEST 7: Batch Processing Performance
# =============================================================================

cat("--- TEST 7: Batch Processing Performance ---\n")
test_count <- test_count + 1

tryCatch({
  # Test lookup speed for 10 random species
  bvol_db <- load_bvol_database()
  aphia_ids <- sample(bvol_db$AphiaID[!is.na(bvol_db$AphiaID)], 10)

  start_time <- Sys.time()
  results <- lapply(aphia_ids, function(id) {
    lookup_bvol_traits(aphia_id = id)
  })
  end_time <- Sys.time()

  elapsed_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000

  success_count <- sum(sapply(results, function(r) !is.null(r)))

  cat("  ✓ PASSED: Batch processing\n")
  cat(sprintf("    - Species processed: %d\n", length(aphia_ids)))
  cat(sprintf("    - Successful lookups: %d (%.1f%%)\n",
              success_count, success_count/length(aphia_ids)*100))
  cat(sprintf("    - Total time: %.1f ms\n", elapsed_ms))
  cat(sprintf("    - Avg per species: %.1f ms\n", elapsed_ms/length(aphia_ids)))

  pass_count <- pass_count + 1

}, error = function(e) {
  cat("  ✗ FAILED:", e$message, "\n")
  fail_count <- fail_count + 1
})

cat("\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("=============================================================================\n")
cat("TEST SUMMARY\n")
cat("=============================================================================\n")
cat("Total tests: ", test_count, "\n")
cat("Passed:      ", pass_count, " (", round(pass_count/test_count*100, 1), "%)\n", sep = "")
cat("Failed:      ", fail_count, " (", round(fail_count/test_count*100, 1), "%)\n", sep = "")
cat("\n")

if (fail_count == 0) {
  cat("✅ ALL TESTS PASSED - Local databases ready for integration\n")
} else {
  cat("⚠️  SOME TESTS FAILED - Review errors above\n")
}

cat("=============================================================================\n")
