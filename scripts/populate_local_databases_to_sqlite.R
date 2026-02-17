# =============================================================================
# Populate SQLite Database with Local Trait Databases
# =============================================================================
#
# This script loads data from local Excel files and populates the SQLite
# ontology database:
# 1. bvol_nomp_version_2024.xlsx (3,846 phytoplankton species)
# 2. species_enriched.xlsx (915 marine invertebrates)
#
# Total: ~4,761 additional species
#
# Version: 1.4.1 (Phase 6 extension)
# Date: 2025-12-26
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("POPULATE SQLITE DATABASE WITH LOCAL TRAIT DATABASES\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("\n")

# Load required modules
source("R/functions/local_trait_databases.R")
source("R/functions/cache_sqlite.R")
source("R/functions/trait_lookup.R")  # For harmonize_size_class

# Configuration
SQLITE_DB_PATH <- "cache/taxonomy.db"
BATCH_SIZE <- 100

# Initialize counters
total_processed <- 0
total_added <- 0
total_skipped <- 0
total_errors <- 0

# Initialize database if it doesn't exist
if (!file.exists(SQLITE_DB_PATH)) {
  cat("Initializing new SQLite database...\n")
  initialize_cache_db(SQLITE_DB_PATH)
  cat("✓ Database initialized\n\n")
}

# =============================================================================
# POPULATE FROM BVOL DATABASE (PHYTOPLANKTON)
# =============================================================================

cat("--- PROCESSING: BVOL Phytoplankton Database ---\n\n")

bvol_db <- load_bvol_database()

if (!is.null(bvol_db)) {
  cat(sprintf("Found %d phytoplankton species\n", nrow(bvol_db)))
  cat("Processing in batches...\n\n")

  bvol_processed <- 0
  bvol_added <- 0
  bvol_skipped <- 0
  bvol_errors <- 0

  # Process each species
  for (i in 1:nrow(bvol_db)) {
    aphia_id <- bvol_db$AphiaID[i]

    # Skip if no valid AphiaID
    if (is.na(aphia_id)) {
      bvol_skipped <- bvol_skipped + 1
      next
    }

    tryCatch({
      # Lookup raw traits
      raw_traits <- lookup_bvol_traits(aphia_id = aphia_id)

      if (is.null(raw_traits)) {
        bvol_skipped <- bvol_skipped + 1
        next
      }

      # Harmonize traits
      harmonized <- harmonize_bvol_traits(raw_traits)

      # Merge raw and harmonized
      species_data <- c(raw_traits, harmonized)
      species_data$species <- raw_traits$species

      # Save to SQLite
      save_species_to_cache(species_data, SQLITE_DB_PATH)

      bvol_added <- bvol_added + 1
      bvol_processed <- bvol_processed + 1

      # Progress indicator
      if (bvol_processed %% BATCH_SIZE == 0) {
        cat(sprintf("  Processed: %d/%d (added: %d, skipped: %d)\n",
                   bvol_processed, nrow(bvol_db), bvol_added, bvol_skipped))
      }

    }, error = function(e) {
      bvol_errors <- bvol_errors + 1
      if (bvol_errors < 10) {  # Show first 10 errors
        cat(sprintf("  ✗ Error with AphiaID %d: %s\n", aphia_id, e$message))
      }
    })
  }

  cat("\n")
  cat("✓ BVOL Processing Complete\n")
  cat(sprintf("  Total processed: %d\n", bvol_processed))
  cat(sprintf("  Added to database: %d\n", bvol_added))
  cat(sprintf("  Skipped (already cached): %d\n", bvol_skipped))
  cat(sprintf("  Errors: %d\n", bvol_errors))
  cat("\n")

  total_processed <- total_processed + bvol_processed
  total_added <- total_added + bvol_added
  total_skipped <- total_skipped + bvol_skipped
  total_errors <- total_errors + bvol_errors
}

# =============================================================================
# POPULATE FROM SPECIES ENRICHED DATABASE (MARINE INVERTEBRATES)
# =============================================================================

cat("--- PROCESSING: Species Enriched Marine Invertebrates Database ---\n\n")

species_db <- load_species_enriched_database()

if (!is.null(species_db)) {
  cat(sprintf("Found %d marine invertebrate species\n", nrow(species_db)))
  cat("Processing in batches...\n\n")

  species_processed <- 0
  species_added <- 0
  species_skipped <- 0
  species_errors <- 0

  # Process each species
  for (i in 1:nrow(species_db)) {
    aphia_id <- species_db$aphiaID[i]

    # Skip if no valid AphiaID
    if (is.na(aphia_id)) {
      species_skipped <- species_skipped + 1
      next
    }

    tryCatch({
      # Lookup raw traits
      raw_traits <- lookup_species_enriched_traits(aphia_id = aphia_id)

      if (is.null(raw_traits)) {
        species_skipped <- species_skipped + 1
        next
      }

      # Harmonize traits
      harmonized <- harmonize_species_enriched_traits(raw_traits)

      # Merge raw and harmonized
      species_data <- c(raw_traits, harmonized)
      species_data$species <- raw_traits$species

      # Save to SQLite
      save_species_to_cache(species_data, SQLITE_DB_PATH)

      species_added <- species_added + 1
      species_processed <- species_processed + 1

      # Progress indicator
      if (species_processed %% BATCH_SIZE == 0) {
        cat(sprintf("  Processed: %d/%d (added: %d, skipped: %d)\n",
                   species_processed, nrow(species_db), species_added, species_skipped))
      }

    }, error = function(e) {
      species_errors <- species_errors + 1
      if (species_errors < 10) {  # Show first 10 errors
        cat(sprintf("  ✗ Error with AphiaID %d: %s\n", aphia_id, e$message))
      }
    })
  }

  cat("\n")
  cat("✓ Species Enriched Processing Complete\n")
  cat(sprintf("  Total processed: %d\n", species_processed))
  cat(sprintf("  Added to database: %d\n", species_added))
  cat(sprintf("  Skipped (already cached): %d\n", species_skipped))
  cat(sprintf("  Errors: %d\n", species_errors))
  cat("\n")

  total_processed <- total_processed + species_processed
  total_added <- total_added + species_added
  total_skipped <- total_skipped + species_skipped
  total_errors <- total_errors + species_errors
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("POPULATION SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat(sprintf("Total species processed:  %d\n", total_processed))
cat(sprintf("Added to database:        %d\n", total_added))
cat(sprintf("Skipped (already cached): %d\n", total_skipped))
cat(sprintf("Errors:                   %d\n", total_errors))
cat("\n")

# Database statistics
if (file.exists(SQLITE_DB_PATH)) {
  cat("FINAL DATABASE STATISTICS\n")
  cat("───────────────────────────────────────────────────────────────\n")
  print_cache_stats(SQLITE_DB_PATH)
}

cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("✓ POPULATION COMPLETE\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("\n")
