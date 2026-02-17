# =============================================================================
# Automated Size Data Extraction
# =============================================================================
#
# This script automatically extracts body size data from FishBase and
# SeaLifeBase for all species in the ontology database.
#
# Date: 2025-12-25
# Version: 1.0
#
# =============================================================================

cat("=============================================================================\n")
cat("AUTOMATED SIZE DATA EXTRACTION\n")
cat("=============================================================================\n\n")

# Load required packages
if (!requireNamespace("rfishbase", quietly = TRUE)) {
  cat("Installing rfishbase package...\n")
  install.packages("rfishbase", repos = "https://cloud.r-project.org")
}

library(rfishbase)

# Load trait lookup functions (for WoRMS)
source("R/functions/trait_lookup.R")

# =============================================================================
# STEP 1: READ ONTOLOGY SPECIES LIST
# =============================================================================

cat("STEP 1: Reading ontology database...\n")
ontology_file <- "data/ontology_traits.csv"

if (!file.exists(ontology_file)) {
  stop("Ontology file not found: ", ontology_file)
}

ontology <- read.csv(ontology_file, stringsAsFactors = FALSE)
unique_species <- unique(ontology[, c("taxon_name", "aphia_id")])

cat("  Found", nrow(unique_species), "unique species in ontology\n\n")

# =============================================================================
# STEP 2: EXTRACT SIZE DATA FROM FISHBASE/SEALIFEBASE
# =============================================================================

cat("STEP 2: Extracting size data from databases...\n\n")

size_data <- data.frame(
  taxon_name = character(),
  aphia_id = integer(),
  trait_category = character(),
  trait_name = character(),
  trait_modality = character(),
  trait_value_min = numeric(),
  trait_value_max = numeric(),
  unit = character(),
  ontology = character(),
  ontology_id = character(),
  source = character(),
  notes = character(),
  stringsAsFactors = FALSE
)

# Progress tracking
total <- nrow(unique_species)
pb <- txtProgressBar(min = 0, max = total, style = 3)
success_count <- 0
fail_count <- 0

for (i in 1:total) {
  species_name <- unique_species$taxon_name[i]
  aphia_id <- unique_species$aphia_id[i]

  # Initialize variables
  max_length <- NA
  max_weight <- NA
  source_db <- NA
  size_extracted <- FALSE

  tryCatch({

    # =================================================================
    # TRY FISHBASE FIRST (for fish species)
    # =================================================================

    fishbase_data <- tryCatch({
      rfishbase::species(species_name)
    }, error = function(e) NULL)

    if (!is.null(fishbase_data) && nrow(fishbase_data) > 0) {

      # Extract max length (TL - Total Length)
      if ("Length" %in% names(fishbase_data) && !is.na(fishbase_data$Length[1])) {
        max_length <- fishbase_data$Length[1]
        source_db <- "FishBase"
        size_extracted <- TRUE
      }

      # Extract max weight
      if ("Weight" %in% names(fishbase_data) && !is.na(fishbase_data$Weight[1])) {
        max_weight <- fishbase_data$Weight[1]
        if (is.na(source_db)) source_db <- "FishBase"
        size_extracted <- TRUE
      }

      cat(sprintf("  [%d/%d] %-30s → FishBase: Length=%.1f cm, Weight=%.1f kg\n",
                  i, total, substr(species_name, 1, 30),
                  ifelse(is.na(max_length), 0, max_length),
                  ifelse(is.na(max_weight), 0, max_weight)))
    }

    # =================================================================
    # TRY SEALIFEBASE IF FISHBASE FAILED (for invertebrates)
    # =================================================================

    if (!size_extracted) {

      sealifebase_data <- tryCatch({
        rfishbase::species(species_name, server = "sealifebase")
      }, error = function(e) NULL)

      if (!is.null(sealifebase_data) && nrow(sealifebase_data) > 0) {

        # Extract max length
        if ("Length" %in% names(sealifebase_data) && !is.na(sealifebase_data$Length[1])) {
          max_length <- sealifebase_data$Length[1]
          source_db <- "SeaLifeBase"
          size_extracted <- TRUE
        }

        # Extract max weight
        if ("Weight" %in% names(sealifebase_data) && !is.na(sealifebase_data$Weight[1])) {
          max_weight <- sealifebase_data$Weight[1]
          if (is.na(source_db)) source_db <- "SeaLifeBase"
          size_extracted <- TRUE
        }

        cat(sprintf("  [%d/%d] %-30s → SeaLifeBase: Length=%.1f cm, Weight=%.1f kg\n",
                    i, total, substr(species_name, 1, 30),
                    ifelse(is.na(max_length), 0, max_length),
                    ifelse(is.na(max_weight), 0, max_weight)))
      }
    }

    # =================================================================
    # ADD TO SIZE DATA IF SUCCESSFUL
    # =================================================================

    if (size_extracted) {

      # Add length record
      if (!is.na(max_length) && max_length > 0) {
        size_data <- rbind(size_data, data.frame(
          taxon_name = species_name,
          aphia_id = aphia_id,
          trait_category = "morphology",
          trait_name = "max_length",
          trait_modality = "body_size",
          trait_value_min = NA,
          trait_value_max = max_length,
          unit = "cm",
          ontology = "PATO",
          ontology_id = "PATO:0000117",
          source = source_db,
          notes = "Maximum total length (automated extraction)",
          stringsAsFactors = FALSE
        ))
      }

      # Add weight record
      if (!is.na(max_weight) && max_weight > 0) {
        size_data <- rbind(size_data, data.frame(
          taxon_name = species_name,
          aphia_id = aphia_id,
          trait_category = "morphology",
          trait_name = "max_weight",
          trait_modality = "body_mass",
          trait_value_min = NA,
          trait_value_max = max_weight,
          unit = "kg",
          ontology = "PATO",
          ontology_id = "PATO:0000125",
          source = source_db,
          notes = "Maximum weight (automated extraction)",
          stringsAsFactors = FALSE
        ))
      }

      success_count <- success_count + 1

    } else {
      cat(sprintf("  [%d/%d] %-30s → No size data found\n",
                  i, total, substr(species_name, 1, 30)))
      fail_count <- fail_count + 1
    }

    # Rate limiting (be nice to APIs)
    Sys.sleep(0.5)

  }, error = function(e) {
    cat(sprintf("  [%d/%d] %-30s → ERROR: %s\n",
                i, total, substr(species_name, 1, 30), e$message))
    fail_count <<- fail_count + 1
  })

  setTxtProgressBar(pb, i)
}

close(pb)

cat("\n\n")

# =============================================================================
# STEP 3: SAVE RESULTS
# =============================================================================

cat("STEP 3: Saving extracted size data...\n\n")

if (nrow(size_data) > 0) {

  output_file <- "data/size_traits_automated.csv"
  write.csv(size_data, output_file, row.names = FALSE)

  cat("✓ Size data saved to:", output_file, "\n")
  cat("  Records extracted:", nrow(size_data), "\n")
  cat("  Species with size data:", length(unique(size_data$taxon_name)), "\n\n")

  # Summary statistics
  cat("Summary:\n")
  cat("  Total species attempted:", total, "\n")
  cat("  Successful extractions:", success_count, sprintf(" (%.1f%%)\n", success_count/total*100))
  cat("  Failed extractions:", fail_count, sprintf(" (%.1f%%)\n", fail_count/total*100))

  # Breakdown by trait type
  cat("\nTrait breakdown:\n")
  length_records <- sum(size_data$trait_name == "max_length")
  weight_records <- sum(size_data$trait_name == "max_weight")
  cat("  Max length records:", length_records, "\n")
  cat("  Max weight records:", weight_records, "\n")

  # Breakdown by source
  cat("\nSource breakdown:\n")
  source_table <- table(size_data$source)
  for (src in names(source_table)) {
    cat(sprintf("  %-15s: %d records\n", src, source_table[src]))
  }

  # Size distribution
  cat("\nSize distribution (length only):\n")
  lengths <- size_data[size_data$trait_name == "max_length", "trait_value_max"]
  if (length(lengths) > 0) {
    cat(sprintf("  Min: %.1f cm\n", min(lengths, na.rm = TRUE)))
    cat(sprintf("  Max: %.1f cm\n", max(lengths, na.rm = TRUE)))
    cat(sprintf("  Mean: %.1f cm\n", mean(lengths, na.rm = TRUE)))
    cat(sprintf("  Median: %.1f cm\n", median(lengths, na.rm = TRUE)))
  }

} else {
  cat("⚠️  No size data extracted\n")
  cat("  This might happen if:\n")
  cat("  - Species names don't match FishBase/SeaLifeBase\n")
  cat("  - Species are not in these databases (e.g., phytoplankton)\n")
  cat("  - API connection issues\n")
}

cat("\n")

# =============================================================================
# STEP 4: OPTIONAL - MERGE WITH ONTOLOGY
# =============================================================================

cat("=============================================================================\n")
cat("NEXT STEPS\n")
cat("=============================================================================\n\n")

if (nrow(size_data) > 0) {
  cat("✓ Size data successfully extracted!\n\n")
  cat("To merge with ontology database:\n")
  cat("  1. Review extracted data: data/size_traits_automated.csv\n")
  cat("  2. Run merge script:\n")
  cat("     source('scripts/merge_trait_data.R')\n")
  cat("  3. Retrain ML models:\n")
  cat("     source('scripts/train_trait_models.R')\n\n")
  cat("Or run all at once:\n")
  cat("  # Update size_traits_template with automated data\n")
  cat("  file.copy('data/size_traits_automated.csv',\n")
  cat("            'data/size_traits_template.csv',\n")
  cat("            overwrite = TRUE)\n")
  cat("  source('scripts/merge_trait_data.R')\n")
  cat("  source('scripts/train_trait_models.R')\n")
} else {
  cat("⚠️  No data extracted. Try:\n")
  cat("  1. Check internet connection\n")
  cat("  2. Verify species names match FishBase/SeaLifeBase\n")
  cat("  3. Add size data manually for problematic species\n")
}

cat("\n")
cat("=============================================================================\n")
