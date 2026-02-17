# =============================================================================
# Merge Additional Trait Data into Ontology
# =============================================================================
#
# This script merges size and protection trait data into the ontology database
#
# Usage:
#   1. Fill in data/size_traits_template.csv
#   2. Fill in data/protection_traits_template.csv
#   3. Run this script
#
# =============================================================================

cat("=============================================================================\n")
cat("MERGE TRAIT DATA INTO ONTOLOGY\n")
cat("=============================================================================\n\n")

# Read existing ontology
ontology_file <- "data/ontology_traits.csv"
ontology <- read.csv(ontology_file, stringsAsFactors = FALSE)

cat("Existing ontology:\n")
cat("  Records:", nrow(ontology), "\n")
cat("  Species:", length(unique(ontology$taxon_name)), "\n\n")

# Read size traits
size_file <- "data/size_traits_template.csv"
if (file.exists(size_file)) {
  size_traits <- read.csv(size_file, stringsAsFactors = FALSE)
  cat("Size traits to add:\n")
  cat("  Records:", nrow(size_traits), "\n")
  cat("  Species:", length(unique(size_traits$taxon_name)), "\n\n")
} else {
  cat("⚠️  Size traits file not found:", size_file, "\n")
  size_traits <- data.frame()
}

# Read protection traits
protection_file <- "data/protection_traits_template.csv"
if (file.exists(protection_file)) {
  protection_traits <- read.csv(protection_file, stringsAsFactors = FALSE)
  cat("Protection traits to add:\n")
  cat("  Records:", nrow(protection_traits), "\n")
  cat("  Species:", length(unique(protection_traits$taxon_name)), "\n\n")
} else {
  cat("⚠️  Protection traits file not found:", protection_file, "\n")
  protection_traits <- data.frame()
}

# Standardize column names (handle missing columns)
standard_cols <- c("taxon_name", "aphia_id", "trait_category", "trait_name",
                   "trait_modality", "trait_score", "unit", "ontology",
                   "ontology_id", "source", "notes")

# Add missing columns with NA
for (df_name in c("ontology", "size_traits", "protection_traits")) {
  df <- get(df_name)
  if (nrow(df) > 0) {
    missing_cols <- setdiff(standard_cols, names(df))
    for (col in missing_cols) {
      df[[col]] <- NA
    }
    assign(df_name, df[, standard_cols])
  }
}

# Combine all data
combined <- rbind(
  ontology[, standard_cols],
  if (nrow(size_traits) > 0) size_traits[, standard_cols] else NULL,
  if (nrow(protection_traits) > 0) protection_traits[, standard_cols] else NULL
)

cat("Combined ontology:\n")
cat("  Total records:", nrow(combined), "\n")
cat("  Unique species:", length(unique(combined$taxon_name)), "\n\n")

# Save merged ontology
output_file <- "data/ontology_traits_enhanced.csv"
write.csv(combined, output_file, row.names = FALSE)

cat("✓ Enhanced ontology saved to:", output_file, "\n\n")

# Summary by trait category
cat("Trait coverage by category:\n")
trait_summary <- table(combined$trait_category)
for (category in names(trait_summary)) {
  cat(sprintf("  %-15s: %d records\n", category, trait_summary[category]))
}

cat("\nNext steps:\n")
cat("  1. Review", output_file, "\n")
cat("  2. If satisfied, replace original:\n")
cat("     file.rename('", output_file, "', '", ontology_file, "')\n")
cat("  3. Retrain ML models:\n")
cat("     source('scripts/train_trait_models.R')\n\n")

cat("=============================================================================\n")
