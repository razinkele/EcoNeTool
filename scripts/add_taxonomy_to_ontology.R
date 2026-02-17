# =============================================================================
# Add Taxonomy to Ontology Database
# =============================================================================
#
# This script adds taxonomy columns (phylum, class, order, family, genus)
# to the ontology_traits.csv file by querying WoRMS for each unique species.
#
# Date: 2025-12-25
# Version: 1.0
#
# =============================================================================

cat("=============================================================================\n")
cat("ADD TAXONOMY TO ONTOLOGY DATABASE\n")
cat("=============================================================================\n\n")

# Load required functions
source("R/functions/trait_lookup.R")

# Read current ontology
ontology_file <- "data/ontology_traits.csv"
ontology <- read.csv(ontology_file, stringsAsFactors = FALSE)

cat("Current ontology:\n")
cat("  Records:", nrow(ontology), "\n")
cat("  Species:", length(unique(ontology$taxon_name)), "\n")
cat("  Columns:", paste(names(ontology), collapse = ", "), "\n\n")

# Get unique species
unique_species <- unique(ontology[, c("taxon_name", "aphia_id")])
cat("Unique species to lookup:", nrow(unique_species), "\n\n")

# Create taxonomy lookup table
taxonomy_table <- data.frame(
  taxon_name = character(),
  aphia_id = integer(),
  phylum = character(),
  class = character(),
  order = character(),
  family = character(),
  genus = character(),
  stringsAsFactors = FALSE
)

cat("Querying WoRMS for taxonomy...\n")
pb <- txtProgressBar(min = 0, max = nrow(unique_species), style = 3)

for (i in 1:nrow(unique_species)) {
  species_name <- unique_species$taxon_name[i]
  aphia_id <- unique_species$aphia_id[i]

  tryCatch({
    # Query WoRMS
    worms_result <- lookup_worms_traits(species_name)

    if (worms_result$success && !is.null(worms_result$traits)) {
      worms <- worms_result$traits

      taxonomy_table <- rbind(taxonomy_table, data.frame(
        taxon_name = species_name,
        aphia_id = aphia_id,
        phylum = worms$phylum %||% "",
        class = worms$class %||% "",
        order = worms$order %||% "",
        family = worms$family %||% "",
        genus = worms$genus %||% "",
        stringsAsFactors = FALSE
      ))
    } else {
      # Add empty row if lookup fails
      taxonomy_table <- rbind(taxonomy_table, data.frame(
        taxon_name = species_name,
        aphia_id = aphia_id,
        phylum = "",
        class = "",
        order = "",
        family = "",
        genus = "",
        stringsAsFactors = FALSE
      ))
    }

    # Rate limiting (be nice to WoRMS API)
    Sys.sleep(0.5)

  }, error = function(e) {
    # Skip species with errors
  })

  setTxtProgressBar(pb, i)
}
close(pb)

cat("\n\n✓ Taxonomy lookup complete\n")
cat("  Species with taxonomy:", sum(taxonomy_table$phylum != ""), "\n\n")

# Merge taxonomy into ontology
ontology_enhanced <- merge(ontology, taxonomy_table,
                           by = c("taxon_name", "aphia_id"),
                           all.x = TRUE)

# Reorder columns (taxonomy first, then traits)
col_order <- c("taxon_name", "aphia_id", "phylum", "class", "order",
               "family", "genus", setdiff(names(ontology_enhanced),
               c("taxon_name", "aphia_id", "phylum", "class", "order", "family", "genus")))
ontology_enhanced <- ontology_enhanced[, col_order]

# Save enhanced ontology
output_file <- "data/ontology_traits_with_taxonomy.csv"
write.csv(ontology_enhanced, output_file, row.names = FALSE)

cat("✓ Enhanced ontology saved to:", output_file, "\n")
cat("  Records:", nrow(ontology_enhanced), "\n")
cat("  Columns:", ncol(ontology_enhanced), "\n")
cat("  New columns: phylum, class, order, family, genus\n\n")

cat("Next steps:\n")
cat("  1. Review", output_file, "\n")
cat("  2. If satisfied, replace original:\n")
cat("     file.rename('", output_file, "', '", ontology_file, "')\n\n")

cat("=============================================================================\n")
