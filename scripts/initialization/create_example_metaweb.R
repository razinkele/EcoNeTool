# create_example_metaweb.R
# Create example metaweb from existing Baltic food web data
# This demonstrates the metaweb data structure and serves as a template

library(igraph)

# Load existing Baltic food web data
load("BalticFW.Rdata")

# ============================================================================
# Convert existing network to metaweb format
# ============================================================================

cat("Creating example metaweb from Baltic food web...\n")

# Extract species information
species_data <- data.frame(
  species_id = paste0("SP", sprintf("%03d", 1:vcount(net))),
  species_name = V(net)$name,
  functional_group = as.character(info$fg),
  biomass = info$meanB,
  body_mass = info$bodymasses,
  metabolic_type = info$met.types,
  efficiency = info$efficiencies,
  stringsAsFactors = FALSE
)

# Extract interactions from network
edgelist <- as_edgelist(net, names = FALSE)
interactions_data <- data.frame(
  predator_id = species_data$species_id[edgelist[, 1]],
  prey_id = species_data$species_id[edgelist[, 2]],
  quality_code = 1,  # Assume all documented (since from published study)
  source = "Frelat & Kortsch (2020) Baltic Food Web Tutorial",
  notes = "Original data from Gulf of Riga, 1979-2016",
  stringsAsFactors = FALSE
)

# Create metadata
metadata <- list(
  region = "Baltic Sea - Gulf of Riga",
  time_period = "1979-2016",
  citation = "Frelat, R., & Kortsch, S. (2020). Baltic Food Web Tutorial",
  url = "https://github.com/rfrelat/BalticFoodWeb",
  created_by = "EcoNeTool conversion",
  notes = "Converted from BalticFW.Rdata for demonstration purposes"
)

# Source the metaweb functions
source("functions.R")

# Create metaweb object
baltic_metaweb <- create_metaweb(
  species = species_data,
  interactions = interactions_data,
  metadata = metadata
)

# Validate metaweb
cat("\nValidating metaweb...\n")
validate_metaweb(baltic_metaweb)

# Print summary
cat("\n")
print(baltic_metaweb)

# Display link quality summary
cat("\nLink quality summary:\n")
quality_summary <- summarize_link_quality(baltic_metaweb)
print(quality_summary)

# ============================================================================
# Save metaweb to metawebs library
# ============================================================================

# Save as RDS
output_file <- "metawebs/baltic/baltic_kortsch2021.rds"
saveRDS(baltic_metaweb, output_file)
cat("\nMetaweb saved to:", output_file, "\n")

# Also export as CSV for reference
export_metaweb_csv(
  baltic_metaweb,
  "metawebs/baltic/baltic_kortsch2021_species.csv",
  "metawebs/baltic/baltic_kortsch2021_interactions.csv"
)

# ============================================================================
# Test loading the metaweb
# ============================================================================

cat("\n--- Testing metaweb loading ---\n")

# Test RDS import
test_metaweb <- readRDS(output_file)
cat("\nLoaded metaweb from RDS:\n")
print(test_metaweb)

# Test CSV import
test_metaweb_csv <- import_metaweb_csv(
  "metawebs/baltic/baltic_kortsch2021_species.csv",
  "metawebs/baltic/baltic_kortsch2021_interactions.csv",
  metadata = list(region = "Baltic Sea", source = "CSV import test")
)
cat("\nLoaded metaweb from CSV:\n")
print(test_metaweb_csv)

# Test conversion to igraph
test_network <- metaweb_to_igraph(test_metaweb)
cat("\nConverted to igraph:\n")
cat("Vertices:", vcount(test_network), "\n")
cat("Edges:", ecount(test_network), "\n")

# ============================================================================
# Create template CSV files
# ============================================================================

cat("\n--- Creating template CSV files ---\n")

# Minimal example for documentation
template_species <- data.frame(
  species_id = c("SP001", "SP002", "SP003", "SP004", "SP005"),
  species_name = c(
    "Gadus morhua",
    "Clupea harengus",
    "Mytilus edulis",
    "Calanus finmarchicus",
    "Phytoplankton"
  ),
  functional_group = c("Fish", "Fish", "Benthos", "Zooplankton", "Phytoplankton"),
  stringsAsFactors = FALSE
)

template_interactions <- data.frame(
  predator_id = c("SP001", "SP001", "SP002", "SP003", "SP004"),
  prey_id = c("SP002", "SP003", "SP004", "SP004", "SP005"),
  quality_code = c(1, 1, 2, 1, 1),
  source = c(
    "doi:10.1111/example1",
    "doi:10.1111/example2",
    "Similar species documented",
    "doi:10.1111/example3",
    "doi:10.1111/example4"
  ),
  notes = c(
    "Gut content analysis",
    "Observed predation",
    "Inferred from similar herring species",
    "Laboratory feeding experiments",
    "Known grazer"
  ),
  stringsAsFactors = FALSE
)

write.csv(template_species, "metawebs/species_template.csv", row.names = FALSE)
write.csv(template_interactions, "metawebs/interactions_template.csv", row.names = FALSE)

cat("Template CSV files created:\n")
cat("  metawebs/species_template.csv\n")
cat("  metawebs/interactions_template.csv\n")

# ============================================================================
# Demonstrate metaweb manipulation
# ============================================================================

cat("\n--- Demonstrating metaweb manipulation ---\n")

# Make a copy to modify
demo_metaweb <- baltic_metaweb

# Add a new species
cat("\n1. Adding new species 'Pusa hispida' (Ringed seal)...\n")
demo_metaweb <- add_species_to_metaweb(
  demo_metaweb,
  species_id = "SP999",
  species_name = "Pusa hispida",
  functional_group = "Mammal",
  traits = list(body_mass = 50000, trophic_level = 4.5)
)

# Add trophic links for the new species
cat("2. Adding trophic links for Ringed seal...\n")
# Seals eat herring (assuming SP002 is herring)
demo_metaweb <- add_trophic_link(
  demo_metaweb,
  predator_id = "SP999",
  prey_id = "SP002",  # Adjust based on actual species IDs
  quality_code = 2,
  source = "Expert opinion",
  notes = "Ringed seals are known to feed on herring in Baltic Sea"
)

# Display updated summary
cat("\nUpdated metaweb:\n")
print(demo_metaweb)

# Test merging metawebs
cat("\n3. Testing metaweb merging...\n")

# Create a simple second metaweb
species2 <- data.frame(
  species_id = c("SP998", "SP997"),
  species_name = c("Halichoerus grypus", "Salmo salar"),
  functional_group = c("Mammal", "Fish"),
  stringsAsFactors = FALSE
)

interactions2 <- data.frame(
  predator_id = c("SP998"),
  prey_id = c("SP997"),
  quality_code = 1,
  source = "Literature",
  stringsAsFactors = FALSE
)

metaweb2 <- create_metaweb(
  species2,
  interactions2,
  list(region = "Baltic Sea - Additional species")
)

merged_metaweb <- merge_metawebs(demo_metaweb, metaweb2, conflict_resolution = "keep_higher_quality")

cat("\nMerged metaweb:\n")
print(merged_metaweb)

cat("\n✓ Example metaweb creation and manipulation complete!\n")
cat("\nNext steps:\n")
cat("1. Download published metawebs from literature sources\n")
cat("2. Convert to standard format using this script as template\n")
cat("3. Populate metawebs/ directory with regional metawebs\n")
cat("4. Test loading in Shiny app UI\n")
