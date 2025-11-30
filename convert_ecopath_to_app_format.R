#!/usr/bin/env Rscript
#' Convert ECOPATH database exports to EcoNeTool format
#'
#' This script converts raw ECOPATH database tables to the format expected
#' by the 'Import ECOPATH CSV/Excel Exports' feature in the app

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Load required packages
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr", quiet = TRUE)
}
library(tidyr)

cat("============================================================\n")
cat("ECOPATH Database to EcoNeTool Format Converter\n")
cat("============================================================\n\n")

# ============================================================================
# STEP 1: Read the extracted database tables
# ============================================================================

cat("Step 1: Reading extracted database files...\n")

group_file <- "coast 2011-04-10 10.00_EcopathGroup.csv"
diet_file <- "coast 2011-04-10 10.00_EcopathDietComp.csv"

if (!file.exists(group_file) || !file.exists(diet_file)) {
  stop("Error: Extracted CSV files not found.\n",
       "Please run 'extract_ecopath_windows.R' first.")
}

ecopath_groups <- read.csv(group_file, stringsAsFactors = FALSE)
ecopath_diet <- read.csv(diet_file, stringsAsFactors = FALSE)

cat("  ✓ Loaded", nrow(ecopath_groups), "groups\n")
cat("  ✓ Loaded", nrow(ecopath_diet), "diet entries\n\n")


# ============================================================================
# STEP 2: Create Basic Estimates file
# ============================================================================

cat("Step 2: Creating Basic Estimates file...\n")

# Filter out Import/Export/Fleet groups (common in ECOPATH but not species)
valid_groups <- ecopath_groups[
  !grepl("^import$|^export$|^fleet", tolower(ecopath_groups$GroupName)) &
  ecopath_groups$GroupName != "" &
  !is.na(ecopath_groups$GroupName),
]

cat("  Filtered to", nrow(valid_groups), "valid groups\n")

# Create Basic Estimates in the format expected by the app
basic_estimates <- data.frame(
  Group = valid_groups$GroupName,
  Biomass = valid_groups$Biomass,
  `P/B` = valid_groups$ProdBiom,
  `Q/B` = valid_groups$ConsBiom,
  EE = valid_groups$EcoEfficiency,
  Production = valid_groups$Production,
  Consumption = valid_groups$Consumption,
  Unassimilated = valid_groups$Unassimilated,
  Respiration = valid_groups$Respiration,
  GroupID = valid_groups$GroupID,  # Keep for mapping
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Handle missing values (ECOPATH may have NAs)
basic_estimates$Biomass[is.na(basic_estimates$Biomass)] <- 1
basic_estimates$`P/B`[is.na(basic_estimates$`P/B`)] <- 0.5
basic_estimates$`Q/B`[is.na(basic_estimates$`Q/B`)] <- 1.5
basic_estimates$EE[is.na(basic_estimates$EE)] <- 0.5

cat("  ✓ Created Basic Estimates with", nrow(basic_estimates), "species\n")
cat("  Columns:", paste(colnames(basic_estimates), collapse = ", "), "\n\n")


# ============================================================================
# STEP 3: Create Diet Composition Matrix
# ============================================================================

cat("Step 3: Creating Diet Composition matrix...\n")

# Filter diet data to only include valid groups
valid_group_ids <- valid_groups$GroupID
ecopath_diet_filtered <- ecopath_diet[
  ecopath_diet$PredID %in% valid_group_ids &
  ecopath_diet$PreyID %in% valid_group_ids,
]

cat("  Filtered to", nrow(ecopath_diet_filtered), "valid diet entries\n")

# Create a mapping from GroupID to GroupName
id_to_name <- setNames(valid_groups$GroupName, valid_groups$GroupID)

# Add species names to diet data
ecopath_diet_filtered$Predator <- id_to_name[as.character(ecopath_diet_filtered$PredID)]
ecopath_diet_filtered$Prey <- id_to_name[as.character(ecopath_diet_filtered$PreyID)]

# Remove any entries where mapping failed
ecopath_diet_filtered <- ecopath_diet_filtered[
  !is.na(ecopath_diet_filtered$Predator) &
  !is.na(ecopath_diet_filtered$Prey),
]

# Convert long format to wide format matrix
# In ECOPATH: rows are prey, columns are predators
diet_matrix_long <- ecopath_diet_filtered[, c("Prey", "Predator", "Diet")]

# Pivot to wide format
diet_matrix <- pivot_wider(
  diet_matrix_long,
  names_from = Predator,
  values_from = Diet,
  values_fill = 0
)

# Convert to data frame with Prey as first column
diet_matrix <- as.data.frame(diet_matrix)

# Ensure all species are included (both as predators and prey)
all_species <- sort(unique(valid_groups$GroupName))

# Add missing prey rows (species that don't appear as prey)
missing_prey <- setdiff(all_species, diet_matrix$Prey)
if (length(missing_prey) > 0) {
  missing_df <- data.frame(Prey = missing_prey, stringsAsFactors = FALSE)
  for (col in setdiff(colnames(diet_matrix), "Prey")) {
    missing_df[[col]] <- 0
  }
  diet_matrix <- rbind(diet_matrix, missing_df)
}

# Add missing predator columns (species that don't appear as predators)
missing_pred <- setdiff(all_species, colnames(diet_matrix))
if (length(missing_pred) > 0) {
  for (pred in missing_pred) {
    diet_matrix[[pred]] <- 0
  }
}

# Sort rows and columns by species name for consistency
diet_matrix <- diet_matrix[order(diet_matrix$Prey), ]
prey_col <- diet_matrix$Prey
diet_matrix <- diet_matrix[, c("Prey", sort(setdiff(colnames(diet_matrix), "Prey")))]

cat("  ✓ Created diet matrix:", nrow(diet_matrix), "prey ×",
    ncol(diet_matrix) - 1, "predators\n\n")


# ============================================================================
# STEP 4: Save formatted files
# ============================================================================

cat("Step 4: Saving formatted files...\n")

basic_output <- "ECOPATH_Basic_Estimates.csv"
diet_output <- "ECOPATH_Diet_Matrix.csv"

# Remove GroupID column (not needed in final output)
basic_estimates_output <- basic_estimates[, !colnames(basic_estimates) %in% "GroupID"]

write.csv(basic_estimates_output, basic_output, row.names = FALSE)
write.csv(diet_matrix, diet_output, row.names = FALSE)

cat("  ✓ Saved:", basic_output, "\n")
cat("  ✓ Saved:", diet_output, "\n\n")


# ============================================================================
# SUMMARY
# ============================================================================

cat("============================================================\n")
cat("SUCCESS: Files ready for EcoNeTool import!\n")
cat("============================================================\n\n")

cat("Generated files:\n")
cat("  1.", basic_output, "\n")
cat("     - Contains:", nrow(basic_estimates_output), "species/groups\n")
cat("     - Columns: Group, Biomass, P/B, Q/B, EE, etc.\n\n")

cat("  2.", diet_output, "\n")
cat("     - Diet matrix:", nrow(diet_matrix), "×", ncol(diet_matrix) - 1, "\n")
cat("     - Format: Rows = Prey, Columns = Predators\n\n")

cat("HOW TO USE IN THE APP:\n")
cat("1. Go to the 'Input Food Web' tab\n")
cat("2. Find the 'Import ECOPATH CSV/Excel Exports' section\n")
cat("3. Upload these files:\n")
cat("   - Basic Estimates: ", basic_output, "\n")
cat("   - Diet Composition: ", diet_output, "\n")
cat("4. Click 'Import Exported Files'\n\n")

cat("============================================================\n")

# Display first few rows for verification
cat("\nPreview of Basic Estimates (first 5 rows):\n")
print(head(basic_estimates_output, 5))

cat("\nPreview of Diet Matrix (first 5 rows, first 6 columns):\n")
print(diet_matrix[1:min(5, nrow(diet_matrix)), 1:min(6, ncol(diet_matrix))])
