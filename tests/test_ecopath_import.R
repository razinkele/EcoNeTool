# Test ECOPATH Import Fixes
# Verifies functional group classification and body mass extraction

cat("================================================================================\n")
cat("Testing ECOPATH Import with Coastal Model\n")
cat("================================================================================\n\n")

# Source required functions
source("R/functions/functional_group_utils.R")

# Load RODBC for database access
if (!require(RODBC, quietly = TRUE)) {
  stop("RODBC package required for ECOPATH import")
}

# Test file
ecopath_file <- "coast 2011-04-10 10.00.ewemdb"

if (!file.exists(ecopath_file)) {
  stop("Coastal model file not found: ", ecopath_file)
}

cat("Loading ECOPATH database:", ecopath_file, "\n\n")

# Connect to database
con <- tryCatch({
  odbcConnectAccess2007(ecopath_file)
}, error = function(e) {
  odbcConnectAccess(ecopath_file)
})

if (con == -1) {
  stop("Failed to connect to ECOPATH database")
}

# Read group table
group_table <- sqlFetch(con, "EcopathGroup")
odbcClose(con)

cat("Database loaded successfully\n")
cat("Number of groups:", nrow(group_table), "\n\n")

# Get column names
col_names <- tolower(names(group_table))
col_names_orig <- names(group_table)

cat("================================================================================\n")
cat("Column Detection\n")
cat("================================================================================\n\n")

# Detect GroupName column
name_col <- which(grepl("group.*name|^name$|groupname", col_names))[1]
cat("Group Name Column:", if (!is.na(name_col)) col_names_orig[name_col] else "NOT FOUND", "\n")

# Detect body mass column
bodymass_col <- which(grepl("individual.*weight|indiv.*wt|body.*mass|^weight$|^mass$|^bodymass$|^indwt$", col_names))[1]
cat("Body Mass Column:", if (!is.na(bodymass_col)) col_names_orig[bodymass_col] else "NOT FOUND", "\n\n")

# Extract species names
if (is.na(name_col)) {
  stop("Cannot find group name column")
}
species_names <- as.character(group_table[[name_col]])

# Extract body mass values if available
bodymass_values_raw <- if (!is.na(bodymass_col)) {
  as.numeric(group_table[[bodymass_col]])
} else {
  NULL
}

cat("================================================================================\n")
cat("Functional Group Classification\n")
cat("================================================================================\n\n")

# Assign functional groups
functional_groups <- sapply(species_names, assign_functional_group)

# Create results data frame
results <- data.frame(
  Species = species_names,
  FunctionalGroup = functional_groups,
  stringsAsFactors = FALSE
)

# Add body mass if available
if (!is.null(bodymass_values_raw)) {
  results$BodyMass_Raw <- bodymass_values_raw
  results$BodyMass_Valid <- bodymass_values_raw
  results$BodyMass_Valid[results$BodyMass_Valid < -9000] <- NA
  results$BodyMass_Valid[results$BodyMass_Valid <= 0] <- NA
}

# Print classification results
cat("Classification Results:\n")
cat("────────────────────────────────────────────────────────────────────────────\n")
for (i in 1:nrow(results)) {
  cat(sprintf("%-40s → %-15s",
              substr(results$Species[i], 1, 40),
              results$FunctionalGroup[i]))
  if (!is.null(bodymass_values_raw)) {
    if (!is.na(results$BodyMass_Valid[i])) {
      cat(sprintf("  (%.4f g)", results$BodyMass_Valid[i]))
    } else {
      cat("  (no mass data)")
    }
  }
  cat("\n")
}
cat("────────────────────────────────────────────────────────────────────────────\n\n")

cat("================================================================================\n")
cat("Validation Tests\n")
cat("================================================================================\n\n")

# Test 1: Macrozoobenthos groups should be Benthos
macrobent_indices <- grep("Macrozoo", results$Species, ignore.case = TRUE)
if (length(macrobent_indices) > 0) {
  macrobent_groups <- results$FunctionalGroup[macrobent_indices]
  macrobent_correct <- all(macrobent_groups == "Benthos")

  cat("Test 1: Macrozoobenthos Classification\n")
  cat("  Found:", length(macrobent_indices), "Macrozoobenthos groups\n")
  for (idx in macrobent_indices) {
    status <- if (results$FunctionalGroup[idx] == "Benthos") "✓" else "✗"
    cat(sprintf("  %s %-40s → %s\n",
                status,
                substr(results$Species[idx], 1, 40),
                results$FunctionalGroup[idx]))
  }
  cat("  Result:", if (macrobent_correct) "✓ PASS" else "✗ FAIL", "\n\n")
} else {
  cat("Test 1: No Macrozoobenthos groups found\n\n")
}

# Test 2: Specific misclassification fixes
cat("Test 2: Specific Classification Fixes\n")

# Check Mesozooplankton
mesozoo_idx <- grep("Mesozooplankton", results$Species, ignore.case = TRUE)
if (length(mesozoo_idx) > 0) {
  for (idx in mesozoo_idx) {
    status <- if (results$FunctionalGroup[idx] == "Zooplankton") "✓" else "✗"
    cat(sprintf("  %s %-40s → %s (expected: Zooplankton)\n",
                status,
                substr(results$Species[idx], 1, 40),
                results$FunctionalGroup[idx]))
  }
}

# Check Cormorans
cormoran_idx <- grep("cormoran", results$Species, ignore.case = TRUE)
if (length(cormoran_idx) > 0) {
  for (idx in cormoran_idx) {
    status <- if (results$FunctionalGroup[idx] == "Birds") "✓" else "✗"
    cat(sprintf("  %s %-40s → %s (expected: Birds)\n",
                status,
                substr(results$Species[idx], 1, 40),
                results$FunctionalGroup[idx]))
  }
}
cat("\n")

# Test 3: Fish species should be Fish
fish_species <- c("Vimba", "Ruffe", "Stickleback", "Pike-perch", "Perch", "Pike", "Roach", "Bream")
fish_found <- species_names %in% fish_species
if (any(fish_found)) {
  fish_indices <- which(fish_found)
  fish_groups <- results$FunctionalGroup[fish_indices]
  fish_correct <- all(fish_groups == "Fish")

  cat("Test 2: Fish Species Classification\n")
  cat("  Found:", length(fish_indices), "fish species\n")
  for (idx in fish_indices) {
    status <- if (results$FunctionalGroup[idx] == "Fish") "✓" else "✗"
    cat(sprintf("  %s %-40s → %s\n",
                status,
                substr(results$Species[idx], 1, 40),
                results$FunctionalGroup[idx]))
  }
  cat("  Result:", if (fish_correct) "✓ PASS" else "✗ FAIL", "\n\n")
} else {
  cat("Test 3: No fish species found in test list\n\n")
}

# Test 4: Body mass data extraction
if (!is.null(bodymass_values_raw)) {
  n_total <- length(bodymass_values_raw)
  n_valid <- sum(!is.na(results$BodyMass_Valid))
  n_missing <- n_total - n_valid

  cat("Test 4: Body Mass Data Extraction\n")
  cat("  Total groups:", n_total, "\n")
  cat("  With valid body mass:", n_valid, sprintf("(%.1f%%)\n", 100 * n_valid / n_total))
  cat("  Missing/invalid:", n_missing, sprintf("(%.1f%%)\n", 100 * n_missing / n_total))

  if (n_valid > 0) {
    cat("  Body mass range:", sprintf("%.6f - %.2f g\n",
                                      min(results$BodyMass_Valid, na.rm = TRUE),
                                      max(results$BodyMass_Valid, na.rm = TRUE)))
  }

  # Check if Benthos groups have varying body masses
  benthos_indices <- which(results$FunctionalGroup == "Benthos")
  if (length(benthos_indices) > 0) {
    benthos_masses <- results$BodyMass_Valid[benthos_indices]
    benthos_masses <- benthos_masses[!is.na(benthos_masses)]

    if (length(benthos_masses) > 1) {
      n_unique <- length(unique(benthos_masses))
      cat("  Benthos body masses: ", length(benthos_masses), " groups, ",
          n_unique, " unique values\n")
      cat("  Result:", if (n_unique > 1) "✓ PASS - Body masses vary" else "✗ FAIL - All same value", "\n\n")
    }
  }
} else {
  cat("Test 4: No body mass column found in database\n")
  cat("  Result: ⚠ WARNING - Will use functional group estimates\n\n")
}

cat("================================================================================\n")
cat("Summary Statistics\n")
cat("================================================================================\n\n")

# Count by functional group
fg_counts <- table(results$FunctionalGroup)
cat("Functional Groups:\n")
for (fg in sort(names(fg_counts))) {
  cat(sprintf("  %-20s: %3d groups\n", fg, fg_counts[fg]))
}
cat("\n")

cat("================================================================================\n")
cat("Test Complete\n")
cat("================================================================================\n")
