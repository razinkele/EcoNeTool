# ============================================================================
# Check Rpath Parameter Requirements
# ============================================================================

library(Rpath)

cat("\n=== RPATH PARAMETER REQUIREMENTS ===\n\n")

# Create a minimal params object to see structure
cat("[1] Creating Minimal Rpath Params\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

groups <- c("Phytoplankton", "Zooplankton", "Fish", "Detritus", "Fleet")
types <- c(1, 0, 0, 2, 3)

params <- create.rpath.params(group = groups, type = types)

cat("MODEL COLUMNS (what Rpath expects):\n")
for (col in names(params$model)) {
  cat(sprintf("  %-20s\n", col))
}

cat("\nDIET COLUMNS:\n")
cat(sprintf("  %d columns: Group + predator columns\n", ncol(params$diet)))

# Check what's in the model structure
cat("\n[2] MODEL PARAMETER DETAILS\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

model_cols <- names(params$model)

# Categorize
basic_cols <- c("Group", "Type", "Biomass", "PB", "QB", "EE")
production_cols <- c("ProdCons", "BioAcc", "Unassim")
detritus_cols <- c("DetInput", "Detritus")
fleet_cols <- grep("Fleet", model_cols, value = TRUE)
other_cols <- setdiff(model_cols, c(basic_cols, production_cols, detritus_cols, fleet_cols))

cat("BASIC (required for mass balance):\n")
for (col in basic_cols[basic_cols %in% model_cols]) {
  cat(sprintf("  %-20s\n", col))
}

cat("\nPRODUCTION MODIFIERS:\n")
for (col in production_cols[production_cols %in% model_cols]) {
  cat(sprintf("  %-20s\n", col))
}

cat("\nDETRITUS:\n")
for (col in detritus_cols[detritus_cols %in% model_cols]) {
  cat(sprintf("  %-20s\n", col))
}

cat("\nFLEET:\n")
for (col in fleet_cols) {
  cat(sprintf("  %-20s\n", col))
}

if (length(other_cols) > 0) {
  cat("\nOTHER:\n")
  for (col in other_cols) {
    cat(sprintf("  %-20s\n", col))
  }
}

# Now check what we're currently using
cat("\n[3] WHAT WE CURRENTLY PASS TO RPATH\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

source("R/config.R")
source("R/functions/ecopath_import.R")
source("R/functions/rpath_integration.R")

ecopath_data <- parse_ecopath_native_windows("examples/LTgoby.eweaccdb")
our_params <- convert_ecopath_to_rpath(ecopath_data, model_name = "LTgoby")

cat("OUR MODEL COLUMNS:\n")
for (col in names(our_params$model)) {
  cat(sprintf("  %-20s\n", col))
}

# Compare
cat("\n[4] COMPARISON: RPATH vs OUR CONVERSION\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

rpath_cols <- names(params$model)
our_cols <- names(our_params$model)

missing_in_ours <- setdiff(rpath_cols, our_cols)
extra_in_ours <- setdiff(our_cols, rpath_cols)

if (length(missing_in_ours) > 0) {
  cat("❌ MISSING (we should add these):\n")
  for (col in missing_in_ours) {
    cat(sprintf("  - %s\n", col))
  }
} else {
  cat("✅ We have all required Rpath columns\n")
}

if (length(extra_in_ours) > 0) {
  cat("\n➕ EXTRA (we have but Rpath base doesn't):\n")
  for (col in extra_in_ours) {
    cat(sprintf("  + %s\n", col))
  }
  cat("\nNote: Extra columns may be useful for analysis/export\n")
}

# Check what's available in EcopathGroup but not being used
cat("\n[5] AVAILABLE IN ECOPATH BUT NOT USED\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

ecopath_cols <- names(ecopath_data$group_data)

# Important columns we might want to add
potentially_useful <- c(
  "Immigration", "Emigration",  # Movement
  "Catch",  # Fishery
  "vbK", "Loo", "Winf", "t0",  # Growth (for multistanza)
  "QmQo", "CmCo",  # Temperature response
  "EmigRate",  # Migration rate
  "OtherMort"  # Additional mortality
)

cat("POTENTIALLY USEFUL COLUMNS:\n")
for (col in potentially_useful) {
  if (col %in% ecopath_cols) {
    # Check if it's being used
    in_our_params <- col %in% our_cols
    has_data <- sum(!is.na(ecopath_data$group_data[[col]])) > 0
    non_zero <- sum(ecopath_data$group_data[[col]] != 0, na.rm = TRUE) > 0

    status <- if (in_our_params) "✓ Used" else "✗ Not used"
    data_status <- if (has_data) {
      if (non_zero) sprintf("(has non-zero data)") else "(all zeros)"
    } else {
      "(all NA)"
    }

    cat(sprintf("  %-20s  %s  %s\n", col, status, data_status))
  }
}

cat("\n[6] CHECKING SEPARATE TABLES\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

# Check EcopathFleet table
library(RODBC)
con <- RODBC::odbcConnectAccess2007("examples/LTgoby.eweaccdb")

# Fleet table
if ("EcopathFleet" %in% RODBC::sqlTables(con)$TABLE_NAME) {
  fleet_data <- RODBC::sqlFetch(con, "EcopathFleet", stringsAsFactors = FALSE)
  cat(sprintf("EcopathFleet table: %d rows\n", nrow(fleet_data)))
  if (nrow(fleet_data) > 0) {
    cat("  Columns:", paste(names(fleet_data), collapse=", "), "\n")
    cat("  Fleets:", paste(fleet_data$FleetName, collapse=", "), "\n")
  }
} else {
  cat("No EcopathFleet table\n")
}

# Discard fate
if ("EcopathDiscardFate" %in% RODBC::sqlTables(con)$TABLE_NAME) {
  discard_data <- RODBC::sqlFetch(con, "EcopathDiscardFate", stringsAsFactors = FALSE)
  cat(sprintf("\nEcopathDiscardFate table: %d rows\n", nrow(discard_data)))
  if (nrow(discard_data) > 0) {
    cat("  Columns:", paste(names(discard_data), collapse=", "), "\n")
  }
} else {
  cat("\nNo EcopathDiscardFate table\n")
}

# Stanza
if ("Stanza" %in% RODBC::sqlTables(con)$TABLE_NAME) {
  stanza_data <- RODBC::sqlFetch(con, "Stanza", stringsAsFactors = FALSE)
  cat(sprintf("\nStanza table: %d rows\n", nrow(stanza_data)))
  if (nrow(stanza_data) > 0) {
    cat("  Columns:", paste(names(stanza_data), collapse=", "), "\n")
  }
} else {
  cat("\nNo Stanza table\n")
}

# Pedigree
if ("EcopathGroupPedigree" %in% RODBC::sqlTables(con)$TABLE_NAME) {
  pedigree_data <- RODBC::sqlFetch(con, "EcopathGroupPedigree", stringsAsFactors = FALSE)
  cat(sprintf("\nEcopathGroupPedigree table: %d rows\n", nrow(pedigree_data)))
  if (nrow(pedigree_data) > 0) {
    cat("  Columns:", paste(names(pedigree_data), collapse=", "), "\n")
    cat("  Variables tracked:", paste(unique(pedigree_data$VarName), collapse=", "), "\n")
  }
}

RODBC::odbcClose(con)

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("\n=== RECOMMENDATIONS ===\n\n")

cat("1. ✅ ALREADY WORKING:\n")
cat("   - Basic mass balance parameters (B, P/B, Q/B, EE)\n")
cat("   - Diet matrix\n")
cat("   - Production modifiers (BioAcc, Unassim)\n")
cat("   - Detritus (DetInput, DetFate)\n")
cat("   - Fleet structure (when present)\n\n")

cat("2. ➕ SHOULD ADD:\n")
cat("   - Immigration/Emigration (available in data)\n")
cat("   - Multistanza parameters (Stanza table)\n")
cat("   - Pedigree/calibration data (for confidence intervals)\n")
cat("   - Fleet details from EcopathFleet table\n")
cat("   - Discard fate information\n\n")

cat("3. 📊 DISPLAY ENHANCEMENTS:\n")
cat("   - Create calibration results table\n")
cat("   - Show pedigree/confidence levels\n")
cat("   - Display multistanza group structure\n")
cat("   - Show fleet catches and landings\n\n")
