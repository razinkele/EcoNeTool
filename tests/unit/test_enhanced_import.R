# ============================================================================
# Test Enhanced ECOPATH Import with All New Tables
# ============================================================================

cat("\n=== TESTING ENHANCED ECOPATH IMPORT ===\n\n")

library(RODBC)

source("R/config.R")
source("R/functions/ecopath_import.R")

cat("[1] Import ECOPATH Database with All New Tables\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

ecopath_data <- parse_ecopath_native_windows("examples/LTgoby.eweaccdb")

cat("\n[2] Verify Import Results\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

cat("✓ BASIC DATA:\n")
cat(sprintf("  Group data: %d groups\n", nrow(ecopath_data$group_data)))
cat(sprintf("  Diet data: %d entries\n", nrow(ecopath_data$diet_data)))

cat("\n✓ MULTISTANZA DATA:\n")
if (!is.null(ecopath_data$stanza_params)) {
  cat(sprintf("  Stanza parameters: %d multistanza groups\n", nrow(ecopath_data$stanza_params)))
  cat("  Columns:", paste(colnames(ecopath_data$stanza_params), collapse=", "), "\n")

  # Show details
  if (nrow(ecopath_data$stanza_params) > 0) {
    cat("\n  Multistanza Group Details:\n")
    for (i in 1:nrow(ecopath_data$stanza_params)) {
      cat(sprintf("    - %s (ID: %d)\n",
                  ecopath_data$stanza_params$StanzaName[i],
                  ecopath_data$stanza_params$StanzaID[i]))
      cat(sprintf("      BABsplit: %.4f, WmatWinf: %.4f, RecPower: %.4f\n",
                  ecopath_data$stanza_params$BABsplit[i],
                  ecopath_data$stanza_params$WmatWinf[i],
                  ecopath_data$stanza_params$RecPower[i]))
    }
  }
} else {
  cat("  No multistanza parameters found\n")
}

cat("\n✓ FLEET DATA:\n")
if (!is.null(ecopath_data$fleet_data)) {
  cat(sprintf("  Fleet details: %d fleets\n", nrow(ecopath_data$fleet_data)))
  cat("  Columns:", paste(colnames(ecopath_data$fleet_data), collapse=", "), "\n")

  # Show details
  if (nrow(ecopath_data$fleet_data) > 0) {
    cat("\n  Fleet Details:\n")
    for (i in 1:nrow(ecopath_data$fleet_data)) {
      cat(sprintf("    - %s (ID: %d)\n",
                  ecopath_data$fleet_data$FleetName[i],
                  ecopath_data$fleet_data$FleetID[i]))
      if ("FixedCost" %in% colnames(ecopath_data$fleet_data)) {
        cat(sprintf("      Fixed Cost: %.2f, Variable Cost: %.2f\n",
                    ecopath_data$fleet_data$FixedCost[i],
                    ecopath_data$fleet_data$VariableCost[i]))
      }
    }
  }
} else {
  cat("  No fleet details found\n")
}

cat("\n✓ DISCARD FATE DATA:\n")
if (!is.null(ecopath_data$discard_fate)) {
  cat(sprintf("  Discard fate entries: %d\n", nrow(ecopath_data$discard_fate)))
  cat("  Columns:", paste(colnames(ecopath_data$discard_fate), collapse=", "), "\n")

  if (nrow(ecopath_data$discard_fate) > 0) {
    cat(sprintf("  Groups with discard fate: %d\n",
                length(unique(ecopath_data$discard_fate$GroupID))))
  }
} else {
  cat("  No discard fate data found\n")
}

cat("\n✓ PEDIGREE/CALIBRATION DATA:\n")
if (!is.null(ecopath_data$pedigree_data)) {
  cat(sprintf("  Pedigree entries: %d\n", nrow(ecopath_data$pedigree_data)))
  cat("  Columns:", paste(colnames(ecopath_data$pedigree_data), collapse=", "), "\n")

  if ("VarName" %in% colnames(ecopath_data$pedigree_data)) {
    unique_vars <- unique(ecopath_data$pedigree_data$VarName)
    cat(sprintf("  Variables tracked: %d\n", length(unique_vars)))
    cat("    -", paste(unique_vars, collapse="\n    - "), "\n")

    # Count pedigree levels by variable
    cat("\n  Pedigree Entries by Variable:\n")
    for (var in unique_vars) {
      count <- sum(ecopath_data$pedigree_data$VarName == var)
      cat(sprintf("    - %s: %d entries\n", var, count))
    }
  }
} else {
  cat("  No pedigree data found\n")
}

if (!is.null(ecopath_data$pedigree_levels)) {
  cat(sprintf("\n  Pedigree confidence levels: %d levels\n", nrow(ecopath_data$pedigree_levels)))
  cat("  Columns:", paste(colnames(ecopath_data$pedigree_levels), collapse=", "), "\n")

  # Show confidence levels if available
  if ("Description" %in% colnames(ecopath_data$pedigree_levels)) {
    cat("\n  Confidence Level Descriptions:\n")
    for (i in 1:nrow(ecopath_data$pedigree_levels)) {
      cat(sprintf("    Level %d: %s\n",
                  ecopath_data$pedigree_levels$LevelID[i],
                  ecopath_data$pedigree_levels$Description[i]))
    }
  }
} else {
  cat("  No pedigree confidence levels found\n")
}

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("\n=== ENHANCED IMPORT TEST COMPLETE ===\n\n")

cat("Summary:\n")
cat(sprintf("  ✓ Group data: %d groups\n", nrow(ecopath_data$group_data)))
cat(sprintf("  ✓ Diet data: %d entries\n", nrow(ecopath_data$diet_data)))
cat(sprintf("  ✓ Multistanza: %s\n",
            if (!is.null(ecopath_data$stanza_params))
              paste(nrow(ecopath_data$stanza_params), "groups")
            else "Not available"))
cat(sprintf("  ✓ Fleet data: %s\n",
            if (!is.null(ecopath_data$fleet_data))
              paste(nrow(ecopath_data$fleet_data), "fleets")
            else "Not available"))
cat(sprintf("  ✓ Discard fate: %s\n",
            if (!is.null(ecopath_data$discard_fate))
              paste(nrow(ecopath_data$discard_fate), "entries")
            else "Not available"))
cat(sprintf("  ✓ Pedigree data: %s\n",
            if (!is.null(ecopath_data$pedigree_data))
              paste(nrow(ecopath_data$pedigree_data), "entries")
            else "Not available"))
cat(sprintf("  ✓ Pedigree levels: %s\n",
            if (!is.null(ecopath_data$pedigree_levels))
              paste(nrow(ecopath_data$pedigree_levels), "levels")
            else "Not available"))

cat("\n✅ All new tables successfully imported!\n\n")
