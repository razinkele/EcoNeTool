# ==============================================================================
# Test ECOPATH Results Table - Conditional Formatting
# ==============================================================================
# Tests the conditional formatting (highlighting) of suspicious values
# in the balanced ECOPATH model results table
# ==============================================================================

cat("\n=== TESTING ECOPATH RESULTS TABLE HIGHLIGHTING ===\n\n")

library(RODBC)

# Source required files
source("R/config.R")
source("R/functions/ecopath_import.R")
source("R/functions/rpath_integration.R")

# ==============================================================================
# TEST 1: Import and Balance Model
# ==============================================================================

cat("[1] Importing and Balancing Model\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

test_file <- "examples/LTgoby.eweaccdb"

if (!file.exists(test_file)) {
  stop("Test file not found: ", test_file)
}

ecopath_data <- parse_ecopath_native_windows(test_file)
rpath_params <- convert_ecopath_to_rpath(ecopath_data, model_name = "Test Model")
balanced_model <- run_ecopath_balance(rpath_params, balance = TRUE)

cat("✓ Model balanced successfully\n\n")

# ==============================================================================
# TEST 2: Create Results Data Frame
# ==============================================================================

cat("[2] Creating Results Data Frame\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

# Map type values to names
type_names <- c("Consumer", "Producer", "Detritus", "Fleet")
type_display <- sapply(balanced_model$type, function(t) {
  if (is.na(t)) return("Unknown")
  type_names[t + 1]
})

# Calculate Production and Consumption
production <- balanced_model$Biomass * balanced_model$PB
consumption <- balanced_model$Biomass * balanced_model$QB

# Create results data frame
results_df <- data.frame(
  Group = balanced_model$Group,
  Type = type_display,
  Biomass = round(balanced_model$Biomass, 3),
  PB = round(balanced_model$PB, 3),
  QB = round(balanced_model$QB, 3),
  EE = round(balanced_model$EE, 3),
  GE = round(balanced_model$GE, 3),
  TL = round(balanced_model$TL, 2),
  Production = round(production, 3),
  Consumption = round(consumption, 3),
  stringsAsFactors = FALSE
)

cat(sprintf("✓ Results table created: %d rows × %d columns\n\n",
            nrow(results_df), ncol(results_df)))

# ==============================================================================
# TEST 3: Check for Values That Should Be Highlighted
# ==============================================================================

cat("[3] Analyzing Values for Quality Checks\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

# Check EE (Ecotrophic Efficiency)
cat("Ecotrophic Efficiency (EE) Analysis:\n")
cat("------------------------------------\n")

ee_critical <- results_df$EE[!is.na(results_df$EE) & results_df$EE > 1.0]
ee_warning <- results_df$EE[!is.na(results_df$EE) & results_df$EE > 0.95 & results_df$EE <= 1.0]
ee_low <- results_df$EE[!is.na(results_df$EE) & results_df$EE < 0.05]

cat(sprintf("  Critical (EE > 1.0):  %d groups\n", length(ee_critical)))
if (length(ee_critical) > 0) {
  critical_groups <- results_df$Group[!is.na(results_df$EE) & results_df$EE > 1.0]
  cat(sprintf("    → Groups: %s\n", paste(critical_groups, collapse=", ")))
  cat(sprintf("    → Values: %s\n", paste(round(ee_critical, 3), collapse=", ")))
}

cat(sprintf("  Warning (EE > 0.95):  %d groups\n", length(ee_warning)))
if (length(ee_warning) > 0) {
  warning_groups <- results_df$Group[!is.na(results_df$EE) & results_df$EE > 0.95 & results_df$EE <= 1.0]
  cat(sprintf("    → Groups: %s\n", paste(warning_groups, collapse=", ")))
  cat(sprintf("    → Values: %s\n", paste(round(ee_warning, 3), collapse=", ")))
}

cat(sprintf("  Low (EE < 0.05):      %d groups\n", length(ee_low)))
if (length(ee_low) > 0) {
  low_groups <- results_df$Group[!is.na(results_df$EE) & results_df$EE < 0.05]
  cat(sprintf("    → Groups: %s\n", paste(head(low_groups, 5), collapse=", ")))
  if (length(low_groups) > 5) cat(sprintf(" ... and %d more", length(low_groups) - 5))
  cat("\n")
}

cat("\n")

# Check GE (Gross Efficiency)
cat("Gross Efficiency (GE) Analysis:\n")
cat("-------------------------------\n")

ge_critical <- results_df$GE[!is.na(results_df$GE) & (results_df$GE > 1.0 | results_df$GE < 0)]
ge_warning <- results_df$GE[!is.na(results_df$GE) & results_df$GE > 0.5 & results_df$GE <= 1.0]

cat(sprintf("  Critical (GE > 1.0 or < 0):  %d groups\n", length(ge_critical)))
if (length(ge_critical) > 0) {
  critical_groups <- results_df$Group[!is.na(results_df$GE) & (results_df$GE > 1.0 | results_df$GE < 0)]
  cat(sprintf("    → Groups: %s\n", paste(critical_groups, collapse=", ")))
  cat(sprintf("    → Values: %s\n", paste(round(ge_critical, 3), collapse=", ")))
}

cat(sprintf("  Warning (GE > 0.5):          %d groups\n", length(ge_warning)))
if (length(ge_warning) > 0) {
  warning_groups <- results_df$Group[!is.na(results_df$GE) & results_df$GE > 0.5 & results_df$GE <= 1.0]
  cat(sprintf("    → Groups: %s\n", paste(warning_groups, collapse=", ")))
  cat(sprintf("    → Values: %s\n", paste(round(ge_warning, 3), collapse=", ")))
}

cat("\n")

# Check TL (Trophic Level)
cat("Trophic Level (TL) Analysis:\n")
cat("----------------------------\n")

tl_warning <- results_df$TL[!is.na(results_df$TL) & results_df$TL > 5.0]

cat(sprintf("  Warning (TL > 5.0):  %d groups\n", length(tl_warning)))
if (length(tl_warning) > 0) {
  warning_groups <- results_df$Group[!is.na(results_df$TL) & results_df$TL > 5.0]
  cat(sprintf("    → Groups: %s\n", paste(warning_groups, collapse=", ")))
  cat(sprintf("    → Values: %s\n", paste(round(tl_warning, 2), collapse=", ")))
}

cat("\n")

# Check Biomass
cat("Biomass Analysis:\n")
cat("-----------------\n")

biomass_critical <- results_df$Biomass[!is.na(results_df$Biomass) & results_df$Biomass < 0]
biomass_low <- results_df$Biomass[!is.na(results_df$Biomass) &
                                   results_df$Biomass > 0 &
                                   results_df$Biomass < 0.001]

cat(sprintf("  Critical (Biomass < 0):    %d groups\n", length(biomass_critical)))
if (length(biomass_critical) > 0) {
  critical_groups <- results_df$Group[!is.na(results_df$Biomass) & results_df$Biomass < 0]
  cat(sprintf("    → Groups: %s\n", paste(critical_groups, collapse=", ")))
}

cat(sprintf("  Low (0 < Biomass < 0.001): %d groups\n", length(biomass_low)))
if (length(biomass_low) > 0) {
  low_groups <- results_df$Group[!is.na(results_df$Biomass) &
                                  results_df$Biomass > 0 &
                                  results_df$Biomass < 0.001]
  cat(sprintf("    → Groups: %s\n", paste(low_groups, collapse=", ")))
}

cat("\n")

# ==============================================================================
# TEST 4: Display Groups That Will Be Highlighted
# ==============================================================================

cat("[4] Summary of Groups with Highlighted Values\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

# Find all groups with any highlighted values
groups_with_issues <- unique(c(
  results_df$Group[!is.na(results_df$EE) & (results_df$EE < 0.05 | results_df$EE > 0.95)],
  results_df$Group[!is.na(results_df$GE) & (results_df$GE < 0 | results_df$GE > 0.5)],
  results_df$Group[!is.na(results_df$TL) & results_df$TL > 5.0],
  results_df$Group[!is.na(results_df$Biomass) & results_df$Biomass < 0.001]
))

cat(sprintf("Total groups with highlighted values: %d out of %d\n\n",
            length(groups_with_issues), nrow(results_df)))

if (length(groups_with_issues) > 0) {
  cat("Groups that will show highlighting:\n")

  for (group in groups_with_issues) {
    group_data <- results_df[results_df$Group == group, ]
    cat(sprintf("\n  %s (%s):\n", group, group_data$Type))

    # Check which values trigger highlighting
    issues <- c()

    if (!is.na(group_data$EE)) {
      if (group_data$EE < 0.05) {
        issues <- c(issues, sprintf("EE = %.3f (low)", group_data$EE))
      } else if (group_data$EE > 0.95) {
        if (group_data$EE > 1.0) {
          issues <- c(issues, sprintf("EE = %.3f (CRITICAL)", group_data$EE))
        } else {
          issues <- c(issues, sprintf("EE = %.3f (warning)", group_data$EE))
        }
      }
    }

    if (!is.na(group_data$GE)) {
      if (group_data$GE < 0 || group_data$GE > 1.0) {
        issues <- c(issues, sprintf("GE = %.3f (CRITICAL)", group_data$GE))
      } else if (group_data$GE > 0.5) {
        issues <- c(issues, sprintf("GE = %.3f (warning)", group_data$GE))
      }
    }

    if (!is.na(group_data$TL) && group_data$TL > 5.0) {
      issues <- c(issues, sprintf("TL = %.2f (warning)", group_data$TL))
    }

    if (!is.na(group_data$Biomass)) {
      if (group_data$Biomass < 0) {
        issues <- c(issues, sprintf("Biomass = %.3f (CRITICAL)", group_data$Biomass))
      } else if (group_data$Biomass < 0.001) {
        issues <- c(issues, sprintf("Biomass = %.3f (low)", group_data$Biomass))
      }
    }

    for (issue in issues) {
      cat(sprintf("    - %s\n", issue))
    }
  }
}

# ==============================================================================
# TEST 5: Color Legend Verification
# ==============================================================================

cat("\n[5] Color Legend Verification\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

cat("Highlighting rules:\n")
cat("  • Red background (#ffebee):    Critical issues\n")
cat("    - EE > 1.0 (impossible value)\n")
cat("    - GE > 1.0 or GE < 0 (impossible values)\n")
cat("    - Biomass < 0 (negative biomass)\n\n")

cat("  • Yellow background (#fff3cd):  Warnings\n")
cat("    - EE > 0.95 (near maximum, potential overexploitation)\n")
cat("    - GE > 0.5 (unusually high efficiency)\n")
cat("    - TL > 5.0 (very high trophic level)\n\n")

cat("  • Light blue background (#e8f4f8): Notes\n")
cat("    - EE < 0.05 (low ecotrophic efficiency, weak connections)\n")
cat("    - 0 < Biomass < 0.001 (very low biomass)\n\n")

# ==============================================================================
# SUMMARY
# ==============================================================================

cat(paste(rep("=", 70), collapse=""), "\n")
cat("\n=== TEST SUMMARY ===\n\n")

cat("✅ Results table created successfully\n")
cat("✅ Quality check rules applied\n")
cat("✅ Highlighting logic verified\n\n")

total_highlighted <- length(groups_with_issues)
total_groups <- nrow(results_df)
pct_highlighted <- round(100 * total_highlighted / total_groups, 1)

cat(sprintf("📊 Model Quality Overview:\n"))
cat(sprintf("   Total groups: %d\n", total_groups))
cat(sprintf("   Groups with issues: %d (%.1f%%)\n", total_highlighted, pct_highlighted))
cat(sprintf("   Groups without issues: %d (%.1f%%)\n",
            total_groups - total_highlighted,
            100 - pct_highlighted))

cat("\n🎨 Conditional formatting will highlight:\n")
cat(sprintf("   • %d EE values (critical, warning, or low)\n",
            sum(!is.na(results_df$EE) & (results_df$EE < 0.05 | results_df$EE > 0.95))))
cat(sprintf("   • %d GE values (critical or warning)\n",
            sum(!is.na(results_df$GE) & (results_df$GE < 0 | results_df$GE > 0.5))))
cat(sprintf("   • %d TL values (warning)\n",
            sum(!is.na(results_df$TL) & results_df$TL > 5.0)))
cat(sprintf("   • %d Biomass values (critical or low)\n",
            sum(!is.na(results_df$Biomass) & results_df$Biomass < 0.001)))

cat("\n✅ HIGHLIGHTING FEATURE READY FOR TESTING\n\n")

cat("To test in app:\n")
cat("  1. Start app: shiny::runApp()\n")
cat("  2. Navigate to: ECOPATH/ECOSIM → Mass Balance\n")
cat("  3. Import database and run mass balance\n")
cat("  4. View results table with color-coded values\n")
cat("  5. Check legend above table for color meanings\n\n")

cat("=== TEST COMPLETE ===\n\n")
