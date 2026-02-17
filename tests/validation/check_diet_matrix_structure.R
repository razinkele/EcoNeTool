# Check Diet Matrix Structure in Rpath
# ======================================

library(RODBC)
source("R/config.R")
source("R/functions/ecopath_import.R")
source("R/functions/rpath_integration.R")

cat("\n=== CHECKING DIET MATRIX STRUCTURE ===\n\n")

ecopath_data <- parse_ecopath_native_windows("examples/LTgoby.eweaccdb")
rpath_params <- convert_ecopath_to_rpath(ecopath_data, model_name = "Diet Test")
balanced_model <- run_ecopath_balance(rpath_params, balance = TRUE)

cat("\n[1] Diet Matrix Dimensions\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

DC <- balanced_model$DC
cat(sprintf("DC matrix dimensions: %d rows × %d columns\n", nrow(DC), ncol(DC)))
cat(sprintf("Number of groups: %d\n", balanced_model$NUM_GROUPS))

cat("\n[2] Check: What does Herring eat?\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

herring_idx <- which(balanced_model$Group == "Herring")
zooplankton_idx <- which(balanced_model$Group == "Zooplankton")

cat(sprintf("Herring index: %d\n", herring_idx))
cat(sprintf("Zooplankton index: %d\n", zooplankton_idx))

cat("\n--- Testing Hypothesis 1: DC[predator, prey] ---\n")
cat(sprintf("DC[Herring=%d, Zooplankton=%d] = %.3f\n",
            herring_idx, zooplankton_idx, DC[herring_idx, zooplankton_idx]))

cat("\n--- Testing Hypothesis 2: DC[prey, predator] ---\n")
cat(sprintf("DC[Zooplankton=%d, Herring=%d] = %.3f\n",
            zooplankton_idx, herring_idx, DC[zooplankton_idx, herring_idx]))

cat("\n[3] Herring's Full Diet (checking row)\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

herring_diet_row <- DC[herring_idx, ]
cat("Herring's diet (as predator = row):\n")
for (j in 1:length(herring_diet_row)) {
  if (!is.na(herring_diet_row[j]) && herring_diet_row[j] > 0) {
    prey_name <- balanced_model$Group[j]
    cat(sprintf("  → Eats: %s (%.1f%%)\n", prey_name, herring_diet_row[j] * 100))
  }
}

cat("\n[4] Herring's Full Diet (checking column)\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

herring_diet_col <- DC[, herring_idx]
cat("Herring's diet (as predator = column):\n")
for (i in 1:length(herring_diet_col)) {
  if (!is.na(herring_diet_col[i]) && herring_diet_col[i] > 0) {
    prey_name <- balanced_model$Group[i]
    cat(sprintf("  → Eats: %s (%.1f%%)\n", prey_name, herring_diet_col[i] * 100))
  }
}

cat("\n[5] Who Eats Herring? (checking as prey)\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

# Check column (Herring as prey)
predators_col <- DC[, herring_idx]
cat("Predators of Herring (from column):\n")
for (i in 1:length(predators_col)) {
  if (!is.na(predators_col[i]) && predators_col[i] > 0) {
    predator_name <- balanced_model$Group[i]
    cat(sprintf("  ← Eaten by: %s (%.1f%% of their diet)\n",
                predator_name, predators_col[i] * 100))
  }
}

# Check row (Herring as prey)
cat("\nPredators of Herring (from row):\n")
predators_row <- DC[herring_idx, ]
for (j in 1:length(predators_row)) {
  if (j != herring_idx && !is.na(predators_row[j]) && predators_row[j] > 0) {
    predator_name <- balanced_model$Group[j]
    cat(sprintf("  ← Eaten by: %s (%.1f%% of their diet)\n",
                predator_name, predators_row[j] * 100))
  }
}

cat("\n[6] CONCLUSION\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

# Test ecological sense
herring_eats_zoo_row <- DC[herring_idx, zooplankton_idx]
herring_eats_zoo_col <- DC[zooplankton_idx, herring_idx]

cat("Ecological test: Herring should eat Zooplankton\n\n")

if (!is.na(herring_eats_zoo_row) && herring_eats_zoo_row > 0.5) {
  cat(sprintf("✓ CORRECT: DC[herring, zooplankton] = %.3f (Herring eats Zooplankton)\n",
              herring_eats_zoo_row))
  cat("  → DC matrix format: DC[predator, prey]\n")
  cat("  → My TL algorithm was CORRECT\n")
} else if (!is.na(herring_eats_zoo_col) && herring_eats_zoo_col > 0.5) {
  cat(sprintf("✓ CORRECT: DC[zooplankton, herring] = %.3f (Herring eats Zooplankton)\n",
              herring_eats_zoo_col))
  cat("  → DC matrix format: DC[prey, predator]\n")
  cat("  → My TL algorithm was WRONG - need to transpose!\n")
} else {
  cat("✗ ERROR: Cannot find Herring eating Zooplankton in either direction!\n")
  cat(sprintf("  DC[herring=%d, zooplankton=%d] = %.3f\n",
              herring_idx, zooplankton_idx, herring_eats_zoo_row))
  cat(sprintf("  DC[zooplankton=%d, herring=%d] = %.3f\n",
              zooplankton_idx, herring_idx, herring_eats_zoo_col))
}

cat("\n=== CHECK COMPLETE ===\n\n")
