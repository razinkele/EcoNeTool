# Check for complex circular feeding patterns

library(RODBC)
source("R/config.R")
source("R/functions/ecopath_import.R")
source("R/functions/rpath_integration.R")

cat("\n=== CHECKING FOR CIRCULAR FEEDING PATTERNS ===\n\n")

ecopath_data <- parse_ecopath_native_windows("examples/LTgoby.eweaccdb")
rpath_params <- convert_ecopath_to_rpath(ecopath_data, model_name = "CircularTest")

diet <- rpath_params$diet

# Create adjacency matrix for easier circular detection
n_groups <- nrow(diet) - 1  # -1 for Import row
group_names <- as.character(diet$Group[1:n_groups])

# Create matrix showing who eats whom (pred → prey)
adj_matrix <- matrix(0, nrow = n_groups, ncol = n_groups)
rownames(adj_matrix) <- group_names
colnames(adj_matrix) <- group_names

predator_cols <- names(diet)[names(diet) != "Group"]

for (i in 1:n_groups) {
  prey_name <- group_names[i]

  for (pred_name in predator_cols) {
    if (pred_name %in% group_names) {
      pred_idx <- which(group_names == pred_name)
      diet_val <- diet[[pred_name]][i]

      if (!is.na(diet_val) && diet_val > 0) {
        # pred_idx eats i
        adj_matrix[pred_idx, i] <- diet_val
      }
    }
  }
}

cat("[1] Diet Matrix as Adjacency Matrix\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

cat("Who eats whom (rows = predators, columns = prey):\n")
cat("(Only showing non-zero connections)\n\n")

for (i in 1:min(5, n_groups)) {
  prey_count <- sum(adj_matrix[i, ] > 0)
  if (prey_count > 0) {
    cat(sprintf("%s eats %d groups:\n", group_names[i], prey_count))
    prey_idx <- which(adj_matrix[i, ] > 0)
    for (j in prey_idx[1:min(5, length(prey_idx))]) {
      cat(sprintf("  → %s (%.1f%%)\n", group_names[j], adj_matrix[i, j] * 100))
    }
    if (length(prey_idx) > 5) cat(sprintf("  ... and %d more\n", length(prey_idx) - 5))
    cat("\n")
  }
}

# Check for 2-cycles (A eats B, B eats A)
cat("\n[2] Checking for 2-Cycles (A eats B, B eats A)\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

cycles_2_found <- FALSE
for (i in 1:(n_groups-1)) {
  for (j in (i+1):n_groups) {
    if (adj_matrix[i, j] > 0 && adj_matrix[j, i] > 0) {
      cycles_2_found <- TRUE
      cat(sprintf("🔄 2-CYCLE: '%s' ←→ '%s'\n",
                 group_names[i], group_names[j]))
      cat(sprintf("   %s eats %s: %.1f%%\n",
                 group_names[i], group_names[j], adj_matrix[i, j] * 100))
      cat(sprintf("   %s eats %s: %.1f%%\n\n",
                 group_names[j], group_names[i], adj_matrix[j, i] * 100))
    }
  }
}

if (!cycles_2_found) {
  cat("✓ No 2-cycles found\n")
}

# Check for 3-cycles (A eats B, B eats C, C eats A)
cat("\n[3] Checking for 3-Cycles (A eats B, B eats C, C eats A)\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

cycles_3_found <- FALSE
for (i in 1:min(10, n_groups)) {  # Limit to first 10 groups for speed
  for (j in 1:n_groups) {
    if (adj_matrix[i, j] > 0) {  # i eats j
      for (k in 1:n_groups) {
        if (adj_matrix[j, k] > 0 && adj_matrix[k, i] > 0) {  # j eats k, k eats i
          cycles_3_found <- TRUE
          cat(sprintf("🔄 3-CYCLE: '%s' → '%s' → '%s' → '%s'\n",
                     group_names[i], group_names[j], group_names[k], group_names[i]))
          cat(sprintf("   %s eats %s: %.1f%%\n",
                     group_names[i], group_names[j], adj_matrix[i, j] * 100))
          cat(sprintf("   %s eats %s: %.1f%%\n",
                     group_names[j], group_names[k], adj_matrix[j, k] * 100))
          cat(sprintf("   %s eats %s: %.1f%%\n\n",
                     group_names[k], group_names[i], adj_matrix[k, i] * 100))
        }
      }
    }
  }
}

if (!cycles_3_found) {
  cat("✓ No 3-cycles found (in first 10 groups checked)\n")
}

# Check specific problematic groups
cat("\n[4] Detailed Check: Phytoplankton\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

phyto_idx <- which(group_names == "Phytoplankton")

if (length(phyto_idx) > 0) {
  # What does phytoplankton eat?
  phyto_prey <- which(adj_matrix[phyto_idx, ] > 0)

  cat("Phytoplankton eats:\n")
  if (length(phyto_prey) == 0) {
    cat("  ✓ Nothing (correct - it's a producer)\n")
  } else {
    cat("  ⚠️  ERROR: Producers shouldn't eat anything!\n")
    for (prey in phyto_prey) {
      cat(sprintf("  → %s (%.1f%%)\n", group_names[prey], adj_matrix[phyto_idx, prey] * 100))
    }
  }

  # What eats phytoplankton?
  phyto_predators <- which(adj_matrix[, phyto_idx] > 0)

  cat("\nWhat eats phytoplankton:\n")
  if (length(phyto_predators) == 0) {
    cat("  ⚠️  Warning: Nothing eats phytoplankton!\n")
  } else {
    for (pred in phyto_predators) {
      cat(sprintf("  ← %s (%.1f%%)\n", group_names[pred], adj_matrix[pred, phyto_idx] * 100))
    }
  }

  # Check if any predators of phytoplankton are also producers
  cat("\nChecking if predators are also producers:\n")
  model_df <- rpath_params$model
  for (pred in phyto_predators) {
    pred_name <- group_names[pred]
    pred_type <- model_df$Type[which(model_df$Group == pred_name)]
    if (length(pred_type) > 0 && pred_type == 1) {
      cat(sprintf("  ⚠️  %s is a PRODUCER but eats phytoplankton!\n", pred_name))
    }
  }
}

cat("\n=== DIAGNOSIS COMPLETE ===\n\n")
cat("The TL calculation issue is likely due to:\n")
cat("  1. Circular feeding loops (even after removing self-feeding)\n")
cat("  2. Complex food web structure with many interconnections\n")
cat("  3. Rpath TL calculation algorithm limitations\n\n")
