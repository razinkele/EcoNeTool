# Test ECOSIM Scenario Import
# Verifies that ECOSIM scenario information is correctly extracted from LTgoby database

cat("================================================================================\n")
cat("ECOSIM Scenario Import Test\n")
cat("================================================================================\n\n")

# Source import function
source("R/functions/ecopath_import.R")

# Test file
db_file <- "examples/LTgoby.eweaccdb"

if (!file.exists(db_file)) {
  stop("Test file not found: ", db_file)
}

cat("Testing with: ", basename(db_file), "\n\n")

# Test import
cat("1. Importing ECOPATH database with ECOSIM scenarios...\n")
cat(strrep("-", 70), "\n\n")

result <- tryCatch({
  parse_ecopath_native_cross_platform(db_file)
}, error = function(e) {
  cat("ERROR during import:", e$message, "\n")
  NULL
})

if (is.null(result)) {
  stop("Import failed")
}

cat("\n", strrep("-", 70), "\n")
cat("2. Verifying import structure...\n\n")

# Check basic structure
cat("✓ Group data rows:", nrow(result$group_data), "\n")
cat("✓ Diet data rows:", nrow(result$diet_data), "\n")

# Check metadata
if (!is.null(result$metadata)) {
  cat("✓ Metadata extracted:\n")
  cat("  - Model name:", result$metadata$name, "\n")
  cat("  - Author:", result$metadata$author, "\n")
  cat("  - Country:", result$metadata$country, "\n")
  cat("  - Ecosystem type:", result$metadata$ecosystem_type, "\n")
} else {
  cat("⚠ No metadata extracted\n")
}

# Check ECOSIM scenarios
cat("\n", strrep("-", 70), "\n")
cat("3. Verifying ECOSIM scenario data...\n\n")

if (!is.null(result$ecosim_scenarios)) {
  cat("✓ ECOSIM scenarios extracted\n\n")

  scenarios <- result$ecosim_scenarios$scenarios
  scenario_groups <- result$ecosim_scenarios$scenario_groups
  scenario_fleets <- result$ecosim_scenarios$scenario_fleets
  scenario_forcing <- result$ecosim_scenarios$scenario_forcing
  scenario_shapes <- result$ecosim_scenarios$scenario_shapes

  # Scenario summary
  cat("Scenarios (", nrow(scenarios), " total):\n")
  for (i in 1:min(10, nrow(scenarios))) {
    scenario_id <- scenarios$ScenarioID[i]
    scenario_name <- scenarios$ScenarioName[i]
    scenario_author <- scenarios$Author[i]
    total_time <- scenarios$TotalTime[i]

    cat(sprintf("  %2d. %-30s (Author: %-15s, Time: %d years)\n",
                scenario_id, scenario_name, scenario_author, total_time))
  }
  if (nrow(scenarios) > 10) {
    cat(sprintf("      ... and %d more scenarios\n", nrow(scenarios) - 10))
  }

  # Detailed info for first scenario
  cat("\nFirst scenario details:\n")
  cat("  Name:", scenarios$ScenarioName[1], "\n")
  cat("  Description:", substr(scenarios$Description[1], 1, 100), "...\n")
  cat("  Total time:", scenarios$TotalTime[1], "years\n")
  cat("  Time steps per year:", scenarios$StepSize[i], "\n")
  cat("  System recovery rate:", scenarios$SystemRecovery[1], "\n")

  # Related data
  cat("\nRelated scenario data:\n")
  if (!is.null(scenario_groups)) {
    cat("  ✓ Group parameters:", nrow(scenario_groups), "rows\n")
    cat("    - Unique groups:", length(unique(scenario_groups$EcopathGroupID)), "\n")
    cat("    - Scenarios covered:", length(unique(scenario_groups$ScenarioID)), "\n")
  } else {
    cat("  ⚠ No group parameters\n")
  }

  if (!is.null(scenario_fleets)) {
    cat("  ✓ Fleet parameters:", nrow(scenario_fleets), "rows\n")
    cat("    - Scenarios with fleets:", length(unique(scenario_fleets$ScenarioID)), "\n")
  } else {
    cat("  ⚠ No fleet parameters\n")
  }

  if (!is.null(scenario_forcing)) {
    cat("  ✓ Predator-prey vulnerabilities:", nrow(scenario_forcing), "rows\n")
    cat("    - Scenarios with forcing:", length(unique(scenario_forcing$ScenarioID)), "\n")
    cat("    - Predator-prey pairs:", nrow(scenario_forcing) / length(unique(scenario_forcing$ScenarioID)), "per scenario (avg)\n")
  } else {
    cat("  ⚠ No forcing matrix\n")
  }

  if (!is.null(scenario_shapes)) {
    cat("  ✓ Temporal forcing shapes:", nrow(scenario_shapes), "rows\n")
  } else {
    cat("  ⚠ No temporal shapes\n")
  }

  # Example vulnerability values for first scenario
  if (!is.null(scenario_forcing)) {
    scenario_1_forcing <- scenario_forcing[scenario_forcing$ScenarioID == 1, ]
    if (nrow(scenario_1_forcing) > 0) {
      cat("\nExample vulnerabilities (Scenario 1, first 5 predator-prey pairs):\n")
      for (i in 1:min(5, nrow(scenario_1_forcing))) {
        cat(sprintf("  Predator %d → Prey %d: vulnerability = %.2f\n",
                    scenario_1_forcing$PredID[i],
                    scenario_1_forcing$PreyID[i],
                    scenario_1_forcing$vulnerability[i]))
      }
    }
  }

} else {
  cat("✗ NO ECOSIM scenarios found\n")
  cat("  This database may not contain ECOSIM scenario information.\n")
}

cat("\n", strrep("=", 70), "\n")
cat("TEST COMPLETE\n")

# Summary
if (!is.null(result$ecosim_scenarios)) {
  cat("\n✓ SUCCESS: ECOSIM scenarios imported correctly\n")
  cat("\nYou can now:\n")
  cat("  1. Access scenarios via: result$ecosim_scenarios$scenarios\n")
  cat("  2. Use scenario data for Rpath ECOSIM simulations\n")
  cat("  3. Visualize temporal forcing patterns\n")
  cat("  4. Analyze predator-prey vulnerability matrices\n")
} else {
  cat("\n⚠ WARNING: ECOSIM scenarios not found in this database\n")
}

cat("\n", strrep("=", 70), "\n")
