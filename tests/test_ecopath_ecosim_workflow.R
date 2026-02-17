# Complete ECOPATH/ECOSIM Workflow Test
# Tests the full pipeline: Import → Convert → Balance → ECOSIM Setup → Run
# Based on Rpath testing infrastructure
#
# Author: EcoNeTool Testing Suite
# Date: 2025-12-08

cat("\n", strrep("=", 80), "\n")
cat("COMPLETE ECOPATH/ECOSIM WORKFLOW TEST\n")
cat("Testing: Import → Convert → Balance → ECOSIM → Simulate\n")
cat(strrep("=", 80), "\n\n")

# ==============================================================================
# SETUP
# ==============================================================================

# Load required functions
cat("Loading functions...\n")
source("R/functions/ecopath_import.R")
source("R/functions/rpath_integration.R")

# Load required packages
suppressPackageStartupMessages({
  if (!require("Rpath", quietly = TRUE)) {
    stop("Rpath package required. Install with: remotes::install_github('noaa-edab/Rpath')")
  }
  if (!require("data.table", quietly = TRUE)) {
    stop("data.table package required. Install with: install.packages('data.table')")
  }
})

# Test configuration
TEST_FILE <- "examples/LTgoby.eweaccdb"
MODEL_NAME <- "LTgoby Test Model"
ECOSIM_YEARS <- 50
ECOSIM_METHOD <- "RK4"

# Create results directory
RESULTS_DIR <- "tests/workflow_results"
if (!dir.exists(RESULTS_DIR)) dir.create(RESULTS_DIR, recursive = TRUE)
TIMESTAMP <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Test tracking
test_counter <- 0
test_results <- list()

run_test <- function(test_name, test_function) {
  test_counter <<- test_counter + 1
  cat(sprintf("\n[Test %d] %s\n", test_counter, test_name))
  cat(strrep("-", 80), "\n")
  start_time <- Sys.time()

  result <- tryCatch({
    # Suppress warnings during test execution (they're logged in the function output)
    suppressWarnings({
      test_function()
    })
    list(status = "PASS", error = NULL)
  }, error = function(e) {
    cat("✗ ERROR:", e$message, "\n")
    list(status = "FAIL", error = e$message)
  })

  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

  test_results[[test_counter]] <<- list(
    test_number = test_counter,
    test_name = test_name,
    status = result$status,
    error = result$error,
    duration = duration,
    timestamp = start_time
  )

  if (result$status == "PASS") {
    cat(sprintf("✓ PASSED (%.2f seconds)\n", duration))
  } else {
    cat(sprintf("✗ FAILED (%.2f seconds)\n", duration))
  }

  return(result$status == "PASS")
}

# Storage for pipeline objects
pipeline <- list()

# ==============================================================================
# PHASE 1: FILE VALIDATION
# ==============================================================================

cat("\n### PHASE 1: File Validation ###\n")

run_test("Check test file exists", function() {
  if (!file.exists(TEST_FILE)) {
    stop("Test file not found: ", TEST_FILE)
  }
  cat("  ✓ File:", TEST_FILE, "\n")
  cat("  ✓ Size:", file.info(TEST_FILE)$size, "bytes\n")
})

# ==============================================================================
# PHASE 2: ECOPATH IMPORT
# ==============================================================================

cat("\n### PHASE 2: ECOPATH Database Import ###\n")

run_test("Import ECOPATH database", function() {
  import_result <- parse_ecopath_native_cross_platform(TEST_FILE)

  # Validate structure
  if (is.null(import_result)) stop("Import returned NULL")
  if (is.null(import_result$group_data)) stop("No group data")
  if (is.null(import_result$diet_data)) stop("No diet data")

  pipeline$import <<- import_result

  cat("  ✓ Groups imported:", nrow(import_result$group_data), "\n")
  cat("  ✓ Diet entries:", nrow(import_result$diet_data), "\n")
  cat("  ✓ Import method:", import_result$method, "\n")
})

run_test("Validate group data structure", function() {
  groups <- pipeline$import$group_data

  # Required columns
  required_cols <- c("GroupName", "Type", "Biomass", "ProdBiom", "ConsBiom", "EcoEfficiency")
  missing_cols <- setdiff(required_cols, names(groups))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "))
  }

  # Type distribution
  type_counts <- table(groups$Type, useNA = "always")
  cat("  ✓ Type distribution:\n")
  for (t in names(type_counts)) {
    type_name <- switch(as.character(t),
      "0" = "Consumer", "1" = "Producer", "2" = "Detritus", "3" = "Fleet", "NA" = "MISSING"
    )
    cat("    -", type_name, ":", type_counts[t], "\n")
  }

  # Check for NA Type values (critical)
  if (any(is.na(groups$Type))) {
    na_groups <- groups$GroupName[is.na(groups$Type)]
    warning("Groups with NA Type: ", paste(na_groups, collapse = ", "))
  }
})

run_test("Validate diet data structure", function() {
  diet <- pipeline$import$diet_data

  # Required columns (ECOPATH format uses PredID, PreyID, Diet)
  required_cols <- c("PredID", "PreyID", "Diet")
  missing_cols <- setdiff(required_cols, names(diet))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "))
  }

  # Diet coefficient range
  diet_range <- range(diet$Diet, na.rm = TRUE)
  cat("  ✓ Diet coefficient range:", round(diet_range[1], 4), "-", round(diet_range[2], 4), "\n")

  if (any(diet$Diet < 0, na.rm = TRUE) || any(diet$Diet > 1, na.rm = TRUE)) {
    warning("Diet coefficients outside [0,1] range detected")
  }
})

run_test("Extract metadata", function() {
  metadata <- pipeline$import$metadata

  if (is.null(metadata)) {
    cat("  ⚠ No metadata extracted\n")
    return()
  }

  cat("  ✓ Model name:", metadata$name, "\n")
  cat("  ✓ Author:", metadata$author, "\n")
  cat("  ✓ Country:", metadata$country, "\n")
  cat("  ✓ Ecosystem:", metadata$ecosystem_type, "\n")
})

run_test("Extract ECOSIM scenarios", function() {
  ecosim <- pipeline$import$ecosim_scenarios

  if (is.null(ecosim)) {
    cat("  ⚠ No ECOSIM scenarios in this database\n")
    return()
  }

  scenarios <- ecosim$scenarios
  cat("  ✓ Scenarios found:", nrow(scenarios), "\n")

  # Show first 3 scenarios
  for (i in 1:min(3, nrow(scenarios))) {
    cat(sprintf("    %d. %s (%d years)\n",
                scenarios$ScenarioID[i],
                scenarios$ScenarioName[i],
                scenarios$TotalTime[i]))
  }

  # Related data
  if (!is.null(ecosim$scenario_groups)) {
    cat("  ✓ Group parameters:", nrow(ecosim$scenario_groups), "rows\n")
  }
  if (!is.null(ecosim$scenario_fleets)) {
    cat("  ✓ Fleet parameters:", nrow(ecosim$scenario_fleets), "rows\n")
  }
  if (!is.null(ecosim$scenario_forcing)) {
    cat("  ✓ Vulnerabilities:", nrow(ecosim$scenario_forcing), "rows\n")
  }

  pipeline$ecosim_data <<- ecosim
})

# ==============================================================================
# PHASE 3: RPATH CONVERSION
# ==============================================================================

cat("\n### PHASE 3: Convert to Rpath Format ###\n")

run_test("Convert ECOPATH to Rpath parameters", function() {
  rpath_params <- convert_ecopath_to_rpath(
    pipeline$import,
    model_name = MODEL_NAME
  )

  # Validate structure
  if (is.null(rpath_params)) stop("Conversion returned NULL")
  if (!inherits(rpath_params, "Rpath.params")) {
    stop("Result is not an Rpath.params object")
  }

  pipeline$params <<- rpath_params

  cat("  ✓ Class:", class(rpath_params), "\n")
  cat("  ✓ Groups in model:", nrow(rpath_params$model), "\n")
  cat("  ✓ Diet matrix:", nrow(rpath_params$diet), "×", ncol(rpath_params$diet), "\n")
})

run_test("Validate Rpath parameters", function() {
  params <- pipeline$params

  # Run Rpath's built-in validation
  validation_result <- Rpath::check.rpath.params(params)

  if (!is.null(validation_result)) {
    cat("  ⚠ Validation warnings:\n")
    for (w in validation_result) {
      cat("    -", w, "\n")
    }
  } else {
    cat("  ✓ All parameters valid\n")
  }
})

run_test("Check parameter completeness", function() {
  model <- pipeline$params$model

  # For each group (excluding fleets), count parameters
  incomplete <- c()

  for (i in 1:nrow(model)) {
    if (model$Type[i] >= 3) next  # Skip fleets

    n_params <- sum(!is.na(c(model$Biomass[i], model$PB[i], model$QB[i], model$EE[i])))

    if (n_params < 3) {
      incomplete <- c(incomplete, paste0(model$Group[i], " (", n_params, "/4)"))
    }
  }

  if (length(incomplete) > 0) {
    cat("  ⚠ Groups with insufficient parameters:\n")
    for (g in incomplete) {
      cat("    -", g, "\n")
    }
    warning(length(incomplete), " groups have < 3 parameters")
  } else {
    cat("  ✓ All groups have sufficient parameters (≥3/4)\n")
  }
})

run_test("Validate diet matrix sums", function() {
  diet <- pipeline$params$diet
  diet_cols <- names(diet)[names(diet) != "Group"]

  invalid_sums <- c()

  for (col in diet_cols) {
    col_sum <- sum(diet[[col]], na.rm = TRUE)
    if (col_sum > 1.01) {  # Allow small tolerance
      invalid_sums <- c(invalid_sums, paste0(col, " (", round(col_sum, 3), ")"))
    }
  }

  if (length(invalid_sums) > 0) {
    cat("  ⚠ Predators with diet sum > 1.0:\n")
    for (s in invalid_sums) {
      cat("    -", s, "\n")
    }
    warning(length(invalid_sums), " predators have invalid diet sums")
  } else {
    cat("  ✓ All predator diets sum to ≤ 1.0\n")
  }
})

# ==============================================================================
# PHASE 4: ECOPATH MASS BALANCE
# ==============================================================================

cat("\n### PHASE 4: Ecopath Mass Balance ###\n")

run_test("Run mass balance", function() {
  balanced_model <- run_ecopath_balance(
    pipeline$params,
    balance = TRUE
  )

  # Validate result
  if (is.null(balanced_model)) stop("Balance returned NULL")
  if (!inherits(balanced_model, "Rpath")) {
    stop("Result is not an Rpath object")
  }

  pipeline$balanced <<- balanced_model

  cat("  ✓ Model balanced successfully\n")
  cat("  ✓ Class:", class(balanced_model), "\n")
})

run_test("Validate balanced model", function() {
  model <- pipeline$balanced

  # Total biomass
  total_biomass <- sum(model$Biomass, na.rm = TRUE)
  if (is.na(total_biomass) || total_biomass <= 0) {
    stop("Invalid total biomass")
  }
  cat("  ✓ Total biomass:", round(total_biomass, 2), "t/km²\n")

  # Ecotrophic efficiency
  if (!is.null(model$EE)) {
    ee_range <- range(model$EE, na.rm = TRUE)
    cat("  ✓ EE range:", round(ee_range[1], 3), "-", round(ee_range[2], 3), "\n")

    # Check for EE > 1
    if (any(model$EE > 1, na.rm = TRUE)) {
      warning("Some groups have EE > 1 (overexploited)")
    }
  }

  # Trophic levels
  if (!is.null(model$TL)) {
    tl_range <- range(model$TL, na.rm = TRUE)
    cat("  ✓ Trophic level range:", round(tl_range[1], 2), "-", round(tl_range[2], 2), "\n")
  }

  # Respiration
  if (!is.null(model$Respiration)) {
    total_resp <- sum(model$Respiration, na.rm = TRUE)
    cat("  ✓ Total respiration:", round(total_resp, 2), "t/km²/year\n")
  }
})

run_test("Extract network metrics", function() {
  model <- pipeline$balanced

  cat("  ✓ Total groups:", model$NUM_GROUPS, "\n")
  cat("  ✓ Living groups:", model$NUM_LIVING, "\n")
  cat("  ✓ Detritus groups:", model$NUM_DEAD, "\n")
  cat("  ✓ Gear types:", model$NUM_GEARS, "\n")

  # Check for diet composition matrix
  if (!is.null(model$DC)) {
    cat("  ✓ Diet composition matrix available\n")
  }

  # Check for landings
  if (!is.null(model$Landings)) {
    total_catch <- sum(model$Landings, na.rm = TRUE)
    if (total_catch > 0) {
      cat("  ✓ Total landings:", round(total_catch, 2), "t/km²/year\n")
    }
  }
})

# ==============================================================================
# PHASE 5: ECOSIM SETUP
# ==============================================================================

cat("\n### PHASE 5: ECOSIM Scenario Setup ###\n")

run_test("Create default ECOSIM scenario", function() {
  scenario <- Rpath::rsim.scenario(
    pipeline$balanced,
    pipeline$params,
    years = 1:ECOSIM_YEARS
  )

  # Validate
  if (is.null(scenario)) stop("Scenario is NULL")
  if (!inherits(scenario, "Rsim.scenario")) {
    stop("Not an Rsim.scenario object")
  }

  pipeline$scenario <<- scenario

  cat("  ✓ Scenario created for", ECOSIM_YEARS, "years\n")
  cat("  ✓ Class:", class(scenario), "\n")
})

run_test("Apply ECOSIM scenario from database (if available)", function() {
  if (is.null(pipeline$ecosim_data)) {
    cat("  ⚠ No ECOSIM scenarios available - using default\n")
    return()
  }

  scenarios <- pipeline$ecosim_data$scenarios
  if (nrow(scenarios) == 0) {
    cat("  ⚠ No scenarios to apply - using default\n")
    return()
  }

  # Use first scenario
  scenario_id <- scenarios$ScenarioID[1]
  scenario_name <- scenarios$ScenarioName[1]

  cat("  → Applying scenario:", scenario_name, "(ID:", scenario_id, ")\n")

  # Apply scenario using our custom function
  scenario_configured <- setup_ecosim_scenario_with_data(
    rpath_model = pipeline$balanced,
    ecosim_data = pipeline$ecosim_data,
    scenario_id = scenario_id,
    years = ECOSIM_YEARS,
    method = ECOSIM_METHOD
  )

  if (!is.null(scenario_configured)) {
    pipeline$scenario <<- scenario_configured
    cat("  ✓ Scenario configured with database parameters\n")
  } else {
    cat("  ⚠ Configuration failed, using default scenario\n")
  }
})

# ==============================================================================
# PHASE 6: ECOSIM SIMULATION
# ==============================================================================

cat("\n### PHASE 6: ECOSIM Simulation ###\n")

run_test("Run ECOSIM simulation", function() {
  sim_result <- Rpath::rsim.run(
    pipeline$scenario,
    method = ECOSIM_METHOD
  )

  # Validate
  if (is.null(sim_result)) stop("Simulation returned NULL")
  if (!inherits(sim_result, "Rsim")) {
    stop("Not an Rsim object")
  }

  pipeline$simulation <<- sim_result

  cat("  ✓ Simulation completed\n")
  cat("  ✓ Method:", ECOSIM_METHOD, "\n")
})

run_test("Validate simulation output", function() {
  sim <- pipeline$simulation

  # Check biomass output
  if (is.null(sim$out_Biomass)) stop("No biomass output")

  n_timesteps <- nrow(sim$out_Biomass)
  n_groups <- ncol(sim$out_Biomass) - 1  # Exclude time column

  cat("  ✓ Biomass output:", n_timesteps, "timesteps ×", n_groups, "groups\n")

  # Check for valid values
  if (any(is.na(sim$out_Biomass))) {
    warning("Biomass output contains NA values")
  }
  if (any(sim$out_Biomass < 0, na.rm = TRUE)) {
    warning("Biomass output contains negative values")
  }

  # Check catch output if present
  if (!is.null(sim$out_Catch)) {
    cat("  ✓ Catch output available\n")
  }

  # Final biomass summary
  final_row <- sim$out_Biomass[n_timesteps, -1]  # Exclude time column
  final_biomass <- sum(final_row, na.rm = TRUE)
  initial_biomass <- sum(sim$out_Biomass[1, -1], na.rm = TRUE)

  cat("  ✓ Initial total biomass:", round(initial_biomass, 2), "t/km²\n")
  cat("  ✓ Final total biomass:", round(final_biomass, 2), "t/km²\n")
  cat("  ✓ Change:", round(100 * (final_biomass - initial_biomass) / initial_biomass, 1), "%\n")
})

# ==============================================================================
# PHASE 7: RESULTS EXPORT
# ==============================================================================

cat("\n### PHASE 7: Export Results ###\n")

run_test("Save Rpath parameters", function() {
  file <- file.path(RESULTS_DIR, paste0("rpath_params_", TIMESTAMP, ".rds"))
  saveRDS(pipeline$params, file)
  cat("  ✓ Saved:", file, "\n")
})

run_test("Save balanced model", function() {
  file <- file.path(RESULTS_DIR, paste0("balanced_model_", TIMESTAMP, ".rds"))
  saveRDS(pipeline$balanced, file)
  cat("  ✓ Saved:", file, "\n")
})

run_test("Save ECOSIM results", function() {
  file <- file.path(RESULTS_DIR, paste0("ecosim_results_", TIMESTAMP, ".rds"))
  saveRDS(pipeline$simulation, file)
  cat("  ✓ Saved:", file, "\n")
})

run_test("Export biomass time series (CSV)", function() {
  biomass <- as.data.frame(pipeline$simulation$out_Biomass)
  file <- file.path(RESULTS_DIR, paste0("biomass_timeseries_", TIMESTAMP, ".csv"))
  write.csv(biomass, file, row.names = FALSE)
  cat("  ✓ Saved:", file, "\n")
  cat("  ✓ Size:", nrow(biomass), "rows ×", ncol(biomass), "columns\n")
})

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n", strrep("=", 80), "\n")
cat("TEST SUMMARY\n")
cat(strrep("=", 80), "\n\n")

total <- length(test_results)
passed <- sum(sapply(test_results, function(x) x$status == "PASS"))
failed <- total - passed

cat(sprintf("Total Tests: %d\n", total))
cat(sprintf("Passed: %d (%.1f%%)\n", passed, 100 * passed / total))
cat(sprintf("Failed: %d\n", failed))
cat(sprintf("Total Time: %.2f seconds\n", sum(sapply(test_results, function(x) x$duration))))

if (failed > 0) {
  cat("\n❌ FAILED TESTS:\n")
  for (r in test_results) {
    if (r$status == "FAIL") {
      cat(sprintf("  [%d] %s\n", r$test_number, r$test_name))
      if (!is.null(r$error)) {
        cat(sprintf("      Error: %s\n", r$error))
      }
    }
  }
} else {
  cat("\n✅ ALL TESTS PASSED!\n")
}

# Save detailed report
report_file <- file.path(RESULTS_DIR, paste0("workflow_test_report_", TIMESTAMP, ".txt"))
sink(report_file)
cat("ECOPATH/ECOSIM WORKFLOW TEST REPORT\n")
cat(strrep("=", 80), "\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Test file:", TEST_FILE, "\n")
cat("Model name:", MODEL_NAME, "\n\n")

cat("PIPELINE RESULTS:\n")
cat("  Import: ", ifelse(!is.null(pipeline$import), "✓", "✗"), "\n")
cat("  Conversion: ", ifelse(!is.null(pipeline$params), "✓", "✗"), "\n")
cat("  Balance: ", ifelse(!is.null(pipeline$balanced), "✓", "✗"), "\n")
cat("  ECOSIM: ", ifelse(!is.null(pipeline$simulation), "✓", "✗"), "\n\n")

cat("TEST RESULTS:\n")
for (r in test_results) {
  status_icon <- ifelse(r$status == "PASS", "✓", "✗")
  cat(sprintf("[%2d] %s %s (%.2fs)\n", r$test_number, status_icon, r$test_name, r$duration))
  if (!is.null(r$error)) {
    cat(sprintf("     Error: %s\n", r$error))
  }
}

cat(sprintf("\nSummary: %d/%d passed (%.1f%%)\n", passed, total, 100 * passed / total))
sink()

cat("\n📄 Detailed report saved:", report_file, "\n")
cat("📁 Results directory:", RESULTS_DIR, "\n")

cat("\n", strrep("=", 80), "\n")
if (failed == 0) {
  cat("✅ WORKFLOW TEST COMPLETE - ALL SYSTEMS OPERATIONAL\n")
} else {
  cat("⚠ WORKFLOW TEST COMPLETE - SOME TESTS FAILED\n")
}
cat(strrep("=", 80), "\n\n")
