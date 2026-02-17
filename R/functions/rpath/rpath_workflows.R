# ==============================================================================
# RPATH COMPLETE WORKFLOWS
# ==============================================================================
# End-to-end pipelines from ECOPATH database import through Ecosim simulation
#
# Features:
#   - Complete workflow: Import → Convert → Balance → MTI → Simulate
#   - Automatic step-by-step execution
#   - Comprehensive results collection
#   - Usage examples and templates
#
# ==============================================================================

# ==============================================================================
# COMPLETE WORKFLOW: ECOPATH DATABASE → ECOSIM SIMULATION
# ==============================================================================

run_complete_rpath_workflow <- function(ecopath_db_file,
                                        years = 50,
                                        run_mti = TRUE,
                                        run_simulation = TRUE) {
  #' Complete Rpath Workflow: Import → Balance → Simulate
  #'
  #' End-to-end pipeline from ECOPATH database to Ecosim simulation
  #'
  #' @param ecopath_db_file Path to .ewemdb file
  #' @param years Ecosim simulation duration
  #' @param run_mti Calculate Mixed Trophic Impacts
  #' @param run_simulation Run Ecosim simulation
  #' @return List with all results
  #' @export

  message("================================================================================")
  message("RPATH COMPLETE WORKFLOW")
  message("================================================================================\n")

  results <- list()

  # Step 1: Import ECOPATH database
  message("STEP 1: Import ECOPATH database")
  message("--------------------------------------------------------------------------------")

  if (!file.exists(ecopath_db_file)) {
    stop("Database file not found: ", ecopath_db_file)
  }

  # Use cross-platform import function
  # Note: parse_ecopath_native_cross_platform() should be sourced from ecopath/ directory
  ecopath_data <- parse_ecopath_native_cross_platform(ecopath_db_file)
  results$ecopath_import <- ecopath_data

  # Step 2: Convert to Rpath format
  message("\nSTEP 2: Convert to Rpath format")
  message("--------------------------------------------------------------------------------")

  rpath_params <- convert_ecopath_to_rpath(ecopath_data)
  results$rpath_params <- rpath_params

  # Step 3: Run Ecopath mass-balance
  message("\nSTEP 3: Run Ecopath mass-balance model")
  message("--------------------------------------------------------------------------------")

  rpath_model <- run_ecopath_balance(rpath_params, balance = TRUE)
  results$ecopath_model <- rpath_model

  # Step 4: Mixed Trophic Impact analysis
  if (run_mti) {
    message("\nSTEP 4: Calculate Mixed Trophic Impacts")
    message("--------------------------------------------------------------------------------")

    mti <- calculate_rpath_mti(rpath_params, rpath_model)
    results$mti <- mti
  }

  # Step 5: Ecosim simulation
  if (run_simulation) {
    message("\nSTEP 5: Run Ecosim dynamic simulation")
    message("--------------------------------------------------------------------------------")

    scenario <- setup_ecosim_scenario(rpath_model, years = years)
    sim_results <- run_ecosim_simulation(scenario)
    results$ecosim <- sim_results
  }

  message("\n================================================================================")
  message("WORKFLOW COMPLETE ✓")
  message("================================================================================\n")

  return(results)
}

# ==============================================================================
# USAGE EXAMPLES
# ==============================================================================

# Example 1: Basic workflow
# --------------------------
# Complete end-to-end workflow from database to simulation
#
# results <- run_complete_rpath_workflow("coast 2011-04-10 10.00.ewemdb", years = 50)
#
# # Access results:
# ecopath_data <- results$ecopath_import
# rpath_params <- results$rpath_params
# rpath_model <- results$ecopath_model
# mti_matrix <- results$mti
# sim_results <- results$ecosim

# Example 2: Import and balance only
# ------------------------------------
# Skip simulation, just get balanced model
#
# # Load ecopath import functions
# source("R/functions/ecopath/load_all.R")
#
# # Import database
# ecopath_data <- parse_ecopath_native_cross_platform("your-file.ewemdb")
#
# # Convert to Rpath format
# rpath_params <- convert_ecopath_to_rpath(ecopath_data)
#
# # Run mass-balance
# model <- run_ecopath_balance(rpath_params)
#
# # Inspect results
# print(model$TL)  # Trophic levels
# print(model$EE)  # Ecotrophic efficiencies

# Example 3: Run Ecosim simulation
# ----------------------------------
# Run simulation with custom parameters
#
# # Setup scenario (50 years)
# scenario <- setup_ecosim_scenario(model, years = 50)
#
# # Run simulation
# sim <- run_ecosim_simulation(scenario)
#
# # Plot results
# plot_ecosim_results(sim, type = "biomass")
#
# # Extract specific group biomass
# cod_biomass <- extract_ecosim_biomass(sim, group_name = "Cod")
# plot(cod_biomass$time, cod_biomass$Cod, type = "l",
#      xlab = "Year", ylab = "Biomass (tons/km²)",
#      main = "Cod Biomass Over Time")

# Example 4: MTI analysis
# ------------------------
# Analyze trophic impacts
#
# # Calculate MTI matrix
# mti <- calculate_rpath_mti(rpath_params, model)
#
# # Visualize with heatmap
# heatmap(mti,
#         main = "Mixed Trophic Impacts",
#         xlab = "Impacting Group",
#         ylab = "Impacted Group",
#         scale = "none",
#         col = colorRampPalette(c("red", "white", "blue"))(100))
#
# # Find strongest impacts
# max_positive <- which(mti == max(mti, na.rm = TRUE), arr.ind = TRUE)
# max_negative <- which(mti == min(mti, na.rm = TRUE), arr.ind = TRUE)

# Example 5: Fishing scenario evaluation
# ----------------------------------------
# Compare baseline vs. reduced fishing
#
# # Evaluate 50% reduction in fishing effort
# fishing_sim <- evaluate_fishing_scenario(
#   rpath_model = model,
#   fleet_name = "Trawl",
#   effort_multiplier = 0.5,  # 50% of current effort
#   years = 50
# )
#
# # Compare baseline vs. scenario
# baseline_biomass <- extract_ecosim_biomass(fishing_sim$baseline)
# scenario_biomass <- extract_ecosim_biomass(fishing_sim$scenario)
#
# # Plot comparison for specific species
# plot(baseline_biomass$time, baseline_biomass$Cod, type = "l", col = "black",
#      ylim = range(c(baseline_biomass$Cod, scenario_biomass$Cod)),
#      xlab = "Year", ylab = "Cod Biomass (tons/km²)",
#      main = "Impact of Fishing Reduction on Cod")
# lines(scenario_biomass$time, scenario_biomass$Cod, col = "blue")
# legend("topright", legend = c("Baseline", "50% Reduction"),
#        col = c("black", "blue"), lty = 1)

# Example 6: Sensitivity analysis
# ---------------------------------
# Test parameter uncertainty
#
# # Test sensitivity to Q/B ratio
# sensitivity <- run_sensitivity_analysis(
#   rpath_model = model,
#   parameter = "QB",  # Q/B ratio
#   variation = 0.1,   # ±10%
#   n_sims = 100
# )
#
# # Extract results
# biomass_values <- sapply(sensitivity, function(x) x$biomass)
# throughput_values <- sapply(sensitivity, function(x) x$throughput)
#
# # Plot distribution
# par(mfrow = c(1, 2))
# hist(biomass_values, main = "Total Biomass Sensitivity",
#      xlab = "Total Biomass (tons/km²)", col = "lightblue")
# hist(throughput_values, main = "System Throughput Sensitivity",
#      xlab = "Throughput (tons/km²/year)", col = "lightgreen")

# Example 7: Using imported ECOSIM scenarios
# --------------------------------------------
# Setup simulation with pre-configured ECOSIM parameters
#
# # Import database with ECOSIM scenarios
# ecopath_data <- parse_ecopath_native_cross_platform("model-with-scenarios.ewemdb")
#
# # Convert and balance
# rpath_params <- convert_ecopath_to_rpath(ecopath_data)
# rpath_model <- run_ecopath_balance(rpath_params)
#
# # Get available scenarios
# print(ecopath_data$ecosim_scenarios$scenarios)
#
# # Setup scenario 1
# scenario <- setup_ecosim_scenario_with_data(
#   rpath_model = rpath_model,
#   ecosim_data = ecopath_data$ecosim_scenarios,
#   scenario_id = 1,
#   years = 50,
#   method = "RK4"
# )
#
# # Run simulation
# sim_results <- run_ecosim_simulation(scenario)

# ==============================================================================
# WORKFLOW TEMPLATES
# ==============================================================================

# Template 1: Rapid Assessment
# ------------------------------
# Quick model assessment without simulation
#
# rapid_assessment <- function(db_file) {
#   # Import
#   ecopath_data <- parse_ecopath_native_cross_platform(db_file)
#
#   # Convert
#   rpath_params <- convert_ecopath_to_rpath(ecopath_data)
#
#   # Balance
#   model <- run_ecopath_balance(rpath_params)
#
#   # Quick metrics
#   list(
#     n_groups = length(model$Group),
#     total_biomass = sum(model$Biomass, na.rm = TRUE),
#     mean_tl = mean(model$TL, na.rm = TRUE),
#     max_tl = max(model$TL, na.rm = TRUE),
#     system_throughput = sum(model$Q, na.rm = TRUE)
#   )
# }

# Template 2: Scenario Comparison
# ---------------------------------
# Compare multiple management scenarios
#
# compare_scenarios <- function(model, scenarios, years = 50) {
#   results <- list()
#
#   for (i in seq_along(scenarios)) {
#     scenario_name <- names(scenarios)[i]
#     effort_mult <- scenarios[[i]]
#
#     message("Running scenario: ", scenario_name, " (effort = ", effort_mult, ")")
#
#     # Setup and run
#     scenario <- setup_ecosim_scenario(model, years = years)
#     scenario$fishing$ForcedEffort <- scenario$fishing$ForcedEffort * effort_mult
#     sim <- run_ecosim_simulation(scenario)
#
#     results[[scenario_name]] <- sim
#   }
#
#   return(results)
# }
#
# Usage:
# scenarios <- list(
#   "Status Quo" = 1.0,
#   "Reduction 25%" = 0.75,
#   "Reduction 50%" = 0.5,
#   "Closure" = 0.0
# )
# comparison <- compare_scenarios(model, scenarios, years = 50)
