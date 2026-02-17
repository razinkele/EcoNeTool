# =============================================================================
# Test Suite for Uncertainty Quantification (Phase 4)
# =============================================================================

cat("=============================================================================\n")
cat("UNCERTAINTY QUANTIFICATION TEST SUITE\n")
cat("=============================================================================\n\n")

# Load required functions
source("R/functions/uncertainty_quantification.R")

test_count <- 0
pass_count <- 0
fail_count <- 0

# =============================================================================
# TEST 1: Database Authority Weights
# =============================================================================

cat("\n--- TEST 1: Database Authority Weights ---\n")
test_count <- test_count + 1

tryCatch({
  # Test known sources
  fishbase_weight <- get_database_weight("FishBase")
  ml_weight <- get_database_weight("ML_prediction")
  worms_weight <- get_database_weight("WoRMS")
  unknown_weight <- get_database_weight("UnknownDB")

  cat("  FishBase weight:", fishbase_weight, "\n")
  cat("  ML_prediction weight:", ml_weight, "\n")
  cat("  WoRMS weight:", worms_weight, "\n")
  cat("  Unknown DB weight:", unknown_weight, "\n")

  # Validate
  stopifnot(fishbase_weight == 1.0)
  stopifnot(ml_weight == 0.5)
  stopifnot(worms_weight == 0.6)
  stopifnot(unknown_weight == 0.5)  # Default
  stopifnot(fishbase_weight > ml_weight)  # FishBase more authoritative

  cat("  ✓ PASSED: Database weights correctly assigned\n")
  pass_count <- pass_count + 1

}, error = function(e) {
  cat("  ✗ FAILED:", e$message, "\n")
  fail_count <- fail_count + 1
})


# =============================================================================
# TEST 2: Threshold Distance Calculation
# =============================================================================

cat("\n--- TEST 2: Threshold Distance Calculation ---\n")
test_count <- test_count + 1

tryCatch({
  # Test distances for different scenarios
  dist_middle <- calculate_threshold_distance(3.0, "MS3")  # Middle of 1-5 cm class
  dist_boundary <- calculate_threshold_distance(4.9, "MS3")  # Near upper boundary
  dist_lower_boundary <- calculate_threshold_distance(1.1, "MS3")  # Near lower boundary

  cat("  Distance at 3.0 cm (MS3 middle):", round(dist_middle, 3), "\n")
  cat("  Distance at 4.9 cm (MS3 boundary):", round(dist_boundary, 3), "\n")
  cat("  Distance at 1.1 cm (MS3 lower):", round(dist_lower_boundary, 3), "\n")

  # Validate
  stopifnot(dist_middle == 1.0)  # Far from boundary
  stopifnot(dist_boundary < 0.1)  # Very close to boundary
  stopifnot(dist_lower_boundary < 0.1)  # Very close to boundary

  cat("  ✓ PASSED: Threshold distances calculated correctly\n")
  pass_count <- pass_count + 1

}, error = function(e) {
  cat("  ✗ FAILED:", e$message, "\n")
  fail_count <- fail_count + 1
})


# =============================================================================
# TEST 3: Trait Confidence Calculation (High Confidence)
# =============================================================================

cat("\n--- TEST 3: Trait Confidence Calculation (High Confidence) ---\n")
test_count <- test_count + 1

tryCatch({
  # Test high confidence scenario: FishBase data, far from boundary
  conf_result <- calculate_trait_confidence(
    trait_value = "MS4",
    raw_value = 15.0,
    source = "FishBase",
    threshold_distance = 1.0
  )

  cat("  Confidence:", round(conf_result$confidence, 3), "\n")
  cat("  Category:", conf_result$category, "\n")
  cat("  Interval:", round(conf_result$interval_lower, 3), "-", round(conf_result$interval_upper, 3), "\n")

  # Validate
  stopifnot(conf_result$confidence == 1.0)  # FishBase (1.0) * far from boundary (1.0)
  stopifnot(conf_result$category == "high")
  stopifnot(conf_result$interval_lower >= 0.5)

  cat("  ✓ PASSED: High confidence correctly calculated\n")
  pass_count <- pass_count + 1

}, error = function(e) {
  cat("  ✗ FAILED:", e$message, "\n")
  fail_count <- fail_count + 1
})


# =============================================================================
# TEST 4: Trait Confidence Calculation (ML Prediction)
# =============================================================================

cat("\n--- TEST 4: Trait Confidence Calculation (ML Prediction) ---\n")
test_count <- test_count + 1

tryCatch({
  # Test ML prediction scenario: lower base authority, ML probability
  conf_result <- calculate_trait_confidence(
    trait_value = "FS1",
    source = "ML_prediction",
    ml_probability = 0.8
  )

  cat("  Confidence:", round(conf_result$confidence, 3), "\n")
  cat("  Category:", conf_result$category, "\n")

  # Validate
  expected_conf <- 0.5 * 0.8  # ML base (0.5) * ML probability (0.8) = 0.4
  stopifnot(abs(conf_result$confidence - expected_conf) < 0.01)
  stopifnot(conf_result$category == "low")  # 0.4 is low confidence

  cat("  ✓ PASSED: ML prediction confidence correctly calculated\n")
  pass_count <- pass_count + 1

}, error = function(e) {
  cat("  ✗ FAILED:", e$message, "\n")
  fail_count <- fail_count + 1
})


# =============================================================================
# TEST 5: Trait Confidence Calculation (Boundary Effect)
# =============================================================================

cat("\n--- TEST 5: Trait Confidence Calculation (Boundary Effect) ---\n")
test_count <- test_count + 1

tryCatch({
  # Test boundary penalty: FishBase but near class boundary
  conf_result <- calculate_trait_confidence(
    trait_value = "MS3",
    raw_value = 4.9,
    source = "FishBase",
    threshold_distance = 0.025  # Very close to boundary
  )

  cat("  Confidence:", round(conf_result$confidence, 3), "\n")
  cat("  Category:", conf_result$category, "\n")

  # Validate
  stopifnot(conf_result$confidence < 0.1)  # Should be very low due to boundary
  stopifnot(conf_result$category == "low")

  cat("  ✓ PASSED: Boundary effect correctly reduces confidence\n")
  pass_count <- pass_count + 1

}, error = function(e) {
  cat("  ✗ FAILED:", e$message, "\n")
  fail_count <- fail_count + 1
})


# =============================================================================
# TEST 6: Batch Confidence Calculation
# =============================================================================

cat("\n--- TEST 6: Batch Confidence Calculation ---\n")
test_count <- test_count + 1

tryCatch({
  # Test calculating confidence for all traits in a record
  trait_record <- list(
    MS = "MS4",
    size_cm = 15.0,
    MS_source = "FishBase",

    FS = "FS1",
    FS_source = "WoRMS",

    MB = "MB5",
    MB_source = "ML_prediction",
    MB_ml_probability = 0.9,

    EP = "EP2",
    EP_source = "BIOTIC",

    PR = "PR0",
    PR_source = "Rule-based"
  )

  all_conf <- calculate_all_trait_confidence(trait_record)

  cat("  MS confidence:", round(all_conf$MS_confidence, 3), "\n")
  cat("  FS confidence:", round(all_conf$FS_confidence, 3), "\n")
  cat("  MB confidence:", round(all_conf$MB_confidence, 3), "\n")
  cat("  EP confidence:", round(all_conf$EP_confidence, 3), "\n")
  cat("  PR confidence:", round(all_conf$PR_confidence, 3), "\n")

  # Validate
  stopifnot(!is.null(all_conf$MS_confidence))
  stopifnot(!is.null(all_conf$FS_confidence))
  stopifnot(!is.null(all_conf$MB_confidence))
  stopifnot(!is.null(all_conf$EP_confidence))
  stopifnot(!is.null(all_conf$PR_confidence))
  stopifnot(all_conf$MS_confidence > all_conf$PR_confidence)  # FishBase > Rule-based

  cat("  ✓ PASSED: Batch confidence calculation works\n")
  pass_count <- pass_count + 1

}, error = function(e) {
  cat("  ✗ FAILED:", e$message, "\n")
  fail_count <- fail_count + 1
})


# =============================================================================
# TEST 7: Uncertainty Propagation (Species-Level)
# =============================================================================

cat("\n--- TEST 7: Uncertainty Propagation (Species-Level) ---\n")
test_count <- test_count + 1

tryCatch({
  # Create test species data
  species_traits <- data.frame(
    species = c("Gadus morhua", "Clupea harengus", "Sprattus sprattus"),
    MS_confidence = c(1.0, 0.9, 0.8),
    FS_confidence = c(0.9, 1.0, 0.7),
    MB_confidence = c(1.0, 0.95, 0.85),
    EP_confidence = c(0.85, 0.9, 0.75),
    PR_confidence = c(0.7, 0.8, 0.6),
    stringsAsFactors = FALSE
  )

  # Propagate uncertainty
  species_summary <- propagate_uncertainty(species_traits)

  cat("  Species with overall confidence:\n")
  for (i in 1:nrow(species_summary)) {
    cat("    ", species_summary$species[i], ":",
        round(species_summary$overall_confidence[i], 3), "\n")
  }

  # Validate
  stopifnot("overall_confidence" %in% names(species_summary))
  stopifnot(all(species_summary$overall_confidence > 0))
  stopifnot(all(species_summary$overall_confidence <= 1))
  # Gadus morhua should have highest confidence (all traits high)
  stopifnot(species_summary$overall_confidence[1] > species_summary$overall_confidence[3])

  cat("  ✓ PASSED: Species-level uncertainty propagation works\n")
  pass_count <- pass_count + 1

}, error = function(e) {
  cat("  ✗ FAILED:", e$message, "\n")
  fail_count <- fail_count + 1
})


# =============================================================================
# TEST 8: Uncertainty Propagation (Edge-Level)
# =============================================================================

cat("\n--- TEST 8: Uncertainty Propagation (Edge-Level) ---\n")
test_count <- test_count + 1

tryCatch({
  # Create test adjacency matrix
  species_names <- c("Gadus morhua", "Clupea harengus", "Sprattus sprattus")
  adj_matrix <- matrix(0, nrow = 3, ncol = 3,
                      dimnames = list(species_names, species_names))
  # Gadus eats Clupea and Sprattus
  adj_matrix[1, 2] <- 1
  adj_matrix[1, 3] <- 1

  # Reuse species data from TEST 7
  species_traits <- data.frame(
    species = species_names,
    MS_confidence = c(1.0, 0.9, 0.8),
    FS_confidence = c(0.9, 1.0, 0.7),
    MB_confidence = c(1.0, 0.95, 0.85),
    EP_confidence = c(0.85, 0.9, 0.75),
    PR_confidence = c(0.7, 0.8, 0.6),
    stringsAsFactors = FALSE
  )

  # Calculate species-level confidence first
  species_summary <- propagate_uncertainty(species_traits)

  # Propagate to edges
  edge_confidence <- propagate_uncertainty(species_summary, adj_matrix)

  cat("  Edge confidence:\n")
  for (i in 1:nrow(edge_confidence)) {
    cat("    ", edge_confidence$predator[i], "→", edge_confidence$prey[i], ":",
        round(edge_confidence$edge_confidence[i], 3), "\n")
  }

  # Validate
  stopifnot(nrow(edge_confidence) == 2)  # Two edges
  stopifnot("edge_confidence" %in% names(edge_confidence))
  stopifnot(all(edge_confidence$edge_confidence > 0))
  stopifnot(all(edge_confidence$edge_confidence <= 1))

  cat("  ✓ PASSED: Edge-level uncertainty propagation works\n")
  pass_count <- pass_count + 1

}, error = function(e) {
  cat("  ✗ FAILED:", e$message, "\n")
  fail_count <- fail_count + 1
})


# =============================================================================
# TEST 9: Visualization Helper Functions
# =============================================================================

cat("\n--- TEST 9: Visualization Helper Functions ---\n")
test_count <- test_count + 1

tryCatch({
  # Test confidence values
  confidence_vals <- c(0.3, 0.5, 0.7, 0.9, 1.0)

  # Map to node size
  node_sizes <- map_confidence_to_size(confidence_vals)
  cat("  Node sizes:", paste(round(node_sizes, 1), collapse = ", "), "\n")

  # Map to border width
  border_widths <- map_confidence_to_border(confidence_vals)
  cat("  Border widths:", paste(round(border_widths, 1), collapse = ", "), "\n")

  # Map to edge opacity
  edge_opacities <- map_confidence_to_opacity(confidence_vals)
  cat("  Edge opacities:", paste(round(edge_opacities, 2), collapse = ", "), "\n")

  # Validate
  stopifnot(length(node_sizes) == 5)
  stopifnot(length(border_widths) == 5)
  stopifnot(length(edge_opacities) == 5)
  # Higher confidence should give larger nodes
  stopifnot(node_sizes[5] > node_sizes[1])
  # Lower confidence should give thicker borders
  stopifnot(border_widths[1] > border_widths[5])
  # Higher confidence should give more opaque edges
  stopifnot(edge_opacities[5] > edge_opacities[1])

  cat("  ✓ PASSED: Visualization helpers work correctly\n")
  pass_count <- pass_count + 1

}, error = function(e) {
  cat("  ✗ FAILED:", e$message, "\n")
  fail_count <- fail_count + 1
})


# =============================================================================
# TEST 10: Integration with Trait Lookup
# =============================================================================

cat("\n--- TEST 10: Integration with Trait Lookup ---\n")
test_count <- test_count + 1

tryCatch({
  # Test that uncertainty functions are compatible with trait lookup output
  source("R/functions/trait_lookup.R")

  # Simulate trait lookup result
  mock_result <- list(
    MS = "MS4",
    size_cm = 15.0,
    MS_source = "FishBase",

    FS = "FS1",
    FS_source = "WoRMS",

    MB = "MB5",
    MB_source = "Rule-based",

    EP = "EP2",
    EP_source = "BIOTIC",

    PR = "PR0",
    PR_source = "Rule-based"
  )

  # Calculate confidence (should work seamlessly)
  all_conf <- calculate_all_trait_confidence(mock_result)

  # Validate integration
  stopifnot(!is.null(all_conf$MS_confidence))
  stopifnot(!is.null(all_conf$MS_interval_lower))
  stopifnot(!is.null(all_conf$MS_interval_upper))
  stopifnot(!is.null(all_conf$MS_confidence_category))

  cat("  ✓ PASSED: Uncertainty quantification integrates with trait lookup\n")
  pass_count <- pass_count + 1

}, error = function(e) {
  cat("  ✗ FAILED:", e$message, "\n")
  fail_count <- fail_count + 1
})


# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=============================================================================\n")
cat("TEST SUMMARY\n")
cat("=============================================================================\n")
cat("Total tests: ", test_count, "\n")
cat("Passed:      ", pass_count, " (", round(pass_count/test_count*100, 1), "%)\n", sep="")
cat("Failed:      ", fail_count, " (", round(fail_count/test_count*100, 1), "%)\n", sep="")

if (fail_count == 0) {
  cat("\n✅ ALL TESTS PASSED - Phase 4 ready for production\n")
} else {
  cat("\n⚠️  SOME TESTS FAILED - Review errors above\n")
}

cat("=============================================================================\n")
