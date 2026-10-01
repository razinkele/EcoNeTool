# =============================================================================
# Standalone test script for Uncertainty Quantification (C-8 contract)
# =============================================================================
#
# Run from the repository root:
#   Rscript tests/test_uncertainty_quantification.R
#
# Exits with status 1 when any check fails. The testthat suite
# (tests/testthat/test-trait-provenance.R) covers the same contract in more
# depth; this script is the quick console check.
#
# Contract (R/functions/uncertainty_quantification.R, spec C3.2 - C3.4):
# - DATABASE_WEIGHTS has a key for every source label; an unknown label warns
#   and gets 0.5.
# - calculate_threshold_distance() measures on the profile-adjusted size and
#   returns min(1, max(0.3, d / 0.1)); NA for non-finite input.
# - Confidence labels come from confidence_to_label() (bands 0.34 / 0.67).
# - calculate_all_trait_confidence() multiplies in the ML probability only for
#   source "ML" and the phylogenetic confidence only for "Phylogenetic".
# =============================================================================

if (!file.exists("app.R") || !dir.exists("R/functions")) {
  stop("Run this script from the repository root: Rscript tests/test_uncertainty_quantification.R")
}

cat("=============================================================================\n")
cat("UNCERTAINTY QUANTIFICATION TEST SUITE\n")
cat("=============================================================================\n\n")

# Load what the functions under test need: %||% and the harmonisation config
# (validation_utils.R, harmonization_config.R), then confidence_to_label(),
# get_harm_config() and apply_size_adjustment() (harmonization.R).
source("R/functions/validation_utils.R")
source("R/config/harmonization_config.R")
source("R/functions/trait_lookup/harmonization.R")
source("R/functions/uncertainty_quantification.R")

test_count <- 0
pass_count <- 0
fail_count <- 0
failed_names <- character(0)

# Run one test block: any error (stopifnot included) or warning-free failure
# counts as a failure. Counters are updated with <<- (plain <- inside the
# handler would change a local copy only).
run_test <- function(title, body) {
  cat("\n--- ", title, " ---\n", sep = "")
  test_count <<- test_count + 1
  tryCatch({
    body()
    cat("  PASSED\n")
    pass_count <<- pass_count + 1
  }, error = function(e) {
    cat("  FAILED:", conditionMessage(e), "\n")
    fail_count <<- fail_count + 1
    failed_names <<- c(failed_names, title)
  })
}

near <- function(a, b, tol = 1e-9) isTRUE(abs(a - b) < tol)

# =============================================================================
# TEST 1: Database authority weights
# =============================================================================
run_test("TEST 1: Database Authority Weights", function() {
  reset_database_weight_warnings()
  stopifnot(near(get_database_weight("FishBase"), 1.0))
  stopifnot(near(get_database_weight("ML_prediction"), 0.5))
  stopifnot(near(get_database_weight("ML"), 0.5))            # alias
  stopifnot(near(get_database_weight("WoRMS"), 0.6))
  stopifnot(near(get_database_weight("Phylogenetic"), 0.55))
  stopifnot(near(get_database_weight("Rule-based"), 0.40))
  stopifnot(near(get_database_weight("Default"), 0.30))
  stopifnot(get_database_weight("FishBase") > get_database_weight("ML_prediction"))

  # An unknown source label warns (once) and falls back to 0.5.
  warned <- FALSE
  w <- withCallingHandlers(
    get_database_weight("UnknownDB"),
    warning = function(cond) {
      warned <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  stopifnot(near(w, 0.5))
  stopifnot(warned)
  cat("  FishBase 1.0, ML 0.5, WoRMS 0.6, Phylogenetic 0.55, Rule-based 0.4, Default 0.3\n")
})

# =============================================================================
# TEST 2: Threshold distance (floored at 0.3, NA for non-finite input)
# =============================================================================
run_test("TEST 2: Threshold Distance Calculation", function() {
  dist_middle <- calculate_threshold_distance(3.0, "MS3")      # middle of 1-5 cm
  dist_upper <- calculate_threshold_distance(4.9, "MS3")       # 2.5% below 5 cm
  dist_lower <- calculate_threshold_distance(1.1, "MS3")       # 2.5% above 1 cm
  dist_mid_edge <- calculate_threshold_distance(1.2, "MS3")    # 5% above: 0.5
  cat("  3.0 cm:", dist_middle, " 4.9 cm:", dist_upper, " 1.1 cm:", dist_lower,
      " 1.2 cm:", dist_mid_edge, "\n")

  stopifnot(near(dist_middle, 1.0))
  stopifnot(near(dist_upper, 0.3))        # 0.025 / 0.1 = 0.25, floored at 0.3
  stopifnot(near(dist_lower, 0.3))
  stopifnot(near(dist_mid_edge, 0.5))
  stopifnot(dist_upper >= 0.3, dist_lower >= 0.3)  # never 0 any more

  # NA (not NaN) for non-finite or missing input and for a bad class
  for (bad in list(NA, NaN, Inf, "abc")) {
    r <- calculate_threshold_distance(bad, "MS3")
    stopifnot(is.na(r), !is.nan(r))
  }
  stopifnot(is.na(calculate_threshold_distance(3.0, NA_character_)))
  stopifnot(is.na(calculate_threshold_distance(3.0, "MS9")))
})

# =============================================================================
# TEST 3: Trait confidence (high confidence)
# =============================================================================
run_test("TEST 3: Trait Confidence Calculation (High Confidence)", function() {
  conf_result <- calculate_trait_confidence(
    trait_value = "MS4",
    raw_value = 15.0,
    source = "FishBase",
    threshold_distance = 1.0
  )
  cat("  Confidence:", conf_result$confidence, " Category:", conf_result$category, "\n")

  stopifnot(near(conf_result$confidence, 1.0))
  stopifnot(identical(conf_result$category, "high"))
  stopifnot(conf_result$interval_lower >= 0.5)
  stopifnot(identical(conf_result$category, confidence_to_label(conf_result$confidence)))
})

# =============================================================================
# TEST 4: Trait confidence (ML prediction)
# =============================================================================
run_test("TEST 4: Trait Confidence Calculation (ML Prediction)", function() {
  conf_result <- calculate_trait_confidence(
    trait_value = "FS1",
    source = "ML_prediction",
    ml_probability = 0.8
  )
  cat("  Confidence:", conf_result$confidence, " Category:", conf_result$category, "\n")

  stopifnot(near(conf_result$confidence, 0.5 * 0.8))
  # 0.4 is in [0.34, 0.67): "medium" under the canonical bands
  stopifnot(identical(conf_result$category, "medium"))
  stopifnot(identical(conf_result$category, confidence_to_label(0.4)))
})

# =============================================================================
# TEST 5: Trait confidence (boundary effect)
# =============================================================================
run_test("TEST 5: Trait Confidence Calculation (Boundary Effect)", function() {
  dist <- calculate_threshold_distance(4.9, "MS3")
  conf_result <- calculate_trait_confidence(
    trait_value = "MS3",
    raw_value = 4.9,
    source = "FishBase",
    threshold_distance = dist
  )
  cat("  Distance:", dist, " Confidence:", conf_result$confidence,
      " Category:", conf_result$category, "\n")

  # FishBase 1.0 x floored factor 0.3 = 0.3 (low), not ~0 as before the floor
  stopifnot(near(conf_result$confidence, 0.3))
  stopifnot(identical(conf_result$category, "low"))
  stopifnot(conf_result$confidence > 0)
})

# =============================================================================
# TEST 6: Batch confidence calculation
# =============================================================================
run_test("TEST 6: Batch Confidence Calculation", function() {
  trait_record <- list(
    MS = "MS4", size_cm = 15.0, MS_source = "FishBase",
    FS = "FS1", FS_source = "WoRMS",
    MB = "MB5", MB_source = "ML", MB_ml_probability = 0.9,
    EP = "EP2", EP_source = "BIOTIC",
    PR = "PR0", PR_source = "Rule-based",
    RS = "RS2", RS_source = "Phylogenetic", RS_phylo_confidence = 0.8,
    TT = NA, TT_source = "FishBase"
  )
  all_conf <- calculate_all_trait_confidence(trait_record)

  for (nm in c("MS", "FS", "MB", "EP", "PR", "RS")) {
    cat("  ", nm, "confidence:", round(all_conf[[paste0(nm, "_confidence")]], 3), "\n")
  }

  stopifnot(near(all_conf$MS_confidence, 1.0))
  stopifnot(near(all_conf$FS_confidence, 0.6))
  stopifnot(near(all_conf$MB_confidence, 0.5 * 0.9))        # ML weight x probability
  stopifnot(near(all_conf$EP_confidence, 0.9))
  stopifnot(near(all_conf$PR_confidence, 0.4))
  stopifnot(near(all_conf$RS_confidence, 0.55 * 0.8))       # phylo weight x vote
  stopifnot(all_conf$MS_confidence > all_conf$PR_confidence)  # FishBase > Rule-based
  # NA code: no confidence entry (NA iff the code is NA)
  stopifnot(is.null(all_conf$TT_confidence))
  # Labels are confidence_to_label() of the number
  stopifnot(identical(all_conf$MS_confidence_category, "high"))
  stopifnot(identical(all_conf$PR_confidence_category, confidence_to_label(0.4)))

  # The ML probability is ignored unless the source is "ML"
  rec2 <- list(FS = "FS1", FS_source = "FishBase", FS_ml_probability = 0.1)
  stopifnot(near(calculate_all_trait_confidence(rec2)$FS_confidence, 1.0))
})

# =============================================================================
# TEST 7: Uncertainty propagation (species level)
# =============================================================================
run_test("TEST 7: Uncertainty Propagation (Species-Level)", function() {
  species_traits <- data.frame(
    species = c("Gadus morhua", "Clupea harengus", "Sprattus sprattus"),
    MS_confidence = c(1.0, 0.9, 0.8),
    FS_confidence = c(0.9, 1.0, 0.7),
    MB_confidence = c(1.0, 0.95, 0.85),
    EP_confidence = c(0.85, 0.9, 0.75),
    PR_confidence = c(0.7, 0.8, 0.6),
    stringsAsFactors = FALSE
  )
  species_summary <- propagate_uncertainty(species_traits)

  stopifnot("overall_confidence" %in% names(species_summary))
  stopifnot(all(species_summary$overall_confidence > 0))
  stopifnot(all(species_summary$overall_confidence <= 1))
  stopifnot(species_summary$overall_confidence[1] > species_summary$overall_confidence[3])
})

# =============================================================================
# TEST 8: Uncertainty propagation (edge level)
# =============================================================================
run_test("TEST 8: Uncertainty Propagation (Edge-Level)", function() {
  species_names <- c("Gadus morhua", "Clupea harengus", "Sprattus sprattus")
  adj_matrix <- matrix(0, nrow = 3, ncol = 3, dimnames = list(species_names, species_names))
  adj_matrix[1, 2] <- 1
  adj_matrix[1, 3] <- 1

  species_traits <- data.frame(
    species = species_names,
    MS_confidence = c(1.0, 0.9, 0.8),
    FS_confidence = c(0.9, 1.0, 0.7),
    MB_confidence = c(1.0, 0.95, 0.85),
    EP_confidence = c(0.85, 0.9, 0.75),
    PR_confidence = c(0.7, 0.8, 0.6),
    stringsAsFactors = FALSE
  )
  species_summary <- propagate_uncertainty(species_traits)
  edge_confidence <- propagate_uncertainty(species_summary, adj_matrix)

  stopifnot(nrow(edge_confidence) == 2)
  stopifnot("edge_confidence" %in% names(edge_confidence))
  stopifnot(all(edge_confidence$edge_confidence > 0))
  stopifnot(all(edge_confidence$edge_confidence <= 1))
})

# =============================================================================
# TEST 9: Visualization helper functions
# =============================================================================
run_test("TEST 9: Visualization Helper Functions", function() {
  confidence_vals <- c(0.3, 0.5, 0.7, 0.9, 1.0)
  node_sizes <- map_confidence_to_size(confidence_vals)
  border_widths <- map_confidence_to_border(confidence_vals)
  edge_opacities <- map_confidence_to_opacity(confidence_vals)

  stopifnot(length(node_sizes) == 5, length(border_widths) == 5, length(edge_opacities) == 5)
  stopifnot(node_sizes[5] > node_sizes[1])         # higher confidence, larger node
  stopifnot(border_widths[1] > border_widths[5])   # lower confidence, thicker border
  stopifnot(edge_opacities[5] > edge_opacities[1]) # higher confidence, more opaque
})

# =============================================================================
# TEST 10: Pipeline-shaped record (provenance sources and labels)
# =============================================================================
# The old test 10 sourced R/functions/trait_lookup.R, which was split into
# R/functions/trait_lookup/ long ago. It now checks that a record carrying the
# source labels the pipeline writes (Default, Phylogenetic, OfflineDB, ML) is
# scored with no unknown-source warning, and that the label is the canonical one.
run_test("TEST 10: Pipeline Source Labels Are All Weighted", function() {
  reset_database_weight_warnings()
  record <- list(
    MS = "MS4", size_cm = 15.0, MS_source = "FishBase",
    FS = "FS2", FS_source = "OfflineDB",
    MB = "MB4", MB_source = "Default",
    EP = "EP3", EP_source = "Ontology",
    PR = "PR0", PR_source = "Default"
  )
  warned <- FALSE
  all_conf <- withCallingHandlers(
    calculate_all_trait_confidence(record),
    warning = function(cond) {
      warned <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  stopifnot(!warned)
  stopifnot(near(all_conf$FS_confidence, 0.80))
  stopifnot(near(all_conf$MB_confidence, 0.30))
  stopifnot(identical(all_conf$MB_confidence_category, "low"))
  stopifnot(!is.null(all_conf$MS_interval_lower), !is.null(all_conf$MS_interval_upper))
  stopifnot(identical(all_conf$MS_confidence_category, "high"))
})

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n=============================================================================\n")
cat("TEST SUMMARY\n")
cat("=============================================================================\n")
cat("Total tests: ", test_count, "\n", sep = "")
cat("Passed:      ", pass_count, "\n", sep = "")
cat("Failed:      ", fail_count, "\n", sep = "")

if (fail_count == 0) {
  cat("\nALL ", test_count, " TESTS PASSED\n", sep = "")
  cat("=============================================================================\n")
} else {
  cat("\n", fail_count, " OF ", test_count, " TESTS FAILED: ",
      paste(failed_names, collapse = "; "), "\n", sep = "")
  cat("=============================================================================\n")
  quit(status = 1)
}
