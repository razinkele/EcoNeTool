# =============================================================================
# Regression tests for the 2026-07-17 deep-analysis critical fixes
# =============================================================================
# Covers:
#   #7  FS0 primary-producer regex must not code consumers as autotrophs
#   #3  batch trait combine must union path-dependent columns (bind_rows)
# =============================================================================

source_app_dependencies()

# -----------------------------------------------------------------------------
# #7 - Foraging strategy: diet nouns must not force FS0 (primary producer)
# -----------------------------------------------------------------------------

test_that("a predator whose diet mentions algae is not coded as a primary producer", {
  fs <- harmonize_foraging_strategy(
    feeding_info = "predator, feeds on algae and diatoms",
    trophic_level = 3.5
  )
  expect_false(fs == "FS0",
               info = "A TL 3.5 predator must never be FS0 just because its diet lists algae")
  expect_equal(fs, "FS1", info = "'predator' text should resolve to FS1")
})

test_that("a herbivore grazing on algae is a grazer, not a primary producer", {
  fs <- harmonize_foraging_strategy(
    feeding_info = "herbivore feeding on algae",
    trophic_level = 2.0
  )
  expect_false(fs == "FS0", info = "A herbivore is a consumer, not an autotroph")
  expect_equal(fs, "FS4", info = "'herbivore' should resolve to FS4 grazer")
})

test_that("a true primary producer is still coded FS0", {
  # Producer-specific vocabulary must continue to map to FS0.
  expect_equal(
    harmonize_foraging_strategy(feeding_info = "photosynthesis", trophic_level = 1.0),
    "FS0"
  )
  expect_equal(
    harmonize_foraging_strategy(feeding_info = "autotroph", trophic_level = 1.0),
    "FS0"
  )
  expect_equal(
    harmonize_foraging_strategy(feeding_info = "primary_producer", trophic_level = 1.0),
    "FS0"
  )
})

# -----------------------------------------------------------------------------
# #3 - Combining per-species results with divergent column sets
# -----------------------------------------------------------------------------

test_that("combine_trait_results unions divergent columns with NA fill", {
  # Mimics the real bug: an offline-complete species returns a narrow frame,
  # a full-pipeline species returns extra confidence/imputation columns.
  offline_only <- data.frame(
    species = "Species A", MS = "MS3", FS = "FS1",
    stringsAsFactors = FALSE
  )
  full_path <- data.frame(
    species = "Species B", MS = "MS2", FS = "FS4",
    overall_confidence = 0.82, imputation_method = "rf_predicted",
    stringsAsFactors = FALSE
  )

  combined <- combine_trait_results(list(offline_only, full_path))

  expect_equal(nrow(combined), 2)
  expect_true(all(c("species", "MS", "FS", "overall_confidence") %in% names(combined)))
  # Row from the narrow frame gets NA for the column it never had.
  expect_true(is.na(combined$overall_confidence[combined$species == "Species A"]))
  expect_equal(combined$overall_confidence[combined$species == "Species B"], 0.82)
})

test_that("combine_trait_results drops NULL entries from failed lookups", {
  a <- data.frame(species = "Species A", MS = "MS3", stringsAsFactors = FALSE)
  combined <- combine_trait_results(list(a, NULL))
  expect_equal(nrow(combined), 1)
  expect_equal(combined$species, "Species A")
})
