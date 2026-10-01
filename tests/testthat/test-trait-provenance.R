# Sub-project C, PR C-8 (spec C3): one provenance and confidence contract.
# Every trait code carries the database that produced it (T_source), how it
# was decided (T_method: observed / rule / default / ml / phylo) and a
# numeric confidence in [0, 1] that is NA exactly when the code is NA.
# Imputed codes are scored after imputation, the overall label uses the
# canonical 0.34 / 0.67 bands, the offline DB's stored confidences are
# honoured, degraded lookups are cached for one day, and imputed or own
# codes never vote as phylogenetic relatives or ML training labels.
# Every test here is offline: the lookups are stubbed.

source_app_dependencies()
source(file.path(get_app_root(), "R/functions/uncertainty_quantification.R"), local = FALSE)

# Run `code` with `cfg` as this session's harmonization config.
with_session_config <- function(cfg, code) {
  session <- shiny::MockShinySession$new()
  on.exit(session$close(), add = TRUE)
  session$userData$harm_config <- cfg
  shiny::withReactiveDomain(session, code)
}

# ---------------------------------------------------------------------------
# Task 1 - database weights, boundary distance, label bands (C3.3, C3.4)
# ---------------------------------------------------------------------------

test_that("get_database_weight('ML') is the ML_prediction weight and an unknown key warns once (C3.3)", {
  reset_database_weight_warnings()
  withr::defer(reset_database_weight_warnings())
  expect_identical(get_database_weight("ML"), DATABASE_WEIGHTS[["ML_prediction"]])
  expect_warning(w <- get_database_weight("NoSuchDB"), "\\[uq\\] no database weight for source 'NoSuchDB'")
  expect_identical(w, 0.5)
  expect_no_warning(get_database_weight("NoSuchDB"))
  expect_warning(get_database_weight(NA_character_), "source '<missing>'")
})

test_that("the spec's weight keys exist with the spec's values (C3.3)", {
  spec <- c(Phylogenetic = 0.55, OfflineDB = 0.80, Ontology = 0.60, Taxonomy = 0.50,
            "Rule-based" = 0.40, "Depth-based" = 0.40, Harmonized = 0.50)
  for (k in names(spec)) expect_identical(DATABASE_WEIGHTS[[k]], spec[[k]], info = k)
})

test_that("every source label the pipeline writes has a weight, with no warning (C3.3)", {
  reset_database_weight_warnings()
  withr::defer(reset_database_weight_warnings())
  labels <- c("FishBase", "SeaLifeBase", "BIOTIC", "MAREDAT", "PTDB", "BVOL", "SpeciesEnriched",
              "freshwaterecology.info", "PelagicTraits", "WoRMS", "WoRMS_Traits", "AlgaeBase",
              "BlackSea", "ArcticTraits", "Cefas", "CoralTraits", "PolyTraits", "EMODnet", "OBIS",
              "TraitBank", "Ontology", "Taxonomy", "Depth-based", "Harmonized", "Default", "ML",
              "Phylogenetic", "OfflineDB",
              # offline DB primary_source values
              "ontology", "biotic", "maredat", "ptdb", "bvol", "species_enriched", "blacksea",
              "arctic", "cefas", "coral")
  for (l in labels) expect_no_warning(get_database_weight(l))
  expect_identical(get_database_weight("biotic"), DATABASE_WEIGHTS[["BIOTIC"]])
  expect_identical(get_database_weight("species_enriched"), DATABASE_WEIGHTS[["SpeciesEnriched"]])
})

test_that("a size on a class boundary keeps at least 0.3 of its weight (F37)", {
  for (size in c(5, 20, 50, 150)) {
    ms <- harmonize_size_class(size)
    expect_gte(calculate_threshold_distance(size, ms), 0.3)
  }
  expect_identical(calculate_threshold_distance(12, "MS4"), 1)
})

test_that("the boundary distance is measured on the profile-adjusted size (F37)", {
  cfg <- HARMONIZATION_CONFIG
  cfg$active_profile <- "arctic" # size_multiplier 1.2: 16.5 cm is classified as 19.8 cm
  with_session_config(cfg, {
    expect_identical(harmonize_size_class(16.5), "MS4")
    # 19.8 cm is 0.2 cm below the MS4/MS5 boundary: the floor, not the raw 16.5 cm's 1.0.
    expect_identical(calculate_threshold_distance(16.5, "MS4"), 0.3)
  })
})

test_that("a non-finite size gives NA, not NaN, and no error (F37)", {
  for (bad in list(NA_real_, NaN, Inf, -Inf, "x", NULL)) {
    expect_identical(calculate_threshold_distance(bad, "MS3"), NA_real_)
  }
  expect_identical(calculate_threshold_distance(3, NA_character_), NA_real_)
  expect_identical(calculate_threshold_distance(3, "MS9"), NA_real_)
})

test_that("trait confidence categories use the 0.34 / 0.67 bands (C3.2)", {
  # 0.55 x 0.8 = 0.44: "low" under the old 0.5 / 0.7 bands, "medium" now.
  conf <- calculate_trait_confidence("EP3", source = "Phylogenetic", phylo_confidence = 0.8)
  expect_equal(conf$confidence, 0.44)
  expect_identical(conf$category, confidence_to_label(0.44))
  expect_identical(conf$category, "medium")
  uq <- readLines(file.path(get_app_root(), "R/functions/uncertainty_quantification.R"), warn = FALSE)
  expect_false(any(grepl(">= 0.7|>= 0.5", uq)))
})

test_that("an ML probability only scales an ML-sourced trait; RS/TT/ST are scored (C3.1, C3.3)", {
  rec <- list(MS = "MS4", MS_source = "FishBase", MS_ml_probability = 0.2,
              FS = "FS1", FS_source = "ML", FS_ml_probability = 0.6,
              EP = "EP3", EP_source = "Phylogenetic", EP_phylo_confidence = 0.8,
              RS = "RS2", RS_source = "Cefas", TT = NA_character_)
  out <- calculate_all_trait_confidence(rec)
  expect_identical(out$MS_confidence, 1)
  expect_equal(out$FS_confidence, 0.5 * 0.6)
  expect_equal(out$EP_confidence, 0.55 * 0.8)
  expect_identical(out$RS_confidence, DATABASE_WEIGHTS[["Cefas"]])
  expect_null(out$TT_confidence)
  expect_null(out$MB_confidence)
})

