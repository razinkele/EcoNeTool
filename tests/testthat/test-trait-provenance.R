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

# ---------------------------------------------------------------------------
# Task 2 - harmonisers say how they decided; PR has no PR0 default (F26)
# ---------------------------------------------------------------------------

test_that("harmonize_protection with no text and no taxon rule is NA (F26)", {
  expect_identical(harmonize_protection(character(), NULL), NA_character_)
  expect_identical(harmonize_protection(NULL, list(phylum = "Chordata", class = "Mammalia")), NA_character_)
  d <- harmonize_protection_detail(character(), NULL)
  expect_identical(d$method, NA_character_)
  # Rules and text still decide.
  expect_identical(harmonize_protection(NULL, list(phylum = "Mollusca", class = "Bivalvia")), "PR6")
  expect_identical(harmonize_protection("mucus"), "PR1")
})

test_that("each harmoniser reports observed / rule / default (C3.1)", {
  fish <- list(phylum = "Chordata", class = "Actinopteri", order = "Gadiformes")
  expect_identical(harmonize_mobility_detail("burrowing", NULL, NULL)[c("code", "method")],
                   list(code = "MB3", method = "observed"))
  expect_identical(harmonize_mobility_detail(NULL, NULL, fish)[c("code", "method")],
                   list(code = "MB5", method = "rule"))
  expect_identical(harmonize_mobility_detail(NULL, NULL, NULL)[c("code", "method")],
                   list(code = "MB4", method = "default"))

  expect_identical(harmonize_environmental_detail(NULL, NULL, "benthopelagic", NULL)$method, "observed")
  expect_identical(harmonize_environmental_detail(5, 15, NULL, NULL)[c("code", "method", "basis")],
                   list(code = "EP3", method = "rule", basis = "depth"))
  expect_identical(harmonize_environmental_detail(NULL, NULL, NULL, fish)[c("code", "method", "basis")],
                   list(code = "EP2", method = "rule", basis = "taxon"))
  expect_identical(harmonize_environmental_detail(NULL, NULL, NULL, NULL)$method, "default")

  expect_identical(harmonize_protection_detail("spines", NULL)$method, "observed")
  expect_identical(harmonize_protection_detail(NULL, fish)[c("code", "method")],
                   list(code = "PR0", method = "rule"))

  expect_identical(harmonize_foraging_detail(NULL, 1.2)[c("code", "method", "basis")],
                   list(code = "FS0", method = "observed", basis = "trophic_level"))
  expect_identical(harmonize_foraging_detail("predator", NULL)[c("code", "method", "basis")],
                   list(code = "FS1", method = "observed", basis = "text"))
  expect_identical(harmonize_foraging_detail(NULL, NULL)[c("code", "method")],
                   list(code = "FS6", method = "default"))
})

test_that("the classic harmonisers return the same codes as their detail versions", {
  inputs <- list(list(NULL, NULL), list("predator", 3.2), list(NULL, 2.0), list("nothing known", 2.2))
  for (x in inputs) {
    expect_identical(harmonize_foraging_strategy(x[[1]], x[[2]]), harmonize_foraging_detail(x[[1]], x[[2]])$code)
  }
  expect_identical(harmonize_mobility("swimming"), harmonize_mobility_detail("swimming")$code)
  expect_identical(harmonize_environmental_position(300, 500), "EP2")
})

test_that("size precedence: FishBase > SeaLifeBase > WoRMS > BIOTIC > MAREDAT > PTDB > BVOL (F28)", {
  expect_identical(select_size_by_precedence(list(WoRMS = 20, SeaLifeBase = 12)),
                   list(size_cm = 12, source = "SeaLifeBase"))
  expect_identical(select_size_by_precedence(list(BVOL = 0.001, MAREDAT = 0.15, PTDB = 0.002))$source, "MAREDAT")
  expect_identical(select_size_by_precedence(list(WoRMS = NA, BIOTIC = 3))$source, "BIOTIC")
  expect_null(select_size_by_precedence(list(WoRMS = NULL, Other = 4))$size_cm)
})

test_that("a text-derived code names the first database that supplied that input (F28)", {
  raw <- list(worms = list(phylum = "Mollusca"), biotic = list(feeding_mode = "suspension"),
              cefas = list(feeding_mode = "filter"), fishbase = list(trophic_level = 3.1))
  expect_identical(text_input_source(raw, "feeding"), "BIOTIC")
  expect_identical(text_input_source(raw, "trophic_level"), "FishBase")
  expect_identical(text_input_source(raw, "protection"), "Harmonized")
})

test_that("offline source labels, methods and confidences (F20)", {
  expect_identical(offline_source_label("biotic"), "BIOTIC")
  expect_identical(offline_source_label("species_enriched"), "SpeciesEnriched")
  expect_identical(offline_source_label(NA), "OfflineDB")
  expect_identical(offline_trait_method("ontology"), "rule")
  expect_identical(offline_trait_method("ptdb"), "observed")
  row <- data.frame(primary_source = "ontology", PR_confidence = 0, MS_confidence = 0.4)
  expect_identical(offline_trait_confidence(row, "MS"), 0.4)
  # A stored 0.0 is "unknown": the source's weight instead.
  expect_identical(offline_trait_confidence(row, "PR"), DATABASE_WEIGHTS[["Ontology"]])
})

test_that("imputation_method aggregates the per-trait methods (C3.1)", {
  r <- data.frame(MS = "MS3", MS_method = "observed", FS = "FS1", FS_method = "rule",
                  MB = NA_character_, MB_method = NA_character_, EP = "EP2", EP_method = "phylo",
                  PR = "PR0", PR_method = "ml", RS = NA_character_, TT = NA_character_, ST = NA_character_)
  expect_identical(aggregate_imputation_method(r), "ml+phylo")
  r$EP_method <- "observed"
  r$PR_method <- "rule"
  expect_identical(aggregate_imputation_method(r), "observed")
  r$PR_method <- "default"
  expect_identical(aggregate_imputation_method(r), "default")
})

test_that("the overall confidence is the geometric mean, labelled with the canonical bands (C3.2)", {
  r <- data.frame(MS_confidence = 0.5, FS_confidence = 0.8, MB_confidence = 0.8,
                  EP_confidence = 0.8, PR_confidence = 0.3)
  o <- overall_trait_confidence(r)
  expect_equal(o$value, exp(mean(log(c(0.5, 0.8, 0.8, 0.8, 0.3)))))
  expect_identical(o$label, "medium")
  expect_identical(overall_trait_confidence(data.frame(MS_confidence = NA_real_))$label, "none")
  expect_identical(overall_trait_confidence(data.frame(MS_confidence = -1))$label, "none")
})

