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
source(file.path(get_app_root(), "R/functions/offline_db_rebuild.R"), local = FALSE)

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


# ---------------------------------------------------------------------------
# A stubbed lookup pipeline: every database lookup is "not found" unless the
# test supplies it, so lookup_species_traits() runs offline and fast.
# ---------------------------------------------------------------------------

# Run `code` with each function in `fns` (name -> function) assigned in
# globalenv, restoring (or removing) whatever was there before on the way out.
with_globals <- function(fns, code) {
  old <- mget(names(fns), envir = globalenv(), ifnotfound = list(NULL))
  on.exit({
    for (nm in names(fns)) {
      if (is.null(old[[nm]])) {
        if (exists(nm, envir = globalenv(), inherits = FALSE)) rm(list = nm, envir = globalenv())
      } else {
        assign(nm, old[[nm]], envir = globalenv())
      }
    }
  }, add = TRUE)
  for (nm in names(fns)) assign(nm, fns[[nm]], envir = globalenv())
  force(code)
}

not_found <- function(...) list(success = FALSE, traits = list())
found <- function(traits) {
  force(traits)
  function(...) list(success = TRUE, traits = traits)
}
worms_taxon <- function(phylum, class, order = NA_character_, ...) {
  found(list(phylum = phylum, class = class, order = order, family = NA_character_,
             genus = NA_character_, aphia_id = 1L, isMarine = TRUE, ...))
}

pipeline_stubs <- function() {
  lookups <- c("lookup_worms_traits", "lookup_ontology_traits", "lookup_fishbase_traits",
               "lookup_sealifebase_traits", "lookup_biotic_traits", "lookup_maredat_traits",
               "lookup_ptdb_traits", "lookup_algaebase_traits", "lookup_freshwaterecology_traits",
               "lookup_blacksea_traits", "lookup_arctic_traits", "lookup_cefas_traits",
               "lookup_coral_traits", "lookup_pelagic_traits", "lookup_worms_traits_api",
               "lookup_polytraits", "lookup_emodnet_traits", "lookup_obis_traits", "lookup_traitbank")
  stubs <- stats::setNames(rep(list(not_found), length(lookups)), lookups)
  c(stubs, list(
    lookup_offline_traits = function(...) NULL,
    lookup_bvol_traits = function(...) NULL,
    lookup_species_enriched_traits = function(...) NULL,
    apply_ml_fallback = function(harmonized_traits, raw_traits, verbose = FALSE) harmonized_traits,
    apply_phylogenetic_imputation = function(species_name, current_traits, ...) current_traits
  ))
}

# lookup_species_traits() with every lookup stubbed; `stubs` overrides some.
# Without a `cache_dir` it uses a fresh temp directory.
run_pipeline <- function(stubs = list(), species = "Testus maximus", cache_dir = NULL) {
  if (is.null(cache_dir)) {
    cache_dir <- tempfile("taxonomy_")
    dir.create(cache_dir)
    on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  }
  with_globals(utils::modifyList(pipeline_stubs(), stubs),
               suppressMessages(lookup_species_traits(species, cache_dir = cache_dir)))
}

offline_row <- function(species = "Testus maximus") {
  data.frame(species = species, MS = "MS3", FS = "FS1", MB = "MB2", EP = "EP1", PR = "PR0",
             primary_source = "ontology", stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# Task 3 - the offline DB: stored confidences, RS/TT/ST, path, writer (C3.5)
# ---------------------------------------------------------------------------

test_that("a complete offline row keeps its stored confidences; the label is their geometric mean (F20)", {
  reset_offline_vocab_gate()
  db <- make_offline_db_fixture(data.frame(
    species = "Testus maximus", MS = "MS3", FS = "FS6", MB = "MB1", EP = "EP3", PR = "PR6",
    MS_confidence = 0.5, FS_confidence = 0.8, MB_confidence = 0.8, EP_confidence = 0.8,
    PR_confidence = 0.3, primary_source = "biotic", stringsAsFactors = FALSE))
  real <- lookup_offline_traits
  res <- run_pipeline(list(
    lookup_offline_traits = function(species_name, db_path = db) real(species_name, db_path),
    lookup_worms_traits = worms_taxon("Mollusca", "Bivalvia")))
  expect_identical(res$MS_confidence, 0.5)
  expect_identical(res$PR_confidence, 0.3)
  expect_equal(res$overall_confidence, exp(mean(log(c(0.5, 0.8, 0.8, 0.8, 0.3)))))
  expect_identical(res$confidence, confidence_to_label(res$overall_confidence))
  expect_identical(res$confidence, "medium") # was a hard-coded "high"
  expect_identical(res$MS_source, "BIOTIC")
  expect_identical(res$MS_method, "observed")
  expect_identical(res$imputation_method, "observed")
  expect_identical(res$source, "offline:biotic")
})

test_that("an offline DB from another vocabulary is skipped with a rebuild warning (C3.5)", {
  reset_offline_vocab_gate()
  withr::defer(reset_offline_vocab_gate())
  db <- make_offline_db_fixture(offline_row(), vocab_version = 1L)
  expect_warning(res <- lookup_offline_traits("Testus maximus", db_path = db), "rebuild required")
  expect_null(res)
})

test_that("the default offline DB path resolves from any working directory (F24)", {
  reset_offline_vocab_gate()
  srv <- readLines(file.path(get_app_root(), "R/modules/trait_research_server.R"), warn = FALSE)
  expect_false(any(grepl('db_path <- "cache/offline_traits.db"', srv, fixed = TRUE)))
  fixture <- make_offline_db_fixture(offline_row())
  root <- withr::local_tempdir()
  dir.create(file.path(root, "R", "functions"), recursive = TRUE)
  dir.create(file.path(root, "tests", "testthat"), recursive = TRUE)
  dir.create(file.path(root, "cache"))
  file.create(file.path(root, "app.R"))
  file.copy(fixture, file.path(root, "cache", "offline_traits.db"))
  withr::local_options(econetool.app_root = NULL)
  withr::local_dir(file.path(root, "tests", "testthat"))
  res <- lookup_offline_traits("Testus maximus")
  expect_identical(res$MS, "MS3")
})

test_that("an offline row with only RS is served with its source and confidence (F22)", {
  reset_offline_vocab_gate()
  db <- make_offline_db_fixture(data.frame(species = "Testus maximus", RS = "RS2", RS_confidence = 0.6,
                                           primary_source = "cefas", stringsAsFactors = FALSE))
  real <- lookup_offline_traits
  res <- run_pipeline(list(
    lookup_offline_traits = function(species_name, db_path = db) real(species_name, db_path),
    lookup_worms_traits = worms_taxon("Chordata", "Actinopteri", "Gadiformes")))
  expect_identical(res$RS, "RS2")
  expect_identical(res$RS_source, "Cefas")
  expect_identical(res$RS_method, "observed")
  expect_identical(res$RS_confidence, 0.6)
})

test_that("the RS/TT/ST writer enriches an existing species and counts rows changed (F22)", {
  for (upsert in c(TRUE, FALSE)) {
    db <- make_offline_db_fixture(offline_row())
    con <- DBI::dbConnect(RSQLite::SQLite(), db)
    n_new <- upsert_extended_traits(con, "Testus maximus", "cefas", "RS2", NA, NA, 0.7, 0, 0,
                                    use_upsert = upsert)
    n_again <- upsert_extended_traits(con, "Testus maximus", "cefas", "RS3", NA, NA, 0.7, 0, 0,
                                      use_upsert = upsert)
    n_other <- upsert_extended_traits(con, "Novus species", "cefas", NA, "TT2", NA, 0, 0.7, 0,
                                      use_upsert = upsert)
    row <- DBI::dbGetQuery(con, "SELECT * FROM species_traits WHERE species = 'Testus maximus'")
    other <- DBI::dbGetQuery(con, "SELECT * FROM species_traits WHERE species = 'Novus species'")
    DBI::dbDisconnect(con)
    expect_identical(c(n_new, n_again, n_other), c(1L, 0L, 1L), info = upsert)
    expect_identical(row$RS, "RS2", info = upsert) # filled once, never overwritten
    expect_identical(row$RS_confidence, 0.7, info = upsert)
    expect_identical(row$MS, "MS3", info = upsert) # core codes untouched
    expect_identical(row$primary_source, "ontology", info = upsert)
    expect_identical(other$TT, "TT2", info = upsert)
  }
  build <- readLines(file.path(get_app_root(), "scripts/initialization/build_offline_trait_db.R"), warn = FALSE)
  build <- build[!startsWith(trimws(build), "#")]
  expect_true(any(grepl("upsert_extended_traits(con, sp, source_label", build, fixed = TRUE)))
  expect_true(any(grepl("inserted <- inserted + affected", build, fixed = TRUE)))
})

test_that("a session with its own harmonization settings is not served offline codes", {
  reset_offline_vocab_gate()
  cfg <- HARMONIZATION_CONFIG
  cfg$size_thresholds$MS3_MS4 <- 6
  called <- FALSE
  spy <- function(...) {
    called <<- TRUE
    NULL
  }
  with_session_config(cfg, {
    run_pipeline(list(lookup_offline_traits = spy,
                      lookup_worms_traits = worms_taxon("Chordata", "Actinopteri", "Gadiformes")))
  })
  expect_false(called)
  run_pipeline(list(lookup_offline_traits = spy,
                    lookup_worms_traits = worms_taxon("Chordata", "Actinopteri", "Gadiformes")))
  expect_true(called)
})

# ---------------------------------------------------------------------------
# Task 4 - the harmonisation phase: sources at assignment, size precedence,
# fuzzy fallbacks respect the offline DB, PR left for imputation (C3.6)
# ---------------------------------------------------------------------------

test_that("a WoRMS size wins when SeaLifeBase reports no size (F28)", {
  res <- run_pipeline(list(
    lookup_worms_traits = worms_taxon("Mollusca", "Bivalvia", max_length_cm = 20),
    lookup_sealifebase_traits = found(list(trophic_level = 2.1))))
  expect_identical(res$MS, "MS5")
  expect_identical(res$MS_source, "WoRMS")
  expect_identical(res$MS_method, "observed")
})

test_that("SeaLifeBase outranks WoRMS for size (F28 precedence)", {
  res <- run_pipeline(list(
    lookup_worms_traits = worms_taxon("Mollusca", "Bivalvia", max_length_cm = 20),
    lookup_sealifebase_traits = found(list(max_length_cm = 4))))
  expect_identical(res$MS, "MS3")
  expect_identical(res$MS_source, "SeaLifeBase")
})

test_that("a MAREDAT size (ESD) feeds MS (F28)", {
  res <- run_pipeline(list(
    lookup_worms_traits = worms_taxon("Chordata", "Appendicularia"),
    lookup_maredat_traits = found(list(size_um = 1500, max_length_cm = 0.15))))
  expect_identical(res$MS, "MS2")
  expect_identical(res$MS_source, "MAREDAT")
})

test_that("BIOTIC and PTDB sizes feed MS (F28)", {
  biotic <- run_pipeline(list(
    lookup_worms_traits = worms_taxon("Annelida", "Polychaeta"),
    lookup_biotic_traits = found(list(max_length_cm = 12))))
  expect_identical(biotic$MS_source, "BIOTIC")
  expect_identical(biotic$MS, "MS4")
  ptdb <- run_pipeline(list(
    lookup_worms_traits = worms_taxon("Bacillariophyta", "Bacillariophyceae"),
    lookup_ptdb_traits = found(list(cell_volume_um3 = 1000, max_length_cm = 0.001))))
  expect_identical(ptdb$MS_source, "PTDB")
  expect_identical(ptdb$MS, "MS1")
})

test_that("an offline-prefilled MS / FS / MB / EP is never replaced or cleared (F27)", {
  offline <- data.frame(species = "Testus maximus", MS = "MS3", FS = "FS5", MB = "MB3", EP = "EP4",
                        MS_confidence = 0.7, FS_confidence = 0.7, MB_confidence = 0.7, EP_confidence = 0.7,
                        primary_source = "biotic", stringsAsFactors = FALSE)
  res <- run_pipeline(list(
    lookup_offline_traits = function(...) offline,
    lookup_worms_traits = worms_taxon("Annelida", "Polychaeta"),
    lookup_ontology_traits = found(data.frame(trait = "x")),
    extract_primary_feeding = function(...) list(modality = NA, score = NA, ontology_id = NA),
    harmonize_fuzzy_foraging = function(...) list(class = "FS1", confidence = "high", modalities = "x"),
    harmonize_fuzzy_mobility = function(...) list(class = "MB5", confidence = "high", modalities = "x"),
    harmonize_fuzzy_habitat = function(...) list(class = "EP1", confidence = "high", modalities = "x")))
  expect_identical(c(res$MS, res$FS, res$MB, res$EP), c("MS3", "FS5", "MB3", "EP4")) # no size: MS kept too
  expect_identical(c(res$MS_source, res$FS_source, res$MB_source, res$EP_source), rep("BIOTIC", 4))
})

test_that("the ontology fallback is labelled Ontology / rule when nothing was prefilled", {
  res <- run_pipeline(list(
    lookup_worms_traits = worms_taxon("Annelida", "Polychaeta"),
    lookup_ontology_traits = found(data.frame(trait = "x")),
    extract_primary_feeding = function(...) list(modality = NA, score = NA, ontology_id = NA),
    harmonize_fuzzy_foraging = function(...) list(class = "FS1", confidence = "high", modalities = "x"),
    harmonize_fuzzy_mobility = function(...) list(class = NA, confidence = NA, modalities = character()),
    harmonize_fuzzy_habitat = function(...) list(class = NA, confidence = NA, modalities = character())))
  expect_identical(res$FS, "FS1")
  expect_identical(res$FS_source, "Ontology")
  expect_identical(res$FS_method, "rule")
})

test_that("with no protection text and no taxon rule, PR reaches imputation as NA (F26)", {
  seen_ml <- "not called"
  seen_phylo <- NULL
  run_pipeline(list(
    lookup_worms_traits = worms_taxon("Chordata", "Mammalia", "Carnivora"),
    apply_ml_fallback = function(harmonized_traits, raw_traits, verbose = FALSE) {
      seen_ml <<- harmonized_traits$PR
      harmonized_traits
    },
    apply_phylogenetic_imputation = function(species_name, current_traits, ...) {
      seen_phylo <<- current_traits
      current_traits
    }))
  expect_identical(seen_ml, NA_character_)
  expect_identical(seen_phylo$PR, NA_character_)
  expect_identical(seen_phylo$PR_source, NA_character_) # was "Taxonomy" for a guessed PR0
})

test_that("FS, MB and EP name the database or rule that decided them (F28)", {
  res <- run_pipeline(list(
    lookup_worms_traits = worms_taxon("Mollusca", "Bivalvia"),
    lookup_biotic_traits = found(list(feeding_mode = "filter feeder", living_habit = "burrow dwelling")),
    lookup_cefas_traits = found(list(feeding_mode = "deposit"))))
  expect_identical(res$FS_source, "BIOTIC") # first feeding contributor, not the last database queried
  expect_identical(res$FS_method, "observed")
  expect_identical(res$EP_source, "BIOTIC")
  expect_identical(res$EP_method, "observed")
  expect_identical(res$PR_source, "Taxonomy")
  expect_identical(res$PR_method, "rule")

  fish <- run_pipeline(list(
    lookup_worms_traits = worms_taxon("Chordata", "Actinopteri", "Gadiformes"),
    lookup_fishbase_traits = found(list(max_length_cm = 100, trophic_level = 4.1, body_shape = "fusiform",
                                        depth_min = 10, depth_max = 30))))
  expect_identical(fish$FS_source, "FishBase")
  expect_identical(fish$MB_source, "Taxonomy") # fish MB comes from the taxon rule, not the body shape
  expect_identical(fish$MB_method, "rule")
  expect_identical(fish$EP_source, "Depth-based")
  expect_identical(fish$EP_method, "rule")
})

# ---------------------------------------------------------------------------
# Task 5 - pipeline order: ML -> phylo -> scoring -> overall label (C3.2, C3.3)
# ---------------------------------------------------------------------------

# A phylogenetic-imputation stub that fills EP like the real one does.
phylo_fills_ep <- function(species_name, current_traits, ...) {
  if (is.na(current_traits$EP)) {
    current_traits$EP <- "EP3"
    current_traits$EP_source <- "Phylogenetic"
    current_traits$EP_phylo_confidence <- 0.8
  }
  current_traits
}

test_that("a phylo-imputed EP is method phylo and scored after imputation (C3.2, C3.3)", {
  res <- run_pipeline(list(
    lookup_worms_traits = worms_taxon("Chordata", "Actinopteri", "Gadiformes"),
    apply_phylogenetic_imputation = phylo_fills_ep))
  expect_identical(res$EP, "EP3")
  expect_identical(res$EP_method, "phylo")
  expect_equal(res$EP_confidence, 0.8 * DATABASE_WEIGHTS[["Phylogenetic"]])
  expect_gt(res$EP_confidence, 0)
  expect_match(res$imputation_method, "phylo")
})

test_that("an ML-filled code is method ml, scored by its probability; ML never rescales other codes", {
  ml <- function(harmonized_traits, raw_traits, verbose = FALSE) {
    for (t in c("MS", "MB")) {
      if (is.na(harmonized_traits[[t]])) harmonized_traits[[t]] <- if (t == "MS") "MS3" else "MB3"
      harmonized_traits[[paste0(t, "_ml_probability")]] <- 0.6
      harmonized_traits[[paste0(t, "_ml_confidence")]] <- "medium"
    }
    harmonized_traits
  }
  res <- run_pipeline(list(
    lookup_worms_traits = worms_taxon("Mollusca", "Gastropoda", max_length_cm = 3),
    apply_ml_fallback = ml))
  # MB had no mobility text, so ML filled it.
  expect_identical(res$MB, "MB3")
  expect_identical(res$MB_source, "ML")
  expect_identical(res$MB_method, "ml")
  expect_equal(res$MB_confidence, 0.6 * DATABASE_WEIGHTS[["ML_prediction"]])
  expect_match(res$imputation_method, "ml")
  # MS came from the WoRMS size: ML's probability for MS is neither kept nor applied.
  expect_identical(res$MS_method, "observed")
  expect_null(res$MS_ml_probability)
  expect_identical(res$MS_confidence, DATABASE_WEIGHTS[["WoRMS"]])
})

test_that("a measured size on a class boundary does not make otherwise-good data low (F37)", {
  res <- run_pipeline(list(
    lookup_worms_traits = worms_taxon("Chordata", "Actinopteri", "Gadiformes"),
    lookup_fishbase_traits = found(list(max_length_cm = 50, trophic_level = 4.1,
                                        depth_min = 10, depth_max = 30))))
  expect_identical(res$MS, "MS6")
  expect_equal(res$MS_confidence, 0.3) # FishBase 1.0 x the 0.3 floor (was 0, so the whole row was "low")
  expect_false(identical(res$confidence, "low"))
  expect_identical(res$confidence, confidence_to_label(res$overall_confidence))
})

test_that("an undecided PR falls back to PR0 labelled Default / default (C-8 user decision 1)", {
  res <- run_pipeline(list(lookup_worms_traits = worms_taxon("Chordata", "Mammalia", "Carnivora")))
  expect_identical(res$PR, "PR0")
  expect_identical(res$PR_source, "Default")
  expect_identical(res$PR_method, "default")
  expect_identical(res$PR_confidence, DATABASE_WEIGHTS[["Default"]])
  expect_match(res$imputation_method, "default")
})

test_that("every code has a method and a confidence, and the label matches the overall value (acceptance 5)", {
  cases <- list(
    fish = list(lookup_worms_traits = worms_taxon("Chordata", "Actinopteri", "Gadiformes"),
                lookup_fishbase_traits = found(list(max_length_cm = 130, trophic_level = 4.4,
                                                    depth_min = 150, depth_max = 600))),
    mussel = list(lookup_worms_traits = worms_taxon("Mollusca", "Bivalvia", max_length_cm = 20)),
    copepod = list(lookup_worms_traits = worms_taxon("Arthropoda", "Copepoda"),
                   apply_phylogenetic_imputation = phylo_fills_ep),
    medusa = list(lookup_worms_traits = worms_taxon("Cnidaria", "Scyphozoa", max_length_cm = 40)),
    urchin = list(lookup_worms_traits = worms_taxon("Echinodermata", "Echinoidea")),
    nematode = list(lookup_worms_traits = worms_taxon("Nematoda", "Enoplea"))
  )
  for (nm in names(cases)) {
    res <- run_pipeline(cases[[nm]], species = nm)
    for (t in TRAIT_COLUMNS) {
      code <- res[[t]]
      conf <- res[[paste0(t, "_confidence")]]
      if (is.na(code)) {
        expect_true(is.null(conf) || is.na(conf), info = paste(nm, t))
      } else {
        expect_false(is.na(res[[paste0(t, "_method")]]), info = paste(nm, t))
        expect_false(is.na(res[[paste0(t, "_source")]]), info = paste(nm, t))
        expect_true(isTRUE(conf > 0 && conf <= 1), info = paste(nm, t))
      }
    }
    expect_identical(res$confidence, confidence_to_label(res$overall_confidence), info = nm)
  }
})

test_that("no private confidence bands or legacy method labels are left in the orchestrator (C3.2)", {
  orch <- readLines(file.path(get_app_root(), "R/functions/trait_lookup/orchestrator.R"), warn = FALSE)
  code <- orch[!startsWith(trimws(orch), "#")]
  expect_false(any(grepl(">= 0.7|>= 0.5", code)))
  expect_false(any(grepl("rf_predicted", code, fixed = TRUE)))
  expect_false(any(grepl('confidence <- "high"', code, fixed = TRUE)))
})

test_that("a partly prefilled offline row keeps its stored confidences through the pipeline", {
  reset_offline_vocab_gate()
  db <- make_offline_db_fixture(data.frame(
    species = "Testus maximus", FS = "FS6", MB = "MB1", EP = "EP3",
    FS_confidence = 0.8, MB_confidence = 0.6, EP_confidence = 0.7,
    primary_source = "biotic", stringsAsFactors = FALSE))
  real <- lookup_offline_traits
  res <- run_pipeline(list(
    lookup_offline_traits = function(species_name, db_path = db) real(species_name, db_path),
    lookup_worms_traits = worms_taxon("Chordata", "Actinopteri", "Gadiformes"),
    lookup_fishbase_traits = found(list(max_length_cm = 130, trophic_level = 4.4,
                                        depth_min = 150, depth_max = 600))))
  # Stored confidences survive, with the offline source and method.
  expect_identical(res$FS_confidence, 0.8)
  expect_identical(res$MB_confidence, 0.6)
  expect_identical(res$EP_confidence, 0.7)
  for (t in c("FS", "MB", "EP")) {
    expect_identical(res[[paste0(t, "_source")]], "BIOTIC", info = t)
    expect_identical(res[[paste0(t, "_method")]], "observed", info = t)
  }
  # MS was not in the offline row: it comes live from the FishBase size and is scored now.
  expect_identical(res$MS_source, "FishBase")
  expect_false(is.na(res$MS_confidence))
  expect_true(res$MS_confidence > 0 && res$MS_confidence <= 1)
})


# ---------------------------------------------------------------------------
# Task 6 - the cache contract: provenance in the envelope, degraded lookups
# live one day (C3.7, C3.8)
# ---------------------------------------------------------------------------

read_envelope <- function(cache_dir, species = "Testus maximus") {
  readRDS(file.path(cache_dir, paste0(gsub(" ", "_", species), ".rds")))
}

test_that("the envelope's harmonized block carries T, T_source, T_method for every trait (C3.7)", {
  cache_dir <- withr::local_tempdir()
  run_pipeline(list(lookup_worms_traits = worms_taxon("Mollusca", "Bivalvia", max_length_cm = 20)),
               cache_dir = cache_dir)
  env <- read_envelope(cache_dir)
  for (t in TRAIT_COLUMNS) {
    expect_true(all(c(t, paste0(t, "_source"), paste0(t, "_method")) %in% names(env$harmonized)), info = t)
  }
  expect_identical(env$harmonized$MS_method, "observed")
  expect_identical(env$harmonized$phylum, "Mollusca")
  expect_identical(env$harmonized$trait_vocab_version, current_trait_vocab_version())
  expect_false(env$harmonized$degraded)
  expect_false(env$degraded)
  expect_null(env$ttl_days)
})

test_that("a WoRMS failure gives a degraded envelope with a 1-day TTL (C3.7, acceptance 6)", {
  cache_dir <- withr::local_tempdir()
  res <- suppressWarnings(run_pipeline(list(
    lookup_worms_traits = function(...) list(success = FALSE, traits = list(), error = "timeout"),
    lookup_fishbase_traits = found(list(max_length_cm = 100))), cache_dir = cache_dir))
  expect_true(res$degraded)
  f <- file.path(cache_dir, "Testus_maximus.rds")
  env <- readRDS(f)
  expect_true(env$degraded)
  expect_identical(env$ttl_days, 1)
  expect_false(is.null(read_cache_field(f, "traits", config_hash = env$config_hash)))
  # Older than a day: stale. A healthy envelope of the same age is still served.
  env$timestamp <- Sys.time() - 1.5 * 86400
  saveRDS(env, f)
  expect_null(read_cache_field(f, "traits", config_hash = env$config_hash))
  env$ttl_days <- NULL
  saveRDS(env, f)
  expect_false(is.null(read_cache_field(f, "traits", config_hash = env$config_hash)))
})

test_that("the orchestrator degrades the row on a reported lookup error; a local 'not found' does not (C3.7)", {
  failed <- run_pipeline(list(
    lookup_worms_traits = worms_taxon("Mollusca", "Bivalvia", max_length_cm = 20),
    lookup_sealifebase_traits = function(...) list(success = FALSE, traits = list(), error = "HTTP 503")))
  expect_true(failed$degraded)
  fine <- run_pipeline(list(
    lookup_worms_traits = worms_taxon("Mollusca", "Bivalvia", max_length_cm = 20),
    lookup_ontology_traits = function(...) list(success = FALSE, traits = list(),
                                                error = "Species not found in ontology traits database"),
    lookup_biotic_traits = function(...) list(success = FALSE, traits = list(), error = "BIOTIC file not found")))
  expect_false(fine$degraded)
})

test_that("an envelope from an older trait vocabulary is stale (C3.7)", {
  f <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(traits = data.frame(MB = "MB2"), timestamp = Sys.time(), config_hash = "h",
               trait_vocab_version = current_trait_vocab_version() - 1L), f)
  expect_null(read_cache_field(f, "traits", config_hash = "h", vocab_version = current_trait_vocab_version()))
})

test_that("an API lookup that throws warns and reports the error (C3.8)", {
  skip_if_not_installed("httr")
  source(file.path(get_app_root(), "R/functions/trait_lookup/api_trait_databases.R"), local = FALSE)
  testthat::local_mocked_bindings(GET = function(...) stop("connection reset"), .package = "httr")
  expect_warning(res <- lookup_polytraits("Hediste diversicolor"),
                 "\\[lookup_polytraits\\] lookup failed for 'Hediste diversicolor': connection reset")
  expect_identical(res$error, "connection reset")
  expect_false(res$success)
})

test_that("the degraded badge is constant markup and phylo / default sources have colours", {
  source(file.path(get_app_root(), "R/modules/trait_research_server.R"), local = FALSE)
  out <- format_degraded_badge(c(TRUE, FALSE, NA))
  expect_match(out[1], "partial")
  expect_match(out[1], "WoRMS gave no classification or a database could not be reached", fixed = TRUE)
  expect_identical(out[2:3], c("", ""))
  expect_false(identical(source_badge_color("Phylogenetic"), "#9e9e9e"))
  expect_true(nzchar(source_badge_color("Default")))
})


# ---------------------------------------------------------------------------
# Task 7 - relatives and training labels: observed / rule codes only, never
# the target itself (C3.7, F29, F34)
# ---------------------------------------------------------------------------

gadus <- list(phylum = "Chordata", class = "Actinopteri", order = "Gadiformes",
              family = "Gadidae", genus = "Gadus")

# A C-8 envelope whose EP was decided by `ep_method`.
write_relative <- function(cache_dir, species, ep, ep_method, taxonomy = gadus, hash = "h") {
  traits <- data.frame(species = species, EP = ep, EP_source = "X", EP_method = ep_method,
                       stringsAsFactors = FALSE)
  h <- c(list(species = species, EP = ep, EP_method = ep_method), taxonomy)
  saveRDS(list(traits = traits, harmonized = h, species = species, timestamp = Sys.time(),
               config_hash = hash), file.path(cache_dir, paste0(gsub(" ", "_", species), ".rds")))
}

test_that("the target's own file and an ML-coded relative never vote (F29, F34)", {
  source(file.path(get_app_root(), "R/functions/phylogenetic_imputation.R"), local = FALSE)
  cache_dir <- withr::local_tempdir()
  write_relative(cache_dir, "Gadus morhua", "EP1", "observed")      # the target itself
  write_relative(cache_dir, "Gadus macrocephalus", "EP1", "ml")     # an imputed code
  write_relative(cache_dir, "Gadus ogac", "EP3", "default")         # a fall-through code (PR0)
  write_relative(cache_dir, "Gadus chalcogrammus", "EP2", "observed")
  rel <- find_closest_relatives(gadus, list(EP = NA), cache_dir, min_matches = 1,
                                traits_needed = "EP", config_hash = "h", target_species = "Gadus morhua")
  expect_identical(rel$species[!is.na(rel$EP)], "Gadus chalcogrammus")
  expect_false("Gadus morhua" %in% rel$species)
  expect_false("Gadus ogac" %in% rel$species)
})

test_that("a legacy envelope (no T_method) is skipped with a single warning (C3.7)", {
  source(file.path(get_app_root(), "R/functions/phylogenetic_imputation.R"), local = FALSE)
  cache_dir <- withr::local_tempdir()
  for (sp in c("Gadus legacya", "Gadus legacyb")) {
    saveRDS(list(species = sp, harmonized = c(list(species = sp, EP = "EP2"), gadus), timestamp = Sys.time(),
                 config_hash = "h"), file.path(cache_dir, paste0(gsub(" ", "_", sp), ".rds")))
  }
  expect_warning(rel <- find_closest_relatives(gadus, list(EP = NA), cache_dir, min_matches = 1,
                                               traits_needed = "EP", config_hash = "h"),
                 "skipped 2 cache file\\(s\\) without trait provenance")
  expect_identical(nrow(rel), 0L)
})

test_that("min_matches is enforced (C3.7)", {
  source(file.path(get_app_root(), "R/functions/phylogenetic_imputation.R"), local = FALSE)
  cache_dir <- withr::local_tempdir()
  write_relative(cache_dir, "Gadus chalcogrammus", "EP2", "observed")
  write_relative(cache_dir, "Gadus macrocephalus", "EP2", "rule")
  expect_identical(nrow(find_closest_relatives(gadus, list(EP = NA), cache_dir, min_matches = 3,
                                               traits_needed = "EP", config_hash = "h")), 0L)
  expect_identical(nrow(find_closest_relatives(gadus, list(EP = NA), cache_dir, min_matches = 2,
                                               traits_needed = "EP", config_hash = "h")), 2L)
})

test_that("ML training rows use only observed / rule codes (C3.7, F34)", {
  cache_dir <- withr::local_tempdir()
  write_relative(cache_dir, "Gadus chalcogrammus", "EP2", "observed")
  write_relative(cache_dir, "Gadus macrocephalus", "EP1", "phylo")
  saveRDS(list(species = "Gadus legacy", harmonized = c(list(EP = "EP2"), gadus), timestamp = Sys.time()),
          file.path(cache_dir, "Gadus_legacy.rds"))
  expect_warning(rows <- training_rows_from_cache(list.files(cache_dir, full.names = TRUE)),
                 "skipped 1 cache file")
  expect_identical(rows$species, "Gadus chalcogrammus")
  expect_identical(rows$EP, "EP2")
  expect_identical(rows$phylum, "chordata")
})

test_that("the training script runs on the current loader, filters labels and stamps the vocabulary", {
  script <- readLines(file.path(get_app_root(), "scripts/train_trait_models.R"), warn = FALSE)
  code <- script[!startsWith(trimws(script), "#")]
  expect_false(any(grepl('source("R/functions/trait_lookup.R"', code, fixed = TRUE)))
  expect_true(any(grepl('source("R/functions/trait_lookup/load_all.R")', code, fixed = TRUE)))
  expect_true(any(grepl("training_rows_from_cache(cache_files)", code, fixed = TRUE)))
  expect_true(any(grepl("trait_vocab_version = current_trait_vocab_version()", code, fixed = TRUE)))
})


# ---------------------------------------------------------------------------
# Final review fixes - transient failures set result$error; offline RS/TT/ST
# are not overwritten by live sources
# ---------------------------------------------------------------------------

test_that("a live source cannot replace an offline-prefilled RS (final review)", {
  reset_offline_vocab_gate()
  db <- make_offline_db_fixture(data.frame(species = "Testus maximus", RS = "RS2", RS_confidence = 0.6,
                                           primary_source = "cefas", stringsAsFactors = FALSE))
  real <- lookup_offline_traits
  res <- run_pipeline(list(
    lookup_offline_traits = function(species_name, db_path = db) real(species_name, db_path),
    lookup_worms_traits = worms_taxon("Mollusca", "Bivalvia"),
    lookup_cefas_traits = found(list(reproductive_mode = "broadcast spawner"))))
  expect_identical(harmonize_reproductive_strategy("broadcast spawner"), "RS1")
  expect_identical(res$RS, "RS2")
  expect_identical(res$RS_source, "Cefas")
  expect_identical(res$RS_confidence, 0.6)
})

test_that("a FishBase / SeaLifeBase connection error or timeout sets result$error and warns", {
  skip_if_not_installed("rfishbase")
  testthat::local_mocked_bindings(species = function(...) stop("connection refused"), .package = "rfishbase")
  expect_warning(fb <- lookup_fishbase_traits("Gadus morhua"), "fishbase")
  expect_false(is.null(fb$error))
  expect_match(fb$error, "FishBase connection error or timeout")
  expect_false(fb$success)
  expect_warning(sl <- lookup_sealifebase_traits("Mytilus edulis"), "sealifebase")
  expect_match(sl$error, "SeaLifeBase connection error or timeout")

  testthat::local_mocked_bindings(species = function(...) stop("Time limit exceeded"), .package = "rfishbase")
  expect_warning(fb2 <- lookup_fishbase_traits("Gadus morhua"), "fishbase")
  expect_false(is.null(fb2$error))
})

test_that("a FishBase / SeaLifeBase 'not found' (0 rows) leaves result$error NULL", {
  skip_if_not_installed("rfishbase")
  testthat::local_mocked_bindings(species = function(...) data.frame(), .package = "rfishbase")
  expect_no_warning(fb <- lookup_fishbase_traits("Nonexistus speciesus"))
  expect_null(fb$error)
  expect_match(fb$note, "species not found")
  expect_no_warning(sl <- lookup_sealifebase_traits("Nonexistus speciesus"))
  expect_null(sl$error)
  expect_match(sl$note, "not found in SeaLifeBase")
})

test_that("a failing auxiliary FishBase call warns but does not set result$error", {
  skip_if_not_installed("rfishbase")
  testthat::local_mocked_bindings(
    species = function(...) data.frame(Length = 100, Weight = 5000),
    morphology = function(...) stop("morphology down"),
    ecology = function(...) stop("ecology down"),
    .package = "rfishbase")
  w <- testthat::capture_warnings(fb <- lookup_fishbase_traits("Gadus morhua"))
  expect_true(any(grepl("morphology", w)))
  expect_true(any(grepl("ecology", w)))
  expect_null(fb$error)
  expect_true(fb$success)
})

test_that("a freshwaterecology HTTP error sets result$error; a missing key stays note-only", {
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
  with_globals(list(get_api_key = function(...) "k"), {
    testthat::local_mocked_bindings(
      GET = function(...) structure(list(status_code = 503L), class = "response"),
      http_error = function(...) TRUE,
      status_code = function(...) 503L,
      .package = "httr")
    expect_warning(res <- lookup_freshwaterecology_traits("Salmo trutta"), "HTTP error 503")
    expect_match(res$error, "503")
  })
  with_globals(list(get_api_key = function(...) ""), {
    expect_no_warning(res <- lookup_freshwaterecology_traits("Salmo trutta"))
    expect_null(res$error)
    expect_match(res$note, "API key not configured")
  })
})
