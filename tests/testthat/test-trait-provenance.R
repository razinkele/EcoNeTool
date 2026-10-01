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
