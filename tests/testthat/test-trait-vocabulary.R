# Sub-project C, PR C-5 (spec C1, C3.5 last bullet, C3.7 last bullet): one
# trait vocabulary. The same MB/EP/PR code used to mean different things in
# the live harmonizer, the fuzzy ontology harmonizer, the offline-DB writer,
# the local BVOL/SpeciesEnriched mappers, the UI legend and the food-web
# model (F17). These tests pin the vocabulary (TRAIT_VOCAB), the pattern
# engine, the taxon rules, the model matrices and the gates that stop codes
# from the old vocabulary being served.

source_app_dependencies()

# Run `code` with `cfg` as this session's harmonization config.
with_session_config <- function(cfg, code) {
  session <- shiny::MockShinySession$new()
  on.exit(session$close(), add = TRUE)
  session$userData$harm_config <- cfg
  shiny::withReactiveDomain(session, code)
}

# ---------------------------------------------------------------------------
# Task 1 - the vocabulary and the pattern engine
# ---------------------------------------------------------------------------

test_that("trait codes and labels come from the vocabulary (MB2 = passive floater)", {
  expect_identical(trait_codes("MB"), paste0("MB", 1:5))
  expect_identical(trait_codes("EP"), paste0("EP", 1:4))
  expect_identical(trait_codes("PR"), paste0("PR", 0:8))
  expect_identical(trait_codes("FS"), paste0("FS", 0:7))
  expect_identical(trait_codes("MS"), paste0("MS", 1:7))
  expect_error(trait_codes("XX"), "trait must be one of")
  expect_identical(unname(trait_definitions()$MB),
                   c("Sessile", "Passive floater / drifter", "Crawler-burrower",
                     "Facultative / limited swimmer", "Obligate swimmer"))
  expect_identical(trait_code_label("PR7"), "Spines / ossicle plates")
  expect_identical(trait_code_label("FS0"), "Primary producer / none")
  expect_true(is.na(trait_code_label("MB9")))
  expect_true(is.na(trait_code_label(NA)))
})

test_that("the config's label and pattern sections are the vocabulary's and pass the validator", {
  expect_identical(HARMONIZATION_CONFIG$mobility_labels, TRAIT_VOCAB$labels$MB)
  expect_identical(HARMONIZATION_CONFIG$environmental_labels, TRAIT_VOCAB$labels$EP)
  expect_identical(HARMONIZATION_CONFIG$size_labels, TRAIT_VOCAB$labels$MS)
  expect_identical(HARMONIZATION_CONFIG$mobility_patterns, TRAIT_VOCAB$patterns$mobility)
  v <- validate_harmonization_config(HARMONIZATION_CONFIG)
  expect_true(v$ok, info = paste(v$errors, collapse = "; "))
})

test_that("pattern precedence lists exactly the pattern codes", {
  for (trait in c("mobility", "environmental", "protection")) {
    codes <- sub("_.*$", "", names(TRAIT_VOCAB$patterns[[trait]]))
    expect_setequal(TRAIT_VOCAB$pattern_precedence[[trait]], codes)
  }
  expect_setequal(TRAIT_VOCAB$pattern_precedence$foraging,
                  sub("_.*$", "", names(HARMONIZATION_CONFIG$foraging_patterns)))
})

test_that("environmental text: leading boundary and precedence (F35, F77, F36)", {
  expect_identical(classify_by_patterns("benthopelagic", "environmental"), "EP2")
  for (zone in c("subtidal", "sublittoral", "intertidal", "littoral", "eulittoral")) {
    expect_true(is.na(classify_by_patterns(zone, "environmental")), info = zone)
  }
  expect_identical(classify_by_patterns("benthic surface", "environmental"), "EP3")
  expect_true(is.na(classify_by_patterns("subsurface deposit", "environmental")))
  expect_identical(classify_by_patterns("burrowing", "environmental"), "EP4")
  expect_identical(classify_by_patterns("mesopelagic", "environmental"), "EP1")
})

test_that("bare 'benthic' / 'benthos' say nothing about EP; taxonomy decides (PR #15, master parity)", {
  # WoRMS functional group "benthos" is appended to the habitat text, and
  # database_lookups derives habitat "benthic" from it. Neither word tells
  # epibenthic from endobenthic, so neither may short-circuit to EP3.
  expect_true(is.na(classify_by_patterns("benthos", "environmental")))
  expect_true(is.na(classify_by_patterns("benthic", "environmental")))
  expect_true(is.na(classify_by_patterns("Benthic", "environmental")))
  expect_identical(classify_by_patterns("benthic surface", "environmental"), "EP3")
  expect_identical(classify_by_patterns("bottom", "environmental"), "EP3")
  expect_identical(classify_by_patterns("seabed", "environmental"), "EP3")
  # Master (6db5082) matched neither word in its EP text rules, had no depth,
  # and fell to the Mollusca/Bivalvia infaunal_bivalves rule: EP4.
  bivalve <- list(phylum = "Mollusca", class = "Bivalvia", order = "Cardiida")
  expect_identical(harmonize_environmental_position(habitat_info = c("benthos", "benthic"),
                                                    taxonomic_info = bivalve), "EP4")
  # Legacy (v1) patterns are historical and untouched.
  expect_identical(get_trait_vocab()$legacy_patterns$environmental$EP3_epibenthic,
                   "epibenthic|epifauna|surface dwelling|on substrate|^surface$")
})

test_that("BIOTIC living-habit labels map to EP4 / EP3", {
  habits <- c("Burrow dwelling" = "EP4", "Attached" = "EP3", "Tube dwelling" = "EP3", "Free living" = "EP3",
              burrower = "EP4", attached = "EP3", tube_dweller = "EP3", free_living = "EP3",
              crevice_dweller = "EP3", epizoic = "EP3")
  for (h in names(habits)) {
    expect_identical(classify_by_patterns(h, "environmental"), habits[[h]], info = h)
  }
})

test_that("protection text: soft shell, soft, exoskeleton, heavy exoskeleton", {
  expect_identical(classify_by_patterns("soft shell", "protection"), "PR5")
  expect_identical(classify_by_patterns("soft", "protection"), "PR0")
  expect_identical(classify_by_patterns("exoskeleton", "protection"), "PR4")
  expect_identical(classify_by_patterns("heavy exoskeleton", "protection"), "PR8")
  expect_identical(classify_by_patterns("chitinous", "protection"), "PR4")
  expect_identical(classify_by_patterns("calcareous_tube", "protection"), "PR2")
  expect_identical(classify_by_patterns("calcium_exoskeleton", "protection"), "PR8")
  expect_identical(classify_by_patterns("mucus", "protection"), "PR1")
})

test_that("mobility text: stems keep matching, specific swimmers beat generic ones", {
  expect_identical(classify_by_patterns("burrowing", "mobility"), "MB3")
  expect_identical(classify_by_patterns("planktonic", "mobility"), "MB2")
  expect_identical(classify_by_patterns("floater", "mobility"), "MB2")
  expect_identical(classify_by_patterns("limited_swimmer", "mobility"), "MB4")
  expect_identical(classify_by_patterns("swimmer", "mobility"), "MB5")
  expect_identical(classify_by_patterns("Sedentary, temporary attachment", "mobility"), "MB1")
})

test_that("NA, NULL and empty text give NA without a warning", {
  expect_no_warning(expect_true(is.na(classify_by_patterns(NULL, "mobility"))))
  expect_no_warning(expect_true(is.na(classify_by_patterns(NA, "environmental"))))
  expect_no_warning(expect_true(is.na(classify_by_patterns(c("", "  "), "protection"))))
})

test_that("an invalid session pattern warns and is skipped", {
  cfg <- HARMONIZATION_CONFIG
  cfg$environmental_patterns$EP4_endobenthic <- "(("
  with_session_config(cfg, {
    expect_warning(res <- classify_by_patterns("burrowing", "environmental"),
                   "\\[harmonization\\] invalid environmental pattern for EP4")
    expect_true(is.na(res))
  })
})

test_that("a pre-C-5 session config cannot drop codes or restore old meanings", {
  old <- HARMONIZATION_CONFIG
  old$mobility_labels <- NULL
  old$environmental_labels <- NULL
  old$size_labels <- NULL
  # The v1 mobility section, as a JSON exported before C-5 carries it.
  old$mobility_patterns <- list(
    MB1_sessile = "sessile|attached|fixed|cemented|anchored|immobile",
    MB2_burrower = "burrow|infauna|endobenthic|burrowing|sediment dweller",
    MB3_crawler = "crawl|creep|benthic|epibenthic|slow moving|sluggish|limited.?movement",
    MB4_swimmer_limited = "slow swim|limited swim|weak swim|drift|plankton|limited.?swim|float",
    MB5_swimmer = "swim|pelagic|nektonic|fast|active|mobile|free-swimming"
  )
  old$environmental_patterns$EP3_epibenthic <- "epibenthic|epifauna|surface dwelling|on substrate|^surface$"
  with_session_config(old, {
    expect_identical(trait_codes("MB"), paste0("MB", 1:5))
    expect_identical(classify_by_patterns("burrowing", "mobility"), "MB3")      # not the old MB2
    expect_identical(classify_by_patterns("planktonic drifter", "mobility"), "MB2")  # not the old MB4
    expect_identical(classify_by_patterns("attached", "environmental"), "EP3") # v2 default, not v1
  })
})

test_that("a genuinely customised pattern is honoured", {
  cfg <- HARMONIZATION_CONFIG
  cfg$environmental_patterns$EP3_epibenthic <- "reef"
  with_session_config(cfg, {
    expect_identical(classify_by_patterns("coral reef", "environmental"), "EP3")
    expect_true(is.na(classify_by_patterns("attached", "environmental")))
  })
})

test_that("the vocabulary cannot be pinned by a JSON config", {
  v <- validate_harmonization_config(list(trait_vocab_version = 1L, pattern_precedence = list()))
  expect_null(v$config$trait_vocab_version)
  expect_null(v$config$pattern_precedence)
})

test_that("harm_config_hash changes when the trait vocabulary changes (F72 contribution)", {
  old <- TRAIT_VOCAB
  withr::defer(assign("TRAIT_VOCAB", old, envir = globalenv()))
  before <- harm_config_hash(HARMONIZATION_CONFIG)
  bumped <- old
  bumped$trait_vocab_version <- old$trait_vocab_version + 1L
  assign("TRAIT_VOCAB", bumped, envir = globalenv())
  expect_false(identical(harm_config_hash(HARMONIZATION_CONFIG), before))
})

test_that("the fuzzy ontology harmonizers use the same vocabulary", {
  ont <- function(category, name, modality) {
    data.frame(trait_category = category, trait_name = name, trait_modality = modality,
               trait_score = 3, ontology_id = NA, source = "test", notes = NA, stringsAsFactors = FALSE)
  }
  expect_identical(harmonize_fuzzy_habitat(ont("habitat", "zone", "benthopelagic"))$class, "EP2")
  expect_true(is.na(harmonize_fuzzy_habitat(ont("habitat", "zone", "intertidal"))$class))
  expect_true(is.na(harmonize_fuzzy_habitat(ont("habitat", "zone", "subtidal"))$class))
  expect_identical(harmonize_fuzzy_mobility(ont("life_history", "mobility", "floater"))$class, "MB2")
  expect_identical(harmonize_fuzzy_mobility(ont("life_history", "mobility", "burrower"))$class, "MB3")
})


# ---------------------------------------------------------------------------
# Task 2 - taxon rules in the live cascades
# ---------------------------------------------------------------------------

test_that("Cnidaria are classified by class (F38)", {
  expect_identical(harmonize_mobility(taxonomic_info = list(phylum = "Cnidaria", class = "Scyphozoa")), "MB2")
  expect_identical(harmonize_mobility(taxonomic_info = list(phylum = "Cnidaria", class = "Anthozoa")), "MB1")
  expect_identical(harmonize_mobility(taxonomic_info = list(phylum = "Cnidaria", class = "Hydrozoa")), "MB1")
  expect_identical(harmonize_mobility(mobility_info = "pelagic",
                                      taxonomic_info = list(phylum = "Cnidaria", class = "Hydrozoa")), "MB2")
  expect_true(is.na(apply_taxon_rules(list(phylum = "Cnidaria", class = "Unknownia"), "mobility")))
})

test_that("echinoderm rules target PR7 / PR1 and outrank text (F38)", {
  expect_identical(harmonize_protection(taxonomic_info = list(phylum = "Echinodermata", class = "Echinoidea")),
                   "PR7")
  expect_identical(harmonize_protection(taxonomic_info = list(phylum = "Echinodermata", class = "Holothuroidea")),
                   "PR1")
  expect_identical(harmonize_protection("calcareous plates",
                                        list(phylum = "Echinodermata", class = "Echinoidea")), "PR7")
  expect_identical(harmonize_protection("calcareous plates"), "PR6")
})

test_that("small arthropods get PR4, Malacostraca PR8", {
  expect_identical(harmonize_protection(taxonomic_info = list(phylum = "Arthropoda", class = "Copepoda")), "PR4")
  expect_identical(harmonize_protection(taxonomic_info = list(phylum = "Arthropoda", class = "Ostracoda")), "PR4")
  expect_identical(harmonize_protection(taxonomic_info = list(phylum = "Arthropoda", class = "Malacostraca")),
                   "PR8")
})

test_that("a copepod at 10-20 m with no habitat text is pelagic (F36)", {
  expect_identical(harmonize_environmental_position(depth_min = 10, depth_max = 20,
                                                    taxonomic_info = list(phylum = "Arthropoda",
                                                                          class = "Copepoda")),
                   "EP1")
})

test_that("the environmental default is epibenthic, as its comment says", {
  expect_identical(harmonize_environmental_position(), "EP3")
})

test_that("a zero-length taxonomy field does not abort the harmonizers (F33)", {
  tax <- list(phylum = "Nematoda", class = character(0))
  expect_no_error(harmonize_protection(NULL, tax))
  expect_no_error(harmonize_mobility(NULL, NULL, tax))
  expect_no_error(harmonize_environmental_position(taxonomic_info = tax))
  expect_no_error(harmonize_mobility(NULL, NULL, list(phylum = "Cnidaria", class = character(0))))
})

test_that("a disabled rule flag switches its taxon rule off", {
  cfg <- HARMONIZATION_CONFIG
  cfg$taxonomic_rules$echinoderms_calcium_plates <- FALSE
  with_session_config(cfg, {
    expect_identical(harmonize_protection("calcareous plates",
                                          list(phylum = "Echinodermata", class = "Echinoidea")), "PR6")
  })
})

test_that("cnidarians_sessile is retired: setting it FALSE warns", {
  cfg <- HARMONIZATION_CONFIG
  cfg$taxonomic_rules$cnidarians_sessile <- FALSE
  expect_warning(validate_harmonization_config(cfg), "cnidarians_sessile is retired")
})


# ---------------------------------------------------------------------------
# Task 3 - the model matrices (F71, F70)
# ---------------------------------------------------------------------------

local({
  source(file.path(get_app_root(), "R/functions/trait_foodweb.R"), local = FALSE)
})

test_that("TRAIT_DEFINITIONS is derived from the vocabulary labels", {
  for (trait in c("MS", "FS", "MB", "EP", "PR")) {
    expect_identical(names(TRAIT_DEFINITIONS[[trait]]), trait_codes(trait), info = trait)
  }
  expect_identical(TRAIT_DEFINITIONS, trait_definitions())
  expect_identical(TRAIT_DEFINITIONS$MB[["MB2"]], "Passive floater / drifter")
})

test_that("PR1 and PR4 rows validate (F71)", {
  d <- data.frame(species = c("a", "b"), MS = "MS3", FS = "FS1", MB = "MB3", EP = "EP3", PR = c("PR1", "PR4"),
                  stringsAsFactors = FALSE)
  expect_true(validate_trait_data(d)$valid)
})

test_that("PR_MS has a PR1 row equal to PR0, and MS1 / MS6 columns copy their neighbours", {
  expect_identical(rownames(PR_MS), trait_codes("PR"))
  expect_identical(PR_MS["PR1", ], PR_MS["PR0", ])
  for (m in list(EP_MS, PR_MS)) {
    expect_identical(colnames(m), paste0("MS", 1:6))
    expect_identical(m[, "MS1"], m[, "MS2"])
    expect_identical(m[, "MS6"], m[, "MS5"])
  }
})

test_that("an MS3 FS6 consumer eating MS1 prey gets the minimum of the five matrix cells (F70)", {
  consumer <- c(MS = "MS3", FS = "FS6", MB = "MB3", EP = "EP3")
  resource <- c(MS = "MS1", MB = "MB2", EP = "EP1", PR = "PR0")
  expected <- min(MS_MS["MS3", "MS1"], FS_MS["FS6", "MS1"], MB_MB["MB3", "MB2"],
                  EP_MS["EP3", "MS1"], PR_MS["PR0", "MS1"])
  expect_equal(calc_interaction_probability(consumer, resource), expected)
  expect_gt(expected, 0.05)
})

test_that("no hard-coded fallback probability is left in calc_interaction_probability (F70)", {
  src <- paste(deparse(body(calc_interaction_probability)), collapse = "\n")
  expect_false(grepl("0\\.05|0\\.5\\b", src))
})

test_that("a pair exactly at the threshold is not linked (strict >)", {
  d <- data.frame(species = c("pred", "prey"), MS = c("MS4", "MS3"), FS = c("FS1", "FS0"),
                  MB = c("MB5", "MB3"), EP = c("EP2", "EP3"), PR = c("PR0", "PR0"), stringsAsFactors = FALSE)
  p <- construct_trait_foodweb(d, threshold = 0, return_probs = TRUE)["pred", "prey"]
  expect_gt(p, 0)
  expect_equal(construct_trait_foodweb(d, threshold = p)["pred", "prey"], 0)
  expect_equal(construct_trait_foodweb(d, threshold = p - 0.01)["pred", "prey"], 1)
  g <- trait_foodweb_to_igraph(d, threshold = p)
  expect_equal(igraph::ecount(g), 0)
})

test_that("at the default threshold a sessile consumer keeps its mobile prey (MB1 row recalibrated)", {
  expect_identical(unname(MB_MB["MB1", c("MB2", "MB3", "MB4", "MB5")]), rep(0.10, 4))
  expect_identical(MB_MB["MB1", "MB1"], 0.95)
  # A mussel-like filter feeder and drifting phytoplankton ("simple" example pair)
  d <- data.frame(species = c("filter_feeder", "phyto"), MS = c("MS3", "MS1"), FS = c("FS6", "FS0"),
                  MB = c("MB1", "MB2"), EP = c("EP2", "EP4"), PR = c("PR6", "PR0"), stringsAsFactors = FALSE)
  expect_equal(construct_trait_foodweb(d)["filter_feeder", "phyto"], 1)
  expect_equal(construct_trait_foodweb(d, return_probs = TRUE)["filter_feeder", "phyto"], 0.10)
})

test_that("there are no self-loops at any threshold", {
  d <- data.frame(species = c("a", "b", "c"), MS = c("MS4", "MS4", "MS3"), FS = c("FS1", "FS3", "FS6"),
                  MB = c("MB5", "MB4", "MB2"), EP = c("EP2", "EP2", "EP1"), PR = c("PR0", "PR1", "PR4"),
                  stringsAsFactors = FALSE)
  for (thr in c(0, 0.05, 0.5)) {
    expect_true(all(diag(construct_trait_foodweb(d, threshold = thr)) == 0), info = thr)
  }
  expect_true(all(diag(construct_trait_foodweb(d, return_probs = TRUE)) == 0))
})

test_that("an unknown code is an error, not a silent floor value", {
  d <- data.frame(species = c("a", "b"), MS = "MS3", FS = "FS1", MB = "MB3", EP = "EP3", PR = c("PR0", "PR9"),
                  stringsAsFactors = FALSE)
  expect_error(construct_trait_foodweb(d), "Invalid PR codes")
})

test_that("the template only uses vocabulary codes", {
  set.seed(1)
  expect_true(validate_trait_data(create_trait_template(30))$valid)
})

# ---------------------------------------------------------------------------
# Task 4 - every other consumer reads the vocabulary
# ---------------------------------------------------------------------------

test_that("BVOL phytoplankton drift (MB2)", {
  h <- harmonize_bvol_traits(list(size_cm = 0.002, trophy = "AU"))
  expect_identical(h$MB, "MB2")
})

test_that("SpeciesEnriched mobility / position / growth form use the vocabulary", {
  h <- harmonize_species_enriched_traits(list(
    size_cm = 3, feeding_method = "filter feeder", mobility = "Crawler or Walker",
    environmental_position = "Epibenthic", body_flexibility = "None (less than 10 degrees)",
    growth_form = "Bivalved"
  ))
  expect_identical(h$MB, "MB3")
  expect_identical(h$EP, "EP3")
  expect_identical(h$PR, "PR6")
  h2 <- harmonize_species_enriched_traits(list(
    size_cm = NA, feeding_method = "", mobility = "Burrower", environmental_position = "Infaunal",
    body_flexibility = "High (greater than 45 degrees)", growth_form = NA
  ))
  expect_identical(h2$MB, "MB3")
  expect_identical(h2$EP, "EP4")
  expect_null(h2[["PR"]])  # [[ ]]: `$PR` would partial-match PR_source
})

test_that("the Trait Research legends are built from the vocabulary", {
  suppressPackageStartupMessages(library(shiny))
  source(file.path(get_app_root(), "R/ui/trait_research_ui.R"), local = FALSE)
  mb <- as.character(trait_vocab_legend_rows("MB"))
  expect_match(mb, "Passive floater / drifter", fixed = TRUE)
  expect_false(grepl("Limited movement", mb, fixed = TRUE))
  ui_src <- readLines(file.path(get_app_root(), "R/ui/trait_research_ui.R"), warn = FALSE)
  expect_false(any(grepl('tags\\$td\\("(MB|EP|PR|FS)[0-9]"\\)', ui_src)))
})

test_that("the help tables are built from the vocabulary and list FS7 and PR1", {
  source(file.path(get_app_root(), "R/functions/trait_help_content.R"), local = FALSE)
  html <- as.character(generate_trait_help_dimensions())
  expect_match(html, "<strong>FS7</strong>", fixed = TRUE)
  expect_match(html, "<strong>PR1</strong>", fixed = TRUE)
  expect_match(html, "Crawler-burrower", fixed = TRUE)
  expect_false(grepl("<td>Limited</td>", html, fixed = TRUE))
})

test_that("the orchestrator's console labels come from the vocabulary", {
  orch <- readLines(file.path(get_app_root(), "R/functions/trait_lookup/orchestrator.R"), warn = FALSE)
  expect_false(any(grepl("(mb|ep|pr)_labels <- c\\(", orch)))
  expect_true(any(grepl("trait_code_label(result$MB)", orch, fixed = TRUE)))
})

# C1.5 static guard: no habitat / mobility / protection vocabulary inside a
# grepl("...") literal in the MB/EP/PR classification code. Returning a code
# ("MB2") stays legal; matching text against a private regex does not.
VOCAB_WORDS <- paste0("burrow|pelagic|benth|tidal|littoral|surface|shell|soft|exoskeleton|spine|",
                      "sessile|drift|swim|crawl|tube|attached")

grepl_literals <- function(code) {
  code <- paste(code, collapse = "\n")
  hits <- regmatches(code, gregexpr('grepl\\(\\s*"[^"]*"', code))[[1]]
  sub('"$', "", sub('^grepl\\(\\s*"', "", hits))
}

test_that("no vocabulary regex is left in the MB/EP/PR cascades (C1.5)", {
  fns <- c("harmonize_mobility_detail", "harmonize_environmental_detail", "harmonize_protection_detail",
           "harmonize_fuzzy_mobility", "harmonize_fuzzy_habitat", "classify_by_patterns", "apply_taxon_rules",
           "harmonize_bvol_traits", "harmonize_species_enriched_traits")
  for (fn in fns) {
    leaks <- grep(VOCAB_WORDS, grepl_literals(deparse(body(get(fn)))), value = TRUE, ignore.case = TRUE)
    expect_identical(leaks, character(0), info = fn)
  }
  build <- readLines(file.path(get_app_root(), "scripts/initialization/build_offline_trait_db.R"), warn = FALSE)
  build <- build[!startsWith(trimws(build), "#")]
  leaks <- grep(VOCAB_WORDS, grepl_literals(build), value = TRUE, ignore.case = TRUE)
  expect_identical(leaks, character(0), info = "build_offline_trait_db.R")
})

# ---------------------------------------------------------------------------
# Task 5 - stale codes are never served (offline DB, cache, ML)
# ---------------------------------------------------------------------------

offline_row <- function(species = "Testus maximus") {
  data.frame(species = species, MS = "MS3", FS = "FS1", MB = "MB2", EP = "EP1", PR = "PR0",
             primary_source = "ontology", stringsAsFactors = FALSE)
}

test_that("an offline DB from another vocabulary is skipped with one warning (C3.5)", {
  reset_offline_vocab_gate()
  withr::defer(reset_offline_vocab_gate())
  db <- make_offline_db_fixture(offline_row(), vocab_version = 1L)
  expect_warning(res <- lookup_offline_traits("Testus maximus", db_path = db),
                 "DB vocab v1 != config v2; rebuild required, offline DB skipped", fixed = TRUE)
  expect_null(res)
  # Once per process: the next lookup is skipped silently.
  expect_no_warning(expect_null(lookup_offline_traits("Testus maximus", db_path = db)))
})

test_that("an offline DB without a vocab stamp (pre-v2 build) is skipped", {
  reset_offline_vocab_gate()
  withr::defer(reset_offline_vocab_gate())
  db <- make_offline_db_fixture(offline_row(), vocab_version = NULL)
  expect_warning(res <- lookup_offline_traits("Testus maximus", db_path = db), "DB vocab vnone")
  expect_null(res)
})

test_that("an offline DB in the current vocabulary is served", {
  reset_offline_vocab_gate()
  db <- make_offline_db_fixture(offline_row(), vocab_version = current_trait_vocab_version())
  res <- expect_no_warning(lookup_offline_traits("Testus maximus", db_path = db))
  expect_identical(res$MB, "MB2")
})

test_that("read_cache_field treats another or a missing vocab version as stale (C3.7)", {
  f <- tempfile(fileext = ".rds")
  withr::defer(unlink(f))
  env <- list(traits = data.frame(MB = "MB2"), timestamp = Sys.time(), config_hash = "h")
  saveRDS(env, f)
  expect_null(read_cache_field(f, "traits", config_hash = "h", vocab_version = 2L))
  env$trait_vocab_version <- 1L
  saveRDS(env, f)
  expect_null(read_cache_field(f, "traits", config_hash = "h", vocab_version = 2L))
  env$trait_vocab_version <- 2L
  saveRDS(env, f)
  expect_equal(read_cache_field(f, "traits", config_hash = "h", vocab_version = 2L)$MB, "MB2")
  # No vocab asked (e.g. classify_species_api envelopes): unchanged behaviour.
  env$trait_vocab_version <- NULL
  saveRDS(env, f)
  expect_equal(read_cache_field(f, "traits")$MB, "MB2")
})

test_that("both orchestrator cache writers stamp trait_vocab_version", {
  orch <- readLines(file.path(get_app_root(), "R/functions/trait_lookup/orchestrator.R"), warn = FALSE)
  orch <- orch[!startsWith(trimws(orch), "#")]
  # Since C-8 both writers build their envelope with build_trait_cache_envelope(),
  # which stamps the version (pinned behaviourally in test-trait-provenance.R).
  expect_equal(sum(grepl("saveRDS(build_trait_cache_envelope(", orch, fixed = TRUE)) +
                 sum(grepl("cache_data <- build_trait_cache_envelope(", orch, fixed = TRUE)), 2L)
  env <- build_trait_cache_envelope(data.frame(species = "x"), list(), config_hash = "h")
  expect_identical(env$trait_vocab_version, current_trait_vocab_version())
})

# Fake randomForest-like models: predict() gives "<trait>2" with probability 0.9.
local_fake_ml_models <- function(env = parent.frame()) {
  registerS3method("predict", "fake_ml_model", function(object, newdata, type = "response", ...) {
    if (identical(type, "prob")) {
      matrix(0.9, nrow = 1, dimnames = list(NULL, object$value))
    } else {
      factor(object$value)
    }
  }, envir = asNamespace("stats"))
  invisible(TRUE)
}

fake_ml_package <- function(vocab_version) {
  models <- lapply(c(MS = "MS", FS = "FS", MB = "MB", EP = "EP", PR = "PR"), function(t) {
    structure(list(value = paste0(t, "2"), forest = list(xlevels = list())), class = "fake_ml_model")
  })
  list(models = models, feature_cols = c("phylum", "class", "order"), performance = list(),
       trait_vocab_version = vocab_version)
}

test_that("an ML model trained before vocab v2 does not predict MB", {
  root <- get_app_root()
  source(file.path(root, "R/functions/ml_trait_prediction.R"), local = FALSE)
  old_load <- load_ml_models
  old_pred <- predict_trait_ml
  withr::defer({
    assign("load_ml_models", old_load, envir = globalenv())
    assign("predict_trait_ml", old_pred, envir = globalenv())
    .ml_cache$vocab_warned <- FALSE
  })
  .ml_cache$vocab_warned <- FALSE
  # The real predict_trait_ml() runs against fake models (so the gate inside it
  # is exercised); load_ml_models() is mocked to return them.
  local_fake_ml_models()
  tax <- list(phylum = "Arthropoda", class = "Malacostraca", order = "Decapoda")

  assign("load_ml_models", function() fake_ml_package(NULL), envir = globalenv())
  expect_warning(p <- predict_missing_traits(list(MS = NA, MB = NA, EP = NA), tax), "MB predictions disabled")
  expect_setequal(names(p), c("MS", "EP", "FS", "PR"))

  assign("load_ml_models", function() fake_ml_package(2L), envir = globalenv())
  expect_no_warning(p2 <- predict_missing_traits(list(MB = NA), tax))
  expect_identical(p2$MB$value, "MB2")
})

test_that("predict_trait_ml() itself refuses MB from a pre-v2 model, warning once (PR #15)", {
  root <- get_app_root()
  source(file.path(root, "R/functions/ml_trait_prediction.R"), local = FALSE)
  withr::defer(.ml_cache$vocab_warned <- FALSE)
  .ml_cache$vocab_warned <- FALSE
  local_fake_ml_models()
  tax <- list(phylum = "Arthropoda", class = "Malacostraca", order = "Decapoda")

  old <- fake_ml_package(NULL)
  w <- testthat::capture_warnings(res <- predict_trait_ml("MB", tax, old))
  expect_null(res)
  expect_length(w, 1L)
  expect_match(w, "MB predictions disabled", fixed = TRUE)
  # Once per process, through the shared flag.
  expect_no_warning(expect_null(predict_trait_ml("MB", tax, old)))
  # Other traits still predict from the same package.
  expect_identical(expect_no_warning(predict_trait_ml("EP", tax, old))$value, "EP2")
  # A v2 model predicts MB.
  expect_identical(expect_no_warning(predict_trait_ml("MB", tax, fake_ml_package(2L)))$value, "MB2")
})

# The build script, run as a child Rscript in a scratch project root holding
# exactly what it sources (same technique as test-rebuild-lock.R).
local_vocab_build_root <- function(env = parent.frame()) {
  root <- tempfile("vocab_build_")
  withr::defer(unlink(root, recursive = TRUE), envir = env)
  app_root <- get_app_root()
  for (rel in c("R/config/harmonization_config.R",
                "R/functions/validation_utils.R",
                "R/functions/trait_lookup/harmonization.R",
                "R/functions/offline_db_rebuild.R",
                "scripts/initialization/build_offline_trait_db.R")) {
    dir.create(file.path(root, dirname(rel)), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(app_root, rel), file.path(root, rel))
  }
  dir.create(file.path(root, "cache"))
  dir.create(file.path(root, "data"))
  writeLines(c(
    "taxon_name,aphia_id,trait_category,trait_name,trait_modality,trait_score",
    "Aurelia aurita,135306,life_history,mobility,floater,3",
    "Aurelia aurita,135306,habitat,zone,pelagic,3"
  ), file.path(root, "data", "ontology_traits.csv"))
  writeLines(c(
    "Species,Max_Length_mm,Longevity_years,Feeding_mode,Living_habit,Mobility,Substratum,Skeleton",
    "Arenicola marina,200,6,deposit_feeder,burrower,burrower,soft_sediment,none",
    "Lanice conchilega,300,2,suspension_feeder,tube_dweller,sessile,soft_sediment,none",
    "Mytilus edulis,100,10,filter_feeder,attached,sessile,hard_substrata,calcium_shell"
  ), file.path(root, "data", "biotic_traits.csv"))
  root
}

test_that("the build writes trait_vocab_version and v2 codes into the DB it installs", {
  skip_if_not_installed("processx")
  skip_if_not_installed("RSQLite")
  root <- local_vocab_build_root()
  res <- processx::run(file.path(R.home("bin"), "Rscript"),
                       args = "scripts/initialization/build_offline_trait_db.R",
                       wd = root, error_on_status = FALSE, timeout = 120,
                       env = c("current", ECONETOOL_REBUILD_LOCK_TOKEN = ""))
  expect_equal(res$status, 0, info = res$stderr)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(root, "cache", "offline_traits.db"))
  withr::defer(DBI::dbDisconnect(con))
  meta <- DBI::dbGetQuery(con, "SELECT value FROM metadata WHERE key = 'trait_vocab_version'")$value
  expect_identical(meta, as.character(current_trait_vocab_version()))
  rows <- DBI::dbGetQuery(con, "SELECT species, MB, EP, PR FROM species_traits ORDER BY species")
  expect_identical(rows$EP[rows$species == "Arenicola marina"], "EP4")
  expect_identical(rows$EP[rows$species == "Lanice conchilega"], "EP3")
  expect_identical(rows$EP[rows$species == "Mytilus edulis"], "EP3")
  expect_identical(rows$MB[rows$species == "Arenicola marina"], "MB3")
  expect_identical(rows$MB[rows$species == "Aurelia aurita"], "MB2")
  expect_identical(rows$EP[rows$species == "Aurelia aurita"], "EP1")
})

# ---------------------------------------------------------------------------
# Final fix wave (I1, F2-F6)
# ---------------------------------------------------------------------------

test_that("FishBase 'bathydemersal' is benthopelagic (EP2), like 'demersal' (I1)", {
  expect_identical(classify_by_patterns("bathydemersal", "environmental"), "EP2")
  expect_identical(classify_by_patterns("Bathydemersal; marine; depth range 200 - 1000 m", "environmental"), "EP2")
  expect_identical(classify_by_patterns("demersal", "environmental"), "EP2")
})

test_that("taxon-rule text conditions respect the leading word boundary (F2)", {
  hydrozoa <- list(phylum = "Cnidaria", class = "Hydrozoa")
  # "benthopelagic" has no mobility pattern (no swim/drift/...), so the taxon
  # rules decide. The medusa/pelagic Hydrozoa rule must NOT fire on the
  # embedded "pelagic"; the next rule (Hydrozoa polyps) gives MB1.
  expect_identical(classify_by_patterns("benthopelagic", "mobility"), NA_character_)
  expect_identical(apply_taxon_rules(hydrozoa, "mobility", text = "benthopelagic"), "MB1")
  expect_identical(harmonize_mobility("benthopelagic", taxonomic_info = hydrozoa), "MB1")
  # The same wrap guards the environmental_pelagic Hydrozoa rule.
  expect_identical(apply_taxon_rules(hydrozoa, "environmental_pelagic", text = "benthopelagic"), NA_character_)
  # Real "medusa" / "pelagic" text still triggers the rule.
  expect_identical(apply_taxon_rules(hydrozoa, "mobility", text = "medusa stage"), "MB2")
  expect_identical(apply_taxon_rules(hydrozoa, "mobility", text = "pelagic"), "MB2")
  expect_identical(apply_taxon_rules(hydrozoa, "environmental_pelagic", text = "pelagic medusa"), "EP1")
})

test_that("ichthyoplankton is pelagic (EP1) (F3)", {
  expect_identical(classify_by_patterns("ichthyoplankton", "environmental"), "EP1")
})

test_that("'nekton' is a mobility word only, not an environmental position (F3)", {
  expect_identical(classify_by_patterns("nekton", "environmental"), NA_character_)
  expect_identical(classify_by_patterns("nekton", "mobility"), "MB5")
  # The orchestrator appends the WoRMS functional group ("nekton") to the
  # habitat text. A deep demersal gadoid must keep EP2 (as on master), not be
  # forced to EP1 before the depth and fish-order rules run.
  cod <- list(phylum = "Chordata", class = "Actinopteri", order = "Gadiformes")
  expect_identical(harmonize_environmental_position(depth_min = 100, depth_max = 400,
                                                    habitat_info = "nekton", taxonomic_info = cod), "EP2")
  expect_identical(harmonize_environmental_position(habitat_info = "nekton", taxonomic_info = cod), "EP2")
  # Legacy (v1) patterns are historical and untouched.
  expect_identical(get_trait_vocab()$legacy_patterns$environmental$EP1_pelagic,
                   "pelagic|water column|planktonic|nektonic|open water")
})

test_that("species with missing trait codes raise ONE warning and leave other links unchanged (F4)", {
  d <- data.frame(species = c("pred", "prey", "ghost"), MS = c("MS4", "MS3", "MS3"),
                  FS = c("FS1", "FS0", "FS3"), MB = c("MB5", "MB3", "MB4"),
                  EP = c("EP2", "EP3", NA), PR = c("PR0", "PR0", "PR0"), stringsAsFactors = FALSE)
  w <- testthat::capture_warnings(adj <- construct_trait_foodweb(d, threshold = 0))
  expect_length(w, 1L)
  # EP is used in both roles, so the ghost can neither be eaten nor eat.
  expect_match(w, paste0("1 species cannot be eaten: ghost (missing EP); ",
                         "1 species cannot eat: ghost (missing EP)"), fixed = TRUE)
  expect_true(all(adj["ghost", ] == 0))
  expect_true(all(adj[, "ghost"] == 0))
  complete <- d[d$species != "ghost", ]
  expect_no_warning(ref <- construct_trait_foodweb(complete, threshold = 0))
  expect_identical(adj[c("pred", "prey"), c("pred", "prey")], ref)
  # An NA in MS (which used to throw inside the pair loop) still gives one warning.
  d2 <- d
  d2$EP[3] <- "EP3"
  d2$MS[3] <- NA
  expect_length(testthat::capture_warnings(construct_trait_foodweb(d2)), 1L)
})

test_that("a consumer with only PR missing still eats but cannot be eaten (F4, role-aware)", {
  d <- data.frame(species = c("pred", "prey", "top"), MS = c("MS4", "MS3", "MS5"),
                  FS = c("FS1", "FS0", "FS1"), MB = c("MB5", "MB3", "MB5"),
                  EP = c("EP2", "EP3", "EP2"), PR = c(NA, "PR0", "PR0"), stringsAsFactors = FALSE)
  ref_d <- d
  ref_d$PR[1] <- "PR0"
  expect_no_warning(ref <- construct_trait_foodweb(ref_d, threshold = 0))
  skip_if(ref["top", "pred"] == 0, "reference table has no top -> pred link; pick other traits")
  w <- testthat::capture_warnings(adj <- construct_trait_foodweb(d, threshold = 0))
  expect_length(w, 1L)
  # Only the traits actually missing are listed (PR #15), not the role's full set.
  expect_match(w, "1 species cannot be eaten: pred (missing PR)", fixed = TRUE)
  expect_false(grepl("MS/MB/EP/PR", w, fixed = TRUE))
  expect_false(grepl("cannot eat", w, fixed = TRUE))
  # PR is only read for resources: pred keeps every consumer link ...
  expect_identical(adj["pred", ], ref["pred", ])
  expect_equal(adj["pred", "prey"], 1)
  # ... but nothing eats it; the other links are unchanged.
  expect_true(all(adj[, "pred"] == 0))
  expect_identical(adj[c("prey", "top"), c("prey", "top")], ref[c("prey", "top"), c("prey", "top")])
})

test_that("a resource with only FS missing is still eaten but eats nothing (F4, role-aware)", {
  d <- data.frame(species = c("pred", "prey"), MS = c("MS4", "MS3"), FS = c("FS1", NA),
                  MB = c("MB5", "MB3"), EP = c("EP2", "EP3"), PR = c("PR0", "PR0"), stringsAsFactors = FALSE)
  w <- testthat::capture_warnings(adj <- construct_trait_foodweb(d, threshold = 0))
  expect_length(w, 1L)
  expect_match(w, "1 species cannot eat: prey (missing FS)", fixed = TRUE)
  expect_false(grepl("cannot be eaten", w, fixed = TRUE))
  expect_equal(adj["pred", "prey"], 1)
  expect_true(all(adj["prey", ] == 0))
})

test_that("a complete trait table builds without warnings (F4)", {
  d <- data.frame(species = c("pred", "prey"), MS = c("MS4", "MS3"), FS = c("FS1", "FS0"),
                  MB = c("MB5", "MB3"), EP = c("EP2", "EP3"), PR = c("PR0", "PR0"), stringsAsFactors = FALSE)
  expect_no_warning(construct_trait_foodweb(d))
  expect_no_warning(construct_trait_foodweb(d, return_probs = TRUE))
})

test_that("offline_db_vocab_status flags a DB in another vocabulary as needing a rebuild (F5)", {
  v1 <- offline_db_vocab_status(make_offline_db_fixture(offline_row(), vocab_version = 1L))
  expect_false(v1$ok)
  expect_identical(v1$version, "1")
  expect_identical(v1$message, sprintf("Rebuild required (trait vocabulary v1, app uses v%s)",
                                       current_trait_vocab_version()))

  ok <- offline_db_vocab_status(make_offline_db_fixture(offline_row(), vocab_version = 2L))
  expect_true(ok$ok)
  expect_identical(ok$version, "2")
  expect_identical(ok$message, "Available")

  unstamped <- offline_db_vocab_status(make_offline_db_fixture(offline_row(), vocab_version = NULL))
  expect_false(unstamped$ok)
  expect_identical(unstamped$version, "none")
  expect_match(unstamped$message, "trait vocabulary none,", fixed = TRUE)

  no_meta <- make_offline_db_fixture(offline_row(), vocab_version = 2L)
  con <- DBI::dbConnect(RSQLite::SQLite(), no_meta)
  DBI::dbExecute(con, "DROP TABLE metadata")
  DBI::dbDisconnect(con)
  nm <- offline_db_vocab_status(no_meta)
  expect_false(nm$ok)
  expect_identical(nm$version, "none")
  expect_match(nm$message, "^Rebuild required")
})

test_that("the DB status panel reads the vocab status and styles a stale DB as a warning (F5)", {
  src <- readLines(file.path(get_app_root(), "R/modules/trait_research_server.R"), warn = FALSE)
  expect_true(any(grepl("offline_db_vocab_status(", src, fixed = TRUE)))
})

test_that("the orchestrator's FS console labels come from the vocabulary (F6)", {
  orch <- readLines(file.path(get_app_root(), "R/functions/trait_lookup/orchestrator.R"), warn = FALSE)
  expect_false(any(grepl("fs_labels", orch, fixed = TRUE)))
  expect_false(any(grepl("\"Xylophagous\"", orch, fixed = TRUE)))
})
