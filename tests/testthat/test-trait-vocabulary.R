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
  expect_identical(classify_by_patterns("benthic", "environmental"), "EP3")
  expect_identical(classify_by_patterns("mesopelagic", "environmental"), "EP1")
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
