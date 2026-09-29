# Sub-project C, PR C-6a (spec C2.1-C2.7): the trait lookups return correct
# raw values. A WoRMS classification without a Class rank aborted the whole
# Trait Research batch (F33), WoRMS body sizes ignored the unit WoRMS gives
# with them (F32), FishBase weights were multiplied by 1000 (F78), the
# AlgaeBase fallback indexed a tibble like a list (F79), a WoRMS
# classification was always "low" (F9), the first substring common-name
# match was "high" and its cache ignored the region (F10), and binomials
# were singularised before they were looked up (F11).
# Every test here is offline: the network calls are mocked.

source_app_dependencies()

# ---------------------------------------------------------------------------
# Fixtures shaped like the live responses (probed 2026-09-29)
# ---------------------------------------------------------------------------

# worrms::wm_attr_data(): one row per measurement; the unit (and deeper
# qualifiers such as Type / Dimension) sit in the `children` list column.
worms_attr <- function(type, value, unit = NA_character_) {
  unit <- rep_len(unit, length(type))
  out <- tibble::tibble(AphiaID = 1L, measurementType = type, measurementValue = as.character(value))
  out$children <- lapply(unit, function(u) {
    if (is.na(u)) return(data.frame())
    data.frame(measurementType = "Unit", measurementValue = u, stringsAsFactors = FALSE)
  })
  out
}

# "micrometre" as WoRMS writes it, with the micro sign (kept out of the source
# as a literal so the file stays ASCII).
micro_m <- paste0(intToUtf8(0xB5), "m")

# One AphiaRecordsByName row (worms_records_by_name_http()).
worms_record <- function(name) {
  data.frame(AphiaID = 1L, scientificname = name, rank = "Species", isMarine = 1L,
             isBrackish = 0L, isFreshwater = 0L, stringsAsFactors = FALSE)
}

# Run lookup_worms_traits() against mocked WoRMS responses. `ranks` is a named
# character vector rank -> name, as worrms::wm_classification() returns it.
mock_worms_lookup <- function(name, ranks, attrs) {
  testthat::local_mocked_bindings(
    wm_classification = function(id, ...) {
      data.frame(rank = names(ranks), scientificname = unname(ranks), stringsAsFactors = FALSE)
    },
    wm_attr_data = function(id, ...) attrs,
    .package = "worrms"
  )
  with_mocked_function(globalenv(), "worms_records_by_name_http", function(...) worms_record(name),
                       lookup_worms_traits(name))
}

# ---------------------------------------------------------------------------
# F33 - zero-length WoRMS ranks
# ---------------------------------------------------------------------------

test_that(".scalar_chr turns a missing rank into NA and keeps the first value", {
  expect_identical(.scalar_chr(character(0)), NA_character_)
  expect_identical(.scalar_chr(NULL), NA_character_)
  expect_identical(.scalar_chr(NA), NA_character_)
  expect_identical(.scalar_chr(""), NA_character_)
  expect_identical(.scalar_chr(c("Bivalvia", "Other")), "Bivalvia")
  expect_identical(.scalar_chr(list("Mollusca")), "Mollusca")
})

test_that("a WoRMS classification without a Class rank does not abort the lookup (F33)", {
  skip_if_not_installed("worrms")
  expect_warning(
    res <- mock_worms_lookup("Enoplus brevis",
                             c(Kingdom = "Animalia", Phylum = "Nematoda", Order = "Enoplida"),
                             worms_attr("Body size", 5)),
    "no unit for 'Enoplus brevis' body size; assuming mm from class"
  )
  expect_true(res$success)
  expect_null(res$error)
  expect_identical(res$traits$phylum, "Nematoda")
  expect_identical(res$traits$class, NA_character_)
  expect_identical(res$traits$order, "Enoplida")
  expect_equal(res$traits$max_length_cm, 0.5)
  expect_identical(res$traits$size_unit_source, "class_heuristic")

  # The NA rank flows through every downstream consumer without an error.
  expect_no_error(route_trait_databases(res$traits$phylum, res$traits$class, 1L, TRUE))
  expect_no_error(harmonize_mobility(NULL, NULL, res$traits))
  expect_no_error(harmonize_environmental_position(taxonomic_info = res$traits))
  expect_no_error(harmonize_protection(NULL, res$traits))
  source(file.path(get_app_root(), "R/functions/phylogenetic_imputation.R"), local = TRUE)
  expect_no_error(calculate_taxonomic_distance(res$traits, list(phylum = "Nematoda", class = "Enoplea")))
})

# ---------------------------------------------------------------------------
# F32 - WoRMS body-size units
# ---------------------------------------------------------------------------

test_that("to_cm converts lengths and refuses non-length units", {
  expect_equal(to_cm(c(1, 1, 1, 1, 1, 1), c("mm", "cm", "m", micro_m, "um", "kg")),
               c(0.1, 1, 100, 1e-4, 1e-4, NA))
  expect_equal(to_cm(20, "CM "), 20)
  expect_true(is.na(to_cm(20, NA_character_)))
})

test_that("a WoRMS body size is read in the unit of its child record (F32, Mytilus)", {
  skip_if_not_installed("worrms")
  res <- mock_worms_lookup("Mytilus edulis",
                           c(Phylum = "Mollusca", Class = "Bivalvia", Order = "Mytilida"),
                           worms_attr(rep("Body size", 3), c(5.1, 10, 20), "cm"))
  expect_true(res$success)
  expect_equal(res$traits$max_length_cm, 20)
  expect_identical(res$traits$size_unit_source, "child")
})

test_that("without a unit child the class heuristic applies, with a warning (F32)", {
  attrs <- worms_attr("Body size", 20)
  expect_warning(size <- worms_body_size_cm(attrs, "Bivalvia", "Mytilus edulis"), "no unit")
  expect_equal(size$max_length_cm, 2)
  expect_identical(size$size_unit_source, "class_heuristic")
  expect_warning(fish <- worms_body_size_cm(attrs, "Teleostei", "Gadus morhua"), "assuming cm")
  expect_equal(fish$max_length_cm, 20)
})

test_that("the qualitative body-size row gives the unit before the class heuristic (F32)", {
  attrs <- worms_attr(c("Body size", "Body size (qualitative)"), c("20", "up to 20 cm"))
  expect_no_warning(size <- worms_body_size_cm(attrs, "Bivalvia", "Mytilus edulis"))
  expect_equal(size$max_length_cm, 20)
  expect_identical(size$size_unit_source, "qualitative")
})

test_that("weight rows (kg) never become a length (F32, Phoca)", {
  attrs <- worms_attr(rep("Body size", 3), c(186, 250, 160), c("cm", "kg", "cm"))
  size <- worms_body_size_cm(attrs, "Mammalia", "Phoca vitulina")
  expect_equal(size$max_length_cm, 186)
  expect_null(worms_body_size_cm(worms_attr("Body size", 87, "kg"), "Mammalia", "Phoca vitulina"))
  expect_null(worms_body_size_cm(worms_attr("Functional group", "benthos"), "Bivalvia", "Mytilus edulis"))
  expect_null(worms_body_size_cm(NULL, "Bivalvia", "Mytilus edulis"))
})

test_that("a microscopic size in micrometres converts to cm (F32)", {
  size <- worms_body_size_cm(worms_attr("Body size", 50, micro_m), "Bacillariophyceae", "Skeletonema costatum")
  expect_equal(size$max_length_cm, 0.005)
  expect_identical(size$size_unit_source, "child")
})

# ---------------------------------------------------------------------------
# F78 - FishBase weight is already in grams
# ---------------------------------------------------------------------------

test_that("FishBase Weight is stored in grams, not multiplied by 1000 (F78)", {
  skip_if_not_installed("rfishbase")
  testthat::local_mocked_bindings(
    species = function(species_list, ...) data.frame(Length = 200, Weight = 96000),
    morphology = function(species_list, ...) NULL,
    ecology = function(species_list, ...) data.frame(FoodTroph = 4.4),
    .package = "rfishbase"
  )
  res <- lookup_fishbase_traits("Gadus morhua")
  expect_true(res$success)
  expect_equal(res$traits$max_weight_g, 96000)
  expect_equal(res$traits$max_length_cm, 200)
})

# ---------------------------------------------------------------------------
# F79 - AlgaeBase fallback reads the WoRMS tibble by column
# ---------------------------------------------------------------------------

test_that("the AlgaeBase fallback reads phylum and class from the WoRMS tibble (F79)", {
  skip_if_not_installed("worrms")
  testthat::local_mocked_bindings(
    wm_records_name = function(name, ...) {
      tibble::tibble(AphiaID = 149098L, phylum = "Ochrophyta", class = "Bacillariophyceae")
    },
    .package = "worrms"
  )
  res <- lookup_algaebase_traits("Skeletonema costatum")
  expect_null(res$error)
  expect_true(res$success)
  expect_identical(res$traits$phylum, "Ochrophyta")
  expect_identical(res$traits$class, "Bacillariophyceae")
})

test_that("an AlgaeBase fallback failure warns and is recorded (F79)", {
  skip_if_not_installed("worrms")
  testthat::local_mocked_bindings(
    wm_records_name = function(name, ...) stop("(500) Internal Server Error"),
    .package = "worrms"
  )
  expect_warning(res <- lookup_algaebase_traits("Skeletonema costatum"),
                 "\\[algaebase\\] WoRMS fallback failed for 'Skeletonema costatum'")
  expect_identical(res$error, "(500) Internal Server Error")
  expect_false(res$success)
})

# rfishbase name-resolution mocks. `common` maps a lower-case query to the
# rows common_to_sci() returns; `sci` lists names validate_names() accepts;
# `region_species` are the species whose ecosystems include the Baltic Sea.
# Returns an environment that records every call.
mock_fishbase_names <- function(common = list(), sci = character(), region_species = character(),
                                env = parent.frame()) {
  calls <- new.env()
  calls$validate <- character()
  calls$common <- character()
  calls$region <- character()
  testthat::local_mocked_bindings(
    validate_names = function(species_list, ...) {
      calls$validate <- c(calls$validate, species_list)
      if (species_list %in% sci) species_list else NA_character_
    },
    common_to_sci = function(x, ...) {
      calls$common <- c(calls$common, x)
      rows <- common[[tolower(x)]]
      if (is.null(rows)) data.frame(Species = character(), ComName = character()) else rows
    },
    faoareas = function(species_list, ...) {
      calls$region <- c(calls$region, species_list)
      data.frame(Species = species_list, FAO = "Pacific, Northwest")
    },
    ecosystem = function(species_list, ...) {
      data.frame(Species = species_list,
                 EcosystemName = if (species_list %in% region_species) "Baltic Sea" else "Sea of Okhotsk")
    },
    .package = "rfishbase",
    .env = env
  )
  calls
}

common_rows <- function(species, comname) {
  data.frame(Species = species, ComName = comname, Language = "English", stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# F9 - a WoRMS classification is "medium"
# ---------------------------------------------------------------------------

mussel_worms <- function(...) {
  list(aphia_id = 140480L, scientific_name = "Mytilus edulis", rank = "Species",
       phylum = "Mollusca", class = "Bivalvia", order = "Mytilida", family = "Mytilidae")
}

test_that("a WoRMS classification is medium confidence (F9)", {
  res <- with_mocked_function(globalenv(), "query_worms", mussel_worms,
    classify_species_api("Mytilus edulis", functional_group_hint = "Benthos", use_cache = FALSE))
  expect_identical(res$functional_group, "Benthos")
  expect_identical(res$source, "WoRMS")
  expect_identical(res$confidence, "medium")
})

test_that("a name-based override and the unmatched-class default stay low (F9)", {
  res <- with_mocked_function(globalenv(), "query_worms", mussel_worms,
    classify_species_api("Mussel worm", functional_group_hint = "Benthos", use_cache = FALSE))
  expect_identical(res$confidence, "medium")  # "worm" only overrides a Fish verdict

  nematode <- function(...) list(aphia_id = 1L, phylum = "Nematoda", class = "Enoplea")
  res <- with_mocked_function(globalenv(), "query_worms", nematode,
    classify_species_api("Enoplus brevis", functional_group_hint = "Benthos", use_cache = FALSE))
  expect_identical(res$functional_group, "Fish")  # classify_by_taxonomy's fallback, not WoRMS evidence
  expect_identical(res$confidence, "low")

  cod_as_benthos <- function(...) list(aphia_id = 1L, phylum = "Mollusca", class = "Bivalvia")
  res <- with_mocked_function(globalenv(), "query_worms", cod_as_benthos,
    classify_species_api("Baltic cod", functional_group_hint = "Benthos", use_cache = FALSE))
  expect_identical(res$functional_group, "Fish")
  expect_identical(res$confidence, "low")
})

test_that("assign_functional_group_enhanced keeps a WoRMS classification (F9)", {
  withr::local_dir(withr::local_tempdir())  # classify_species_api caches under ./cache/taxonomy
  res <- with_mocked_function(globalenv(), "query_fishbase", function(...) NULL,
    with_mocked_function(globalenv(), "query_worms", mussel_worms,
      assign_functional_group_enhanced("Xyzzy obscura", use_api = TRUE)))
  expect_identical(res, "Benthos")
})

# ---------------------------------------------------------------------------
# F10 - common names: exact match first, graded confidence, region-keyed cache
# ---------------------------------------------------------------------------

test_that("an exact common-name match wins and is high confidence (F10)", {
  skip_if_not_installed("rfishbase")
  mock_fishbase_names(common = list(
    "atlantic cod" = common_rows(c("Gadus morhua", "Lepidion lepidion"), c("Atlantic cod", "North Atlantic codling"))
  ))
  res <- resolve_fishbase_name("Atlantic cod")
  expect_identical(res$valid_name, "Gadus morhua")
  expect_identical(res$match_confidence, "high")
})

test_that("one species behind several common-name rows is still one candidate (F10)", {
  skip_if_not_installed("rfishbase")
  mock_fishbase_names(common = list(
    "antenna codlet" = common_rows(rep("Bregmaceros atlanticus", 2), c("Antenna codlet", "Antenna Codlet"))
  ))
  res <- resolve_fishbase_name("Antenna codlet")
  expect_identical(res$valid_name, "Bregmaceros atlanticus")
  expect_identical(res$match_confidence, "high")
})

test_that("an ambiguous common name is low confidence, deterministic, and warns (F10)", {
  skip_if_not_installed("rfishbase")
  mock_fishbase_names(common = list(
    cod = common_rows(c("Gadus macrocephalus", "Eleginus nawaga"), c("Alaska cod", "Arctic cod"))
  ))
  expect_warning(res <- resolve_fishbase_name("cod"), "\\[fishbase\\] 'cod' ambiguous: 2 candidates")
  expect_identical(res$valid_name, "Eleginus nawaga")  # first by Species sort order
  expect_identical(res$match_confidence, "low")
})

test_that("exactly one candidate in the region is medium confidence (F10)", {
  skip_if_not_installed("rfishbase")
  mock_fishbase_names(
    common = list(cod = common_rows(c("Gadus macrocephalus", "Eleginus nawaga", "Gadus morhua"),
                                    c("Alaska cod", "Arctic cod", "Baltic cod"))),
    region_species = "Gadus morhua"
  )
  res <- resolve_fishbase_name("cod", geographic_region = "Baltic")
  expect_identical(res$valid_name, "Gadus morhua")
  expect_identical(res$match_confidence, "medium")
})

test_that("two candidates in the region stay low (F10)", {
  skip_if_not_installed("rfishbase")
  mock_fishbase_names(
    common = list(cod = common_rows(c("Gadus macrocephalus", "Gadus morhua"), c("Alaska cod", "Baltic cod"))),
    region_species = c("Gadus macrocephalus", "Gadus morhua")
  )
  expect_warning(res <- resolve_fishbase_name("cod", geographic_region = "Baltic"), "ambiguous: 2 candidates")
  expect_identical(res$match_confidence, "low")
})

test_that("a broad common name is low without one region query per candidate (F10)", {
  skip_if_not_installed("rfishbase")
  species <- sprintf("Genus%02d species", 1:30)
  calls <- mock_fishbase_names(common = list(cod = common_rows(species, paste(species, "cod"))),
                               region_species = species[1])
  expect_warning(res <- resolve_fishbase_name("cod", geographic_region = "Baltic"), "ambiguous: 30 candidates")
  expect_identical(res$match_confidence, "low")
  expect_length(calls$region, 0)
})

test_that("the classification cache key carries the region and FishBase confidence is passed on (F10)", {
  cache_dir <- withr::local_tempdir()
  fb <- function(...) {
    list(avg_weight_g = NA, max_weight_g = NA, trophic_level = NA, habitat = NA,
         min_depth_m = NA, max_depth_m = NA, match_confidence = "low")
  }
  res <- with_mocked_function(globalenv(), "query_fishbase", fb,
    classify_species_api("Atlantic cod", geographic_region = "Baltic Sea", cache_dir = cache_dir))
  expect_identical(res$confidence, "low")
  expect_true(file.exists(file.path(cache_dir, "Atlantic_cod__Baltic_Sea.classify.rds")))
  with_mocked_function(globalenv(), "query_fishbase", fb,
    classify_species_api("Atlantic cod", cache_dir = cache_dir))
  expect_true(file.exists(file.path(cache_dir, "Atlantic_cod__any.classify.rds")))
})

# ---------------------------------------------------------------------------
# F11 - the original name first, the singular form only as a fallback
# ---------------------------------------------------------------------------

test_that("a binomial resolves as itself and is never singularised (F11)", {
  skip_if_not_installed("rfishbase")
  calls <- mock_fishbase_names(sci = "Pollachius virens")
  res <- resolve_fishbase_name("Pollachius virens")
  expect_identical(res$valid_name, "Pollachius virens")
  expect_identical(res$match_confidence, "high")
  expect_identical(calls$validate, "Pollachius virens")
  expect_false(any(grepl("viren$", c(calls$validate, calls$common))))
})

test_that("a genus-like name resolves before any singular form is tried (F11)", {
  skip_if_not_installed("rfishbase")
  calls <- mock_fishbase_names(common = list(ammodytes = common_rows("Ammodytes tobianus", "Ammodytes")))
  res <- resolve_fishbase_name("Ammodytes")
  expect_identical(res$valid_name, "Ammodytes tobianus")
  expect_false("Ammodyte" %in% c(calls$validate, calls$common))
})

test_that("an English plural falls back to its singular form (F11)", {
  skip_if_not_installed("rfishbase")
  calls <- mock_fishbase_names(common = list(
    sandeel = common_rows(c("Ammodytes marinus", "Ammodytes tobianus"), c("Lesser sandeel", "Lesser sandeel"))
  ))
  expect_warning(res <- resolve_fishbase_name("Sandeels"), "ambiguous: 2 candidates")
  expect_identical(res$valid_name, "Ammodytes marinus")
  expect_identical(calls$validate, c("Sandeels", "Sandeel"))
  expect_identical(calls$common, c("Sandeels", "Sandeel"))
})

test_that("a name nothing resolves returns NULL (F11)", {
  skip_if_not_installed("rfishbase")
  mock_fishbase_names()
  expect_null(resolve_fishbase_name("Nonexistus fictitious"))
})

# ---------------------------------------------------------------------------
# F33 - one failing species does not abort a Trait Research batch
# ---------------------------------------------------------------------------

test_that("lookup_species_traits_safely turns an error into a warning and an error row (F33)", {
  boom <- function(species_name, ...) stop("argument is of length zero")
  expect_warning(
    row <- with_mocked_function(globalenv(), "lookup_species_traits", boom,
                                lookup_species_traits_safely("Enoplus brevis")),
    "\\[trait_research\\] lookup failed for 'Enoplus brevis': argument is of length zero"
  )
  expect_identical(row$species, "Enoplus brevis")
  expect_identical(row$error, "argument is of length zero")
  for (trait in c("MS", "FS", "MB", "EP", "PR", "RS", "TT", "ST")) {
    expect_true(is.na(row[[trait]]), info = trait)
  }
})

# The Trait Research server as a testServer() app (the pattern of
# test-rebuild-observer.R), run in a temp working directory because the
# lookup observer creates ./cache/taxonomy.
local_trait_research_module <- function(env = parent.frame()) {
  skip_if_not_installed("plotly")
  skip_if_not_installed("DT")
  skip_if_not_installed("bs4Dash")
  withr::local_package("bs4Dash", .local_envir = env)
  for (f in c("R/functions/admin_auth.R", "R/functions/offline_db_rebuild.R",
              "R/modules/trait_research_server.R")) {
    source(file.path(get_app_root(), f), local = FALSE)
  }
  withr::local_dir(withr::local_tempdir(.local_envir = env), .local_envir = env)
  mod <- trait_research_server
  formals(mod)$shared_data <- NULL
  mod
}

fake_lookup <- function(species_name, ...) {
  if (grepl("^Bad", species_name)) stop("argument is of length zero")
  data.frame(species = species_name, MS = "MS4", FS = "FS1", MB = "MB5", EP = "EP2", PR = "PR0",
             RS = NA_character_, TT = NA_character_, ST = NA_character_, source = "FishBase",
             stringsAsFactors = FALSE)
}

test_that("a Trait Research batch survives one species whose lookup throws (F33, testServer)", {
  mod <- local_trait_research_module()
  with_mocked_function(globalenv(), "lookup_species_traits", fake_lookup, {
    shiny::testServer(mod, {
      session$setInputs(trait_research_input_method = "manual",
                        trait_research_species_list = "Good one\nBad one\nGood two",
                        trait_research_databases = "worms")
      expect_warning(session$setInputs(trait_research_run_lookup = 1),
                     "\\[trait_research\\] lookup failed for 'Bad one'")
      res <- rv$trait_results
      expect_identical(res$species, c("Good one", "Bad one", "Good two"))
      expect_identical(res$MS, c("MS4", NA, "MS4"))
      expect_identical(res$error, c(NA, "argument is of length zero", NA))
      expect_identical(rv$raw_results[["Bad one"]]$error, "argument is of length zero")
      expect_false(rv$lookup_in_progress)
    })
  })
})

test_that("a batch whose only species fails still renders its table and summary (F33, testServer)", {
  mod <- local_trait_research_module()
  with_mocked_function(globalenv(), "lookup_species_traits", fake_lookup, {
    shiny::testServer(mod, {
      session$setInputs(trait_research_input_method = "manual",
                        trait_research_species_list = "Bad only",
                        trait_research_databases = "worms")
      expect_warning(session$setInputs(trait_research_run_lookup = 1), "lookup failed for 'Bad only'")
      expect_identical(nrow(rv$trait_results), 1L)
      expect_no_error(output$trait_research_table)
      expect_no_error(output$trait_research_summary)
    })
  })
})

# ---------------------------------------------------------------------------
# Cached envelopes written before these fixes are refreshed
# ---------------------------------------------------------------------------

test_that("harm_config_hash changes with the lookup revision, so pre-C-6a envelopes are misses", {
  expect_identical(TRAIT_LOOKUP_REVISION, 2L)
  old <- TRAIT_LOOKUP_REVISION
  withr::defer(assign("TRAIT_LOOKUP_REVISION", old, envir = globalenv()))
  before <- harm_config_hash(HARMONIZATION_CONFIG)
  assign("TRAIT_LOOKUP_REVISION", old - 1L, envir = globalenv())
  expect_false(identical(harm_config_hash(HARMONIZATION_CONFIG), before))
})

# ---------------------------------------------------------------------------
# User decision (2026-09-29): infaunal bivalves before the depth rule
# ---------------------------------------------------------------------------

test_that("an infaunal bivalve with only a shallow depth range is endobenthic (EP4)", {
  mya <- list(phylum = "Mollusca", class = "Bivalvia", order = "Myida", family = "Myidae", genus = "Mya")
  # OBIS depth only (no habitat text): the depth rule used to return EP3 first.
  expect_identical(harmonize_environmental_position(depth_min = 0, depth_max = 10, taxonomic_info = mya), "EP4")
  # Explicit text still wins, and the switch still turns the rule off.
  expect_identical(harmonize_environmental_position(depth_min = 0, depth_max = 10, habitat_info = "epifaunal",
                                                    taxonomic_info = mya), "EP3")
  cfg <- HARMONIZATION_CONFIG
  cfg$taxonomic_rules$infaunal_bivalves <- FALSE
  session <- shiny::MockShinySession$new()
  on.exit(session$close(), add = TRUE)
  session$userData$harm_config <- cfg
  shiny::withReactiveDomain(session, {
    expect_identical(harmonize_environmental_position(depth_min = 0, depth_max = 10, taxonomic_info = mya), "EP3")
  })
})

test_that("shallow and deep fish keep the depth rule before their order rules", {
  cod <- list(phylum = "Chordata", class = "Teleostei", order = "Gadiformes")
  herring <- list(phylum = "Chordata", class = "Teleostei", order = "Clupeiformes")
  expect_identical(harmonize_environmental_position(depth_min = 0, depth_max = 20, taxonomic_info = cod), "EP3")
  expect_identical(harmonize_environmental_position(depth_min = 0, depth_max = 20, taxonomic_info = herring), "EP3")
  expect_identical(harmonize_environmental_position(depth_min = 150, depth_max = 600, taxonomic_info = herring), "EP2")
  expect_identical(harmonize_environmental_position(taxonomic_info = herring), "EP1")
})

# ---------------------------------------------------------------------------
# Final fix wave: a medium WoRMS group never beats a specific name hint (I1)
# ---------------------------------------------------------------------------

mysid_worms <- function(...) {
  list(aphia_id = 1L, scientific_name = "Mysida", phylum = "Arthropoda", class = "Malacostraca", order = "Mysida")
}

test_that("a medium WoRMS group does not override a specific name hint (I1a, Mysids)", {
  expect_identical(assign_functional_group("Mysids"), "Zooplankton")
  api <- with_mocked_function(globalenv(), "query_worms", mysid_worms,
    classify_species_api("Mysids", functional_group_hint = "Zooplankton", use_cache = FALSE))
  expect_identical(api$functional_group, "Benthos")  # classify_by_taxonomy is coarse
  expect_identical(api$confidence, "medium")
  expect_identical(prefer_specific_hint(api$functional_group, api$confidence, "Zooplankton"), "Zooplankton")
})

test_that("a medium WoRMS group replaces the generic Fish fallback (I1b, Limecola)", {
  expect_identical(assign_functional_group("Limecola balthica"), "Fish")  # the default fallback
  expect_identical(prefer_specific_hint("Benthos", "medium", "Fish"), "Benthos")
  expect_identical(prefer_specific_hint("Benthos", "medium", NA_character_), "Benthos")
})

test_that("a medium WoRMS group that agrees with the hint is kept (I1c)", {
  expect_identical(prefer_specific_hint("Benthos", "medium", "Benthos"), "Benthos")
  # an explicit fallback flag lets the API group win over any hint
  expect_identical(prefer_specific_hint("Benthos", "medium", "Zooplankton", hint_is_fallback = TRUE), "Benthos")
  # "high" behaves as before: the API group wins
  expect_identical(prefer_specific_hint("Fish", "high", "Zooplankton"), "Fish")
})

test_that("a low-confidence or missing API group is still rejected (I1d)", {
  expect_identical(prefer_specific_hint("Fish", "low", "Zooplankton"), "Zooplankton")
  expect_identical(prefer_specific_hint("Fish", "low", "Fish"), "Fish")
  expect_identical(prefer_specific_hint(NA, "none", "Benthos"), "Benthos")
  expect_identical(prefer_specific_hint("Benthos", NA, "Zooplankton"), "Zooplankton")
})

test_that("the Ecopath import routes its API result through prefer_specific_hint (I1)", {
  src <- readLines(file.path(get_app_root(), "R/modules/ecopath_import_server.R"), warn = FALSE)
  # via classification_report_fields(), which calls prefer_specific_hint()
  expect_true(any(grepl("classification_report_fields(", src, fixed = TRUE)))
  expect_true(any(grepl("prefer_specific_hint(", deparse(classification_report_fields), fixed = TRUE)))
  expect_false(any(grepl("api_result$confidence %in% c(\"high\", \"medium\")", src, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# M2 - WoRMS's current fish classes
# ---------------------------------------------------------------------------

test_that("WoRMS class Teleostei / Actinopteri is Fish at medium, not the default (M2)", {
  teleost <- function(...) list(aphia_id = 126436L, phylum = "Chordata", class = "Teleostei", order = "Gadiformes")
  res <- with_mocked_function(globalenv(), "query_fishbase", function(...) NULL,
    with_mocked_function(globalenv(), "query_worms", teleost,
      classify_species_api("Gadus morhua", use_cache = FALSE)))
  expect_identical(res$functional_group, "Fish")
  expect_identical(res$confidence, "medium")
  expect_null(attr(classify_by_taxonomy(list(class = "Teleostei")), "default"))
  expect_null(attr(classify_by_taxonomy(list(class = "Actinopteri")), "default"))
})

# ---------------------------------------------------------------------------
# F-a - AlgaeBase fallback on a tibble without phylum / class
# ---------------------------------------------------------------------------

test_that("a WoRMS tibble without phylum or class raises no tibble warning (F79)", {
  skip_if_not_installed("worrms")
  testthat::local_mocked_bindings(
    wm_records_name = function(name, ...) tibble::tibble(AphiaID = 1L),
    .package = "worrms"
  )
  expect_no_warning(res <- lookup_algaebase_traits("Obscura obscura"))
  expect_identical(res$note, "Species found but not classified as algae")
  expect_false(isTRUE(res$success))
  expect_null(res$error)
})

# ---------------------------------------------------------------------------
# F-b - mixed-unit WoRMS sizes: the maximum after conversion wins
# ---------------------------------------------------------------------------

test_that("mixed-unit WoRMS sizes keep the maximum after conversion, with its source (F32)", {
  # The smaller row comes first, so a first-row-wins rule would give 20.
  size <- worms_body_size_cm(worms_attr(rep("Body size", 2), c(20, 500), c("cm", "mm")),
                             "Bivalvia", "Mytilus edulis")
  expect_equal(size$max_length_cm, 50)
  expect_identical(size$size_unit_source, "child")

  expect_warning(size <- worms_body_size_cm(worms_attr(rep("Body size", 2), c(20, 300), c("cm", NA)),
                                            "Bivalvia", "Mytilus edulis"), "no unit")
  expect_equal(size$max_length_cm, 30)
  expect_identical(size$size_unit_source, "class_heuristic")
})

# ---------------------------------------------------------------------------
# Follow-ups: krill is zooplankton; a kept hint is reported as the source
# ---------------------------------------------------------------------------

krill_worms <- function(...) {
  list(aphia_id = 1L, phylum = "Arthropoda", class = "Malacostraca", order = "Euphausiacea")
}

test_that("krill groups keep the Zooplankton hint over a medium WoRMS Benthos", {
  for (nm in c("Krill", "Euphausia superba")) {
    hint <- assign_functional_group(nm)
    expect_identical(hint, "Zooplankton", info = nm)
    # FishBase is mocked too: a "Fish" hint (the pre-fix fallback) would query it.
    api <- with_mocked_function(globalenv(), "query_fishbase", function(...) NULL,
      with_mocked_function(globalenv(), "query_worms", krill_worms,
        classify_species_api(nm, functional_group_hint = hint, use_cache = FALSE)))
    expect_identical(api$functional_group, "Benthos", info = nm)
    expect_identical(api$confidence, "medium", info = nm)
    expect_identical(classification_report_fields(api, hint)$functional_group, "Zooplankton", info = nm)
  }
})

test_that("a kept hint is reported with the name-pattern source and confidence (Mysids)", {
  api <- with_mocked_function(globalenv(), "query_worms", mysid_worms,
    classify_species_api("Mysids", functional_group_hint = "Zooplankton", use_cache = FALSE))
  row <- classification_report_fields(api, "Zooplankton")
  expect_identical(row$functional_group, "Zooplankton")
  expect_identical(row$database_source, "Pattern matching")
  expect_identical(row$confidence, "none")

  # an accepted API group keeps its own source and confidence
  row <- classification_report_fields(list(functional_group = "Benthos", confidence = "medium", source = "WoRMS"),
                                      "Fish")
  expect_identical(row, list(functional_group = "Benthos", database_source = "WoRMS", confidence = "medium"))
  # no API result: the pattern path, unchanged
  row <- classification_report_fields(list(functional_group = NA, confidence = "none", source = NA), "Benthos")
  expect_identical(row, list(functional_group = "Benthos", database_source = "Pattern matching", confidence = "none"))

  src <- readLines(file.path(get_app_root(), "R/modules/ecopath_import_server.R"), warn = FALSE)
  expect_true(any(grepl("classification_report_fields(", src, fixed = TRUE)))
})
