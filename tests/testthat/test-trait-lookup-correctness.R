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
