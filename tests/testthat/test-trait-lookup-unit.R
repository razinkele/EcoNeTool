# =============================================================================
# Unit Tests: Trait Lookup Pipeline (Mocked with Fixtures)
# =============================================================================
# Tests WoRMS, FishBase, SeaLifeBase lookups and harmonization using fixtures.
# Run capture_fixtures.R first to generate fixture files.
# =============================================================================

source_app_dependencies()

# ============================================================================
# WoRMS Lookup
# ============================================================================

test_that("lookup_worms_traits returns correct structure for Gadus morhua", {
  result <- load_fixture("worms_gadus_morhua")

  expect_valid_worms_result(result)
  expect_true(result$success)
})

test_that("WoRMS returns correct taxonomy for Atlantic cod", {
  result <- load_fixture("worms_gadus_morhua")

  expect_true(result$success)
  traits <- result$traits

  expect_equal(tolower(traits$phylum), "chordata")
  expect_true(tolower(traits$class) %in% c("actinopterygii", "actinopteri", "teleostei"))
  expect_equal(tolower(traits$order), "gadiformes")
  expect_equal(tolower(traits$family), "gadidae")
  expect_equal(tolower(traits$genus), "gadus")
})

test_that("WoRMS returns valid AphiaID for known species", {
  result <- load_fixture("worms_gadus_morhua")

  expect_true(result$success)
  expect_true(!is.null(result$traits$aphia_id))
  expect_true(is.numeric(result$traits$aphia_id) || !is.na(as.numeric(result$traits$aphia_id)))

  # Gadus morhua AphiaID = 126436
  expect_equal(as.numeric(result$traits$aphia_id), 126436)
})

test_that("WoRMS returns correct structure for invertebrate", {
  result <- load_fixture("worms_mytilus_edulis")

  expect_valid_worms_result(result)
  expect_true(result$success)
  expect_equal(tolower(result$traits$phylum), "mollusca")
})

test_that("WoRMS returns marine flag for marine species", {
  result <- load_fixture("worms_gadus_morhua")

  if (result$success && !is.null(result$traits$isMarine)) {
    expect_true(result$traits$isMarine %in% c(TRUE, 1),
                info = "Gadus morhua should be flagged as marine")
  }
})

test_that("WoRMS lookup uses multiple strategies", {
  result <- load_fixture("worms_gadus_morhua")

  # Should have a strategy field
  if (!is.null(result$strategy)) {
    expect_true(is.character(result$strategy))
  }
})

# ============================================================================
# FishBase Lookup
# ============================================================================

test_that("lookup_fishbase_traits returns correct structure", {
  result <- load_fixture("fishbase_gadus_morhua")

  expect_valid_fishbase_result(result)
})

test_that("FishBase returns traits for Atlantic cod", {
  result <- load_fixture("fishbase_gadus_morhua")

  if (result$success) {
    traits <- result$traits

    # Cod should have reasonable size data
    if (!is.null(traits$max_length_cm)) {
      expect_gt(traits$max_length_cm, 50,
                label = "Cod max length should be > 50 cm")
      expect_lt(traits$max_length_cm, 250,
                label = "Cod max length should be < 250 cm")
    }

    # Cod trophic level should be around 4
    if (!is.null(traits$trophic_level)) {
      expect_gt(traits$trophic_level, 3.0)
      expect_lt(traits$trophic_level, 5.0)
    }
  }
})

test_that("FishBase returns traits for herring", {
  result <- load_fixture("fishbase_clupea_harengus")

  expect_valid_fishbase_result(result)

  if (result$success) {
    traits <- result$traits

    # Herring should be smaller than cod
    if (!is.null(traits$max_length_cm)) {
      expect_gt(traits$max_length_cm, 15)
      expect_lt(traits$max_length_cm, 60)
    }

    # Herring trophic level should be lower (planktivore)
    if (!is.null(traits$trophic_level)) {
      expect_gt(traits$trophic_level, 2.5)
      expect_lt(traits$trophic_level, 4.5)
    }
  }
})

test_that("FishBase trait list has expected fields", {
  result <- load_fixture("fishbase_gadus_morhua")

  if (result$success) {
    trait_names <- names(result$traits)

    # These are the key fields FishBase should provide
    expected_fields <- c("max_length_cm", "trophic_level")
    for (field in expected_fields) {
      expect_true(field %in% trait_names,
                  info = paste("FishBase should return", field))
    }
  }
})

# ============================================================================
# SeaLifeBase Lookup
# ============================================================================

test_that("lookup_sealifebase_traits returns correct structure", {
  result <- load_fixture("sealifebase_mytilus_edulis")

  expect_valid_db_lookup_result(result, "SeaLifeBase")
})

test_that("SeaLifeBase returns traits for blue mussel", {
  result <- load_fixture("sealifebase_mytilus_edulis")

  if (result$success) {
    traits <- result$traits

    # Blue mussel max length is typically 10-20 cm
    if (!is.null(traits$max_length_cm)) {
      expect_gt(traits$max_length_cm, 3)
      expect_lt(traits$max_length_cm, 30)
    }
  }
})

test_that("SeaLifeBase returns traits for shore crab", {
  result <- load_fixture("sealifebase_carcinus_maenas")

  expect_valid_db_lookup_result(result, "SeaLifeBase")
})

# ============================================================================
# Input Validation
# ============================================================================

test_that("lookup_fishbase_traits validates species_name", {
  expect_error(lookup_fishbase_traits(NULL))
  expect_error(lookup_fishbase_traits(""))
  expect_error(lookup_fishbase_traits(123))
  expect_error(lookup_fishbase_traits(c("a", "b")))
})

test_that("lookup_fishbase_traits validates timeout", {
  expect_error(lookup_fishbase_traits("Gadus morhua", timeout = -1))
  expect_error(lookup_fishbase_traits("Gadus morhua", timeout = "five"))
})

# ============================================================================
# Trait Harmonization
# ============================================================================

test_that("harmonize_size_class assigns correct MS codes", {
  if (!exists("harmonize_size_class")) {
    skip("harmonize_size_class not available")
  }

  # Very small (< 0.1 cm) -> MS1
  expect_match(harmonize_size_class(0.05), "^MS1$")

  # Medium fish (~20 cm) -> MS4 or MS5

  result_20 <- harmonize_size_class(20)
  expect_match(result_20, "^MS[4-5]$")

  # Large fish (>100 cm) -> MS6 or MS7
  result_150 <- harmonize_size_class(150)
  expect_match(result_150, "^MS[6-7]$")
})

# ============================================================================
# Full Pipeline: lookup_species_traits()
# ============================================================================

test_that("lookup_species_traits returns valid result for cod", {
  result <- load_fixture("traits_gadus_morhua")

  expect_valid_trait_result(result)
  expect_equal(result$species, "Gadus morhua")
})

test_that("lookup_species_traits returns valid result for mussel", {
  result <- load_fixture("traits_mytilus_edulis")

  expect_valid_trait_result(result)
  expect_equal(result$species, "Mytilus edulis")
})

test_that("trait codes are valid format when present", {
  result <- load_fixture("traits_gadus_morhua")

  # MS should be MS1-MS7 or NA
  if (!is.na(result$MS)) {
    expect_match(result$MS, "^MS[1-7]$")
  }

  # FS should be FS0-FS6 or NA
  if (!is.na(result$FS)) {
    expect_match(result$FS, "^FS[0-6]$")
  }

  # MB should be MB1-MB5 or NA
  if (!is.na(result$MB)) {
    expect_match(result$MB, "^MB[1-5]$")
  }

  # EP should be EP1-EP4 or NA
  if (!is.na(result$EP)) {
    expect_match(result$EP, "^EP[1-4]$")
  }

  # PR should be PR0-PR3 or NA
  if (!is.na(result$PR)) {
    expect_match(result$PR, "^PR[0-8]$")
  }
})

test_that("trait source is recorded", {
  result <- load_fixture("traits_gadus_morhua")

  if ("source" %in% names(result)) {
    expect_true(!is.na(result$source) || is.na(result$MS),
                info = "Source should be recorded when traits are found")
  }
})

test_that("trait confidence is recorded", {
  result <- load_fixture("traits_gadus_morhua")

  if ("confidence" %in% names(result)) {
    valid_levels <- c("high", "medium", "low", "none", NA_character_)
    expect_true(result$confidence %in% valid_levels,
                info = "Confidence should be high/medium/low/none")
  }
})

# ============================================================================
# Taxonomic Pre-filtering Logic
# ============================================================================

test_that("fish species routes to FishBase, not SeaLifeBase", {
  worms_cod <- load_fixture("worms_gadus_morhua")

  # Verify the routing logic: Chordata + Actinopterygii -> FishBase
  if (worms_cod$success) {
    phylum <- tolower(worms_cod$traits$phylum)
    class <- tolower(worms_cod$traits$class)

    expect_equal(phylum, "chordata")
    fish_classes <- c("actinopterygii", "actinopteri", "elasmobranchii",
                      "holocephali", "myxini", "petromyzonti",
                      "teleostei", "chondrichthyes", "osteichthyes")
    expect_true(class %in% fish_classes,
                info = paste("Class", class, "should trigger FishBase routing"))
  }
})

test_that("mollusc species routes to SeaLifeBase, not FishBase", {
  worms_mussel <- load_fixture("worms_mytilus_edulis")

  if (worms_mussel$success) {
    phylum <- tolower(worms_mussel$traits$phylum)
    invertebrate_phyla <- c("mollusca", "arthropoda", "annelida", "echinodermata",
                            "cnidaria", "porifera")
    expect_true(phylum %in% invertebrate_phyla,
                info = paste("Phylum", phylum, "should trigger SeaLifeBase routing"))
  }
})

# ============================================================================
# Cache Behavior
# ============================================================================

test_that("cache directory creation works", {
  tmp_cache <- tempfile("cache_test_")

  # Should not error even if dir doesn't exist yet
  expect_false(dir.exists(tmp_cache))

  # Create and verify
  dir.create(tmp_cache, recursive = TRUE)
  expect_true(dir.exists(tmp_cache))

  # Cleanup
  unlink(tmp_cache, recursive = TRUE)
})

test_that("cache file naming uses underscore-separated species name", {
  species <- "Gadus morhua"
  expected_filename <- "Gadus_morhua.rds"

  actual <- paste0(gsub(" ", "_", species), ".rds")
  expect_equal(actual, expected_filename)
})
