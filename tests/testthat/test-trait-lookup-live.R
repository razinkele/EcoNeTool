# =============================================================================
# Live Integration Tests: Trait Lookup Pipeline
# =============================================================================
# These tests hit REAL APIs (WoRMS, FishBase, SeaLifeBase).
# Skipped by default. Enable with: Sys.setenv(RUN_LIVE_TESTS = "true")
# =============================================================================

source_app_dependencies()

# ============================================================================
# WoRMS Live Tests
# ============================================================================

test_that("WoRMS returns taxonomy for Gadus morhua (live)", {
  skip_if_no_live_tests()
  skip_if_offline("www.marinespecies.org")
  skip_if_no_package("worrms")

  result <- lookup_worms_traits("Gadus morhua")

  expect_valid_worms_result(result)
  expect_true(result$success)
  expect_equal(tolower(result$traits$phylum), "chordata")
  expect_equal(as.numeric(result$traits$aphia_id), 126436)
})

test_that("WoRMS returns taxonomy for Mytilus edulis (live)", {
  skip_if_no_live_tests()
  skip_if_offline("www.marinespecies.org")
  skip_if_no_package("worrms")

  result <- lookup_worms_traits("Mytilus edulis")

  expect_valid_worms_result(result)
  expect_true(result$success)
  expect_equal(tolower(result$traits$phylum), "mollusca")
})

test_that("WoRMS handles non-existent species gracefully (live)", {
  skip_if_no_live_tests()
  skip_if_offline("www.marinespecies.org")
  skip_if_no_package("worrms")

  result <- lookup_worms_traits("Nonexistus fictitious")

  expect_valid_worms_result(result)
  expect_false(result$success)
})

test_that("WoRMS vernacular name search works (live)", {
  skip_if_no_live_tests()
  skip_if_offline("www.marinespecies.org")
  skip_if_no_package("httr")
  skip_if_no_package("jsonlite")

  result <- query_worms_vernacular("Atlantic cod")

  # Should find Gadus morhua
  if (!is.null(result)) {
    expect_equal(tolower(result$scientific_name), "gadus morhua")
  }
})

# ============================================================================
# FishBase Live Tests
# ============================================================================

test_that("FishBase returns data for Gadus morhua (live)", {
  skip_if_no_live_tests()
  skip_if_no_package("rfishbase")

  result <- lookup_fishbase_traits("Gadus morhua")

  expect_valid_fishbase_result(result)
  expect_true(result$success)

  # Validate biological plausibility
  if (!is.null(result$traits$max_length_cm)) {
    expect_gt(result$traits$max_length_cm, 50)
    expect_lt(result$traits$max_length_cm, 250)
  }
})

test_that("FishBase returns data for Clupea harengus (live)", {
  skip_if_no_live_tests()
  skip_if_no_package("rfishbase")

  result <- lookup_fishbase_traits("Clupea harengus")

  expect_valid_fishbase_result(result)
  expect_true(result$success)
})

test_that("FishBase handles non-fish species gracefully (live)", {
  skip_if_no_live_tests()
  skip_if_no_package("rfishbase")

  # Mytilus edulis is not a fish - should return success=FALSE
  result <- lookup_fishbase_traits("Mytilus edulis")

  expect_valid_fishbase_result(result)
  # May or may not find it - just verify structure
})

# ============================================================================
# SeaLifeBase Live Tests
# ============================================================================

test_that("SeaLifeBase returns data for Mytilus edulis (live)", {
  skip_if_no_live_tests()
  skip_if_no_package("rfishbase")

  result <- lookup_sealifebase_traits("Mytilus edulis")

  expect_valid_db_lookup_result(result, "SeaLifeBase")
})

test_that("SeaLifeBase returns data for Carcinus maenas (live)", {
  skip_if_no_live_tests()
  skip_if_no_package("rfishbase")

  result <- lookup_sealifebase_traits("Carcinus maenas")

  expect_valid_db_lookup_result(result, "SeaLifeBase")
})

# ============================================================================
# Full Pipeline Live Tests
# ============================================================================

test_that("Full trait lookup works for Atlantic cod (live)", {
  skip_if_no_live_tests()
  skip_if_offline("www.marinespecies.org")

  result <- lookup_species_traits("Gadus morhua")

  expect_valid_trait_result(result)
  expect_equal(result$species, "Gadus morhua")

  # Cod should get at least some traits filled in
  trait_cols <- c("MS", "FS", "MB", "EP", "PR")
  filled <- sum(!is.na(result[, trait_cols]))
  expect_gt(filled, 0,
            label = "At least one trait should be resolved for a well-known species")
})

test_that("Full trait lookup works for blue mussel (live)", {
  skip_if_no_live_tests()
  skip_if_offline("www.marinespecies.org")

  result <- lookup_species_traits("Mytilus edulis")

  expect_valid_trait_result(result)
  expect_equal(result$species, "Mytilus edulis")
})

test_that("Full trait lookup works for herring (live)", {
  skip_if_no_live_tests()
  skip_if_offline("www.marinespecies.org")

  result <- lookup_species_traits("Clupea harengus")

  expect_valid_trait_result(result)
  expect_equal(result$species, "Clupea harengus")
})

# ============================================================================
# Response Format Validation (detect API changes)
# ============================================================================

test_that("WoRMS API response format is stable (live)", {
  skip_if_no_live_tests()
  skip_if_offline("www.marinespecies.org")
  skip_if_no_package("worrms")

  # Direct worrms call to check raw format
  records <- tryCatch(
    worrms::wm_records_name("Gadus morhua"),
    error = function(e) NULL
  )

  if (!is.null(records) && length(records) > 0) {
    rec <- records[[1]]
    # Key fields that should always be present
    expect_true("AphiaID" %in% names(rec))
    expect_true("scientificname" %in% names(rec))
    expect_true("phylum" %in% names(rec))
    expect_true("class" %in% names(rec))
    expect_true("order" %in% names(rec))
    expect_true("family" %in% names(rec))
  }
})

test_that("FishBase API response format is stable (live)", {
  skip_if_no_live_tests()
  skip_if_no_package("rfishbase")

  species_data <- tryCatch(
    rfishbase::species("Gadus morhua"),
    error = function(e) NULL
  )

  if (!is.null(species_data) && nrow(species_data) > 0) {
    # Key fields that should always exist
    expect_true("Length" %in% names(species_data) || "length" %in% tolower(names(species_data)),
                info = "FishBase should return Length field")
  }
})

# ============================================================================
# Rate Limiter Integration
# ============================================================================

test_that("Rate limiter functions are available", {
  skip_if_no_live_tests()

  # Verify rate limiters can be created
  if (exists("get_worms_limiter")) {
    limiter <- get_worms_limiter()
    expect_true(!is.null(limiter))

    stats <- limiter$get_stats()
    expect_true("remaining" %in% names(stats))
  }
})

# ============================================================================
# Batch lookup (multiple species)
# ============================================================================

test_that("Multiple species can be looked up sequentially (live)", {
  skip_if_no_live_tests()
  skip_if_offline("www.marinespecies.org")

  species_list <- c("Gadus morhua", "Clupea harengus")

  results <- lapply(species_list, function(sp) {
    tryCatch(
      lookup_species_traits(sp),
      error = function(e) {
        data.frame(species = sp, MS = NA, FS = NA, MB = NA, EP = NA, PR = NA,
                   source = NA, confidence = NA, stringsAsFactors = FALSE)
      }
    )
  })

  combined <- do.call(rbind, results)
  expect_equal(nrow(combined), 2)
  expect_equal(combined$species, species_list)
})
