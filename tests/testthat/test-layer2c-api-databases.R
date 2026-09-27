library(testthat)

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
source(file.path(app_root, "R/config.R"))
source(file.path(app_root, "R/functions/validation_utils.R"))
source(file.path(app_root, "R/functions/functional_group_utils.R"))
source(file.path(app_root, "R/config/harmonization_config.R"))
source(file.path(app_root, "R/functions/trait_lookup/harmonization.R"))
source(file.path(app_root, "R/functions/trait_lookup/api_trait_databases.R"))

# Every test that can reach the network is gated by skip_if_no_live_tests()
# (helper-fixtures.R) and bounds the call with with_timeout() (F84). The
# offline suite runs on every push/PR in CI; these run nightly with
# RUN_LIVE_TESTS=true. skip_if_offline() is the helper-fixtures.R version:
# a local redefinition used to shadow it. test-live-gating-guard.R enforces
# all of this.
LIVE_TIMEOUT <- 15

# =============================================================================
# 0. All 5 functions exist
# =============================================================================
test_that("all 5 API lookup functions are defined", {
  expect_true(exists("lookup_worms_traits_api"),  info = "lookup_worms_traits_api missing")
  expect_true(exists("lookup_polytraits"),         info = "lookup_polytraits missing")
  expect_true(exists("lookup_emodnet_traits"),     info = "lookup_emodnet_traits missing")
  expect_true(exists("lookup_obis_traits"),        info = "lookup_obis_traits missing")
  expect_true(exists("lookup_traitbank"),          info = "lookup_traitbank missing")
})

# =============================================================================
# 1. lookup_worms_traits_api - argument validation (returns before any HTTP)
# =============================================================================
test_that("lookup_worms_traits_api returns correct structure with NULL aphia_id", {
  res <- lookup_worms_traits_api(species_name = "Nereis diversicolor", aphia_id = NULL)
  expect_type(res, "list")
  expect_named(res, c("species", "source", "success", "traits"), ignore.order = TRUE)
  expect_equal(res$source,  "WoRMS_Traits")
  expect_false(res$success)
  expect_type(res$traits, "list")
})

test_that("lookup_worms_traits_api returns FALSE for non-positive aphia_id", {
  res <- lookup_worms_traits_api(species_name = "Test", aphia_id = -1)
  expect_false(res$success)
  res2 <- lookup_worms_traits_api(species_name = "Test", aphia_id = 0)
  expect_false(res2$success)
})

test_that("lookup_worms_traits_api returns FALSE for non-numeric aphia_id", {
  res <- lookup_worms_traits_api(species_name = "Test", aphia_id = "abc")
  expect_false(res$success)
})

test_that("lookup_worms_traits_api live lookup for AphiaID 126436 (Gadus morhua)", {
  skip_if_no_live_tests()
  skip_if_offline()
  skip_if_not_installed("worrms")
  res <- with_timeout(lookup_worms_traits_api(
    species_name = "Gadus morhua",
    aphia_id     = 126436,
    timeout      = 10
  ), timeout = LIVE_TIMEOUT)
  skip_if(is.null(res), "WoRMS did not answer within 15 s")
  expect_type(res, "list")
  expect_equal(res$source, "WoRMS_Traits")
  # Even if WoRMS returns no attributes, structure must be intact
  expect_type(res$traits,  "list")
  expect_type(res$success, "logical")
})

# =============================================================================
# 2. lookup_polytraits - structure (HTTP even for a nonexistent name)
# =============================================================================
test_that("lookup_polytraits returns correct structure", {
  skip_if_no_live_tests()
  res <- with_timeout(lookup_polytraits("XXXXXXNONEXISTENT_SPECIES_ZZZZ", timeout = 2),
                      timeout = LIVE_TIMEOUT)
  skip_if(is.null(res), "PolyTraits did not answer within 15 s")
  expect_type(res, "list")
  expect_named(res, c("species", "source", "success", "traits"), ignore.order = TRUE)
  expect_equal(res$source,  "PolyTraits")
  expect_false(res$success)
  expect_type(res$traits, "list")
})

test_that("lookup_polytraits species field matches input", {
  skip_if_no_live_tests()
  res <- with_timeout(lookup_polytraits("Hediste diversicolor", timeout = 1), timeout = LIVE_TIMEOUT)
  skip_if(is.null(res), "PolyTraits did not answer within 15 s")
  expect_equal(res$species, "Hediste diversicolor")
})

test_that("lookup_polytraits live lookup for a known polychaete", {
  skip_if_no_live_tests()
  skip_if_offline()
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
  res <- with_timeout(lookup_polytraits("Nereis diversicolor", timeout = 10), timeout = LIVE_TIMEOUT)
  skip_if(is.null(res), "PolyTraits did not answer within 15 s")
  expect_type(res, "list")
  expect_equal(res$source, "PolyTraits")
  expect_type(res$traits,  "list")
  expect_type(res$success, "logical")
})

# =============================================================================
# 3. lookup_emodnet_traits - structure
# =============================================================================
test_that("lookup_emodnet_traits returns correct structure (Btrait may be absent)", {
  # Without Btrait the function returns before any I/O. With it,
  # Btrait::getTrait() may fetch, so that case is a live test.
  if (requireNamespace("Btrait", quietly = TRUE)) skip_if_no_live_tests()
  res <- with_timeout(lookup_emodnet_traits("Abra alba"), timeout = LIVE_TIMEOUT)
  skip_if(is.null(res), "EMODnet did not answer within 15 s")
  expect_type(res, "list")
  expect_named(res, c("species", "source", "success", "traits"), ignore.order = TRUE)
  expect_equal(res$source,  "EMODnet")
  # Success depends on Btrait being installed; we only check shape
  expect_type(res$success, "logical")
  expect_type(res$traits,  "list")
})

test_that("lookup_emodnet_traits returns FALSE gracefully without Btrait", {
  skip_if(requireNamespace("Btrait", quietly = TRUE),
          "Btrait is installed - graceful-degradation test not applicable")
  res <- lookup_emodnet_traits("Abra alba")
  expect_false(res$success)
})

# =============================================================================
# 4. lookup_obis_traits - structure (HTTP even for a nonexistent name)
# =============================================================================
test_that("lookup_obis_traits returns correct structure", {
  skip_if_no_live_tests()
  res <- with_timeout(lookup_obis_traits("XXXXXXNONEXISTENT_SPECIES_ZZZZ", timeout = 5),
                      timeout = LIVE_TIMEOUT)
  skip_if(is.null(res), "OBIS did not answer within 15 s")
  expect_type(res, "list")
  expect_named(res, c("species", "source", "success", "traits"), ignore.order = TRUE)
  expect_equal(res$source,  "OBIS")
  expect_type(res$success, "logical")
  expect_type(res$traits,  "list")
})

test_that("lookup_obis_traits species field matches input", {
  skip_if_no_live_tests()
  res <- with_timeout(lookup_obis_traits("Fake species", timeout = 1), timeout = LIVE_TIMEOUT)
  skip_if(is.null(res), "OBIS did not answer within 15 s")
  expect_equal(res$species, "Fake species")
})

test_that("lookup_obis_traits live lookup for Abra alba", {
  skip_if_no_live_tests()
  skip_if_offline()
  skip_if_not_installed("robis")
  # OBIS API is very slow and can exceed R's C-level elapsed time limit.
  # Only run when ECONETOOL_TEST_OBIS_LIVE=true is set.
  skip_if(!identical(Sys.getenv("ECONETOOL_TEST_OBIS_LIVE"), "true"),
          "OBIS live test skipped by default (set ECONETOOL_TEST_OBIS_LIVE=true to enable)")
  res <- with_timeout(lookup_obis_traits("Abra alba", timeout = 10), timeout = LIVE_TIMEOUT)
  skip_if(is.null(res), "OBIS did not answer within 15 s")
  expect_type(res, "list")
  expect_equal(res$source, "OBIS")
  expect_type(res$traits,  "list")
  expect_type(res$success, "logical")
  expect_true(!isTRUE(res$success) || length(res$traits) > 0,
              info = "a successful OBIS lookup must carry traits")
})

# =============================================================================
# 5. lookup_traitbank - structure (HTTP even for a nonexistent name)
# =============================================================================
test_that("lookup_traitbank returns correct structure", {
  skip_if_no_live_tests()
  res <- with_timeout(lookup_traitbank("XXXXXXNONEXISTENT_SPECIES_ZZZZ", timeout = 2),
                      timeout = LIVE_TIMEOUT)
  skip_if(is.null(res), "TraitBank did not answer within 15 s")
  expect_type(res, "list")
  expect_named(res, c("species", "source", "success", "traits"), ignore.order = TRUE)
  expect_equal(res$source,  "TraitBank")
  expect_type(res$success, "logical")
  expect_type(res$traits,  "list")
})

test_that("lookup_traitbank species field matches input", {
  skip_if_no_live_tests()
  res <- with_timeout(lookup_traitbank("Fake species xyz", timeout = 1), timeout = LIVE_TIMEOUT)
  skip_if(is.null(res), "TraitBank did not answer within 15 s")
  expect_equal(res$species, "Fake species xyz")
})

test_that("lookup_traitbank live lookup for Abra alba", {
  skip_if_no_live_tests()
  skip_if_offline()
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
  res <- with_timeout(lookup_traitbank("Abra alba", timeout = 10), timeout = LIVE_TIMEOUT)
  skip_if(is.null(res), "TraitBank did not answer within 15 s")
  expect_type(res, "list")
  expect_equal(res$source, "TraitBank")
  expect_type(res$traits,  "list")
  expect_type(res$success, "logical")
})

# =============================================================================
# 6. Return-value invariants (all functions)
# =============================================================================
test_that("all functions always return the four required list fields", {
  skip_if_no_live_tests()
  required_fields <- c("species", "source", "success", "traits")

  results <- with_timeout(list(
    worms      = lookup_worms_traits_api("X", aphia_id = NULL),
    polytraits = lookup_polytraits("X", timeout = 1),
    emodnet    = lookup_emodnet_traits("X"),
    obis       = lookup_obis_traits("X", timeout = 1),
    traitbank  = lookup_traitbank("X", timeout = 1)
  ), timeout = LIVE_TIMEOUT)
  skip_if(is.null(results), "lookups did not answer within 15 s")

  for (nm in names(results)) {
    r <- results[[nm]]
    expect_named(r, required_fields, ignore.order = TRUE,
                 info = paste(nm, "missing required fields"))
    expect_true(is.logical(r$success),
                info = paste(nm, "$success should be logical"))
    expect_true(is.list(r$traits),
                info = paste(nm, "$traits should be a list"))
  }
})

test_that("orchestrator has API database routing flags", {
  orch_text <- readLines(file.path(app_root, "R/functions/trait_lookup/orchestrator.R"))
  orch_joined <- paste(orch_text, collapse = "\n")
  expect_true(grepl("query_worms_attrs", orch_joined), info = "Missing query_worms_attrs")
  expect_true(grepl("query_polytraits", orch_joined), info = "Missing query_polytraits")
  expect_true(grepl("query_emodnet", orch_joined), info = "Missing query_emodnet")
  expect_true(grepl("query_obis", orch_joined), info = "Missing query_obis")
  expect_true(grepl("query_traitbank", orch_joined), info = "Missing query_traitbank")
})
