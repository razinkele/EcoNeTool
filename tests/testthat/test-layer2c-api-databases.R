library(testthat)

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
source(file.path(app_root, "R/config.R"))
source(file.path(app_root, "R/functions/validation_utils.R"))
source(file.path(app_root, "R/functions/functional_group_utils.R"))
source(file.path(app_root, "R/config/harmonization_config.R"))
source(file.path(app_root, "R/functions/trait_lookup/harmonization.R"))
source(file.path(app_root, "R/functions/trait_lookup/api_trait_databases.R"))

# =============================================================================
# Helper: skip_if_offline
# =============================================================================
skip_if_offline <- function() {
  online <- tryCatch({
    con <- url("https://www.google.com", open = "r")
    close(con)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!online) skip("No internet connection — skipping live API test")
}

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
# 1. lookup_worms_traits_api — structure
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

test_that("lookup_worms_traits_api live lookup for AphiaID 126436 (Nereis diversicolor)", {
  skip_if_offline()
  skip_if_not_installed("worrms")
  res <- lookup_worms_traits_api(
    species_name = "Nereis diversicolor",
    aphia_id     = 126436,
    timeout      = 20
  )
  expect_type(res, "list")
  expect_equal(res$source, "WoRMS_Traits")
  # Even if WoRMS returns no attributes, structure must be intact
  expect_type(res$traits,  "list")
  expect_type(res$success, "logical")
})

# =============================================================================
# 2. lookup_polytraits — structure
# =============================================================================
test_that("lookup_polytraits returns correct structure", {
  # Offline: httr not reachable -> should return FALSE gracefully
  res <- lookup_polytraits("XXXXXXNONEXISTENT_SPECIES_ZZZZ", timeout = 2)
  expect_type(res, "list")
  expect_named(res, c("species", "source", "success", "traits"), ignore.order = TRUE)
  expect_equal(res$source,  "PolyTraits")
  expect_false(res$success)
  expect_type(res$traits, "list")
})

test_that("lookup_polytraits species field matches input", {
  res <- lookup_polytraits("Hediste diversicolor", timeout = 1)
  expect_equal(res$species, "Hediste diversicolor")
})

test_that("lookup_polytraits live lookup for a known polychaete", {
  skip_if_offline()
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
  res <- lookup_polytraits("Nereis diversicolor", timeout = 15)
  expect_type(res, "list")
  expect_equal(res$source, "PolyTraits")
  expect_type(res$traits,  "list")
  expect_type(res$success, "logical")
})

# =============================================================================
# 3. lookup_emodnet_traits — structure
# =============================================================================
test_that("lookup_emodnet_traits returns correct structure (Btrait may be absent)", {
  res <- lookup_emodnet_traits("Abra alba")
  expect_type(res, "list")
  expect_named(res, c("species", "source", "success", "traits"), ignore.order = TRUE)
  expect_equal(res$source,  "EMODnet")
  # Success depends on Btrait being installed; we only check shape
  expect_type(res$success, "logical")
  expect_type(res$traits,  "list")
})

test_that("lookup_emodnet_traits returns FALSE gracefully without Btrait", {
  # If Btrait is installed, skip this; if not, it must return FALSE
  if (requireNamespace("Btrait", quietly = TRUE)) {
    skip("Btrait is installed — graceful-degradation test not applicable")
  }
  res <- lookup_emodnet_traits("Abra alba")
  expect_false(res$success)
})

# =============================================================================
# 4. lookup_obis_traits — structure
# =============================================================================
test_that("lookup_obis_traits returns correct structure", {
  res <- lookup_obis_traits("XXXXXXNONEXISTENT_SPECIES_ZZZZ", timeout = 5)
  expect_type(res, "list")
  expect_named(res, c("species", "source", "success", "traits"), ignore.order = TRUE)
  expect_equal(res$source,  "OBIS")
  expect_type(res$success, "logical")
  expect_type(res$traits,  "list")
})

test_that("lookup_obis_traits species field matches input", {
  res <- lookup_obis_traits("Fake species", timeout = 1)
  expect_equal(res$species, "Fake species")
})

test_that("lookup_obis_traits live lookup for Abra alba", {
  skip_if_offline()
  skip_if_not_installed("robis")
  # OBIS API is very slow and can exceed R's C-level elapsed time limit.
  # Only run when ECONETOOL_TEST_OBIS_LIVE=true is set.
  if (!identical(Sys.getenv("ECONETOOL_TEST_OBIS_LIVE"), "true")) {
    skip("OBIS live test skipped by default (set ECONETOOL_TEST_OBIS_LIVE=true to enable)")
  }
  res <- lookup_obis_traits("Abra alba", timeout = 20)
  expect_type(res, "list")
  expect_equal(res$source, "OBIS")
  expect_type(res$traits,  "list")
  expect_type(res$success, "logical")
  if (res$success) {
    expect_true(length(res$traits) > 0)
  }
})

# =============================================================================
# 5. lookup_traitbank — structure
# =============================================================================
test_that("lookup_traitbank returns correct structure", {
  res <- lookup_traitbank("XXXXXXNONEXISTENT_SPECIES_ZZZZ", timeout = 2)
  expect_type(res, "list")
  expect_named(res, c("species", "source", "success", "traits"), ignore.order = TRUE)
  expect_equal(res$source,  "TraitBank")
  expect_type(res$success, "logical")
  expect_type(res$traits,  "list")
})

test_that("lookup_traitbank species field matches input", {
  res <- lookup_traitbank("Fake species xyz", timeout = 1)
  expect_equal(res$species, "Fake species xyz")
})

test_that("lookup_traitbank live lookup for Abra alba", {
  skip_if_offline()
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
  res <- lookup_traitbank("Abra alba", timeout = 20)
  expect_type(res, "list")
  expect_equal(res$source, "TraitBank")
  expect_type(res$traits,  "list")
  expect_type(res$success, "logical")
})

# =============================================================================
# 6. Return-value invariants (all functions)
# =============================================================================
test_that("all functions always return the four required list fields", {
  required_fields <- c("species", "source", "success", "traits")

  results <- list(
    worms      = lookup_worms_traits_api("X", aphia_id = NULL),
    polytraits = lookup_polytraits("X", timeout = 1),
    emodnet    = lookup_emodnet_traits("X"),
    obis       = lookup_obis_traits("X", timeout = 1),
    traitbank  = lookup_traitbank("X", timeout = 1)
  )

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
