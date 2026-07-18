# Unit tests for route_trait_databases() - taxonomy routing must never crash
# on NULL/NA/empty WoRMS taxonomy (deep-analysis #8/#10).

source_app_dependencies()

test_that("fish route queries FishBase and skips invert DBs", {
  f <- route_trait_databases("chordata", "actinopterygii", TRUE, TRUE)
  expect_true(f$fishbase)
  expect_false(f$sealifebase)
  expect_false(f$maredat)
})

test_that("empty phylum (WoRMS timeout with NULL taxonomy) uses fallback, does not error", {
  # This is the #8 crash: success=TRUE but phylum is empty.
  expect_error(route_trait_databases("", "", NA, TRUE), NA)  # NA = expect no error
  f <- route_trait_databases("", "", NA, TRUE)
  expect_true(f$fishbase && f$sealifebase && f$biotic)  # fallback = try everything
})

test_that("NULL phylum does not error and routes to fallback", {
  expect_error(route_trait_databases(NULL, NULL, NULL, TRUE), NA)
  f <- route_trait_databases(NULL, NULL, NULL, TRUE)
  expect_true(f$fishbase && f$sealifebase)
})

test_that("NA isMarine on a phyto route does not error and does not force freshwater", {
  # This is the #10 crash: is_marine == FALSE with is_marine = NA -> if(NA).
  expect_error(route_trait_databases("bacillariophyta", "bacillariophyceae", NA, TRUE), NA)
  f <- route_trait_databases("bacillariophyta", "bacillariophyceae", NA, TRUE)
  expect_false(f$freshwater)   # NA marine != FALSE, so not freshwater
  expect_true(f$ptdb)
})

test_that("freshwater phyto (isMarine FALSE) adds freshwater source", {
  f <- route_trait_databases("chlorophyta", "chlorophyceae", FALSE, TRUE)
  expect_true(f$freshwater)
})

test_that("worms_ok FALSE uses fallback regardless of taxonomy", {
  f <- route_trait_databases("chordata", "actinopterygii", TRUE, FALSE)
  expect_true(f$fishbase && f$sealifebase && f$maredat && f$ptdb)
})

test_that("anthozoa invertebrate also flags coral", {
  f <- route_trait_databases("cnidaria", "anthozoa", TRUE, TRUE)
  expect_true(f$sealifebase && f$coral)
})

test_that("polychaeta invertebrate also flags polytraits", {
  f <- route_trait_databases("annelida", "polychaeta", TRUE, TRUE)
  expect_true(f$sealifebase && f$polytraits)
})

test_that("zooplankton route (non-invert phylum) queries MAREDAT and SeaLifeBase", {
  # Chaetognatha phylum is NOT in the invertebrate-phyla list, so it reaches the
  # zooplankton class branch. (Copepods are arthropoda and route as inverts -
  # a pre-existing quirk this extraction deliberately preserves.)
  f <- route_trait_databases("chaetognatha", "chaetognatha", TRUE, TRUE)
  expect_true(f$maredat && f$sealifebase)
})
