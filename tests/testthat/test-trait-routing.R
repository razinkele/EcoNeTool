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
  # zooplankton class branch. (Copepods are arthropoda: they route as inverts
  # and get MAREDAT added there - see the copepod test below.)
  f <- route_trait_databases("chaetognatha", "chaetognatha", TRUE, TRUE)
  expect_true(f$maredat && f$sealifebase)
})

test_that("copepods and ostracods (arthropoda) also query MAREDAT, keeping invert sources", {
  # WoRMS classes copepods/ostracods under phylum Arthropoda, so the invertebrate
  # branch matched first and MAREDAT (the zooplankton database) was never asked.
  for (cls in c("copepoda", "ostracoda")) {
    f <- route_trait_databases("arthropoda", cls, TRUE, TRUE)
    expect_true(f$maredat, info = cls)
    expect_true(f$sealifebase && f$biotic, info = cls)  # benthic species keep their sources
  }
  # Benthic arthropods are unchanged.
  expect_false(route_trait_databases("arthropoda", "malacostraca", TRUE, TRUE)$maredat)
})

test_that("order-level zooplankton (water fleas, mysids, krill) also query MAREDAT", {
  # WoRMS ranks these as orders, not classes: water fleas are Anomopoda / Ctenopoda /
  # Onychopoda / Haplopoda (WoRMS has no "Cladocera" rank), mysids are Mysida and krill
  # Euphausiacea, all inside Branchiopoda / Malacostraca. Classes alone cannot see them.
  cases <- list(c("branchiopoda", "anomopoda"),   # Daphnia, Bosmina
                c("branchiopoda", "ctenopoda"),   # Penilia
                c("branchiopoda", "onychopoda"),  # Podon, Evadne, Cercopagis
                c("branchiopoda", "haplopoda"),   # Leptodora
                c("malacostraca", "mysida"),      # Neomysis
                c("malacostraca", "euphausiacea"))  # Euphausia, Thysanoessa
  for (cs in cases) {
    f <- route_trait_databases("arthropoda", cs[1], TRUE, TRUE, order = cs[2])
    expect_true(f$maredat, info = cs[2])
    expect_true(f$sealifebase && f$biotic, info = cs[2])
  }
  # Order matching is case-insensitive (WoRMS returns "Mysida").
  expect_true(route_trait_databases("Arthropoda", "Malacostraca", TRUE, TRUE, order = "Mysida")$maredat)
  # Benthic orders in the same classes stay off MAREDAT; clam shrimps share the
  # Diplostraca superorder with water fleas but are not plankton.
  for (o in c("amphipoda", "decapoda", "isopoda", "spinicaudata")) {
    expect_false(route_trait_databases("arthropoda", "malacostraca", TRUE, TRUE, order = o)$maredat, info = o)
  }
  # order is optional: existing callers and NA/NULL order are unaffected.
  expect_false(route_trait_databases("arthropoda", "malacostraca", TRUE, TRUE)$maredat)
  expect_false(route_trait_databases("arthropoda", "malacostraca", TRUE, TRUE, order = NA)$maredat)
  expect_false(route_trait_databases("arthropoda", "malacostraca", TRUE, TRUE, order = NULL)$maredat)
})

test_that("lookup_species_traits passes the WoRMS order to the router", {
  src <- readLines(app_path("R/functions/trait_lookup/orchestrator.R"), warn = FALSE)
  call_at <- grep("route_trait_databases(", src, fixed = TRUE)
  call_at <- call_at[!grepl("<- function", src[call_at], fixed = TRUE)]
  expect_length(call_at, 1)
  call_src <- paste(src[call_at:(call_at + 6)], collapse = "\n")
  expect_true(grepl("order = raw_traits$worms$order", call_src, fixed = TRUE))
})
