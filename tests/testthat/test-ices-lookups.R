# Unit + live tests for ICES vocab + DATRAS abundance helpers (PR10-S).
# Live tests follow the established convention: gated behind
# RUN_LIVE_TESTS=true so CI runs them on the nightly schedule but not
# on every push.

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")

skip_if_no_live_tests <- function() {
  if (!isTRUE(as.logical(Sys.getenv("RUN_LIVE_TESTS")))) {
    testthat::skip("Live API tests disabled. Set RUN_LIVE_TESTS=true to enable.")
  }
}

test_that("ICES helpers return structured results (offline contract)", {
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/ices_lookups.R"), local = TRUE)

  # Without packages or network, helpers must still return a structured
  # list; never NULL, never throw — same contract as the trait-lookup
  # functions. Even if icesVocab is installed, the offline-mode shape
  # has to be intact.
  expect_true(exists("get_ices_area_codes", mode = "function"))
  expect_true(exists("get_ices_area_detail", mode = "function"))
  expect_true(exists("lookup_datras_indices", mode = "function"))

  # Bad inputs return structured failures, not crashes
  bad <- get_ices_area_detail(code = "")
  expect_false(bad$success)
  expect_equal(bad$source, "icesVocab")
  expect_true(nzchar(bad$error))

  bad2 <- lookup_datras_indices(aphia_id = -1)
  expect_false(bad2$success)
  expect_equal(bad2$source, "DATRAS")
  expect_true(nzchar(bad2$error))
})

test_that("get_ices_area_codes returns a non-empty data.frame (live)", {
  skip_if_no_live_tests()
  skip_if_not_installed("icesVocab")
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/ices_lookups.R"), local = TRUE)

  res <- get_ices_area_codes(timeout = 60)
  expect_true(res$success)
  expect_s3_class(res$data, "data.frame")
  expect_gt(nrow(res$data), 0)
})

test_that("lookup_datras_indices returns structured data for cod (live)", {
  skip_if_no_live_tests()
  skip_if_not_installed("icesDatras")
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/ices_lookups.R"), local = TRUE)

  # Gadus morhua (AphiaID 126436) in BITS — Baltic surveys reliably
  # report cod indices, so this is a low-flake live probe.
  res <- lookup_datras_indices(
    aphia_id = 126436,
    surveys  = "BITS",
    timeout  = 60
  )
  # Either we got data (success) or DATRAS legitimately had no rows
  # for the requested year; both are valid outcomes for a live call.
  if (res$success) {
    expect_s3_class(res$data, "data.frame")
    expect_true(all(c("survey", "year", "biomass_index") %in% names(res$data)))
    expect_true(all(res$data$survey == "BITS"))
  } else {
    expect_true(nzchar(res$error))
  }
})

test_that("lookup_ices_subdivision rejects invalid coordinates", {
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/ices_lookups.R"), local = TRUE)

  for (bad in list(c(NA_real_, 57), c(Inf, 57), c(200, 57),
                   c(19, 200), c(19, -91))) {
    label <- paste(bad, collapse = ",")
    res <- lookup_ices_subdivision(bad[1], bad[2])
    expect_false(res$success, info = label)
    expect_equal(res$source, "ICES_GIS_WFS", info = label)
    expect_match(res$error, "lon/lat invalid", fixed = TRUE, info = label)
  }
})

# Square polygon helper for ICES area fixtures (used by Tasks 2-4 tests).
.sq <- function(xmin, xmax, ymin, ymax) {
  sf::st_polygon(list(rbind(
    c(xmin, ymin), c(xmax, ymin), c(xmax, ymax), c(xmin, ymax), c(xmin, ymin)
  )))
}

test_that("lookup_ices_subdivision maps a point to its ICES area (fixture)", {
  skip_if_not_installed("sf")
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/ices_lookups.R"), local = TRUE)

  fixture <- sf::st_sf(
    Area_Full  = c("27.3.d.27", "27.3.d.28.2"),
    Major_FA   = c("27", "27"),
    SubArea    = c("3", "3"),
    Division   = c("d", "d"),
    SubDivisio = c("27", "28"),
    Unit       = c(" ", "2"),                 # live data uses " " for empty
    geometry   = sf::st_sfc(.sq(18, 19, 56, 57), .sq(19, 20, 56, 57), crs = 4326)
  )
  .ices_cache$areas_sf <- fixture
  on.exit(
    if (!is.null(.ices_cache$areas_sf)) rm("areas_sf", envir = .ices_cache),
    add = TRUE
  )

  a <- lookup_ices_subdivision(18.5, 56.5)
  expect_true(a$success)
  expect_equal(a$data$area_full, "27.3.d.27")
  expect_equal(a$data$subdivision, "27")
  expect_equal(a$data$unit, "")               # " " trimmed to ""

  b <- lookup_ices_subdivision(19.5, 56.5)
  expect_equal(b$data$area_full, "27.3.d.28.2")
  expect_equal(b$data$unit, "2")
})

test_that("lookup_ices_subdivision fails cleanly for a point in no area", {
  skip_if_not_installed("sf")
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/ices_lookups.R"), local = TRUE)

  .ices_cache$areas_sf <- sf::st_sf(
    Area_Full = "27.3.d.27", Major_FA = "27", SubArea = "3",
    Division = "d", SubDivisio = "27", Unit = " ",
    geometry = sf::st_sfc(.sq(18, 19, 56, 57), crs = 4326)
  )
  on.exit(
    if (!is.null(.ices_cache$areas_sf)) rm("areas_sf", envir = .ices_cache),
    add = TRUE
  )

  res <- lookup_ices_subdivision(25, 56.5)   # east of the only polygon
  expect_false(res$success)
  expect_match(res$error, "no ICES area")
})

test_that("lookup_ices_subdivision picks the most specific area on overlap + warns", {
  skip_if_not_installed("sf")
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/ices_lookups.R"), local = TRUE)

  .ices_cache$areas_sf <- sf::st_sf(
    Area_Full  = c("27.3", "27.3.d.28.2"),     # coarse + specific, overlapping
    Major_FA   = c("27", "27"),
    SubArea    = c("3", "3"),
    Division   = c("", "d"),
    SubDivisio = c("", "28"),
    Unit       = c("", "2"),
    geometry   = sf::st_sfc(.sq(18, 21, 56, 58), .sq(19, 20, 56.5, 57.5), crs = 4326)
  )
  on.exit(
    if (!is.null(.ices_cache$areas_sf)) rm("areas_sf", envir = .ices_cache),
    add = TRUE
  )

  expect_warning(
    res <- lookup_ices_subdivision(19.5, 57),  # inside BOTH polygons
    "boundary|specific"
  )
  expect_true(res$success)
  expect_equal(res$data$area_full, "27.3.d.28.2")   # longest Area_Full wins
})
