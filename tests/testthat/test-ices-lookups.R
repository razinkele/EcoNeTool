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

test_that(".datras_reshape_indices yields one abundance row per IndexArea", {
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/ices_lookups.R"), local = TRUE)

  # A getIndices()-shaped frame: ICES returns one row per IndexArea
  # (e.g. BITS splits Baltic cod into East/West stocks) and the abundance
  # lives in Age_0..Age_N columns — there is NO single "Index" column.
  # The reshape must (a) keep both areas as separate rows and (b) sum the
  # age columns into a numeric abundance_index (NA treated as 0).
  idx <- data.frame(
    Survey    = c("BITS", "BITS"),
    Year      = c(2023, 2023),
    Quarter   = c(1L, 1L),
    AphiaID   = c(126436, 126436),
    Species   = c("Gadus morhua", "Gadus morhua"),
    IndexArea = c("BS_CodEast", "BS_CodWest"),
    Age_0     = c(10, 5),
    Age_1     = c(20, 5),
    Age_2     = c(0, NA_real_),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  out <- .datras_reshape_indices(idx, "BITS", 2023)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)                                  # one row per IndexArea
  expect_true(all(c("survey", "year", "quarter",
                    "index_area", "abundance_index") %in% names(out)))
  expect_equal(out$index_area, c("BS_CodEast", "BS_CodWest"))
  expect_equal(out$abundance_index, c(30, 10))                # rowSums of Age_*, na.rm
  expect_false("biomass_index" %in% names(out))               # corrected naming
})

test_that(".datras_reshape_indices min_age=0 preserves the all-ages sum (backward compat)", {
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/ices_lookups.R"), local = TRUE)
  idx <- data.frame(
    Survey = c("BITS", "BITS"), Year = c(2023, 2023), Quarter = c(1L, 1L),
    IndexArea = c("BS_CodEast", "BS_CodWest"),
    Age_0 = c(10, 5), Age_1 = c(20, 5), Age_2 = c(0, NA_real_), Age_3 = c(4, 6),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  out0 <- .datras_reshape_indices(idx, "BITS", 2023)               # default min_age = 0
  out0b <- .datras_reshape_indices(idx, "BITS", 2023, min_age = 0L)
  expect_equal(out0$abundance_index, c(34, 16))                   # 10+20+0+4 ; 5+5+6 = 16 (na.rm drops the NA)
  expect_equal(out0b$abundance_index, c(34, 16))                  # explicit 0 == default
})

test_that(".datras_reshape_indices min_age=2 drops age-0/1 (keeps 2+)", {
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/ices_lookups.R"), local = TRUE)
  idx <- data.frame(
    Survey = c("BITS", "BITS"), Year = c(2023, 2023), Quarter = c(1L, 1L),
    IndexArea = c("BS_CodEast", "BS_CodWest"),
    Age_0 = c(10, 5), Age_1 = c(20, 5), Age_2 = c(0, NA_real_), Age_3 = c(4, 6),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  out2 <- .datras_reshape_indices(idx, "BITS", 2023, min_age = 2L)
  expect_equal(out2$abundance_index, c(4, 6))                     # Age_2+Age_3, na.rm (row2: NA+6)
})

test_that(".datras_reshape_indices min_age=2 with only age-0/1 yields NA, not 0 (sparse guard)", {
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/ices_lookups.R"), local = TRUE)
  idx <- data.frame(
    Survey = "BITS", Year = 2023, Quarter = 1L, IndexArea = "X",
    Age_0 = 10, Age_1 = 20,
    stringsAsFactors = FALSE, check.names = FALSE
  )
  out <- .datras_reshape_indices(idx, "BITS", 2023, min_age = 2L)
  expect_true(is.na(out$abundance_index))                        # NA, never 0
})

test_that(".datras_reshape_indices with no Age_ columns yields NA (min_age irrelevant)", {
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/ices_lookups.R"), local = TRUE)
  idx <- data.frame(Survey = "BITS", Year = 2023, Quarter = 1L, IndexArea = "X",
                    stringsAsFactors = FALSE, check.names = FALSE)
  expect_true(is.na(.datras_reshape_indices(idx, "BITS", 2023, min_age = 0L)$abundance_index))
  expect_true(is.na(.datras_reshape_indices(idx, "BITS", 2023, min_age = 2L)$abundance_index))
})

test_that(".datras_reshape_indices min_age=2: a row whose kept ages are ALL NA yields NA, not 0 (false-zero guard)", {
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/ices_lookups.R"), local = TRUE)
  # Realistic case: the frame HAS Age_2/Age_3 columns (an adult area populates
  # them), but a recruitment-only area carries NA across every kept age. Without
  # the guard, rowSums(na.rm=TRUE) over that row returns 0 (a false zero); with
  # the guard it must be NA so the year is dropped, not plotted as ~0 abundance.
  idx <- data.frame(
    Survey = c("BITS", "BITS"), Year = c(2023, 2023), Quarter = c(1L, 1L),
    IndexArea = c("adult", "recruit"),
    Age_0 = c(1, 30), Age_1 = c(2, 40),
    Age_2 = c(5, NA_real_), Age_3 = c(7, NA_real_),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  out <- .datras_reshape_indices(idx, "BITS", 2023, min_age = 2L)
  expect_equal(out$abundance_index[1], 12)            # adult: Age_2+Age_3 = 5+7
  expect_true(is.na(out$abundance_index[2]))          # recruit: kept ages all NA -> NA, not 0
})

test_that(".datras_reshape_indices min_age=0 keeps a genuine zero as 0 (does not over-NA)", {
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/ices_lookups.R"), local = TRUE)
  # Guard the guard: the all-NA->NA restoration is gated to min_age > 0, so the
  # default (R3) path is byte-identical to today. A row with a real 0 and an NA
  # stays a summed value, never forced to NA.
  idx <- data.frame(Survey = "BITS", Year = 2023, Quarter = 1L, IndexArea = "X",
                    Age_0 = NA_real_, Age_1 = 0,
                    stringsAsFactors = FALSE, check.names = FALSE)
  expect_equal(.datras_reshape_indices(idx, "BITS", 2023, min_age = 0L)$abundance_index, 0)
})

test_that(".datras_reshape_indices min_age=2 parses multi-digit age suffixes (Age_10)", {
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/ices_lookups.R"), local = TRUE)
  idx <- data.frame(
    Survey = "BITS", Year = 2023, Quarter = 1L, IndexArea = "X",
    Age_0 = 100, Age_2 = 3, Age_10 = 5,
    stringsAsFactors = FALSE, check.names = FALSE
  )
  out <- .datras_reshape_indices(idx, "BITS", 2023, min_age = 2L)
  expect_equal(out$abundance_index, 8)   # Age_2 + Age_10 = 3 + 5 (Age_0 dropped, 10 >= 2 kept)
})

test_that("lookup_datras_indices passes species + real quarters and returns abundance rows", {
  skip_if_not_installed("icesDatras")
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/ices_lookups.R"), local = TRUE)

  captured <- new.env()
  captured$species  <- NULL
  captured$quarters <- integer(0)

  # Mock the three icesDatras calls so the function's call-construction
  # logic is exercised without network: it must resolve REAL quarters
  # (BITS -> 1, 4), never quarter = -1, and pass species = aphia_id.
  testthat::local_mocked_bindings(
    getSurveyYearList = function(survey) 2023L,
    getSurveyYearQuarterList = function(survey, year) c(1L, 4L),
    getIndices = function(survey, year, quarter, species) {
      captured$species  <- species
      captured$quarters <- c(captured$quarters, quarter)
      data.frame(
        Survey    = "BITS", Year = 2023L, Quarter = quarter,
        AphiaID   = 126436, Species = "Gadus morhua",
        IndexArea = c("BS_CodEast", "BS_CodWest"),   # two stocks per call
        Age_0     = c(1, 2), Age_1 = c(3, 4),
        stringsAsFactors = FALSE, check.names = FALSE
      )
    },
    .package = "icesDatras"
  )

  res <- lookup_datras_indices(126436, surveys = "BITS", timeout = 5)

  expect_true(res$success)
  expect_equal(captured$species, 126436)              # species threaded through
  expect_false(-1 %in% captured$quarters)             # NOT the rejected quarter=-1
  expect_true(all(captured$quarters %in% c(1L, 4L)))  # real quarters used
  expect_true(all(c("survey", "year", "quarter", "index_area",
                    "abundance_index") %in% names(res$data)))
  expect_false("biomass_index" %in% names(res$data))  # corrected naming
  expect_equal(nrow(res$data), 4L)                    # 2 IndexAreas x 2 quarters
})

test_that("lookup_datras_indices steps back to the most recent year WITH indices", {
  skip_if_not_installed("icesDatras")
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/ices_lookups.R"), local = TRUE)

  # The year list advertises years (e.g. 2026) before their indices are
  # computed (~6-18 months later). max(year list) is therefore often empty;
  # the function must step back to the most recent year that returns rows.
  testthat::local_mocked_bindings(
    getSurveyYearList = function(survey) c(2024L, 2025L, 2026L),
    getSurveyYearQuarterList = function(survey, year) 1L,
    getIndices = function(survey, year, quarter, species) {
      if (year == 2025L) {
        data.frame(
          Survey = "BITS", Year = 2025L, Quarter = quarter,
          IndexArea = "BS_CodEast", Age_0 = 7, Age_1 = 3,
          stringsAsFactors = FALSE, check.names = FALSE
        )
      } else {
        # 2026 (and 2024) have no computed indices yet / anymore
        data.frame()
      }
    },
    .package = "icesDatras"
  )

  res <- lookup_datras_indices(126436, surveys = "BITS", timeout = 5)

  expect_true(res$success)
  expect_equal(unique(res$data$year), 2025L)   # stepped back from empty 2026
  expect_equal(res$data$abundance_index, 10)   # 7 + 3
})

test_that("lookup_datras_indices returns ALL explicitly-requested years (no step-back)", {
  skip_if_not_installed("icesDatras")
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/ices_lookups.R"), local = TRUE)

  # The newest-first step-back only applies when `years` is NULL. When a
  # caller names specific years, every requested year that has data must be
  # returned, not just the most recent one.
  testthat::local_mocked_bindings(
    getSurveyYearQuarterList = function(survey, year) 1L,
    getIndices = function(survey, year, quarter, species) {
      data.frame(
        Survey = "BITS", Year = year, Quarter = quarter,
        IndexArea = "BS_CodEast", Age_0 = 1,
        stringsAsFactors = FALSE, check.names = FALSE
      )
    },
    .package = "icesDatras"
  )

  res <- lookup_datras_indices(126436, surveys = "BITS",
                               years = c(2024L, 2025L), timeout = 5)

  expect_true(res$success)
  expect_setequal(unique(res$data$year), c(2024L, 2025L))
})

test_that("lookup_datras_indices defaults exclude BIAS (not a DATRAS survey)", {
  skip_if_not_installed("icesDatras")
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/ices_lookups.R"), local = TRUE)

  default_surveys <- eval(formals(lookup_datras_indices)$surveys)
  expect_false("BIAS" %in% default_surveys)
  expect_setequal(default_surveys, c("BITS", "NS-IBTS", "BTS"))
})

test_that("lookup_datras_indices min_age changes the sum and keeps a separate cache entry", {
  skip_if_not_installed("icesDatras")
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/ices_lookups.R"), local = TRUE)

  testthat::local_mocked_bindings(
    getSurveyYearList = function(survey) 2023L,
    getSurveyYearQuarterList = function(survey, year) 1L,
    getIndices = function(survey, year, quarter, species) data.frame(
      Survey = "BITS", Year = 2023L, Quarter = quarter,
      IndexArea = "X",
      Age_0 = 100, Age_1 = 50, Age_2 = 8, Age_3 = 2,
      stringsAsFactors = FALSE, check.names = FALSE
    ),
    .package = "icesDatras"
  )

  all_ages <- lookup_datras_indices(126436, surveys = "BITS", years = 2023, min_age = 0L)
  aged     <- lookup_datras_indices(126436, surveys = "BITS", years = 2023, min_age = 2L)

  expect_true(all_ages$success, label = "all_ages$success")
  expect_true(aged$success,     label = "aged$success")
  expect_equal(sum(all_ages$data$abundance_index), 160)   # 100+50+8+2
  expect_equal(sum(aged$data$abundance_index), 10)        # 8+2 only (age-0/1 dropped)
  # Distinct cache entries prove the min_age suffix is in the key (not one shared entry)
  expect_false(is.null(.ices_cache[["datras_126436_BITS_2023_a0"]]))
  expect_false(is.null(.ices_cache[["datras_126436_BITS_2023_a2"]]))
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
  # A live network call can legitimately fail (API down) or find no indices
  # for the most-recent year yet. skip_if (not the if/else "empty test"
  # anti-pattern) makes that visible as a skip with a reason, while still
  # asserting the real contract whenever data IS returned.
  skip_if(!isTRUE(res$success),
          paste("DATRAS returned no cod indices for BITS:", res$error))
  expect_s3_class(res$data, "data.frame")
  expect_true(all(c("survey", "year", "quarter",
                    "index_area", "abundance_index") %in% names(res$data)))
  expect_false("biomass_index" %in% names(res$data))
  expect_true(all(res$data$survey == "BITS"))
  expect_true(is.numeric(res$data$abundance_index))
  expect_gt(nrow(res$data), 0)
})

test_that("fetch_sag_ssb rejects an empty stock key (offline contract)", {
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/ices_lookups.R"), local = TRUE)

  bad <- fetch_sag_ssb("")
  expect_false(bad$success)
  expect_equal(bad$source, "SAG_SSB")
  expect_true(nzchar(bad$error))
})

test_that("fetch_sag_ssb returns an SSB series for Baltic sprat (live)", {
  skip_if_no_live_tests()
  skip_if_not_installed("icesSAG")
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/rpath/survey_validation.R"), local = TRUE)
  source(file.path(app_root, "R/functions/ices_lookups.R"), local = TRUE)

  # spr.27.22-32 SSB is reported in tonnes and is a low-flake assessed stock.
  res <- fetch_sag_ssb("spr.27.22-32", timeout = 90)
  skip_if(!isTRUE(res$success),
          paste("SAG returned no SSB for spr.27.22-32:", res$error))
  expect_s3_class(res$data, "data.frame")
  expect_setequal(names(res$data), c("year", "survey_value"))
  expect_true(is.numeric(res$data$survey_value))
  expect_gt(nrow(res$data), 0)
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

test_that("lookup_ices_subdivision loads from a layer_path file when cache empty", {
  skip_if_not_installed("sf")
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/ices_lookups.R"), local = TRUE)

  if (!is.null(.ices_cache$areas_sf)) rm("areas_sf", envir = .ices_cache)
  on.exit(
    if (!is.null(.ices_cache$areas_sf)) rm("areas_sf", envir = .ices_cache),
    add = TRUE
  )

  fixture <- sf::st_sf(
    Area_Full = "27.3.d.27", Major_FA = "27", SubArea = "3",
    Division = "d", SubDivisio = "27", Unit = " ",
    geometry = sf::st_sfc(.sq(18, 19, 56, 57), crs = 4326)
  )
  tmp <- tempfile(fileext = ".gpkg")
  on.exit(unlink(tmp), add = TRUE)
  sf::st_write(fixture, tmp, quiet = TRUE)

  res <- lookup_ices_subdivision(18.5, 56.5, layer_path = tmp)
  expect_true(res$success)
  expect_equal(res$data$area_full, "27.3.d.27")
})

test_that("lookup_ices_subdivision surfaces download failures as structured failure + warning", {
  skip_if_not_installed("sf")
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/ices_lookups.R"), local = TRUE)

  # Force the download branch: empty in-session cache, point on-disk cache
  # at a non-existent path, point the URL at an unresolvable host.
  if (!is.null(.ices_cache$areas_sf)) rm("areas_sf", envir = .ices_cache)
  .ICES_AREAS_CACHE_FILE <- tempfile(fileext = ".gpkg")            # nonexistent
  .ICES_AREAS_WFS_URL    <- "http://invalid.example.invalid/nope"  # unresolvable

  expect_warning(
    res <- lookup_ices_subdivision(19, 57, timeout = 2),
    "layer load failed"
  )
  expect_false(res$success)
  expect_true(nzchar(res$error) && !is.na(res$error))
})

test_that("lookup_ices_subdivision resolves a real Baltic point (live)", {
  skip_if_not(identical(Sys.getenv("RUN_LIVE_TESTS"), "true"),
              "Live ICES WFS test; set RUN_LIVE_TESTS=true to enable")
  skip_if_not_installed("sf")
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/ices_lookups.R"), local = TRUE)

  # Force a real network fetch: clear BOTH the in-session env entry and the
  # on-disk gpkg cache. Otherwise a prior session's cache would silently
  # short-circuit the download path, defeating the live test. The function
  # will re-create cache/spatial/ices_areas.gpkg on success.
  if (!is.null(.ices_cache$areas_sf)) rm("areas_sf", envir = .ices_cache)
  if (file.exists(.ICES_AREAS_CACHE_FILE)) unlink(.ICES_AREAS_CACHE_FILE)

  # Also clear AFTER, so repeated local live runs continue to exercise the
  # WFS download path instead of silently hitting the cache we just wrote.
  on.exit({
    if (!is.null(.ices_cache$areas_sf)) rm("areas_sf", envir = .ices_cache)
    if (file.exists(.ICES_AREAS_CACHE_FILE)) unlink(.ICES_AREAS_CACHE_FILE)
  }, add = TRUE)

  res <- lookup_ices_subdivision(19.0, 57.0)
  expect_true(res$success)
  expect_match(res$data$area_full, "^27\\.3\\.")
})
