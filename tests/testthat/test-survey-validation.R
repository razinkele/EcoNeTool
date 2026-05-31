# Unit + gated live tests for the R2 DATRAS survey-trends pure core.

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")

test_that("aggregate_survey_series pins quarter and sums chosen areas per year", {
  source(file.path(app_root, "R/functions/rpath/survey_validation.R"), local = TRUE)

  # lookup_datras_indices()$data shape: survey/year/quarter/index_area/abundance_index
  reshaped <- data.frame(
    survey = "BITS",
    year = c(2022L, 2022L, 2022L, 2023L, 2023L),
    quarter = c(1L, 1L, 4L, 1L, 1L),
    index_area = c("BS_CodEast", "BS_CodWest", "BS_CodEast", "BS_CodEast", "BS_CodWest"),
    abundance_index = c(10, 5, 99, 20, 8),
    stringsAsFactors = FALSE
  )

  # Quarter 1 only (drops the Q4=99 row); East+West areas summed per year.
  out <- aggregate_survey_series(reshaped, quarter = 1L,
                                 areas = c("BS_CodEast", "BS_CodWest"))
  expect_s3_class(out, "data.frame")
  expect_setequal(names(out), c("year", "survey_value"))
  expect_equal(out$year, c(2022L, 2023L))
  expect_equal(out$survey_value, c(15, 28))   # 10+5 ; 20+8 ; Q4 99 excluded

  # East only -> the stock double-count guard: West dropped.
  east <- aggregate_survey_series(reshaped, quarter = 1L, areas = "BS_CodEast")
  expect_equal(east$survey_value, c(10, 20))
})

test_that("aggregate_sag_ssb extracts Year+SSB and drops NA SSB years", {
  source(file.path(app_root, "R/functions/rpath/survey_validation.R"), local = TRUE)

  # icesSAG::getSummaryTable()-shaped frame (Year, SSB among many cols)
  ss <- data.frame(Year = 2018:2022, SSB = c(100, NA, 120, 130, 140),
                   F = c(0.3, 0.3, 0.3, 0.3, 0.3), stringsAsFactors = FALSE)
  out <- aggregate_sag_ssb(ss)
  expect_setequal(names(out), c("year", "survey_value"))
  expect_equal(out$year, c(2018L, 2020L, 2021L, 2022L))   # NA-SSB year dropped
  expect_equal(out$survey_value, c(100, 120, 130, 140))

  # missing SSB column -> empty frame, not an error
  empty <- aggregate_sag_ssb(data.frame(Year = 2020, F = 0.3))
  expect_equal(nrow(empty), 0)
})

test_that("clupeid SAG map pairs herring/sprat AphiaIDs to their stock keys", {
  source(file.path(app_root, "R/functions/rpath/survey_validation.R"), local = TRUE)
  expect_equal(SURVEY_TRENDS_CLUPEID_SAG[["126417"]], "her.27.25-2932")  # herring
  expect_equal(SURVEY_TRENDS_CLUPEID_SAG[["126425"]], "spr.27.22-32")    # sprat
})

test_that(".map_group_to_aphia flags BITS-non-indexed species as not_surveyed", {
  source(file.path(app_root, "R/functions/rpath/survey_validation.R"), local = TRUE)

  dict <- data.frame(group = c("Cod", "Turbot", "Herring"),
                     aphia_id = c(126436, 127149, 126417), stringsAsFactors = FALSE)
  bits <- c(126436, 127141, 127143)   # cod, flounder, plaice (verified BITS-indexed)

  # BITS-indexed species -> demersal
  expect_equal(.map_group_to_aphia("Cod", dict, SURVEY_TRENDS_PELAGIC_APHIA, bits)$class,
               "demersal")
  # resolves, demersal, but NOT BITS-indexed -> not_surveyed (no wasted fetch)
  expect_equal(.map_group_to_aphia("Turbot", dict, SURVEY_TRENDS_PELAGIC_APHIA, bits)$class,
               "not_surveyed")
  # pelagic takes precedence over the allowlist
  expect_equal(.map_group_to_aphia("Herring", dict, SURVEY_TRENDS_PELAGIC_APHIA, bits)$class,
               "pelagic")
  # back-compat: no allowlist (NULL) -> any non-pelagic resolved name is demersal
  expect_equal(.map_group_to_aphia("Turbot", dict, SURVEY_TRENDS_PELAGIC_APHIA)$class,
               "demersal")
  # the shipped constant exists and contains cod/flounder/plaice
  expect_true(all(c(126436, 127141, 127143) %in% SURVEY_TRENDS_BITS_INDEXED_APHIA))
})

test_that(".map_group_to_aphia: dictionary hit, multi-match->NA, pelagic class", {
  skip_if_not_installed("worrms")
  source(file.path(app_root, "R/functions/rpath/survey_validation.R"), local = TRUE)

  dict <- data.frame(group = "Cod", aphia_id = 126436, stringsAsFactors = FALSE)

  # Dictionary hit -> demersal
  hit <- .map_group_to_aphia("Cod", dict, SURVEY_TRENDS_PELAGIC_APHIA)
  expect_equal(hit$aphia_id, 126436)
  expect_equal(hit$class, "demersal")

  testthat::local_mocked_bindings(
    wm_name2id = function(name) {
      if (name == "Herring") return(126417L)        # pelagic
      if (name == "Flounder") return(127141L)       # clean single demersal
      if (name == "Demersal fish") return(c(1L, 2L))  # ambiguous multi-match
      stop("no match")
    },
    .package = "worrms"
  )

  # Pelagic AphiaID -> excluded class
  pel <- .map_group_to_aphia("Herring", dict, SURVEY_TRENDS_PELAGIC_APHIA)
  expect_equal(pel$class, "pelagic")

  # Clean fallback resolution -> demersal
  fl <- .map_group_to_aphia("Flounder", dict, SURVEY_TRENDS_PELAGIC_APHIA)
  expect_equal(fl$aphia_id, 127141)
  expect_equal(fl$class, "demersal")

  # Multi-match -> NA / unmapped (never silent [1])
  amb <- .map_group_to_aphia("Demersal fish", dict, SURVEY_TRENDS_PELAGIC_APHIA)
  expect_true(is.na(amb$aphia_id))
  expect_equal(amb$class, "unmapped")

  # Resolver error -> NA / unmapped
  none <- .map_group_to_aphia("Nonsense", dict, SURVEY_TRENDS_PELAGIC_APHIA)
  expect_true(is.na(none$aphia_id))
  expect_equal(none$class, "unmapped")
})

test_that("compute_survey_trends: rel series, ref rank-of-n, glyph, guards, schema", {
  source(file.path(app_root, "R/functions/rpath/survey_validation.R"), local = TRUE)

  # Cod: 6 rising years, ref year = 2018 (value 11, 2nd lowest -> rank 2 of 6).
  cod <- data.frame(
    group = "Cod",
    year = 2016:2021,
    survey_value = c(10, 12, 11, 20, 30, 40),  # 2018 is index 3 (value 11)... set ref below
    is_ref_year = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  # 2-year group (Dab) -> omitted (<3 years); all-zero group (Plaice) -> excluded.
  dab <- data.frame(group = "Dab", year = c(2020, 2021),
                    survey_value = c(5, 6), is_ref_year = c(FALSE, TRUE),
                    stringsAsFactors = FALSE)
  plaice <- data.frame(group = "Plaice", year = 2016:2020,
                       survey_value = rep(0, 5), is_ref_year = c(rep(FALSE,4), TRUE),
                       stringsAsFactors = FALSE)
  series <- rbind(cod, dab, plaice)

  res <- compute_survey_trends(series)

  expect_setequal(names(res), c("success", "status", "trends", "excluded"))
  expect_true(res$success)
  expect_equal(res$status, "ok")

  # Cod rel series normalised to mean 1
  cod_rows <- res$trends[res$trends$group == "Cod", ]
  expect_equal(mean(cod_rows$rel), 1)
  expect_equal(unique(cod_rows$n_years), 6L)
  # ref year (2018, value 11) is the 2nd lowest of 6 -> rank 2
  expect_equal(unique(cod_rows$ref_rank), 2)
  # >=5 yr rising series -> direction "up"
  expect_equal(unique(cod_rows$direction), "up")

  # Dab omitted (<3 yr) and Plaice excluded (all-zero), both with reasons
  expect_true(all(c("Dab", "Plaice") %in% res$excluded$group))
  expect_false("Dab" %in% res$trends$group)
  expect_false("Plaice" %in% res$trends$group)
})

test_that("compute_survey_trends: <1 usable group -> insufficient", {
  source(file.path(app_root, "R/functions/rpath/survey_validation.R"), local = TRUE)
  only_short <- data.frame(group = "Dab", year = c(2020, 2021),
                           survey_value = c(5, 6), is_ref_year = c(FALSE, TRUE),
                           stringsAsFactors = FALSE)
  res <- compute_survey_trends(only_short)
  expect_false(res$success)
  expect_equal(res$status, "insufficient")
})

# R1: 4-year series -> group IS included in trends (4>=3) but direction = NA_character_
#     (glyph requires >=5 non-NA years)
test_that("R1: 4-year rising Cod series yields direction NA (no glyph below 5 years)", {
  source(file.path(app_root, "R/functions/rpath/survey_validation.R"), local = TRUE)

  cod4 <- data.frame(
    group        = "Cod",
    year         = 2018:2021,
    survey_value = c(5, 10, 15, 20),
    is_ref_year  = c(FALSE, FALSE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  res <- compute_survey_trends(cod4)

  cod_rows <- res$trends[res$trends$group == "Cod", ]
  expect_true(nrow(cod_rows) > 0L)         # group IS in trends (4 >= 3)
  expect_true(all(is.na(cod_rows$direction)))  # no glyph for <5 years
})

# R2: All-NA survey_value -> group dropped to 0 non-NA rows after FIX 2 ->
#     excluded (n < 3), NOT in trends
test_that("R2: all-NA survey_value group is excluded, not in trends", {
  source(file.path(app_root, "R/functions/rpath/survey_validation.R"), local = TRUE)

  all_na <- data.frame(
    group        = "Ghost",
    year         = 2015:2019,
    survey_value = rep(NA_real_, 5),
    is_ref_year  = c(rep(FALSE, 4), TRUE),
    stringsAsFactors = FALSE
  )
  res <- compute_survey_trends(all_na)

  expect_true("Ghost" %in% res$excluded$group)
  expect_false(isTRUE("Ghost" %in% res$trends$group))
})

# R3: Duplicate (group, year) row warns with "survey_trends" and de-dups to
#     one trends row per distinct year
test_that("R3: duplicate (group, year) warns 'survey_trends' and de-dups correctly", {
  source(file.path(app_root, "R/functions/rpath/survey_validation.R"), local = TRUE)

  cod_dup <- data.frame(
    group        = "Cod",
    year         = c(2016L, 2017L, 2017L, 2018L),  # 2017 duplicated
    survey_value = c(10, 12, 99, 20),
    is_ref_year  = c(FALSE, FALSE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  expect_warning(
    res <- compute_survey_trends(cod_dup),
    "survey_trends"
  )

  cod_rows <- res$trends[res$trends$group == "Cod", ]
  # De-duped: 3 distinct years -> 3 rows in trends
  expect_equal(nrow(cod_rows), 3L)
  expect_equal(sort(unique(cod_rows$year)), c(2016L, 2017L, 2018L))
})

# R4: aggregate_survey_series() warns "survey_trends" when an area-year cell
#     has an NA abundance_index
test_that("R4: partial-NA abundance_index in aggregate_survey_series warns 'survey_trends'", {
  source(file.path(app_root, "R/functions/rpath/survey_validation.R"), local = TRUE)

  reshaped_na <- data.frame(
    survey          = "BITS",
    year            = c(2020L, 2020L, 2021L, 2021L),
    quarter         = c(1L, 1L, 1L, 1L),
    index_area      = c("BS_CodEast", "BS_CodWest", "BS_CodEast", "BS_CodWest"),
    abundance_index = c(10, NA_real_, 20, 8),
    stringsAsFactors = FALSE
  )

  expect_warning(
    aggregate_survey_series(reshaped_na, quarter = 1L, areas = NULL),
    "survey_trends"
  )
})

test_that("aggregate_bias_series sums per-SD rows per year, scoped to one stock", {
  source(file.path(app_root, "R/functions/rpath/survey_validation.R"), local = TRUE)

  bias <- data.frame(
    aphia_id = c(126417, 126417, 126417, 126417, 126425),
    stock = c("her.27.25-2932", "her.27.25-2932", "her.27.25-2932",
              "her.27.28", "spr.27.22-32"),  # 4th row: a DIFFERENT herring stock
    sd = c("25-28.2", "29+32", "25-28.2", "28.1", "22-32"),
    year = c(2010L, 2010L, 2011L, 2010L, 2010L),
    abundance_index = c(100, 50, 200, 999, 777),
    unit = "million_ind", source = "test",
    stringsAsFactors = FALSE
  )

  # stock-scoped: only her.27.25-2932 rows; the her.27.28 (999) row excluded
  out <- aggregate_bias_series(bias, 126417, "her.27.25-2932")
  expect_s3_class(out, "data.frame")
  expect_setequal(names(out), c("year", "survey_value"))
  expect_equal(out$year, c(2010L, 2011L))
  expect_equal(out$survey_value, c(150, 200))  # 100+50 ; 200 ; cross-stock 999 dropped

  # sprat scoped to its own stock
  expect_equal(aggregate_bias_series(bias, 126425, "spr.27.22-32")$survey_value, 777)

  # missing required column -> empty frame with the canonical schema, not an error
  empty <- aggregate_bias_series(data.frame(aphia_id = 126417, year = 2010), 126417)
  expect_equal(nrow(empty), 0)
  expect_setequal(names(empty), c("year", "survey_value"))
})

test_that("aggregate_bias_series with stock_key=NULL sums across all stocks for the aphia", {
  source(file.path(app_root, "R/functions/rpath/survey_validation.R"), local = TRUE)

  bias <- data.frame(
    aphia_id = c(126417, 126417, 126417),
    stock = c("her.27.25-2932", "her.27.28", "her.27.25-2932"),
    sd = c("a", "b", "c"),
    year = c(2010L, 2010L, 2011L),
    abundance_index = c(100, 999, 200),
    unit = "u", source = "s", stringsAsFactors = FALSE
  )
  # No stock_key -> both 2010 herring stocks summed (100 + 999), 2011 = 200
  out <- aggregate_bias_series(bias, 126417)
  expect_equal(out$year, c(2010L, 2011L))
  expect_equal(out$survey_value, c(1099, 200))
})

test_that("aggregate_bias_series drops a whole year if one SD row is NA (na.rm=FALSE)", {
  source(file.path(app_root, "R/functions/rpath/survey_validation.R"), local = TRUE)

  bias <- data.frame(
    aphia_id = 126417, stock = "her.27.25-2932", sd = c("a", "b", "c"),
    year = c(2010L, 2010L, 2011L), abundance_index = c(100, NA, 200),
    unit = "u", source = "s", stringsAsFactors = FALSE
  )
  # 2010 has rows 100 + NA -> whole year poisoned to NA -> warn + dropped (NOT 100)
  expect_warning(out <- aggregate_bias_series(bias, 126417, "her.27.25-2932"),
                 "NA BIAS index")
  expect_equal(out$year, 2011L)
  expect_equal(out$survey_value, 200)
})

test_that("aggregate_bias_series warns on an unparseable index distinctly from absent", {
  source(file.path(app_root, "R/functions/rpath/survey_validation.R"), local = TRUE)

  bias <- data.frame(
    aphia_id = 126417, stock = "her.27.25-2932", sd = "a",
    year = c(2010L, 2011L), abundance_index = c("1,234", "200"),  # bad: thousands sep
    unit = "u", source = "s", stringsAsFactors = FALSE
  )
  expect_warning(aggregate_bias_series(bias, 126417, "her.27.25-2932"),
                 "non-numeric abundance_index")
})

test_that("select_clupeid_series chooses BIAS, falls back to SAG, then excludes", {
  source(file.path(app_root, "R/functions/rpath/survey_validation.R"), local = TRUE)

  win <- 2014:2024
  bias_in <- data.frame(
    aphia_id = 126417, stock = "her.27.25-2932", sd = "x",
    year = c(2018L, 2019L, 2020L), abundance_index = c(10, 20, 30),
    unit = "u", source = "s", stringsAsFactors = FALSE
  )
  fake_ok   <- function(key) list(success = TRUE,
                                  data = data.frame(year = 2015:2024,
                                                    survey_value = 1:10))
  fake_fail <- function(key) list(success = FALSE, data = NULL, error = "boom")

  # Branch 1: BIAS rows in window -> BIAS chosen
  b1 <- select_clupeid_series(bias_in, 126417, "her.27.25-2932", win, 2019,
                              fetch_sag = fake_fail)
  expect_equal(b1$class, "clupeid (BIAS acoustic)")
  expect_equal(b1$series$year, c(2018L, 2019L, 2020L))
  expect_equal(b1$series$is_ref_year, c(FALSE, TRUE, FALSE))
  expect_null(b1$excluded_reason)

  # Branch 2: BIAS rows exist but ALL outside window -> SAG fallback
  bias_old <- transform(bias_in, year = c(1990L, 1991L, 1992L))
  b2 <- select_clupeid_series(bias_old, 126417, "her.27.25-2932", win, 2019,
                              fetch_sag = fake_ok)
  expect_equal(b2$class, "clupeid (SAG SSB)")
  expect_true(all(b2$series$year %in% win))

  # Branch 3: BIAS empty + SAG success -> SAG
  b3 <- select_clupeid_series(bias_in[0, ], 126417, "her.27.25-2932", win, 2019,
                              fetch_sag = fake_ok)
  expect_equal(b3$class, "clupeid (SAG SSB)")
  expect_true(nrow(b3$series) > 0)

  # Branch 4: BIAS empty + SAG failure -> exclusion with honest class
  b4 <- select_clupeid_series(bias_in[0, ], 126417, "her.27.25-2932", win, 2019,
                              fetch_sag = fake_fail)
  expect_null(b4$series)
  expect_equal(b4$class, "clupeid (excluded)")
  expect_equal(b4$excluded_reason, "no BIAS or SAG data in window")

  # Branch 5: malformed SAG (success=TRUE but data=NULL) -> exclusion, no crash
  fake_bad <- function(key) list(success = TRUE, data = NULL)
  b5 <- select_clupeid_series(bias_in[0, ], 126417, "her.27.25-2932", win, 2019,
                              fetch_sag = fake_bad)
  expect_null(b5$series)
  expect_equal(b5$excluded_reason, "no BIAS or SAG data in window")
})

test_that("shipped bias_indices.csv has the required schema and clean values", {
  source(file.path(app_root, "R/functions/rpath/survey_validation.R"), local = TRUE)
  f <- file.path(app_root, "R/config/bias_indices.csv")
  skip_if(!file.exists(f), "R/config/bias_indices.csv not present")
  df <- read.csv(f, stringsAsFactors = FALSE)
  expect_true(all(c("aphia_id", "stock", "sd", "year",
                    "abundance_index", "unit", "source") %in% names(df)))
  skip_if(nrow(df) == 0,
          "bias_indices.csv is a header-only stub; populate from WGBIFS to enable BIAS path")
  # Assert against the routing constant, not hard-coded literals (robust to
  # adding a clupeid stock later).
  expect_true(all(as.integer(df$aphia_id) %in% as.integer(names(SURVEY_TRENDS_CLUPEID_SAG))))
  expect_true(all(df$stock %in% unname(SURVEY_TRENDS_CLUPEID_SAG)))
  expect_false(anyNA(suppressWarnings(as.integer(df$year))))
  idx <- suppressWarnings(as.numeric(df$abundance_index))
  expect_false(anyNA(idx))            # clean numeric, no transcription junk
  expect_true(all(idx >= 0))          # abundance indices are non-negative
  expect_true(all(nzchar(trimws(df$source))))   # mechanical no-fabrication guard
})

test_that("survey-trends pipeline returns a real per-year series for cod (live)", {
  skip_if(!identical(Sys.getenv("RUN_LIVE_TESTS"), "true"),
          "Live DATRAS test; set RUN_LIVE_TESTS=true to enable")
  skip_if_not_installed("icesDatras")
  skip_if_not_installed("worrms")
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/ices_lookups.R"), local = TRUE)
  source(file.path(app_root, "R/functions/rpath/survey_validation.R"), local = TRUE)

  aid <- worrms::wm_name2id("Gadus morhua")
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  res <- lookup_datras_indices(aid, surveys = "BITS",
                               years = seq(this_year - 10, this_year), timeout = 90)
  skip_if(!isTRUE(res$success),
          paste("DATRAS returned no cod indices for BITS:", res$error))

  ser <- aggregate_survey_series(res$data, quarter = 1L, areas = NULL)
  expect_s3_class(ser, "data.frame")
  expect_true(all(c("year", "survey_value") %in% names(ser)))
  expect_gt(nrow(ser), 0)
  expect_true(is.numeric(ser$survey_value))
})
