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

  # Cod: 6 rising years, ref year = 2018 (the lowest -> rank 1 of 6).
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
