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
