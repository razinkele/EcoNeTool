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
