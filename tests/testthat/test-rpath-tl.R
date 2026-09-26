# =============================================================================
# A2: Rpath trophic levels and diagnostics (F40, F44, F41)
# =============================================================================
# F40 - run_ecopath_balance() overwrote Rpath's TL with a recomputation that
#       read DC[i, j] as "prey j of predator i"; Rpath's DC is prey x
#       predator, so the override was transposed.
# F44 - the diagnostics rejected the class-"Rpath" list the server stores and
#       counted detritus (type 2) as living.
# F41 - the converter zeroed cannibalism without renormalising the column.

source_app_dependencies()

local({
  root <- get_app_root()
  source(file.path(root, "R/functions/rpath/rpath_workflows.R"), local = FALSE)
  source(file.path(root, "R/functions/rpath/rpath_conversion.R"), local = FALSE)
})

# What Rpath::rpath() returns: a LIST of per-group vectors, class "Rpath".
rpath_object <- function() {
  structure(
    list(
      NUM_GROUPS = 5,
      Group = c("Phyto", "Det", "Zoo", "Cod", "Fleet"),
      type = c(1, 2, 0, 0, 3),
      TL = c(1, 1, 2, 3, NA),
      Biomass = c(20, 50, 5, 1, NA),
      PB = c(100, 0, 30, 0.5, NA)
    ),
    class = "Rpath"
  )
}

test_that("diagnostics accept a class-Rpath list and exclude detritus from living", {
  d <- calculate_ecopath_diagnostics(rpath_object())

  expect_false(is.null(d))
  expect_equal(d$n_groups, 3L)
  expect_equal(d$mean_trophic_level, (1 + 2 + 3) / 3)
  expect_equal(d$n_detritus, 1L)
  expect_equal(d$total_biomass, 20 + 5 + 1)
  expect_equal(d$primary_production, 20 * 100)
})

test_that("trophic pyramid accepts a class-Rpath list and excludes detritus", {
  bins <- trophic_pyramid_bins(rpath_object())

  expect_false(is.null(bins))
  expect_equal(sum(bins, na.rm = TRUE), 20 + 5 + 1)  # Det's 50 is not living
  expect_equal(unname(bins[1]), 20)                  # only Phyto at TL 1
})

test_that("an Rpath object with ragged vectors warns and returns NULL", {
  bad <- rpath_object()
  bad$TL <- c(1, 2)
  expect_warning(d <- calculate_ecopath_diagnostics(bad), "5 groups but 2 'TL'")
  expect_null(d)
})

test_that("an unsupported model object warns and returns NULL", {
  expect_warning(d <- calculate_ecopath_diagnostics(list(TL = 1)), "unsupported")
  expect_null(d)
})

test_that("rpath_balancing.R no longer recomputes or overwrites Rpath TL (F40)", {
  code <- readLines(app_path("R/functions/rpath/rpath_balancing.R"), warn = FALSE)
  code <- code[!startsWith(trimws(code), "#")]

  expect_false(any(grepl("calculate_rpath_trophic_levels", code, fixed = TRUE)))
  expect_false(any(grepl("model$TL <-", code, fixed = TRUE)))
  # The balanced object's column is lowercase `type`; `model$Type` after the
  # Rpath::rpath() call always counted 0 living groups.
  expect_true(any(grepl("sum(model$type < 2", code, fixed = TRUE)))
})

test_that("live: Rpath balance of Phyto -> Zoo -> Fish keeps TL 1, 2, 3", {
  skip_if_no_live_tests()
  skip_if_not_installed("Rpath")
  source(app_path("R/functions/rpath/rpath_balancing.R"), local = FALSE)
  ecopath_data <- list(
    group_data = data.frame(
      GroupID = 1:4, GroupName = c("Phyto", "Det", "Zoo", "Fish"), Type = c(1, 2, 0, 0),
      Biomass = c(10, 5, 2, 0.5), ProdBiom = c(100, NA, 20, 1),
      ConsBiom = c(NA, NA, 60, 4), EcoEfficiency = c(NA, NA, NA, NA),
      stringsAsFactors = FALSE
    ),
    diet_data = data.frame(PredID = c(3, 4), PreyID = c(1, 3), Diet = c(1, 1))
  )
  params <- suppressWarnings(convert_ecopath_to_rpath(ecopath_data))
  model <- with_timeout(run_ecopath_balance(params), timeout = 60)
  tl <- setNames(as.numeric(model$TL), model$Group)
  expect_equal(unname(tl[c("Phyto", "Zoo", "Fish")]), c(1, 2, 3), tolerance = 1e-6)
})
