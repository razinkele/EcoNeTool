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
