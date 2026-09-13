# =============================================================================
# Rpath diagnostics and trophic pyramid (#12 / #13)
# =============================================================================
# Both read trophic levels. rpath_values$params$model is the Rpath *input*
# object from create.rpath.params(): it carries uppercase $Type and has no $TL
# at all. TL is an *output* of balancing (rpath_balancing.R sets model$TL), and
# the balanced model uses lowercase $type. Reading TL off params$model gave
# mean TL = NA for every model, and max(NULL) = -Inf made the pyramid's
# seq(1, ceiling(-Inf)) throw "'to' must be a finite number".

source_app_dependencies()

local({
  root <- get_app_root()
  source(file.path(root, "R/functions/rpath/rpath_workflows.R"), local = FALSE)
})

# A balanced ecopath_model: lowercase type, TL present.
balanced_model <- function() {
  data.frame(
    Group = c("Phytoplankton", "Detritus", "Zooplankton", "Cod", "Fleet"),
    type = c(1, 2, 0, 0, 3),
    Biomass = c(20, 50, 5, 1, NA),
    PB = c(100, 0, 30, 0.5, NA),
    TL = c(1.0, 1.0, 2.1, 3.6, NA),
    stringsAsFactors = FALSE
  )
}

# The Rpath params object: uppercase Type, NO TL column.
params_model <- function() {
  data.frame(
    Group = c("Phytoplankton", "Detritus", "Zooplankton", "Cod"),
    Type = c(1, 2, 0, 0),
    Biomass = c(20, 50, 5, 1),
    PB = c(100, 0, 30, 0.5),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# #12 - diagnostics
# ---------------------------------------------------------------------------

test_that("calculate_ecopath_diagnostics returns a real mean trophic level", {
  d <- calculate_ecopath_diagnostics(balanced_model())

  # Living groups are type < 3: Phyto 1.0, Detritus 1.0, Zoo 2.1, Cod 3.6
  expect_equal(d$mean_trophic_level, mean(c(1.0, 1.0, 2.1, 3.6)))
  expect_false(is.na(d$mean_trophic_level))
})

test_that("calculate_ecopath_diagnostics counts groups by lowercase type", {
  d <- calculate_ecopath_diagnostics(balanced_model())

  expect_equal(d$n_groups, 4L)      # type < 3
  expect_equal(d$n_producers, 1L)   # type == 1
  expect_equal(d$n_consumers, 2L)   # type == 0
  expect_equal(d$total_biomass, 20 + 50 + 5 + 1)
  expect_equal(d$primary_production, 20 * 100)
})

test_that("calculate_ecopath_diagnostics rejects an unbalanced params object", {
  # The exact bug: handed params$model, it must say so rather than quietly
  # reporting mean TL = NA.
  expect_warning(d <- calculate_ecopath_diagnostics(params_model()),
                 regexp = "TL", ignore.case = TRUE)
  expect_null(d)
})

# ---------------------------------------------------------------------------
# #13 - trophic pyramid
# ---------------------------------------------------------------------------

test_that("trophic_pyramid_bins bins the living groups by trophic level", {
  bins <- trophic_pyramid_bins(balanced_model())

  expect_true(is.numeric(bins))
  expect_true(length(bins) > 0)
  # Every living group's biomass must land somewhere - total is conserved.
  expect_equal(sum(bins, na.rm = TRUE), 20 + 50 + 5 + 1)
})

test_that("trophic_pyramid_bins keeps TL = 1 producers in the base bin", {
  # cut() is right-closed and excludes the lowest break by default, so the
  # primary producers at exactly TL 1.0 - the base of the pyramid - silently
  # vanished from the plot.
  bins <- trophic_pyramid_bins(balanced_model())
  expect_equal(unname(bins[1]), 70)  # Phytoplankton 20 + Detritus 50
})

test_that("trophic_pyramid_bins does not throw when every TL is 1", {
  flat <- data.frame(
    Group = c("Phytoplankton", "Detritus"), type = c(1, 2),
    Biomass = c(20, 50), PB = c(100, 0), TL = c(1.0, 1.0),
    stringsAsFactors = FALSE
  )
  # ceiling(max(TL)) == 1 makes seq(1, 1, by = 0.5) a single point; the old
  # code would produce an unusable break vector.
  expect_no_error(bins <- trophic_pyramid_bins(flat))
  expect_equal(sum(bins, na.rm = TRUE), 70)
})

test_that("trophic_pyramid_bins warns and returns NULL without a TL column", {
  expect_warning(b <- trophic_pyramid_bins(params_model()),
                 regexp = "TL", ignore.case = TRUE)
  expect_null(b)
})

# ---------------------------------------------------------------------------
# The server must read the balanced model, not the params object
# ---------------------------------------------------------------------------

test_that("rpath_server.R never reads TL off params$model", {
  code <- readLines(app_path("R/modules/rpath_server.R"), warn = FALSE)
  code <- code[!startsWith(trimws(code), "#")]

  # Find every `model <- rpath_values$params$model` and check no $TL read
  # follows it before the next model reassignment.
  offenders <- character(0)
  assigns <- grep("model *<- *rpath_values[$]params[$]model", code)
  for (a in assigns) {
    nxt <- assigns[assigns > a]
    end <- if (length(nxt)) nxt[1] - 1L else length(code)
    block <- code[a:min(end, a + 60L)]
    if (any(grepl("[$]TL", block))) {
      offenders <- c(offenders, sprintf("line %d", a))
    }
  }

  expect_equal(offenders, character(0),
               label = "params$model blocks that read $TL")
})
