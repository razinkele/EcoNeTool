# =============================================================================
# calculate_mti() / calculate_keystoneness(): known answers (F65)
# =============================================================================
# The old MTI row-normalised over prey rows and returned -(I - DC)^-1 DC, which
# can never be positive, so producers never "helped" their consumers, and the
# old KS = log(1 + OE) / log(1 + p) was not Libralato's index. The known
# answers below were computed by hand from Ulanowicz & Puccia (1990) and
# Libralato et al. (2006) and re-verified independently (spec A, section 5).

source_app_dependencies()

local({
  root <- get_app_root()
  source(file.path(root, "R/functions/keystoneness.R"), local = FALSE)
})
suppressPackageStartupMessages(library(igraph))

# P -> Z -> F (P eaten by Z, Z eaten by F)
chain <- function() make_graph(c("P", "Z", "Z", "F"), directed = TRUE)
chain_info <- function() data.frame(species = c("P", "Z", "F"), meanB = c(10, 5, 1))

# P -> Z1, P -> Z2 (two grazers competing for one producer)
fan <- function() make_graph(c("P", "Z1", "P", "Z2"), directed = TRUE)
fan_info <- function() data.frame(species = c("P", "Z1", "Z2"), meanB = c(10, 3, 1))

test_that("MTI of a three-level chain matches the known answer", {
  mti <- calculate_mti(chain(), chain_info())
  # Rows = impactor, columns = impacted
  expected_x3 <- rbind(
    P = c(-1, 1, 1),
    Z = c(-1, -2, 1),
    F = c(1, -1, -1)
  )
  colnames(expected_x3) <- c("P", "Z", "F")
  expect_equal(3 * mti, expected_x3, tolerance = 1e-8)
})

test_that("a producer has a positive impact on its consumer", {
  mti <- calculate_mti(chain(), chain_info())
  # The F65 regression: the old formula could never return a positive value.
  expect_gt(mti["P", "Z"], 0)
})

test_that("predation pressure uses Q/B x B when QB is present", {
  mti <- calculate_mti(fan(), fan_info())
  expect_equal(mti["Z1", "Z2"], -0.375, tolerance = 1e-8)
  expect_equal(mti["Z2", "Z1"], -0.125, tolerance = 1e-8)

  info_qb <- fan_info()
  info_qb$QB <- c(0, 1, 9)  # consumption Z1 = 3, Z2 = 9: the shares swap
  mti_qb <- calculate_mti(fan(), info_qb)
  expect_equal(mti_qb["Z1", "Z2"], -0.125, tolerance = 1e-8)
  expect_equal(mti_qb["Z2", "Z1"], -0.375, tolerance = 1e-8)
})

test_that("diet_prop on the edges replaces the equal diet split", {
  # Z eats P1 (80%) and P2 (20%); with an equal split both would be 0.5.
  net <- make_graph(c("P1", "Z", "P2", "Z"), directed = TRUE)
  E(net)$diet_prop <- c(0.8, 0.2)
  info <- data.frame(species = c("P1", "Z", "P2"), meanB = c(1, 1, 1))
  mti <- calculate_mti(net, info)
  # Direct effect of each prey on Z is its diet share (no other paths here).
  expect_equal(mti["P1", "Z"] / mti["P2", "Z"], 4, tolerance = 1e-8)
})

test_that("keystoneness uses Libralato's epsilon and log10(eps * (1 - p)), as in EwE", {
  ks <- calculate_keystoneness(chain(), chain_info())
  ks <- ks[match(c("P", "Z", "F"), ks$species), ]

  expect_equal(ks$overall_effect, rep(sqrt(2 / 9), 3), tolerance = 1e-7)
  expect_equal(ks$relative_biomass, c(10, 5, 1) / 16, tolerance = 1e-8)
  # EwE reports KS with log10 (F-3). The natural-log values were
  # -1.7328680, -1.1267321, -0.8165772.
  expected <- log10(sqrt(2 / 9) * (1 - c(10, 5, 1) / 16))
  expect_equal(ks$keystoneness, expected, tolerance = 1e-7)
  expect_equal(ks$keystoneness, c(-1.7328680, -1.1267321, -0.8165772) / log(10), tolerance = 1e-7)
})

test_that("keystoneness returns the documented columns, statuses and ranks", {
  ks <- calculate_keystoneness(chain(), chain_info())

  expect_named(ks, c("species", "overall_effect", "relative_biomass",
                     "keystoneness", "keystone_status", "ks_rank"))
  expect_true(all(ks$keystone_status %in% c("Keystone", "Dominant", "Other", "Undefined")))
  # Sorted by KS descending; F (-0.355) is the only one in the top quartile
  # and holds 6.25% of biomass, so it is Dominant, not Keystone.
  expect_equal(ks$species, c("F", "Z", "P"))
  expect_equal(ks$ks_rank, 1:3)
  expect_equal(ks$keystone_status, c("Dominant", "Other", "Other"))
})

test_that("a low-biomass top-quartile species is Keystone", {
  # Same chain, but the top predator holds < 5% of the biomass.
  info <- data.frame(species = c("P", "Z", "F"), meanB = c(100, 50, 1))
  ks <- calculate_keystoneness(chain(), info)
  expect_equal(ks$keystone_status[ks$species == "F"], "Keystone")
})

test_that("an isolated node has undefined keystoneness, not an error", {
  net <- make_graph(c("P", "Z"), directed = TRUE) + vertex("X")
  info <- data.frame(species = c("P", "Z", "X"), meanB = c(10, 5, 1))
  ks <- calculate_keystoneness(net, info)
  expect_true(is.na(ks$keystoneness[ks$species == "X"]))
  expect_equal(ks$keystone_status[ks$species == "X"], "Undefined")
  expect_true(is.na(ks$ks_rank[ks$species == "X"]))
})

test_that("a web with no basal species still yields a finite MTI", {
  net <- make_graph(c("A", "B", "B", "A"), directed = TRUE)
  info <- data.frame(species = c("A", "B"), meanB = c(1, 1))
  expect_no_error(mti <- calculate_mti(net, info))
  expect_true(all(is.finite(mti)))
})

test_that("a singular (I - Q) falls back to the pseudo-inverse with a warning", {
  # No small food web makes (I - Q) numerically singular (exhaustive search of
  # every 2- and 3-node digraph), so force the branch through the rcond seam.
  with_mocked_function(globalenv(), ".mti_rcond", function(A) 0, {
    expect_warning(mti <- calculate_mti(chain(), chain_info()), "pseudo-inverse")
  })
  expect_true(all(is.finite(mti)))
  # For an invertible matrix the pseudo-inverse equals the inverse.
  expect_equal(3 * mti["P", "Z"], 1, tolerance = 1e-8)
})

test_that("partially missing diet_prop falls back to the equal split with a warning", {
  net <- make_graph(c("P1", "Z", "P2", "Z"), directed = TRUE)
  info <- data.frame(species = c("P1", "Z", "P2"), meanB = c(1, 1, 1))
  binary <- calculate_mti(net, info)
  E(net)$diet_prop <- c(0.8, NA)
  expect_warning(mixed <- calculate_mti(net, info), "equal diet split")
  expect_equal(mixed, binary, tolerance = 1e-12)
})

test_that("an NA biomass gives an Undefined status, not an error", {
  info <- data.frame(species = c("P", "Z", "F"), meanB = c(10, NA, 1))
  expect_no_error(ks <- calculate_keystoneness(chain(), info))
  expect_equal(ks$keystone_status[ks$species == "Z"], "Undefined")
  expect_true(all(ks$keystone_status[ks$species != "Z"] %in% c("Keystone", "Dominant", "Other")))
})

# ---------------------------------------------------------------------------
# F-5: help text matches the four statuses calculate_keystoneness() returns
# ---------------------------------------------------------------------------

test_that("dashboard and keystoneness help name the current statuses (F-5)", {
  root <- get_app_root()
  dash <- paste(readLines(file.path(root, "R/ui/dashboard_ui.R"), warn = FALSE), collapse = "\n")
  expect_false(grepl("Keystone, Dominant, or Rare", dash, fixed = TRUE))
  expect_true(grepl("Keystone, Dominant, Other or Undefined", dash, fixed = TRUE))

  ks_ui <- paste(readLines(file.path(root, "R/ui/keystoneness_ui.R"), warn = FALSE), collapse = "\n")
  expect_false(grepl("Producers now show", ks_ui, fixed = TRUE))
  undefined_item <- regmatches(ks_ui, regexpr("<strong>Undefined:</strong>[^<]*", ks_ui))
  expect_match(undefined_item, "biomass", ignore.case = TRUE)
  expect_match(undefined_item, "missing|NA", ignore.case = FALSE)
})
