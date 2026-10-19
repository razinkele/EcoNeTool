# Tests for P0/P1 bugfixes from deep review (2026-04-04)
# Run: Rscript -e "testthat::test_file('tests/testthat/test-p0p1-fixes.R')"

library(testthat)
library(igraph)
suppressPackageStartupMessages(library(shiny))  # needed for sourcing data_import_server.R

# Source required files in dependency order
app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
source(file.path(app_root, "R/config.R"))
source(file.path(app_root, "R/functions/validation_utils.R"))
source(file.path(app_root, "R/functions/functional_group_utils.R"))
source(file.path(app_root, "R/functions/flux_calculations.R"))
source(file.path(app_root, "R/functions/trophic_levels.R"))
source(file.path(app_root, "R/functions/topological_metrics.R"))
source(file.path(app_root, "R/modules/data_import_server.R"))

test_that("endotherm vertebrates get nonzero metabolic losses", {
  info <- data.frame(
    bodymasses = c(100, 5000, 2000),
    met.types = c("invertebrates", "endotherm vertebrates", "endotherm vertebrates"),
    stringsAsFactors = FALSE
  )
  losses <- calculate_losses(info, temp = 10)
  expect_true(all(losses > 0), info = "All metabolic losses must be positive")
  expect_true(losses[2] > losses[1], info = "Endotherm (5kg mammal) should have higher losses than invertebrate (100g)")
})

test_that("fluxind handles zero-flux matrix without NaN/Inf", {
  zero_mat <- matrix(0, nrow = 3, ncol = 3)
  result <- fluxind(zero_mat)
  expect_false(any(is.nan(unlist(result))), info = "No NaN values in result")
  expect_false(any(is.infinite(unlist(result))), info = "No Inf values in result")
})

test_that("fluxind handles single-species matrix", {
  one_mat <- matrix(0, nrow = 1, ncol = 1)
  result <- fluxind(one_mat, loop = FALSE)
  expect_false(any(is.nan(unlist(result))))
  expect_false(any(is.infinite(unlist(result))))
})

test_that("parse_adjacency_df creates prey-to-predator edges for diet matrix", {
  # Diet matrix: rows = prey, columns = predators
  # Prey A is eaten by Predator C (mat[1,2] = 0.5)
  mat_df <- data.frame(
    Species = c("PreyA", "PreyB"),
    PredatorC = c(0.5, 0.3),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  result <- parse_adjacency_df(mat_df)
  net <- result$net

  # Edge should go from PreyA -> PredatorC (energy flow: prey to predator)
  edges <- as_edgelist(net)
  expect_true(any(edges[,1] == "PreyA" & edges[,2] == "PredatorC"),
              info = "Edge direction must be prey -> predator (energy flow)")
})

test_that("topological indicators handle single-species network", {
  net <- make_empty_graph(n = 1, directed = TRUE)
  V(net)$name <- "Solo"
  result <- get_topological_indicators(net)
  expect_false(any(is.nan(unlist(result))))
  expect_false(any(is.infinite(unlist(result))))
})

test_that("safe_load_rdata rejects unexpected objects in uploaded RData", {
  tmp <- tempfile(fileext = ".RData")
  on.exit(unlink(tmp), add = TRUE)

  net <- make_ring(3, directed = TRUE)
  V(net)$name <- c("A", "B", "C")
  info <- data.frame(species = c("A", "B", "C"), fg = c("A", "B", "C"),
                     meanB = c(1, 1, 1), stringsAsFactors = FALSE)
  malicious <- list(cmd = "rm -rf /")

  save(net, info, malicious, file = tmp)

  expect_error(
    safe_load_rdata(tmp, required = c("net", "info"), allowed = c("net", "info")),
    "unexpected object"
  )
})

test_that("safe_load_rdata allows known bundled helper functions", {
  tmp <- tempfile(fileext = ".RData")
  on.exit(unlink(tmp), add = TRUE)

  net <- make_ring(2, directed = TRUE)
  V(net)$name <- c("A", "B")
  info <- data.frame(species = c("A", "B"), fg = c("A", "B"),
                     meanB = c(1, 1), stringsAsFactors = FALSE)
  fluxind <- function(x) x
  plotfw <- function(x) x
  trophiclevels <- function(x) x

  save(net, info, fluxind, plotfw, trophiclevels, file = tmp)

  env <- safe_load_rdata(
    tmp,
    required = c("net", "info"),
    allowed = c("net", "info", "fluxind", "plotfw", "trophiclevels")
  )

  expect_true(exists("fluxind", envir = env, inherits = FALSE))
  expect_true(exists("plotfw", envir = env, inherits = FALSE))
  expect_true(exists("trophiclevels", envir = env, inherits = FALSE))
})

test_that("safe_read_rds validates metaweb object shape before accepting it", {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)

  metaweb <- list(
    species = data.frame(species_name = c("A", "B"), stringsAsFactors = FALSE),
    interactions = data.frame(predator_id = c(1, 2), prey_id = c(2, 1), stringsAsFactors = FALSE),
    metadata = list(region = "test-region")
  )
  class(metaweb) <- "metaweb"
  saveRDS(metaweb, tmp)

  loaded <- safe_read_rds(tmp, expected_class = "metaweb",
                          expected_names = c("species", "interactions", "metadata"))
  expect_s3_class(loaded, "metaweb")

  bad <- tempfile(fileext = ".rds")
  on.exit(unlink(bad), add = TRUE)
  saveRDS(list(hello = "world"), bad)
  expect_error(
    safe_read_rds(bad, expected_class = "metaweb",
                  expected_names = c("species", "interactions", "metadata")),
    "Refusing RDS upload"
  )
})

test_that(".read_legacy_api_keys reads whitelisted literal keys and never runs code", {
  f <- tempfile(fileext = ".R")
  on.exit(unlink(f), add = TRUE)
  marker <- tempfile()
  writeLines(c(
    "# legacy key file",
    'worms_key <- "abc123"',
    'traitbank_key = "xyz"',
    sprintf('file.create("%s")', gsub("\\\\", "/", marker)),  # must NOT execute
    'evil <- "x"',                                             # not whitelisted
    "algaebase_username <- Sys.getenv('USER')"                 # non-literal
  ), f)
  keys <- suppressWarnings(.read_legacy_api_keys(f))
  expect_equal(keys, list(worms_key = "abc123", traitbank_key = "xyz"))
  expect_false(file.exists(marker))
  expect_warning(.read_legacy_api_keys(f), "unsafe legacy API key assignment 'evil'")
  expect_equal(.read_legacy_api_keys(tempfile()), list())
})

test_that(".read_legacy_api_keys accepts the template's API_KEYS$name <- form", {
  # config/api_keys.R.template (and therefore every real legacy file) writes
  # API_KEYS$key <- "value"; a bare-name-only parser silently drops them all.
  f <- tempfile(fileext = ".R")
  on.exit(unlink(f), add = TRUE)
  writeLines(c(
    'API_KEYS$freshwaterecology_key <- "fw-123"  # trailing comment',
    'API_KEYS$algaebase_username <- "me"',
    'API_KEYS$evil <- "x"',                 # not whitelisted
    'OTHER$worms_key <- "nope"'             # wrong container
  ), f)
  keys <- suppressWarnings(.read_legacy_api_keys(f))
  expect_equal(keys, list(freshwaterecology_key = "fw-123", algaebase_username = "me"))
})

test_that("every .Rdata loader shares one allowlist that accepts the bundled examples", {
  expect_setequal(RDATA_ALLOWED_OBJECTS, c("net", "info", "fluxind", "plotfw", "trophiclevels"))
  for (f in list.files(file.path(app_root, "examples"), pattern = "[.]Rdata$", full.names = TRUE)) {
    env <- safe_load_rdata(f, allowed = RDATA_ALLOWED_OBJECTS)
    expect_true(exists("net", envir = env, inherits = FALSE), info = basename(f))
  }
  # No caller may hand-write its own list again (Data Import used to allow only net/info,
  # which refused examples/BalticFW.Rdata).
  for (rel in c("R/data_loading.R", "R/functions/network_comparison.R",
                "R/modules/data_import_server.R")) {
    src <- paste(readLines(file.path(app_root, rel), warn = FALSE), collapse = "\n")
    expect_false(grepl("allowed = c(", src, fixed = TRUE), info = rel)
    expect_true(grepl("RDATA_ALLOWED_OBJECTS", src, fixed = TRUE), info = rel)
  }
})
