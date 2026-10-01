# =============================================================================
# Test Helpers: Fixture Loading and Mocking
# =============================================================================
# Loaded automatically by testthat before tests run

# ---------------------------------------------------------------------------
# App root detection
# ---------------------------------------------------------------------------
get_app_root <- function() {
  # From tests/testthat/ go up two levels to project root
  normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
}

# ---------------------------------------------------------------------------
# Source app dependencies (call once per test session)
# ---------------------------------------------------------------------------
source_app_dependencies <- function() {
  app_root <- get_app_root()

  # Core utilities first
  source(file.path(app_root, "R/functions/validation_utils.R"), local = FALSE)
  source(file.path(app_root, "R/config/harmonization_config.R"), local = FALSE)

  # Then domain functions
  source(file.path(app_root, "R/functions/ecobase_connection.R"), local = FALSE)
  source(file.path(app_root, "R/functions/taxonomic_api_utils.R"), local = FALSE)
  source(file.path(app_root, "R/functions/trait_lookup/database_lookups.R"), local = FALSE)
  source(file.path(app_root, "R/functions/trait_lookup/harmonization.R"), local = FALSE)
  source(file.path(app_root, "R/functions/trait_lookup/provenance.R"), local = FALSE)
  source(file.path(app_root, "R/functions/uncertainty_quantification.R"), local = FALSE)
  source(file.path(app_root, "R/functions/trait_lookup/orchestrator.R"), local = FALSE)
  source(file.path(app_root, "R/functions/local_trait_databases.R"), local = FALSE)
  source(file.path(app_root, "R/functions/cache_sqlite.R"), local = FALSE)
  source(file.path(app_root, "R/functions/api_rate_limiter.R"), local = FALSE)

  # Functional group utils (needed for assign_functional_groups)
  tryCatch(
    source(file.path(app_root, "R/functions/functional_group_utils.R"), local = FALSE),
    error = function(e) message("Note: functional_group_utils.R not sourced: ", e$message)
  )
}

# ---------------------------------------------------------------------------
# Fixture directory
# ---------------------------------------------------------------------------
fixture_path <- function(...) {
  testthat::test_path("fixtures", ...)
}

# ---------------------------------------------------------------------------
# Load / save RDS fixtures
# ---------------------------------------------------------------------------
load_fixture <- function(name) {
  path <- fixture_path(paste0(name, ".rds"))
  if (!file.exists(path)) {
    skip(paste0("Fixture not found: ", basename(path),
                ". Run tests/testthat/capture_fixtures.R to create fixtures."))
  }
  readRDS(path)
}

save_fixture <- function(object, name) {
  dir.create(fixture_path(), showWarnings = FALSE, recursive = TRUE)
  saveRDS(object, fixture_path(paste0(name, ".rds")))
}

# ---------------------------------------------------------------------------
# Skip helpers
# ---------------------------------------------------------------------------
skip_if_offline <- function(host = "www.marinespecies.org") {
  skip_on_cran()
  tryCatch(
    {
      con <- url(paste0("https://", host), open = "r")
      close(con)
    },
    error = function(e) {
      skip(paste0("Offline or ", host, " unreachable"))
    }
  )
}

skip_if_no_live_tests <- function() {
  if (!identical(Sys.getenv("RUN_LIVE_TESTS"), "true")) {
    skip("Live API tests disabled. Set RUN_LIVE_TESTS=true to enable.")
  }
}

skip_if_no_ecobase <- function() {
  skip_if_no_live_tests()
  tryCatch(
    {
      con <- url("http://sirs.agrocampus-ouest.fr/EcoBase/", open = "r")
      close(con)
    },
    error = function(e) {
      skip("EcoBase server unreachable")
    }
  )
}

skip_if_no_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    skip(paste0("Package '", pkg, "' not installed"))
  }
}

# ---------------------------------------------------------------------------
# Mock helpers: replace a function temporarily
# ---------------------------------------------------------------------------
with_mocked_function <- function(pkg_env, func_name, mock_fn, code) {
  # Save original
  original <- NULL
  if (exists(func_name, envir = pkg_env)) {
    original <- get(func_name, envir = pkg_env)
  }

  # Replace
  assign(func_name, mock_fn, envir = pkg_env)
  on.exit({
    if (!is.null(original)) {
      assign(func_name, original, envir = pkg_env)
    } else {
      rm(list = func_name, envir = pkg_env)
    }
  })

  force(code)
}

# ---------------------------------------------------------------------------
# Assertion helpers
# ---------------------------------------------------------------------------
expect_valid_ecobase_model <- function(model) {
  expect_type(model, "list")
  expect_true("net" %in% names(model))
  expect_true("info" %in% names(model))
  expect_s3_class(model$net, "igraph")
  expect_s3_class(model$info, "data.frame")

  # Info must have required columns
  required_cols <- c("species", "fg", "meanB", "PB", "QB", "EE")
  for (col in required_cols) {
    expect_true(col %in% names(model$info),
                info = paste("Missing column:", col))
  }

  # Network vertex count must match info rows
  expect_equal(igraph::vcount(model$net), nrow(model$info))
}

expect_valid_trait_result <- function(result) {
  expect_s3_class(result, "data.frame")
  expect_true("species" %in% names(result))

  # Trait codes should be present (may be NA)
  trait_cols <- c("MS", "FS", "MB", "EP", "PR")
  for (col in trait_cols) {
    expect_true(col %in% names(result),
                info = paste("Missing trait column:", col))
  }
}

expect_valid_worms_result <- function(result) {
  expect_type(result, "list")
  expect_true("success" %in% names(result))
  if (result$success) {
    expect_true("traits" %in% names(result))
    traits <- result$traits
    expect_true(!is.null(traits$phylum))
    expect_true(!is.null(traits$class))
  }
}

expect_valid_fishbase_result <- function(result) {
  expect_type(result, "list")
  expect_true("species" %in% names(result))
  expect_true("source" %in% names(result))
  expect_true("success" %in% names(result))
  expect_equal(result$source, "FishBase")
}

expect_valid_db_lookup_result <- function(result, expected_source) {
  expect_type(result, "list")
  expect_true("species" %in% names(result))
  expect_true("source" %in% names(result))
  expect_true("success" %in% names(result))
  expect_equal(result$source, expected_source)
}

# ---------------------------------------------------------------------------
# Edge contract (R/functions/network_finalize.R header): A -> B means B eats A
# ---------------------------------------------------------------------------
# Asserts that `predator` eats `prey` in `net` and that the reverse link is
# absent. Vertices are given by name (or index). Every orientation test uses
# this, so a predator->prey regression fails with a readable message.
assert_prey_to_predator <- function(net, prey, predator) {
  testthat::expect_true(
    igraph::are_adjacent(net, prey, predator),
    label = sprintf("edge %s -> %s (prey -> predator) exists", prey, predator)
  )
  testthat::expect_false(
    igraph::are_adjacent(net, predator, prey),
    label = sprintf("reverse edge %s -> %s (predator -> prey) exists", predator, prey)
  )
}

# ---------------------------------------------------------------------------
# Offline trait DB fixture
# ---------------------------------------------------------------------------
# A temp SQLite file with the build script's schema (offline_db_schema_sql()
# in R/functions/offline_db_rebuild.R, the same statements the writer runs)
# plus metadata. `rows` is a data frame of species_traits columns; `species`
# is required. vocab_version = NULL writes no trait_vocab_version row (a
# pre-v2 build). The file is removed when the calling test ends.
make_offline_db_fixture <- function(rows = NULL, vocab_version = 2L, env = parent.frame()) {
  testthat::skip_if_not_installed("RSQLite")
  source(file.path(get_app_root(), "R/functions/offline_db_rebuild.R"), local = TRUE)
  path <- tempfile("offline_traits_", fileext = ".db")
  withr::defer(unlink(path), envir = env)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  for (stmt in offline_db_schema_sql()) DBI::dbExecute(con, stmt)
  DBI::dbExecute(con, "INSERT INTO metadata (key, value) VALUES ('build_timestamp', ?)",
                 params = list(format(Sys.time(), "%Y-%m-%dT%H:%M:%S")))
  if (!is.null(vocab_version)) {
    DBI::dbExecute(con, "INSERT INTO metadata (key, value) VALUES ('trait_vocab_version', ?)",
                   params = list(as.character(vocab_version)))
  }
  if (!is.null(rows)) DBI::dbAppendTable(con, "species_traits", rows)
  path
}
