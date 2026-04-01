#!/usr/bin/env Rscript
# =============================================================================
# Database Query Test Runner
# =============================================================================
# Runs testthat-based tests for EcoBase and trait lookup database queries.
#
# Usage:
#   Rscript tests/run_database_tests.R              # Unit tests only (fast, offline)
#   Rscript tests/run_database_tests.R live          # Include live API tests
#   Rscript tests/run_database_tests.R capture       # Capture fixtures from real APIs
#   Rscript tests/run_database_tests.R ecobase       # Only EcoBase tests
#   Rscript tests/run_database_tests.R traits        # Only trait lookup tests
#
# Run from the EcoNeTool root directory.
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) > 0) args[1] else "unit"

cat("\n")
cat("========================================\n")
cat("  EcoNeTool Database Test Suite\n")
cat("========================================\n\n")

# Verify we're in the right directory
if (!file.exists("R/functions/ecobase_connection.R")) {
  cat("ERROR: Please run from the EcoNeTool root directory.\n")
  cat("  Expected: Rscript tests/run_database_tests.R\n")
  quit(status = 1)
}

# Check testthat is installed
if (!requireNamespace("testthat", quietly = TRUE)) {
  cat("ERROR: testthat package not installed.\n")
  cat("  Install with: install.packages('testthat')\n")
  quit(status = 1)
}

library(testthat)

# ---------------------------------------------------------------------------
# Mode: capture fixtures
# ---------------------------------------------------------------------------
if (mode == "capture") {
  cat("Mode: CAPTURE FIXTURES (requires internet)\n\n")
  source("tests/testthat/capture_fixtures.R")
  quit(status = 0)
}

# ---------------------------------------------------------------------------
# Mode: live (enable real API calls)
# ---------------------------------------------------------------------------
if (mode == "live") {
  cat("Mode: LIVE TESTS (unit + live API calls)\n")
  cat("  This will hit real APIs and may take several minutes.\n\n")
  Sys.setenv(RUN_LIVE_TESTS = "true")
} else {
  cat("Mode: UNIT TESTS ONLY (offline, using fixtures)\n")
  cat("  To run live tests: Rscript tests/run_database_tests.R live\n\n")
}

# ---------------------------------------------------------------------------
# Check fixtures exist
# ---------------------------------------------------------------------------
fixture_dir <- "tests/testthat/fixtures"
fixture_count <- length(list.files(fixture_dir, pattern = "\\.rds$"))

if (fixture_count == 0 && mode != "live") {
  cat("WARNING: No fixtures found in ", fixture_dir, "\n")
  cat("  Unit tests will be skipped. Run capture first:\n")
  cat("  Rscript tests/run_database_tests.R capture\n\n")
}

cat("Fixtures available:", fixture_count, "\n\n")

# ---------------------------------------------------------------------------
# Select test files
# ---------------------------------------------------------------------------
if (mode == "ecobase") {
  test_filter <- "ecobase"
  cat("Filter: EcoBase tests only\n\n")
} else if (mode == "traits") {
  test_filter <- "trait-lookup"
  cat("Filter: Trait lookup tests only\n\n")
} else {
  test_filter <- NULL
}

# ---------------------------------------------------------------------------
# Run tests
# ---------------------------------------------------------------------------
cat("Running tests...\n")
cat("========================================\n\n")

results <- tryCatch(
  test_dir(
    "tests/testthat",
    filter = test_filter,
    reporter = "summary",
    stop_on_failure = FALSE
  ),
  error = function(e) {
    cat("\nTest runner error:", e$message, "\n")
    NULL
  }
)

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
cat("\n========================================\n")
cat("  Test Run Complete\n")
cat("========================================\n\n")

if (!is.null(results)) {
  # testthat 3.x returns results differently
  if (is.data.frame(results)) {
    n_pass <- sum(results$passed)
    n_fail <- sum(results$failed)
    n_skip <- sum(results$skipped)
    cat(sprintf("Passed:  %d\n", n_pass))
    cat(sprintf("Failed:  %d\n", n_fail))
    cat(sprintf("Skipped: %d\n", n_skip))
  }
}

# Reset env var
Sys.unsetenv("RUN_LIVE_TESTS")

cat("\nDone.\n")
