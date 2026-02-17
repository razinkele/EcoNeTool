#!/usr/bin/env Rscript
#' Test Runner for EcoNeTool Habitat Loading
#'
#' Runs all test suites and reports results
#'
#' Usage:
#'   Rscript tests/run_tests.R [test_name]
#'
#' Arguments:
#'   test_name - Optional. Run specific test: 'functions', 'loading', or 'all' (default)
#'
#' @author Claude (Anthropic)
#' @date 2025-12-19

# Parse arguments
args <- commandArgs(trailingOnly = TRUE)
test_type <- if (length(args) > 0) args[1] else "all"

cat("\n")
cat("========================================\n")
cat("  EcoNeTool Test Runner\n")
cat("========================================\n\n")

# Check if we're in correct directory
if (!file.exists("tests/test_habitat_functions.R") ||
    !file.exists("tests/test_habitat_loading.R")) {
  cat("✗ Error: Test files not found\n")
  cat("  Please run this script from the EcoNeTool root directory:\n")
  cat("    Rscript tests/run_tests.R\n\n")
  quit(status = 1)
}

run_test_suite <- function(test_file, suite_name) {
  cat(sprintf("\n📋 Running %s...\n", suite_name))
  cat(strrep("=", 50), "\n")

  result <- system2("Rscript", args = test_file, stdout = TRUE, stderr = TRUE)
  status <- attr(result, "status")

  # Print output
  cat(paste(result, collapse = "\n"), "\n")

  # Return status (NULL means success, 0 means success, anything else is failure)
  return(is.null(status) || status == 0)
}

# Track overall results
all_passed <- TRUE

# Run unit tests
if (test_type %in% c("all", "functions")) {
  functions_passed <- run_test_suite(
    "tests/test_habitat_functions.R",
    "Unit Tests (Functions)"
  )
  all_passed <- all_passed && functions_passed
}

# Run integration tests
if (test_type %in% c("all", "loading")) {
  loading_passed <- run_test_suite(
    "tests/test_habitat_loading.R",
    "Integration Tests (Loading)"
  )
  all_passed <- all_passed && loading_passed
}

# Final summary
cat("\n")
cat("========================================\n")
cat("  Final Test Summary\n")
cat("========================================\n\n")

if (all_passed) {
  cat("✅ ALL TEST SUITES PASSED!\n\n")
  cat("Habitat loading functionality is working correctly.\n")
  cat("Status: PRODUCTION READY ✅\n\n")
  quit(status = 0)
} else {
  cat("⚠️  SOME TESTS FAILED\n\n")
  cat("Please review the test output above.\n")
  cat("Status: NEEDS ATTENTION ⚠️\n\n")
  quit(status = 1)
}
