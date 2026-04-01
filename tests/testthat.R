library(testthat)

# Helper files in tests/testthat/ (helper-*.R) are auto-loaded by testthat.
# See helper-fixtures.R for source_app_dependencies() and fixture utilities.
#
# This project is not an R package, so use test_dir() via run_database_tests.R
# rather than test_check(). This file exists for compatibility.

test_dir(
  "tests/testthat",
  reporter = "summary",
  stop_on_failure = FALSE
)
