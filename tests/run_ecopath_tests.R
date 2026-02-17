# Master Test Runner for ECOPATH/ECOSIM Functionality
# Coordinates all Rpath-related tests using the Rpath testing infrastructure
#
# Author: EcoNeTool Testing Suite
# Date: 2025-12-08
#
# Usage:
#   source("tests/run_ecopath_tests.R")
#
# This will run all ECOPATH/ECOSIM tests in sequence and generate a summary report

cat("\n")
cat(strrep("=", 80), "\n")
cat("ECOPATH/ECOSIM TESTING SUITE\n")
cat("EcoNeTool - Marine Food Web Network Analysis\n")
cat(strrep("=", 80), "\n\n")

# Configuration
TESTS_TO_RUN <- c(
  "test_ecopath_import.R",           # Basic import functionality
  "test_ecosim_import.R",            # ECOSIM scenario import
  "test_ecopath_balancing.R",        # Balancing workflow with parameter fixes
  "test_parameter_editors.R",        # Parameter editor validation
  "test_ecopath_ecosim_workflow.R"   # End-to-end workflow
)

RESULTS_DIR <- "tests/test_results"
if (!dir.exists(RESULTS_DIR)) dir.create(RESULTS_DIR, recursive = TRUE)

TIMESTAMP <- format(Sys.time(), "%Y%m%d_%H%M%S")
LOG_FILE <- file.path(RESULTS_DIR, paste0("test_suite_log_", TIMESTAMP, ".txt"))

# Start logging
sink(LOG_FILE, split = TRUE)

cat("Test Suite Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("R Version:", R.version.string, "\n")
cat("Platform:", R.version$platform, "\n\n")

# Package checks
cat("Checking required packages...\n")
required_packages <- c("Rpath", "data.table", "RODBC")
for (pkg in required_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    pkg_version <- tryCatch(
      as.character(packageVersion(pkg)),
      error = function(e) "unknown"
    )
    cat("  ✓", pkg, "(version", pkg_version, ")\n")
  } else {
    cat("  ✗", pkg, "NOT INSTALLED\n")
  }
}
cat("\n")

# Test execution
test_suite_results <- list()

for (i in seq_along(TESTS_TO_RUN)) {
  test_file <- TESTS_TO_RUN[i]
  test_path <- file.path("tests", test_file)

  cat(strrep("=", 80), "\n")
  cat("Running Test", i, "of", length(TESTS_TO_RUN), ":", test_file, "\n")
  cat(strrep("=", 80), "\n\n")

  if (!file.exists(test_path)) {
    cat("✗ TEST FILE NOT FOUND:", test_path, "\n\n")
    test_suite_results[[i]] <- list(
      test_file = test_file,
      status = "SKIP",
      error = "File not found",
      duration = 0
    )
    next
  }

  start_time <- Sys.time()

  result <- tryCatch({
    source(test_path, echo = FALSE, print.eval = FALSE)
    list(status = "PASS", error = NULL)
  }, error = function(e) {
    cat("\n✗ TEST FAILED WITH ERROR:\n")
    cat(e$message, "\n\n")
    list(status = "FAIL", error = e$message)
  })

  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

  test_suite_results[[i]] <- list(
    test_file = test_file,
    status = result$status,
    error = result$error,
    duration = duration,
    timestamp = start_time
  )

  cat("\n")
  if (result$status == "PASS") {
    cat("✓ TEST COMPLETED (", round(duration, 2), " seconds)\n")
  } else {
    cat("✗ TEST FAILED (", round(duration, 2), " seconds)\n")
  }
  cat("\n")
}

# Summary
cat("\n")
cat(strrep("=", 80), "\n")
cat("TEST SUITE SUMMARY\n")
cat(strrep("=", 80), "\n\n")

total_tests <- length(test_suite_results)

# Count results safely
passed <- 0
failed <- 0
skipped <- 0
for (r in test_suite_results) {
  if (is.null(r) || is.null(r$status)) next
  if (r$status == "PASS") {
    passed <- passed + 1
  } else if (r$status == "FAIL") {
    failed <- failed + 1
  } else if (r$status == "SKIP") {
    skipped <- skipped + 1
  }
}

cat("Total Tests:", total_tests, "\n")
cat("Passed:     ", passed, sprintf("(%.1f%%)\n", 100 * passed / total_tests))
cat("Failed:     ", failed, sprintf("(%.1f%%)\n", 100 * failed / total_tests))
cat("Skipped:    ", skipped, sprintf("(%.1f%%)\n", 100 * skipped / total_tests))
total_duration <- 0
for (r in test_suite_results) {
  if (is.null(r) || is.null(r$duration)) next
  total_duration <- total_duration + r$duration
}
cat("\nTotal Duration:", round(total_duration, 2), "seconds\n")

cat("\nDetailed Results:\n")
for (i in seq_along(test_suite_results)) {
  r <- test_suite_results[[i]]
  status_icon <- switch(r$status,
    "PASS" = "✓",
    "FAIL" = "✗",
    "SKIP" = "⊘"
  )
  cat(sprintf("  [%d] %s %s (%.2fs)\n", i, status_icon, r$test_file, r$duration))
  if (!is.null(r$error)) {
    cat(sprintf("      Error: %s\n", r$error))
  }
}

if (failed > 0) {
  cat("\n❌ SOME TESTS FAILED\n")
  cat("\nFailed Tests:\n")
  for (r in test_suite_results) {
    if (r$status == "FAIL") {
      cat("  •", r$test_file, "\n")
      if (!is.null(r$error)) {
        cat("    Error:", r$error, "\n")
      }
    }
  }
} else if (passed == total_tests) {
  cat("\n✅ ALL TESTS PASSED!\n")
}

cat("\n📄 Full log saved to:", LOG_FILE, "\n")
cat("\n")
cat(strrep("=", 80), "\n")
cat("TEST SUITE COMPLETE\n")
cat(strrep("=", 80), "\n\n")

# Stop logging
sink()

# Save results object
results_file <- file.path(RESULTS_DIR, paste0("test_results_", TIMESTAMP, ".rds"))
saveRDS(test_suite_results, results_file)
cat("📊 Test results object saved to:", results_file, "\n\n")

# Return results invisibly
invisible(test_suite_results)
