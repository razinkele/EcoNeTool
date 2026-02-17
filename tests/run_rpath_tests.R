#!/usr/bin/env Rscript
# Automated Rpath Module Test Runner
# Usage: Rscript run_rpath_tests.R

cat("
================================================================================
RPATH MODULE AUTOMATED TEST RUNNER
================================================================================
")

# Check if test file exists
if (!file.exists("test_rpath_module_ltgoby.R")) {
  stop("Test script not found: test_rpath_module_ltgoby.R")
}

# Check if LTgoby model exists
if (!file.exists("examples/LTgoby.eweaccdb")) {
  stop("Test data not found: examples/LTgoby.eweaccdb")
}

cat("\nRunning tests...\n")
cat("================================================================================\n")

# Run the test script
source("test_rpath_module_ltgoby.R")

cat("\n================================================================================\n")
cat("TEST RUN COMPLETE\n")
cat("================================================================================\n")
