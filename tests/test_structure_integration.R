# Comprehensive Structure Integration Test
# Tests the reorganized codebase to ensure everything works correctly

cat("=== TESTING REORGANIZED STRUCTURE ===\n\n")
cat("This test verifies that Phase 1 & 2 changes work correctly:\n")
cat("  - Phase 1: Performance optimizations\n")
cat("  - Phase 2: File organization\n\n")

# Initialize test counters
tests_passed <- 0
tests_failed <- 0
warnings_count <- 0

test_result <- function(name, passed, message = "") {
  if (passed) {
    cat("  ✓", name, "\n")
    if (message != "") cat("    ", message, "\n")
    tests_passed <<- tests_passed + 1
  } else {
    cat("  ✗", name, "\n")
    if (message != "") cat("    ERROR:", message, "\n")
    tests_failed <<- tests_failed + 1
  }
}

test_warning <- function(name, message) {
  cat("  ⚠", name, "\n")
  cat("    WARNING:", message, "\n")
  warnings_count <<- warnings_count + 1
}

# =============================================================================
# TEST SUITE 1: FILE STRUCTURE
# =============================================================================

cat("\n[SUITE 1] File Structure Validation\n")
cat(strrep("-", 70), "\n")

# Test 1.1: Core files exist
test_result("Core application files exist",
  file.exists("app.R") && file.exists("run_app.R"))

# Test 1.2: Directory structure
expected_dirs <- c(
  "R", "R/config", "R/functions", "R/modules", "R/ui",
  "tests", "scripts", "scripts/utils", "scripts/data_conversion",
  "scripts/setup", "archive", "archive/phase1", "archive/backups"
)

dirs_exist <- sapply(expected_dirs, dir.exists)
test_result("All expected directories exist",
  all(dirs_exist),
  paste(sum(dirs_exist), "/", length(dirs_exist), "directories found"))

# Test 1.3: Integration files in new location
integration_files <- c(
  "R/functions/ecopath_import.R",
  "R/functions/rpath_integration.R"
)
integration_exist <- sapply(integration_files, file.exists)
test_result("Integration files moved correctly",
  all(integration_exist),
  paste(sum(integration_exist), "/", length(integration_files), "files found"))

# Test 1.4: Old files removed from root
old_files <- c("functions.R", "rpath_integration.R", "ecopath_windows_import.R")
old_removed <- !any(sapply(old_files, file.exists))
test_result("Old duplicate files removed",
  old_removed,
  if (old_removed) "Root directory clean" else "Old files still present")

# Test 1.5: Test files organized
test_files_count <- length(list.files("tests", pattern = "\\.R$"))
test_result("Test files organized in tests/",
  test_files_count >= 30,
  paste(test_files_count, "test files found"))

# =============================================================================
# TEST SUITE 2: SOURCE PATH VALIDATION
# =============================================================================

cat("\n[SUITE 2] Source Path Validation\n")
cat(strrep("-", 70), "\n")

# Test 2.1: Read app.R and check source calls
app_content <- readLines("app.R")

# Extract source() calls
source_lines <- grep('source\\("', app_content, value = TRUE)
source_lines <- gsub("^\\s+", "", source_lines)  # Remove leading whitespace

cat("Found", length(source_lines), "source() calls in app.R\n")

# Test 2.2: Verify all sourced files exist
source_files <- gsub('.*source\\("([^"]+)".*', '\\1', source_lines)
source_files <- source_files[source_files != source_lines]  # Filter valid extractions

files_exist <- sapply(source_files, function(f) {
  exists <- file.exists(f)
  if (!exists) {
    test_warning(paste("Missing source file:", f), "File not found")
  }
  exists
})

test_result("All sourced files exist",
  all(files_exist),
  paste(sum(files_exist), "/", length(files_exist), "source files found"))

# Test 2.3: Check for old paths in app.R
old_paths <- c(
  'source\\("ecopath_windows_import.R"\\)',
  'source\\("rpath_integration.R"\\)(?!.*/)'  # Not in subdirectory
)

old_paths_found <- sapply(old_paths, function(p) {
  any(grepl(p, app_content, perl = TRUE))
})

test_result("No old paths in app.R",
  !any(old_paths_found),
  if (any(old_paths_found)) "Old paths still present" else "All paths updated")

# Test 2.4: Check rpath_module.R paths
if (file.exists("R/modules/rpath_module.R")) {
  module_content <- readLines("R/modules/rpath_module.R")

  # Count source calls to rpath_integration
  source_count <- sum(grepl('source.*rpath_integration', module_content))

  test_result("Rpath module sources file once",
    source_count == 1,
    paste("Found", source_count, "source() call(s)"))

  # Check for correct path
  has_correct_path <- any(grepl('source\\("R/functions/rpath_integration.R"', module_content))
  test_result("Rpath module uses correct path",
    has_correct_path)
}

# =============================================================================
# TEST SUITE 3: SYNTAX VALIDATION
# =============================================================================

cat("\n[SUITE 3] Syntax Validation\n")
cat(strrep("-", 70), "\n")

# Test 3.1: Parse app.R
app_parsed <- tryCatch({
  parse("app.R")
  TRUE
}, error = function(e) {
  test_warning("app.R syntax error", e$message)
  FALSE
})
test_result("app.R syntax valid", app_parsed)

# Test 3.2: Parse rpath_module.R
module_parsed <- tryCatch({
  parse("R/modules/rpath_module.R")
  TRUE
}, error = function(e) {
  test_warning("rpath_module.R syntax error", e$message)
  FALSE
})
test_result("rpath_module.R syntax valid", module_parsed)

# Test 3.3: Parse all R/functions files
functions_dir <- "R/functions"
function_files <- list.files(functions_dir, pattern = "\\.R$", full.names = TRUE)

function_errors <- c()
for (f in function_files) {
  parsed <- tryCatch({
    parse(f)
    TRUE
  }, error = function(e) {
    function_errors <<- c(function_errors, basename(f))
    FALSE
  })
}

test_result("All R/functions files parse correctly",
  length(function_errors) == 0,
  if (length(function_errors) > 0)
    paste("Errors in:", paste(function_errors, collapse = ", "))
  else
    paste("All", length(function_files), "files valid"))

# =============================================================================
# TEST SUITE 4: MODULE LOADING
# =============================================================================

cat("\n[SUITE 4] Module Loading Test\n")
cat(strrep("-", 70), "\n")

# Test 4.1: Load configuration
config_loaded <- tryCatch({
  source("R/config.R", local = TRUE)
  TRUE
}, error = function(e) {
  test_warning("Config loading error", e$message)
  FALSE
})
test_result("R/config.R loads successfully", config_loaded)

# Test 4.2: Load key function files
key_functions <- c(
  "R/functions/trophic_levels.R",
  "R/functions/network_visualization.R",
  "R/functions/topological_metrics.R"
)

functions_loaded <- 0
for (func_file in key_functions) {
  loaded <- tryCatch({
    test_env <- new.env()
    source(func_file, local = test_env)
    functions_loaded <- functions_loaded + 1
    TRUE
  }, error = function(e) {
    test_warning(paste("Error loading", basename(func_file)), e$message)
    FALSE
  })
}

test_result("Key function files load successfully",
  functions_loaded == length(key_functions),
  paste(functions_loaded, "/", length(key_functions), "loaded"))

# Test 4.3: Load integration files (without dependencies)
integration_loadable <- tryCatch({
  # Check if file is syntactically correct and has expected structure
  ecopath_content <- readLines("R/functions/ecopath_import.R")
  rpath_content <- readLines("R/functions/rpath_integration.R")

  # Look for key function definitions
  has_ecopath_func <- any(grepl("parse_ecopath_native_cross_platform", ecopath_content))
  has_rpath_func <- any(grepl("convert_ecopath_to_rpath", rpath_content))

  has_ecopath_func && has_rpath_func
}, error = function(e) {
  FALSE
})

test_result("Integration files contain expected functions",
  integration_loadable,
  if (integration_loadable) "Key functions found" else "Functions missing")

# =============================================================================
# TEST SUITE 5: DEPENDENCIES CHECK
# =============================================================================

cat("\n[SUITE 5] Package Dependencies\n")
cat(strrep("-", 70), "\n")

# Test 5.1: Core packages
core_packages <- c("shiny", "bs4Dash", "igraph", "fluxweb", "visNetwork", "DT")
core_available <- sapply(core_packages, function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
})

test_result("Core packages available",
  all(core_available),
  paste(sum(core_available), "/", length(core_available), "packages found"))

if (!all(core_available)) {
  missing <- core_packages[!core_available]
  test_warning("Missing core packages", paste(missing, collapse = ", "))
}

# Test 5.2: Optional packages
optional_packages <- c("Rpath", "data.table", "readxl", "sf")
optional_available <- sapply(optional_packages, function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
})

test_result("Optional packages checked",
  TRUE,  # Not a failure if missing
  paste(sum(optional_available), "/", length(optional_available),
        "optional packages available"))

if (!all(optional_available)) {
  missing <- optional_packages[!optional_available]
  cat("    Note:", paste(missing, collapse = ", "), "not installed (optional)\n")
}

# =============================================================================
# TEST SUITE 6: REACTIVE CACHING (Phase 1 verification)
# =============================================================================

cat("\n[SUITE 6] Phase 1 Optimizations Verification\n")
cat(strrep("-", 70), "\n")

# Test 6.1: Check for flux_results cached reactive
has_flux_cache <- any(grepl("flux_results.*<-.*reactive\\(", app_content))
test_result("Flux results caching implemented", has_flux_cache)

# Test 6.2: Check for keystoneness_results cached reactive
has_keystoneness_cache <- any(grepl("keystoneness_results.*<-.*reactive\\(", app_content))
test_result("Keystoneness caching implemented", has_keystoneness_cache)

# Test 6.3: Check for bindCache usage
has_bind_cache <- any(grepl("bindCache", app_content))
test_result("bindCache() used for reactive caching", has_bind_cache)

# Test 6.4: Check for vectorized diet matrix
has_vectorized <- any(grepl("cbind.*prey.*pred", app_content))
test_result("Vectorized diet matrix construction", has_vectorized)

# Test 6.5: Check memory cleanup in rpath_module
if (file.exists("R/modules/rpath_module.R")) {
  has_gc <- any(grepl("gc\\(\\)", module_content))
  has_cleanup <- any(grepl("rpath_values\\$.*<-.*NULL", module_content))
  test_result("Memory cleanup implemented in Rpath module",
    has_gc && has_cleanup,
    paste("gc():", has_gc, "| cleanup:", has_cleanup))
}

# =============================================================================
# TEST SUITE 7: FULL APP INITIALIZATION
# =============================================================================

cat("\n[SUITE 7] Full App Initialization Test\n")
cat(strrep("-", 70), "\n")

cat("Attempting to source app.R and check for initialization errors...\n")

app_init_success <- tryCatch({
  # Create isolated environment
  test_env <- new.env()

  # Set working directory context
  test_env$.test_mode <- TRUE

  # Try to source app.R in test environment
  # This will load all modules and check for errors
  # but won't actually start the Shiny server
  suppressMessages({
    source("app.R", local = test_env)
  })

  TRUE
}, error = function(e) {
  cat("  Initialization error:", e$message, "\n")
  if (grepl("could not find function", e$message)) {
    cat("  This might be a package dependency issue\n")
  }
  FALSE
}, warning = function(w) {
  cat("  Warning during initialization:", w$message, "\n")
  TRUE  # Warnings are OK
})

test_result("App initializes without critical errors",
  app_init_success,
  if (app_init_success) "All modules loaded" else "See error above")

# =============================================================================
# TEST SUITE 8: PERFORMANCE OPTIMIZATIONS
# =============================================================================

cat("\n[SUITE 8] Performance Features Check\n")
cat(strrep("-", 70), "\n")

# Test 8.1: Progress indicators
has_progress <- any(grepl("withProgress", app_content))
test_result("Progress indicators present",
  has_progress,
  if (has_progress) "Found in app.R" else "Not yet implemented")

# Test 8.2: Check for duplicate functions.R removal
no_functions_r <- !file.exists("functions.R")
test_result("Duplicate functions.R removed", no_functions_r)

# Test 8.3: Root directory cleanliness
root_r_files <- list.files(pattern = "^[^.]+\\.R$")  # R files not starting with .
root_clean <- length(root_r_files) <= 3  # app.R, run_app.R, maybe one test script
test_result("Root directory clean",
  root_clean,
  paste(length(root_r_files), "R files in root"))

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n", strrep("=", 70), "\n")
cat("TEST SUMMARY\n")
cat(strrep("=", 70), "\n\n")

total_tests <- tests_passed + tests_failed
pass_rate <- if (total_tests > 0) round(100 * tests_passed / total_tests, 1) else 0

cat("Results:\n")
cat("  ✓ Passed:  ", tests_passed, "\n")
cat("  ✗ Failed:  ", tests_failed, "\n")
cat("  ⚠ Warnings:", warnings_count, "\n")
cat("  ━━━━━━━━━━━━━━━━━━\n")
cat("  Total:     ", total_tests, "\n")
cat("  Pass Rate: ", pass_rate, "%\n\n")

# Detailed status
if (tests_failed == 0 && warnings_count == 0) {
  cat("✓ EXCELLENT: All tests passed with no warnings!\n")
  cat("  The reorganized structure is working perfectly.\n")
  cat("  Ready to proceed with Phase 3.\n")
} else if (tests_failed == 0 && warnings_count > 0) {
  cat("✓ GOOD: All tests passed with minor warnings.\n")
  cat("  The structure is functional. Review warnings above.\n")
  cat("  Safe to proceed with Phase 3.\n")
} else if (tests_failed <= 2) {
  cat("⚠ ACCEPTABLE: Most tests passed with a few failures.\n")
  cat("  Review failed tests above before proceeding.\n")
  cat("  May need minor fixes before Phase 3.\n")
} else {
  cat("✗ ISSUES DETECTED: Multiple test failures.\n")
  cat("  Review and fix failures before proceeding.\n")
  cat("  Do not proceed to Phase 3 yet.\n")
}

cat("\n", strrep("=", 70), "\n")

# Return status code
if (tests_failed > 0) {
  cat("\nReturning exit code 1 (failures detected)\n")
  quit(status = 1, save = "no")
} else {
  cat("\nReturning exit code 0 (all tests passed)\n")
}
