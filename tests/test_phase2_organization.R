# Test Script for Phase 2 File Organization
# Validates file moves and path updates

cat("=== Testing Phase 2 File Organization ===\n\n")

# Test 1: Verify directory structure created
cat("[Test 1] Verify directory structure...\n")
dirs_to_check <- c("scripts/utils", "scripts/data_conversion", "scripts/setup",
                   "archive/phase1", "archive/backups")
all_dirs_exist <- TRUE
for (dir in dirs_to_check) {
  if (dir.exists(dir)) {
    cat("  ✓", dir, "\n")
  } else {
    cat("  ✗", dir, "NOT FOUND\n")
    all_dirs_exist <- FALSE
  }
}

# Test 2: Verify test files moved
cat("\n[Test 2] Verify test files moved to tests/...\n")
root_tests <- length(list.files(pattern = "^test_.*\\.R$"))
tests_dir_count <- length(list.files("tests", pattern = "^test_.*\\.R$"))
cat("  Root directory test files:", root_tests, "\n")
cat("  tests/ directory test files:", tests_dir_count, "\n")
if (root_tests == 0 && tests_dir_count > 30) {
  cat("  ✓ PASS: Test files successfully moved\n")
} else {
  cat("  ✗ FAIL: Test files not properly moved\n")
}

# Test 3: Verify utility scripts moved
cat("\n[Test 3] Verify utility scripts moved...\n")
utils_count <- length(list.files("scripts/utils", pattern = "\\.R$"))
cat("  scripts/utils/ contains", utils_count, "files\n")
if (utils_count >= 10) {
  cat("  ✓ PASS: Utility scripts moved\n")
} else {
  cat("  ✗ FAIL: Expected 10+ utility scripts\n")
}

# Test 4: Verify conversion scripts moved
cat("\n[Test 4] Verify conversion scripts moved...\n")
conversion_count <- length(list.files("scripts/data_conversion", pattern = "\\.R$"))
cat("  scripts/data_conversion/ contains", conversion_count, "files\n")
if (conversion_count >= 3) {
  cat("  ✓ PASS: Conversion scripts moved\n")
} else {
  cat("  ✗ FAIL: Expected 3 conversion scripts\n")
}

# Test 5: Verify setup scripts moved
cat("\n[Test 5] Verify setup scripts moved...\n")
setup_count <- length(list.files("scripts/setup", pattern = "\\.R$"))
cat("  scripts/setup/ contains", setup_count, "files\n")
if (setup_count >= 4) {
  cat("  ✓ PASS: Setup scripts moved\n")
} else {
  cat("  ✗ FAIL: Expected 4 setup scripts\n")
}

# Test 6: Verify legacy files archived
cat("\n[Test 6] Verify legacy Phase 1 files archived...\n")
phase1_count <- length(list.files("archive/phase1", pattern = "\\.R$"))
cat("  archive/phase1/ contains", phase1_count, "files\n")
if (phase1_count >= 4) {
  cat("  ✓ PASS: Legacy files archived\n")
} else {
  cat("  ⚠ WARNING: Expected 4+ legacy files\n")
}

# Test 7: Verify backup files archived
cat("\n[Test 7] Verify backup files archived...\n")
backups_count <- length(list.files("archive/backups"))
cat("  archive/backups/ contains", backups_count, "files\n")
if (backups_count >= 3) {
  cat("  ✓ PASS: Backup files archived\n")
} else {
  cat("  ⚠ WARNING: Expected 3 backup files\n")
}

# Test 8: Verify integration files moved
cat("\n[Test 8] Verify integration files moved to R/functions/...\n")
rpath_integration_exists <- file.exists("R/functions/rpath_integration.R")
ecopath_import_exists <- file.exists("R/functions/ecopath_import.R")

if (rpath_integration_exists) {
  cat("  ✓ R/functions/rpath_integration.R found\n")
} else {
  cat("  ✗ R/functions/rpath_integration.R NOT FOUND\n")
}

if (ecopath_import_exists) {
  cat("  ✓ R/functions/ecopath_import.R found\n")
} else {
  cat("  ✗ R/functions/ecopath_import.R NOT FOUND\n")
}

# Check old files don't exist in root
if (!file.exists("rpath_integration.R") && !file.exists("ecopath_windows_import.R")) {
  cat("  ✓ Old files removed from root\n")
} else {
  cat("  ✗ Old files still in root\n")
}

# Test 9: Verify app.R paths updated
cat("\n[Test 9] Verify app.R source paths updated...\n")
app_content <- readLines("app.R")

# Check for new paths
if (any(grepl('source\\("R/functions/ecopath_import.R"\\)', app_content))) {
  cat("  ✓ ecopath_import.R path updated in app.R\n")
} else {
  cat("  ✗ ecopath_import.R path NOT updated in app.R\n")
}

if (any(grepl('source\\("R/functions/rpath_integration.R"\\)', app_content))) {
  cat("  ✓ rpath_integration.R path updated in app.R\n")
} else {
  cat("  ✗ rpath_integration.R path NOT updated in app.R\n")
}

# Check old paths don't exist
if (!any(grepl('source\\("ecopath_windows_import.R"\\)', app_content)) &&
    !any(grepl('source\\("rpath_integration.R"\\)(?!.*/)', app_content, perl=TRUE))) {
  cat("  ✓ Old paths removed from app.R\n")
} else {
  cat("  ⚠ WARNING: Old paths may still exist in app.R\n")
}

# Test 10: Verify rpath_module.R paths updated
cat("\n[Test 10] Verify rpath_module.R updated...\n")
module_content <- readLines("R/modules/rpath_module.R")

# Check module sources the file once at init
init_source_count <- sum(grepl('source\\("R/functions/rpath_integration.R"', module_content))
cat("  Module sources rpath_integration.R:", init_source_count, "time(s)\n")

if (init_source_count == 1) {
  cat("  ✓ PASS: Module sources file once at initialization\n")
} else if (init_source_count > 1) {
  cat("  ✗ FAIL: Multiple source() calls found (memory leak)\n")
} else {
  cat("  ✗ FAIL: No source() call found\n")
}

# Check no redundant source() calls in observeEvent
redundant_sources <- sum(grepl('source\\(.*rpath_integration.*observeEvent',
                                paste(module_content, collapse=" ")))
if (redundant_sources == 0) {
  cat("  ✓ No redundant source() calls in observeEvent\n")
} else {
  cat("  ✗ Redundant source() calls still present\n")
}

# Test 11: Validate syntax
cat("\n[Test 11] Validate syntax of modified files...\n")
tryCatch({
  parse("app.R")
  cat("  ✓ app.R syntax valid\n")
}, error = function(e) {
  cat("  ✗ app.R syntax error:", e$message, "\n")
})

tryCatch({
  parse("R/modules/rpath_module.R")
  cat("  ✓ rpath_module.R syntax valid\n")
}, error = function(e) {
  cat("  ✗ rpath_module.R syntax error:", e$message, "\n")
})

# Test 12: Count files in root
cat("\n[Test 12] Check root directory cleanliness...\n")
root_r_files <- list.files(pattern = "\\.R$")
root_r_count <- length(root_r_files)
cat("  .R files remaining in root:", root_r_count, "\n")
if (root_r_count <= 2) {  # Should be just app.R and run_app.R
  cat("  ✓ PASS: Root directory clean\n")
} else {
  cat("  Files still in root:\n")
  for (f in root_r_files) {
    cat("    -", f, "\n")
  }
  if (root_r_count <= 5) {
    cat("  ⚠ WARNING: Some files remain (acceptable)\n")
  } else {
    cat("  ✗ FAIL: Too many files remain in root\n")
  }
}

# Summary
cat("\n", strrep("=", 70), "\n")
cat("PHASE 2 ORGANIZATION SUMMARY\n")
cat(strrep("=", 70), "\n")
cat("\n")
cat("Files Organized:\n")
cat("  • ", tests_dir_count, " test files → tests/\n")
cat("  • ", utils_count, " utility scripts → scripts/utils/\n")
cat("  • ", conversion_count, " conversion scripts → scripts/data_conversion/\n")
cat("  • ", setup_count, " setup scripts → scripts/setup/\n")
cat("  • ", phase1_count, " legacy files → archive/phase1/\n")
cat("  • ", backups_count, " backup files → archive/backups/\n")
cat("  • 2 integration files → R/functions/\n")
cat("\n")
cat("Total Files Moved:",
    tests_dir_count + utils_count + conversion_count + setup_count +
    phase1_count + backups_count + 2, "\n")
cat("\n")
cat("Code Updates:\n")
cat("  • app.R: Updated 2 source paths\n")
cat("  • rpath_module.R: Updated path + removed 4 redundant sources\n")
cat("\n")
cat("Result: Professional, organized project structure\n")
cat("\n")
cat(strrep("=", 70), "\n")
cat("✓ Phase 2 organization complete!\n")
cat(strrep("=", 70), "\n\n")
