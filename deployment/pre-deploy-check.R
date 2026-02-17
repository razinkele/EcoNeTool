#!/usr/bin/env Rscript
# ============================================================================
# EcoNeTool - Pre-Deployment Check Script
# ============================================================================
#
# This script validates the application before deployment
# Run this before deploying to catch common issues
#
# Usage:
#   Rscript pre-deploy-check.R
#
# ============================================================================

cat("================================================================================\n")
cat("EcoNeTool - Pre-Deployment Validation\n")
cat("================================================================================\n\n")

setwd("..")
errors <- 0
warnings <- 0
checks_passed <- 0
print_check <- function(name, status, message = "") {
  if (status == "PASS") {
    cat(sprintf("✓ %s\n", name))
    checks_passed <<- checks_passed + 1
  } else if (status == "WARN") {
    cat(sprintf("⚠ %s: %s\n", name, message))
    warnings <<- warnings + 1
  } else {
    cat(sprintf("✗ %s: %s\n", name, message))
    errors <<- errors + 1
  }
}

# Check for required files and directories
cat("[1] Checking Required Files...\n")
if (file.exists("app.R")) {
  print_check("File: app.R", "PASS")
} else {
  print_check("File: app.R", "FAIL", "File not found")
}
if (file.exists("BalticFW.Rdata")) {
  print_check("File: BalticFW.Rdata", "PASS")
} else {
  print_check("File: BalticFW.Rdata", "FAIL", "File not found")
}
if (file.exists("run_app.R")) {
  print_check("File: run_app.R (optional)", "PASS")
}
if (dir.exists("R/functions")) {
  print_check("Directory: R/functions", "PASS")
  # Check for at least one key function file
  key_funcs <- c("ecopath_import.R", "metaweb_core.R")
  for (kf in key_funcs) {
    if (file.exists(file.path("R/functions", kf))) {
      print_check(paste("  Function file:", kf), "PASS")
    } else {
      print_check(paste("  Function file:", kf), "WARN", "Not found")
    }
  }
} else {
  print_check("Directory: R/functions", "FAIL", "Directory not found")
}
# Check 1: Required files exist
# ============================================================================
cat("\n[1] Checking Required Files...\n")


required_files <- c(
  "app.R",
  "BalticFW.Rdata"
)
for (file in required_files) {
  if (file.exists(file)) {
    print_check(paste("File:", file), "PASS")
  } else {
    print_check(paste("File:", file), "ERROR", "File not found")
  }
}

# Check optional files
optional_files <- c("run_app.R")
for (file in optional_files) {
  if (file.exists(file)) {
    print_check(paste("File:", file, "(optional)"), "PASS")
  } else {
    print_check(paste("File:", file, "(optional)"), "WARN", "File not found")
  }
}

# Check metaweb directory (Phase 2)
if (dir.exists("metawebs")) {
  print_check("Directory: metawebs", "PASS")

  # Check for metaweb subdirectories
  metaweb_dirs <- c("baltic", "arctic", "atlantic")
  for (dir in metaweb_dirs) {
    metaweb_path <- file.path("metawebs", dir)
    if (dir.exists(metaweb_path)) {
      print_check(paste("  Metaweb dir:", dir), "PASS")
    } else {
      print_check(paste("  Metaweb dir:", dir), "WARN", "Directory not found")
    }
  }
} else {
  print_check("Directory: metawebs", "WARN", "Metaweb directory not found (Phase 2 disabled)")
}

# ============================================================================
# Check 2: Validate data file
# ============================================================================
cat("\n[2] Validating Data File...\n")

if (file.exists("BalticFW.Rdata")) {
  tryCatch({
    load("BalticFW.Rdata")

    # Check for required objects
    if (exists("net")) {
      print_check("Data: net object", "PASS")
    } else {
      print_check("Data: net object", "ERROR", "net not found in BalticFW.Rdata")
    }

    if (exists("info")) {
      print_check("Data: info object", "PASS")

      # Validate info structure
      required_cols <- c("meanB", "fg", "bodymasses", "met.types", "efficiencies")
      missing_cols <- setdiff(required_cols, colnames(info))

      if (length(missing_cols) == 0) {
        print_check("Data: info columns", "PASS")
      } else {
        print_check("Data: info columns", "ERROR",
                   paste("Missing columns:", paste(missing_cols, collapse = ", ")))
      }
    } else {
      print_check("Data: info object", "ERROR", "info not found in BalticFW.Rdata")
    }

  }, error = function(e) {
    print_check("Data file loading", "ERROR", e$message)
  })
} else {
  print_check("BalticFW.Rdata", "ERROR", "Data file not found")
}

# ============================================================================
# Check 2.5: Validate Metaweb Files (CRITICAL-003)
# ============================================================================
cat("\n[2.5] Validating Metaweb Files (Phase 2)...\n")

# Expected metaweb files based on METAWEB_PATHS in app.R
expected_metawebs <- list(
  "baltic_kortsch2021" = "metawebs/baltic/baltic_kortsch2021.rds",
  "kongsfjorden_farage2021" = "metawebs/arctic/kongsfjorden_farage2021.rds",
  "north_sea_frelat2022" = "metawebs/atlantic/north_sea_frelat2022.rds",
  "barents_arctic_kortsch2015" = "metawebs/arctic/barents_arctic_kortsch2015.rds",
  "barents_boreal_kortsch2015" = "metawebs/arctic/barents_boreal_kortsch2015.rds"
)

metawebs_found <- 0
for (metaweb_name in names(expected_metawebs)) {
  metaweb_file <- expected_metawebs[[metaweb_name]]
  if (file.exists(metaweb_file)) {
    print_check(paste("  Metaweb:", metaweb_name), "PASS")
    metawebs_found <- metawebs_found + 1
  } else {
    print_check(paste("  Metaweb:", metaweb_name), "WARN",
               paste("File not found:", metaweb_file))
  }
}

if (metawebs_found > 0) {
  cat(sprintf("   Found %d of %d metawebs\n", metawebs_found, length(expected_metawebs)))
} else {
  print_check("Metaweb files", "ERROR", "No metaweb files found - Phase 2 will not work")
}

# ============================================================================
# Check 3: R package dependencies
# ============================================================================
cat("\n[3] Checking R Package Dependencies...\n")

required_packages <- c(
  "shiny", "bs4Dash", "igraph", "fluxweb", "visNetwork",
  "ggplot2", "DT", "dplyr", "tidyr", "jsonlite"
)

# Phase 1 (Spatial Analysis) packages
phase1_packages <- c("sf", "leaflet")

optional_packages <- c("sp", "readxl", "Hmisc")

missing_packages <- c()
for (pkg in required_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    # Package available - don't print individual successes to reduce noise
  } else {
    print_check(paste("Package:", pkg), "ERROR", "Not installed")
    missing_packages <- c(missing_packages, pkg)
  }
}

if (length(missing_packages) == 0) {
  print_check("All required packages", "PASS")
  cat(sprintf("   Checked %d packages\n", length(required_packages)))
} else {
  cat(sprintf("\n   Missing packages: %s\n", paste(missing_packages, collapse = ", ")))
  cat("   Run: Rscript deployment/install_dependencies.R\n")
}

# Check Phase 1 packages
missing_phase1 <- c()
for (pkg in phase1_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    print_check(paste("Phase 1 package:", pkg), "PASS")
  } else {
    print_check(paste("Phase 1 package:", pkg), "WARN", "Phase 1 Spatial Analysis will not work")
    missing_phase1 <- c(missing_phase1, pkg)
  }
}

# Check optional packages
for (pkg in optional_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    print_check(paste("Optional package:", pkg), "PASS")
  } else {
    print_check(paste("Optional package:", pkg), "WARN", "Some features may not work")
  }
}

# ============================================================================
# Check 4: Validate R syntax
# ============================================================================
cat("\n[4] Checking R Syntax...\n")


r_files <- c("app.R")
if (file.exists("run_app.R")) {
  r_files <- c(r_files, "run_app.R")
}
for (file in r_files) {
  if (file.exists(file)) {
    result <- tryCatch({
      parse(file)
      TRUE
    }, error = function(e) {
      print_check(paste("Syntax:", file), "ERROR", e$message)
      FALSE
    })
    if (result) {
      print_check(paste("Syntax:", file), "PASS")
    }
  }
}

# ============================================================================
# Check 5: Validate app structure
# ============================================================================
cat("\n[5] Validating App Structure...\n")


# Check if required functions exist in app.R and R/functions/
app_content <- paste(readLines("app.R"), collapse = "\n")
func_files <- list.files("R/functions", pattern = "[.]R$", full.names = TRUE)
func_content <- ""
for (ff in func_files) {
  func_content <- paste(func_content, paste(readLines(ff), collapse = "\n"), sep = "\n")
}
all_content <- paste(app_content, func_content, sep = "\n")

# Check for key components
if (grepl("calculate_losses", all_content)) {
  print_check("Function: calculate_losses", "PASS")
} else {
  print_check("Function: calculate_losses", "WARN", "Not found")
}

if (grepl("get_fluxweb_results", all_content)) {
  print_check("Function: get_fluxweb_results", "PASS")
} else {
  print_check("Function: get_fluxweb_results", "ERROR", "Not found")
}

if (grepl("trophiclevels", all_content)) {
  print_check("Function: trophiclevels", "PASS")
} else {
  print_check("Function: trophiclevels", "ERROR", "Not found")
}

# Check Phase 1 spatial functions (CRITICAL fixes)
phase1_functions <- c(
  "create_hexagonal_grid",
  "assign_species_to_hexagons",
  "extract_local_networks",
  "calculate_spatial_metrics",
  "create_spatial_foodweb_data"
)
for (func in phase1_functions) {
  if (grepl(func, all_content)) {
    print_check(paste("Phase 1 function:", func), "PASS")
  } else {
    print_check(paste("Phase 1 function:", func), "WARN", "Phase 1 may not work")
  }
}

# Check Phase 2 metaweb functions
phase2_functions <- c(
  "create_metaweb",
  "validate_metaweb",
  "metaweb_to_igraph"
)
for (func in phase2_functions) {
  if (grepl(func, all_content)) {
    print_check(paste("Phase 2 function:", func), "PASS")
  } else {
    print_check(paste("Phase 2 function:", func), "WARN", "Phase 2 may not work")
  }
}

# Check UI and server are in app.R
if (grepl("dashboardPage", app_content)) {
  print_check("UI: dashboardPage", "PASS")
} else {
  print_check("UI: dashboardPage", "ERROR", "Not found")
}

if (grepl("server.*function", app_content)) {
  print_check("Server: server function", "PASS")
} else {
  print_check("Server: server function", "ERROR", "Not found")
}

if (grepl("shinyApp", app_content)) {
  print_check("App: shinyApp call", "PASS")
} else {
  print_check("App: shinyApp call", "ERROR", "Not found")
}

# Check for Rpath module refactoring (v1.0.20)
cat("\n[5.5] Validating Rpath Module Structure...\n")

# Check for new modular Rpath files
if (file.exists("R/ui/rpath_ui.R")) {
  print_check("Rpath UI module: R/ui/rpath_ui.R", "PASS")

  # Check for rpathModuleUI function
  rpath_ui_content <- paste(readLines("R/ui/rpath_ui.R"), collapse = "\n")
  if (grepl("rpathModuleUI\\s*<-\\s*function", rpath_ui_content)) {
    print_check("  Function: rpathModuleUI", "PASS")
  } else {
    print_check("  Function: rpathModuleUI", "ERROR", "Not found in rpath_ui.R")
  }

  # Check for NS() usage (proper module pattern)
  if (grepl("ns\\s*<-\\s*NS\\(id\\)", rpath_ui_content)) {
    print_check("  Namespace: NS(id)", "PASS")
  } else {
    print_check("  Namespace: NS(id)", "WARN", "Module may not use proper namespacing")
  }
} else {
  print_check("Rpath UI module: R/ui/rpath_ui.R", "ERROR", "File not found")
}

if (file.exists("R/modules/rpath_server.R")) {
  print_check("Rpath Server module: R/modules/rpath_server.R", "PASS")

  # Check for rpathModuleServer function
  rpath_server_content <- paste(readLines("R/modules/rpath_server.R"), collapse = "\n")
  if (grepl("rpathModuleServer\\s*<-\\s*function", rpath_server_content)) {
    print_check("  Function: rpathModuleServer", "PASS")
  } else {
    print_check("  Function: rpathModuleServer", "ERROR", "Not found in rpath_server.R")
  }

  # Check for moduleServer() usage
  if (grepl("moduleServer\\(", rpath_server_content)) {
    print_check("  Module pattern: moduleServer()", "PASS")
  } else {
    print_check("  Module pattern: moduleServer()", "ERROR", "Not using moduleServer()")
  }
} else {
  print_check("Rpath Server module: R/modules/rpath_server.R", "ERROR", "File not found")
}

# Check for auxillary parser (required for comments/tooltips)
if (file.exists("R/functions/auxillary_parser.R")) {
  print_check("Auxillary parser: R/functions/auxillary_parser.R", "PASS")

  auxillary_content <- paste(readLines("R/functions/auxillary_parser.R"), collapse = "\n")

  # Check for key functions
  auxillary_functions <- c("parse_auxillary_valueid", "organize_auxillary_data", "extract_citations_from_remarks")
  for (func in auxillary_functions) {
    if (grepl(paste0(func, "\\s*<-\\s*function"), auxillary_content)) {
      print_check(paste("  Function:", func), "PASS")
    } else {
      print_check(paste("  Function:", func), "ERROR", "Not found")
    }
  }
} else {
  print_check("Auxillary parser: R/functions/auxillary_parser.R", "ERROR", "File not found")
}

# Check for proper sourcing in app.R
if (file.exists("app.R")) {
  app_content <- paste(readLines("app.R"), collapse = "\n")

  if (grepl('source\\(["\']R/ui/rpath_ui\\.R["\']\\)', app_content)) {
    print_check("App.R sources: R/ui/rpath_ui.R", "PASS")
  } else {
    print_check("App.R sources: R/ui/rpath_ui.R", "ERROR", "Not sourced in app.R")
  }

  if (grepl('source\\(["\']R/modules/rpath_server\\.R["\']\\)', app_content)) {
    print_check("App.R sources: R/modules/rpath_server.R", "PASS")
  } else {
    print_check("App.R sources: R/modules/rpath_server.R", "ERROR", "Not sourced in app.R")
  }

  if (grepl('source\\(["\']R/functions/auxillary_parser\\.R["\']\\)', app_content)) {
    print_check("App.R sources: R/functions/auxillary_parser.R", "PASS")
  } else {
    print_check("App.R sources: R/functions/auxillary_parser.R", "ERROR", "Not sourced in app.R")
  }

  # Check that old monolithic file is NOT sourced
  if (grepl('source\\(["\']R/modules/rpath_module\\.R["\']\\)', app_content)) {
    print_check("App.R sources: OLD rpath_module.R", "ERROR", "Still sourcing old monolithic file!")
  } else {
    print_check("App.R sources: OLD rpath_module.R removed", "PASS")
  }
}

# Check for critical fixes (from CODE_REVIEW_REPORT.md)
cat("\n[5.6] Validating Critical Fixes...\n")

# CRITICAL-001: Check for global current_metaweb
if (grepl("current_metaweb\\s*<-\\s*reactiveVal\\(NULL\\)", app_content)) {
  app_lines <- readLines("app.R")
  server_start <- which(grepl("^server\\s*<-\\s*function", app_lines))[1]
  metaweb_line <- which(grepl("current_metaweb\\s*<-\\s*reactiveVal", app_lines))[1]
  if (!is.na(server_start) && !is.na(metaweb_line)) {
    if (metaweb_line - server_start < 50) {
      print_check("FIX: current_metaweb global scope", "PASS")
    } else {
      print_check("FIX: current_metaweb global scope", "WARN",
                 "current_metaweb may not be in global scope")
    }
  }
} else {
  print_check("FIX: current_metaweb declaration", "ERROR", "Not found")
}

# CRITICAL-002: Check for Phase 1 validation
if (grepl("Please create a hexagonal grid first", app_content)) {
  print_check("FIX: Phase 1 step validation", "PASS")
} else {
  print_check("FIX: Phase 1 step validation", "WARN", "Validation messages not found")
}

# CRITICAL-003: Check for METAWEB_PATHS validation
if (file.exists("R/config.R")) {
  config_content <- paste(readLines("R/config.R"), collapse = "\n")
  if (grepl("METAWEB_PATHS\\s*<-\\s*list", config_content)) {
    print_check("FIX: METAWEB_PATHS definition", "PASS")
  } else {
    print_check("FIX: METAWEB_PATHS definition", "WARN", "Not found in R/config.R")
  }
} else if (grepl("METAWEB_PATHS\\s*<-\\s*list", app_content)) {
  print_check("FIX: METAWEB_PATHS definition", "PASS")
} else {
  print_check("FIX: METAWEB_PATHS definition", "WARN", "Not found in app.R or R/config.R")
}

# ============================================================================
# Check 7: Check for common issues
# ============================================================================
cat("\n[7] Checking for Common Issues...\n")

# Check for temporary files
temp_patterns <- c("*~", "*.tmp", "*.log", ".Rhistory", ".RData")
temp_found <- FALSE
for (pattern in temp_patterns) {
  temp_files <- list.files(pattern = glob2rx(pattern), recursive = FALSE)
  if (length(temp_files) > 0) {
    if (!temp_found) {
      print_check("Temporary files", "WARN", "Found temporary files in root")
      temp_found <- TRUE
    }
  }
}
if (!temp_found) {
  print_check("Temporary files", "PASS")
}

# Check for backup files
backup_files <- list.files(pattern = ".*backup.*\\.R$", ignore.case = TRUE)
if (length(backup_files) > 0) {
  print_check("Backup files", "WARN",
             sprintf("Found %d backup files", length(backup_files)))
} else {
  print_check("Backup files", "PASS")
}

# Check data file size
if (file.exists("BalticFW.Rdata")) {
  file_size_mb <- file.size("BalticFW.Rdata") / 1024 / 1024
  if (file_size_mb > 50) {
    print_check("Data file size", "WARN",
               sprintf("Large file: %.1f MB", file_size_mb))
  } else {
    print_check("Data file size", "PASS")
  }
}

# ============================================================================
# Summary
# ============================================================================
cat("\n================================================================================\n")
cat("Pre-Deployment Check Summary\n")
cat("================================================================================\n\n")

total_checks <- checks_passed + warnings + errors

cat(sprintf("Total Checks: %d\n", total_checks))
cat(sprintf("✓ Passed: %d\n", checks_passed))
if (warnings > 0) {
  cat(sprintf("⚠ Warnings: %d\n", warnings))
}
if (errors > 0) {
  cat(sprintf("✗ Errors: %d\n", errors))
}

cat("\n")

if (errors > 0) {
  cat("❌ DEPLOYMENT NOT RECOMMENDED\n")
  cat("   Please fix the errors above before deploying.\n\n")
  quit(status = 1)
} else if (warnings > 0) {
  cat("⚠️  DEPLOYMENT POSSIBLE WITH WARNINGS\n")
  cat("   Review warnings before deploying to production.\n\n")
  quit(status = 0)
} else {
  cat("✅ ALL CHECKS PASSED\n")
  cat("   Application is ready for deployment!\n\n")
  quit(status = 0)
}
