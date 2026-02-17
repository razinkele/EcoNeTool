# ECOPATH Balancing Test
# Tests the complete balancing workflow including error detection and parameter editor fixes
# Specifically addresses: "missing value where TRUE/FALSE needed" error
#
# Author: EcoNeTool Testing Suite
# Date: 2025-12-08

cat("\n", strrep("=", 80), "\n")
cat("ECOPATH BALANCING TEST\n")
cat("Testing: Error Detection → Parameter Editing → Successful Balance\n")
cat(strrep("=", 80), "\n\n")

# ==============================================================================
# SETUP
# ==============================================================================

cat("Loading functions...\n")
source("R/functions/ecopath_import.R")
source("R/functions/rpath_integration.R")

suppressPackageStartupMessages({
  if (!require("Rpath", quietly = TRUE)) {
    stop("Rpath package required. Install with: remotes::install_github('noaa-edab/Rpath')")
  }
  if (!require("data.table", quietly = TRUE)) {
    stop("data.table required")
  }
})

# Test file
TEST_FILE <- "examples/LTgoby.eweaccdb"

# Test tracking
test_counter <- 0
test_results <- list()

run_test <- function(test_name, test_function) {
  test_counter <<- test_counter + 1
  cat(sprintf("\n[Test %d] %s\n", test_counter, test_name))
  cat(strrep("-", 80), "\n")

  result <- tryCatch({
    test_function()
    list(status = "PASS", error = NULL)
  }, error = function(e) {
    cat("✗ ERROR:", e$message, "\n")
    list(status = "FAIL", error = e$message)
  })

  test_results[[test_counter]] <<- list(
    test_number = test_counter,
    test_name = test_name,
    status = result$status,
    error = result$error
  )

  if (result$status == "PASS") {
    cat("✓ PASSED\n")
  } else {
    cat("✗ FAILED\n")
  }

  return(result$status == "PASS")
}

# Storage
balancing_data <- list()

# ==============================================================================
# PHASE 1: IMPORT AND CONVERT
# ==============================================================================

cat("\n### PHASE 1: Import and Convert ###\n")

run_test("Import ECOPATH database", function() {
  import_result <- parse_ecopath_native_cross_platform(TEST_FILE)
  if (is.null(import_result)) stop("Import failed")

  balancing_data$import <<- import_result
  cat("  ✓ Groups:", nrow(import_result$group_data), "\n")
  cat("  ✓ Diet entries:", nrow(import_result$diet_data), "\n")
})

run_test("Convert to Rpath format (expect warnings)", function() {
  # This SHOULD produce warnings about insufficient parameters
  params <- suppressWarnings({
    convert_ecopath_to_rpath(balancing_data$import, model_name = "LTgoby Balancing Test")
  })

  if (is.null(params)) stop("Conversion failed")

  balancing_data$original_params <<- params
  cat("  ✓ Converted successfully\n")
  cat("  ✓ Model groups:", nrow(params$model), "\n")
})

# ==============================================================================
# PHASE 2: DETECT BALANCING ISSUES
# ==============================================================================

cat("\n### PHASE 2: Detect Balancing Issues ###\n")

run_test("Identify groups with insufficient parameters", function() {
  model <- balancing_data$original_params$model

  issues <- list()
  for (i in 1:nrow(model)) {
    if (model$Type[i] >= 3) next  # Skip fleets

    n_params <- sum(!is.na(c(model$Biomass[i], model$PB[i], model$QB[i], model$EE[i])))

    if (n_params < 3) {
      issues[[length(issues) + 1]] <- list(
        row = i,
        group = model$Group[i],
        type = model$Type[i],
        n_params = n_params,
        biomass = model$Biomass[i],
        pb = model$PB[i],
        qb = model$QB[i],
        ee = model$EE[i]
      )
    }
  }

  balancing_data$param_issues <<- issues

  if (length(issues) == 0) {
    cat("  ⚠ No issues found (unexpected for LTgoby)\n")
  } else {
    cat("  ✓ Found", length(issues), "groups with insufficient parameters:\n")
    for (issue in issues) {
      cat("    -", issue$group, ":", issue$n_params, "out of 4 parameters\n")
      cat("      B:", ifelse(is.na(issue$biomass), "MISSING", round(issue$biomass, 3)))
      cat("  P/B:", ifelse(is.na(issue$pb), "MISSING", round(issue$pb, 3)))
      cat("  Q/B:", ifelse(is.na(issue$qb), "MISSING", round(issue$qb, 3)))
      cat("  EE:", ifelse(is.na(issue$ee), "MISSING", round(issue$ee, 3)), "\n")
    }
  }
})

run_test("Check for NA values in Type column", function() {
  model <- balancing_data$original_params$model

  if (any(is.na(model$Type))) {
    na_groups <- model$Group[is.na(model$Type)]
    stop("Groups have NA Type values: ", paste(na_groups, collapse = ", "))
  }

  cat("  ✓ No NA values in Type column\n")
})

run_test("Validate diet matrix sums", function() {
  diet <- balancing_data$original_params$diet
  diet_cols <- names(diet)[names(diet) != "Group"]

  invalid_sums <- c()
  for (col in diet_cols) {
    col_sum <- sum(diet[[col]], na.rm = TRUE)
    if (col_sum > 1.01) {
      invalid_sums <- c(invalid_sums, paste0(col, "=", round(col_sum, 3)))
    }
  }

  if (length(invalid_sums) > 0) {
    cat("  ⚠ Invalid diet sums:", paste(invalid_sums, collapse = ", "), "\n")
  } else {
    cat("  ✓ All diet sums valid (≤ 1.0)\n")
  }
})

# ==============================================================================
# PHASE 3: ATTEMPT BALANCE WITH ORIGINAL PARAMETERS
# ==============================================================================

cat("\n### PHASE 3: Attempt Balance with Original Parameters ###\n")

run_test("Try balancing without fixes (expect to fail or succeed)", function() {
  # This test is expected to fail for problematic databases
  # But will pass for well-formed databases

  balance_result <- tryCatch({
    Rpath::rpath(balancing_data$original_params)
  }, error = function(e) {
    cat("  ⚠ Balance failed (expected):", e$message, "\n")
    NULL
  })

  balancing_data$original_balance_result <<- balance_result

  if (is.null(balance_result)) {
    if (length(balancing_data$param_issues) > 0) {
      cat("  ✓ Balance failed as expected (groups with insufficient parameters)\n")
      cat("  → This demonstrates the need for parameter editors!\n")
    } else {
      cat("  ⚠ Balance failed unexpectedly (no obvious parameter issues)\n")
    }
  } else {
    cat("  ✓ Balance succeeded with original parameters\n")
    cat("  → Database is well-formed\n")
  }
})

# ==============================================================================
# PHASE 4: FIX PARAMETERS (SIMULATE PARAMETER EDITOR)
# ==============================================================================

cat("\n### PHASE 4: Fix Parameters (Simulate Parameter Editor) ###\n")

run_test("Apply fixes for insufficient parameters", function() {
  if (length(balancing_data$param_issues) == 0) {
    cat("  ⚠ No issues to fix\n")
    balancing_data$fixed_params <<- balancing_data$original_params
    return()
  }

  # Copy parameters for editing
  fixed_params <- balancing_data$original_params
  model <- fixed_params$model

  n_fixed <- 0

  for (issue in balancing_data$param_issues) {
    row <- issue$row
    group_name <- issue$group
    group_type <- issue$type

    cat("  → Fixing", group_name, "(Type:", group_type, ")\n")

    # Strategy: Add missing Biomass if needed and possible
    if (is.na(model$Biomass[row])) {
      # Estimate Biomass based on type
      if (group_type == 0) {
        # Consumer - use moderate biomass
        model$Biomass[row] <- 5.0
        cat("    + Added Biomass = 5.0 (consumer default)\n")
      } else if (group_type == 1) {
        # Producer - use higher biomass
        model$Biomass[row] <- 10.0
        cat("    + Added Biomass = 10.0 (producer default)\n")
      } else if (group_type == 2) {
        # Detritus - use very high biomass
        model$Biomass[row] <- 50.0
        cat("    + Added Biomass = 50.0 (detritus default)\n")
      }
      n_fixed <- n_fixed + 1
    }

    # Alternative: Add missing EE if Biomass can't be estimated
    if (is.na(model$EE[row]) && sum(!is.na(c(model$Biomass[row], model$PB[row], model$QB[row], model$EE[row]))) < 3) {
      if (!is.na(model$PB[row]) && model$Type[row] == 1) {
        # Producers typically have high EE
        model$EE[row] <- 0.95
        cat("    + Added EE = 0.95 (producer default)\n")
        n_fixed <- n_fixed + 1
      }
    }
  }

  fixed_params$model <- model
  balancing_data$fixed_params <<- fixed_params

  cat("  ✓ Fixed", n_fixed, "parameter issues\n")
})

run_test("Re-validate fixed parameters", function() {
  model <- balancing_data$fixed_params$model

  remaining_issues <- 0
  for (i in 1:nrow(model)) {
    if (model$Type[i] >= 3) next

    n_params <- sum(!is.na(c(model$Biomass[i], model$PB[i], model$QB[i], model$EE[i])))
    if (n_params < 3) {
      remaining_issues <- remaining_issues + 1
      cat("  ⚠ Still insufficient:", model$Group[i], "has only", n_params, "parameters\n")
    }
  }

  if (remaining_issues > 0) {
    warning("Still have ", remaining_issues, " groups with insufficient parameters")
  } else {
    cat("  ✓ All groups now have sufficient parameters (≥ 3 out of 4)\n")
  }

  balancing_data$remaining_issues <<- remaining_issues
})

# ==============================================================================
# PHASE 5: BALANCE WITH FIXED PARAMETERS
# ==============================================================================

cat("\n### PHASE 5: Balance with Fixed Parameters ###\n")

run_test("Balance model with fixed parameters", function() {
  balanced <- tryCatch({
    Rpath::rpath(balancing_data$fixed_params)
  }, error = function(e) {
    cat("  ✗ Balance still failed:", e$message, "\n")
    stop("Balance failed even after parameter fixes: ", e$message)
  })

  if (is.null(balanced)) {
    stop("Balance returned NULL")
  }

  balancing_data$balanced_model <<- balanced

  cat("  ✓ Model balanced successfully!\n")
  cat("  ✓ Class:", class(balanced), "\n")
})

run_test("Validate balanced model results", function() {
  model <- balancing_data$balanced_model

  # Total biomass
  total_biomass <- sum(model$Biomass, na.rm = TRUE)
  if (is.na(total_biomass) || total_biomass <= 0) {
    stop("Invalid total biomass: ", total_biomass)
  }
  cat("  ✓ Total biomass:", round(total_biomass, 2), "t/km²\n")

  # EE range
  if (!is.null(model$EE)) {
    ee_range <- range(model$EE, na.rm = TRUE)
    cat("  ✓ EE range:", round(ee_range[1], 3), "-", round(ee_range[2], 3), "\n")

    if (any(model$EE > 1.0, na.rm = TRUE)) {
      overexploited <- sum(model$EE > 1.0, na.rm = TRUE)
      cat("  ⚠", overexploited, "groups have EE > 1.0 (overexploited)\n")
    }
  }

  # Trophic levels
  if (!is.null(model$TL)) {
    tl_range <- range(model$TL, na.rm = TRUE)
    cat("  ✓ Trophic level range:", round(tl_range[1], 2), "-", round(tl_range[2], 2), "\n")
  }

  # Respiration
  if (!is.null(model$Respiration)) {
    total_resp <- sum(model$Respiration, na.rm = TRUE)
    cat("  ✓ Total respiration:", round(total_resp, 2), "t/km²/year\n")
  }

  balancing_data$final_biomass <<- total_biomass
})

# ==============================================================================
# PHASE 6: COMPARE RESULTS
# ==============================================================================

cat("\n### PHASE 6: Compare Results ###\n")

run_test("Compare original vs fixed parameters", function() {
  if (!is.null(balancing_data$original_balance_result) && !is.null(balancing_data$balanced_model)) {
    # Both balanced
    orig_biomass <- sum(balancing_data$original_balance_result$Biomass, na.rm = TRUE)
    fixed_biomass <- balancing_data$final_biomass

    cat("  ✓ Original parameters balanced\n")
    cat("    Biomass:", round(orig_biomass, 2), "t/km²\n")
    cat("  ✓ Fixed parameters balanced\n")
    cat("    Biomass:", round(fixed_biomass, 2), "t/km²\n")
    cat("  → Difference:", round(100 * (fixed_biomass - orig_biomass) / orig_biomass, 2), "%\n")

  } else if (is.null(balancing_data$original_balance_result) && !is.null(balancing_data$balanced_model)) {
    # Only fixed version balanced
    cat("  ⚠ Original parameters: FAILED to balance\n")
    cat("  ✓ Fixed parameters: BALANCED successfully\n")
    cat("    Biomass:", round(balancing_data$final_biomass, 2), "t/km²\n")
    cat("\n  🎯 KEY FINDING: Parameter editors fixed the balancing error!\n")

  } else if (!is.null(balancing_data$original_balance_result) && is.null(balancing_data$balanced_model)) {
    # Shouldn't happen - original worked but fixed didn't
    cat("  ✓ Original parameters: balanced\n")
    cat("  ✗ Fixed parameters: FAILED (unexpected)\n")
    warning("Fixes made things worse - this shouldn't happen")

  } else {
    # Both failed
    cat("  ✗ Both original and fixed parameters failed to balance\n")
    cat("  → Additional fixes needed beyond adding missing parameters\n")
  }
})

run_test("Document parameter changes made", function() {
  if (length(balancing_data$param_issues) == 0) {
    cat("  ⚠ No parameter changes were needed\n")
    return()
  }

  cat("  Summary of changes made:\n")

  original_model <- balancing_data$original_params$model
  fixed_model <- balancing_data$fixed_params$model

  n_changes <- 0

  for (issue in balancing_data$param_issues) {
    row <- issue$row
    group_name <- issue$group

    # Check what changed
    changes <- c()

    if (is.na(original_model$Biomass[row]) && !is.na(fixed_model$Biomass[row])) {
      changes <- c(changes, paste0("Biomass: NA → ", round(fixed_model$Biomass[row], 2)))
      n_changes <- n_changes + 1
    }

    if (is.na(original_model$EE[row]) && !is.na(fixed_model$EE[row])) {
      changes <- c(changes, paste0("EE: NA → ", round(fixed_model$EE[row], 2)))
      n_changes <- n_changes + 1
    }

    if (length(changes) > 0) {
      cat("    •", group_name, ":", paste(changes, collapse = ", "), "\n")
    }
  }

  cat("  ✓ Total parameter changes:", n_changes, "\n")
})

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n", strrep("=", 80), "\n")
cat("TEST SUMMARY\n")
cat(strrep("=", 80), "\n\n")

total <- length(test_results)
passed <- sum(sapply(test_results, function(x) x$status == "PASS"))
failed <- total - passed

cat(sprintf("Total Tests: %d\n", total))
cat(sprintf("Passed: %d (%.1f%%)\n", passed, 100 * passed / total))
cat(sprintf("Failed: %d\n", failed))

if (failed > 0) {
  cat("\n❌ FAILED TESTS:\n")
  for (r in test_results) {
    if (r$status == "FAIL") {
      cat(sprintf("  [%d] %s - %s\n", r$test_number, r$test_name, r$error))
    }
  }
}

cat("\n📊 BALANCING WORKFLOW RESULTS:\n")
cat("  Database: LTgoby.eweaccdb\n")
cat("  Groups:", nrow(balancing_data$original_params$model), "\n")
cat("  Initial issues:", length(balancing_data$param_issues), "groups with insufficient parameters\n")

if (!is.null(balancing_data$original_balance_result)) {
  cat("  Original balance: ✓ SUCCESS\n")
} else {
  cat("  Original balance: ✗ FAILED (as expected)\n")
}

cat("  Parameter fixes applied:", ifelse(length(balancing_data$param_issues) > 0, "YES", "NO"), "\n")
cat("  Remaining issues:", balancing_data$remaining_issues, "\n")

if (!is.null(balancing_data$balanced_model)) {
  cat("  Fixed balance: ✓ SUCCESS\n")
  cat("  Final biomass:", round(balancing_data$final_biomass, 2), "t/km²\n")
} else {
  cat("  Fixed balance: ✗ FAILED\n")
}

cat("\n🎯 CONCLUSION:\n")

if (is.null(balancing_data$original_balance_result) && !is.null(balancing_data$balanced_model)) {
  cat("  ✅ Parameter editors successfully fixed the balancing error!\n")
  cat("  ✅ This validates the parameter editor workflow:\n")
  cat("     1. Import database\n")
  cat("     2. Detect parameter issues\n")
  cat("     3. Fix using parameter editors\n")
  cat("     4. Balance successfully\n")
} else if (!is.null(balancing_data$original_balance_result)) {
  cat("  ✅ Database was already well-formed and balanced without fixes\n")
  cat("  ✅ Parameter editors are available if needed in future\n")
} else {
  cat("  ⚠ Additional investigation needed for this database\n")
  cat("  → May require manual parameter adjustments beyond automated fixes\n")
}

cat("\n", strrep("=", 80), "\n")
if (failed == 0) {
  cat("✅ BALANCING TEST COMPLETE - ALL TESTS PASSED\n")
} else {
  cat("⚠ BALANCING TEST COMPLETE - SOME TESTS FAILED\n")
}
cat(strrep("=", 80), "\n\n")
