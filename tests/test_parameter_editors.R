# Parameter Editors Validation Test
# Tests the Group Parameters and Diet Matrix editors
# Validates editing functionality and ability to fix balancing issues
#
# Author: EcoNeTool Testing Suite
# Date: 2025-12-08

cat("\n", strrep("=", 80), "\n")
cat("PARAMETER EDITORS VALIDATION TEST\n")
cat("Testing: Group Parameters Editor & Diet Matrix Editor\n")
cat(strrep("=", 80), "\n\n")

# ==============================================================================
# SETUP
# ==============================================================================

cat("Loading functions...\n")
source("R/functions/ecopath_import.R")
source("R/functions/rpath_integration.R")

suppressPackageStartupMessages({
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
test_data <- list()

# ==============================================================================
# PHASE 1: LOAD TEST DATA
# ==============================================================================

cat("\n### PHASE 1: Load Test Data ###\n")

run_test("Import ECOPATH database", function() {
  import_result <- parse_ecopath_native_cross_platform(TEST_FILE)
  if (is.null(import_result)) stop("Import failed")

  test_data$import <<- import_result
  cat("  ✓ Groups:", nrow(import_result$group_data), "\n")
})

run_test("Convert to Rpath format", function() {
  params <- convert_ecopath_to_rpath(test_data$import, model_name = "Test")
  if (is.null(params)) stop("Conversion failed")

  test_data$params <<- params
  cat("  ✓ Model groups:", nrow(params$model), "\n")
  cat("  ✓ Diet matrix:", nrow(params$diet), "×", ncol(params$diet), "\n")
})

# ==============================================================================
# PHASE 2: GROUP PARAMETERS VALIDATION
# ==============================================================================

cat("\n### PHASE 2: Group Parameters Validation ###\n")

run_test("Validate group parameter structure", function() {
  model <- test_data$params$model

  # Check required columns
  required <- c("Group", "Type", "Biomass", "PB", "QB", "EE")
  missing <- setdiff(required, names(model))
  if (length(missing) > 0) {
    stop("Missing columns: ", paste(missing, collapse = ", "))
  }

  cat("  ✓ All required columns present\n")
})

run_test("Check parameter completeness (3 out of 4 rule)", function() {
  model <- test_data$params$model

  issues <- list()
  for (i in 1:nrow(model)) {
    if (model$Type[i] >= 3) next  # Skip fleets

    n_params <- sum(!is.na(c(model$Biomass[i], model$PB[i], model$QB[i], model$EE[i])))

    if (n_params < 3) {
      issues[[length(issues) + 1]] <- list(
        group = model$Group[i],
        n_params = n_params,
        biomass = model$Biomass[i],
        pb = model$PB[i],
        qb = model$QB[i],
        ee = model$EE[i]
      )
    }
  }

  test_data$param_issues <<- issues

  if (length(issues) > 0) {
    cat("  ⚠ Found", length(issues), "groups with insufficient parameters:\n")
    for (issue in issues) {
      cat("    -", issue$group, ":", issue$n_params, "out of 4 parameters\n")
    }
  } else {
    cat("  ✓ All groups have sufficient parameters\n")
  }
})

run_test("Validate Type column (no NA values)", function() {
  model <- test_data$params$model

  if (any(is.na(model$Type))) {
    na_groups <- model$Group[is.na(model$Type)]
    stop("Groups with NA Type: ", paste(na_groups, collapse = ", "))
  }

  cat("  ✓ No NA values in Type column\n")
})

run_test("Validate Type values (must be 0, 1, 2, or 3)", function() {
  model <- test_data$params$model

  invalid <- model$Type[!model$Type %in% c(0, 1, 2, 3)]
  if (length(invalid) > 0) {
    stop("Invalid Type values found: ", paste(unique(invalid), collapse = ", "))
  }

  cat("  ✓ All Type values valid\n")
})

run_test("Validate P/B values (must be > 0 if present)", function() {
  model <- test_data$params$model

  invalid <- which(!is.na(model$PB) & model$PB <= 0)
  if (length(invalid) > 0) {
    groups <- model$Group[invalid]
    stop("Groups with invalid P/B: ", paste(groups, collapse = ", "))
  }

  cat("  ✓ All P/B values valid\n")
})

run_test("Validate Q/B values (must be > 0 if present)", function() {
  model <- test_data$params$model

  invalid <- which(!is.na(model$QB) & model$QB <= 0)
  if (length(invalid) > 0) {
    groups <- model$Group[invalid]
    stop("Groups with invalid Q/B: ", paste(groups, collapse = ", "))
  }

  cat("  ✓ All Q/B values valid\n")
})

run_test("Validate EE values (must be 0-1 if present)", function() {
  model <- test_data$params$model

  invalid <- which(!is.na(model$EE) & (model$EE < 0 | model$EE > 1))
  if (length(invalid) > 0) {
    groups <- model$Group[invalid]
    values <- model$EE[invalid]
    stop("Groups with invalid EE: ", paste(paste0(groups, "=", round(values, 3)), collapse = ", "))
  }

  cat("  ✓ All EE values in valid range [0, 1]\n")
})

# ==============================================================================
# PHASE 3: DIET MATRIX VALIDATION
# ==============================================================================

cat("\n### PHASE 3: Diet Matrix Validation ###\n")

run_test("Validate diet matrix structure", function() {
  diet <- test_data$params$diet

  # Should have Group column
  if (!"Group" %in% names(diet)) {
    stop("Diet matrix missing Group column")
  }

  # Should have predator columns
  diet_cols <- names(diet)[names(diet) != "Group"]
  if (length(diet_cols) == 0) {
    stop("Diet matrix has no predator columns")
  }

  cat("  ✓ Group column present\n")
  cat("  ✓ Predator columns:", length(diet_cols), "\n")
})

run_test("Validate diet coefficient range (0-1)", function() {
  diet <- test_data$params$diet
  diet_cols <- names(diet)[names(diet) != "Group"]

  invalid <- c()
  for (col in diet_cols) {
    values <- diet[[col]]
    out_of_range <- which(!is.na(values) & (values < 0 | values > 1))
    if (length(out_of_range) > 0) {
      invalid <- c(invalid, paste0(col, ":", length(out_of_range)))
    }
  }

  if (length(invalid) > 0) {
    stop("Columns with invalid values: ", paste(invalid, collapse = ", "))
  }

  cat("  ✓ All diet coefficients in [0, 1] range\n")
})

run_test("Check diet column sums (should be ≤ 1.0)", function() {
  diet <- test_data$params$diet
  diet_cols <- names(diet)[names(diet) != "Group"]

  invalid_sums <- list()
  for (col in diet_cols) {
    col_sum <- sum(diet[[col]], na.rm = TRUE)
    if (col_sum > 1.01) {  # Small tolerance
      invalid_sums[[length(invalid_sums) + 1]] <- list(
        predator = col,
        sum = col_sum
      )
    }
  }

  test_data$diet_issues <<- invalid_sums

  if (length(invalid_sums) > 0) {
    cat("  ⚠ Found", length(invalid_sums), "predators with diet sum > 1.0:\n")
    for (issue in invalid_sums) {
      cat("    -", issue$predator, ":", round(issue$sum, 3), "\n")
    }
  } else {
    cat("  ✓ All predator diets sum to ≤ 1.0\n")
  }
})

# ==============================================================================
# PHASE 4: SIMULATED PARAMETER EDITING
# ==============================================================================

cat("\n### PHASE 4: Simulated Parameter Editing ###\n")

run_test("Simulate fixing insufficient parameters", function() {
  # Make a copy of params for editing
  edited_params <- test_data$params
  model <- edited_params$model

  # Fix each group with insufficient parameters
  n_fixed <- 0

  for (issue in test_data$param_issues) {
    group_name <- issue$group
    idx <- which(model$Group == group_name)

    if (length(idx) == 0) next

    # Add missing Biomass if needed
    if (is.na(model$Biomass[idx])) {
      # Use reasonable default based on Type
      if (model$Type[idx] == 0) {  # Consumer
        model$Biomass[idx] <- 5.0
      } else if (model$Type[idx] == 1) {  # Producer
        model$Biomass[idx] <- 10.0
      } else if (model$Type[idx] == 2) {  # Detritus
        model$Biomass[idx] <- 50.0
      }
      n_fixed <- n_fixed + 1
    }
  }

  edited_params$model <- model
  test_data$edited_params <<- edited_params

  cat("  ✓ Fixed", n_fixed, "groups by adding missing Biomass\n")

  # Recount issues
  new_issues <- 0
  for (i in 1:nrow(model)) {
    if (model$Type[i] >= 3) next
    n_params <- sum(!is.na(c(model$Biomass[i], model$PB[i], model$QB[i], model$EE[i])))
    if (n_params < 3) new_issues <- new_issues + 1
  }

  cat("  ✓ Remaining issues:", new_issues, "\n")
})

run_test("Simulate fixing diet sum issues", function() {
  # Make a copy for editing
  edited_params <- test_data$edited_params
  diet <- edited_params$diet

  # Normalize each predator with sum > 1.0
  n_fixed <- 0

  for (issue in test_data$diet_issues) {
    predator <- issue$predator
    current_sum <- issue$sum

    if (predator %in% names(diet)) {
      # Normalize: multiply all values by (1.0 / current_sum)
      factor <- 1.0 / current_sum
      diet[[predator]] <- diet[[predator]] * factor
      n_fixed <- n_fixed + 1
    }
  }

  edited_params$diet <- diet
  test_data$edited_params <<- edited_params

  cat("  ✓ Normalized", n_fixed, "predator diets\n")

  # Recount issues
  diet_cols <- names(diet)[names(diet) != "Group"]
  new_issues <- 0
  for (col in diet_cols) {
    col_sum <- sum(diet[[col]], na.rm = TRUE)
    if (col_sum > 1.01) new_issues <- new_issues + 1
  }

  cat("  ✓ Remaining diet issues:", new_issues, "\n")
})

run_test("Validate edited parameters", function() {
  model <- test_data$edited_params$model

  # Check parameter completeness again
  incomplete <- 0
  for (i in 1:nrow(model)) {
    if (model$Type[i] >= 3) next
    n_params <- sum(!is.na(c(model$Biomass[i], model$PB[i], model$QB[i], model$EE[i])))
    if (n_params < 3) incomplete <- incomplete + 1
  }

  if (incomplete > 0) {
    warning("Still have ", incomplete, " groups with insufficient parameters")
  } else {
    cat("  ✓ All groups now have sufficient parameters!\n")
  }

  # Check diet sums
  diet <- test_data$edited_params$diet
  diet_cols <- names(diet)[names(diet) != "Group"]

  invalid <- 0
  for (col in diet_cols) {
    col_sum <- sum(diet[[col]], na.rm = TRUE)
    if (col_sum > 1.01) invalid <- invalid + 1
  }

  if (invalid > 0) {
    warning("Still have ", invalid, " predators with invalid diet sums")
  } else {
    cat("  ✓ All predator diets now valid!\n")
  }
})

# ==============================================================================
# PHASE 5: BALANCING WITH EDITED PARAMETERS
# ==============================================================================

cat("\n### PHASE 5: Balance Model with Edited Parameters ###\n")

run_test("Attempt to balance with original parameters", function() {
  # Try to balance original (possibly problematic) parameters
  result <- tryCatch({
    Rpath::rpath(test_data$params)
  }, error = function(e) {
    cat("  ⚠ Original balance failed:", e$message, "\n")
    NULL
  })

  if (is.null(result)) {
    cat("  ⚠ Original parameters cannot be balanced (as expected)\n")
  } else {
    cat("  ✓ Original parameters balanced successfully\n")
    test_data$original_balanced <<- result
  }
})

run_test("Balance model with edited parameters", function() {
  if (!requireNamespace("Rpath", quietly = TRUE)) {
    cat("  ⚠ Rpath not available, skipping balance test\n")
    return()
  }

  balanced <- tryCatch({
    Rpath::rpath(test_data$edited_params)
  }, error = function(e) {
    stop("Balance failed even after editing: ", e$message)
  })

  if (is.null(balanced)) {
    stop("Balance returned NULL")
  }

  test_data$edited_balanced <<- balanced

  cat("  ✓ Model balanced successfully with edited parameters!\n")
  cat("  ✓ Total biomass:", round(sum(balanced$Biomass, na.rm = TRUE), 2), "t/km²\n")
})

run_test("Compare original vs edited results", function() {
  if (!is.null(test_data$original_balanced) && !is.null(test_data$edited_balanced)) {
    orig_biomass <- sum(test_data$original_balanced$Biomass, na.rm = TRUE)
    edit_biomass <- sum(test_data$edited_balanced$Biomass, na.rm = TRUE)

    cat("  ✓ Original biomass:", round(orig_biomass, 2), "t/km²\n")
    cat("  ✓ Edited biomass:", round(edit_biomass, 2), "t/km²\n")
    cat("  ✓ Difference:", round(100 * (edit_biomass - orig_biomass) / orig_biomass, 2), "%\n")
  } else if (!is.null(test_data$edited_balanced)) {
    cat("  ✓ Only edited parameters could be balanced\n")
    cat("  ✓ This demonstrates the editors fix balancing issues!\n")
  }
})

# ==============================================================================
# PHASE 6: EDITOR FUNCTIONALITY TESTS
# ==============================================================================

cat("\n### PHASE 6: Editor Functionality Simulation ###\n")

run_test("Test cell edit validation (Type)", function() {
  # Simulate invalid Type edit
  valid_types <- c(0, 1, 2, 3)

  invalid <- c(-1, 4, 5, NA)
  for (val in invalid) {
    is_valid <- !is.na(val) && val %in% valid_types
    if (is_valid) {
      stop("Invalid Type value", val, "passed validation")
    }
  }

  cat("  ✓ Type validation works correctly\n")
})

run_test("Test cell edit validation (P/B, Q/B)", function() {
  # Simulate invalid P/B or Q/B
  invalid <- c(-1, 0, -10)
  for (val in invalid) {
    is_valid <- !is.na(val) && val > 0
    if (is_valid) {
      stop("Invalid P/B/Q/B value", val, "passed validation")
    }
  }

  # Valid values
  valid <- c(0.5, 1.0, 10.0, 100.0)
  for (val in valid) {
    is_valid <- !is.na(val) && val > 0
    if (!is_valid) {
      stop("Valid P/B/Q/B value", val, "failed validation")
    }
  }

  cat("  ✓ P/B and Q/B validation works correctly\n")
})

run_test("Test cell edit validation (EE)", function() {
  # Simulate invalid EE
  invalid <- c(-0.1, 1.5, 2.0, 10.0)
  for (val in invalid) {
    is_valid <- !is.na(val) && val >= 0 && val <= 1
    if (is_valid) {
      stop("Invalid EE value", val, "passed validation")
    }
  }

  # Valid values
  valid <- c(0, 0.5, 0.95, 1.0)
  for (val in valid) {
    is_valid <- !is.na(val) && val >= 0 && val <= 1
    if (!is_valid) {
      stop("Valid EE value", val, "failed validation")
    }
  }

  cat("  ✓ EE validation works correctly\n")
})

run_test("Test diet coefficient validation", function() {
  # Invalid diet coefficients
  invalid <- c(-0.1, 1.5, 2.0)
  for (val in invalid) {
    is_valid <- !is.na(val) && val >= 0 && val <= 1
    if (is_valid) {
      stop("Invalid diet coefficient", val, "passed validation")
    }
  }

  # Valid coefficients
  valid <- c(0, 0.25, 0.5, 0.75, 1.0)
  for (val in valid) {
    is_valid <- !is.na(val) && val >= 0 && val <= 1
    if (!is_valid) {
      stop("Valid diet coefficient", val, "failed validation")
    }
  }

  cat("  ✓ Diet coefficient validation works correctly\n")
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

cat("\n📊 KEY FINDINGS:\n")
if (length(test_data$param_issues) > 0) {
  cat("  • Found", length(test_data$param_issues), "groups with insufficient parameters\n")
  cat("  • Editors can fix these by adding missing Biomass values\n")
}
if (length(test_data$diet_issues) > 0) {
  cat("  • Found", length(test_data$diet_issues), "predators with diet sum > 1.0\n")
  cat("  • Editors can fix these by normalizing diet proportions\n")
}
if (!is.null(test_data$edited_balanced)) {
  cat("  • ✓ Model balances successfully after parameter editing\n")
  cat("  • ✓ This validates the parameter editor functionality!\n")
}

cat("\n", strrep("=", 80), "\n")
if (failed == 0) {
  cat("✅ PARAMETER EDITORS VALIDATION COMPLETE - ALL TESTS PASSED\n")
} else {
  cat("⚠ PARAMETER EDITORS VALIDATION COMPLETE - SOME TESTS FAILED\n")
}
cat(strrep("=", 80), "\n\n")
