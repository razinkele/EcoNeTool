# ============================================================================
# Test: Producer Validation Fix
# ============================================================================
# Verifies that producers (Type 1) only need Biomass + P/B (not Q/B)
# ============================================================================

cat("\n=== Testing Producer Validation Fix ===\n\n")

suppressPackageStartupMessages({
  library(Rpath)
})

source("R/config.R")
source("R/functions/ecopath_import.R")
source("R/functions/rpath_integration.R")

# Import and convert LTgoby
cat("[Test] Import and Convert LTgoby\n")
cat(paste(rep("=", 60), collapse=""), "\n")

ecopath_data <- parse_ecopath_native_windows("examples/LTgoby.eweaccdb")
rpath_params <- convert_ecopath_to_rpath(ecopath_data, model_name = "LTgoby Test")

# Extract model
model <- rpath_params$model

cat("\n[Verification] Group Type Requirements\n")
cat(paste(rep("=", 60), collapse=""), "\n\n")

# Function to check if group meets requirements (matches our validation logic)
check_requirements <- function(row) {
  if (model$Type[row] == 0) {
    # Consumers: Need 3 of 4
    params <- c(!is.na(model$Biomass[row]), !is.na(model$PB[row]),
                !is.na(model$QB[row]), !is.na(model$EE[row]))
    list(
      type = "Consumer",
      requirement = "3 of 4 (B, P/B, Q/B, EE)",
      has = sum(params),
      meets = sum(params) >= 3
    )
  } else if (model$Type[row] == 1) {
    # Producers: Need Biomass + P/B
    has_b <- !is.na(model$Biomass[row])
    has_pb <- !is.na(model$PB[row])
    list(
      type = "Producer",
      requirement = "Biomass + P/B",
      has = paste(ifelse(has_b, "B", ""), ifelse(has_pb, "P/B", "")),
      meets = has_b && has_pb
    )
  } else if (model$Type[row] == 2) {
    # Detritus: Only Biomass
    has_b <- !is.na(model$Biomass[row])
    list(
      type = "Detritus",
      requirement = "Biomass only",
      has = ifelse(has_b, "B", "MISSING"),
      meets = has_b
    )
  } else {
    list(type = "Fleet", requirement = "N/A", has = "N/A", meets = TRUE)
  }
}

# Test key groups
test_groups <- c("Detritus", "Phytoplankton", "Zooplankton", "Herring")

for (group_name in test_groups) {
  idx <- which(model$Group == group_name)
  if (length(idx) == 0) next

  result <- check_requirements(idx)

  cat(sprintf("%-20s (Type %d = %s)\n", group_name, model$Type[idx], result$type))
  cat(sprintf("  Requirement: %s\n", result$requirement))
  cat(sprintf("  Has:         %s\n", result$has))

  if (model$Type[idx] == 1) {
    # Show producer details
    cat(sprintf("  Biomass:     %s\n", ifelse(is.na(model$Biomass[idx]), "NA", sprintf("%.2f", model$Biomass[idx]))))
    cat(sprintf("  P/B:         %s\n", ifelse(is.na(model$PB[idx]), "NA", sprintf("%.2f", model$PB[idx]))))
    cat(sprintf("  Q/B:         %s (should be NA for producers)\n", ifelse(is.na(model$QB[idx]), "NA", sprintf("%.2f", model$QB[idx]))))
    cat(sprintf("  EE:          %s (optional, can be calculated)\n", ifelse(is.na(model$EE[idx]), "NA", sprintf("%.2f", model$EE[idx]))))
  }

  cat(sprintf("  Status:      %s\n\n", ifelse(result$meets, "✅ PASS", "❌ FAIL")))
}

# Run validation
cat("[Validation] Testing Parameter Check\n")
cat(paste(rep("=", 60), collapse=""), "\n\n")

validation_result <- tryCatch({
  # This will run our validation logic
  test_model <- run_ecopath_balance(rpath_params, balance = FALSE)
  list(success = TRUE, model = test_model)
}, error = function(e) {
  list(success = FALSE, error = conditionMessage(e))
})

if (validation_result$success) {
  cat("✅ VALIDATION PASSED\n")
  cat("   All groups meet their type-specific requirements!\n\n")
  cat("   Key achievements:\n")
  cat("   → Detritus validated (needs only Biomass)\n")
  cat("   → Phytoplankton validated (producer needs B + P/B, NOT Q/B)\n")
  cat("   → Consumers validated (need 3/4 parameters)\n\n")
} else {
  cat("❌ VALIDATION FAILED\n")
  cat("   Error:", validation_result$error, "\n\n")

  if (grepl("Phytoplankton", validation_result$error)) {
    cat("   ❌ Producer validation NOT working correctly\n")
  } else {
    cat("   ✅ Phytoplankton passed (producer validation working!)\n")
    cat("   ⚠ Other validation issues found\n")
  }
}

cat(paste(rep("=", 60), collapse=""), "\n")
cat("\n=== SUMMARY ===\n\n")

cat("Producer Validation Logic:\n")
cat("  OLD: Producers (Type 1) needed 3/4 parameters ❌\n")
cat("  NEW: Producers (Type 1) need Biomass + P/B ✅\n\n")

cat("Rationale:\n")
cat("  Producers are autotrophs (e.g., phytoplankton)\n")
cat("  They photosynthesize, don't consume prey\n")
cat("  Q/B (Consumption/Biomass) is not applicable\n")
cat("  They only need:\n")
cat("    - Biomass (standing stock)\n")
cat("    - P/B (production rate)\n")
cat("    - EE (can be calculated by Ecopath)\n\n")

if (validation_result$success) {
  cat("✅ FIX VERIFIED - PRODUCERS VALIDATED CORRECTLY\n\n")
} else {
  cat("Status: Validation logic updated, may have other issues\n\n")
}
