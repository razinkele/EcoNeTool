#!/usr/bin/env Rscript
# =============================================================================
# Fixture Capture Script
# =============================================================================
# Run this ONCE with internet access to capture real API responses as RDS files.
# These fixtures are then used by unit tests for fast, offline testing.
#
# Usage:
#   Rscript tests/testthat/capture_fixtures.R
#
# Run from the EcoNeTool project root directory.
# =============================================================================

cat("=== Fixture Capture Script ===\n\n")

# Ensure we're in the right directory
if (!file.exists("R/functions/ecobase_connection.R")) {
  stop("Please run from the EcoNeTool root directory")
}

# Source dependencies
source("R/functions/validation_utils.R")
source("R/config/harmonization_config.R")
source("R/functions/ecobase_connection.R")
source("R/functions/taxonomic_api_utils.R")
source("R/functions/trait_lookup/database_lookups.R")
source("R/functions/trait_lookup/harmonization.R")
source("R/functions/trait_lookup/orchestrator.R")
source("R/functions/local_trait_databases.R")
source("R/functions/api_rate_limiter.R")

# Functional group utils for assign_functional_groups
tryCatch(
  source("R/functions/functional_group_utils.R"),
  error = function(e) message("Note: functional_group_utils.R: ", e$message)
)

fixture_dir <- "tests/testthat/fixtures"
dir.create(fixture_dir, showWarnings = FALSE, recursive = TRUE)

save_fix <- function(obj, name) {
  path <- file.path(fixture_dir, paste0(name, ".rds"))
  saveRDS(obj, path)
  cat("  Saved:", path, "\n")
}

# =========================================================================
# 1. EcoBase Fixtures
# =========================================================================
cat("\n[1/5] Capturing EcoBase fixtures...\n")

tryCatch({
  models <- get_ecobase_models()
  save_fix(models, "ecobase_models")

  # Capture a small model (403 = North Sea, commonly used in tests)
  input_403 <- get_ecobase_model_input(403)
  save_fix(input_403, "ecobase_model_403_input")

  output_403 <- get_ecobase_model_output(403)
  save_fix(output_403, "ecobase_model_403_output")

  metadata_403 <- get_ecobase_model_metadata(403)
  save_fix(metadata_403, "ecobase_model_403_metadata")

  # Full conversion
  converted_403 <- convert_ecobase_to_econetool(403)
  save_fix(converted_403, "ecobase_model_403_converted")

  cat("  EcoBase fixtures captured successfully\n")
}, error = function(e) {
  cat("  FAILED:", e$message, "\n")
  cat("  EcoBase may be offline. Run again later.\n")
})

# =========================================================================
# 2. WoRMS Fixtures
# =========================================================================
cat("\n[2/5] Capturing WoRMS fixtures...\n")

test_species_worms <- c(
  "Gadus morhua",      # Atlantic cod (fish)
  "Clupea harengus",   # Herring (fish)
  "Mytilus edulis",     # Blue mussel (invertebrate)
  "Carcinus maenas"    # Shore crab (invertebrate)
)

for (sp in test_species_worms) {
  tryCatch({
    result <- lookup_worms_traits(sp)
    save_fix(result, paste0("worms_", gsub(" ", "_", tolower(sp))))
    cat("  ", sp, "-> success:", result$success, "\n")
  }, error = function(e) {
    cat("  ", sp, "-> FAILED:", e$message, "\n")
  })
  Sys.sleep(1)  # Rate limiting
}

# =========================================================================
# 3. FishBase Fixtures
# =========================================================================
cat("\n[3/5] Capturing FishBase fixtures...\n")

test_species_fishbase <- c("Gadus morhua", "Clupea harengus")

for (sp in test_species_fishbase) {
  tryCatch({
    result <- lookup_fishbase_traits(sp)
    save_fix(result, paste0("fishbase_", gsub(" ", "_", tolower(sp))))
    cat("  ", sp, "-> success:", result$success, "\n")
  }, error = function(e) {
    cat("  ", sp, "-> FAILED:", e$message, "\n")
  })
  Sys.sleep(1)
}

# =========================================================================
# 4. SeaLifeBase Fixtures
# =========================================================================
cat("\n[4/5] Capturing SeaLifeBase fixtures...\n")

test_species_slb <- c("Mytilus edulis", "Carcinus maenas")

for (sp in test_species_slb) {
  tryCatch({
    result <- lookup_sealifebase_traits(sp)
    save_fix(result, paste0("sealifebase_", gsub(" ", "_", tolower(sp))))
    cat("  ", sp, "-> success:", result$success, "\n")
  }, error = function(e) {
    cat("  ", sp, "-> FAILED:", e$message, "\n")
  })
  Sys.sleep(1)
}

# =========================================================================
# 5. Full Trait Lookup Pipeline Fixtures
# =========================================================================
cat("\n[5/5] Capturing full trait lookup fixtures...\n")

test_species_traits <- c(
  "Gadus morhua",
  "Mytilus edulis",
  "Clupea harengus"
)

for (sp in test_species_traits) {
  tryCatch({
    result <- lookup_species_traits(sp)
    save_fix(result, paste0("traits_", gsub(" ", "_", tolower(sp))))
    cat("  ", sp, "-> captured\n")
  }, error = function(e) {
    cat("  ", sp, "-> FAILED:", e$message, "\n")
  })
  Sys.sleep(2)
}

# =========================================================================
# Summary
# =========================================================================
cat("\n=== Fixture Capture Complete ===\n")
fixtures <- list.files(fixture_dir, pattern = "\\.rds$")
cat("Total fixtures:", length(fixtures), "\n")
for (f in fixtures) {
  size <- file.size(file.path(fixture_dir, f))
  cat(sprintf("  %-50s %s\n", f, format(size, big.mark = ",")))
}
cat("\nYou can now run unit tests offline.\n")
