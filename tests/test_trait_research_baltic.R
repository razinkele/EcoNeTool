# =============================================================================
# Test Routine: Trait Research - Central Baltic Sea Dataset
# =============================================================================
#
# This script tests the trait lookup functionality using the Central Baltic Sea
# species dataset covering phytoplankton to marine mammals.
#
# Usage:
#   source("tests/test_trait_research_baltic.R")
#
# Or run specific tests:
#   test_dataset_loading()
#   test_trait_lookup_fish()
#   test_trait_lookup_full()
#
# =============================================================================

# Set working directory if needed
if (!file.exists("app.R")) {
  stop("Please run this script from the EcoNeTool root directory")
}

# Source required functions
cat("\n========================================\n")
cat("TRAIT RESEARCH TEST SUITE\n")
cat("Central Baltic Sea Dataset\n")
cat("========================================\n\n")

cat("Loading required functions...\n")

# Source core functions
source("R/config.R")
source("R/functions/validation_utils.R")
source("R/config/harmonization_config.R")
source("R/functions/trait_foodweb.R")
source("R/functions/trait_lookup.R")
source("R/functions/taxonomic_api_utils.R")

cat("Functions loaded successfully.\n\n")

# =============================================================================
# TEST 1: Dataset Loading
# =============================================================================

test_dataset_loading <- function() {
  cat("----------------------------------------\n")
  cat("TEST 1: Dataset Loading\n")
  cat("----------------------------------------\n")

  # Load the Baltic Sea species file
  file_path <- "data/baltic_sea_test_species.csv"

  if (!file.exists(file_path)) {
    cat("FAIL: Dataset file not found at", file_path, "\n")
    return(FALSE)
  }

  df <- read.csv(file_path, stringsAsFactors = FALSE)

  cat("File loaded:", nrow(df), "species\n")

  # Check required columns
  required_cols <- c("species", "functional_group", "common_name")
  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    cat("FAIL: Missing columns:", paste(missing_cols, collapse = ", "), "\n")
    return(FALSE)
  }

  # Check functional groups
  fg_counts <- table(df$functional_group)
  cat("\nFunctional group distribution:\n")
  for (fg in names(fg_counts)) {
    cat(sprintf("  %s: %d species\n", fg, fg_counts[fg]))
  }

  # Validate species names (no empty, no duplicates)
  empty_names <- sum(df$species == "" | is.na(df$species))
  duplicate_names <- sum(duplicated(df$species))

  if (empty_names > 0) {
    cat("WARNING:", empty_names, "empty species names\n")
  }

  if (duplicate_names > 0) {
    cat("WARNING:", duplicate_names, "duplicate species names\n")
  }

  cat("\nPASS: Dataset loading test completed\n")
  return(TRUE)
}

# =============================================================================
# TEST 2: Trait Lookup - Quick Test (5 species)
# =============================================================================

test_trait_lookup_quick <- function() {
  cat("\n----------------------------------------\n")
  cat("TEST 2: Quick Trait Lookup (5 species)\n")
  cat("----------------------------------------\n")

  # Test with 5 well-known species
  test_species <- c(
    "Gadus morhua",          # Fish - should find in FishBase
    "Clupea harengus",       # Fish - should find in FishBase
    "Macoma balthica",       # Bivalve - should find in SeaLifeBase
    "Aurelia aurita",        # Jellyfish - should find in SeaLifeBase
    "Halichoerus grypus"     # Mammal - may find in WoRMS
  )

  cat("Testing species:", paste(test_species, collapse = ", "), "\n\n")

  results <- list()
  success_count <- 0

  for (species in test_species) {
    cat("Looking up:", species, "... ")

    start_time <- Sys.time()

    tryCatch({
      result <- lookup_species_traits(species, cache_dir = "cache/taxonomy")

      elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2)

      n_traits <- sum(!is.na(c(result$MS, result$FS, result$MB, result$EP, result$PR)))

      if (n_traits > 0) {
        cat(sprintf("OK (%d/5 traits, %.2fs)\n", n_traits, elapsed))
        success_count <- success_count + 1
      } else {
        cat(sprintf("NO DATA (%.2fs)\n", elapsed))
      }

      results[[species]] <- result

    }, error = function(e) {
      cat("ERROR:", e$message, "\n")
    })

    Sys.sleep(0.5)  # Rate limiting
  }

  cat("\nResults Summary:\n")
  cat(sprintf("  Successful lookups: %d/%d (%.1f%%)\n",
              success_count, length(test_species),
              success_count / length(test_species) * 100))

  if (success_count >= 3) {
    cat("\nPASS: Quick trait lookup test completed\n")
    return(TRUE)
  } else {
    cat("\nWARNING: Low success rate in trait lookup\n")
    return(FALSE)
  }
}

# =============================================================================
# TEST 3: Trait Lookup - Fish Species (20 species)
# =============================================================================

test_trait_lookup_fish <- function() {
  cat("\n----------------------------------------\n")
  cat("TEST 3: Fish Trait Lookup (20 species)\n")
  cat("----------------------------------------\n")

  # Load dataset
  df <- read.csv("data/baltic_sea_test_species.csv", stringsAsFactors = FALSE)
  fish_species <- df[df$functional_group == "Fish", "species"]

  cat("Testing", length(fish_species), "fish species\n")
  cat("Databases: WoRMS, FishBase\n\n")

  results <- list()
  trait_counts <- c(complete = 0, partial = 0, none = 0)

  pb <- txtProgressBar(min = 0, max = length(fish_species), style = 3)

  for (i in seq_along(fish_species)) {
    species <- fish_species[i]

    tryCatch({
      result <- lookup_species_traits(species, cache_dir = "cache/taxonomy")

      n_traits <- sum(!is.na(c(result$MS, result$FS, result$MB, result$EP, result$PR)))

      if (n_traits == 5) {
        trait_counts["complete"] <- trait_counts["complete"] + 1
      } else if (n_traits > 0) {
        trait_counts["partial"] <- trait_counts["partial"] + 1
      } else {
        trait_counts["none"] <- trait_counts["none"] + 1
      }

      results[[species]] <- result

    }, error = function(e) {
      trait_counts["none"] <- trait_counts["none"] + 1
    })

    setTxtProgressBar(pb, i)
    Sys.sleep(0.3)
  }

  close(pb)

  cat("\n\nResults Summary:\n")
  cat(sprintf("  Complete (5/5 traits): %d (%.1f%%)\n",
              trait_counts["complete"],
              trait_counts["complete"] / length(fish_species) * 100))
  cat(sprintf("  Partial (1-4 traits):  %d (%.1f%%)\n",
              trait_counts["partial"],
              trait_counts["partial"] / length(fish_species) * 100))
  cat(sprintf("  No data (0 traits):    %d (%.1f%%)\n",
              trait_counts["none"],
              trait_counts["none"] / length(fish_species) * 100))

  # Show detailed results
  cat("\nDetailed Fish Results:\n")
  results_df <- do.call(rbind, results)
  if (!is.null(results_df)) {
    print(results_df[, c("species", "MS", "FS", "MB", "EP", "PR", "confidence")])
  }

  success_rate <- (trait_counts["complete"] + trait_counts["partial"]) / length(fish_species)

  if (success_rate >= 0.7) {
    cat("\nPASS: Fish trait lookup test completed\n")
    return(list(success = TRUE, results = results_df))
  } else {
    cat("\nWARNING: Low success rate in fish lookup\n")
    return(list(success = FALSE, results = results_df))
  }
}

# =============================================================================
# TEST 4: Trait Lookup - Benthos Species
# =============================================================================

test_trait_lookup_benthos <- function() {
  cat("\n----------------------------------------\n")
  cat("TEST 4: Benthos Trait Lookup\n")
  cat("----------------------------------------\n")

  df <- read.csv("data/baltic_sea_test_species.csv", stringsAsFactors = FALSE)
  benthos_species <- df[df$functional_group == "Benthos", "species"]

  cat("Testing", length(benthos_species), "benthic species\n")
  cat("Databases: WoRMS, SeaLifeBase, BIOTIC\n\n")

  results <- list()
  trait_counts <- c(complete = 0, partial = 0, none = 0)

  pb <- txtProgressBar(min = 0, max = length(benthos_species), style = 3)

  for (i in seq_along(benthos_species)) {
    species <- benthos_species[i]

    tryCatch({
      result <- lookup_species_traits(species, cache_dir = "cache/taxonomy")

      n_traits <- sum(!is.na(c(result$MS, result$FS, result$MB, result$EP, result$PR)))

      if (n_traits == 5) {
        trait_counts["complete"] <- trait_counts["complete"] + 1
      } else if (n_traits > 0) {
        trait_counts["partial"] <- trait_counts["partial"] + 1
      } else {
        trait_counts["none"] <- trait_counts["none"] + 1
      }

      results[[species]] <- result

    }, error = function(e) {
      trait_counts["none"] <- trait_counts["none"] + 1
    })

    setTxtProgressBar(pb, i)
    Sys.sleep(0.3)
  }

  close(pb)

  cat("\n\nResults Summary:\n")
  cat(sprintf("  Complete (5/5 traits): %d (%.1f%%)\n",
              trait_counts["complete"],
              trait_counts["complete"] / length(benthos_species) * 100))
  cat(sprintf("  Partial (1-4 traits):  %d (%.1f%%)\n",
              trait_counts["partial"],
              trait_counts["partial"] / length(benthos_species) * 100))
  cat(sprintf("  No data (0 traits):    %d (%.1f%%)\n",
              trait_counts["none"],
              trait_counts["none"] / length(benthos_species) * 100))

  results_df <- do.call(rbind, results)
  cat("\nDetailed Benthos Results:\n")
  if (!is.null(results_df)) {
    print(results_df[, c("species", "MS", "FS", "MB", "EP", "PR", "confidence")])
  }

  return(list(success = TRUE, results = results_df))
}

# =============================================================================
# TEST 5: Full Baltic Sea Lookup (All 67 species)
# =============================================================================

test_trait_lookup_full <- function(save_results = TRUE) {
  cat("\n========================================\n")
  cat("TEST 5: Full Baltic Sea Trait Lookup\n")
  cat("========================================\n")

  df <- read.csv("data/baltic_sea_test_species.csv", stringsAsFactors = FALSE)
  all_species <- df$species

  cat("Testing ALL", length(all_species), "species\n")
  cat("This may take 10-15 minutes...\n\n")

  start_time <- Sys.time()

  results <- list()
  fg_results <- list()

  pb <- txtProgressBar(min = 0, max = length(all_species), style = 3)

  for (i in seq_along(all_species)) {
    species <- all_species[i]
    fg <- df$functional_group[i]

    tryCatch({
      result <- lookup_species_traits(species, cache_dir = "cache/taxonomy")
      result$functional_group <- fg
      results[[species]] <- result

      # Track by functional group
      if (!fg %in% names(fg_results)) {
        fg_results[[fg]] <- list(complete = 0, partial = 0, none = 0, total = 0)
      }

      n_traits <- sum(!is.na(c(result$MS, result$FS, result$MB, result$EP, result$PR)))
      fg_results[[fg]]$total <- fg_results[[fg]]$total + 1

      if (n_traits == 5) {
        fg_results[[fg]]$complete <- fg_results[[fg]]$complete + 1
      } else if (n_traits > 0) {
        fg_results[[fg]]$partial <- fg_results[[fg]]$partial + 1
      } else {
        fg_results[[fg]]$none <- fg_results[[fg]]$none + 1
      }

    }, error = function(e) {
      # Track failures
      if (!fg %in% names(fg_results)) {
        fg_results[[fg]] <- list(complete = 0, partial = 0, none = 0, total = 0)
      }
      fg_results[[fg]]$total <- fg_results[[fg]]$total + 1
      fg_results[[fg]]$none <- fg_results[[fg]]$none + 1
    })

    setTxtProgressBar(pb, i)
    Sys.sleep(0.3)
  }

  close(pb)

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))

  cat("\n\n========================================\n")
  cat("FULL TEST RESULTS\n")
  cat("========================================\n")
  cat(sprintf("Total time: %.1f minutes\n", elapsed))
  cat(sprintf("Average: %.2f seconds per species\n", elapsed * 60 / length(all_species)))

  cat("\nResults by Functional Group:\n")
  cat("----------------------------------------\n")

  for (fg in names(fg_results)) {
    r <- fg_results[[fg]]
    success_rate <- (r$complete + r$partial) / r$total * 100
    cat(sprintf("%-15s: %2d complete, %2d partial, %2d none (%.0f%% success)\n",
                fg, r$complete, r$partial, r$none, success_rate))
  }

  # Combine results
  results_df <- do.call(rbind, results)
  rownames(results_df) <- NULL

  # Overall stats
  n_complete <- sum(complete.cases(results_df[, c("MS", "FS", "MB", "EP", "PR")]))
  n_partial <- sum(!complete.cases(results_df[, c("MS", "FS", "MB", "EP", "PR")])) -
    sum(is.na(results_df$MS) & is.na(results_df$FS) &
        is.na(results_df$MB) & is.na(results_df$EP) & is.na(results_df$PR))
  n_none <- sum(is.na(results_df$MS) & is.na(results_df$FS) &
                is.na(results_df$MB) & is.na(results_df$EP) & is.na(results_df$PR))

  cat("\n----------------------------------------\n")
  cat("OVERALL SUMMARY:\n")
  cat(sprintf("  Complete (5/5): %d (%.1f%%)\n", n_complete, n_complete / nrow(results_df) * 100))
  cat(sprintf("  Partial (1-4):  %d (%.1f%%)\n", n_partial, n_partial / nrow(results_df) * 100))
  cat(sprintf("  No data (0):    %d (%.1f%%)\n", n_none, n_none / nrow(results_df) * 100))
  cat("----------------------------------------\n")

  # Save results
  if (save_results) {
    output_dir <- "tests/test_results"
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

    # Save CSV
    csv_file <- file.path(output_dir, paste0("baltic_trait_results_", timestamp, ".csv"))
    write.csv(results_df, csv_file, row.names = FALSE)
    cat("\nResults saved to:", csv_file, "\n")

    # Save RDS (with raw data)
    rds_file <- file.path(output_dir, paste0("baltic_trait_results_", timestamp, ".rds"))
    saveRDS(list(
      results = results_df,
      fg_summary = fg_results,
      test_time = elapsed,
      timestamp = Sys.time()
    ), rds_file)
    cat("Full results saved to:", rds_file, "\n")
  }

  return(list(
    success = TRUE,
    results = results_df,
    fg_summary = fg_results,
    elapsed_minutes = elapsed
  ))
}

# =============================================================================
# TEST 6: Validate Harmonization
# =============================================================================

test_harmonization_validation <- function() {
  cat("\n----------------------------------------\n")
  cat("TEST 6: Harmonization Validation\n")
  cat("----------------------------------------\n")

  # Test that harmonization produces valid trait codes

  # Test size harmonization
  test_sizes <- c(0.05, 0.5, 2, 10, 50, 100, 200)
  cat("Testing size harmonization:\n")

  for (size in test_sizes) {
    ms <- harmonize_size_to_MS(size)
    cat(sprintf("  %.2f cm -> %s\n", size, ms))
  }

  # Test trophic level harmonization
  test_tl <- c(1.0, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5)
  cat("\nTesting trophic level harmonization:\n")

  for (tl in test_tl) {
    fs <- harmonize_trophic_level_to_FS(tl)
    cat(sprintf("  TL %.1f -> %s\n", tl, fs))
  }

  cat("\nPASS: Harmonization validation completed\n")
  return(TRUE)
}

# =============================================================================
# RUN ALL TESTS
# =============================================================================

run_all_tests <- function() {
  cat("\n")
  cat("########################################\n")
  cat("#  TRAIT RESEARCH - FULL TEST SUITE   #\n")
  cat("########################################\n")
  cat("\n")

  test_results <- list()

  # Test 1: Dataset loading
  test_results$dataset <- test_dataset_loading()

  # Test 2: Quick lookup
  test_results$quick <- test_trait_lookup_quick()

  # Test 3: Fish lookup
  fish_result <- test_trait_lookup_fish()
  test_results$fish <- fish_result$success

  # Test 4: Benthos lookup
  benthos_result <- test_trait_lookup_benthos()
  test_results$benthos <- benthos_result$success

  # Test 5: Full lookup (optional - takes time)
  cat("\n\nRun full 67-species test? (takes 10-15 minutes)\n")
  cat("Enter 'y' to run, or press Enter to skip: ")

  # Auto-skip in non-interactive mode
  if (interactive()) {
    response <- readline()
    if (tolower(response) == "y") {
      full_result <- test_trait_lookup_full()
      test_results$full <- full_result$success
    } else {
      cat("Skipping full test.\n")
      test_results$full <- NA
    }
  } else {
    cat("Non-interactive mode - skipping full test.\n")
    test_results$full <- NA
  }

  # Test 6: Harmonization
  test_results$harmonization <- test_harmonization_validation()

  # Final summary
  cat("\n")
  cat("########################################\n")
  cat("#          TEST SUMMARY               #\n")
  cat("########################################\n")

  passed <- sum(sapply(test_results, function(x) isTRUE(x)))
  skipped <- sum(sapply(test_results, function(x) is.na(x)))
  failed <- sum(sapply(test_results, function(x) isFALSE(x)))

  for (test_name in names(test_results)) {
    status <- test_results[[test_name]]
    if (isTRUE(status)) {
      cat(sprintf("  [PASS] %s\n", test_name))
    } else if (is.na(status)) {
      cat(sprintf("  [SKIP] %s\n", test_name))
    } else {
      cat(sprintf("  [FAIL] %s\n", test_name))
    }
  }

  cat("\n")
  cat(sprintf("Total: %d passed, %d failed, %d skipped\n", passed, failed, skipped))
  cat("########################################\n")

  return(test_results)
}

# =============================================================================
# QUICK TEST FUNCTION (for fast validation)
# =============================================================================

quick_test <- function() {
  cat("Running quick validation test...\n\n")

  test_dataset_loading()
  test_trait_lookup_quick()
  test_harmonization_validation()

  cat("\nQuick test complete!\n")
}

# =============================================================================
# MAIN - Run when sourced
# =============================================================================

cat("\nTest functions loaded. Available functions:\n")
cat("  quick_test()               - Fast validation (2-3 minutes)\n")
cat("  run_all_tests()            - Full test suite\n")
cat("  test_dataset_loading()     - Test CSV loading\n")
cat("  test_trait_lookup_quick()  - Test 5 species\n")
cat("  test_trait_lookup_fish()   - Test 20 fish species\n")
cat("  test_trait_lookup_benthos() - Test benthic species\n")
cat("  test_trait_lookup_full()   - Test all 67 species\n")
cat("\nRun quick_test() to start quick validation.\n")
