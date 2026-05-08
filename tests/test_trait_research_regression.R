# =============================================================================
# Trait Research Regression Tests
# =============================================================================
#
# Covers four bugs found and fixed in the trait research pipeline. Each test
# is a minimal reproduction of the original failure mode plus an assertion
# that the fix is in place.
#
# Run with:  Rscript tests/test_trait_research_regression.R
# =============================================================================

cat("=============================================================================\n")
cat("TRAIT RESEARCH REGRESSION TESTS\n")
cat("=============================================================================\n\n")

# Load only the dependencies the assertions below actually exercise. The
# tests cover code inspection (no R loading needed) plus three functions:
# bind_rows (dplyr), predict_trait_ml (ml_trait_prediction.R, which uses
# randomForest), and find_closest_relatives + impute_traits_from_relatives
# (phylogenetic_imputation.R). Sourcing the full trait_lookup/load_all.R
# chain pulls in readxl, R6, etc. that the regression tests never use,
# which would needlessly bloat CI deps.
suppressPackageStartupMessages({
  library(dplyr)
})
source("R/functions/validation_utils.R")
source("R/functions/phylogenetic_imputation.R")
source("R/functions/ml_trait_prediction.R")

test_count <- 0; pass_count <- 0; fail_count <- 0

record_test <- function(name, status, message = "") {
  test_count <<- test_count + 1
  if (status == "PASS") { pass_count <<- pass_count + 1; sym <- "✓" }
  else                  { fail_count <<- fail_count + 1; sym <- "✗" }
  cat(sprintf("  %s [%s] %s%s\n", sym, status, name,
              if (nzchar(message)) paste0(" — ", message) else ""))
}

# =============================================================================
# Bug 1: Run handler must read the canonical species list, not the textarea
# =============================================================================
# Pre-fix: the Run observer parsed input$trait_research_species_list directly,
# so test mode (where the textarea is hidden) processed the 5 default fish
# instead of the selected dataset. The fix routes Run through get_species_list().
cat("\nBug 1: Run handler reads get_species_list()\n")

server_src <- paste(readLines("R/modules/trait_research_server.R"), collapse = "\n")
run_block  <- regmatches(server_src,
                         regexpr("observeEvent\\(input\\$trait_research_run_lookup,\\s*\\{[^}]+",
                                 server_src))
record_test("Run observer body calls get_species_list()",
            if (grepl("get_species_list\\(\\)", run_block)) "PASS" else "FAIL",
            "regression: Run was previously parsing the textarea directly")

# =============================================================================
# Bug 2: phylo_sources must not crash on NA_character_ source columns
# =============================================================================
# Pre-fix orchestrator.R built phylo_sources via
#   if (!is.null(x) && x == "Phylogenetic")
# which evaluates to NA when x is NA_character_, then `if (NA)` throws "missing
# value where TRUE/FALSE needed", silently aborting every bird/mammal lookup
# (their *_source columns are NA after phylo finds no relatives).
cat("\nBug 2: NA-safe phylo_sources construction\n")

result <- list(
  MS_source = NA_character_, FS_source = NA_character_,
  MB_source = NA_character_, EP_source = NA_character_,
  PR_source = NA_character_
)
err <- tryCatch({
  phylo_sources <- c(
    if (isTRUE(result$MS_source == "Phylogenetic")) "Phylogenetic",
    if (isTRUE(result$FS_source == "Phylogenetic")) "Phylogenetic",
    if (isTRUE(result$MB_source == "Phylogenetic")) "Phylogenetic",
    if (isTRUE(result$EP_source == "Phylogenetic")) "Phylogenetic",
    if (isTRUE(result$PR_source == "Phylogenetic")) "Phylogenetic"
  )
  NULL
}, error = function(e) conditionMessage(e))
record_test("isTRUE() pattern handles NA_character_",
            if (is.null(err)) "PASS" else "FAIL",
            err %||% "")

# Mirror in the deployed orchestrator.R
orch_src <- paste(readLines("R/functions/trait_lookup/orchestrator.R"), collapse = "\n")
record_test("orchestrator uses isTRUE() guard",
            if (grepl("isTRUE\\(result\\$MS_source == \"Phylogenetic\"\\)", orch_src))
              "PASS" else "FAIL",
            "regression: would re-introduce 'missing value where TRUE/FALSE needed'")

# =============================================================================
# Bug 3: rbind on heterogeneous result schemas must succeed
# =============================================================================
# Per-species result data.frames have different column sets — fish (5 traits)
# get all 15 *_interval_lower/upper/category cols, birds with only MS+PR get
# 6. do.call(rbind, …) requires identical columns; bind_rows fills with NA.
cat("\nBug 3: bind_rows merges heterogeneous result frames\n")

fish <- data.frame(species = "Gadus morhua", MS = "MS5", FS = "FS1",
                   MS_interval_lower = 0.5, FS_interval_lower = 0.6,
                   stringsAsFactors = FALSE)
bird <- data.frame(species = "Somateria mollissima", MS = "MS4", FS = NA_character_,
                   stringsAsFactors = FALSE)

old_err <- tryCatch({ do.call(rbind, list(fish, bird)); NULL },
                   error = function(e) conditionMessage(e))
record_test("do.call(rbind) fails on heterogeneous frames (precondition)",
            if (!is.null(old_err) && grepl("columns", old_err)) "PASS" else "FAIL",
            old_err %||% "rbind unexpectedly succeeded")

merged <- as.data.frame(dplyr::bind_rows(list(fish, bird)))
record_test("bind_rows produces 2-row × union-cols frame",
            if (nrow(merged) == 2 && ncol(merged) == ncol(fish)) "PASS" else "FAIL",
            sprintf("got %dr × %dc", nrow(merged), ncol(merged)))
record_test("bind_rows fills missing columns with NA",
            if (is.na(merged$FS_interval_lower[2])) "PASS" else "FAIL")

server_uses_bind_rows <- grepl("dplyr::bind_rows\\(results_list\\)", server_src)
record_test("trait_research_server.R uses dplyr::bind_rows",
            if (server_uses_bind_rows) "PASS" else "FAIL",
            "regression: would re-introduce do.call(rbind) heterogeneity bug")

# =============================================================================
# Bug 4: ML predict() must dispatch on randomForest objects
# =============================================================================
# Pre-fix load_ml_models() did readRDS without loading randomForest's
# namespace, so predict() on the deserialised model (class
# c("randomForest.formula", "randomForest")) hit "no applicable method for
# 'predict'" and every ML imputation silently failed.
cat("\nBug 4: ML predict() dispatches via randomForest namespace\n")

if (!file.exists("models/trait_ml_models.rds")) {
  record_test("ML model file present", "FAIL",
              "models/trait_ml_models.rds missing — re-run scripts/train_trait_models.R")
} else {
  models_pkg <- load_ml_models()
  record_test("load_ml_models() returns a model package",
              if (!is.null(models_pkg) && !is.null(models_pkg$models)) "PASS" else "FAIL")

  # Use a taxon known to be in the trained xlevels — predict should fire.
  prediction <- predict_trait_ml(
    "MS",
    list(phylum = "Chordata", class = "Aves", order = "Anseriformes"),
    models_package = models_pkg
  )
  record_test("predict_trait_ml() returns a non-NULL result for in-distribution taxon",
              if (!is.null(prediction) && !is.null(prediction$value)) "PASS" else "FAIL",
              if (!is.null(prediction)) sprintf("got %s with conf=%s",
                                                prediction$value, prediction$confidence)
              else "predict_trait_ml returned NULL")
}

# =============================================================================
# Bug 5: phylogenetic imputation accepts partial-coverage relatives
# =============================================================================
# Pre-fix find_closest_relatives required `all()` of the missing traits to be
# present on a candidate relative; a cached species with only MS+PR was
# discarded even though it could impute MS+PR alone. Fixed to `any()`.
cat("\nBug 5: phylo accepts relatives with partial trait coverage\n")

tmp <- tempfile("phylo_test_", fileext = "")
dir.create(tmp); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

# Build a tiny cache: one Anatidae species with ONLY MS (FS, MB, EP, PR all NA)
saveRDS(list(
  species   = "Anas platyrhynchos",
  traits    = data.frame(species = "Anas platyrhynchos",
                         MS = "MS4", FS = NA, MB = NA, EP = NA, PR = NA,
                         stringsAsFactors = FALSE),
  harmonized = list(species = "Anas platyrhynchos",
                    phylum = "Chordata", class = "Aves", order = "Anseriformes",
                    family = "Anatidae", genus = "Anas",
                    MS = "MS4", FS = NA, MB = NA, EP = NA, PR = NA),
  timestamp = Sys.time()
), file.path(tmp, "Anas_platyrhynchos.rds"))

target_tax <- list(phylum = "Chordata", class = "Aves",
                   order = "Anseriformes", family = "Anatidae", genus = "Branta")
target_tr  <- list(MS = NA, FS = NA, MB = NA, EP = NA, PR = NA)

rels <- find_closest_relatives(target_taxonomy = target_tax,
                               target_traits   = target_tr,
                               cache_dir       = tmp,
                               max_distance    = 3)
record_test("find_closest_relatives returns a partial-coverage relative",
            if (nrow(rels) == 1 && rels$species[1] == "Anas platyrhynchos") "PASS" else "FAIL",
            sprintf("got %d relatives", nrow(rels)))

imp <- impute_traits_from_relatives(target_tr, rels, min_agreement = 0.6)
record_test("imputation fills MS even when relative lacks FS/MB/EP/PR",
            if (length(imp) == 1 && !is.null(imp$MS) && imp$MS$value == "MS4") "PASS" else "FAIL",
            paste0("imputed: ", paste(names(imp), collapse = ", ")))

# =============================================================================
# Summary
# =============================================================================
cat("\n=============================================================================\n")
cat(sprintf("Total: %d   Passed: %d   Failed: %d\n", test_count, pass_count, fail_count))
cat("=============================================================================\n")
if (fail_count > 0) quit(status = 1)
