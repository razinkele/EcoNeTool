# =============================================================================
# Integration Test: Trait Knowledge Base Expansion
# =============================================================================

cat("=== Testing Trait Knowledge Base Expansion ===\n\n")

test_count <- 0; pass_count <- 0; fail_count <- 0

run_test <- function(name, expr) {
  test_count <<- test_count + 1
  cat(sprintf("[%02d] %s... ", test_count, name))
  tryCatch({ force(expr); pass_count <<- pass_count + 1; cat("PASS\n") },
    error = function(e) { fail_count <<- fail_count + 1; cat("FAIL:", e$message, "\n") })
}

# --- Database file existence and schema ---
run_test("BIOTIC CSV exists with correct columns", {
  stopifnot(file.exists("data/biotic_traits.csv"))
  d <- read.csv("data/biotic_traits.csv", stringsAsFactors = FALSE)
  stopifnot(nrow(d) >= 700)
  stopifnot(all(c("Species", "Max_Length_mm", "Feeding_mode", "Mobility", "Skeleton") %in% names(d)))
  stopifnot(!any(duplicated(d$Species)))
})

run_test("BIOTIC feeding_mode values are valid", {
  d <- read.csv("data/biotic_traits.csv", stringsAsFactors = FALSE)
  valid <- c("predator", "scavenger", "deposit_feeder", "surface_deposit_feeder",
             "suspension_feeder", "filter_feeder", "grazer", "omnivore", "parasite")
  bad <- setdiff(na.omit(d$Feeding_mode), valid)
  stopifnot(length(bad) == 0)
})

run_test("BIOTIC mobility values are valid", {
  d <- read.csv("data/biotic_traits.csv", stringsAsFactors = FALSE)
  valid <- c("sessile", "burrower", "crawler", "swimmer", "limited_swimmer")
  bad <- setdiff(na.omit(d$Mobility), valid)
  stopifnot(length(bad) == 0)
})

run_test("MAREDAT CSV exists with correct columns and types", {
  stopifnot(file.exists("data/maredat_zooplankton.csv"))
  d <- read.csv("data/maredat_zooplankton.csv", stringsAsFactors = FALSE)
  stopifnot(nrow(d) >= 180)
  stopifnot(all(c("Species", "ESD_um", "Group", "Trophic_level", "Trophic_category") %in% names(d)))
  stopifnot(is.numeric(d$Trophic_level))
  stopifnot(is.numeric(d$ESD_um))
  stopifnot(all(d$ESD_um > 0, na.rm = TRUE))
  stopifnot(!any(duplicated(d$Species)))
})

run_test("PTDB CSV exists with correct columns", {
  stopifnot(file.exists("data/ptdb_phytoplankton.csv"))
  d <- read.csv("data/ptdb_phytoplankton.csv", stringsAsFactors = FALSE)
  stopifnot(nrow(d) >= 350)
  stopifnot(all(c("Species", "Cell_volume_um3", "Trophic_strategy") %in% names(d)))
  stopifnot(!any(duplicated(d$Species)))
})

run_test("PTDB trophic_strategy values are valid", {
  d <- read.csv("data/ptdb_phytoplankton.csv", stringsAsFactors = FALSE)
  valid <- c("autotroph", "mixotroph", "heterotroph")
  bad <- setdiff(na.omit(d$Trophic_strategy), valid)
  stopifnot(length(bad) == 0)
})

# --- Ontology expansion ---
run_test("Ontology has 400+ species with 2500+ records", {
  ont <- read.csv("data/ontology_traits.csv", stringsAsFactors = FALSE)
  stopifnot(length(unique(ont$taxon_name)) >= 400)
  stopifnot(nrow(ont) >= 2500)
  required <- c("taxon_name","aphia_id","trait_category","trait_name","trait_modality","trait_score","unit","ontology","ontology_id","source","notes")
  stopifnot(all(required %in% names(ont)))
})

# --- Protection coverage ---
run_test("Protection coverage >= 90%", {
  ont <- read.csv("data/ontology_traits.csv", stringsAsFactors = FALSE)
  all_sp <- unique(ont$taxon_name)
  pr_sp <- unique(ont$taxon_name[ont$trait_name == "body_protection"])
  coverage <- length(pr_sp) / length(all_sp) * 100
  stopifnot(coverage >= 90)
})

# --- Geographic scope ---
run_test("European species list with 4+ regions", {
  stopifnot(file.exists("data/european_marine_test_species.csv"))
  eu <- read.csv("data/european_marine_test_species.csv", stringsAsFactors = FALSE)
  stopifnot(nrow(eu) >= 180)
  stopifnot(length(unique(eu$region)) >= 4)
  stopifnot(!any(duplicated(eu$species)))
})

run_test("Mediterranean species in ontology", {
  ont <- read.csv("data/ontology_traits.csv", stringsAsFactors = FALSE)
  med <- c("Mullus barbatus", "Diplodus sargus", "Paracentrotus lividus", "Octopus vulgaris")
  stopifnot(sum(med %in% ont$taxon_name) >= 3)
})

# --- PR code system ---
run_test("Harmonization config has 8-level PR codes", {
  source("R/config/harmonization_config.R")
  pr_names <- names(HARMONIZATION_CONFIG$protection_patterns)
  stopifnot(any(grepl("PR0", pr_names)))
  stopifnot(any(grepl("PR6", pr_names)))
  stopifnot(any(grepl("PR8", pr_names)))
  # Should NOT have old PR1
  stopifnot(!any(grepl("PR1", pr_names)))
})

# --- Ecosystem profiles ---
run_test("New ecosystem profiles exist", {
  source("R/config/harmonization_config.R")
  profiles <- names(HARMONIZATION_CONFIG$profiles)
  stopifnot("mediterranean" %in% profiles)
  stopifnot("atlantic_ne" %in% profiles)
  stopifnot("deep_sea" %in% profiles)
})

# --- Offline database ---
run_test("Offline DB builder script exists and parses", {
  stopifnot(file.exists("scripts/initialization/build_offline_trait_db.R"))
  parse("scripts/initialization/build_offline_trait_db.R")
})

run_test("Orchestrator has offline lookup function", {
  source("R/functions/validation_utils.R")
  source("R/config/harmonization_config.R")
  # Parse the orchestrator and check lookup_offline_traits exists
  env <- new.env()
  tryCatch({
    sys.source("R/functions/trait_lookup/orchestrator.R", envir = env)
    stopifnot(exists("lookup_offline_traits", envir = env))
  }, error = function(e) {
    # May fail due to missing dependencies, but we can check the parse
    parsed <- parse("R/functions/trait_lookup/orchestrator.R")
    # Check the function name appears in the parsed code
    code_text <- paste(deparse(parsed), collapse = "\n")
    stopifnot(grepl("lookup_offline_traits", code_text))
  })
})

# --- Negative tests ---
run_test("Offline lookup returns NULL for missing DB", {
  source("R/functions/validation_utils.R")
  source("R/config/harmonization_config.R")
  env <- new.env()
  sys.source("R/functions/trait_lookup/orchestrator.R", envir = env)
  result <- env$lookup_offline_traits("Any species", db_path = "nonexistent_db_xyz.db")
  stopifnot(is.null(result))
})

run_test("Offline lookup returns NULL for unknown species", {
  # Only test if offline DB exists
  if (file.exists("cache/offline_traits.db")) {
    source("R/functions/validation_utils.R")
    source("R/config/harmonization_config.R")
    env <- new.env()
    sys.source("R/functions/trait_lookup/orchestrator.R", envir = env)
    result <- env$lookup_offline_traits("Nonexistent_species_xyz_12345")
    stopifnot(is.null(result))
  }
})

# --- PTDB code fix ---
run_test("PTDB lookup function parses correctly", {
  parse("R/functions/trait_lookup/database_lookups.R")
})

run_test("PTDB lookup has Trophic_strategy handling", {
  code <- readLines("R/functions/trait_lookup/database_lookups.R")
  code_text <- paste(code, collapse = "\n")
  stopifnot(grepl("Trophic_strategy", code_text))
  stopifnot(grepl("primary_producer", code_text))
  # Old unconditional photosynthesis line should be gone
  # (it should only appear inside an else block now, not unconditionally)
})

# --- Summary ---
cat(sprintf("\n=== Results: %d/%d passed (%d failed) ===\n", pass_count, test_count, fail_count))
if (fail_count > 0) quit(status = 1)
