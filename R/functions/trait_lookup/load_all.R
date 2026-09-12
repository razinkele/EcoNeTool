# =============================================================================
# TRAIT LOOKUP MODULE LOADER
# =============================================================================
# Load all trait lookup functions in correct order.
# Replaces the old monolithic trait_lookup.R file.
#
# Usage: source("R/functions/trait_lookup/load_all.R")
# =============================================================================

# Core utilities first: orchestrator.R calls app_path() in top-level code, so
# this loader must be self-sufficient rather than assuming the caller already
# sourced validation_utils.R. Re-sourcing it is harmless (function definitions).
if (!exists("app_path", mode = "function")) {
  source("R/functions/validation_utils.R")
}

# Database query functions (individual DB lookups)
source("R/functions/trait_lookup/database_lookups.R")

# Bundled CSV trait databases (Black Sea, Arctic, Cefas, Coral, Pelagic)
source("R/functions/trait_lookup/csv_trait_databases.R")

# API-based trait databases (WoRMS Traits, PolyTraits, EMODnet, OBIS, TraitBank)
source("R/functions/trait_lookup/api_trait_databases.R")

# Harmonization rules (raw traits -> MS/FS/MB/EP/PR codes)
source("R/functions/trait_lookup/harmonization.R")

# Main orchestrator (lookup_species_traits + batch_lookup_traits)
source("R/functions/trait_lookup/orchestrator.R")

message("\u2713 Trait lookup module loaded (5 files)")
