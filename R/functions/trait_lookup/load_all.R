# =============================================================================
# TRAIT LOOKUP MODULE LOADER
# =============================================================================
# Load all trait lookup functions in correct order.
# Replaces the old monolithic trait_lookup.R file.
#
# Usage: source("R/functions/trait_lookup/load_all.R")
# =============================================================================

# Database query functions (individual DB lookups)
source("R/functions/trait_lookup/database_lookups.R")

# Harmonization rules (raw traits -> MS/FS/MB/EP/PR codes)
source("R/functions/trait_lookup/harmonization.R")

# Main orchestrator (lookup_species_traits + batch_lookup_traits)
source("R/functions/trait_lookup/orchestrator.R")

message("\u2713 Trait lookup module loaded (3 files)")
