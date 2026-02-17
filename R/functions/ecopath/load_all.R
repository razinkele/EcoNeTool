# ==============================================================================
# ECOPATH IMPORT FUNCTIONS LOADER
# ==============================================================================
# Load all ECOPATH import functions in correct order
#
# Usage:
#   source("R/functions/ecopath/load_all.R")
#
# This replaces the old monolithic ecopath_import.R file
# ==============================================================================

# CSV/Excel import
source("R/functions/ecopath/ecopath_csv.R")

# Platform-specific importers
source("R/functions/ecopath/ecopath_windows.R")
source("R/functions/ecopath/ecopath_unix.R")

# Cross-platform wrapper (main entry point)
source("R/functions/ecopath/ecopath_import.R")

# Diagnostics and validation utilities
source("R/functions/ecopath/ecopath_diagnostics.R")

message("✓ ECOPATH import functions loaded (5 files)")
