# ==============================================================================
# RPATH FUNCTIONS LOADER
# ==============================================================================
# Load all Rpath-related functions in correct order
#
# Usage:
#   source("R/functions/rpath/load_all.R")
#
# This replaces the old monolithic rpath_integration.R file
# ==============================================================================

# Package check and data conversion
source("R/functions/rpath/rpath_conversion.R")

# Mass balance and trophic level calculation
source("R/functions/rpath/rpath_balancing.R")

# EcoSim simulation and analysis
source("R/functions/rpath/rpath_simulation.R")

# Complete end-to-end workflows
source("R/functions/rpath/rpath_workflows.R")

# Survey trends (R2: DATRAS survey validation of biomass inputs)
source("R/functions/rpath/survey_validation.R")

message("✓ Rpath functions loaded (5 files)")
