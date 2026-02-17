# ============================================================================
# EcoNeTool - Configuration Constants
# ============================================================================
#
# This file contains all configuration parameters and constants used
# throughout the EcoNeTool application.
#
# ============================================================================

# ============================================================================
# COLOR SCHEME
# ============================================================================

# Color scheme for functional groups (Benthos, Birds, Detritus, Fish, Mammals, Phytoplankton, Zooplankton)
# Order: Benthos, Birds, Detritus, Fish, Mammals, Phytoplankton, Zooplankton
# Colors: light brown (Benthos), purple (Birds), brown (Detritus), blue (Fish), red (Mammals), green (Phytoplankton/Phytobenthos), light blue (Zooplankton)
COLOR_SCHEME <- c("burlywood", "purple", "brown", "blue", "red", "green", "lightblue")

# ============================================================================
# TROPHIC LEVEL PARAMETERS
# ============================================================================

# Maximum iterations for convergence
TROPHIC_LEVEL_MAX_ITER <- 100

# Convergence threshold
TROPHIC_LEVEL_CONVERGENCE <- 0.0001

# ============================================================================
# FLUX CALCULATION PARAMETERS
# ============================================================================

# Convert J/sec to kJ/day
FLUX_CONVERSION_FACTOR <- 86.4

# Small value to avoid log(0)
FLUX_LOG_EPSILON <- 0.00001

# ============================================================================
# ECOPATH IMPORT DEFAULTS
# ============================================================================

# Default values when P/B, Q/B, or efficiency data is missing
DEFAULT_PB_RATIO <- 0.5  # Production/Biomass ratio
DEFAULT_QB_RATIO <- 1.5  # Consumption/Biomass ratio

# Default efficiency values by functional group
DEFAULT_EFFICIENCY_PHYTOPLANKTON <- 0.4
DEFAULT_EFFICIENCY_ZOOPLANKTON <- 0.75
DEFAULT_EFFICIENCY_BENTHOS <- 0.7
DEFAULT_EFFICIENCY_FISH <- 0.85
DEFAULT_EFFICIENCY_DETRITUS <- 0.2
DEFAULT_EFFICIENCY_DEFAULT <- 0.7

# Default body mass estimates by functional group (grams)
DEFAULT_BODYMASS_PHYTOPLANKTON <- 0.00001
DEFAULT_BODYMASS_ZOOPLANKTON <- 0.001
DEFAULT_BODYMASS_BENTHOS <- 1.0
DEFAULT_BODYMASS_FISH <- 100.0
DEFAULT_BODYMASS_DETRITUS <- 0.0001
DEFAULT_BODYMASS_DEFAULT <- 1.0

# ============================================================================
# METABOLIC CALCULATION PARAMETERS
# ============================================================================

# Default temperature for metabolic calculations (°C)
# Note: 3.5°C is specific to Gulf of Riga; adjust for other systems
DEFAULT_TEMPERATURE <- 3.5

# Metabolic scaling constants
METABOLIC_X0_INVERTEBRATES <- 17.17  # Normalization constant for invertebrates
METABOLIC_X0_VERTEBRATES <- 18.47    # Normalization constant for vertebrates
METABOLIC_E <- 0.69                   # Activation energy
METABOLIC_A <- -0.29                  # Allometric exponent

# ============================================================================
# VISUALIZATION PARAMETERS
# ============================================================================

# Scaling factor for node size by biomass
NODE_SIZE_SCALE <- 25

# Minimum node size
NODE_SIZE_MIN <- 4

# Scaling factor for edge width by flux
EDGE_WIDTH_SCALE <- 15

# Minimum edge width
EDGE_WIDTH_MIN <- 0.1

# Arrow size for topology networks
EDGE_ARROW_SIZE_TOPOLOGY <- 0.3

# Arrow size for flux networks
EDGE_ARROW_SIZE_FLUX <- 0.05

# Trophic level vertical spacing (pixels)
VIS_TROPHIC_LEVEL_SPACING <- 250

# visNetwork physics parameters
VIS_SPRING_LENGTH <- 400
VIS_GRAVITATIONAL_CONSTANT <- -8000

# ============================================================================
# SPATIAL ANALYSIS PARAMETERS
# ============================================================================

# Conversion factor: degrees to meters (approximate)
# Note: 1 degree latitude ≈ 111 km at equator
DEGREES_TO_METERS <- 111000

# Maximum number of hexagons allowed (prevent memory issues)
MAX_HEXAGONS <- 1000000

# Minimum hexagon cell size (meters)
MIN_CELL_SIZE_METERS <- 100

# Maximum hexagon cell size (meters)
MAX_CELL_SIZE_METERS <- 100000

# ============================================================================
# DATA FILE PATHS
# ============================================================================

# Main data file (Baltic Sea food web)
DATA_FILE <- "examples/LTCoast.Rdata"

# ============================================================================
# UPLOAD LIMITS
# ============================================================================

# Maximum file upload size in MB
MAX_UPLOAD_SIZE_MB <- 50

# ============================================================================
# METAWEB LIBRARY PATHS (Phase 2)
# ============================================================================

# Regional metaweb file paths
METAWEB_PATHS <- list(
  "baltic_kortsch2021" = "metawebs/baltic/baltic_kortsch2021.rds",
  "kongsfjorden_farage2021" = "metawebs/arctic/kongsfjorden_farage2021.rds",
  "north_sea_frelat2022" = "metawebs/atlantic/north_sea_frelat2022.rds",
  "barents_arctic_kortsch2015" = "metawebs/arctic/barents_arctic_kortsch2015.rds",
  "barents_boreal_kortsch2015" = "metawebs/arctic/barents_boreal_kortsch2015.rds"
)

# ============================================================================
# METAWEB VALIDATION (Startup)
# ============================================================================

# Validate that metaweb files exist (CRITICAL-003 fix)
# This runs once when config is loaded
validate_metaweb_paths <- function() {
  missing_metawebs <- names(METAWEB_PATHS)[!sapply(METAWEB_PATHS, file.exists)]

  if (length(missing_metawebs) > 0) {
    warning("Missing metaweb files: ", paste(missing_metawebs, collapse = ", "))

    available_metawebs <- names(METAWEB_PATHS)[sapply(METAWEB_PATHS, file.exists)]
    if (length(available_metawebs) > 0) {
      message("Available metawebs: ", paste(available_metawebs, collapse = ", "))
    } else {
      warning("No metaweb files found! Phase 1 and Phase 2 features will not work.")
    }
  }

  invisible(missing_metawebs)
}

# Run validation when config is loaded
validate_metaweb_paths()

# ============================================================================
# API KEY MANAGEMENT (Phase 2: Database Integrations)
# ============================================================================

# API keys for external database services
# Users should copy config/api_keys.R.template to config/api_keys.R
# and fill in their own API keys

# Default API keys (public/demo keys where available)
API_KEYS <- list(
  # AlgaeBase credentials (requires registration at algaebase.org)
  algaebase_username = "",
  algaebase_password = "",

  # freshwaterecology.info API key (public demo key provided)
  freshwaterecology_key = "a06a5fe6-dfe5-11f0-a4f8-525400ca9bdd",

  # SeaLifeBase uses same API as FishBase (no key needed)
  # SHARK API (Swedish ocean archives) - no key needed, public API

  # Future database keys can be added here
  traitbank_key = "",
  worms_key = ""  # WoRMS usually doesn't require key, but rate limits apply
)

# File path for user's custom API keys
API_KEYS_FILE <- "config/api_keys.R"

# Load user's API keys if file exists (overrides defaults)
if (file.exists(API_KEYS_FILE)) {
  tryCatch({
    source(API_KEYS_FILE, local = TRUE)
    message("✓ Loaded custom API keys from ", API_KEYS_FILE)
  }, error = function(e) {
    warning("Failed to load API keys from ", API_KEYS_FILE, ": ", e$message)
  })
} else {
  # Inform user about API key template
  if (file.exists("config/api_keys.R.template")) {
    message("ℹ API key template available. Copy config/api_keys.R.template to config/api_keys.R and add your credentials.")
  }
}

#' Get API key for a service
#'
#' @param service Character, name of the service (e.g., "algaebase_username")
#' @return Character, API key or empty string if not set
#' @export
get_api_key <- function(service) {
  key <- API_KEYS[[service]]
  if (is.null(key) || key == "") {
    return("")
  }
  return(key)
}

#' Check if API key is configured for a service
#'
#' @param service Character, name of the service
#' @return Logical, TRUE if key is set and non-empty
#' @export
has_api_key <- function(service) {
  key <- get_api_key(service)
  return(nchar(key) > 0)
}

# ============================================================================
# VERSION MANAGEMENT (Centralized)
# ============================================================================

#' Load version information from VERSION file
#'
#' @return List with version information
#' @export
load_version_info <- function() {
  version_file <- "VERSION"

  # Default version info (fallback)
  version_info <- list(
    VERSION = "1.4.2",
    VERSION_NAME = "Local Databases Integration + Performance & Robustness",
    RELEASE_DATE = "2025-12-26",
    STATUS = "stable",
    MAJOR = 1,
    MINOR = 4,
    PATCH = 2
  )

  # Try to read VERSION file
  if (file.exists(version_file)) {
    tryCatch({
      lines <- readLines(version_file, warn = FALSE)

      # Parse VERSION file (key=value format)
      for (line in lines) {
        # Skip comments and empty lines
        if (grepl("^\\s*#", line) || grepl("^\\s*$", line)) next

        # Parse key=value
        if (grepl("=", line)) {
          parts <- strsplit(line, "=")[[1]]
          if (length(parts) == 2) {
            key <- trimws(parts[1])
            value <- trimws(parts[2])

            # Remove quotes if present
            value <- gsub('^"|"$', '', value)

            # Convert numeric values
            if (key %in% c("MAJOR", "MINOR", "PATCH")) {
              value <- as.numeric(value)
            }

            version_info[[key]] <- value
          }
        }
      }
    }, error = function(e) {
      warning("Failed to load VERSION file: ", e$message)
    })
  } else {
    warning("VERSION file not found, using default version info")
  }

  return(version_info)
}

#' Get version string
#'
#' @param format Format: "short" (1.1.1), "long" (1.1.1 - Name), "full" (Version 1.1.1 - Name)
#' @return Character, formatted version string
#' @export
get_version <- function(format = "short") {
  info <- ECONETOOL_VERSION

  if (format == "short") {
    return(info$VERSION)
  } else if (format == "long") {
    return(paste(info$VERSION, "-", info$VERSION_NAME))
  } else if (format == "full") {
    return(paste("Version", info$VERSION, "-", info$VERSION_NAME))
  } else {
    return(info$VERSION)
  }
}

#' Get version info as HTML
#'
#' @param include_date Include release date
#' @return HTML string
#' @export
get_version_html <- function(include_date = TRUE) {
  info <- ECONETOOL_VERSION

  html <- paste0(
    "<strong>Version ", info$VERSION, "</strong> - ", info$VERSION_NAME
  )

  if (include_date && !is.null(info$RELEASE_DATE) && info$RELEASE_DATE != "") {
    html <- paste0(html, "<br><small>Released: ", info$RELEASE_DATE, "</small>")
  }

  return(html)
}

# Load version information at startup
ECONETOOL_VERSION <- load_version_info()

# Print version info
message("EcoNeTool ", get_version("long"))
