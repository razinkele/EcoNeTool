# =============================================================================
# TRAIT LOOKUP - Bundled CSV Trait Database Functions
# =============================================================================
# Lookup functions for 5 bundled CSV trait databases:
#   - Black Sea Traits
#   - Arctic Traits
#   - CEFAS NW Europe Benthic Traits
#   - Coral Trait Database
#   - Pelagic Trait Database
#
# Each function returns: list(species, source, success, traits)
# =============================================================================

# ---------------------------------------------------------------------------
# Module-level helpers
# ---------------------------------------------------------------------------

.csv_cache <- new.env(parent = emptyenv())

.load_csv_cached <- function(file_path, cache_key) {
  # Use normalised file_path as effective cache key so different files
  # (e.g. tempfiles in tests) are never confused, while still accepting the
  # cache_key parameter for API compatibility.
  effective_key <- tryCatch(normalizePath(file_path, mustWork = FALSE),
                             error = function(e) file_path)
  if (!is.null(.csv_cache[[effective_key]])) return(.csv_cache[[effective_key]])
  if (!file.exists(file_path)) return(NULL)
  data <- tryCatch(
    read.csv(file_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
    error = function(e) {
      message("Error reading ", file_path, ": ", e$message)
      NULL
    }
  )
  if (!is.null(data)) .csv_cache[[effective_key]] <- data
  data
}

.find_species <- function(data, species_name, species_col = "species") {
  if (!species_col %in% names(data)) {
    for (alt in c("Species", "taxon", "Taxon", "scientific_name",
                  "ScientificName", "species_name", "specie_name")) {
      if (alt %in% names(data)) {
        species_col <- alt
        break
      }
    }
  }
  if (!species_col %in% names(data)) return(NULL)

  # Exact match (case-insensitive)
  idx <- which(tolower(data[[species_col]]) == tolower(species_name))
  if (length(idx) > 0) return(data[idx[1], , drop = FALSE])

  # Genus-level fallback
  genus <- strsplit(species_name, " ")[[1]][1]
  idx <- which(grepl(paste0("^", genus), data[[species_col]], ignore.case = TRUE))
  if (length(idx) > 0) return(data[idx[1], , drop = FALSE])

  NULL
}

.extract <- function(row, col_name) {
  if (col_name %in% names(row) &&
      !is.na(row[[col_name]]) &&
      row[[col_name]] != "") {
    row[[col_name]]
  } else {
    NULL
  }
}

.extract_num <- function(row, col_name) {
  val <- .extract(row, col_name)
  if (!is.null(val) && is.numeric(val)) val else NULL
}

# ---------------------------------------------------------------------------
# 1. Black Sea Traits
# ---------------------------------------------------------------------------

#' Lookup Black Sea trait database
#'
#' @param species_name Scientific name (character)
#' @param csv_file Path to blacksea_traits.csv (default: bundled file)
#' @return list(species, source, success, traits)
#' @export
lookup_blacksea_traits <- function(
    species_name,
    csv_file = file.path(
      dirname(dirname(dirname(dirname(sys.frame(1)$ofile)))),
      "data", "external_traits", "blacksea_traits.csv"
    )) {

  result <- list(
    species = species_name,
    source  = "BlackSea",
    success = FALSE,
    traits  = list()
  )

  data <- .load_csv_cached(csv_file, "blacksea")
  if (is.null(data) || nrow(data) == 0) return(result)

  row <- .find_species(data, species_name)
  if (is.null(row)) return(result)

  result$success <- TRUE
  result$traits  <- list(
    feeding_mode       = .extract(row, "feeding_mode"),
    mobility_info      = .extract(row, "mobility"),
    size_mm            = .extract_num(row, "body_size_mm"),
    reproductive_mode  = .extract(row, "reproductive_mode"),
    temperature_affinity = .extract(row, "temperature_affinity"),
    salinity_affinity  = .extract(row, "salinity_affinity"),
    depth              = .extract_num(row, "depth")
  )
  result
}

# ---------------------------------------------------------------------------
# 2. Arctic Traits
# ---------------------------------------------------------------------------

#' Lookup Arctic trait database
#'
#' @param species_name Scientific name (character)
#' @param csv_file Path to arctic_traits.csv (default: bundled file)
#' @return list(species, source, success, traits)
#' @export
lookup_arctic_traits <- function(
    species_name,
    csv_file = file.path(
      dirname(dirname(dirname(dirname(sys.frame(1)$ofile)))),
      "data", "external_traits", "arctic_traits.csv"
    )) {

  result <- list(
    species = species_name,
    source  = "ArcticTraits",
    success = FALSE,
    traits  = list()
  )

  data <- .load_csv_cached(csv_file, "arctic")
  if (is.null(data) || nrow(data) == 0) return(result)

  row <- .find_species(data, species_name)
  if (is.null(row)) return(result)

  temp_pref <- .extract(row, "temperature_preference")
  if (is.null(temp_pref)) temp_pref <- "arctic"

  result$success <- TRUE
  result$traits  <- list(
    feeding_mode          = .extract(row, "feeding_mode"),
    mobility_info         = .extract(row, "mobility"),
    size_mm               = .extract_num(row, "body_size_mm"),
    reproductive_mode     = .extract(row, "reproductive_mode"),
    temperature_preference = temp_pref
  )
  result
}

# ---------------------------------------------------------------------------
# 3. CEFAS NW Europe Benthic Traits
# ---------------------------------------------------------------------------

#' Lookup CEFAS benthic trait database
#'
#' @param species_name Scientific name (character)
#' @param csv_file Path to cefas_benthic_traits.csv (default: bundled file)
#' @return list(species, source, success, traits)
#' @export
lookup_cefas_traits <- function(
    species_name,
    csv_file = file.path(
      dirname(dirname(dirname(dirname(sys.frame(1)$ofile)))),
      "data", "external_traits", "cefas_benthic_traits.csv"
    )) {

  result <- list(
    species = species_name,
    source  = "Cefas",
    success = FALSE,
    traits  = list()
  )

  data <- .load_csv_cached(csv_file, "cefas")
  if (is.null(data) || nrow(data) == 0) return(result)

  row <- .find_species(data, species_name)
  if (is.null(row)) return(result)

  result$success <- TRUE
  result$traits  <- list(
    feeding_mode      = .extract(row, "feeding_mode"),
    mobility_info     = .extract(row, "mobility"),
    size_mm           = .extract(row, "body_size"),
    longevity_years   = .extract(row, "lifespan"),
    larval_development = .extract(row, "larval_development"),
    reproductive_mode = .extract(row, "reproductive_mode"),
    living_habit      = .extract(row, "living_habit"),
    bioturbation_mode = .extract(row, "bioturbation_mode")
  )
  result
}

# ---------------------------------------------------------------------------
# 4. Coral Trait Database
# ---------------------------------------------------------------------------

#' Lookup Coral trait database
#'
#' @param species_name Scientific name (character)
#' @param csv_file Path to coral_traits.csv (default: bundled file)
#' @return list(species, source, success, traits)
#' @export
lookup_coral_traits <- function(
    species_name,
    csv_file = file.path(
      dirname(dirname(dirname(dirname(sys.frame(1)$ofile)))),
      "data", "external_traits", "coral_traits.csv"
    )) {

  result <- list(
    species = species_name,
    source  = "CoralTraits",
    success = FALSE,
    traits  = list()
  )

  data <- .load_csv_cached(csv_file, "coral")
  if (is.null(data) || nrow(data) == 0) return(result)

  row <- .find_species(data, species_name)
  if (is.null(row)) return(result)

  result$success <- TRUE
  result$traits  <- list(
    growth_form       = .extract(row, "growth_form"),
    reproductive_mode = .extract(row, "reproductive_mode"),
    thermal_tolerance = .extract_num(row, "thermal_tolerance_max"),
    depth_min         = .extract_num(row, "depth_lower"),
    depth_max         = .extract_num(row, "depth_upper")
  )
  result
}

# ---------------------------------------------------------------------------
# 5. Pelagic Trait Database
# ---------------------------------------------------------------------------

#' Lookup Pelagic trait database
#'
#' @param species_name Scientific name (character)
#' @param csv_file Path to pelagic_traits.csv (default: bundled file)
#' @return list(species, source, success, traits)
#' @export
lookup_pelagic_traits <- function(
    species_name,
    csv_file = file.path(
      dirname(dirname(dirname(dirname(sys.frame(1)$ofile)))),
      "data", "external_traits", "pelagic_traits.csv"
    )) {

  result <- list(
    species = species_name,
    source  = "PelagicTraits",
    success = FALSE,
    traits  = list()
  )

  data <- .load_csv_cached(csv_file, "pelagic")
  if (is.null(data) || nrow(data) == 0) return(result)

  row <- .find_species(data, species_name)
  if (is.null(row)) return(result)

  result$success <- TRUE
  result$traits  <- list(
    habitat_use       = .extract(row, "habitat_use"),
    morphology        = .extract(row, "morphology"),
    body_length_cm    = .extract_num(row, "body_length_cm"),
    nutritional_quality = .extract(row, "nutritional_quality"),
    feeding_mode      = .extract(row, "feeding_mode")
  )
  result
}
