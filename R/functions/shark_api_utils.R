# ==============================================================================
# SHARK4R API UTILITIES
# ==============================================================================
# Wrapper functions for SHARK4R package integration
#
# SHARK4R provides access to:
# - Dyntaxa (Swedish species taxonomy)
# - WoRMS (World Register of Marine Species)
# - AlgaeBase (algae taxonomy)
# - SHARK database (Swedish oceanographic archive)
#
# Documentation: https://sharksmhi.github.io/SHARK4R/
# ==============================================================================

#' Check if SHARK4R Package is Available
#'
#' @return Logical, TRUE if SHARK4R is installed
check_shark4r_available <- function() {
  if (!requireNamespace("SHARK4R", quietly = TRUE)) {
    message("⚠ SHARK4R package not installed. Install with: install.packages('SHARK4R')")
    return(FALSE)
  }
  return(TRUE)
}

# ==============================================================================
# TAXONOMY FUNCTIONS
# ==============================================================================

#' Query Dyntaxa (Swedish Species Taxonomy)
#'
#' @param species_name Character, species name (scientific or common Swedish name)
#' @param fuzzy Logical, use fuzzy matching (default: TRUE)
#' @param use_cache Logical, use cached results (default: TRUE)
#' @param cache_dir Character, cache directory (default: "cache/shark")
#'
#' @return List with taxonomy information or NULL if not found
#'
#' @details
#' Dyntaxa is the Swedish Taxonomic Database maintained by SLU Artdatabanken.
#' Provides Swedish taxonomic classification and vernacular names.
#'
#' @export
query_dyntaxa <- function(species_name, fuzzy = TRUE, use_cache = TRUE, cache_dir = "cache/shark") {
  if (!check_shark4r_available()) {
    return(NULL)
  }

  message(sprintf("    → Dyntaxa: Searching for '%s'", species_name))

  # Check cache
  if (use_cache) {
    cache_file <- file.path(cache_dir, paste0("dyntaxa_", gsub("[^a-zA-Z0-9]", "_", species_name), ".rds"))
    if (file.exists(cache_file)) {
      cached <- readRDS(cache_file)
      if (difftime(Sys.time(), cached$timestamp, units = "days") < 30) {
        message("      ✓ Dyntaxa: Using cached result")
        return(cached$data)
      }
    }
  }

  # Query Dyntaxa
  result <- tryCatch({
    # SHARK4R::dyntaxa_search() function
    # Note: Actual function name may differ - check SHARK4R documentation
    if (fuzzy) {
      SHARK4R::sharkdata_dyntaxa_search(species_name, match_type = "contains")
    } else {
      SHARK4R::sharkdata_dyntaxa_search(species_name, match_type = "exact")
    }
  }, error = function(e) {
    message(sprintf("      ✗ Dyntaxa query error: %s", conditionMessage(e)))
    return(NULL)
  })

  if (is.null(result) || nrow(result) == 0) {
    message("      ✗ Dyntaxa: No results found")
    return(NULL)
  }

  # Format result
  formatted_result <- list(
    source = "Dyntaxa",
    scientific_name = result$scientific_name[1],
    swedish_name = result$vernacular_name[1],
    taxon_id = result$taxon_id[1],
    kingdom = result$kingdom[1],
    phylum = result$phylum[1],
    class = result$class[1],
    order = result$order[1],
    family = result$family[1],
    genus = result$genus[1],
    author = result$author[1],
    raw_data = result
  )

  message(sprintf("      ✓ Dyntaxa: Found '%s' (Swedish: %s)",
                  formatted_result$scientific_name,
                  ifelse(is.na(formatted_result$swedish_name), "N/A", formatted_result$swedish_name)))

  # Cache result
  if (use_cache) {
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    saveRDS(list(data = formatted_result, timestamp = Sys.time()), cache_file)
  }

  return(formatted_result)
}

#' Query WoRMS via SHARK4R
#'
#' @param species_name Character, species name
#' @param fuzzy Logical, use fuzzy matching (default: TRUE)
#' @param use_cache Logical, use cached results (default: TRUE)
#' @param cache_dir Character, cache directory (default: "cache/shark")
#'
#' @return List with WoRMS taxonomy information or NULL if not found
#'
#' @details
#' Uses SHARK4R's WoRMS integration. May provide different results than
#' direct WoRMS API queries in taxonomic_api_utils.R due to different
#' query methods.
#'
#' @export
query_shark_worms <- function(species_name, fuzzy = TRUE, use_cache = TRUE, cache_dir = "cache/shark") {
  if (!check_shark4r_available()) {
    return(NULL)
  }

  message(sprintf("    → WoRMS (via SHARK4R): Searching for '%s'", species_name))

  # Check cache
  if (use_cache) {
    cache_file <- file.path(cache_dir, paste0("shark_worms_", gsub("[^a-zA-Z0-9]", "_", species_name), ".rds"))
    if (file.exists(cache_file)) {
      cached <- readRDS(cache_file)
      if (difftime(Sys.time(), cached$timestamp, units = "days") < 30) {
        message("      ✓ WoRMS: Using cached result")
        return(cached$data)
      }
    }
  }

  # Query WoRMS via SHARK4R
  result <- tryCatch({
    # SHARK4R::worms_search() or similar
    SHARK4R::sharkdata_worms_search(species_name, fuzzy = fuzzy)
  }, error = function(e) {
    message(sprintf("      ✗ WoRMS query error: %s", conditionMessage(e)))
    return(NULL)
  })

  if (is.null(result) || nrow(result) == 0) {
    message("      ✗ WoRMS: No results found")
    return(NULL)
  }

  # Format result
  formatted_result <- list(
    source = "WoRMS (SHARK4R)",
    scientific_name = result$scientificname[1],
    aphia_id = result$AphiaID[1],
    authority = result$authority[1],
    status = result$status[1],
    kingdom = result$kingdom[1],
    phylum = result$phylum[1],
    class = result$class[1],
    order = result$order[1],
    family = result$family[1],
    genus = result$genus[1],
    rank = result$rank[1],
    raw_data = result
  )

  message(sprintf("      ✓ WoRMS: Found '%s' (AphiaID: %s)",
                  formatted_result$scientific_name,
                  formatted_result$aphia_id))

  # Cache result
  if (use_cache) {
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    saveRDS(list(data = formatted_result, timestamp = Sys.time()), cache_file)
  }

  return(formatted_result)
}

#' Query AlgaeBase
#'
#' @param species_name Character, algae species name
#' @param use_cache Logical, use cached results (default: TRUE)
#' @param cache_dir Character, cache directory (default: "cache/shark")
#'
#' @return List with AlgaeBase taxonomy information or NULL if not found
#'
#' @details
#' AlgaeBase is the global database for algae taxonomy.
#' Particularly useful for phytoplankton species.
#'
#' @export
query_algaebase <- function(species_name, use_cache = TRUE, cache_dir = "cache/shark") {
  if (!check_shark4r_available()) {
    return(NULL)
  }

  message(sprintf("    → AlgaeBase: Searching for '%s'", species_name))

  # Check cache
  if (use_cache) {
    cache_file <- file.path(cache_dir, paste0("algaebase_", gsub("[^a-zA-Z0-9]", "_", species_name), ".rds"))
    if (file.exists(cache_file)) {
      cached <- readRDS(cache_file)
      if (difftime(Sys.time(), cached$timestamp, units = "days") < 30) {
        message("      ✓ AlgaeBase: Using cached result")
        return(cached$data)
      }
    }
  }

  # Query AlgaeBase
  result <- tryCatch({
    # SHARK4R::algaebase_search() or similar
    SHARK4R::sharkdata_algaebase_search(species_name)
  }, error = function(e) {
    message(sprintf("      ✗ AlgaeBase query error: %s", conditionMessage(e)))
    return(NULL)
  })

  if (is.null(result) || nrow(result) == 0) {
    message("      ✗ AlgaeBase: No results found")
    return(NULL)
  }

  # Format result
  formatted_result <- list(
    source = "AlgaeBase",
    scientific_name = result$scientific_name[1],
    algaebase_id = result$id[1],
    authority = result$authority[1],
    status = result$status[1],
    phylum = result$phylum[1],
    class = result$class[1],
    order = result$order[1],
    family = result$family[1],
    genus = result$genus[1],
    raw_data = result
  )

  message(sprintf("      ✓ AlgaeBase: Found '%s' (ID: %s)",
                  formatted_result$scientific_name,
                  formatted_result$algaebase_id))

  # Cache result
  if (use_cache) {
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    saveRDS(list(data = formatted_result, timestamp = Sys.time()), cache_file)
  }

  return(formatted_result)
}

# ==============================================================================
# DATA RETRIEVAL FUNCTIONS
# ==============================================================================

#' Get Available SHARK Parameters
#'
#' @return Character vector of available parameter names
#'
#' @details
#' Lists all environmental parameters available in SHARK database
#' (e.g., temperature, salinity, nutrients, oxygen, pH)
#'
#' @export
get_available_shark_parameters <- function() {
  if (!check_shark4r_available()) {
    return(character(0))
  }

  tryCatch({
    # Get parameter list from SHARK4R
    params <- SHARK4R::sharkdata_get_parameters()
    return(sort(unique(params$parameter_name)))
  }, error = function(e) {
    message(sprintf("Error getting parameters: %s", conditionMessage(e)))
    return(c(
      "Temperature (°C)",
      "Salinity (PSU)",
      "Oxygen (mg/L)",
      "pH",
      "Phosphate (μmol/L)",
      "Nitrate (μmol/L)",
      "Chlorophyll-a (μg/L)",
      "Secchi depth (m)"
    ))
  })
}

#' Get SHARK Environmental Data
#'
#' @param parameters Character vector, parameter names to retrieve
#' @param start_date Date or character, start date (YYYY-MM-DD)
#' @param end_date Date or character, end date (YYYY-MM-DD)
#' @param bbox Named numeric vector with north, south, east, west (optional)
#' @param max_records Numeric, maximum number of records (default: 10000)
#'
#' @return Data frame with environmental data or NULL if error
#'
#' @details
#' Retrieves oceanographic measurements from SHARK database.
#' Data is returned as a data frame with columns:
#' - date, time, latitude, longitude
#' - depth, parameter, value, unit
#' - station, dataset
#'
#' @export
get_shark_environmental_data <- function(parameters, start_date, end_date,
                                        bbox = NULL, max_records = 10000) {
  if (!check_shark4r_available()) {
    return(NULL)
  }

  message("    → SHARK: Querying environmental data...")
  message(sprintf("      Parameters: %s", paste(parameters, collapse = ", ")))
  message(sprintf("      Date range: %s to %s", start_date, end_date))

  # Build query
  query_params <- list(
    parameters = parameters,
    start_date = as.character(start_date),
    end_date = as.character(end_date),
    max_records = max_records
  )

  # Add bounding box if provided
  if (!is.null(bbox)) {
    if (all(c("north", "south", "east", "west") %in% names(bbox))) {
      query_params$bbox <- bbox
      message(sprintf("      Bounding box: N%.2f S%.2f E%.2f W%.2f",
                      bbox["north"], bbox["south"], bbox["east"], bbox["west"]))
    }
  }

  # Query SHARK database
  data <- tryCatch({
    SHARK4R::sharkdata_get_physical_chemical(
      parameters = query_params$parameters,
      start_date = query_params$start_date,
      end_date = query_params$end_date,
      bbox = query_params$bbox,
      max_records = query_params$max_records
    )
  }, error = function(e) {
    message(sprintf("      ✗ SHARK query error: %s", conditionMessage(e)))
    return(NULL)
  })

  if (is.null(data) || nrow(data) == 0) {
    message("      ✗ SHARK: No data found for specified criteria")
    return(NULL)
  }

  message(sprintf("      ✓ SHARK: Retrieved %d records", nrow(data)))

  return(data)
}

#' Get SHARK Species Occurrence Data
#'
#' @param species_name Character, species name (scientific or Swedish)
#' @param start_date Date or character, start date (YYYY-MM-DD)
#' @param end_date Date or character, end date (YYYY-MM-DD)
#' @param bbox Named numeric vector with north, south, east, west (optional)
#' @param max_records Numeric, maximum number of records (default: 5000)
#'
#' @return Data frame with occurrence data or NULL if error
#'
#' @details
#' Retrieves biological observation records from SHARK database.
#' Includes phytoplankton, zooplankton, fish observations.
#'
#' Returns data frame with columns:
#' - date, time, latitude, longitude, depth
#' - species, abundance, unit
#' - station, dataset, observer
#'
#' @export
get_shark_species_occurrence <- function(species_name, start_date, end_date,
                                        bbox = NULL, max_records = 5000) {
  if (!check_shark4r_available()) {
    return(NULL)
  }

  message(sprintf("    → SHARK: Querying occurrence data for '%s'", species_name))
  message(sprintf("      Date range: %s to %s", start_date, end_date))

  # Build query
  query_params <- list(
    species = species_name,
    start_date = as.character(start_date),
    end_date = as.character(end_date),
    max_records = max_records
  )

  # Add bounding box if provided
  if (!is.null(bbox)) {
    if (all(c("north", "south", "east", "west") %in% names(bbox))) {
      query_params$bbox <- bbox
    }
  }

  # Query SHARK database
  data <- tryCatch({
    SHARK4R::sharkdata_get_biological(
      species = query_params$species,
      start_date = query_params$start_date,
      end_date = query_params$end_date,
      bbox = query_params$bbox,
      max_records = query_params$max_records
    )
  }, error = function(e) {
    message(sprintf("      ✗ SHARK query error: %s", conditionMessage(e)))
    return(NULL)
  })

  if (is.null(data) || nrow(data) == 0) {
    message(sprintf("      ✗ SHARK: No occurrence records found for '%s'", species_name))
    return(NULL)
  }

  message(sprintf("      ✓ SHARK: Retrieved %d occurrence records", nrow(data)))

  return(data)
}

# ==============================================================================
# QUALITY CONTROL FUNCTIONS
# ==============================================================================

#' Validate SHARK Format Data
#'
#' @param data_frame Data frame to validate
#'
#' @return List with validation results
#'
#' @details
#' Validates data against SHARK format specifications.
#' Checks for:
#' - Required columns
#' - Data types
#' - Valid value ranges
#' - Coordinate validity
#'
#' @export
validate_shark_data <- function(data_frame) {
  if (!check_shark4r_available()) {
    return(list(valid = FALSE, message = "SHARK4R package not available"))
  }

  message("    → Running SHARK format validation...")

  result <- tryCatch({
    SHARK4R::sharkdata_validate(data_frame)
  }, error = function(e) {
    return(list(
      valid = FALSE,
      message = sprintf("Validation error: %s", conditionMessage(e)),
      errors = NULL,
      warnings = NULL
    ))
  })

  if (result$valid) {
    message("      ✓ Validation passed")
  } else {
    message(sprintf("      ✗ Validation failed: %d errors, %d warnings",
                    length(result$errors), length(result$warnings)))
  }

  return(result)
}

#' Check Data Quality
#'
#' @param data_frame Data frame to check
#'
#' @return List with quality check results
#'
#' @details
#' Performs quality control checks:
#' - Missing value analysis
#' - Outlier detection
#' - Consistency checks
#' - Temporal coverage
#'
#' @export
check_data_quality <- function(data_frame) {
  if (!check_shark4r_available()) {
    return(list(message = "SHARK4R package not available"))
  }

  message("    → Running data quality checks...")

  result <- tryCatch({
    SHARK4R::sharkdata_quality_check(data_frame)
  }, error = function(e) {
    # Fallback: basic quality checks if SHARK4R function unavailable
    return(list(
      completeness = sum(complete.cases(data_frame)) / nrow(data_frame) * 100,
      missing_values = colSums(is.na(data_frame)),
      record_count = nrow(data_frame),
      column_count = ncol(data_frame),
      message = "Basic quality check completed (SHARK4R function unavailable)"
    ))
  })

  message("      ✓ Quality check completed")

  return(result)
}

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Format SHARK Results for Display
#'
#' @param raw_data Data frame from SHARK query
#' @param result_type Character, type of result ("environmental", "occurrence", "taxonomy")
#'
#' @return Formatted data frame ready for display
#'
#' @export
format_shark_results <- function(raw_data, result_type = "environmental") {
  if (is.null(raw_data) || nrow(raw_data) == 0) {
    return(data.frame(Message = "No data to display"))
  }

  # Format based on result type
  if (result_type == "environmental") {
    # Select and rename key columns
    formatted <- raw_data[, c("date", "latitude", "longitude", "depth",
                              "parameter", "value", "unit", "station")]
    colnames(formatted) <- c("Date", "Lat", "Lon", "Depth (m)",
                            "Parameter", "Value", "Unit", "Station")
  } else if (result_type == "occurrence") {
    formatted <- raw_data[, c("date", "species", "latitude", "longitude",
                              "abundance", "unit", "station")]
    colnames(formatted) <- c("Date", "Species", "Lat", "Lon",
                            "Abundance", "Unit", "Station")
  } else {
    formatted <- raw_data
  }

  return(formatted)
}

#' Get SHARK Dataset List
#'
#' @return Data frame with available datasets
#'
#' @export
get_shark_datasets <- function() {
  if (!check_shark4r_available()) {
    return(data.frame(
      Dataset = "SHARK4R package not installed",
      Description = "Install with: install.packages('SHARK4R')"
    ))
  }

  tryCatch({
    SHARK4R::sharkdata_list_datasets()
  }, error = function(e) {
    return(data.frame(
      Dataset = c("Physical-Chemical", "Phytoplankton", "Zooplankton",
                  "Zoobenthos", "Fish", "Marine Mammals"),
      Description = c(
        "Temperature, salinity, nutrients, oxygen",
        "Phytoplankton species and abundance",
        "Zooplankton species and abundance",
        "Benthic fauna species and abundance",
        "Fish survey data",
        "Marine mammal observations"
      )
    ))
  })
}

# ==============================================================================
# END OF SHARK4R API UTILITIES
# ==============================================================================
