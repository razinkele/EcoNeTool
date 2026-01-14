# Automated Trait Lookup System
# Implements best-practice workflow for trait-based food web construction
# Database hierarchy: WoRMS → FishBase → BIOTIC → MAREDAT → PTDB → TraitBank

# Required packages: httr, jsonlite, rfishbase, worrms
# Note: Some functions require existing taxonomic_api_utils.R

# =============================================================================
# DEPENDENCIES (loaded by app.R before this file)
# =============================================================================
# - validation_utils.R: %||%, with_timeout(), is_valid_value(), safe_get()
# - harmonization_config.R: HARMONIZATION_CONFIG
#
# DO NOT source these here - they are loaded in app.R to prevent circular deps
# If running standalone, source them first:
#   source("R/functions/validation_utils.R")
#   source("R/config/harmonization_config.R")
# =============================================================================

# Verify HARMONIZATION_CONFIG exists (loaded by app.R)
if (!exists("HARMONIZATION_CONFIG")) {
  warning("HARMONIZATION_CONFIG not found. Loading from config file...")
  if (file.exists("R/config/harmonization_config.R")) {
    source("R/config/harmonization_config.R")
  } else {
    stop("Cannot find R/config/harmonization_config.R - run from app root directory")
  }
}

# ============================================================================
# DATABASE LOOKUP FUNCTIONS
# ============================================================================

# NOTE: with_timeout() is now defined in validation_utils.R
# Use with_timeout(expr, timeout, on_timeout, verbose) from there

#' Lookup FishBase traits for a species
#'
#' @param species_name Scientific name (character, non-empty)
#' @param timeout Timeout in seconds (default: 5s, must be > 0)
#' @return List with raw trait data
#' @export
lookup_fishbase_traits <- function(species_name, timeout = 5) {

  # Input validation
  if (missing(species_name) || is.null(species_name) || !is.character(species_name)) {
    stop("species_name must be a non-NULL character string", call. = FALSE)
  }
  if (length(species_name) != 1 || nchar(trimws(species_name)) == 0) {
    stop("species_name must be a single non-empty string", call. = FALSE)
  }
  if (!is.numeric(timeout) || timeout <= 0) {
    stop("timeout must be a positive number", call. = FALSE)
  }

  if (!requireNamespace("rfishbase", quietly = TRUE)) {
    warning("Package 'rfishbase' not installed. Install with: install.packages('rfishbase')")
    return(NULL)
  }

  result <- list(
    species = species_name,
    source = "FishBase",
    success = FALSE,
    traits = list()
  )

  tryCatch({
    # Get species data (let rfishbase handle its own timeout)
    species_data <- tryCatch({
      rfishbase::species(species_name)
    }, error = function(e) {
      # Handle connection errors gracefully
      if (grepl("open|connection|timeout|time limit", e$message, ignore.case = TRUE)) {
        message("      ⚠️  Connection error or timeout - FishBase may be unavailable")
        return(NULL)
      }
      # Other errors - return NULL but log them
      message("      ⚠️  Error: ", e$message)
      return(NULL)
    })

    if (is.null(species_data) || nrow(species_data) == 0) {
      result$note <- "Connection error, timeout, or species not found"
      return(result)
    }

    # Get morphology data (optional - don't fail if unavailable)
    morph_data <- tryCatch({
      rfishbase::morphology(species_name)
    }, error = function(e) NULL)

    # Get ecology data (optional - don't fail if unavailable)
    ecology_data <- tryCatch({
      rfishbase::ecology(species_name)
    }, error = function(e) NULL)

    # Extract relevant traits using safe_get() for consistent NULL/NA handling
    traits <- list()

    # Size (length in cm) - try Length first, then LengthFemale
    traits$max_length_cm <- coalesce_values(
      safe_get(species_data, "Length"),
      safe_get(species_data, "LengthFemale")
    )

    # Weight (convert kg to g)
    weight_kg <- safe_get(species_data, "Weight")
    if (is_valid_value(weight_kg)) {
      traits$max_weight_g <- weight_kg * 1000
    }

    # Trophic level
    traits$trophic_level <- safe_get(ecology_data, "FoodTroph")

    # Diet / Food items
    traits$diet_troph <- safe_get(ecology_data, "DietTroph")

    # Feeding type
    traits$feeding_type <- safe_get(ecology_data, "FeedingType")

    # Depth range
    traits$depth_min <- safe_get(species_data, "DepthRangeShallow")
    traits$depth_max <- safe_get(species_data, "DepthRangeDeep")

    # Habitat (convert to logical)
    neritic <- safe_get(ecology_data, "Neritic")
    if (is_valid_value(neritic)) traits$neritic <- as.logical(neritic)

    supralittoral <- safe_get(ecology_data, "SupraLittoralZone")
    if (is_valid_value(supralittoral)) traits$supralittoral <- as.logical(supralittoral)

    pelagic <- safe_get(ecology_data, "Pelagic")
    if (is_valid_value(pelagic)) traits$pelagic <- as.logical(pelagic)

    # Body shape (for mobility inference)
    traits$body_shape <- safe_get(morph_data, "BodyShapeI")

    # Remove NULL entries from traits list
    traits <- traits[!sapply(traits, is.null)]

    result$traits <- traits
    result$success <- length(traits) > 0

  }, error = function(e) {
    result$error <- conditionMessage(e)
  })

  return(result)
}


#' Lookup SeaLifeBase database traits
#'
#' @param species_name Scientific name
#' @param timeout Timeout in seconds (default: 10s)
#' @return List with raw trait data
#' @export
lookup_sealifebase_traits <- function(species_name, timeout = 10) {

  result <- list(
    species = species_name,
    source = "SeaLifeBase",
    success = FALSE,
    traits = list()
  )

  # Check if rfishbase is available
  if (!requireNamespace("rfishbase", quietly = TRUE)) {
    result$note <- "rfishbase package not installed. Install with: install.packages('rfishbase')"
    return(result)
  }

  tryCatch({
    # Query SeaLifeBase using rfishbase (let rfishbase handle its own timeout)
    species_data <- tryCatch({
      rfishbase::species(species_name, server = "sealifebase")
    }, error = function(e) {
      # Handle connection errors gracefully
      if (grepl("open|connection|timeout|time limit", e$message, ignore.case = TRUE)) {
        message("      ⚠️  Connection error or timeout - SeaLifeBase may be unavailable")
        return(NULL)
      }
      # Other errors - return NULL but log them
      message("      ⚠️  Error: ", e$message)
      return(NULL)
    })

    if (is.null(species_data) || nrow(species_data) == 0) {
      result$note <- "Connection error, timeout, or species not found in SeaLifeBase"
      return(result)
    }

    traits <- list()

    # Maximum length (cm) - check column exists
    if (!is.null(species_data) && "Length" %in% names(species_data)) {
      if (!is.na(species_data$Length)) {
        traits$max_length_cm <- species_data$Length
      }
    }

    # Weight (g) - check column exists
    if (!is.null(species_data) && "Weight" %in% names(species_data)) {
      if (!is.na(species_data$Weight)) {
        traits$weight_g <- species_data$Weight
      }
    }

    # Trophic level - check column exists
    if (!is.null(species_data) && "FoodTroph" %in% names(species_data)) {
      if (!is.na(species_data$FoodTroph)) {
        traits$trophic_level <- species_data$FoodTroph
      }
    }

    # Habitat (demersal, pelagic, benthic, etc.) - check column exists
    if (!is.null(species_data) && "DemersPelag" %in% names(species_data)) {
      if (!is.na(species_data$DemersPelag)) {
        traits$habitat <- species_data$DemersPelag
      }
    }

    # Try to get morphology data (optional - don't fail if unavailable)
    morph_data <- tryCatch({
      rfishbase::morphology(species_name, server = "sealifebase")
    }, error = function(e) NULL)

    # Body shape (for mobility inference) - check column exists
    if (!is.null(morph_data) && "BodyShapeI" %in% names(morph_data)) {
      if (!is.na(morph_data$BodyShapeI)) {
        traits$body_shape <- morph_data$BodyShapeI
      }
    }

    result$traits <- traits
    result$success <- length(traits) > 0

  }, error = function(e) {
    result$error <- conditionMessage(e)
  })

  return(result)
}


#' Lookup AlgaeBase database traits
#'
#' @param species_name Scientific name
#' @return List with raw trait data
#' @export
lookup_algaebase_traits <- function(species_name) {

  result <- list(
    species = species_name,
    source = "AlgaeBase",
    success = FALSE,
    traits = list()
  )

  # AlgaeBase does not have a public API as of 2025
  # This is a placeholder function for future implementation
  # Currently falls back to WoRMS for basic taxonomic classification

  tryCatch({
    # Check if worrms is available for fallback
    if (!requireNamespace("worrms", quietly = TRUE)) {
      result$note <- "AlgaeBase API not available. worrms package needed for taxonomic fallback."
      return(result)
    }

    # Use WoRMS to confirm it's an algae species
    worms_data <- worrms::wm_records_name(species_name, marine_only = FALSE)

    if (is.null(worms_data) || length(worms_data) == 0) {
      result$note <- "Species not found in WoRMS (AlgaeBase fallback)"
      return(result)
    }

    # Check if it's algae/phytoplankton
    phylum <- worms_data[[1]]$phylum
    class <- worms_data[[1]]$class

    # Comprehensive list of algae/phytoplankton phyla (case-insensitive matching)
    # Must match the routing logic in lookup_traits_for_species()
    algae_phyla <- c("Chlorophyta", "Rhodophyta", "Ochrophyta", "Phaeophyceae",
                     "Bacillariophyta", "Dinophyta", "Dinoflagellata", "Haptophyta",
                     "Cryptophyta", "Cyanobacteria", "Euglenozoa", "Charophyta",
                     "Myzozoa", "Miozoa")  # Myzozoa/Miozoa includes dinoflagellates

    if (tolower(phylum) %in% tolower(algae_phyla) || grepl("phyceae", class, ignore.case = TRUE)) {
      traits <- list(
        phylum = phylum,
        class = class,
        functional_group = "Phytoplankton",
        habitat = "pelagic",
        note = "Taxonomic classification from WoRMS (AlgaeBase API not available)"
      )

      result$traits <- traits
      result$success <- TRUE
    } else {
      result$note <- "Species found but not classified as algae"
    }

  }, error = function(e) {
    result$error <- conditionMessage(e)
  })

  return(result)
}


#' Lookup SHARK database traits (Swedish Ocean Archives)
#'
#' @param species_name Scientific name
#' @return List with raw trait data
#' @export
lookup_shark_traits <- function(species_name) {

  result <- list(
    species = species_name,
    source = "SHARK",
    success = FALSE,
    traits = list()
  )

  # Check if shark4r is available
  if (!requireNamespace("shark4r", quietly = TRUE)) {
    result$note <- "shark4r package not installed. Install with: remotes::install_github('sharksmhi/shark4r')"
    return(result)
  }

  tryCatch({
    # Use existing shark_api_utils.R functions if available
    # Otherwise use shark4r package directly

    # Query SHARK database for species occurrence data
    shark_data <- shark4r::get_datasets(
      datatype = "PhysicalChemical",
      scientificname = species_name
    )

    if (is.null(shark_data) || nrow(shark_data) == 0) {
      result$note <- "Species not found in SHARK database"
      return(result)
    }

    traits <- list()

    # Extract habitat from depth information
    if (!is.null(shark_data$sample_min_depth_m) && !is.na(shark_data$sample_min_depth_m)) {
      min_depth <- min(shark_data$sample_min_depth_m, na.rm = TRUE)
      max_depth <- max(shark_data$sample_max_depth_m, na.rm = TRUE)

      traits$depth_range_m <- c(min_depth, max_depth)

      # Infer habitat from depth
      if (max_depth < 50) {
        traits$habitat <- "coastal"
      } else if (max_depth < 200) {
        traits$habitat <- "shelf"
      } else {
        traits$habitat <- "deep"
      }
    }

    # Extract geographic distribution
    if (!is.null(shark_data$sample_latitude_dd)) {
      traits$latitude_range <- range(shark_data$sample_latitude_dd, na.rm = TRUE)
    }

    result$traits <- traits
    result$success <- length(traits) > 0

  }, error = function(e) {
    result$error <- conditionMessage(e)
  })

  return(result)
}


#' Lookup freshwaterecology.info database traits
#'
#' @param species_name Scientific name
#' @return List with raw trait data
#' @export
lookup_freshwaterecology_traits <- function(species_name) {

  result <- list(
    species = species_name,
    source = "freshwaterecology.info",
    success = FALSE,
    traits = list()
  )

  # Check if httr and jsonlite are available
  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    result$note <- "httr and jsonlite packages required. Install with: install.packages(c('httr', 'jsonlite'))"
    return(result)
  }

  tryCatch({
    # Get API key from config (loaded by app.R)
    api_key <- if (exists("get_api_key") && is.function(get_api_key)) {
      get_api_key("freshwaterecology_key")
    } else {
      ""
    }

    if (api_key == "") {
      result$note <- "freshwaterecology.info API key not configured"
      return(result)
    }

    # Construct API URL
    base_url <- "https://www.freshwaterecology.info/api/taxon"
    url <- paste0(base_url, "/", URLencode(species_name), "?key=", api_key)

    # Make API request
    response <- httr::GET(url)

    if (httr::http_error(response)) {
      result$note <- paste0("API request failed with status: ", httr::status_code(response))
      return(result)
    }

    # Parse JSON response
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content)

    if (is.null(data) || length(data) == 0) {
      result$note <- "Species not found in freshwaterecology.info"
      return(result)
    }

    # Extract traits using safe_get() for consistent NULL/NA handling
    traits <- list(
      max_length_mm = safe_get(data, "size_max"),
      feeding_type = safe_get(data, "feeding_type"),
      locomotion = safe_get(data, "locomotion"),
      habitat = safe_get(data, "habitat"),
      reproduction = safe_get(data, "reproduction")
    )

    # Remove NULL entries
    traits <- traits[!sapply(traits, is.null)]

    result$traits <- traits
    result$success <- length(traits) > 0

  }, error = function(e) {
    result$error <- conditionMessage(e)
  })

  return(result)
}


#' Lookup BIOTIC database traits
#'
#' @param species_name Scientific name
#' @param biotic_file Path to local BIOTIC CSV file (optional)
#' @return List with raw trait data
#' @export
lookup_biotic_traits <- function(species_name, biotic_file = NULL) {

  result <- list(
    species = species_name,
    source = "BIOTIC",
    success = FALSE,
    traits = list()
  )

  # Check for local BIOTIC file
  if (is.null(biotic_file)) {
    biotic_file <- file.path("data", "biotic_traits.csv")
  }

  if (!file.exists(biotic_file)) {
    result$error <- "BIOTIC file not found. Download from: https://www.dassh.ac.uk/lifehistory/"
    return(result)
  }

  tryCatch({
    # Read BIOTIC data
    biotic_data <- read.csv(biotic_file, stringsAsFactors = FALSE)

    # Find species (try exact match first, then partial)
    species_row <- biotic_data[biotic_data$Species == species_name, ]

    if (nrow(species_row) == 0) {
      # Try genus match
      genus <- strsplit(species_name, " ")[[1]][1]
      species_row <- biotic_data[grepl(paste0("^", genus), biotic_data$Species), ]
    }

    if (nrow(species_row) == 0) {
      return(result)
    }

    # Use first match if multiple
    if (nrow(species_row) > 1) {
      species_row <- species_row[1, ]
    }

    # Extract traits (column names may vary - adapt as needed)
    traits <- list()

    # Size
    if ("Max_Length_mm" %in% names(species_row) && !is.na(species_row$Max_Length_mm)) {
      traits$max_length_cm <- species_row$Max_Length_mm / 10
    }

    # Longevity
    if ("Longevity_years" %in% names(species_row) && !is.na(species_row$Longevity_years)) {
      traits$longevity_years <- species_row$Longevity_years
    }

    # Feeding mode
    if ("Feeding_mode" %in% names(species_row) && !is.na(species_row$Feeding_mode)) {
      traits$feeding_mode <- species_row$Feeding_mode
    }

    # Living habit
    if ("Living_habit" %in% names(species_row) && !is.na(species_row$Living_habit)) {
      traits$living_habit <- species_row$Living_habit
    }

    # Mobility
    if ("Mobility" %in% names(species_row) && !is.na(species_row$Mobility)) {
      traits$mobility <- species_row$Mobility
    }

    # Substratum affinity
    if ("Substratum" %in% names(species_row) && !is.na(species_row$Substratum)) {
      traits$substratum <- species_row$Substratum
    }

    # Skeleton type (for protection inference)
    if ("Skeleton" %in% names(species_row) && !is.na(species_row$Skeleton)) {
      traits$skeleton <- species_row$Skeleton
    }

    result$traits <- traits
    result$success <- length(traits) > 0

  }, error = function(e) {
    result$error <- conditionMessage(e)
  })

  return(result)
}


#' Lookup plankton traits from MAREDAT
#'
#' @param species_name Scientific name
#' @param maredat_file Path to local MAREDAT file
#' @return List with raw trait data
#' @export
lookup_maredat_traits <- function(species_name, maredat_file = NULL) {

  result <- list(
    species = species_name,
    source = "MAREDAT",
    success = FALSE,
    traits = list()
  )

  # Check for local MAREDAT file
  if (is.null(maredat_file)) {
    maredat_file <- file.path("data", "maredat_zooplankton.csv")
  }

  if (!file.exists(maredat_file)) {
    result$error <- "MAREDAT file not found. Download from: https://doi.pangaea.de/10.1594/PANGAEA.777398"
    return(result)
  }

  tryCatch({
    # Read MAREDAT data
    maredat_data <- read.csv(maredat_file, stringsAsFactors = FALSE)

    # Find species
    species_row <- maredat_data[maredat_data$Species == species_name, ]

    if (nrow(species_row) == 0) {
      # Try genus
      genus <- strsplit(species_name, " ")[[1]][1]
      species_row <- maredat_data[grepl(paste0("^", genus), maredat_data$Species), ]
    }

    if (nrow(species_row) == 0) {
      return(result)
    }

    # Use first match
    if (nrow(species_row) > 1) {
      species_row <- species_row[1, ]
    }

    traits <- list()

    # Body size (ESD - Equivalent Spherical Diameter in µm)
    if ("ESD_um" %in% names(species_row) && !is.na(species_row$ESD_um)) {
      traits$size_um <- species_row$ESD_um
      traits$max_length_cm <- species_row$ESD_um / 10000  # Convert µm to cm
    }

    # Taxonomic group (for feeding mode inference)
    if ("Group" %in% names(species_row) && !is.na(species_row$Group)) {
      traits$taxonomic_group <- species_row$Group
    }

    # Trophic level (if available)
    if ("Trophic_level" %in% names(species_row) && !is.na(species_row$Trophic_level)) {
      traits$trophic_level <- species_row$Trophic_level
    }

    result$traits <- traits
    result$success <- length(traits) > 0

  }, error = function(e) {
    result$error <- conditionMessage(e)
  })

  return(result)
}


# In-memory cache for PTDB data (loaded once, reused for all lookups)
.ptdb_cache <- new.env(parent = emptyenv())

#' Get or Load PTDB Data (Cached)
#'
#' Loads PTDB data from file on first call, returns cached data on subsequent calls.
#' @param ptdb_file Path to PTDB CSV file
#' @return data.frame with PTDB data, or NULL if file not found
#' @keywords internal
get_ptdb_data <- function(ptdb_file) {
  cache_key <- basename(ptdb_file)

  # Return cached data if available
  if (exists(cache_key, envir = .ptdb_cache)) {
    return(get(cache_key, envir = .ptdb_cache))
  }

  # Load and cache data
  if (!file.exists(ptdb_file)) {
    return(NULL)
  }

  ptdb_data <- read.csv(ptdb_file, stringsAsFactors = FALSE)
  assign(cache_key, ptdb_data, envir = .ptdb_cache)
  message("  [PTDB] Loaded and cached ", nrow(ptdb_data), " species from ", basename(ptdb_file))
  return(ptdb_data)
}

#' Lookup phytoplankton traits from PTDB
#'
#' @param species_name Scientific name
#' @param ptdb_file Path to local PTDB file
#' @return List with raw trait data
#' @export
lookup_ptdb_traits <- function(species_name, ptdb_file = NULL) {

  result <- list(
    species = species_name,
    source = "PTDB",
    success = FALSE,
    traits = list()
  )

  # Check for local PTDB file
  if (is.null(ptdb_file)) {
    ptdb_file <- file.path("data", "ptdb_phytoplankton.csv")
  }

  if (!file.exists(ptdb_file)) {
    result$error <- "PTDB file not found. Download from: https://github.com/simonabarbaglia/PTDB"
    return(result)
  }

  tryCatch({
    # Get PTDB data from cache (or load if first call)
    ptdb_data <- get_ptdb_data(ptdb_file)
    if (is.null(ptdb_data)) {
      result$error <- "Failed to load PTDB data"
      return(result)
    }

    # Find species - use case-insensitive matching
    species_lower <- tolower(trimws(species_name))
    species_col <- tolower(trimws(ptdb_data$Species))

    # Try exact match first (case-insensitive)
    species_row <- ptdb_data[species_col == species_lower, ]

    if (nrow(species_row) == 0) {
      # Try partial match (species name contains or is contained in database)
      species_row <- ptdb_data[grepl(species_lower, species_col, fixed = TRUE) |
                               grepl(species_col, species_lower, fixed = TRUE), ]
    }

    if (nrow(species_row) == 0) {
      # Try genus match
      genus <- tolower(strsplit(species_name, " ")[[1]][1])
      species_row <- ptdb_data[grepl(paste0("^", genus), species_col), ]
    }

    if (nrow(species_row) == 0) {
      return(result)
    }

    if (nrow(species_row) > 1) {
      species_row <- species_row[1, ]
    }

    traits <- list()

    # Cell size
    if ("Cell_volume_um3" %in% names(species_row) && !is.na(species_row$Cell_volume_um3)) {
      traits$cell_volume_um3 <- species_row$Cell_volume_um3
      # Approximate diameter from volume
      traits$size_um <- (6 * species_row$Cell_volume_um3 / pi)^(1/3)
      traits$max_length_cm <- traits$size_um / 10000
    }

    # Growth form
    if ("Growth_form" %in% names(species_row) && !is.na(species_row$Growth_form)) {
      traits$growth_form <- species_row$Growth_form
    }

    # Taxonomic class
    if ("Class" %in% names(species_row) && !is.na(species_row$Class)) {
      traits$taxonomic_class <- species_row$Class
    }

    # All phytoplankton are primary producers
    traits$trophic_level <- 1.0
    traits$feeding_mode <- "photosynthesis"

    result$traits <- traits
    result$success <- length(traits) > 0

  }, error = function(e) {
    result$error <- conditionMessage(e)
  })

  return(result)
}


#' Use WoRMS for taxonomic information and basic traits
#'
#' @param species_name Scientific name
#' @param timeout Timeout in seconds (default: 3s)
#' @return List with taxonomic and habitat information
#' @export
lookup_worms_traits <- function(species_name, timeout = 3) {

  result <- list(
    species = species_name,
    source = "WoRMS",
    success = FALSE,
    traits = list()
  )

  if (!requireNamespace("worrms", quietly = TRUE)) {
    warning("Package 'worrms' not installed. Install with: install.packages('worrms')")
    return(result)
  }

  # Clean species name (remove life stages, parentheticals)
  clean_name <- species_name
  clean_name <- trimws(clean_name)
  clean_name <- gsub("^(adult|juvenile|young|immature|larvae|larval)\\s+", "", clean_name, ignore.case = TRUE)
  clean_name <- gsub("\\s*\\([^)]+\\)\\s*", "", clean_name)  # Remove parentheticals
  clean_name <- trimws(clean_name)

  # Multiple query strategies
  query_attempts <- list()

  # Strategy 1: Exact match with cleaned name (fuzzy=FALSE, marine_only=FALSE)
  query_attempts[[1]] <- list(
    name = clean_name,
    fuzzy = FALSE,
    marine_only = FALSE,
    label = "exact match"
  )

  # Strategy 2: Fuzzy match with cleaned name (fuzzy=TRUE)
  query_attempts[[2]] <- list(
    name = clean_name,
    fuzzy = TRUE,
    marine_only = FALSE,
    label = "fuzzy match"
  )

  # Strategy 3: Genus only (for subspecies like "Mytilus edulis trossulus")
  if (grepl("\\s", clean_name)) {
    genus <- strsplit(clean_name, "\\s+")[[1]][1]
    query_attempts[[3]] <- list(
      name = genus,
      fuzzy = FALSE,
      marine_only = FALSE,
      label = "genus only"
    )
  }

  # Strategy 4: First two words only (for trinomial names)
  name_parts <- strsplit(clean_name, "\\s+")[[1]]
  if (length(name_parts) >= 3) {
    binomial <- paste(name_parts[1:2], collapse = " ")
    query_attempts[[4]] <- list(
      name = binomial,
      fuzzy = FALSE,
      marine_only = FALSE,
      label = "binomial (first 2 words)"
    )
  }

  # Try each strategy
  aphia_records <- NULL
  successful_strategy <- NULL

  for (i in seq_along(query_attempts)) {
    attempt <- query_attempts[[i]]
    if (is.null(attempt)) next  # Skip if strategy wasn't added

    tryCatch({
      aphia_records <- with_timeout(
        worrms::wm_records_name(
          name = attempt$name,
          fuzzy = attempt$fuzzy,
          marine_only = attempt$marine_only
        ),
        timeout = timeout,
        on_timeout = NULL
      )

      if (!is.null(aphia_records) && length(aphia_records) > 0) {
        successful_strategy <- attempt$label
        message("      → WoRMS: Found via ", attempt$label, " ('", attempt$name, "')")
        break
      }
    }, error = function(e) {
      # Continue to next strategy
      NULL
    })
  }

  # If still not found, return failure
  if (is.null(aphia_records) || (is.data.frame(aphia_records) && nrow(aphia_records) == 0)) {
    message("      → WoRMS: All strategies failed for '", species_name, "'")
    return(result)
  }

  tryCatch({
    # WoRMS returns a data.frame, extract first row
    if (is.data.frame(aphia_records)) {
      match <- aphia_records[1, ]
      aphia_id <- match$AphiaID
    } else {
      # Fallback for unexpected structure
      aphia_id <- aphia_records[[1]]$AphiaID
    }

    # Get full classification with timeout
    classification <- with_timeout(
      worrms::wm_classification(aphia_id),
      timeout = timeout,
      on_timeout = NULL
    )

    traits <- list()

    # Taxonomic information
    if (!is.null(classification) && nrow(classification) > 0) {
      traits$phylum <- classification$scientificname[classification$rank == "Phylum"]
      traits$class <- classification$scientificname[classification$rank == "Class"]
      traits$order <- classification$scientificname[classification$rank == "Order"]
      traits$family <- classification$scientificname[classification$rank == "Family"]

      # Handle multiple matches
      if (length(traits$phylum) > 1) traits$phylum <- traits$phylum[1]
      if (length(traits$class) > 1) traits$class <- traits$class[1]
      if (length(traits$order) > 1) traits$order <- traits$order[1]
      if (length(traits$family) > 1) traits$family <- traits$family[1]
    }

    # Marine/brackish/freshwater (handle data.frame structure)
    if (is.data.frame(aphia_records)) {
      traits$isMarine <- match$isMarine
      traits$isBrackish <- match$isBrackish
      traits$isFreshwater <- match$isFreshwater
      traits$scientificname <- match$scientificname
      traits$rank <- match$rank
    } else {
      traits$isMarine <- aphia_records[[1]]$isMarine
      traits$isBrackish <- aphia_records[[1]]$isBrackish
      traits$isFreshwater <- aphia_records[[1]]$isFreshwater
      traits$scientificname <- aphia_records[[1]]$scientificname
      traits$rank <- aphia_records[[1]]$rank
    }
    traits$aphia_id <- aphia_id

    # Get attributes from WoRMS Traits Portal (habitat, body size, etc.)
    attributes <- tryCatch(
      with_timeout(
        worrms::wm_attr_data(aphia_id),
        timeout = timeout,
        on_timeout = NULL
      ),
      error = function(e) NULL
    )

    if (!is.null(attributes) && nrow(attributes) > 0) {
      # Extract specific traits from WoRMS Traits Portal

      # 1. Body size - Extract all body size measurements
      body_size_rows <- attributes[grepl("body size", attributes$measurementType, ignore.case = TRUE), ]
      if (nrow(body_size_rows) > 0) {
        # Get numeric body size values
        sizes <- suppressWarnings(as.numeric(body_size_rows$measurementValue))
        sizes <- sizes[!is.na(sizes)]

        if (length(sizes) > 0) {
          # Take maximum body size
          traits$max_length_mm <- max(sizes)

          # Determine units from qualitative body size if available
          qual_size <- attributes$measurementValue[grepl("body size \\(qualitative\\)", attributes$measurementType, ignore.case = TRUE)]
          if (length(qual_size) > 0 && grepl("mm", qual_size[1])) {
            traits$max_length_cm <- traits$max_length_mm / 10
          } else {
            # Assume mm for invertebrates, cm for fish
            if (!is.null(traits$class) && tolower(traits$class) %in% c("actinopterygii", "actinopteri", "elasmobranchii")) {
              traits$max_length_cm <- traits$max_length_mm  # Already in cm for fish
            } else {
              traits$max_length_cm <- traits$max_length_mm / 10  # Convert mm to cm for invertebrates
            }
          }
        }
      }

      # 2. Functional group - Extract for habitat/environmental position
      func_group_rows <- attributes[grepl("functional group", attributes$measurementType, ignore.case = TRUE), ]
      if (nrow(func_group_rows) > 0) {
        traits$functional_group <- func_group_rows$measurementValue[1]

        # Extract habitat from functional group
        if (grepl("benthos", traits$functional_group, ignore.case = TRUE)) {
          traits$habitat <- "benthic"
        } else if (grepl("plankton", traits$functional_group, ignore.case = TRUE)) {
          traits$habitat <- "pelagic"
        }
      }

      # 3. AMBI ecological group - Disturbance tolerance (useful for EP inference)
      ambi_rows <- attributes[grepl("AMBI ecological group", attributes$measurementType, ignore.case = TRUE), ]
      if (nrow(ambi_rows) > 0) {
        traits$ambi_group <- ambi_rows$measurementValue[1]
      }

      # 4. Store all other attributes generically
      for (i in 1:nrow(attributes)) {
        attr_name <- tolower(gsub(" ", "_", attributes$measurementType[i]))
        if (!attr_name %in% c("body_size", "functional_group", "ambi_ecological_group")) {
          traits[[attr_name]] <- attributes$measurementValue[i]
        }
      }
    }

    result$traits <- traits
    result$success <- length(traits) > 0
    result$strategy <- successful_strategy

  }, error = function(e) {
    message("      → WoRMS: Error processing species '", species_name, "': ", conditionMessage(e))
    result$error <- conditionMessage(e)
  })

  return(result)
}


#' Lookup Ontology-Based Traits
#'
#' Retrieves fuzzy-coded trait data from the ontology traits database.
#' Uses standardized ontology vocabularies (ECO, FOODON, ENVO, PCO).
#'
#' @param species_name Scientific name
#' @param aphia_id WoRMS AphiaID (preferred for matching)
#' @param ontology_file Path to ontology traits CSV (default: data/ontology_traits.csv)
#' @return List with ontology trait data
#' @export
#' @examples
#' # By species name
#' traits <- lookup_ontology_traits("Macoma balthica")
#'
#' # By AphiaID (more reliable)
#' traits <- lookup_ontology_traits(aphia_id = 141579)
#'
#' # Access fuzzy trait scores
#' feeding_modes <- traits$traits[traits$traits$trait_category == "feeding", ]
lookup_ontology_traits <- function(species_name = NULL, aphia_id = NULL,
                                   ontology_file = "data/ontology_traits.csv") {

  result <- list(
    species = species_name,
    aphia_id = aphia_id,
    source = "Ontology",
    success = FALSE,
    traits = data.frame()
  )

  # Check if ontology file exists
  if (!file.exists(ontology_file)) {
    result$error <- paste("Ontology traits file not found:", ontology_file)
    return(result)
  }

  # Must provide either species_name or aphia_id
  if (is.null(species_name) && is.null(aphia_id)) {
    result$error <- "Must provide either species_name or aphia_id"
    return(result)
  }

  tryCatch({
    # Read ontology traits database
    ontology_data <- read.csv(ontology_file, stringsAsFactors = FALSE)

    # Match by AphiaID (preferred) or species name
    if (!is.null(aphia_id)) {
      traits <- ontology_data[ontology_data$aphia_id == aphia_id, ]

      # If no match by AphiaID and species name provided, try name
      if (nrow(traits) == 0 && !is.null(species_name)) {
        traits <- ontology_data[ontology_data$taxon_name == species_name, ]
      }
    } else {
      # Match by species name
      traits <- ontology_data[ontology_data$taxon_name == species_name, ]

      # Try fuzzy name matching if no exact match
      if (nrow(traits) == 0) {
        # Try genus-only match
        genus <- strsplit(species_name, " ")[[1]][1]
        traits <- ontology_data[grepl(paste0("^", genus, " "), ontology_data$taxon_name), ]

        # If still no match, try genus spp.
        if (nrow(traits) == 0) {
          genus_spp <- paste0(genus, " spp.")
          traits <- ontology_data[ontology_data$taxon_name == genus_spp, ]
        }
      }
    }

    if (nrow(traits) > 0) {
      result$traits <- traits
      result$success <- TRUE
      result$n_traits <- nrow(traits)

      # Extract unique AphiaID if found
      if (is.null(result$aphia_id) && length(unique(traits$aphia_id)) == 1) {
        result$aphia_id <- unique(traits$aphia_id)[1]
      }

      # Extract unique species name if found
      if (is.null(result$species) && length(unique(traits$taxon_name)) == 1) {
        result$species <- unique(traits$taxon_name)[1]
      }

      # Summarize trait categories
      result$trait_summary <- table(traits$trait_category)

    } else {
      result$error <- "Species not found in ontology traits database"
    }

  }, error = function(e) {
    result$error <- paste("Error reading ontology traits:", conditionMessage(e))
  })

  return(result)
}


#' Extract Primary Feeding Mode from Ontology Traits
#'
#' Determines the primary (highest-scored) feeding mode from fuzzy ontology data.
#'
#' @param ontology_traits Data frame from lookup_ontology_traits()
#' @return List with primary feeding mode, score, and ontology ID
#' @export
extract_primary_feeding <- function(ontology_traits) {

  if (is.null(ontology_traits) || nrow(ontology_traits) == 0) {
    return(list(modality = NA, score = NA, ontology_id = NA))
  }

  # Filter to feeding modes only
  feeding <- ontology_traits[
    ontology_traits$trait_category == "feeding" &
    ontology_traits$trait_name == "feeding_mode",
  ]

  if (nrow(feeding) == 0) {
    return(list(modality = NA, score = NA, ontology_id = NA))
  }

  # Find highest score
  max_score <- max(feeding$trait_score, na.rm = TRUE)
  primary <- feeding[feeding$trait_score == max_score, ][1, ]  # Take first if tie

  return(list(
    modality = primary$trait_modality,
    score = primary$trait_score,
    ontology_id = primary$ontology_id,
    source = primary$source
  ))
}


#' Get Fuzzy Trait Profile
#'
#' Extracts all trait modalities with scores for a specific trait category.
#'
#' @param ontology_traits Data frame from lookup_ontology_traits()
#' @param trait_category Category: "feeding", "habitat", "life_history"
#' @param trait_name Specific trait: "feeding_mode", "diet", "mobility", etc.
#' @return Data frame with modalities and scores
#' @export
get_fuzzy_profile <- function(ontology_traits, trait_category, trait_name = NULL) {

  if (is.null(ontology_traits) || nrow(ontology_traits) == 0) {
    return(data.frame())
  }

  # Filter by category
  profile <- ontology_traits[ontology_traits$trait_category == trait_category, ]

  # Further filter by trait name if specified
  if (!is.null(trait_name)) {
    profile <- profile[profile$trait_name == trait_name, ]
  }

  if (nrow(profile) == 0) {
    return(data.frame())
  }

  # Return relevant columns
  profile[, c("trait_name", "trait_modality", "trait_score", "ontology_id", "source", "notes")]
}


#' Harmonize Fuzzy Foraging Traits to Categorical FS Class
#'
#' Converts fuzzy-scored ontology feeding modes to categorical foraging strategy
#'
#' @param ontology_traits Data frame from ontology traits database
#' @return List with class, confidence, modalities, source
#' @export
#' @examples
#' result <- harmonize_fuzzy_foraging(ontology_traits)
#' # Returns: list(class = "FS5", confidence = "high", modalities = c("deposit:3", "suspension:2"))
harmonize_fuzzy_foraging <- function(ontology_traits) {

  result <- list(
    class = NA_character_,
    confidence = "none",
    modalities = character(),
    source = "Fuzzy"
  )

  if (is.null(ontology_traits) || nrow(ontology_traits) == 0) {
    return(result)
  }

  # Filter to feeding modes only
  feeding <- ontology_traits[
    ontology_traits$trait_category == "feeding" &
    ontology_traits$trait_name == "feeding_mode",
  ]

  if (nrow(feeding) == 0) {
    return(result)
  }

  # Find primary mode (highest score)
  max_score <- max(feeding$trait_score, na.rm = TRUE)
  primary <- feeding[feeding$trait_score == max_score, ][1, ]  # Take first if tie

  # Map ontology modality to FS class
  modality <- tolower(primary$trait_modality)

  fs_class <- NA_character_

  # FS0: Primary producer (photosynthesis, autotroph)
  if (grepl("photosyn|autotroph|producer|primary.producer", modality)) {
    fs_class <- "FS0"

  # FS1: Predator (active predation)
  } else if (grepl("predator|carnivore|piscivore|hunter", modality)) {
    fs_class <- "FS1"

  # FS2: Scavenger/Detritivore
  } else if (grepl("scavenger|detritivore|carrion", modality)) {
    fs_class <- "FS2"

  # FS3: Xylophagous (wood boring) - rare
  } else if (grepl("xylophag|wood.bor", modality)) {
    fs_class <- "FS3"

  # FS4: Grazer/Herbivore
  } else if (grepl("graz|herbivore|scraper|browser", modality)) {
    fs_class <- "FS4"

  # FS5: Deposit feeder (surface or subsurface)
  } else if (grepl("deposit|sediment.feeder|surface.deposit|subsurface.deposit", modality)) {
    fs_class <- "FS5"

  # FS6: Filter/Suspension feeder
  } else if (grepl("filter|suspension|planktivore|strain", modality)) {
    fs_class <- "FS6"
  }

  if (!is.na(fs_class)) {
    result$class <- fs_class

    # Determine confidence based on score and multi-modality
    secondary_modes <- feeding[feeding$trait_score >= 2 & feeding$trait_score < max_score, ]
    n_secondary <- nrow(secondary_modes)

    if (max_score == 3 && n_secondary == 0) {
      result$confidence <- "high"  # Single dominant mode
    } else if (max_score == 3 && n_secondary > 0) {
      result$confidence <- "medium"  # Multi-modal
    } else if (max_score == 2) {
      result$confidence <- "medium"  # Secondary mode only
    } else {
      result$confidence <- "low"  # Weak evidence
    }

    # Record all modalities with scores
    result$modalities <- paste0(feeding$trait_modality, ":", feeding$trait_score)
  }

  return(result)
}


#' Harmonize Fuzzy Mobility Traits to Categorical MB Class
#'
#' Converts fuzzy-scored ontology mobility to categorical mobility class
#'
#' @param ontology_traits Data frame from ontology traits database
#' @return List with class, confidence, modalities, source
#' @export
harmonize_fuzzy_mobility <- function(ontology_traits) {

  result <- list(
    class = NA_character_,
    confidence = "none",
    modalities = character(),
    source = "Fuzzy"
  )

  if (is.null(ontology_traits) || nrow(ontology_traits) == 0) {
    return(result)
  }

  # Filter to mobility/life_history traits
  mobility <- ontology_traits[
    ontology_traits$trait_category == "life_history" &
    ontology_traits$trait_name == "mobility",
  ]

  if (nrow(mobility) == 0) {
    return(result)
  }

  # Find primary mode (highest score)
  max_score <- max(mobility$trait_score, na.rm = TRUE)
  primary <- mobility[mobility$trait_score == max_score, ][1, ]

  # Map ontology modality to MB class
  modality <- tolower(primary$trait_modality)

  mb_class <- NA_character_

  # MB1: Sessile
  if (grepl("sessile|attached|fixed", modality)) {
    mb_class <- "MB1"

  # MB2: Burrower
  } else if (grepl("burrow|infauna|tube.dwell", modality)) {
    mb_class <- "MB2"

  # MB3: Crawler/Floater
  } else if (grepl("crawl|creep|walk|benthic.mobile|floater|drift", modality)) {
    mb_class <- "MB3"

  # MB4: Limited swimmer
  } else if (grepl("limited.swim|facultative.swim|weak.swim", modality)) {
    mb_class <- "MB4"

  # MB5: Swimmer
  } else if (grepl("swimmer|pelagic|nekt", modality)) {
    mb_class <- "MB5"
  }

  if (!is.na(mb_class)) {
    result$class <- mb_class

    # Determine confidence
    secondary_modes <- mobility[mobility$trait_score >= 2 & mobility$trait_score < max_score, ]

    if (max_score == 3 && nrow(secondary_modes) == 0) {
      result$confidence <- "high"
    } else if (max_score >= 2) {
      result$confidence <- "medium"
    } else {
      result$confidence <- "low"
    }

    result$modalities <- paste0(mobility$trait_modality, ":", mobility$trait_score)
  }

  return(result)
}


#' Harmonize Fuzzy Habitat Traits to Categorical EP Class
#'
#' Converts fuzzy-scored ontology habitat to environmental position class
#'
#' @param ontology_traits Data frame from ontology traits database
#' @return List with class, confidence, modalities, source
#' @export
harmonize_fuzzy_habitat <- function(ontology_traits) {

  result <- list(
    class = NA_character_,
    confidence = "none",
    modalities = character(),
    source = "Fuzzy"
  )

  if (is.null(ontology_traits) || nrow(ontology_traits) == 0) {
    return(result)
  }

  # Filter to habitat/zone traits
  habitat <- ontology_traits[
    ontology_traits$trait_category == "habitat" &
    ontology_traits$trait_name == "zone",
  ]

  if (nrow(habitat) == 0) {
    return(result)
  }

  # Find primary zone (highest score)
  max_score <- max(habitat$trait_score, na.rm = TRUE)
  primary <- habitat[habitat$trait_score == max_score, ][1, ]

  # Map ontology modality to EP class
  modality <- tolower(primary$trait_modality)

  ep_class <- NA_character_

  # EP1: Pelagic
  if (grepl("pelagic|water.column|planktonic", modality)) {
    ep_class <- "EP1"

  # EP2: Benthopelagic
  } else if (grepl("benthopel|demersal|near.bottom", modality)) {
    ep_class <- "EP2"

  # EP3: Benthic (subtidal/offshore)
  } else if (grepl("benthic|subtidal|offshore|deep", modality) &&
             !grepl("intertidal|tidal|littoral", modality)) {
    ep_class <- "EP3"

  # EP4: Intertidal/Tidal
  } else if (grepl("intertidal|tidal|littoral|eulittoral", modality)) {
    ep_class <- "EP4"
  }

  if (!is.na(ep_class)) {
    result$class <- ep_class

    # Determine confidence
    secondary_zones <- habitat[habitat$trait_score >= 2 & habitat$trait_score < max_score, ]

    if (max_score == 3 && nrow(secondary_zones) == 0) {
      result$confidence <- "high"
    } else if (max_score >= 2) {
      result$confidence <- "medium"
    } else {
      result$confidence <- "low"
    }

    result$modalities <- paste0(habitat$trait_modality, ":", habitat$trait_score)
  }

  return(result)
}


# ============================================================================
# HARMONIZATION HELPER FUNCTIONS
# ============================================================================

#' Check if Taxonomic Rule is Enabled
#'
#' @param rule_name String name of rule (e.g., "fish_obligate_swimmers")
#' @return Boolean TRUE/FALSE
is_rule_enabled <- function(rule_name) {
  # HARMONIZATION_CONFIG loaded at file init or by app.R
  if (!exists("HARMONIZATION_CONFIG")) {
    stop("HARMONIZATION_CONFIG not loaded - source trait_lookup.R from app root", call. = FALSE)
  }
  rule <- HARMONIZATION_CONFIG$taxonomic_rules[[rule_name]]
  return(isTRUE(rule))
}


#' Apply Ecosystem Profile Size Adjustment
#'
#' @param size_cm Raw size in cm
#' @return Adjusted size in cm based on active ecosystem profile
apply_size_adjustment <- function(size_cm) {
  # HARMONIZATION_CONFIG loaded at file init or by app.R
  if (!exists("HARMONIZATION_CONFIG")) {
    return(size_cm)  # No adjustment if config missing
  }

  active_profile_name <- HARMONIZATION_CONFIG$active_profile %||% "temperate"

  if (active_profile_name == "temperate") {
    return(size_cm)  # No adjustment for temperate (baseline)
  }

  profile <- HARMONIZATION_CONFIG$profiles[[active_profile_name]]
  multiplier <- profile$size_multiplier %||% 1.0

  return(size_cm * multiplier)
}


#' Get Pattern from Configuration
#'
#' @param pattern_name String name (e.g., "MB1_sessile", "FS1_predator")
#' @param pattern_type Type: "mobility", "foraging", "environmental", "protection"
#' @return Regular expression pattern string, or NULL if not found
get_config_pattern <- function(pattern_name, pattern_type = "mobility") {
  # HARMONIZATION_CONFIG loaded at file init or by app.R
  if (!exists("HARMONIZATION_CONFIG")) {
    return(NULL)
  }

  pattern_list <- switch(pattern_type,
    "mobility" = HARMONIZATION_CONFIG$mobility_patterns,
    "foraging" = HARMONIZATION_CONFIG$foraging_patterns,
    "environmental" = HARMONIZATION_CONFIG$environmental_patterns,
    "protection" = HARMONIZATION_CONFIG$protection_patterns,
    NULL
  )

  pattern_list[[pattern_name]]  # Returns NULL if not found
}


# ============================================================================
# TRAIT HARMONIZATION (Raw → Categorical Classes)
# ============================================================================

#' Convert size measurements to MS size class
#'
#' @param size_cm Maximum body length in cm
#' @return MS code (MS1-MS7)
#' @export
harmonize_size_class <- function(size_cm) {

  if (is.null(size_cm) || is.na(size_cm)) {
    return(NA)
  }

  # Get thresholds from configuration
  thresh <- HARMONIZATION_CONFIG$size_thresholds

  # Apply ecosystem profile adjustment
  size_adjusted <- apply_size_adjustment(size_cm)

  # Size class thresholds (configurable, default following Olivier et al.)
  # MS1: < thresh$MS1_MS2 - microplankton, bacteria
  # MS2: thresh$MS1_MS2 to thresh$MS2_MS3 - mesoplankton, small invertebrates
  # MS3: thresh$MS2_MS3 to thresh$MS3_MS4 - small fish, large invertebrates
  # MS4: thresh$MS3_MS4 to thresh$MS4_MS5 - medium fish, crabs
  # MS5: thresh$MS4_MS5 to thresh$MS5_MS6 - large fish
  # MS6: thresh$MS5_MS6 to thresh$MS6_MS7 - very large fish
  # MS7: >= thresh$MS6_MS7 - marine mammals, large sharks

  if (size_adjusted < thresh$MS1_MS2) {
    return("MS1")
  } else if (size_adjusted < thresh$MS2_MS3) {
    return("MS2")
  } else if (size_adjusted < thresh$MS3_MS4) {
    return("MS3")
  } else if (size_adjusted < thresh$MS4_MS5) {
    return("MS4")
  } else if (size_adjusted < thresh$MS5_MS6) {
    return("MS5")
  } else if (size_adjusted < thresh$MS6_MS7) {
    return("MS6")
  } else {
    return("MS7")
  }
}


#' Convert feeding mode/type to FS foraging strategy
#'
#' @param feeding_info Character vector with feeding information
#' @param trophic_level Numeric trophic level (if available)
#' @return FS code (FS0-FS6)
#' @export
harmonize_foraging_strategy <- function(feeding_info = NULL, trophic_level = NULL) {

  # Default based on trophic level
  if (!is.null(trophic_level) && !is.na(trophic_level)) {
    if (trophic_level < 1.5) {
      return("FS0")  # Primary producer
    }
  }

  if (is.null(feeding_info) || all(is.na(feeding_info))) {
    # Default: predator if TL > 2, else filter feeder
    if (!is.null(trophic_level) && !is.na(trophic_level) && trophic_level > 2.5) {
      return("FS1")  # Predator
    }
    return("FS6")  # Filter feeder (conservative)
  }

  # Convert to lowercase for matching
  feeding_lower <- tolower(paste(feeding_info, collapse = " "))

  # Get patterns from configuration
  patterns <- HARMONIZATION_CONFIG$foraging_patterns

  # Pattern matching (using configurable patterns)
  if (grepl(patterns$FS0_primary_producer, feeding_lower, ignore.case = TRUE)) {
    return("FS0")  # None (primary producer)
  }

  if (grepl(patterns$FS1_predator, feeding_lower, ignore.case = TRUE)) {
    return("FS1")  # Predator
  }

  if (grepl(patterns$FS2_scavenger, feeding_lower, ignore.case = TRUE)) {
    return("FS2")  # Scavenger
  }

  if (grepl(patterns$FS3_omnivore, feeding_lower, ignore.case = TRUE)) {
    return("FS3")  # Omnivore
  }

  if (grepl(patterns$FS4_grazer, feeding_lower, ignore.case = TRUE)) {
    return("FS4")  # Grazer
  }

  if (grepl(patterns$FS5_deposit, feeding_lower, ignore.case = TRUE)) {
    return("FS5")  # Deposit feeder
  }

  if (grepl(patterns$FS6_filter, feeding_lower, ignore.case = TRUE)) {
    return("FS6")  # Filter feeder
  }

  # Default based on trophic level if no match
  if (!is.null(trophic_level) && !is.na(trophic_level)) {
    if (trophic_level > 3.0) {
      return("FS1")  # Predator
    } else if (trophic_level > 2.0) {
      return("FS1")  # Predator
    } else {
      return("FS6")  # Filter feeder
    }
  }

  # Conservative default
  return("FS6")
}


#' Convert mobility information to MB class
#'
#' @param mobility_info Character vector with mobility information
#' @param body_shape Body shape code (for fish)
#' @param taxonomic_info Taxonomic classification
#' @return MB code (MB1-MB5)
#' @export
harmonize_mobility <- function(mobility_info = NULL, body_shape = NULL, taxonomic_info = NULL) {

  if (!is.null(mobility_info)) {
    mobility_lower <- tolower(paste(mobility_info, collapse = " "))

    # Get patterns from configuration
    pattern_sessile <- get_config_pattern("MB1_sessile", "mobility")
    pattern_burrower <- get_config_pattern("MB2_burrower", "mobility")
    pattern_crawler <- get_config_pattern("MB3_crawler", "mobility")
    pattern_swimmer_limited <- get_config_pattern("MB4_swimmer_limited", "mobility")
    pattern_swimmer <- get_config_pattern("MB5_swimmer", "mobility")

    if (!is.null(pattern_sessile) && grepl(pattern_sessile, mobility_lower, ignore.case = TRUE)) {
      return("MB1")  # Sessile
    }

    if (!is.null(pattern_burrower) && grepl(pattern_burrower, mobility_lower, ignore.case = TRUE)) {
      return("MB2")  # Burrower
    }

    if (!is.null(pattern_crawler) && grepl(pattern_crawler, mobility_lower, ignore.case = TRUE)) {
      return("MB3")  # Crawler
    }

    if (!is.null(pattern_swimmer_limited) && grepl(pattern_swimmer_limited, mobility_lower, ignore.case = TRUE)) {
      return("MB4")  # Limited swimmer
    }

    if (!is.null(pattern_swimmer) && grepl(pattern_swimmer, mobility_lower, ignore.case = TRUE)) {
      return("MB5")  # Obligate swimmer
    }
  }

  # Use taxonomic inference (configurable rules)
  if (!is.null(taxonomic_info)) {
    phylum <- taxonomic_info$phylum
    class <- taxonomic_info$class

    # Fish are typically obligate swimmers (if rule enabled)
    if (is_rule_enabled("fish_obligate_swimmers")) {
      if (!is.null(class) && grepl("Actinopteri|Elasmobranchii|Teleostei", class)) {
        return("MB5")
      }
    }

    # Molluscs
    if (!is.null(phylum) && phylum == "Mollusca") {
      if (!is.null(class)) {
        # Bivalves sessile (if rule enabled)
        if (class == "Bivalvia" && is_rule_enabled("bivalves_sessile")) {
          return("MB1")
        }
        # Cephalopods swimmers (if rule enabled)
        if (class == "Cephalopoda" && is_rule_enabled("cephalopods_swimmers")) {
          return("MB5")
        }
        if (class == "Gastropoda") return("MB3")  # Snails crawl
      }
    }

    # Arthropods
    if (!is.null(phylum) && phylum == "Arthropoda") {
      if (!is.null(class)) {
        if (grepl("Copepoda", class)) return("MB5")  # Copepods swim
        if (grepl("Malacostraca", class)) return("MB4")  # Crabs/shrimp
      }
    }

    # Cnidarians (jellyfish) - sessile if rule enabled
    if (!is.null(phylum) && phylum == "Cnidaria") {
      if (is_rule_enabled("cnidarians_sessile")) {
        return("MB1")
      }
      return("MB2")  # Passive floaters
    }

    # Porifera (sponges)
    if (!is.null(phylum) && phylum == "Porifera") {
      return("MB1")  # Sessile
    }
  }

  # Default: facultative swimmer
  return("MB4")
}


#' Convert habitat/depth information to EP environmental position
#'
#' @param depth_min Minimum depth (m)
#' @param depth_max Maximum depth (m)
#' @param habitat_info Character vector with habitat information
#' @param taxonomic_info Taxonomic classification
#' @return EP code (EP1-EP4)
#' @export
harmonize_environmental_position <- function(depth_min = NULL, depth_max = NULL,
                                            habitat_info = NULL, taxonomic_info = NULL) {

  # Use habitat information if available
  if (!is.null(habitat_info)) {
    habitat_lower <- tolower(paste(habitat_info, collapse = " "))

    if (grepl("infauna|buried|sediment interior", habitat_lower)) {
      return("EP1")  # Infaunal
    }

    if (grepl("epibenthic|epifauna|benthic surface|bottom", habitat_lower)) {
      return("EP2")  # Epibenthic
    }

    if (grepl("benthopelagic|demersal|near.bottom", habitat_lower)) {
      return("EP3")  # Benthopelagic
    }

    if (grepl("pelagic|planktonic|surface|midwater", habitat_lower)) {
      return("EP4")  # Pelagic
    }
  }

  # Use depth range
  if (!is.null(depth_min) && !is.null(depth_max)) {
    avg_depth <- (depth_min + depth_max) / 2

    # Very shallow species are likely epibenthic or infaunal
    if (avg_depth < 50) {
      # Check if burrowing
      if (!is.null(habitat_info) && grepl("burrow", tolower(paste(habitat_info, collapse = " ")))) {
        return("EP1")
      }
      return("EP2")
    }

    # Deep species often benthopelagic
    if (avg_depth > 200) {
      return("EP3")
    }
  }

  # Taxonomic inference (configurable rules)
  if (!is.null(taxonomic_info)) {
    phylum <- taxonomic_info$phylum
    class <- taxonomic_info$class

    # Phytoplankton (if rule enabled)
    if (is_rule_enabled("phytoplankton_pelagic")) {
      if (!is.null(taxonomic_info$feeding_mode) &&
          grepl("photosyn", tolower(taxonomic_info$feeding_mode))) {
        return("EP4")  # Pelagic (need light)
      }
      # Also check for phytoplankton classes
      if (!is.null(class) && grepl("Bacillariophyceae|Dinophyceae|Prymnesiophyceae", class)) {
        return("EP4")
      }
    }

    # Zooplankton (if rule enabled)
    if (is_rule_enabled("zooplankton_pelagic")) {
      if (!is.null(class) && grepl("Copepoda|Cladocera", class)) {
        return("EP4")  # Pelagic
      }
    }

    # Many molluscs are epibenthic or infaunal
    if (!is.null(phylum) && phylum == "Mollusca") {
      if (!is.null(class) && class == "Bivalvia") {
        # Some bivalves are infaunal (if rule enabled)
        if (is_rule_enabled("infaunal_bivalves")) {
          return("EP1")
        }
      }
    }

    # Fish - depends on species but default pelagic
    if (!is.null(class) && grepl("Actinopteri|Teleostei", class)) {
      return("EP4")  # Default pelagic for fish
    }
  }

  # Default: epibenthic (conservative)
  return("EP2")
}


#' Convert protection information to PR code
#'
#' @param skeleton_info Skeleton/protection information
#' @param taxonomic_info Taxonomic classification
#' @return PR code (PR0, PR2, PR3, PR5-PR8)
#' @export
harmonize_protection <- function(skeleton_info = NULL, taxonomic_info = NULL) {

  if (!is.null(skeleton_info)) {
    skeleton_lower <- tolower(paste(skeleton_info, collapse = " "))

    if (grepl("none|soft|naked", skeleton_lower)) {
      return("PR0")  # No protection
    }

    if (grepl("tube", skeleton_lower)) {
      return("PR2")  # Tube
    }

    if (grepl("burrow", skeleton_lower)) {
      return("PR3")  # Burrow
    }

    if (grepl("thin.*shell|soft.*shell|weak.*shell", skeleton_lower)) {
      return("PR5")  # Soft shell
    }

    if (grepl("shell|calcareous|calcium", skeleton_lower)) {
      return("PR6")  # Hard shell
    }

    if (grepl("spine|spiny|setae", skeleton_lower)) {
      return("PR7")  # Few spines
    }

    if (grepl("armou?r|exoskeleton|carapace|heavily", skeleton_lower)) {
      return("PR8")  # Armoured
    }
  }

  # Taxonomic inference (configurable rules)
  if (!is.null(taxonomic_info)) {
    phylum <- taxonomic_info$phylum
    class <- taxonomic_info$class

    # Molluscs
    if (!is.null(phylum) && phylum == "Mollusca") {
      if (!is.null(class)) {
        # Bivalves hard shell (if rule enabled)
        if (class == "Bivalvia" && is_rule_enabled("bivalves_hard_shell")) {
          return("PR6")
        }
        # Gastropods hard shell (if rule enabled)
        if (class == "Gastropoda" && is_rule_enabled("gastropods_hard_shell")) {
          return("PR6")
        }
        if (class == "Cephalopoda") return("PR0")  # Soft-bodied
      }
      # Default hard shell for molluscs (if any shell rule enabled)
      if (is_rule_enabled("bivalves_hard_shell") || is_rule_enabled("gastropods_hard_shell")) {
        return("PR6")
      }
    }

    # Arthropods
    if (!is.null(phylum) && phylum == "Arthropoda") {
      # Crustaceans exoskeleton (if rule enabled)
      if (is_rule_enabled("crustaceans_exoskeleton")) {
        if (!is.null(class) && grepl("Malacostraca", class)) {
          return("PR8")  # Crabs/lobsters are armoured
        }
        return("PR4")  # Exoskeleton for crustaceans
      }
      return("PR5")  # Soft exoskeleton for small arthropods
    }

    # Echinoderms (if rule enabled)
    if (!is.null(phylum) && phylum == "Echinodermata") {
      if (is_rule_enabled("echinoderms_calcium_plates")) {
        return("PR5")  # Calcium plates
      }
      return("PR7")  # Spiny
    }

    # Cnidarians
    if (!is.null(phylum) && phylum == "Cnidaria") {
      return("PR0")  # Soft-bodied
    }

    # Annelids
    if (!is.null(phylum) && phylum == "Annelida") {
      # Check if tube-dwelling
      if (!is.null(taxonomic_info$living_habit) &&
          grepl("tube", tolower(taxonomic_info$living_habit))) {
        return("PR2")
      }
      return("PR0")  # Soft-bodied
    }

    # Fish
    if (!is.null(class) && grepl("Actinopteri|Teleostei", class)) {
      return("PR0")  # No hard protection
    }

    # Porifera
    if (!is.null(phylum) && phylum == "Porifera") {
      return("PR7")  # Spicules
    }
  }

  # Default: no protection
  return("PR0")
}

# ============================================================================
# HIERARCHICAL WORKFLOW ORCHESTRATOR
# ============================================================================

#' Automated trait lookup using hierarchical database workflow
#'
#' @param species_name Scientific name
#' @param biotic_file Path to BIOTIC CSV (optional)
#' @param maredat_file Path to MAREDAT CSV (optional)
#' @param ptdb_file Path to PTDB CSV (optional)
#' @param cache_dir Directory for caching results (optional)
#' @return Data frame with harmonized trait codes
#' @export
lookup_species_traits <- function(species_name,
                                  biotic_file = NULL,
                                  maredat_file = NULL,
                                  ptdb_file = NULL,
                                  cache_dir = NULL) {

  # Check cache first
  if (!is.null(cache_dir) && dir.exists(cache_dir)) {
    cache_file <- file.path(cache_dir, paste0(gsub(" ", "_", species_name), ".rds"))
    if (file.exists(cache_file)) {
      cached <- readRDS(cache_file)
      if (Sys.time() - cached$timestamp < as.difftime(30, units = "days")) {
        message("Using cached traits for ", species_name)
        return(cached$traits)
      }
    }
  }

  message("\n╔═══════════════════════════════════════════════════════════════╗")
  message("║ TRAIT LOOKUP: ", species_name, paste(rep(" ", max(0, 47 - nchar(species_name))), collapse = ""), "║")
  message("╠═══════════════════════════════════════════════════════════════╣")
  message("║ Target Traits:                                                 ║")
  message("║   MS (Max Size) - Size class (MS1-MS7)                        ║")
  message("║   FS (Foraging Strategy) - Feeding mode (FS0-FS6)             ║")
  message("║   MB (Mobility) - Movement capability (MB1-MB5)               ║")
  message("║   EP (Environmental Position) - Depth/habitat (EP1-EP4)       ║")
  message("║   PR (Predator Resistance) - Protection (PR1-PR3)             ║")
  message("╚═══════════════════════════════════════════════════════════════╝")

  # Initialize result
  result <- data.frame(
    species = species_name,
    MS = NA_character_,
    FS = NA_character_,
    MB = NA_character_,
    EP = NA_character_,
    PR = NA_character_,
    source = NA_character_,
    confidence = NA_character_,
    stringsAsFactors = FALSE
  )

  # Collect raw traits from all sources
  raw_traits <- list()
  sources_used <- character()

  # Helper function to check if we have all needed raw data
  check_completeness <- function() {
    has_size <- !is.null(size_cm)
    has_feeding <- length(feeding_mode) > 0 || !is.null(trophic_level)
    has_mobility <- length(mobility_info) > 0 || !is.null(body_shape)
    has_habitat <- length(habitat_info) > 0 || !is.null(depth_min)
    has_protection <- length(protection_info) > 0

    list(
      complete = has_size && has_feeding && has_mobility && has_habitat,
      has_size = has_size,
      has_feeding = has_feeding,
      has_mobility = has_mobility,
      has_habitat = has_habitat,
      has_protection = has_protection
    )
  }

  # Start timing
  total_start <- Sys.time()

  # 1. WoRMS (taxonomy + basic habitat)
  message("\n[1/10] 🌊 WoRMS - Taxonomic classification...")
  db_start <- Sys.time()
  worms_data <- lookup_worms_traits(species_name)
  db_time <- round(as.numeric(difftime(Sys.time(), db_start, units = "secs")), 2)

  if (worms_data$success) {
    raw_traits$worms <- worms_data$traits
    sources_used <- c(sources_used, "WoRMS")
    message("  ✓ SUCCESS (", db_time, "s)")
    message("    → Phylum: ", raw_traits$worms$phylum)
    message("    → Class: ", raw_traits$worms$class)
    message("    → Order: ", raw_traits$worms$order)

    # Check if body size was extracted from WoRMS Traits Portal
    if (!is.null(raw_traits$worms$max_length_cm) && !is.na(raw_traits$worms$max_length_cm)) {
      message("    → Max Length: ", raw_traits$worms$max_length_cm, " cm (from WoRMS Traits Portal) [→ MS]")
    } else {
      message("    → Provides: Taxonomic context for inference rules")
    }
  } else {
    message("  ✗ FAILED (", db_time, "s) - Species not found in WoRMS")
  }

  # 2. Ontology Traits (fuzzy-scored semantic traits)
  message("\n[2/10] 🏷️  Ontology - Fuzzy trait profiles...")
  db_start <- Sys.time()

  # Try to lookup by AphiaID if available, otherwise by name
  aphia_id_for_lookup <- if (worms_data$success && !is.null(raw_traits$worms$aphia_id)) {
    raw_traits$worms$aphia_id
  } else {
    NULL
  }

  ontology_data <- lookup_ontology_traits(
    species_name = species_name,
    aphia_id = aphia_id_for_lookup
  )
  db_time <- round(as.numeric(difftime(Sys.time(), db_start, units = "secs")), 2)

  if (ontology_data$success) {
    raw_traits$ontology <- ontology_data$traits
    sources_used <- c(sources_used, "Ontology")
    message("  ✓ SUCCESS (", db_time, "s)")
    message("    → Trait records: ", ontology_data$n_traits)

    # Summarize trait categories
    if (length(ontology_data$trait_summary) > 0) {
      for (cat_name in names(ontology_data$trait_summary)) {
        message("    → ", cat_name, ": ", ontology_data$trait_summary[[cat_name]], " modalities")
      }
    }

    # Extract primary feeding mode if available
    primary_feeding <- extract_primary_feeding(ontology_data$traits)
    if (!is.na(primary_feeding$modality)) {
      message("    → Primary feeding: ", primary_feeding$modality,
              " (score=", primary_feeding$score, ") [→ FS]")
    }
  } else {
    message("  ✗ SKIPPED (", db_time, "s) - Species not in ontology database")
  }

  # Initialize variables for trait merging
  # Start with WoRMS Traits Portal data if available
  size_cm <- NULL
  if (worms_data$success && !is.null(raw_traits$worms$max_length_cm)) {
    size_cm <- raw_traits$worms$max_length_cm
  }
  trophic_level <- NULL
  feeding_mode <- character()
  mobility_info <- character()
  habitat_info <- character()
  if (worms_data$success && !is.null(raw_traits$worms$habitat)) {
    habitat_info <- c(habitat_info, raw_traits$worms$habitat)
  }
  if (worms_data$success && !is.null(raw_traits$worms$functional_group)) {
    habitat_info <- c(habitat_info, raw_traits$worms$functional_group)
  }
  protection_info <- character()
  body_shape <- NULL
  depth_min <- NULL
  depth_max <- NULL

  # ═══════════════════════════════════════════════════════════════════════
  # TAXONOMIC PRE-FILTERING - Smart database routing
  # ═══════════════════════════════════════════════════════════════════════

  # Determine which databases to query based on taxonomy
  query_fishbase <- FALSE
  query_sealifebase <- FALSE
  query_biotic <- FALSE
  query_bvol <- FALSE
  query_species_enriched <- FALSE
  query_freshwater <- FALSE
  query_maredat <- FALSE
  query_ptdb <- FALSE
  query_algaebase <- FALSE
  query_shark <- FALSE

  if (worms_data$success) {
    phylum <- tolower(raw_traits$worms$phylum)
    class <- tolower(raw_traits$worms$class)

    message("\n🎯 SMART ROUTING based on taxonomy...")

    # Fish → FishBase only
    if (phylum == "chordata" && class %in% c("actinopterygii", "actinopteri", "elasmobranchii",
                                              "holocephali", "myxini", "petromyzonti",
                                              "teleostei", "chondrichthyes", "osteichthyes")) {
      query_fishbase <- TRUE
      message("  → Detected FISH (", class, ") → Querying: FishBase")
      message("  → Skipping: SeaLifeBase, BIOTIC, MAREDAT, PTDB, AlgaeBase")

    # Marine invertebrates → SeaLifeBase + SpeciesEnriched + BIOTIC
    } else if (phylum %in% c("mollusca", "arthropoda", "annelida", "echinodermata",
                             "cnidaria", "porifera", "platyhelminthes", "nematoda",
                             "bryozoa", "brachiopoda", "nemertea", "sipuncula")) {
      query_sealifebase <- TRUE
      query_species_enriched <- TRUE
      query_biotic <- TRUE
      query_shark <- TRUE  # May have occurrence data
      message("  → Detected MARINE INVERTEBRATE (", phylum, ") → Querying: SeaLifeBase, SpeciesEnriched, BIOTIC, SHARK")
      message("  → Skipping: FishBase, freshwater, MAREDAT, PTDB, AlgaeBase")

    # Zooplankton → MAREDAT + SeaLifeBase
    } else if (class %in% c("copepoda", "cladocera", "ostracoda", "mysida", "euphausiacea",
                            "chaetognatha", "appendicularia", "thaliacea")) {
      query_maredat <- TRUE
      query_sealifebase <- TRUE
      message("  → Detected ZOOPLANKTON (", class, ") → Querying: MAREDAT, SeaLifeBase")
      message("  → Skipping: FishBase, BIOTIC, PTDB, AlgaeBase")

    # Phytoplankton / Algae → BVOL + PTDB + AlgaeBase + freshwater (if not marine)
    } else if (phylum %in% c("chlorophyta", "ochrophyta", "rhodophyta", "cyanobacteria",
                             "bacillariophyta", "dinoflagellata", "haptophyta", "cryptophyta",
                             "euglenozoa", "charophyta", "myzozoa", "miozoa") ||
               grepl("phyceae", class)) {
      query_bvol <- TRUE
      query_ptdb <- TRUE
      query_algaebase <- TRUE
      # Also query freshwater if species is not marine (freshwater phytoplankton)
      is_marine <- raw_traits$worms$isMarine
      if (!is.null(is_marine) && is_marine == FALSE) {
        query_freshwater <- TRUE
        message("  → Detected FRESHWATER PHYTOPLANKTON/ALGAE (", phylum, ") → Querying: BVOL, PTDB, AlgaeBase, freshwater")
      } else {
        message("  → Detected PHYTOPLANKTON/ALGAE (", phylum, ") → Querying: BVOL, PTDB, AlgaeBase")
      }
      message("  → Skipping: FishBase, SeaLifeBase, BIOTIC, MAREDAT")

    # Freshwater taxa → freshwaterecology.info
    } else if (class %in% c("oligochaeta", "hirudinea", "gastropoda") &&
               !is.null(raw_traits$worms$isMarine) && raw_traits$worms$isMarine == FALSE) {
      query_freshwater <- TRUE
      query_biotic <- TRUE
      message("  → Detected FRESHWATER (", class, ") → Querying: freshwaterecology.info, BIOTIC")
      message("  → Skipping: FishBase, SeaLifeBase, MAREDAT, PTDB, AlgaeBase")

    # Unknown/ambiguous → Query invertebrate databases
    } else {
      query_sealifebase <- TRUE
      query_biotic <- TRUE
      query_freshwater <- TRUE
      message("  ⚠️  Ambiguous taxonomy (", phylum, "/", class, ") → Querying: SeaLifeBase, BIOTIC, freshwater")
      message("  → Skipping: FishBase (not a fish), MAREDAT, PTDB, AlgaeBase")
    }

  } else {
    # WoRMS failed → Try all databases (fallback mode)
    message("\n⚠️  WoRMS FAILED - FALLBACK MODE: Trying all databases")
    message("   (This will be slow - consider improving species name matching)")
    query_fishbase <- TRUE
    query_sealifebase <- TRUE
    query_biotic <- TRUE
    query_freshwater <- TRUE
    query_maredat <- TRUE
    query_ptdb <- TRUE
    query_algaebase <- TRUE
    query_shark <- TRUE
  }

  message("")  # Blank line

  # 2. FishBase (for fish) - ONLY IF TAXONOMICALLY APPROPRIATE
  if (query_fishbase) {
    message("\n[3/10] 🐟 FishBase - Fish morphology & ecology...")
    db_start <- Sys.time()
    fishbase_data <- lookup_fishbase_traits(species_name)
    db_time <- round(as.numeric(difftime(Sys.time(), db_start, units = "secs")), 2)

    if (fishbase_data$success) {
      raw_traits$fishbase <- fishbase_data$traits
      sources_used <- c(sources_used, "FishBase")
      message("  ✓ SUCCESS (", db_time, "s)")

      if (!is.null(fishbase_data$traits$max_length_cm)) {
        size_cm <- fishbase_data$traits$max_length_cm
        message("    → Max Length: ", size_cm, " cm [→ MS]")
      }
      if (!is.null(fishbase_data$traits$trophic_level)) {
        trophic_level <- fishbase_data$traits$trophic_level
        message("    → Trophic Level: ", trophic_level, " [→ FS]")
      }
      if (!is.null(fishbase_data$traits$feeding_type)) {
        feeding_mode <- c(feeding_mode, fishbase_data$traits$feeding_type)
        message("    → Feeding Type: ", fishbase_data$traits$feeding_type, " [→ FS]")
      }
      if (!is.null(fishbase_data$traits$body_shape)) {
        body_shape <- fishbase_data$traits$body_shape
        message("    → Body Shape: ", body_shape, " [→ MB]")
      }
      if (!is.null(fishbase_data$traits$depth_min)) {
        depth_min <- fishbase_data$traits$depth_min
        depth_max <- fishbase_data$traits$depth_max
        message("    → Depth Range: ", depth_min, "-", depth_max, " m [→ EP]")
      }
      message("    → Provides: HIGH-PRIORITY data for MS, FS, MB, EP")
    } else {
      message("  ✗ FAILED (", db_time, "s) - Not found in FishBase")
    }
  } else {
    message("\n[3/10] 🐟 FishBase - SKIPPED (not a fish based on taxonomy)")
  }

  # Check if we already have everything we need
  completeness <- check_completeness()
  if (completeness$complete) {
    message("\n⚡ EARLY EXIT: All required raw data collected!")
    message("   Skipping remaining databases to save time...")
    # Skip to harmonization
  } else {

  # 3. SeaLifeBase (for marine invertebrates)
  if (query_sealifebase) {
    message("\n[4/10] 🐚 SeaLifeBase - Marine invertebrate traits...")
    db_start <- Sys.time()
    sealifebase_data <- lookup_sealifebase_traits(species_name)
    db_time <- round(as.numeric(difftime(Sys.time(), db_start, units = "secs")), 2)

    if (sealifebase_data$success) {
      raw_traits$sealifebase <- sealifebase_data$traits
      sources_used <- c(sources_used, "SeaLifeBase")
      message("  ✓ SUCCESS (", db_time, "s)")

      if (is.null(size_cm) && !is.null(sealifebase_data$traits$max_length_cm)) {
        size_cm <- sealifebase_data$traits$max_length_cm
        message("    → Max Length: ", size_cm, " cm [→ MS]")
      }
      if (is.null(trophic_level) && !is.null(sealifebase_data$traits$trophic_level)) {
        trophic_level <- sealifebase_data$traits$trophic_level
        message("    → Trophic Level: ", trophic_level, " [→ FS]")
      }
      message("    → Provides: MEDIUM-PRIORITY data for MS, FS, MB")
    } else {
      message("  ✗ FAILED (", db_time, "s) - Not found in SeaLifeBase")
    }
  } else {
    message("\n[4/10] 🐚 SeaLifeBase - SKIPPED (not a marine invertebrate based on taxonomy)")
  }

  # 4. BIOTIC (for invertebrates)
  if (query_biotic) {
    message("\n[5/10] 🦀 BIOTIC - Benthic invertebrate biological traits...")
    db_start <- Sys.time()
    biotic_data <- lookup_biotic_traits(species_name, biotic_file)
    db_time <- round(as.numeric(difftime(Sys.time(), db_start, units = "secs")), 2)

    if (biotic_data$success) {
      raw_traits$biotic <- biotic_data$traits
      sources_used <- c(sources_used, "BIOTIC")
      message("  ✓ SUCCESS (", db_time, "s)")

      if (!is.null(biotic_data$traits$mobility)) {
        mobility_info <- c(mobility_info, biotic_data$traits$mobility)
        message("    → Mobility: ", biotic_data$traits$mobility, " [→ MB]")
      }
      if (!is.null(biotic_data$traits$feeding_mode)) {
        feeding_mode <- c(feeding_mode, biotic_data$traits$feeding_mode)
        message("    → Feeding Mode: ", biotic_data$traits$feeding_mode, " [→ FS]")
      }
      if (!is.null(biotic_data$traits$living_habit)) {
        habitat_info <- c(habitat_info, biotic_data$traits$living_habit)
        message("    → Living Habit: ", biotic_data$traits$living_habit, " [→ EP]")
      }
      message("    → Provides: Categorical traits for MB, FS, EP, PR")
    } else {
      message("  ✗ FAILED (", db_time, "s) - Not in local BIOTIC database")
    }
  } else {
    message("\n[5/10] 🦀 BIOTIC - SKIPPED (not applicable based on taxonomy)")
  }

  # 5. BVOL (for phytoplankton)
  if (query_bvol) {
    message("\n[6/10] 🌿 BVOL - Phytoplankton biovolume traits...")
    db_start <- Sys.time()

    # Source local databases if not already loaded
    if (!exists("lookup_bvol_traits")) {
      source("R/functions/local_trait_databases.R")
    }

    # Lookup by AphiaID (preferred) with species name as fallback
    bvol_traits <- lookup_bvol_traits(
      aphia_id = aphia_id_for_lookup,
      species_name = species_name
    )

    db_time <- round(as.numeric(difftime(Sys.time(), db_start, units = "secs")), 2)

    if (!is.null(bvol_traits)) {
      # Harmonize traits
      harmonized_bvol <- harmonize_bvol_traits(bvol_traits)

      # Merge into raw_traits
      raw_traits$bvol <- c(bvol_traits, harmonized_bvol)
      sources_used <- c(sources_used, "BVOL")
      message("  ✓ SUCCESS (", db_time, "s)")

      if (!is.null(bvol_traits$size_cm)) {
        if (is.null(size_cm)) {
          size_cm <- bvol_traits$size_cm
        }
        message("    → Size: ", sprintf("%.6f", bvol_traits$size_cm), " cm (from biovolume) [→ MS]")
      }
      if (!is.null(bvol_traits$trophy)) {
        message("    → Trophy: ", bvol_traits$trophy, " [→ FS]")
      }
      message("    → Harmonized: MS=", harmonized_bvol$MS, ", FS=", harmonized_bvol$FS,
              ", MB=", harmonized_bvol$MB, ", EP=", harmonized_bvol$EP, ", PR=", harmonized_bvol$PR)
      message("    → Confidence: ", sprintf("%.2f", harmonized_bvol$overall_confidence))
    } else {
      message("  ✗ FAILED (", db_time, "s) - Not in local BVOL database")
    }
  } else {
    message("\n[6/10] 🌿 BVOL - SKIPPED (not phytoplankton based on taxonomy)")
  }

  # 6. SpeciesEnriched (for marine invertebrates)
  if (query_species_enriched) {
    message("\n[7/10] 🦑 SpeciesEnriched - Marine invertebrate traits...")
    db_start <- Sys.time()

    # Source local databases if not already loaded
    if (!exists("lookup_species_enriched_traits")) {
      source("R/functions/local_trait_databases.R")
    }

    # Lookup by AphiaID (preferred) with species name as fallback
    enriched_traits <- lookup_species_enriched_traits(
      aphia_id = aphia_id_for_lookup,
      species_name = species_name
    )

    db_time <- round(as.numeric(difftime(Sys.time(), db_start, units = "secs")), 2)

    if (!is.null(enriched_traits)) {
      # Harmonize traits
      harmonized_enriched <- harmonize_species_enriched_traits(enriched_traits)

      # Merge into raw_traits
      raw_traits$species_enriched <- c(enriched_traits, harmonized_enriched)
      sources_used <- c(sources_used, "SpeciesEnriched")
      message("  ✓ SUCCESS (", db_time, "s)")

      if (!is.null(enriched_traits$size_cm)) {
        if (is.null(size_cm)) {
          size_cm <- enriched_traits$size_cm
        }
        message("    → Size: ", sprintf("%.2f", enriched_traits$size_cm), " cm [→ MS]")
      }
      if (!is.null(enriched_traits$mobility)) {
        mobility_info <- c(mobility_info, enriched_traits$mobility)
        message("    → Mobility: ", enriched_traits$mobility, " [→ MB]")
      }
      if (!is.null(enriched_traits$feeding_method)) {
        feeding_mode <- c(feeding_mode, enriched_traits$feeding_method)
        message("    → Feeding: ", enriched_traits$feeding_method, " [→ FS]")
      }
      message("    → Harmonized: MS=", harmonized_enriched$MS, ", FS=", harmonized_enriched$FS,
              ", MB=", harmonized_enriched$MB, ", EP=", harmonized_enriched$EP, ", PR=", harmonized_enriched$PR)
      message("    → Confidence: ", sprintf("%.2f", harmonized_enriched$overall_confidence))
    } else {
      message("  ✗ FAILED (", db_time, "s) - Not in local SpeciesEnriched database")
    }
  } else {
    message("\n[7/10] 🦑 SpeciesEnriched - SKIPPED (not marine invertebrate based on taxonomy)")
  }

  # 7. freshwaterecology.info (for freshwater species)
  if (query_freshwater) {
    message("\n[8/12] 🌊 freshwaterecology.info - Freshwater species...")
    db_start <- Sys.time()
    freshwater_data <- lookup_freshwaterecology_traits(species_name)
    db_time <- round(as.numeric(difftime(Sys.time(), db_start, units = "secs")), 2)

    if (freshwater_data$success) {
      raw_traits$freshwater <- freshwater_data$traits
      sources_used <- c(sources_used, "freshwaterecology.info")
      message("  ✓ SUCCESS (", db_time, "s)")

      if (is.null(size_cm) && !is.null(freshwater_data$traits$max_length_mm)) {
        size_cm <- freshwater_data$traits$max_length_mm / 10
        message("    → Max Length: ", freshwater_data$traits$max_length_mm, " mm (", size_cm, " cm) [→ MS]")
      }
      if (!is.null(freshwater_data$traits$locomotion)) {
        mobility_info <- c(mobility_info, freshwater_data$traits$locomotion)
        message("    → Locomotion: ", freshwater_data$traits$locomotion, " [→ MB]")
      }
      message("    → Provides: Data for MS, FS, MB, EP")
    } else {
      message("  ✗ FAILED (", db_time, "s) - Not a freshwater species or API issue")
    }
  } else {
    message("\n[8/12] 🌊 freshwaterecology.info - SKIPPED (not freshwater based on taxonomy)")
  }

  # 8. MAREDAT (for zooplankton)
  if (query_maredat) {
    message("\n[9/12] 🦐 MAREDAT - Zooplankton traits...")
    db_start <- Sys.time()
    maredat_data <- lookup_maredat_traits(species_name, maredat_file)
    db_time <- round(as.numeric(difftime(Sys.time(), db_start, units = "secs")), 2)

    if (maredat_data$success) {
      raw_traits$maredat <- maredat_data$traits
      sources_used <- c(sources_used, "MAREDAT")
      message("  ✓ SUCCESS (", db_time, "s)")
      message("    → Provides: Zooplankton size and feeding data [→ MS, FS]")
    } else {
      message("  ✗ FAILED (", db_time, "s) - Not in local MAREDAT database")
    }
  } else {
    message("\n[9/12] 🦐 MAREDAT - SKIPPED (not zooplankton based on taxonomy)")
  }

  # 9. PTDB (for phytoplankton)
  if (query_ptdb) {
    message("\n[10/12] 🌿 PTDB - Phytoplankton traits...")
    db_start <- Sys.time()
    ptdb_data <- lookup_ptdb_traits(species_name, ptdb_file)
    db_time <- round(as.numeric(difftime(Sys.time(), db_start, units = "secs")), 2)

    if (ptdb_data$success) {
      raw_traits$ptdb <- ptdb_data$traits
      sources_used <- c(sources_used, "PTDB")
      message("  ✓ SUCCESS (", db_time, "s)")
      message("    → Provides: Phytoplankton cell size [→ MS, FS=FS0]")
    } else {
      message("  ✗ FAILED (", db_time, "s) - Not in local PTDB database")
    }
  } else {
    message("\n[10/12] 🌿 PTDB - SKIPPED (not phytoplankton based on taxonomy)")
  }

  # 10. AlgaeBase (for algae/phytoplankton)
  if (query_algaebase) {
    message("\n[11/12] 🌱 AlgaeBase - Algae taxonomy...")
    db_start <- Sys.time()
    algaebase_data <- lookup_algaebase_traits(species_name)
    db_time <- round(as.numeric(difftime(Sys.time(), db_start, units = "secs")), 2)

    if (algaebase_data$success) {
      raw_traits$algaebase <- algaebase_data$traits
      sources_used <- c(sources_used, "AlgaeBase")
      message("  ✓ SUCCESS (", db_time, "s)")
      message("    → Provides: Algae identification [→ FS=FS0, EP]")
    } else {
      message("  ✗ FAILED (", db_time, "s) - Not an algae or not in AlgaeBase")
    }
  } else {
    message("\n[11/12] 🌱 AlgaeBase - SKIPPED (not algae based on taxonomy)")
  }

  # 11. SHARK (for Swedish waters species)
  if (query_shark) {
    message("\n[12/12] 🦈 SHARK - Swedish Ocean Archives...")
    db_start <- Sys.time()
    shark_data <- lookup_shark_traits(species_name)
    db_time <- round(as.numeric(difftime(Sys.time(), db_start, units = "secs")), 2)

    if (shark_data$success) {
      raw_traits$shark <- shark_data$traits
      sources_used <- c(sources_used, "SHARK")
      message("  ✓ SUCCESS (", db_time, "s)")

      if (is.null(depth_min) && !is.null(shark_data$traits$depth_range_m)) {
        depth_min <- shark_data$traits$depth_range_m[1]
        depth_max <- shark_data$traits$depth_range_m[2]
        message("    → Depth Range: ", depth_min, "-", depth_max, " m [→ EP]")
      }
      message("    → Provides: Geographic/depth data [→ EP]")
    } else {
      message("  ✗ FAILED (", db_time, "s) - Not in Swedish waters")
    }
  } else {
    message("\n[12/12] 🦈 SHARK - SKIPPED (no Swedish waters data needed)")
  }

  } # End of else block for early exit

  # If no data found
  if (length(raw_traits) == 0) {
    message("\n❌ NO DATA FOUND - No databases contained information for this species")
    result$confidence <- "none"
    total_time <- round(as.numeric(difftime(Sys.time(), total_start, units = "secs")), 2)
    message("\n⏱️  Total lookup time: ", total_time, "s")
    return(result)
  }

  # Collect additional traits that weren't already collected during database queries
  # These are collected from raw_traits to handle the early exit case

  # Update feeding_mode if not collected yet
  if (length(feeding_mode) == 0) {
    feeding_mode <- c(
      raw_traits$fishbase$feeding_type,
      raw_traits$sealifebase$feeding_type,
      raw_traits$biotic$feeding_mode,
      raw_traits$freshwater$feeding_type,
      raw_traits$ptdb$feeding_mode
    )
  }

  # Update mobility_info if not collected yet
  if (length(mobility_info) == 0) {
    mobility_info <- c(
      raw_traits$biotic$mobility,
      raw_traits$biotic$living_habit,
      raw_traits$freshwater$locomotion
    )
  }

  # Update habitat_info if not collected yet
  if (length(habitat_info) == 0) {
    habitat_info <- c(
      raw_traits$fishbase$habitat,
      raw_traits$sealifebase$habitat,
      raw_traits$biotic$living_habit,
      raw_traits$biotic$substratum,
      raw_traits$freshwater$habitat,
      raw_traits$shark$habitat,
      raw_traits$algaebase$habitat
    )
  }

  # Update protection_info if not collected yet
  if (length(protection_info) == 0) {
    protection_info <- c(
      raw_traits$biotic$skeleton
    )
  }

  # HARMONIZATION PHASE
  message("\n╔═══════════════════════════════════════════════════════════════╗")
  message("║ HARMONIZATION - Converting raw data to categorical traits     ║")
  message("╚═══════════════════════════════════════════════════════════════╝")

  # 1. MS - Max Size Class
  message("\n[MS] Max Size Class:")
  if (!is.null(size_cm)) {
    message("  📏 Input: ", size_cm, " cm")
    result$MS <- harmonize_size_class(size_cm)
    message("  ✓ Output: ", result$MS)
    if (size_cm < 0.1) message("     (< 0.1 cm = Tiny - MS1)")
    else if (size_cm < 1.0) message("     (0.1-1 cm = Very Small - MS2)")
    else if (size_cm < 5.0) message("     (1-5 cm = Small - MS3)")
    else if (size_cm < 20.0) message("     (5-20 cm = Medium - MS4)")
    else if (size_cm < 50.0) message("     (20-50 cm = Large - MS5)")
    else if (size_cm < 150.0) message("     (50-150 cm = Very Large - MS6)")
    else message("     (> 150 cm = Giant - MS7)")
  } else {
    message("  ❌ No size data available")
    result$MS <- NA_character_
  }

  # 2. FS - Foraging Strategy
  message("\n[FS] Foraging Strategy:")
  if (length(feeding_mode) > 0 || !is.null(trophic_level)) {
    if (!is.null(trophic_level)) message("  🍴 Trophic Level: ", trophic_level)
    if (length(feeding_mode) > 0) message("  🍴 Feeding Modes: ", paste(feeding_mode, collapse = ", "))
    result$FS <- harmonize_foraging_strategy(feeding_mode, trophic_level)
    message("  ✓ Output: ", result$FS)
    fs_labels <- c("FS0"="Primary Producer", "FS1"="Predator", "FS2"="Scavenger",
                   "FS3"="Omnivore", "FS4"="Grazer", "FS5"="Deposit Feeder", "FS6"="Filter Feeder")
    if (!is.na(result$FS) && result$FS %in% names(fs_labels)) {
      message("     (", fs_labels[result$FS], ")")
    }
  } else {
    # Try fuzzy harmonization from ontology traits
    if (!is.null(raw_traits$ontology)) {
      message("  🔍 Trying fuzzy harmonization from ontology...")
      fuzzy_fs <- harmonize_fuzzy_foraging(raw_traits$ontology)
      if (!is.na(fuzzy_fs$class)) {
        result$FS <- fuzzy_fs$class
        sources_used <- c(sources_used, "Fuzzy")
        message("  ✓ Output: ", result$FS, " (from fuzzy ontology, confidence=", fuzzy_fs$confidence, ")")
        fs_labels <- c("FS0"="Primary Producer", "FS1"="Predator", "FS2"="Scavenger",
                       "FS3"="Omnivore", "FS4"="Grazer", "FS5"="Deposit Feeder", "FS6"="Filter Feeder")
        if (result$FS %in% names(fs_labels)) {
          message("     (", fs_labels[result$FS], ")")
        }
        message("     Modalities: ", paste(fuzzy_fs$modalities, collapse = ", "))
      } else {
        message("  ❌ No feeding/trophic data available (including ontology)")
        result$FS <- NA_character_
      }
    } else {
      message("  ❌ No feeding/trophic data available")
      result$FS <- NA_character_
    }
  }

  # 3. MB - Mobility
  message("\n[MB] Mobility:")
  if (length(mobility_info) > 0 || !is.null(body_shape)) {
    if (!is.null(body_shape)) message("  🏊 Body Shape: ", body_shape)
    if (length(mobility_info) > 0) message("  🏊 Mobility Info: ", paste(mobility_info, collapse = ", "))
    result$MB <- harmonize_mobility(mobility_info, body_shape, raw_traits$worms)
    message("  ✓ Output: ", result$MB)
    mb_labels <- c("MB1"="Sessile", "MB2"="Limited Movement", "MB3"="Floater/Drifter",
                   "MB4"="Crawler/Walker", "MB5"="Swimmer")
    if (!is.na(result$MB) && result$MB %in% names(mb_labels)) {
      message("     (", mb_labels[result$MB], ")")
    }
  } else {
    # Try fuzzy harmonization from ontology traits
    if (!is.null(raw_traits$ontology)) {
      message("  🔍 Trying fuzzy harmonization from ontology...")
      fuzzy_mb <- harmonize_fuzzy_mobility(raw_traits$ontology)
      if (!is.na(fuzzy_mb$class)) {
        result$MB <- fuzzy_mb$class
        sources_used <- c(sources_used, "Fuzzy")
        message("  ✓ Output: ", result$MB, " (from fuzzy ontology, confidence=", fuzzy_mb$confidence, ")")
        mb_labels <- c("MB1"="Sessile", "MB2"="Burrower", "MB3"="Crawler",
                       "MB4"="Limited Swimmer", "MB5"="Swimmer")
        if (result$MB %in% names(mb_labels)) {
          message("     (", mb_labels[result$MB], ")")
        }
        message("     Modalities: ", paste(fuzzy_mb$modalities, collapse = ", "))
      } else {
        message("  ❌ No mobility data available (including ontology)")
        result$MB <- NA_character_
      }
    } else {
      message("  ❌ No mobility data available")
      result$MB <- NA_character_
    }
  }

  # 4. EP - Environmental Position
  message("\n[EP] Environmental Position:")
  if (!is.null(depth_min) || length(habitat_info) > 0) {
    if (!is.null(depth_min)) message("  🌊 Depth Range: ", depth_min, "-", depth_max, " m")
    if (length(habitat_info) > 0) message("  🌊 Habitat Info: ", paste(habitat_info, collapse = ", "))
    result$EP <- harmonize_environmental_position(depth_min, depth_max, habitat_info, raw_traits$worms)
    message("  ✓ Output: ", result$EP)
    ep_labels <- c("EP1"="Pelagic", "EP2"="Epibenthic", "EP3"="Endobenthic", "EP4"="Interstitial")
    if (!is.na(result$EP) && result$EP %in% names(ep_labels)) {
      message("     (", ep_labels[result$EP], ")")
    }
  } else {
    # Try fuzzy harmonization from ontology traits
    if (!is.null(raw_traits$ontology)) {
      message("  🔍 Trying fuzzy harmonization from ontology...")
      fuzzy_ep <- harmonize_fuzzy_habitat(raw_traits$ontology)
      if (!is.na(fuzzy_ep$class)) {
        result$EP <- fuzzy_ep$class
        sources_used <- c(sources_used, "Fuzzy")
        message("  ✓ Output: ", result$EP, " (from fuzzy ontology, confidence=", fuzzy_ep$confidence, ")")
        ep_labels <- c("EP1"="Pelagic", "EP2"="Benthopelagic", "EP3"="Benthic", "EP4"="Intertidal")
        if (result$EP %in% names(ep_labels)) {
          message("     (", ep_labels[result$EP], ")")
        }
        message("     Modalities: ", paste(fuzzy_ep$modalities, collapse = ", "))
      } else {
        message("  ❌ No depth/habitat data available (including ontology)")
        result$EP <- NA_character_
      }
    } else {
      message("  ❌ No depth/habitat data available")
      result$EP <- NA_character_
    }
  }

  # 5. PR - Predator Resistance
  message("\n[PR] Predator Resistance:")
  if (length(protection_info) > 0) {
    message("  🛡️  Protection Info: ", paste(protection_info, collapse = ", "))
    result$PR <- harmonize_protection(protection_info, raw_traits$worms)
    message("  ✓ Output: ", result$PR)
    pr_labels <- c("PR1"="Soft-bodied", "PR2"="Moderately Protected", "PR3"="Well Protected")
    if (!is.na(result$PR) && result$PR %in% names(pr_labels)) {
      message("     (", pr_labels[result$PR], ")")
    }
  } else {
    message("  🛡️  Using taxonomic inference from WoRMS")
    result$PR <- harmonize_protection(protection_info, raw_traits$worms)
    message("  ✓ Output: ", result$PR)
  }

  # ═══════════════════════════════════════════════════════════════════════
  # ML FALLBACK - Predict missing traits using Random Forest models
  # ═══════════════════════════════════════════════════════════════════════

  # Check if there are missing traits and we have taxonomic data
  missing_traits <- c(
    if (is.na(result$MS)) "MS",
    if (is.na(result$FS)) "FS",
    if (is.na(result$MB)) "MB",
    if (is.na(result$EP)) "EP",
    if (is.na(result$PR)) "PR"
  )

  if (length(missing_traits) > 0 && !is.null(raw_traits$worms)) {
    message("\n╔═══════════════════════════════════════════════════════════════╗")
    message("║ ML FALLBACK - Predicting missing traits                       ║")
    message("╚═══════════════════════════════════════════════════════════════╝")
    message("\n🤖 Attempting ML prediction for: ", paste(missing_traits, collapse = ", "))

    # Source ML prediction functions if not already loaded
    if (!exists("apply_ml_fallback")) {
      ml_file <- "R/functions/ml_trait_prediction.R"
      if (file.exists(ml_file)) {
        source(ml_file, local = TRUE)
      } else {
        message("  ⚠️  ML prediction functions not found at: ", ml_file)
      }
    }

    # Apply ML predictions if function is available
    if (exists("apply_ml_fallback")) {
      tryCatch({
        # Prepare harmonized traits for ML prediction
        harmonized_for_ml <- list(
          MS = result$MS,
          FS = result$FS,
          MB = result$MB,
          EP = result$EP,
          PR = result$PR
        )

        # Apply ML fallback (function defined in ml_trait_prediction.R)
        result_with_ml <- apply_ml_fallback(harmonized_for_ml, raw_traits, verbose = TRUE)

        # Update result with ML predictions
        if (!is.na(result_with_ml$MS) && is.na(result$MS)) {
          result$MS <- result_with_ml$MS
          sources_used <- c(sources_used, "ML")
        }
        if (!is.na(result_with_ml$FS) && is.na(result$FS)) {
          result$FS <- result_with_ml$FS
          sources_used <- c(sources_used, "ML")
        }
        if (!is.na(result_with_ml$MB) && is.na(result$MB)) {
          result$MB <- result_with_ml$MB
          sources_used <- c(sources_used, "ML")
        }
        if (!is.na(result_with_ml$EP) && is.na(result$EP)) {
          result$EP <- result_with_ml$EP
          sources_used <- c(sources_used, "ML")
        }
        if (!is.na(result_with_ml$PR) && is.na(result$PR)) {
          result$PR <- result_with_ml$PR
          sources_used <- c(sources_used, "ML")
        }

        # Store ML metadata if available
        for (trait in c("MS", "FS", "MB", "EP", "PR")) {
          conf_field <- paste0(trait, "_ml_confidence")
          prob_field <- paste0(trait, "_ml_probability")
          if (!is.null(result_with_ml[[conf_field]])) {
            result[[conf_field]] <- result_with_ml[[conf_field]]
          }
          if (!is.null(result_with_ml[[prob_field]])) {
            result[[prob_field]] <- result_with_ml[[prob_field]]
          }
        }

      }, error = function(e) {
        message("  ⚠️  ML prediction error: ", e$message)
      })
    }
  }

  # =================================================================
  # UNCERTAINTY QUANTIFICATION - Calculate probabilistic confidence
  # =================================================================

  # Source uncertainty quantification functions
  if (!exists("calculate_all_trait_confidence")) {
    tryCatch({
      source("R/functions/uncertainty_quantification.R", local = TRUE)
    }, error = function(e) {
      message("  ⚠️  Uncertainty quantification not available: ", e$message)
    })
  }

  # Calculate confidence for all traits
  if (exists("calculate_all_trait_confidence")) {
    message("\n╔═══════════════════════════════════════════════════════════════╗")
    message("║ UNCERTAINTY QUANTIFICATION                                     ║")
    message("╚═══════════════════════════════════════════════════════════════╝")

    # Prepare trait record with sources and raw values
    trait_record <- list(
      MS = result$MS,
      size_cm = size_cm,
      MS_source = result$MS_source,
      MS_ml_probability = result$MS_ml_probability,

      FS = result$FS,
      FS_source = result$FS_source,
      FS_ml_probability = result$FS_ml_probability,

      MB = result$MB,
      MB_source = result$MB_source,
      MB_ml_probability = result$MB_ml_probability,

      EP = result$EP,
      EP_source = result$EP_source,
      EP_ml_probability = result$EP_ml_probability,

      PR = result$PR,
      PR_source = result$PR_source,
      PR_ml_probability = result$PR_ml_probability
    )

    # Calculate confidence for all traits
    confidence_data <- calculate_all_trait_confidence(trait_record)

    # Merge confidence data into result
    for (field in names(confidence_data)) {
      result[[field]] <- confidence_data[[field]]
    }

    # Display confidence summary
    if (!is.null(result$MS_confidence)) {
      message("  MS confidence: ", round(result$MS_confidence * 100, 1), "% (",
              result$MS_confidence_category, ")")
    }
    if (!is.null(result$FS_confidence)) {
      message("  FS confidence: ", round(result$FS_confidence * 100, 1), "% (",
              result$FS_confidence_category, ")")
    }
    if (!is.null(result$MB_confidence)) {
      message("  MB confidence: ", round(result$MB_confidence * 100, 1), "% (",
              result$MB_confidence_category, ")")
    }
    if (!is.null(result$EP_confidence)) {
      message("  EP confidence: ", round(result$EP_confidence * 100, 1), "% (",
              result$EP_confidence_category, ")")
    }
    if (!is.null(result$PR_confidence)) {
      message("  PR confidence: ", round(result$PR_confidence * 100, 1), "% (",
              result$PR_confidence_category, ")")
    }

    # Calculate overall confidence (geometric mean)
    confidence_values <- c(
      result$MS_confidence, result$FS_confidence, result$MB_confidence,
      result$EP_confidence, result$PR_confidence
    )
    valid_confidence <- confidence_values[!is.na(confidence_values)]

    if (length(valid_confidence) > 0) {
      overall_confidence <- exp(mean(log(valid_confidence)))  # Geometric mean
      result$overall_confidence <- overall_confidence

      if (overall_confidence >= 0.7) {
        overall_category <- "high"
      } else if (overall_confidence >= 0.5) {
        overall_category <- "medium"
      } else {
        overall_category <- "low"
      }

      message("\n  Overall confidence: ", round(overall_confidence * 100, 1), "% (",
              overall_category, ")")
    }
  }

  # =================================================================
  # PHYLOGENETIC IMPUTATION - Use sister species as final fallback
  # =================================================================

  # Check if we still have missing traits after ML
  missing_after_ml <- c(
    if (is.na(result$MS)) "MS",
    if (is.na(result$FS)) "FS",
    if (is.na(result$MB)) "MB",
    if (is.na(result$EP)) "EP",
    if (is.na(result$PR)) "PR"
  )

  if (length(missing_after_ml) > 0 && !is.null(raw_traits$worms) && !is.null(cache_dir)) {
    # Source phylogenetic imputation functions
    if (!exists("apply_phylogenetic_imputation")) {
      tryCatch({
        source("R/functions/phylogenetic_imputation.R", local = TRUE)
      }, error = function(e) {
        message("  ⚠️  Phylogenetic imputation not available: ", e$message)
      })
    }

    # Apply phylogenetic imputation
    if (exists("apply_phylogenetic_imputation")) {
      result <- apply_phylogenetic_imputation(
        species_name = species_name,
        current_traits = result,
        taxonomy = raw_traits$worms,
        cache_dir = cache_dir,
        max_distance = 3,  # Within order
        min_relatives = 3,
        min_agreement = 0.6,
        verbose = TRUE
      )

      # Update sources if phylogenetic imputation was used
      phylo_sources <- c(
        if (!is.null(result$MS_source) && result$MS_source == "Phylogenetic") "Phylogenetic",
        if (!is.null(result$FS_source) && result$FS_source == "Phylogenetic") "Phylogenetic",
        if (!is.null(result$MB_source) && result$MB_source == "Phylogenetic") "Phylogenetic",
        if (!is.null(result$EP_source) && result$EP_source == "Phylogenetic") "Phylogenetic",
        if (!is.null(result$PR_source) && result$PR_source == "Phylogenetic") "Phylogenetic"
      )

      if (length(phylo_sources) > 0) {
        sources_used <- c(sources_used, "Phylogenetic")
      }
    }
  }

  # Set source and confidence
  result$source <- paste(unique(sources_used), collapse = "+")

  # Set categorical confidence (backward compatibility)
  n_traits_found <- sum(!is.na(c(result$MS, result$FS, result$MB, result$EP, result$PR)))

  # Use probabilistic confidence if available, otherwise fallback to count-based
  if (!is.null(result$overall_confidence)) {
    if (result$overall_confidence >= 0.7) {
      result$confidence <- "high"
    } else if (result$overall_confidence >= 0.5) {
      result$confidence <- "medium"
    } else {
      result$confidence <- "low"
    }
  } else {
    # Fallback: count-based confidence
    if (n_traits_found == 5) {
      result$confidence <- "high"
    } else if (n_traits_found >= 3) {
      result$confidence <- "medium"
    } else {
      result$confidence <- "low"
    }
  }

  # FINAL SUMMARY
  message("\n╔═══════════════════════════════════════════════════════════════╗")
  message("║ RESULT SUMMARY                                                 ║")
  message("╠═══════════════════════════════════════════════════════════════╣")
  message("║ Traits Found: ", n_traits_found, "/5                                           ", paste(rep(" ", max(0, 18-nchar(as.character(n_traits_found)))), collapse=""), "║")
  message("║ Data Sources: ", result$source, paste(rep(" ", max(0, 46-nchar(result$source))), collapse=""), "║")
  message("║ Confidence:   ", toupper(result$confidence), paste(rep(" ", max(0, 46-nchar(result$confidence))), collapse=""), "║")
  message("║                                                                ║")

  # Show which traits were found
  trait_status <- c(
    paste0("MS=", ifelse(is.na(result$MS), "❌", paste0("✓ ", result$MS))),
    paste0("FS=", ifelse(is.na(result$FS), "❌", paste0("✓ ", result$FS))),
    paste0("MB=", ifelse(is.na(result$MB), "❌", paste0("✓ ", result$MB))),
    paste0("EP=", ifelse(is.na(result$EP), "❌", paste0("✓ ", result$EP))),
    paste0("PR=", ifelse(is.na(result$PR), "❌", paste0("✓ ", result$PR)))
  )
  message("║ ", paste(trait_status, collapse=" | "), paste(rep(" ", max(0, 48-sum(nchar(trait_status))-12)), collapse=""), "║")
  message("╚═══════════════════════════════════════════════════════════════╝")

  total_time <- round(as.numeric(difftime(Sys.time(), total_start, units = "secs")), 2)
  message("\n⏱️  Total lookup time: ", total_time, "s")

  if (n_traits_found == 5) {
    message("✅ SUCCESS: All 5 categorical traits assigned!")
  } else if (n_traits_found >= 3) {
    message("⚠️  PARTIAL: ", n_traits_found, "/5 traits assigned (missing: ",
           paste(c(if(is.na(result$MS)) "MS", if(is.na(result$FS)) "FS",
                  if(is.na(result$MB)) "MB", if(is.na(result$EP)) "EP",
                  if(is.na(result$PR)) "PR"), collapse=", "), ")")
  } else {
    message("❌ INCOMPLETE: Only ", n_traits_found, "/5 traits assigned (insufficient data)")
  }

  # Cache result
  if (!is.null(cache_dir)) {
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE)
    }
    cache_file <- file.path(cache_dir, paste0(gsub(" ", "_", species_name), ".rds"))

    # Prepare harmonized data structure (for ML model training)
    harmonized_data <- list(
      species = species_name,
      MS = result$MS,
      FS = result$FS,
      MB = result$MB,
      EP = result$EP,
      PR = result$PR
    )

    # Add taxonomy from WoRMS for ML training
    if (!is.null(raw_traits$worms)) {
      harmonized_data$phylum <- raw_traits$worms$phylum
      harmonized_data$class <- raw_traits$worms$class
      harmonized_data$order <- raw_traits$worms$order
      harmonized_data$family <- raw_traits$worms$family
      harmonized_data$genus <- raw_traits$worms$genus
    }

    # Add ML metadata if available
    for (trait in c("MS", "FS", "MB", "EP", "PR")) {
      conf_field <- paste0(trait, "_ml_confidence")
      prob_field <- paste0(trait, "_ml_probability")
      if (!is.null(result[[conf_field]])) {
        harmonized_data[[conf_field]] <- result[[conf_field]]
      }
      if (!is.null(result[[prob_field]])) {
        harmonized_data[[prob_field]] <- result[[prob_field]]
      }
    }

    # Add uncertainty quantification metadata if available
    for (trait in c("MS", "FS", "MB", "EP", "PR")) {
      confidence_field <- paste0(trait, "_confidence")
      interval_lower_field <- paste0(trait, "_interval_lower")
      interval_upper_field <- paste0(trait, "_interval_upper")
      category_field <- paste0(trait, "_confidence_category")

      if (!is.null(result[[confidence_field]])) {
        harmonized_data[[confidence_field]] <- result[[confidence_field]]
      }
      if (!is.null(result[[interval_lower_field]])) {
        harmonized_data[[interval_lower_field]] <- result[[interval_lower_field]]
      }
      if (!is.null(result[[interval_upper_field]])) {
        harmonized_data[[interval_upper_field]] <- result[[interval_upper_field]]
      }
      if (!is.null(result[[category_field]])) {
        harmonized_data[[category_field]] <- result[[category_field]]
      }
    }

    # Add overall confidence if available
    if (!is.null(result$overall_confidence)) {
      harmonized_data$overall_confidence <- result$overall_confidence
    }

    # Build cache data structure
    cache_data <- list(
      traits = result,
      harmonized = harmonized_data,
      species = species_name,
      timestamp = Sys.time()
    )

    # Include raw traits for reference
    if (!is.null(raw_traits$ontology)) {
      cache_data$ontology_traits <- raw_traits$ontology
    }
    if (!is.null(raw_traits$worms)) {
      cache_data$worms_taxonomy <- raw_traits$worms
    }

    saveRDS(cache_data, cache_file)
  }

  return(result)
}


#' Batch lookup traits for multiple species
#'
#' @param species_list Character vector of species names
#' @param ... Additional arguments passed to lookup_species_traits
#' @return Data frame with all species traits
#' @export
batch_lookup_traits <- function(species_list, ...) {

  results_list <- list()

  for (i in seq_along(species_list)) {
    message("\n[", i, "/", length(species_list), "] Processing ", species_list[i])

    result <- lookup_species_traits(species_list[i], ...)
    results_list[[i]] <- result

    # Rate limiting (be nice to APIs)
    if (i < length(species_list)) {
      Sys.sleep(0.5)
    }
  }

  # Combine results
  results_df <- do.call(rbind, results_list)
  rownames(results_df) <- NULL

  return(results_df)
}
