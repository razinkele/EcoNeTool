# =============================================================================
# TRAIT LOOKUP - Database Query Functions
# =============================================================================
# Functions for querying individual trait databases.
# Part of the trait_lookup module (split from trait_lookup.R)
# =============================================================================

#' Lookup FishBase traits for a species
#'
#' @param species_name Scientific name (character, non-empty)
#' @param timeout Timeout in seconds (default: 20s, must be > 0)
#' @return List with raw trait data
#' @export
lookup_fishbase_traits <- function(species_name, timeout = 20) {

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

  result <- list(
    species = species_name,
    source = "FishBase",
    success = FALSE,
    traits = list()
  )

  if (!requireNamespace("rfishbase", quietly = TRUE)) {
    warning("Package 'rfishbase' not installed. Install with: install.packages('rfishbase')")
    return(result)
  }

  tryCatch({
    # rfishbase uses duckdbfs::open_dataset against remote parquet files; without
    # an enforced timeout each call can hang for 30s+ when FishBase's CDN is slow.
    # Wrap in with_timeout() (setTimeLimit-based) \u2014 rfishbase's R-level dplyr
    # collect step is interruptible, so the deadline fires within ~1s of the
    # set value (verified at 3.7s for a 2s deadline). The previous code took the
    # `timeout` parameter but never applied it.
    species_data <- tryCatch(
      with_timeout(rfishbase::species(species_name),
                   timeout = timeout, on_timeout = NULL),
      error = function(e) {
        if (grepl("open|connection|timeout|time limit", e$message, ignore.case = TRUE)) {
          warning(sprintf("[fishbase] connection error or timeout for '%s': %s",
                          species_name, conditionMessage(e)), call. = FALSE)
          return(NULL)
        }
        warning(sprintf("[fishbase] species() failed for '%s': %s",
                        species_name, conditionMessage(e)), call. = FALSE)
        return(NULL)
      })

    if (is.null(species_data) || nrow(species_data) == 0) {
      result$note <- "Connection error, timeout, or species not found"
      return(result)
    }

    # Optional auxiliary lookups; on timeout treat as missing data.
    morph_data <- tryCatch(
      with_timeout(rfishbase::morphology(species_name),
                   timeout = timeout, on_timeout = NULL),
      error = function(e) NULL)

    ecology_data <- tryCatch(
      with_timeout(rfishbase::ecology(species_name),
                   timeout = timeout, on_timeout = NULL),
      error = function(e) NULL)

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
    # <<- so the error surfaces on the returned result; `<-` would only
    # mutate the closure-local copy and silently drop the failure.
    result$error <<- conditionMessage(e)
  })

  return(result)
}


#' Lookup SeaLifeBase database traits
#'
#' @param species_name Scientific name
#' @param timeout Timeout in seconds (default: 20s)
#' @return List with raw trait data
#' @export
lookup_sealifebase_traits <- function(species_name, timeout = 20) {

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
    # SeaLifeBase shares rfishbase's duckdbfs+parquet backend. Same timeout
    # rationale as lookup_fishbase_traits \u2014 wrap each rfishbase call in
    # with_timeout() so a slow CDN day is bounded.
    species_data <- tryCatch(
      with_timeout(rfishbase::species(species_name, server = "sealifebase"),
                   timeout = timeout, on_timeout = NULL),
      error = function(e) {
        if (grepl("open|connection|timeout|time limit", e$message, ignore.case = TRUE)) {
          warning(sprintf("[sealifebase] connection error or timeout for '%s': %s",
                          species_name, conditionMessage(e)), call. = FALSE)
          return(NULL)
        }
        warning(sprintf("[sealifebase] species() failed for '%s': %s",
                        species_name, conditionMessage(e)), call. = FALSE)
        return(NULL)
      })

    if (is.null(species_data) || nrow(species_data) == 0) {
      result$note <- "Connection error, timeout, or species not found in SeaLifeBase"
      return(result)
    }

    traits <- list()

    # Helper: extract a scalar trait if the column exists and has a non-NA
    # length-1 value. Empty tibbles (nrow == 0) yield length-0 column vectors;
    # `is.na(length0)` returns logical(0), and `if (logical(0))` errors with
    # "argument is of length zero" — which previously aborted the whole tryCatch
    # and discarded any traits already collected.
    pick_scalar <- function(df, col) {
      if (is.null(df) || !(col %in% names(df))) return(NULL)
      v <- df[[col]]
      if (length(v) != 1 || is.na(v)) return(NULL)
      v
    }

    if (!is.null(v <- pick_scalar(species_data, "Length")))      traits$max_length_cm <- v
    if (!is.null(v <- pick_scalar(species_data, "Weight")))      traits$weight_g      <- v
    if (!is.null(v <- pick_scalar(species_data, "FoodTroph")))   traits$trophic_level <- v
    if (!is.null(v <- pick_scalar(species_data, "DemersPelag"))) traits$habitat       <- v

    # Try to get morphology data (optional - don't fail if unavailable).
    # rfishbase returns an empty tibble (nrow == 0) when no morphology row
    # exists for the species (e.g., Mytilus edulis on SeaLifeBase).
    morph_data <- tryCatch(
      with_timeout(rfishbase::morphology(species_name, server = "sealifebase"),
                   timeout = timeout, on_timeout = NULL),
      error = function(e) NULL)

    if (!is.null(v <- pick_scalar(morph_data, "BodyShapeI"))) traits$body_shape <- v

    result$traits <- traits
    result$success <- length(traits) > 0

  }, error = function(e) {
    # <<- so the error actually surfaces on the returned result; previous
    # `<-` only mutated the local copy in the closure and was silently lost.
    result$error <<- conditionMessage(e)
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
    # <<- so the error surfaces on the returned result; `<-` would only
    # mutate the closure-local copy and silently drop the failure.
    result$error <<- conditionMessage(e)
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
    # <<- so the error surfaces on the returned result; `<-` would only
    # mutate the closure-local copy and silently drop the failure.
    result$error <<- conditionMessage(e)
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
    # <<- so the error surfaces on the returned result; `<-` would only
    # mutate the closure-local copy and silently drop the failure.
    result$error <<- conditionMessage(e)
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
    # <<- so the error surfaces on the returned result; `<-` would only
    # mutate the closure-local copy and silently drop the failure.
    result$error <<- conditionMessage(e)
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
    # <<- so the error surfaces on the returned result; `<-` would only
    # mutate the closure-local copy and silently drop the failure.
    result$error <<- conditionMessage(e)
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

    # Trophic strategy -> feeding mode
    if ("Trophic_strategy" %in% names(species_row) && !is.na(species_row$Trophic_strategy)) {
      strategy <- trimws(species_row$Trophic_strategy)
      traits$trophic_strategy <- strategy
      traits$feeding_mode <- switch(strategy,
        "autotroph" = "primary_producer",
        "mixotroph" = "mixotroph",
        "heterotroph" = "predator",
        {
          message("  [PTDB] Unrecognized Trophic_strategy '", strategy, "' for ", species_name)
          "primary_producer"
        }
      )
      traits$trophic_level <- switch(strategy,
        "autotroph" = 1.0,
        "mixotroph" = 1.5,
        "heterotroph" = 2.0,
        1.0
      )
    } else {
      traits$feeding_mode <- "primary_producer"
      traits$trophic_level <- 1.0
    }

    # Cell size
    if ("Cell_volume_um3" %in% names(species_row) &&
        !is.na(species_row$Cell_volume_um3) &&
        species_row$Cell_volume_um3 > 0) {
      traits$cell_volume_um3 <- species_row$Cell_volume_um3
      traits$max_length_cm <- (species_row$Cell_volume_um3^(1/3)) / 10000
    } else if ("Cell_length_um" %in% names(species_row) &&
               !is.na(species_row$Cell_length_um) &&
               species_row$Cell_length_um > 0) {
      traits$cell_length_um <- species_row$Cell_length_um
      traits$max_length_cm <- species_row$Cell_length_um / 10000
    }

    # Motility
    if ("Motility" %in% names(species_row) && !is.na(species_row$Motility)) {
      traits$motility <- species_row$Motility
    }

    # Growth form
    if ("Growth_form" %in% names(species_row) && !is.na(species_row$Growth_form)) {
      traits$growth_form <- species_row$Growth_form
    }

    # Harmful Algae Bloom flag (previously not extracted)
    if ("Harmful" %in% names(species_row) && !is.na(species_row$Harmful)) {
      traits$is_hab <- as.logical(species_row$Harmful)
    }

    # Habitat
    if ("Habitat" %in% names(species_row) && !is.na(species_row$Habitat)) {
      traits$habitat <- species_row$Habitat
    }

    result$traits <- traits
    result$success <- length(traits) > 0

  }, error = function(e) {
    # <<- so the error surfaces on the returned result; `<-` would only
    # mutate the closure-local copy and silently drop the failure.
    result$error <<- conditionMessage(e)
  })

  return(result)
}


#' Look up WoRMS AphiaRecordsByName via direct REST call.
#'
#' Replaces `worrms::wm_records_name(...)` in the slow path: that wrapper goes
#' through crul, where R's `setTimeLimit()` (used by `with_timeout`) cannot
#' interrupt a blocking curl read. With `httr::timeout()` the deadline is
#' enforced at the curl level, so a slow WoRMS day is bounded by `timeout_sec`
#' instead of multiplying through retry strategies.
#'
#' Returns the same data.frame shape `worrms::wm_records_name` does (or NULL on
#' timeout / 4xx / empty), so downstream code in lookup_worms_traits is unchanged.
worms_records_by_name_http <- function(name, fuzzy = FALSE, marine_only = FALSE, timeout_sec = 10) {
  url <- paste0("https://www.marinespecies.org/rest/AphiaRecordsByName/",
                utils::URLencode(name, reserved = TRUE))
  resp <- tryCatch(
    httr::GET(url,
              query = list(like = tolower(as.character(fuzzy)),
                           marine_only = tolower(as.character(marine_only))),
              httr::timeout(timeout_sec)),
    error = function(e) NULL
  )
  if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)
  res <- tryCatch(
    jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE),
    error = function(e) NULL
  )
  if (is.null(res) || !is.data.frame(res) || nrow(res) == 0) return(NULL)
  res
}

#' Use WoRMS for taxonomic information and basic traits
#'
#' @param species_name Scientific name
#' @param timeout Timeout in seconds (default: 10s — applied to each HTTP call
#'   via `httr::timeout()`, enforced at the curl layer)
#' @return List with taxonomic and habitat information
#' @export
lookup_worms_traits <- function(species_name, timeout = 10) {

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
  timeout_count <- 0

  for (i in seq_along(query_attempts)) {
    attempt <- query_attempts[[i]]
    if (is.null(attempt)) next  # Skip if strategy wasn't added

    tryCatch({
      # Direct httr::GET so the timeout is enforced at the curl layer; the
      # previous worrms::wm_records_name wrapped in with_timeout couldn't
      # interrupt a blocking network read because setTimeLimit() doesn't
      # fire mid-syscall.
      aphia_records <- worms_records_by_name_http(
        name = attempt$name,
        fuzzy = attempt$fuzzy,
        marine_only = attempt$marine_only,
        timeout_sec = timeout
      )

      if (!is.null(aphia_records) && length(aphia_records) > 0) {
        successful_strategy <- attempt$label
        message("      \u2192 WoRMS: Found via ", attempt$label, " ('", attempt$name, "')")
        break
      } else {
        # NULL from worms_records_by_name_http includes the curl-timeout case
        # (httr::GET throws an error caught by its tryCatch and returns NULL).
        # Treat each NULL as a possible timeout for the "API down" guard below.
        timeout_count <- timeout_count + 1
      }
    }, error = function(e) {
      if (grepl("timeout|time limit|elapsed", e$message, ignore.case = TRUE)) {
        timeout_count <<- timeout_count + 1
      }
      NULL
    })

    # If 2+ timeouts, API is likely down — skip remaining strategies
    if (timeout_count >= 2) {
      message("      \u2192 WoRMS: Multiple timeouts, skipping remaining strategies")
      break
    }
  }

  # If still not found, return failure
  if (is.null(aphia_records) || (is.data.frame(aphia_records) && nrow(aphia_records) == 0)) {
    message("      \u2192 WoRMS: All strategies failed for '", species_name, "'")
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
      traits$genus <- classification$scientificname[classification$rank == "Genus"]

      # Handle multiple matches
      if (length(traits$phylum) > 1) traits$phylum <- traits$phylum[1]
      if (length(traits$class) > 1) traits$class <- traits$class[1]
      if (length(traits$order) > 1) traits$order <- traits$order[1]
      if (length(traits$family) > 1) traits$family <- traits$family[1]
      if (length(traits$genus) > 1) traits$genus <- traits$genus[1]
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

          # Detect length unit from the qualitative body-size string; WoRMS
          # embeds the unit in the value text, not as a separate field.
          qual_size <- attributes$measurementValue[grepl("body size \\(qualitative\\)",
                                                         attributes$measurementType,
                                                         ignore.case = TRUE)]
          unit <- NA_character_
          if (length(qual_size) > 0) {
            q <- tolower(qual_size[1])
            if      (grepl("\\bcm\\b", q)) unit <- "cm"
            else if (grepl("\\bmm\\b", q)) unit <- "mm"
            else if (grepl("\\bm\\b",  q)) unit <- "m"
          }

          if (is.na(unit)) {
            # Vertebrate classes whose WoRMS body-size measurements are
            # conventionally reported in cm. Bony fish span Actinopterygii /
            # Actinopteri / Teleostei depending on classification level; the
            # cartilaginous & jawless fish classes round out the fish list;
            # mammals (incl. cetaceans/pinnipeds, returned at class level as
            # Mammalia), reptiles, and birds also report cm — without them
            # a 200 cm seal silently records as 20 cm.
            cm_reporting_classes <- c(
              "actinopterygii", "actinopteri", "teleostei",
              "elasmobranchii", "holocephali", "chondrichthyes",
              "sarcopterygii", "myxini", "petromyzonti",
              "mammalia", "reptilia", "aves"
            )
            unit <- if (!is.null(traits$class) &&
                        tolower(traits$class) %in% cm_reporting_classes) "cm" else "mm"
          }

          traits$max_length_cm <- switch(unit,
                                         cm = traits$max_length_mm,
                                         mm = traits$max_length_mm / 10,
                                         m  = traits$max_length_mm * 100)
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
    warning(sprintf("[worms] error processing species '%s': %s",
                    species_name, conditionMessage(e)), call. = FALSE)
    # <<- so the error surfaces on the returned result; `<-` would only
    # mutate the closure-local copy.
    result$error <<- conditionMessage(e)
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
    # <<- so the error surfaces on the returned result; `<-` would only
    # mutate the closure-local copy.
    result$error <<- paste("Error reading ontology traits:", conditionMessage(e))
  })

  return(result)
}
