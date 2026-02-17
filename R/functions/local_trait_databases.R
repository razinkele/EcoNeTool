# =============================================================================
# LOCAL TRAIT DATABASES
# =============================================================================
#
# Functions for querying local Excel trait databases:
# 1. bvol_nomp_version_2024.xlsx - Phytoplankton biovolume and carbon
# 2. species_enriched.xlsx - Marine invertebrate biological traits
#
# Both databases link to WoRMS via AphiaID
#
# Version: 1.4.1 (Phase 6 extension)
# Date: 2025-12-26
# =============================================================================

# Required packages
if (!requireNamespace("readxl", quietly = TRUE)) {
  message("Installing readxl package...")
  install.packages("readxl")
}

library(readxl)


# =============================================================================
# CONFIGURATION
# =============================================================================

LOCAL_DB_CONFIG <- list(
  bvol_file = "data/bvol_nomp_version_2024.xlsx",
  species_enriched_file = "data/species_enriched.xlsx",

  # Cache loaded data to avoid repeated file reads
  cache_enabled = TRUE,

  # Column mappings
  bvol_columns = list(
    aphia_id = "AphiaID",
    division = "Division",
    class = "Class",
    order = "Order",
    genus = "Genus",
    species = "Species",
    trophy = "Trophy",
    geometric_shape = "Geometric_shape",
    length_l1 = "Length(l1)µm",
    height_h = "Height(h)µm",
    diameter_d1 = "Diameter(d1)µm",
    diameter_d2 = "Diameter(d2)µm",
    volume = "Calculated_volume_µm3/counting_unit",
    carbon = "Calculated_Carbon_pg/counting_unit",
    size_class = "SizeClassNo"
  ),

  species_enriched_columns = list(
    aphia_id = "aphiaID",
    taxonomy_name = "taxonomyName",
    common_name = "synonymCommonName",
    size_range_male = "biology_male_size_range",
    size_range_female = "biology_female_size_range",
    size_maturity_male = "biology_male_size_at_maturity",
    size_maturity_female = "biology_female_size_at_maturity",
    growth_form = "biology_growth_form",
    mobility = "biology_mobility",
    feeding_method = "biology_characteristic_feeding_method",
    diet = "biology_dietfood_source",
    feeds_on = "biology_typically_feeds_on",
    env_position = "biology_environmental_position",
    body_flexibility = "biology_body_flexibility"
  )
)

# Cache environment (avoids <<-)
.db_cache <- new.env(parent = emptyenv())
.db_cache$bvol <- NULL
.db_cache$species_enriched <- NULL


# =============================================================================
# DATA LOADING
# =============================================================================

#' Load BVOL Phytoplankton Database
#'
#' Loads the phytoplankton biovolume database with caching
#'
#' @param force_reload Logical. Force reload from file (ignore cache)
#' @return data.frame with phytoplankton trait data
#'
load_bvol_database <- function(force_reload = FALSE) {

  if (!force_reload && !is.null(.db_cache$bvol)) {
    return(.db_cache$bvol)
  }

  file_path <- LOCAL_DB_CONFIG$bvol_file

  if (!file.exists(file_path)) {
    warning("BVOL database not found: ", file_path)
    return(NULL)
  }

  tryCatch({
    bvol_data <- read_excel(file_path)

    # Store in global cache
    .db_cache$bvol <- bvol_data

    message(sprintf("✓ Loaded BVOL database: %d phytoplankton species",
                    nrow(bvol_data)))

    return(bvol_data)

  }, error = function(e) {
    warning("Failed to load BVOL database: ", e$message)
    return(NULL)
  })
}


#' Load Species Enriched Database
#'
#' Loads the marine invertebrate traits database with caching
#'
#' @param force_reload Logical. Force reload from file (ignore cache)
#' @return data.frame with marine invertebrate trait data
#'
load_species_enriched_database <- function(force_reload = FALSE) {

  if (!force_reload && !is.null(.db_cache$species_enriched)) {
    return(.db_cache$species_enriched)
  }

  file_path <- LOCAL_DB_CONFIG$species_enriched_file

  if (!file.exists(file_path)) {
    warning("Species enriched database not found: ", file_path)
    return(NULL)
  }

  tryCatch({
    species_data <- read_excel(file_path)

    # Store in global cache
    .db_cache$species_enriched <- species_data

    message(sprintf("✓ Loaded species enriched database: %d marine invertebrates",
                    nrow(species_data)))

    return(species_data)

  }, error = function(e) {
    warning("Failed to load species enriched database: ", e$message)
    return(NULL)
  })
}


# =============================================================================
# LOOKUP FUNCTIONS
# =============================================================================

#' Lookup Species in BVOL Database
#'
#' Query phytoplankton traits by AphiaID or species name
#'
#' @param aphia_id Numeric. WoRMS AphiaID (preferred)
#' @param species_name Character. Species name (fallback)
#' @return List with trait data or NULL if not found
#'
#' @examples
#' traits <- lookup_bvol_traits(aphia_id = 160140)  # Skeletonema costatum
#'
lookup_bvol_traits <- function(aphia_id = NULL, species_name = NULL) {

  # Load database
  bvol_db <- load_bvol_database()

  if (is.null(bvol_db)) {
    return(NULL)
  }

  # Search by AphiaID (preferred)
  if (!is.null(aphia_id) && !is.na(aphia_id)) {
    matches <- bvol_db[bvol_db$AphiaID == aphia_id & !is.na(bvol_db$AphiaID), ]

    if (nrow(matches) > 0) {
      return(parse_bvol_record(matches[1, ]))
    }
  }

  # Search by species name (fallback)
  if (!is.null(species_name)) {
    species_col <- bvol_db$Species
    matches <- bvol_db[!is.na(species_col) &
                       grepl(species_name, species_col, ignore.case = TRUE), ]

    if (nrow(matches) > 0) {
      return(parse_bvol_record(matches[1, ]))
    }
  }

  return(NULL)
}


#' Lookup Species in Species Enriched Database
#'
#' Query marine invertebrate traits by AphiaID or species name
#'
#' @param aphia_id Numeric. WoRMS AphiaID (preferred)
#' @param species_name Character. Species name (fallback)
#' @return List with trait data or NULL if not found
#'
#' @examples
#' traits <- lookup_species_enriched_traits(aphia_id = 141433)  # Abra alba
#'
lookup_species_enriched_traits <- function(aphia_id = NULL, species_name = NULL) {

  # Load database
  species_db <- load_species_enriched_database()

  if (is.null(species_db)) {
    return(NULL)
  }

  # Search by AphiaID (preferred)
  if (!is.null(aphia_id) && !is.na(aphia_id)) {
    matches <- species_db[species_db$aphiaID == aphia_id & !is.na(species_db$aphiaID), ]

    if (nrow(matches) > 0) {
      return(parse_species_enriched_record(matches[1, ]))
    }
  }

  # Search by species name (fallback)
  if (!is.null(species_name)) {
    taxonomy_col <- species_db$taxonomyName
    matches <- species_db[!is.na(taxonomy_col) &
                          grepl(species_name, taxonomy_col, ignore.case = TRUE), ]

    if (nrow(matches) > 0) {
      return(parse_species_enriched_record(matches[1, ]))
    }
  }

  return(NULL)
}


# =============================================================================
# RECORD PARSING
# =============================================================================

#' Parse BVOL Record to Trait Structure
#'
#' Converts a BVOL database row to standardized trait format
#'
#' @param record data.frame row from BVOL database
#' @return List with extracted traits
#'
parse_bvol_record <- function(record) {

  if (is.null(record) || nrow(record) == 0) {
    return(NULL)
  }

  # Extract size (use maximum dimension available)
  size_cm <- extract_phytoplankton_size(record)

  # Extract trophic mode
  trophy <- as.character(record$Trophy)

  # Build trait structure
  traits <- list(
    source = "BVOL",
    aphia_id = as.numeric(record$AphiaID),

    # Taxonomy
    division = as.character(record$Division),
    class = as.character(record$Class),
    order = as.character(record$Order),
    genus = as.character(record$Genus),
    species = as.character(record$Species),

    # Size (convert µm to cm)
    size_cm = size_cm,
    max_length_cm = size_cm,

    # Raw morphological data
    length_um = as.numeric(record$`Length(l1)µm`),
    height_um = as.numeric(record$`Height(h)µm`),
    diameter_d1_um = as.numeric(record$`Diameter(d1)µm`),
    diameter_d2_um = as.numeric(record$`Diameter(d2)µm`),
    volume_um3 = as.numeric(record$`Calculated_volume_µm3/counting_unit`),
    carbon_pg = as.numeric(record$`Calculated_Carbon_pg/counting_unit`),

    # Feeding type (trophy)
    feeding_type = trophy,
    trophy = trophy,

    # Shape
    geometric_shape = as.character(record$Geometric_shape),

    # Metadata
    worms_rank = as.character(record$`WORMS Rank`),
    size_class_no = as.numeric(record$SizeClassNo)
  )

  return(traits)
}


#' Parse Species Enriched Record
#'
#' Converts a species_enriched database row to standardized trait format
#'
#' @param record data.frame row from species_enriched database
#' @return List with extracted traits
#'
parse_species_enriched_record <- function(record) {

  if (is.null(record) || nrow(record) == 0) {
    return(NULL)
  }

  # Extract size from ranges
  size_cm <- extract_size_from_range(record)

  # Build trait structure
  traits <- list(
    source = "SpeciesEnriched",
    aphia_id = as.numeric(record$aphiaID),

    # Taxonomy
    species = as.character(record$taxonomyName),
    common_name = as.character(record$synonymCommonName),
    authority = as.character(record$taxonomyAuthority),

    # Size
    size_cm = size_cm$mean,
    max_length_cm = size_cm$max,
    min_length_cm = size_cm$min,

    # Raw size data
    male_size_range = as.character(record$biology_male_size_range),
    female_size_range = as.character(record$biology_female_size_range),
    male_size_maturity = as.character(record$biology_male_size_at_maturity),
    female_size_maturity = as.character(record$biology_female_size_at_maturity),

    # Biological traits
    growth_form = as.character(record$biology_growth_form),
    growth_rate = as.character(record$biology_growth_rate),
    body_flexibility = as.character(record$biology_body_flexibility),
    mobility = as.character(record$biology_mobility),

    # Feeding
    feeding_method = as.character(record$biology_characteristic_feeding_method),
    diet_source = as.character(record$biology_dietfood_source),
    feeds_on = as.character(record$biology_typically_feeds_on),

    # Habitat
    environmental_position = as.character(record$biology_environmental_position),

    # Metadata
    nbn_key = as.character(record$NBNVersionKey),
    url = as.character(record$url)
  )

  return(traits)
}


# =============================================================================
# SIZE EXTRACTION UTILITIES
# =============================================================================

#' Extract Phytoplankton Size
#'
#' Calculate representative size from phytoplankton morphological dimensions
#'
#' @param record BVOL database row
#' @return Numeric size in cm
#'
extract_phytoplankton_size <- function(record) {

  # Get all available dimensions (in µm)
  length_um <- as.numeric(record$`Length(l1)µm`)
  height_um <- as.numeric(record$`Height(h)µm`)
  diameter_d1_um <- as.numeric(record$`Diameter(d1)µm`)
  diameter_d2_um <- as.numeric(record$`Diameter(d2)µm`)

  # Use largest dimension available
  dims <- c(length_um, height_um, diameter_d1_um, diameter_d2_um)
  dims <- dims[!is.na(dims)]

  if (length(dims) == 0) {
    return(NA)
  }

  max_dim_um <- max(dims, na.rm = TRUE)

  # Convert µm to cm (1 µm = 0.0001 cm)
  max_dim_cm <- max_dim_um * 0.0001

  return(max_dim_cm)
}


#' Extract Size from Text Range
#'
#' Parse size ranges like "10-20 mm" or "up to 50 cm"
#'
#' @param record species_enriched database row
#' @return List with min, max, mean in cm
#'
extract_size_from_range <- function(record) {

  # Try male range first, then female
  size_text <- as.character(record$biology_male_size_range)

  if (is.na(size_text) || size_text == "") {
    size_text <- as.character(record$biology_female_size_range)
  }

  if (is.na(size_text) || size_text == "") {
    return(list(min = NA, max = NA, mean = NA))
  }

  # Parse text patterns
  # Pattern: "10-20 mm", "up to 50 cm", "5 cm", etc.

  # Extract numbers
  numbers <- as.numeric(unlist(regmatches(size_text, gregexpr("[0-9.]+", size_text))))

  # Extract unit
  unit <- "cm"  # default
  if (grepl("mm", size_text, ignore.case = TRUE)) {
    unit <- "mm"
  } else if (grepl("m\\b", size_text, ignore.case = TRUE)) {
    unit <- "m"
  }

  # Convert to cm
  if (unit == "mm") {
    numbers <- numbers / 10
  } else if (unit == "m") {
    numbers <- numbers * 100
  }

  if (length(numbers) == 0) {
    return(list(min = NA, max = NA, mean = NA))
  } else if (length(numbers) == 1) {
    return(list(min = numbers[1], max = numbers[1], mean = numbers[1]))
  } else {
    return(list(
      min = min(numbers, na.rm = TRUE),
      max = max(numbers, na.rm = TRUE),
      mean = mean(numbers, na.rm = TRUE)
    ))
  }
}


# =============================================================================
# TRAIT HARMONIZATION MAPPING
# =============================================================================

#' Map BVOL Traits to Harmonized Schema
#'
#' Convert BVOL raw traits to MS, FS, MB, EP, PR
#'
#' @param bvol_traits List from parse_bvol_record()
#' @return List with harmonized trait assignments
#'
harmonize_bvol_traits <- function(bvol_traits) {

  if (is.null(bvol_traits)) {
    return(NULL)
  }

  harmonized <- list(
    source = "BVOL_harmonized"
  )

  # MS: Size Class (directly from size_cm if available)
  if (!is.na(bvol_traits$size_cm)) {
    # Phytoplankton are typically < 1 mm (0.1 cm)
    # Use existing harmonize_size_class function
    harmonized$MS <- harmonize_size_class(bvol_traits$size_cm)
    harmonized$MS_confidence <- 0.9  # High confidence from direct measurement
    harmonized$MS_source <- "BVOL_measured"
  }

  # FS: Foraging Strategy (from trophy)
  trophy <- tolower(as.character(bvol_traits$trophy))

  if (!is.na(trophy) && grepl("auto", trophy)) {
    harmonized$FS <- "FS0"  # Primary producer
    harmonized$FS_confidence <- 0.95
    harmonized$FS_source <- "BVOL_trophy"
  } else if (!is.na(trophy) && grepl("mixo", trophy)) {
    harmonized$FS <- "FS4"  # Mixotroph (grazer + photosynthesis)
    harmonized$FS_confidence <- 0.85
    harmonized$FS_source <- "BVOL_trophy"
  } else if (!is.na(trophy) && grepl("hetero", trophy)) {
    harmonized$FS <- "FS6"  # Filter/suspension feeder
    harmonized$FS_confidence <- 0.80
    harmonized$FS_source <- "BVOL_trophy"
  }

  # MB: Mobility (phytoplankton are generally planktonic)
  harmonized$MB <- "MB4"  # Floater/drifter
  harmonized$MB_confidence <- 0.90
  harmonized$MB_source <- "BVOL_taxon"

  # EP: Environmental Position (phytoplankton are pelagic)
  harmonized$EP <- "EP1"  # Pelagic
  harmonized$EP_confidence <- 0.95
  harmonized$EP_source <- "BVOL_taxon"

  # PR: Protection (no hard structures typically)
  harmonized$PR <- "PR0"  # None
  harmonized$PR_confidence <- 0.85
  harmonized$PR_source <- "BVOL_taxon"

  # Overall confidence (geometric mean of available confidences)
  confidences <- c(
    harmonized$MS_confidence,
    harmonized$FS_confidence,
    harmonized$MB_confidence,
    harmonized$EP_confidence,
    harmonized$PR_confidence
  )
  confidences <- confidences[!is.na(confidences) & is.numeric(confidences)]

  if (length(confidences) > 0) {
    harmonized$overall_confidence <- exp(mean(log(confidences)))
  } else {
    harmonized$overall_confidence <- 0.5  # Default if no confidences available
  }

  return(harmonized)
}


#' Map Species Enriched Traits to Harmonized Schema
#'
#' Convert species_enriched raw traits to MS, FS, MB, EP, PR
#'
#' @param species_traits List from parse_species_enriched_record()
#' @return List with harmonized trait assignments
#'
harmonize_species_enriched_traits <- function(species_traits) {

  if (is.null(species_traits)) {
    return(NULL)
  }

  harmonized <- list(
    source = "SpeciesEnriched_harmonized"
  )

  # MS: Size Class
  if (!is.na(species_traits$size_cm)) {
    harmonized$MS <- harmonize_size_class(species_traits$size_cm)
    harmonized$MS_confidence <- 0.85
    harmonized$MS_source <- "SpeciesEnriched_size"
  }

  # FS: Foraging Strategy (from feeding_method)
  feeding <- tolower(species_traits$feeding_method)

  if (grepl("predator|carniv", feeding)) {
    harmonized$FS <- "FS1"
    harmonized$FS_confidence <- 0.90
  } else if (grepl("scaveng|detritivor", feeding)) {
    harmonized$FS <- "FS2"
    harmonized$FS_confidence <- 0.90
  } else if (grepl("graz|herbiv", feeding)) {
    harmonized$FS <- "FS4"
    harmonized$FS_confidence <- 0.90
  } else if (grepl("deposit|sediment", feeding)) {
    harmonized$FS <- "FS5"
    harmonized$FS_confidence <- 0.90
  } else if (grepl("filter|suspension", feeding)) {
    harmonized$FS <- "FS6"
    harmonized$FS_confidence <- 0.90
  }
  harmonized$FS_source <- "SpeciesEnriched_feeding"

  # MB: Mobility (from mobility field)
  mobility <- tolower(species_traits$mobility)

  if (grepl("crawl|walk", mobility)) {
    harmonized$MB <- "MB2"  # Crawler
    harmonized$MB_confidence <- 0.90
  } else if (grepl("burrow", mobility)) {
    harmonized$MB <- "MB3"  # Burrower
    harmonized$MB_confidence <- 0.90
  } else if (grepl("swim", mobility)) {
    harmonized$MB <- "MB5"  # Swimmer
    harmonized$MB_confidence <- 0.90
  } else if (grepl("sessile|attach", mobility)) {
    harmonized$MB <- "MB1"  # Sessile
    harmonized$MB_confidence <- 0.95
  }
  harmonized$MB_source <- "SpeciesEnriched_mobility"

  # EP: Environmental Position
  env_pos <- tolower(species_traits$environmental_position)

  if (grepl("pelagic|water column", env_pos)) {
    harmonized$EP <- "EP1"
    harmonized$EP_confidence <- 0.90
  } else if (grepl("benthic|seabed|bottom", env_pos)) {
    harmonized$EP <- "EP2"
    harmonized$EP_confidence <- 0.90
  } else if (grepl("epibenthic|on surface", env_pos)) {
    harmonized$EP <- "EP3"
    harmonized$EP_confidence <- 0.90
  }
  harmonized$EP_source <- "SpeciesEnriched_position"

  # PR: Protection (from body_flexibility or growth_form)
  flexibility <- tolower(species_traits$body_flexibility)
  growth_form <- tolower(species_traits$growth_form)

  if (grepl("shell|test|exoskeleton", growth_form)) {
    harmonized$PR <- "PR3"  # Hard shell/exoskeleton
    harmonized$PR_confidence <- 0.95
  } else if (grepl("soft|flexible", flexibility)) {
    harmonized$PR <- "PR0"  # No protection
    harmonized$PR_confidence <- 0.85
  }
  harmonized$PR_source <- "SpeciesEnriched_morphology"

  # Overall confidence (geometric mean of available confidences)
  confidences <- c(
    harmonized$MS_confidence,
    harmonized$FS_confidence,
    harmonized$MB_confidence,
    harmonized$EP_confidence,
    harmonized$PR_confidence
  )
  confidences <- confidences[!is.na(confidences) & is.numeric(confidences)]

  if (length(confidences) > 0) {
    harmonized$overall_confidence <- exp(mean(log(confidences)))
  } else {
    harmonized$overall_confidence <- 0.5  # Default if no confidences available
  }

  return(harmonized)
}


# =============================================================================
# EXPORT
# =============================================================================

# Main functions exported for integration:
# - lookup_bvol_traits()
# - lookup_species_enriched_traits()
# - harmonize_bvol_traits()
# - harmonize_species_enriched_traits()
# - load_bvol_database()
# - load_species_enriched_database()
