# =============================================================================
# TRAIT LOOKUP - API-Based Trait Database Functions
# =============================================================================
# Five functions for querying external trait databases via APIs:
#   1. lookup_worms_traits_api   - WoRMS attribute data via worrms package
#   2. lookup_polytraits         - PolyTraits two-step REST API
#   3. lookup_emodnet_traits     - EMODnet Btrait (optional GitHub package)
#   4. lookup_obis_traits        - OBIS occurrence MoF records via robis
#   5. lookup_traitbank          - EOL TraitBank via Pages API v3
#
# All functions return list(species, source, success, traits).
# %||% and with_timeout() are sourced from R/functions/validation_utils.R.
# =============================================================================


# =============================================================================
# 1. WoRMS Traits API
# =============================================================================

#' Look up Traits from WoRMS Attribute Data
#'
#' Queries the WoRMS attribute/measurement data for a given AphiaID using the
#' worrms package.  Parses measurementType / measurementValue pairs to extract
#' functional_group, feeding_type, body_size, zone, substratum, environment,
#' salinity and ambi_group.
#'
#' @param species_name Character. Species name (used for labelling only).
#' @param aphia_id     Numeric. WoRMS AphiaID (required, must be > 0).
#' @param timeout      Numeric. Seconds before aborting the API call (default 10).
#' @return Named list: species, source, success (logical), traits (named list).
#' @export
lookup_worms_traits_api <- function(species_name = NULL, aphia_id = NULL, timeout = 10) {
  result <- list(
    species = species_name %||% paste0("AphiaID:", aphia_id),
    source  = "WoRMS_Traits",
    success = FALSE,
    traits  = list()
  )

  if (is.null(aphia_id) || !is.numeric(aphia_id) || aphia_id <= 0) return(result)
  if (!requireNamespace("worrms", quietly = TRUE)) {
    message("  [WoRMS Traits] Package 'worrms' not installed. Install: install.packages('worrms')")
    return(result)
  }

  tryCatch({
    attrs <- with_timeout(
      worrms::wm_attr_data(aphia_id),
      timeout    = timeout,
      on_timeout = NULL
    )
    if (is.null(attrs) || nrow(attrs) == 0) return(result)

    traits <- list()
    for (i in seq_len(nrow(attrs))) {
      mt <- tolower(attrs$measurementType[i])
      mv <- attrs$measurementValue[i]
      if (is.na(mv) || mv == "") next
      if (grepl("functional group", mt))          traits$functional_group <- mv
      if (grepl("body size", mt))                 traits$body_size        <- mv
      if (grepl("feeding type|feeding.type|diet", mt)) traits$feeding_type <- mv
      if (grepl("substratum|substrate", mt))      traits$substratum       <- mv
      if (grepl("zone|zonation", mt))             traits$zone             <- mv
      if (grepl("ambi", mt))                      traits$ambi_group       <- mv
      if (grepl("environment|habitat", mt))       traits$environment      <- mv
      if (grepl("salinity", mt))                  traits$salinity         <- mv
    }

    result$traits  <- traits
    result$success <- length(traits) > 0
    result
  }, error = function(e) {
    message("  [WoRMS Traits] Error: ", e$message)
    result
  })
}


# =============================================================================
# 2. PolyTraits REST API (two-step)
# =============================================================================

#' Look up Traits from the PolyTraits Database
#'
#' Performs a two-step REST lookup:
#'   Step 1 – resolves species name to a taxonID via the /taxon/ endpoint.
#'   Step 2 – fetches trait records for that taxonID via the /traits/ endpoint.
#'
#' Parses the \code{trait} and \code{modality} fields of each returned record.
#'
#' @param species_name Character. Scientific name to look up.
#' @param timeout      Numeric. Seconds before aborting each HTTP call (default 10).
#' @return Named list: species, source, success (logical), traits (named list).
#' @export
lookup_polytraits <- function(species_name, timeout = 10) {
  result <- list(
    species = species_name,
    source  = "PolyTraits",
    success = FALSE,
    traits  = list()
  )

  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    message("  [PolyTraits] Packages 'httr'/'jsonlite' required. Install: install.packages(c('httr','jsonlite'))")
    return(result)
  }

  tryCatch({
    # Step 1: resolve species name -> taxonID
    taxon_url  <- paste0(
      "http://polytraits.lifewatchgreece.eu/taxon/",
      utils::URLencode(species_name),
      "/json/"
    )
    taxon_resp <- httr::GET(taxon_url, httr::timeout(timeout))
    if (httr::http_error(taxon_resp)) return(result)

    taxon_data <- jsonlite::fromJSON(
      httr::content(taxon_resp, as = "text", encoding = "UTF-8")
    )
    if (is.null(taxon_data) || length(taxon_data) == 0) return(result)

    taxon_id <- taxon_data$taxonID[1]
    if (is.null(taxon_id) || taxon_id == "") return(result)

    # Step 2: fetch trait records for this taxonID
    traits_url  <- paste0(
      "http://polytraits.lifewatchgreece.eu/traits/",
      taxon_id,
      "/json/"
    )
    traits_resp <- httr::GET(traits_url, httr::timeout(timeout))
    if (httr::http_error(traits_resp)) return(result)

    traits_json <- jsonlite::fromJSON(
      httr::content(traits_resp, as = "text", encoding = "UTF-8")
    )
    if (is.null(traits_json) || length(traits_json) == 0) return(result)

    trait_records <- traits_json[[1]]
    if (!is.data.frame(trait_records) || nrow(trait_records) == 0) return(result)

    traits <- list()
    for (i in seq_len(nrow(trait_records))) {
      tn <- tolower(trait_records$trait[i]    %||% "")
      mv <- trait_records$modality[i]         %||% ""
      if (mv == "") next
      if (grepl("body size|size", tn))                     traits$body_size          <- mv
      if (grepl("feeding|diet", tn))                       traits$feeding_mode       <- mv
      if (grepl("mobility|movement", tn))                  traits$mobility_info      <- mv
      if (grepl("reproduction|reproductive|sexual", tn))   traits$reproductive_mode  <- mv
      if (grepl("larval", tn))                             traits$larval_development <- mv
      if (grepl("depth|zonation", tn))                     traits$habitat_info       <- mv
    }

    result$traits  <- traits
    result$success <- length(traits) > 0
    result
  }, error = function(e) {
    message("  [PolyTraits] Error: ", e$message)
    result
  })
}


# =============================================================================
# 3. EMODnet Btrait (optional GitHub package)
# =============================================================================

#' Look up Traits from the EMODnet Btrait Package
#'
#' Uses the optional \pkg{Btrait} GitHub package (karlines/Btrait) to query
#' EMODnet trait data.  Gracefully degrades when the package is not installed.
#'
#' @param species_name Character. Scientific name to look up.
#' @return Named list: species, source, success (logical), traits (named list).
#' @export
lookup_emodnet_traits <- function(species_name) {
  result <- list(
    species = species_name,
    source  = "EMODnet",
    success = FALSE,
    traits  = list()
  )

  if (!requireNamespace("Btrait", quietly = TRUE)) {
    message("  [EMODnet] Package 'Btrait' not installed. Install: remotes::install_github('EMODnet/Btrait')")
    return(result)
  }

  tryCatch({
    trait_data <- Btrait::getTrait(species_name)
    if (is.null(trait_data) || nrow(trait_data) == 0) return(result)

    traits <- list()
    row    <- trait_data[1, ]
    for (col in names(row)) {
      val <- row[[col]]
      if (!is.na(val) && val != "") {
        cl <- tolower(col)
        if (grepl("^feeding",      cl)) traits$feeding_mode  <- val
        if (grepl("^mobility",     cl)) traits$mobility_info <- val
        if (grepl("^size",         cl)) traits$body_size     <- val
        if (grepl("^bioturbation", cl)) traits$bioturbation  <- val
        if (grepl("^living",       cl)) traits$living_habit  <- val
      }
    }

    result$traits  <- traits
    result$success <- length(traits) > 0
    result
  }, error = function(e) {
    message("  [EMODnet] Error: ", e$message)
    result
  })
}


# =============================================================================
# 4. OBIS Occurrence / MoF Traits
# =============================================================================

#' Look up Traits from OBIS Occurrence Records (MoF)
#'
#' Queries OBIS via \code{robis::occurrence(mof = TRUE)} and extracts body
#' size, biomass, and depth range from Measurement-or-Fact (MoF) fields and
#' occurrence depth columns.
#'
#' @param species_name Character. Scientific name to look up.
#' @param timeout      Numeric. Seconds before aborting the API call (default 30).
#' @return Named list: species, source, success (logical), traits (named list).
#' @export
lookup_obis_traits <- function(species_name, timeout = 30) {
  result <- list(
    species = species_name,
    source  = "OBIS",
    success = FALSE,
    traits  = list()
  )

  if (!requireNamespace("robis", quietly = TRUE)) {
    message("  [OBIS] Package 'robis' not installed. Install: install.packages('robis')")
    return(result)
  }

  tryCatch({
    occ <- with_timeout(
      robis::occurrence(
        scientificname = species_name,
        mof            = TRUE,
        fields         = c("scientificName",
                           "minimumDepthInMeters",
                           "maximumDepthInMeters")
      ),
      timeout    = timeout,
      on_timeout = NULL
    )
    if (is.null(occ) || nrow(occ) == 0) return(result)

    traits <- list()

    # MoF records embedded in the occurrence object
    if ("mof" %in% names(attributes(occ)) || "measurementType" %in% names(occ)) {
      mof_data <- if ("mof" %in% names(occ)) occ$mof else occ
      if (is.data.frame(mof_data) && "measurementType" %in% names(mof_data)) {
        for (i in seq_len(min(nrow(mof_data), 100))) {
          mt <- tolower(mof_data$measurementType[i] %||% "")
          mv <- mof_data$measurementValue[i]         %||% ""
          if (mv == "") next
          if (grepl("body size|length|size", mt) && is.null(traits$body_size))
            traits$body_size <- mv
          if (grepl("biomass|weight", mt) && is.null(traits$biomass))
            traits$biomass <- mv
        }
      }
    }

    # Depth range from occurrence columns
    if ("minimumDepthInMeters" %in% names(occ)) {
      d <- occ$minimumDepthInMeters
      if (any(!is.na(d))) traits$depth_min <- median(d, na.rm = TRUE)
    }
    if ("maximumDepthInMeters" %in% names(occ)) {
      d <- occ$maximumDepthInMeters
      if (any(!is.na(d))) traits$depth_max <- median(d, na.rm = TRUE)
    }

    result$traits  <- traits
    result$success <- length(traits) > 0
    result
  }, error = function(e) {
    message("  [OBIS] Error: ", e$message)
    result
  })
}


# =============================================================================
# 5. TraitBank via EOL Pages API v3
# =============================================================================

#' Look up Traits from EOL TraitBank (Pages API v3)
#'
#' Uses the Encyclopedia of Life (EOL) search and pages REST APIs to retrieve
#' trait-like data objects (diet, habitat, body mass) for a species.
#'
#' @param species_name Character. Scientific name to look up.
#' @param timeout      Numeric. Seconds before aborting each HTTP call (default 15).
#' @return Named list: species, source, success (logical), traits (named list).
#' @export
lookup_traitbank <- function(species_name, timeout = 15) {
  result <- list(
    species = species_name,
    source  = "TraitBank",
    success = FALSE,
    traits  = list()
  )

  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    message("  [TraitBank] Packages 'httr'/'jsonlite' required. Install: install.packages(c('httr','jsonlite'))")
    return(result)
  }

  tryCatch({
    # Step 1: search EOL for the page ID
    search_url  <- paste0(
      "https://eol.org/api/search/1.0.json?q=",
      utils::URLencode(species_name),
      "&page=1&exact=true"
    )
    search_resp <- httr::GET(search_url, httr::timeout(timeout))
    if (httr::http_error(search_resp)) return(result)

    search_data <- jsonlite::fromJSON(
      httr::content(search_resp, as = "text", encoding = "UTF-8")
    )
    if (is.null(search_data$results) || length(search_data$results) == 0) return(result)

    page_id <- search_data$results$id[1]
    if (is.null(page_id)) return(result)

    # Step 2: fetch detailed page data
    pages_url  <- paste0(
      "https://eol.org/api/pages/1.0/", page_id,
      ".json?details=true&common_names=false&images_per_page=0"
    )
    pages_resp <- httr::GET(pages_url, httr::timeout(timeout))
    if (httr::http_error(pages_resp)) return(result)

    pages_data <- jsonlite::fromJSON(
      httr::content(pages_resp, as = "text", encoding = "UTF-8")
    )

    traits <- list()
    if (!is.null(pages_data$dataObjects) && is.data.frame(pages_data$dataObjects)) {
      for (i in seq_len(min(nrow(pages_data$dataObjects), 20))) {
        desc <- tolower(pages_data$dataObjects$description[i] %||% "")
        if (grepl("body mass|mass|weight", desc) && is.null(traits$body_mass))
          traits$body_mass <- desc
        if (grepl("habitat|environment", desc) && is.null(traits$habitat))
          traits$habitat <- desc
        if (grepl("diet|food|feeds on", desc) && is.null(traits$diet))
          traits$diet <- desc
      }
    }

    result$traits  <- traits
    result$success <- length(traits) > 0
    result
  }, error = function(e) {
    message("  [TraitBank] Error: ", e$message)
    result
  })
}
