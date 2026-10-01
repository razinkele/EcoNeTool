# =============================================================================
# Uncertainty Quantification for Trait Predictions
# =============================================================================
#
# This module provides probabilistic confidence scoring for trait predictions,
# replacing simple categorical confidence (high/medium/low) with numerical
# confidence intervals and uncertainty propagation through food webs.
#
# Features:
# - Database authority weights
# - Threshold distance-based confidence adjustment
# - Confidence interval calculation
# - Uncertainty propagation through networks
# =============================================================================

# =============================================================================
# DATABASE AUTHORITY WEIGHTS
# =============================================================================
# Higher values = more authoritative source
# Based on data quality, peer review status, and taxonomic coverage

DATABASE_WEIGHTS <- list(
  # Curated databases (highest authority)
  "FishBase" = 1.0,
  "SeaLifeBase" = 0.95,
  "BIOTIC" = 0.90,

  # Community databases (medium-high authority)
  "freshwaterecology" = 0.85,
  "MAREDAT" = 0.80,
  "PTDB" = 0.75,
  "BVOL" = 0.75,
  "PelagicTraits" = 0.75,
  "PolyTraits" = 0.75,
  "SpeciesEnriched" = 0.70,
  "Cefas" = 0.70,
  "EMODnet" = 0.70,
  "BlackSea" = 0.60,
  "ArcticTraits" = 0.60,
  "CoralTraits" = 0.60,

  # Taxonomic databases (medium authority - good for traits, limited for ecology)
  "WoRMS" = 0.60,
  "WoRMS_Traits" = 0.60,
  "AlgaeBase" = 0.60,
  "OBIS" = 0.50,
  "TraitBank" = 0.50,

  # The offline trait DB (codes harmonised at build time) and the fuzzy
  # ontology profiles
  "OfflineDB" = 0.80,
  "Ontology" = 0.60,

  # Predicted/inferred data (lower authority)
  "ML_prediction" = 0.50,
  "Phylogenetic" = 0.55,
  "Taxonomy" = 0.50,
  "Harmonized" = 0.50,
  "Rule-based" = 0.40,
  "Depth-based" = 0.40,
  # A harmoniser's fall-through code (MB4, EP3, FS6, the PR0 fallback): no
  # evidence at all, so it ranks below every rule.
  "Default" = 0.30
)

# Source labels that name a DATABASE_WEIGHTS key differently: the
# orchestrator's labels ("ML", "freshwaterecology.info") and the offline DB's
# primary_source values (lower case; see offline_source_label()).
DATABASE_WEIGHT_ALIASES <- c(
  "ML" = "ML_prediction",
  "freshwaterecology.info" = "freshwaterecology",
  "species_enriched" = "SpeciesEnriched",
  "coral" = "CoralTraits",
  "arctic" = "ArcticTraits"
)

# Unknown-source warnings: once per source label per process.
.uq_weight_warned <- new.env(parent = emptyenv())

#' Reset the once-per-source unknown-weight warnings (tests)
reset_database_weight_warnings <- function() {
  rm(list = ls(.uq_weight_warned, all.names = TRUE), envir = .uq_weight_warned)
  invisible(TRUE)
}

#' Get Database Authority Weight
#'
#' Returns the authority weight for a given data source. Labels are matched
#' exactly, then through DATABASE_WEIGHT_ALIASES, then case-insensitively (the
#' offline DB stores "biotic", "ontology", ...). A label that matches nothing
#' - or a missing one - gets 0.5 with a warning (once per label per process),
#' so no source falls to the default silently (spec C3.3).
#'
#' @param source Character string. Data source name; comma-separated names
#'   take the best weight.
#' @return Numeric value between 0 and 1.
#'
#' @examples
#' get_database_weight("FishBase")  # Returns 1.0
#' get_database_weight("ML")        # Returns 0.5 (alias of ML_prediction)
#'
get_database_weight <- function(source) {
  if (is.null(source) || length(source) == 0 || is.na(source[1]) || !nzchar(source[1])) {
    source <- "<missing>"
  }
  source <- as.character(source[1])

  # Multiple sources (comma-separated): use the best one
  if (grepl(",", source, fixed = TRUE)) {
    parts <- trimws(strsplit(source, ",", fixed = TRUE)[[1]])
    return(max(vapply(parts, get_database_weight, numeric(1))))
  }

  key <- source
  if (key %in% names(DATABASE_WEIGHT_ALIASES)) key <- DATABASE_WEIGHT_ALIASES[[key]]
  if (key %in% names(DATABASE_WEIGHTS)) return(DATABASE_WEIGHTS[[key]])
  ci <- match(tolower(key), tolower(names(DATABASE_WEIGHTS)))
  if (!is.na(ci)) return(DATABASE_WEIGHTS[[ci]])

  if (!isTRUE(.uq_weight_warned[[source]])) {
    assign(source, TRUE, envir = .uq_weight_warned)
    warning(sprintf("[uq] no database weight for source '%s'; using 0.5", source), call. = FALSE)
  }
  0.5
}


# =============================================================================
# SIZE CLASS THRESHOLD DISTANCE
# =============================================================================
# Species close to size class boundaries have higher uncertainty

#' Calculate Distance to Nearest Size Threshold
#'
#' Calculates how far a size measurement is from the nearest class boundary.
#' The distance is measured on the profile-adjusted size - the value
#' harmonize_size_class() classified - so an ecosystem profile cannot put the
#' size outside its own class (F37). Within 10% of a boundary the factor falls
#' linearly, but never below 0.3: a measured size on a boundary is still
#' measured data, and a factor of 0 used to turn the whole species "low".
#'
#' @param size_cm Numeric. Body size in cm (raw, before the profile adjustment).
#' @param size_class Character. Assigned size class (MS1-MS7).
#' @return Numeric factor in [0.3, 1], or NA_real_ for a missing or
#'   non-finite size, a missing class or a code outside MS1-MS7.
#'
#' @examples
#' calculate_threshold_distance(3.0, "MS3")  # Returns 1.0 (middle of class)
#' calculate_threshold_distance(4.9, "MS3")  # Returns 0.3 (near boundary)
#'
calculate_threshold_distance <- function(size_cm, size_class) {
  size_cm <- suppressWarnings(as.numeric(size_cm))
  if (length(size_cm) != 1L || !is.finite(size_cm) ||
        length(size_class) != 1L || is.na(size_class)) {
    return(NA_real_)
  }
  if (exists("apply_size_adjustment", mode = "function")) {
    size_cm <- apply_size_adjustment(size_cm)
  }
  if (!is.finite(size_cm)) return(NA_real_)

  # Load harmonization config if available. get_harm_config() returns
  # the per-session config when called inside a Shiny session and falls
  # back to the global default otherwise (e.g., from a build script).
  cfg <- if (exists("get_harm_config", mode = "function")) get_harm_config() else NULL
  if (!is.null(cfg) && !is.null(cfg$size_thresholds)) {
    thresholds <- cfg$size_thresholds
  } else {
    # Default thresholds
    thresholds <- list(
      MS1_MS2 = 0.1,
      MS2_MS3 = 1.0,
      MS3_MS4 = 5.0,
      MS4_MS5 = 20.0,
      MS5_MS6 = 50.0,
      MS6_MS7 = 150.0
    )
  }

  # Define class boundaries
  boundaries <- c(
    0,                       # MS1 lower
    thresholds$MS1_MS2,      # MS1/MS2
    thresholds$MS2_MS3,      # MS2/MS3
    thresholds$MS3_MS4,      # MS3/MS4
    thresholds$MS4_MS5,      # MS4/MS5
    thresholds$MS5_MS6,      # MS5/MS6
    thresholds$MS6_MS7,      # MS6/MS7
    Inf                      # MS7 upper
  )

  # Get class index
  class_num <- suppressWarnings(as.integer(gsub("MS", "", size_class)))
  if (is.na(class_num) || class_num < 1 || class_num > 7) {
    return(NA_real_)
  }

  # Get lower and upper boundaries for this class
  lower_bound <- boundaries[class_num]
  upper_bound <- boundaries[class_num + 1]

  # Calculate distance from both boundaries
  if (is.infinite(upper_bound)) {
    # MS7: only check lower boundary
    dist_to_boundary <- (size_cm - lower_bound) / lower_bound
  } else if (lower_bound == 0) {
    # MS1: only check upper boundary
    dist_to_boundary <- (upper_bound - size_cm) / upper_bound
  } else {
    # All other classes: check both boundaries
    dist_to_lower <- (size_cm - lower_bound) / (upper_bound - lower_bound)
    dist_to_upper <- (upper_bound - size_cm) / (upper_bound - lower_bound)
    dist_to_boundary <- min(dist_to_lower, dist_to_upper)
  }

  # Confidence factor: 1 far from a boundary, falling linearly within 10% of
  # one, floored at 0.3 (spec C3.4).
  min(1, max(0.3, dist_to_boundary / 0.1))
}


# =============================================================================
# TRAIT CONFIDENCE CALCULATION
# =============================================================================

#' Calculate Confidence for a Single Trait
#'
#' Calculates probabilistic confidence for a trait prediction based on:
#' - Data source authority
#' - Distance from class boundaries (for size traits)
#' - ML prediction probability (if ML-derived)
#'
#' @param trait_value Character. The harmonized trait value (e.g., "MS4", "FS1").
#' @param raw_value Numeric. The raw measurement (e.g., 15.0 cm for size). Optional.
#' @param source Character. Data source (e.g., "FishBase", "ML_prediction").
#' @param threshold_distance Numeric. Distance factor from boundaries (0-1). Default 1.0.
#' @param ml_probability Numeric. ML prediction probability (0-1). Optional.
#' @param phylo_confidence Numeric. Phylogenetic-imputation confidence (0-1).
#'   Optional; multiplies the weight like `ml_probability` (spec C3.3).
#' @return List with: confidence, interval_lower, interval_upper, category,
#'   source, notes. `category` is confidence_to_label(confidence).
#'
#' @examples
#' calculate_trait_confidence("MS4", 15.0, "FishBase", 1.0)
#' calculate_trait_confidence("FS1", NA, "ML", ml_probability = 0.75)
#'
calculate_trait_confidence <- function(trait_value, raw_value = NA, source = "Unknown",
                                      threshold_distance = 1.0, ml_probability = NA,
                                      phylo_confidence = NA) {

  # Base confidence from data source
  base_confidence <- get_database_weight(source)

  # Adjust for ML probability if available
  if (!is.null(ml_probability) && length(ml_probability) == 1 && !is.na(ml_probability)) {
    base_confidence <- base_confidence * ml_probability
  }

  # Adjust for the phylogenetic vote if available
  if (!is.null(phylo_confidence) && length(phylo_confidence) == 1 && !is.na(phylo_confidence)) {
    base_confidence <- base_confidence * phylo_confidence
  }

  # Adjust for threshold distance (for size traits)
  final_confidence <- base_confidence * threshold_distance

  # Calculate confidence interval
  # Interval width inversely proportional to confidence
  interval_width <- (1 - final_confidence) * 0.5  # 0 to 0.5

  # For categorical traits, interval represents probability of adjacent classes
  # For continuous traits (size), interval represents measurement uncertainty
  interval_lower <- max(0, final_confidence - interval_width)
  interval_upper <- min(1, final_confidence + interval_width)

  # Categorize confidence level: the canonical bands (0.34 / 0.67), never a
  # private copy (spec C3.2).
  category <- confidence_to_label(final_confidence)

  # Generate notes
  notes <- paste0(
    "Source: ", source,
    " | Confidence: ", round(final_confidence * 100, 1), "%",
    if (!is.null(ml_probability) && length(ml_probability) == 1 && !is.na(ml_probability)) paste0(" | ML prob: ", round(ml_probability * 100, 1), "%"),
    if (threshold_distance < 1.0) paste0(" | Near boundary (", round(threshold_distance * 100, 1), "%)"),
    ""
  )

  return(list(
    confidence = final_confidence,
    interval_lower = interval_lower,
    interval_upper = interval_upper,
    category = category,
    source = source,
    notes = notes
  ))
}


# =============================================================================
# UNCERTAINTY PROPAGATION THROUGH NETWORKS
# =============================================================================

#' Propagate Uncertainty Through Food Web Edges
#'
#' Calculates confidence for food web interactions based on the confidence
#' of all involved traits (size, foraging, mobility, habitat, protection).
#' Uses geometric mean to combine trait confidences.
#'
#' @param species_traits Data frame. Must have columns: species, MS_confidence,
#'   FS_confidence, MB_confidence, EP_confidence, PR_confidence.
#' @param adjacency_matrix Matrix or data frame. Food web adjacency matrix
#'   with predator rows and prey columns. Optional.
#' @return Data frame with edge confidence scores, or species confidence summary
#'   if adjacency_matrix not provided.
#'
#' @examples
#' # Species-level confidence summary
#' propagate_uncertainty(species_traits)
#'
#' # Edge-level confidence
#' propagate_uncertainty(species_traits, adjacency_matrix)
#'
propagate_uncertainty <- function(species_traits, adjacency_matrix = NULL) {

  # Ensure required columns exist
  required_cols <- c("species")
  trait_cols <- c("MS_confidence", "FS_confidence", "MB_confidence",
                  "EP_confidence", "PR_confidence")

  if (!all(required_cols %in% names(species_traits))) {
    stop("species_traits must contain 'species' column")
  }

  # Add missing confidence columns with default 0.5
  for (col in trait_cols) {
    if (!(col %in% names(species_traits))) {
      species_traits[[col]] <- 0.5
    }
  }

  # Calculate overall species confidence (geometric mean of all traits)
  species_traits$overall_confidence <- apply(
    species_traits[, trait_cols], 1,
    function(x) {
      valid <- x[!is.na(x) & x > 0]
      if (length(valid) == 0) return(0.5)
      exp(mean(log(valid)))  # Geometric mean
    }
  )

  # If no adjacency matrix, return species-level summary
  if (is.null(adjacency_matrix)) {
    return(species_traits[, c("species", trait_cols, "overall_confidence")])
  }

  # Otherwise, calculate edge-level confidence
  predators <- rownames(adjacency_matrix)
  prey <- colnames(adjacency_matrix)

  edge_confidence <- data.frame(
    predator = character(),
    prey = character(),
    edge_confidence = numeric(),
    predator_confidence = numeric(),
    prey_confidence = numeric(),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(predators)) {
    for (j in seq_along(prey)) {
      if (adjacency_matrix[i, j] > 0) {  # Edge exists
        pred_name <- predators[i]
        prey_name <- prey[j]

        # Get confidence for both species
        pred_conf <- species_traits$overall_confidence[species_traits$species == pred_name]
        prey_conf <- species_traits$overall_confidence[species_traits$species == prey_name]

        if (length(pred_conf) == 0) pred_conf <- 0.5
        if (length(prey_conf) == 0) prey_conf <- 0.5

        # Edge confidence = geometric mean of predator and prey
        edge_conf <- sqrt(pred_conf * prey_conf)

        edge_confidence <- rbind(edge_confidence, data.frame(
          predator = pred_name,
          prey = prey_name,
          edge_confidence = edge_conf,
          predator_confidence = pred_conf,
          prey_confidence = prey_conf,
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  return(edge_confidence)
}


# =============================================================================
# CONFIDENCE VISUALIZATION HELPERS
# =============================================================================

#' Map Confidence to Node Size
#'
#' Converts confidence score to node size for network visualization.
#' Higher confidence = larger nodes.
#'
#' @param confidence Numeric vector. Confidence scores (0-1).
#' @param base_size Numeric. Base node size. Default 10.
#' @param scale_factor Numeric. Scaling factor. Default 30.
#' @return Numeric vector. Node sizes.
#'
map_confidence_to_size <- function(confidence, base_size = 10, scale_factor = 30) {
  base_size + (confidence * scale_factor)
}


#' Map Confidence to Border Width
#'
#' Converts confidence score to node border width for visualization.
#' Lower confidence = thicker border (more uncertainty).
#'
#' @param confidence Numeric vector. Confidence scores (0-1).
#' @param min_width Numeric. Minimum border width. Default 1.
#' @param max_width Numeric. Maximum border width. Default 5.
#' @return Numeric vector. Border widths.
#'
map_confidence_to_border <- function(confidence, min_width = 1, max_width = 5) {
  min_width + ((1 - confidence) * (max_width - min_width))
}


#' Map Confidence to Edge Opacity
#'
#' Converts edge confidence to opacity for visualization.
#' Lower confidence = more transparent (uncertain interactions).
#'
#' @param confidence Numeric vector. Confidence scores (0-1).
#' @param min_opacity Numeric. Minimum opacity. Default 0.3.
#' @param max_opacity Numeric. Maximum opacity. Default 1.0.
#' @return Numeric vector. Opacity values.
#'
map_confidence_to_opacity <- function(confidence, min_opacity = 0.3, max_opacity = 1.0) {
  min_opacity + (confidence * (max_opacity - min_opacity))
}


# =============================================================================
# BATCH CONFIDENCE CALCULATION
# =============================================================================

#' Calculate Confidence for All Traits in a Species Record
#'
#' Scores every non-NA trait of MS, FS, MB, EP, PR, RS, TT and ST (spec C3.1:
#' `T_confidence` is NA if and only if `T` is NA). The weight of `T_source` is
#' multiplied by `T_ml_probability` only when the source is "ML", and by
#' `T_phylo_confidence` only when it is "Phylogenetic" (C3.3): the ML block
#' predicts every missing trait, but its probability says nothing about a
#' trait FishBase supplied. MS is also scaled by its boundary distance when
#' `size_cm` is given. A trait whose scoring fails gets a warning and an NA
#' confidence; it never aborts the lookup (C3.8).
#'
#' @param trait_record List with the trait codes and, per trait,
#'   `<T>_source`, `<T>_ml_probability`, `<T>_phylo_confidence`; optionally
#'   `size_cm` (the measured size MS was harmonised from).
#' @return List with `<T>_confidence`, `<T>_interval_lower`,
#'   `<T>_interval_upper` and `<T>_confidence_category` for each scored trait.
#'
#' @examples
#' record <- list(MS = "MS4", size_cm = 15, MS_source = "FishBase",
#'                FS = "FS1", FS_source = "WoRMS")
#' calculate_all_trait_confidence(record)
#'
calculate_all_trait_confidence <- function(trait_record) {
  result <- list()
  scalar <- function(x) if (length(x) >= 1L) x[[1]] else NA

  for (trait in c("MS", "FS", "MB", "EP", "PR", "RS", "TT", "ST")) {
    value <- scalar(trait_record[[trait]])
    if (is.null(value) || is.na(value)) next
    source <- scalar(trait_record[[paste0(trait, "_source")]])
    if (is.null(source)) source <- NA_character_

    scored <- tryCatch({
      ml_p <- if (identical(source, "ML")) scalar(trait_record[[paste0(trait, "_ml_probability")]]) else NA
      phylo_p <- if (identical(source, "Phylogenetic")) {
        scalar(trait_record[[paste0(trait, "_phylo_confidence")]])
      } else {
        NA
      }
      dist <- 1.0
      if (trait == "MS" && !is.null(trait_record$size_cm)) {
        dist <- calculate_threshold_distance(trait_record$size_cm, value)
        if (is.na(dist)) dist <- 1.0
      }
      calculate_trait_confidence(
        trait_value = value,
        raw_value = if (trait == "MS") trait_record$size_cm %||% NA else NA,
        source = source,
        threshold_distance = dist,
        ml_probability = ml_p %||% NA,
        phylo_confidence = phylo_p %||% NA
      )
    }, error = function(e) {
      warning(sprintf("[uq] %s confidence failed (source '%s'): %s",
                      trait, source, conditionMessage(e)), call. = FALSE)
      NULL
    })

    if (is.null(scored) || !isTRUE(is.finite(scored$confidence))) {
      result[[paste0(trait, "_confidence")]] <- NA_real_
      next
    }
    result[[paste0(trait, "_confidence")]] <- scored$confidence
    result[[paste0(trait, "_interval_lower")]] <- scored$interval_lower
    result[[paste0(trait, "_interval_upper")]] <- scored$interval_upper
    result[[paste0(trait, "_confidence_category")]] <- scored$category
  }

  result
}


# =============================================================================
# NOTE: %||% operator is now defined in validation_utils.R
# Do not redefine here - use the canonical version from validation_utils.R


# =============================================================================
# EXPORT FUNCTIONS
# =============================================================================

# Main functions exported for use in other modules:
# - get_database_weight()
# - calculate_threshold_distance()
# - calculate_trait_confidence()
# - propagate_uncertainty()
# - calculate_all_trait_confidence()
# - map_confidence_to_size()
# - map_confidence_to_border()
# - map_confidence_to_opacity()
