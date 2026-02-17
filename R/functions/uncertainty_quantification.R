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
#
# Date: 2025-12-25
# Version: 1.0
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

  # Taxonomic databases (medium authority - good for traits, limited for ecology)
  "WoRMS" = 0.60,

  # Predicted/inferred data (lower authority)
  "ML_prediction" = 0.50,
  "Phylogenetic" = 0.40,
  "Rule-based" = 0.30
)

#' Get Database Authority Weight
#'
#' Returns the authority weight for a given data source.
#'
#' @param source Character string. Data source name.
#' @return Numeric value between 0 and 1. Returns 0.5 if source not found.
#'
#' @examples
#' get_database_weight("FishBase")  # Returns 1.0
#' get_database_weight("ML_prediction")  # Returns 0.5
#'
get_database_weight <- function(source) {
  if (is.null(source) || is.na(source)) {
    return(0.5)
  }

  # Check for multiple sources (comma-separated)
  if (grepl(",", source)) {
    sources <- trimws(strsplit(source, ",")[[1]])
    weights <- sapply(sources, function(s) {
      if (s %in% names(DATABASE_WEIGHTS)) {
        DATABASE_WEIGHTS[[s]]
      } else {
        0.5
      }
    })
    return(max(weights))  # Use best source
  }

  # Single source
  if (source %in% names(DATABASE_WEIGHTS)) {
    return(DATABASE_WEIGHTS[[source]])
  } else {
    return(0.5)  # Default for unknown sources
  }
}


# =============================================================================
# SIZE CLASS THRESHOLD DISTANCE
# =============================================================================
# Species close to size class boundaries have higher uncertainty

#' Calculate Distance to Nearest Size Threshold
#'
#' Calculates how far a size measurement is from the nearest class boundary.
#' Species close to boundaries (within 10%) have reduced confidence.
#'
#' @param size_cm Numeric. Body size in cm.
#' @param size_class Character. Assigned size class (MS1-MS7).
#' @return Numeric. Distance factor between 0 (at boundary) and 1 (far from boundary).
#'
#' @examples
#' calculate_threshold_distance(3.0, "MS3")  # Returns 1.0 (middle of class)
#' calculate_threshold_distance(4.9, "MS3")  # Returns 0.02 (near boundary)
#'
calculate_threshold_distance <- function(size_cm, size_class) {
  if (is.na(size_cm) || is.na(size_class)) {
    return(1.0)  # No penalty if missing
  }

  # Load harmonization config if available
  if (exists("HARMONIZATION_CONFIG")) {
    thresholds <- HARMONIZATION_CONFIG$size_thresholds
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
  class_num <- as.integer(gsub("MS", "", size_class))
  if (is.na(class_num) || class_num < 1 || class_num > 7) {
    return(1.0)
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

  # Convert to confidence factor (0 at boundary, 1 far from boundary)
  # Within 10% of boundary → reduced confidence
  if (dist_to_boundary < 0.1) {
    return(dist_to_boundary / 0.1)  # Linear decrease from 1.0 to 0
  } else {
    return(1.0)
  }
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
#' @return List with: confidence, interval_lower, interval_upper, source, notes
#'
#' @examples
#' calculate_trait_confidence("MS4", 15.0, "FishBase", 1.0)
#' calculate_trait_confidence("FS1", NA, "ML_prediction", ml_probability = 0.75)
#'
calculate_trait_confidence <- function(trait_value, raw_value = NA, source = "Unknown",
                                      threshold_distance = 1.0, ml_probability = NA) {

  # Base confidence from data source
  base_confidence <- get_database_weight(source)

  # Adjust for ML probability if available
  if (!is.na(ml_probability)) {
    base_confidence <- base_confidence * ml_probability
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

  # Categorize confidence level
  if (final_confidence >= 0.7) {
    category <- "high"
  } else if (final_confidence >= 0.5) {
    category <- "medium"
  } else {
    category <- "low"
  }

  # Generate notes
  notes <- paste0(
    "Source: ", source,
    " | Confidence: ", round(final_confidence * 100, 1), "%",
    if (!is.na(ml_probability)) paste0(" | ML prob: ", round(ml_probability * 100, 1), "%"),
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
#' Wrapper function to calculate confidence for all 5 traits (MS, FS, MB, EP, PR)
#' given a complete trait record with sources and raw measurements.
#'
#' @param trait_record List. Must contain: MS, FS, MB, EP, PR (trait values),
#'   and optionally: MS_source, size_cm, MS_ml_probability, etc.
#' @return List with confidence metadata for all traits.
#'
#' @examples
#' record <- list(MS = "MS4", size_cm = 15, MS_source = "FishBase",
#'                FS = "FS1", FS_source = "WoRMS")
#' calculate_all_trait_confidence(record)
#'
calculate_all_trait_confidence <- function(trait_record) {

  result <- list()

  # MS - Max Size
  if (!is.null(trait_record$MS) && !is.na(trait_record$MS)) {
    threshold_dist <- if (!is.null(trait_record$size_cm)) {
      calculate_threshold_distance(trait_record$size_cm, trait_record$MS)
    } else {
      1.0
    }

    ms_conf <- calculate_trait_confidence(
      trait_value = trait_record$MS,
      raw_value = trait_record$size_cm,
      source = trait_record$MS_source %||% "Unknown",
      threshold_distance = threshold_dist,
      ml_probability = trait_record$MS_ml_probability
    )

    result$MS_confidence <- ms_conf$confidence
    result$MS_interval_lower <- ms_conf$interval_lower
    result$MS_interval_upper <- ms_conf$interval_upper
    result$MS_confidence_category <- ms_conf$category
  }

  # FS - Foraging Strategy
  if (!is.null(trait_record$FS) && !is.na(trait_record$FS)) {
    fs_conf <- calculate_trait_confidence(
      trait_value = trait_record$FS,
      source = trait_record$FS_source %||% "Unknown",
      ml_probability = trait_record$FS_ml_probability
    )

    result$FS_confidence <- fs_conf$confidence
    result$FS_interval_lower <- fs_conf$interval_lower
    result$FS_interval_upper <- fs_conf$interval_upper
    result$FS_confidence_category <- fs_conf$category
  }

  # MB - Mobility
  if (!is.null(trait_record$MB) && !is.na(trait_record$MB)) {
    mb_conf <- calculate_trait_confidence(
      trait_value = trait_record$MB,
      source = trait_record$MB_source %||% "Unknown",
      ml_probability = trait_record$MB_ml_probability
    )

    result$MB_confidence <- mb_conf$confidence
    result$MB_interval_lower <- mb_conf$interval_lower
    result$MB_interval_upper <- mb_conf$interval_upper
    result$MB_confidence_category <- mb_conf$category
  }

  # EP - Environmental Position
  if (!is.null(trait_record$EP) && !is.na(trait_record$EP)) {
    ep_conf <- calculate_trait_confidence(
      trait_value = trait_record$EP,
      source = trait_record$EP_source %||% "Unknown",
      ml_probability = trait_record$EP_ml_probability
    )

    result$EP_confidence <- ep_conf$confidence
    result$EP_interval_lower <- ep_conf$interval_lower
    result$EP_interval_upper <- ep_conf$interval_upper
    result$EP_confidence_category <- ep_conf$category
  }

  # PR - Protection
  if (!is.null(trait_record$PR) && !is.na(trait_record$PR)) {
    pr_conf <- calculate_trait_confidence(
      trait_value = trait_record$PR,
      source = trait_record$PR_source %||% "Unknown",
      ml_probability = trait_record$PR_ml_probability
    )

    result$PR_confidence <- pr_conf$confidence
    result$PR_interval_lower <- pr_conf$interval_lower
    result$PR_interval_upper <- pr_conf$interval_upper
    result$PR_confidence_category <- pr_conf$category
  }

  return(result)
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
