# =============================================================================
# Phylogenetic Trait Imputation
# =============================================================================
#
# This module implements taxonomic distance-based trait imputation, using
# WoRMS classification to infer missing traits from closely related species.
#
# Strategy:
# - Calculate taxonomic distance using WoRMS hierarchy (phylum → genus)
# - Find closest relatives with complete trait data
# - Weighted majority vote based on phylogenetic proximity
# - Conservative approach: only impute when consensus is strong
#
# Date: 2025-12-25
# Version: 1.0
# =============================================================================

# =============================================================================
# TAXONOMIC DISTANCE CALCULATION
# =============================================================================

#' Calculate Taxonomic Distance Between Two Species
#'
#' Calculates phylogenetic distance based on WoRMS classification hierarchy.
#' Distance increases at each taxonomic rank where species differ.
#'
#' Rank weights (higher = more distant):
#' - Phylum: 5 points
#' - Class: 4 points
#' - Order: 3 points
#' - Family: 2 points
#' - Genus: 1 point
#' - Species: 0 points (same species)
#'
#' @param taxonomy1 List with: phylum, class, order, family, genus
#' @param taxonomy2 List with: phylum, class, order, family, genus
#' @return Numeric distance (0 = identical, 5+ = very distant)
#'
#' @examples
#' # Same genus (Gadus morhua vs Gadus ogac)
#' calculate_taxonomic_distance(
#'   list(phylum="Chordata", class="Actinopteri", order="Gadiformes",
#'        family="Gadidae", genus="Gadus"),
#'   list(phylum="Chordata", class="Actinopteri", order="Gadiformes",
#'        family="Gadidae", genus="Gadus")
#' )
#' # Returns: 0 (same genus)
#'
#' # Same family, different genus
#' calculate_taxonomic_distance(
#'   list(phylum="Chordata", class="Actinopteri", order="Gadiformes",
#'        family="Gadidae", genus="Gadus"),
#'   list(phylum="Chordata", class="Actinopteri", order="Gadiformes",
#'        family="Gadidae", genus="Melanogrammus")
#' )
#' # Returns: 1 (different genus, same family)
#'
calculate_taxonomic_distance <- function(taxonomy1, taxonomy2) {

  # Define rank hierarchy and weights
  ranks <- c("phylum", "class", "order", "family", "genus")
  weights <- c(5, 4, 3, 2, 1)

  distance <- 0

  for (i in seq_along(ranks)) {
    rank <- ranks[i]

    # Get values for this rank
    val1 <- taxonomy1[[rank]]
    val2 <- taxonomy2[[rank]]

    # Handle missing values
    if (is.null(val1) || is.na(val1) || is.null(val2) || is.na(val2)) {
      # If either is missing at this rank, assume different
      # Use weight of this rank
      distance <- distance + weights[i]
      break  # Can't compare deeper ranks
    }

    # Normalize for comparison (case-insensitive, trim whitespace)
    val1 <- tolower(trimws(as.character(val1)))
    val2 <- tolower(trimws(as.character(val2)))

    # If different at this rank, add weight and stop
    # (all deeper ranks will also be different)
    if (val1 != val2) {
      distance <- distance + weights[i]
      break
    }
  }

  return(distance)
}


# =============================================================================
# FIND CLOSEST RELATIVES
# =============================================================================

#' Find Closest Taxonomic Relatives with Complete Trait Data
#'
#' Searches cached species for the closest phylogenetic relatives that have
#' complete trait information for the target traits.
#'
#' @param target_taxonomy List with WoRMS classification (phylum, class, order, family, genus)
#' @param target_traits List with current trait values (MS, FS, MB, EP, PR)
#' @param cache_dir Directory containing cached species data
#' @param max_distance Maximum taxonomic distance to consider (default: 3 = within family)
#' @param min_matches Minimum number of relatives required (default: 3)
#' @param traits_needed Which traits to look for (default: c("MS", "FS", "MB", "EP", "PR"))
#' @return Data frame with: species, distance, and trait values for each relative
#'
#' @examples
#' relatives <- find_closest_relatives(
#'   target_taxonomy = list(phylum="Chordata", class="Actinopteri",
#'                         order="Gadiformes", family="Gadidae", genus="Gadus"),
#'   target_traits = list(MS=NA, FS="FS1", MB="MB5", EP=NA, PR=NA),
#'   cache_dir = "cache/taxonomy",
#'   max_distance = 3,
#'   min_matches = 5
#' )
#'
find_closest_relatives <- function(target_taxonomy,
                                   target_traits,
                                   cache_dir,
                                   max_distance = 3,
                                   min_matches = 3,
                                   traits_needed = c("MS", "FS", "MB", "EP", "PR")) {

  if (!dir.exists(cache_dir)) {
    return(data.frame())
  }

  # Find which traits are missing
  missing_traits <- traits_needed[sapply(traits_needed, function(t) {
    is.null(target_traits[[t]]) || is.na(target_traits[[t]])
  })]

  if (length(missing_traits) == 0) {
    return(data.frame())  # No missing traits to impute
  }

  # Get all cached species
  cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)

  if (length(cache_files) == 0) {
    return(data.frame())
  }

  # Search for relatives with complete data
  relatives <- data.frame(
    species = character(),
    distance = numeric(),
    stringsAsFactors = FALSE
  )

  # Add columns for each trait
  for (trait in traits_needed) {
    relatives[[trait]] <- character()
  }

  for (cache_file in cache_files) {
    tryCatch({
      cache_data <- readRDS(cache_file)

      # Extract taxonomy
      relative_taxonomy <- NULL
      if (!is.null(cache_data$harmonized)) {
        relative_taxonomy <- list(
          phylum = cache_data$harmonized$phylum,
          class = cache_data$harmonized$class,
          order = cache_data$harmonized$order,
          family = cache_data$harmonized$family,
          genus = cache_data$harmonized$genus
        )
      } else if (!is.null(cache_data$worms_taxonomy)) {
        relative_taxonomy <- list(
          phylum = cache_data$worms_taxonomy$phylum,
          class = cache_data$worms_taxonomy$class,
          order = cache_data$worms_taxonomy$order,
          family = cache_data$worms_taxonomy$family,
          genus = cache_data$worms_taxonomy$genus
        )
      }

      if (is.null(relative_taxonomy)) next

      # Calculate distance
      distance <- calculate_taxonomic_distance(target_taxonomy, relative_taxonomy)

      # Skip if too distant
      if (distance > max_distance) next

      # Extract trait values
      relative_traits <- list()
      if (!is.null(cache_data$harmonized)) {
        for (trait in traits_needed) {
          relative_traits[[trait]] <- cache_data$harmonized[[trait]]
        }
      } else if (!is.null(cache_data$traits)) {
        for (trait in traits_needed) {
          relative_traits[[trait]] <- cache_data$traits[[trait]]
        }
      }

      # Check if this relative has the needed traits
      has_needed_traits <- all(sapply(missing_traits, function(t) {
        !is.null(relative_traits[[t]]) && !is.na(relative_traits[[t]])
      }))

      if (!has_needed_traits) next

      # Get species name
      species_name <- if (!is.null(cache_data$species)) {
        cache_data$species
      } else if (!is.null(cache_data$harmonized$species)) {
        cache_data$harmonized$species
      } else {
        gsub("\\.rds$", "", basename(cache_file))
      }

      # Add to relatives
      new_row <- data.frame(
        species = species_name,
        distance = distance,
        stringsAsFactors = FALSE
      )

      for (trait in traits_needed) {
        new_row[[trait]] <- relative_traits[[trait]] %||% NA
      }

      relatives <- rbind(relatives, new_row)

    }, error = function(e) {
      # Skip problematic cache files
    })
  }

  # Sort by distance (closest first)
  if (nrow(relatives) > 0) {
    relatives <- relatives[order(relatives$distance), ]
  }

  return(relatives)
}


# =============================================================================
# TRAIT IMPUTATION WITH WEIGHTED VOTING
# =============================================================================

#' Impute Missing Traits from Phylogenetic Relatives
#'
#' Uses weighted majority voting to impute missing trait values based on
#' closely related species. Closer relatives receive higher weights.
#'
#' Weighting scheme:
#' - Same genus (distance 0): weight = 5
#' - Same family (distance 1-2): weight = 3-4
#' - Same order (distance 3): weight = 2
#'
#' @param target_traits List with current trait values (NA for missing)
#' @param relatives Data frame from find_closest_relatives()
#' @param min_agreement Minimum proportion of relatives that must agree (default: 0.6)
#' @return List with imputed trait values and metadata
#'
#' @examples
#' imputed <- impute_traits_from_relatives(
#'   target_traits = list(MS=NA, FS="FS1", MB="MB5", EP=NA, PR=NA),
#'   relatives = relatives_df,
#'   min_agreement = 0.6
#' )
#'
impute_traits_from_relatives <- function(target_traits,
                                         relatives,
                                         min_agreement = 0.6) {

  if (nrow(relatives) == 0) {
    return(list())
  }

  # Define weighting by distance
  # Closer relatives get higher weights
  distance_weights <- c(
    "0" = 5,  # Same genus
    "1" = 4,  # Different genus, same family
    "2" = 3,  # Different family, same order
    "3" = 2   # Different order, same class
  )

  result <- list()

  # Get trait names
  trait_names <- c("MS", "FS", "MB", "EP", "PR")

  for (trait in trait_names) {
    # Skip if already have this trait
    if (!is.null(target_traits[[trait]]) && !is.na(target_traits[[trait]])) {
      next
    }

    # Skip if no relatives have this trait
    if (!(trait %in% names(relatives))) {
      next
    }

    # Get trait values from relatives
    relative_values <- relatives[[trait]]
    relative_distances <- relatives$distance

    # Remove NA values
    valid_idx <- !is.na(relative_values)
    if (sum(valid_idx) == 0) next

    relative_values <- relative_values[valid_idx]
    relative_distances <- relative_distances[valid_idx]

    # Calculate weights for each relative
    weights <- sapply(relative_distances, function(d) {
      if (as.character(d) %in% names(distance_weights)) {
        distance_weights[[as.character(d)]]
      } else {
        1  # Default weight for very distant relatives
      }
    })

    # Weighted voting
    vote_table <- table(relative_values)
    weighted_votes <- sapply(names(vote_table), function(value) {
      sum(weights[relative_values == value])
    })

    # Total weight
    total_weight <- sum(weights)

    # Winner and its support
    winner <- names(weighted_votes)[which.max(weighted_votes)]
    winner_weight <- max(weighted_votes)
    winner_agreement <- winner_weight / total_weight

    # Only impute if agreement threshold met
    if (winner_agreement >= min_agreement) {
      # Calculate confidence based on:
      # - Agreement level
      # - Number of relatives
      # - Average distance

      n_relatives <- length(relative_values)
      avg_distance <- mean(relative_distances)

      # Base confidence from agreement
      confidence <- winner_agreement

      # Adjust for number of relatives (more = better)
      if (n_relatives >= 5) {
        confidence <- confidence * 1.0
      } else if (n_relatives >= 3) {
        confidence <- confidence * 0.9
      } else {
        confidence <- confidence * 0.7
      }

      # Adjust for distance (closer = better)
      if (avg_distance <= 1) {
        confidence <- confidence * 1.0
      } else if (avg_distance <= 2) {
        confidence <- confidence * 0.9
      } else {
        confidence <- confidence * 0.8
      }

      result[[trait]] <- list(
        value = winner,
        confidence = confidence,
        source = "Phylogenetic",
        n_relatives = n_relatives,
        avg_distance = avg_distance,
        agreement = winner_agreement,
        relatives_used = paste(head(relatives$species[valid_idx], 5), collapse = ", ")
      )
    }
  }

  return(result)
}


# =============================================================================
# INTEGRATION FUNCTION
# =============================================================================

#' Apply Phylogenetic Imputation to Fill Missing Traits
#'
#' Main integration function that combines relative finding and trait imputation.
#' Called by lookup_species_traits() after database lookups and ML predictions.
#'
#' @param species_name Target species name
#' @param current_traits List with current trait values (NA for missing)
#' @param taxonomy WoRMS taxonomy for target species
#' @param cache_dir Cache directory
#' @param max_distance Maximum taxonomic distance (default: 3 = within order)
#' @param min_relatives Minimum relatives needed (default: 3)
#' @param min_agreement Minimum voting agreement (default: 0.6)
#' @param verbose Print detailed information (default: FALSE)
#' @return Updated traits list with imputed values
#'
apply_phylogenetic_imputation <- function(species_name,
                                          current_traits,
                                          taxonomy,
                                          cache_dir,
                                          max_distance = 3,
                                          min_relatives = 3,
                                          min_agreement = 0.6,
                                          verbose = FALSE) {

  # Check which traits are missing
  missing_traits <- c(
    if (is.na(current_traits$MS)) "MS",
    if (is.na(current_traits$FS)) "FS",
    if (is.na(current_traits$MB)) "MB",
    if (is.na(current_traits$EP)) "EP",
    if (is.na(current_traits$PR)) "PR"
  )

  if (length(missing_traits) == 0) {
    return(current_traits)  # All traits already known
  }

  if (verbose) {
    message("\n╔═══════════════════════════════════════════════════════════════╗")
    message("║ PHYLOGENETIC IMPUTATION                                        ║")
    message("╚═══════════════════════════════════════════════════════════════╝")
    message("  Missing traits: ", paste(missing_traits, collapse = ", "))
    message("  Searching for close relatives...")
  }

  # Find closest relatives
  relatives <- find_closest_relatives(
    target_taxonomy = taxonomy,
    target_traits = current_traits,
    cache_dir = cache_dir,
    max_distance = max_distance,
    min_matches = min_relatives,
    traits_needed = c("MS", "FS", "MB", "EP", "PR")
  )

  if (nrow(relatives) == 0) {
    if (verbose) {
      message("  ⚠️  No close relatives found")
    }
    return(current_traits)
  }

  if (verbose) {
    message("  Found ", nrow(relatives), " relatives within distance ", max_distance)
    message("  Closest relatives:")
    for (i in 1:min(5, nrow(relatives))) {
      message("    ", i, ". ", relatives$species[i], " (distance: ", relatives$distance[i], ")")
    }
  }

  # Impute traits
  imputations <- impute_traits_from_relatives(
    target_traits = current_traits,
    relatives = relatives,
    min_agreement = min_agreement
  )

  if (length(imputations) == 0) {
    if (verbose) {
      message("  ⚠️  No traits could be imputed (insufficient agreement)")
    }
    return(current_traits)
  }

  # Apply imputations
  for (trait in names(imputations)) {
    if (is.na(current_traits[[trait]])) {
      imputation <- imputations[[trait]]
      current_traits[[trait]] <- imputation$value

      # Store metadata
      current_traits[[paste0(trait, "_source")]] <- "Phylogenetic"
      current_traits[[paste0(trait, "_phylo_confidence")]] <- imputation$confidence
      current_traits[[paste0(trait, "_phylo_n_relatives")]] <- imputation$n_relatives
      current_traits[[paste0(trait, "_phylo_avg_distance")]] <- imputation$avg_distance
      current_traits[[paste0(trait, "_phylo_agreement")]] <- imputation$agreement

      if (verbose) {
        message("  ✓ ", trait, " = ", imputation$value,
                " (", round(imputation$agreement * 100, 0), "% agreement, ",
                imputation$n_relatives, " relatives)")
      }
    }
  }

  return(current_traits)
}


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Null Coalescing Operator
# NOTE: %||% operator is now defined in validation_utils.R
# Do not redefine here - use the canonical version from validation_utils.R


# =============================================================================
# EXPORT FUNCTIONS
# =============================================================================

# Main functions exported for use in other modules:
# - calculate_taxonomic_distance()
# - find_closest_relatives()
# - impute_traits_from_relatives()
# - apply_phylogenetic_imputation()
