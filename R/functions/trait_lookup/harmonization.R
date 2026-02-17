# =============================================================================
# TRAIT LOOKUP - Harmonization Rules
# =============================================================================
# Functions for converting raw traits to standardized trait codes (MS, FS, MB, EP, PR).
# Part of the trait_lookup module (split from trait_lookup.R)
# =============================================================================

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
# TRAIT HARMONIZATION (Raw -> Categorical Classes)
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
