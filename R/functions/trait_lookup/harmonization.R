# =============================================================================
# TRAIT LOOKUP - Harmonization Rules
# =============================================================================
# Functions for converting raw traits to standardized trait codes (MS, FS, MB, EP, PR).
# =============================================================================

#' Convert categorical confidence label to numeric [0, 1]
#'
#' Single source of truth for the categorical-to-numeric mapping that
#' shows up in three places (build script per-source literals,
#' harmonize_fuzzy_*() return values, ML _confidence outputs). The
#' inline `.conf_to_num` map in the build script's ontology block uses
#' the same values; that copy can collapse onto this helper.
#'
#' @param label "none" / "low" / "medium" / "high" (case-insensitive)
#' @return Numeric in [0, 1]; NA for unrecognized labels.
#' @export
confidence_to_num <- function(label) {
  if (is.null(label) || length(label) == 0) return(NA_real_)
  label <- tolower(as.character(label))
  conf_map <- c(none = 0.0, low = 0.33, medium = 0.66, high = 1.0)
  out <- unname(conf_map[label])
  out[is.na(out)] <- NA_real_
  out
}

#' Convert numeric confidence to categorical label
#'
#' Inverse of confidence_to_num(). Useful for UI display and CSV exports
#' where readers expect a label. Bands chosen to round-trip with
#' confidence_to_num: `confidence_to_label(confidence_to_num("high"))`
#' returns "high".
#'
#' @param x Numeric in [0, 1].
#' @return One of "none" / "low" / "medium" / "high" / NA.
#' @export
confidence_to_label <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- suppressWarnings(as.numeric(x))
  out <- vapply(x, function(v) {
    if (is.na(v))        return(NA_character_)
    if (v <= 0)          return("none")
    if (v <  0.34)       return("low")
    if (v <  0.67)       return("medium")
    "high"
  }, character(1))
  out
}


#' Get the active harmonization config, preferring per-session overrides
#'
#' Inside a Shiny session, returns the session-local config (set by
#' harmonization_settings_server when the user adjusts size-threshold
#' sliders). Outside Shiny — build scripts, regression tests, console
#' use — falls back to the unmutated process-wide HARMONIZATION_CONFIG.
#'
#' Pre-PR9α the harmonize_* helpers read HARMONIZATION_CONFIG directly
#' from globalenv, so the slider's writeback (assign envir=globalenv) at
#' harmonization_settings_server.R:37 contaminated every concurrent
#' Shiny session. The accessor reroutes the read path to session$userData
#' and the writeback gets deleted.
#'
#' @return The harmonization config list, or NULL if no config is loaded
#'   anywhere (callers should treat NULL as "use sensible defaults").
get_harm_config <- function() {
  session <- if (requireNamespace("shiny", quietly = TRUE)) {
    shiny::getDefaultReactiveDomain()
  } else NULL
  if (!is.null(session) && !is.null(session$userData$harm_config)) {
    return(session$userData$harm_config)
  }
  if (!exists("HARMONIZATION_CONFIG", envir = .GlobalEnv)) return(NULL)
  get("HARMONIZATION_CONFIG", envir = .GlobalEnv)
}


#' Revision of the raw values the trait lookups return
#'
#' harm_config_hash() hashes it in. Bump it when a lookup fix changes raw
#' values that cached envelopes already hold (sizes, weights, taxonomy), so
#' every cache/taxonomy envelope written before the fix is a miss and is
#' refreshed on first read instead of serving the old value for 30 days.
#' 2L: C-6a (WoRMS body-size units, FishBase grams, NA ranks).
TRAIT_LOOKUP_REVISION <- 2L


#' Hash of the effective harmonization config (trait-cache key, F72)
#'
#' Harmonized codes in cache/taxonomy/<species>.rds depend on the config that
#' produced them, and every session shares those files. Writers stamp this
#' hash into the envelope; readers pass their own hash to read_cache_field()
#' and treat a mismatch as a miss. `last_modified` and `version` are dropped
#' (they do not change any code). The JSON text is hashed rather than the R
#' object, so 150L after a JSON round trip hashes like 150.
#'
#' The trait vocabulary (TRAIT_VOCAB: version, default patterns, precedence,
#' taxon rules) is hashed in too. It is not part of any config, so a JSON file
#' cannot pin it, but it changes the codes just as much: bumping
#' trait_vocab_version (or editing a default pattern) turns every envelope
#' written under the old vocabulary into a miss for every reader and for
#' phylogenetic imputation, instead of serving old MB codes for 30 days.
#' TRAIT_LOOKUP_REVISION does the same for fixes to the raw lookup values.
#'
#' @param cfg Config list; defaults to this session's config.
#' @return Character(1) xxhash64 digest, or NULL when no config is loaded.
harm_config_hash <- function(cfg = get_harm_config()) {
  if (is.null(cfg)) return(NULL)
  cfg$last_modified <- NULL
  cfg$version <- NULL
  cfg$.trait_vocab <- get_trait_vocab()
  cfg$.lookup_revision <- TRAIT_LOOKUP_REVISION
  json <- jsonlite::toJSON(cfg, auto_unbox = TRUE, digits = NA)
  digest::digest(as.character(json), algo = "xxhash64", serialize = FALSE)
}


#' Hash of the process-wide default config
#'
#' For cache writes whose codes were NOT harmonized with the session config:
#' offline-DB rows were harmonized at build time with the defaults. Reads the
#' same global fallback as get_harm_config() and never writes it.
#'
#' @return Character(1) digest, or NULL when no config is loaded.
harm_default_config_hash <- function() {
  if (!exists("HARMONIZATION_CONFIG", envir = .GlobalEnv)) return(NULL)
  harm_config_hash(get("HARMONIZATION_CONFIG", envir = .GlobalEnv))
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

  # FS3: Omnivore (explicit modality). MUST come before predator/grazer
  # branches so "omnivore" doesn't accidentally match a partial keyword.
  if (grepl("omnivor|mixed.diet|generalist", modality)) {
    fs_class <- "FS3"

  # FS0: Primary producer (photosynthesis, autotroph)
  } else if (grepl("photosyn|autotroph|producer|primary.producer", modality)) {
    fs_class <- "FS0"

  # FS1: Predator (active predation)
  } else if (grepl("predator|carnivore|piscivore|hunter", modality)) {
    fs_class <- "FS1"

  # FS2: Scavenger/Detritivore
  } else if (grepl("scavenger|detritivore|carrion", modality)) {
    fs_class <- "FS2"

  # FS7: Xylophagous (wood boring) - rare
  } else if (grepl("xylophag|wood.bor|wood.eat|lignivor", modality)) {
    fs_class <- "FS7"

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

  # Multi-mode aggregation: if a species has high-scored evidence for both
  # carnivory and herbivory/filter-feeding (without an explicit "omnivore"
  # modality), promote to FS3. Catches the common case where ontologies
  # express omnivory as multiple feeding modes rather than one label.
  if (!is.na(fs_class) && fs_class != "FS3") {
    high_scoring <- feeding[feeding$trait_score >= 2, , drop = FALSE]
    if (nrow(high_scoring) >= 2) {
      mods <- tolower(high_scoring$trait_modality)
      has_carn <- any(grepl("predator|carnivore|piscivore|hunter|scavenger", mods))
      has_herb <- any(grepl("graz|herbivore|scraper|browser|filter|suspension|planktivore|deposit", mods))
      if (has_carn && has_herb) {
        fs_class <- "FS3"
      }
    }
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

  # Map ontology modality to MB class through the shared vocabulary
  # (TRAIT_VOCAB), so the fuzzy path cannot drift from the live cascade.
  mb_class <- classify_by_patterns(primary$trait_modality, "mobility")

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

  # Map ontology modality to EP class through the shared vocabulary. Zonation
  # modalities (intertidal, subtidal) give NA on purpose: they are depth
  # zones, not a position relative to the substrate (F35, F77).
  ep_class <- classify_by_patterns(primary$trait_modality, "environmental")

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
  cfg <- get_harm_config()
  if (is.null(cfg)) {
    stop("HARMONIZATION_CONFIG not loaded - source R/config/harmonization_config.R first",
         call. = FALSE)
  }
  rule <- cfg$taxonomic_rules[[rule_name]]
  return(isTRUE(rule))
}


#' Apply Ecosystem Profile Size Adjustment
#'
#' @param size_cm Raw size in cm
#' @return Adjusted size in cm based on active ecosystem profile
apply_size_adjustment <- function(size_cm) {
  cfg <- get_harm_config()
  if (is.null(cfg)) return(size_cm)  # No adjustment if config missing

  active_profile_name <- cfg$active_profile %||% "temperate"
  if (active_profile_name == "temperate") {
    return(size_cm)  # No adjustment for temperate (baseline)
  }

  profile <- cfg$profiles[[active_profile_name]]
  multiplier <- profile$size_multiplier %||% 1.0
  return(size_cm * multiplier)
}


#' Effective text patterns for one trait (vocabulary defaults + session tuning)
#'
#' For mobility / environmental / protection the defaults are
#' TRAIT_VOCAB$patterns; the session config (get_harm_config()) may override
#' them key by key. Only keys the vocabulary defines are honoured, so a stale
#' key from a pre-v2 JSON (e.g. MB2_burrower) can never resurrect the old
#' meaning, and a value identical to the pre-v2 default of a kept key is
#' treated as "not customised". Foraging patterns are entirely config-owned.
#'
#' @param trait "mobility", "environmental", "protection" or "foraging".
#' @return Named list key -> regex (keys like "MB1_sessile"), or NULL.
trait_patterns <- function(trait) {
  section <- paste0(trait, "_patterns")
  cfg <- get_harm_config() %||% list()
  vocab <- get_trait_vocab()
  defaults <- vocab$patterns[[trait]]
  if (is.null(defaults)) return(cfg[[section]])
  user <- cfg[[section]]
  if (!is.list(user) || length(user) == 0L) return(defaults)
  legacy <- vocab$legacy_patterns[[trait]] %||% list()
  keep <- Filter(function(k) {
    v <- user[[k]]
    is.character(v) && length(v) == 1L && !is.na(v) && nzchar(v) && !identical(v, legacy[[k]])
  }, intersect(names(user), names(defaults)))
  utils::modifyList(defaults, user[keep])
}


#' Get Pattern from Configuration
#'
#' Thin wrapper kept for callers of the pre-v2 API.
#'
#' @param pattern_name String name (e.g., "MB1_sessile", "FS1_predator")
#' @param pattern_type Type: "mobility", "foraging", "environmental", "protection"
#' @return Regular expression pattern string, or NULL if not found
get_config_pattern <- function(pattern_name, pattern_type = "mobility") {
  if (!pattern_type %in% c("mobility", "foraging", "environmental", "protection")) return(NULL)
  trait_patterns(pattern_type)[[pattern_name]]
}


#' Classify free text into a trait code using the vocabulary patterns
#'
#' Lower-cases the text and tests the codes in
#' get_trait_vocab()$pattern_precedence[[trait]] order; the first match wins.
#' Each pattern is wrapped as (?<![a-z])(?:<pattern>) (perl): a LEADING
#' boundary only, so stems keep working ("burrow" matches "burrowing") but
#' "tidal" no longer matches inside "subtidal" (F77), "pelagic" inside
#' "benthopelagic" (F35) or "surface" inside "subsurface".
#'
#' @param text Character vector (collapsed with spaces); NULL / NA / "" allowed.
#' @param trait "mobility", "environmental", "protection" or "foraging".
#' @return The code (e.g. "EP2"), or NA_character_ when nothing matches. An
#'   invalid pattern warns ("[harmonization] invalid <trait> pattern for
#'   <code>: ...") and is skipped.
classify_by_patterns <- function(text, trait) {
  if (length(text) == 0L) return(NA_character_)
  text <- as.character(unlist(text))
  text <- text[!is.na(text)]
  if (length(text) == 0L) return(NA_character_)
  txt <- tolower(paste(text, collapse = " "))
  if (!nzchar(trimws(txt))) return(NA_character_)

  pats <- trait_patterns(trait)
  if (length(pats) == 0L) return(NA_character_)
  codes <- sub("_.*$", "", names(pats))
  precedence <- get_trait_vocab()$pattern_precedence[[trait]] %||% unique(codes)
  for (code in precedence) {
    for (key in names(pats)[codes == code]) {
      hit <- tryCatch(
        suppressWarnings(grepl(paste0("(?<![a-z])(?:", pats[[key]], ")"), txt, perl = TRUE)),
        error = function(e) {
          warning(sprintf("[harmonization] invalid %s pattern for %s: %s",
                          trait, code, conditionMessage(e)), call. = FALSE)
          FALSE
        }
      )
      if (isTRUE(hit)) return(code)
    }
  }
  NA_character_
}


#' First taxonomic rule that fires for a taxon
#'
#' Rules live in get_trait_vocab()$taxon_rules[[trait]] (ordered). A rule fires
#' when every `match` field of the taxonomy matches its regex
#' (case-insensitive), its optional `text` regex matches the trait text, and
#' at least one of its `flag`s is enabled (is_rule_enabled()). Taxonomy fields
#' that are NULL, NA or zero-length count as absent, so a WoRMS record with
#' class = character(0) cannot raise "missing value where TRUE/FALSE needed".
#'
#' @param taxonomy List or one-row data frame (phylum, class, order, ...), or NULL.
#' @param trait "mobility", "environmental_pelagic", "environmental" or "protection".
#' @param text Optional trait text (for rules with a `text` condition).
#' @param override_only Only consider rules with override_text = TRUE.
#' @return The rule's code, or NA_character_.
apply_taxon_rules <- function(taxonomy, trait, text = NULL, override_only = FALSE) {
  if (is.null(taxonomy) || !is.list(taxonomy)) return(NA_character_)
  field <- function(name) {
    v <- taxonomy[[name]]
    if (length(v) == 0L) return(NA_character_)
    v <- as.character(unlist(v))[1]
    if (is.na(v) || !nzchar(v)) NA_character_ else v
  }
  text_lower <- tolower(paste(as.character(unlist(text))[!is.na(unlist(text))], collapse = " "))
  for (rule in get_trait_vocab()$taxon_rules[[trait]]) {
    if (override_only && !isTRUE(rule$override_text)) next
    if (!is.null(rule$flag) && !any(vapply(rule$flag, is_rule_enabled, logical(1)))) next
    # Same leading word boundary as classify_by_patterns(): "benthopelagic"
    # must not satisfy a "pelagic" text condition.
    if (!is.null(rule$text) &&
        !isTRUE(grepl(paste0("(?<![a-z])(?:", rule$text, ")"), text_lower, perl = TRUE))) next
    matched <- all(vapply(names(rule$match), function(rank) {
      v <- field(rank)
      isTRUE(!is.na(v) && grepl(rule$match[[rank]], v, ignore.case = TRUE))
    }, logical(1)))
    if (matched) return(rule$code)
  }
  NA_character_
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

  # Get thresholds from session-aware config (per-session if user has
  # adjusted sliders this session; otherwise the global default).
  cfg <- get_harm_config()
  if (is.null(cfg) || is.null(cfg$size_thresholds)) return(NA)
  thresh <- cfg$size_thresholds

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
  patterns <- (get_harm_config() %||% list())$foraging_patterns
  if (is.null(patterns)) return(NA_character_)

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

  # 1. Explicit text, through the shared vocabulary patterns.
  code <- classify_by_patterns(mobility_info, "mobility")
  if (!is.na(code)) return(code)

  # 2. Taxonomic rules (TRAIT_VOCAB$taxon_rules$mobility, switchable in the
  #    harmonization settings).
  code <- apply_taxon_rules(taxonomic_info, "mobility", text = mobility_info)
  if (!is.na(code)) return(code)

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

  # 1. Explicit habitat text, through the shared vocabulary patterns.
  code <- classify_by_patterns(habitat_info, "environmental")
  if (!is.na(code)) return(code)

  # 2. Pelagic taxa (phyto- and zooplankton, medusae) and infaunal bivalves.
  #    Before the depth rule: a copepod caught at 10-20 m is pelagic, not
  #    epibenthic (F36), and a shallow Mya is endobenthic (C-6a).
  code <- apply_taxon_rules(taxonomic_info, "environmental_pelagic", text = habitat_info)
  if (!is.na(code)) return(code)

  # 3. Depth range
  avg_depth <- suppressWarnings(mean(as.numeric(c(depth_min[1], depth_max[1]))))
  if (length(depth_min) > 0 && length(depth_max) > 0 && isTRUE(is.finite(avg_depth))) {
    # Very shallow species are likely epibenthic (burrowers were caught by
    # the habitat text in step 1)
    if (avg_depth < 50) return("EP3")
    # Deep species often benthopelagic
    if (avg_depth > 200) return("EP2")
  }

  # 4. Other taxonomic rules (fish by order)
  code <- apply_taxon_rules(taxonomic_info, "environmental", text = habitat_info)
  if (!is.na(code)) return(code)

  # Default: epibenthic (conservative)
  return("EP3")
}


#' Convert protection information to PR code
#'
#' @param skeleton_info Skeleton/protection information
#' @param taxonomic_info Taxonomic classification
#' @return PR code (PR0-PR8). Pre-PR1b PR1 and PR4 had no branches and
#'   any matching input silently produced NA; both gaps now closed and
#'   the labels come from the trait vocabulary (TRAIT_VOCAB, via
#'   trait_code_label()).
#' @export
harmonize_protection <- function(skeleton_info = NULL, taxonomic_info = NULL) {

  # 1. Taxon rules that outrank any text (echinoderm ossicles, F38).
  code <- apply_taxon_rules(taxonomic_info, "protection", text = skeleton_info, override_only = TRUE)
  if (!is.na(code)) return(code)

  # 2. Explicit text, through the shared vocabulary patterns.
  code <- classify_by_patterns(skeleton_info, "protection")
  if (!is.na(code)) return(code)

  # 3. Taxonomic rules (TRAIT_VOCAB$taxon_rules$protection).
  code <- apply_taxon_rules(taxonomic_info, "protection", text = skeleton_info)
  if (!is.na(code)) return(code)

  # Default: no protection
  return("PR0")
}

#' Harmonize Reproductive Strategy
#' @param reproduction_text Character, raw reproduction description
#' @return Character, RS code (RS1-RS4) or NA
harmonize_reproductive_strategy <- function(reproduction_text) {
  if (is.null(reproduction_text) || is.na(reproduction_text) || reproduction_text == "") {
    return(NA_character_)
  }
  text <- tolower(reproduction_text)
  patterns <- (get_harm_config() %||% list())$reproductive_patterns
  if (is.null(patterns)) return(NA_character_)
  for (i in seq_along(patterns)) {
    if (grepl(patterns[[i]], text)) {
      return(paste0("RS", i))
    }
  }
  return(NA_character_)
}

#' Harmonize Temperature Tolerance
#' @param temperature_text Character, raw temperature/biogeographic description
#' @return Character, TT code (TT1-TT4) or NA
harmonize_temperature_tolerance <- function(temperature_text) {
  if (is.null(temperature_text) || is.na(temperature_text) || temperature_text == "") {
    return(NA_character_)
  }
  text <- tolower(temperature_text)
  patterns <- (get_harm_config() %||% list())$temperature_patterns
  if (is.null(patterns)) return(NA_character_)
  for (i in seq_along(patterns)) {
    if (grepl(patterns[[i]], text)) {
      return(paste0("TT", i))
    }
  }
  return(NA_character_)
}

#' Band a numeric coral thermal-tolerance maximum into a TT code
#'
#' CoralTraits supplies thermal tolerance as a numeric maximum (degrees C),
#' not a biogeographic text description, so harmonize_temperature_tolerance()
#' (regex over text) cannot consume it. This is the single source of truth for
#' the `> 30 -> TT4 / > 25 -> TT3 / else TT2` banding, shared by the live
#' orchestrator path and the offline-DB build writer. NA-safe: NULL / NA /
#' non-numeric input returns NA (the former inline orchestrator banding
#' crashed on NA via `if (NA > 30)`).
#'
#' @param thermal_max Numeric (or numeric-coercible) max thermal tolerance, degrees C.
#' @return Character, TT code (TT2-TT4) or NA_character_.
#' @export
coral_thermal_to_tt <- function(thermal_max) {
  v <- suppressWarnings(as.numeric(thermal_max))
  if (length(v) == 0 || is.na(v)) {
    return(NA_character_)
  }
  if (v > 30) return("TT4")
  if (v > 25) return("TT3")
  "TT2"
}

#' Harmonize Salinity Tolerance
#' @param salinity_text Character, raw salinity/habitat description
#' @return Character, ST code (ST1-ST5) or NA
harmonize_salinity_tolerance <- function(salinity_text) {
  if (is.null(salinity_text) || is.na(salinity_text) || salinity_text == "") {
    return(NA_character_)
  }
  text <- tolower(salinity_text)
  patterns <- (get_harm_config() %||% list())$salinity_patterns
  if (is.null(patterns)) return(NA_character_)
  for (i in seq_along(patterns)) {
    if (grepl(patterns[[i]], text)) {
      return(paste0("ST", i))
    }
  }
  return(NA_character_)
}
