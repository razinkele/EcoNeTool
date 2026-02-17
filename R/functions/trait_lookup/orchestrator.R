# =============================================================================
# TRAIT LOOKUP - Main Orchestrator
# =============================================================================
# Main workflow: lookup_species_traits() queries databases and harmonizes results.
# Part of the trait_lookup module (split from trait_lookup.R)
# =============================================================================

# Automated Trait Lookup System
# Implements best-practice workflow for trait-based food web construction
# Database hierarchy: WoRMS -> FishBase -> BIOTIC -> MAREDAT -> PTDB -> TraitBank

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

  message("\n\u2554\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2557")
  message("\u2551 TRAIT LOOKUP: ", species_name, paste(rep(" ", max(0, 47 - nchar(species_name))), collapse = ""), "\u2551")
  message("\u2560\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2563")
  message("\u2551 Target Traits:                                                 \u2551")
  message("\u2551   MS (Max Size) - Size class (MS1-MS7)                        \u2551")
  message("\u2551   FS (Foraging Strategy) - Feeding mode (FS0-FS6)             \u2551")
  message("\u2551   MB (Mobility) - Movement capability (MB1-MB5)               \u2551")
  message("\u2551   EP (Environmental Position) - Depth/habitat (EP1-EP4)       \u2551")
  message("\u2551   PR (Predator Resistance) - Protection (PR1-PR3)             \u2551")
  message("\u255a\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u255d")

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
  message("\n[1/10] \U0001f30a WoRMS - Taxonomic classification...")
  db_start <- Sys.time()
  worms_data <- lookup_worms_traits(species_name)
  db_time <- round(as.numeric(difftime(Sys.time(), db_start, units = "secs")), 2)

  if (worms_data$success) {
    raw_traits$worms <- worms_data$traits
    sources_used <- c(sources_used, "WoRMS")
    message("  \u2713 SUCCESS (", db_time, "s)")
    message("    \u2192 Phylum: ", raw_traits$worms$phylum)
    message("    \u2192 Class: ", raw_traits$worms$class)
    message("    \u2192 Order: ", raw_traits$worms$order)

    # Check if body size was extracted from WoRMS Traits Portal
    if (!is.null(raw_traits$worms$max_length_cm) && !is.na(raw_traits$worms$max_length_cm)) {
      message("    \u2192 Max Length: ", raw_traits$worms$max_length_cm, " cm (from WoRMS Traits Portal) [\u2192 MS]")
    } else {
      message("    \u2192 Provides: Taxonomic context for inference rules")
    }
  } else {
    message("  \u2717 FAILED (", db_time, "s) - Species not found in WoRMS")
  }

  # 2. Ontology Traits (fuzzy-scored semantic traits)
  message("\n[2/10] \U0001f3f7\ufe0f  Ontology - Fuzzy trait profiles...")
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
    message("  \u2713 SUCCESS (", db_time, "s)")
    message("    \u2192 Trait records: ", ontology_data$n_traits)

    # Summarize trait categories
    if (length(ontology_data$trait_summary) > 0) {
      for (cat_name in names(ontology_data$trait_summary)) {
        message("    \u2192 ", cat_name, ": ", ontology_data$trait_summary[[cat_name]], " modalities")
      }
    }

    # Extract primary feeding mode if available
    primary_feeding <- extract_primary_feeding(ontology_data$traits)
    if (!is.na(primary_feeding$modality)) {
      message("    \u2192 Primary feeding: ", primary_feeding$modality,
              " (score=", primary_feeding$score, ") [\u2192 FS]")
    }
  } else {
    message("  \u2717 SKIPPED (", db_time, "s) - Species not in ontology database")
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

    message("\n\U0001f3af SMART ROUTING based on taxonomy...")

    # Fish -> FishBase only
    if (phylum == "chordata" && class %in% c("actinopterygii", "actinopteri", "elasmobranchii",
                                              "holocephali", "myxini", "petromyzonti",
                                              "teleostei", "chondrichthyes", "osteichthyes")) {
      query_fishbase <- TRUE
      message("  \u2192 Detected FISH (", class, ") \u2192 Querying: FishBase")
      message("  \u2192 Skipping: SeaLifeBase, BIOTIC, MAREDAT, PTDB, AlgaeBase")

    # Marine invertebrates -> SeaLifeBase + SpeciesEnriched + BIOTIC
    } else if (phylum %in% c("mollusca", "arthropoda", "annelida", "echinodermata",
                             "cnidaria", "porifera", "platyhelminthes", "nematoda",
                             "bryozoa", "brachiopoda", "nemertea", "sipuncula")) {
      query_sealifebase <- TRUE
      query_species_enriched <- TRUE
      query_biotic <- TRUE
      query_shark <- TRUE  # May have occurrence data
      message("  \u2192 Detected MARINE INVERTEBRATE (", phylum, ") \u2192 Querying: SeaLifeBase, SpeciesEnriched, BIOTIC, SHARK")
      message("  \u2192 Skipping: FishBase, freshwater, MAREDAT, PTDB, AlgaeBase")

    # Zooplankton -> MAREDAT + SeaLifeBase
    } else if (class %in% c("copepoda", "cladocera", "ostracoda", "mysida", "euphausiacea",
                            "chaetognatha", "appendicularia", "thaliacea")) {
      query_maredat <- TRUE
      query_sealifebase <- TRUE
      message("  \u2192 Detected ZOOPLANKTON (", class, ") \u2192 Querying: MAREDAT, SeaLifeBase")
      message("  \u2192 Skipping: FishBase, BIOTIC, PTDB, AlgaeBase")

    # Phytoplankton / Algae -> BVOL + PTDB + AlgaeBase + freshwater (if not marine)
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
        message("  \u2192 Detected FRESHWATER PHYTOPLANKTON/ALGAE (", phylum, ") \u2192 Querying: BVOL, PTDB, AlgaeBase, freshwater")
      } else {
        message("  \u2192 Detected PHYTOPLANKTON/ALGAE (", phylum, ") \u2192 Querying: BVOL, PTDB, AlgaeBase")
      }
      message("  \u2192 Skipping: FishBase, SeaLifeBase, BIOTIC, MAREDAT")

    # Freshwater taxa -> freshwaterecology.info
    } else if (class %in% c("oligochaeta", "hirudinea", "gastropoda") &&
               !is.null(raw_traits$worms$isMarine) && raw_traits$worms$isMarine == FALSE) {
      query_freshwater <- TRUE
      query_biotic <- TRUE
      message("  \u2192 Detected FRESHWATER (", class, ") \u2192 Querying: freshwaterecology.info, BIOTIC")
      message("  \u2192 Skipping: FishBase, SeaLifeBase, MAREDAT, PTDB, AlgaeBase")

    # Unknown/ambiguous -> Query invertebrate databases
    } else {
      query_sealifebase <- TRUE
      query_biotic <- TRUE
      query_freshwater <- TRUE
      message("  \u26a0\ufe0f  Ambiguous taxonomy (", phylum, "/", class, ") \u2192 Querying: SeaLifeBase, BIOTIC, freshwater")
      message("  \u2192 Skipping: FishBase (not a fish), MAREDAT, PTDB, AlgaeBase")
    }

  } else {
    # WoRMS failed -> Try all databases (fallback mode)
    message("\n\u26a0\ufe0f  WoRMS FAILED - FALLBACK MODE: Trying all databases")
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
    message("\n[3/10] \U0001f41f FishBase - Fish morphology & ecology...")
    db_start <- Sys.time()
    fishbase_data <- lookup_fishbase_traits(species_name)
    db_time <- round(as.numeric(difftime(Sys.time(), db_start, units = "secs")), 2)

    if (fishbase_data$success) {
      raw_traits$fishbase <- fishbase_data$traits
      sources_used <- c(sources_used, "FishBase")
      message("  \u2713 SUCCESS (", db_time, "s)")

      if (!is.null(fishbase_data$traits$max_length_cm)) {
        size_cm <- fishbase_data$traits$max_length_cm
        message("    \u2192 Max Length: ", size_cm, " cm [\u2192 MS]")
      }
      if (!is.null(fishbase_data$traits$trophic_level)) {
        trophic_level <- fishbase_data$traits$trophic_level
        message("    \u2192 Trophic Level: ", trophic_level, " [\u2192 FS]")
      }
      if (!is.null(fishbase_data$traits$feeding_type)) {
        feeding_mode <- c(feeding_mode, fishbase_data$traits$feeding_type)
        message("    \u2192 Feeding Type: ", fishbase_data$traits$feeding_type, " [\u2192 FS]")
      }
      if (!is.null(fishbase_data$traits$body_shape)) {
        body_shape <- fishbase_data$traits$body_shape
        message("    \u2192 Body Shape: ", body_shape, " [\u2192 MB]")
      }
      if (!is.null(fishbase_data$traits$depth_min)) {
        depth_min <- fishbase_data$traits$depth_min
        depth_max <- fishbase_data$traits$depth_max
        message("    \u2192 Depth Range: ", depth_min, "-", depth_max, " m [\u2192 EP]")
      }
      message("    \u2192 Provides: HIGH-PRIORITY data for MS, FS, MB, EP")
    } else {
      message("  \u2717 FAILED (", db_time, "s) - Not found in FishBase")
    }
  } else {
    message("\n[3/10] \U0001f41f FishBase - SKIPPED (not a fish based on taxonomy)")
  }

  # Check if we already have everything we need
  completeness <- check_completeness()
  if (completeness$complete) {
    message("\n\u26a1 EARLY EXIT: All required raw data collected!")
    message("   Skipping remaining databases to save time...")
    # Skip to harmonization
  } else {

  # 3. SeaLifeBase (for marine invertebrates)
  if (query_sealifebase) {
    message("\n[4/10] \U0001f41a SeaLifeBase - Marine invertebrate traits...")
    db_start <- Sys.time()
    sealifebase_data <- lookup_sealifebase_traits(species_name)
    db_time <- round(as.numeric(difftime(Sys.time(), db_start, units = "secs")), 2)

    if (sealifebase_data$success) {
      raw_traits$sealifebase <- sealifebase_data$traits
      sources_used <- c(sources_used, "SeaLifeBase")
      message("  \u2713 SUCCESS (", db_time, "s)")

      if (is.null(size_cm) && !is.null(sealifebase_data$traits$max_length_cm)) {
        size_cm <- sealifebase_data$traits$max_length_cm
        message("    \u2192 Max Length: ", size_cm, " cm [\u2192 MS]")
      }
      if (is.null(trophic_level) && !is.null(sealifebase_data$traits$trophic_level)) {
        trophic_level <- sealifebase_data$traits$trophic_level
        message("    \u2192 Trophic Level: ", trophic_level, " [\u2192 FS]")
      }
      message("    \u2192 Provides: MEDIUM-PRIORITY data for MS, FS, MB")
    } else {
      message("  \u2717 FAILED (", db_time, "s) - Not found in SeaLifeBase")
    }
  } else {
    message("\n[4/10] \U0001f41a SeaLifeBase - SKIPPED (not a marine invertebrate based on taxonomy)")
  }

  # 4. BIOTIC (for invertebrates)
  if (query_biotic) {
    message("\n[5/10] \U0001f980 BIOTIC - Benthic invertebrate biological traits...")
    db_start <- Sys.time()
    biotic_data <- lookup_biotic_traits(species_name, biotic_file)
    db_time <- round(as.numeric(difftime(Sys.time(), db_start, units = "secs")), 2)

    if (biotic_data$success) {
      raw_traits$biotic <- biotic_data$traits
      sources_used <- c(sources_used, "BIOTIC")
      message("  \u2713 SUCCESS (", db_time, "s)")

      if (!is.null(biotic_data$traits$mobility)) {
        mobility_info <- c(mobility_info, biotic_data$traits$mobility)
        message("    \u2192 Mobility: ", biotic_data$traits$mobility, " [\u2192 MB]")
      }
      if (!is.null(biotic_data$traits$feeding_mode)) {
        feeding_mode <- c(feeding_mode, biotic_data$traits$feeding_mode)
        message("    \u2192 Feeding Mode: ", biotic_data$traits$feeding_mode, " [\u2192 FS]")
      }
      if (!is.null(biotic_data$traits$living_habit)) {
        habitat_info <- c(habitat_info, biotic_data$traits$living_habit)
        message("    \u2192 Living Habit: ", biotic_data$traits$living_habit, " [\u2192 EP]")
      }
      message("    \u2192 Provides: Categorical traits for MB, FS, EP, PR")
    } else {
      message("  \u2717 FAILED (", db_time, "s) - Not in local BIOTIC database")
    }
  } else {
    message("\n[5/10] \U0001f980 BIOTIC - SKIPPED (not applicable based on taxonomy)")
  }

  # 5. BVOL (for phytoplankton)
  if (query_bvol) {
    message("\n[6/10] \U0001f33f BVOL - Phytoplankton biovolume traits...")
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
      message("  \u2713 SUCCESS (", db_time, "s)")

      if (!is.null(bvol_traits$size_cm)) {
        if (is.null(size_cm)) {
          size_cm <- bvol_traits$size_cm
        }
        message("    \u2192 Size: ", sprintf("%.6f", bvol_traits$size_cm), " cm (from biovolume) [\u2192 MS]")
      }
      if (!is.null(bvol_traits$trophy)) {
        message("    \u2192 Trophy: ", bvol_traits$trophy, " [\u2192 FS]")
      }
      message("    \u2192 Harmonized: MS=", harmonized_bvol$MS, ", FS=", harmonized_bvol$FS,
              ", MB=", harmonized_bvol$MB, ", EP=", harmonized_bvol$EP, ", PR=", harmonized_bvol$PR)
      message("    \u2192 Confidence: ", sprintf("%.2f", harmonized_bvol$overall_confidence))
    } else {
      message("  \u2717 FAILED (", db_time, "s) - Not in local BVOL database")
    }
  } else {
    message("\n[6/10] \U0001f33f BVOL - SKIPPED (not phytoplankton based on taxonomy)")
  }

  # 6. SpeciesEnriched (for marine invertebrates)
  if (query_species_enriched) {
    message("\n[7/10] \U0001f991 SpeciesEnriched - Marine invertebrate traits...")
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
      message("  \u2713 SUCCESS (", db_time, "s)")

      if (!is.null(enriched_traits$size_cm)) {
        if (is.null(size_cm)) {
          size_cm <- enriched_traits$size_cm
        }
        message("    \u2192 Size: ", sprintf("%.2f", enriched_traits$size_cm), " cm [\u2192 MS]")
      }
      if (!is.null(enriched_traits$mobility)) {
        mobility_info <- c(mobility_info, enriched_traits$mobility)
        message("    \u2192 Mobility: ", enriched_traits$mobility, " [\u2192 MB]")
      }
      if (!is.null(enriched_traits$feeding_method)) {
        feeding_mode <- c(feeding_mode, enriched_traits$feeding_method)
        message("    \u2192 Feeding: ", enriched_traits$feeding_method, " [\u2192 FS]")
      }
      message("    \u2192 Harmonized: MS=", harmonized_enriched$MS, ", FS=", harmonized_enriched$FS,
              ", MB=", harmonized_enriched$MB, ", EP=", harmonized_enriched$EP, ", PR=", harmonized_enriched$PR)
      message("    \u2192 Confidence: ", sprintf("%.2f", harmonized_enriched$overall_confidence))
    } else {
      message("  \u2717 FAILED (", db_time, "s) - Not in local SpeciesEnriched database")
    }
  } else {
    message("\n[7/10] \U0001f991 SpeciesEnriched - SKIPPED (not marine invertebrate based on taxonomy)")
  }

  # 7. freshwaterecology.info (for freshwater species)
  if (query_freshwater) {
    message("\n[8/12] \U0001f30a freshwaterecology.info - Freshwater species...")
    db_start <- Sys.time()
    freshwater_data <- lookup_freshwaterecology_traits(species_name)
    db_time <- round(as.numeric(difftime(Sys.time(), db_start, units = "secs")), 2)

    if (freshwater_data$success) {
      raw_traits$freshwater <- freshwater_data$traits
      sources_used <- c(sources_used, "freshwaterecology.info")
      message("  \u2713 SUCCESS (", db_time, "s)")

      if (is.null(size_cm) && !is.null(freshwater_data$traits$max_length_mm)) {
        size_cm <- freshwater_data$traits$max_length_mm / 10
        message("    \u2192 Max Length: ", freshwater_data$traits$max_length_mm, " mm (", size_cm, " cm) [\u2192 MS]")
      }
      if (!is.null(freshwater_data$traits$locomotion)) {
        mobility_info <- c(mobility_info, freshwater_data$traits$locomotion)
        message("    \u2192 Locomotion: ", freshwater_data$traits$locomotion, " [\u2192 MB]")
      }
      message("    \u2192 Provides: Data for MS, FS, MB, EP")
    } else {
      message("  \u2717 FAILED (", db_time, "s) - Not a freshwater species or API issue")
    }
  } else {
    message("\n[8/12] \U0001f30a freshwaterecology.info - SKIPPED (not freshwater based on taxonomy)")
  }

  # 8. MAREDAT (for zooplankton)
  if (query_maredat) {
    message("\n[9/12] \U0001f990 MAREDAT - Zooplankton traits...")
    db_start <- Sys.time()
    maredat_data <- lookup_maredat_traits(species_name, maredat_file)
    db_time <- round(as.numeric(difftime(Sys.time(), db_start, units = "secs")), 2)

    if (maredat_data$success) {
      raw_traits$maredat <- maredat_data$traits
      sources_used <- c(sources_used, "MAREDAT")
      message("  \u2713 SUCCESS (", db_time, "s)")
      message("    \u2192 Provides: Zooplankton size and feeding data [\u2192 MS, FS]")
    } else {
      message("  \u2717 FAILED (", db_time, "s) - Not in local MAREDAT database")
    }
  } else {
    message("\n[9/12] \U0001f990 MAREDAT - SKIPPED (not zooplankton based on taxonomy)")
  }

  # 9. PTDB (for phytoplankton)
  if (query_ptdb) {
    message("\n[10/12] \U0001f33f PTDB - Phytoplankton traits...")
    db_start <- Sys.time()
    ptdb_data <- lookup_ptdb_traits(species_name, ptdb_file)
    db_time <- round(as.numeric(difftime(Sys.time(), db_start, units = "secs")), 2)

    if (ptdb_data$success) {
      raw_traits$ptdb <- ptdb_data$traits
      sources_used <- c(sources_used, "PTDB")
      message("  \u2713 SUCCESS (", db_time, "s)")
      message("    \u2192 Provides: Phytoplankton cell size [\u2192 MS, FS=FS0]")
    } else {
      message("  \u2717 FAILED (", db_time, "s) - Not in local PTDB database")
    }
  } else {
    message("\n[10/12] \U0001f33f PTDB - SKIPPED (not phytoplankton based on taxonomy)")
  }

  # 10. AlgaeBase (for algae/phytoplankton)
  if (query_algaebase) {
    message("\n[11/12] \U0001f331 AlgaeBase - Algae taxonomy...")
    db_start <- Sys.time()
    algaebase_data <- lookup_algaebase_traits(species_name)
    db_time <- round(as.numeric(difftime(Sys.time(), db_start, units = "secs")), 2)

    if (algaebase_data$success) {
      raw_traits$algaebase <- algaebase_data$traits
      sources_used <- c(sources_used, "AlgaeBase")
      message("  \u2713 SUCCESS (", db_time, "s)")
      message("    \u2192 Provides: Algae identification [\u2192 FS=FS0, EP]")
    } else {
      message("  \u2717 FAILED (", db_time, "s) - Not an algae or not in AlgaeBase")
    }
  } else {
    message("\n[11/12] \U0001f331 AlgaeBase - SKIPPED (not algae based on taxonomy)")
  }

  # 11. SHARK (for Swedish waters species)
  if (query_shark) {
    message("\n[12/12] \U0001f988 SHARK - Swedish Ocean Archives...")
    db_start <- Sys.time()
    shark_data <- lookup_shark_traits(species_name)
    db_time <- round(as.numeric(difftime(Sys.time(), db_start, units = "secs")), 2)

    if (shark_data$success) {
      raw_traits$shark <- shark_data$traits
      sources_used <- c(sources_used, "SHARK")
      message("  \u2713 SUCCESS (", db_time, "s)")

      if (is.null(depth_min) && !is.null(shark_data$traits$depth_range_m)) {
        depth_min <- shark_data$traits$depth_range_m[1]
        depth_max <- shark_data$traits$depth_range_m[2]
        message("    \u2192 Depth Range: ", depth_min, "-", depth_max, " m [\u2192 EP]")
      }
      message("    \u2192 Provides: Geographic/depth data [\u2192 EP]")
    } else {
      message("  \u2717 FAILED (", db_time, "s) - Not in Swedish waters")
    }
  } else {
    message("\n[12/12] \U0001f988 SHARK - SKIPPED (no Swedish waters data needed)")
  }

  } # End of else block for early exit

  # If no data found
  if (length(raw_traits) == 0) {
    message("\n\u274c NO DATA FOUND - No databases contained information for this species")
    result$confidence <- "none"
    total_time <- round(as.numeric(difftime(Sys.time(), total_start, units = "secs")), 2)
    message("\n\u23f1\ufe0f  Total lookup time: ", total_time, "s")
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
  message("\n\u2554\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2557")
  message("\u2551 HARMONIZATION - Converting raw data to categorical traits     \u2551")
  message("\u255a\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u255d")

  # 1. MS - Max Size Class
  message("\n[MS] Max Size Class:")
  if (!is.null(size_cm)) {
    message("  \U0001f4cf Input: ", size_cm, " cm")
    result$MS <- harmonize_size_class(size_cm)
    message("  \u2713 Output: ", result$MS)
    if (size_cm < 0.1) message("     (< 0.1 cm = Tiny - MS1)")
    else if (size_cm < 1.0) message("     (0.1-1 cm = Very Small - MS2)")
    else if (size_cm < 5.0) message("     (1-5 cm = Small - MS3)")
    else if (size_cm < 20.0) message("     (5-20 cm = Medium - MS4)")
    else if (size_cm < 50.0) message("     (20-50 cm = Large - MS5)")
    else if (size_cm < 150.0) message("     (50-150 cm = Very Large - MS6)")
    else message("     (> 150 cm = Giant - MS7)")
  } else {
    message("  \u274c No size data available")
    result$MS <- NA_character_
  }

  # 2. FS - Foraging Strategy
  message("\n[FS] Foraging Strategy:")
  if (length(feeding_mode) > 0 || !is.null(trophic_level)) {
    if (!is.null(trophic_level)) message("  \U0001f374 Trophic Level: ", trophic_level)
    if (length(feeding_mode) > 0) message("  \U0001f374 Feeding Modes: ", paste(feeding_mode, collapse = ", "))
    result$FS <- harmonize_foraging_strategy(feeding_mode, trophic_level)
    message("  \u2713 Output: ", result$FS)
    fs_labels <- c("FS0"="Primary Producer", "FS1"="Predator", "FS2"="Scavenger",
                   "FS3"="Omnivore", "FS4"="Grazer", "FS5"="Deposit Feeder", "FS6"="Filter Feeder")
    if (!is.na(result$FS) && result$FS %in% names(fs_labels)) {
      message("     (", fs_labels[result$FS], ")")
    }
  } else {
    # Try fuzzy harmonization from ontology traits
    if (!is.null(raw_traits$ontology)) {
      message("  \U0001f50d Trying fuzzy harmonization from ontology...")
      fuzzy_fs <- harmonize_fuzzy_foraging(raw_traits$ontology)
      if (!is.na(fuzzy_fs$class)) {
        result$FS <- fuzzy_fs$class
        sources_used <- c(sources_used, "Fuzzy")
        message("  \u2713 Output: ", result$FS, " (from fuzzy ontology, confidence=", fuzzy_fs$confidence, ")")
        fs_labels <- c("FS0"="Primary Producer", "FS1"="Predator", "FS2"="Scavenger",
                       "FS3"="Omnivore", "FS4"="Grazer", "FS5"="Deposit Feeder", "FS6"="Filter Feeder")
        if (result$FS %in% names(fs_labels)) {
          message("     (", fs_labels[result$FS], ")")
        }
        message("     Modalities: ", paste(fuzzy_fs$modalities, collapse = ", "))
      } else {
        message("  \u274c No feeding/trophic data available (including ontology)")
        result$FS <- NA_character_
      }
    } else {
      message("  \u274c No feeding/trophic data available")
      result$FS <- NA_character_
    }
  }

  # 3. MB - Mobility
  message("\n[MB] Mobility:")
  if (length(mobility_info) > 0 || !is.null(body_shape)) {
    if (!is.null(body_shape)) message("  \U0001f3ca Body Shape: ", body_shape)
    if (length(mobility_info) > 0) message("  \U0001f3ca Mobility Info: ", paste(mobility_info, collapse = ", "))
    result$MB <- harmonize_mobility(mobility_info, body_shape, raw_traits$worms)
    message("  \u2713 Output: ", result$MB)
    mb_labels <- c("MB1"="Sessile", "MB2"="Limited Movement", "MB3"="Floater/Drifter",
                   "MB4"="Crawler/Walker", "MB5"="Swimmer")
    if (!is.na(result$MB) && result$MB %in% names(mb_labels)) {
      message("     (", mb_labels[result$MB], ")")
    }
  } else {
    # Try fuzzy harmonization from ontology traits
    if (!is.null(raw_traits$ontology)) {
      message("  \U0001f50d Trying fuzzy harmonization from ontology...")
      fuzzy_mb <- harmonize_fuzzy_mobility(raw_traits$ontology)
      if (!is.na(fuzzy_mb$class)) {
        result$MB <- fuzzy_mb$class
        sources_used <- c(sources_used, "Fuzzy")
        message("  \u2713 Output: ", result$MB, " (from fuzzy ontology, confidence=", fuzzy_mb$confidence, ")")
        mb_labels <- c("MB1"="Sessile", "MB2"="Burrower", "MB3"="Crawler",
                       "MB4"="Limited Swimmer", "MB5"="Swimmer")
        if (result$MB %in% names(mb_labels)) {
          message("     (", mb_labels[result$MB], ")")
        }
        message("     Modalities: ", paste(fuzzy_mb$modalities, collapse = ", "))
      } else {
        message("  \u274c No mobility data available (including ontology)")
        result$MB <- NA_character_
      }
    } else {
      message("  \u274c No mobility data available")
      result$MB <- NA_character_
    }
  }

  # 4. EP - Environmental Position
  message("\n[EP] Environmental Position:")
  if (!is.null(depth_min) || length(habitat_info) > 0) {
    if (!is.null(depth_min)) message("  \U0001f30a Depth Range: ", depth_min, "-", depth_max, " m")
    if (length(habitat_info) > 0) message("  \U0001f30a Habitat Info: ", paste(habitat_info, collapse = ", "))
    result$EP <- harmonize_environmental_position(depth_min, depth_max, habitat_info, raw_traits$worms)
    message("  \u2713 Output: ", result$EP)
    ep_labels <- c("EP1"="Pelagic", "EP2"="Epibenthic", "EP3"="Endobenthic", "EP4"="Interstitial")
    if (!is.na(result$EP) && result$EP %in% names(ep_labels)) {
      message("     (", ep_labels[result$EP], ")")
    }
  } else {
    # Try fuzzy harmonization from ontology traits
    if (!is.null(raw_traits$ontology)) {
      message("  \U0001f50d Trying fuzzy harmonization from ontology...")
      fuzzy_ep <- harmonize_fuzzy_habitat(raw_traits$ontology)
      if (!is.na(fuzzy_ep$class)) {
        result$EP <- fuzzy_ep$class
        sources_used <- c(sources_used, "Fuzzy")
        message("  \u2713 Output: ", result$EP, " (from fuzzy ontology, confidence=", fuzzy_ep$confidence, ")")
        ep_labels <- c("EP1"="Pelagic", "EP2"="Benthopelagic", "EP3"="Benthic", "EP4"="Intertidal")
        if (result$EP %in% names(ep_labels)) {
          message("     (", ep_labels[result$EP], ")")
        }
        message("     Modalities: ", paste(fuzzy_ep$modalities, collapse = ", "))
      } else {
        message("  \u274c No depth/habitat data available (including ontology)")
        result$EP <- NA_character_
      }
    } else {
      message("  \u274c No depth/habitat data available")
      result$EP <- NA_character_
    }
  }

  # 5. PR - Predator Resistance
  message("\n[PR] Predator Resistance:")
  if (length(protection_info) > 0) {
    message("  \U0001f6e1\ufe0f  Protection Info: ", paste(protection_info, collapse = ", "))
    result$PR <- harmonize_protection(protection_info, raw_traits$worms)
    message("  \u2713 Output: ", result$PR)
    pr_labels <- c("PR1"="Soft-bodied", "PR2"="Moderately Protected", "PR3"="Well Protected")
    if (!is.na(result$PR) && result$PR %in% names(pr_labels)) {
      message("     (", pr_labels[result$PR], ")")
    }
  } else {
    message("  \U0001f6e1\ufe0f  Using taxonomic inference from WoRMS")
    result$PR <- harmonize_protection(protection_info, raw_traits$worms)
    message("  \u2713 Output: ", result$PR)
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
    message("\n\u2554\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2557")
    message("\u2551 ML FALLBACK - Predicting missing traits                       \u2551")
    message("\u255a\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u255d")
    message("\n\U0001f916 Attempting ML prediction for: ", paste(missing_traits, collapse = ", "))

    # Source ML prediction functions if not already loaded
    # Note: Using local = FALSE (default) so that %||% operator from validation_utils.R is available
    if (!exists("apply_ml_fallback")) {
      ml_file <- "R/functions/ml_trait_prediction.R"
      if (file.exists(ml_file)) {
        source(ml_file)
      } else {
        message("  \u26a0\ufe0f  ML prediction functions not found at: ", ml_file)
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
        message("  \u26a0\ufe0f  ML prediction error: ", e$message)
      })
    }
  }

  # =================================================================
  # UNCERTAINTY QUANTIFICATION - Calculate probabilistic confidence
  # =================================================================

  # Source uncertainty quantification functions
  # Note: Using local = FALSE (default) so that %||% operator from validation_utils.R is available
  if (!exists("calculate_all_trait_confidence")) {
    message("  [DEBUG] Sourcing uncertainty_quantification.R...")
    tryCatch({
      source("R/functions/uncertainty_quantification.R")
      message("  [DEBUG] uncertainty_quantification.R sourced successfully")
    }, error = function(e) {
      message("  \u26a0\ufe0f  Uncertainty quantification not available: ", e$message)
    })
  } else {
    message("  [DEBUG] calculate_all_trait_confidence already exists in environment")
  }

  # Calculate confidence for all traits
  if (exists("calculate_all_trait_confidence")) {
    message("\n\u2554\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2557")
    message("\u2551 UNCERTAINTY QUANTIFICATION                                     \u2551")
    message("\u255a\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u255d")

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
    message("  [DEBUG] Calling calculate_all_trait_confidence...")
    confidence_data <- calculate_all_trait_confidence(trait_record)
    message("  [DEBUG] calculate_all_trait_confidence completed")

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
    # Note: Using local = FALSE (default) so that %||% operator from validation_utils.R is available
    if (!exists("apply_phylogenetic_imputation")) {
      tryCatch({
        source("R/functions/phylogenetic_imputation.R")
      }, error = function(e) {
        message("  \u26a0\ufe0f  Phylogenetic imputation not available: ", e$message)
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
  message("\n\u2554\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2557")
  message("\u2551 RESULT SUMMARY                                                 \u2551")
  message("\u2560\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2563")
  message("\u2551 Traits Found: ", n_traits_found, "/5                                           ", paste(rep(" ", max(0, 18-nchar(as.character(n_traits_found)))), collapse=""), "\u2551")
  message("\u2551 Data Sources: ", result$source, paste(rep(" ", max(0, 46-nchar(result$source))), collapse=""), "\u2551")
  message("\u2551 Confidence:   ", toupper(result$confidence), paste(rep(" ", max(0, 46-nchar(result$confidence))), collapse=""), "\u2551")
  message("\u2551                                                                \u2551")

  # Show which traits were found
  trait_status <- c(
    paste0("MS=", ifelse(is.na(result$MS), "\u274c", paste0("\u2713 ", result$MS))),
    paste0("FS=", ifelse(is.na(result$FS), "\u274c", paste0("\u2713 ", result$FS))),
    paste0("MB=", ifelse(is.na(result$MB), "\u274c", paste0("\u2713 ", result$MB))),
    paste0("EP=", ifelse(is.na(result$EP), "\u274c", paste0("\u2713 ", result$EP))),
    paste0("PR=", ifelse(is.na(result$PR), "\u274c", paste0("\u2713 ", result$PR)))
  )
  message("\u2551 ", paste(trait_status, collapse=" | "), paste(rep(" ", max(0, 48-sum(nchar(trait_status))-12)), collapse=""), "\u2551")
  message("\u255a\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u255d")

  total_time <- round(as.numeric(difftime(Sys.time(), total_start, units = "secs")), 2)
  message("\n\u23f1\ufe0f  Total lookup time: ", total_time, "s")

  if (n_traits_found == 5) {
    message("\u2705 SUCCESS: All 5 categorical traits assigned!")
  } else if (n_traits_found >= 3) {
    message("\u26a0\ufe0f  PARTIAL: ", n_traits_found, "/5 traits assigned (missing: ",
           paste(c(if(is.na(result$MS)) "MS", if(is.na(result$FS)) "FS",
                  if(is.na(result$MB)) "MB", if(is.na(result$EP)) "EP",
                  if(is.na(result$PR)) "PR"), collapse=", "), ")")
  } else {
    message("\u274c INCOMPLETE: Only ", n_traits_found, "/5 traits assigned (insufficient data)")
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
