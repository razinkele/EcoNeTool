# Harmonization Configuration
# Configuration for trait harmonization thresholds and rules
# Version: 1.2.0
# Created: 2025-12-25

# Default Harmonization Configuration
HARMONIZATION_CONFIG <- list(

  # SIZE CLASS THRESHOLDS (cm)
  size_thresholds = list(
    MS1_MS2 = 0.1,    # 1 mm - microorganisms, larvae
    MS2_MS3 = 1.0,    # 1 cm - small invertebrates
    MS3_MS4 = 5.0,    # 5 cm - medium invertebrates, small fish
    MS4_MS5 = 20.0,   # 20 cm - large invertebrates, medium fish
    MS5_MS6 = 50.0,   # 50 cm - large fish
    MS6_MS7 = 150.0   # 150 cm - very large fish, marine mammals
  ),

  # FORAGING STRATEGY PATTERNS
  foraging_patterns = list(
    # FS0 must match only producer-specific vocabulary. Diet nouns
    # (plant|algae|phytoplankton|diatom|...) were removed 2026-07-17: because FS0
    # is tested before FS1-FS6 and all sources are paste()-collapsed, any consumer
    # whose diet text mentioned algae/diatoms was silently coded as an autotroph,
    # inverting trophic structure at the base of the food web. True producers are
    # still caught here (photosyn|autotroph|producer) and by the trophic_level<1.5
    # guard in harmonize_foraging_strategy().
    FS0_primary_producer = "photosyn|autotroph|autotrop|primary.?produc|producer",
    FS1_predator = "predat|carnivor|pisciv|hunter|predaceous|carnivore|predator|parasit",
    FS2_scavenger = "scaveng|detritivor|carrion|scavenger|detritus feeder",
    FS3_omnivore = "omnivore|omnivorous|mixed diet|generalist feeder",
    FS4_grazer = "graz|herbiv|scraper|browser|grazer|herbivore|algivore",
    FS5_deposit = "deposit|sediment|burrower|deposit feeder|mud feeder|sand feeder",
    FS6_filter = "filter|suspension|planktivor|strain|filter feeder|suspension feeder|bivalve",
    FS7_xylophagous = "xylophag|wood.bor|wood.eat|lignivor"
  ),

  # Human-readable FS labels. Single source of truth — same data-driven
  # UI pattern as protection_labels (PR1b) and reproductive/temperature/salinity
  # labels (PR8b Phase B). Pre-P4 the UI hard-coded a table with the wrong
  # ordering (FS1=Herbivore, FS2=Omnivore, FS3=Predator, FS4=Scavenger),
  # mismatching foraging_patterns (FS1=predator, FS2=scavenger, FS3=omnivore,
  # FS4=grazer/herbivore) and silently producing wrong harmonized codes.
  foraging_labels = list(
    FS0 = list(label = "Primary producer",
               examples = "Phytoplankton, diatoms, macroalgae"),
    FS1 = list(label = "Predator / Carnivore",
               examples = "Active hunters: piscivorous fish, cephalopods"),
    FS2 = list(label = "Scavenger / Detritivore",
               examples = "Carrion feeders, detritus feeders"),
    FS3 = list(label = "Omnivore",
               examples = "Mixed-diet generalists"),
    FS4 = list(label = "Grazer / Herbivore",
               examples = "Algae scrapers, browsers, herbivorous fish"),
    FS5 = list(label = "Deposit feeder",
               examples = "Sediment-ingesting infauna, lugworms"),
    FS6 = list(label = "Filter / Suspension feeder",
               examples = "Bivalves, planktivorous fish, sponges"),
    FS7 = list(label = "Xylophagous",
               examples = "Wood-borers: shipworms (Teredinidae), gribbles")
  ),

  # MOBILITY PATTERNS
  mobility_patterns = list(
    MB1_sessile = "sessile|attached|fixed|cemented|anchored|immobile",
    MB2_burrower = "burrow|infauna|endobenthic|burrowing|sediment dweller",
    MB3_crawler = "crawl|creep|benthic|epibenthic|slow moving|sluggish|limited.?movement",
    MB4_swimmer_limited = "slow swim|limited swim|weak swim|drift|plankton|limited.?swim|float",
    MB5_swimmer = "swim|pelagic|nektonic|fast|active|mobile|free-swimming"
  ),

  # ENVIRONMENTAL POSITION PATTERNS
  environmental_patterns = list(
    EP1_pelagic = "pelagic|water column|planktonic|nektonic|open water",
    EP2_benthopelagic = "benthopelagic|demersal|near bottom|benthic-pelagic",
    # Btrait sediment-position label "Surface" -> epibenthic. Anchored ^surface$
    # (the bare label) so it cannot match "Subsurface_deposit", which is a
    # FEEDING label and must reach FS5, not EP3.
    EP3_epibenthic = "epibenthic|epifauna|surface dwelling|on substrate|^surface$",
    EP4_endobenthic = "endobenthic|infauna|burrowing|within sediment|interstitial"
  ),

  # PROTECTION MECHANISM PATTERNS (9-level PR0-PR8, matching harmonize_protection)
  # PR1 (mucus/cuticle) was missing pre-PR1b — UI listed it but config and
  # harmonize_protection had no branch, so any "mucus"-flagged species got
  # NA. Added between PR0_none and PR2_tube to fill the gap.
  protection_patterns = list(
    PR0_none = "soft.?bod|naked|unprotected|no shell|no armor|jellyfish|cephalopod|^soft$|crustose|cushion|stalked",
    # Btrait morphology label "Tunic" (the leathery ascidian covering) -> PR1.
    PR1_mucus = "mucus|slime|cuticle|cuticular|hagfish|tunic",
    PR2_tube = "tube|tube.?dwell|calcareous tube|parchment tube",
    PR3_burrow = "deep burrow|permanent burrow|burrow refuge",
    PR4_exoskeleton = "exoskeleton|chitinous|thin carapace|small arthropod",
    PR5_soft_shell = "soft.?shell|partial shell|flexible shell|cartilage|thin shell",
    PR6_hard_shell = "shell|calcified|calcareous|bivalve shell|gastropod shell|hard carapace|test|barnacle",
    PR7_spines = "spine|spiny|spicule|prickle|thorn|ossicle|urchin",
    PR8_armoured = "armoured|armored|heavy carapace|thick carapace|lobster|crab carapace"
  ),

  # Human-readable PR labels. Single source of truth — the UI renders this
  # via R/ui/trait_research_ui.R, so the on-screen legend can no longer
  # drift from the harmonization rules. Pre-PR1b the UI hard-coded a table
  # that disagreed with config for PR2/3/4/7 (e.g., UI PR2 = "Soft tissue"
  # vs config PR2 = "Tube"; UI PR7 = "Scales" vs config PR7 = "Spines").
  protection_labels = list(
    PR0 = list(label = "None / Soft body",  examples = "Jellyfish, naked sea slugs, cephalopods"),
    PR1 = list(label = "Mucus / Cuticle",   examples = "Hagfish, some larvae"),
    PR2 = list(label = "Tube",              examples = "Tube worms (Polychaeta), serpulids"),
    PR3 = list(label = "Burrow refuge",     examples = "Permanent-burrow dwellers"),
    PR4 = list(label = "Thin exoskeleton",  examples = "Copepods, small arthropods"),
    PR5 = list(label = "Soft shell",        examples = "Molting crabs, juvenile bivalves"),
    PR6 = list(label = "Hard shell",        examples = "Mussels, snails, barnacles"),
    PR7 = list(label = "Spines",            examples = "Sea urchins, spiny fish"),
    PR8 = list(label = "Armoured",          examples = "Lobsters, sturgeon, heavy carapace")
  ),

  # REPRODUCTIVE STRATEGY PATTERNS
  reproductive_patterns = list(
    # Btrait egg-development-location labels: Sexual_pelagic -> RS1;
    # Sexual_benthic / Sexual_brooded -> RS2.
    RS1_broadcast = "broadcast|free.spawn|pelagic.larv|planktotrophic|sexual_pelagic",
    RS2_brooder = "brood|direct.develop|lecithotrophic|vivip|ovovivip|sexual_benthic|sexual_brooded",
    RS3_budding = "bud|fission|fragment|asexual|vegetat",
    RS4_mixed = "mixed|both|alternating|sequential"
  ),

  # PR8b Phase B: human-readable labels for the extended modalities.
  # Same data-driven UI pattern as protection_labels (PR1b) so the
  # legend tables in trait_research_ui.R can never drift from the
  # harmonize_*_strategy / _tolerance helpers.
  reproductive_labels = list(
    RS1 = list(label = "Broadcast spawner",
               examples = "Most pelagic fish, sea urchins, mussels"),
    RS2 = list(label = "Brooder / direct developer",
               examples = "Sharks, gobies, peracarid crustaceans"),
    RS3 = list(label = "Asexual / budding / fission",
               examples = "Many cnidarians, some annelids"),
    RS4 = list(label = "Mixed / sequential strategy",
               examples = "Hermaphroditic fish, alternating-generation cnidarians")
  ),

  # TEMPERATURE TOLERANCE PATTERNS
  temperature_patterns = list(
    TT1_cold_steno = "arctic|polar|cold.stenothermal|psychrophil",
    TT2_cold_eury = "boreal|cold.temperate|cold.eurythermal|subarctic",
    TT3_warm_eury = "warm.temperate|eurythermal|cosmopolitan|temperate",
    TT4_warm_steno = "tropical|warm.stenothermal|thermophil|subtropical"
  ),

  temperature_labels = list(
    TT1 = list(label = "Cold-stenothermal",
               examples = "Arctic / polar specialists"),
    TT2 = list(label = "Cold-eurythermal",
               examples = "Boreal, subarctic species"),
    TT3 = list(label = "Warm-eurythermal",
               examples = "Temperate cosmopolitans"),
    TT4 = list(label = "Warm-stenothermal",
               examples = "Tropical / subtropical specialists")
  ),

  # SALINITY TOLERANCE PATTERNS
  salinity_patterns = list(
    ST1_fresh = "freshwater|limnetic",
    ST2_oligo = "oligohaline|brackish.low",
    ST3_meso = "mesohaline|brackish",
    ST4_poly = "polyhaline|marine.brackish",
    ST5_eu = "euhaline|marine|full.saline"
  ),

  salinity_labels = list(
    ST1 = list(label = "Freshwater",      examples = "Limnetic species"),
    ST2 = list(label = "Oligohaline",     examples = "Brackish, low salinity (0.5-5 PSU)"),
    ST3 = list(label = "Mesohaline",      examples = "Brackish, intermediate (5-18 PSU)"),
    ST4 = list(label = "Polyhaline",      examples = "Marine-brackish (18-30 PSU)"),
    ST5 = list(label = "Euhaline",        examples = "Full marine salinity (>30 PSU)")
  ),

  # TAXONOMIC INFERENCE RULES
  taxonomic_rules = list(
    fish_obligate_swimmers = TRUE,
    cephalopods_swimmers = TRUE,
    bivalves_sessile_or_burrowers = TRUE,
    gastropods_crawlers = TRUE,
    crustaceans_varied = TRUE,
    phytoplankton_primary_producers = TRUE,
    zooplankton_filter_feeders = TRUE,
    carnivorous_fish_predators = TRUE,
    herbivorous_fish_grazers = TRUE,
    bivalves_filter_feeders = TRUE,
    molluscs_have_shells = TRUE,
    arthropods_exoskeleton = TRUE,
    fish_no_protection = FALSE,
    echinoderms_calcareous = TRUE,
    fish_class_based_EP = TRUE,
    benthic_invertebrates_EP2_EP3 = TRUE,
    zooplankton_pelagic = TRUE,
    bivalves_sessile = TRUE,
    cnidarians_sessile = TRUE,
    phytoplankton_pelagic = TRUE,
    infaunal_bivalves = TRUE,
    bivalves_hard_shell = TRUE,
    gastropods_hard_shell = TRUE,
    crustaceans_exoskeleton = TRUE,
    echinoderms_calcium_plates = TRUE
  ),

  # ECOSYSTEM PROFILES
  active_profile = "temperate",

  profiles = list(
    arctic = list(
      description = "Arctic and subarctic marine ecosystems (Baltic Sea)",
      size_multiplier = 1.2,
      size_thresholds_adjust = list(MS3_MS4 = 6.0, MS4_MS5 = 24.0)
    ),
    temperate = list(
      description = "Temperate marine ecosystems (North Sea)",
      size_multiplier = 1.0,
      size_thresholds_adjust = list()
    ),
    tropical = list(
      description = "Tropical and subtropical ecosystems",
      size_multiplier = 0.9,
      size_thresholds_adjust = list(MS3_MS4 = 4.5, MS4_MS5 = 18.0)
    ),
    mediterranean = list(
      description = "Mediterranean marine ecosystems",
      size_multiplier = 0.95,
      size_thresholds_adjust = list(MS3_MS4 = 4.5, MS4_MS5 = 18.0)
    ),
    atlantic_ne = list(
      description = "NE Atlantic / Celtic Sea / Bay of Biscay",
      size_multiplier = 1.05,
      size_thresholds_adjust = list()
    ),
    deep_sea = list(
      description = "Deep-sea and bathyal ecosystems (>200m)",
      size_multiplier = 1.3,
      size_thresholds_adjust = list(MS4_MS5 = 25.0, MS5_MS6 = 60.0)
    ),
    baltic = list(
      description = "Baltic Sea (brackish, low salinity)",
      size_multiplier = 0.9,
      size_thresholds_adjust = list()
    ),
    black_sea = list(
      description = "Black Sea marine ecosystems",
      size_multiplier = 1.0,
      size_thresholds_adjust = list()
    )
  ),

  version = "1.3.0",
  last_modified = Sys.Date()
)

# Helper functions
get_size_threshold <- function(boundary, profile = NULL) {
  if (is.null(profile)) profile <- HARMONIZATION_CONFIG$active_profile
  profile_config <- HARMONIZATION_CONFIG$profiles[[profile]]
  if (!is.null(profile_config$size_thresholds_adjust[[boundary]])) {
    return(profile_config$size_thresholds_adjust[[boundary]])
  }
  threshold <- HARMONIZATION_CONFIG$size_thresholds[[boundary]]
  if (!is.null(profile_config$size_multiplier)) {
    threshold <- threshold * profile_config$size_multiplier
  }
  return(threshold)
}

get_foraging_pattern <- function(strategy) {
  HARMONIZATION_CONFIG$foraging_patterns[[strategy]]
}

check_taxonomic_rule <- function(rule_name) {
  rule <- HARMONIZATION_CONFIG$taxonomic_rules[[rule_name]]
  if (is.null(rule)) return(FALSE)
  return(rule)
}

# Shared by the writer, the reader, and the slider module, so all three
# resolve to the same file whatever the working directory is. app_path() is
# not assumed to exist: this file is sourced directly by test helpers before
# validation_utils.R in some paths, so it falls back to the historical
# wd-relative value rather than erroring at source time.
HARMONIZATION_CONFIG_FILE <- if (exists("app_path", mode = "function")) {
  app_path("config/harmonization_custom.json")
} else {
  "config/harmonization_custom.json"
}

# The six size-class boundaries in MS order. The validator, the slider module
# and the tests all iterate this one vector.
HARM_THRESHOLD_KEYS <- c("MS1_MS2", "MS2_MS3", "MS3_MS4", "MS4_MS5", "MS5_MS6", "MS6_MS7")

# Diet nouns an FS0 (primary producer) pattern must never match. They were
# removed from FS0 on 2026-07-17: FS0 is tested before FS1-FS6 on the pasted
# feeding text, so a consumer whose diet mentions algae or diatoms was coded
# as an autotroph, inverting the base of the food web. The validator rejects
# any FS0 pattern matching one of these, so a stale server-default file can
# never bring the inversion back.
HARM_FS0_DIET_NOUNS <- c("plant", "algae", "phytoplankton", "diatom", "dinoflagellate",
                         "seaweed", "macroalgae")

#' Does a foraging pattern compile the way harmonize_foraging_strategy() uses it?
#'
#' A length-1, non-blank string that grepl(..., ignore.case = TRUE) accepts.
#' Blank is refused because an empty pattern matches every text. The tryCatch
#' is a validation probe, not a swallowed failure: FALSE is reported to the
#' caller as a validation error.
#'
#' @param pattern Candidate regular expression.
#' @return TRUE or FALSE.
harm_pattern_compiles <- function(pattern) {
  if (!is.character(pattern) || length(pattern) != 1L || is.na(pattern) ||
        !nzchar(trimws(pattern))) {
    return(FALSE)
  }
  tryCatch({
    suppressWarnings(grepl(pattern, "", ignore.case = TRUE))
    TRUE
  }, error = function(e) FALSE)
}

#' Validate (and normalise) a harmonization config
#'
#' Shared by the server-default loader, JSON import and the "Save as server
#' default" button (spec B section 4.1, F2/F8). Unknown top-level keys are
#' dropped; missing keys are filled from HARMONIZATION_CONFIG with
#' utils::modifyList() BEFORE the checks run.
#'
#' @param cfg A config list (e.g. from jsonlite::fromJSON(simplifyVector = FALSE)).
#' @return list(ok = logical(1), errors = character(), config = <normalised list>).
#'   `config` is HARMONIZATION_CONFIG when `cfg` is not a named list.
validate_harmonization_config <- function(cfg) {
  if (!is.list(cfg) || is.null(names(cfg))) {
    return(list(ok = FALSE, errors = "config must be a JSON object (a named list)",
                config = HARMONIZATION_CONFIG))
  }
  cfg <- utils::modifyList(HARMONIZATION_CONFIG, cfg[intersect(names(cfg), names(HARMONIZATION_CONFIG))])
  errors <- character()

  thr <- if (is.list(cfg$size_thresholds)) cfg$size_thresholds else list()
  vals <- vapply(HARM_THRESHOLD_KEYS, function(k) {
    v <- thr[[k]]
    if (is.numeric(v) && length(v) == 1L && is.finite(v) && v > 0) as.numeric(v) else NA_real_
  }, numeric(1))
  if (anyNA(vals)) {
    errors <- c(errors, sprintf("size_thresholds: %s must be a finite number > 0",
                                paste(HARM_THRESHOLD_KEYS[is.na(vals)], collapse = ", ")))
  } else if (any(diff(vals) <= 0)) {
    errors <- c(errors, "size_thresholds must be strictly increasing from MS1_MS2 to MS6_MS7")
  }

  pats <- if (is.list(cfg$foraging_patterns)) cfg$foraging_patterns else list()
  bad_pats <- names(pats)[!vapply(pats, harm_pattern_compiles, logical(1))]
  if (length(pats) == 0L || length(bad_pats) > 0L) {
    errors <- c(errors, sprintf("foraging_patterns: not a valid non-empty regular expression: %s",
                                paste(bad_pats, collapse = ", ")))
  }
  fs0 <- pats$FS0_primary_producer
  if (harm_pattern_compiles(fs0)) {
    diet_hits <- HARM_FS0_DIET_NOUNS[grepl(fs0, HARM_FS0_DIET_NOUNS, ignore.case = TRUE)]
    if (length(diet_hits) > 0L) {
      errors <- c(errors, sprintf(paste0(
        "foraging_patterns: FS0_primary_producer matches diet nouns (%s); FS0 is tested first, ",
        "so consumers whose diet mentions them would be coded as producers"),
        paste(diet_hits, collapse = ", ")))
    }
  }

  rules <- if (is.list(cfg$taxonomic_rules)) cfg$taxonomic_rules else list()
  is_flag <- vapply(rules, function(x) is.logical(x) && length(x) == 1L && !is.na(x), logical(1))
  if (length(rules) == 0L || !all(is_flag)) {
    errors <- c(errors, sprintf("taxonomic_rules: must be TRUE or FALSE: %s",
                                paste(names(rules)[!is_flag], collapse = ", ")))
  }

  profile <- cfg$active_profile
  if (!is.character(profile) || length(profile) != 1L || !profile %in% names(cfg$profiles)) {
    errors <- c(errors, sprintf("active_profile '%s' is not one of: %s",
                                paste(format(profile), collapse = ","),
                                paste(names(cfg$profiles), collapse = ", ")))
  }

  list(ok = length(errors) == 0L, errors = errors, config = cfg)
}

save_harmonization_config <- function(config = HARMONIZATION_CONFIG,
                                      file = HARMONIZATION_CONFIG_FILE) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  json_data <- jsonlite::toJSON(config, pretty = TRUE, auto_unbox = TRUE, digits = NA)
  # Write beside the target, then rename over it, so a crash or a full disk
  # mid-write can never leave a truncated server default behind.
  tmp <- paste0(file, ".tmp")
  writeLines(json_data, tmp)
  if (!file.rename(tmp, file)) {
    unlink(tmp)
    stop(sprintf("could not move '%s' into place", tmp), call. = FALSE)
  }
  message("✓ Harmonization configuration saved to: ", file)
  invisible(file)
}

load_harmonization_config <- function(file = HARMONIZATION_CONFIG_FILE) {
  if (!file.exists(file)) return(HARMONIZATION_CONFIG)
  raw <- tryCatch({
    jsonlite::fromJSON(file, simplifyVector = FALSE)
  }, error = function(e) {
    # Falling back to defaults is right; doing it silently is not - the user
    # would see their saved settings quietly revert with no explanation.
    warning(sprintf("[harmonization] could not parse '%s', using defaults: %s",
                    file, conditionMessage(e)), call. = FALSE)
    NULL
  })
  if (is.null(raw)) return(HARMONIZATION_CONFIG)
  checked <- validate_harmonization_config(raw)
  if (!checked$ok) {
    warning(sprintf("[harmonization] invalid config in '%s', using defaults: %s",
                    file, paste(checked$errors, collapse = "; ")), call. = FALSE)
    return(HARMONIZATION_CONFIG)
  }
  checked$config
}

#' Write a config to a JSON file (the "Export as JSON" download)
#'
#' @param file Destination path.
#' @param config Config list; defaults to the process-wide default.
#' @return invisible(TRUE).
export_config_json <- function(file, config = HARMONIZATION_CONFIG) {
  jsonlite::write_json(config, file, auto_unbox = TRUE, pretty = TRUE, digits = NA)
  invisible(TRUE)
}

#' Read and validate a config JSON file (the "Import" upload)
#'
#' Never assigns a global: the caller decides where the config goes (the
#' settings module puts it in the session only).
#'
#' @param file Path to a JSON file.
#' @return The validated, normalised config list. Stops with the joined
#'   validation errors when the file is invalid, or with the parse error.
import_config_json <- function(file) {
  raw <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  checked <- validate_harmonization_config(raw)
  if (!checked$ok) {
    stop(paste(checked$errors, collapse = "; "), call. = FALSE)
  }
  checked$config
}
