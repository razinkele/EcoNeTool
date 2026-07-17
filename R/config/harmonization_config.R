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
    FS0_primary_producer = "photosyn|autotrop|producer|plant|algae|phytoplankton|diatom|dinoflagellate",
    FS1_predator = "predat|carnivor|pisciv|hunter|predaceous|carnivore|predator",
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
    EP3_epibenthic = "epibenthic|epifauna|surface dwelling|on substrate",
    EP4_endobenthic = "endobenthic|infauna|burrowing|within sediment|interstitial"
  ),

  # PROTECTION MECHANISM PATTERNS (9-level PR0-PR8, matching harmonize_protection)
  # PR1 (mucus/cuticle) was missing pre-PR1b — UI listed it but config and
  # harmonize_protection had no branch, so any "mucus"-flagged species got
  # NA. Added between PR0_none and PR2_tube to fill the gap.
  protection_patterns = list(
    PR0_none = "soft.?bod|naked|unprotected|no shell|no armor|jellyfish|cephalopod|^soft$|crustose|cushion|stalked",
    PR1_mucus = "mucus|slime|cuticle|cuticular|hagfish",
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
    RS1_broadcast = "broadcast|free.spawn|pelagic.larv|planktotrophic",
    RS2_brooder = "brood|direct.develop|lecithotrophic|vivip|ovovivip",
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

save_harmonization_config <- function(config = HARMONIZATION_CONFIG, file = "config/harmonization_custom.json") {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  json_data <- jsonlite::toJSON(config, pretty = TRUE, auto_unbox = TRUE)
  writeLines(json_data, file)
  message("✓ Harmonization configuration saved to: ", file)
}

load_harmonization_config <- function(file = "config/harmonization_custom.json") {
  if (!file.exists(file)) return(HARMONIZATION_CONFIG)
  tryCatch({
    jsonlite::fromJSON(file, simplifyVector = FALSE)
  }, error = function(e) HARMONIZATION_CONFIG)
}
