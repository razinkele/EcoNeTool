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
    FS6_filter = "filter|suspension|planktivor|strain|filter feeder|suspension feeder|bivalve"
  ),

  # MOBILITY PATTERNS
  mobility_patterns = list(
    MB1_sessile = "sessile|attached|fixed|cemented|anchored|immobile",
    MB2_burrower = "burrow|infauna|endobenthic|burrowing|sediment dweller",
    MB3_crawler = "crawl|creep|benthic|epibenthic|slow moving|sluggish",
    MB4_swimmer_limited = "slow swim|limited swim|weak swim|drift|plankton",
    MB5_swimmer = "swim|pelagic|nektonic|fast|active|mobile|free-swimming"
  ),

  # ENVIRONMENTAL POSITION PATTERNS
  environmental_patterns = list(
    EP1_pelagic = "pelagic|water column|planktonic|nektonic|open water",
    EP2_epibenthic = "epibenthic|epifauna|surface dwelling|on substrate",
    EP3_endobenthic = "endobenthic|infauna|burrowing|within sediment",
    EP4_demersal = "demersal|near bottom|benthic-pelagic|benthopelagic"
  ),

  # PROTECTION MECHANISM PATTERNS
  protection_patterns = list(
    PR1_none = "soft.?body|naked|unprotected|no shell|no armor",
    PR2_partial = "partial|thin shell|soft shell|flexible|cartilage",
    PR3_shell = "shell|mollusc|bivalve|gastropod|calcified|armored|exoskeleton|carapace|test",
    PR4_spines = "spine|spiny|prickle|thorn|ray|venomous"
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
    zooplankton_pelagic = TRUE
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
    )
  ),

  version = "1.2.0",
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
