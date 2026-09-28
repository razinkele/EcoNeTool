# Harmonization Configuration
# Configuration for trait harmonization thresholds and rules
# Version: 1.2.0
# Created: 2025-12-25

# =============================================================================
# TRAIT VOCABULARY - the single source of truth (NOT user-overridable)
# =============================================================================
# The food-web model (MB_MB / EP_MS / PR_MS in R/functions/trait_foodweb.R)
# prices these codes. Before vocab v2 (deep analysis 2026-09, F17) one MB code
# meant different things in six places: the live harmonizer, the fuzzy
# ontology harmonizer, the offline-DB writer, the local BVOL/SpeciesEnriched
# mappers and the UI legend each had their own table. Everything that assigns,
# labels or validates an MS/FS/MB/EP/PR code now reads this constant through
# get_trait_vocab() / trait_codes(). A session config or saved JSON may tune
# the default *_patterns (merged in trait_patterns(), harmonization.R), never
# the codes, labels, precedence or taxon rules.
#
# Bump trait_vocab_version whenever a code changes meaning. Offline DBs
# (metadata.trait_vocab_version) and cache envelopes stamped with another
# version are then ignored until rebuilt / refreshed, so stale codes are never
# priced by the new matrices.
TRAIT_VOCAB <- local({
  lab <- function(label, description, examples) {
    list(label = label, description = description, examples = examples)
  }
  list(
    trait_vocab_version = 2L,

    labels = list(
      MS = list(
        MS1 = lab("XS (Extra Small)", "< 0.1 cm", "Bacteria, picoplankton, small diatoms"),
        MS2 = lab("S (Small)", "0.1 - 1 cm", "Copepods, copepod nauplii, large diatoms"),
        MS3 = lab("SM (Small-Medium)", "1 - 5 cm", "Amphipods, krill, larval fish"),
        MS4 = lab("M (Medium)", "5 - 20 cm", "Shrimp, gobies, small crabs"),
        MS5 = lab("ML (Medium-Large)", "20 - 50 cm", "Herring, mackerel, plaice"),
        MS6 = lab("L (Large)", "50 - 150 cm", "Cod, tuna, large fish"),
        MS7 = lab("XL (Extra Large - rarely prey)", "> 150 cm", "Sharks, marine mammals")
      ),
      FS = list(
        FS0 = lab("Primary producer / none", "Photosynthesis or chemosynthesis; never a consumer",
                  "Phytoplankton, diatoms, macroalgae"),
        FS1 = lab("Predator / Carnivore", "Active pursuit of live prey",
                  "Active hunters: piscivorous fish, cephalopods"),
        FS2 = lab("Scavenger / Detritivore", "Dead or moribund organisms, detritus",
                  "Carrion feeders, detritus feeders"),
        FS3 = lab("Omnivore", "Mixed diet (plants and animals)", "Mixed-diet generalists"),
        FS4 = lab("Grazer / Herbivore", "Algae or plant consumption",
                  "Algae scrapers, browsers, herbivorous fish"),
        FS5 = lab("Deposit feeder", "Sediment organic matter", "Sediment-ingesting infauna, lugworms"),
        FS6 = lab("Filter / Suspension feeder", "Suspended particles",
                  "Bivalves, planktivorous fish, sponges"),
        FS7 = lab("Xylophagous", "Wood boring", "Wood-borers: shipworms (Teredinidae), gribbles")
      ),
      MB = list(
        MB1 = lab("Sessile", "Permanently attached, no locomotion",
                  "Barnacles, mussels, sponges, sea anemones, hydroid colonies"),
        MB2 = lab("Passive floater / drifter", "Moves with the water (plankton, medusae)",
                  "Phytoplankton, jellyfish, salps, ctenophores"),
        MB3 = lab("Crawler-burrower", "Benthic locomotion, including infaunal burrowers",
                  "Crabs, sea stars, snails, lugworms, burrowing clams"),
        MB4 = lab("Facultative / limited swimmer", "Swims occasionally, rests on or near the bottom",
                  "Flatfish, shrimp, mysids"),
        MB5 = lab("Obligate swimmer", "Continuous active swimming", "Most fish, squid, marine mammals")
      ),
      EP = list(
        EP1 = lab("Pelagic", "Water column, no substrate contact", "Plankton, herring, jellyfish"),
        EP2 = lab("Benthopelagic", "Near the bottom, swims in the water column", "Cod, whiting, demersal fish"),
        EP3 = lab("Epibenthic", "On the sediment or rock surface (incl. attached and tube-dwelling taxa)",
                  "Sea stars, crabs, mussels, tube worms"),
        EP4 = lab("Endobenthic / Infaunal", "Within the sediment (burrowing)",
                  "Burrowing bivalves, lugworms, burrowing shrimp")
      ),
      PR = list(
        PR0 = lab("None / Soft body", "No protective structure", "Jellyfish, naked sea slugs, most fish"),
        PR1 = lab("Mucus / Cuticle", "Mucus coat, cuticle or leathery body wall",
                  "Hagfish, sea cucumbers, some larvae"),
        PR2 = lab("Tube", "Protective tube or case", "Tube worms (Polychaeta), serpulids"),
        PR3 = lab("Burrow refuge", "Permanent burrow used as a refuge", "Burrowing shrimp, permanent-burrow dwellers"),
        PR4 = lab("Thin exoskeleton", "Thin chitinous exoskeleton", "Copepods, amphipods, small arthropods"),
        PR5 = lab("Soft shell", "Thin calcium carbonate shell", "Moulting crabs, juvenile bivalves"),
        PR6 = lab("Hard shell", "Thick calcium carbonate shell or test", "Mussels, snails, barnacles"),
        PR7 = lab("Spines / ossicle plates", "Spines, spicules or calcareous ossicle plates",
                  "Sea urchins, sea stars, brittle stars, sponges"),
        PR8 = lab("Armoured", "Heavy carapace or armour", "Crabs, lobsters, sturgeon")
      )
    ),

    # Default text patterns (case-insensitive, perl). classify_by_patterns()
    # wraps each pattern as (?<![a-z])(?:<pattern>): every alternative gets a
    # LEADING word boundary only, so stems still match ("burrow" matches
    # "burrowing", "float" matches "floater") while "tidal" no longer matches
    # inside "subtidal" or "pelagic" inside "benthopelagic". Multi-word
    # alternatives use ".?" because the data use underscores
    # ("limited_swimmer", "tube_dweller", "free_living").
    # Zonation words (subtidal, sublittoral, intertidal, littoral) appear in
    # NO environmental pattern on purpose: they describe a depth zone, not the
    # position relative to the substrate, so taxonomy or depth decides.
    patterns = list(
      mobility = list(
        MB1_sessile = "sessile|attach|cemented|fixed|anchored|immobile|colonial hydroid",
        MB2_drifter = "drift|(holo|mero|zoo|phyto|ichthyo)?plankton|float|passive|medusa",
        MB3_crawler_burrower = paste0("crawl|creep|walk|burrow|infaun|endobenth|tube.?dwell|",
                                      "limited.?movement|slow.?moving|sluggish"),
        MB4_facultative_swimmer = "limited.?swim|facultative.?swim|weak.?swim|slow.?swim|swim\\w*.{0,20}occasional",
        MB5_obligate_swimmer = "swim|nekton|active.?swim"
      ),
      environmental = list(
        EP1_pelagic = paste0("(epi|meso|bathy|abysso)?pelagic|water.?column|(holo|mero|zoo|phyto)?plankton|",
                             "nekton|open.?water|midwater|neust"),
        EP2_benthopelagic = "bentho.?pelagic|benthic.pelagic|demersal|near.?bottom|hyperbenth",
        EP3_epibenthic = paste0("epibenth|epifaun|epilith|epiflor|epiphyt|epizo|benthic|benthos|bottom|seabed|",
                                "^surface$|surface.?dwell|on.?substrate|attached|sessile|tube|free.?living|crevice"),
        EP4_endobenthic = "endobenth|infaun|burrow|interstitial|within.?sediment|buried|lithotom"
      ),
      protection = list(
        PR0_none = "^soft$|soft.?bod(y|ied)|naked|none|unprotected|jellyfish|cephalopod|crustose|cushion|stalked",
        PR1_mucus = "mucus|slime|cuticle|cuticular|hagfish|tunic|leathery",
        PR2_tube = "tube|tubicol|parchment",
        PR3_burrow = "burrow",
        PR4_exoskeleton = "exoskeleton|chitin|thin.?carapace|small.?arthropod",
        PR5_soft_shell = "soft.?shell|thin.?shell|weak.?shell|partial.?shell|flexible.?shell|cartilage",
        PR6_hard_shell = "shell|calcif|calcareous|calcium|bivalve|test|barnacle",
        PR7_spines = "spine|spiny|spicule|prickle|thorn|ossicle|urchin",
        PR8_armoured = paste0("armou?r|heavy.?carapace|thick.?carapace|hard.?carapace|crab.?carapace|",
                              "^carapace$|heavy.?exoskeleton|calci\\w*.exoskeleton|lobster")
      )
    ),

    # Codes are tested in this order; the first match wins. Specific before
    # generic where one phrase legitimately holds two terms: "benthic
    # surface" -> EP3 before EP1, "soft shell" -> PR5 before PR6's "shell",
    # "limited_swimmer" -> MB4 before MB5's "swim", "calcareous tube" -> PR2
    # before PR6's "calcareous".
    pattern_precedence = list(
      environmental = c("EP4", "EP2", "EP3", "EP1"),
      protection = c("PR8", "PR7", "PR5", "PR2", "PR6", "PR4", "PR3", "PR1", "PR0"),
      mobility = c("MB1", "MB4", "MB5", "MB3", "MB2"),
      foraging = c("FS0", "FS1", "FS2", "FS3", "FS4", "FS5", "FS6", "FS7")
    ),

    # Ordered taxonomic rules for apply_taxon_rules(). A rule fires when every
    # `match` entry (taxonomy field -> regex, case-insensitive) matches, its
    # optional `text` regex matches the trait text, and at least one of its
    # `flag`s (taxonomic_rules switches in the harmonization settings) is on.
    # `override_text = TRUE` rules win over text patterns (protection only).
    # environmental_pelagic runs BEFORE the depth rule, environmental after it.
    taxon_rules = list(
      mobility = list(
        list(match = list(class = "Actinopteri|Elasmobranchii|Teleostei"), code = "MB5",
             flag = "fish_obligate_swimmers"),
        list(match = list(phylum = "^Mollusca$", class = "^Bivalvia$"), code = "MB1", flag = "bivalves_sessile"),
        list(match = list(phylum = "^Mollusca$", class = "^Cephalopoda$"), code = "MB5",
             flag = "cephalopods_swimmers"),
        list(match = list(phylum = "^Mollusca$", class = "^Gastropoda$"), code = "MB3"),
        list(match = list(phylum = "^Arthropoda$", class = "Copepoda"), code = "MB5"),
        list(match = list(phylum = "^Arthropoda$", class = "Malacostraca"), code = "MB4"),
        # Cnidaria by class (F38): medusae drift, polyps are sessile. Other or
        # unknown Cnidaria get no taxon code.
        list(match = list(phylum = "^Cnidaria$", class = "^(Scyphozoa|Cubozoa)$"), code = "MB2"),
        list(match = list(phylum = "^Ctenophora$"), code = "MB2"),
        list(match = list(phylum = "^Cnidaria$", class = "^Hydrozoa$"), text = "medusa|pelagic", code = "MB2"),
        list(match = list(phylum = "^Cnidaria$", class = "^(Anthozoa|Staurozoa|Hydrozoa)$"), code = "MB1"),
        list(match = list(phylum = "^Porifera$"), code = "MB1")
      ),
      environmental_pelagic = list(
        list(match = list(feeding_mode = "photosyn"), code = "EP1", flag = "phytoplankton_pelagic"),
        list(match = list(class = "Bacillariophyceae|Dinophyceae|Prymnesiophyceae"), code = "EP1",
             flag = "phytoplankton_pelagic"),
        list(match = list(class = "Copepoda|Cladocera|Branchiopoda|Appendicularia|Thaliacea"), code = "EP1",
             flag = "zooplankton_pelagic"),
        list(match = list(phylum = "^(Chaetognatha|Ctenophora)$"), code = "EP1", flag = "zooplankton_pelagic"),
        list(match = list(phylum = "^Cnidaria$", class = "^(Scyphozoa|Cubozoa)$"), code = "EP1",
             flag = "zooplankton_pelagic"),
        list(match = list(phylum = "^Cnidaria$", class = "^Hydrozoa$"), text = "medusa|pelagic", code = "EP1",
             flag = "zooplankton_pelagic")
      ),
      environmental = list(
        list(match = list(phylum = "^Mollusca$", class = "^Bivalvia$"), code = "EP4", flag = "infaunal_bivalves"),
        # Fish by order: defaulting all fish to EP1 mislabelled every flatfish,
        # goby, eel and sandeel; unknown orders fall back to EP2.
        list(match = list(class = "Actinopteri|Teleostei",
                          order = paste0("Pleuronectiformes|Gobiiformes|Anguilliformes|Scorpaeniformes|",
                                         "Lophiiformes|Ophidiiformes")),
             code = "EP3"),
        list(match = list(class = "Actinopteri|Teleostei",
                          order = "Clupeiformes|Scombriformes|Beloniformes|Carangiformes|Atheriniformes"),
             code = "EP1"),
        list(match = list(class = "Actinopteri|Teleostei",
                          order = "Gadiformes|Perciformes|Aulopiformes|Stomiiformes|Myctophiformes"),
             code = "EP2"),
        list(match = list(class = "Actinopteri|Teleostei"), code = "EP2")
      ),
      protection = list(
        # Echinoderm ossicles beat any text: an urchin described as having
        # "calcareous plates" is PR7, not PR6 (F38).
        list(match = list(phylum = "^Echinodermata$", class = "^(Echinoidea|Asteroidea|Ophiuroidea|Crinoidea)$"),
             code = "PR7", flag = "echinoderms_calcium_plates", override_text = TRUE),
        list(match = list(phylum = "^Echinodermata$", class = "^Holothuroidea$"),
             code = "PR1", flag = "echinoderms_calcium_plates", override_text = TRUE),
        list(match = list(phylum = "^Mollusca$", class = "^Bivalvia$"), code = "PR6", flag = "bivalves_hard_shell"),
        list(match = list(phylum = "^Mollusca$", class = "^Gastropoda$"), code = "PR6",
             flag = "gastropods_hard_shell"),
        list(match = list(phylum = "^Mollusca$", class = "^Cephalopoda$"), code = "PR0"),
        list(match = list(phylum = "^Mollusca$"), code = "PR6",
             flag = c("bivalves_hard_shell", "gastropods_hard_shell")),
        list(match = list(phylum = "^Arthropoda$", class = "Malacostraca"), code = "PR8",
             flag = "crustaceans_exoskeleton"),
        # Copepods, cladocerans, ostracods and other small arthropods.
        list(match = list(phylum = "^Arthropoda$"), code = "PR4"),
        list(match = list(phylum = "^Cnidaria$"), code = "PR0"),
        list(match = list(phylum = "^Annelida$", living_habit = "tube"), code = "PR2"),
        list(match = list(phylum = "^Annelida$"), code = "PR0"),
        list(match = list(class = "Actinopteri|Teleostei"), code = "PR0"),
        list(match = list(phylum = "^Porifera$"), code = "PR7")
      )
    ),

    # The pre-v2 default patterns of the keys v2 kept. A server default or
    # imported JSON saved before v2 carries these verbatim (the export writes
    # the whole config); trait_patterns() treats such a value as "not
    # customised" so the v2 default applies instead of silently restoring the
    # v1 behaviour. Historical values: never edit.
    legacy_patterns = list(
      mobility = list(
        MB1_sessile = "sessile|attached|fixed|cemented|anchored|immobile"
      ),
      environmental = list(
        EP1_pelagic = "pelagic|water column|planktonic|nektonic|open water",
        EP2_benthopelagic = "benthopelagic|demersal|near bottom|benthic-pelagic",
        EP3_epibenthic = "epibenthic|epifauna|surface dwelling|on substrate|^surface$",
        EP4_endobenthic = "endobenthic|infauna|burrowing|within sediment|interstitial"
      ),
      protection = list(
        PR0_none = "soft.?bod|naked|unprotected|no shell|no armor|jellyfish|cephalopod|^soft$|crustose|cushion|stalked",
        PR1_mucus = "mucus|slime|cuticle|cuticular|hagfish|tunic",
        PR2_tube = "tube|tube.?dwell|calcareous tube|parchment tube",
        PR3_burrow = "deep burrow|permanent burrow|burrow refuge",
        PR4_exoskeleton = "exoskeleton|chitinous|thin carapace|small arthropod",
        PR5_soft_shell = "soft.?shell|partial shell|flexible shell|cartilage|thin shell",
        PR6_hard_shell = "shell|calcified|calcareous|bivalve shell|gastropod shell|hard carapace|test|barnacle",
        PR7_spines = "spine|spiny|spicule|prickle|thorn|ossicle|urchin",
        PR8_armoured = "armoured|armored|heavy carapace|thick carapace|lobster|crab carapace"
      )
    )
  )
})

#' The trait vocabulary (codes, labels, default patterns, precedence, taxon rules)
#'
#' Never a session value: the vocabulary is what the food-web model prices.
#' @return The TRAIT_VOCAB list.
get_trait_vocab <- function() {
  TRAIT_VOCAB
}

#' Vocabulary version stamped into offline DBs and cache envelopes
#' @return integer(1)
current_trait_vocab_version <- function() {
  get_trait_vocab()$trait_vocab_version
}

#' Valid codes for one trait, in order
#'
#' @param trait One of "MS", "FS", "MB", "EP", "PR".
#' @return Character vector, e.g. c("MB1", ..., "MB5").
trait_codes <- function(trait) {
  labels <- get_trait_vocab()$labels
  if (!is.character(trait) || length(trait) != 1L || !trait %in% names(labels)) {
    stop(sprintf("trait must be one of: %s", paste(names(labels), collapse = ", ")), call. = FALSE)
  }
  names(labels[[trait]])
}

#' Named character vectors of code labels per trait (TRAIT_DEFINITIONS)
#' @return list(MS = c(MS1 = "..."), FS = ..., MB = ..., EP = ..., PR = ...)
trait_definitions <- function() {
  lapply(get_trait_vocab()$labels, function(codes) vapply(codes, function(x) x$label, character(1)))
}

#' Human-readable label of one code, e.g. "MB2" -> "Passive floater / drifter"
#' @param code A trait code.
#' @return character(1); NA for NA or an unknown code.
trait_code_label <- function(code) {
  if (length(code) != 1L || is.na(code)) return(NA_character_)
  trait <- sub("[0-9]+$", "", as.character(code))
  info <- get_trait_vocab()$labels[[trait]][[as.character(code)]]
  if (is.null(info)) NA_character_ else info$label
}

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

  # Code labels: copies of TRAIT_VOCAB$labels, kept here so an exported
  # config documents them. Readers (UI legends, TRAIT_DEFINITIONS) use
  # get_trait_vocab(), never these copies: labels are not user-overridable.
  size_labels = TRAIT_VOCAB$labels$MS,
  foraging_labels = TRAIT_VOCAB$labels$FS,
  mobility_labels = TRAIT_VOCAB$labels$MB,
  environmental_labels = TRAIT_VOCAB$labels$EP,
  protection_labels = TRAIT_VOCAB$labels$PR,

  # MB / EP / PR PATTERNS: the tunable defaults. trait_patterns() merges a
  # session or JSON value over TRAIT_VOCAB$patterns key by key.
  mobility_patterns = TRAIT_VOCAB$patterns$mobility,
  environmental_patterns = TRAIT_VOCAB$patterns$environmental,
  protection_patterns = TRAIT_VOCAB$patterns$protection,

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

# Slider min/max/step (cm) per size-class boundary. The sliders in
# R/ui/harmonization_settings_ui.R are built from this, and the validator
# rejects any threshold outside [min, max] or off the step grid. Reason: the
# browser slider clamps and snaps, and its echo writes the adjusted value back
# into the session config, so an imported MS6_MS7 = 500 silently became 300.
HARM_THRESHOLD_RANGES <- list(
  MS1_MS2 = c(min = 0.01, max = 0.5, step = 0.01),
  MS2_MS3 = c(min = 0.1, max = 5.0, step = 0.1),
  MS3_MS4 = c(min = 1.0, max = 20.0, step = 0.5),
  MS4_MS5 = c(min = 5.0, max = 50.0, step = 1.0),
  MS5_MS6 = c(min = 20.0, max = 100.0, step = 5.0),
  MS6_MS7 = c(min = 50.0, max = 300.0, step = 10.0)
)

# Diet nouns an FS0 (primary producer) pattern must never match. They were
# removed from FS0 on 2026-07-17: FS0 is tested before FS1-FS6 on the pasted
# feeding text, so a consumer whose diet mentions algae or diatoms was coded
# as an autotroph, inverting the base of the food web. The validator rejects
# any FS0 pattern matching one of these, so a stale server-default file can
# never bring the inversion back.
HARM_FS0_DIET_NOUNS <- c("plant", "algae", "phytoplankton", "diatom", "dinoflagellate",
                         "seaweed", "macroalgae")

# Fix round 1 (2026-09-28 review): checking an FS0 pattern only against the
# seven bare nouns above missed forms like "photosyn|algal" or
# "photosyn|microalgae" - "algal" and "microalgae" are not substrings of any
# HARM_FS0_DIET_NOUNS entry, so grepl(fs0, HARM_FS0_DIET_NOUNS) never fired,
# yet both match real feeding text ("algal film", "grazes microalgae"). This
# probe set folds in plurals, common compounds/adjectives, and realistic
# pasted diet phrases, so an FS0 pattern is checked against what a real
# feeding-text field is likely to contain, not just the seven bare nouns.
# HARM_FS0_DIET_NOUNS itself is unchanged (still the base of the probe set).
HARM_FS0_DIET_PROBES <- unique(c(
  HARM_FS0_DIET_NOUNS,
  paste0(HARM_FS0_DIET_NOUNS, "s"),
  c("microalgae", "macroalgae", "algal", "seagrass", "seagrasses", "kelp",
    "seaweed", "seaweeds", "plant material", "planktonic algae"),
  c("feeds on diatoms", "grazes microalgae", "algal film",
    "herbivore eating plants", "phytoplankton feeder"),
  # Final fix wave: producer vocabulary that shares no substring with the
  # entries above ("plantae", "vegetation", "macrophyte", ...).
  c("plantae", "plant matter", "vegetation", "macrophyte", "macrophytes",
    "periphyton", "microphytobenthos")
))

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

#' Shape errors in a raw config, checked BEFORE it is merged with the defaults
#'
#' utils::modifyList() deletes a key whose new value is NULL (JSON null) and
#' silently ignores an unnamed list (a JSON array) given where the defaults hold
#' a named list, so both must be caught on the raw input. Walks every level at
#' which the defaults hold a non-empty named list.
#'
#' @param raw The raw (parsed) value at `path`.
#' @param default The HARMONIZATION_CONFIG value at the same path.
#' @param path Dotted path used in the error text.
#' @return character() of errors.
harm_raw_shape_errors <- function(raw, default, path = "") {
  errors <- character()
  for (key in intersect(names(raw), names(default))) {
    here <- if (nzchar(path)) paste0(path, ".", key) else key
    val <- raw[[key]]
    def <- default[[key]]
    if (is.null(val)) {
      errors <- c(errors, sprintf("%s: explicit null is not allowed (the key exists in the defaults)", here))
    } else if (is.list(def) && length(def) > 0L && !is.null(names(def))) {
      if (!is.list(val)) {
        errors <- c(errors, sprintf("%s: must be a JSON object with named keys", here))
      } else if (length(val) > 0L && (is.null(names(val)) || !all(nzchar(names(val))))) {
        errors <- c(errors, sprintf("%s: must be a JSON object with named keys, not an array", here))
      } else {
        errors <- c(errors, harm_raw_shape_errors(val, def, here))
      }
    }
  }
  errors
}

#' Validate (and normalise) a harmonization config
#'
#' Shared by the server-default loader, JSON import and the "Save as server
#' default" button (spec B section 4.1, F2/F8). Unknown top-level keys are
#' dropped; explicit nulls and arrays-for-objects are rejected on the raw input;
#' missing keys are then filled from HARMONIZATION_CONFIG with
#' utils::modifyList() BEFORE the remaining checks run. Every section an import
#' can carry is checked, because an imported config can be saved as the server
#' default: all *_patterns sections, all *_labels sections, profiles, the size
#' thresholds (against HARM_THRESHOLD_RANGES), taxonomic_rules and
#' active_profile.
#'
#' @param cfg A config list (e.g. from jsonlite::fromJSON(simplifyVector = FALSE)).
#' @return list(ok = logical(1), errors = character(), config = <normalised list>).
#'   `config` is HARMONIZATION_CONFIG when `cfg` is not a named list.
validate_harmonization_config <- function(cfg) {
  if (!is.list(cfg) || is.null(names(cfg))) {
    return(list(ok = FALSE, errors = "config must be a JSON object (a named list)",
                config = HARMONIZATION_CONFIG))
  }
  cfg <- cfg[intersect(names(cfg), names(HARMONIZATION_CONFIG))]
  errors <- harm_raw_shape_errors(cfg, HARMONIZATION_CONFIG)
  cfg <- utils::modifyList(HARMONIZATION_CONFIG, cfg)

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
  # A separate check (not folded into the NA path above) so the "strictly
  # increasing" error is still reported alongside an out-of-range value.
  for (key in HARM_THRESHOLD_KEYS[!is.na(vals)]) {
    rng <- HARM_THRESHOLD_RANGES[[key]]
    v <- vals[[key]]
    steps <- (v - rng[["min"]]) / rng[["step"]]
    if (v < rng[["min"]] - 1e-8 || v > rng[["max"]] + 1e-8) {
      errors <- c(errors, sprintf("size_thresholds: %s = %s is outside the slider range [%s, %s]",
                                  key, format(v), format(rng[["min"]]), format(rng[["max"]])))
    } else if (abs(round(steps) - steps) >= 1e-8) {
      errors <- c(errors, sprintf("size_thresholds: %s = %s is not on the slider step (%s from %s)",
                                  key, format(v), format(rng[["step"]]), format(rng[["min"]])))
    }
  }

  # Every *_patterns section: a named list holding all of the defaults' keys,
  # each value a single compilable, non-blank regular expression.
  for (section in grep("_patterns$", names(HARMONIZATION_CONFIG), value = TRUE)) {
    pats <- cfg[[section]]
    if (!is.list(pats) || length(pats) == 0L || is.null(names(pats))) {
      errors <- c(errors, sprintf("%s: must be a named list of regular expressions", section))
      next
    }
    missing_keys <- setdiff(names(HARMONIZATION_CONFIG[[section]]), names(pats))
    if (length(missing_keys) > 0L) {
      errors <- c(errors, sprintf("%s: missing patterns: %s", section, paste(missing_keys, collapse = ", ")))
    }
    bad_pats <- names(pats)[!vapply(pats, harm_pattern_compiles, logical(1))]
    if (length(bad_pats) > 0L) {
      errors <- c(errors, sprintf("%s: not a valid non-empty regular expression: %s",
                                  section, paste(bad_pats, collapse = ", ")))
    }
  }
  fs0 <- cfg$foraging_patterns$FS0_primary_producer
  if (harm_pattern_compiles(fs0)) {
    diet_hits <- HARM_FS0_DIET_PROBES[grepl(fs0, HARM_FS0_DIET_PROBES, ignore.case = TRUE)]
    if (length(diet_hits) > 0L) {
      errors <- c(errors, sprintf(
        paste0("foraging_patterns: FS0_primary_producer matches diet nouns (%s); FS0 is tested first, ",
               "so consumers whose diet mentions them would be coded as producers"),
        paste(diet_hits, collapse = ", ")
      ))
    }
  }

  # Every *_labels section feeds a legend table (trait_research_ui.R).
  for (section in grep("_labels$", names(HARMONIZATION_CONFIG), value = TRUE)) {
    labels <- cfg[[section]]
    if (!is.list(labels) || length(labels) == 0L || is.null(names(labels))) {
      errors <- c(errors, sprintf("%s: must be a non-empty named list", section))
    }
  }

  # Profiles: apply_size_adjustment() multiplies measured lengths by
  # size_multiplier, so it must be a single finite number > 0 where present.
  profiles <- cfg$profiles
  if (!is.list(profiles) || length(profiles) == 0L || is.null(names(profiles))) {
    errors <- c(errors, "profiles: must be a non-empty named list")
  } else {
    for (name in names(profiles)) {
      prof <- profiles[[name]]
      if (!is.list(prof)) {
        errors <- c(errors, sprintf("profiles: %s must be a JSON object", name))
        next
      }
      mult <- prof$size_multiplier
      if (!is.null(mult) && !(is.numeric(mult) && length(mult) == 1L && is.finite(mult) && mult > 0)) {
        errors <- c(errors, sprintf("profiles: %s size_multiplier must be a finite number > 0", name))
      }
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
  # mid-write can never leave a truncated server default behind. Fix round 1:
  # writeLines() itself can fail (tmp path blocked, disk full mid-write) and
  # used to leave a stray/partial <file>.tmp behind - clean it up on ANY
  # failure, but only ever remove a regular file we could have written
  # ourselves, never a directory that happens to occupy the tmp path (that
  # is a pre-existing filesystem condition, not something this function made).
  tmp <- paste0(file, ".tmp")
  on.exit({
    if (file.exists(tmp) && !dir.exists(tmp)) unlink(tmp, force = TRUE)
  }, add = TRUE)
  writeLines(json_data, tmp)
  if (!file.rename(tmp, file)) {
    stop(sprintf("could not move '%s' into place", tmp), call. = FALSE)
  }
  message("✓ Harmonization configuration saved to: ", file)
  invisible(file)
}

#' Warn that the server-default file was rejected
#'
#' The warning carries class "harm_config_rejected" and the error texts in
#' `$errors`, so the settings module can show them to the user at session
#' start (production keeps no logs, so the warning alone is invisible there).
#'
#' @param text Warning message.
#' @param errors Character vector of the reasons.
harm_warn_rejected <- function(text, errors) {
  warning(warningCondition(text, class = "harm_config_rejected", call = NULL,
                           errors = errors))
}

#' Read the server-default config file
#'
#' Returns HARMONIZATION_CONFIG when the file is missing. When it cannot be
#' parsed or does not validate, warns (class "harm_config_rejected", see
#' harm_warn_rejected()) and returns HARMONIZATION_CONFIG.
#'
#' @param file Path to the JSON file.
#' @return A config list.
load_harmonization_config <- function(file = HARMONIZATION_CONFIG_FILE) {
  if (!file.exists(file)) return(HARMONIZATION_CONFIG)
  # A flag, not a NULL result: a file holding the JSON literal `null` also
  # parses to NULL and must reach the validator (and warn), not silently
  # become the defaults.
  parse_failed <- FALSE
  raw <- tryCatch({
    jsonlite::fromJSON(file, simplifyVector = FALSE)
  }, error = function(e) {
    # Falling back to defaults is right; doing it silently is not - the user
    # would see their saved settings quietly revert with no explanation.
    parse_failed <<- TRUE
    harm_warn_rejected(sprintf("[harmonization] could not parse '%s', using defaults: %s",
                               file, conditionMessage(e)),
                       paste("could not parse the file:", conditionMessage(e)))
    NULL
  })
  if (parse_failed) return(HARMONIZATION_CONFIG)
  checked <- validate_harmonization_config(raw)
  if (!checked$ok) {
    harm_warn_rejected(sprintf("[harmonization] invalid config in '%s', using defaults: %s",
                               file, paste(checked$errors, collapse = "; ")),
                       checked$errors)
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
