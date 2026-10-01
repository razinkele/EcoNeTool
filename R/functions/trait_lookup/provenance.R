# =============================================================================
# TRAIT LOOKUP - Provenance and confidence contract (spec C3)
# =============================================================================
# Every trait code T in a lookup result carries:
#   T_source      the database (or rule family) that produced the code, set
#                 when the code is assigned;
#   T_method      "observed" (trait-database text or a measured value),
#                 "rule" (taxonomic or depth rule, fuzzy ontology profile),
#                 "default" (a harmoniser's fall-through code, no evidence),
#                 "ml" or "phylo";
#   T_confidence  numeric in [0, 1], NA exactly when T is NA.
# Only "observed" and "rule" codes may vote as phylogenetic relatives or
# serve as ML training labels.
# Part of the trait_lookup module; pure helpers, no I/O except the cache
# readers at the end.
# =============================================================================

#' All trait columns of a lookup result, in display order
TRAIT_COLUMNS <- c("MS", "FS", "MB", "EP", "PR", "RS", "TT", "ST")

#' The five traits the food-web model uses (and the overall confidence averages)
CORE_TRAIT_COLUMNS <- c("MS", "FS", "MB", "EP", "PR")

#' Methods whose codes count as evidence for relatives and training
RELATIVE_METHODS <- c("observed", "rule")

#' Which database's size wins when several report one (F28)
SIZE_SOURCE_PRECEDENCE <- c("FishBase", "SeaLifeBase", "WoRMS", "BIOTIC", "MAREDAT", "PTDB", "BVOL",
                            "SpeciesEnriched", "freshwaterecology.info", "PelagicTraits")

#' Pick the body size by database precedence (F28)
#'
#' @param candidates Named list source label -> size in cm (NULL / NA /
#'   non-positive entries are ignored). Names outside SIZE_SOURCE_PRECEDENCE
#'   are ignored too.
#' @return list(size_cm, source): the first usable size in
#'   SIZE_SOURCE_PRECEDENCE order, or list(size_cm = NULL, source = NA).
select_size_by_precedence <- function(candidates) {
  for (src in SIZE_SOURCE_PRECEDENCE) {
    v <- suppressWarnings(as.numeric(candidates[[src]]))
    if (length(v) >= 1L && isTRUE(is.finite(v[1])) && v[1] > 0) {
      return(list(size_cm = v[1], source = src))
    }
  }
  list(size_cm = NULL, source = NA_character_)
}

# raw_traits element, field, source label - in database priority order.
TEXT_INPUT_SOURCES <- list(
  feeding = list(
    c("fishbase", "feeding_type", "FishBase"), c("sealifebase", "feeding_type", "SeaLifeBase"),
    c("biotic", "feeding_mode", "BIOTIC"), c("species_enriched", "feeding_method", "SpeciesEnriched"),
    c("freshwater", "feeding_type", "freshwaterecology.info"), c("ptdb", "feeding_mode", "PTDB"),
    c("blacksea", "feeding_mode", "BlackSea"), c("arctic", "feeding_mode", "ArcticTraits"),
    c("cefas", "feeding_mode", "Cefas"), c("pelagic", "feeding_mode", "PelagicTraits"),
    c("worms_attrs", "feeding_type", "WoRMS_Traits"), c("polytraits", "feeding_mode", "PolyTraits"),
    c("emodnet", "feeding_mode", "EMODnet"), c("traitbank", "diet", "TraitBank")
  ),
  trophic_level = list(
    c("fishbase", "trophic_level", "FishBase"), c("sealifebase", "trophic_level", "SeaLifeBase"),
    c("traitbank", "trophic_level", "TraitBank")
  ),
  mobility = list(
    c("biotic", "mobility", "BIOTIC"), c("biotic", "living_habit", "BIOTIC"),
    c("species_enriched", "mobility", "SpeciesEnriched"), c("freshwater", "locomotion", "freshwaterecology.info"),
    c("blacksea", "mobility_info", "BlackSea"), c("arctic", "mobility_info", "ArcticTraits"),
    c("cefas", "mobility_info", "Cefas"), c("polytraits", "mobility_info", "PolyTraits"),
    c("emodnet", "mobility_info", "EMODnet")
  ),
  habitat = list(
    c("worms", "habitat", "WoRMS"), c("worms", "functional_group", "WoRMS"),
    c("fishbase", "habitat", "FishBase"), c("sealifebase", "habitat", "SeaLifeBase"),
    c("biotic", "living_habit", "BIOTIC"), c("biotic", "substratum", "BIOTIC"),
    c("freshwater", "habitat", "freshwaterecology.info"), c("algaebase", "habitat", "AlgaeBase"),
    c("worms_attrs", "zone", "WoRMS_Traits")
  ),
  protection = list(
    c("biotic", "skeleton", "BIOTIC")
  )
)

#' The database that supplied a text- or value-derived code (F28)
#'
#' harmonize_*() reads text pooled from several databases, so the source of a
#' text-derived code is the first database, in priority order, that supplied
#' any input of that kind. Before C-8, FS_source was the database queried
#' last, whatever it held.
#'
#' @param raw_traits The orchestrator's raw_traits list (one $traits list per
#'   database).
#' @param kind "feeding", "trophic_level", "mobility", "habitat" or "protection".
#' @return Source label, or "Harmonized" when no database supplied that input.
text_input_source <- function(raw_traits, kind) {
  for (entry in TEXT_INPUT_SOURCES[[kind]]) {
    v <- raw_traits[[entry[1]]][[entry[2]]]
    if (length(v) > 0 && any(!is.na(v))) return(entry[3])
  }
  "Harmonized"
}

# The offline DB's lower-case primary_source -> the orchestrator's source label.
OFFLINE_SOURCE_LABELS <- c(
  ontology = "Ontology", biotic = "BIOTIC", maredat = "MAREDAT", ptdb = "PTDB", bvol = "BVOL",
  species_enriched = "SpeciesEnriched", blacksea = "BlackSea", arctic = "ArcticTraits",
  cefas = "Cefas", coral = "CoralTraits"
)

#' Source label for a code read from the offline DB (F20)
#'
#' @param primary_source The row's primary_source.
#' @return The matching label ("biotic" -> "BIOTIC"), or "OfflineDB" for an
#'   unknown or missing value.
offline_source_label <- function(primary_source) {
  key <- tolower(as.character(primary_source %||% NA_character_)[1])
  if (is.na(key) || !key %in% names(OFFLINE_SOURCE_LABELS)) return("OfflineDB")
  OFFLINE_SOURCE_LABELS[[key]]
}

#' Method for a code read from the offline DB (F20)
#'
#' Ontology rows were harmonised from fuzzy profiles ("rule"); every other
#' source wrote trait-database values ("observed").
#'
#' @param primary_source The row's primary_source.
#' @return "rule" or "observed".
offline_trait_method <- function(primary_source) {
  if (identical(offline_source_label(primary_source), "Ontology")) "rule" else "observed"
}

#' Confidence of one trait read from the offline DB (F20)
#'
#' The stored value, unless it is missing or 0.0 (the schema default, i.e.
#' "unknown"); then the weight of the row's source.
#'
#' @param offline One-row data frame from lookup_offline_traits().
#' @param trait Trait column, e.g. "PR".
#' @return Numeric in (0, 1].
offline_trait_confidence <- function(offline, trait) {
  col <- paste0(trait, "_confidence")
  stored <- if (col %in% names(offline)) suppressWarnings(as.numeric(offline[[col]][1])) else NA_real_
  if (isTRUE(stored > 0)) return(stored)
  get_database_weight(offline_source_label(offline$primary_source))
}

#' Mark ML- and phylo-filled codes with their method
#'
#' @param result One-row lookup result.
#' @return `result` with `T_method` "ml" where `T_source` is "ML" and "phylo"
#'   where it is "Phylogenetic".
mark_imputed_methods <- function(result) {
  for (trait in TRAIT_COLUMNS) {
    src <- result[[paste0(trait, "_source")]]
    if (length(src) != 1L || is.na(src) || is.na(result[[trait]])) next
    if (identical(src, "ML")) result[[paste0(trait, "_method")]] <- "ml"
    if (identical(src, "Phylogenetic")) result[[paste0(trait, "_method")]] <- "phylo"
  }
  result
}

#' The row's imputation_method (spec C3.1)
#'
#' @param result One-row lookup result with `T` / `T_method` columns.
#' @return "observed" when every non-NA code is observed or rule-based,
#'   otherwise the sorted other methods joined with "+", e.g. "ml+phylo".
aggregate_imputation_method <- function(result) {
  methods <- character()
  for (trait in TRAIT_COLUMNS) {
    value <- result[[trait]]
    if (length(value) != 1L || is.na(value)) next
    m <- result[[paste0(trait, "_method")]]
    if (length(m) == 1L && !is.na(m)) methods <- c(methods, m)
  }
  other <- sort(unique(setdiff(methods, RELATIVE_METHODS)))
  if (length(other) == 0) "observed" else paste(other, collapse = "+")
}

#' Overall confidence and its label (spec C3.2)
#'
#' @param result One-row lookup result with `T_confidence` columns.
#' @return list(value, label): the geometric mean of the non-NA MS..PR
#'   confidences and confidence_to_label() of it; list(NA, "none") when there
#'   is none or the mean is not finite.
overall_trait_confidence <- function(result) {
  vals <- vapply(CORE_TRAIT_COLUMNS, function(t) {
    v <- result[[paste0(t, "_confidence")]]
    if (length(v) == 1L) suppressWarnings(as.numeric(v)) else NA_real_
  }, numeric(1))
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(list(value = NA_real_, label = "none"))
  overall <- suppressWarnings(exp(mean(log(vals))))
  if (!isTRUE(is.finite(overall))) return(list(value = NA_real_, label = "none"))
  list(value = overall, label = confidence_to_label(overall))
}

#' The cache envelope's `harmonized` block (spec C3.7)
#'
#' Phylogenetic imputation and ML training read it, so every trait carries
#' its code, source, method and confidence, together with the taxonomy, the
#' vocabulary version and whether the lookup was degraded.
#'
#' @param result One-row lookup result.
#' @param taxonomy WoRMS taxonomy list (phylum ... genus), or NULL.
#' @param degraded TRUE when a database failed during the lookup.
#' @return Named list.
build_harmonized_block <- function(result, taxonomy = NULL, degraded = FALSE) {
  h <- list(species = as.character(result$species[1]))
  for (trait in TRAIT_COLUMNS) {
    for (suffix in c("", "_source", "_method", "_confidence")) {
      col <- paste0(trait, suffix)
      h[[col]] <- if (col %in% names(result)) result[[col]][1] else NA
    }
  }
  for (rank in c("phylum", "class", "order", "family", "genus")) {
    h[[rank]] <- if (is.null(taxonomy)) NA_character_ else .scalar_chr(taxonomy[[rank]])
  }
  # ML and uncertainty metadata, kept for diagnostics
  for (trait in CORE_TRAIT_COLUMNS) {
    for (suffix in c("_ml_confidence", "_ml_probability", "_interval_lower", "_interval_upper",
                     "_confidence_category")) {
      col <- paste0(trait, suffix)
      if (col %in% names(result)) h[[col]] <- result[[col]][1]
    }
  }
  if ("overall_confidence" %in% names(result)) h$overall_confidence <- result$overall_confidence[1]
  h$trait_vocab_version <- current_trait_vocab_version()
  h$degraded <- isTRUE(degraded)
  h
}

#' A trait cache envelope (spec C3.7)
#'
#' A degraded lookup (a database failed, or WoRMS gave no classification) is
#' cached with `ttl_days = 1`, so it is retried the next day instead of
#' serving the partial result for 30 days; read_cache_field() honours it.
#'
#' @param result One-row lookup result.
#' @param harmonized build_harmonized_block() output.
#' @param config_hash harm_config_hash() of the settings the codes were made with.
#' @param degraded TRUE for a degraded lookup.
#' @param extra Named list of further fields (raw ontology traits, taxonomy).
#' @return The envelope list for saveRDS().
build_trait_cache_envelope <- function(result, harmonized, config_hash, degraded = FALSE, extra = list()) {
  envelope <- c(list(
    traits = result,
    harmonized = harmonized,
    species = as.character(result$species[1]),
    timestamp = Sys.time(),
    config_hash = config_hash,
    trait_vocab_version = current_trait_vocab_version(),
    degraded = isTRUE(degraded)
  ), extra)
  if (isTRUE(degraded)) envelope$ttl_days <- 1
  envelope
}
