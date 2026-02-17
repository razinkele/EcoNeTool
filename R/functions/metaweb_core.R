create_metaweb <- function(species, interactions, metadata = list()) {
  # Validate inputs
  if (!is.data.frame(species)) stop("species must be a data.frame")
  if (!is.data.frame(interactions)) stop("interactions must be a data.frame")
  
  # Check required columns
  required_species_cols <- c("species_id", "species_name")
  required_interaction_cols <- c("predator_id", "prey_id")
  
  missing_species <- setdiff(required_species_cols, colnames(species))
  if (length(missing_species) > 0) {
    stop("Missing required species columns: ", paste(missing_species, collapse = ", "))
  }
  
  missing_interaction <- setdiff(required_interaction_cols, colnames(interactions))
  if (length(missing_interaction) > 0) {
    stop("Missing required interaction columns: ", paste(missing_interaction, collapse = ", "))
  }
  
  # Add quality_code if missing (default to 1)
  if (!"quality_code" %in% colnames(interactions)) {
    interactions$quality_code <- 1
  }
  
  # Add source if missing
  if (!"source" %in% colnames(interactions)) {
    interactions$source <- "unspecified"
  }
  
  # Validate quality codes (1-4)
  if (any(interactions$quality_code < 1 | interactions$quality_code > 4, na.rm = TRUE)) {
    stop("quality_code must be between 1 and 4")
  }
  
  # Validate that all species in interactions exist in species list
  all_species_in_interactions <- unique(c(interactions$predator_id, interactions$prey_id))
  unknown_species <- setdiff(all_species_in_interactions, species$species_id)
  if (length(unknown_species) > 0) {
    warning("Unknown species in interactions: ", paste(unknown_species, collapse = ", "))
  }
  
  # Create metaweb object
  metaweb <- list(
    species = species,
    interactions = interactions,
    metadata = metadata,
    created = Sys.time(),
    version = "1.0"
  )
  
  class(metaweb) <- "metaweb"
  return(metaweb)
}

#' Validate a metaweb object
#'
#' Checks metaweb structure and identifies potential issues like orphan links.
#'
#' @param metaweb Metaweb object to validate
#' @return TRUE if valid, otherwise throws error or warnings
#' @export
validate_metaweb <- function(metaweb) {
  if (!inherits(metaweb, "metaweb")) {
    stop("Object is not of class 'metaweb'")
  }
  
  # Check structure
  required_elements <- c("species", "interactions", "metadata")
  missing <- setdiff(required_elements, names(metaweb))
  if (length(missing) > 0) {
    stop("Missing metaweb elements: ", paste(missing, collapse = ", "))
  }
  
  # Check for orphan links
  all_species <- metaweb$species$species_id
  predators <- unique(metaweb$interactions$predator_id)
  prey <- unique(metaweb$interactions$prey_id)
  
  orphan_predators <- setdiff(predators, all_species)
  orphan_prey <- setdiff(prey, all_species)
  
  if (length(orphan_predators) > 0) {
    warning("Orphan predators (not in species list): ", 
            paste(orphan_predators, collapse = ", "))
  }
  
  if (length(orphan_prey) > 0) {
    warning("Orphan prey (not in species list): ", 
            paste(orphan_prey, collapse = ", "))
  }
  
  return(TRUE)
}

# NOTE: %||% operator is now defined in validation_utils.R

#' Print metaweb summary
#'
#' @param x Metaweb object
#' @param ... Additional arguments (ignored)
#' @export
print.metaweb <- function(x, ...) {
  cat("Metaweb object\n")
  cat("==============\n")
  cat("Region:", x$metadata$region %||% "Not specified", "\n")
  cat("Time period:", x$metadata$time_period %||% "Not specified", "\n")
  cat("Species:", nrow(x$species), "\n")
  cat("Interactions:", nrow(x$interactions), "\n")
  
  # Link quality summary
  if ("quality_code" %in% colnames(x$interactions)) {
    cat("\nLink quality distribution:\n")
    quality_table <- table(x$interactions$quality_code)
    quality_labels <- c("1 (documented)", "2 (similar sp.)", "3 (inferred)", "4 (expert)")
    for (i in 1:4) {
      count <- quality_table[as.character(i)]
      if (is.na(count)) count <- 0
      cat(sprintf("  %s: %d\n", quality_labels[i], count))
    }
  }
  
  cat("\nCreated:", format(x$created, "%Y-%m-%d %H:%M:%S"), "\n")
  
  invisible(x)
}

#' Convert metaweb to igraph network
#'
#' @param metaweb Metaweb object
#' @return igraph object
#' @export
metaweb_to_igraph <- function(metaweb) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required")
  }
  
  # Create edge list
  edges <- metaweb$interactions[, c("predator_id", "prey_id")]
  
  # Create igraph object
  g <- igraph::graph_from_data_frame(
    d = edges,
    directed = TRUE,
    vertices = metaweb$species
  )
  
  # Add edge attributes
  if ("quality_code" %in% colnames(metaweb$interactions)) {
    igraph::E(g)$quality_code <- metaweb$interactions$quality_code
  }
  if ("source" %in% colnames(metaweb$interactions)) {
    igraph::E(g)$source <- metaweb$interactions$source
  }
  
  return(g)
}

#' Get link quality description
#'
#' @param quality_code Integer 1-4
#' @return Character description
#' @export
get_link_quality_description <- function(quality_code) {
  descriptions <- c(
    "1" = "Documented in literature for these species",
    "2" = "Documented for similar species/region",
    "3" = "Inferred from traits",
    "4" = "Expert opinion (not validated)"
  )
  return(descriptions[as.character(quality_code)])
}

#' Summarize link quality in metaweb
#'
#' @param metaweb Metaweb object
#' @return Data frame with quality summary
#' @export
summarize_link_quality <- function(metaweb) {
  quality_summary <- data.frame(
    quality_code = 1:4,
    description = sapply(1:4, get_link_quality_description),
    count = 0,
    percentage = 0
  )
  
  if ("quality_code" %in% colnames(metaweb$interactions)) {
    quality_table <- table(metaweb$interactions$quality_code)
    for (i in 1:4) {
      count <- quality_table[as.character(i)]
      if (!is.na(count)) {
        quality_summary$count[i] <- count
      }
    }
    quality_summary$percentage <- round(100 * quality_summary$count / sum(quality_summary$count), 1)
  }
  
  return(quality_summary)
}

#' Add species to metaweb
#'
#' @param metaweb Metaweb object
#' @param species_id Unique species identifier
#' @param species_name Species scientific name
#' @param functional_group Functional group (optional)
#' @param traits Named list of additional traits (optional)
#' @return Updated metaweb object
#' @export
add_species_to_metaweb <- function(metaweb, species_id, species_name, 
                                   functional_group = NA, traits = list()) {
  # Check if species already exists
  if (species_id %in% metaweb$species$species_id) {
    stop("Species ", species_id, " already exists in metaweb")
  }
  
  # Create new species row
  new_species <- data.frame(
    species_id = species_id,
    species_name = species_name,
    stringsAsFactors = FALSE
  )
  
  if (!is.na(functional_group)) {
    new_species$functional_group <- functional_group
  }
  
  # Add additional traits
  for (trait_name in names(traits)) {
    new_species[[trait_name]] <- traits[[trait_name]]
  }

  # Ensure all columns match in both directions
  # Add columns from metaweb$species that are missing in new_species
  missing_cols_in_new <- setdiff(colnames(metaweb$species), colnames(new_species))
  for (col in missing_cols_in_new) {
    new_species[[col]] <- NA
  }

  # Add columns from new_species that are missing in metaweb$species
  missing_cols_in_old <- setdiff(colnames(new_species), colnames(metaweb$species))
  for (col in missing_cols_in_old) {
    metaweb$species[[col]] <- NA
  }

  # Ensure column order matches
  new_species <- new_species[, colnames(metaweb$species), drop = FALSE]

  # Add to species list
  metaweb$species <- rbind(metaweb$species, new_species)
  
  return(metaweb)
}

#' Remove species from metaweb
#'
#' @param metaweb Metaweb object
#' @param species_id Species identifier to remove
#' @param remove_links Logical, remove associated links? (default TRUE)
#' @return Updated metaweb object
#' @export
remove_species_from_metaweb <- function(metaweb, species_id, remove_links = TRUE) {
  # Check if species exists
  if (!species_id %in% metaweb$species$species_id) {
    stop("Species ", species_id, " not found in metaweb")
  }
  
  # Remove species
  metaweb$species <- metaweb$species[metaweb$species$species_id != species_id, ]
  
  # Remove associated links if requested
  if (remove_links) {
    metaweb$interactions <- metaweb$interactions[
      metaweb$interactions$predator_id != species_id & 
        metaweb$interactions$prey_id != species_id,
    ]
  }
  
  return(metaweb)
}

#' Add trophic link to metaweb
#'
#' @param metaweb Metaweb object
#' @param predator_id Predator species identifier
#' @param prey_id Prey species identifier
#' @param quality_code Link quality (1-4)
#' @param source Citation or source of link
#' @param notes Additional notes (optional)
#' @return Updated metaweb object
#' @export
add_trophic_link <- function(metaweb, predator_id, prey_id, 
                             quality_code = 1, source = "user_added", notes = "") {
  # Validate species exist
  if (!predator_id %in% metaweb$species$species_id) {
    stop("Predator ", predator_id, " not found in species list")
  }
  if (!prey_id %in% metaweb$species$species_id) {
    stop("Prey ", prey_id, " not found in species list")
  }
  
  # Check if link already exists
  existing_link <- metaweb$interactions$predator_id == predator_id & 
                   metaweb$interactions$prey_id == prey_id
  if (any(existing_link)) {
    warning("Link from ", predator_id, " to ", prey_id, " already exists. Updating.")
    metaweb$interactions <- metaweb$interactions[!existing_link, ]
  }
  
  # Validate quality code
  if (quality_code < 1 || quality_code > 4) {
    stop("quality_code must be between 1 and 4")
  }
  
  # Create new link
  new_link <- data.frame(
    predator_id = predator_id,
    prey_id = prey_id,
    quality_code = quality_code,
    source = source,
    stringsAsFactors = FALSE
  )
  
  if (notes != "") {
    new_link$notes <- notes
  }
  
  # Ensure all columns match
  missing_cols <- setdiff(colnames(metaweb$interactions), colnames(new_link))
  for (col in missing_cols) {
    new_link[[col]] <- NA
  }
  
  # Add to interactions
  metaweb$interactions <- rbind(metaweb$interactions, new_link)
  
  return(metaweb)
}

#' Remove trophic link from metaweb
#'
#' @param metaweb Metaweb object
#' @param predator_id Predator species identifier
#' @param prey_id Prey species identifier
#' @return Updated metaweb object
#' @export
remove_trophic_link <- function(metaweb, predator_id, prey_id) {
  # Find and remove link
  link_exists <- metaweb$interactions$predator_id == predator_id & 
                 metaweb$interactions$prey_id == prey_id
  
  if (!any(link_exists)) {
    warning("Link from ", predator_id, " to ", prey_id, " not found")
    return(metaweb)
  }
  
  metaweb$interactions <- metaweb$interactions[!link_exists, ]
  
  return(metaweb)
}

#' Merge two metawebs
#'
#' Combines species and interactions from two metawebs. Useful for creating
#' regional metawebs from multiple sources.
#'
#' @param metaweb1 First metaweb object
#' @param metaweb2 Second metaweb object
#' @param conflict_resolution How to handle duplicate links: "keep_first", "keep_second",
#'                           "keep_higher_quality", "keep_both"
#' @return Merged metaweb object
#' @export
merge_metawebs <- function(metaweb1, metaweb2, conflict_resolution = "keep_higher_quality") {
  # Merge species lists (harmonize columns first)
  # Get all unique columns from both data frames
  all_species_cols <- unique(c(colnames(metaweb1$species), colnames(metaweb2$species)))

  # Add missing columns to each data frame with NA values
  for (col in setdiff(all_species_cols, colnames(metaweb1$species))) {
    metaweb1$species[[col]] <- NA
  }
  for (col in setdiff(all_species_cols, colnames(metaweb2$species))) {
    metaweb2$species[[col]] <- NA
  }

  # Ensure same column order
  metaweb1$species <- metaweb1$species[, all_species_cols, drop = FALSE]
  metaweb2$species <- metaweb2$species[, all_species_cols, drop = FALSE]

  # Now merge (remove duplicates by species_id)
  merged_species <- rbind(metaweb1$species, metaweb2$species)
  merged_species <- merged_species[!duplicated(merged_species$species_id), ]

  # Merge interactions (harmonize columns first)
  all_interaction_cols <- unique(c(colnames(metaweb1$interactions), colnames(metaweb2$interactions)))

  # Add missing columns
  for (col in setdiff(all_interaction_cols, colnames(metaweb1$interactions))) {
    metaweb1$interactions[[col]] <- NA
  }
  for (col in setdiff(all_interaction_cols, colnames(metaweb2$interactions))) {
    metaweb2$interactions[[col]] <- NA
  }

  # Ensure same column order
  metaweb1$interactions <- metaweb1$interactions[, all_interaction_cols, drop = FALSE]
  metaweb2$interactions <- metaweb2$interactions[, all_interaction_cols, drop = FALSE]

  # Now merge
  merged_interactions <- rbind(metaweb1$interactions, metaweb2$interactions)
  
  # Handle duplicate links based on conflict resolution
  duplicate_links <- duplicated(merged_interactions[, c("predator_id", "prey_id")]) |
                     duplicated(merged_interactions[, c("predator_id", "prey_id")], fromLast = TRUE)
  
  if (any(duplicate_links)) {
    if (conflict_resolution == "keep_first") {
      merged_interactions <- merged_interactions[!duplicated(merged_interactions[, c("predator_id", "prey_id")]), ]
    } else if (conflict_resolution == "keep_second") {
      merged_interactions <- merged_interactions[!duplicated(merged_interactions[, c("predator_id", "prey_id")], fromLast = TRUE), ]
    } else if (conflict_resolution == "keep_higher_quality") {
      # Keep link with lowest quality_code (1 is best)
      merged_interactions <- merged_interactions[order(merged_interactions$quality_code), ]
      merged_interactions <- merged_interactions[!duplicated(merged_interactions[, c("predator_id", "prey_id")]), ]
    } else if (conflict_resolution == "keep_both") {
      # Keep all links (don't remove duplicates)
    } else {
      stop("Invalid conflict_resolution option")
    }
  }
  
  # Merge metadata
  merged_metadata <- list(
    region = paste(metaweb1$metadata$region %||% "unknown",
                   metaweb2$metadata$region %||% "unknown", sep = " + "),
    merged_from = c(metaweb1$metadata$citation %||% "metaweb1",
                    metaweb2$metadata$citation %||% "metaweb2"),
    merged_date = Sys.time()
  )
  
  # Create merged metaweb
  merged_metaweb <- create_metaweb(merged_species, merged_interactions, merged_metadata)
  
  return(merged_metaweb)
}

#' Import metaweb from CSV files
#'
#' @param species_file Path to species CSV file
#' @param interactions_file Path to interactions CSV file
#' @param metadata List with metadata (optional)
#' @return Metaweb object
#' @export
