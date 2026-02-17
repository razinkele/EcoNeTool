# Trait-Based Food Web Construction
# Constructs food webs using trait-matching probabilities
# Based on size class, foraging strategy, mobility, environmental position, and protection

# ============================================================================
# PROBABILITY MATRICES
# ============================================================================

#' Size class × Size class interaction probabilities
#' Consumer size (rows) × Resource size (columns)
#' Only MS3-MS6 are predators; MS7 (XL) excluded as prey
MS_MS <- matrix(
  c(
    0.95, 0.80, 0.50, 0.05, 0.05, 0.05,  # MS3 (SM consumer)
    0.80, 0.95, 0.80, 0.50, 0.05, 0.05,  # MS4 (M consumer)
    0.50, 0.80, 0.95, 0.80, 0.50, 0.05,  # MS5 (ML consumer)
    0.20, 0.50, 0.80, 0.95, 0.80, 0.50   # MS6 (L consumer)
  ),
  nrow = 4, byrow = TRUE,
  dimnames = list(
    Consumer_MS = c("MS3", "MS4", "MS5", "MS6"),
    Resource_MS = c("MS1", "MS2", "MS3", "MS4", "MS5", "MS6")
  )
)

#' Foraging strategy × Prey size interaction probabilities
#' Consumer foraging strategy (rows) × Resource size (columns)
#' FS0 (None) and FS3 (Parasite) excluded
FS_MS <- matrix(
  c(
    0.20, 0.50, 0.95, 0.80, 0.50, 0.20,  # FS1 Predator
    0.05, 0.20, 0.50, 0.95, 0.80, 0.50,  # FS2 Scavenger
    0.95, 0.80, 0.05, 0.05, 0.05, 0.05,  # FS4 Grazer
    0.80, 0.80, 0.50, 0.05, 0.05, 0.05,  # FS5 Deposit feeder
    0.95, 0.95, 0.20, 0.05, 0.05, 0.05   # FS6 Filter feeder
  ),
  nrow = 5, byrow = TRUE,
  dimnames = list(
    Consumer_FS = c("FS1", "FS2", "FS4", "FS5", "FS6"),
    Resource_MS = c("MS1", "MS2", "MS3", "MS4", "MS5", "MS6")
  )
)

#' Mobility × Mobility interaction probabilities
#' Consumer mobility (rows) × Resource mobility (columns)
MB_MB <- matrix(
  c(
    0.95, 0.05, 0.05, 0.05, 0.05,  # MB1 Sessile
    0.80, 0.95, 0.20, 0.20, 0.20,  # MB2 Passive floater
    0.80, 0.95, 0.95, 0.20, 0.20,  # MB3 Crawler-burrower
    0.80, 0.80, 0.80, 0.80, 0.80,  # MB4 Facultative swimmer
    0.20, 0.50, 0.95, 0.95, 0.95   # MB5 Obligate swimmer
  ),
  nrow = 5, byrow = TRUE,
  dimnames = list(
    Consumer_MB = c("MB1", "MB2", "MB3", "MB4", "MB5"),
    Resource_MB = c("MB1", "MB2", "MB3", "MB4", "MB5")
  )
)

#' Environmental position × Prey size interaction probabilities
#' Consumer environmental position (rows) × Resource size (columns)
EP_MS <- matrix(
  c(
    0.05, 0.50, 0.80, 0.05,  # EP1 Infaunal
    0.50, 0.80, 0.95, 0.80,  # EP2 Epibenthic
    0.80, 0.95, 0.80, 0.50,  # EP3 Benthopelagic
    0.95, 0.80, 0.50, 0.05   # EP4 Pelagic
  ),
  nrow = 4, byrow = TRUE,
  dimnames = list(
    Consumer_EP = c("EP1", "EP2", "EP3", "EP4"),
    Resource_MS = c("MS2", "MS3", "MS4", "MS5")
  )
)

#' Protection × Prey size interaction probabilities
#' Resource protection (rows) × Resource size (columns)
#' Note: This affects resource vulnerability
PR_MS <- matrix(
  c(
    0.95, 0.95, 0.80, 0.50,  # PR0 None
    0.80, 0.80, 0.50, 0.05,  # PR2 Tube
    0.80, 0.80, 0.50, 0.05,  # PR3 Burrow
    0.50, 0.50, 0.80, 0.50,  # PR6 Hard shell
    0.20, 0.20, 0.50, 0.20,  # PR7 Few spines
    0.20, 0.20, 0.50, 0.20   # PR8 Armoured
  ),
  nrow = 6, byrow = TRUE,
  dimnames = list(
    Resource_PR = c("PR0", "PR2", "PR3", "PR6", "PR7", "PR8"),
    Resource_MS = c("MS2", "MS3", "MS4", "MS5")
  )
)

# ============================================================================
# TRAIT DEFINITIONS
# ============================================================================

#' Trait value definitions for reference
TRAIT_DEFINITIONS <- list(
  MS = c(
    MS1 = "XS (Extra Small)",
    MS2 = "S (Small)",
    MS3 = "SM (Small-Medium)",
    MS4 = "M (Medium)",
    MS5 = "ML (Medium-Large)",
    MS6 = "L (Large)",
    MS7 = "XL (Extra Large - rarely prey)"
  ),
  FS = c(
    FS0 = "None",
    FS1 = "Predator",
    FS2 = "Scavenger",
    FS3 = "Parasite (excluded)",
    FS4 = "Grazer",
    FS5 = "Deposit feeder",
    FS6 = "Filter feeder"
  ),
  MB = c(
    MB1 = "Sessile",
    MB2 = "Passive floater",
    MB3 = "Crawler-burrower",
    MB4 = "Facultative swimmer",
    MB5 = "Obligate swimmer"
  ),
  EP = c(
    EP1 = "Infaunal",
    EP2 = "Epibenthic",
    EP3 = "Benthopelagic",
    EP4 = "Pelagic"
  ),
  PR = c(
    PR0 = "None",
    PR2 = "Tube",
    PR3 = "Burrow",
    PR5 = "Soft shell",
    PR6 = "Hard shell",
    PR7 = "Few spines",
    PR8 = "Armoured"
  )
)

# ============================================================================
# CORE FUNCTIONS
# ============================================================================

#' Calculate interaction probability between two species
#'
#' @param consumer_traits Named vector of consumer traits (MS, FS, MB, EP)
#' @param resource_traits Named vector of resource traits (MS, MB, EP, PR)
#' @return Numeric probability [0, 1] or NA if interaction impossible
#' @examples
#' consumer <- c(MS = "MS4", FS = "FS1", MB = "MB5", EP = "EP4")
#' resource <- c(MS = "MS2", MB = "MB2", EP = "EP4", PR = "PR0")
#' calc_interaction_probability(consumer, resource)
calc_interaction_probability <- function(consumer_traits, resource_traits) {

  # Validate required traits
  required_consumer <- c("MS", "FS", "MB", "EP")
  required_resource <- c("MS", "MB", "EP", "PR")

  if (!all(required_consumer %in% names(consumer_traits))) {
    stop("Consumer must have traits: MS, FS, MB, EP")
  }
  if (!all(required_resource %in% names(resource_traits))) {
    stop("Resource must have traits: MS, MB, EP, PR")
  }

  # Extract traits
  c_MS <- consumer_traits["MS"]
  c_FS <- consumer_traits["FS"]
  c_MB <- consumer_traits["MB"]
  c_EP <- consumer_traits["EP"]

  r_MS <- resource_traits["MS"]
  r_MB <- resource_traits["MB"]
  r_EP <- resource_traits["EP"]
  r_PR <- resource_traits["PR"]

  # Exclusion rules
  # 1. MS7 (XL) excluded as prey
  if (r_MS == "MS7") return(0)

  # 2. Only MS3-MS6 can be predators (have size-based predation)
  if (!c_MS %in% c("MS3", "MS4", "MS5", "MS6")) return(0)

  # 3. FS0 (None) and FS3 (Parasite) excluded as consumers
  if (c_FS %in% c("FS0", "FS3")) return(0)

  # 4. Resource cannot be larger than consumer for most strategies
  # (This is implicit in the MS_MS matrix structure)

  # Initialize probability vector
  probs <- numeric()

  # 1. Size-based predation (MS × MS)
  if (c_MS %in% rownames(MS_MS) && r_MS %in% colnames(MS_MS)) {
    probs <- c(probs, MS_MS[c_MS, r_MS])
  } else {
    return(0)  # Invalid size combination
  }

  # 2. Foraging strategy × prey size (FS × MS)
  if (c_FS %in% rownames(FS_MS) && r_MS %in% colnames(FS_MS)) {
    probs <- c(probs, FS_MS[c_FS, r_MS])
  } else {
    return(0)  # Invalid foraging strategy
  }

  # 3. Mobility matching (MB × MB)
  if (c_MB %in% rownames(MB_MB) && r_MB %in% colnames(MB_MB)) {
    probs <- c(probs, MB_MB[c_MB, r_MB])
  } else {
    return(0)  # Invalid mobility combination
  }

  # 4. Environmental position × prey size (EP × MS)
  if (c_EP %in% rownames(EP_MS) && r_MS %in% colnames(EP_MS)) {
    probs <- c(probs, EP_MS[c_EP, r_MS])
  } else {
    # EP_MS only covers MS2-MS5, so MS1 and MS6 need special handling
    if (r_MS %in% c("MS1", "MS6")) {
      # Use lower probability for extreme sizes
      probs <- c(probs, 0.05)
    } else {
      return(0)
    }
  }

  # 5. Protection × prey size (PR × MS)
  if (r_PR %in% rownames(PR_MS) && r_MS %in% colnames(PR_MS)) {
    probs <- c(probs, PR_MS[r_PR, r_MS])
  } else {
    # PR_MS only covers MS2-MS5
    if (r_MS %in% c("MS1", "MS6")) {
      # Use default probability
      probs <- c(probs, 0.50)
    } else if (!r_PR %in% rownames(PR_MS)) {
      # PR5 (soft shell) not in matrix - treat as moderate protection
      probs <- c(probs, 0.50)
    } else {
      return(0)
    }
  }

  # Aggregation rule: minimum of all probabilities
  return(min(probs))
}


#' Construct food web from species trait data
#'
#' @param species_data Data frame with columns: species, MS, FS, MB, EP, PR
#' @param threshold Minimum probability threshold for link (default 0.05)
#' @param return_probs If TRUE, return probability matrix instead of binary adjacency
#' @return Adjacency matrix (rows = consumers, columns = resources)
#' @export
construct_trait_foodweb <- function(species_data, threshold = 0.05, return_probs = FALSE) {

  # Validate input
  required_cols <- c("species", "MS", "FS", "MB", "EP", "PR")
  if (!all(required_cols %in% colnames(species_data))) {
    stop(paste("species_data must contain columns:", paste(required_cols, collapse = ", ")))
  }

  n_species <- nrow(species_data)
  species_names <- species_data$species

  # Initialize probability matrix
  prob_matrix <- matrix(0, nrow = n_species, ncol = n_species,
                        dimnames = list(Consumer = species_names, Resource = species_names))

  # Calculate all pairwise probabilities
  for (i in 1:n_species) {
    consumer_traits <- c(
      MS = species_data$MS[i],
      FS = species_data$FS[i],
      MB = species_data$MB[i],
      EP = species_data$EP[i]
    )

    for (j in 1:n_species) {
      # Skip self-loops
      if (i == j) next

      resource_traits <- c(
        MS = species_data$MS[j],
        MB = species_data$MB[j],
        EP = species_data$EP[j],
        PR = species_data$PR[j]
      )

      # Calculate probability
      prob <- tryCatch(
        calc_interaction_probability(consumer_traits, resource_traits),
        error = function(e) 0
      )

      prob_matrix[i, j] <- prob
    }
  }

  # Return probabilities or binary adjacency matrix
  if (return_probs) {
    return(prob_matrix)
  } else {
    # Apply threshold
    adjacency <- ifelse(prob_matrix >= threshold, 1, 0)
    return(adjacency)
  }
}


#' Convert trait-based food web to igraph object
#'
#' @param species_data Data frame with trait information
#' @param threshold Minimum probability threshold (default 0.05)
#' @param include_probs Include edge weights as probabilities
#' @return igraph object
#' @export
trait_foodweb_to_igraph <- function(species_data, threshold = 0.05, include_probs = TRUE) {

  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for this function")
  }

  # Get probability matrix
  prob_matrix <- construct_trait_foodweb(species_data, threshold = 0, return_probs = TRUE)

  # Create edge list from probabilities above threshold
  edges <- which(prob_matrix >= threshold, arr.ind = TRUE)
  edge_list <- data.frame(
    from = rownames(prob_matrix)[edges[, 1]],
    to = colnames(prob_matrix)[edges[, 2]],
    probability = prob_matrix[edges]
  )

  # Create igraph
  if (include_probs) {
    g <- igraph::graph_from_data_frame(edge_list, directed = TRUE,
                                       vertices = species_data)
  } else {
    g <- igraph::graph_from_data_frame(edge_list[, 1:2], directed = TRUE,
                                       vertices = species_data)
  }

  return(g)
}


#' Validate trait codes in species data
#'
#' @param species_data Data frame with trait columns
#' @return List with valid (TRUE/FALSE) and messages (character vector)
#' @export
validate_trait_data <- function(species_data) {

  messages <- character()
  valid <- TRUE

  # Check required columns
  required_cols <- c("species", "MS", "FS", "MB", "EP", "PR")
  missing_cols <- setdiff(required_cols, colnames(species_data))
  if (length(missing_cols) > 0) {
    messages <- c(messages, paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    valid <- FALSE
    return(list(valid = valid, messages = messages))
  }

  # Check for duplicated species names
  if (any(duplicated(species_data$species))) {
    dup_species <- species_data$species[duplicated(species_data$species)]
    messages <- c(messages, paste("Duplicated species:", paste(unique(dup_species), collapse = ", ")))
    valid <- FALSE
  }

  # Validate trait codes
  valid_MS <- paste0("MS", 1:7)
  valid_FS <- paste0("FS", c(0:6))
  valid_MB <- paste0("MB", 1:5)
  valid_EP <- paste0("EP", 1:4)
  valid_PR <- paste0("PR", c(0, 2, 3, 5, 6, 7, 8))

  # Check MS
  invalid_MS <- !species_data$MS %in% valid_MS & !is.na(species_data$MS)
  if (any(invalid_MS, na.rm = TRUE)) {
    messages <- c(messages, sprintf("Invalid MS codes in rows: %s",
                                   paste(which(invalid_MS), collapse = ", ")))
    valid <- FALSE
  }

  # Check FS
  invalid_FS <- !species_data$FS %in% valid_FS & !is.na(species_data$FS)
  if (any(invalid_FS, na.rm = TRUE)) {
    messages <- c(messages, sprintf("Invalid FS codes in rows: %s",
                                   paste(which(invalid_FS), collapse = ", ")))
    valid <- FALSE
  }

  # Check MB
  invalid_MB <- !species_data$MB %in% valid_MB & !is.na(species_data$MB)
  if (any(invalid_MB, na.rm = TRUE)) {
    messages <- c(messages, sprintf("Invalid MB codes in rows: %s",
                                   paste(which(invalid_MB), collapse = ", ")))
    valid <- FALSE
  }

  # Check EP
  invalid_EP <- !species_data$EP %in% valid_EP & !is.na(species_data$EP)
  if (any(invalid_EP, na.rm = TRUE)) {
    messages <- c(messages, sprintf("Invalid EP codes in rows: %s",
                                   paste(which(invalid_EP), collapse = ", ")))
    valid <- FALSE
  }

  # Check PR
  invalid_PR <- !species_data$PR %in% valid_PR & !is.na(species_data$PR)
  if (any(invalid_PR, na.rm = TRUE)) {
    messages <- c(messages, sprintf("Invalid PR codes in rows: %s",
                                   paste(which(invalid_PR), collapse = ", ")))
    valid <- FALSE
  }

  # Warnings for excluded categories
  if (any(species_data$MS == "MS7", na.rm = TRUE)) {
    messages <- c(messages, "Warning: MS7 (XL) species will not appear as prey")
  }
  if (any(species_data$FS %in% c("FS0", "FS3"), na.rm = TRUE)) {
    messages <- c(messages, "Warning: FS0 (None) and FS3 (Parasite) will not appear as consumers")
  }

  # Success message
  if (valid && length(messages) == 0) {
    messages <- "All trait codes are valid"
  }

  return(list(valid = valid, messages = messages))
}


#' Create a template data frame for trait-based food web
#'
#' @param n_species Number of species to include in template
#' @return Data frame with example trait data
#' @export
create_trait_template <- function(n_species = 5) {

  example_species <- data.frame(
    species = paste0("Species_", 1:n_species),
    MS = sample(paste0("MS", 1:6), n_species, replace = TRUE),
    FS = sample(paste0("FS", c(1, 2, 4, 5, 6)), n_species, replace = TRUE),
    MB = sample(paste0("MB", 1:5), n_species, replace = TRUE),
    EP = sample(paste0("EP", 1:4), n_species, replace = TRUE),
    PR = sample(paste0("PR", c(0, 2, 3, 5, 6, 7, 8)), n_species, replace = TRUE),
    stringsAsFactors = FALSE
  )

  return(example_species)
}


#' Get trait descriptions
#'
#' @param trait_type Type of trait: "MS", "FS", "MB", "EP", or "PR"
#' @return Named character vector of trait descriptions
#' @export
get_trait_descriptions <- function(trait_type = NULL) {
  if (is.null(trait_type)) {
    return(TRAIT_DEFINITIONS)
  } else {
    if (trait_type %in% names(TRAIT_DEFINITIONS)) {
      return(TRAIT_DEFINITIONS[[trait_type]])
    } else {
      stop("trait_type must be one of: MS, FS, MB, EP, PR")
    }
  }
}
