calculate_mti <- function(net, info) {
  tryCatch({
    # Input validation using utility functions
    validate_network(net, require_directed = FALSE, min_vertices = 1)
    validate_dataframe(info, required_cols = "meanB")

    if (nrow(info) != vcount(net)) {
      stop(sprintf("Number of rows in 'info' (%d) must match number of vertices in 'net' (%d)",
                   nrow(info), vcount(net)), call. = FALSE)
    }
    n <- vcount(net)
    adj_matrix <- as_adjacency_matrix(net, sparse = FALSE)

    # Create Diet Composition (DC) matrix
    # DC[i,j] = proportion of predator i's diet that is prey j
    # Rows = predators, Columns = prey

    # Calculate row sums (total consumption per predator)
    row_sums <- rowSums(adj_matrix)

    # Vectorized row normalization (avoid loop for 3-5x speedup)
    # Use sweep() to divide each row by its sum, with safe divisor for zero rows
    safe_divisor <- ifelse(row_sums > 0, row_sums, 1)
    DC <- sweep(adj_matrix, 1, safe_divisor, "/")
    DC[row_sums == 0, ] <- 0  # Zero out rows with no consumption

    rownames(DC) <- colnames(DC) <- V(net)$name

    # Create identity matrix
    I <- diag(n)

    # Calculate (I - DC)^(-1)
    # This represents direct and indirect effects through the food web
    I_minus_DC <- I - DC

    # Check if matrix is invertible
    if (abs(det(I_minus_DC)) < 1e-10) {
      warning("Diet composition matrix is singular or near-singular. MTI calculation may be unstable.")
      # Use pseudo-inverse
      I_minus_DC_inv <- MASS::ginv(I_minus_DC)
    } else {
      I_minus_DC_inv <- solve(I_minus_DC)
    }

    # Calculate MTI matrix
    # MTI = - (I - DC)^(-1) * DC
    MTI <- -I_minus_DC_inv %*% DC

    # Set diagonal to 0 (species doesn't impact itself in this analysis)
    diag(MTI) <- 0

    rownames(MTI) <- colnames(MTI) <- V(net)$name

    return(MTI)

  }, error = function(e) {
    stop(sprintf("Failed to calculate Mixed Trophic Impact (MTI): %s", e$message), call. = FALSE)
  })
}

#' Calculate Keystoneness Index
#'
#' Computes the keystoneness index for each species based on their
#' overall impact on the ecosystem and their relative biomass.
#'
#' @param net An igraph object representing the food web
#' @param info Data frame with 'meanB' column for biomass
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{species}{Species name}
#'   \item{overall_effect}{Total impact on the ecosystem (sum of absolute MTI values)}
#'   \item{relative_biomass}{Biomass relative to total ecosystem biomass}
#'   \item{keystoneness}{Keystoneness index (high values = keystone species)}
#'   \item{keystone_status}{Classification: "Keystone", "Dominant", or "Rare"}
#' }
#'
#' @details
#' The keystoneness index (KS) is calculated as:
#' KS_i = log(1 + OE_i) / log(1 + RB_i)
#'
#' Where:
#' - OE_i = Overall Effect of species i (sum of absolute MTI values)
#' - RB_i = Relative Biomass of species i (as proportion of total biomass)
#'
#' High keystoneness values indicate species with large ecosystem impacts
#' relative to their biomass (classic keystone species).
#'
#' Classification:
#' - Keystone: High impact, low biomass (KS > 1, RB < 0.05)
#' - Dominant: High impact, high biomass (KS > 0, RB >= 0.05)
#' - Rare: Low impact, low biomass (KS <= 1, RB < 0.05)
#'
#' @references
#' Libralato, S., et al. (2006). A method for identifying keystone species in
#' food web models. Ecological Modelling, 195(3-4), 153-171.
calculate_keystoneness <- function(net, info) {
  tryCatch({
    # Input validation using utility functions
    validate_network(net, require_directed = FALSE, min_vertices = 1)
    validate_dataframe(info, required_cols = "meanB")

    if (nrow(info) != vcount(net)) {
      stop(sprintf("Number of rows in 'info' (%d) must match number of vertices in 'net' (%d)",
                   nrow(info), vcount(net)), call. = FALSE)
    }

    # Calculate MTI matrix
    MTI <- calculate_mti(net, info)

    # Calculate overall effect (sum of absolute MTI values for each impactor)
    # This represents the total impact a species has on the ecosystem
    overall_effect <- colSums(abs(MTI))

    # Calculate relative biomass
    total_biomass <- sum(info$meanB, na.rm = TRUE)

    if (total_biomass <= 0) {
      stop("Total biomass must be positive to calculate keystoneness", call. = FALSE)
    }

    relative_biomass <- info$meanB / total_biomass

    # Calculate keystoneness index
    # KS = log(1 + overall_effect) / log(1 + relative_biomass)
    # High KS means high impact relative to biomass
    keystoneness <- log(1 + overall_effect) / log(1 + relative_biomass)

    # Handle infinite or undefined values
    keystoneness[is.infinite(keystoneness)] <- NA
    keystoneness[is.nan(keystoneness)] <- NA

    # Classify species
    keystone_status <- sapply(1:length(keystoneness), function(i) {
      if (is.na(keystoneness[i])) return("Undefined")
      if (keystoneness[i] > 1 && relative_biomass[i] < 0.05) return("Keystone")
      if (keystoneness[i] > 0 && relative_biomass[i] >= 0.05) return("Dominant")
      return("Rare")
    })

    # Create results data frame
    results <- data.frame(
      species = V(net)$name,
      overall_effect = overall_effect,
      relative_biomass = relative_biomass,
      keystoneness = keystoneness,
      keystone_status = keystone_status,
      stringsAsFactors = FALSE
    )

    # Sort by keystoneness (descending)
    results <- results[order(-results$keystoneness), ]

    return(results)

  }, error = function(e) {
    stop(sprintf("Failed to calculate keystoneness indices: %s", e$message), call. = FALSE)
  })
}

# ============================================================================
# METAWEB MANAGEMENT FUNCTIONS (MARBEFES WP3.2 Phase 2)
# ============================================================================

#' Create a metaweb object
#'
#' A metaweb contains all documented species and trophic interactions in a region,
#' serving as the basis for extracting local food webs. This implements the MARBEFES
#' guidance for regional metaweb assembly (Phase 2).
#'
#' @param species Data frame with species information (species_id, species_name, functional_group, traits)
#' @param interactions Data frame with trophic links (predator_id, prey_id, quality_code, source)
#' @param metadata List with metaweb metadata (region, time_period, authors, citation, etc.)
#' @return Object of class 'metaweb'
#' @export
#'
#' @details
#' Link quality codes (MARBEFES guidance):
#' \itemize{
#'   \item 1 = Documented in peer-reviewed literature for these species
#'   \item 2 = Documented for similar species or different region  
#'   \item 3 = Inferred from traits or body size relationships
#'   \item 4 = Expert opinion, not validated
#' }
#'
#' @examples
#' species <- data.frame(
#'   species_id = c("SP001", "SP002", "SP003"),
#'   species_name = c("Gadus morhua", "Clupea harengus", "Calanus finmarchicus"),
#'   functional_group = c("Fish", "Fish", "Zooplankton"),
#'   stringsAsFactors = FALSE
#' )
#' interactions <- data.frame(
#'   predator_id = c("SP001", "SP001"),
#'   prey_id = c("SP002", "SP003"),
#'   quality_code = c(1, 1),
#'   source = c("doi:10.1111/xxx", "doi:10.1111/yyy"),
#'   stringsAsFactors = FALSE
#' )
#' metadata <- list(
#'   region = "Baltic Sea",
#'   time_period = "1979-2016",
#'   citation = "Kortsch et al. 2021"
#' )
#' metaweb <- create_metaweb(species, interactions, metadata)
#'
#' @references
#' MARBEFES WP3.2 Guidelines for assessing seascape ecosystem organisation
#' and function - Ecological Interaction Networks (Draft v2, 2024)
