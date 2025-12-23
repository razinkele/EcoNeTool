get_topological_indicators <- function(net) {
  tryCatch({
    # Input validation using utility function
    validate_network(net, require_directed = FALSE, min_vertices = 1)

    S <- vcount(net)
    if (S <= 1) {
      warning("Network has only one or zero species. Metrics may be undefined.")
    }

    C <- ecount(net) / (S * (S - 1))
    pred <- degree(net, mode = "in") > 0
    G <- sum(degree(net, mode = "in")[pred]) / sum(pred)
    prey <- degree(net, mode = "out") > 0
    V <- sum(degree(net, mode = "out")[prey]) / sum(prey)
    sp <- distances(net)
    ShortPath <- mean(sp[upper.tri(sp)])
    tlnodes <- calculate_trophic_levels(net)
    TL <- mean(tlnodes)
    netmatrix <- as_adjacency_matrix(net, sparse = FALSE)
    webtl <- netmatrix * tlnodes
    webtl[webtl == 0] <- NA
    omninodes <- apply(webtl, 2, sd, na.rm = TRUE)
    Omni <- mean(omninodes, na.rm = TRUE)

    list(S = S, C = C, G = G, V = V, ShortPath = ShortPath, TL = TL, Omni = Omni)

  }, error = function(e) {
    stop(sprintf("Failed to calculate topological indicators: %s", e$message), call. = FALSE)
  })
}

#' Calculate node-weighted (quantitative) indicators for a food web
#'
#' Computes network metrics weighted by node biomass. These metrics account
#' for the relative importance of species based on their biomass.
#'
#' @param net An igraph object representing the food web
#' @param info Data frame containing species information with 'meanB' column for biomass
#'
#' @return A list containing:
#' \describe{
#'   \item{nwC}{Node-weighted connectance}
#'   \item{nwG}{Node-weighted generality}
#'   \item{nwV}{Node-weighted vulnerability}
#'   \item{nwTL}{Node-weighted mean trophic level}
#' }
#'
#' @details
#' Node-weighted metrics give more importance to high-biomass species.
#' - nwC = sum(degree * biomass) / (2 * sum(biomass) * (S-1))
#' - nwG = sum(in-degree * biomass for predators) / sum(predator biomass)
#' - nwV = sum(out-degree * biomass for prey) / sum(prey biomass)
#' - nwTL = sum(TL * biomass) / sum(biomass)
#'
#' @references
#' Olivier, P., et al. (2019). Exploring the temporal variability of a food web
#' using long-term biomonitoring data. Ecography, 42(11), 2107-2121.
get_node_weighted_indicators <- function(net, info) {
  tryCatch({
    # Input validation using utility functions
    validate_network(net, require_directed = FALSE, min_vertices = 1)
    validate_dataframe(info, required_cols = "meanB")

    if (nrow(info) != vcount(net)) {
      stop(sprintf("Number of rows in 'info' (%d) must match number of vertices in 'net' (%d)",
                   nrow(info), vcount(net)), call. = FALSE)
    }

    biomass <- info$meanB

    # Check for NA or negative biomass
    if (any(is.na(biomass))) {
      warning("NA values found in biomass, results may be unreliable")
    }
    if (any(biomass < 0, na.rm = TRUE)) {
      stop("Biomass values must be non-negative", call. = FALSE)
    }

    tlnodes <- calculate_trophic_levels(net)
    nwC <- sum(degree(net) * biomass) / (2 * sum(biomass) * (vcount(net) - 1))
    pred <- degree(net, mode = "in") > 0
    nwG <- sum((degree(net, mode = "in") * biomass)[pred]) / (sum(biomass[pred]))
    prey <- degree(net, mode = "out") > 0
    nwV <- sum((degree(net, mode = "out") * biomass)[prey]) / (sum(biomass[prey]))
    nwTL <- sum(tlnodes * biomass) / sum(biomass)

    list(nwC = nwC, nwG = nwG, nwV = nwV, nwTL = nwTL)

  }, error = function(e) {
    stop(sprintf("Failed to calculate node-weighted indicators: %s", e$message), call. = FALSE)
  })
}

# ============================================================================
# FLUX-BASED METRICS
# ============================================================================

#' Calculate link-weighted flux indicators
#'
#' Computes Shannon diversity-based indicators from an energy flux matrix.
#' These metrics account for the distribution of energy flows across trophic links.
#'
#' @param fluxes Numeric matrix of energy fluxes between species (from fluxing())
#' @param loop Logical, whether to include self-loops in connectance calculation
#'
#' @return A list containing:
#' \describe{
#'   \item{lwC}{Link-weighted connectance}
#'   \item{lwG}{Link-weighted generality (effective number of prey)}
#'   \item{lwV}{Link-weighted vulnerability (effective number of predators)}
#' }
#'
#' @details
#' Uses Shannon diversity indices to calculate effective numbers of trophic
#' interactions. Higher values indicate more evenly distributed energy flows.
#'
#' @references
#' Bersier, L. F., et al. (2002). Quantitative descriptors of food web matrices.
#' Ecology, 83(9), 2394-2407.
