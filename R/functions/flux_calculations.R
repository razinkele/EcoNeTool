fluxind <- function(fluxes, loop = FALSE) {
  tryCatch({
    # Input validation
    validate_parameter(fluxes, "fluxes")

    if (!is.matrix(fluxes) && !is.data.frame(fluxes) && !inherits(fluxes, "Matrix")) {
      stop("Parameter 'fluxes' must be a matrix, data.frame, or Matrix object", call. = FALSE)
    }

    res <- list()

    # The flux matrix
    W.net <- as.matrix(fluxes)

  ### Taxon-specific Shannon indices of inflows
  # sum of k species inflows --> colsums
  sum.in <- apply(W.net, 2, sum)

  # Diversity of k species inflows
  # columns divided by the total col sum
  H.in.mat <- t(t(W.net) / sum.in) * t(log(t(W.net) / sum.in))
  H.in.mat[!is.finite(H.in.mat)] <- 0  # converts NaN to 0's
  H.in <- apply(H.in.mat, 2, sum) * -1

  # Effective number of prey or resources = N(R,k)
  # The reciprocal of H(R,k) --> N (R,k) is the equivalent number of prey for species k
  N.res <- ifelse(sum.in == 0, H.in, exp(H.in))

  ### Taxon-specific Shannon indices of outflows
  # sum of k species outflows --> rowsums
  sum.out <- apply(W.net, 1, sum)

  # Diversity of k species outflows
  # rows divided by the total row sum
  H.out.mat <- (W.net / sum.out) * log(W.net / sum.out)
  H.out.mat[!is.finite(H.out.mat)] <- 0  # converts NaN to 0's
  H.out <- apply(H.out.mat, 1, sum) * -1

  # Effective number of predators or consumers = N(C,k)
  # The reciprocal of H(C,k) --> N (C,k) is the equivalent number of predators for species k
  N.con <- ifelse(sum.out == 0, H.out, exp(H.out))

  ### Quantitative Weighted connectance
  no.species <- ncol(W.net)

  # The weighted link density (LDw) is:
  # In the weighted version the effective number of predators for species i is weighted by i's
  # contribution to the total outflow the same is the case for the inflows
  tot.mat <- sum(W.net)
  # LD.w <- (sum((sum.in/tot.mat)*N.res) + sum((sum.out/tot.mat)*N.con))/2
  # equivalent to next formula, but next one is closer to manuscript
  LD <- 1 / (2 * tot.mat) * (sum(sum.in * N.res) + sum(sum.out * N.con))

  # Weighted connectance
  res$lwC <- LD / ifelse(loop, no.species, no.species - 1)

  # positional.index
  pos.ind <- sum.in * N.res / (sum.in * N.res + sum.out * N.con)  # positional index
  basal.sp <- pos.ind[pos.ind == 0]  # basal species = 0
  top.sp <- pos.ind[pos.ind == 1]  # definition according to Bersier et al. 2002 top species = [0.99, 1]

  con.sp <- length(pos.ind) - length(basal.sp)  # all consumer taxa except basal
  # weighted quantitative Generality
  res$lwG <- sum(sum.in * N.res / sum(W.net))

  res.sp <- length(pos.ind) - length(top.sp)
  # weighted quantitative Vulnerability
  res$lwV <- sum(sum.out * N.con / sum(W.net))

  return(res)

  }, error = function(e) {
    stop(sprintf("Failed to calculate flux indices: %s", e$message), call. = FALSE)
  })
}

# ============================================================================
# METABOLIC AND FLUX CALCULATIONS
# ============================================================================

#' Calculate metabolic losses for species
#'
#' Computes species-specific metabolic losses using the allometric equation
#' from metabolic theory of ecology (Brown et al. 2004).
#'
#' @param info Data frame with columns: bodymasses (body mass in grams),
#'        met.types (metabolic type: "invertebrates", "ectotherm vertebrates", or "Other")
#' @param temp Temperature in degrees Celsius (default = 3.5°C for Gulf of Riga spring)
#'
#' @return Numeric vector of metabolic losses (J/sec) for each species
#'
#' @details
#' Uses the classic allometric equation:
#' X_i = exp((a * log(M_i) + x0) - E/(k*T))
#'
#' Where:
#' - M_i is body mass of species i (grams)
#' - a = -0.29 (allometric scaling constant for biomass)
#' - x0 is normalization constant (17.17 for invertebrates, 18.47 for vertebrates)
#' - E = 0.69 (activation energy)
#' - k = 0.00008617343 (Boltzmann constant)
#' - T is temperature in Kelvin
#'
#' Note: Use a = 0.71 when using abundance instead of biomass (0.71 = 1 - 0.29)
#'
#' @references
#' Brown, J. H., et al. (2004). Toward a metabolic theory of ecology.
#' Ecology, 85(7), 1771-1789.
calculate_losses <- function(info, temp = DEFAULT_TEMPERATURE) {
  tryCatch({
    # Input validation using utility functions
    validate_dataframe(info, required_cols = c("bodymasses", "met.types"))
    validate_numeric_range(temp, "temp", min = -10, max = 50)

    # Constants from metabolic theory (from R/config.R)
    boltz <- 0.00008617343  # Boltzmann constant
    a <- METABOLIC_A        # Allometric scaling (for biomass)
    E <- METABOLIC_E        # Activation energy

    # Normalization constants (intercept of body-mass metabolism scaling relationship)
    losses_param <- list(
      "invertebrates" = METABOLIC_X0_INVERTEBRATES,
      "ectotherm vertebrates" = METABOLIC_X0_VERTEBRATES,
      "Other" = 0
    )

    # Get x0 for each species based on metabolic type
    x0 <- unlist(losses_param[info$met.types])

    # Validate body masses
    if (any(info$bodymasses <= 0, na.rm = TRUE)) {
      stop("Body masses must be positive (log transformation requires positive values)", call. = FALSE)
    }

    # Calculate losses using allometric equation
    # Formula: exp((a * log(M_i) + x0) - E/(k*(T+273.15)))
    losses <- exp((a * log(info$bodymasses) + x0) - E / (boltz * (273.15 + temp)))

    return(losses)

  }, error = function(e) {
    stop(sprintf("Failed to calculate metabolic losses: %s", e$message), call. = FALSE)
  })
}

#' Calculate energy fluxes using metabolic theory
#'
#' Computes biomass fluxes between species using the fluxweb package,
#' which applies metabolic theory of ecology. Returns both flux matrix
#' and weighted network.
#'
#' @param net An igraph object representing the food web
#' @param info Data frame with columns: meanB (biomass), bodymasses (body mass),
#'        met.types (metabolic type), efficiencies (assimilation efficiencies)
#' @param temp Temperature in degrees Celsius (default = 3.5°C for Gulf of Riga)
#' @param flux_conversion Conversion factor from J/sec to kJ/day (default = 86.4)
#'
#' @return A list containing:
#' \describe{
#'   \item{fluxes}{Matrix of energy fluxes (kJ/day/km²) between species}
#'   \item{netLW}{Weighted igraph object with flux as edge weights}
#'   \item{losses}{Calculated metabolic losses (J/sec) for each species}
#' }
#'
#' @details
#' Uses allometric scaling based on metabolic theory:
#' X_i = exp((a * log(M_i) + x0) - E/(k*T))
#'
#' Losses are calculated from body mass and metabolic type, then used with
#' prey-level assimilation efficiencies to compute energy fluxes.
#' Fluxes are converted from J/sec to kJ/day (multiply by flux_conversion).
#'
#' @references
#' Brown, J. H., et al. (2004). Toward a metabolic theory of ecology.
#' Ecology, 85(7), 1771-1789.
#'
#' Gauzens, B., et al. (2019). fluxweb: An R package to easily estimate energy
#' fluxes in food webs. Methods in Ecology and Evolution, 10(2), 270-279.
get_fluxweb_results <- function(net, info, temp = DEFAULT_TEMPERATURE, flux_conversion = FLUX_CONVERSION_FACTOR) {
  tryCatch({
    # Input validation using utility functions
    validate_network(net, require_directed = FALSE, min_vertices = 1)
    validate_dataframe(info, required_cols = c("meanB", "bodymasses", "met.types", "efficiencies"))
    validate_numeric_range(temp, "temp", min = -10, max = 50)
    validate_numeric_range(flux_conversion, "flux_conversion", min = 0, max = 1000)

    if (nrow(info) != vcount(net)) {
      stop(sprintf("Number of rows in 'info' (%d) must match number of vertices in 'net' (%d)",
                   nrow(info), vcount(net)), call. = FALSE)
    }
    netmatrix <- as_adjacency_matrix(net, sparse = FALSE)
    biomass <- info$meanB

    # Validate inputs
    if (any(is.na(biomass))) {
      warning("NA values in biomass, flux calculations may fail")
    }
    if (any(biomass < 0, na.rm = TRUE)) {
      stop("Biomass values must be non-negative")
    }

    # Calculate metabolic losses from body mass and metabolic type
    # Following Brown et al. (2004) metabolic theory
    losses <- calculate_losses(info, temp)

    # Calculate fluxes using fluxweb package
    # Uses prey-level assimilation efficiencies
    fluxes <- fluxing(netmatrix, biomass, losses, info$efficiencies, ef.level = "prey")

    # Convert J/sec to kJ/day
    fluxes <- fluxes * flux_conversion

    # Ensure rownames and colnames are preserved (from netmatrix)
    if (is.null(rownames(fluxes)) && !is.null(rownames(netmatrix))) {
      rownames(fluxes) <- rownames(netmatrix)
      colnames(fluxes) <- colnames(netmatrix)
    }

    # Create weighted network
    netLW <- graph_from_adjacency_matrix(fluxes, weighted = TRUE)

    # Explicitly set vertex names to ensure they're preserved
    if (!is.null(V(net)$name)) {
      V(netLW)$name <- V(net)$name
    }

    list(fluxes = fluxes, netLW = netLW, losses = losses)

  }, error = function(e) {
    stop(sprintf("Failed to calculate fluxweb results: %s", e$message), call. = FALSE)
  })
}

# ============================================================================
# KEYSTONE SPECIES ANALYSIS
# ============================================================================

#' Calculate Mixed Trophic Impact (MTI) matrix
#'
#' Computes the direct and indirect impacts of each species on all others
#' using the ECOPATH approach. MTI represents the net effect of increasing
#' the biomass of one species on all other species in the food web.
#'
#' @param net An igraph object representing the food web
#' @param info Data frame with 'meanB' column for biomass
#'
#' @return A matrix where MTI[i,j] represents the impact of species j on species i
#'
#' @details
#' The MTI is calculated using the equation:
#' MTI = -I * (DC + FC)^(-1) * DC
#'
#' Where:
#' - DC (Diet Composition) = consumption matrix normalized by total consumption
#' - FC (Fishery Catch) = assumed zero for natural systems
#' - I = identity matrix
#'
#' Positive MTI values indicate a positive impact (increase in impactor increases impacted)
#' Negative MTI values indicate a negative impact (increase in impactor decreases impacted)
#'
#' @references
#' Ulanowicz, R. E., & Puccia, C. J. (1990). Mixed trophic impacts in ecosystems.
#' Coenoses, 5(1), 7-16.
#'
#' Libralato, S., et al. (2006). A method for identifying keystone species in
#' food web models. Ecological Modelling, 195(3-4), 153-171.
