
# ============================================================================
# TROPHIC LEVEL CALCULATIONS
# ============================================================================

#' Calculate Trophic Levels for a Food Web (Iterative Method)
#'
#' Computes trophic levels using an iterative algorithm. Basal species
#' (no prey) are assigned TL = 1. Consumer species have TL = 1 + mean(TL of prey).
#' The algorithm iterates until convergence or maximum iterations reached.
#'
#' @param net An igraph object representing the food web (directed graph)
#' @param max_iter Maximum number of iterations (default: 100)
#' @param convergence Convergence threshold (default: 0.0001)
#'
#' @return A numeric vector of trophic levels for each species/node
#'
#' @details
#' The algorithm uses fixed-point iteration:
#' - Initialize all species to TL = 1
#' - Iterate: for each consumer, TL = 1 + mean(prey TL)
#' - Stop when max change < convergence or max iterations reached
#'
#' @examples
#' tl <- calculate_trophic_levels(net)
#' mean(tl)  # Mean trophic level of the food web
#'
#' @references
#' Williams, R. J., & Martinez, N. D. (2004). Limits to trophic levels and
#' omnivory in complex food webs. Proceedings of the Royal Society B, 271(1540), 549-556.
#'
#' @export
calculate_trophic_levels <- function(net, max_iter = 100, convergence = 0.0001) {
  # Input validation
  tryCatch({
    # Validate network
    validate_network(net, require_directed = TRUE, min_vertices = 1)

    # Validate parameters
    validate_numeric_range(max_iter, "max_iter", min = 1, max = 10000)
    validate_numeric_range(convergence, "convergence", min = 0, max = 1)

    n <- vcount(net)

    # Initialize trophic levels
    tl <- rep(1, n)
    adj <- as_adjacency_matrix(net, sparse = FALSE)

    # Iterate until convergence
    converged <- FALSE
    for (iter in 1:max_iter) {
      tl_old <- tl

      for (i in 1:n) {
        # Find prey of species i (incoming edges in prey->predator convention)
        # In igraph directed networks: edge from A to B means A is eaten by B
        # So prey of species i are those with edges TO i (column i)
        prey_indices <- which(adj[, i] > 0)

        if (length(prey_indices) > 0) {
          # TL = 1 + mean TL of prey
          tl[i] <- 1 + mean(tl[prey_indices])
        } else {
          # Basal species
          tl[i] <- 1
        }
      }

      # Check for convergence
      max_change <- max(abs(tl - tl_old))
      if (max_change < convergence) {
        converged <- TRUE
        break
      }
    }

    if (!converged) {
      warning(sprintf("Trophic level calculation did not converge after %d iterations (max change: %.6f)",
                      max_iter, max_change))
    }

    # Set names if available
    if (!is.null(V(net)$name)) {
      names(tl) <- V(net)$name
    }

    return(tl)

  }, error = function(e) {
    stop(sprintf("Failed to calculate trophic levels: %s", e$message), call. = FALSE)
  })
}

#' Calculate Trophic Levels Using Shortest-Weighted Path Method
#'
#' Alternative trophic level calculation using shortest path to basal species.
#' This is the method from the original BalticFW.Rdata.
#'
#' @param net An igraph object representing the food web
#'
#' @return A numeric vector of short-weighted trophic levels (SWTL) for each species
#'
#' @details
#' Uses shortest path to basal species, weighted by number of prey:
#' - Basal species (no prey) get TL = 1
#' - Consumers: TL = 1 + weighted mean of prey TL based on shortest paths
#'
#' @export
calculate_trophic_levels_shortpath <- function(net) {
  # Input validation
  tryCatch({
    # Validate network
    validate_network(net, require_directed = TRUE, min_vertices = 1)

    mat <- get.adjacency(net, sparse = FALSE)
    edge.list_web <- graph.adjacency(mat, mode = "directed")

    # Basal species are those with no prey
    # In prey->predator convention: species with no incoming edges (col sum = 0)
    basal <- rownames(mat)[apply(mat, 2, sum) == 0]

    if (length(basal) == 0) {
      warning("No basal species detected (all species have prey). This may indicate a cyclic food web.")
    }

    paths_prey <- suppressWarnings(shortest.paths(
      graph = net, v = V(net), to = V(net)[basal],
      mode = "in", weights = NULL, algorithm = "unweighted"
    ))

    paths_prey[is.infinite(paths_prey)] <- NA
    shortest_paths <- suppressWarnings(as.matrix(apply(paths_prey, 1, min, na.rm = TRUE)))
    # for species with no prey apart from them
    shortest_paths[is.infinite(shortest_paths)] <- NA

    in_deg <- apply(mat, 2, sum)  # ==degree(net, mode = "in")
    # Shortest TL
    sTL <- 1 + shortest_paths  # Commonly, detritus have a TL value of 1. (Shortest path to basal = 0)

    S <- dim(mat)[1]  # == vcount(net)
    # Creating the matrix
    short_TL_matrix <- mat * matrix(rep(sTL, length(sTL)), ncol = length(sTL))

    prey_ave <- ifelse(in_deg == 0, 0, 1 / in_deg)

    sumShortTL <- apply(short_TL_matrix, 2, sum, na.rm = TRUE)  # sum all shortest path

    # Short-weighted TL weight by the number of prey
    SWTL <- 1 + (prey_ave * sumShortTL)

    # check that only basal species have TL of 1
    SWTL[!rownames(mat) %in% basal & SWTL == 1] <- NA

    # Set names if available
    if (!is.null(V(net)$name)) {
      names(SWTL) <- V(net)$name
    }

    return(SWTL)

  }, error = function(e) {
    stop(sprintf("Failed to calculate trophic levels (shortpath method): %s", e$message), call. = FALSE)
  })
}

# ============================================================================
# DEPRECATED FUNCTIONS (For Backward Compatibility)
# ============================================================================

#' @title Calculate Trophic Levels (Deprecated)
#' @description **DEPRECATED:** Use `calculate_trophic_levels()` instead.
#' @param ... All parameters passed to `calculate_trophic_levels()`
#' @return Trophic levels vector
#' @keywords internal
trophiclevels <- function(...) {
  .Deprecated("calculate_trophic_levels",
              msg = "trophiclevels() is deprecated. Use calculate_trophic_levels() instead.")
  calculate_trophic_levels(...)
}

#' @title Calculate Trophic Levels Shortpath (Deprecated)
#' @description **DEPRECATED:** Use `calculate_trophic_levels_shortpath()` instead.
#' @param ... All parameters passed to `calculate_trophic_levels_shortpath()`
#' @return Trophic levels vector
#' @keywords internal
trophiclevels_shortpath <- function(...) {
  .Deprecated("calculate_trophic_levels_shortpath",
              msg = "trophiclevels_shortpath() is deprecated. Use calculate_trophic_levels_shortpath() instead.")
  calculate_trophic_levels_shortpath(...)
}
