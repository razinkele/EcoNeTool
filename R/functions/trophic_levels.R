
# ============================================================================
# TROPHIC LEVEL CALCULATIONS
# ============================================================================

#' Calculate Trophic Levels for a Food Web (Iterative Method)
#'
#' Computes trophic levels by fixed-point iteration under the edge contract
#' (prey -> predator, `adj[prey, predator]`; see R/functions/network_finalize.R).
#' Basal species (no incoming edge) get TL = 1; every other species gets
#' TL = 1 + mean(TL of its prey).
#'
#' @param net An igraph object representing the food web (directed graph)
#' @param max_iter Maximum number of iterations (default: 100)
#' @param convergence Convergence threshold (default: 0.0001)
#'
#' @return A named numeric vector, one trophic level per vertex. `NA` marks a
#'   vertex whose TL is undefined:
#'   \itemize{
#'     \item no path from any basal species (e.g. a pure cannibal loop), or
#'     \item still changing by more than `convergence` after `max_iter` passes.
#'   }
#'   Every `NA` is announced with a `warning()` naming the vertices. Callers
#'   must use `na.rm = TRUE` (or equivalent) when aggregating.
#'
#' @details
#' - A self-loop counts as an incoming edge, so a vertex whose only prey is
#'   itself is not basal.
#' - Only vertices reachable from a basal vertex are iterated, and a consumer
#'   averages only over prey in that reachable set.
#' - With no basal vertex at all, every TL is `NA` (one warning).
#'
#' @examples
#' tl <- calculate_trophic_levels(net)
#' mean(tl, na.rm = TRUE)  # Mean trophic level of the food web
#'
#' @references
#' Williams, R. J., & Martinez, N. D. (2004). Limits to trophic levels and
#' omnivory in complex food webs. Proceedings of the Royal Society B, 271(1540), 549-556.
#'
#' @export
calculate_trophic_levels <- function(net, max_iter = 100, convergence = 0.0001) {
  tryCatch({
    validate_network(net, require_directed = TRUE, min_vertices = 1)
    validate_numeric_range(max_iter, "max_iter", min = 1, max = 10000)
    validate_numeric_range(convergence, "convergence", min = 0, max = 1)

    n <- vcount(net)
    node_names <- V(net)$name
    if (is.null(node_names)) node_names <- as.character(seq_len(n))
    .name_list <- function(idx) {
      shown <- head(node_names[idx], 10)
      paste0(paste(shown, collapse = ", "), if (length(idx) > 10) ", ..." else "")
    }

    # adj[prey, predator]: prey of i are the non-zero rows of column i.
    adj <- as.matrix(as_adjacency_matrix(net, sparse = FALSE))
    basal <- which(colSums(adj) == 0)
    tl <- rep(NA_real_, n)

    if (length(basal) == 0) {
      warning(sprintf(
        "No basal species (every node has prey): all %d trophic levels are NA", n
      ), call. = FALSE)
    } else {
      hops <- igraph::distances(net, v = basal, mode = "out")
      reach <- which(colSums(is.finite(hops)) > 0)
      unreachable <- setdiff(seq_len(n), reach)
      if (length(unreachable) > 0) {
        warning(sprintf("%d node(s) have no path from a basal species: %s",
                        length(unreachable), .name_list(unreachable)), call. = FALSE)
      }

      tl[reach] <- 1
      consumers <- setdiff(reach, basal)
      prey_of <- lapply(consumers, function(i) intersect(which(adj[, i] > 0), reach))
      change <- rep(0, n)
      converged <- length(consumers) == 0
      iter <- 0
      while (!converged && iter < max_iter) {
        iter <- iter + 1
        tl_old <- tl
        for (k in seq_along(consumers)) {
          tl[consumers[k]] <- 1 + mean(tl[prey_of[[k]]])
        }
        change <- abs(tl - tl_old)
        change[is.na(change)] <- 0
        converged <- max(change) < convergence
      }

      if (!converged) {
        stuck <- which(change >= convergence)
        tl[stuck] <- NA_real_
        warning(sprintf(
          "Trophic levels of %d node(s) did not converge after %d iterations and are NA: %s",
          length(stuck), max_iter, .name_list(stuck)
        ), call. = FALSE)
      }
    }

    names(tl) <- V(net)$name
    tl
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
