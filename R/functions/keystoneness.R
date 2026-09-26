#' Calculate the Mixed Trophic Impact (MTI) matrix
#'
#' Ulanowicz & Puccia (1990) as used by Ecopath, on the app-wide edge contract
#' (A -> B means B eats A; every matrix below is indexed [prey, predator]).
#'
#' @param net Directed igraph food web. Optional edge attribute `diet_prop`
#'   (diet proportion of the edge's prey in the edge's predator's diet).
#' @param info Data frame aligned to `V(net)` (one row per vertex, same order,
#'   as produced by finalize_network()) with `meanB`; optional `QB`.
#'
#' @return Numeric n x n matrix with dimnames `V(net)$name`. `MTI[i, j]` is the
#'   net (direct + indirect) impact of a small increase of i on j. The diagonal
#'   is kept (self-impact); displays blank it.
#'
#' @details
#' \enumerate{
#'   \item \code{D}: `diet_prop` weighted adjacency when every edge has one,
#'     else the binary adjacency (equal diet split). Columns (predators)
#'     normalised to sum to 1; predators without prey keep a zero column.
#'   \item Consumption \code{cons_k = B_k * QB_k} where `QB_k` is finite and
#'     positive, else \code{cons_k = B_k} (biomass proxy).
#'   \item Flows \code{T[p, k] = D[p, k] * cons_k};
#'     \code{FC = T / rowSums(T)}: share of prey p's total predation taken by k.
#'   \item \code{Q = D - t(FC)}; \code{MTI = solve(I - Q) - I}. If
#'     \code{rcond(I - Q) < 1e-10} the Moore-Penrose inverse (MASS::ginv) is
#'     used and a warning() is raised.
#' }
#'
#' @references
#' Ulanowicz, R. E., & Puccia, C. J. (1990). Mixed trophic impacts in
#' ecosystems. Coenoses, 5(1), 7-16.
#' @export
calculate_mti <- function(net, info) {
  tryCatch({
    validate_network(net, require_directed = TRUE, min_vertices = 1)
    validate_dataframe(info, required_cols = "meanB")

    n <- vcount(net)
    if (nrow(info) != n) {
      stop(sprintf("Number of rows in 'info' (%d) must match number of vertices in 'net' (%d)",
                   nrow(info), n), call. = FALSE)
    }
    sp <- V(net)$name
    if (is.null(sp)) sp <- as.character(seq_len(n))

    # 1. Diet composition D[prey, predator], columns sum to 1
    has_diet <- "diet_prop" %in% igraph::edge_attr_names(net)
    if (has_diet && anyNA(igraph::E(net)$diet_prop)) {
      warning("[calculate_mti] some edges have no diet_prop; using an equal diet split for all",
              call. = FALSE)
      has_diet <- FALSE
    }
    D <- if (has_diet) {
      as.matrix(igraph::as_adjacency_matrix(net, attr = "diet_prop", sparse = FALSE))
    } else {
      as.matrix(igraph::as_adjacency_matrix(net, sparse = FALSE))
    }
    D[!is.finite(D) | D < 0] <- 0
    col_tot <- colSums(D)
    D <- sweep(D, 2, ifelse(col_tot > 0, col_tot, 1), "/")

    # 2. Consumption per predator: B * Q/B where available, else biomass proxy
    B <- as.numeric(info$meanB)
    cons <- B
    if ("QB" %in% names(info)) {
      qb <- suppressWarnings(as.numeric(info$QB))
      use_qb <- is.finite(qb) & qb > 0
      cons[use_qb] <- B[use_qb] * qb[use_qb]
    }
    cons[!is.finite(cons) | cons < 0] <- 0

    # 3. Flows and predation shares FC[prey, predator]
    flows <- sweep(D, 2, cons, "*")
    row_tot <- rowSums(flows)
    FC <- flows / ifelse(row_tot > 0, row_tot, 1)

    # 4. Net impacts
    I <- diag(n)
    A <- I - (D - t(FC))
    if (.mti_rcond(A) < 1e-10) {
      warning("[calculate_mti] (I - Q) is singular or near-singular; using the pseudo-inverse",
              call. = FALSE)
      A_inv <- MASS::ginv(A)
    } else {
      A_inv <- solve(A)
    }
    MTI <- A_inv - I
    dimnames(MTI) <- list(sp, sp)
    MTI
  }, error = function(e) {
    stop(sprintf("Failed to calculate Mixed Trophic Impact (MTI): %s", e$message), call. = FALSE)
  })
}

#' Reciprocal condition number used by calculate_mti()'s singularity check
#'
#' A named seam so tests can force the pseudo-inverse branch.
#' @keywords internal
.mti_rcond <- function(A) rcond(A)

#' Calculate Keystoneness Index
#'
#' Libralato et al. (2006) keystoneness from the MTI matrix.
#'
#' @param net An igraph object representing the food web (see calculate_mti())
#' @param info Data frame aligned to `V(net)` with `meanB` (optional `QB`)
#'
#' @return A data frame sorted by `keystoneness` (descending, `NA` last) with
#'   columns:
#' \describe{
#'   \item{species}{Species name}
#'   \item{overall_effect}{epsilon_i = sqrt(sum over j != i of MTI[i, j]^2)}
#'   \item{relative_biomass}{p_i = B_i / sum(B)}
#'   \item{keystoneness}{KS_i = log10(epsilon_i * (1 - p_i)), base-10 log as
#'     reported by EwE; NA when undefined}
#'   \item{keystone_status}{"Keystone", "Dominant", "Other" or "Undefined"}
#'   \item{ks_rank}{Rank by KS, 1 = highest; NA when KS is NA}
#' }
#'
#' @details
#' Classification (a design choice; Libralato ranks without cut-offs):
#' \itemize{
#'   \item Keystone: KS >= 75th percentile of KS and p < 0.05
#'   \item Dominant: KS >= 75th percentile of KS and p >= 0.05
#'   \item Other: every other species with a finite KS
#'   \item Undefined: KS not finite (e.g. epsilon = 0 or p = 1)
#' }
#'
#' @references
#' Libralato, S., Christensen, V., & Pauly, D. (2006). A method for identifying
#' keystone species in food web models. Ecological Modelling, 195(3-4), 153-171.
#' @export
calculate_keystoneness <- function(net, info) {
  tryCatch({
    validate_network(net, require_directed = TRUE, min_vertices = 1)
    validate_dataframe(info, required_cols = "meanB")

    if (nrow(info) != vcount(net)) {
      stop(sprintf("Number of rows in 'info' (%d) must match number of vertices in 'net' (%d)",
                   nrow(info), vcount(net)), call. = FALSE)
    }

    MTI <- calculate_mti(net, info)

    # epsilon_i: overall effect of impactor i (row), self-impact excluded
    off_diag <- MTI
    diag(off_diag) <- 0
    overall_effect <- sqrt(rowSums(off_diag^2))

    biomass <- as.numeric(info$meanB)
    total_biomass <- sum(biomass, na.rm = TRUE)
    if (!(total_biomass > 0)) {
      stop("Total biomass must be positive to calculate keystoneness", call. = FALSE)
    }
    relative_biomass <- biomass / total_biomass

    keystoneness <- suppressWarnings(log10(overall_effect * (1 - relative_biomass)))
    keystoneness[!is.finite(keystoneness)] <- NA_real_

    ks_cut <- if (all(is.na(keystoneness))) {
      NA_real_
    } else {
      stats::quantile(keystoneness, 0.75, na.rm = TRUE, names = FALSE)
    }
    top <- !is.na(keystoneness) & keystoneness >= ks_cut
    keystone_status <- ifelse(
      is.na(keystoneness), "Undefined",
      ifelse(top & relative_biomass < 0.05, "Keystone",
             ifelse(top, "Dominant", "Other"))
    )

    results <- data.frame(
      species = rownames(MTI),
      overall_effect = unname(overall_effect),
      relative_biomass = relative_biomass,
      keystoneness = unname(keystoneness),
      keystone_status = keystone_status,
      ks_rank = as.integer(rank(-keystoneness, ties.method = "min", na.last = "keep")),
      stringsAsFactors = FALSE
    )

    results <- results[order(-results$keystoneness, na.last = TRUE), ]
    rownames(results) <- NULL
    results
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
