# =============================================================================
# NETWORK COMPARISON
# =============================================================================
# Pure (non-Shiny) functions behind the "Network Comparison" tab: put two
# (net, info) pairs side by side and report differences in topology, species
# composition and trophic links.
#
# Conventions:
#   * Species are matched by vertex name after trimws() and case folding;
#     display names are taken from web A where a species is shared.
#   * Edges follow the app-wide contract (prey -> predator), so a node's prey
#     count is its in-degree and its predator count its out-degree.
#   * Node-weighted indicators are only reported when BOTH infos carry meanB.

#' Normalise species names for matching
#'
#' @param x Character vector of vertex names.
#' @return Trimmed, lower-cased names.
#' @keywords internal
.cmp_key <- function(x) {
  tolower(trimws(as.character(x)))
}

#' Jaccard index of two sets, 0 when both are empty
#' @keywords internal
.cmp_jaccard <- function(a, b) {
  u <- length(union(a, b))
  if (u == 0) return(0)
  length(intersect(a, b)) / u
}

#' Edge list of a web keyed by normalised prey/predator names
#' @keywords internal
.cmp_edge_keys <- function(net) {
  el <- igraph::as_edgelist(net, names = TRUE)
  if (nrow(el) == 0) {
    return(data.frame(prey = character(0), predator = character(0), stringsAsFactors = FALSE))
  }
  data.frame(prey = .cmp_key(el[, 1]), predator = .cmp_key(el[, 2]), stringsAsFactors = FALSE)
}

#' Per-web node summary used by the per-species table
#' @keywords internal
.cmp_node_summary <- function(net) {
  tl <- tryCatch(
    calculate_trophic_levels(net),
    error = function(e) {
      warning(sprintf("[compare_networks] trophic levels failed: %s", conditionMessage(e)),
              call. = FALSE)
      rep(NA_real_, igraph::vcount(net))
    }
  )
  data.frame(
    key = .cmp_key(igraph::V(net)$name),
    name = as.character(igraph::V(net)$name),
    prey = as.numeric(igraph::degree(net, mode = "in")),
    predators = as.numeric(igraph::degree(net, mode = "out")),
    tl = as.numeric(tl),
    stringsAsFactors = FALSE
  )
}

#' Topological (and optionally node-weighted) indicators as a named numeric vector
#' @keywords internal
.cmp_metric_vector <- function(net, info, with_weighted) {
  topo <- unlist(get_topological_indicators(net))
  if (!with_weighted) return(topo)
  nw <- tryCatch(
    unlist(get_node_weighted_indicators(net, info)),
    error = function(e) {
      warning(sprintf("[compare_networks] node-weighted indicators failed: %s",
                      conditionMessage(e)), call. = FALSE)
      NULL
    }
  )
  c(topo, nw)
}

#' Split "prey\rpredator" ids back into display names taken from web A
#' @keywords internal
.cmp_display_links <- function(ids, nodes_a) {
  if (length(ids) == 0) {
    return(data.frame(prey = character(0), predator = character(0), stringsAsFactors = FALSE))
  }
  parts <- strsplit(ids, "\r", fixed = TRUE)
  prey_key <- vapply(parts, function(p) p[1], character(1))
  pred_key <- vapply(parts, function(p) p[2], character(1))
  data.frame(
    prey = nodes_a$name[match(prey_key, nodes_a$key)],
    predator = nodes_a$name[match(pred_key, nodes_a$key)],
    stringsAsFactors = FALSE
  )
}

#' Compare two food webs
#'
#' @param net_a,net_b igraph networks (prey -> predator edges, named vertices).
#' @param info_a,info_b Species info data frames aligned to the networks
#'   (one row per vertex, in vertex order, as produced by finalize_network()).
#'   A `meanB` column in both enables node-weighted indicators.
#' @param label_a,label_b Display labels for the two webs.
#' @return A list:
#' \describe{
#'   \item{metrics}{data.frame(metric, a, b, delta) with delta = b - a.}
#'   \item{species}{list(shared, only_a, only_b, jaccard); names as displayed.}
#'   \item{links}{list(shared, only_a, only_b, jaccard,
#'     n_unshared_species_a, n_unshared_species_b). The three data frames hold
#'     prey/predator pairs restricted to shared species; the two counts are
#'     links in A / B touching a species the other web lacks.}
#'   \item{per_species}{data.frame for shared species: prey_a, prey_b,
#'     delta_prey, predators_a, predators_b, delta_predators, tl_a, tl_b, delta_tl.}
#'   \item{labels}{c(a = label_a, b = label_b).}
#' }
#' @export
compare_networks <- function(net_a, info_a, net_b, info_b,
                             label_a = "Network A", label_b = "Network B") {
  validate_network(net_a, require_directed = TRUE, min_vertices = 1)
  validate_network(net_b, require_directed = TRUE, min_vertices = 1)
  if (is.null(igraph::V(net_a)$name) || is.null(igraph::V(net_b)$name)) {
    stop("compare_networks(): both networks must have named vertices", call. = FALSE)
  }

  # --- Metrics -------------------------------------------------------------
  with_weighted <- is.data.frame(info_a) && is.data.frame(info_b) &&
    "meanB" %in% names(info_a) && "meanB" %in% names(info_b) &&
    nrow(info_a) == igraph::vcount(net_a) && nrow(info_b) == igraph::vcount(net_b)
  m_a <- .cmp_metric_vector(net_a, info_a, with_weighted)
  m_b <- .cmp_metric_vector(net_b, info_b, with_weighted)
  common <- intersect(names(m_a), names(m_b))
  metrics <- data.frame(
    metric = common,
    a = as.numeric(m_a[common]),
    b = as.numeric(m_b[common]),
    stringsAsFactors = FALSE
  )
  metrics$delta <- metrics$b - metrics$a

  # --- Species -------------------------------------------------------------
  nodes_a <- .cmp_node_summary(net_a)
  nodes_b <- .cmp_node_summary(net_b)
  shared_keys <- intersect(nodes_a$key, nodes_b$key)
  species <- list(
    shared = nodes_a$name[match(shared_keys, nodes_a$key)],
    only_a = nodes_a$name[!nodes_a$key %in% shared_keys],
    only_b = nodes_b$name[!nodes_b$key %in% shared_keys],
    jaccard = .cmp_jaccard(nodes_a$key, nodes_b$key)
  )

  # --- Links (over shared species only) ------------------------------------
  edges_a <- .cmp_edge_keys(net_a)
  edges_b <- .cmp_edge_keys(net_b)
  in_shared_a <- edges_a$prey %in% shared_keys & edges_a$predator %in% shared_keys
  in_shared_b <- edges_b$prey %in% shared_keys & edges_b$predator %in% shared_keys
  ids_a <- unique(paste(edges_a$prey[in_shared_a], edges_a$predator[in_shared_a], sep = "\r"))
  ids_b <- unique(paste(edges_b$prey[in_shared_b], edges_b$predator[in_shared_b], sep = "\r"))
  links <- list(
    shared = .cmp_display_links(intersect(ids_a, ids_b), nodes_a),
    only_a = .cmp_display_links(setdiff(ids_a, ids_b), nodes_a),
    only_b = .cmp_display_links(setdiff(ids_b, ids_a), nodes_a),
    jaccard = .cmp_jaccard(ids_a, ids_b),
    n_unshared_species_a = sum(!in_shared_a),
    n_unshared_species_b = sum(!in_shared_b)
  )

  # --- Per shared species --------------------------------------------------
  ia <- match(shared_keys, nodes_a$key)
  ib <- match(shared_keys, nodes_b$key)
  per_species <- data.frame(
    species = nodes_a$name[ia],
    prey_a = nodes_a$prey[ia],
    prey_b = nodes_b$prey[ib],
    predators_a = nodes_a$predators[ia],
    predators_b = nodes_b$predators[ib],
    tl_a = nodes_a$tl[ia],
    tl_b = nodes_b$tl[ib],
    stringsAsFactors = FALSE
  )
  per_species$delta_prey <- per_species$prey_b - per_species$prey_a
  per_species$delta_predators <- per_species$predators_b - per_species$predators_a
  per_species$delta_tl <- per_species$tl_b - per_species$tl_a
  rownames(per_species) <- NULL

  list(
    metrics = metrics,
    species = species,
    links = links,
    per_species = per_species,
    labels = c(a = label_a, b = label_b)
  )
}

#' List bundled example webs usable as a comparison slot
#'
#' @param dir Directory holding `*.Rdata` files with `net` + `info`.
#' @return Character vector of full paths (templates excluded), possibly empty.
#' @export
list_example_networks <- function(dir = app_path("examples")) {
  if (!dir.exists(dir)) return(character(0))
  files <- list.files(dir, pattern = "\\.(Rdata|rda|RData)$", full.names = TRUE)
  files[!grepl("template", basename(files), ignore.case = TRUE)]
}

#' Load one example web into a comparison slot
#'
#' @param path Path to an `.Rdata` file containing `net` and `info`.
#' @return list(net, info, label) with info aligned by finalize_network().
#' @export
load_example_network <- function(path) {
  if (!file.exists(path)) stop(sprintf("Example file not found: %s", path), call. = FALSE)
  env <- new.env()
  load(path, envir = env)
  if (!exists("net", envir = env, inherits = FALSE) || !igraph::is_igraph(env$net)) {
    stop("Example file must contain an igraph object named 'net'", call. = FALSE)
  }
  if (!exists("info", envir = env, inherits = FALSE)) {
    stop("Example file must contain a data frame named 'info'", call. = FALSE)
  }
  net <- igraph::upgrade_graph(env$net)
  info <- env$info
  if (is.null(igraph::V(net)$name) || all(grepl("^[0-9]+$", igraph::V(net)$name))) {
    if ("species" %in% colnames(info)) {
      igraph::V(net)$name <- as.character(info$species)
    } else if (!is.null(rownames(info))) {
      igraph::V(net)$name <- rownames(info)
    }
  }
  finalized <- finalize_network(net, info)
  list(net = finalized$net, info = finalized$info,
       label = tools::file_path_sans_ext(basename(path)))
}
