# =============================================================================
# NETWORK FINALISATION
# =============================================================================
# One place that makes an (net, info) pair satisfy the contract the rendering
# code assumes. plotfw() and create_foodweb_visnetwork() index info$colfg[i]
# by VERTEX index, so info's row order must match V(net)$name exactly, and the
# frame must carry fg, colfg and meanB.
#
# Both invariants used to be re-established by hand in each import module,
# which is why two paths lost them:
#   #18 data_import_server.R merged an uploaded Species_Info sheet with
#       merge(..., by = "species"), which re-sorts alphabetically while the
#       graph keeps matrix order - so traits and colours attached to the wrong
#       species whenever a sheet was supplied.
#   #27 metaweb_manager_server.R exported an info frame with no colfg, no
#       `species` column (the metaweb calls it species_name) and fg = "Other",
#       which is not one of get_functional_group_levels() - the Food Web tab
#       then failed its required-columns check.

#' Candidate column names holding the species key
.SPECIES_KEY_CANDIDATES <- c("species", "species_name", "Species", "GroupName",
                             "Group", "name")

#' Locate the column holding species names
#'
#' @param info Data frame.
#' @return Column name, or NULL when none matches.
#' @keywords internal
.find_species_key <- function(info) {
  hit <- .SPECIES_KEY_CANDIDATES[.SPECIES_KEY_CANDIDATES %in% names(info)]
  if (length(hit) > 0) hit[1] else NULL
}

#' Colour for a functional group, from the canonical palette
#'
#' @param fg Character vector of functional group names.
#' @return Character vector of colours; "gray" for anything off the canonical
#'   list, so an unexpected group degrades visibly instead of erroring.
#' @export
fg_to_color <- function(fg) {
  levels_ref <- get_functional_group_levels()
  vapply(as.character(fg), function(g) {
    if (is.na(g)) return("gray")
    idx <- which(levels_ref == g)
    if (length(idx) == 0) return("gray")
    unname(COLOR_SCHEME[idx])
  }, character(1), USE.NAMES = FALSE)
}

#' Align an info frame to a network and derive its display columns
#'
#' Produces exactly one info row per vertex, in vertex order, carrying every
#' column the rendering code requires. Rows for vertices absent from `info`
#' are created with defaults; info rows that are not vertices are dropped,
#' because a one-to-one correspondence is what makes positional indexing safe.
#'
#' @param net An igraph network whose vertices carry `name`.
#' @param info Data frame of species attributes. The species key may be named
#'   `species`, `species_name`, `Species`, `GroupName`, `Group` or `name`.
#' @return List with `net` (unchanged) and `info` (aligned and completed).
#' @export
#' @examples
#' \dontrun{
#' finalized <- finalize_network(net, info)
#' info_reactive(finalized$info)
#' }
finalize_network <- function(net, info) {
  if (is.null(net) || !igraph::is_igraph(net)) {
    stop("finalize_network(): 'net' must be an igraph object", call. = FALSE)
  }
  vertex_names <- igraph::V(net)$name
  if (is.null(vertex_names) || any(is.na(vertex_names))) {
    stop("finalize_network(): every vertex must carry a name", call. = FALSE)
  }

  if (is.null(info) || !is.data.frame(info) || nrow(info) == 0) {
    info <- data.frame(species = character(0), stringsAsFactors = FALSE)
    key <- "species"
  } else {
    info <- as.data.frame(info, stringsAsFactors = FALSE)
    key <- .find_species_key(info)
    if (is.null(key)) {
      stop("finalize_network(): info has no recognisable species column",
           call. = FALSE)
    }
  }

  # Name-key join: match by NAME, never by position. This is the whole point -
  # merge() and hand-built frames both arrive in arbitrary order.
  idx <- match(vertex_names, as.character(info[[key]]))
  aligned <- info[idx, , drop = FALSE]
  rownames(aligned) <- NULL

  # The key column is rewritten from the graph, so vertices that had no info
  # row (idx = NA) still get their name rather than NA.
  aligned[[key]] <- vertex_names
  if (!identical(key, "species")) {
    aligned$species <- vertex_names
  }

  # Functional group: canonical levels, inferred where absent.
  fg_char <- if ("fg" %in% names(aligned)) as.character(aligned$fg) else NA_character_
  needs_fg <- is.na(fg_char)
  if (any(needs_fg)) {
    inferred <- tryCatch(
      assign_functional_groups(vertex_names[needs_fg]),
      error = function(e) {
        warning(sprintf("[finalize_network] functional group inference failed: %s",
                        conditionMessage(e)), call. = FALSE)
        rep(NA_character_, sum(needs_fg))
      }
    )
    fg_char[needs_fg] <- inferred
  }
  aligned$fg <- factor(fg_char, levels = get_functional_group_levels())

  # Colour is always recomputed, never carried over: fg may have just changed,
  # and a stale colfg is exactly the misalignment this function exists to stop.
  aligned$colfg <- fg_to_color(aligned$fg)

  # The estimators branch on fg with bare if(), which errors on NA. An fg that
  # is off the canonical list (metaweb's "Other") becomes NA at the factor()
  # above, so substitute a label that reaches their default branch:
  # invertebrates / efficiency 0.7 / body mass 1.
  fg_plain <- as.character(aligned$fg)
  fg_plain[is.na(fg_plain)] <- "Unknown"
  .fill <- function(col, value) {
    if (!col %in% names(aligned)) {
      aligned[[col]] <<- value
    } else {
      missing_rows <- is.na(aligned[[col]])
      if (any(missing_rows)) {
        fill_values <- if (length(value) == 1L) value else value[missing_rows]
        aligned[[col]][missing_rows] <<- fill_values
      }
    }
  }

  .fill("meanB", 1.0)
  .fill("bodymasses", vapply(
    seq_along(vertex_names),
    function(i) estimate_body_mass_enhanced(vertex_names[i], fg_plain[i]),
    numeric(1)
  ))
  .fill("met.types", vapply(fg_plain, estimate_metabolic_type_by_fg, character(1),
                            USE.NAMES = FALSE))
  .fill("efficiencies", vapply(fg_plain, estimate_efficiency_by_fg, numeric(1),
                               USE.NAMES = FALSE))
  .fill("losses", 0.1)

  rownames(aligned) <- make.unique(as.character(aligned$species))

  list(net = net, info = aligned)
}
