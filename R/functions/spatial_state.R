# Spatial tab session state helpers (deep-analysis fix D1)
#
# Pure functions behind spatial_server(): which per-session values to clear
# when an upstream input changes (F58), whether the cached EUSeaMap layer
# covers the extent in use (F57), sf_use_s2() save/restore (F60) and species
# upload validation (F61). No Shiny, so they are unit-tested directly.

#' Spatial state nodes cleared when an upstream input changes (F58)
#'
#' The spatial tab's per-session values form a small DAG:
#' study_area -> hex_grid -> local_networks -> metrics_data, and
#' hex_grid / habitat_clipped -> grid_with_habitat. Changing a node must clear
#' everything downstream of it, or a new grid shows the previous run's
#' networks and metrics (hex ids restart at HEX_0001, so the merge repaints
#' them). species_data is an independent input and is never cleared.
#' euseamap_data is shared with the Ecopath tab and is coverage-checked
#' (euseamap_covers()) instead of cleared.
#'
#' @param from "study_area", "grid", "networks" or "habitat".
#' @return Character vector of node names to reset.
spatial_invalidation_targets <- function(from) {
  switch(from,
    study_area = c("hex_grid", "local_networks", "metrics_data", "habitat_clipped", "grid_with_habitat"),
    grid = c("local_networks", "metrics_data", "grid_with_habitat"),
    networks = "metrics_data",
    habitat = "grid_with_habitat",
    stop(sprintf("[spatial_state] unknown invalidation source '%s'", from), call. = FALSE)
  )
}

#' Does the loaded EUSeaMap layer cover a bounding box? (F57)
#'
#' @param layer sf layer from load_regional_euseamap(), or NULL.
#' @param bbox c(xmin, ymin, xmax, ymax) in WGS84, or NULL.
#' @return TRUE only when a layer is loaded and its box (euseamap_layer_bbox())
#'   contains `bbox`.
euseamap_covers <- function(layer, bbox) {
  if (is.null(layer) || is.null(bbox)) return(FALSE)
  have <- euseamap_layer_bbox(layer)
  need <- suppressWarnings(as.numeric(bbox))
  if (length(have) != 4L || length(need) != 4L || anyNA(c(have, need))) return(FALSE)
  tol <- 1e-9
  need[1] >= have[1] - tol && need[2] >= have[2] - tol &&
    need[3] <= have[3] + tol && need[4] <= have[4] + tol
}

#' Extent the habitat layer must cover (F57)
#'
#' The study area (buffered by habitat_load_bbox()) if one is set, otherwise
#' the grid's extent. There is deliberately no default box: the module used to
#' load a fixed 1 x 1 degree box when neither existed.
#'
#' @param study_area_sf sf study area, or NULL.
#' @param hex_grid sf grid, or NULL.
#' @return c(xmin, ymin, xmax, ymax) in WGS84, or NULL.
habitat_target_bbox <- function(study_area_sf, hex_grid) {
  wgs84_bbox <- function(x) {
    if (!is.na(sf::st_crs(x)) && sf::st_crs(x) != sf::st_crs(4326)) x <- sf::st_transform(x, 4326)
    unname(as.numeric(sf::st_bbox(x)))
  }
  if (!is.null(study_area_sf)) return(habitat_load_bbox(wgs84_bbox(study_area_sf)))
  if (!is.null(hex_grid)) return(wgs84_bbox(hex_grid))
  NULL
}

#' Evaluate with s2 off, restoring the previous setting even on error (F60)
#'
#' sf_use_s2() is process-wide: Shiny Server runs every session in one R
#' process, so a toggle that is not restored leaks to all users.
#'
#' @param expr Expression to evaluate.
#' @return The value of `expr`.
with_s2_disabled <- function(expr) {
  old <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old)), add = TRUE)
  force(expr)
}

#' Read one Broad Belt Transect (BBT) study area polygon (F60)
#'
#' @param name Value of the `Name` field.
#' @param path GeoJSON file (default the bundled data/BBT.geojson).
#' @return sf with the matching feature(s), Z/M dropped.
read_bbt_polygon <- function(name, path = app_path("data/BBT.geojson")) {
  if (!file.exists(path)) stop(sprintf("BBT file not found: %s", path), call. = FALSE)
  with_s2_disabled({
    bbt <- sf::st_read(path, quiet = TRUE)
    hit <- bbt[!is.na(bbt$Name) & bbt$Name == name, ]
    if (nrow(hit) == 0) stop(sprintf("BBT area '%s' not found in %s", name, basename(path)), call. = FALSE)
    sf::st_zm(hit)
  })
}

#' Clean an uploaded species occurrence table (F61)
#'
#' Only the column names used to be checked, so non-numeric or out-of-range
#' coordinates reached addCircleMarkers() and killed the session.
#'
#' @param df Data frame read from the upload (species, lon, lat, optional biomass).
#' @param bbox Optional c(xmin, ymin, xmax, ymax) of the study area, used to
#'   spot swapped lon/lat.
#' @return list(data, dropped, messages). Errors when no row survives.
validate_species_occurrences <- function(df, bbox = NULL) {
  # A ';'-separated file read with read.csv() arrives as one column.
  if (ncol(df) == 1L && grepl(";", names(df)[1], fixed = TRUE)) {
    df <- utils::read.csv2(text = c(names(df)[1], as.character(df[[1]])), stringsAsFactors = FALSE)
  }
  num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))
  df$lon <- num(df$lon)
  df$lat <- num(df$lat)
  if ("biomass" %in% names(df)) df$biomass <- num(df$biomass)
  species <- trimws(as.character(df$species))

  keep <- is.finite(df$lon) & is.finite(df$lat) & df$lon >= -180 & df$lon <= 180 &
    df$lat >= -90 & df$lat <= 90 & !is.na(species) & nzchar(species)
  dropped <- sum(!keep)
  df <- df[keep, , drop = FALSE]
  if (nrow(df) == 0) {
    stop(sprintf("No valid occurrences: all %d rows lack a species or valid lon/lat", dropped), call. = FALSE)
  }

  messages <- character()
  if (dropped > 0) {
    messages <- c(messages, sprintf("%d row(s) dropped: missing species or invalid lon/lat", dropped))
  }
  if (!is.null(bbox)) {
    inside <- function(x, y) x >= bbox[1] & x <= bbox[3] & y >= bbox[2] & y <= bbox[4]
    if (mean(!inside(df$lon, df$lat)) > 0.5 && mean(inside(df$lat, df$lon)) > 0.5) {
      messages <- c(messages, "lon/lat appear swapped: most points fall inside the study area only when swapped")
    }
  }
  rownames(df) <- NULL
  list(data = df, dropped = as.integer(dropped), messages = messages)
}
