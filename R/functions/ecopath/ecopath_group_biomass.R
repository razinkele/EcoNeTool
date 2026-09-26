# ==============================================================================
# ECOPATH GROUP BIOMASS
# ==============================================================================
# One definition of "group biomass" shared by the network import
# (ecopath_import_server.R) and the Rpath conversion (rpath_conversion.R), so
# the two paths agree by construction.
#
# EwE field semantics (verified 2026-09-26 against
# examples/Coastal model EE 1.ewemdb and examples/LT2022_0.5ST_final7.eweaccdb
# read through parse_ecopath_native_cross_platform()):
#   - EcopathGroup carries `Biomass` and `Area`; there is no separate
#     total-area biomass column (no BiomassAreaInput or similar), and the
#     derived columns (`Production`, `Consumption`) are stored as 0, i.e. the
#     table holds the user's INPUTS only.
#   - The EwE basic-input field is "Biomass in habitat area" (t/km2) and
#     `Area` is the habitat-area fraction (0-1) of the model area. Ecopath's
#     total-area biomass is Biomass x Area.
#   - Coastal model EE 1 has five groups with Area < 1 (four macrozoobenthos
#     groups at 0.2, Polychaetes at 0.1); LT2022 has Area = 1 throughout.
# Rpath has no habitat-area parameter, so it must be given the total-area
# biomass. Before this helper the network path multiplied by Area and the
# Rpath path did not, so Rpath balanced those five groups on 5-10x their
# model-area biomass.
# ==============================================================================

#' Total-area biomass of each EwE group
#'
#' @param group_table EwE group table (e.g. `group_data` from
#'   parse_ecopath_native_cross_platform()).
#' @param biomass_col Name of the biomass-in-habitat-area column.
#' @param area_col Name of the habitat-area fraction column; if absent (or
#'   NULL) every group is taken to occupy the whole model area.
#' @return Numeric vector, one value per row: Biomass x Area. Missing biomass
#'   (NA, the EwE -9999 sentinel, or negative) stays NA_real_ so Ecopath can
#'   estimate it; callers apply their own defaults. A missing, sentinel or
#'   non-positive Area counts as 1.
#' @export
ewe_group_biomass <- function(group_table, biomass_col = "Biomass", area_col = "Area") {
  if (is.null(group_table) || !biomass_col %in% names(group_table)) {
    stop(sprintf("ewe_group_biomass(): group table has no '%s' column", biomass_col),
         call. = FALSE)
  }
  biomass <- suppressWarnings(as.numeric(group_table[[biomass_col]]))
  biomass[is.na(biomass) | biomass < 0] <- NA_real_

  area <- if (!is.null(area_col) && area_col %in% names(group_table)) {
    suppressWarnings(as.numeric(group_table[[area_col]]))
  } else {
    rep(1, length(biomass))
  }
  bad_area <- !is.na(area) & area > -9000 & area <= 0
  if (any(bad_area)) {
    names_col <- intersect(c("GroupName", "Group"), names(group_table))
    labels <- if (length(names_col) > 0) group_table[[names_col[1]]][bad_area] else which(bad_area)
    warning(sprintf("[ewe_group_biomass] non-positive habitat area treated as 1 for: %s",
                    paste(labels, collapse = ", ")), call. = FALSE)
  }
  area[is.na(area) | area <= 0] <- 1

  biomass * area
}
