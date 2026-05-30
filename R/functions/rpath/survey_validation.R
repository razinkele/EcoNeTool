# ==============================================================================
# R2 DATRAS Survey Trends - pure core (no Shiny / no network)
# ==============================================================================
# Compares an Ecopath model's demersal biomass inputs against ICES DATRAS (BITS)
# survey ABUNDANCE trends, per group across years. Trend-only: no cross-group
# comparison (every cross-group biomass-vs-abundance view is body-weight
# confounded; see the design spec). Read-only diagnostic.
# ==============================================================================

# Curated pelagic AphiaIDs that BITS bottom-trawl under-samples (assessed via
# BIAS acoustic, not DATRAS). Excluded from the demersal trend by default.
SURVEY_TRENDS_PELAGIC_APHIA <- c(
  126417,  # Clupea harengus  (herring)
  126425   # Sprattus sprattus (sprat)
)

#' Collapse a getIndices-shaped frame to one survey_value per year
#'
#' @param reshaped_df data.frame from lookup_datras_indices()$data, columns
#'   survey/year/quarter/index_area/abundance_index.
#' @param quarter Integer quarter to keep (e.g. 1 for BITS Q1).
#' @param areas Character vector of index_area values to sum; NULL = all.
#' @return data.frame(year, survey_value), one row per year. na.rm = FALSE on
#'   the per-year sum so a missing area-year surfaces as NA + warning().
#' @keywords internal
aggregate_survey_series <- function(reshaped_df, quarter, areas = NULL) {
  df <- reshaped_df[reshaped_df$quarter == quarter, , drop = FALSE]
  if (!is.null(areas)) {
    df <- df[df$index_area %in% areas, , drop = FALSE]
  }
  if (nrow(df) == 0) {
    return(data.frame(year = integer(0), survey_value = numeric(0),
                      stringsAsFactors = FALSE))
  }
  years <- sort(unique(as.integer(df$year)))
  survey_value <- vapply(years, function(yr) {
    v <- df$abundance_index[as.integer(df$year) == yr]
    if (anyNA(v)) {
      warning(sprintf("[survey_trends] NA abundance in area sum for year %d", yr),
              call. = FALSE)
    }
    sum(v, na.rm = FALSE)
  }, numeric(1))
  data.frame(year = years, survey_value = survey_value, stringsAsFactors = FALSE)
}
