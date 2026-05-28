# =============================================================================
# ICES vocab + DATRAS abundance helpers (PR10-S scaffolding)
# =============================================================================
# Thin wrappers over icesVocab and icesDatras that follow EcoNeTool's
# established conventions:
#   - structured `list(success, source, data, error)` returns so callers
#     can branch on `$success` instead of `is.null()` (matches the recent
#     PR6 swallowed-error work)
#   - with_timeout() around live network calls
#   - in-memory cache via a module-private env so repeated calls within a
#     session don't re-hit ICES (avoids the duckdbfs-style finalizer bug
#     PR8 hit with FishBase)
#
# This is the minimum surface needed for downstream code to display ICES
# area names alongside informal region labels and to fetch abundance
# indices for a given species. Cache layer + biomass calibration
# (DATRAS scopes M / L per the PR10 scoping doc) are separate follow-ups.
# =============================================================================

# Module-private cache. Keyed by the call signature. Values are the
# already-wrapped result lists; callers don't see this directly.
.ices_cache <- new.env(parent = emptyenv())

# Official ICES Areas layer (gis.ices.dk GeoServer WFS, GeoJSON output).
# outputFormat=application/json is REQUIRED: that output is lon/lat with a
# null CRS (-> WGS84); the GML output is lat/lon (axis-swapped). Never add an
# srsName parameter. (Verified live; see the design spec.)
.ICES_AREAS_WFS_URL <- paste0(
  "https://gis.ices.dk/geoserver/ows?service=WFS&version=2.0.0",
  "&request=GetFeature&typeNames=ices_eg:ICES_AREAS_VISA_SIMPLE_5KM",
  "&outputFormat=application/json"
)
.ICES_AREAS_CACHE_FILE <- file.path("cache", "spatial", "ices_areas.gpkg")


#' List available ICES area codes
#'
#' Wraps icesVocab::getCodeList("ICES_Area"). Cached after the first
#' successful call.
#'
#' @param timeout Seconds before the live ICES vocab request is bounded.
#' @return list(success, source = "icesVocab", data, error). On
#'   success, `data` is the data.frame returned by getCodeList.
#' @export
get_ices_area_codes <- function(timeout = 30) {
  if (!is.null(.ices_cache$area_codes)) return(.ices_cache$area_codes)

  result <- list(
    success = FALSE,
    source  = "icesVocab",
    data    = NULL,
    error   = NA_character_
  )

  if (!requireNamespace("icesVocab", quietly = TRUE)) {
    result$error <- "icesVocab package not installed"
    return(result)
  }

  tryCatch({
    codes <- with_timeout(
      icesVocab::getCodeList("ICES_Area"),
      timeout = timeout, on_timeout = NULL
    )
    if (is.null(codes) || (is.data.frame(codes) && nrow(codes) == 0)) {
      result$error <- "icesVocab returned no codes (timeout or empty response)"
      return(result)
    }
    result$data    <- codes
    result$success <- TRUE
    .ices_cache$area_codes <- result
    result
  }, error = function(e) {
    warning("[get_ices_area_codes] ", conditionMessage(e), call. = FALSE)
    result$error <- conditionMessage(e)
    result
  })
}


#' Get descriptive metadata for one ICES area code
#'
#' @param code ICES area code (e.g., "27.3.d.27" for Baltic SD27).
#' @param timeout Seconds before the live ICES vocab request is bounded.
#' @return list(success, source, data, error). On success `data` holds
#'   the per-code detail row.
#' @export
get_ices_area_detail <- function(code, timeout = 30) {
  result <- list(
    success = FALSE,
    source  = "icesVocab",
    data    = NULL,
    error   = NA_character_
  )

  if (is.null(code) || is.na(code) || nchar(trimws(code)) == 0) {
    result$error <- "code is empty"
    return(result)
  }

  cache_key <- paste0("area_detail_", code)
  if (!is.null(.ices_cache[[cache_key]])) return(.ices_cache[[cache_key]])

  if (!requireNamespace("icesVocab", quietly = TRUE)) {
    result$error <- "icesVocab package not installed"
    return(result)
  }

  tryCatch({
    detail <- with_timeout(
      icesVocab::getCodeDetail("ICES_Area", code),
      timeout = timeout, on_timeout = NULL
    )
    if (is.null(detail) || (is.data.frame(detail) && nrow(detail) == 0)) {
      result$error <- sprintf("no detail returned for code '%s'", code)
      return(result)
    }
    result$data    <- detail
    result$success <- TRUE
    .ices_cache[[cache_key]] <- result
    result
  }, error = function(e) {
    warning("[get_ices_area_detail] ", conditionMessage(e), call. = FALSE)
    result$error <- conditionMessage(e)
    result
  })
}


#' Look up DATRAS abundance indices for a species
#'
#' Pulls survey indices from icesDatras for one species across one or
#' more surveys. Returns a tidy summary suitable for the trait-research
#' UI; downstream calibration of Ecopath biomass values from these
#' numbers is deferred to a future scope-L follow-up (the science of
#' reconciling CPUE with mass-balance constraints needs domain input).
#'
#' @param aphia_id WoRMS AphiaID for the species.
#' @param surveys Character vector of ICES survey codes to query.
#'   Defaults to the four MARBEFES study-region surveys.
#' @param years Optional integer vector of years; NULL = use the
#'   most recent year icesDatras reports for each survey.
#' @param timeout Per-call network budget in seconds.
#' @return list(success, source = "DATRAS", data, error). On success
#'   `data` is a data.frame with columns survey / year / quarter /
#'   biomass_index (or NA when icesDatras returns nothing for that
#'   survey-year combo). Failures populate `error` and warning() so
#'   the failure mode is visible.
#' @export
lookup_datras_indices <- function(aphia_id,
                                  surveys = c("BITS", "NS-IBTS",
                                              "BTS",  "BIAS"),
                                  years = NULL,
                                  timeout = 30) {
  result <- list(
    success = FALSE,
    source  = "DATRAS",
    data    = NULL,
    error   = NA_character_
  )

  if (is.null(aphia_id) || !is.numeric(aphia_id) || aphia_id <= 0) {
    result$error <- "aphia_id must be a positive numeric"
    return(result)
  }

  if (!requireNamespace("icesDatras", quietly = TRUE)) {
    result$error <- "icesDatras package not installed"
    return(result)
  }

  cache_key <- paste0("datras_", aphia_id, "_",
                      paste(surveys, collapse = "."),
                      if (is.null(years)) "" else paste0("_", paste(years, collapse = ".")))
  if (!is.null(.ices_cache[[cache_key]])) return(.ices_cache[[cache_key]])

  rows <- list()
  for (svy in surveys) {
    survey_years <- if (!is.null(years)) years else {
      yl <- tryCatch(with_timeout(
        icesDatras::getSurveyYearList(svy),
        timeout = timeout, on_timeout = NULL
      ), error = function(e) NULL)
      if (is.null(yl) || length(yl) == 0) {
        warning(sprintf("[lookup_datras_indices] no year list for survey '%s'", svy),
                call. = FALSE)
        next
      }
      max(as.integer(yl), na.rm = TRUE)  # most recent year
    }

    for (yr in survey_years) {
      idx <- tryCatch(with_timeout(
        icesDatras::getIndices(svy, yr, quarter = -1),
        timeout = timeout, on_timeout = NULL
      ), error = function(e) {
        warning(sprintf("[lookup_datras_indices] %s %d failed: %s",
                        svy, yr, conditionMessage(e)), call. = FALSE)
        NULL
      })

      if (is.null(idx) || nrow(idx) == 0) next

      # icesDatras returns indices keyed on "Species" (Latin name) or
      # "AphiaID" depending on the survey. Filter by AphiaID when
      # available, fall back to a no-op when it isn't.
      if ("AphiaID" %in% names(idx)) {
        idx <- idx[as.integer(idx$AphiaID) == as.integer(aphia_id), , drop = FALSE]
      }
      if (nrow(idx) == 0) next

      rows[[length(rows) + 1L]] <- data.frame(
        survey  = svy,
        year    = yr,
        quarter = if ("Quarter" %in% names(idx)) idx$Quarter[1] else NA_integer_,
        biomass_index = if ("Index" %in% names(idx)) idx$Index[1] else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    result$error <- sprintf("no DATRAS rows for AphiaID %s in surveys [%s]",
                            aphia_id, paste(surveys, collapse = ", "))
    return(result)
  }

  result$data    <- do.call(rbind, rows)
  result$success <- TRUE
  .ices_cache[[cache_key]] <- result
  result
}


#' Look up the ICES Statistical Area code for a (lon, lat) point
#'
#' Point-in-polygon against the ICES Areas layer. Returns the MOST SPECIFIC
#' code the layer carries for the point (a Division, Subdivision, or Unit;
#' see the design spec) - despite the name, not always a Subdivision.
#'
#' @param lon,lat Numeric scalars, WGS84 degrees.
#' @param layer_path Optional local .gpkg/.geojson override; NULL = cache/download.
#' @param timeout Network budget (s) for the one-time layer fetch.
#' @return list(success, source = "ICES_GIS_WFS", data, error). On success,
#'   `data` is a 1-row data.frame: area_full, major_fa, subarea, division,
#'   subdivision, unit (component columns may be "").
#' @export
lookup_ices_subdivision <- function(lon, lat, layer_path = NULL, timeout = 60) {
  result <- list(success = FALSE, source = "ICES_GIS_WFS",
                 data = NULL, error = NA_character_)

  if (!is.numeric(lon) || !is.numeric(lat) ||
      length(lon) != 1 || length(lat) != 1 ||
      !is.finite(lon) || !is.finite(lat) ||
      lon < -180 || lon > 180 || lat < -90 || lat > 90) {
    result$error <- "lon/lat invalid (need finite scalars in [-180,180]/[-90,90])"
    return(result)
  }

  if (!requireNamespace("sf", quietly = TRUE)) {
    result$error <- "sf package not installed"
    return(result)
  }

  areas <- tryCatch(
    .load_ices_areas(layer_path = layer_path, timeout = timeout),
    error = function(e) {
      warning("[lookup_ices_subdivision] layer load failed: ",
              conditionMessage(e), call. = FALSE)
      result$error <<- conditionMessage(e)   # <<- mutates the OUTER result
      NULL
    }
  )
  if (is.null(areas)) return(result)

  # Point-in-polygon. The 5km-simplified polygons are S2-invalid, so run
  # planar (S2 off) and restore on exit (mirrors emodnet_habitat_utils.R).
  old_s2 <- sf::sf_use_s2(); sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(old_s2), add = TRUE)

  point <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
  point <- sf::st_transform(point, sf::st_crs(areas))
  hits  <- sf::st_filter(areas, point, .predicate = sf::st_intersects)

  if (nrow(hits) == 0) {
    result$error <- "no ICES area at this point (on land or outside ICES Area 27)"
    return(result)
  }
  if (nrow(hits) > 1) {
    # Deterministic tie-break: longest Area_Full first (most specific), then
    # lexicographic as a stable secondary key so equal-nchar ties don't depend
    # on WFS feature order. Warn so non-determinism is visible.
    af <- as.character(hits$Area_Full)
    hits <- hits[order(nchar(af), af, decreasing = TRUE), ][1, ]
    warning("[lookup_ices_subdivision] point on a shared boundary; returning ",
            "most specific area ", hits$Area_Full[1], call. = FALSE)
  }

  .clean <- function(x) {
    v <- trimws(as.character(x))
    ifelse(is.na(v), "", v)
  }
  result$data <- data.frame(
    area_full   = .clean(hits$Area_Full[1]),
    major_fa    = .clean(hits$Major_FA[1]),
    subarea     = .clean(hits$SubArea[1]),
    division    = .clean(hits$Division[1]),
    subdivision = .clean(hits$SubDivisio[1]),
    unit        = .clean(hits$Unit[1]),
    stringsAsFactors = FALSE
  )
  result$success <- TRUE
  result
}


# TEMPORARY stub - full version (download + cache) lands in Task 4.
.load_ices_areas <- function(layer_path = NULL, timeout = 60) {
  if (!is.null(.ices_cache$areas_sf)) return(.ices_cache$areas_sf)
  stop("ICES areas layer not loaded (stub)")
}
