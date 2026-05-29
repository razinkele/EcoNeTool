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
# Resolved at source-time by walking up from cwd looking for app.R, so the
# cache lands at <project-root>/cache/spatial/... regardless of whether the
# caller is the Shiny app (cwd = project root) or a testthat block (cwd =
# tests/testthat/). Falls back to the bare relative path if app.R isn't
# found within 8 levels (preserves the previous behaviour).
.ICES_AREAS_CACHE_FILE <- local({
  d <- getwd()
  for (i in 1:8) {
    if (file.exists(file.path(d, "app.R"))) {
      return(file.path(d, "cache", "spatial", "ices_areas.gpkg"))
    }
    parent <- dirname(d)
    if (parent == d) break
    d <- parent
  }
  file.path("cache", "spatial", "ices_areas.gpkg")
})


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


#' Reshape a getIndices() frame into tidy abundance rows
#'
#' icesDatras::getIndices() returns one row per IndexArea (e.g. BITS splits
#' Baltic cod into East/West stocks) and spreads the abundance across
#' Age_0..Age_N columns — there is NO single "Index" column (verified live
#' against icesDatras 1.4.1; see the R3 design spec). This collapses each
#' row's age columns into a numeric `abundance_index` (total numbers-per-hour
#' summed across ages, NA treated as 0) and keeps `index_area` as a row
#' dimension so stock-area structure (East/West cod, roundfish areas) isn't
#' silently dropped. The data is abundance, NOT biomass.
#'
#' Age column names from the WFS can be messy (e.g. `Age_7 xsi:nil="true"`),
#' so they are matched by the `^Age_` prefix and coerced defensively.
#'
#' @param idx data.frame as returned by getIndices().
#' @param svy,yr Survey code / year, used as fallbacks if the frame lacks
#'   Survey/Year columns.
#' @return data.frame: survey, year, quarter, index_area, abundance_index.
#' @keywords internal
.datras_reshape_indices <- function(idx, svy, yr) {
  age_cols <- grep("^Age_", names(idx), value = TRUE)
  if (length(age_cols) == 0) {
    abundance_index <- rep(NA_real_, nrow(idx))
  } else {
    age_mat <- suppressWarnings(
      vapply(idx[age_cols],
             function(col) as.numeric(as.character(col)),
             numeric(nrow(idx)))
    )
    if (is.null(dim(age_mat))) age_mat <- matrix(age_mat, nrow = nrow(idx))
    abundance_index <- rowSums(age_mat, na.rm = TRUE)
  }

  data.frame(
    survey          = if ("Survey" %in% names(idx)) as.character(idx$Survey) else svy,
    year            = if ("Year" %in% names(idx)) as.integer(idx$Year) else as.integer(yr),
    quarter         = if ("Quarter" %in% names(idx)) as.integer(idx$Quarter) else NA_integer_,
    index_area      = if ("IndexArea" %in% names(idx)) as.character(idx$IndexArea) else NA_character_,
    abundance_index = abundance_index,
    stringsAsFactors = FALSE
  )
}


#' Look up DATRAS abundance indices for a species
#'
#' Pulls survey indices from icesDatras for one species across one or
#' more surveys. Returns a tidy summary suitable for the trait-research
#' UI; downstream calibration of Ecopath biomass values from these
#' numbers is deferred to a future scope-L follow-up (the science of
#' reconciling CPUE with mass-balance constraints needs domain input).
#'
#' @param aphia_id WoRMS AphiaID for the species. Passed to getIndices()
#'   as `species`, so DATRAS filters server-side.
#' @param surveys Character vector of ICES DATRAS survey codes to query.
#'   Defaults to the three MARBEFES study-region trawl surveys. (BIAS is
#'   NOT a DATRAS survey - it is the Baltic acoustic survey, whose indices
#'   live in a different ICES database - so it is excluded.)
#' @param years Optional integer vector of years; NULL = use the most
#'   recent year that actually has computed indices for each survey.
#' @param timeout Per-call network budget in seconds.
#' @return list(success, source = "DATRAS", data, error). On success
#'   `data` is a data.frame with columns survey / year / quarter /
#'   index_area / abundance_index. `abundance_index` is the sum across
#'   the Age_0..Age_N columns getIndices() returns (total numbers-per-hour,
#'   abundance NOT biomass); one row per (survey, year, quarter, IndexArea)
#'   so stock-area structure (e.g. East/West Baltic cod) is preserved.
#'   Failures populate `error` and warning() so the failure mode is visible.
#' @export
lookup_datras_indices <- function(aphia_id,
                                  surveys = c("BITS", "NS-IBTS", "BTS"),
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

  # When `years` is NULL we search newest-first and stop at the first year
  # that actually returns indices, but only step back a bounded number of
  # years so a species genuinely absent from a survey doesn't trigger a long
  # scan back through the whole time series.
  max_year_lookback <- 3L

  rows <- list()
  for (svy in surveys) {
    survey_years <- if (!is.null(years)) {
      sort(as.integer(years), decreasing = TRUE)
    } else {
      yl <- tryCatch(with_timeout(
        icesDatras::getSurveyYearList(svy),
        timeout = timeout, on_timeout = NULL
      ), error = function(e) NULL)
      if (is.null(yl) || length(yl) == 0) {
        warning(sprintf("[lookup_datras_indices] no year list for survey '%s'", svy),
                call. = FALSE)
        next
      }
      # newest years first, bounded to the lookback window
      head(sort(as.integer(yl), decreasing = TRUE), max_year_lookback)
    }

    got_rows_for_survey <- FALSE
    for (yr in survey_years) {
      # Step-back stops at the most recent indexed year ONLY when `years`
      # was NULL; explicit years are all returned.
      if (is.null(years) && isTRUE(got_rows_for_survey)) break
      # getIndices requires a real quarter (it rejects quarter = -1 with a
      # bare FALSE); resolve the survey-year's actual quarters first.
      quarters <- tryCatch(with_timeout(
        icesDatras::getSurveyYearQuarterList(svy, yr),
        timeout = timeout, on_timeout = NULL
      ), error = function(e) NULL)
      if (is.null(quarters) || length(quarters) == 0) {
        warning(sprintf("[lookup_datras_indices] no quarter list for %s %d", svy, yr),
                call. = FALSE)
        next
      }

      for (q in as.integer(quarters)) {
        # Pass species = aphia_id so DATRAS filters server-side. (getIndices
        # returns one row per IndexArea, with abundance spread across
        # Age_0..Age_N columns - reshaped by .datras_reshape_indices.)
        idx <- tryCatch(with_timeout(
          icesDatras::getIndices(svy, yr, quarter = q, species = aphia_id),
          timeout = timeout, on_timeout = NULL
        ), error = function(e) {
          warning(sprintf("[lookup_datras_indices] %s %d Q%s failed: %s",
                          svy, yr, q, conditionMessage(e)), call. = FALSE)
          NULL
        })

        # Guard the nrow() check: getIndices returns a bare FALSE (logical)
        # for an unavailable quarter, and nrow(FALSE) is NULL -> `if (NULL == 0)`
        # errors. Surface the upstream shape via warning() and skip the quarter.
        if (is.null(idx)) next
        if (!is.data.frame(idx)) {
          warning(sprintf("[lookup_datras_indices] %s %d Q%s: icesDatras returned %s, not a data.frame; skipping",
                          svy, yr, q, paste(class(idx), collapse = "/")),
                  call. = FALSE)
          next
        }
        if (nrow(idx) == 0) next

        rows[[length(rows) + 1L]] <- .datras_reshape_indices(idx, svy, yr)
        got_rows_for_survey <- TRUE  # this year had indices; stop stepping back
      }
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


#' Load (and cache) the ICES Areas sf layer. Source precedence:
#' in-session cache -> layer_path -> on-disk gpkg cache -> WFS download.
.load_ices_areas <- function(layer_path = NULL, timeout = 60) {
  if (!is.null(.ices_cache$areas_sf)) return(.ices_cache$areas_sf)

  src <- NULL
  write_cache <- FALSE
  if (!is.null(layer_path) && file.exists(layer_path)) {
    src <- layer_path
  } else if (file.exists(.ICES_AREAS_CACHE_FILE)) {
    src <- .ICES_AREAS_CACHE_FILE
  } else {
    # Two-step download: options(timeout) bounds libcurl C-level I/O (a real
    # network timeout); with_timeout()/setTimeLimit would NOT bound GDAL/curl.
    tmp <- tempfile(fileext = ".geojson")
    old_to <- options(timeout = timeout)
    on.exit(options(old_to), add = TRUE)
    utils::download.file(.ICES_AREAS_WFS_URL, tmp,
                         mode = "wb", method = "libcurl", quiet = TRUE)
    src <- tmp
    write_cache <- TRUE
  }

  areas <- sf::st_read(src, quiet = TRUE)
  if (is.na(sf::st_crs(areas))) sf::st_crs(areas) <- 4326   # GeoJSON crs:null
  areas <- sf::st_make_valid(areas)                         # 5km polys S2-invalid

  if (isTRUE(write_cache)) {
    dir.create(dirname(.ICES_AREAS_CACHE_FILE),
               recursive = TRUE, showWarnings = FALSE)
    sf::st_write(areas, .ICES_AREAS_CACHE_FILE, quiet = TRUE, delete_dsn = TRUE)
  }

  .ices_cache$areas_sf <- areas
  areas
}
