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
