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

# AphiaIDs for which BITS getIndices() actually computes survey indices.
# Verified empirically against icesDatras (BITS Q1/Q4, 2020-2026): only these
# three return data; dab/turbot/brill/eelpout/etc. have NO BITS index, so a
# group resolving outside this set is classed "not_surveyed" (excluded up front,
# no wasted DATRAS fetch). BITS-specific; revisit if other surveys are added.
SURVEY_TRENDS_BITS_INDEXED_APHIA <- c(
  126436,  # Gadus morhua        (cod)
  127141,  # Platichthys flesus  (flounder)
  127143   # Pleuronectes platessa (plaice)
)

# Clupeid AphiaID -> ICES SAG stock key. BITS bottom-trawl can't index
# herring/sprat (the dominant Baltic groups); instead their assessed SSB
# time series comes from SAG (icesSAG). Trend-only: SSB units differ by stock
# (sprat = tonnes, central Baltic herring = relative "ratio"), so only the
# relative trend is comparable, not the level. Names are AphiaIDs as strings.
SURVEY_TRENDS_CLUPEID_SAG <- c(
  "126417" = "her.27.25-2932",  # Clupea harengus, central Baltic herring
  "126425" = "spr.27.22-32"     # Sprattus sprattus, Baltic sprat
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

#' Collapse an icesSAG getSummaryTable() frame to one SSB value per year
#'
#' The clupeid (herring/sprat) analogue of aggregate_survey_series: feeds the
#' same compute_survey_trends() contract from SAG SSB instead of DATRAS
#' abundance. SSB-NA years are dropped.
#'
#' @param summary_df data.frame from icesSAG::getSummaryTable() (cols incl.
#'   Year, SSB).
#' @return data.frame(year, survey_value), one row per year with a non-NA SSB.
#' @keywords internal
aggregate_sag_ssb <- function(summary_df) {
  if (!all(c("Year", "SSB") %in% names(summary_df))) {
    return(data.frame(year = integer(0), survey_value = numeric(0),
                      stringsAsFactors = FALSE))
  }
  yr <- as.integer(summary_df$Year)
  ssb <- suppressWarnings(as.numeric(summary_df$SSB))
  keep <- !is.na(ssb) & !is.na(yr)
  data.frame(year = yr[keep], survey_value = ssb[keep], stringsAsFactors = FALSE)
}

#' Collapse a curated BIAS acoustic frame to one survey_value per year
#'
#' The acoustic (herring/sprat) analogue of aggregate_survey_series /
#' aggregate_sag_ssb: feeds the same compute_survey_trends() contract from the
#' curated BIAS index CSV. Per-SD rows are summed per year (mirrors the BITS
#' area-summing path) but the sum is SCOPED TO ONE STOCK: an AphiaID can span
#' several ICES stocks (herring 126417 -> her.27.25-2932 / her.27.28 / ...), and
#' summing across them would be a cross-stock body-of-fish confound. A single NA
#' among a year's SD rows makes the whole-year sum NA (na.rm = FALSE), which
#' warns then drops the year (matching aggregate_survey_series). A non-numeric
#' (unparseable) index value warns distinctly so a transcription error is not
#' silently read as "BIAS lacked that year".
#'
#' @param bias_df data.frame from R/config/bias_indices.csv (must contain
#'   aphia_id, year, abundance_index; stock used when stock_key is given).
#' @param aphia_id Numeric WoRMS AphiaID to filter to.
#' @param stock_key Optional ICES stock key; when supplied (and a `stock` column
#'   exists) rows are additionally filtered to that single stock.
#' @return data.frame(year, survey_value), one row per year with a non-NA summed
#'   index. Empty frame (canonical schema) if required columns are missing or no
#'   rows match.
#' @keywords internal
aggregate_bias_series <- function(bias_df, aphia_id, stock_key = NULL) {
  if (!all(c("aphia_id", "year", "abundance_index") %in% names(bias_df))) {
    return(data.frame(year = integer(0), survey_value = numeric(0),
                      stringsAsFactors = FALSE))
  }
  keep <- as.integer(bias_df$aphia_id) == as.integer(aphia_id)
  if (!is.null(stock_key) && "stock" %in% names(bias_df)) {
    keep <- keep & bias_df$stock == stock_key
  }
  df <- bias_df[keep, , drop = FALSE]
  if (nrow(df) == 0) {
    return(data.frame(year = integer(0), survey_value = numeric(0),
                      stringsAsFactors = FALSE))
  }
  raw <- df$abundance_index
  num <- suppressWarnings(as.numeric(raw))
  unparsed <- is.na(num) & !is.na(raw) & nzchar(trimws(as.character(raw)))
  if (any(unparsed)) {
    warning(sprintf("[survey_trends] non-numeric abundance_index for aphia %d (check CSV): %s",
                    as.integer(aphia_id),
                    paste(unique(as.character(raw[unparsed])), collapse = ", ")),
            call. = FALSE)
  }
  years <- sort(unique(as.integer(df$year)))
  survey_value <- vapply(years, function(yr) {
    v <- num[as.integer(df$year) == yr]
    if (anyNA(v)) {
      warning(sprintf("[survey_trends] NA BIAS index in sum for aphia %d year %d",
                      as.integer(aphia_id), yr), call. = FALSE)
    }
    sum(v, na.rm = FALSE)
  }, numeric(1))
  out <- data.frame(year = years, survey_value = survey_value, stringsAsFactors = FALSE)
  out[!is.na(out$survey_value), , drop = FALSE]
}

#' Resolve an Ecopath group name to a single AphiaID + class
#'
#' Dictionary first; else worrms::wm_name2id with the R3 scalar guard (a
#' multi-match yields NA, never a silent first element). Classifies the result
#' as demersal / pelagic (excluded) / unmapped.
#'
#' @param group_name Character Ecopath group name.
#' @param dictionary data.frame(group, aphia_id).
#' @param pelagic_set Numeric vector of pelagic AphiaIDs to exclude.
#' @param bits_indexed Optional numeric vector of AphiaIDs BITS getIndices
#'   actually computes; a resolved, non-pelagic AphiaID outside this set is
#'   classed "not_surveyed". NULL (default) disables the check (any non-pelagic
#'   resolved name -> "demersal").
#' @return list(aphia_id = numeric scalar or NA_real_, class = chr:
#'   demersal/pelagic/not_surveyed/unmapped).
#' @keywords internal
.map_group_to_aphia <- function(group_name, dictionary, pelagic_set,
                                bits_indexed = NULL) {
  hit <- dictionary$aphia_id[dictionary$group == group_name]
  aid <- if (length(hit) == 1 && !is.na(hit) && hit > 0) {
    as.numeric(hit)
  } else {
    raw <- tryCatch(
      worrms::wm_name2id(group_name),
      error = function(e) {
        warning(sprintf("[survey_trends] wm_name2id failed for '%s': %s",
                        group_name, conditionMessage(e)), call. = FALSE)
        NA_real_
      }
    )
    if (length(raw) == 1 && !is.na(raw) && raw > 0) as.numeric(raw) else NA_real_
  }
  class <- if (is.na(aid)) {
    "unmapped"
  } else if (aid %in% pelagic_set) {
    "pelagic"
  } else if (!is.null(bits_indexed) && !(aid %in% bits_indexed)) {
    "not_surveyed"
  } else {
    "demersal"
  }
  list(aphia_id = aid, class = class)
}

#' Per-group DATRAS survey trend (trend-only; no cross-group comparison)
#'
#' @param series_df data.frame(group, year, survey_value, is_ref_year). Per-group
#'   is_ref_year (so the no-common-year fallback needs no scalar ref year).
#' @return list(success, status, trends, excluded):
#'   trends = df(group, year, rel, ref_rank, n_years, direction) long format;
#'   excluded = df(group, reason). Degenerate series are excluded + reported,
#'   never normalised to NaN/Inf.
#' @keywords internal
compute_survey_trends <- function(series_df) {
  trends_list <- list()
  excluded_list <- list()

  add_excluded <- function(g, reason) {
    excluded_list[[length(excluded_list) + 1L]] <<-
      data.frame(group = g, reason = reason, stringsAsFactors = FALSE)
  }

  for (g in unique(series_df$group)) {
    s <- series_df[series_df$group == g, , drop = FALSE]
    if (anyDuplicated(s$year)) {
      warning(sprintf("[survey_trends] duplicate (group, year) for '%s'; de-duping", g),
              call. = FALSE)
      s <- s[!duplicated(s$year), , drop = FALSE]
    }
    s <- s[order(s$year), , drop = FALSE]
    s <- s[!is.na(s$survey_value), , drop = FALSE]
    n <- nrow(s)
    if (n < 3) {
      add_excluded(g, sprintf("trend omitted: %d survey year(s)", n))
      next
    }
    m <- mean(s$survey_value)
    if (is.na(m) || m == 0) {
      add_excluded(g, "all-zero or all-NA survey series")
      next
    }

    rel <- s$survey_value / m
    ref_idx <- which(s$is_ref_year %in% TRUE)
    ref_rank <- if (length(ref_idx) == 1L) {
      rank(s$survey_value, ties.method = "average")[ref_idx]
    } else {
      NA_real_
    }

    direction <- NA_character_
    if (n >= 5) {
      recent <- mean(utils::tail(s$survey_value, 3))
      earlier <- mean(utils::head(s$survey_value, n - 3))
      delta <- if (is.na(earlier) || earlier == 0) NA_real_ else (recent - earlier) / earlier
      direction <- if (is.na(delta)) NA_character_
                   else if (abs(delta) < 0.10) "flat"
                   else if (delta > 0) "up"
                   else "down"
    }

    trends_list[[length(trends_list) + 1L]] <- data.frame(
      group = g, year = s$year, rel = rel,
      ref_rank = ref_rank, n_years = n, direction = direction,
      stringsAsFactors = FALSE
    )
  }

  trends <- if (length(trends_list)) do.call(rbind, trends_list) else NULL
  excluded <- if (length(excluded_list)) {
    do.call(rbind, excluded_list)
  } else {
    data.frame(group = character(0), reason = character(0), stringsAsFactors = FALSE)
  }

  ok <- !is.null(trends) && length(unique(trends$group)) >= 1L
  list(
    success = ok,
    status  = if (ok) "ok" else "insufficient",
    trends  = trends,
    excluded = excluded
  )
}
