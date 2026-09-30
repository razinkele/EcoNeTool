# ==============================================================================
# SHARK4R API UTILITIES
# ==============================================================================
# Wrappers for the SHARK Data tab around SHARK4R (>= 1.2.0):
# - taxonomy: WoRMS (no key), Dyntaxa (DYNTAXA_KEY), AlgaeBase (ALGAEBASE_KEY)
# - data: SHARK physical-chemical and biological records (get_shark_data)
# - quality control: SHARK field and outlier checks, plus local checks
#
# SHARK4R 1.2.0 removed every sharkdata_* function the tab used to call. The
# functions called here are listed in SHARK4R_FUNCTIONS; test-shark-rewire.R
# fails if a SHARK4R:: call is not listed or not exported.
#
# Documentation: https://sharksmhi.github.io/SHARK4R/
# ==============================================================================

SHARK4R_MIN_VERSION <- "1.2.0"

# Every SHARK4R function the app calls (here and in database_lookups.R).
SHARK4R_FUNCTIONS <- c(
  "match_worms_taxa", "match_dyntaxa_taxa", "parse_scientific_names",
  "match_algaebase_taxa", "get_shark_data", "translate_shark_datatype",
  "check_fields", "check_outliers"
)

# SHARK answers slowly (25 s for 96 rows, 43 s for an empty taxon query).
SHARK_QUERY_TIMEOUT_S <- 90

# Exact SHARK parameter names (get_shark_options()$parameters, 2026-09-29).
SHARK_ENV_PARAMETERS <- c(
  "Temperature (CTD)" = "Temperature CTD",
  "Salinity (CTD)" = "Salinity CTD",
  "Dissolved oxygen (CTD)" = "Dissolved oxygen O2 CTD",
  "pH" = "pH",
  "Phosphate" = "Phosphate PO4-P",
  "Nitrate" = "Nitrate NO3-N",
  "Chlorophyll-a" = "Chlorophyll-a",
  "Secchi depth" = "Secchi depth"
)

# SHARK data types that SHARK4R::check_fields() knows (after
# translate_shark_datatype()), in SHARK's own spelling.
SHARK_QC_DATATYPES <- c(
  "Bacterioplankton", "Chlorophyll", "Epibenthos", "Grey seal", "Harbour Porpoise",
  "Harbour seal", "Physical and Chemical", "Phytoplankton", "Picoplankton",
  "Primary production", "Ringed seal", "Seal pathology", "Sedimentation",
  "Zoobenthos", "Zooplankton"
)

SHARK4R_MISSING_MESSAGE <- "SHARK4R 1.2.0 or newer is not installed"

#' Is a usable SHARK4R installed? (does not load the package)
#'
#' Reads the installed DESCRIPTION only, so the UI can call it at start-up
#' without loading SHARK4R and its sf/terra dependencies.
#' @return Logical.
shark4r_installed <- function() {
  if (!nzchar(system.file(package = "SHARK4R"))) return(FALSE)
  isTRUE(utils::packageVersion("SHARK4R") >= SHARK4R_MIN_VERSION)
}

#' Check that SHARK4R is installed and exports every function the app calls
#'
#' @return Logical. Warns when the installed SHARK4R lacks functions from
#'   SHARK4R_FUNCTIONS.
check_shark4r_available <- function() {
  if (!shark4r_installed()) return(FALSE)
  missing <- setdiff(SHARK4R_FUNCTIONS, getNamespaceExports("SHARK4R"))
  if (length(missing) > 0) {
    warning(sprintf("[shark] SHARK4R %s lacks: %s", as.character(utils::packageVersion("SHARK4R")),
                    paste(missing, collapse = ", ")), call. = FALSE)
    return(FALSE)
  }
  TRUE
}

#' Subscription key for a key-gated taxonomy source
#'
#' @param source "dyntaxa" or "algaebase".
#' @return The key, or NA when the environment variable is unset or empty.
shark_subscription_key <- function(source) {
  var <- switch(source, dyntaxa = "DYNTAXA_KEY", algaebase = "ALGAEBASE_KEY",
                stop("unknown taxonomy source: ", source))
  key <- Sys.getenv(var, "")
  if (nzchar(key)) key else NA_character_
}

#' Taxonomy sources the SHARK tab can query on this server
#'
#' WoRMS needs no key. Dyntaxa and AlgaeBase are offered only when their
#' subscription key is set (DYNTAXA_KEY / ALGAEBASE_KEY).
#' @return Named character vector for checkboxGroupInput(choices = ).
shark_taxonomy_source_choices <- function() {
  choices <- c("WoRMS (World Register)" = "worms")
  if (!is.na(shark_subscription_key("dyntaxa"))) {
    choices <- c("Dyntaxa (Swedish Taxonomy)" = "dyntaxa", choices)
  }
  if (!is.na(shark_subscription_key("algaebase"))) {
    choices <- c(choices, "AlgaeBase (Algae Database)" = "algaebase")
  }
  choices
}

# ==============================================================================
# CACHE (successful taxonomy lookups only, 30 days)
# ==============================================================================

.shark_cache_file <- function(cache_dir, prefix, name) {
  file.path(cache_dir, paste0(prefix, "_", gsub("[^a-zA-Z0-9]", "_", name), ".rds"))
}

.shark_cache_read <- function(file) {
  if (!file.exists(file)) return(NULL)
  cached <- tryCatch(readRDS(file), error = function(e) NULL)
  if (!is.list(cached) || !is.list(cached$data) || !identical(cached$data$status, "found")) return(NULL)
  if (difftime(Sys.time(), cached$timestamp, units = "days") >= 30) return(NULL)
  cached$data
}

.shark_cache_write <- function(file, data) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(data = data, timestamp = Sys.time()), file)
}

# ==============================================================================
# TAXONOMY FUNCTIONS
# ==============================================================================
# Each returns list(source, query, status, message, ...). status is "found",
# "not_found", "no_key" or "error"; the other fields are set when "found".

#' Query WoRMS via SHARK4R::match_worms_taxa()
#'
#' @param species_name Character, species name.
#' @param fuzzy Logical, fuzzy WoRMS name matching.
#' @param use_cache Logical, read and write the 30-day cache.
#' @param cache_dir Character, cache directory.
#' @return Taxonomy result list (see above) with scientific_name, aphia_id,
#'   authority, taxon_status, kingdom, phylum, class, order, family, genus, rank.
query_shark_worms <- function(species_name, fuzzy = TRUE, use_cache = TRUE,
                              cache_dir = app_path("cache", "shark")) {
  base <- list(source = "WoRMS (SHARK4R)", query = species_name, status = "error")
  if (!check_shark4r_available()) return(utils::modifyList(base, list(message = SHARK4R_MISSING_MESSAGE)))

  cache_file <- .shark_cache_file(cache_dir, "shark_worms", species_name)
  if (use_cache) {
    hit <- .shark_cache_read(cache_file)
    if (!is.null(hit)) return(hit)
  }

  failure <- NULL
  res <- tryCatch(
    SHARK4R::match_worms_taxa(species_name, fuzzy = isTRUE(fuzzy), best_match_only = TRUE,
                              max_retries = 1, sleep_time = 0, verbose = FALSE),
    error = function(e) {
      failure <<- conditionMessage(e)
      warning(sprintf("[shark] WoRMS lookup failed for '%s': %s", species_name, failure), call. = FALSE)
      NULL
    }
  )
  if (!is.null(failure)) return(utils::modifyList(base, list(message = failure)))

  aphia_id <- if (NROW(res) > 0) suppressWarnings(as.integer(.scalar_chr(res$AphiaID))) else NA_integer_
  if (is.na(aphia_id)) return(utils::modifyList(base, list(status = "not_found")))

  result <- utils::modifyList(base, list(
    status = "found",
    scientific_name = .scalar_chr(res$scientificname),
    aphia_id = aphia_id,
    authority = .scalar_chr(res$authority),
    taxon_status = .scalar_chr(res$status),
    kingdom = .scalar_chr(res$kingdom),
    phylum = .scalar_chr(res$phylum),
    class = .scalar_chr(res$class),
    order = .scalar_chr(res$order),
    family = .scalar_chr(res$family),
    genus = .scalar_chr(res$genus),
    rank = .scalar_chr(res$rank)
  ))
  if (use_cache) .shark_cache_write(cache_file, result)
  result
}

#' Query Dyntaxa (Swedish taxonomy) via SHARK4R::match_dyntaxa_taxa()
#'
#' Needs DYNTAXA_KEY. Searches scientific and Swedish names. With
#' fuzzy = FALSE only a name equal to the query (ignoring case) counts.
#' @inheritParams query_shark_worms
#' @return Taxonomy result list with matched_name, scientific_name (the
#'   recommended name), taxon_id, author.
query_dyntaxa <- function(species_name, fuzzy = TRUE, use_cache = TRUE,
                          cache_dir = app_path("cache", "shark")) {
  base <- list(source = "Dyntaxa", query = species_name, status = "error")
  if (!check_shark4r_available()) return(utils::modifyList(base, list(message = SHARK4R_MISSING_MESSAGE)))
  key <- shark_subscription_key("dyntaxa")
  if (is.na(key)) {
    return(utils::modifyList(base, list(
      status = "no_key", message = "Dyntaxa needs a subscription key in the DYNTAXA_KEY environment variable"
    )))
  }

  cache_file <- .shark_cache_file(cache_dir, "dyntaxa", species_name)
  if (use_cache) {
    hit <- .shark_cache_read(cache_file)
    if (!is.null(hit)) return(hit)
  }

  failure <- NULL
  res <- tryCatch(
    SHARK4R::match_dyntaxa_taxa(species_name, subscription_key = key,
                                multiple_options = !isTRUE(fuzzy), verbose = FALSE),
    error = function(e) {
      failure <<- conditionMessage(e)
      warning(sprintf("[shark] Dyntaxa lookup failed for '%s': %s", species_name, failure), call. = FALSE)
      NULL
    }
  )
  if (!is.null(failure)) return(utils::modifyList(base, list(message = failure)))

  taxon_id <- if (NROW(res) > 0) .scalar_chr(res$taxon_id) else NA_character_
  if (is.na(taxon_id)) return(utils::modifyList(base, list(status = "not_found")))

  result <- utils::modifyList(base, list(
    status = "found",
    matched_name = .scalar_chr(res$best_match),
    scientific_name = .scalar_chr(res$valid_name),
    taxon_id = taxon_id,
    author = .scalar_chr(res$author)
  ))
  if (use_cache) .shark_cache_write(cache_file, result)
  result
}

#' Query AlgaeBase via SHARK4R::match_algaebase_taxa()
#'
#' Needs ALGAEBASE_KEY (an AlgaeBase API subscription key; the Trait Research
#' AlgaeBase username/password is a different credential).
#' @inheritParams query_shark_worms
#' @return Taxonomy result list with scientific_name, algaebase_id, authority,
#'   taxon_status, phylum, class.
query_algaebase <- function(species_name, use_cache = TRUE, cache_dir = app_path("cache", "shark")) {
  base <- list(source = "AlgaeBase", query = species_name, status = "error")
  if (!check_shark4r_available()) return(utils::modifyList(base, list(message = SHARK4R_MISSING_MESSAGE)))
  key <- shark_subscription_key("algaebase")
  if (is.na(key)) {
    return(utils::modifyList(base, list(
      status = "no_key", message = "AlgaeBase needs a subscription key in the ALGAEBASE_KEY environment variable"
    )))
  }

  cache_file <- .shark_cache_file(cache_dir, "algaebase", species_name)
  if (use_cache) {
    hit <- .shark_cache_read(cache_file)
    if (!is.null(hit)) return(hit)
  }

  failure <- NULL
  res <- tryCatch({
    parsed <- SHARK4R::parse_scientific_names(species_name)
    SHARK4R::match_algaebase_taxa(genera = parsed$genus, species = parsed$species,
                                  subscription_key = key, sleep_time = 0, verbose = FALSE)
  }, error = function(e) {
    failure <<- conditionMessage(e)
    warning(sprintf("[shark] AlgaeBase lookup failed for '%s': %s", species_name, failure), call. = FALSE)
    NULL
  })
  if (!is.null(failure)) return(utils::modifyList(base, list(message = failure)))

  algaebase_id <- if (NROW(res) > 0) .scalar_chr(res$id) else NA_character_
  if (is.na(algaebase_id)) return(utils::modifyList(base, list(status = "not_found")))

  name <- .scalar_chr(res$accepted_name)
  if (is.na(name)) name <- .scalar_chr(res$input_name)
  result <- utils::modifyList(base, list(
    status = "found",
    scientific_name = name,
    algaebase_id = algaebase_id,
    authority = .scalar_chr(res$authorship),
    taxon_status = .scalar_chr(res$taxonomic_status),
    phylum = .scalar_chr(res$phylum),
    class = .scalar_chr(res$class)
  ))
  if (use_cache) .shark_cache_write(cache_file, result)
  result
}

# ==============================================================================
# DATA RETRIEVAL FUNCTIONS
# ==============================================================================
# Both return list(status, data, message, total). status is "ok" (data holds
# at most max_records rows), "empty" or "error"; data is NULL unless "ok".

# bbox (named north/south/east/west) -> SHARK bounds c(lon_min, lat_min,
# lon_max, lat_max). All four blank -> NULL (no spatial filter).
.shark_bounds <- function(bbox) {
  if (is.null(bbox)) return(list(bounds = NULL))
  v <- suppressWarnings(as.numeric(bbox[c("west", "south", "east", "north")]))
  if (all(is.na(v))) return(list(bounds = NULL))
  if (!all(is.finite(v))) return(list(error = "Fill in all four bounding-box fields, or leave all four blank"))
  if (v[1] >= v[3] || v[2] >= v[4]) {
    return(list(error = "Bounding box: West must be less than East and South less than North"))
  }
  list(bounds = v)
}

.shark_fetch <- function(label, ..., start_date, end_date, bbox, max_records) {
  fail <- function(msg) list(status = "error", data = NULL, message = msg, total = 0L)
  if (!check_shark4r_available()) return(fail(SHARK4R_MISSING_MESSAGE))

  start <- tryCatch(as.Date(start_date), error = function(e) as.Date(NA))
  end <- tryCatch(as.Date(end_date), error = function(e) as.Date(NA))
  if (length(start) != 1 || length(end) != 1 || is.na(start) || is.na(end) || start > end) {
    return(fail("Invalid date range"))
  }
  box <- .shark_bounds(bbox)
  if (!is.null(box$error)) return(fail(box$error))
  max_records <- suppressWarnings(as.integer(max_records))
  if (length(max_records) != 1 || is.na(max_records) || max_records < 1) max_records <- 5000L

  timed_out <- structure(list(), class = "shark_timeout")
  failure <- NULL
  raw <- tryCatch(
    with_timeout(
      SHARK4R::get_shark_data(..., fromYear = as.integer(format(start, "%Y")),
                              toYear = as.integer(format(end, "%Y")), bounds = box$bounds,
                              verbose = FALSE),
      timeout = SHARK_QUERY_TIMEOUT_S, on_timeout = timed_out
    ),
    error = function(e) {
      failure <<- conditionMessage(e)
      warning(sprintf("[shark] %s query failed: %s", label, failure), call. = FALSE)
      NULL
    }
  )
  if (!is.null(failure)) return(fail(paste("SHARK query failed:", failure)))
  if (inherits(raw, "shark_timeout")) {
    warning(sprintf("[shark] %s query timed out after %d s", label, SHARK_QUERY_TIMEOUT_S), call. = FALSE)
    return(fail(sprintf("SHARK did not answer within %d s; narrow the query", SHARK_QUERY_TIMEOUT_S)))
  }

  if (NROW(raw) > 0 && "sample_date" %in% names(raw)) {
    dates <- suppressWarnings(as.Date(raw$sample_date))
    raw <- raw[!is.na(dates) & dates >= start & dates <= end, , drop = FALSE]
  }
  total <- NROW(raw)
  if (total == 0) {
    return(list(status = "empty", data = NULL, message = "No records match the query", total = 0L))
  }
  shown <- min(total, max_records)
  note <- if (shown < total) {
    sprintf("Showing the first %d of %d records", shown, total)
  } else {
    sprintf("Retrieved %d records", total)
  }
  list(status = "ok", data = as.data.frame(raw[seq_len(shown), , drop = FALSE]), message = note,
       total = total)
}

#' Get SHARK physical-chemical data
#'
#' @param parameters Character vector of exact SHARK parameter names
#'   (see SHARK_ENV_PARAMETERS).
#' @param start_date,end_date Date or "YYYY-MM-DD".
#' @param bbox Named numeric c(north, south, east, west), or NULL / all NA.
#' @param max_records Maximum rows kept.
#' @return list(status, data, message, total); see above.
get_shark_environmental_data <- function(parameters, start_date, end_date, bbox = NULL, max_records = 10000) {
  parameters <- as.character(parameters[!is.na(parameters) & nzchar(parameters)])
  if (length(parameters) == 0) {
    return(list(status = "error", data = NULL, message = "Choose at least one parameter", total = 0L))
  }
  .shark_fetch("environmental", dataTypes = "Physical and Chemical", parameters = parameters,
               start_date = start_date, end_date = end_date, bbox = bbox, max_records = max_records)
}

#' Get SHARK records for one taxon (any biological data type)
#'
#' @param species_name Exact scientific name as used in SHARK.
#' @inheritParams get_shark_environmental_data
#' @return list(status, data, message, total); see above.
get_shark_species_occurrence <- function(species_name, start_date, end_date, bbox = NULL, max_records = 5000) {
  species_name <- trimws(.scalar_chr(species_name))
  if (is.na(species_name) || !nzchar(species_name)) {
    return(list(status = "error", data = NULL, message = "Enter a scientific name", total = 0L))
  }
  .shark_fetch("occurrence", taxonName = species_name,
               start_date = start_date, end_date = end_date, bbox = bbox, max_records = max_records)
}

# Display column -> SHARK column (get_shark_data(), headerLang = "internal_key").
SHARK_DISPLAY_COLUMNS <- list(
  environmental = c(
    "Date" = "sample_date", "Station" = "station_name", "Lat" = "sample_latitude_dd",
    "Lon" = "sample_longitude_dd", "Depth (m)" = "sample_min_depth_m", "Parameter" = "parameter",
    "Value" = "value", "Unit" = "unit", "Quality flag" = "quality_flag"
  ),
  occurrence = c(
    "Date" = "sample_date", "Species" = "scientific_name", "Data type" = "delivery_datatype",
    "Station" = "station_name", "Lat" = "sample_latitude_dd", "Lon" = "sample_longitude_dd",
    "Parameter" = "parameter", "Value" = "value", "Unit" = "unit"
  )
)

#' Format SHARK records for display
#'
#' @param raw_data Data frame from get_shark_data().
#' @param result_type "environmental" or "occurrence".
#' @return Data frame with the display columns; a column SHARK did not
#'   return is NA.
format_shark_results <- function(raw_data, result_type = c("environmental", "occurrence")) {
  result_type <- match.arg(result_type)
  if (is.null(raw_data) || NROW(raw_data) == 0) {
    return(data.frame(Message = "No data to display"))
  }
  cols <- SHARK_DISPLAY_COLUMNS[[result_type]]
  out <- lapply(cols, function(col) if (col %in% names(raw_data)) raw_data[[col]] else rep(NA, NROW(raw_data)))
  data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
}

# ==============================================================================
# QUALITY CONTROL FUNCTIONS
# ==============================================================================

#' Read an uploaded QC file (.txt/.tsv tab-separated, otherwise CSV)
#'
#' @param path File path.
#' @param name Original file name (decides the delimiter).
#' @return Data frame, or NULL with a warning when the file cannot be read.
read_shark_qc_file <- function(path, name = path) {
  ext <- tolower(tools::file_ext(name))
  tryCatch({
    if (ext %in% c("txt", "tsv")) {
      utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    }
  }, error = function(e) {
    warning(sprintf("[shark] could not read QC file '%s': %s", name, conditionMessage(e)), call. = FALSE)
    NULL
  })
}

#' SHARK data type for the QC checks
#'
#' @param data_frame Uploaded data.
#' @param selected The UI choice: "auto" or one of SHARK_QC_DATATYPES.
#' @return A SHARK_QC_DATATYPES value, or NA when "auto" cannot decide (no
#'   delivery_datatype column, or not exactly one known value in it).
resolve_shark_qc_datatype <- function(data_frame, selected = "auto") {
  if (!identical(selected, "auto")) {
    return(if (isTRUE(selected %in% SHARK_QC_DATATYPES)) selected else NA_character_)
  }
  if (!"delivery_datatype" %in% names(data_frame)) return(NA_character_)
  values <- unique(stats::na.omit(as.character(data_frame$delivery_datatype)))
  if (length(values) == 1 && values %in% SHARK_QC_DATATYPES) values else NA_character_
}

SHARK_QC_NO_DATATYPE <- "Choose the data type: the file has no single known delivery_datatype value"

#' Validate SHARK format with SHARK4R::check_fields()
#'
#' @param data_frame Data frame to validate.
#' @param datatype One of SHARK_QC_DATATYPES, or NA.
#' @return list(valid, message, errors, warnings, n_errors, n_warnings);
#'   errors/warnings are the unique messages.
validate_shark_data <- function(data_frame, datatype) {
  result <- function(valid, message, errors = character(0), warnings = character(0), n_errors = 0L,
                     n_warnings = 0L) {
    list(valid = valid, message = message, errors = errors, warnings = warnings,
         n_errors = n_errors, n_warnings = n_warnings)
  }
  if (!check_shark4r_available()) return(result(FALSE, SHARK4R_MISSING_MESSAGE))
  if (is.na(datatype)) return(result(FALSE, SHARK_QC_NO_DATATYPE))

  failure <- NULL
  issues <- tryCatch(
    SHARK4R::check_fields(data_frame, SHARK4R::translate_shark_datatype(datatype), level = "warning"),
    error = function(e) {
      failure <<- conditionMessage(e)
      warning(sprintf("[shark] format validation failed: %s", failure), call. = FALSE)
      NULL
    }
  )
  if (!is.null(failure)) return(result(FALSE, paste("Validation error:", failure)))

  levels <- if (NROW(issues) > 0) as.character(issues$level) else character(0)
  messages <- if (NROW(issues) > 0) as.character(issues$message) else character(0)
  is_error <- levels == "error"
  n_errors <- sum(is_error)
  n_warnings <- sum(!is_error)
  result(n_errors == 0,
         sprintf("%s: %d errors, %d warnings", datatype, n_errors, n_warnings),
         unique(messages[is_error]), unique(messages[!is_error]), n_errors, n_warnings)
}

#' Data completeness (local; no SHARK4R)
#'
#' @param data_frame Data frame to check.
#' @return list(completeness = percent of rows with no missing value,
#'   missing_values, record_count, column_count, message).
check_data_quality <- function(data_frame) {
  n <- NROW(data_frame)
  list(
    completeness = if (n > 0) sum(stats::complete.cases(data_frame)) / n * 100 else NA_real_,
    missing_values = colSums(is.na(data_frame)),
    record_count = n,
    column_count = NCOL(data_frame),
    message = "Completeness is the share of rows with no missing value"
  )
}

#' Outliers against SHARK4R's bundled thresholds (SHARK4R::check_outliers())
#'
#' Checks each parameter in the file. A parameter without a SHARK threshold
#' for the data type is listed in `unchecked`, not warned about.
#' @param data_frame Data in SHARK long format (parameter, value columns).
#' @param datatype One of SHARK_QC_DATATYPES, or NA.
#' @return list(message, outliers (data frame or NULL), checked, unchecked).
check_shark_outliers <- function(data_frame, datatype) {
  result <- function(message, outliers = NULL, checked = character(0), unchecked = character(0)) {
    list(message = message, outliers = outliers, checked = checked, unchecked = unchecked)
  }
  if (!check_shark4r_available()) return(result(SHARK4R_MISSING_MESSAGE))
  if (is.na(datatype)) return(result(SHARK_QC_NO_DATATYPE))
  if (!all(c("parameter", "value") %in% names(data_frame))) {
    return(result("Outlier check needs 'parameter' and 'value' columns (SHARK long format)"))
  }

  d <- data_frame
  d$delivery_datatype <- datatype
  d$value <- suppressWarnings(as.numeric(d$value))
  checked <- character(0)
  unchecked <- character(0)
  found <- list()
  for (p in unique(stats::na.omit(as.character(d$parameter)))) {
    no_threshold <- FALSE
    failed <- FALSE
    res <- tryCatch(
      withCallingHandlers(
        SHARK4R::check_outliers(d, parameter = p, datatype = datatype, return_df = TRUE, verbose = FALSE),
        warning = function(w) {
          if (grepl("No thresholds found", conditionMessage(w), fixed = TRUE)) {
            no_threshold <<- TRUE
            invokeRestart("muffleWarning")
          }
        }
      ),
      error = function(e) {
        failed <<- TRUE
        warning(sprintf("[shark] outlier check failed for '%s': %s", p, conditionMessage(e)), call. = FALSE)
        NULL
      }
    )
    if (failed || no_threshold) {
      unchecked <- c(unchecked, p)
      next
    }
    checked <- c(checked, p)
    if (NROW(res) > 0) found[[p]] <- as.data.frame(res)
  }
  outliers <- if (length(found) > 0) do.call(rbind, unname(found)) else NULL
  result(sprintf("%d outlier values in %d checked parameters; %d parameters have no SHARK threshold",
                 NROW(outliers), length(checked), length(unchecked)),
         outliers, checked, unchecked)
}

#' Coordinate checks (local; no SHARK4R)
#'
#' @param data_frame Data with sample_latitude_dd / sample_longitude_dd.
#' @return list(message, zero, out_of_range, missing) counts of rows.
check_shark_coordinates <- function(data_frame) {
  cols <- c("sample_latitude_dd", "sample_longitude_dd")
  if (!all(cols %in% names(data_frame))) {
    return(list(message = "No sample_latitude_dd / sample_longitude_dd columns",
                zero = NA_integer_, out_of_range = NA_integer_, missing = NA_integer_))
  }
  lat <- suppressWarnings(as.numeric(as.character(data_frame$sample_latitude_dd)))
  lon <- suppressWarnings(as.numeric(as.character(data_frame$sample_longitude_dd)))
  zero <- sum(lat %in% 0 | lon %in% 0)
  out_of_range <- sum((!is.na(lat) & abs(lat) > 90) | (!is.na(lon) & abs(lon) > 180))
  missing <- sum(is.na(lat) | is.na(lon))
  list(message = sprintf("%d rows with a zero coordinate, %d out of range, %d missing",
                         zero, out_of_range, missing),
       zero = zero, out_of_range = out_of_range, missing = missing)
}

#' Run the selected QC checks
#'
#' @param data_frame Uploaded data.
#' @param checks Subset of c("format", "completeness", "outliers", "coordinates").
#' @param datatype One of SHARK_QC_DATATYPES, or NA.
#' @return list(error = FALSE, datatype, validation, quality, outliers,
#'   coordinates, data_summary); an unselected check is NULL.
run_shark_qc <- function(data_frame, checks, datatype) {
  list(
    error = FALSE,
    datatype = datatype,
    validation = if ("format" %in% checks) validate_shark_data(data_frame, datatype),
    quality = if ("completeness" %in% checks) check_data_quality(data_frame),
    outliers = if ("outliers" %in% checks) check_shark_outliers(data_frame, datatype),
    coordinates = if ("coordinates" %in% checks) check_shark_coordinates(data_frame),
    data_summary = list(rows = NROW(data_frame), columns = NCOL(data_frame), column_names = colnames(data_frame))
  )
}

#' Plain-text QC report
#'
#' @param qc Result of run_shark_qc().
#' @param max_items Maximum validation messages listed.
#' @return Character vector of report lines.
format_shark_qc_report <- function(qc, max_items = 20L) {
  rule <- strrep("=", 80)
  lines <- c(rule, "SHARK DATA QUALITY CONTROL REPORT", rule, "",
             "DATA SUMMARY", "------------",
             paste("Rows:", qc$data_summary$rows),
             paste("Columns:", qc$data_summary$columns),
             paste("Data type:", if (is.na(qc$datatype)) "unknown" else qc$datatype), "")
  v <- qc$validation
  if (!is.null(v)) {
    lines <- c(lines, "FORMAT VALIDATION (SHARK4R::check_fields)", "-----------------------------------------",
               paste("Result:", if (isTRUE(v$valid)) "PASSED" else "FAILED"), v$message)
    shown <- utils::head(v$errors, max_items)
    if (length(shown) > 0) lines <- c(lines, paste(" -", shown))
    if (length(v$errors) > length(shown)) {
      lines <- c(lines, sprintf(" ... and %d more", length(v$errors) - length(shown)))
    }
    lines <- c(lines, "")
  }
  q <- qc$quality
  if (!is.null(q)) {
    completeness <- if (is.na(q$completeness)) "n/a" else sprintf("%.1f%%", q$completeness)
    lines <- c(lines, "COMPLETENESS", "------------", paste("Data completeness:", completeness),
               paste("Record count:", q$record_count), "")
  }
  if (!is.null(qc$outliers)) {
    lines <- c(lines, "OUTLIERS (SHARK4R::check_outliers)", "----------------------------------",
               qc$outliers$message, "")
  }
  if (!is.null(qc$coordinates)) {
    lines <- c(lines, "COORDINATES", "-----------", qc$coordinates$message, "")
  }
  c(lines, rule)
}

# ==============================================================================
# END OF SHARK4R API UTILITIES
# ==============================================================================
