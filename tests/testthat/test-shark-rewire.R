# C-6b (spec C2.8, F12): the SHARK Data tab called 12 SHARK4R functions that
# SHARK4R 1.2.0 does not export, so every SHARK action failed in production.
# These tests pin the rewire to the 1.2.0 API. SHARK4R is mocked with
# local_mocked_bindings(.package = "SHARK4R"); no test touches the network
# (test-shark-live.R holds the live checks).

app_root <- get_app_root()

source_shark <- function(env = parent.frame()) {
  # The UI and server call shiny, DT, leaflet and bs4Dash unqualified, as app.R
  # attaches them. Attach them only for the calling test.
  for (pkg in c("shiny", "DT", "leaflet", "bs4Dash")) withr::local_package(pkg, .local_envir = env)
  source(file.path(app_root, "R/functions/validation_utils.R"), local = FALSE)
  source(file.path(app_root, "R/functions/shark_api_utils.R"), local = FALSE)
  source(file.path(app_root, "R/modules/shark_server.R"), local = FALSE)
  source(file.path(app_root, "R/ui/shark_ui.R"), local = FALSE)
}

# Assign `fn` to `nm` in globalenv for the calling test, restoring (or
# removing) whatever was there before.
local_global_mock <- function(nm, fn, env = parent.frame()) {
  had <- exists(nm, envir = globalenv(), inherits = FALSE)
  old <- if (had) get(nm, envir = globalenv()) else NULL
  assign(nm, fn, envir = globalenv())
  withr::defer(
    if (had) assign(nm, old, envir = globalenv()) else rm(list = nm, envir = globalenv()),
    envir = env
  )
}

html_of <- function(x) paste(as.character(htmltools::renderTags(x)$html), collapse = "\n")

# One match_worms_taxa() row, columns as returned by SHARK4R 1.2.0
# (live, 2026-09-29, "Gadus morhua"; trimmed to the columns the app reads).
worms_row <- function(name = "Gadus morhua") {
  tibble::tibble(
    name = name, AphiaID = 126436L, scientificname = "Gadus morhua", authority = "Linnaeus, 1758",
    status = "accepted", rank = "Species", kingdom = "Animalia", phylum = "Chordata",
    class = "Teleostei", order = "Gadiformes", family = "Gadidae", genus = "Gadus"
  )
}

# What match_worms_taxa() returns for a name WoRMS does not know ("Xx yy", live).
worms_no_content <- function(name = "Xx yy") {
  tibble::tibble(name = name, AphiaID = NA_integer_, scientificname = NA_character_,
                 status = "no content", rank = NA_character_)
}

# get_shark_data() rows (internal_key headers; values from the live
# Kattegat June 2024 query), dates spanning two years.
shark_rows <- function(dates = as.Date(c("2023-12-31", "2024-01-15", "2024-06-06", "2024-12-31", "2025-01-01"))) {
  n <- length(dates)
  tibble::tibble(
    delivery_datatype = "Physical and Chemical", station_name = "FLADEN", sample_date = dates,
    sample_latitude_dd = 57.19267, sample_longitude_dd = 11.658, sample_min_depth_m = 0,
    sample_max_depth_m = 0, scientific_name = NA_character_, parameter = "Temperature CTD",
    value = seq_len(n) + 10, unit = "C", quality_flag = NA_character_
  )
}

# ---------------------------------------------------------------------------
# The export list (spec rollout item 3)
# ---------------------------------------------------------------------------

shark4r_calls_in_app <- function() {
  files <- c(list.files(file.path(app_root, "R"), pattern = "\\.R$", recursive = TRUE, full.names = TRUE),
             file.path(app_root, "app.R"))
  files <- files[!grepl("safeBackup", basename(files), fixed = TRUE)]
  code <- unlist(lapply(files, readLines, warn = FALSE))
  code <- code[!startsWith(trimws(code), "#")]
  unique(sub("^SHARK4R::", "", unlist(regmatches(code, gregexpr("SHARK4R::[A-Za-z0-9_.]+", code)))))
}

test_that("every SHARK4R:: call in the app is listed in SHARK4R_FUNCTIONS (F12)", {
  source(file.path(app_root, "R/functions/shark_api_utils.R"), local = FALSE)
  used <- shark4r_calls_in_app()
  expect_true("get_shark_data" %in% used, info = "premise: the scan finds the SHARK calls")
  expect_equal(setdiff(used, SHARK4R_FUNCTIONS), character(0))
  expect_equal(setdiff(SHARK4R_FUNCTIONS, used), character(0))
})

test_that("SHARK4R exports every function the app calls (F12)", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source(file.path(app_root, "R/functions/shark_api_utils.R"), local = FALSE)
  expect_equal(setdiff(SHARK4R_FUNCTIONS, getNamespaceExports("SHARK4R")), character(0))
  expect_true(shark4r_installed())
})

test_that("no SHARK handler reports failures with message() (F12)", {
  for (f in c("R/functions/shark_api_utils.R", "R/modules/shark_server.R")) {
    code <- readLines(file.path(app_root, f), warn = FALSE)
    code <- code[!startsWith(trimws(code), "#")]
    expect_false(any(grepl("\\bmessage\\(", code)), info = f)
  }
})

test_that("every QC data type is one SHARK4R::check_fields() knows", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source(file.path(app_root, "R/functions/shark_api_utils.R"), local = FALSE)
  for (dt in SHARK_QC_DATATYPES) {
    translated <- SHARK4R::translate_shark_datatype(dt)
    expect_no_error(suppressWarnings(SHARK4R::check_fields(data.frame(x = 1), translated)))
  }
})

# ---------------------------------------------------------------------------
# Taxonomy
# ---------------------------------------------------------------------------

test_that("query_shark_worms reads a match_worms_taxa() row (F12)", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  seen <- NULL
  local_mocked_bindings(match_worms_taxa = function(taxa_names, ...) {
    seen <<- list(taxa_names = taxa_names, ...)
    worms_row(taxa_names)
  }, .package = "SHARK4R")

  r <- query_shark_worms("Gadus morhua", fuzzy = FALSE, use_cache = FALSE)
  expect_equal(r$status, "found")
  expect_identical(r$aphia_id, 126436L)
  expect_equal(r$scientific_name, "Gadus morhua")
  expect_equal(r$taxon_status, "accepted")
  expect_equal(r$class, "Teleostei")
  expect_equal(r$family, "Gadidae")
  expect_false(seen$fuzzy)
  expect_equal(seen$max_retries, 1)
  expect_equal(seen$sleep_time, 0)
  expect_false(seen$verbose)
})

test_that("a name WoRMS does not know is not_found, not a result (F12)", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  local_mocked_bindings(match_worms_taxa = function(taxa_names, ...) worms_no_content(taxa_names),
                        .package = "SHARK4R")
  expect_equal(query_shark_worms("Xx yy", use_cache = FALSE)$status, "not_found")
})

test_that("a failing WoRMS lookup warns and returns status error (F12)", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  local_mocked_bindings(match_worms_taxa = function(...) stop("HTTP 503"), .package = "SHARK4R")
  expect_warning(r <- query_shark_worms("Gadus morhua", use_cache = FALSE), "[shark] WoRMS lookup failed",
                 fixed = TRUE)
  expect_equal(r$status, "error")
  expect_equal(r$message, "HTTP 503")
})

test_that("only found WoRMS results are cached", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  dir <- withr::local_tempdir()
  calls <- 0
  local_mocked_bindings(match_worms_taxa = function(taxa_names, ...) {
    calls <<- calls + 1
    if (taxa_names == "Xx yy") worms_no_content(taxa_names) else worms_row(taxa_names)
  }, .package = "SHARK4R")

  query_shark_worms("Gadus morhua", cache_dir = dir)
  expect_equal(query_shark_worms("Gadus morhua", cache_dir = dir)$aphia_id, 126436L)
  expect_equal(calls, 1)
  query_shark_worms("Xx yy", cache_dir = dir)
  query_shark_worms("Xx yy", cache_dir = dir)
  expect_equal(calls, 3)
})

test_that("Dyntaxa without DYNTAXA_KEY is no_key and makes no call", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  withr::local_envvar(DYNTAXA_KEY = "")
  local_mocked_bindings(match_dyntaxa_taxa = function(...) stop("must not be called"), .package = "SHARK4R")
  r <- query_dyntaxa("torsk", use_cache = FALSE)
  expect_equal(r$status, "no_key")
  expect_match(r$message, "DYNTAXA_KEY", fixed = TRUE)
})

test_that("query_dyntaxa reads a match_dyntaxa_taxa() row; fuzzy maps to multiple_options", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  withr::local_envvar(DYNTAXA_KEY = "k")
  seen <- NULL
  local_mocked_bindings(match_dyntaxa_taxa = function(taxon_names, ...) {
    seen <<- list(...)
    # Columns as built by SHARK4R 1.2.0 match_dyntaxa_taxa() (source)
    tibble::tibble(search_pattern = taxon_names, taxon_id = 206199L, best_match = "torsk",
                   author = NA_character_, valid_name = "Gadus morhua")
  }, .package = "SHARK4R")

  r <- query_dyntaxa("torsk", fuzzy = TRUE, use_cache = FALSE)
  expect_equal(r$status, "found")
  expect_equal(r$matched_name, "torsk")
  expect_equal(r$scientific_name, "Gadus morhua")
  expect_equal(r$taxon_id, "206199")
  expect_true(is.na(r$author))
  expect_equal(seen$subscription_key, "k")
  expect_false(seen$multiple_options)
  query_dyntaxa("torsk", fuzzy = FALSE, use_cache = FALSE)
  expect_true(seen$multiple_options)
})

test_that("a Dyntaxa row without a taxon_id is not_found", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  withr::local_envvar(DYNTAXA_KEY = "k")
  local_mocked_bindings(match_dyntaxa_taxa = function(taxon_names, ...) {
    tibble::tibble(search_pattern = taxon_names, taxon_id = NA, best_match = NA, author = NA, valid_name = NA)
  }, .package = "SHARK4R")
  expect_equal(query_dyntaxa("Xx yy", use_cache = FALSE)$status, "not_found")
})

test_that("query_algaebase splits the name and reads a match_algaebase_taxa() row", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  withr::local_envvar(ALGAEBASE_KEY = "")
  expect_equal(query_algaebase("Skeletonema marinoi", use_cache = FALSE)$status, "no_key")

  withr::local_envvar(ALGAEBASE_KEY = "k")
  seen <- NULL
  local_mocked_bindings(match_algaebase_taxa = function(genera, species, ...) {
    seen <<- list(genera = genera, species = species, ...)
    # Columns of SHARK4R 1.2.0's AlgaeBase result (its error-row template)
    tibble::tibble(input_name = paste(genera, species), id = 12345L, phylum = "Bacillariophyta",
                   class = "Mediophyceae", taxonomic_status = "accepted",
                   accepted_name = "Skeletonema marinoi", authorship = "Sarno & Zingone")
  }, .package = "SHARK4R")
  r <- query_algaebase("Skeletonema marinoi", use_cache = FALSE)
  expect_equal(r$status, "found")
  expect_equal(r$algaebase_id, "12345")
  expect_equal(r$scientific_name, "Skeletonema marinoi")
  expect_equal(r$authority, "Sarno & Zingone")
  expect_equal(seen$genera, "Skeletonema")
  expect_equal(seen$species, "marinoi")
  expect_equal(seen$subscription_key, "k")
})

test_that("the taxonomy sources offered follow the subscription keys", {
  source(file.path(app_root, "R/functions/shark_api_utils.R"), local = FALSE)
  withr::local_envvar(DYNTAXA_KEY = "", ALGAEBASE_KEY = "")
  expect_equal(unname(shark_taxonomy_source_choices()), "worms")
  withr::local_envvar(DYNTAXA_KEY = "a", ALGAEBASE_KEY = "b")
  expect_equal(unname(shark_taxonomy_source_choices()), c("dyntaxa", "worms", "algaebase"))
})

# ---------------------------------------------------------------------------
# SHARK data
# ---------------------------------------------------------------------------

test_that("environmental queries call get_shark_data with years, bounds and exact parameters (F12)", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  seen <- NULL
  local_mocked_bindings(get_shark_data = function(...) {
    seen <<- list(...)
    shark_rows()
  }, .package = "SHARK4R")

  r <- get_shark_environmental_data(
    parameters = c("Temperature CTD", "Salinity CTD"), start_date = as.Date("2024-01-01"),
    end_date = as.Date("2024-12-31"), bbox = c(north = 58, south = 57, east = 12, west = 11), max_records = 2
  )
  expect_equal(seen$dataTypes, "Physical and Chemical")
  expect_equal(seen$parameters, c("Temperature CTD", "Salinity CTD"))
  expect_identical(seen$fromYear, 2024L)
  expect_identical(seen$toYear, 2024L)
  expect_equal(seen$bounds, c(11, 57, 12, 58))
  expect_false(seen$verbose)
  # 2023-12-31 and 2025-01-01 fall outside the dates; 3 remain, 2 are shown
  expect_equal(r$status, "ok")
  expect_equal(r$total, 3L)
  expect_equal(nrow(r$data), 2L)
  expect_equal(r$message, "Showing the first 2 of 3 records")
})

test_that("a blank bounding box sends no bounds; a partial or inverted one is refused", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  calls <- 0
  seen <- NULL
  local_mocked_bindings(get_shark_data = function(...) {
    calls <<- calls + 1
    seen <<- list(...)
    shark_rows()
  }, .package = "SHARK4R")
  blank <- c(north = NA, south = NA, east = NA, west = NA)
  r <- get_shark_environmental_data("Temperature CTD", "2024-01-01", "2024-12-31", bbox = blank)
  expect_equal(r$status, "ok")
  expect_true("bounds" %in% names(seen))
  expect_null(seen$bounds)

  partial <- get_shark_environmental_data("Temperature CTD", "2024-01-01", "2024-12-31",
                                          bbox = c(north = 58, south = NA, east = NA, west = NA))
  expect_equal(partial$status, "error")
  expect_match(partial$message, "all four", fixed = TRUE)
  inverted <- get_shark_environmental_data("Temperature CTD", "2024-01-01", "2024-12-31",
                                           bbox = c(north = 57, south = 58, east = 12, west = 11))
  expect_equal(inverted$status, "error")
  expect_equal(calls, 1)
})

test_that("no parameters, a bad date range or no rows give a status, not an error", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  local_mocked_bindings(get_shark_data = function(...) shark_rows()[0, ], .package = "SHARK4R")
  expect_equal(get_shark_environmental_data(character(0), "2024-01-01", "2024-12-31")$status, "error")
  expect_equal(get_shark_environmental_data("pH", "2024-12-31", "2024-01-01")$message, "Invalid date range")
  empty <- get_shark_environmental_data("pH", "2024-01-01", "2024-12-31")
  expect_equal(empty$status, "empty")
  expect_null(empty$data)
})

test_that("a failing or timed-out SHARK query warns and returns status error (F12)", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  local_mocked_bindings(get_shark_data = function(...) stop("HTTP 500"), .package = "SHARK4R")
  expect_warning(r <- get_shark_environmental_data("pH", "2024-01-01", "2024-12-31"),
                 "[shark] environmental query failed", fixed = TRUE)
  expect_equal(r$status, "error")
  expect_match(r$message, "HTTP 500", fixed = TRUE)

  local_global_mock("with_timeout", function(expr, timeout = 10, on_timeout = NULL, verbose = FALSE) on_timeout)
  expect_warning(t <- get_shark_species_occurrence("Macoma balthica", "2022-01-01", "2022-12-31"),
                 "timed out", fixed = TRUE)
  expect_equal(t$status, "error")
})

test_that("occurrence queries filter on the taxon name", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  seen <- NULL
  local_mocked_bindings(get_shark_data = function(...) {
    seen <<- list(...)
    shark_rows(as.Date("2022-06-13"))
  }, .package = "SHARK4R")
  r <- get_shark_species_occurrence("  Macoma balthica ", "2022-01-01", "2022-12-31")
  expect_equal(seen$taxonName, "Macoma balthica")
  expect_null(seen$dataTypes)
  expect_equal(r$status, "ok")
  expect_equal(get_shark_species_occurrence("  ", "2022-01-01", "2022-12-31")$status, "error")
})

test_that("format_shark_results maps SHARK columns and fills missing ones with NA", {
  source(file.path(app_root, "R/functions/shark_api_utils.R"), local = FALSE)
  env <- format_shark_results(as.data.frame(shark_rows()), "environmental")
  expect_equal(names(env), c("Date", "Station", "Lat", "Lon", "Depth (m)", "Parameter", "Value", "Unit",
                             "Quality flag"))
  expect_equal(env$Parameter[1], "Temperature CTD")
  occ <- format_shark_results(data.frame(scientific_name = "Macoma balthica", value = 5), "occurrence")
  expect_equal(occ$Species, "Macoma balthica")
  expect_true(is.na(occ$Station))
  expect_equal(format_shark_results(NULL, "occurrence")$Message, "No data to display")
})

# ---------------------------------------------------------------------------
# Quality control
# ---------------------------------------------------------------------------

test_that("format validation summarises SHARK4R::check_fields() issues (F12)", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  seen <- NULL
  seen_level <- NULL
  local_mocked_bindings(check_fields = function(data, datatype, level = "error", ...) {
    seen <<- datatype
    seen_level <<- level
    tibble::tibble(level = c("error", "error", "warning"), field = c("a", "a", "b"), row = NA_integer_,
                   message = c("Required field a is missing", "Required field a is missing", "b is empty"))
  }, .package = "SHARK4R")
  v <- validate_shark_data(data.frame(x = 1), "Physical and Chemical")
  expect_equal(seen, "PhysicalChemical")
  # M1: level = "warning" so warning rows exist in the result, not just errors.
  expect_equal(seen_level, "warning")
  expect_false(v$valid)
  expect_equal(v$n_errors, 2L)
  expect_equal(v$errors, "Required field a is missing")
  expect_equal(v$warnings, "b is empty")
  expect_false(validate_shark_data(data.frame(x = 1), NA_character_)$valid)
})

test_that("the QC data type comes from the UI or a single delivery_datatype value", {
  source(file.path(app_root, "R/functions/shark_api_utils.R"), local = FALSE)
  one <- data.frame(delivery_datatype = c("Zoobenthos", "Zoobenthos"))
  mixed <- data.frame(delivery_datatype = c("Zoobenthos", "Zooplankton"))
  expect_equal(resolve_shark_qc_datatype(one, "auto"), "Zoobenthos")
  expect_true(is.na(resolve_shark_qc_datatype(mixed, "auto")))
  expect_true(is.na(resolve_shark_qc_datatype(data.frame(x = 1), "auto")))
  expect_equal(resolve_shark_qc_datatype(mixed, "Zooplankton"), "Zooplankton")
  expect_true(is.na(resolve_shark_qc_datatype(one, "Not a type")))
})

test_that("outliers use SHARK4R thresholds; parameters without one are listed, not warned", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  # Real check_outliers(): its thresholds ship with SHARK4R (offline).
  d <- data.frame(station_name = "S1", sample_date = as.Date("2022-06-01"),
                  parameter = c("Abundance", "Abundance", "No such parameter"),
                  value = c(1, 1e12, 5), stringsAsFactors = FALSE)
  expect_no_warning(o <- check_shark_outliers(d, "Zoobenthos"))
  expect_equal(o$checked, "Abundance")
  expect_equal(o$unchecked, "No such parameter")
  expect_equal(nrow(o$outliers), 1L)
  expect_equal(o$outliers$value, 1e12)
  expect_match(check_shark_outliers(data.frame(x = 1), "Zoobenthos")$message, "long format", fixed = TRUE)
})

test_that("coordinate checks count zero, out-of-range and missing positions", {
  source(file.path(app_root, "R/functions/shark_api_utils.R"), local = FALSE)
  d <- data.frame(sample_latitude_dd = c(57, 0, 95, NA), sample_longitude_dd = c(11, 11, 11, 11))
  k <- check_shark_coordinates(d)
  expect_equal(k$zero, 1L)
  expect_equal(k$out_of_range, 1L)
  expect_equal(k$missing, 1L)
  expect_true(is.na(check_shark_coordinates(data.frame(x = 1))$zero))
})

test_that("QC files are read by extension, and an unreadable file warns", {
  source(file.path(app_root, "R/functions/shark_api_utils.R"), local = FALSE)
  dir <- withr::local_tempdir()
  tsv <- file.path(dir, "a.txt")
  writeLines(c("station_name\tvalue", "S1\t2"), tsv)
  csv <- file.path(dir, "a.csv")
  writeLines(c("station_name,value", "S1,2"), csv)
  expect_equal(read_shark_qc_file(tsv, "a.txt")$value, 2)
  expect_equal(read_shark_qc_file(csv, "a.csv")$value, 2)
  expect_warning(bad <- read_shark_qc_file(file.path(dir, "missing.csv"), "missing.csv"),
                 "[shark] could not read QC file", fixed = TRUE)
  expect_null(bad)
})

test_that("run_shark_qc runs only the selected checks, and the report lists them", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  local_mocked_bindings(check_fields = function(...) {
    tibble::tibble(level = "error", field = paste0("f", 1:25), row = NA_integer_,
                   message = paste("Required field", paste0("f", 1:25), "is missing"))
  }, .package = "SHARK4R")
  d <- data.frame(delivery_datatype = "Zoobenthos", sample_latitude_dd = 57, sample_longitude_dd = 11)
  qc <- run_shark_qc(d, c("format", "coordinates"), resolve_shark_qc_datatype(d))
  expect_null(qc$quality)
  expect_null(qc$outliers)
  expect_equal(qc$validation$n_errors, 25L)
  report <- format_shark_qc_report(qc)
  expect_true("Result: FAILED" %in% report)
  expect_true(" ... and 5 more" %in% report)
  expect_false(any(grepl("COMPLETENESS", report, fixed = TRUE)))
  expect_true("COORDINATES" %in% report)
})

# ---------------------------------------------------------------------------
# Rendering (B3: third-party text only through tag builders or escaped)
# ---------------------------------------------------------------------------

test_that("taxonomy cards escape third-party text and show why a lookup gave nothing", {
  source_shark()
  evil <- list(status = "found", scientific_name = "<script>alert(1)</script>", aphia_id = "1 onmouseover=x",
               authority = NA, taxon_status = "accepted", class = "<b>x</b>", family = NA)
  html <- html_of(shark_taxonomy_card("worms", evil))
  expect_false(grepl("<script>", html, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;", html, fixed = TRUE))
  expect_false(grepl("href", html, fixed = TRUE))  # a non-integer AphiaID gets no link

  good <- html_of(shark_taxonomy_card("worms", list(status = "found", aphia_id = 126436L)))
  expect_true(grepl("taxdetails&amp;id=126436", good, fixed = TRUE))
  expect_match(html_of(shark_taxonomy_card("dyntaxa", list(status = "no_key", message = "needs DYNTAXA_KEY"))),
               "needs DYNTAXA_KEY", fixed = TRUE)
  expect_match(html_of(shark_taxonomy_card("worms", list(status = "not_found"))), "No results found", fixed = TRUE)
  expect_match(html_of(shark_taxonomy_card("worms", list(status = "error", message = "<i>503</i>"))),
               "Lookup failed: &lt;i&gt;503", fixed = TRUE)
})

test_that("occurrence popups escape every field", {
  source_shark()
  payload <- "<img src=x onerror=alert(1)>"
  d <- data.frame(Species = payload, Date = payload, Parameter = payload, Value = payload, Unit = payload,
                  stringsAsFactors = FALSE)
  popup <- shark_occurrence_popup(d)
  expect_false(grepl("<img", popup, fixed = TRUE))
  matches <- gregexpr("&lt;img", popup, fixed = TRUE)[[1]]
  expect_equal(length(matches), 5)
  expect_true(all(matches > 0))
})

test_that("query status lines distinguish idle, ok, empty and error", {
  source_shark()
  expect_match(html_of(shark_query_status(NULL, "Nothing yet")), "Nothing yet", fixed = TRUE)
  expect_match(html_of(shark_query_status(list(status = "ok", message = "Retrieved 3 records"), "")),
               "alert-success", fixed = TRUE)
  expect_match(html_of(shark_query_status(list(status = "empty", message = "none"), "")), "alert-warning",
               fixed = TRUE)
  expect_match(html_of(shark_query_status(list(status = "error", message = "<b>x</b>"), "")),
               "&lt;b&gt;x", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# UI and server wiring
# ---------------------------------------------------------------------------

test_that("the SHARK UI offers exact SHARK parameters, the key-gated sources and a QC data type", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  withr::local_envvar(DYNTAXA_KEY = "", ALGAEBASE_KEY = "")
  html <- html_of(shark_ui())
  expect_true(grepl('value="Temperature CTD"', html, fixed = TRUE))
  expect_false(grepl('value="temperature"', html, fixed = TRUE))
  expect_true(grepl('id="shark_qc_datatype"', html, fixed = TRUE))
  expect_true(grepl('value="worms"', html, fixed = TRUE))
  expect_false(grepl('value="dyntaxa"', html, fixed = TRUE))
  expect_true(grepl("Not configured on this server (no subscription key): Dyntaxa (DYNTAXA_KEY)", html,
                    fixed = TRUE))
})

test_that("the environmental query defaults to a small area and one recent year, with a slow-query notice (I1)", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  html <- html_of(shark_ui())
  prev_year <- as.integer(format(Sys.Date(), "%Y")) - 1
  expect_true(grepl(sprintf('data-initial-date="%d-01-01"', prev_year), html, fixed = TRUE))
  expect_true(grepl(sprintf('data-initial-date="%d-12-31"', prev_year), html, fixed = TRUE))
  expect_true(grepl('id="shark_bbox_north"', html, fixed = TRUE))
  expect_match(html, 'id="shark_bbox_north"[^/]*value="58"')
  expect_match(html, 'id="shark_bbox_south"[^/]*value="57"')
  expect_match(html, 'id="shark_bbox_east"[^/]*value="12"')
  expect_match(html, 'id="shark_bbox_west"[^/]*value="11"')
  notice <- "SHARK queries take 25-90 s and pause the app for all users; keep the area and year range small."
  matches <- gregexpr(notice, html, fixed = TRUE)[[1]]
  expect_equal(length(matches), 2)
  expect_true(all(matches > 0))
})

test_that("without SHARK4R >= 1.2.0 the tab shows installation help and the server does nothing", {
  source_shark()
  local_global_mock("shark4r_installed", function() FALSE)
  html <- html_of(shark_ui())
  expect_true(grepl("not available on this server", html, fixed = TRUE))
  expect_false(grepl("shark_tabs", html, fixed = TRUE))
  expect_null(shark_server(NULL, NULL, NULL))
})

test_that("the server runs a taxonomy search and an environmental query end to end (testServer)", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  local_mocked_bindings(
    match_worms_taxa = function(taxa_names, ...) worms_row(taxa_names),
    get_shark_data = function(...) shark_rows(),
    .package = "SHARK4R"
  )
  cache_root <- withr::local_tempdir()  # the default cache_dir must not land in the repo
  local_global_mock("app_path", function(...) file.path(cache_root, ...))
  shiny::testServer(shark_server, {
    session$setInputs(shark_species_name = "Gadus morhua", shark_taxonomy_sources = "worms",
                      shark_fuzzy_search = TRUE, shark_search_taxonomy = 1)
    expect_match(as.character(output$shark_taxonomy_results$html), "126436", fixed = TRUE)

    session$setInputs(shark_date_range = as.Date(c("2024-01-01", "2024-12-31")),
                      shark_parameters = "Temperature CTD", shark_max_env_records = 5000,
                      shark_query_environmental = 1)
    expect_equal(shark_data$environmental$status, "ok")
    expect_match(as.character(output$shark_environmental_status$html), "Retrieved 3 records", fixed = TRUE)
  })
})

# ---------------------------------------------------------------------------
# Fix round 1: the QC warnings/status panel must not show a green "completed"
# when format validation failed, and must surface the coordinate check's
# message when the lat/lon columns are missing.
# ---------------------------------------------------------------------------

test_that("QC status and warnings surface a failed format validation, not a green 'completed'", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  qc <- run_shark_qc(data.frame(x = 1), "format", NA_character_)
  expect_false(qc$validation$valid)
  expect_equal(qc$validation$message, SHARK_QC_NO_DATATYPE)

  status_html <- html_of(shark_qc_status_ui(qc))
  expect_false(grepl("alert-success", status_html, fixed = TRUE))
  expect_match(status_html, SHARK_QC_NO_DATATYPE, fixed = TRUE)

  warn_html <- html_of(shark_qc_warnings_ui(qc))
  expect_match(warn_html, SHARK_QC_NO_DATATYPE, fixed = TRUE)
})

test_that("a check_fields result with errors shows those errors in the warnings panel", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  local_mocked_bindings(check_fields = function(data, datatype, ...) {
    tibble::tibble(level = "error", field = "a", row = NA_integer_, message = "Required field a is missing")
  }, .package = "SHARK4R")
  qc <- run_shark_qc(data.frame(delivery_datatype = "Zoobenthos"), "format", "Zoobenthos")
  expect_false(qc$validation$valid)

  warn_html <- html_of(shark_qc_warnings_ui(qc))
  expect_match(warn_html, "Required field a is missing", fixed = TRUE)
  expect_false(grepl("alert-success", html_of(shark_qc_status_ui(qc)), fixed = TRUE))
})

test_that("a clean QC result still shows 'No warnings detected'", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  qc <- run_shark_qc(data.frame(sample_latitude_dd = 57, sample_longitude_dd = 11), "coordinates", NA_character_)
  expect_match(html_of(shark_qc_warnings_ui(qc)), "No warnings detected", fixed = TRUE)
  expect_match(html_of(shark_qc_status_ui(qc)), "alert-success", fixed = TRUE)
})

test_that("missing lat/lon columns surface the coordinate check message in the warnings panel", {
  skip_if_not_installed("SHARK4R", "1.2.0")
  source_shark()
  qc <- run_shark_qc(data.frame(x = 1), "coordinates", NA_character_)
  expect_true(is.na(qc$coordinates$missing))
  expect_match(html_of(shark_qc_warnings_ui(qc)), "No sample_latitude_dd / sample_longitude_dd columns",
               fixed = TRUE)
})
