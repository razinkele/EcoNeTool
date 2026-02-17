#' Test ECOPATH Metadata Extraction

source("ecopath_windows_import.R")

cat("\n=== Testing ECOPATH Metadata Extraction ===\n\n")

db_file <- "coast 2011-04-10 10.00.ewemdb"

if (!file.exists(db_file)) {
  cat("File not found:", db_file, "\n")
  quit()
}

cat("Parsing ECOPATH database with metadata...\n\n")

result <- parse_ecopath_native_windows(db_file)

cat("\n=== Extracted Metadata ===\n\n")

if (!is.null(result$metadata)) {
  meta <- result$metadata

  cat("Name:", ifelse(is.na(meta$name), "NOT SET", meta$name), "\n")
  cat("Area Name:", ifelse(is.na(meta$area_name), "NOT SET", meta$area_name), "\n")
  cat("Country:", ifelse(is.na(meta$country), "NOT SET", meta$country), "\n")
  cat("Ecosystem Type:", ifelse(is.na(meta$ecosystem_type), "NOT SET", meta$ecosystem_type), "\n")
  cat("First Year:", ifelse(is.na(meta$first_year) || meta$first_year == -9999, "NOT SET", meta$first_year), "\n")
  cat("Num Years:", ifelse(is.na(meta$num_years), "NOT SET", meta$num_years), "\n")
  cat("Date Start:", ifelse(is.na(meta$date_start), "NOT SET", meta$date_start), "\n")
  cat("Date End:", ifelse(is.na(meta$date_end), "NOT SET", meta$date_end), "\n")
  cat("Author:", ifelse(is.na(meta$author), "NOT SET", meta$author), "\n")

  cat("\n=== Formatted for Dashboard ===\n\n")

  # Build location
  location_parts <- c()
  if (!is.na(meta$area_name) && meta$area_name != "") {
    location_parts <- c(location_parts, meta$area_name)
  } else if (!is.na(meta$name) && meta$name != "") {
    location_parts <- c(location_parts, meta$name)
  }
  if (!is.na(meta$country) && meta$country != "") {
    location_parts <- c(location_parts, meta$country)
  }
  if (!is.na(meta$ecosystem_type) && meta$ecosystem_type != "") {
    location_parts <- c(location_parts, paste0("(", meta$ecosystem_type, ")"))
  }

  location_text <- if (length(location_parts) > 0) {
    paste(location_parts, collapse = ", ")
  } else {
    "ECOPATH Import"
  }

  # Build time period
  time_period_text <- if (!is.na(meta$first_year) && meta$first_year != -9999) {
    if (!is.na(meta$num_years) && meta$num_years > 1) {
      end_year <- meta$first_year + meta$num_years - 1
      paste0(meta$first_year, "-", end_year)
    } else {
      as.character(meta$first_year)
    }
  } else if (!is.na(meta$date_start) && !is.na(meta$date_end)) {
    paste(meta$date_start, "to", meta$date_end)
  } else {
    "Model-derived"
  }

  cat("Location:", location_text, "\n")
  cat("Time Period:", time_period_text, "\n")

} else {
  cat("No metadata extracted\n")
}

cat("\n=== END ===\n")
