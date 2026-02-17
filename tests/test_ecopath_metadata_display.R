#' Test ECOPATH Metadata Extraction and Display
#'
#' Verifies that metadata is properly extracted from ECOPATH databases
#' and formatted for dashboard display

cat("\n=== Testing ECOPATH Metadata Extraction and Display ===\n\n")

# Load required function
source("ecopath_windows_import.R")

# Test file
db_file <- "coast 2011-04-10 10.00.ewemdb"

if (!file.exists(db_file)) {
  cat("Test file not found:", db_file, "\n")
  cat("Please ensure the database file exists in the current directory.\n")
  quit()
}

cat("Test file:", db_file, "\n\n")

# Import database
cat("=== Step 1: Import Database ===\n\n")

result <- tryCatch({
  parse_ecopath_native_cross_platform(db_file)
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  quit()
})

cat("\n✓ Database imported successfully\n")
cat("  Method:", result$method, "\n")
cat("  Groups:", nrow(result$group_data), "\n")
cat("  Diet links:", nrow(result$diet_data), "\n\n")

# Check metadata
cat("=== Step 2: Check Metadata ===\n\n")

if (is.null(result$metadata)) {
  cat("✗ No metadata extracted\n")
  cat("  The EcopathModel table may be missing or empty.\n")
} else {
  cat("✓ Metadata extracted\n\n")

  meta <- result$metadata

  cat("Raw metadata fields:\n")
  cat("  - name:", ifelse(is.na(meta$name), "NA", meta$name), "\n")
  cat("  - area_name:", ifelse(is.na(meta$area_name), "NA", meta$area_name), "\n")
  cat("  - country:", ifelse(is.na(meta$country), "NA", meta$country), "\n")
  cat("  - ecosystem_type:", ifelse(is.na(meta$ecosystem_type), "NA", meta$ecosystem_type), "\n")
  cat("  - first_year:", ifelse(is.na(meta$first_year), "NA", meta$first_year), "\n")
  cat("  - num_years:", ifelse(is.na(meta$num_years), "NA", meta$num_years), "\n")
  cat("  - date_start:", ifelse(is.na(meta$date_start), "NA", meta$date_start), "\n")
  cat("  - date_end:", ifelse(is.na(meta$date_end), "NA", meta$date_end), "\n")
  cat("  - author:", ifelse(is.na(meta$author), "NA", meta$author), "\n")
}

# Simulate dashboard display logic
cat("\n=== Step 3: Simulate Dashboard Display ===\n\n")

# Build location string (same logic as app.R lines 1079-1094)
location_text <- "ECOPATH Import"

if (!is.null(result$metadata)) {
  meta <- result$metadata

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
  if (length(location_parts) > 0) {
    location_text <- paste(location_parts, collapse = ", ")
  }
}

cat("Location Box Display:\n")
cat("┌─────────────────────────────────────────┐\n")
cat("│ Location                                │\n")
cat("│", sprintf("%-40s", location_text), "│\n")
cat("└─────────────────────────────────────────┘\n\n")

# Build time period string (same logic as app.R lines 1096-1107)
time_period_text <- "Model-derived"

if (!is.null(result$metadata)) {
  meta <- result$metadata

  if (!is.na(meta$first_year) && meta$first_year != -9999) {
    if (!is.na(meta$num_years) && meta$num_years > 1) {
      end_year <- meta$first_year + meta$num_years - 1
      time_period_text <- paste0(meta$first_year, "-", end_year)
    } else {
      time_period_text <- as.character(meta$first_year)
    }
  } else if (!is.na(meta$date_start) && !is.na(meta$date_end)) {
    time_period_text <- paste(meta$date_start, "to", meta$date_end)
  }
}

cat("Time Period Box Display:\n")
cat("┌─────────────────────────────────────────┐\n")
cat("│ Time Period                             │\n")
cat("│", sprintf("%-40s", time_period_text), "│\n")
cat("└─────────────────────────────────────────┘\n\n")

# Verification
cat("=== Step 4: Verification ===\n\n")

has_location <- location_text != "ECOPATH Import"
has_time_period <- time_period_text != "Model-derived"

cat("Metadata extraction:", ifelse(!is.null(result$metadata), "✓ PASS", "✗ FAIL"), "\n")
cat("Location customized:", ifelse(has_location, "✓ PASS", "⚠ GENERIC (no metadata)"), "\n")
cat("Time period customized:", ifelse(has_time_period, "✓ PASS", "⚠ GENERIC (no metadata)"), "\n\n")

if (!is.null(result$metadata) && (has_location || has_time_period)) {
  cat("✓ SUCCESS: Metadata is properly extracted and formatted!\n")
  cat("✓ Dashboard boxes will display model-specific information.\n")
} else if (!is.null(result$metadata)) {
  cat("⚠ PARTIAL: Metadata extracted but all fields are empty/missing.\n")
  cat("  This is OK if the ECOPATH database doesn't have metadata filled in.\n")
  cat("  Dashboard will show generic fallback values.\n")
} else {
  cat("✗ ISSUE: No metadata extracted from database.\n")
  cat("  The EcopathModel table may be missing.\n")
}

cat("\n=== END ===\n")
