#' Test EcoBase Metadata Extraction
#'
#' Tests the new metadata extraction functionality for EcoBase models

source("R/functions/ecobase_connection.R")

cat("\n=== Testing EcoBase Metadata Extraction ===\n\n")

model_id <- 608  # Central Baltic Sea

cat("Testing model:", model_id, "\n\n")

# Test metadata extraction function
cat("=== Step 1: Extract Metadata ===\n\n")

metadata <- get_ecobase_model_metadata(model_id)

if (!is.null(metadata)) {
  cat("\n=== Extracted Metadata ===\n\n")

  cat("Model Name:", ifelse(is.na(metadata$model_name), "NOT SET", metadata$model_name), "\n")
  cat("Country:", ifelse(is.na(metadata$country), "NOT SET", metadata$country), "\n")
  cat("Ecosystem Type:", ifelse(is.na(metadata$ecosystem_type), "NOT SET", metadata$ecosystem_type), "\n")
  cat("Area:", ifelse(is.na(metadata$area), "NOT SET", metadata$area), "\n")
  cat("Region:", ifelse(is.na(metadata$region), "NOT SET", metadata$region), "\n")
  cat("Model Year:", ifelse(is.na(metadata$model_year), "NOT SET", metadata$model_year), "\n")
  cat("Model Period:", ifelse(is.na(metadata$model_period), "NOT SET", metadata$model_period), "\n")
  cat("Author:", ifelse(is.na(metadata$author), "NOT SET", metadata$author), "\n")

  cat("\n=== Formatted for Dashboard ===\n\n")

  # Build location (same logic as app.R)
  location_parts <- c()
  if (!is.na(metadata$model_name) && metadata$model_name != "") {
    location_parts <- c(location_parts, metadata$model_name)
  }
  if (!is.na(metadata$country) && metadata$country != "" && metadata$country != "Not affiliated") {
    location_parts <- c(location_parts, metadata$country)
  }
  if (!is.na(metadata$ecosystem_type) && metadata$ecosystem_type != "") {
    location_parts <- c(location_parts, paste0("(", metadata$ecosystem_type, ")"))
  }

  location_text <- if (length(location_parts) > 0) {
    paste(location_parts, collapse = ", ")
  } else {
    paste0("EcoBase Model #", model_id)
  }

  # Build time period (same logic as app.R)
  time_period_text <- if (!is.na(metadata$model_year) && metadata$model_year != "" && metadata$model_year != "0") {
    as.character(metadata$model_year)
  } else {
    "EcoBase Import"
  }

  cat("Location:", location_text, "\n")
  cat("Time Period:", time_period_text, "\n")

} else {
  cat("No metadata extracted\n")
}

cat("\n=== Step 2: Test Hybrid Import with Metadata ===\n\n")

result <- convert_ecobase_to_econetool_hybrid(model_id)

cat("\n=== Import Results ===\n\n")

cat("Network:\n")
cat("  - Species/groups:", igraph::vcount(result$net), "\n")
cat("  - Trophic links:", igraph::ecount(result$net), "\n")

cat("\nMetadata included in result:", !is.null(result$metadata), "\n")

if (!is.null(result$metadata)) {
  cat("\nMetadata fields:\n")
  print(names(result$metadata))

  cat("\n=== Dashboard Display ===\n\n")

  # Same formatting logic as app.R
  location_parts <- c()
  if (!is.na(result$metadata$model_name) && result$metadata$model_name != "") {
    location_parts <- c(location_parts, result$metadata$model_name)
  }
  if (!is.na(result$metadata$country) && result$metadata$country != "" && result$metadata$country != "Not affiliated") {
    location_parts <- c(location_parts, result$metadata$country)
  }
  if (!is.na(result$metadata$ecosystem_type) && result$metadata$ecosystem_type != "") {
    location_parts <- c(location_parts, paste0("(", result$metadata$ecosystem_type, ")"))
  }

  location_text <- if (length(location_parts) > 0) {
    paste(location_parts, collapse = ", ")
  } else {
    paste0("EcoBase Model #", model_id)
  }

  time_period_text <- if (!is.na(result$metadata$model_year) && result$metadata$model_year != "" && result$metadata$model_year != "0") {
    as.character(result$metadata$model_year)
  } else {
    "EcoBase Import"
  }

  cat("Dashboard will show:\n")
  cat("  Location:", location_text, "\n")
  cat("  Time Period:", time_period_text, "\n")
  cat("  Source: EcoBase #", model_id, "\n")
}

cat("\n=== END ===\n")
