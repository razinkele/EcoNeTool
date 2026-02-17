#' Diagnose EcoBase Data Structure
#'
#' This script examines the actual structure of data returned from EcoBase
#' to understand how to properly extract parameters

cat("\n=== EcoBase Data Structure Diagnostic ===\n\n")

# Source the module
source("R/functions/ecobase_connection.R")

# Check packages
cat("Checking packages...\n")
require_ecobase_packages()
library(RCurl)
library(XML)

cat("✓ Packages loaded\n\n")

# Download a small model
model_id <- 403
cat("Downloading model", model_id, "input parameters...\n\n")

h <- basicTextGatherer()
curlPerform(
  url = paste0('http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client.php?no_model=', model_id),
  writefunction = h$update,
  verbose = FALSE
)

# Parse XML
cat("Parsing XML response...\n")
data <- xmlTreeParse(h$value(), useInternalNodes = TRUE)

cat("✓ XML parsed\n\n")

# Extract group data
cat("Extracting group data with xpathSApply...\n")
input_data <- xpathSApply(data, '//group', function(x) xmlToList(x))

cat("✓ Extracted", length(input_data), "groups\n\n")

# Examine structure
cat("=== STRUCTURE ANALYSIS ===\n\n")

cat("1. Overall structure:\n")
cat("   Class:", class(input_data), "\n")
cat("   Length:", length(input_data), "\n")
cat("   Mode:", mode(input_data), "\n\n")

# First group
cat("2. First group (index 1):\n")
first_group <- input_data[[1]]
cat("   Class:", class(first_group), "\n")
cat("   Length:", length(first_group), "\n")
cat("   Names:", paste(head(names(first_group), 10), collapse = ", "), "\n")
cat("   Type:", typeof(first_group), "\n\n")

if (is.list(first_group)) {
  cat("   First group is a LIST\n")
  cat("   Elements:\n")
  for (elem_name in head(names(first_group), 10)) {
    elem_value <- first_group[[elem_name]]
    cat("      ", elem_name, "=", elem_value, "(", class(elem_value), ")\n")
  }
} else if (is.character(first_group)) {
  cat("   First group is CHARACTER VECTOR\n")
  cat("   Value:", first_group, "\n")
}

cat("\n3. Second group (index 2):\n")
if (length(input_data) >= 2) {
  second_group <- input_data[[2]]
  cat("   Class:", class(second_group), "\n")
  cat("   Length:", length(second_group), "\n")
  cat("   Names:", paste(head(names(second_group), 10), collapse = ", "), "\n\n")

  if (is.list(second_group)) {
    cat("   Elements:\n")
    for (elem_name in head(names(second_group), 10)) {
      elem_value <- second_group[[elem_name]]
      cat("      ", elem_name, "=", substr(as.character(elem_value), 1, 50), "(", class(elem_value), ")\n")
    }
  }
}

# Check if any groups are lists
cat("\n4. Checking group types:\n")
group_types <- sapply(input_data[1:min(10, length(input_data))], class)
cat("   First 10 group types:", paste(unique(group_types), collapse = ", "), "\n")

# Count list vs character
n_lists <- sum(sapply(input_data, is.list))
n_chars <- sum(sapply(input_data, is.character))
cat("   Total groups that are lists:", n_lists, "\n")
cat("   Total groups that are characters:", n_chars, "\n\n")

# If we have list groups, examine one in detail
if (n_lists > 0) {
  cat("5. Detailed examination of a list group:\n")
  list_idx <- which(sapply(input_data, is.list))[1]
  cat("   Found list at index:", list_idx, "\n\n")

  list_group <- input_data[[list_idx]]
  cat("   All fields in this group:\n")
  for (field_name in names(list_group)) {
    field_value <- list_group[[field_name]]
    if (is.list(field_value)) {
      cat("      ", field_name, ": [LIST with", length(field_value), "elements]\n")
    } else {
      cat("      ", field_name, ":", substr(as.character(field_value), 1, 60), "\n")
    }
  }
}

# Try to find biomass, pb, qb fields
cat("\n6. Looking for expected fields (biomass, pb, qb, ee):\n")
for (i in 1:min(20, length(input_data))) {
  group <- input_data[[i]]
  if (is.list(group)) {
    has_biomass <- !is.null(group[["biomass"]]) || !is.null(group[["B"]])
    has_pb <- !is.null(group[["pb"]]) || !is.null(group[["PB"]])
    has_qb <- !is.null(group[["qb"]]) || !is.null(group[["QB"]])

    if (has_biomass || has_pb || has_qb) {
      cat("   Group", i, "has fields:", paste(names(group), collapse = ", "), "\n")
      break
    }
  }
}

cat("\n=== END DIAGNOSTIC ===\n\n")
