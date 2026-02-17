#' Check Diet Structure in Parsed EcoBase Data

library(RCurl)
library(XML)

source("R/functions/ecobase_connection.R")

cat("\n=== Examining Diet Structure ===\n\n")

# Get model data
model_id <- 403
cat("Downloading model", model_id, "...\n")
input_data <- get_ecobase_model_input(model_id)

cat("\n✓ Got", length(input_data), "groups\n\n")

# Check first few groups for diet data
cat("Checking for diet data in groups:\n\n")

for (i in 1:min(10, length(input_data))) {
  group <- input_data[[i]]

  if (is.list(group)) {
    group_name <- group[["group_name"]]
    if (is.null(group_name)) group_name <- paste0("Group_", i)

    cat(i, ".", group_name, "\n")
    cat("   Fields:", paste(head(names(group), 20), collapse = ", "), "\n")

    # Check for diet-related fields
    diet_fields <- names(group)[grepl("diet", names(group), ignore.case = TRUE)]
    if (length(diet_fields) > 0) {
      cat("   >>> Diet fields found:", paste(diet_fields, collapse = ", "), "\n")

      for (diet_field in diet_fields) {
        diet_value <- group[[diet_field]]
        cat("       ", diet_field, "- Type:", class(diet_value), ", Length:", length(diet_value), "\n")

        if (is.list(diet_value)) {
          cat("       ", diet_field, "contents:\n")
          for (j in 1:min(3, length(diet_value))) {
            cat("          ", j, ":", names(diet_value)[j], "=", diet_value[[j]], "\n")
          }
        }
      }
    } else {
      cat("   No diet fields\n")
    }
    cat("\n")
  }
}

cat("\n=== Detailed Look at Group with Diet ===\n\n")

# Find a group with diet
for (i in 1:length(input_data)) {
  group <- input_data[[i]]
  if (is.list(group)) {
    diet_fields <- names(group)[grepl("diet", names(group), ignore.case = TRUE)]
    if (length(diet_fields) > 0) {
      cat("Found diet in group", i, ":", group[["group_name"]], "\n\n")

      cat("All fields in this group:\n")
      for (field_name in names(group)) {
        field_value <- group[[field_name]]
        if (is.list(field_value)) {
          cat("  ", field_name, ": [LIST with", length(field_value), "elements]\n")
        } else {
          cat("  ", field_name, ":", substr(as.character(field_value), 1, 50), "\n")
        }
      }

      cat("\nDiet field details:\n")
      for (diet_field in diet_fields) {
        diet_value <- group[[diet_field]]
        cat("\n", diet_field, ":\n")
        if (is.list(diet_value)) {
          cat("  Type: LIST\n")
          cat("  Length:", length(diet_value), "\n")
          cat("  Names:", paste(names(diet_value), collapse = ", "), "\n")
          cat("  Structure:\n")
          str(diet_value, max.level = 2)
        }
      }

      break
    }
  }
}

cat("\n=== END ===\n")
