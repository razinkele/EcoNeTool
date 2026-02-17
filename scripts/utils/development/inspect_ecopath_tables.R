# Inspect ECOPATH Database Tables
# Shows all tables and their columns to understand ECOSIM scenario structure

library(RODBC)

db_file <- "examples/LTgoby.eweaccdb"

cat("================================================================================\n")
cat("ECOPATH Database Table Inspector\n")
cat("================================================================================\n\n")
cat("Database:", db_file, "\n\n")

# Connect to database
con <- RODBC::odbcConnectAccess2007(db_file)

if (con == -1) {
  stop("Failed to connect to database")
}

# Get all tables
tables_info <- RODBC::sqlTables(con)
tables <- tables_info$TABLE_NAME

# Filter out system tables
tables <- tables[!grepl("^MSys|^~", tables)]

cat("Found", length(tables), "tables:\n")
cat(strrep("-", 80), "\n")

# Inspect each table
for (tname in tables) {
  cat("\nTable:", tname, "\n")

  tryCatch({
    # Get table data
    data <- RODBC::sqlFetch(con, tname, stringsAsFactors = FALSE, max = 5)

    cat("  Rows:", RODBC::sqlQuery(con, paste0("SELECT COUNT(*) FROM [", tname, "]"))[[1]], "\n")
    cat("  Columns (", ncol(data), "):", paste(colnames(data), collapse = ", "), "\n")

    # Show first few rows for small tables or ECOSIM-related tables
    if (grepl("ecosim|scenario|temporal|time|simulation|forcing|mediation", tolower(tname))) {
      cat("\n  ECOSIM-RELATED TABLE - Sample Data:\n")
      if (nrow(data) > 0) {
        print(head(data, 3))
      } else {
        cat("    (empty table)\n")
      }
    } else if (RODBC::sqlQuery(con, paste0("SELECT COUNT(*) FROM [", tname, "]"))[[1]] <= 10) {
      cat("\n  Sample Data (small table):\n")
      if (nrow(data) > 0) {
        print(head(data, 3))
      } else {
        cat("    (empty table)\n")
      }
    }

  }, error = function(e) {
    cat("  ERROR reading table:", e$message, "\n")
  })
}

# Close connection
RODBC::odbcClose(con)

cat("\n", strrep("=", 80), "\n")
cat("Inspection complete\n")
