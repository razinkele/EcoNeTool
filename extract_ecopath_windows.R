#!/usr/bin/env Rscript
#' Extract ECOPATH Database on Windows
#'
#' This script attempts to extract data from .ewemdb/.mdb files using
#' Windows-compatible methods (no mdbtools required)

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

library(DBI)

#' Method 1: Using RODBC (requires Microsoft Access Database Engine)
extract_with_rodbc <- function(db_file) {
  if (!requireNamespace("RODBC", quietly = TRUE)) {
    cat("Installing RODBC package...\n")
    install.packages("RODBC", quiet = TRUE)
  }

  library(RODBC)

  tryCatch({
    cat("Attempting connection with RODBC...\n")

    # Try Access 2007+ format first
    con <- tryCatch({
      odbcConnectAccess2007(db_file)
    }, error = function(e) {
      # Try older Access format
      odbcConnectAccess(db_file)
    })

    # List all tables
    tables <- sqlTables(con)
    cat("\nAvailable tables:\n")
    print(tables$TABLE_NAME)

    # Common ECOPATH table names
    table_names <- c("EcopathGroup", "stanzaEcopathGroup", "Group",
                     "EcopathDietComp", "DietComposition", "Diet")

    results <- list()

    for (tbl in table_names) {
      data <- tryCatch({
        sqlFetch(con, tbl)
      }, error = function(e) NULL)

      if (!is.null(data) && nrow(data) > 0) {
        cat("\n✓ Found table:", tbl, "with", nrow(data), "rows\n")
        results[[tbl]] <- data

        # Save to CSV
        csv_file <- paste0(tools::file_path_sans_ext(basename(db_file)),
                          "_", tbl, ".csv")
        write.csv(data, csv_file, row.names = FALSE)
        cat("  Saved to:", csv_file, "\n")
      }
    }

    odbcClose(con)
    return(results)

  }, error = function(e) {
    cat("\n✗ RODBC method failed:", e$message, "\n")
    cat("\nYou may need to install Microsoft Access Database Engine:\n")
    cat("https://www.microsoft.com/en-us/download/details.aspx?id=54920\n")
    return(NULL)
  })
}


#' Method 2: Using odbc package with Access driver
extract_with_odbc <- function(db_file) {
  if (!requireNamespace("odbc", quietly = TRUE)) {
    cat("Installing odbc package...\n")
    install.packages("odbc", quiet = TRUE)
  }

  library(odbc)
  library(DBI)

  tryCatch({
    cat("\nAttempting connection with odbc package...\n")

    con <- dbConnect(
      odbc::odbc(),
      .connection_string = paste0(
        "Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
        "DBQ=", normalizePath(db_file), ";"
      )
    )

    # List all tables
    tables <- dbListTables(con)
    cat("\nAvailable tables:\n")
    print(tables)

    results <- list()

    for (tbl in tables) {
      if (grepl("Ecopath|Group|Diet", tbl, ignore.case = TRUE)) {
        data <- dbReadTable(con, tbl)
        cat("\n✓ Found table:", tbl, "with", nrow(data), "rows\n")
        results[[tbl]] <- data

        # Save to CSV
        csv_file <- paste0(tools::file_path_sans_ext(basename(db_file)),
                          "_", tbl, ".csv")
        write.csv(data, csv_file, row.names = FALSE)
        cat("  Saved to:", csv_file, "\n")
      }
    }

    dbDisconnect(con)
    return(results)

  }, error = function(e) {
    cat("\n✗ odbc method failed:", e$message, "\n")
    return(NULL)
  })
}


#' Main extraction function
extract_ecopath_database <- function(db_file) {
  if (!file.exists(db_file)) {
    stop("File not found: ", db_file)
  }

  cat(paste0(rep("=", 60), collapse = ""), "\n")
  cat("ECOPATH Database Extractor for Windows\n")
  cat(paste0(rep("=", 60), collapse = ""), "\n")
  cat("Database:", db_file, "\n")
  cat("Size:", round(file.size(db_file) / 1024, 2), "KB\n\n")

  # Try Method 1: RODBC
  cat("\n--- METHOD 1: RODBC ---\n")
  results <- extract_with_rodbc(db_file)

  if (is.null(results)) {
    # Try Method 2: odbc
    cat("\n--- METHOD 2: odbc package ---\n")
    results <- extract_with_odbc(db_file)
  }

  if (is.null(results)) {
    cat("\n", paste0(rep("=", 60), collapse = ""), "\n")
    cat("FAILED: Could not extract data\n")
    cat("\nAlternative solutions:\n")
    cat("1. Install Microsoft Access Database Engine 2016:\n")
    cat("   https://www.microsoft.com/en-us/download/details.aspx?id=54920\n\n")
    cat("2. Use WSL with mdbtools (see WINDOWS_MDBTOOLS_GUIDE.md)\n\n")
    cat("3. Export from ECOPATH software directly\n")
    cat(paste0(rep("=", 60), collapse = ""), "\n")
  } else {
    cat("\n", paste0(rep("=", 60), collapse = ""), "\n")
    cat("SUCCESS: Data extracted to CSV files\n")
    cat("\nYou can now use these CSV files with the\n")
    cat("'Import ECOPATH CSV/Excel Exports' section in the app.\n")
    cat(paste0(rep("=", 60), collapse = ""), "\n")
  }

  return(results)
}


# ============================================================================
# RUN EXTRACTION
# ============================================================================

# Set the database file path
db_file <- "coast 2011-04-10 10.00.ewemdb"

# Run extraction
if (file.exists(db_file)) {
  results <- extract_ecopath_database(db_file)
} else {
  cat("Error: Database file not found:", db_file, "\n")
  cat("Please update the db_file path in this script.\n")
}
