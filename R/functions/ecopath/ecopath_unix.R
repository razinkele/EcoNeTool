# ==============================================================================
# ECOPATH UNIX/LINUX IMPORT
# ==============================================================================
# Unix/Linux-specific ECOPATH database parsing using mdb-tools
#
# Features:
#   - Read .ewemdb, .mdb, .accdb files using Hmisc + mdb-tools
#   - Extract group and diet data
#   - Basic metadata extraction
#
# Requirements:
#   - Hmisc package
#   - mdb-tools system package (mdb-export command)
#
# ==============================================================================

parse_ecopath_native_linux <- function(db_file) {
  #' Parse ECOPATH Native Database on Linux/Mac
  #'
  #' Uses Hmisc package + mdbtools to read ECOPATH .ewemdb, .mdb, or .accdb files
  #' on Linux and Mac systems.
  #'
  #' @param db_file Path to ECOPATH database file
  #' @return List containing group_data, diet_data, and metadata
  #' @export

  if (!file.exists(db_file)) {
    stop("Database file not found: ", db_file)
  }

  # Check for Hmisc package
  if (!requireNamespace("Hmisc", quietly = TRUE)) {
    stop(
      "Package 'Hmisc' required for reading ECOPATH databases on Linux/Mac.\n",
      "\n",
      "Installation steps:\n",
      "1. Install mdbtools: sudo apt-get install mdbtools (Ubuntu/Debian)\n",
      "                     brew install mdbtools (macOS with Homebrew)\n",
      "2. Install Hmisc package: install.packages('Hmisc')\n"
    )
  }

  # Check if mdbtools is available
  mdb_check <- system("which mdb-tables", intern = TRUE, ignore.stderr = TRUE)
  if (length(mdb_check) == 0 || mdb_check == "") {
    stop(
      "mdbtools not found on system.\n",
      "\n",
      "Installation:\n",
      "  Ubuntu/Debian: sudo apt-get install mdbtools\n",
      "  macOS: brew install mdbtools\n",
      "  Fedora: sudo dnf install mdbtools\n"
    )
  }

  tryCatch({
    message("Connecting to ECOPATH database using mdbtools: ", basename(db_file))

    # List tables using Hmisc::mdb.get
    tables <- tryCatch({
      Hmisc::mdb.get(db_file, tables = TRUE)
    }, error = function(e) {
      stop("Failed to read database tables: ", e$message)
    })

    # Filter out system tables
    tables <- tables[!grepl("^MSys|^~", tables)]

    message("ECOPATH database tables found (", length(tables), "): ",
            paste(head(tables, 10), collapse = ", "),
            if (length(tables) > 10) "..." else "")

    # Find group/basic input table
    group_table_names <- c("EcopathGroup", "Group", "Groups", "BasicInput")
    diet_table_names <- c("EcopathDietComp", "DietComposition", "Diet")

    group_table <- NULL
    diet_table <- NULL

    # Try to read group table
    for (tname in group_table_names) {
      if (tname %in% tables) {
        message("Reading group table: ", tname)
        group_table <- tryCatch({
          Hmisc::mdb.get(db_file, tables = tname)
        }, error = function(e) {
          message("  → Warning: Could not read ", tname, ": ", e$message)
          NULL
        })
        if (!is.null(group_table)) {
          message("  → Loaded ", nrow(group_table), " rows, ", ncol(group_table), " columns")
          break
        }
      }
    }

    if (is.null(group_table)) {
      stop("Could not find group table. Available tables: ", paste(tables, collapse = ", "))
    }

    # Try to read diet table
    for (tname in diet_table_names) {
      if (tname %in% tables) {
        message("Reading diet table: ", tname)
        diet_table <- tryCatch({
          Hmisc::mdb.get(db_file, tables = tname)
        }, error = function(e) {
          message("  → Warning: Could not read ", tname, ": ", e$message)
          NULL
        })
        if (!is.null(diet_table)) {
          message("  → Loaded ", nrow(diet_table), " rows, ", ncol(diet_table), " columns")
          break
        }
      }
    }

    if (is.null(diet_table)) {
      warning("Diet composition table not found. Network may be incomplete.")
      # Create empty diet table
      diet_table <- data.frame(
        PreyID = integer(0),
        PredatorID = integer(0),
        DietComp = numeric(0)
      )
    }

    # Try to read metadata
    metadata <- list()
    if ("EcopathModel" %in% tables) {
      model_info <- tryCatch({
        Hmisc::mdb.get(db_file, tables = "EcopathModel")
      }, error = function(e) NULL)
      if (!is.null(model_info) && nrow(model_info) > 0) {
        metadata$model_name <- model_info$Name[1]
        metadata$description <- model_info$Description[1]
      }
    }

    return(list(
      group_data = group_table,
      diet_data = diet_table,
      tables = tables,
      metadata = metadata,
      method = "Hmisc + mdbtools (Linux/Mac)"
    ))

  }, error = function(e) {
    stop("Error parsing ECOPATH database on Linux/Mac: ", e$message)
  })
}
