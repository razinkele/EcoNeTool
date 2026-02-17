# ==============================================================================
# ECOPATH WINDOWS IMPORT
# ==============================================================================
# Windows-specific ECOPATH database parsing using DBI/RODBC
#
# Features:
#   - Read .ewemdb, .mdb, .accdb files using RODBC
#   - Comprehensive table extraction (groups, diet, metadata, scenarios)
#   - Multistanza group support (multi-stage life history)
#   - ECOSIM scenario import
#   - Fleet and pedigree data extraction
#
# Requirements:
#   - RODBC package
#   - Microsoft Access Database Engine (32-bit or 64-bit matching R)
#
# ==============================================================================

parse_ecopath_native_windows <- function(db_file) {
  #' Parse ECOPATH Native Database on Windows
  #'
  #' Uses RODBC to read ECOPATH .ewemdb, .mdb, or .accdb files on Windows.
  #' Requires Microsoft Access Database Engine to be installed.
  #'
  #' @param db_file Path to ECOPATH database file
  #' @return List containing group_data, diet_data, and table list
  #' @export

  if (!file.exists(db_file)) {
    stop("Database file not found: ", db_file)
  }

  # Check for RODBC package
  if (!requireNamespace("RODBC", quietly = TRUE)) {
    stop(
      "Package 'RODBC' required for reading ECOPATH databases on Windows.\n",
      "\n",
      "Installation steps:\n",
      "1. Install RODBC package: install.packages('RODBC')\n",
      "2. Install Microsoft Access Database Engine:\n",
      "   https://www.microsoft.com/en-us/download/details.aspx?id=54920\n",
      "   - R 64-bit: Download AccessDatabaseEngine_X64.exe\n",
      "   - R 32-bit: Download AccessDatabaseEngine.exe\n",
      "\n",
      "Your R architecture: ", R.version$arch
    )
  }

  tryCatch({
    message("Connecting to ECOPATH database: ", basename(db_file))

    # Create connection using RODBC
    # odbcConnectAccess2007 works for .mdb, .accdb, and .ewemdb files
    con <- RODBC::odbcConnectAccess2007(db_file)

    # Check connection
    if (con == -1) {
      stop(
        "Failed to connect to database.\n",
        "\n",
        "Common issues:\n",
        "1. Microsoft Access Database Engine not installed\n",
        "   Download: https://www.microsoft.com/en-us/download/details.aspx?id=54920\n",
        "2. Architecture mismatch (R 32-bit vs 64-bit)\n",
        "   Your R: ", R.version$arch, "\n",
        "3. Database file is corrupted or incompatible\n"
      )
    }

    # List available tables
    tables_info <- RODBC::sqlTables(con)
    tables <- tables_info$TABLE_NAME

    # Filter out system tables
    tables <- tables[!grepl("^MSys|^~", tables)]

    message("ECOPATH database tables found (", length(tables), "): ",
            paste(head(tables, 10), collapse = ", "),
            if (length(tables) > 10) "..." else "")

    # Find group/basic input table
    # MULTISTANZA SUPPORT: Read both regular and stanza groups
    group_table_names <- c("EcopathGroup", "Group", "Groups", "BasicInput")
    diet_table_names <- c("EcopathDietComp", "DietComposition", "Diet")

    group_table <- NULL
    stanza_table <- NULL
    diet_table <- NULL

    # Try to read regular group table
    for (tname in group_table_names) {
      if (tname %in% tables) {
        message("Reading group table: ", tname)
        group_table <- RODBC::sqlFetch(con, tname, stringsAsFactors = FALSE)
        message("  → Loaded ", nrow(group_table), " rows, ", ncol(group_table), " columns")
        message("  → Columns: ", paste(colnames(group_table), collapse = ", "))
        break
      }
    }

    # Try to read multistanza group table (species with multiple life stages)
    if ("stanzaEcopathGroup" %in% tables) {
      message("Reading multistanza group table: stanzaEcopathGroup")
      stanza_table <- RODBC::sqlFetch(con, "stanzaEcopathGroup", stringsAsFactors = FALSE)
      message("  → Loaded ", nrow(stanza_table), " multistanza groups")
      message("  → Columns: ", paste(colnames(stanza_table), collapse = ", "))
    }

    # Merge regular and stanza groups if both exist
    if (!is.null(group_table) && !is.null(stanza_table)) {
      message("Merging regular and multistanza groups...")

      # Add stanza indicator to both tables
      group_table$is_multistanza <- FALSE
      stanza_table$is_multistanza <- TRUE

      # Find common columns
      common_cols <- intersect(colnames(group_table), colnames(stanza_table))
      message("  → Common columns: ", length(common_cols))

      # Add missing columns from each table to the other with NA values
      for (col in setdiff(colnames(stanza_table), colnames(group_table))) {
        group_table[[col]] <- NA
      }
      for (col in setdiff(colnames(group_table), colnames(stanza_table))) {
        stanza_table[[col]] <- NA
      }

      # Combine both tables
      group_table <- rbind(group_table, stanza_table)
      message("  → Combined ", nrow(group_table), " total groups (", sum(group_table$is_multistanza), " multistanza)")
    } else if (!is.null(stanza_table) && is.null(group_table)) {
      # Only stanza groups found
      message("Using only multistanza groups (no regular group table found)")
      group_table <- stanza_table
      group_table$is_multistanza <- TRUE
    } else if (!is.null(group_table)) {
      # Only regular groups found
      group_table$is_multistanza <- FALSE
    }

    # Try to read diet table
    for (tname in diet_table_names) {
      if (tname %in% tables) {
        message("Reading diet table: ", tname)
        diet_table <- RODBC::sqlFetch(con, tname, stringsAsFactors = FALSE)
        message("  → Loaded ", nrow(diet_table), " rows, ", ncol(diet_table), " columns")
        message("  → Columns: ", paste(colnames(diet_table), collapse = ", "))
        break
      }
    }

    # Try to read metadata from EcopathModel table
    metadata <- NULL
    if ("EcopathModel" %in% tables) {
      message("Reading metadata from EcopathModel table...")
      tryCatch({
        model_info <- RODBC::sqlFetch(con, "EcopathModel", stringsAsFactors = FALSE)
        if (nrow(model_info) > 0) {
          metadata <- list(
            # Basic identification
            name = if (!is.null(model_info$Name)) model_info$Name[1] else NA,
            area_name = if (!is.null(model_info$AreaName)) model_info$AreaName[1] else NA,
            description = if (!is.null(model_info$Description)) model_info$Description[1] else NA,

            # Geographic info
            country = if (!is.null(model_info$Country)) model_info$Country[1] else NA,
            ecosystem_type = if (!is.null(model_info$EcosystemType)) model_info$EcosystemType[1] else NA,
            area = if (!is.null(model_info$Area)) model_info$Area[1] else NA,
            min_lon = if (!is.null(model_info$MinLon)) model_info$MinLon[1] else NA,
            max_lon = if (!is.null(model_info$MaxLon)) model_info$MaxLon[1] else NA,
            min_lat = if (!is.null(model_info$MinLat)) model_info$MinLat[1] else NA,
            max_lat = if (!is.null(model_info$MaxLat)) model_info$MaxLat[1] else NA,

            # Temporal info
            first_year = if (!is.null(model_info$FirstYear)) model_info$FirstYear[1] else NA,
            num_years = if (!is.null(model_info$NumYears)) model_info$NumYears[1] else NA,
            date_start = if (!is.null(model_info$DateStart)) model_info$DateStart[1] else NA,
            date_end = if (!is.null(model_info$DateEnd)) model_info$DateEnd[1] else NA,

            # Attribution
            author = if (!is.null(model_info$Author)) model_info$Author[1] else NA,
            contact = if (!is.null(model_info$Contact)) model_info$Contact[1] else NA,

            # Publication references
            publication_doi = if (!is.null(model_info$PublicationDOI)) model_info$PublicationDOI[1] else NA,
            publication_uri = if (!is.null(model_info$PublicationURI)) model_info$PublicationURI[1] else NA,
            publication_ref = if (!is.null(model_info$PublicationRef)) model_info$PublicationRef[1] else NA,

            # Technical
            last_saved = if (!is.null(model_info$LastSaved)) model_info$LastSaved[1] else NA,
            code_ecobase = if (!is.null(model_info$CodeEcobase)) model_info$CodeEcobase[1] else NA
          )
          message("  → Extracted metadata: ", metadata$name)
        }
      }, error = function(e) {
        message("  → Could not read metadata: ", e$message)
      })
    }

    # Try to read multistanza parameters from Stanza table
    stanza_params <- NULL
    if ("Stanza" %in% tables) {
      message("Reading multistanza parameters from Stanza table...")
      tryCatch({
        stanza_params <- RODBC::sqlFetch(con, "Stanza", stringsAsFactors = FALSE)
        message("  → Loaded ", nrow(stanza_params), " multistanza groups")
        message("  → Columns: ", paste(colnames(stanza_params), collapse = ", "))
      }, error = function(e) {
        message("  → Could not read Stanza table: ", e$message)
      })
    }

    # Try to read fleet details from EcopathFleet table
    fleet_data <- NULL
    if ("EcopathFleet" %in% tables) {
      message("Reading fleet details from EcopathFleet table...")
      tryCatch({
        fleet_data <- RODBC::sqlFetch(con, "EcopathFleet", stringsAsFactors = FALSE)
        message("  → Loaded ", nrow(fleet_data), " fleets")
        if (nrow(fleet_data) > 0) {
          message("  → Fleet names: ", paste(fleet_data$FleetName, collapse = ", "))
        }
      }, error = function(e) {
        message("  → Could not read EcopathFleet table: ", e$message)
      })
    }

    # Try to read discard fate from EcopathDiscardFate table
    discard_fate <- NULL
    if ("EcopathDiscardFate" %in% tables) {
      message("Reading discard fate from EcopathDiscardFate table...")
      tryCatch({
        discard_fate <- RODBC::sqlFetch(con, "EcopathDiscardFate", stringsAsFactors = FALSE)
        message("  → Loaded ", nrow(discard_fate), " discard fate entries")
      }, error = function(e) {
        message("  → Could not read EcopathDiscardFate table: ", e$message)
      })
    }

    # Try to read pedigree/calibration data from EcopathGroupPedigree table
    pedigree_data <- NULL
    if ("EcopathGroupPedigree" %in% tables) {
      message("Reading pedigree/calibration data from EcopathGroupPedigree table...")
      tryCatch({
        pedigree_data <- RODBC::sqlFetch(con, "EcopathGroupPedigree", stringsAsFactors = FALSE)
        message("  → Loaded ", nrow(pedigree_data), " pedigree entries")
        if (nrow(pedigree_data) > 0 && "VarName" %in% colnames(pedigree_data)) {
          unique_vars <- unique(pedigree_data$VarName)
          message("  → Variables tracked: ", paste(unique_vars, collapse = ", "))
        }
      }, error = function(e) {
        message("  → Could not read EcopathGroupPedigree table: ", e$message)
      })
    }

    # Try to read Pedigree lookup table (confidence levels)
    pedigree_levels <- NULL
    if ("Pedigree" %in% tables) {
      message("Reading pedigree confidence levels from Pedigree table...")
      tryCatch({
        pedigree_levels <- RODBC::sqlFetch(con, "Pedigree", stringsAsFactors = FALSE)
        message("  → Loaded ", nrow(pedigree_levels), " pedigree confidence levels")
      }, error = function(e) {
        message("  → Could not read Pedigree table: ", e$message)
      })
    }

    # Try to read Auxillary table (tooltips/comments/remarks for parameter values)
    auxillary_data <- NULL
    if ("Auxillary" %in% tables) {
      message("Reading tooltips/comments from Auxillary table...")
      tryCatch({
        auxillary_data <- RODBC::sqlFetch(con, "Auxillary", stringsAsFactors = FALSE)
        message("  → Loaded ", nrow(auxillary_data), " tooltips/comments")

        # Count non-empty remarks
        if ("Remark" %in% colnames(auxillary_data)) {
          non_empty_remarks <- sum(!is.na(auxillary_data$Remark) & nchar(as.character(auxillary_data$Remark)) > 0)
          message("  → ", non_empty_remarks, " entries with remarks/comments")
        }
      }, error = function(e) {
        message("  → Could not read Auxillary table: ", e$message)
      })
    }

    # Try to read ECOSIM scenario information
    ecosim_scenarios <- NULL
    if ("EcosimScenario" %in% tables) {
      message("Reading ECOSIM scenario information...")
      tryCatch({
        # Main scenario table
        scenarios <- RODBC::sqlFetch(con, "EcosimScenario", stringsAsFactors = FALSE)

        if (nrow(scenarios) > 0) {
          message("  → Found ", nrow(scenarios), " ECOSIM scenarios")

          # Read related scenario tables if available
          scenario_groups <- NULL
          scenario_fleets <- NULL
          scenario_forcing <- NULL
          scenario_shapes <- NULL

          # Group-specific parameters
          if ("EcosimScenarioGroup" %in% tables) {
            scenario_groups <- RODBC::sqlFetch(con, "EcosimScenarioGroup", stringsAsFactors = FALSE)
            message("  → Loaded scenario group parameters: ", nrow(scenario_groups), " rows")
          }

          # Fleet-specific parameters
          if ("EcosimScenarioFleet" %in% tables) {
            scenario_fleets <- RODBC::sqlFetch(con, "EcosimScenarioFleet", stringsAsFactors = FALSE)
            message("  → Loaded scenario fleet parameters: ", nrow(scenario_fleets), " rows")
          }

          # Predator-prey forcing matrix (vulnerabilities)
          if ("EcosimScenarioForcingMatrix" %in% tables) {
            scenario_forcing <- RODBC::sqlFetch(con, "EcosimScenarioForcingMatrix", stringsAsFactors = FALSE)
            message("  → Loaded predator-prey vulnerabilities: ", nrow(scenario_forcing), " rows")
          }

          # Temporal forcing shapes
          if ("EcosimShape" %in% tables) {
            scenario_shapes <- RODBC::sqlFetch(con, "EcosimShape", stringsAsFactors = FALSE)
            message("  → Loaded temporal forcing shapes: ", nrow(scenario_shapes), " rows")
          }

          # Package all ECOSIM data together
          ecosim_scenarios <- list(
            scenarios = scenarios,
            scenario_groups = scenario_groups,
            scenario_fleets = scenario_fleets,
            scenario_forcing = scenario_forcing,
            scenario_shapes = scenario_shapes
          )

          # Print scenario summary
          message("\n  ECOSIM Scenarios Available:")
          for (i in 1:min(5, nrow(scenarios))) {
            scenario_name <- if (!is.null(scenarios$ScenarioName)) scenarios$ScenarioName[i] else paste("Scenario", scenarios$ScenarioID[i])
            scenario_desc <- if (!is.null(scenarios$Description)) {
              desc <- scenarios$Description[i]
              if (!is.na(desc) && nchar(desc) > 50) {
                paste0(substr(desc, 1, 47), "...")
              } else {
                desc
              }
            } else {
              NA
            }
            message(sprintf("    %d. %s", scenarios$ScenarioID[i], scenario_name))
            if (!is.na(scenario_desc)) {
              message(sprintf("       %s", scenario_desc))
            }
          }
          if (nrow(scenarios) > 5) {
            message(sprintf("    ... and %d more scenarios", nrow(scenarios) - 5))
          }
        }
      }, error = function(e) {
        message("  → Could not read ECOSIM scenarios: ", e$message)
      })
    }

    # Close connection
    RODBC::odbcClose(con)
    message("✓ Database connection closed")

    # Validate results
    if (is.null(group_table)) {
      stop(
        "Could not find group/basic input table.\n",
        "Available tables: ", paste(tables, collapse = ", "), "\n",
        "Expected one of: ", paste(group_table_names, collapse = ", ")
      )
    }

    if (is.null(diet_table)) {
      stop(
        "Could not find diet composition table.\n",
        "Available tables: ", paste(tables, collapse = ", "), "\n",
        "Expected one of: ", paste(diet_table_names, collapse = ", ")
      )
    }

    # Return results
    list(
      group_data = group_table,
      diet_data = diet_table,
      metadata = metadata,
      stanza_params = stanza_params,
      fleet_data = fleet_data,
      discard_fate = discard_fate,
      pedigree_data = pedigree_data,
      pedigree_levels = pedigree_levels,
      auxillary_data = auxillary_data,
      ecosim_scenarios = ecosim_scenarios,
      tables = tables,
      method = "RODBC (Windows)"
    )

  }, error = function(e) {
    stop("Error reading ECOPATH database: ", e$message)
  })
}
