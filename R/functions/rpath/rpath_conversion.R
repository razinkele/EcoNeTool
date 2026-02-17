# ==============================================================================
# RPATH DATA CONVERSION
# ==============================================================================
# Convert ECOPATH database imports to Rpath format for mass-balance modeling
#
# Features:
#   - Check Rpath package installation
#   - Convert ECOPATH group and diet data to Rpath.params
#   - Validate parameter completeness
#   - Handle missing values and edge cases
#
# Installation:
#   remotes::install_github("noaa-edab/Rpath", build_vignettes=TRUE)
#
# References:
#   - Lucey et al. (2020) Ecological Modelling 427: 109057
#   - Whitehouse & Aydin (2020) Ecological Modelling 429: 109074
#   - https://noaa-edab.github.io/Rpath/
#
# ==============================================================================

# ==============================================================================
# PACKAGE CHECK AND INSTALLATION
# ==============================================================================

check_rpath_installed <- function() {
  #' Check if Rpath Package is Installed
  #'
  #' Checks for Rpath package and provides installation instructions if missing
  #'
  #' @return TRUE if installed, stops with instructions if not
  #' @export

  if (!requireNamespace("Rpath", quietly = TRUE)) {
    stop(
      "Package 'Rpath' required for ECOPATH/ECOSIM functionality.\n",
      "\n",
      "Installation instructions:\n",
      "1. Install remotes package: install.packages('remotes')\n",
      "2. Install Rpath from GitHub:\n",
      "   remotes::install_github('noaa-edab/Rpath', build_vignettes=TRUE)\n",
      "\n",
      "Alternative (if issues):\n",
      "   install.packages('pak')\n",
      "   pak::pak('noaa-edab/Rpath')\n",
      "\n",
      "Documentation: https://noaa-edab.github.io/Rpath/\n"
    )
  }

  return(TRUE)
}

# ==============================================================================
# DATA CONVERSION: ECOPATH DATABASE → RPATH FORMAT
# ==============================================================================

convert_ecopath_to_rpath <- function(ecopath_data, model_name = "EcoNeTool Model") {
  #' Convert ECOPATH Database Import to Rpath Format
  #'
  #' Transforms ECOPATH group and diet data into Rpath parameter format
  #' for mass-balance modeling and dynamic simulations
  #'
  #' @param ecopath_data List with group_data and diet_data from parse_ecopath_native()
  #' @param model_name Character string for model identification
  #' @return Rpath.params object ready for model balancing
  #' @export

  check_rpath_installed()

  message("Converting ECOPATH data to Rpath format...")
  message("Model: ", model_name)

  # Validate input structure
  if (!is.list(ecopath_data)) {
    stop("ecopath_data must be a list")
  }
  if (is.null(ecopath_data$group_data)) {
    stop("ecopath_data must contain 'group_data' element")
  }
  if (is.null(ecopath_data$diet_data)) {
    stop("ecopath_data must contain 'diet_data' element")
  }

  # Extract data
  groups <- ecopath_data$group_data
  diet <- ecopath_data$diet_data

  # Validate groups data
  required_cols <- c("GroupName", "Type", "GroupID")
  missing_cols <- setdiff(required_cols, names(groups))
  if (length(missing_cols) > 0) {
    stop("group_data missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  message("  Groups: ", nrow(groups), " rows")
  message("  Diet entries: ", nrow(diet), " rows")

  # Clean group names (trim whitespace) for consistency
  groups$GroupName <- trimws(groups$GroupName)

  # Validate Type column (critical for Rpath)
  if (any(is.na(groups$Type))) {
    na_groups <- groups$GroupName[is.na(groups$Type)]
    stop("Groups have missing Type values: ", paste(na_groups, collapse = ", "),
         "\nType must be: 0 (consumer), 1 (producer), 2 (detritus), or 3 (fleet)")
  }

  # Filter out groups with Type < 0 (usually invalid entries)
  living_groups <- groups[!is.na(groups$Type) & groups$Type >= 0, ]

  # Check if there are any fleet groups (Type == 3)
  # Rpath package has a bug where it fails if there are no fleets
  # Workaround: Add a dummy fleet if none exist
  has_fleets <- any(living_groups$Type == 3)

  if (!has_fleets) {
    message("  → No fleets detected, adding dummy fleet for Rpath compatibility")

    # Create a dummy fleet entry
    dummy_fleet <- living_groups[1, ]  # Copy structure from first group
    dummy_fleet$GroupName <- "DummyFleet"
    dummy_fleet$GroupID <- max(living_groups$GroupID, na.rm = TRUE) + 1
    dummy_fleet$Type <- 3  # Fleet type
    dummy_fleet$Biomass <- NA
    dummy_fleet$ProdBiom <- NA
    dummy_fleet$ConsBiom <- NA
    dummy_fleet$EcoEfficiency <- NA

    # Add to living_groups
    living_groups <- rbind(living_groups, dummy_fleet)
  }

  n_groups <- nrow(living_groups)
  n_living <- sum(living_groups$Type == 0 | living_groups$Type == 1)
  n_detritus <- sum(living_groups$Type == 2)
  n_fleets <- sum(living_groups$Type == 3)

  message("  → Total groups: ", n_groups)
  message("  → Living groups: ", n_living)
  message("  → Detritus pools: ", n_detritus)
  if (n_fleets > 0) message("  → Fleets: ", n_fleets)

  # Create Rpath parameter structure
  # Initialize with create.rpath.params()
  # Note: stgroup parameter is not used in create.rpath.params
  # Stanza information is set separately in the stanzas list
  params <- Rpath::create.rpath.params(
    group = living_groups$GroupName,  # Group names already trimmed above
    type = living_groups$Type
  )

  # ===========================================================================
  # BASIC PARAMETERS (from EcopathGroup table)
  # ===========================================================================

  # Helper function to clean ECOPATH missing value indicators
  clean_ecopath_missing <- function(x) {
    # ECOPATH uses -9999 as missing value indicator
    # Replace with NA for Rpath
    x[x < -9000] <- NA
    return(x)
  }

  # Biomass (B) - tons/km²
  params$model$Biomass <- clean_ecopath_missing(living_groups$Biomass)

  # Production/Biomass ratio (P/B) - per year
  params$model$PB <- clean_ecopath_missing(living_groups$ProdBiom)

  # Consumption/Biomass ratio (Q/B) - per year
  params$model$QB <- clean_ecopath_missing(living_groups$ConsBiom)

  # Ecotrophic Efficiency (EE) - proportion (0-1)
  if ("EcoEfficiency" %in% names(living_groups)) {
    params$model$EE <- clean_ecopath_missing(living_groups$EcoEfficiency)
  }

  # Biomass accumulation rate
  if ("BiomAccRate" %in% names(living_groups)) {
    params$model$BioAcc <- clean_ecopath_missing(living_groups$BiomAccRate)
  }

  # Unassimilated/Consumption (GE) - gross efficiency
  if ("Unassim" %in% names(living_groups)) {
    params$model$Unassim <- clean_ecopath_missing(living_groups$Unassim)
  }

  # Detritus fate - how mortality becomes detritus
  if ("DetritusFate" %in% names(living_groups)) {
    params$model$DetFate <- clean_ecopath_missing(living_groups$DetritusFate)
    # Replace NA values with 0 (no mortality goes to detritus)
    # Rpath can't handle NA values in DetFate
    params$model$DetFate[is.na(params$model$DetFate)] <- 0.0
  } else {
    # If column doesn't exist, initialize with 0 for all groups
    params$model$DetFate <- rep(0.0, nrow(params$model))
  }

  # ===========================================================================
  # FISHERIES DATA (catches, landings, discards)
  # ===========================================================================

  # Total catch
  if ("Catch" %in% names(living_groups)) {
    params$model$Catch <- clean_ecopath_missing(living_groups$Catch)
  }

  # Immigration
  if ("Immigration" %in% names(living_groups)) {
    params$model$Immigration <- clean_ecopath_missing(living_groups$Immigration)
  }

  # Emigration
  if ("Emigration" %in% names(living_groups)) {
    params$model$Emigration <- clean_ecopath_missing(living_groups$Emigration)
  }

  # ===========================================================================
  # DIET COMPOSITION MATRIX
  # ===========================================================================

  message("  → Diet links: ", nrow(diet))

  # Rpath diet format requirements:
  # - Rows: prey groups (Type < 3) + "Import" row
  # - Columns: predator/consumer groups (Type < 2) only
  #   First column is "Group" with prey names

  # Get predator groups (Type < 2: consumers and producers)
  predator_groups <- living_groups[!is.na(living_groups$Type) & living_groups$Type < 2, ]

  # Get prey groups (Type < 3: all except fleets)
  prey_groups <- living_groups[!is.na(living_groups$Type) & living_groups$Type < 3, ]

  # Validate we have predators and prey
  if (nrow(predator_groups) == 0) {
    stop("No predator groups found (Type < 2). Check that Type column is correctly set.")
  }
  if (nrow(prey_groups) == 0) {
    stop("No prey groups found (Type < 3). Check that Type column is correctly set.")
  }

  # Initialize diet data.frame matching Rpath format
  # Rows: prey + Import, Columns: Group + predators
  n_prey <- nrow(prey_groups)
  n_pred <- nrow(predator_groups)

  diet_df <- data.frame(Group = c(prey_groups$GroupName, "Import"))

  # Add columns for each predator (initialized with NA)
  for (pred_name in predator_groups$GroupName) {
    diet_df[[pred_name]] <- NA
  }

  # Fill diet data from EcopathDietComp
  if (!is.null(diet) && nrow(diet) > 0) {
    # Validate diet table has required columns
    required_diet_cols <- c("PredID", "PreyID", "Diet")
    missing_diet_cols <- setdiff(required_diet_cols, names(diet))
    if (length(missing_diet_cols) > 0) {
      stop("diet_data missing required columns: ", paste(missing_diet_cols, collapse = ", "),
           "\nAvailable columns: ", paste(names(diet), collapse = ", "))
    }

    for (i in 1:nrow(diet)) {
      pred_id <- diet$PredID[i]
      prey_id <- diet$PreyID[i]
      diet_val <- diet$Diet[i]

      # Skip if any value is NA
      if (is.na(pred_id) || is.na(prey_id) || is.na(diet_val)) next

      # Find predator and prey in our groups
      pred_match <- predator_groups[predator_groups$GroupID == pred_id, ]
      prey_match <- prey_groups[prey_groups$GroupID == prey_id, ]

      if (nrow(pred_match) > 0 && nrow(prey_match) > 0) {
        pred_name <- pred_match$GroupName[1]
        prey_name <- prey_match$GroupName[1]

        # Find row and column indices in diet_df
        prey_row <- which(diet_df$Group == prey_name)

        # Ensure we have exactly one match
        if (length(prey_row) == 1 && pred_name %in% colnames(diet_df)) {
          diet_df[prey_row, pred_name] <- diet_val
        } else if (length(prey_row) > 1) {
          # Multiple matches - use first one and warn
          warning("Multiple matches found for prey: ", prey_name, ". Using first match.")
          diet_df[prey_row[1], pred_name] <- diet_val
        }
      }
    }
  }

  # Replace remaining NA values with 0 (no diet connection)
  # Rpath can't handle NA values in diet matrix
  for (col in colnames(diet_df)) {
    if (col != "Group") {
      diet_df[[col]][is.na(diet_df[[col]])] <- 0.0
    }
  }

  # CRITICAL FIX: Remove cannibalism (self-feeding) from diet matrix
  # Self-feeding causes circular references in TL calculation
  # A group eating itself doesn't increase its trophic level
  message("  → Checking for cannibalism in diet matrix...")
  cannibalism_found <- FALSE

  for (pred_name in colnames(diet_df)) {
    if (pred_name != "Group") {
      # Find if this predator also appears as prey (same name in Group column)
      prey_row <- which(diet_df$Group == pred_name)

      if (length(prey_row) > 0 && diet_df[[pred_name]][prey_row] > 0) {
        cannibalism_value <- diet_df[[pred_name]][prey_row]
        message(sprintf("    • Removing cannibalism: '%s' eating itself (%.1f%%)",
                        pred_name, cannibalism_value * 100))
        diet_df[[pred_name]][prey_row] <- 0.0
        cannibalism_found <- TRUE
      }
    }
  }

  if (cannibalism_found) {
    message("    ✓ Cannibalism removed to prevent circular TL calculation")
  } else {
    message("    ✓ No cannibalism found")
  }

  # Convert to data.table (Rpath requires data.table, not data.frame)
  params$diet <- data.table::as.data.table(diet_df)

  # ===========================================================================
  # STANZA GROUPS (multi-stanza species)
  # ===========================================================================

  # Check if stanza data available
  if ("StanzaID" %in% names(living_groups)) {
    # Create stanza structure for age-structured groups
    stanza_groups <- unique(living_groups$StanzaID[living_groups$StanzaID > 0])

    if (length(stanza_groups) > 0) {
      message("  → Stanza groups: ", length(stanza_groups))

      # Initialize stanza parameters
      params$stanzas <- list()

      for (stanza_id in stanza_groups) {
        stanza_members <- living_groups[living_groups$StanzaID == stanza_id, ]

        params$stanzas[[stanza_id]] <- list(
          StanzaName = stanza_members$GroupName[1],
          nstanzas = nrow(stanza_members),
          VBGF_Ksp = mean(stanza_members$vbK, na.rm = TRUE),
          Wmat = mean(stanza_members$Winf, na.rm = TRUE)
        )
      }
    }
  }

  # ===========================================================================
  # PEDIGREE (uncertainty/quality indicators)
  # ===========================================================================

  # If pedigree data available, add confidence levels
  if ("BiomassCV" %in% names(living_groups)) {
    params$pedigree <- data.frame(
      Group = living_groups$GroupName,
      B = living_groups$BiomassCV,
      PB = living_groups$ProdBiomCV,
      QB = living_groups$ConsBiomCV
    )
  }

  # ===========================================================================
  # VALIDATION: Check for critical missing values
  # ===========================================================================

  message("\nValidating Rpath parameters...")

  # For each living group (not fleets), check we have enough data
  for (i in 1:nrow(params$model)) {
    group_name <- params$model$Group[i]
    group_type <- params$model$Type[i]

    # Skip fleets (Type == 3)
    if (group_type == 3) next

    # Get parameter values
    b <- params$model$Biomass[i]
    pb <- params$model$PB[i]
    qb <- params$model$QB[i]
    ee <- params$model$EE[i]

    # Count how many parameters are NOT NA
    n_params <- sum(!is.na(c(b, pb, qb, ee)))

    # Ecopath requires at least 3 out of 4 parameters
    if (n_params < 3) {
      warning("Group '", group_name, "' has only ", n_params, " parameters (needs at least 3):",
              "\n  Biomass: ", ifelse(is.na(b), "MISSING", b),
              "\n  P/B: ", ifelse(is.na(pb), "MISSING", pb),
              "\n  Q/B: ", ifelse(is.na(qb), "MISSING", qb),
              "\n  EE: ", ifelse(is.na(ee), "MISSING", ee))
    }

    # Check for invalid values (negative or zero where not allowed)
    if (!is.na(pb) && pb <= 0) {
      warning("Group '", group_name, "' has invalid P/B: ", pb, " (must be > 0)")
    }
    if (!is.na(qb) && qb <= 0) {
      warning("Group '", group_name, "' has invalid Q/B: ", qb, " (must be > 0)")
    }
    if (!is.na(ee) && (ee < 0 || ee > 1)) {
      warning("Group '", group_name, "' has invalid EE: ", ee, " (must be 0-1)")
    }
  }

  # Check diet matrix has valid values
  diet_cols <- names(params$diet)[names(params$diet) != "Group"]
  for (col in diet_cols) {
    diet_sum <- sum(params$diet[[col]], na.rm = TRUE)
    if (diet_sum > 1.01) {
      warning("Predator '", col, "' has diet sum > 1.0 (", round(diet_sum, 3), ")")
    }
  }

  message("✓ Validation complete")
  message("✓ Conversion complete")
  message("  → Ready for mass-balance with rpath()")

  return(params)
}
