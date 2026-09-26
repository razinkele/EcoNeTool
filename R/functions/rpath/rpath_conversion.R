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

#' Diet-column sum tolerance shared by the converter and the UI diet panel
#'
#' EwE exports round-trip diet fractions with small residues above 1 (e.g.
#' 1.000500). 1e-3 clears those rounding residues (up to 5e-4 seen in
#' examples/LT2022_0.5ST_final7.eweaccdb) while still catching real
#' over-full diets.
RPATH_DIET_SUM_TOL <- 1e-3

# ==============================================================================
# DIET-MATRIX CELL EDITING
# ==============================================================================

#' Apply a single diet-matrix cell edit
#'
#' Pure helper behind the diet_matrix_table cell-edit observer. DT's
#' `_cell_edit` event reports `row` as a 1-based R index already; only `col`
#' is 0-based (and only because the table renders with rownames = FALSE). The
#' observer previously added 1 to BOTH, so every edit landed on the next prey
#' row - silently balancing Ecopath on a diet matrix the user never entered,
#' and growing the column to nrow+1 (a ragged data.table) when the last row
#' was edited. Keeping this as a pure function makes the index contract
#' testable without a Shiny session.
#'
#' @param diet data.table diet matrix: first column is the prey/Group name,
#'   remaining columns are predators; rows are prey.
#' @param edit_info The `input$..._cell_edit` list with `row`, `col`, `value`.
#' @return list(diet = <updated or unchanged>, status = "ok"|"skip"|"invalid").
#'   "skip" = edit targeted the non-editable Group column; "invalid" = value
#'   outside [0, 1]; in both cases `diet` is returned unchanged.
#' @export
apply_diet_cell_edit <- function(diet, edit_info) {
  row <- edit_info$row          # DT _cell_edit row is already 1-based
  col <- edit_info$col + 1      # DT col is 0-based (rownames = FALSE)

  if (col == 1) return(list(diet = diet, status = "skip"))  # Group column

  new_value <- as.numeric(edit_info$value)
  if (!is.na(new_value) && (new_value < 0 || new_value > 1)) {
    return(list(diet = diet, status = "invalid"))
  }

  col_name <- names(diet)[col]
  diet[[col_name]][row] <- new_value
  list(diet = diet, status = "ok")
}

# ==============================================================================
# DIET-MATRIX CONSTRUCTION (pure: no Rpath needed)
# ==============================================================================

#' Build the Rpath diet data frame from EwE group and diet tables
#'
#' Rows are prey groups (Type < 3) plus "Import"; columns are "Group" followed
#' by one column per predator (Type < 2). Cell [prey, predator] is the diet
#' proportion; absent links are 0. Cannibalism (a group eating itself) is
#' KEPT as entered: Rpath solves trophic levels as a linear system, so a
#' self-loop is well defined, and zeroing it without renormalising left the
#' predator's diet summing to less than 1.
#'
#' @param living_groups EwE group table (GroupName, Type, GroupID), fleets and
#'   any dummy fleet already appended.
#' @param diet EwE diet table with PredID, PreyID, Diet (may be NULL/empty).
#' @return data.frame with a "Group" column plus one numeric column per
#'   predator.
#' @export
build_rpath_diet_frame <- function(living_groups, diet) {
  # Get predator groups (Type < 2: consumers and producers)
  predator_groups <- living_groups[!is.na(living_groups$Type) & living_groups$Type < 2, ]

  # Get prey groups (Type < 3: all except fleets)
  prey_groups <- living_groups[!is.na(living_groups$Type) & living_groups$Type < 3, ]

  if (nrow(predator_groups) == 0) {
    stop("No predator groups found (Type < 2). Check that Type column is correctly set.")
  }
  if (nrow(prey_groups) == 0) {
    stop("No prey groups found (Type < 3). Check that Type column is correctly set.")
  }

  # Rows: prey + Import, Columns: Group + predators
  diet_df <- data.frame(Group = c(prey_groups$GroupName, "Import"), stringsAsFactors = FALSE)
  for (pred_name in predator_groups$GroupName) {
    diet_df[[pred_name]] <- NA_real_
  }

  if (!is.null(diet) && nrow(diet) > 0) {
    required_diet_cols <- c("PredID", "PreyID", "Diet")
    missing_diet_cols <- setdiff(required_diet_cols, names(diet))
    if (length(missing_diet_cols) > 0) {
      stop("diet_data missing required columns: ", paste(missing_diet_cols, collapse = ", "),
           "\nAvailable columns: ", paste(names(diet), collapse = ", "))
    }

    for (i in seq_len(nrow(diet))) {
      pred_id <- diet$PredID[i]
      prey_id <- diet$PreyID[i]
      diet_val <- diet$Diet[i]
      if (is.na(pred_id) || is.na(prey_id) || is.na(diet_val)) next

      pred_match <- predator_groups[predator_groups$GroupID == pred_id, ]
      prey_match <- prey_groups[prey_groups$GroupID == prey_id, ]
      if (nrow(pred_match) == 0 || nrow(prey_match) == 0) next

      pred_name <- pred_match$GroupName[1]
      prey_name <- prey_match$GroupName[1]
      prey_row <- which(diet_df$Group == prey_name)

      if (length(prey_row) == 1 && pred_name %in% colnames(diet_df)) {
        diet_df[prey_row, pred_name] <- diet_val
      } else if (length(prey_row) > 1) {
        warning("Multiple matches found for prey: ", prey_name, ". Using first match.",
                call. = FALSE)
        diet_df[prey_row[1], pred_name] <- diet_val
      }
    }
  }

  # Rpath can't handle NA values in the diet matrix: no link = 0.
  for (col in setdiff(colnames(diet_df), "Group")) {
    diet_df[[col]][is.na(diet_df[[col]])] <- 0.0
  }

  check_rpath_diet_sums(diet_df)
  diet_df
}

#' Warn about predator diet columns summing to more than 1
#'
#' The data are not changed: an over-full diet is a data-entry problem the
#' user must fix, and silently rescaling it would hide that.
#'
#' @param diet_df Diet data frame from build_rpath_diet_frame().
#' @param tol Tolerance above 1 before warning (default RPATH_DIET_SUM_TOL).
#' @return Invisibly, the names of the offending predator columns.
#' @export
check_rpath_diet_sums <- function(diet_df, tol = RPATH_DIET_SUM_TOL) {
  cols <- setdiff(names(diet_df), "Group")
  sums <- vapply(cols, function(col) sum(diet_df[[col]], na.rm = TRUE), numeric(1))
  over <- cols[sums > 1 + tol]
  for (col in over) {
    warning(sprintf("[rpath conversion] diet of '%s' sums to %.6f (> 1); data left unchanged",
                    col, sums[[col]]), call. = FALSE)
  }

  # A group whose diet is (almost) entirely itself makes Rpath's TL/balance
  # system singular (an opaque Lapack error). Warn so the cause is visible.
  for (col in cols) {
    self_row <- diet_df$Group == col
    self_val <- diet_df[[col]][self_row]
    if (length(self_val) == 1 && !is.na(self_val) && self_val >= 1 - tol) {
      warning(sprintf(
        "[rpath conversion] '%s' is a pure-cannibal group (self-diet = %.6f); Rpath balance may fail",
        col, self_val), call. = FALSE)
    }
  }

  invisible(over)
}

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

  diet_df <- build_rpath_diet_frame(living_groups, diet)

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

  message("✓ Validation complete")
  message("✓ Conversion complete")
  message("  → Ready for mass-balance with rpath()")

  return(params)
}
