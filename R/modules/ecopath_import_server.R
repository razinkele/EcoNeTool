#' ECOPATH Import Server Logic
#'
#' Handles ECOPATH file parsing (native .ewemdb/.mdb and CSV),
#' metadata preview, and model import workflows.
#' Merges the ECOPATH parser function and native DB import handler.
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param net_reactive reactiveVal for network
#' @param info_reactive reactiveVal for species info
#' @param metaweb_metadata reactiveVal for metadata
#' @param dashboard_trigger reactiveVal for dashboard updates
#' @param ecopath_import_data reactiveVal for passing data to Rpath module
#' @param ecopath_native_status_data reactiveVal for native import status display
#' @param plugin_states reactiveVal for plugin states
#' @param euseamap_data reactiveVal for EUSeaMap habitat data
#' @param current_metaweb reactiveVal for current metaweb
#' @param refresh_data_editor function to refresh data editor tables
ecopath_import_server <- function(input, output, session, net_reactive, info_reactive,
                                   metaweb_metadata, dashboard_trigger,
                                   ecopath_import_data, ecopath_native_status_data,
                                   plugin_states, euseamap_data, current_metaweb,
                                   refresh_data_editor) {

  # ============================================================================
  # ECOPATH PARSER FUNCTION
  # ============================================================================

  # NOTE: parse_ecopath_data() function is now defined in R/functions/ecopath_import.R
  # It is automatically available through the source() call at the top of this file

  #' Parse ECOPATH native database file (.ewemdb, .mdb)
  #'
  #' Reads ECOPATH with Ecosim native database files using mdbtools
  #' @param db_file Path to ECOPATH database file
  #' @return List with 'net' (igraph object) and 'info' (data.frame)
  parse_ecopath_native <- function(db_file, session = NULL) {
    # Check if file exists
    if (!file.exists(db_file)) {
      stop("Database file not found: ", db_file)
    }

    # CROSS-PLATFORM IMPLEMENTATION
    # Detect OS and use appropriate database reader
    os_type <- Sys.info()["sysname"]

    message("Operating System: ", os_type)
    message("Using cross-platform ECOPATH import...")

    tryCatch({
      # Import database using cross-platform function
      import_result <- parse_ecopath_native_cross_platform(db_file)

      # Extract tables from import result
      group_table <- import_result$group_data
      diet_table <- import_result$diet_data
      tables <- import_result$tables
      metadata <- import_result$metadata  # Extract metadata

      # Log success
      message("✓ Database imported successfully using ", import_result$method)

      # Extract species/group information
      # ECOPATH column names may vary, so we try different common names
      col_names <- tolower(colnames(group_table))
      col_names_orig <- colnames(group_table)

      # Find group name column
      name_col <- which(grepl("group.*name|^name$|groupname", col_names))[1]
      if (is.na(name_col)) name_col <- 1  # Default to first column

      # Find biomass column (more flexible patterns)
      biomass_col <- which(grepl("^biomass$|^b$|^biom$|trophic.*area|habitat.*area|^ba$", col_names))[1]

      # Find P/B column (more flexible patterns)
      # ECOPATH uses P/B, PB, ProdBiom, etc.
      pb_col <- which(grepl("^p/b$|^pb$|^p.b$|prod.*biom|production.*biomass|^prodbiom$", col_names))[1]

      # Find Q/B column (more flexible patterns)
      # ECOPATH uses Q/B, QB, ConsBiom, etc.
      qb_col <- which(grepl("^q/b$|^qb$|^q.b$|cons.*biom|consumption.*biomass|^consbiom$", col_names))[1]

      # Find body mass column (ECOPATH specific)
      # ECOPATH may have: IndividualWeight, BodyMass, IndWt, Weight, Mass
      bodymass_col <- which(grepl("individual.*weight|indiv.*wt|body.*mass|^weight$|^mass$|^bodymass$|^indwt$", col_names))[1]

      # Log column detection
      message("Group table column detection:")
      message("  Name: ", col_names_orig[name_col], " (column ", name_col, ")")
      message("  Biomass: ", if (!is.na(biomass_col)) paste0(col_names_orig[biomass_col], " (column ", biomass_col, ")") else "NOT FOUND - using defaults")
      message("  P/B: ", if (!is.na(pb_col)) paste0(col_names_orig[pb_col], " (column ", pb_col, ")") else "NOT FOUND - using defaults")
      message("  Q/B: ", if (!is.na(qb_col)) paste0(col_names_orig[qb_col], " (column ", qb_col, ")") else "NOT FOUND - using defaults")
      message("  Body Mass: ", if (!is.na(bodymass_col)) paste0(col_names_orig[bodymass_col], " (column ", bodymass_col, ")") else "NOT FOUND - will estimate by functional group")

      # Extract data
      species_names <- as.character(group_table[[name_col]])
      biomass_values <- if (!is.na(biomass_col)) as.numeric(group_table[[biomass_col]]) else rep(1, length(species_names))
      pb_values <- if (!is.na(pb_col)) as.numeric(group_table[[pb_col]]) else rep(0.5, length(species_names))
      qb_values <- if (!is.na(qb_col)) as.numeric(group_table[[qb_col]]) else rep(1.5, length(species_names))
      bodymass_values_raw <- if (!is.na(bodymass_col)) as.numeric(group_table[[bodymass_col]]) else NULL

      # ECOPATH uses -9999 or -9999.00 as missing value indicator
      # Replace with NA or reasonable defaults
      clean_ecopath_value <- function(x, default_val, param_name = "") {
        n_missing <- sum(is.na(x))
        n_sentinel <- sum(x < -9000, na.rm = TRUE)  # -9999 values
        n_negative <- sum(x < 0 & x >= -9000, na.rm = TRUE)  # Other negative values

        x[is.na(x)] <- default_val
        x[x < -9000] <- default_val  # Catch -9999, -9999.00, etc.
        x[x < 0] <- default_val      # Biomass/PB/QB can't be negative

        # Log if any values were replaced
        total_replaced <- n_missing + n_sentinel + n_negative
        if (total_replaced > 0 && param_name != "") {
          message("  Cleaned ", total_replaced, " ", param_name, " value(s):")
          if (n_sentinel > 0) message("    - ", n_sentinel, " ECOPATH missing value(s) (-9999) → ", default_val)
          if (n_negative > 0) message("    - ", n_negative, " negative value(s) → ", default_val)
          if (n_missing > 0) message("    - ", n_missing, " NA value(s) → ", default_val)
        }

        return(x)
      }

      # Clean biomass values (default to 1 if missing)
      biomass_values <- clean_ecopath_value(biomass_values, 1, "Biomass")

      # Apply habitat area proportion if available
      # ECOPATH biomass is given per unit area, but organisms may use only fraction of total habitat
      # Area column contains habitat area proportion (0-1), representing fraction of habitat used
      area_col <- which(grepl("^area$|habitat.*prop|^habarea$|^habprop$", col_names))[1]
      if (!is.na(area_col)) {
        area_proportions <- as.numeric(group_table[[area_col]])
        area_proportions <- clean_ecopath_value(area_proportions, 1, "")  # Default to 1 (full area)
        # Multiply biomass by area proportion to get actual biomass
        biomass_values <- biomass_values * area_proportions
        message("  Applied habitat area proportions to biomass")
        message("    Range: ", round(min(area_proportions, na.rm = TRUE), 3), " - ", round(max(area_proportions, na.rm = TRUE), 3))
      }

      # Clean P/B values (default to 0.5 if missing)
      pb_values <- clean_ecopath_value(pb_values, 0.5, "P/B")

      # Clean Q/B values (default to 1.5 if missing)
      qb_values <- clean_ecopath_value(qb_values, 1.5, "Q/B")

      # Log data quality summary
      message("\nData quality summary (BEFORE filtering):")
      message("  Total groups in database: ", length(species_names))
      message("  Biomass range: ", round(min(biomass_values), 3), " - ", round(max(biomass_values), 3))
      message("  P/B range: ", round(min(pb_values), 3), " - ", round(max(pb_values), 3))
      message("  Q/B range: ", round(min(qb_values), 3), " - ", round(max(qb_values), 3))

      # Show all group names for verification
      message("\nAll group names loaded from database:")
      for (i in seq_along(species_names)) {
        message(sprintf("  %2d. %s", i, species_names[i]))
      }

      # Remove NA, empty, or "Import" and "Export" groups (common in ECOPATH)
      valid_idx <- !is.na(species_names) & species_names != "" &
                   !grepl("^import$|^export$|^fleet", tolower(species_names))

      # Log which groups were filtered out
      filtered_groups <- species_names[!valid_idx]
      if (length(filtered_groups) > 0) {
        message("\n⚠ Filtered out ", length(filtered_groups), " technical groups:")
        for (g in filtered_groups) {
          message("  ✗ ", g, " (technical construct, not a biological group)")
        }
        message("  Remaining groups: ", sum(valid_idx), " (biological groups only)")
      }

      species_names <- species_names[valid_idx]
      biomass_values <- biomass_values[valid_idx]
      pb_values <- pb_values[valid_idx]
      qb_values <- qb_values[valid_idx]

      n_species <- length(species_names)

      # Process diet composition
      # ECOPATH diet tables usually have: Predator, Prey, DietComp (proportion)
      diet_cols <- tolower(colnames(diet_table))
      diet_cols_orig <- colnames(diet_table)

      # Try to identify predator column (more flexible patterns)
      pred_col <- which(grepl("predator|consumer|^pred$|eating", diet_cols))[1]

      # Try to identify prey column (more flexible patterns)
      prey_col <- which(grepl("prey|resource|^food$|eaten", diet_cols))[1]

      # Try to identify diet proportion column (more flexible patterns)
      diet_col <- which(grepl("diet|proportion|comp|percent|fraction|dc$", diet_cols))[1]

      # If standard patterns didn't work, try positional heuristics
      # ECOPATH often has: Column1=Predator, Column2=Prey, Column3=Diet
      if (is.na(pred_col) && ncol(diet_table) >= 3) {
        # First column is often predator ID or name
        pred_col <- 1
      }

      if (is.na(prey_col) && ncol(diet_table) >= 3) {
        # Second column is often prey ID or name
        prey_col <- 2
      }

      if (is.na(diet_col) && ncol(diet_table) >= 3) {
        # Third column is often diet proportion
        # Look for numeric column
        for (i in 1:min(5, ncol(diet_table))) {
          if (is.numeric(diet_table[[i]]) && all(diet_table[[i]] >= 0 & diet_table[[i]] <= 1, na.rm = TRUE)) {
            diet_col <- i
            break
          }
        }
        # If still not found, default to third column
        if (is.na(diet_col)) diet_col <- 3
      }

      # Final check
      if (is.na(pred_col) || is.na(prey_col) || is.na(diet_col)) {
        error_msg <- paste0(
          "Could not identify predator, prey, and diet columns in diet table.\n\n",
          "Available columns in diet table:\n",
          paste(seq_along(diet_cols_orig), ": ", diet_cols_orig, collapse = "\n"), "\n\n",
          "Detected columns:\n",
          "  Predator column: ", if (!is.na(pred_col)) diet_cols_orig[pred_col] else "NOT FOUND", "\n",
          "  Prey column: ", if (!is.na(prey_col)) diet_cols_orig[prey_col] else "NOT FOUND", "\n",
          "  Diet column: ", if (!is.na(diet_col)) diet_cols_orig[diet_col] else "NOT FOUND", "\n\n",
          "Please ensure your ECOPATH database has a diet composition table with:\n",
          "  - A predator/consumer column\n",
          "  - A prey/resource column\n",
          "  - A diet proportion column (0-1)"
        )
        stop(error_msg)
      }

      # Log successful column detection
      message("Successfully identified diet table columns:")
      message("  Predator: ", diet_cols_orig[pred_col], " (column ", pred_col, ")")
      message("  Prey: ", diet_cols_orig[prey_col], " (column ", prey_col, ")")
      message("  Diet proportion: ", diet_cols_orig[diet_col], " (column ", diet_col, ")")

      # Create diet matrix
      diet_matrix <- matrix(0, nrow = n_species, ncol = n_species)
      rownames(diet_matrix) <- colnames(diet_matrix) <- species_names

      # Check if diet table uses IDs or names
      # ECOPATH native databases typically use numeric IDs (PredID, PreyID)
      uses_ids <- is.numeric(diet_table[[pred_col]]) && "GroupID" %in% colnames(group_table)

      if (uses_ids) {
        # Match by ID (ECOPATH native format)
        message("Diet table uses IDs - mapping via GroupID")

        # Create ID to index mapping
        group_ids <- group_table$GroupID[valid_idx]
        id_to_idx <- setNames(seq_along(species_names), group_ids)

        # Fill diet matrix using ID mapping (VECTORIZED)
        # Pre-filter valid entries
        pred_ids <- as.numeric(diet_table[[pred_col]])
        prey_ids <- as.numeric(diet_table[[prey_col]])
        diet_props <- as.numeric(diet_table[[diet_col]])

        valid_entries <- !is.na(pred_ids) & !is.na(prey_ids) & !is.na(diet_props) & diet_props > 0

        if (sum(valid_entries) > 0) {
          # Vectorize ID lookups
          pred_indices <- id_to_idx[as.character(pred_ids[valid_entries])]
          prey_indices <- id_to_idx[as.character(prey_ids[valid_entries])]
          valid_pairs <- !is.na(pred_indices) & !is.na(prey_indices)

          if (sum(valid_pairs) > 0) {
            # Single matrix indexing operation
            idx <- cbind(prey_indices[valid_pairs], pred_indices[valid_pairs])
            diet_matrix[idx] <- diet_props[valid_entries][valid_pairs]
            links_added <- sum(valid_pairs)
          } else {
            links_added <- 0
          }
        } else {
          links_added <- 0
        }
        message("  Added ", links_added, " diet links from ", nrow(diet_table), " diet entries")

      } else {
        # Match by name (CSV export format - VECTORIZED)
        message("Diet table uses names - matching directly")

        # Pre-filter and convert
        pred_names <- as.character(diet_table[[pred_col]])
        prey_names <- as.character(diet_table[[prey_col]])
        diet_props <- as.numeric(diet_table[[diet_col]])

        # Vectorize validity checks
        valid_entries <- !is.na(pred_names) & !is.na(prey_names) &
                        !is.na(diet_props) & diet_props > 0 &
                        pred_names %in% species_names & prey_names %in% species_names

        if (sum(valid_entries) > 0) {
          # Vectorize name matching
          name_to_idx <- setNames(seq_along(species_names), species_names)
          pred_indices <- name_to_idx[pred_names[valid_entries]]
          prey_indices <- name_to_idx[prey_names[valid_entries]]

          # Single matrix indexing operation
          idx <- cbind(prey_indices, pred_indices)
          diet_matrix[idx] <- diet_props[valid_entries]
          links_added <- sum(valid_entries)
        } else {
          links_added <- 0
        }
        message("  Added ", links_added, " diet links")
      }

      # Convert to binary adjacency matrix
      # diet_matrix[prey, predator] = diet proportion
      # This is already the correct format for trophiclevels() function
      # which expects adjacency[prey, predator] = 1 (edges from prey to predator)
      adjacency_matrix <- (diet_matrix > 0) * 1

      # Ensure rownames and colnames are preserved
      rownames(adjacency_matrix) <- rownames(diet_matrix)
      colnames(adjacency_matrix) <- colnames(diet_matrix)

      # Create network
      net <- igraph::graph_from_adjacency_matrix(adjacency_matrix, mode = "directed")
      net <- igraph::upgrade_graph(net)

      # Explicitly set vertex names to ensure they're preserved
      igraph::V(net)$name <- species_names

      # Log network creation
      message("\nNetwork created:")
      message("  Vertices (species/groups): ", igraph::vcount(net))
      message("  Edges (trophic links): ", igraph::ecount(net))
      message("  Expected vertices: ", length(species_names))
      if (igraph::vcount(net) != length(species_names)) {
        message("  ⚠ WARNING: Vertex count mismatch! Some species may have been lost during network creation.")
        message("  Vertex names in network: ", paste(igraph::V(net)$name, collapse = ", "))
      }

      # Assign functional groups using shared utility with topology heuristics
      indegrees <- igraph::degree(net, mode = "in")
      outdegrees <- igraph::degree(net, mode = "out")

      # Check if taxonomic API should be used
      checkbox_checked <- !is.null(input$use_taxonomic_api) && input$use_taxonomic_api
      plugin_enabled <- !is.null(plugin_states()) && plugin_states()[["taxonomic_api"]]
      use_api <- checkbox_checked && plugin_enabled

      # Warn if checkbox is checked but plugin is not enabled
      if (checkbox_checked && !plugin_enabled) {
        showNotification(
          "Taxonomic Database Integration plugin is not enabled. Go to Settings > Plugins to enable it.",
          type = "warning",
          duration = 10
        )
        message("WARNING: Taxonomic API checkbox is checked but plugin is disabled!")
        message("  Enable plugin in Settings > Plugins > Advanced > Taxonomic Database Integration")
        message("  Falling back to pattern matching for species classification")
      }

      if (use_api) {
        message("Using taxonomic API for enhanced species classification...")
        message("  Querying FishBase, WoRMS, and OBIS (this may take a few minutes)")
        message("  Results will be cached for faster subsequent imports")
        message(sprintf("  Processing %d species...", length(species_names)))

        # Initialize progress
        total_species <- length(species_names)

        # Create Shiny progress object (this works inside observeEvent!)
        # CSS in app.R lines 216-223 centers it on screen
        progress <- Progress$new(
          min = 0,
          max = total_species
        )
        on.exit(progress$close())

        progress$set(
          message = "🔍 Taxonomic Database Search",
          value = 0,
          detail = sprintf("Initializing classification for %d species\nQuerying: FishBase → WoRMS → OBIS\nResults will be cached locally", total_species)
        )

        # Also update custom reactive for detailed UI
        taxonomic_progress(list(
          current = 0,
          total = total_species,
          percent = 0,
          database = "Initializing",
          search_term = "",
          message = sprintf("Starting taxonomic verification for %d species. Will query FishBase, WoRMS, and OBIS databases.", total_species)
        ))

        # Initialize taxonomic report data
        taxonomic_report_data <- data.frame(
          species = character(total_species),
          functional_group = character(total_species),
          database_source = character(total_species),
          confidence = character(total_species),
          stringsAsFactors = FALSE
        )

        # Enhanced classification with API verification
        # Process each species and collect results
        # Try to extract geographic region from model metadata or filename
        geographic_region <- NULL

        # Check if model_metadata exists and has region info
        if (exists("model_metadata") && !is.null(model_metadata)) {
          if ("region" %in% names(model_metadata)) {
            geographic_region <- model_metadata$region
          } else if ("name" %in% names(model_metadata)) {
            # Try to extract region from model name (e.g., "Baltic Sea Model", "North Sea")
            model_name <- tolower(model_metadata$name)
            if (grepl("baltic", model_name)) {
              geographic_region <- "Baltic"
            } else if (grepl("north sea", model_name)) {
              geographic_region <- "North Sea"
            } else if (grepl("mediterranean", model_name)) {
              geographic_region <- "Mediterranean"
            } else if (grepl("atlantic", model_name)) {
              geographic_region <- "Atlantic"
            } else if (grepl("pacific", model_name)) {
              geographic_region <- "Pacific"
            }
          }
        }

        # If no metadata, try to extract from filename
        if (is.null(geographic_region) && !is.null(input$ecopath_native_file)) {
          filename <- tolower(input$ecopath_native_file$name)
          if (grepl("baltic", filename)) {
            geographic_region <- "Baltic"
          } else if (grepl("north.*sea", filename)) {
            geographic_region <- "North Sea"
          } else if (grepl("mediterranean", filename)) {
            geographic_region <- "Mediterranean"
          } else if (grepl("atlantic", filename)) {
            geographic_region <- "Atlantic"
          } else if (grepl("pacific", filename)) {
            geographic_region <- "Pacific"
          }
        }

        if (!is.null(geographic_region)) {
          message(sprintf("  Geographic region detected: %s", geographic_region))
        } else {
          message("  No geographic region detected - using first match for ambiguous species")
        }

        # NOTE: Using for loop instead of lapply so session$flushReact() works
        classification_results <- list()

        # Progress message accumulator for real-time updates (use environment for closure safety)
        progress_env <- new.env(parent = emptyenv())
        progress_env$messages <- character()

        for (i in seq_along(species_names)) {
          sp <- species_names[i]

          # Update progress (both Shiny progress bar and custom UI)
          percent <- round((i / total_species) * 100)

          # Check if this is a combined species name
          is_combined <- grepl("\\s+and\\s+|\\s*&\\s*|\\s*/\\s*", sp, ignore.case = TRUE)
          combined_note <- if (is_combined) " (combined name - will query first species)" else ""

          message(sprintf("  [%d/%d] Querying: %s", i, total_species, sp))

          # Get pattern-based hint first (fast, used to optimize API queries)
          pattern_hint <- assign_functional_group(
            sp,
            pb_values[i],
            indegrees[i],
            outdegrees[i],
            use_topology = TRUE
          )

          # Detect birds by name patterns
          is_bird <- grepl("bird|duck|cormorant|gull|tern|gannet|aves", sp, ignore.case = TRUE)

          # Reset progress messages for this species
          progress_env$messages <- c(
            sprintf("[%d/%d] %s%s", i, total_species, sp, combined_note),
            "",
            sprintf("📊 Progress: %d%%", percent)
          )

          # Update Shiny's built-in progress (this WILL show in real-time!)
          # Build verbose detail message similar to console output
          detail_lines <- c(
            "🔍 Database sequence:",
            if (pattern_hint %in% c("Detritus", "Phytoplankton", "Zooplankton", "Benthos") || is_bird) {
              if (is_bird) {
                "   ⊗ Skipping FishBase (bird species)"
              } else {
                "   ⊗ Skipping FishBase (non-fish pattern)"
              }
            } else {
              "   → Trying FishBase..."
            },
            "   → Trying WoRMS...",
            "   → Trying OBIS...",
            "",
            sprintf("[%d/%d] %s%s", i, total_species, sp, combined_note),
            "",
            sprintf("📊 Progress: %d%%", percent),
            "💾 Cache: Checking for previous result..."
          )

          progress$set(
            value = i,
            detail = paste(detail_lines, collapse = "\n")
          )

          # Also update custom reactive for detailed UI (may not update in real-time)
          taxonomic_progress(list(
            current = i,
            total = total_species,
            percent = percent,
            database = "Querying",
            search_term = sp,
            message = sprintf("Querying databases for species classification. Results are cached for faster subsequent imports.")
          ))

          # Create progress callback for real-time updates
          progress_callback <- function(msg) {
            # Add message to accumulator
            progress_env$messages <- c(progress_env$messages, msg)

            # Update progress modal with accumulated messages
            progress$set(
              value = i,
              detail = paste(progress_env$messages, collapse = "\n")
            )
          }

          # Get full API result with database source information
          # Pass pattern hint to skip FishBase for non-fish groups and birds
          hint_for_api <- if (is_bird) "Birds" else pattern_hint
          api_result <- classify_species_api(
            sp,
            functional_group_hint = hint_for_api,
            geographic_region = geographic_region,
            progress_callback = progress_callback
          )

          # Update progress with result
          result_db <- if (!is.na(api_result$source)) api_result$source else "Pattern matching"
          result_icon <- switch(result_db,
            "FishBase" = "🐟",
            "WoRMS" = "🌊",
            "OBIS" = "🗺️",
            "Pattern matching" = "🔍",
            "✓"
          )

          # Build verbose result message
          result_detail_lines <- c(
            sprintf("[%d/%d] Querying: %s%s", i, total_species, sp, combined_note),
            "",
            sprintf("📊 Progress: %d%%", percent),
            "",
            sprintf("%s Database: %s", result_icon, result_db),
            sprintf("   Confidence: %s", if (!is.na(api_result$confidence)) api_result$confidence else "unknown"),
            sprintf("   Classification: %s", if (!is.na(api_result$functional_group)) api_result$functional_group else pattern_hint),
            "",
            if (!is.na(api_result$body_mass_g)) {
              sprintf("   Body mass: %.2f g", api_result$body_mass_g)
            } else { NULL },
            if (!is.na(api_result$trophic_level)) {
              sprintf("   Trophic level: %.2f", api_result$trophic_level)
            } else { NULL },
            if (!is.na(api_result$habitat) && api_result$habitat != "") {
              sprintf("   Habitat: %s", api_result$habitat)
            } else { NULL },
            if (!is.na(api_result$min_depth_m) && !is.na(api_result$max_depth_m)) {
              sprintf("   Depth range: %d-%d m", api_result$min_depth_m, api_result$max_depth_m)
            } else { NULL },
            "",
            if (result_db != "Pattern matching") {
              "✓ Result cached for future imports"
            } else {
              "⚠ Using pattern-based classification"
            }
          )

          progress$set(
            value = i,
            detail = paste(result_detail_lines[!sapply(result_detail_lines, is.null)], collapse = "\n")
          )

          # Use API result if available and confident
          if (!is.na(api_result$functional_group) && api_result$confidence %in% c("high", "medium")) {
            final_fg <- api_result$functional_group
          } else {
            final_fg <- pattern_hint
          }

          # Store result (including body mass from FishBase)
          classification_results[[i]] <- list(
            species = sp,
            database_source = if(!is.na(api_result$source)) api_result$source else "Pattern matching",
            confidence = api_result$confidence,
            functional_group = final_fg,
            body_mass_g = if(!is.na(api_result$body_mass_g)) api_result$body_mass_g else NA,
            trophic_level = if(!is.na(api_result$trophic_level)) api_result$trophic_level else NA,
            habitat = if(!is.na(api_result$habitat)) api_result$habitat else NA
          )
        }

        # Build taxonomic report data frame from results
        taxonomic_report_data <- data.frame(
          species = sapply(classification_results, function(x) x$species),
          functional_group = sapply(classification_results, function(x) x$functional_group),
          database_source = sapply(classification_results, function(x) x$database_source),
          confidence = sapply(classification_results, function(x) x$confidence),
          stringsAsFactors = FALSE
        )

        # Extract functional groups vector
        functional_groups <- taxonomic_report_data$functional_group

        # Store report for display
        taxonomic_report(taxonomic_report_data)

        # Clear progress when done
        taxonomic_progress(NULL)

        # Show summary message
        db_summary <- table(taxonomic_report_data$database_source)
        message("\nTaxonomic Database Match Summary:")
        for (src in names(db_summary)) {
          message(sprintf("  %s: %d species", src, db_summary[src]))
        }

        # Show functional group distribution
        fg_summary <- table(functional_groups)
        message("\nFunctional Group Distribution:")
        for (fg_name in sort(names(fg_summary))) {
          message(sprintf("  %s: %d species", fg_name, fg_summary[fg_name]))
        }

        # Show detailed classification for verification
        message("\nDetailed Classification (first 10 species):")
        for (i in 1:min(10, nrow(taxonomic_report_data))) {
          message(sprintf("  %2d. %-30s → %-15s (source: %s, confidence: %s)",
                          i,
                          taxonomic_report_data$species[i],
                          taxonomic_report_data$functional_group[i],
                          taxonomic_report_data$database_source[i],
                          taxonomic_report_data$confidence[i]))
        }
      } else {
        # Standard pattern matching (faster, offline)
        functional_groups <- assign_functional_groups(
          species_names,
          pb_values,
          indegrees,
          outdegrees,
          use_topology = TRUE  # Use network topology for ECOPATH imports
        )
      }

      # Body masses: Use ECOPATH data if available, otherwise estimate by functional group
      # Extract FishBase body masses if taxonomic API was used
      fishbase_masses <- rep(NA, length(species_names))
      if (exists("classification_results") && length(classification_results) > 0) {
        for (i in seq_along(classification_results)) {
          if (!is.null(classification_results[[i]]$body_mass_g)) {
            fishbase_masses[i] <- classification_results[[i]]$body_mass_g
          }
        }
      }

      if (!is.null(bodymass_values_raw)) {
        # Clean ECOPATH body mass values (remove sentinels, convert negative/zero to NA)
        bodymass_values_clean <- bodymass_values_raw
        bodymass_values_clean[bodymass_values_clean < -9000] <- NA  # ECOPATH missing value
        bodymass_values_clean[bodymass_values_clean <= 0] <- NA      # Invalid masses

        # Priority order: ECOPATH > FishBase > Estimation
        body_masses <- sapply(1:length(functional_groups), function(i) {
          if (!is.na(bodymass_values_clean[i]) && bodymass_values_clean[i] > 0) {
            # Priority 1: Use ECOPATH data
            bodymass_values_clean[i]
          } else if (!is.na(fishbase_masses[i])) {
            # Priority 2: Use FishBase data with ontogenetic stage adjustment
            fishbase_mass <- fishbase_masses[i]
            name_lower <- tolower(species_names[i])

            # Apply ontogenetic stage multipliers to FishBase adult weight
            stage_multiplier <- 1.0
            if (grepl("\\blarva\\b|\\blarval\\b", name_lower)) {
              stage_multiplier <- 0.10  # Larvae = 10% of adult
            } else if (grepl("\\bjuvenile\\b", name_lower)) {
              stage_multiplier <- 0.30  # Juvenile = 30% of adult
            } else if (grepl("\\bsub-adult\\b|\\bsubadult\\b", name_lower)) {
              stage_multiplier <- 0.70  # Sub-adult = 70% of adult
            }

            fishbase_mass * stage_multiplier
          } else {
            # Priority 3: Use enhanced estimation with size/stage extraction
            estimate_body_mass_enhanced(species_names[i], functional_groups[i])
          }
        })

        n_ecopath <- sum(!is.na(bodymass_values_clean) & bodymass_values_clean > 0)
        n_fishbase <- sum(is.na(bodymass_values_clean) & !is.na(fishbase_masses))
        n_estimated <- length(body_masses) - n_ecopath - n_fishbase
        message("Body mass assignment:")
        message("  From ECOPATH data: ", n_ecopath, " species")
        message("  From FishBase API: ", n_fishbase, " species")
        message("  Estimated with size/stage info: ", n_estimated, " species")
      } else {
        # No body mass column found - use FishBase or estimate
        body_masses <- sapply(1:length(species_names), function(i) {
          if (!is.na(fishbase_masses[i])) {
            # Use FishBase data with ontogenetic stage adjustment
            fishbase_mass <- fishbase_masses[i]
            name_lower <- tolower(species_names[i])

            # Apply ontogenetic stage multipliers
            stage_multiplier <- 1.0
            if (grepl("\\blarva\\b|\\blarval\\b", name_lower)) {
              stage_multiplier <- 0.10
            } else if (grepl("\\bjuvenile\\b", name_lower)) {
              stage_multiplier <- 0.30
            } else if (grepl("\\bsub-adult\\b|\\bsubadult\\b", name_lower)) {
              stage_multiplier <- 0.70
            }

            fishbase_mass * stage_multiplier
          } else {
            # Estimate using enhanced method with size/stage extraction
            estimate_body_mass_enhanced(species_names[i], functional_groups[i])
          }
        })

        n_fishbase <- sum(!is.na(fishbase_masses))
        n_estimated <- length(body_masses) - n_fishbase
        message("Body mass assignment:")
        message("  From FishBase API: ", n_fishbase, " species")
        message("  Estimated with size/stage info: ", n_estimated, " species")
      }

      # Assign metabolic types using shared utility
      met_types <- sapply(functional_groups, estimate_metabolic_type_by_fg)

      # Calculate efficiencies using shared utility
      efficiencies <- sapply(functional_groups, estimate_efficiency_by_fg)

      # Create info data frame
      info <- data.frame(
        meanB = biomass_values,
        fg = factor(functional_groups, levels = get_functional_group_levels()),
        bodymasses = body_masses,
        met.types = met_types,
        efficiencies = efficiencies,
        PB = pb_values,
        QB = qb_values,
        row.names = species_names,
        stringsAsFactors = FALSE
      )

      # Log final data frame
      message("\nFinal info data frame:")
      message("  Rows (species/groups): ", nrow(info))
      message("  Expected: ", length(species_names))
      if (nrow(info) != length(species_names)) {
        message("  ⚠ WARNING: Row count mismatch!")
      }

      # Return both processed data (net/info) and raw data (group_data/diet_data)
      # Raw data is needed for Rpath conversion
      # ECOSIM scenarios passed through from import
      return(list(
        net = net,
        info = info,
        metadata = metadata,
        group_data = group_table,
        diet_data = diet_table,
        ecosim_scenarios = import_result$ecosim_scenarios,
        total_groups_loaded = length(biomass_values) + length(filtered_groups),
        filtered_groups = filtered_groups
      ))

    }, error = function(e) {
      stop(paste("Error parsing ECOPATH database:", e$message))
    })
  }

  # ============================================================================
  # ECOPATH DATA IMPORT HANDLER
  # ============================================================================

  # Output status message for ECOPATH upload
  output$ecopath_upload_status <- renderPrint({
    if (is.null(input$ecopath_file) && is.null(input$ecopath_diet_file)) {
      cat("No ECOPATH files uploaded yet.\n\n")
      cat("Please upload both:\n")
      cat("  1. Basic Estimates file\n")
      cat("  2. Diet Composition matrix\n")
    } else if (is.null(input$ecopath_file)) {
      cat("Missing: Basic Estimates file\n")
    } else if (is.null(input$ecopath_diet_file)) {
      cat("Missing: Diet Composition file\n")
    } else {
      cat("Files selected:\n")
      cat("  Basic Estimates:", input$ecopath_file$name, "\n")
      cat("  Diet Composition:", input$ecopath_diet_file$name, "\n\n")
      cat("Click 'Import ECOPATH Data' button to process.\n")
    }
  })

  # Handle ECOPATH import when button clicked
  observeEvent(input$load_ecopath, {
    req(input$ecopath_file, input$ecopath_diet_file)

    tryCatch({
      basic_file <- input$ecopath_file$datapath
      diet_file <- input$ecopath_diet_file$datapath

      # Update status
      output$ecopath_upload_status <- renderPrint({
        cat("Processing ECOPATH files...\n\n")
        cat("Basic Estimates:", input$ecopath_file$name, "\n")
        cat("Diet Composition:", input$ecopath_diet_file$name, "\n\n")
        cat("Parsing and converting to EcoNeTool format...\n")
      })

      # Parse ECOPATH data
      result <- parse_ecopath_data(basic_file, diet_file)

      # Process loaded data (use local variables, no global state)
      ecopath_net <- result$net
      ecopath_info <- result$info

      # Upgrade igraph if needed
      ecopath_net <- igraph::upgrade_graph(ecopath_net)

      # Assign colors by matching functional group names to COLOR_SCHEME
      fg_levels <- get_functional_group_levels()
      ecopath_info$colfg <- sapply(as.character(ecopath_info$fg), function(fg) {
        idx <- which(fg_levels == fg)
        if (length(idx) == 0) return("gray")
        COLOR_SCHEME[idx]
      })

      # Update reactive values
      net_reactive(ecopath_net)
      info_reactive(ecopath_info)

      # Refresh data editor tables
      refresh_data_editor()

      output$ecopath_upload_status <- renderPrint({
        cat("✓ SUCCESS: ECOPATH data imported!\n\n")
        cat("Conversion complete:\n")
        cat("  - Species/groups:", vcount(net_reactive()), "\n")
        cat("  - Trophic links:", ecount(net_reactive()), "\n")
        cat("  - Functional groups:", nlevels(info_reactive()$fg), "\n\n")

        cat("Functional group distribution:\n")
        fg_table <- table(info_reactive()$fg)
        for (fg_name in names(fg_table)) {
          cat("  ", fg_name, ":", fg_table[fg_name], "\n")
        }

        cat("\n⚠ Note: Default values assigned for:\n")
        cat("  - Body masses (based on functional groups)\n")
        cat("  - Metabolic types\n")
        cat("  - Assimilation efficiencies\n\n")
        cat("Use the 'Internal Data Editor' tab to refine these values.\n")
        cat("Navigate to other tabs to explore your ECOPATH model.\n")
      })

    }, error = function(e) {
      output$ecopath_upload_status <- renderPrint({
        cat("✗ ERROR importing ECOPATH data:\n\n")
        cat(e$message, "\n\n")
        cat("Common issues:\n")
        cat("  - Species names don't match between files\n")
        cat("  - Missing required columns (Group name, Biomass)\n")
        cat("  - File format not recognized\n")
        cat("  - Diet matrix structure incorrect\n\n")
        cat("Please check your ECOPATH export files.\n")
      })
    })
  })

  # ============================================================================
  # ECOPATH NATIVE DATABASE IMPORT HANDLER
  # ============================================================================

  # Reactive value to store ECOPATH native metadata preview
  ecopath_native_metadata <- reactiveVal(NULL)

  # Extract metadata when file is selected (before import button clicked)
  observeEvent(input$ecopath_native_file, {
    if (!is.null(input$ecopath_native_file)) {
      tryCatch({
        db_file <- input$ecopath_native_file$datapath

        # Quick metadata-only extraction
        result <- parse_ecopath_native_cross_platform(db_file)

        # Store metadata and basic counts
        ecopath_native_metadata(list(
          metadata = result$metadata,
          n_groups = nrow(result$group_data),
          n_links = nrow(result$diet_data),
          filename = input$ecopath_native_file$name,
          filesize = input$ecopath_native_file$size
        ))
      }, error = function(e) {
        # If extraction fails, store error
        ecopath_native_metadata(list(error = e$message))
      })
    } else {
      ecopath_native_metadata(NULL)
    }
  })

  # Dynamic UI for model preview or guide
  output$ecopath_native_preview_ui <- renderUI({
    preview_data <- ecopath_native_metadata()

    if (is.null(preview_data)) {
      # Show guide when no file selected
      box(
        title = "Native Database Guide",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        HTML("
          <h5>Installation Requirements</h5>
          <p><strong>Windows:</strong> ✓ Supported (RODBC + Microsoft Access Database Engine)</p>
          <pre style='background: #f8f9fa; padding: 5px; font-size: 11px;'>install.packages('RODBC')
# Download MS Access Database Engine from:
# https://www.microsoft.com/download/details.aspx?id=54920</pre>
          <p><strong>Linux/Mac:</strong> mdbtools + Hmisc required</p>
          <pre style='background: #f8f9fa; padding: 5px; font-size: 11px;'>sudo apt-get install mdbtools
install.packages('Hmisc')</pre>
          <hr>
          <h5>Supported Files</h5>
          <ul style='font-size: 12px;'>
            <li>.ewemdb - ECOPATH 6.x ✓</li>
            <li>.mdb - ECOPATH 5.x ✓</li>
            <li>.accdb - Access 2007+ ✓</li>
          </ul>
          <p style='font-size: 12px;'>Direct database import loads all model data: groups, diet matrix, biomass, P/B, Q/B, and parameters.</p>
        ")
      )
    } else if (!is.null(preview_data$error)) {
      # Show error if extraction failed
      box(
        title = "Error Reading Database",
        status = "danger",
        solidHeader = TRUE,
        width = 12,
        HTML(paste0("
          <p style='color: #d9534f;'><strong>Could not read database file</strong></p>
          <pre style='background: #f8f9fa; padding: 10px; font-size: 11px; color: #d9534f;'>", preview_data$error, "</pre>
          <p style='font-size: 12px;'>Please ensure:</p>
          <ul style='font-size: 12px;'>
            <li>File is a valid ECOPATH database (.ewemdb, .eweaccdb, .mdb, .eiidb, .accdb)</li>
            <li>Required packages are installed (RODBC on Windows, Hmisc on Linux/Mac)</li>
            <li>Microsoft Access Database Engine is installed (Windows only)</li>
          </ul>
        "))
      )
    } else {
      # Show model preview
      meta <- preview_data$metadata

      # Helper function to format metadata value
      fmt <- function(val) {
        if (is.null(val) || length(val) == 0 || (length(val) == 1 && is.na(val)) || val == "" || val == -9999) {
          "<span style='color: #999;'>Not specified</span>"
        } else {
          as.character(val)
        }
      }

      # Helper function to safely check if metadata field has valid value
      has_value <- function(field) {
        !is.null(field) && length(field) > 0 && !all(is.na(field)) && field[1] != "" && field[1] != -9999
      }

      # Build location string
      location_parts <- c()
      if (!is.null(meta) && has_value(meta$area_name)) {
        location_parts <- c(location_parts, meta$area_name)
      } else if (!is.null(meta) && has_value(meta$name)) {
        location_parts <- c(location_parts, meta$name)
      }
      if (!is.null(meta) && has_value(meta$country)) {
        location_parts <- c(location_parts, meta$country)
      }
      location_text <- if (length(location_parts) > 0) paste(location_parts, collapse = ", ") else fmt(NA)

      # Build time period string
      time_period_text <- fmt(NA)
      if (!is.null(meta) && has_value(meta$first_year)) {
        if (has_value(meta$num_years) && meta$num_years > 1) {
          end_year <- meta$first_year + meta$num_years - 1
          time_period_text <- paste0(meta$first_year, "-", end_year)
        } else {
          time_period_text <- as.character(meta$first_year)
        }
      }

      # Build geographic coordinates
      coords_text <- fmt(NA)
      if (!is.null(meta) && has_value(meta$min_lat) && has_value(meta$max_lat) && has_value(meta$min_lon) && has_value(meta$max_lon)) {
        coords_text <- sprintf("%.2f°-%.2f°N, %.2f°-%.2f°E", meta$min_lat, meta$max_lat, meta$min_lon, meta$max_lon)
      }

      # Build area text
      area_text <- fmt(meta$area)
      if (!is.null(meta) && has_value(meta$area) && meta$area > 0) {
        area_text <- paste0(meta$area, " km²")
      }

      # Build publication link
      pub_html <- ""
      if (!is.null(meta) && has_value(meta$publication_doi)) {
        pub_html <- paste0("<tr><td style='padding: 2px 0;'><strong>DOI:</strong></td><td><a href='https://doi.org/", meta$publication_doi, "' target='_blank' style='color: #337ab7;'>", meta$publication_doi, "</a></td></tr>")
      } else if (!is.null(meta) && has_value(meta$publication_uri)) {
        pub_html <- paste0("<tr><td style='padding: 2px 0;'><strong>Publication:</strong></td><td><a href='", meta$publication_uri, "' target='_blank' style='color: #337ab7;'>Link</a></td></tr>")
      } else if (!is.null(meta) && has_value(meta$publication_ref)) {
        pub_html <- paste0("<tr><td style='padding: 2px 0;'><strong>Reference:</strong></td><td style='font-size: 11px;'>", meta$publication_ref, "</td></tr>")
      }

      # Build description HTML (truncated if too long)
      desc_html <- ""
      if (!is.null(meta) && has_value(meta$description)) {
        desc_text <- meta$description
        if (nchar(desc_text) > 150) {
          desc_text <- paste0(substr(desc_text, 1, 147), "...")
        }
        desc_html <- paste0("<p style='font-size: 11px; color: #555; font-style: italic; margin: 8px 0;'>", desc_text, "</p>")
      }

      box(
        title = "Model Preview",
        status = "success",
        solidHeader = TRUE,
        width = 12,
        HTML(paste0("
          <h5 style='margin-top: 0;'>", preview_data$filename, "</h5>
          <p style='font-size: 11px; color: #888;'>", round(preview_data$filesize / 1024, 1), " KB</p>
          ", desc_html, "
          <hr style='margin: 10px 0;'>
          <table style='width: 100%; font-size: 12px;'>
            <tr style='background: #f0f9ff;'><td colspan='2' style='padding: 4px 0; font-weight: bold;'>GEOGRAPHIC</td></tr>
            <tr><td style='padding: 2px 0; width: 30%;'>Location:</td><td>", location_text, "</td></tr>
            <tr><td style='padding: 2px 0;'>Ecosystem Type:</td><td>", fmt(meta$ecosystem_type), "</td></tr>
            <tr><td style='padding: 2px 0;'>Area:</td><td>", area_text, "</td></tr>
            <tr><td style='padding: 2px 0;'>Coordinates:</td><td>", coords_text, "</td></tr>
            <tr style='background: #f0f9ff;'><td colspan='2' style='padding: 4px 0; font-weight: bold;'>TEMPORAL</td></tr>
            <tr><td style='padding: 2px 0;'>Time Period:</td><td>", time_period_text, "</td></tr>
            <tr style='background: #f0f9ff;'><td colspan='2' style='padding: 4px 0; font-weight: bold;'>ATTRIBUTION</td></tr>
            <tr><td style='padding: 2px 0;'>Author:</td><td>", fmt(meta$author), "</td></tr>
            <tr><td style='padding: 2px 0;'>Contact:</td><td>", fmt(meta$contact), "</td></tr>
            ", pub_html, "
            <tr style='background: #f0f9ff;'><td colspan='2' style='padding: 4px 0; font-weight: bold;'>MODEL DATA</td></tr>
            <tr><td style='padding: 2px 0;'><strong>Species/Groups:</strong></td><td><strong>", preview_data$n_groups, "</strong></td></tr>
            <tr><td style='padding: 2px 0;'><strong>Diet Links:</strong></td><td><strong>", preview_data$n_links, "</strong></td></tr>
          </table>
          <hr style='margin: 10px 0;'>
          <p style='font-size: 12px; color: #5cb85c;'><i class='fa fa-check-circle'></i> Ready to import</p>
        "))
      )
    }
  })

  # Output status message for ECOPATH native upload
  output$ecopath_native_status <- renderPrint({
    if (is.null(input$ecopath_native_file)) {
      cat("No ECOPATH database file uploaded yet.\n\n")
      cat("Accepted formats:\n")
      cat("  - .ewemdb (ECOPATH 6.x)\n")
      cat("  - .mdb (ECOPATH 5.x)\n")
      cat("  - .eiidb (Alternative format)\n\n")
      cat("Example: 'coast 2011-04-10 10.00.ewemdb'\n")
    } else {
      cat("File selected:", input$ecopath_native_file$name, "\n")
      cat("File size:", round(input$ecopath_native_file$size / 1024, 2), "KB\n\n")
      cat("Click 'Import Native Database' button to process.\n")
    }
  })

  # Render taxonomic API checkbox based on plugin state
  output$taxonomic_api_checkbox_ui <- renderUI({
    plugin_enabled <- !is.null(plugin_states()) && isTRUE(plugin_states()[["taxonomic_api"]])

    # Debug output (only shown when ECO_NT_DEBUG=true)
    if (exists("log_debug")) {
      log_debug("Taxonomic", "Plugin enabled:", plugin_enabled)
      if (!is.null(plugin_states())) {
        log_debug("Taxonomic", "Plugin state:", plugin_states()[["taxonomic_api"]])
      }
    }

    if (plugin_enabled) {
      # Plugin is enabled - show normal checkbox
      tags$div(
        style = "margin-top: 15px; padding: 10px; background-color: #f0f8ff; border-radius: 5px; border-left: 4px solid #0066cc;",
        checkboxInput(
          "use_taxonomic_api",
          tagList(
            HTML("<strong>Enable Taxonomic Database Integration</strong>"),
            tags$a(
              href = "#",
              onclick = "return false;",
              style = "margin-left: 8px; color: #0066cc;",
              title = "When enabled, queries FishBase, WoRMS, and OBIS for authoritative species classification and trait data. Results are cached locally. Requires internet connection for first query. Slower but more accurate.",
              icon("info-circle")
            )
          ),
          value = FALSE
        ),
        tags$small(
          style = "color: #666; display: block; margin-top: -10px; margin-left: 25px; font-style: italic;",
          "Automatically classifies species using online databases (FishBase, WoRMS, OBIS)"
        )
      )
    } else {
      # Plugin is disabled - show disabled checkbox with help message
      # Check if packages are installed
      httr_installed <- requireNamespace("httr", quietly = TRUE)
      jsonlite_installed <- requireNamespace("jsonlite", quietly = TRUE)
      packages_ok <- httr_installed && jsonlite_installed

      help_msg <- if (!packages_ok) {
        "Missing required packages. Install: install.packages(c('httr', 'jsonlite'))"
      } else {
        "Enable in Settings → Plugins → Taxonomic Database Integration"
      }

      tags$div(
        style = "margin-top: 15px; padding: 10px; background-color: #fff3cd; border-radius: 5px; border-left: 4px solid #ffc107;",
        div(
          style = "opacity: 0.6; pointer-events: none;",
          checkboxInput(
            "use_taxonomic_api",
            HTML("<strong>Taxonomic Database Integration</strong> <span class='badge badge-warning'>Disabled</span>"),
            value = FALSE
          )
        ),
        tags$small(
          style = "color: #856404; display: block; margin-top: -10px; margin-left: 25px;",
          icon("exclamation-triangle"), " ", help_msg
        )
      )
    }
  })

  # Reactive value to track taxonomic API progress
  taxonomic_progress <- reactiveVal(NULL)

  # Reactive value to store taxonomic database match report
  taxonomic_report <- reactiveVal(NULL)

  # Render taxonomic progress panel (right side)
  output$taxonomic_progress_panel_ui <- renderUI({
    progress_data <- taxonomic_progress()
    if (is.null(progress_data)) return(NULL)

    # Extract detailed progress info
    current_db <- if (!is.null(progress_data$database)) progress_data$database else "Processing"
    search_term <- if (!is.null(progress_data$search_term)) progress_data$search_term else ""
    species_current <- if (!is.null(progress_data$current)) progress_data$current else 0
    species_total <- if (!is.null(progress_data$total)) progress_data$total else 0

    # Determine database icon and color
    db_icon <- switch(current_db,
      "FishBase" = "fish",
      "WoRMS" = "water",
      "OBIS" = "map-marker-alt",
      "Initializing" = "database",
      "Querying" = "search",
      "database"
    )

    db_color <- switch(current_db,
      "FishBase" = "#0066cc",
      "WoRMS" = "#009688",
      "OBIS" = "#4caf50",
      "Initializing" = "#666",
      "Querying" = "#2196f3",
      "#666"
    )

    box(
      title = tagList(
        icon("spinner", class = "fa-spin"),
        " Taxonomic Database Search"
      ),
      status = "info",
      solidHeader = TRUE,
      width = 12,

      # Progress bar
      tags$div(
        style = "margin-bottom: 20px;",
        tags$h5(
          style = "margin: 0 0 10px 0; color: #555; font-weight: normal; font-size: 16px;",
          sprintf("Processing species %d of %d", species_current, species_total)
        ),
        div(
          class = "progress",
          style = "height: 32px; margin-bottom: 0;",
          div(
            class = "progress-bar progress-bar-striped active",
            role = "progressbar",
            style = sprintf("width: %d%%; background-color: %s;", progress_data$percent, db_color),
            tags$strong(style = "font-size: 16px;", sprintf("%d%%", progress_data$percent))
          )
        )
      ),

      # Database information
      tags$div(
        style = "padding: 15px; background-color: #f8f9fa; border-radius: 5px; margin-bottom: 15px;",
        tags$div(
          style = "margin-bottom: 12px;",
          tags$div(
            style = "font-size: 13px; color: #666; margin-bottom: 4px;",
            "Current Database"
          ),
          tags$div(
            style = sprintf("font-size: 18px; color: %s; font-weight: bold;", db_color),
            icon(db_icon, style = "margin-right: 8px;"),
            current_db
          )
        ),

        # Search term
        if (nchar(search_term) > 0) {
          tags$div(
            tags$div(
              style = "font-size: 13px; color: #666; margin-bottom: 4px;",
              "Searching for"
            ),
            tags$code(
              style = "background-color: #fff; padding: 6px 10px; border-radius: 3px; display: inline-block; font-size: 14px;",
              search_term
            )
          )
        } else { NULL }
      ),

      # Status message
      if (!is.null(progress_data$message) && nchar(progress_data$message) > 0) {
        tags$div(
          style = "padding: 12px; background-color: #e3f2fd; border-left: 3px solid #2196f3; border-radius: 3px; font-size: 14px; color: #1565c0;",
          icon("info-circle", style = "margin-right: 6px;"),
          progress_data$message
        )
      } else { NULL }
    )
  })

  # Render progress text
  output$taxonomic_progress_text <- renderPrint({
    progress_info <- taxonomic_progress()
    if (!is.null(progress_info) && !is.null(progress_info$message)) {
      cat(progress_info$message)
    }
  })

  # Render taxonomic database match report
  output$taxonomic_report_ui <- renderUI({
    report_data <- taxonomic_report()

    if (is.null(report_data)) {
      return(NULL)
    }

    # Create summary statistics
    db_summary <- table(report_data$database_source)
    total_species <- nrow(report_data)

    tagList(
      div(
        style = "margin-top: 15px; padding: 12px; background-color: #e8f5e9; border-left: 4px solid #4caf50; border-radius: 4px;",
        h5(icon("database"), " Taxonomic Database Match Report", style = "margin-top: 0; color: #2e7d32;"),

        # Summary stats
        div(
          style = "margin-bottom: 10px;",
          h6("Summary:", style = "font-weight: bold; margin-bottom: 5px;"),
          tags$ul(
            style = "margin: 0; padding-left: 20px;",
            lapply(names(db_summary), function(src) {
              tags$li(sprintf("%s: %d species (%.1f%%)", src, db_summary[src],
                             (db_summary[src] / total_species) * 100))
            })
          )
        ),

        # Detailed table
        h6("Detailed Results:", style = "font-weight: bold; margin-top: 10px; margin-bottom: 5px;"),
        DTOutput("taxonomic_report_table"),

        # Download button
        div(
          style = "margin-top: 10px;",
          downloadButton("download_taxonomic_report", "Download Report (CSV)",
                        class = "btn-sm btn-success")
        )
      )
    )
  })

  # Render taxonomic report table
  output$taxonomic_report_table <- renderDT({
    report_data <- taxonomic_report()
    req(report_data)

    datatable(
      report_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'tip',
        order = list(list(2, 'desc'))  # Order by database_source
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'database_source',
        backgroundColor = styleEqual(
          c('FishBase', 'WoRMS', 'OBIS', 'Pattern matching'),
          c('#c8e6c9', '#fff9c4', '#b3e5fc', '#f5f5f5')
        )
      ) %>%
      formatStyle(
        'confidence',
        backgroundColor = styleEqual(
          c('high', 'medium', 'low'),
          c('#a5d6a7', '#fff59d', '#ffcc80')
        )
      )
  })

  # Download handler for taxonomic report
  output$download_taxonomic_report <- downloadHandler(
    filename = function() {
      paste0("taxonomic_report_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      report_data <- taxonomic_report()
      req(report_data)
      write.csv(report_data, file, row.names = FALSE)
    }
  )

  # Render formatted ECOPATH native import status
  output$ecopath_native_status_ui <- renderUI({
    status_data <- ecopath_native_status_data()

    if (is.null(status_data)) {
      return(NULL)
    }

    # Log what we're reading from the reactive value
    message("\n=== RENDERING SUCCESS BOX ===")
    message("Species count from reactive: ", status_data$species_count)
    message("Links count from reactive: ", status_data$links_count)
    message("FG count from reactive: ", status_data$fg_count)
    if (!is.null(status_data$fg_distribution)) {
      message("FG distribution from reactive:")
      for (fg_name in names(status_data$fg_distribution)) {
        message(sprintf("  %s: %d", fg_name, status_data$fg_distribution[[fg_name]]))
      }
    }
    message("=============================\n")

    if (status_data$success) {
      # Success message - formatted box
      box(
        title = tagList(
          icon("check-circle", style = "color: #4caf50;"),
          " ECOPATH Import Success"
        ),
        status = "success",
        solidHeader = TRUE,
        width = 12,

        # Database info
        tags$div(
          style = "margin-bottom: 15px;",
          tags$strong("Database: "),
          tags$code(
            style = "background-color: #f5f5f5; padding: 4px 8px; border-radius: 3px; font-size: 13px;",
            status_data$filename
          )
        ),

        # Conversion stats
        tags$div(
          style = "padding: 15px; background-color: #f8f9fa; border-radius: 5px; margin-bottom: 15px;",
          tags$h5("Conversion Complete", style = "margin-top: 0; color: #333; font-size: 15px;"),

          # Show loaded vs imported if groups were filtered
          if (!is.null(status_data$total_groups_loaded) && status_data$total_groups_loaded > status_data$species_count) {
            tags$div(
              style = "margin-bottom: 10px; padding: 8px; background-color: #e3f2fd; border-left: 3px solid #2196f3; border-radius: 3px; font-size: 13px;",
              sprintf("Loaded %d groups from database, imported %d biological groups",
                      status_data$total_groups_loaded,
                      status_data$species_count),
              if (!is.null(status_data$filtered_groups) && length(status_data$filtered_groups) > 0) {
                tags$div(
                  style = "margin-top: 5px; font-size: 12px; color: #666;",
                  sprintf("Filtered out %d technical groups: %s",
                          length(status_data$filtered_groups),
                          paste(status_data$filtered_groups, collapse = ", "))
                )
              } else { NULL }
            )
          } else { NULL },

          tags$div(
            style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 15px;",
            # Species/groups
            tags$div(
              style = "text-align: center; padding: 10px; background-color: #fff; border-radius: 4px;",
              tags$div(style = "font-size: 24px; font-weight: bold; color: #0066cc;", status_data$species_count),
              tags$div(style = "font-size: 12px; color: #666; margin-top: 4px;", "Species/Groups")
            ),
            # Trophic links
            tags$div(
              style = "text-align: center; padding: 10px; background-color: #fff; border-radius: 4px;",
              tags$div(style = "font-size: 24px; font-weight: bold; color: #009688;", status_data$links_count),
              tags$div(style = "font-size: 12px; color: #666; margin-top: 4px;", "Trophic Links")
            ),
            # Functional groups
            tags$div(
              style = "text-align: center; padding: 10px; background-color: #fff; border-radius: 4px;",
              tags$div(style = "font-size: 24px; font-weight: bold; color: #4caf50;", status_data$fg_count),
              tags$div(style = "font-size: 12px; color: #666; margin-top: 4px;", "Functional Groups")
            )
          )
        ),

        # Functional group distribution
        if (!is.null(status_data$fg_distribution)) {
          tags$div(
            style = "margin-bottom: 15px;",
            tags$h6("Functional Group Distribution", style = "margin-bottom: 10px; color: #555;"),
            tags$div(
              style = "display: grid; grid-template-columns: repeat(2, 1fr); gap: 8px; padding: 10px; background-color: #f8f9fa; border-radius: 4px;",
              lapply(names(status_data$fg_distribution), function(fg_name) {
                count <- status_data$fg_distribution[fg_name]
                tags$div(
                  style = "padding: 6px 10px; background-color: #fff; border-radius: 3px; font-size: 13px;",
                  tags$strong(fg_name, ": "),
                  tags$span(style = "color: #666;", count)
                )
              })
            )
          )
        } else { NULL },

        # Ratios (if available)
        if (!is.null(status_data$mean_pb) || !is.null(status_data$mean_qb)) {
          tags$div(
            style = "margin-bottom: 15px; padding: 12px; background-color: #e3f2fd; border-left: 3px solid #2196f3; border-radius: 3px;",
            tags$h6("P/B and Q/B Ratios", style = "margin: 0 0 8px 0; color: #1565c0;"),
            if (!is.null(status_data$mean_pb)) {
              tags$div(
                style = "font-size: 13px; margin-bottom: 4px;",
                tags$strong("Mean P/B: "),
                tags$span(style = "font-family: monospace;", status_data$mean_pb)
              )
            } else { NULL },
            if (!is.null(status_data$mean_qb)) {
              tags$div(
                style = "font-size: 13px;",
                tags$strong("Mean Q/B: "),
                tags$span(style = "font-family: monospace;", status_data$mean_qb)
              )
            } else { NULL }
          )
        } else { NULL },

        # Notes
        tags$div(
          style = "padding: 12px; background-color: #fff3cd; border-left: 3px solid #ffc107; border-radius: 3px; margin-bottom: 12px;",
          tags$strong(icon("info-circle"), " Note:"),
          tags$div(
            style = "margin-top: 8px; font-size: 13px;",
            "Default values assigned for:",
            tags$ul(
              style = "margin: 5px 0 0 0; padding-left: 20px;",
              tags$li("Body masses (based on functional groups)"),
              tags$li("Metabolic types (vertebrates vs invertebrates)"),
              tags$li("Assimilation efficiencies")
            )
          )
        ),

        # Success checklist
        tags$div(
          style = "font-size: 13px;",
          if (status_data$pb_qb_preserved) {
            tags$div(
              style = "color: #4caf50; margin-bottom: 4px;",
              icon("check-circle"), " P/B and Q/B ratios preserved from ECOPATH model"
            )
          } else { NULL },
          if (status_data$metaweb_created) {
            tags$div(
              style = "color: #4caf50; margin-bottom: 4px;",
              icon("check-circle"), " Metaweb format created and loaded"
            )
          } else {
            tags$div(
              style = "color: #ff9800;",
              icon("exclamation-triangle"), " Metaweb conversion had issues (check console)"
            )
          },
          tags$div(
            style = "color: #4caf50;",
            icon("check-circle"), " Dashboard boxes updated"
          )
        ),

        # Next steps
        hr(),
        tags$div(
          style = "font-size: 13px; color: #666;",
          tags$p(
            style = "margin: 5px 0;",
            icon("arrow-right"), " Use the ", tags$strong("'Internal Data Editor'"), " tab to refine these values."
          ),
          tags$p(
            style = "margin: 5px 0;",
            icon("arrow-right"), " Navigate to other tabs to explore your ECOPATH model."
          ),
          tags$p(
            style = "margin: 5px 0;",
            icon("arrow-right"), " For keystoneness analysis, go to ", tags$strong("'Keystoneness Analysis'"), " tab."
          )
        )
      )
    } else {
      # Error message
      tags$div(
        style = "margin-top: 15px; padding: 15px; background-color: #ffebee; border-left: 4px solid #f44336; border-radius: 4px;",
        h5(
          icon("times-circle", style = "color: #f44336;"),
          " ERROR importing ECOPATH native database",
          style = "margin-top: 0; color: #c62828;"
        ),
        tags$pre(
          style = "background-color: #fff; padding: 10px; border-radius: 3px; overflow-x: auto; font-size: 12px;",
          status_data$error_message
        ),
        tags$div(
          style = "margin-top: 10px;",
          HTML(status_data$solution_html)
        )
      )
    }
  })

  # Ensure taxonomic outputs are not suspended when hidden
  outputOptions(output, "taxonomic_progress_panel_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "taxonomic_progress_text", suspendWhenHidden = FALSE)
  outputOptions(output, "taxonomic_report_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "taxonomic_report_table", suspendWhenHidden = FALSE)

  # ======================================================================
  # EMODnet Habitat Integration
  # ======================================================================

  # Observer to load EUSeaMap data when EMODnet habitat enrichment is enabled
  observeEvent(input$enable_emodnet_habitat, {
    if (input$enable_emodnet_habitat && is.null(euseamap_data())) {
      showNotification("Loading EUSeaMap habitat data (optimized regional loading)...",
                       type = "message", duration = NULL, id = "emodnet_loading")

      tryCatch({
        # Determine region from sampling location if available
        bbt_name <- NULL
        custom_bbox <- NULL

        # Check if there's a sampling location in the data
        if (!is.null(current_network()) && "sampling_lon" %in% names(current_network()$nodes)) {
          # Use first valid sampling location
          sampling_lon <- current_network()$nodes$sampling_lon[1]
          sampling_lat <- current_network()$nodes$sampling_lat[1]

          if (!is.na(sampling_lon) && !is.na(sampling_lat)) {
            # Create bbox around sampling point (±2 degrees = small area, avoids geometry errors)
            custom_bbox <- c(
              sampling_lon - 2, sampling_lat - 2,
              sampling_lon + 2, sampling_lat + 2
            )
            cat("\n🗺️  Loading habitat for sampling location:", sampling_lon, ",", sampling_lat, "\n")
          }
        }

        # If no custom bbox determined, default to small test area (Baltic)
        if (is.null(custom_bbox)) {
          cat("\n⚠️  No sampling location found, using default Baltic test area\n")
          custom_bbox <- c(20, 55, 21, 56)  # Small 1x1 degree test area
        }

        # Load regional EUSeaMap data with custom bbox (avoids large regional bbox!)
        euseamap <- load_regional_euseamap(
          bbt_name = bbt_name,
          custom_bbox = custom_bbox,
          path = "data/EUSeaMap_2025/EUSeaMap_2025.gdb"
        )
        euseamap_data(euseamap)

        # Get region info
        region <- attr(euseamap, "region") %||% "baltic"

        removeNotification("emodnet_loading")
        showNotification(
          sprintf("✓ EUSeaMap loaded: %d polygons (%s region)", nrow(euseamap), toupper(region)),
          type = "message",
          duration = 5
        )
      }, error = function(e) {
        removeNotification("emodnet_loading")
        showNotification(
          paste("Failed to load EUSeaMap:", e$message,
                "\nPlease ensure EUSeaMap_2025.gdb exists in data/ directory"),
          type = "error",
          duration = 10
        )
        # Disable checkbox if loading failed
        updateCheckboxInput(session, "enable_emodnet_habitat", value = FALSE)
      })
    }
  })

  # Handle ECOPATH native import when button clicked
  observeEvent(input$load_ecopath_native, {
    req(input$ecopath_native_file)

    tryCatch({
      db_file <- input$ecopath_native_file$datapath

      # Clear previous status
      ecopath_native_status_data(NULL)

      # Parse ECOPATH native database (pass session for progress updates)
      result <- parse_ecopath_native(db_file, session = session)

      # Store data for Rpath module (ECOPATH/ECOSIM integration)
      ecopath_import_data(result)

      # Process loaded data (use local variables, no global state)
      native_net <- result$net
      native_info <- result$info

      # Validate required data
      if (is.null(native_net) || is.null(native_info)) {
        stop("Invalid ECOPATH data: Missing network or species information")
      }
      if (!"fg" %in% colnames(native_info)) {
        stop("Invalid ECOPATH data: Missing functional group column")
      }

      # Upgrade igraph if needed
      native_net <- igraph::upgrade_graph(native_net)

      # Assign colors by matching functional group names to COLOR_SCHEME
      fg_levels <- get_functional_group_levels()
      native_info$colfg <- sapply(as.character(native_info$fg), function(fg) {
        idx <- which(fg_levels == fg)
        if (length(idx) == 0) return("gray")
        COLOR_SCHEME[idx]
      })

      # Update reactive values for dashboard
      net_reactive(native_net)
      info_reactive(native_info)

      # Refresh data editor tables
      refresh_data_editor()

      # Update dashboard metadata IMMEDIATELY (before metaweb conversion)
      # Extract location and time period from ECOPATH metadata if available
      location_text <- "ECOPATH Import"
      time_period_text <- "Model-derived"

      if (!is.null(result$metadata)) {
        meta <- result$metadata

        # Helper function to safely check if metadata field has valid value
        has_val <- function(field) {
          !is.null(field) && length(field) > 0 && !all(is.na(field)) && field[1] != "" && field[1] != -9999
        }

        # Build location from available metadata
        location_parts <- c()
        if (has_val(meta$area_name)) {
          location_parts <- c(location_parts, meta$area_name)
        } else if (has_val(meta$name)) {
          location_parts <- c(location_parts, meta$name)
        }
        if (has_val(meta$country)) {
          location_parts <- c(location_parts, meta$country)
        }
        if (has_val(meta$ecosystem_type)) {
          location_parts <- c(location_parts, paste0("(", meta$ecosystem_type, ")"))
        }
        if (length(location_parts) > 0) {
          location_text <- paste(location_parts, collapse = ", ")
        }

        # Build time period from available metadata
        if (has_val(meta$first_year)) {
          if (has_val(meta$num_years) && meta$num_years > 1) {
            end_year <- meta$first_year + meta$num_years - 1
            time_period_text <- paste0(meta$first_year, "-", end_year)
          } else {
            time_period_text <- as.character(meta$first_year)
          }
        } else if (has_val(meta$date_start) && has_val(meta$date_end)) {
          time_period_text <- paste(meta$date_start, "to", meta$date_end)
        }
      }

      metaweb_metadata(list(
        location = location_text,
        time_period = time_period_text,
        source = input$ecopath_native_file$name
      ))

      # Trigger dashboard update
      dashboard_trigger(dashboard_trigger() + 1)

      # Convert to metaweb format and update current metaweb
      metaweb_created <- FALSE
      tryCatch({
        # Create species data frame for metaweb (use native_net and native_info, not reactive values)
        species_data <- data.frame(
          species_id = igraph::V(native_net)$name,
          species_name = igraph::V(native_net)$name,
          functional_group = as.character(native_info$fg),
          biomass = native_info$meanB,
          stringsAsFactors = FALSE
        )

        # Add optional columns if available
        if ("PB" %in% colnames(native_info)) species_data$pb_ratio <- native_info$PB
        if ("QB" %in% colnames(native_info)) species_data$qb_ratio <- native_info$QB
        if ("bodymasses" %in% colnames(native_info)) species_data$body_mass <- native_info$bodymasses
        if ("taxon" %in% colnames(native_info)) species_data$taxon <- native_info$taxon

        # ============================================================
        # Add EMODnet habitat data if enabled
        # ============================================================
        if (isTRUE(input$enable_emodnet_habitat) && !is.null(euseamap_data())) {
          tryCatch({
            # Get sampling location from inputs
            sampling_lon <- input$sampling_longitude
            sampling_lat <- input$sampling_latitude

            # Validate coordinates
            if (!is.null(sampling_lon) && !is.null(sampling_lat) &&
                !is.na(sampling_lon) && !is.na(sampling_lat)) {

              # Add habitat data to all species
              species_data <- add_habitat_to_species(
                species_data,
                sampling_lon,
                sampling_lat,
                euseamap_data()
              )

              showNotification(
                sprintf("✓ Added habitat data at %.4f°E, %.4f°N", sampling_lon, sampling_lat),
                type = "message",
                duration = 3
              )
            } else {
              warning("Invalid sampling coordinates for habitat enrichment")
            }
          }, error = function(e) {
            warning("Failed to add habitat data: ", e$message)
            showNotification(
              paste("Warning: Could not add habitat data:", e$message),
              type = "warning",
              duration = 5
            )
          })
        }
        # ============================================================

        # Create interactions data frame for metaweb (use native_net, not reactive value)
        edges <- as_edgelist(native_net)
        interactions_data <- data.frame(
          predator_id = edges[,1],
          prey_id = edges[,2],
          quality_code = 3,  # ECOPATH data = code 3 (model-derived)
          source = paste0("ECOPATH: ", input$ecopath_native_file$name),
          stringsAsFactors = FALSE
        )

        # Create metaweb object
        metaweb <- create_metaweb(
          species = species_data,
          interactions = interactions_data,
          metadata = list(
            name = tools::file_path_sans_ext(input$ecopath_native_file$name),
            source = "ECOPATH native database",
            region = "Imported ECOPATH model",
            file = input$ecopath_native_file$name
          )
        )

        # Store metaweb
        current_metaweb(metaweb)
        metaweb_created <- TRUE

      }, error = function(e) {
        warning("Could not convert to metaweb format: ", e$message)
        print(paste("Metaweb conversion error:", e$message))
      })

      # Populate status data for formatted display (use native_info and native_net, not reactive values)
      # Calculate network metrics once (avoid redundant calls)
      species_count <- vcount(native_net)
      links_count <- ecount(native_net)
      fg_count <- nlevels(native_info$fg)

      # Create functional group distribution (as.list preserves names automatically)
      fg_table <- table(native_info$fg)
      fg_distribution <- as.list(fg_table)

      # Log what we're about to send to the UI
      message("\n=== UPDATING SUCCESS BOX DATA ===")
      message("Species count: ", species_count)
      message("Links count: ", links_count)
      message("Functional groups: ", fg_count)
      message("FG distribution:")
      message(paste(sprintf("  %s: %d", names(fg_distribution), unlist(fg_distribution)), collapse = "\n"))
      message("=================================\n")

      ecopath_native_status_data(list(
        success = TRUE,
        filename = input$ecopath_native_file$name,
        species_count = species_count,
        links_count = links_count,
        fg_count = fg_count,
        fg_distribution = fg_distribution,
        mean_pb = if ("PB" %in% colnames(native_info)) round(mean(native_info$PB, na.rm = TRUE), 3) else NULL,
        mean_qb = if ("QB" %in% colnames(native_info)) round(mean(native_info$QB, na.rm = TRUE), 3) else NULL,
        pb_qb_preserved = TRUE,
        metaweb_created = metaweb_created,
        total_groups_loaded = result$total_groups_loaded,
        filtered_groups = result$filtered_groups
      ))

    }, error = function(e) {
      # Build error solution HTML
      os <- Sys.info()["sysname"]

      solution_html <- if (os == "Windows") {
        paste0(
          "<div style='padding: 10px; background-color: #fff3cd; border-radius: 4px; margin-bottom: 10px;'>",
          "<strong>⚠ WINDOWS USERS:</strong><br>",
          "Windows ECOPATH import requires RODBC package.<br><br>",
          "<strong>SOLUTION:</strong><br>",
          "1. Install RODBC package: <code>install.packages('RODBC')</code><br>",
          "2. Install Microsoft Access Database Engine:<br>",
          "&nbsp;&nbsp;<a href='https://www.microsoft.com/download/details.aspx?id=54920' target='_blank'>Download here</a> (Choose 64-bit if using 64-bit R)<br><br>",
          "<strong>ALTERNATIVE:</strong> Use CSV/Excel export method above",
          "</div>"
        )
      } else {
        paste0(
          "<div style='padding: 10px; background-color: #fff3cd; border-radius: 4px; margin-bottom: 10px;'>",
          "<strong>LINUX/MAC USERS:</strong><br>",
          "Install mdbtools package:<br>",
          "&nbsp;&nbsp;Linux: <code>sudo apt-get install mdbtools</code><br>",
          "&nbsp;&nbsp;Mac: <code>brew install mdbtools</code><br><br>",
          "Also install Hmisc package: <code>install.packages('Hmisc')</code>",
          "</div>"
        )
      }

      solution_html <- paste0(
        solution_html,
        "<div style='padding: 10px; background-color: #f5f5f5; border-radius: 4px;'>",
        "<strong>Alternative solutions:</strong><br><br>",
        "1. Missing Hmisc package: <code>install.packages('Hmisc')</code><br><br>",
        "2. Use CSV/Excel exports instead (all platforms):<br>",
        "&nbsp;&nbsp;- Export from ECOPATH: File > Export<br>",
        "&nbsp;&nbsp;- Use 'Import ECOPATH CSV/Excel Exports' above<br><br>",
        "3. Corrupted database file: Re-export from ECOPATH software",
        "</div>"
      )

      ecopath_native_status_data(list(
        success = FALSE,
        error_message = e$message,
        solution_html = solution_html
      ))
    })
  })
}
