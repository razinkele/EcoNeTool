# Server logic for Trait-Based Food Web Construction Module

traitfoodweb_server <- function(input, output, session) {

  # ============================================================================
  # REACTIVE VALUES
  # ============================================================================

  rv <- reactiveValues(
    trait_data = NULL,
    adjacency_matrix = NULL,
    prob_matrix = NULL,
    network_igraph = NULL,
    validation_result = NULL
  )

  # Separate reactiveVal for lookup log (for better reactivity)
  lookup_log <- reactiveVal(character())

  # Maximum log entries to prevent unbounded memory growth
  MAX_LOG_ENTRIES <- 1000

  # Helper function to log messages (to console and progress)
  # Uses isolate() for thread-safe read-modify-write pattern
  append_log <- function(entry, update_progress = FALSE) {
    # Log to console for debugging
    cat(entry, "\n")

    # Update lookup_log with bounded size (circular buffer)
    isolate({
      current <- lookup_log()
      # Trim old entries if exceeding max size
      if (length(current) >= MAX_LOG_ENTRIES) {
        current <- tail(current, MAX_LOG_ENTRIES - 100)  # Keep last 900
      }
      lookup_log(c(current, entry))
    })
  }

  # Helper to clear log (prevents memory buildup between sessions)
  clear_log <- function() {
    lookup_log(character())
  }

  # ============================================================================
  # EXAMPLE DATASETS
  # ============================================================================

  get_example_dataset <- function(type) {
    if (type == "simple") {
      # Simple 5-species example
      data.frame(
        species = c("Predatory_fish", "Small_fish", "Zooplankton", "Phytoplankton", "Benthic_filter_feeder"),
        MS = c("MS5", "MS3", "MS2", "MS1", "MS3"),
        FS = c("FS1", "FS1", "FS6", "FS0", "FS6"),
        MB = c("MB5", "MB5", "MB4", "MB2", "MB1"),
        EP = c("EP4", "EP4", "EP3", "EP4", "EP2"),
        PR = c("PR0", "PR0", "PR0", "PR0", "PR6"),
        stringsAsFactors = FALSE
      )
    } else if (type == "marine") {
      # Marine invertebrates example
      data.frame(
        species = c("Sea_star", "Crab", "Mussel", "Barnacle", "Polychaete",
                   "Amphipod", "Copepod", "Diatom", "Kelp", "Sponge"),
        MS = c("MS4", "MS4", "MS3", "MS2", "MS3",
              "MS2", "MS2", "MS1", "MS5", "MS4"),
        FS = c("FS1", "FS1", "FS6", "FS6", "FS5",
              "FS4", "FS6", "FS0", "FS0", "FS6"),
        MB = c("MB3", "MB3", "MB1", "MB1", "MB3",
              "MB4", "MB5", "MB2", "MB1", "MB1"),
        EP = c("EP2", "EP2", "EP2", "EP2", "EP1",
              "EP3", "EP4", "EP4", "EP2", "EP2"),
        PR = c("PR7", "PR8", "PR6", "PR6", "PR0",
              "PR5", "PR0", "PR0", "PR0", "PR7"),
        stringsAsFactors = FALSE
      )
    } else if (type == "complex") {
      # Complex 20-species food web
      set.seed(42)
      n <- 20
      data.frame(
        species = paste0("Species_", sprintf("%02d", 1:n)),
        MS = sample(paste0("MS", 1:6), n, replace = TRUE,
                   prob = c(0.15, 0.20, 0.25, 0.20, 0.15, 0.05)),
        FS = sample(paste0("FS", c(1, 2, 4, 5, 6)), n, replace = TRUE,
                   prob = c(0.25, 0.10, 0.25, 0.20, 0.20)),
        MB = sample(paste0("MB", 1:5), n, replace = TRUE),
        EP = sample(paste0("EP", 1:4), n, replace = TRUE),
        PR = sample(paste0("PR", c(0, 2, 3, 5, 6, 7, 8)), n, replace = TRUE,
                   prob = c(0.30, 0.10, 0.10, 0.10, 0.15, 0.15, 0.10)),
        stringsAsFactors = FALSE
      )
    }
  }

  # ============================================================================
  # LOAD TRAIT DATA
  # ============================================================================

  # Load data based on input method
  observeEvent(input$trait_input_method, {
    if (input$trait_input_method == "example") {
      rv$trait_data <- get_example_dataset(input$trait_example_dataset)
    }
  })

  observeEvent(input$trait_example_dataset, {
    if (input$trait_input_method == "example") {
      rv$trait_data <- get_example_dataset(input$trait_example_dataset)
    }
  })

  # Upload CSV
  observeEvent(input$trait_file, {
    req(input$trait_file)
    tryCatch({
      rv$trait_data <- read.csv(input$trait_file$datapath, stringsAsFactors = FALSE)
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
    })
  })

  # Create manual template
  observeEvent(input$trait_create_template, {
    req(input$trait_n_species)
    rv$trait_data <- create_trait_template(input$trait_n_species)
  })

  # Automated trait lookup
  observeEvent(input$trait_run_lookup, {
    req(input$trait_species_list)

    # Parse species list
    species_list <- strsplit(input$trait_species_list, "\n")[[1]]
    species_list <- trimws(species_list)
    species_list <- species_list[species_list != ""]

    if (length(species_list) == 0) {
      showNotification("Please enter at least one species name", type = "warning")
      return()
    }

    # Initialize progress log with startup message
    lookup_log(c(
      "Initializing automated trait lookup...",
      paste("Total species to process:", length(species_list))
    ))

    # Determine which databases to use
    databases_to_check <- input$trait_databases
    db_names <- c(
      "worms" = "WoRMS",
      "fishbase" = "FishBase",
      "sealifebase" = "SeaLifeBase",
      "biotic" = "BIOTIC",
      "freshwater" = "freshwaterecology.info",
      "maredat" = "MAREDAT",
      "ptdb" = "PTDB",
      "algaebase" = "AlgaeBase",
      "shark" = "SHARK"
    )

    # Set database file paths
    biotic_file <- if ("biotic" %in% databases_to_check) file.path("data", "biotic_traits.csv") else NULL
    maredat_file <- if ("maredat" %in% databases_to_check) file.path("data", "maredat_zooplankton.csv") else NULL
    ptdb_file <- if ("ptdb" %in% databases_to_check) file.path("data", "ptdb_phytoplankton.csv") else NULL

    # Create cache directory
    cache_dir <- file.path("cache", "taxonomy")
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE)
    }

    # Use Progress$new() for real-time modal updates (same as ECOPATH import)
    progress <- Progress$new(
      min = 0,
      max = length(species_list)
    )
    on.exit(progress$close())

    progress$set(
      message = "🔍 Automated Trait Lookup",
      value = 0,
      detail = sprintf("Initializing trait lookup for %d species\nDatabases: %s\nResults will be cached locally",
                      length(species_list),
                      paste(sapply(databases_to_check, function(x) db_names[x]), collapse = ", "))
    )

    # Show detailed log in console too
    cat("\n========================================\n")
    cat("AUTOMATED TRAIT LOOKUP\n")
    cat("========================================\n")
    cat("Species to process:", length(species_list), "\n")
    cat("Databases selected:", paste(sapply(databases_to_check, function(x) db_names[x]), collapse = ", "), "\n")
    cat("========================================\n\n")

    # Run lookup with detailed progress
    tryCatch({
      results_list <- list()

      for (i in seq_along(species_list)) {
        species <- species_list[i]
        percent <- round((i / length(species_list)) * 100)

        # Initialize progress messages for this species (use environment for closure safety)
        progress_env <- new.env(parent = emptyenv())
        progress_env$messages <- c(
          sprintf("[%d/%d] %s", i, length(species_list), species),
          "",
          sprintf("📊 Progress: %d%%", percent)
        )

        # Update progress with initial message
        progress$set(
          value = i - 1,
          detail = paste(progress_env$messages, collapse = "\n")
        )

        # Log current species to console
        append_log(paste0("\n=== [", i, "/", length(species_list), "] ", species, " ==="))

        # Create progress callback for real-time updates
        progress_callback <- function(msg) {
          # Add message to accumulator
          progress_env$messages <- c(progress_env$messages, msg)

          # Update progress modal with accumulated messages
          progress$set(
            value = i - 1,
            detail = paste(progress_env$messages, collapse = "\n")
          )
        }

        # Check cache first
        cache_file <- file.path(cache_dir, paste0(gsub(" ", "_", species), ".rds"))
        if (file.exists(cache_file)) {
          cached <- readRDS(cache_file)
          cache_age_days <- as.numeric(difftime(Sys.time(), cached$timestamp, units = "days"))
          if (cache_age_days < 30) {
            append_log(paste0("  ✓ Using cached data from ", format(cached$timestamp, "%Y-%m-%d"),
                             " (", round(cache_age_days, 1), " days old - valid for 30 days)"))
            append_log(paste0("      Skipping database queries for this species"))
            results_list[[i]] <- cached$traits
            next
          } else {
            append_log(paste0("  ⚠ Cache expired (", round(cache_age_days, 1), " days old) - re-querying databases"))
          }
        }

        # Initialize result
        result <- data.frame(
          species = species,
          MS = NA_character_,
          FS = NA_character_,
          MB = NA_character_,
          EP = NA_character_,
          PR = NA_character_,
          source = NA_character_,
          confidence = NA_character_,
          stringsAsFactors = FALSE
        )

        # Query each database in hierarchy
        sources_found <- character()

        # 1. WoRMS
        if ("worms" %in% databases_to_check) {
          progress_callback("🌊 Querying WoRMS for taxonomic classification...")
          append_log("  → Querying WoRMS for taxonomic classification...")
          worms_result <- lookup_worms_traits(species)
          if (worms_result$success) {
            success_msg <- sprintf("   ✓ Found: %s %s", worms_result$traits$rank, worms_result$traits$scientificname)
            progress_callback(success_msg)
            progress_callback(sprintf("      Phylum: %s | Class: %s | Order: %s",
                                     worms_result$traits$phylum,
                                     worms_result$traits$class,
                                     worms_result$traits$order))
            append_log(paste0("    ✓ WoRMS: Found ", worms_result$traits$rank, " ", worms_result$traits$scientificname))
            append_log(paste0("      Phylum: ", worms_result$traits$phylum,
                             " | Class: ", worms_result$traits$class,
                             " | Order: ", worms_result$traits$order))
            sources_found <- c(sources_found, "WoRMS")
          } else {
            progress_callback("   ✗ WoRMS: Not found")
            append_log("    ✗ WoRMS: Species not found in database")
          }
        }

        # 2. FishBase
        if ("fishbase" %in% databases_to_check) {
          progress_callback("🐟 Querying FishBase for fish morphology and ecology...")
          append_log("  → Querying FishBase for fish morphology and ecology...")
          fb_result <- lookup_fishbase_traits(species)
          if (fb_result$success) {
            progress_callback("   ✓ FishBase: Found fish species")
            progress_callback(sprintf("      Max length: %s cm | Weight: %s g | TL: %s",
                                     fb_result$traits$max_length_cm,
                                     fb_result$traits$weight_g,
                                     fb_result$traits$trophic_level))
            progress_callback(sprintf("      Habitat: %s | Shape: %s",
                                     fb_result$traits$habitat,
                                     fb_result$traits$body_shape))
            append_log(paste0("    ✓ FishBase: Found fish species"))
            append_log(paste0("      Max length: ", fb_result$traits$max_length_cm, " cm",
                             " | Weight: ", fb_result$traits$weight_g, " g",
                             " | Trophic level: ", fb_result$traits$trophic_level))
            append_log(paste0("      Habitat: ", fb_result$traits$habitat,
                             " | Body shape: ", fb_result$traits$body_shape))
            sources_found <- c(sources_found, "FishBase")
          } else {
            progress_callback("   ✗ FishBase: Not found")
            append_log("    ✗ FishBase: Not a fish or not found in database")
          }
        }

        # 3. SeaLifeBase
        if ("sealifebase" %in% databases_to_check) {
          progress_callback("🐚 Querying SeaLifeBase for marine invertebrate traits...")
          append_log("  → Querying SeaLifeBase for marine invertebrate traits...")
          slb_result <- lookup_sealifebase_traits(species)
          if (slb_result$success) {
            progress_callback("   ✓ SeaLifeBase: Found marine invertebrate")
            progress_callback(sprintf("      Max length: %s cm | Weight: %s g | TL: %s",
                                     slb_result$traits$max_length_cm,
                                     slb_result$traits$weight_g,
                                     slb_result$traits$trophic_level))
            progress_callback(sprintf("      Habitat: %s", slb_result$traits$habitat))
            append_log(paste0("    ✓ SeaLifeBase: Found marine invertebrate"))
            append_log(paste0("      Max length: ", slb_result$traits$max_length_cm, " cm",
                             " | Weight: ", slb_result$traits$weight_g, " g"))
            append_log(paste0("      Habitat: ", slb_result$traits$habitat,
                             " | Trophic level: ", slb_result$traits$trophic_level))
            sources_found <- c(sources_found, "SeaLifeBase")
          } else {
            progress_callback("   ✗ SeaLifeBase: Not found")
            append_log("    ✗ SeaLifeBase: Not found in database")
          }
        }

        # 4. BIOTIC
        if ("biotic" %in% databases_to_check) {
          progress_callback("🦀 Querying BIOTIC for benthic invertebrate biological traits...")
          append_log("  → Querying BIOTIC for benthic invertebrate biological traits...")
          if (!is.null(biotic_file) && file.exists(biotic_file)) {
            biotic_result <- lookup_biotic_traits(species, biotic_file)
            if (biotic_result$success) {
              progress_callback("   ✓ BIOTIC: Found biological trait information")
              progress_callback("      Retrieved categorical traits for benthic species")
              append_log(paste0("    ✓ BIOTIC: Found biological trait information"))
              append_log(paste0("      Retrieved categorical traits for benthic species"))
              sources_found <- c(sources_found, "BIOTIC")
            } else {
              progress_callback("   ✗ BIOTIC: Not found in local database")
              append_log("    ✗ BIOTIC: Species not found in local database")
            }
          } else {
            progress_callback("   ⚠ BIOTIC: Local database file not available")
            append_log("    ⚠ BIOTIC: Local database file (data/biotic_traits.csv) not available")
          }
        }

        # 5. freshwaterecology.info
        if ("freshwater" %in% databases_to_check) {
          progress_callback("🌊 Querying freshwaterecology.info for freshwater species traits...")
          append_log("  → Querying freshwaterecology.info for freshwater species traits...")
          fw_result <- lookup_freshwaterecology_traits(species)
          if (fw_result$success) {
            progress_callback("   ✓ freshwaterecology.info: Found freshwater species")
            progress_callback(sprintf("      Max length: %s mm | Feeding: %s",
                                     fw_result$traits$max_length_mm,
                                     fw_result$traits$feeding_type))
            progress_callback(sprintf("      Locomotion: %s | Habitat: %s",
                                     fw_result$traits$locomotion,
                                     fw_result$traits$habitat))
            append_log(paste0("    ✓ freshwaterecology.info: Found freshwater species"))
            append_log(paste0("      Max length: ", fw_result$traits$max_length_mm, " mm",
                             " | Feeding type: ", fw_result$traits$feeding_type))
            append_log(paste0("      Locomotion: ", fw_result$traits$locomotion,
                             " | Habitat: ", fw_result$traits$habitat))
            sources_found <- c(sources_found, "freshwater")
          } else if (!is.null(fw_result$note)) {
            progress_callback(sprintf("   ⚠ freshwaterecology.info: %s", fw_result$note))
            append_log(paste0("    ⚠ freshwaterecology.info: ", fw_result$note))
          } else {
            progress_callback("   ✗ freshwaterecology.info: No results")
            append_log("    ✗ freshwaterecology.info: API returned no results")
          }
        }

        # 6. MAREDAT
        if ("maredat" %in% databases_to_check) {
          progress_callback("🦐 Querying MAREDAT for zooplankton functional traits...")
          append_log("  → Querying MAREDAT for zooplankton functional traits...")
          if (!is.null(maredat_file) && file.exists(maredat_file)) {
            maredat_result <- lookup_maredat_traits(species, maredat_file)
            if (maredat_result$success) {
              progress_callback("   ✓ MAREDAT: Found zooplankton species")
              progress_callback("      Retrieved body size and feeding mode data")
              append_log(paste0("    ✓ MAREDAT: Found zooplankton species in database"))
              append_log(paste0("      Retrieved body size and feeding mode data"))
              sources_found <- c(sources_found, "MAREDAT")
            } else {
              progress_callback("   ✗ MAREDAT: Not found in local database")
              append_log("    ✗ MAREDAT: Species not found in local database")
            }
          } else {
            progress_callback("   ⚠ MAREDAT: Local database file not available")
            append_log("    ⚠ MAREDAT: Local database file (data/maredat_zooplankton.csv) not available")
          }
        }

        # 7. PTDB
        if ("ptdb" %in% databases_to_check) {
          progress_callback("🌿 Querying PTDB for phytoplankton functional traits...")
          append_log("  → Querying PTDB for phytoplankton functional traits...")
          if (!is.null(ptdb_file) && file.exists(ptdb_file)) {
            ptdb_result <- lookup_ptdb_traits(species, ptdb_file)
            if (ptdb_result$success) {
              progress_callback("   ✓ PTDB: Found phytoplankton species")
              progress_callback("      Retrieved cell size and growth form data")
              append_log(paste0("    ✓ PTDB: Found phytoplankton species in database"))
              append_log(paste0("      Retrieved cell size and growth form data"))
              sources_found <- c(sources_found, "PTDB")
            } else {
              progress_callback("   ✗ PTDB: Not found in local database")
              append_log("    ✗ PTDB: Species not found in local database")
            }
          } else {
            progress_callback("   ⚠ PTDB: Local database file not available")
            append_log("    ⚠ PTDB: Local database file (data/ptdb_phytoplankton.csv) not available")
          }
        }

        # 8. AlgaeBase
        if ("algaebase" %in% databases_to_check) {
          progress_callback("🌱 Querying AlgaeBase for algae taxonomy and traits...")
          append_log("  → Querying AlgaeBase for algae taxonomy and traits...")
          ab_result <- lookup_algaebase_traits(species)
          if (ab_result$success) {
            progress_callback(sprintf("   ✓ AlgaeBase: Identified as %s", ab_result$traits$functional_group))
            progress_callback("      Using WoRMS fallback for algal species classification")
            append_log(paste0("    ✓ AlgaeBase: Identified as ", ab_result$traits$functional_group))
            append_log(paste0("      Using WoRMS fallback for algal species classification"))
            sources_found <- c(sources_found, "AlgaeBase")
          } else {
            progress_callback("   ✗ AlgaeBase: Not an algae or not found")
            append_log("    ✗ AlgaeBase: Not an algae species or not found")
          }
        }

        # 9. SHARK
        if ("shark" %in% databases_to_check) {
          progress_callback("🦈 Querying SHARK for Swedish Ocean Archives data...")
          append_log("  → Querying SHARK for Swedish Ocean Archives data...")
          shark_result <- lookup_shark_traits(species)
          if (shark_result$success) {
            progress_callback("   ✓ SHARK: Found occurrence records")
            progress_callback(sprintf("      Depth range: %s m", paste(shark_result$traits$depth_range_m, collapse="-")))
            progress_callback(sprintf("      Geographic range: Lat %s to %s",
                                     shark_result$traits$latitude_range[1],
                                     shark_result$traits$latitude_range[2]))
            append_log(paste0("    ✓ SHARK: Found occurrence records"))
            append_log(paste0("      Depth range: ", paste(shark_result$traits$depth_range_m, collapse="-"), " m"))
            append_log(paste0("      Geographic range: Lat ", shark_result$traits$latitude_range[1],
                             " to ", shark_result$traits$latitude_range[2]))
            sources_found <- c(sources_found, "SHARK")
          } else if (!is.null(shark_result$note)) {
            progress_callback(sprintf("   ⚠ SHARK: %s", shark_result$note))
            append_log(paste0("    ⚠ SHARK: ", shark_result$note))
          } else {
            progress_callback("   ✗ SHARK: No records in Swedish waters")
            append_log("    ✗ SHARK: No occurrence records found in Swedish waters")
          }
        }

        # Run full lookup with harmonization
        progress_callback("")
        progress_callback("⚙️ Harmonizing raw trait data to categorical classes...")
        append_log("  → Harmonizing raw trait data to categorical classes (MS, FS, MB, EP, PR)...")
        full_result <- lookup_species_traits(
          species,
          biotic_file = biotic_file,
          maredat_file = maredat_file,
          ptdb_file = ptdb_file,
          cache_dir = cache_dir
        )

        results_list[[i]] <- full_result

        # Log final result with details
        n_traits <- sum(!is.na(c(full_result$MS, full_result$FS, full_result$MB, full_result$EP, full_result$PR)))
        progress_callback(sprintf("✓ COMPLETE: %d/5 traits assigned", n_traits))
        progress_callback(sprintf("   MS: %s | FS: %s | MB: %s | EP: %s | PR: %s",
                                 ifelse(is.na(full_result$MS), "NA", full_result$MS),
                                 ifelse(is.na(full_result$FS), "NA", full_result$FS),
                                 ifelse(is.na(full_result$MB), "NA", full_result$MB),
                                 ifelse(is.na(full_result$EP), "NA", full_result$EP),
                                 ifelse(is.na(full_result$PR), "NA", full_result$PR)))
        progress_callback(sprintf("   Sources: %s | Confidence: %s", full_result$source, full_result$confidence))
        progress_callback("   ✓ Result cached for future lookups")

        append_log(paste0("  ✓ HARMONIZATION COMPLETE: ", n_traits, "/5 traits assigned"))
        append_log(paste0("      MS (Max Size): ", ifelse(is.na(full_result$MS), "NA", full_result$MS),
                         " | FS (Foraging): ", ifelse(is.na(full_result$FS), "NA", full_result$FS),
                         " | MB (Mobility): ", ifelse(is.na(full_result$MB), "NA", full_result$MB)))
        append_log(paste0("      EP (Envir. Pos.): ", ifelse(is.na(full_result$EP), "NA", full_result$EP),
                         " | PR (Predator Res.): ", ifelse(is.na(full_result$PR), "NA", full_result$PR)))
        append_log(paste0("      Data sources: ", full_result$source, " | Confidence: ", full_result$confidence))

        # Rate limiting
        if (i < length(species_list)) {
          Sys.sleep(0.5)
        }
      }

      # Combine results
      results_df <- do.call(rbind, results_list)
      rownames(results_df) <- NULL

      # Update reactive data
      rv$trait_data <- results_df[, c("species", "MS", "FS", "MB", "EP", "PR")]

      # Count successes
      n_complete <- sum(complete.cases(rv$trait_data[, c("MS", "FS", "MB", "EP", "PR")]))
      n_partial <- sum(!complete.cases(rv$trait_data[, c("MS", "FS", "MB", "EP", "PR")])) -
                   sum(is.na(rv$trait_data$MS) & is.na(rv$trait_data$FS) &
                       is.na(rv$trait_data$MB) & is.na(rv$trait_data$EP) & is.na(rv$trait_data$PR))
      n_missing <- sum(is.na(rv$trait_data$MS) & is.na(rv$trait_data$FS) &
                      is.na(rv$trait_data$MB) & is.na(rv$trait_data$EP) & is.na(rv$trait_data$PR))

      # Log summary
      append_log("\n========================================")
      append_log(paste0("SUMMARY: ", length(species_list), " species processed"))
      append_log(paste0("  ✓ Complete (5/5 traits): ", n_complete))
      append_log(paste0("  ⚠ Partial (1-4 traits): ", n_partial))
      append_log(paste0("  ✗ No data (0/5 traits): ", n_missing))
      append_log("========================================")

      # Final progress update
      progress$set(value = length(species_list), detail = "Lookup complete!")

      # Show success notification
      showNotification(
        HTML(paste0(
          "<b>Trait lookup complete!</b><br>",
          "Complete: ", n_complete, " | ",
          "Partial: ", n_partial, " | ",
          "No data: ", n_missing, "<br>",
          "You can now edit the table and construct the food web."
        )),
        type = "message",
        duration = 10
      )

    }, error = function(e) {
      progress$set(value = length(species_list), detail = "Error occurred")
      showNotification(
        paste("Error during lookup:", e$message),
        type = "error",
        duration = 10
      )
    })

  })

  # Initialize with example on startup
  observe({
    if (is.null(rv$trait_data)) {
      rv$trait_data <- get_example_dataset("simple")
    }
  })

  # ============================================================================
  # LOOKUP PROGRESS LOG OUTPUT
  # ============================================================================

  output$trait_lookup_log <- renderUI({
    log_entries <- lookup_log()

    # Show placeholder if empty
    if (length(log_entries) == 0) {
      return(tags$div(
        style = "color: #999; font-style: italic;",
        "Waiting for lookup to start..."
      ))
    }

    # Convert log entries to HTML with colored icons
    log_html <- lapply(log_entries, function(entry) {
      # Detect entry type and apply formatting
      if (grepl("^===", entry)) {
        # Species header
        tags$div(
          style = "color: #0066cc; font-weight: bold; margin-top: 10px; border-top: 1px solid #dee2e6; padding-top: 8px;",
          entry
        )
      } else if (grepl("^  →", entry)) {
        # Database query
        tags$div(style = "color: #666; margin-left: 15px;", entry)
      } else if (grepl("^    ✓", entry)) {
        # Success
        tags$div(style = "color: #28a745; margin-left: 30px;", entry)
      } else if (grepl("^    ✗", entry)) {
        # Not found
        tags$div(style = "color: #dc3545; margin-left: 30px;", entry)
      } else if (grepl("^    ⚠", entry)) {
        # Warning
        tags$div(style = "color: #ffc107; margin-left: 30px;", entry)
      } else if (grepl("^  ✓ COMPLETE", entry)) {
        # Final result
        tags$div(
          style = "color: #28a745; font-weight: bold; margin-left: 15px; margin-bottom: 10px;",
          entry
        )
      } else if (grepl("^====", entry)) {
        # Summary separator
        tags$div(
          style = "color: #0066cc; font-weight: bold; border-top: 2px solid #0066cc; margin-top: 10px; padding-top: 10px;",
          entry
        )
      } else if (grepl("^SUMMARY", entry)) {
        # Summary header
        tags$div(style = "color: #0066cc; font-weight: bold; font-size: 14px;", entry)
      } else if (grepl("^  ✓", entry)) {
        # Summary complete
        tags$div(style = "color: #28a745; margin-left: 15px;", entry)
      } else if (grepl("^  ⚠", entry)) {
        # Summary partial
        tags$div(style = "color: #ffc107; margin-left: 15px;", entry)
      } else if (grepl("^  ✗", entry)) {
        # Summary missing
        tags$div(style = "color: #dc3545; margin-left: 15px;", entry)
      } else {
        # Default
        tags$div(style = "color: #333;", entry)
      }
    })

    tags$div(log_html)
  })

  # ============================================================================
  # DATA TABLE (EDITABLE)
  # ============================================================================

  output$trait_data_table <- DT::renderDataTable({
    req(rv$trait_data)

    DT::datatable(
      rv$trait_data,
      editable = TRUE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 't'
      ),
      rownames = FALSE
    )
  })

  # Handle edits to data table
  observeEvent(input$trait_data_table_cell_edit, {
    info <- input$trait_data_table_cell_edit
    rv$trait_data[info$row, info$col] <- info$value
  })

  # ============================================================================
  # VALIDATION
  # ============================================================================

  observeEvent(input$trait_validate, {
    req(rv$trait_data)

    result <- validate_trait_data(rv$trait_data)
    rv$validation_result <- result

    if (result$valid) {
      showNotification("Trait data is valid!", type = "message", duration = 3)
    } else {
      showNotification("Validation errors found. See messages below.", type = "warning", duration = 5)
    }
  })

  output$trait_validation_messages <- renderUI({
    req(rv$validation_result)

    messages <- rv$validation_result$messages
    is_valid <- rv$validation_result$valid

    if (is_valid) {
      div(
        class = "alert alert-success",
        icon("check-circle"),
        strong(" Valid: "),
        paste(messages, collapse = "; ")
      )
    } else {
      div(
        class = "alert alert-danger",
        icon("exclamation-triangle"),
        strong(" Errors: "),
        tags$ul(
          lapply(messages, function(msg) tags$li(msg))
        )
      )
    }
  })

  # ============================================================================
  # CONSTRUCT FOOD WEB
  # ============================================================================

  observeEvent(input$trait_construct_network, {
    req(rv$trait_data)

    # Validate first
    validation <- validate_trait_data(rv$trait_data)
    if (!validation$valid) {
      showNotification("Please fix validation errors before constructing network",
                      type = "error", duration = 5)
      return()
    }

    withProgress(message = 'Constructing food web...', value = 0, {

      incProgress(0.3, detail = "Calculating interaction probabilities")

      # Get probability matrix
      rv$prob_matrix <- construct_trait_foodweb(
        rv$trait_data,
        threshold = 0,
        return_probs = TRUE
      )

      incProgress(0.3, detail = "Creating adjacency matrix")

      # Get adjacency matrix with threshold
      rv$adjacency_matrix <- construct_trait_foodweb(
        rv$trait_data,
        threshold = input$trait_threshold,
        return_probs = FALSE
      )

      incProgress(0.2, detail = "Building network graph")

      # Create igraph object
      rv$network_igraph <- trait_foodweb_to_igraph(
        rv$trait_data,
        threshold = input$trait_threshold,
        include_probs = TRUE
      )

      incProgress(0.2, detail = "Done")
    })

    showNotification("Food web constructed successfully!", type = "message", duration = 3)
  })

  # ============================================================================
  # NETWORK VISUALIZATION
  # ============================================================================

  output$trait_network_plot <- visNetwork::renderVisNetwork({
    req(rv$network_igraph)

    # Convert igraph to visNetwork format
    vis_data <- visNetwork::toVisNetworkData(rv$network_igraph)

    # Color nodes by size class
    size_colors <- c(
      MS1 = "#e6f5ff", MS2 = "#b3d9ff", MS3 = "#80bfff",
      MS4 = "#4da6ff", MS5 = "#1a8cff", MS6 = "#0073e6", MS7 = "#004d99"
    )

    if ("MS" %in% names(vis_data$nodes)) {
      vis_data$nodes$color <- size_colors[vis_data$nodes$MS]
    }

    # Add labels
    vis_data$nodes$label <- vis_data$nodes$id
    vis_data$nodes$title <- paste0(
      "<b>", vis_data$nodes$species, "</b><br>",
      "Size: ", vis_data$nodes$MS, "<br>",
      "Foraging: ", vis_data$nodes$FS, "<br>",
      "Mobility: ", vis_data$nodes$MB, "<br>",
      "Position: ", vis_data$nodes$EP, "<br>",
      "Protection: ", vis_data$nodes$PR
    )

    # Edge formatting
    if (!is.null(vis_data$edges) && nrow(vis_data$edges) > 0) {
      if ("probability" %in% names(vis_data$edges)) {
        vis_data$edges$width <- vis_data$edges$probability * 5
        vis_data$edges$title <- sprintf("Probability: %.2f", vis_data$edges$probability)
      }
      vis_data$edges$arrows <- "to"
    }

    # Create network
    visNetwork::visNetwork(vis_data$nodes, vis_data$edges) %>%
      visNetwork::visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
        nodesIdSelection = TRUE
      ) %>%
      visNetwork::visPhysics(
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(gravitationalConstant = -50)
      ) %>%
      visNetwork::visInteraction(
        navigationButtons = TRUE,
        hover = TRUE
      )
  })

  # ============================================================================
  # NETWORK STATISTICS
  # ============================================================================

  output$trait_network_stats <- renderUI({
    req(rv$network_igraph)

    g <- rv$network_igraph

    n_nodes <- igraph::vcount(g)
    n_edges <- igraph::ecount(g)
    connectance <- n_edges / (n_nodes * (n_nodes - 1))

    avg_degree <- mean(igraph::degree(g, mode = "all"))
    avg_in_degree <- mean(igraph::degree(g, mode = "in"))
    avg_out_degree <- mean(igraph::degree(g, mode = "out"))

    HTML(paste0(
      "<table class='table table-sm'>",
      "<tr><td><b>Species:</b></td><td>", n_nodes, "</td></tr>",
      "<tr><td><b>Interactions:</b></td><td>", n_edges, "</td></tr>",
      "<tr><td><b>Connectance:</b></td><td>", sprintf("%.3f", connectance), "</td></tr>",
      "<tr><td><b>Avg. degree:</b></td><td>", sprintf("%.2f", avg_degree), "</td></tr>",
      "<tr><td><b>Avg. prey:</b></td><td>", sprintf("%.2f", avg_out_degree), "</td></tr>",
      "<tr><td><b>Avg. predators:</b></td><td>", sprintf("%.2f", avg_in_degree), "</td></tr>",
      "</table>"
    ))
  })

  # ============================================================================
  # DATA STATISTICS
  # ============================================================================

  output$trait_data_stats <- renderUI({
    req(rv$trait_data)

    n_species <- nrow(rv$trait_data)

    # Count trait frequencies
    ms_counts <- table(rv$trait_data$MS)
    fs_counts <- table(rv$trait_data$FS)

    HTML(paste0(
      "<h5>Dataset Summary</h5>",
      "<table class='table table-sm'>",
      "<tr><td><b>Total species:</b></td><td>", n_species, "</td></tr>",
      "<tr><td><b>Size classes:</b></td><td>", paste(names(ms_counts), collapse = ", "), "</td></tr>",
      "<tr><td><b>Foraging strategies:</b></td><td>", paste(names(fs_counts), collapse = ", "), "</td></tr>",
      "</table>"
    ))
  })

  # ============================================================================
  # PROBABILITY HEATMAP
  # ============================================================================

  output$trait_probability_heatmap <- renderPlot({
    req(rv$prob_matrix)

    # Create heatmap using base R graphics
    # Define color palette (white -> yellow -> red)
    color_palette <- colorRampPalette(c("white", "yellow", "orange", "red"))(100)

    # Create heatmap
    par(mar = c(8, 8, 3, 2))
    image(
      x = 1:ncol(rv$prob_matrix),
      y = 1:nrow(rv$prob_matrix),
      z = t(rv$prob_matrix),
      col = color_palette,
      xlab = "",
      ylab = "",
      main = "Interaction Probability Matrix",
      axes = FALSE,
      zlim = c(0, 1)
    )

    # Add axes
    axis(1, at = 1:ncol(rv$prob_matrix), labels = colnames(rv$prob_matrix),
         las = 2, cex.axis = 0.8)
    axis(2, at = 1:nrow(rv$prob_matrix), labels = rownames(rv$prob_matrix),
         las = 1, cex.axis = 0.8)

    # Add axis labels
    mtext("Resource (Prey)", side = 1, line = 6)
    mtext("Consumer (Predator)", side = 2, line = 6)

    # Add grid
    abline(h = 0.5:(nrow(rv$prob_matrix) + 0.5), col = "gray90", lwd = 0.5)
    abline(v = 0.5:(ncol(rv$prob_matrix) + 0.5), col = "gray90", lwd = 0.5)

    # Add legend
    legend_breaks <- seq(0, 1, length.out = 5)
    legend("right", legend = sprintf("%.2f", rev(legend_breaks)),
           fill = rev(color_palette[seq(1, 100, length.out = 5)]),
           title = "P(interaction)", cex = 0.8, inset = c(-0.15, 0), xpd = TRUE)
  })

  # ============================================================================
  # TRAIT REFERENCE OUTPUTS
  # ============================================================================

  output$trait_ref_MS <- renderUI({
    traits <- get_trait_descriptions("MS")
    HTML(paste0(
      "<table class='table table-sm table-striped'>",
      "<tr><th>Code</th><th>Description</th></tr>",
      paste0("<tr><td><b>", names(traits), "</b></td><td>", traits, "</td></tr>", collapse = ""),
      "</table>"
    ))
  })

  output$trait_ref_FS <- renderUI({
    traits <- get_trait_descriptions("FS")
    HTML(paste0(
      "<table class='table table-sm table-striped'>",
      "<tr><th>Code</th><th>Description</th></tr>",
      paste0("<tr><td><b>", names(traits), "</b></td><td>", traits, "</td></tr>", collapse = ""),
      "</table>"
    ))
  })

  output$trait_ref_MB <- renderUI({
    traits <- get_trait_descriptions("MB")
    HTML(paste0(
      "<table class='table table-sm table-striped'>",
      "<tr><th>Code</th><th>Description</th></tr>",
      paste0("<tr><td><b>", names(traits), "</b></td><td>", traits, "</td></tr>", collapse = ""),
      "</table>"
    ))
  })

  output$trait_ref_EP <- renderUI({
    traits <- get_trait_descriptions("EP")
    HTML(paste0(
      "<table class='table table-sm table-striped'>",
      "<tr><th>Code</th><th>Description</th></tr>",
      paste0("<tr><td><b>", names(traits), "</b></td><td>", traits, "</td></tr>", collapse = ""),
      "</table>"
    ))
  })

  output$trait_ref_PR <- renderUI({
    traits <- get_trait_descriptions("PR")
    HTML(paste0(
      "<table class='table table-sm table-striped'>",
      "<tr><th>Code</th><th>Description</th></tr>",
      paste0("<tr><td><b>", names(traits), "</b></td><td>", traits, "</td></tr>", collapse = ""),
      "</table>"
    ))
  })

  # ============================================================================
  # PROBABILITY MATRIX REFERENCE OUTPUTS
  # ============================================================================

  output$prob_matrix_MS_MS <- renderTable({
    as.data.frame.matrix(MS_MS)
  }, rownames = TRUE, digits = 2)

  output$prob_matrix_FS_MS <- renderTable({
    as.data.frame.matrix(FS_MS)
  }, rownames = TRUE, digits = 2)

  output$prob_matrix_MB_MB <- renderTable({
    as.data.frame.matrix(MB_MB)
  }, rownames = TRUE, digits = 2)

  output$prob_matrix_EP_MS <- renderTable({
    as.data.frame.matrix(EP_MS)
  }, rownames = TRUE, digits = 2)

  output$prob_matrix_PR_MS <- renderTable({
    as.data.frame.matrix(PR_MS)
  }, rownames = TRUE, digits = 2)

  # ============================================================================
  # DOWNLOAD HANDLERS
  # ============================================================================

  output$trait_download_template <- downloadHandler(
    filename = function() {
      paste0("trait_foodweb_template_", Sys.Date(), ".csv")
    },
    content = function(file) {
      template <- create_trait_template(10)
      write.csv(template, file, row.names = FALSE)
    }
  )

  output$trait_download_adjacency <- downloadHandler(
    filename = function() {
      paste0("trait_foodweb_adjacency_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$adjacency_matrix)
      write.csv(rv$adjacency_matrix, file, row.names = TRUE)
    }
  )

  # ============================================================================
  # HELP CONTENT OUTPUTS
  # ============================================================================

  # Source help content generators
  source("R/functions/trait_help_content.R", local = TRUE)

  output$trait_help_quickstart <- renderUI({
    generate_trait_help_quickstart()
  })

  output$trait_help_dimensions <- renderUI({
    generate_trait_help_dimensions()
  })

  output$trait_help_matrices <- renderUI({
    generate_trait_help_matrices()
  })

  output$trait_help_lookup <- renderUI({
    generate_trait_help_lookup()
  })

  output$trait_help_best_practices <- renderUI({
    generate_trait_help_best_practices()
  })

  # Additional UI outputs for reorganized tabs
  output$trait_data_status <- renderUI({
    if (is.null(rv$trait_data)) {
      tags$div(
        class = "alert alert-warning",
        icon("exclamation-triangle"), " No data loaded"
      )
    } else {
      tags$div(
        class = "alert alert-success",
        icon("check-circle"), " Data loaded: ", nrow(rv$trait_data), " species"
      )
    }
  })

  output$trait_data_quick_stats <- renderUI({
    req(rv$trait_data)
    HTML(paste0(
      "<p><strong>Species:</strong> ", nrow(rv$trait_data), "</p>",
      "<p><strong>Traits Complete:</strong> ",
      sum(complete.cases(rv$trait_data[, c("MS", "FS", "MB", "EP", "PR")])),
      " / ", nrow(rv$trait_data), "</p>"
    ))
  })

  output$trait_example_description <- renderUI({
    req(input$trait_example_dataset)
    descriptions <- list(
      simple = "A simple 5-species food chain demonstrating basic trophic relationships.",
      marine = "A 10-species benthic invertebrate community with diverse feeding strategies.",
      complex = "A 20-species coastal ecosystem including plankton, invertebrates, and fish."
    )
    tags$p(class = "text-muted", descriptions[[input$trait_example_dataset]])
  })

  output$trait_distribution_plot <- renderPlot({
    req(rv$trait_data)

    # Count trait frequencies
    traits_long <- data.frame(
      Trait = rep(c("MS", "FS", "MB", "EP", "PR"), each = nrow(rv$trait_data)),
      Value = c(rv$trait_data$MS, rv$trait_data$FS, rv$trait_data$MB,
               rv$trait_data$EP, rv$trait_data$PR)
    )

    par(mar = c(4, 8, 2, 1))
    trait_table <- table(traits_long$Trait, traits_long$Value)
    barplot(trait_table, beside = TRUE, las = 1, col = rainbow(5, alpha = 0.7),
            main = "Trait Code Distribution",
            xlab = "Frequency", horiz = TRUE,
            legend.text = rownames(trait_table))
  })

}
