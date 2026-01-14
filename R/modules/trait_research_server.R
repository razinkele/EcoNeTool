# Server logic for Trait Research Module
# Handles species trait lookup and results display

trait_research_server <- function(input, output, session, shared_data) {

  # ============================================================================
  # REACTIVE VALUES
  # ============================================================================

  rv <- reactiveValues(
    trait_results = NULL,       # Harmonized trait data (data.frame)
    raw_results = NULL,         # Raw lookup results (list with details)
    lookup_in_progress = FALSE,
    last_lookup_time = NULL
  )

  # ============================================================================
  # TEST DATASETS - Central Baltic Sea Species
  # ============================================================================

  # Load the master Baltic Sea species list
  get_baltic_species_data <- function() {
    file_path <- "data/baltic_sea_test_species.csv"
    if (file.exists(file_path)) {
      read.csv(file_path, stringsAsFactors = FALSE)
    } else {
      # Fallback if file doesn't exist
      data.frame(
        species = c("Gadus morhua", "Clupea harengus", "Sprattus sprattus"),
        functional_group = c("Fish", "Fish", "Fish"),
        common_name = c("Atlantic cod", "Atlantic herring", "European sprat"),
        stringsAsFactors = FALSE
      )
    }
  }

  # Get test dataset by type
  get_test_dataset <- function(type) {
    all_species <- get_baltic_species_data()

    if (type == "baltic_full") {
      return(all_species$species)

    } else if (type == "baltic_fish") {
      fish <- all_species[all_species$functional_group == "Fish", "species"]
      return(fish)

    } else if (type == "baltic_benthos") {
      benthos <- all_species[all_species$functional_group == "Benthos", "species"]
      return(benthos)

    } else if (type == "baltic_plankton") {
      plankton <- all_species[all_species$functional_group %in% c("Phytoplankton", "Zooplankton"), "species"]
      return(plankton)

    } else if (type == "baltic_top_predators") {
      # Fish predators + Birds + Mammals
      top_pred <- all_species[all_species$functional_group %in% c("Bird", "Mammal"), "species"]
      # Add predatory fish
      pred_fish <- c("Gadus morhua", "Sander lucioperca", "Esox lucius", "Salmo salar",
                     "Salmo trutta", "Perca fluviatilis", "Anguilla anguilla")
      return(c(pred_fish, top_pred))
    }

    return(all_species$species)
  }

  # Test dataset descriptions
  output$trait_research_test_description <- renderUI({
    req(input$trait_research_test_dataset)

    descriptions <- list(
      baltic_full = tags$div(
        tags$p(class = "text-muted",
               "Complete Central Baltic Sea food web from phytoplankton to marine mammals."),
        tags$ul(class = "small",
          tags$li("8 Phytoplankton species"),
          tags$li("9 Zooplankton species"),
          tags$li("17 Benthic invertebrates"),
          tags$li("20 Fish species"),
          tags$li("12 Bird species"),
          tags$li("3 Marine mammals")
        )
      ),

      baltic_fish = tags$div(
        tags$p(class = "text-muted",
               "Baltic Sea fish community including commercial and non-commercial species."),
        tags$ul(class = "small",
          tags$li("Pelagic: herring, sprat, smelt"),
          tags$li("Demersal: cod, flounder, sculpin"),
          tags$li("Coastal: perch, pike, stickleback"),
          tags$li("Invasive: round goby")
        )
      ),

      baltic_benthos = tags$div(
        tags$p(class = "text-muted",
               "Soft-bottom and hard-bottom benthic invertebrates."),
        tags$ul(class = "small",
          tags$li("Bivalves: Macoma, Mytilus, Mya"),
          tags$li("Crustaceans: Saduria, amphipods"),
          tags$li("Polychaetes: Hediste, Marenzelleria"),
          tags$li("Gastropods and barnacles")
        )
      ),

      baltic_plankton = tags$div(
        tags$p(class = "text-muted",
               "Pelagic primary producers and consumers."),
        tags$ul(class = "small",
          tags$li("Cyanobacteria (bloom-forming)"),
          tags$li("Diatoms (spring bloom)"),
          tags$li("Dinoflagellates"),
          tags$li("Copepods and cladocerans"),
          tags$li("Jellyfish")
        )
      ),

      baltic_top_predators = tags$div(
        tags$p(class = "text-muted",
               "Apex predators including fish, birds, and mammals."),
        tags$ul(class = "small",
          tags$li("Predatory fish: cod, pike, salmon"),
          tags$li("Sea ducks: eider, scoter"),
          tags$li("Diving birds: cormorant, mergansers"),
          tags$li("Marine mammals: seals, porpoise")
        )
      )
    )

    descriptions[[input$trait_research_test_dataset]]
  })

  # Load test dataset
  observeEvent(input$trait_research_load_test, {
    req(input$trait_research_test_dataset)

    species_list <- get_test_dataset(input$trait_research_test_dataset)

    # Update text area
    updateTextAreaInput(
      session,
      "trait_research_species_list",
      value = paste(species_list, collapse = "\n")
    )

    showNotification(
      sprintf("Loaded %d species from %s test dataset",
              length(species_list),
              input$trait_research_test_dataset),
      type = "message"
    )
  })

  # ============================================================================
  # FILE UPLOAD HANDLING
  # ============================================================================

  observeEvent(input$trait_research_species_file, {
    req(input$trait_research_species_file)

    tryCatch({
      file_ext <- tools::file_ext(input$trait_research_species_file$name)

      if (file_ext == "csv") {
        df <- read.csv(input$trait_research_species_file$datapath, stringsAsFactors = FALSE)
        # Look for species column
        if ("species" %in% tolower(names(df))) {
          col_name <- names(df)[tolower(names(df)) == "species"][1]
          species_list <- trimws(df[[col_name]])
        } else {
          # Use first column
          species_list <- trimws(df[[1]])
        }
      } else {
        # Plain text file - one species per line
        species_list <- readLines(input$trait_research_species_file$datapath)
      }

      # Update text area with loaded species
      species_list <- species_list[species_list != ""]
      updateTextAreaInput(
        session,
        "trait_research_species_list",
        value = paste(species_list, collapse = "\n")
      )

      showNotification(
        paste("Loaded", length(species_list), "species from file"),
        type = "message"
      )

    }, error = function(e) {
      showNotification(
        paste("Error reading file:", e$message),
        type = "error"
      )
    })
  })

  # ============================================================================
  # AUTOMATED TRAIT LOOKUP
  # ============================================================================

  observeEvent(input$trait_research_run_lookup, {
    req(input$trait_research_species_list)

    # Parse species list
    species_list <- strsplit(input$trait_research_species_list, "\n")[[1]]
    species_list <- trimws(species_list)
    species_list <- species_list[species_list != ""]

    if (length(species_list) == 0) {
      showNotification("Please enter at least one species name", type = "warning")
      return()
    }

    rv$lookup_in_progress <- TRUE

    # Database settings
    databases_to_check <- input$trait_research_databases
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

    # File paths for local databases
    biotic_file <- if ("biotic" %in% databases_to_check) file.path("data", "biotic_traits.csv") else NULL
    maredat_file <- if ("maredat" %in% databases_to_check) file.path("data", "maredat_zooplankton.csv") else NULL
    ptdb_file <- if ("ptdb" %in% databases_to_check) file.path("data", "ptdb_phytoplankton.csv") else NULL

    # Cache directory
    cache_dir <- file.path("cache", "taxonomy")
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE)
    }

    # Progress modal
    progress <- Progress$new(min = 0, max = length(species_list))
    on.exit(progress$close())

    progress$set(
      message = "Trait Research",
      value = 0,
      detail = sprintf("Looking up %d species in %s",
                       length(species_list),
                       paste(sapply(databases_to_check, function(x) db_names[x]), collapse = ", "))
    )

    # Console output
    cat("\n========================================\n")
    cat("TRAIT RESEARCH - AUTOMATED LOOKUP\n")
    cat("========================================\n")
    cat("Species:", length(species_list), "\n")
    cat("Databases:", paste(sapply(databases_to_check, function(x) db_names[x]), collapse = ", "), "\n")
    cat("========================================\n\n")

    # Run lookup
    tryCatch({
      results_list <- list()
      raw_list <- list()

      for (i in seq_along(species_list)) {
        species <- species_list[i]
        percent <- round((i / length(species_list)) * 100)

        progress$set(
          value = i,
          detail = sprintf("[%d/%d] %s (%d%%)", i, length(species_list), species, percent)
        )

        cat(sprintf("[%d/%d] %s\n", i, length(species_list), species))

        # Check cache
        cache_file <- file.path(cache_dir, paste0(gsub(" ", "_", species), ".rds"))
        if (file.exists(cache_file)) {
          cached <- readRDS(cache_file)
          cache_age_days <- as.numeric(difftime(Sys.time(), cached$timestamp, units = "days"))
          if (cache_age_days < 30) {
            cat("  -> Using cached data\n")
            results_list[[i]] <- cached$traits
            raw_list[[species]] <- cached$raw_data
            next
          }
        }

        # Initialize raw data collector
        raw_data <- list(
          species = species,
          worms = NULL,
          fishbase = NULL,
          sealifebase = NULL,
          biotic = NULL,
          freshwater = NULL,
          maredat = NULL,
          ptdb = NULL,
          algaebase = NULL,
          shark = NULL
        )

        # Query WoRMS
        if ("worms" %in% databases_to_check) {
          cat("  -> Querying WoRMS...")
          worms_result <- lookup_worms_traits(species)
          if (worms_result$success) {
            raw_data$worms <- worms_result$traits
            cat(" found\n")
          } else {
            cat(" not found\n")
          }
        }

        # Query FishBase
        if ("fishbase" %in% databases_to_check) {
          cat("  -> Querying FishBase...")
          fb_result <- lookup_fishbase_traits(species)
          if (fb_result$success) {
            raw_data$fishbase <- fb_result$traits
            cat(" found\n")
          } else {
            cat(" not found\n")
          }
        }

        # Query SeaLifeBase
        if ("sealifebase" %in% databases_to_check) {
          cat("  -> Querying SeaLifeBase...")
          slb_result <- lookup_sealifebase_traits(species)
          if (slb_result$success) {
            raw_data$sealifebase <- slb_result$traits
            cat(" found\n")
          } else {
            cat(" not found\n")
          }
        }

        # Query BIOTIC
        if ("biotic" %in% databases_to_check && !is.null(biotic_file) && file.exists(biotic_file)) {
          cat("  -> Querying BIOTIC...")
          biotic_result <- lookup_biotic_traits(species, biotic_file)
          if (biotic_result$success) {
            raw_data$biotic <- biotic_result$traits
            cat(" found\n")
          } else {
            cat(" not found\n")
          }
        }

        # Query freshwaterecology.info
        if ("freshwater" %in% databases_to_check) {
          cat("  -> Querying freshwaterecology.info...")
          fw_result <- lookup_freshwaterecology_traits(species)
          if (fw_result$success) {
            raw_data$freshwater <- fw_result$traits
            cat(" found\n")
          } else {
            cat(" not found\n")
          }
        }

        # Query MAREDAT
        if ("maredat" %in% databases_to_check && !is.null(maredat_file) && file.exists(maredat_file)) {
          cat("  -> Querying MAREDAT...")
          maredat_result <- lookup_maredat_traits(species, maredat_file)
          if (maredat_result$success) {
            raw_data$maredat <- maredat_result$traits
            cat(" found\n")
          } else {
            cat(" not found\n")
          }
        }

        # Query PTDB
        if ("ptdb" %in% databases_to_check && !is.null(ptdb_file) && file.exists(ptdb_file)) {
          cat("  -> Querying PTDB...")
          ptdb_result <- lookup_ptdb_traits(species, ptdb_file)
          if (ptdb_result$success) {
            raw_data$ptdb <- ptdb_result$traits
            cat(" found\n")
          } else {
            cat(" not found\n")
          }
        }

        # Query AlgaeBase
        if ("algaebase" %in% databases_to_check) {
          cat("  -> Querying AlgaeBase...")
          ab_result <- lookup_algaebase_traits(species)
          if (ab_result$success) {
            raw_data$algaebase <- ab_result$traits
            cat(" found\n")
          } else {
            cat(" not found\n")
          }
        }

        # Query SHARK
        if ("shark" %in% databases_to_check) {
          cat("  -> Querying SHARK...")
          shark_result <- lookup_shark_traits(species)
          if (shark_result$success) {
            raw_data$shark <- shark_result$traits
            cat(" found\n")
          } else {
            cat(" not found\n")
          }
        }

        # Run full harmonized lookup
        full_result <- lookup_species_traits(
          species,
          biotic_file = biotic_file,
          maredat_file = maredat_file,
          ptdb_file = ptdb_file,
          cache_dir = cache_dir
        )

        results_list[[i]] <- full_result
        raw_list[[species]] <- raw_data

        # Cache the raw data too
        cache_data <- list(
          traits = full_result,
          raw_data = raw_data,
          timestamp = Sys.time()
        )
        saveRDS(cache_data, cache_file)

        # Rate limiting
        if (i < length(species_list)) {
          Sys.sleep(0.3)
        }
      }

      # Combine results
      results_df <- do.call(rbind, results_list)
      rownames(results_df) <- NULL

      # Store results
      rv$trait_results <- results_df
      rv$raw_results <- raw_list
      rv$last_lookup_time <- Sys.time()
      rv$lookup_in_progress <- FALSE

      # Update species selector for raw details
      updateSelectInput(
        session,
        "trait_research_raw_species",
        choices = names(raw_list)
      )

      # Summary stats
      n_complete <- sum(complete.cases(results_df[, c("MS", "FS", "MB", "EP", "PR")]))
      n_partial <- nrow(results_df) - n_complete -
        sum(is.na(results_df$MS) & is.na(results_df$FS) &
            is.na(results_df$MB) & is.na(results_df$EP) & is.na(results_df$PR))
      n_missing <- sum(is.na(results_df$MS) & is.na(results_df$FS) &
                       is.na(results_df$MB) & is.na(results_df$EP) & is.na(results_df$PR))

      # Console summary
      cat("\n========================================\n")
      cat("LOOKUP COMPLETE\n")
      cat(sprintf("Complete: %d | Partial: %d | No data: %d\n", n_complete, n_partial, n_missing))
      cat("========================================\n\n")

      showNotification(
        HTML(sprintf("<b>Trait lookup complete!</b><br>Complete: %d | Partial: %d | No data: %d",
                     n_complete, n_partial, n_missing)),
        type = "message",
        duration = 8
      )

    }, error = function(e) {
      rv$lookup_in_progress <- FALSE
      showNotification(
        paste("Error during lookup:", e$message),
        type = "error"
      )
    })
  })

  # ============================================================================
  # CLEAR RESULTS
  # ============================================================================

  observeEvent(input$trait_research_clear, {
    rv$trait_results <- NULL
    rv$raw_results <- NULL
    rv$last_lookup_time <- NULL

    showNotification("Results cleared", type = "message")
  })

  # ============================================================================
  # USE IN FOOD WEB - Transfer data to shared reactive
  # ============================================================================

  observeEvent(input$trait_research_use_in_foodweb, {
    req(rv$trait_results)

    # Transfer to shared data for Food Web Construction module
    shared_data$trait_data <- rv$trait_results[, c("species", "MS", "FS", "MB", "EP", "PR")]

    showNotification(
      HTML(sprintf("<b>%d species transferred to Food Web Construction!</b><br>Navigate to 'Food Web Construction' in the sidebar to continue.",
                   nrow(rv$trait_results))),
      type = "message",
      duration = 5
    )

    # Note: User should navigate manually to Food Web Construction
    # (Cross-module tab switching requires parent session access)
  })

  # ============================================================================
  # OUTPUT: STATUS
  # ============================================================================

  output$trait_research_status <- renderUI({
    if (rv$lookup_in_progress) {
      tags$div(
        class = "alert alert-info",
        icon("spinner", class = "fa-spin"),
        " Lookup in progress..."
      )
    } else if (is.null(rv$trait_results)) {
      tags$div(
        class = "alert alert-secondary",
        icon("info-circle"),
        " No lookup performed yet. Enter species and click 'Run Trait Lookup'."
      )
    } else {
      tags$div(
        class = "alert alert-success",
        icon("check-circle"),
        sprintf(" Lookup complete: %d species processed", nrow(rv$trait_results)),
        if (!is.null(rv$last_lookup_time)) {
          tags$small(
            class = "text-muted",
            sprintf(" (%s)", format(rv$last_lookup_time, "%H:%M:%S"))
          )
        }
      )
    }
  })

  # ============================================================================
  # OUTPUT: SUMMARY
  # ============================================================================

  output$trait_research_summary <- renderUI({
    req(rv$trait_results)

    df <- rv$trait_results

    n_total <- nrow(df)
    n_complete <- sum(complete.cases(df[, c("MS", "FS", "MB", "EP", "PR")]))
    n_partial <- n_total - n_complete -
      sum(is.na(df$MS) & is.na(df$FS) & is.na(df$MB) & is.na(df$EP) & is.na(df$PR))
    n_missing <- sum(is.na(df$MS) & is.na(df$FS) & is.na(df$MB) & is.na(df$EP) & is.na(df$PR))

    pct_complete <- round(n_complete / n_total * 100)

    tags$div(
      tags$p(
        tags$span(class = "badge badge-success", sprintf("%d Complete", n_complete)),
        " ",
        tags$span(class = "badge badge-warning", sprintf("%d Partial", n_partial)),
        " ",
        tags$span(class = "badge badge-danger", sprintf("%d No data", n_missing))
      ),
      tags$div(
        class = "progress",
        style = "height: 20px;",
        tags$div(
          class = "progress-bar bg-success",
          style = sprintf("width: %d%%;", pct_complete),
          sprintf("%d%%", pct_complete)
        )
      )
    )
  })

  # ============================================================================
  # OUTPUT: TRAIT TABLE
  # ============================================================================

  output$trait_research_table <- DT::renderDataTable({
    req(rv$trait_results)

    # Select columns to display
    display_cols <- c("species", "MS", "FS", "MB", "EP", "PR", "source", "confidence")
    display_df <- rv$trait_results[, display_cols[display_cols %in% names(rv$trait_results)]]

    DT::datatable(
      display_df,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      class = 'stripe hover compact'
    ) %>%
      DT::formatStyle(
        columns = c("MS", "FS", "MB", "EP", "PR"),
        backgroundColor = DT::styleEqual(
          c(NA, ""),
          c("#ffebee", "#ffebee")
        )
      )
  })

  # ============================================================================
  # OUTPUT: RAW DETAILS
  # ============================================================================

  output$trait_research_raw_details <- renderUI({
    req(input$trait_research_raw_species)
    req(rv$raw_results)

    species <- input$trait_research_raw_species
    raw_data <- rv$raw_results[[species]]

    if (is.null(raw_data)) {
      return(tags$p(class = "text-muted", "No raw data available for this species."))
    }

    # Build detail panels for each database
    detail_panels <- lapply(names(raw_data), function(db) {
      if (db == "species") return(NULL)

      db_data <- raw_data[[db]]
      if (is.null(db_data)) {
        return(tags$div(
          class = "card mb-2",
          tags$div(
            class = "card-header py-1",
            tags$span(class = "badge badge-secondary", toupper(db)),
            " No data"
          )
        ))
      }

      # Format the data as a table
      if (is.data.frame(db_data) || is.list(db_data)) {
        data_df <- as.data.frame(t(unlist(db_data)), stringsAsFactors = FALSE)
        colnames(data_df) <- names(unlist(db_data))

        tags$div(
          class = "card mb-2",
          tags$div(
            class = "card-header py-1 bg-success text-white",
            tags$span(class = "badge badge-light", toupper(db)),
            " Found"
          ),
          tags$div(
            class = "card-body py-2",
            style = "font-size: 12px; overflow-x: auto;",
            tags$table(
              class = "table table-sm table-bordered mb-0",
              tags$tbody(
                lapply(names(data_df), function(col) {
                  tags$tr(
                    tags$td(tags$strong(col)),
                    tags$td(as.character(data_df[[col]]))
                  )
                })
              )
            )
          )
        )
      }
    })

    tags$div(detail_panels)
  })

  # ============================================================================
  # DOWNLOADS
  # ============================================================================

  output$trait_research_download_csv <- downloadHandler(
    filename = function() {
      paste0("trait_research_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(rv$trait_results)
      write.csv(rv$trait_results, file, row.names = FALSE)
    }
  )

  output$trait_research_download_rds <- downloadHandler(
    filename = function() {
      paste0("trait_research_", format(Sys.Date(), "%Y%m%d"), ".rds")
    },
    content = function(file) {
      req(rv$trait_results)
      saveRDS(rv$trait_results, file)
    }
  )

  # Excel download handler
  output$trait_research_download_excel <- downloadHandler(
    filename = function() {
      paste0("trait_research_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      req(rv$trait_results)
      if (requireNamespace("writexl", quietly = TRUE)) {
        writexl::write_xlsx(rv$trait_results, file)
      } else {
        # Fallback to CSV if writexl not available
        write.csv(rv$trait_results, file, row.names = FALSE)
      }
    }
  )

  # ============================================================================
  # OUTPUT: SPECIES LIST DISPLAY (Tab 1)
  # ============================================================================

  # Get current species list from input
  get_species_list <- reactive({
    if (input$trait_research_input_method == "manual") {
      species_text <- input$trait_research_species_list
      if (is.null(species_text) || species_text == "") return(character(0))
      species <- strsplit(species_text, "\n")[[1]]
      species <- trimws(species)
      species <- species[species != ""]
      return(species)
    } else if (input$trait_research_input_method == "file") {
      req(input$trait_research_species_file)
      # File handling done elsewhere
      return(character(0))
    } else if (input$trait_research_input_method == "test") {
      return(get_test_dataset(input$trait_research_test_dataset))
    }
    return(character(0))
  })

  output$trait_research_species_display <- renderUI({
    species <- get_species_list()

    if (length(species) == 0) {
      return(tags$p(class = "text-muted", "No species entered yet."))
    }

    # Create a formatted list with status icons
    species_items <- lapply(seq_along(species), function(i) {
      sp <- species[i]

      # Check if species has results
      status_icon <- if (!is.null(rv$trait_results) && sp %in% rv$trait_results$species) {
        icon("check-circle", class = "text-success")
      } else if (rv$lookup_in_progress) {
        icon("hourglass-half", class = "text-warning")
      } else {
        icon("circle", class = "text-muted")
      }

      tags$div(
        style = "padding: 2px 0;",
        status_icon, " ",
        tags$span(sp)
      )
    })

    do.call(tagList, species_items)
  })

  output$trait_research_species_count <- renderText({
    species <- get_species_list()
    paste(length(species), "species")
  })

  # ============================================================================
  # OUTPUT: PROGRESS SUMMARY (Tab 1)
  # ============================================================================

  output$trait_research_progress_summary <- renderUI({
    if (is.null(rv$trait_results)) {
      return(tags$p(class = "text-muted", "Run lookup to see progress summary."))
    }

    species <- get_species_list()
    n_input <- length(species)
    n_found <- nrow(rv$trait_results)

    tags$div(
      tags$p(
        icon("check"), " ",
        tags$strong(n_found), " of ", tags$strong(n_input), " species processed"
      ),
      tags$div(
        class = "progress",
        style = "height: 10px;",
        tags$div(
          class = "progress-bar bg-success",
          style = sprintf("width: %d%%;", round(n_found / max(n_input, 1) * 100))
        )
      )
    )
  })

  output$trait_research_progress_log <- renderUI({
    if (is.null(rv$raw_results)) {
      return(tags$p(class = "text-muted", "No lookup performed yet."))
    }

    # Show progress log entries
    tags$div(
      class = "small",
      style = "font-family: monospace;",
      tags$p(icon("check-circle", class = "text-success"), " Lookup completed"),
      tags$p(
        sprintf("Processed %d species from %d databases",
                nrow(rv$trait_results),
                length(unique(rv$trait_results$source)))
      )
    )
  })

  # ============================================================================
  # OUTPUT: TRAIT SUMMARY STATISTICS (Tab 2)
  # ============================================================================

  output$trait_summary_species <- renderText({
    if (is.null(rv$trait_results)) return("0")
    as.character(nrow(rv$trait_results))
  })

  output$trait_summary_complete <- renderText({
    if (is.null(rv$trait_results)) return("0")
    df <- rv$trait_results
    n <- sum(complete.cases(df[, c("MS", "FS", "MB", "EP", "PR")]))
    as.character(n)
  })

  output$trait_summary_partial <- renderText({
    if (is.null(rv$trait_results)) return("0")
    df <- rv$trait_results
    n_complete <- sum(complete.cases(df[, c("MS", "FS", "MB", "EP", "PR")]))
    n_missing <- sum(is.na(df$MS) & is.na(df$FS) & is.na(df$MB) & is.na(df$EP) & is.na(df$PR))
    n_partial <- nrow(df) - n_complete - n_missing
    as.character(max(0, n_partial))
  })

  output$trait_summary_notfound <- renderText({
    if (is.null(rv$trait_results)) return("0")
    df <- rv$trait_results
    n <- sum(is.na(df$MS) & is.na(df$FS) & is.na(df$MB) & is.na(df$EP) & is.na(df$PR))
    as.character(n)
  })

  # ============================================================================
  # RETURN MODULE DATA (for parent access)
  # ============================================================================

  return(
    list(
      trait_results = reactive({ rv$trait_results }),
      raw_results = reactive({ rv$raw_results })
    )
  )
}
