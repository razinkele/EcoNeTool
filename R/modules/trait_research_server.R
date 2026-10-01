# Server logic for Trait Research Module
# Handles species trait lookup and results display

#' Extract a usable WoRMS AphiaID for a species from a trait-results frame
#'
#' Column-first AphiaID resolution for the DATRAS panel. Returns the
#' `aphia_id` for `sp` from the results data.frame when the column exists and
#' the value is a positive numeric; otherwise NA_real_ (the caller then falls
#' back to a live WoRMS name->id lookup). Pure / no side effects, so it is
#' unit-testable without a Shiny session. No suppressWarnings: a coercion
#' warning on a non-numeric aphia_id is a useful data-quality signal.
#'
#' @param df Trait-results data.frame (rv$trait_results), or NULL.
#' @param sp Species name to match against df$species.
#' @return Positive numeric AphiaID, or NA_real_.
#' @keywords internal
.datras_aphia_for_species <- function(df, sp) {
  if (is.null(df) || !"aphia_id" %in% names(df)) return(NA_real_)
  row <- df[df$species == sp, , drop = FALSE]
  if (nrow(row) == 0) return(NA_real_)
  aid <- as.numeric(row$aphia_id[1])
  if (length(aid) == 1 && !is.na(aid) && aid > 0) aid else NA_real_
}

#' Which rows of a trait-results frame are failed lookups
#'
#' lookup_species_traits_safely() marks a failed lookup with a non-NA
#' `error` column and every trait code NA; a frame without an `error`
#' column at all (e.g. before any lookup ran) has no failed rows.
#'
#' @param df Trait-results data.frame (rv$trait_results).
#' @return Logical vector, length `nrow(df)`.
#' @keywords internal
.trait_research_error_mask <- function(df) {
  if (is.null(df) || !"error" %in% names(df)) return(rep(FALSE, NROW(df)))
  !is.na(df$error)
}

#' Complete / partial / no-data / failed counts for the lookup summary
#'
#' A failed row (see `.trait_research_error_mask()`) has every trait code
#' NA, so counting it under both "No data" and "Failed" double-counted it.
#' Failed rows are excluded from "No data" here; they were already excluded
#' from "Partial" (an all-NA row was never partial).
#'
#' @param results_df Trait-results data.frame.
#' @return `list(n_complete, n_partial, n_missing, n_failed)`.
#' @keywords internal
trait_research_summary_counts <- function(results_df) {
  trait_cols <- c("MS", "FS", "MB", "EP", "PR")
  has_error <- .trait_research_error_mask(results_df)
  all_na <- Reduce(`&`, lapply(trait_cols, function(cn) is.na(results_df[[cn]])))
  n_complete <- sum(complete.cases(results_df[, trait_cols, drop = FALSE]))
  n_failed <- sum(has_error)
  n_missing <- sum(all_na & !has_error)
  n_partial <- nrow(results_df) - n_complete - sum(all_na)
  list(n_complete = n_complete, n_partial = n_partial, n_missing = n_missing, n_failed = n_failed)
}

#' Split a trait-results frame into successful rows to transfer, and a count
#'
#' A failed lookup row (see `.trait_research_error_mask()`) carries no trait
#' data and must not be sent to Food Web Construction.
#'
#' @param results_df Trait-results data.frame.
#' @return `list(data, n_transferred, n_skipped)`. `data` keeps only the
#'   successful rows (all columns, unfiltered).
#' @keywords internal
trait_research_successful_rows <- function(results_df) {
  has_error <- .trait_research_error_mask(results_df)
  list(
    data = results_df[!has_error, , drop = FALSE],
    n_transferred = sum(!has_error),
    n_skipped = sum(has_error)
  )
}

# =============================================================================
# TRAIT TABLE RENDERING HELPERS
# =============================================================================
# At file scope, not nested in the server function, so they can be unit tested.
# They close over nothing.

#' Badge colour for a trait provenance label
#'
#' Always returns a hex colour from a fixed palette. An unrecognised label -
#' including anything attacker-supplied - falls back to grey, so the value
#' interpolated into the style attribute is never caller-controlled.
#'
#' @param source Provenance label.
#' @return Character hex colour.
#' @export
source_badge_color <- function(source) {
  if (is.na(source) || source == "") return("#9e9e9e")  # gray
  colors <- c(
    "FishBase" = "#1565c0", "SeaLifeBase" = "#0277bd", "WoRMS" = "#00695c",
    "WoRMS_Traits" = "#00695c", "BIOTIC" = "#2e7d32", "PTDB" = "#558b2f",
    "MAREDAT" = "#33691e", "AlgaeBase" = "#827717", "BVOL" = "#9e9d24",
    "SpeciesEnriched" = "#f57f17", "Ontology" = "#e65100",
    "BlackSea" = "#4a148c", "ArcticTraits" = "#1a237e", "Cefas" = "#006064",
    "CoralTraits" = "#880e4f", "PelagicTraits" = "#311b92",
    "PolyTraits" = "#1b5e20", "EMODnet" = "#0d47a1", "OBIS" = "#01579b",
    "OfflineDB" = "#37474f", "ML" = "#ff6f00",
    "Depth-based" = "#5d4037", "Taxonomy" = "#455a64",
    "Harmonized" = "#616161"
  )
  col <- colors[source]
  if (is.na(col)) "#9e9e9e" else col
}

#' Format a trait value with its provenance badge
#'
#' The trait columns are rendered unescaped so the badge markup survives, so
#' every value interpolated here must be escaped at the point of assembly -
#' trait values and source labels can both originate in an uploaded CSV.
#'
#' @param value Harmonised trait code.
#' @param source Provenance label.
#' @return HTML string.
#' @export
format_trait_badge <- function(value, source) {
  if (is.na(value) || value == "") {
    return('<span style="color:#bdbdbd">-</span>')
  }
  color <- source_badge_color(source)
  safe_value <- htmltools::htmlEscape(value)
  badge <- if (!is.na(source) && source != "") {
    sprintf(
      paste0('<span style="background:%s;color:white;padding:1px 4px;',
             'border-radius:3px;font-size:9px;margin-left:3px">%s</span>'),
      color, htmltools::htmlEscape(source)
    )
  } else ""
  paste0("<strong>", safe_value, "</strong>", badge)
}

#' Column indices that must be HTML-escaped in the trait results table
#'
#' Only the badge columns carry markup we generate; everything else may hold
#' user-supplied text and must be escaped. Returns indices suitable for DT's
#' `escape` argument with `rownames = FALSE`.
#'
#' @param display_df The frame passed to DT::datatable().
#' @param badge_cols Names of the columns rendered as badge HTML.
#' @return Integer vector of column indices to escape.
#' @export
badge_escape_columns <- function(display_df, badge_cols) {
  which(!names(display_df) %in% badge_cols)
}

trait_research_server <- function(input, output, session, shared_data) {

  # ============================================================================
  # REACTIVE VALUES
  # ============================================================================

  rv <- reactiveValues(
    trait_results = NULL,       # Harmonized trait data (data.frame)
    raw_results = NULL,         # Raw lookup results (list with details)
    lookup_in_progress = FALSE,
    last_lookup_time = NULL,
    datras_result = NULL,       # Last DATRAS abundance fetch (structured list)
    datras_fetching = FALSE     # In-flight guard for the DATRAS fetch button
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
    # Read from the same source as the species count badge so the lookup count
    # always matches what the UI advertises (test mode previously fell through
    # to the textarea, which still held the manual-mode default of 5 species).
    species_list <- get_species_list()

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
      # This session's harmonization settings key the shared trait cache (F72),
      # and envelopes from another trait vocabulary are misses (C-5).
      cfg_hash <- harm_config_hash()
      vocab_ver <- current_trait_vocab_version()

      for (i in seq_along(species_list)) {
        species <- species_list[i]
        percent <- round((i / length(species_list)) * 100)

        progress$set(
          value = i,
          detail = sprintf("[%d/%d] %s (%d%%)", i, length(species_list), species, percent)
        )

        cat(sprintf("[%d/%d] %s\n", i, length(species_list), species))

        # Check cache. read_cache_field() applies the 30-day TTL, the shape
        # guard (a classify_species_api {data,...} envelope collides on the
        # same filename - deep-analysis #4) and the config-hash match (F72).
        cache_file <- file.path(cache_dir, paste0(gsub(" ", "_", species), ".rds"))
        cached_traits <- read_cache_field(cache_file, "traits", config_hash = cfg_hash,
                                          vocab_version = vocab_ver)
        if (!is.null(cached_traits)) {
          cat("  -> Using cached data\n")
          results_list[[i]] <- cached_traits
          # raw_data is only present in legacy caches written by the old
          # re-save here; orchestrator-written caches don't carry it.
          # Synthesise a minimal raw_data summary from the traits row so the
          # raw-details UI still has something to render either way.
          cached_raw <- read_cache_field(cache_file, "raw_data", config_hash = cfg_hash)
          raw_list[[species]] <- if (!is.null(cached_raw)) {
            cached_raw
          } else {
            list(species = species,
                 source = if (is.data.frame(cached_traits)) cached_traits$source else NA)
          }
          next
        }

        # Run harmonized lookup via orchestrator (handles smart routing,
        # early exit, and all database queries in one pass)
        # Previously this code queried each database individually THEN
        # called lookup_species_traits again — doubling API calls.
        # A species whose lookup throws becomes a warning and an error row;
        # the rest of the batch continues (F33). The tryCatch around this
        # loop is only for setup failures.
        full_result <- lookup_species_traits_safely(
          species,
          biotic_file = biotic_file,
          maredat_file = maredat_file,
          ptdb_file = ptdb_file,
          cache_dir = cache_dir
        )

        results_list[[i]] <- full_result
        # Build raw_data summary from the orchestrator result
        raw_data <- list(species = species, source = full_result$source)
        if (!is.null(full_result$error)) raw_data$error <- full_result$error
        raw_list[[species]] <- raw_data

        # NOTE: do NOT re-saveRDS() the cache file here. lookup_species_traits()
        # already writes a richer cache structure on its way out
        # (R/functions/trait_lookup/orchestrator.R near `saveRDS(cache_data, …)`)
        # which includes $harmonized — the taxonomy block (phylum/class/order/
        # family/genus) that phylogenetic_imputation.R::find_closest_relatives
        # needs to compute taxonomic distance. Overwriting that file with a
        # smaller {traits, raw_data, timestamp} structure dropped the taxonomy
        # and made phylo imputation fall through with "no close relatives" for
        # every species. Trust the orchestrator's cache.

        # Rate limiting handled by orchestrator
      }

      # Combine results. Per-species result data.frames have heterogeneous
      # columns: the orchestrator only adds *_interval_lower / *_interval_upper
      # / *_confidence_category for traits that actually have data. Fish (all 5
      # traits) get all 15 interval columns; birds with only MS/PR get 6. So
      # `do.call(rbind, …)` fails on a mixed batch — use bind_rows() which fills
      # missing columns with NA.
      results_df <- as.data.frame(dplyr::bind_rows(results_list))
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

      # Update the DATRAS panel selector from the harmonised species column
      # (matches the df$species == sp filter the AphiaID reactive uses).
      updateSelectInput(
        session,
        "datras_species",
        choices = results_df$species
      )

      # Summary stats (a failed row is excluded from "No data" - see
      # trait_research_summary_counts())
      counts <- trait_research_summary_counts(results_df)
      n_complete <- counts$n_complete
      n_partial <- counts$n_partial
      n_missing <- counts$n_missing
      n_failed <- counts$n_failed

      # Console summary
      cat("\n========================================\n")
      cat("LOOKUP COMPLETE\n")
      cat(sprintf("Complete: %d | Partial: %d | No data: %d | Failed: %d\n",
                  n_complete, n_partial, n_missing, n_failed))
      cat("========================================\n\n")

      showNotification(
        HTML(sprintf("<b>Trait lookup complete!</b><br>Complete: %d | Partial: %d | No data: %d | Failed: %d",
                     n_complete, n_partial, n_missing, n_failed)),
        type = if (n_failed > 0) "warning" else "message",
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

    # Reset the DATRAS panel too, so a cleared session shows no stale table.
    rv$datras_result <- NULL
    updateSelectInput(session, "datras_species", choices = character(0))

    showNotification("Results cleared", type = "message")
  })

  # ============================================================================
  # USE IN FOOD WEB - Transfer data to shared reactive
  # ============================================================================

  observeEvent(input$trait_research_use_in_foodweb, {
    req(rv$trait_results)

    # Failed lookup rows (lookup_species_traits_safely()'s error rows) carry
    # no trait data and must not be transferred (CodeRabbit #2).
    transfer <- trait_research_successful_rows(rv$trait_results)

    if (transfer$n_transferred == 0) {
      showNotification(
        "No species transferred - every trait lookup failed.",
        type = "warning",
        duration = 8
      )
      return()
    }

    # Transfer to shared data for Food Web Construction module
    shared_data$trait_data <- transfer$data[, c("species", "MS", "FS", "MB", "EP", "PR")]

    skipped_note <- if (transfer$n_skipped > 0) {
      sprintf("<br>%d skipped (failed lookup)", transfer$n_skipped)
    } else {
      ""
    }
    showNotification(
      HTML(sprintf("<b>%d species transferred to Food Web Construction!</b>%s<br>Navigate to 'Food Web Construction' in the sidebar to continue.",
                   transfer$n_transferred, skipped_note)),
      type = "message",
      duration = 5
    )

    # Note: User should navigate manually to Food Web Construction
    # (Cross-module tab switching requires parent session access)
  })

  # ============================================================================
  # DATRAS ABUNDANCE INDICES (on-demand drill-down on one species)
  # ============================================================================

  # Column-first AphiaID for the selected species (pure read). The WoRMS
  # name->id fallback for the missing case lives in the fetch observer
  # because it does network I/O.
  datras_aphia_id <- reactive({
    req(input$datras_species)
    .datras_aphia_for_species(rv$trait_results, input$datras_species)
  })

  observeEvent(input$datras_fetch, {
    sp <- input$datras_species
    if (is.null(sp) || sp == "") {
      showNotification("Select a species first (run a trait lookup).",
                       type = "warning")
      return()
    }
    if (isTRUE(rv$datras_fetching)) {
      showNotification("DATRAS fetch already in progress...", type = "warning")
      return()
    }
    rv$datras_fetching <- TRUE
    on.exit(rv$datras_fetching <- FALSE, add = TRUE)
    rv$datras_result <- NULL

    progress <- Progress$new()
    on.exit(progress$close(), add = TRUE)
    progress$set(message = "Fetching DATRAS indices", detail = sp, value = 0.3)

    aid <- datras_aphia_id()
    if (is.na(aid)) {
      # Fallback: resolve the name -> AphiaID live via WoRMS.
      aid <- tryCatch(
        as.numeric(worrms::wm_name2id(sp)),
        error = function(e) {
          warning(sprintf("[datras] wm_name2id failed for '%s': %s",
                          sp, conditionMessage(e)), call. = FALSE)
          NA_real_
        }
      )
    }
    if (length(aid) != 1 || is.na(aid) || aid <= 0) {
      showNotification(sprintf("No WoRMS AphiaID available for '%s'.", sp),
                       type = "warning")
      return()
    }

    progress$set(detail = sprintf("AphiaID %d", as.integer(aid)), value = 0.6)
    res <- lookup_datras_indices(aid)
    rv$datras_result <- res

    if (!isTRUE(res$success)) {
      # Distinguish a legitimate "no data" (informational) from an API fault.
      is_no_data <- grepl("^no DATRAS rows", as.character(res$error))
      showNotification(
        paste("DATRAS:", res$error),
        type = if (is_no_data) "message" else "warning"
      )
    }
  })

  output$datras_status <- renderUI({
    res <- rv$datras_result
    if (is.null(res)) {
      return(tags$span(class = "text-muted",
                       icon("info-circle"), " No DATRAS data fetched yet."))
    }
    if (isTRUE(res$success)) {
      n_surveys <- length(unique(res$data$survey))
      tags$span(
        class = "text-success",
        icon("check-circle"),
        sprintf(" %d row(s) across %d survey(s)", nrow(res$data), n_surveys)
      )
    } else {
      tags$span(class = "text-warning",
                icon("triangle-exclamation"), " ", as.character(res$error))
    }
  })

  output$datras_table <- DT::renderDataTable({
    req(rv$datras_result)
    req(isTRUE(rv$datras_result$success))
    DT::datatable(
      rv$datras_result$data,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE,
      class = "stripe hover compact"
    ) %>%
      DT::formatRound(columns = "abundance_index", digits = 2)
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

    # Build display data frame with badge HTML
    df <- rv$trait_results
    display_df <- data.frame(
      species = df$species,
      stringsAsFactors = FALSE
    )

    # Add trait columns with provenance badges. One list drives both the badge
    # rendering and the escape exemption below, so the two cannot drift apart
    # and silently leave a column unescaped.
    trait_badge_cols <- c("MS", "FS", "MB", "EP", "PR", "RS", "TT", "ST")
    for (trait in trait_badge_cols) {
      source_col <- paste0(trait, "_source")
      vals <- df[[trait]]
      srcs <- if (source_col %in% names(df)) df[[source_col]] else rep(NA_character_, nrow(df))
      display_df[[trait]] <- mapply(format_trait_badge, vals, srcs, USE.NAMES = FALSE)
    }

    # Add metadata columns
    if ("confidence" %in% names(df)) display_df$confidence <- df$confidence
    if ("imputation_method" %in% names(df)) display_df$imputation_method <- df$imputation_method
    if ("source" %in% names(df)) display_df$sources <- df$source
    # A species whose lookup failed (F33) shows why; escaped like `sources`.
    if ("error" %in% names(df)) display_df$error <- df$error

    # Confidence columns (keep numeric for color-coding)
    conf_cols <- c("MS_confidence", "FS_confidence", "MB_confidence",
                   "EP_confidence", "PR_confidence")
    for (cc in conf_cols) {
      if (cc %in% names(df)) display_df[[cc]] <- df[[cc]]
    }

    # Only the badge columns skip escaping. A blanket escape = FALSE also
    # unescaped `species` and `sources`, which carry values from the uploaded
    # CSV - a species name containing markup was stored in the results and
    # executed in the browser of anyone who viewed this table.
    dt <- DT::datatable(
      display_df,
      escape = badge_escape_columns(display_df, trait_badge_cols),
      options = list(pageLength = 15, scrollX = TRUE, dom = 'Bfrtip',
                     buttons = c('copy', 'csv', 'excel')),
      rownames = FALSE, class = 'stripe hover compact'
    )

    # Color-code confidence columns: green >0.8, yellow 0.5-0.8, red <0.5
    conf_cols_present <- conf_cols[conf_cols %in% names(display_df)]
    if (length(conf_cols_present) > 0) {
      dt <- dt %>%
        DT::formatStyle(
          columns = conf_cols_present,
          backgroundColor = DT::styleInterval(
            c(0.5, 0.8),
            c("#ffcdd2", "#fff9c4", "#c8e6c9")
          )
        ) %>%
        DT::formatRound(columns = conf_cols_present, digits = 2)
    }

    dt
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
    # Test mode: read directly from the selected dataset.
    # Manual and File modes both ultimately use the textarea — manual because
    # the user types into it, file because the upload observer writes parsed
    # rows back via updateTextAreaInput(). Treat them identically here so the
    # badge count and the lookup count never disagree.
    if (isTRUE(input$trait_research_input_method == "test")) {
      req(input$trait_research_test_dataset)
      return(get_test_dataset(input$trait_research_test_dataset))
    }
    species_text <- input$trait_research_species_list
    if (is.null(species_text) || species_text == "") return(character(0))
    species <- trimws(strsplit(species_text, "\n")[[1]])
    species[species != ""]
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
  # RADAR CHART: FUZZY TRAIT PROFILE
  # ============================================================================

  output$trait_radar_chart <- plotly::renderPlotly({
    req(rv$trait_results)
    req(nrow(rv$trait_results) > 0)
    species_data <- rv$trait_results[1, ]

    trait_codes <- c("MS", "FS", "MB", "EP", "PR", "RS", "TT", "ST")
    trait_labels <- c("Body Size", "Foraging", "Mobility", "Env. Position",
                      "Protection", "Reproduction", "Temperature", "Salinity")

    scores <- sapply(trait_codes, function(tc) {
      val <- species_data[[tc]]
      if (is.na(val) || val == "") return(-1)
      num <- as.numeric(gsub("[^0-9]", "", val))
      if (is.na(num)) return(-1)
      num
    })

    max_vals <- c(7, 7, 5, 4, 8, 4, 4, 5)
    normalized <- ifelse(scores < 0, 0, (scores + 0.5) / (max_vals + 0.5))

    plotly::plot_ly(
      type = 'scatterpolar',
      r = c(normalized, normalized[1]),
      theta = c(trait_labels, trait_labels[1]),
      fill = 'toself',
      fillcolor = 'rgba(0, 123, 255, 0.2)',
      line = list(color = '#007bff'),
      name = species_data$species
    ) %>%
      plotly::layout(
        polar = list(
          radialaxis = list(visible = TRUE, range = c(0, 1)),
          angularaxis = list(tickfont = list(size = 12))
        ),
        title = paste("Trait Profile:", species_data$species),
        showlegend = FALSE
      )
  })

  # ============================================================================
  # OFFLINE DATABASE MANAGEMENT
  # ============================================================================

  # Reactive trigger to refresh offline DB value boxes after rebuild
  offline_db_trigger <- reactiveVal(0)

  # Single reactive that opens the DB once per trigger invalidation and
  # returns the metadata all three value boxes need. Previously each
  # value box opened/closed its own DBI connection (3x dbConnect per
  # refresh) — same data, three round-trips.
  offline_db_summary <- reactive({
    offline_db_trigger()  # invalidate on rebuild
    db_path <- app_path("cache/offline_traits.db")
    default <- list(
      count = 0,
      age_text = "Not built", age_color = "danger",
      status = "Not Found",   status_color = "danger"
    )
    if (!file.exists(db_path) || !requireNamespace("RSQLite", quietly = TRUE)) {
      return(default)
    }
    tryCatch({
      con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
      on.exit(DBI::dbDisconnect(con))
      count <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM species_traits")$n
      meta <- if (DBI::dbExistsTable(con, "metadata")) {
        DBI::dbGetQuery(con, "SELECT value FROM metadata WHERE key = 'build_timestamp'")
      } else {
        data.frame(value = character(0))
      }
      # A DB in another trait vocabulary is skipped by the lookup gate, whose
      # warning is invisible in production: show the pending rebuild here.
      vocab <- offline_db_vocab_status(db_path)
      age_text <- "Unknown"; age_color <- "warning"
      if (nrow(meta) > 0) {
        build_time <- as.POSIXct(meta$value[1])
        age_days <- as.numeric(difftime(Sys.time(), build_time, units = "days"))
        age_text  <- paste0(round(age_days), " days")
        age_color <- if (age_days < 30) "success"
                     else if (age_days < 90) "warning" else "danger"
      }
      list(
        count = count,
        age_text = age_text, age_color = age_color,
        status = vocab$message,
        status_color = if (isTRUE(vocab$ok)) "success" else "warning"
      )
    }, error = function(e) {
      # A read failure (corrupt/locked/schema-drifted DB) is NOT the same as a
      # missing file - surface it and label it distinctly so the operator does
      # not "rebuild" a present-but-broken DB thinking it was never built (#34).
      warning(sprintf("[offline_db_summary] failed to read %s: %s",
                      db_path, conditionMessage(e)), call. = FALSE)
      modifyList(default, list(age_text = "Read error", status = "Read error"))
    })
  })

  output$offline_db_species_count <- renderValueBox({
    s <- offline_db_summary()
    valueBox(s$count, "Species", icon = icon("fish"), color = "primary")
  })

  output$offline_db_age <- renderValueBox({
    s <- offline_db_summary()
    valueBox(s$age_text, "Database Age", icon = icon("clock"), color = s$age_color)
  })

  output$offline_db_status <- renderValueBox({
    s <- offline_db_summary()
    valueBox(s$status, "Status", icon = icon("check-circle"), color = s$status_color)
  })

  # Background process handle for async rebuild
  offline_rebuild_process <- reactiveVal(NULL)

  # F19: what THIS session's rebuild holds - the lock token and the child's
  # log files. A plain environment, not a reactive: only the exit poller and
  # onSessionEnded read it. The child adopts the lock through the token and
  # releases it itself when it exits; the releases below are token-checked
  # no-ops then, and only matter if the child died before it could release.
  rebuild_lock_dir <- offline_db_lock_path()
  rebuild_state <- new.env()
  rebuild_state$token <- NULL
  rebuild_state$out <- NULL
  rebuild_state$err <- NULL

  release_session_rebuild_lock <- function() {
    if (!is.null(rebuild_state$token)) {
      release_rebuild_lock(rebuild_lock_dir, rebuild_state$token)
      rebuild_state$token <- NULL
    }
  }

  read_rebuild_log <- function(path) {
    if (is.null(path) || !file.exists(path)) return(character(0))
    tryCatch(readLines(path, warn = FALSE), error = function(e) {
      warning(sprintf("[rebuild] could not read build log '%s': %s",
                      path, conditionMessage(e)), call. = FALSE)
      character(0)
    })
  }

  # A closed tab must not leave the lock behind. Release only when the build
  # is no longer running: a live child keeps building (cleanup = FALSE,
  # supervise = FALSE, so it also outlives this R worker) and releases the
  # lock itself when it exits.
  session$onSessionEnded(function() {
    proc <- isolate(offline_rebuild_process())
    if (is.null(proc) || !proc$is_alive()) release_session_rebuild_lock()
  })

  # Single session-level poller for rebuild completion. req() suspends
  # the observer when no process is in flight, so it costs nothing while
  # idle. Created ONCE at module init; previous design created a fresh
  # observe() inside every observeEvent click, which leaked observers
  # under repeated clicks since they had no destroy hook.
  observe({
    proc <- offline_rebuild_process()
    req(proc)
    if (proc$is_alive()) {
      invalidateLater(2000, session)
      return()
    }

    # Process finished - handle completion. Wrap the body in tryCatch so a
    # rendering / notification failure doesn't leave the process reactive
    # in a "finished but unhandled" state forever. `finally` releases this
    # session's lock on success, failure and handler error alike.
    tryCatch(isolate({
      exit_status <- proc$get_exit_status()
      removeNotification("rebuild_progress")

      if (isTRUE(exit_status == 0)) {
        output_text <- paste(read_rebuild_log(rebuild_state$out), collapse = "\n")
        total_match <- regmatches(output_text,
          regexpr("Total species in database: [0-9]+", output_text))
        summary <- if (length(total_match) > 0) total_match else "Build complete"

        showNotification(
          HTML(paste0("<b>Offline database rebuilt!</b><br>", summary)),
          type = "message", duration = 8
        )
      } else {
        err_lines <- read_rebuild_log(rebuild_state$err)
        err_lines <- err_lines[nchar(trimws(err_lines)) > 0]
        last_err <- if (length(err_lines) > 0) {
          tail(err_lines, 1)
        } else {
          "Unknown error"
        }
        showNotification(
          HTML(paste0("<b>Rebuild failed</b> (exit ", exit_status, ")<br>",
                      htmltools::htmlEscape(last_err))),
          type = "error", duration = 15
        )
      }

      offline_db_trigger(offline_db_trigger() + 1)
      offline_rebuild_process(NULL)
    }), error = function(e) {
      warning(sprintf("[rebuild observer] handler error: %s", conditionMessage(e)), call. = FALSE)
      isolate(offline_rebuild_process(NULL))
    }, finally = {
      release_session_rebuild_lock()
      unlink(c(rebuild_state$out, rebuild_state$err))
    })
  })

  # F19: admin gate first (fail closed when ECONETOOL_ADMIN_PASSWORD_HASH is
  # unset), then the process-wide lock, then the child Rscript. The decision
  # lives in request_offline_rebuild() (R/functions/offline_db_rebuild.R) so
  # it is unit-tested without a session.
  observeEvent(input$rebuild_offline_db, {
    # F-a: the previous build's completion has not been handled yet (the
    # child may already have exited and released the lock before the 2 s
    # poller ran). A second launch would overwrite its handle and log paths.
    if (!is.null(offline_rebuild_process())) {
      showNotification("Rebuild already running (previous build not yet finished)",
                       type = "warning", duration = 8)
      return()
    }
    out_file <- tempfile("offline_rebuild_", fileext = ".out")
    err_file <- tempfile("offline_rebuild_", fileext = ".err")

    res <- request_offline_rebuild(
      unlocked = session$userData$admin_unlocked,
      lock_dir = rebuild_lock_dir,
      launch = function(token) {
        build_script <- app_path("scripts/initialization/build_offline_trait_db.R")
        if (!file.exists(build_script)) {
          stop("build script not found: ", build_script, call. = FALSE)
        }
        # Logs go to files, not pipes: nothing reads a pipe until the build
        # ends, and a full pipe buffer (or a closed tab) would stall or kill
        # the child while it holds the lock. cleanup = FALSE stops GC of the
        # handle from killing the child. supervise = FALSE (final review I1):
        # shiny-server ends this R worker ~5 s after the last tab closes (no
        # app_idle_timeout on laguna), and processx's supervisor would kill
        # the child with it, mid-build. Unsupervised, the child outlives the
        # worker; its reg.finalizer(onexit = TRUE) removes its tmp file and
        # releases the lock however it exits. A hard kill (SIGKILL, reboot)
        # skips the finalizer: the next acquire reclaims the lock after
        # REBUILD_LOCK_STALE_MINS and sweeps the orphaned tmp file.
        processx::process$new(
          file.path(R.home("bin"), "Rscript"),
          args = build_script,
          wd = app_path(),
          env = c("current", stats::setNames(token, REBUILD_LOCK_TOKEN_ENV)),
          stdout = out_file, stderr = err_file,
          supervise = FALSE, cleanup = FALSE
        )
      }
    )

    if (!identical(res$status, "started")) {
      unlink(c(out_file, err_file))
      showNotification(res$message,
                       type = if (identical(res$status, "busy")) "warning" else "error",
                       duration = 8)
      return()
    }

    rebuild_state$token <- res$token
    rebuild_state$out <- out_file
    rebuild_state$err <- err_file
    offline_rebuild_process(res$proc)
    showNotification(
      HTML("<b>Rebuilding offline trait database...</b><br>This may take a minute."),
      type = "message", duration = NULL, id = "rebuild_progress"
    )
  })

  output$offline_db_contents <- DT::renderDataTable({
    req(input$view_offline_db > 0)
    db_path <- app_path("cache/offline_traits.db")
    if (!file.exists(db_path) || !requireNamespace("RSQLite", quietly = TRUE)) {
      return(DT::datatable(data.frame(Message = "Database not available")))
    }
    tryCatch({
      con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
      on.exit(DBI::dbDisconnect(con))
      data <- DBI::dbGetQuery(con, "SELECT species, MS, FS, MB, EP, PR, RS, TT, ST, primary_source FROM species_traits LIMIT 500")
      DT::datatable(data, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
    }, error = function(e) DT::datatable(data.frame(Error = e$message)))
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
