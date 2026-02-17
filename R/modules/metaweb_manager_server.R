#' Metaweb Manager Server Module
#'
#' Handles regional metaweb loading, custom metaweb import, species/link
#' editing, network visualization, quality analysis, export, and
#' conversion to active network.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param current_metaweb ReactiveVal holding the current metaweb object
#' @param net_reactive ReactiveVal holding the igraph network
#' @param info_reactive ReactiveVal holding the species info data frame
#' @param metaweb_metadata ReactiveVal holding metadata for dashboard
#' @param dashboard_trigger ReactiveVal to trigger dashboard updates
metaweb_manager_server <- function(input, output, session, current_metaweb,
                                    net_reactive, info_reactive,
                                    metaweb_metadata, dashboard_trigger) {

  # Load regional metaweb
  observeEvent(input$load_regional_btn, {
    req(input$regional_metaweb)

    if (input$regional_metaweb == "") {
      showNotification("Please select a region first", type = "warning")
      return()
    }

    tryCatch({
      # Map region IDs to actual file paths
      metaweb_paths <- list(
        "arctic_barents_arctic" = "metawebs/arctic/barents_arctic_kortsch2015.rds",
        "arctic_barents_boreal" = "metawebs/arctic/barents_boreal_kortsch2015.rds",
        "arctic_kongsfjorden" = "metawebs/arctic/kongsfjorden_farage2021.rds",
        "baltic_nordstrom" = NULL,  # Not yet available
        "baltic_kortsch" = "metawebs/baltic/baltic_kortsch2021.rds",
        "baltic_garrison" = NULL,  # Not yet available
        "atlantic_northsea" = "metawebs/atlantic/north_sea_frelat2022.rds",
        "mediterranean" = NULL  # Not yet available
      )

      # Get the file path for the selected metaweb
      metaweb_file <- metaweb_paths[[input$regional_metaweb]]

      # Check if metaweb is available
      if (is.null(metaweb_file)) {
        showNotification(
          paste0("Metaweb not yet available. Please download from literature sources (see metawebs/README.md)"),
          type = "warning",
          duration = 10
        )
        return()
      }

      if (!file.exists(metaweb_file)) {
        showNotification(
          paste0("Metaweb file not found: ", metaweb_file,
                 ". Please download from literature sources (see metawebs/README.md)"),
          type = "error",
          duration = 10
        )
        return()
      }

      metaweb <- readRDS(metaweb_file)
      current_metaweb(metaweb)

      showNotification(
        paste("Loaded metaweb:", metaweb$metadata$region %||% "Unknown region"),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      showNotification(
        paste("Error loading metaweb:", e$message),
        type = "error",
        duration = 10
      )
    })
  })

  # Import custom metaweb from CSV
  observeEvent(input$import_metaweb_btn, {
    req(input$metaweb_species_file, input$metaweb_interactions_file)

    tryCatch({
      species <- read.csv(input$metaweb_species_file$datapath, stringsAsFactors = FALSE)
      interactions <- read.csv(input$metaweb_interactions_file$datapath, stringsAsFactors = FALSE)

      metadata <- list(
        region = "Custom import",
        imported = Sys.time(),
        species_file = input$metaweb_species_file$name,
        interactions_file = input$metaweb_interactions_file$name
      )

      metaweb <- create_metaweb(species, interactions, metadata)
      current_metaweb(metaweb)

      showNotification(
        paste("Imported metaweb with", nrow(species), "species and", nrow(interactions), "interactions"),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      showNotification(
        paste("Error importing metaweb:", e$message),
        type = "error",
        duration = 10
      )
    })
  })

  # Metaweb summary output
  output$metaweb_summary <- renderPrint({
    if (is.null(current_metaweb())) {
      cat("No metaweb loaded.\n\n")
      cat("Load a pre-built regional metaweb or import your own CSV files to get started.\n")
    } else {
      print(current_metaweb())
    }
  })

  # Update species dropdowns when metaweb changes
  observe({
    req(current_metaweb())

    metaweb <- current_metaweb()
    species_choices <- setNames(
      metaweb$species$species_id,
      metaweb$species$species_name
    )

    updateSelectInput(session, "remove_species_id", choices = species_choices)
    updateSelectInput(session, "link_predator", choices = species_choices)
    updateSelectInput(session, "link_prey", choices = species_choices)
    updateSelectInput(session, "remove_link_predator", choices = species_choices)
    updateSelectInput(session, "remove_link_prey", choices = species_choices)
  })

  # Metaweb network visualization
  output$metaweb_network <- renderVisNetwork({
    req(current_metaweb())

    metaweb <- current_metaweb()

    # Create nodes
    nodes <- data.frame(
      id = metaweb$species$species_id,
      label = metaweb$species$species_name,
      group = metaweb$species$functional_group %||% rep("Other", nrow(metaweb$species)),
      title = paste0(
        "<b>", metaweb$species$species_name, "</b><br>",
        "ID: ", metaweb$species$species_id, "<br>",
        "Group: ", metaweb$species$functional_group %||% "NA"
      )
    )

    # Create edges
    edges <- data.frame(
      from = metaweb$interactions$predator_id,
      to = metaweb$interactions$prey_id,
      arrows = "to",
      title = paste0(
        "Quality: ", metaweb$interactions$quality_code, "<br>",
        "Source: ", metaweb$interactions$source
      ),
      color = ifelse(
        metaweb$interactions$quality_code == 1, "#2ecc71",
        ifelse(metaweb$interactions$quality_code == 2, "#f39c12",
               ifelse(metaweb$interactions$quality_code == 3, "#e67e22", "#e74c3c"))
      )
    )

    visNetwork(nodes, edges) %>%
      visIgraphLayout(layout = "layout_with_fr") %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1),
        nodesIdSelection = TRUE
      ) %>%
      visLegend(width = 0.1, position = "right", main = "Functional Groups")
  })

  # Species table
  output$metaweb_species_table <- renderDT({
    req(current_metaweb())

    DT::datatable(
      current_metaweb()$species,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'ftip'
      ),
      rownames = FALSE,
      filter = 'top'
    )
  })

  # Interactions table
  output$metaweb_interactions_table <- renderDT({
    req(current_metaweb())

    DT::datatable(
      current_metaweb()$interactions,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'ftip'
      ),
      rownames = FALSE,
      filter = 'top'
    )
  })

  # Add species
  observeEvent(input$add_species_btn, {
    req(current_metaweb(), input$new_species_id, input$new_species_name)

    tryCatch({
      updated_metaweb <- add_species_to_metaweb(
        current_metaweb(),
        input$new_species_id,
        input$new_species_name,
        input$new_species_fg
      )
      current_metaweb(updated_metaweb)

      output$metaweb_edit_status <- renderPrint({
        cat("SUCCESS: Species added!\n")
        cat("Added:", input$new_species_name, "(", input$new_species_id, ")\n")
        cat("Current species count:", nrow(updated_metaweb$species), "\n")
      })

      # Clear inputs
      updateTextInput(session, "new_species_id", value = "")
      updateTextInput(session, "new_species_name", value = "")

      showNotification("Species added successfully", type = "message")
    }, error = function(e) {
      output$metaweb_edit_status <- renderPrint({
        cat("ERROR adding species:\n")
        cat(e$message, "\n")
      })
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # Remove species
  observeEvent(input$remove_species_btn, {
    req(current_metaweb(), input$remove_species_id)

    tryCatch({
      updated_metaweb <- remove_species_from_metaweb(
        current_metaweb(),
        input$remove_species_id,
        input$remove_species_links
      )
      current_metaweb(updated_metaweb)

      output$metaweb_edit_status <- renderPrint({
        cat("SUCCESS: Species removed!\n")
        cat("Removed:", input$remove_species_id, "\n")
        cat("Current species count:", nrow(updated_metaweb$species), "\n")
      })

      showNotification("Species removed successfully", type = "message")
    }, error = function(e) {
      output$metaweb_edit_status <- renderPrint({
        cat("ERROR removing species:\n")
        cat(e$message, "\n")
      })
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # Add trophic link
  observeEvent(input$add_link_btn, {
    req(current_metaweb(), input$link_predator, input$link_prey)

    tryCatch({
      updated_metaweb <- add_trophic_link(
        current_metaweb(),
        input$link_predator,
        input$link_prey,
        as.integer(input$link_quality),
        input$link_source,
        input$link_notes
      )
      current_metaweb(updated_metaweb)

      output$metaweb_edit_status <- renderPrint({
        cat("SUCCESS: Trophic link added!\n")
        cat("Predator:", input$link_predator, "\n")
        cat("Prey:", input$link_prey, "\n")
        cat("Quality:", input$link_quality, "\n")
        cat("Current link count:", nrow(updated_metaweb$interactions), "\n")
      })

      # Clear source and notes
      updateTextInput(session, "link_source", value = "")
      updateTextAreaInput(session, "link_notes", value = "")

      showNotification("Trophic link added successfully", type = "message")
    }, error = function(e) {
      output$metaweb_edit_status <- renderPrint({
        cat("ERROR adding link:\n")
        cat(e$message, "\n")
      })
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # Remove trophic link
  observeEvent(input$remove_link_btn, {
    req(current_metaweb(), input$remove_link_predator, input$remove_link_prey)

    tryCatch({
      updated_metaweb <- remove_trophic_link(
        current_metaweb(),
        input$remove_link_predator,
        input$remove_link_prey
      )
      current_metaweb(updated_metaweb)

      output$metaweb_edit_status <- renderPrint({
        cat("SUCCESS: Trophic link removed!\n")
        cat("Removed link from", input$remove_link_predator, "to", input$remove_link_prey, "\n")
        cat("Current link count:", nrow(updated_metaweb$interactions), "\n")
      })

      showNotification("Trophic link removed successfully", type = "message")
    }, error = function(e) {
      output$metaweb_edit_status <- renderPrint({
        cat("ERROR removing link:\n")
        cat(e$message, "\n")
      })
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # Link quality plot
  output$link_quality_plot <- renderPlot({
    req(current_metaweb())

    quality_summary <- summarize_link_quality(current_metaweb())

    barplot(
      quality_summary$count,
      names.arg = paste0("Q", quality_summary$quality_code),
      col = c("#2ecc71", "#f39c12", "#e67e22", "#e74c3c"),
      main = "Link Quality Distribution",
      xlab = "Quality Code",
      ylab = "Number of Links",
      las = 1,
      ylim = c(0, max(quality_summary$count) * 1.1)
    )

    # Add count labels on top of bars
    text(
      x = 1:4 * 1.2 - 0.5,
      y = quality_summary$count,
      labels = quality_summary$count,
      pos = 3,
      cex = 1.2
    )
  })

  # Link quality summary table
  output$link_quality_table <- renderDT({
    req(current_metaweb())

    quality_summary <- summarize_link_quality(current_metaweb())

    DT::datatable(
      quality_summary,
      options = list(
        pageLength = 4,
        dom = 't',
        ordering = FALSE
      ),
      rownames = FALSE
    )
  })

  # Filtered links table
  output$filtered_links_table <- renderDT({
    req(current_metaweb(), input$quality_filter)

    metaweb <- current_metaweb()
    filtered_interactions <- metaweb$interactions[
      metaweb$interactions$quality_code %in% as.integer(input$quality_filter),
    ]

    DT::datatable(
      filtered_interactions,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'ftip'
      ),
      rownames = FALSE,
      filter = 'top'
    )
  })

  # Download metaweb
  output$download_metaweb <- downloadHandler(
    filename = function() {
      if (input$export_format == "csv") {
        paste0(input$export_filename, ".zip")
      } else {
        paste0(input$export_filename, ".rds")
      }
    },
    content = function(file) {
      req(current_metaweb())

      if (input$export_format == "csv") {
        # Export CSV - create a zip file with both files
        temp_dir <- tempdir()
        species_file <- file.path(temp_dir, paste0(input$export_filename, "_species.csv"))
        interactions_file <- file.path(temp_dir, paste0(input$export_filename, "_interactions.csv"))

        write.csv(current_metaweb()$species, species_file, row.names = FALSE)
        write.csv(current_metaweb()$interactions, interactions_file, row.names = FALSE)

        # Create zip file
        old_wd <- setwd(temp_dir)
        on.exit(setwd(old_wd))
        utils::zip(file, files = basename(c(species_file, interactions_file)))
      } else {
        # Export RDS
        saveRDS(current_metaweb(), file)
      }
    }
  )

  # Export to active network
  observeEvent(input$export_to_network_btn, {
    req(current_metaweb())

    tryCatch({
      # Convert metaweb to igraph
      new_net <- metaweb_to_igraph(current_metaweb())

      # Update reactive network
      net_reactive(new_net)

      # Try to update info if it exists
      if ("species_name" %in% colnames(current_metaweb()$species)) {
        # Create a basic info data frame
        info_df <- current_metaweb()$species
        rownames(info_df) <- info_df$species_name

        # Ensure required columns exist (with defaults if missing)
        if (!"meanB" %in% colnames(info_df)) info_df$meanB <- 1
        if (!"fg" %in% colnames(info_df)) info_df$fg <- factor(rep("Other", nrow(info_df)))
        if (!"bodymasses" %in% colnames(info_df)) info_df$bodymasses <- 1
        if (!"met.types" %in% colnames(info_df)) info_df$met.types <- "Other"
        if (!"efficiencies" %in% colnames(info_df)) info_df$efficiencies <- 0.5

        # Update reactive info
        info_reactive(info_df)
      }

      output$export_status <- renderPrint({
        cat("SUCCESS: Metaweb converted to active network!\n")
        cat("Species:", vcount(net_reactive()), "\n")
        cat("Links:", ecount(net_reactive()), "\n")
        cat("\nThe network is now available in:\n")
        cat("  - Food Web Network tab\n")
        cat("  - Topological Metrics tab\n")
        cat("  - Biomass Analysis tab (if biomass data available)\n")
        cat("  - Energy Fluxes tab (if trait data available)\n")
        cat("  - Keystoneness Analysis tab (if biomass data available)\n")
      })

      showNotification(
        "Metaweb converted to active network! Navigate to other tabs to analyze.",
        type = "message",
        duration = 10
      )
    }, error = function(e) {
      output$export_status <- renderPrint({
        cat("ERROR converting metaweb:\n")
        cat(e$message, "\n")
      })
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
}
