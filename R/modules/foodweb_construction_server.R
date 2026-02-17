# Server logic for Food Web Construction Module
# Handles network construction, visualization, and validation

foodweb_construction_server <- function(input, output, session, shared_data) {

  # ============================================================================
  # REACTIVE VALUES
  # ============================================================================

  rv <- reactiveValues(
    trait_data = NULL,
    original_trait_data = NULL,  # For reset functionality
    adjacency_matrix = NULL,
    prob_matrix = NULL,
    network_igraph = NULL,
    validation_result = NULL
  )

  # ============================================================================
  # EXAMPLE DATASETS
  # ============================================================================

  get_example_dataset <- function(type) {
    if (type == "simple") {
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
      set.seed(42)
      n <- 20
      data.frame(
        species = paste0("Species_", sprintf("%02d", 1:n)),
        MS = sample(paste0("MS", 1:6), n, replace = TRUE,
                   prob = c(0.15, 0.20, 0.25, 0.20, 0.15, 0.05)),
        FS = sample(paste0("FS", c(0, 1, 2, 4, 5, 6)), n, replace = TRUE,
                   prob = c(0.15, 0.25, 0.10, 0.20, 0.15, 0.15)),
        MB = sample(paste0("MB", 1:5), n, replace = TRUE),
        EP = sample(paste0("EP", 1:4), n, replace = TRUE),
        PR = sample(paste0("PR", c(0, 2, 3, 5, 6, 7, 8)), n, replace = TRUE,
                   prob = c(0.30, 0.10, 0.10, 0.10, 0.15, 0.15, 0.10)),
        stringsAsFactors = FALSE
      )
    }
  }

  # ============================================================================
  # CHECK FOR DATA FROM TRAIT RESEARCH
  # ============================================================================

  output$foodweb_trait_research_data_status <- renderUI({
    if (!is.null(shared_data$trait_data) && nrow(shared_data$trait_data) > 0) {
      tags$div(
        class = "alert alert-success",
        icon("check-circle"),
        sprintf(" Trait data available from Trait Research: %d species",
                nrow(shared_data$trait_data)),
        actionButton(
          "foodweb_use_research_data",
          "Use This Data",
          class = "btn-sm btn-success ml-3"
        )
      )
    } else {
      tags$div(
        class = "alert alert-info",
        icon("info-circle"),
        " No trait data from Trait Research. Use other input methods or run Trait Research first."
      )
    }
  })

  # Use data from Trait Research directly
  observeEvent(input$foodweb_use_research_data, {
    req(shared_data$trait_data)

    rv$trait_data <- shared_data$trait_data
    rv$original_trait_data <- shared_data$trait_data

    showNotification(
      sprintf("Loaded %d species from Trait Research", nrow(rv$trait_data)),
      type = "message"
    )
  })

  # Preview of research data
  output$foodweb_research_data_preview <- renderUI({
    if (!is.null(shared_data$trait_data) && nrow(shared_data$trait_data) > 0) {
      n <- min(5, nrow(shared_data$trait_data))
      preview_df <- shared_data$trait_data[1:n, c("species", "MS", "FS")]

      tags$div(
        tags$p(sprintf("Preview (first %d of %d species):", n, nrow(shared_data$trait_data))),
        tags$table(
          class = "table table-sm table-bordered",
          tags$thead(
            tags$tr(
              tags$th("Species"),
              tags$th("MS"),
              tags$th("FS")
            )
          ),
          tags$tbody(
            lapply(1:n, function(i) {
              tags$tr(
                tags$td(preview_df$species[i]),
                tags$td(preview_df$MS[i]),
                tags$td(preview_df$FS[i])
              )
            })
          )
        )
      )
    } else {
      tags$p(class = "text-muted", "No data available from Trait Research.")
    }
  })

  # ============================================================================
  # LOAD DATA
  # ============================================================================

  observeEvent(input$foodweb_load_data, {
    method <- input$foodweb_input_method

    if (method == "research") {
      if (!is.null(shared_data$trait_data) && nrow(shared_data$trait_data) > 0) {
        rv$trait_data <- shared_data$trait_data
        rv$original_trait_data <- shared_data$trait_data
        showNotification(
          sprintf("Loaded %d species from Trait Research", nrow(rv$trait_data)),
          type = "message"
        )
      } else {
        showNotification(
          "No data available from Trait Research. Please run trait lookup first.",
          type = "warning"
        )
      }

    } else if (method == "upload") {
      req(input$foodweb_file)
      tryCatch({
        df <- read.csv(input$foodweb_file$datapath, stringsAsFactors = FALSE)
        rv$trait_data <- df
        rv$original_trait_data <- df
        showNotification(
          sprintf("Loaded %d species from file", nrow(df)),
          type = "message"
        )
      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error")
      })

    } else if (method == "example") {
      rv$trait_data <- get_example_dataset(input$foodweb_example_dataset)
      rv$original_trait_data <- rv$trait_data
      showNotification(
        sprintf("Loaded %s example (%d species)",
                input$foodweb_example_dataset, nrow(rv$trait_data)),
        type = "message"
      )

    } else if (method == "manual") {
      rv$trait_data <- create_trait_template(input$foodweb_n_species)
      rv$original_trait_data <- rv$trait_data
      showNotification(
        sprintf("Created blank template for %d species", input$foodweb_n_species),
        type = "message"
      )
    }
  })

  # Initialize with example on startup
  observe({
    if (is.null(rv$trait_data)) {
      rv$trait_data <- get_example_dataset("simple")
      rv$original_trait_data <- rv$trait_data
    }
  })

  # Reset to original
  observeEvent(input$foodweb_reset_data, {
    req(rv$original_trait_data)
    rv$trait_data <- rv$original_trait_data
    showNotification("Data reset to original", type = "message")
  })

  # ============================================================================
  # DATA STATUS AND STATS
  # ============================================================================

  output$foodweb_data_status <- renderUI({
    if (is.null(rv$trait_data)) {
      tags$div(
        class = "alert alert-warning",
        icon("exclamation-triangle"), " No data loaded"
      )
    } else {
      tags$div(
        class = "alert alert-success",
        icon("check-circle"), sprintf(" %d species loaded", nrow(rv$trait_data))
      )
    }
  })

  output$foodweb_data_quick_stats <- renderUI({
    req(rv$trait_data)
    n_complete <- sum(complete.cases(rv$trait_data[, c("MS", "FS", "MB", "EP", "PR")]))
    HTML(sprintf(
      "<p><strong>Species:</strong> %d</p><p><strong>Complete traits:</strong> %d / %d</p>",
      nrow(rv$trait_data), n_complete, nrow(rv$trait_data)
    ))
  })

  output$foodweb_example_description <- renderUI({
    req(input$foodweb_example_dataset)
    descriptions <- list(
      simple = "A simple 5-species food chain demonstrating basic trophic relationships.",
      marine = "A 10-species benthic invertebrate community with diverse feeding strategies.",
      complex = "A 20-species coastal ecosystem including plankton, invertebrates, and fish."
    )
    tags$p(class = "text-muted", descriptions[[input$foodweb_example_dataset]])
  })

  # ============================================================================
  # EDITABLE DATA TABLE
  # ============================================================================

  output$foodweb_data_table <- DT::renderDataTable({
    req(rv$trait_data)

    DT::datatable(
      rv$trait_data,
      editable = TRUE,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = 't'
      ),
      rownames = FALSE
    )
  })

  # Handle cell edits
  observeEvent(input$foodweb_data_table_cell_edit, {
    info <- input$foodweb_data_table_cell_edit
    # DT uses 0-based column indexing, but we have rownames = FALSE so +1 to get R indexing
    col_index <- info$col + 1
    rv$trait_data[info$row, col_index] <- info$value
  })

  # ============================================================================
  # VALIDATION
  # ============================================================================

  observeEvent(input$foodweb_validate, {
    req(rv$trait_data)

    result <- validate_trait_data(rv$trait_data)
    rv$validation_result <- result

    if (result$valid) {
      showNotification("Trait data is valid!", type = "message", duration = 3)
    } else {
      showNotification("Validation errors found. See messages below.", type = "warning", duration = 5)
    }
  })

  output$foodweb_validation_messages <- renderUI({
    if (is.null(rv$validation_result)) {
      return(tags$div(
        class = "alert alert-secondary",
        "Click 'Validate Data' to check for issues."
      ))
    }

    messages <- rv$validation_result$messages
    is_valid <- rv$validation_result$valid

    if (is_valid) {
      tags$div(
        class = "alert alert-success",
        icon("check-circle"),
        tags$strong(" Valid: "),
        paste(messages, collapse = "; ")
      )
    } else {
      tags$div(
        class = "alert alert-danger",
        icon("exclamation-triangle"),
        tags$strong(" Errors: "),
        tags$ul(
          lapply(messages, function(msg) tags$li(msg))
        )
      )
    }
  })

  output$foodweb_data_stats <- renderUI({
    req(rv$trait_data)

    n_species <- nrow(rv$trait_data)
    ms_counts <- table(rv$trait_data$MS)
    fs_counts <- table(rv$trait_data$FS)

    HTML(paste0(
      "<table class='table table-sm'>",
      "<tr><td><b>Total species:</b></td><td>", n_species, "</td></tr>",
      "<tr><td><b>Size classes:</b></td><td>", paste(names(ms_counts), collapse = ", "), "</td></tr>",
      "<tr><td><b>Foraging types:</b></td><td>", paste(names(fs_counts), collapse = ", "), "</td></tr>",
      "</table>"
    ))
  })

  output$foodweb_distribution_plot <- renderPlot({
    req(rv$trait_data)

    par(mar = c(4, 4, 2, 1))
    trait_counts <- sapply(c("MS", "FS", "MB", "EP", "PR"), function(trait) {
      sum(!is.na(rv$trait_data[[trait]]))
    })
    barplot(trait_counts, names.arg = c("MS", "FS", "MB", "EP", "PR"),
            col = rainbow(5, alpha = 0.7),
            main = "Traits per Dimension",
            ylab = "Count")
  })

  # ============================================================================
  # CONSTRUCT FOOD WEB
  # ============================================================================

  observeEvent(input$foodweb_construct_network, {
    req(rv$trait_data)

    # Validate first
    validation <- validate_trait_data(rv$trait_data)
    if (!validation$valid) {
      showNotification(
        "Please fix validation errors before constructing network",
        type = "error", duration = 5
      )
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
        threshold = input$foodweb_threshold,
        return_probs = FALSE
      )

      incProgress(0.2, detail = "Building network graph")

      # Create igraph object
      rv$network_igraph <- trait_foodweb_to_igraph(
        rv$trait_data,
        threshold = input$foodweb_threshold,
        include_probs = TRUE
      )

      incProgress(0.2, detail = "Done")
    })

    showNotification("Food web constructed successfully!", type = "message", duration = 3)
  })

  # ============================================================================
  # NETWORK VISUALIZATION
  # ============================================================================

  output$foodweb_network_plot <- visNetwork::renderVisNetwork({
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
      "<b>", vis_data$nodes$id, "</b><br>",
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

  output$foodweb_network_stats <- renderUI({
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
  # PROBABILITY HEATMAP
  # ============================================================================

  output$foodweb_probability_heatmap <- renderPlot({
    req(rv$prob_matrix)

    color_palette <- colorRampPalette(c("white", "yellow", "orange", "red"))(100)

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

    axis(1, at = 1:ncol(rv$prob_matrix), labels = colnames(rv$prob_matrix),
         las = 2, cex.axis = 0.8)
    axis(2, at = 1:nrow(rv$prob_matrix), labels = rownames(rv$prob_matrix),
         las = 1, cex.axis = 0.8)

    mtext("Resource (Prey)", side = 1, line = 6)
    mtext("Consumer (Predator)", side = 2, line = 6)

    abline(h = 0.5:(nrow(rv$prob_matrix) + 0.5), col = "gray90", lwd = 0.5)
    abline(v = 0.5:(ncol(rv$prob_matrix) + 0.5), col = "gray90", lwd = 0.5)
  })

  # ============================================================================
  # PROBABILITY MATRIX REFERENCE OUTPUTS
  # ============================================================================

  output$foodweb_prob_matrix_MS_MS <- renderTable({
    as.data.frame.matrix(MS_MS)
  }, rownames = TRUE, digits = 2)

  output$foodweb_prob_matrix_FS_MS <- renderTable({
    as.data.frame.matrix(FS_MS)
  }, rownames = TRUE, digits = 2)

  output$foodweb_prob_matrix_MB_MB <- renderTable({
    as.data.frame.matrix(MB_MB)
  }, rownames = TRUE, digits = 2)

  output$foodweb_prob_matrix_EP_MS <- renderTable({
    as.data.frame.matrix(EP_MS)
  }, rownames = TRUE, digits = 2)

  output$foodweb_prob_matrix_PR_MS <- renderTable({
    as.data.frame.matrix(PR_MS)
  }, rownames = TRUE, digits = 2)

  # ============================================================================
  # TRAIT REFERENCE OUTPUTS
  # ============================================================================

  output$foodweb_trait_ref_MS <- renderUI({
    traits <- get_trait_descriptions("MS")
    HTML(paste0(
      "<table class='table table-sm table-striped'>",
      "<tr><th>Code</th><th>Description</th></tr>",
      paste0("<tr><td><b>", names(traits), "</b></td><td>", traits, "</td></tr>", collapse = ""),
      "</table>"
    ))
  })

  output$foodweb_trait_ref_FS <- renderUI({
    traits <- get_trait_descriptions("FS")
    HTML(paste0(
      "<table class='table table-sm table-striped'>",
      "<tr><th>Code</th><th>Description</th></tr>",
      paste0("<tr><td><b>", names(traits), "</b></td><td>", traits, "</td></tr>", collapse = ""),
      "</table>"
    ))
  })

  output$foodweb_trait_ref_MB <- renderUI({
    traits <- get_trait_descriptions("MB")
    HTML(paste0(
      "<table class='table table-sm table-striped'>",
      "<tr><th>Code</th><th>Description</th></tr>",
      paste0("<tr><td><b>", names(traits), "</b></td><td>", traits, "</td></tr>", collapse = ""),
      "</table>"
    ))
  })

  output$foodweb_trait_ref_EP <- renderUI({
    traits <- get_trait_descriptions("EP")
    HTML(paste0(
      "<table class='table table-sm table-striped'>",
      "<tr><th>Code</th><th>Description</th></tr>",
      paste0("<tr><td><b>", names(traits), "</b></td><td>", traits, "</td></tr>", collapse = ""),
      "</table>"
    ))
  })

  output$foodweb_trait_ref_PR <- renderUI({
    traits <- get_trait_descriptions("PR")
    HTML(paste0(
      "<table class='table table-sm table-striped'>",
      "<tr><th>Code</th><th>Description</th></tr>",
      paste0("<tr><td><b>", names(traits), "</b></td><td>", traits, "</td></tr>", collapse = ""),
      "</table>"
    ))
  })

  # ============================================================================
  # DOWNLOADS
  # ============================================================================

  output$foodweb_download_template <- downloadHandler(
    filename = function() {
      paste0("trait_foodweb_template_", Sys.Date(), ".csv")
    },
    content = function(file) {
      template <- create_trait_template(10)
      write.csv(template, file, row.names = FALSE)
    }
  )

  output$foodweb_download_data <- downloadHandler(
    filename = function() {
      paste0("trait_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$trait_data)
      write.csv(rv$trait_data, file, row.names = FALSE)
    }
  )

  output$foodweb_download_adjacency <- downloadHandler(
    filename = function() {
      paste0("adjacency_matrix_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$adjacency_matrix)
      write.csv(rv$adjacency_matrix, file, row.names = TRUE)
    }
  )

  output$foodweb_download_network <- downloadHandler(
    filename = function() {
      paste0("foodweb_network_", Sys.Date(), ".rds")
    },
    content = function(file) {
      req(rv$network_igraph)
      saveRDS(rv$network_igraph, file)
    }
  )

  output$foodweb_download_graphml <- downloadHandler(
    filename = function() {
      paste0("foodweb_network_", Sys.Date(), ".graphml")
    },
    content = function(file) {
      req(rv$network_igraph)
      igraph::write_graph(rv$network_igraph, file, format = "graphml")
    }
  )
}
