#' Data Import Handler Server Module
#'
#' Handles file upload and data loading for RData, Excel, and CSV formats.
#' Supports binary adjacency matrices, proportion diet matrices, and
#' square adjacency matrices. Updates net_reactive and info_reactive when
#' new data is loaded.

#' Parse an adjacency or diet matrix data frame into an igraph network and info table.
#'
#' Handles two layouts:
#'   1. Square adjacency matrix with matching row/column species names (+ optional info_df)
#'   2. Non-square diet matrix (rows = prey, columns = predators)
#'
#' @param mat_df Data frame where the first column contains species/prey names
#'   and remaining columns are numeric interaction values
#' @param info_df Optional data frame with species attributes (species, fg, meanB, etc.)
#' @param threshold Minimum value to count as a link (default 0, meaning any non-zero)
#' @return List with \code{net} (igraph), \code{info} (data.frame),
#'   \code{weights} (named matrix of original values or NULL if binary),
#'   and \code{matrix_type} ("binary", "proportion", or "percentage")
parse_adjacency_df <- function(mat_df, info_df = NULL, threshold = 0) {
  # First column is species names, rest are numeric
  row_names <- as.character(mat_df[[1]])
  col_names <- colnames(mat_df)[-1]
  mat_values <- as.matrix(mat_df[, -1, drop = FALSE])
  storage.mode(mat_values) <- "numeric"
  mat_values[is.na(mat_values)] <- 0
  rownames(mat_values) <- row_names
  colnames(mat_values) <- col_names

  # Detect matrix type based on values
  nonzero <- mat_values[mat_values != 0]
  if (length(nonzero) == 0) {
    matrix_type <- "binary"
  } else if (all(nonzero %in% c(1))) {
    matrix_type <- "binary"
  } else if (max(mat_values) > 1) {
    matrix_type <- "percentage"
    mat_values <- mat_values / 100
  } else {
    matrix_type <- "proportion"
  }

  # Keep original weights before binarising
  weights_matrix <- if (matrix_type != "binary") mat_values else NULL

  # Apply threshold and binarise for network construction
  binary_mat <- mat_values
  binary_mat[binary_mat <= threshold] <- 0
  binary_mat[binary_mat > 0] <- 1

  is_square <- (nrow(binary_mat) == ncol(binary_mat)) &&
    all(row_names == col_names)

  if (is_square) {
    net <- igraph::graph_from_adjacency_matrix(binary_mat, mode = "directed")
    all_species <- row_names
  } else {
    # Diet matrix: columns are predators, rows are prey
    all_species <- unique(c(col_names, row_names))
    net <- igraph::make_empty_graph(n = length(all_species), directed = TRUE)
    igraph::V(net)$name <- all_species

    idx <- which(binary_mat != 0, arr.ind = TRUE)
    if (nrow(idx) > 0) {
      edges <- as.vector(rbind(row_names[idx[, 1]], col_names[idx[, 2]]))
    } else {
      edges <- character(0)
    }
    if (length(edges) > 0) {
      net <- igraph::add_edges(net, edges)
    }
  }

  # Build info data frame
  if (!is.null(info_df) && "species" %in% colnames(info_df)) {
    info <- as.data.frame(info_df)
    missing_sp <- setdiff(all_species, info$species)
    if (length(missing_sp) > 0) {
      extra <- data.frame(species = missing_sp, stringsAsFactors = FALSE)
      info <- merge(info, extra, by = "species", all = TRUE)
    }
  } else {
    info <- data.frame(species = all_species, stringsAsFactors = FALSE)
  }

  # Auto-fill missing columns using functional group utilities
  if (!"fg" %in% colnames(info) || all(is.na(info$fg))) {
    info$fg <- factor(assign_functional_groups(info$species),
                      levels = get_functional_group_levels())
  } else {
    info$fg <- factor(info$fg, levels = get_functional_group_levels())
  }

  fg_char <- as.character(info$fg)

  if (!"meanB" %in% colnames(info) || all(is.na(info$meanB))) {
    info$meanB <- 1.0
  }
  if (!"bodymasses" %in% colnames(info) || all(is.na(info$bodymasses))) {
    info$bodymasses <- sapply(seq_len(nrow(info)), function(i) {
      estimate_body_mass_enhanced(info$species[i], fg_char[i])
    })
  }
  if (!"met.types" %in% colnames(info) || all(is.na(info$met.types))) {
    info$met.types <- sapply(fg_char, estimate_metabolic_type_by_fg)
  }
  if (!"efficiencies" %in% colnames(info) || all(is.na(info$efficiencies))) {
    info$efficiencies <- sapply(fg_char, estimate_efficiency_by_fg)
  }
  if (!"losses" %in% colnames(info) || all(is.na(info$losses))) {
    info$losses <- 0.1
  }

  rownames(info) <- make.unique(info$species)

  if (is_square) {
    igraph::V(net)$name <- all_species
  }

  list(net = net, info = info, weights = weights_matrix, matrix_type = matrix_type)
}

#' Remove disconnected (isolated) nodes from a network and its info table.
#'
#' @param net igraph network
#' @param info data.frame with a \code{species} column
#' @return List with filtered \code{net}, \code{info}, and character vector
#'   \code{removed} containing names of removed nodes
filter_disconnected <- function(net, info) {
  deg <- igraph::degree(net, mode = "all")
  isolated <- which(deg == 0)

  if (length(isolated) == 0) {
    return(list(net = net, info = info, removed = character(0)))
  }

  removed_names <- igraph::V(net)$name[isolated]
  net <- igraph::delete_vertices(net, isolated)
  info <- info[!(info$species %in% removed_names), , drop = FALSE]
  rownames(info) <- make.unique(info$species)

  list(net = net, info = info, removed = removed_names)
}

#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param net_reactive ReactiveVal holding the igraph network
#' @param info_reactive ReactiveVal holding the species info data frame
#' @param dashboard_trigger ReactiveVal to trigger dashboard updates
#' @param refresh_data_editor Function to refresh data editor tables
#' @param diet_matrix_reactive ReactiveVal to store the original diet proportion matrix
data_import_server <- function(input, output, session, net_reactive, info_reactive,
                                dashboard_trigger, refresh_data_editor,
                                diet_matrix_reactive = NULL) {

  # Pending import state
  pending <- reactiveValues(
    net = NULL, info = NULL, format = NULL, isolated = character(0),
    weights = NULL, matrix_type = "binary", raw_df = NULL
  )

  # Helper: assign colors and finalize import into the app
  finalize_import <- function(loaded_net, loaded_info, format_label, removed_msg = NULL) {
    fg_levels <- get_functional_group_levels()
    loaded_info$colfg <- sapply(as.character(loaded_info$fg), function(fg) {
      idx <- which(fg_levels == fg)
      if (length(idx) == 0) return("gray")
      COLOR_SCHEME[idx]
    })

    net_reactive(loaded_net)
    info_reactive(loaded_info)
    refresh_data_editor()

    # Store diet matrix if proportion data was imported
    if (!is.null(diet_matrix_reactive)) {
      diet_matrix_reactive(pending$weights)
    }

    has_diet <- !is.null(pending$weights)
    output$data_upload_status <- renderPrint({
      cat("SUCCESS:", format_label, "data loaded!\n\n")
      cat("Network:", igraph::vcount(loaded_net), "species,",
          igraph::ecount(loaded_net), "links\n")
      cat("Species info:", nrow(loaded_info), "rows\n")
      if (has_diet) {
        cat("Diet matrix: preserved (", pending$matrix_type, " values)\n")
      }
      if (!is.null(removed_msg)) cat("\n", removed_msg, "\n")
      cat("\nAll analysis tabs now use your uploaded data.\n")
    })
  }

  # Helper: check for disconnected nodes and show modal or finalize
  check_disconnected_and_finalize <- function() {
    deg <- igraph::degree(pending$net, mode = "all")
    isolated_names <- igraph::V(pending$net)$name[deg == 0]
    pending$isolated <- isolated_names

    if (length(isolated_names) > 0) {
      showModal(modalDialog(
        title = "Disconnected Nodes Detected",
        tagList(
          tags$p(tags$strong(length(isolated_names)),
                 " species have no trophic links (no edges):"),
          tags$div(
            style = "max-height: 200px; overflow-y: auto; background: #f8f9fa; padding: 8px; border-radius: 4px; margin-bottom: 12px;",
            tags$ul(lapply(isolated_names, tags$li))
          ),
          tags$p("Would you like to remove them from the network?")
        ),
        footer = tagList(
          actionButton("import_keep_all", "Keep All Nodes", icon = icon("check")),
          actionButton("import_remove_isolated", "Remove Disconnected",
                       icon = icon("trash"), class = "btn-danger")
        ),
        easyClose = FALSE
      ))
    } else {
      finalize_import(pending$net, pending$info, pending$format)
    }
  }

  # Status message
  output$data_upload_status <- renderPrint({
    if (is.null(input$data_file)) {
      cat("No file uploaded yet.\n\n")
      cat("Current dataset: Gulf of Riga (default)\n")
      cat("  - 34 species\n")
      cat("  - 207 trophic links\n")
      cat("  - 5 functional groups\n")
    } else {
      cat("File selected:", input$data_file$name, "\n")
      cat("File size:", round(input$data_file$size / 1024, 2), "KB\n\n")
      cat("Click 'Load Data' button to import.\n")
    }
  })

  # Show/hide diet matrix download button
  output$diet_download_ui <- renderUI({
    if (!is.null(diet_matrix_reactive) && !is.null(diet_matrix_reactive())) {
      tagList(
        hr(),
        tags$p(tags$strong("Diet Composition Matrix"),
               " (original proportions preserved)"),
        fluidRow(
          column(6,
            downloadButton("download_diet_csv", "Download as CSV",
                           icon = icon("file-csv"), class = "btn-info btn-sm")
          ),
          column(6,
            downloadButton("download_diet_xlsx", "Download as Excel",
                           icon = icon("file-excel"), class = "btn-success btn-sm")
          )
        )
      )
    }
  })

  # Download diet matrix as CSV
  output$download_diet_csv <- downloadHandler(
    filename = function() {
      paste0("diet_matrix_", format(Sys.time(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      mat <- diet_matrix_reactive()
      df <- data.frame(ScientificName = rownames(mat), mat, check.names = FALSE)
      write.csv(df, file, row.names = FALSE)
    }
  )

  # Download diet matrix as Excel
  output$download_diet_xlsx <- downloadHandler(
    filename = function() {
      paste0("diet_matrix_", format(Sys.time(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      mat <- diet_matrix_reactive()
      df <- data.frame(ScientificName = rownames(mat), mat, check.names = FALSE)
      openxlsx::write.xlsx(df, file)
    }
  )

  # ========================================================================
  # MAIN LOAD HANDLER
  # ========================================================================
  observeEvent(input$load_data, {
    req(input$data_file)

    tryCatch({
      file_path <- input$data_file$datapath
      file_ext <- tools::file_ext(input$data_file$name)

      output$data_upload_status <- renderPrint({
        cat("Processing file:", input$data_file$name, "\n")
        cat("Format:", toupper(file_ext), "\n\n")
        cat("Loading...")
      })

      # --- RData: load directly, no proportion/disconnected flow ---
      if (file_ext %in% c("Rdata", "rda")) {
        env <- new.env()
        load(file_path, envir = env)

        if (!exists("net", envir = env))
          stop("RData file must contain 'net' object (igraph network)")
        if (!exists("info", envir = env))
          stop("RData file must contain 'info' data frame")

        loaded_net <- igraph::upgrade_graph(env$net)
        loaded_info <- env$info

        if (is.null(igraph::V(loaded_net)$name) ||
            all(grepl("^[0-9]+$", igraph::V(loaded_net)$name))) {
          if ("species" %in% colnames(loaded_info)) {
            igraph::V(loaded_net)$name <- as.character(loaded_info$species)
          } else if (!is.null(rownames(loaded_info))) {
            igraph::V(loaded_net)$name <- rownames(loaded_info)
          }
        }

        pending$weights <- NULL
        pending$matrix_type <- "binary"
        finalize_import(loaded_net, loaded_info, "RData")
        return()
      }

      # --- CSV / Excel: parse into matrix ---
      if (file_ext %in% c("xlsx", "xls")) {
        if (!requireNamespace("readxl", quietly = TRUE))
          stop("Package 'readxl' required for Excel files.\nInstall with: install.packages('readxl')")

        sheets <- readxl::excel_sheets(file_path)
        if ("Network" %in% sheets && "Species_Info" %in% sheets) {
          mat_df <- as.data.frame(readxl::read_excel(file_path, sheet = "Network"))
          info_df <- as.data.frame(readxl::read_excel(file_path, sheet = "Species_Info"))
        } else {
          mat_df <- as.data.frame(readxl::read_excel(file_path, sheet = 1))
          info_df <- NULL
        }
        pending$format <- "Excel"
      } else if (file_ext == "csv") {
        mat_df <- read.csv(file_path, check.names = FALSE, fileEncoding = "UTF-8-BOM")
        info_df <- NULL
        pending$format <- "CSV"
      } else {
        stop("Unsupported file format")
      }

      # Store raw data frame for re-parsing with threshold
      pending$raw_df <- mat_df
      pending$raw_info_df <- info_df

      # Initial parse (threshold = 0)
      result <- parse_adjacency_df(mat_df, info_df, threshold = 0)
      pending$net <- result$net
      pending$info <- result$info
      pending$weights <- result$weights
      pending$matrix_type <- result$matrix_type

      # --- If proportion/percentage matrix: prompt user first ---
      if (result$matrix_type != "binary") {
        type_label <- if (result$matrix_type == "percentage") {
          "percentage (0-100, converted to proportions)"
        } else {
          "proportion (0-1)"
        }
        n_links <- igraph::ecount(result$net)

        showModal(modalDialog(
          title = "Diet Composition Matrix Detected",
          tagList(
            tags$p("The uploaded matrix contains ", tags$strong(type_label),
                   " values, not just binary 0/1."),
            tags$p("The network will be built from binary interactions. ",
                   "Original proportions will be preserved and available for download."),
            tags$hr(),
            tags$p(tags$strong("Current links: "), n_links,
                   " (any non-zero value = link)"),
            numericInput("diet_threshold", "Minimum proportion to count as a link:",
                         value = 0, min = 0, max = 1, step = 0.01),
            tags$p(class = "text-muted",
                   "Set to 0 to keep all non-zero interactions. ",
                   "Increase to filter out weak/uncertain diet items.")
          ),
          footer = tagList(
            actionButton("diet_import_cancel", "Cancel", icon = icon("times")),
            actionButton("diet_import_confirm", "Import",
                         icon = icon("check"), class = "btn-primary")
          ),
          easyClose = FALSE
        ))
      } else {
        # Binary matrix — go straight to disconnected-node check
        check_disconnected_and_finalize()
      }

    }, error = function(e) {
      output$data_upload_status <- renderPrint({
        cat("ERROR loading data:\n\n")
        cat(e$message, "\n\n")
        cat("Please check:\n")
        cat("  - File format is correct\n")
        cat("  - Required objects/sheets are present\n")
        cat("  - Data matches expected structure\n")
      })
    })
  })

  # ========================================================================
  # PROPORTION MATRIX MODAL HANDLERS
  # ========================================================================

  # User confirms proportion import (possibly with threshold)
  observeEvent(input$diet_import_confirm, {
    removeModal()
    threshold <- input$diet_threshold %||% 0

    # Re-parse with user-chosen threshold
    result <- parse_adjacency_df(pending$raw_df, pending$raw_info_df, threshold = threshold)
    pending$net <- result$net
    pending$info <- result$info
    pending$weights <- result$weights
    pending$matrix_type <- result$matrix_type

    check_disconnected_and_finalize()
  })

  # User cancels proportion import
  observeEvent(input$diet_import_cancel, {
    removeModal()
    output$data_upload_status <- renderPrint({
      cat("Import cancelled.\n")
    })
  })

  # ========================================================================
  # DISCONNECTED NODES MODAL HANDLERS
  # ========================================================================

  observeEvent(input$import_keep_all, {
    removeModal()
    req(pending$net)
    n_iso <- length(pending$isolated)
    msg <- if (n_iso > 0) paste0("Kept all nodes (including ", n_iso, " disconnected).") else NULL
    finalize_import(pending$net, pending$info, pending$format, msg)
  })

  observeEvent(input$import_remove_isolated, {
    removeModal()
    req(pending$net)
    filt <- filter_disconnected(pending$net, pending$info)
    removed_msg <- paste0(
      "Removed ", length(filt$removed), " disconnected node(s):\n  ",
      paste(head(filt$removed, 20), collapse = ", "),
      if (length(filt$removed) > 20) paste0(" ... and ", length(filt$removed) - 20, " more")
    )
    finalize_import(filt$net, filt$info, pending$format, removed_msg)
  })
}
