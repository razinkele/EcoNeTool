#' Network Comparison Server Module
#'
#' Keeps two session-scoped snapshot slots (A and B) and renders the output of
#' compare_networks() for them. Flat input/output pattern (no moduleServer/NS),
#' like the other server modules; every ID is prefixed `cmp_`.
#'
#' Both slots are snapshots: loading a new web into the live net_reactive never
#' changes a slot already stored, so A survives while the user imports B.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param net_reactive reactiveVal holding the live igraph network
#' @param info_reactive reactiveVal holding the live species info data frame
#' @return (invisibly) list(slot_a, slot_b, comparison): the two snapshot
#'   reactiveVals and the comparison reactive, for tests and other modules.
comparison_server <- function(input, output, session, net_reactive, info_reactive) {

  slot_a <- reactiveVal(NULL)
  slot_b <- reactiveVal(NULL)
  slots <- list(a = slot_a, b = slot_b)

  # --------------------------------------------------------------------------
  # Example web choices (computed once per session)
  # --------------------------------------------------------------------------
  example_files <- list_example_networks()
  example_choices <- stats::setNames(example_files, tools::file_path_sans_ext(basename(example_files)))
  for (slot in c("a", "b")) {
    updateSelectInput(session, paste0("cmp_example_", slot), choices = example_choices)
  }

  # --------------------------------------------------------------------------
  # Slot filling
  # --------------------------------------------------------------------------
  store_slot <- function(slot, net, info, label) {
    slots[[slot]](list(net = net, info = info, label = label,
                       stored_at = format(Sys.time(), "%H:%M:%S")))
    showNotification(sprintf("Network %s set: %s (%d species, %d links)", toupper(slot), label,
                             igraph::vcount(net), igraph::ecount(net)),
                     type = "message", duration = 4)
  }

  lapply(c("a", "b"), function(slot) {
    observeEvent(input[[paste0("cmp_use_current_", slot)]], {
      net <- net_reactive()
      info <- info_reactive()
      if (is.null(net) || !igraph::is_igraph(net)) {
        showNotification("No network is loaded. Use Data Import first.", type = "warning")
        return()
      }
      store_slot(slot, net, info, sprintf("Current network (%d species)", igraph::vcount(net)))
    })

    observeEvent(input[[paste0("cmp_load_example_", slot)]], {
      path <- input[[paste0("cmp_example_", slot)]]
      if (is.null(path) || !nzchar(path)) {
        showNotification("Choose an example web first.", type = "warning")
        return()
      }
      web <- tryCatch(
        load_example_network(path),
        error = function(e) {
          warning(sprintf("[comparison] loading example '%s' failed: %s",
                          basename(path), conditionMessage(e)), call. = FALSE)
          showNotification(paste("Could not load example:", conditionMessage(e)), type = "error")
          NULL
        }
      )
      if (is.null(web)) return()
      store_slot(slot, web$net, web$info, web$label)
    })

    output[[paste0("cmp_slot_summary_", slot)]] <- renderPrint({
      s <- slots[[slot]]()
      if (is.null(s)) {
        cat("Empty. Store the current network or load an example.\n")
      } else {
        cat(s$label, "\n", sep = "")
        cat("Species: ", igraph::vcount(s$net), "   Links: ", igraph::ecount(s$net), "\n", sep = "")
        cat("Biomass (meanB): ", if ("meanB" %in% names(s$info)) "yes" else "no", "\n", sep = "")
        cat("Stored at ", s$stored_at, "\n", sep = "")
      }
    })
  })

  # --------------------------------------------------------------------------
  # Comparison
  # --------------------------------------------------------------------------
  comparison <- reactive({
    a <- slot_a()
    b <- slot_b()
    req(a, b)
    withProgress(message = "Comparing networks...", {
      compare_networks(a$net, a$info, b$net, b$info, a$label, b$label)
    })
  })

  # --------------------------------------------------------------------------
  # Value boxes
  # --------------------------------------------------------------------------
  empty_box <- function(subtitle, icon_name, color) {
    valueBox(value = "-", subtitle = subtitle, icon = icon(icon_name), color = color)
  }

  output$cmp_box_shared_species <- renderValueBox({
    cmp <- tryCatch(comparison(), error = function(e) NULL)
    if (is.null(cmp)) return(empty_box("Shared species", "handshake", "primary"))
    valueBox(value = length(cmp$species$shared),
             subtitle = sprintf("Shared species (Jaccard %.2f)", cmp$species$jaccard),
             icon = icon("handshake"), color = "primary")
  })

  output$cmp_box_only_a <- renderValueBox({
    cmp <- tryCatch(comparison(), error = function(e) NULL)
    if (is.null(cmp)) return(empty_box("Only in A", "arrow-left", "info"))
    valueBox(value = length(cmp$species$only_a), subtitle = "Species only in A",
             icon = icon("arrow-left"), color = "info")
  })

  output$cmp_box_only_b <- renderValueBox({
    cmp <- tryCatch(comparison(), error = function(e) NULL)
    if (is.null(cmp)) return(empty_box("Only in B", "arrow-right", "warning"))
    valueBox(value = length(cmp$species$only_b), subtitle = "Species only in B",
             icon = icon("arrow-right"), color = "warning")
  })

  output$cmp_box_link_jaccard <- renderValueBox({
    cmp <- tryCatch(comparison(), error = function(e) NULL)
    if (is.null(cmp)) return(empty_box("Link similarity", "link", "success"))
    valueBox(value = sprintf("%.2f", cmp$links$jaccard),
             subtitle = sprintf("Link Jaccard (%d shared links)", nrow(cmp$links$shared)),
             icon = icon("link"), color = "success")
  })

  # --------------------------------------------------------------------------
  # Tables
  # --------------------------------------------------------------------------
  output$cmp_metrics_table <- DT::renderDataTable({
    cmp <- comparison()
    df <- cmp$metrics
    names(df) <- c("Metric", cmp$labels[["a"]], cmp$labels[["b"]], "Delta (B - A)")
    DT::datatable(df, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE),
                  escape = TRUE) |>
      DT::formatRound(columns = 2:4, digits = 3)
  })

  output$cmp_species_summary <- renderPrint({
    cmp <- comparison()
    show_list <- function(title, x) {
      cat(title, " (", length(x), "):\n", sep = "")
      if (length(x) == 0) cat("  none\n") else cat(paste0("  ", x), sep = "\n")
    }
    cat("A: ", cmp$labels[["a"]], "\nB: ", cmp$labels[["b"]], "\n\n", sep = "")
    cat(sprintf("Species Jaccard similarity: %.3f\n\n", cmp$species$jaccard))
    show_list("Shared", cmp$species$shared)
    cat("\n")
    show_list("Only in A", cmp$species$only_a)
    cat("\n")
    show_list("Only in B", cmp$species$only_b)
  })

  output$cmp_links_summary <- renderPrint({
    cmp <- comparison()
    cat(sprintf("Link Jaccard similarity (shared species only): %.3f\n", cmp$links$jaccard))
    cat("Shared links:     ", nrow(cmp$links$shared), "\n", sep = "")
    cat("Only in A:        ", nrow(cmp$links$only_a), "\n", sep = "")
    cat("Only in B:        ", nrow(cmp$links$only_b), "\n", sep = "")
    cat("Links involving a species absent from the other web: A ",
        cmp$links$n_unshared_species_a, ", B ", cmp$links$n_unshared_species_b, "\n", sep = "")
  })

  link_differences <- reactive({
    cmp <- comparison()
    rbind(
      cbind(status = rep("Only in A", nrow(cmp$links$only_a)), cmp$links$only_a),
      cbind(status = rep("Only in B", nrow(cmp$links$only_b)), cmp$links$only_b),
      cbind(status = rep("Shared", nrow(cmp$links$shared)), cmp$links$shared)
    )
  })

  output$cmp_links_table <- DT::renderDataTable({
    df <- link_differences()
    names(df) <- c("Status", "Prey", "Predator")
    DT::datatable(df, rownames = FALSE, escape = TRUE,
                  options = list(pageLength = 10, lengthMenu = c(10, 25, 50)))
  })

  output$cmp_per_species_table <- DT::renderDataTable({
    cmp <- comparison()
    df <- cmp$per_species[, c("species", "prey_a", "prey_b", "delta_prey",
                              "predators_a", "predators_b", "delta_predators",
                              "tl_a", "tl_b", "delta_tl")]
    names(df) <- c("Species", "Prey A", "Prey B", "Δ prey",
                   "Predators A", "Predators B", "Δ predators",
                   "TL A", "TL B", "Δ TL")
    DT::datatable(df, rownames = FALSE, escape = TRUE,
                  options = list(pageLength = 15, lengthMenu = c(15, 50, 100))) |>
      DT::formatRound(columns = 8:10, digits = 3)
  })

  # --------------------------------------------------------------------------
  # Downloads
  # --------------------------------------------------------------------------
  output$cmp_download_species <- downloadHandler(
    filename = function() paste0("network_comparison_species_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      cmp <- comparison()
      utils::write.csv(cmp$per_species, file, row.names = FALSE)
    }
  )

  output$cmp_download_links <- downloadHandler(
    filename = function() paste0("network_comparison_links_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      utils::write.csv(link_differences(), file, row.names = FALSE)
    }
  )

  invisible(list(slot_a = slot_a, slot_b = slot_b, comparison = comparison))
}
