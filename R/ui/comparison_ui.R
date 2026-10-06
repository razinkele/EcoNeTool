#' Network Comparison Tab UI
#'
#' Two snapshot slots (A and B) filled from the live network or from a bundled
#' example web, followed by metric, species, link and per-species comparisons.
#' All element IDs are prefixed `cmp_` (outputs live on the shared output object).
#'
#' @return A tabItem for network comparison
comparison_ui <- function() {
  slot_box <- function(slot, title, status) {
    box(
      title = title,
      status = status,
      solidHeader = TRUE,
      width = 6,
      actionButton(paste0("cmp_use_current_", slot),
                   "Use current network",
                   icon = icon("arrow-down"), class = "btn-primary btn-sm"),
      tags$span(style = "margin: 0 8px;", "or"),
      div(
        style = "display: inline-block; vertical-align: middle; min-width: 220px;",
        selectInput(paste0("cmp_example_", slot), NULL, choices = character(0), width = "100%")
      ),
      actionButton(paste0("cmp_load_example_", slot), "Load example",
                   icon = icon("folder-open"), class = "btn-outline-secondary btn-sm"),
      tags$hr(),
      verbatimTextOutput(paste0("cmp_slot_summary_", slot))
    )
  }

  tabItem(
    tabName = "comparison",

    fluidRow(
      box(
        title = "Network Comparison",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        HTML("
          <p>Place two food webs side by side and see how they differ: before/after a
          perturbation, two regions, or an ECOPATH model against a trait-built web.</p>
          <ol>
            <li>Load the first web through <strong>Data Import</strong> (or pick a bundled
            example) and store it as <strong>Network A</strong>.</li>
            <li>Load the second web and store it as <strong>Network B</strong>.</li>
            <li>The tables below update automatically. Deltas are always <em>B &minus; A</em>.</li>
          </ol>
          <p class='text-muted' style='font-size: 0.9em;'>Species are matched by name
          (case and surrounding whitespace ignored). Links are compared among shared species
          only; links touching a species the other web lacks are counted separately.</p>
        ")
      )
    ),

    fluidRow(
      slot_box("a", "Network A", "info"),
      slot_box("b", "Network B", "warning")
    ),

    fluidRow(
      valueBoxOutput("cmp_box_shared_species", width = 3),
      valueBoxOutput("cmp_box_only_a", width = 3),
      valueBoxOutput("cmp_box_only_b", width = 3),
      valueBoxOutput("cmp_box_link_jaccard", width = 3)
    ),

    fluidRow(
      box(
        title = "Topological indicators",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        p(class = "text-muted",
          "S species richness, C connectance, G generality, V vulnerability, ShortPath mean ",
          "shortest path, TL mean trophic level, Omni omnivory. nw* rows are biomass-weighted ",
          "and appear only when both webs carry meanB."),
        DT::dataTableOutput("cmp_metrics_table")
      )
    ),

    fluidRow(
      box(
        title = "Species composition",
        status = "info",
        solidHeader = TRUE,
        width = 6,
        collapsible = TRUE,
        verbatimTextOutput("cmp_species_summary")
      ),
      box(
        title = "Trophic links among shared species",
        status = "info",
        solidHeader = TRUE,
        width = 6,
        collapsible = TRUE,
        verbatimTextOutput("cmp_links_summary"),
        DT::dataTableOutput("cmp_links_table")
      )
    ),

    fluidRow(
      box(
        title = "Shared species: prey, predators and trophic level",
        status = "success",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        DT::dataTableOutput("cmp_per_species_table"),
        tags$hr(),
        downloadButton("cmp_download_species", "Download per-species CSV", class = "btn-sm"),
        downloadButton("cmp_download_links", "Download link differences CSV", class = "btn-sm")
      )
    )
  )
}
