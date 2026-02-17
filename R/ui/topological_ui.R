#' Topological Indicators Tab UI
#'
#' Creates the topological metrics tab.
#'
#' @return A tabItem for topological indicators
topological_ui <- function() {
      # TOPOLOGICAL INDICATORS TAB
      # ========================================================================
      tabItem(
        tabName = "topological",

        fluidRow(
          box(
            title = "Topological Indicators (Qualitative Metrics)",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            HTML("
              <p>These metrics describe the structural properties of the food web network without
              considering node weights (biomass).</p>
              <ul>
                <li><strong>S:</strong> Species richness (number of taxa)</li>
                <li><strong>C:</strong> Connectance (proportion of realized links)</li>
                <li><strong>G:</strong> Generality (mean number of prey per predator)</li>
                <li><strong>V:</strong> Vulnerability (mean number of predators per prey)</li>
                <li><strong>ShortPath:</strong> Mean shortest path length</li>
                <li><strong>TL:</strong> Mean trophic level</li>
                <li><strong>Omni:</strong> Omnivory index (mean SD of prey trophic levels)</li>
              </ul>
            ")
          )
        ),

        fluidRow(
          box(
            title = "Calculated Metrics",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("topo_indicators")
          )
        )
      )

      # ========================================================================
}
