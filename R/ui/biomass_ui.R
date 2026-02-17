#' Biomass Analysis Tab UI
#'
#' Creates the biomass analysis tab with tabbed interface.
#'
#' @return A tabItem for biomass analysis
biomass_ui <- function() {
      # BIOMASS ANALYSIS TAB
      # ========================================================================
      tabItem(
        tabName = "biomass",

        fluidRow(
          tabBox(
            width = 12,
            id = "biomass_tabs",

            # TAB 1: Biomass Network Visualization
            tabPanel(
              title = "Network View",
              icon = icon("project-diagram"),

              br(),
              box(
                title = "Food Web with Biomass-Scaled Nodes",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                maximizable = TRUE,
                HTML("<p>Interactive network visualization with node sizes representing biomass (logarithmic scaling).
                      Nodes are arranged hierarchically by precise trophic level: highest trophic levels at the top, lowest at the bottom.
                      Minor differences in trophic level are visible through vertical positioning.</p>"),
                visNetworkOutput("foodweb_biomass_visnet", height = "600px")
              )
            ),

            # TAB 2: Biomass Distribution
            tabPanel(
              title = "Distribution",
              icon = icon("chart-bar"),

              br(),
              fluidRow(
                column(6,
                  box(
                    title = "Biomass Distribution by Functional Group",
                    status = "success",
                    solidHeader = TRUE,
                    width = 12,
                    collapsible = TRUE,
                    HTML("<p>Box plot showing biomass distribution across functional groups.</p>"),
                    plotOutput("biomass_boxplot", height = "400px")
                  )
                ),
                column(6,
                  box(
                    title = "Biomass Percentage by Functional Group",
                    status = "info",
                    solidHeader = TRUE,
                    width = 12,
                    collapsible = TRUE,
                    HTML("<p>Relative contribution of each functional group to total biomass.</p>"),
                    plotOutput("biomass_barplot", height = "400px")
                  )
                )
              )
            ),

            # TAB 3: Quantitative Metrics
            tabPanel(
              title = "Metrics",
              icon = icon("calculator"),

              br(),
              box(
                title = "Node-weighted Indicators (Quantitative Metrics)",
                status = "warning",
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                HTML("
                  <p>These metrics account for the relative importance of species based on their biomass.</p>
                  <ul>
                    <li><strong>nwC:</strong> Node-weighted connectance - weighted measure of network connectivity</li>
                    <li><strong>nwG:</strong> Node-weighted generality - average number of prey per predator, weighted by biomass</li>
                    <li><strong>nwV:</strong> Node-weighted vulnerability - average number of predators per prey, weighted by biomass</li>
                    <li><strong>nwTL:</strong> Node-weighted mean trophic level - average trophic position weighted by biomass</li>
                  </ul>
                "),
                verbatimTextOutput("node_weighted_indicators")
              )
            )
          )
        )
      )

      # ========================================================================
}
