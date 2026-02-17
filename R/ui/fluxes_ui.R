#' Energy Fluxes Tab UI
#'
#' Creates the energy fluxes tab.
#'
#' @return A tabItem for energy fluxes
fluxes_ui <- function() {
      # ENERGY FLUXES TAB
      # ========================================================================
      tabItem(
        tabName = "fluxes",

        fluidRow(
          box(
            title = "Energy Flux Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            HTML("
              <p>Energy fluxes are calculated using metabolic theory of ecology. Fluxes represent
              biomass flow between species based on allometric scaling and temperature-adjusted
              metabolic rates (T=3.5°C, Gulf of Riga spring conditions).</p>
              <p><strong>Units:</strong> kJ/day/km²</p>
              <p><strong>Note:</strong> Flux values span many orders of magnitude (10<sup>-10</sup> to 10<sup>-1</sup>),
              reflecting the wide range of interaction strengths in the food web.</p>
            ")
          )
        ),

        fluidRow(
          tabBox(
            width = 12,
            id = "fluxes_tabs",

            # TAB 1: Flux Network
            tabPanel(
              title = "Flux-weighted Network",
              icon = icon("bolt"),

              br(),
              box(
                title = "Flux-weighted Network",
                status = "warning",
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                maximizable = TRUE,
                HTML("<p>Edge widths proportional to energy flux magnitude. Hover over edges to see exact values.</p>"),
                visNetworkOutput("flux_network_plot", height = "600px")
              )
            ),

            # TAB 2: Flux Heatmap
            tabPanel(
              title = "Flux Matrix Heatmap",
              icon = icon("fire"),

              br(),
              box(
                title = "Flux Matrix Heatmap (Log-transformed)",
                status = "danger",
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                maximizable = TRUE,
                HTML("<p>Color intensity shows log-transformed flux values. Darker colors indicate stronger energy flows.</p>"),
                plotOutput("flux_heatmap", height = "500px")
              )
            ),

            # TAB 3: Flux Indicators
            tabPanel(
              title = "Flux Indicators",
              icon = icon("chart-bar"),

              br(),
              box(
                title = "Link-weighted Flux Indicators",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                HTML("
                  <p>Shannon diversity indices calculated from energy flux distributions.</p>
                  <ul>
                    <li><strong>Flux diversity:</strong> Distribution evenness of energy flows</li>
                    <li><strong>Effective number of fluxes:</strong> Equivalent number of equally strong flows</li>
                  </ul>
                "),
                verbatimTextOutput("flux_indicators")
              )
            )
          )
        )
      )

      # ========================================================================
}
