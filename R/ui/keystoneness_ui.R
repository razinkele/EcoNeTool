#' Keystoneness Analysis Tab UI
#'
#' Creates the keystoneness analysis tab.
#'
#' @return A tabItem for keystoneness analysis
keystoneness_ui <- function() {
      # KEYSTONENESS ANALYSIS TAB
      # ========================================================================
      tabItem(
        tabName = "keystoneness",

        fluidRow(
          box(
            title = "Keystoneness Analysis (ECOPATH Method)",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            HTML("
              <h4>Identifying Keystone Species</h4>
              <p>Keystoneness analysis identifies species with disproportionately large effects on ecosystem
              structure and function relative to their biomass. This analysis follows the ECOPATH methodology
              using Mixed Trophic Impact (MTI) calculations.</p>

              <h5>Key Concepts:</h5>
              <ul>
                <li><strong>Mixed Trophic Impact (MTI):</strong> Measures direct and indirect effects of one species on all others</li>
                <li><strong>Overall Effect:</strong> Sum of absolute MTI values (total ecosystem impact)</li>
                <li><strong>Keystoneness Index:</strong> Ratio of impact to biomass (high values = keystone species)</li>
              </ul>

              <h5>Species Classifications:</h5>
              <ul>
                <li><strong>Keystone:</strong> High impact, low biomass (KS > 1, biomass < 5% of total)</li>
                <li><strong>Dominant:</strong> High impact, high biomass (KS > 0, biomass ≥ 5% of total)</li>
                <li><strong>Rare:</strong> Low impact, low biomass</li>
              </ul>

              <p><em>Reference: Libralato et al. (2006). Ecological Modelling, 195(3-4), 153-171.</em></p>
            ")
          )
        ),

        fluidRow(
          box(
            title = "Keystoneness Index Rankings",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            icon = icon("ranking-star"),
            DT::dataTableOutput("keystoneness_table"),
            helpText("Species ranked by keystoneness index (highest to lowest)")
          ),
          box(
            title = "Keystoneness vs Biomass Plot",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            plotOutput("keystoneness_plot", height = "400px"),
            helpText("Keystone species appear in upper-left (high impact, low biomass)")
          )
        ),

        fluidRow(
          box(
            title = "Mixed Trophic Impact (MTI) Heatmap",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            maximizable = TRUE,
            plotOutput("mti_heatmap", height = "600px"),
            HTML("
              <p><strong>How to read:</strong></p>
              <ul style='font-size: 12px;'>
                <li>Rows = Impacted species</li>
                <li>Columns = Impacting species (impactor)</li>
                <li>Red = Negative impact (impactor decreases impacted)</li>
                <li>Blue = Positive impact (impactor increases impacted)</li>
                <li>Values represent net effect through direct and indirect pathways</li>
              </ul>
            ")
          )
        ),

        fluidRow(
          box(
            title = "Top Keystone Species Details",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            verbatimTextOutput("keystone_summary")
          )
        )
      )

      # ========================================================================
}
