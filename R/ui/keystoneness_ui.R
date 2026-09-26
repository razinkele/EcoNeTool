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
                <li><strong>Mixed Trophic Impact (MTI):</strong> net effect (direct + indirect) of a small increase
                of one species on every other (Ulanowicz &amp; Puccia 1990). Producers now show positive impacts
                on their consumers, and predators negative impacts on their prey.</li>
                <li><strong>Overall Effect (&epsilon;):</strong> sqrt of the sum of squared MTI values of a
                species on all others (its own self-impact excluded)</li>
                <li><strong>Keystoneness Index (KS):</strong> log(&epsilon; &times; (1 - p)), where p is the species'
                share of total biomass (Libralato et al. 2006). Higher KS = larger impact for its biomass.</li>
              </ul>

              <h5>Species Classifications:</h5>
              <ul>
                <li><strong>Keystone:</strong> KS in the top quartile of the web and biomass &lt; 5% of total</li>
                <li><strong>Dominant:</strong> KS in the top quartile and biomass &ge; 5% of total</li>
                <li><strong>Other:</strong> every other species with a defined KS</li>
                <li><strong>Undefined:</strong> KS cannot be computed (no impact, or the only biomass in the web)</li>
              </ul>

              <h5>Approximations used when EwE data are missing:</h5>
              <ul>
                <li>Without Q/B (consumption/biomass), predation pressure on a prey is apportioned by predator
                biomass instead of predator consumption (B &times; Q/B).</li>
                <li>Without diet proportions (e.g. binary metawebs and trait-based webs), each predator's diet is
                split equally among its prey.</li>
                <li>Detritus is treated as an ordinary prey; EwE routes it through detritus fate, so impacts on
                and of detritus are indicative only.</li>
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
            helpText("Keystone species appear top-left: KS above the dashed top-quartile line, biomass left of 5%")
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
                <li>Rows = Impacting species (impactor)</li>
                <li>Columns = Impacted species</li>
                <li>The diagonal (self-impact) is left blank</li>
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
