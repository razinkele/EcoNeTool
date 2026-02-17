#' Internal Data Editor Tab UI
#'
#' Creates the data editor tab.
#'
#' @return A tabItem for data editor
dataeditor_ui <- function() {
      # INTERNAL DATA EDITOR TAB
      # ========================================================================
      tabItem(
        tabName = "dataeditor",

        fluidRow(
          box(
            title = "Internal Data Editor",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            HTML("
              <h4>Edit Internal Datasheets</h4>
              <p>This tab allows you to directly edit the two main internal datasheets:</p>
              <ul>
                <li><strong>Species Information:</strong> Species attributes including biomass, functional groups, body masses, and metabolic parameters</li>
                <li><strong>Network Adjacency Matrix:</strong> The food web structure showing who eats whom</li>
              </ul>
              <p><strong>Note:</strong> Changes are applied in real-time. Use the 'Update Network' button to refresh all visualizations after editing.</p>
            ")
          )
        ),

        fluidRow(
          tabBox(
            width = 12,
            id = "dataeditor_tabs",

            tabPanel(
              title = "Species Information Table",
              icon = icon("table"),
              HTML("
                <p>Edit species attributes. Double-click cells to edit values.</p>
                <p><em>Required columns: meanB, fg, bodymasses, met.types, efficiencies</em></p>
                <details style='margin-bottom: 10px;'>
                  <summary style='cursor: pointer; color: #337ab7;'><strong>Column Descriptions (click to expand)</strong></summary>
                  <ul style='font-size: 12px; margin-top: 5px;'>
                    <li><strong>species:</strong> Species name or identifier</li>
                    <li><strong>meanB:</strong> Mean biomass of the species (g/km²)</li>
                    <li><strong>fg:</strong> Functional group (Fish, Benthos, Phytoplankton, Zooplankton, Detritus)</li>
                    <li><strong>bodymasses:</strong> Average body mass of individual organism (grams)</li>
                    <li><strong>met.types:</strong> Metabolic type (invertebrates, ectotherm vertebrates, Other)</li>
                    <li><strong>efficiencies:</strong> Assimilation efficiency (0-1, proportion of consumed energy assimilated)</li>
                    <li><strong>taxon:</strong> Taxonomic classification</li>
                    <li><strong>nbY:</strong> Number of years recorded in the dataset</li>
                    <li><strong>losses:</strong> Metabolic losses (J/sec) calculated from body mass and temperature</li>
                    <li><strong>org.type:</strong> Organism type classification</li>
                  </ul>
                </details>
              "),
              DT::dataTableOutput("species_info_table"),
              br(),
              actionButton("save_species_info", "Save Species Info", icon = icon("save"), class = "btn-success"),
              verbatimTextOutput("species_info_status")
            ),

            tabPanel(
              title = "Network Adjacency Matrix",
              icon = icon("project-diagram"),
              HTML("
                <p>Edit the food web structure. Values should be 0 (no interaction) or 1 (predator eats prey).</p>
                <p><strong>Tip:</strong> Hover over species names (underlined) to see their role in the food web.</p>
                <p><em>Rows = Predators, Columns = Prey. Value of 1 in row i, column j means species i eats species j.</em></p>
              "),
              DT::dataTableOutput("network_matrix_table"),
              br(),
              actionButton("save_network_matrix", "Save Network Matrix", icon = icon("save"), class = "btn-success"),
              actionButton("update_network", "Update Network from Matrix", icon = icon("refresh"), class = "btn-primary"),
              verbatimTextOutput("network_matrix_status")
            )
          )
        )
      )

      # ========================================================================
}
