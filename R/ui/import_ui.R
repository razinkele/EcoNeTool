#' Data Import Tab UI
#'
#' Creates the data import tab with options for loading food web data.
#'
#' @return A tabItem for data import
import_ui <- function() {
      # DATA IMPORT TAB
      # ========================================================================
      tabItem(
        tabName = "import",

        fluidRow(
          tabBox(
            width = 12,
            id = "import_tabs",

            # TAB 1: General Import (RData/CSV/Excel)
            tabPanel(
              title = "General Import",
              icon = icon("upload"),

              br(),

              # Export Current Metaweb Section
              fluidRow(
                box(
                  title = "Export Current Metaweb",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  icon = icon("download"),
                  collapsed = TRUE,
                  collapsible = TRUE,

                  p("Export the currently loaded metaweb to RDA format (compatible with BalticFW.Rdata structure)."),

                  fluidRow(
                    column(4,
                      textInput("export_metaweb_name", "File Name (without extension):",
                                value = "my_metaweb", placeholder = "my_metaweb")
                    ),
                    column(4,
                      br(),
                      downloadButton("download_current_metaweb", "Export as RData",
                                   icon = icon("download"), class = "btn-primary")
                    ),
                    column(4,
                      br(),
                      helpText("Format: Frelat & Kortsch (BalticFW.Rdata compatible)")
                    )
                  ),

                  hr(),

                  HTML("
                    <h5><i class='fa fa-info-circle'></i> Exported File Structure:</h5>
                    <ul>
                      <li><strong>net</strong>: igraph network object with trophic interactions</li>
                      <li><strong>info</strong>: species data frame with:
                        <ul>
                          <li>species, functional group (fg), biomass (meanB)</li>
                          <li>body masses, metabolic types, efficiencies</li>
                          <li>taxon, organism type, metabolic losses</li>
                        </ul>
                      </li>
                    </ul>
                    <p class='text-muted'>
                      <i class='fa fa-lightbulb'></i>
                      The exported file can be re-imported using 'Upload Your Data' or loaded with <code>load('my_metaweb.Rdata')</code> in R.
                    </p>
                  ")
                )
              ),

              br(),

              fluidRow(
                column(6,
                  box(
                    title = "Upload Your Data",
                    status = "success",
                    solidHeader = TRUE,
                    width = 12,
                    fileInput(
                      "data_file",
                      "Choose File (Excel, CSV, or RData)",
                      accept = c(
                        ".xlsx", ".xls",
                        ".csv",
                        ".Rdata", ".rda"
                      ),
                      multiple = FALSE
                    ),
                    helpText("Maximum file size: 10 MB"),
                    br(),
                    actionButton("load_data", "Load Data", icon = icon("upload"), class = "btn-primary"),
                    hr(),
                    verbatimTextOutput("data_upload_status")
                  )
                ),
                column(6,
                  box(
                    title = "Data Validation",
                    status = "warning",
                    solidHeader = TRUE,
                    width = 12,
                    HTML("
                      <h5>Data Requirements:</h5>
                      <ul>
                        <li>✓ Species names must match between network and info</li>
                        <li>✓ Network must be square (same row/column names)</li>
                        <li>✓ Biomass values must be positive</li>
                        <li>✓ Functional groups should be consistent</li>
                        <li>✓ Losses and efficiencies: 0-1 range</li>
                        <li>✓ At least 3 species recommended</li>
                      </ul>
                      <h5>After Upload:</h5>
                      <p>Once your data is loaded, all analysis tabs will automatically use your uploaded data.</p>
                      <h5>Reset to Default:</h5>
                      <p>Refresh the page to reload the default Gulf of Riga dataset.</p>
                    ")
                  )
                )
              )
            ),

            # TAB 2: ECOPATH Native Database
            tabPanel(
              title = "ECOPATH Native",
              icon = icon("database"),

              br(),
              fluidRow(
                column(6,
                  box(
                    title = tagList(
                      "Import ECOPATH Native Database",
                      tags$a(
                        href = "#",
                        onclick = "return false;",
                        style = "margin-left: 10px; color: #999;",
                        title = "Import ECOPATH Native Files (.ewemdb, .eweaccdb, .mdb). Upload your ECOPATH with Ecosim database file directly. Example: 'coast 2011-04-10 10.00.ewemdb'",
                        icon("info-circle")
                      )
                    ),
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    fileInput(
                      "ecopath_native_file",
                      "ECOPATH Database File",
                      accept = c(".ewemdb", ".eweaccdb", ".mdb", ".eiidb", ".accdb"),
                      multiple = FALSE
                    ),
                    uiOutput("taxonomic_api_checkbox_ui"),
                    tags$div(
                      id = "emodnet_habitat_section",
                      style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
                      checkboxInput(
                        "enable_emodnet_habitat",
                        HTML("<strong>Add EMODnet Habitat Data</strong> <span style='font-size: 11px; color: #666;'>(EUNIS codes, substrate, depth zones)</span>"),
                        value = FALSE
                      ),
                      conditionalPanel(
                        condition = "input.enable_emodnet_habitat == true",
                        tags$div(
                          style = "padding-left: 20px;",
                          HTML("<p style='font-size: 12px; margin: 5px 0;'>Enter sampling location coordinates:</p>"),
                          fluidRow(
                            column(6,
                              numericInput(
                                "sampling_latitude",
                                "Latitude (°N)",
                                value = 54.5189,
                                min = 53,
                                max = 66,
                                step = 0.0001
                              )
                            ),
                            column(6,
                              numericInput(
                                "sampling_longitude",
                                "Longitude (°E)",
                                value = 18.6466,
                                min = 10,
                                max = 31,
                                step = 0.0001
                              )
                            )
                          ),
                          HTML("<p style='font-size: 11px; color: #888; margin: 0;'><em>Default: Gdansk Bay</em></p>")
                        )
                      )
                    ),
                    actionButton("load_ecopath_native", "Import Native Database", icon = icon("database"), class = "btn-primary"),
                    hr(),
                    # Taxonomic database match report (left panel)
                    uiOutput("taxonomic_report_ui")
                  )
                ),
                column(6,
                  # Dynamic model preview box (shows guide or model metadata)
                  uiOutput("ecopath_native_preview_ui"),
                  # Taxonomic database search progress (right panel)
                  uiOutput("taxonomic_progress_panel_ui"),
                  # ECOPATH import status (right panel, better formatted)
                  uiOutput("ecopath_native_status_ui")
                )
              )
            ),

            # TAB 3: ECOPATH CSV/Excel Export
            tabPanel(
              title = "ECOPATH CSV/Excel",
              icon = icon("file-excel"),

              br(),
              fluidRow(
                column(6,
                  box(
                    title = "Import ECOPATH CSV/Excel Exports",
                    status = "success",
                    solidHeader = TRUE,
                    width = 12,
                    HTML("
                      <h5>Alternative: Import Exported Files</h5>
                      <p>Upload CSV/Excel exports from ECOPATH. <strong>Recommended for Windows users.</strong></p>
                    "),
                    fileInput(
                      "ecopath_file",
                      "1. Basic Estimates File (.xlsx, .csv)",
                      accept = c(".xlsx", ".xls", ".csv"),
                      multiple = FALSE
                    ),
                    fileInput(
                      "ecopath_diet_file",
                      "2. Diet Composition Matrix (.xlsx, .csv)",
                      accept = c(".xlsx", ".xls", ".csv"),
                      multiple = FALSE
                    ),
                    actionButton("load_ecopath", "Import Exported Files", icon = icon("upload"), class = "btn-primary"),
                    hr(),
                    verbatimTextOutput("ecopath_upload_status")
                  )
                ),
                column(6,
                  box(
                    title = "ECOPATH Export Guide",
                    status = "info",
                    solidHeader = TRUE,
                    width = 12,
                    HTML("
                      <h5>Required Files from ECOPATH</h5>
                      <h6>1. Basic Estimates</h6>
                      <p><strong>Export:</strong> File → Export → Basic Estimates</p>
                      <p>Required columns:</p>
                      <ul style='font-size: 12px;'>
                        <li><strong>Group name</strong> - Species/group name</li>
                        <li><strong>Biomass</strong> - Biomass (t/km²)</li>
                        <li><strong>P/B</strong> - Production/Biomass ratio</li>
                        <li><strong>Q/B</strong> - Consumption/Biomass ratio</li>
                      </ul>
                      <h6>2. Diet Composition</h6>
                      <p><strong>Export:</strong> File → Export → Diet Composition</p>
                      <p>Matrix format:</p>
                      <ul style='font-size: 12px;'>
                        <li>Rows = Prey species</li>
                        <li>Columns = Predator species</li>
                        <li>Values = Diet proportion (0-1)</li>
                      </ul>
                    ")
                  )
                )
              )
            ),

            # TAB 4: Format Guide
            tabPanel(
              title = "Format Guide",
              icon = icon("book"),

              br(),
              box(
                title = "Supported File Formats",
                status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            HTML("
              <h5>1. Excel Format (.xlsx, .xls)</h5>
              <p>Excel files should contain the following sheets:</p>

              <h6><strong>Sheet 1: Network (Adjacency Matrix)</strong></h6>
              <p>A square matrix where rows and columns represent species:</p>
              <table class='table table-sm table-bordered' style='width: auto; margin: 10px 0;'>
                <thead><tr><th></th><th>Species_A</th><th>Species_B</th><th>Species_C</th></tr></thead>
                <tbody>
                  <tr><td><strong>Species_A</strong></td><td>0</td><td>1</td><td>0</td></tr>
                  <tr><td><strong>Species_B</strong></td><td>0</td><td>0</td><td>1</td></tr>
                  <tr><td><strong>Species_C</strong></td><td>0</td><td>0</td><td>0</td></tr>
                </tbody>
              </table>
              <p><em>Value = 1 means Species A eats Species B (row → column)</em></p>

              <h6><strong>Sheet 2: Species_Info</strong></h6>
              <p>Species attributes (one row per species):</p>
              <table class='table table-sm table-bordered' style='width: auto; margin: 10px 0;'>
                <thead><tr><th>species</th><th>fg</th><th>meanB</th><th>losses</th><th>efficiencies</th></tr></thead>
                <tbody>
                  <tr><td>Species_A</td><td>Fish</td><td>1250.5</td><td>0.12</td><td>0.85</td></tr>
                  <tr><td>Species_B</td><td>Zooplankton</td><td>850.2</td><td>0.08</td><td>0.75</td></tr>
                  <tr><td>Species_C</td><td>Phytoplankton</td><td>2100.0</td><td>0.05</td><td>0.40</td></tr>
                </tbody>
              </table>

              <h5>Required Columns:</h5>
              <ul>
                <li><strong>species:</strong> Species name (must match network row/column names)</li>
                <li><strong>fg:</strong> Functional group (e.g., Fish, Benthos, Phytoplankton, Zooplankton, Detritus)</li>
                <li><strong>meanB:</strong> Mean biomass (g/km² or your preferred unit)</li>
                <li><strong>losses:</strong> Metabolic losses (J/sec) for flux calculations</li>
                <li><strong>efficiencies:</strong> Assimilation efficiencies (0-1) for flux calculations</li>
              </ul>

              <h5>Optional Columns:</h5>
              <ul>
                <li><strong>bodymasses:</strong> Average body mass (g)</li>
                <li><strong>taxon:</strong> Taxonomic classification</li>
                <li><strong>nbY:</strong> Number of years recorded</li>
              </ul>

              <hr>

              <h5>2. CSV Format (.csv)</h5>
              <p>Two CSV files required:</p>

              <h6><strong>File 1: network.csv (Adjacency Matrix)</strong></h6>
              <pre style='background: #f8f9fa; padding: 10px; border-radius: 5px;'>
species,Species_A,Species_B,Species_C
Species_A,0,1,0
Species_B,0,0,1
Species_C,0,0,0</pre>

              <h6><strong>File 2: species_info.csv</strong></h6>
              <pre style='background: #f8f9fa; padding: 10px; border-radius: 5px;'>
species,fg,meanB,losses,efficiencies
Species_A,Fish,1250.5,0.12,0.85
Species_B,Zooplankton,850.2,0.08,0.75
Species_C,Phytoplankton,2100.0,0.05,0.40</pre>

              <hr>

              <h5>3. RData Format (.Rdata, .rda)</h5>
              <p>R workspace containing two objects:</p>
              <ul>
                <li><strong>net:</strong> igraph object with food web network</li>
                <li><strong>info:</strong> data.frame with species information (columns as above)</li>
              </ul>

              <p><strong>Example R code to create:</strong></p>
              <pre style='background: #f8f9fa; padding: 10px; border-radius: 5px;'>
library(igraph)

# Create adjacency matrix
adj_matrix <- matrix(c(0,1,0, 0,0,1, 0,0,0), nrow=3, byrow=TRUE)
rownames(adj_matrix) <- colnames(adj_matrix) <- c('Species_A', 'Species_B', 'Species_C')

# Create network
net <- graph_from_adjacency_matrix(adj_matrix, mode='directed')

# Create species info
info <- data.frame(
  species = c('Species_A', 'Species_B', 'Species_C'),
  fg = factor(c('Fish', 'Zooplankton', 'Phytoplankton')),
  meanB = c(1250.5, 850.2, 2100.0),
  losses = c(0.12, 0.08, 0.05),
  efficiencies = c(0.85, 0.75, 0.40)
)

# Save
save(net, info, file='my_foodweb.Rdata')</pre>
            ")
              )
            ),

            # TAB 5: Example Datasets
            tabPanel(
              title = "Example Datasets",
              icon = icon("download"),

              br(),
              box(
                title = "Example Datasets",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,

            fluidRow(
              column(4,
                h5("1. Simple 3-Species Chain"),
                p("Perfect for testing - basic linear food chain"),
                tags$ul(
                  tags$li("3 species (Phytoplankton → Zooplankton → Fish)"),
                  tags$li("2 trophic links"),
                  tags$li("Ideal for learning the format")
                ),
                downloadButton("download_simple_rdata", "Download RData", class = "btn-sm btn-primary"),
                downloadButton("download_simple_csv_net", "Network CSV", class = "btn-sm btn-secondary"),
                downloadButton("download_simple_csv_info", "Info CSV", class = "btn-sm btn-secondary")
              ),
              column(4,
                h5("2. Caribbean Reef"),
                p("Realistic tropical reef food web"),
                tags$ul(
                  tags$li("10 species across 4 functional groups"),
                  tags$li("18 trophic interactions"),
                  tags$li("Multiple trophic levels")
                ),
                downloadButton("download_reef_rdata", "Download RData", class = "btn-sm btn-primary"),
                downloadButton("download_reef_csv_net", "Network CSV", class = "btn-sm btn-secondary"),
                downloadButton("download_reef_csv_info", "Info CSV", class = "btn-sm btn-secondary")
              ),
              column(4,
                h5("3. Empty Template"),
                p("Start from scratch with proper structure"),
                tags$ul(
                  tags$li("3 placeholder species"),
                  tags$li("Correct file format"),
                  tags$li("Modify for your own data")
                ),
                downloadButton("download_template_rdata", "Download RData", class = "btn-sm btn-primary"),
                downloadButton("download_template_csv_net", "Network CSV", class = "btn-sm btn-secondary"),
                downloadButton("download_template_csv_info", "Info CSV", class = "btn-sm btn-secondary")
              )
            ),

            hr(),

            HTML("
              <h5>How to Use Example Files:</h5>
              <ol>
                <li><strong>Download</strong> one of the example RData files above</li>
                <li><strong>Upload</strong> it using the file input above</li>
                <li><strong>Click</strong> 'Load Data' button</li>
                <li><strong>Explore</strong> the food web in other tabs</li>
              </ol>

              <h5>File Formats:</h5>
              <p><strong>RData:</strong> Ready to upload directly to EcoNeTool<br>
              <strong>CSV files:</strong> Open in Excel to view/modify structure (2 files needed: network + info)</p>

              <p style='margin-top: 15px;'><em>See the examples/README.md file for detailed format documentation.</em></p>
            ")
              )
            )
          )
        )
      )

      # ========================================================================
}
