#' SHARK Data UI
#'
#' Creates the SHARK4R tab for accessing Swedish marine environmental archives
#'
#' @return A tabItem for SHARK data access
#'
#' @details
#' Provides 4 sub-tabs:
#' 1. Taxonomy - Query Dyntaxa, WoRMS, AlgaeBase
#' 2. Environmental Data - Retrieve oceanographic measurements
#' 3. Species Occurrence - Download biological observations
#' 4. Quality Control - Validate SHARK format data
shark_ui <- function() {
  # ========================================================================
  # SHARK DATA TAB
  # ========================================================================
  tabItem(
    tabName = "shark",

    # Header box with description
    fluidRow(
      box(
        title = "SHARK4R - Swedish Ocean Archives",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        HTML("
          <h4>Access Swedish Marine Environmental Data</h4>
          <p><strong>SHARK</strong> (Svenskt HavsARKiv) is Sweden's national database for marine environmental
          monitoring data, maintained by SMHI (Swedish Meteorological and Hydrological Institute).</p>
          <p>This interface provides access to:</p>
          <ul>
            <li><strong>Taxonomy:</strong> Dyntaxa (Swedish species), WoRMS, AlgaeBase</li>
            <li><strong>Environmental Data:</strong> Temperature, salinity, nutrients, oxygen from 1900s-present</li>
            <li><strong>Species Occurrence:</strong> Biological observations (phytoplankton, zooplankton, fish)</li>
            <li><strong>Quality Control:</strong> Validate SHARK format data files</li>
          </ul>
          <p style='margin-top: 10px;'>
            <strong>Documentation:</strong>
            <a href='https://sharksmhi.github.io/SHARK4R/' target='_blank'>SHARK4R Package</a> |
            <a href='https://www.smhi.se/data/oceanografi/ladda-ner-oceanografiska-observationer' target='_blank'>SHARK Database</a>
          </p>
        ")
      )
    ),

    # Main content with 4 sub-tabs
    fluidRow(
      box(
        width = 12,
        solidHeader = FALSE,
        status = "primary",

        tabsetPanel(
          id = "shark_tabs",
          type = "tabs",

          # ====================================================================
          # TAB 1: TAXONOMY LOOKUP
          # ====================================================================
          tabPanel(
            title = tagList(icon("search"), " Taxonomy"),
            value = "shark_taxonomy",
            br(),

            fluidRow(
              # Left column: Query panel
              column(4,
                box(
                  title = "Search Species Taxonomy",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,

                  textInput("shark_species_name",
                           "Species Name:",
                           placeholder = "e.g., 'torsk', 'Gadus morhua', 'sill'"),

                  checkboxGroupInput("shark_taxonomy_sources",
                    "Data Sources:",
                    choices = c(
                      "Dyntaxa (Swedish Taxonomy)" = "dyntaxa",
                      "WoRMS (World Register)" = "worms",
                      "AlgaeBase (Algae Database)" = "algaebase"
                    ),
                    selected = c("dyntaxa", "worms")
                  ),

                  checkboxInput("shark_fuzzy_search",
                               "Fuzzy matching",
                               value = TRUE),

                  actionButton("shark_search_taxonomy",
                             "Search Taxonomy",
                             icon = icon("search"),
                             class = "btn-primary btn-block"),

                  br(),
                  tags$small(HTML("
                    <strong>Tip:</strong> Try Swedish names like 'torsk' (cod),
                    'sill' (herring), or scientific names for best results.
                  "))
                )
              ),

              # Right column: Results panel
              column(8,
                box(
                  title = "Taxonomy Results",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  height = "600px",
                  style = "overflow-y: auto;",

                  uiOutput("shark_taxonomy_results")
                )
              )
            )
          ),

          # ====================================================================
          # TAB 2: ENVIRONMENTAL DATA
          # ====================================================================
          tabPanel(
            title = tagList(icon("water"), " Environmental Data"),
            value = "shark_environmental",
            br(),

            fluidRow(
              # Left column: Query builder
              column(4,
                box(
                  title = "Query Environmental Data",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,

                  dateRangeInput("shark_date_range",
                               "Date Range:",
                               start = Sys.Date() - 365,
                               end = Sys.Date(),
                               min = "1900-01-01",
                               max = Sys.Date()),

                  selectInput("shark_parameters",
                            "Parameters:",
                            choices = c(
                              "Temperature" = "temperature",
                              "Salinity" = "salinity",
                              "Oxygen" = "oxygen",
                              "pH" = "ph",
                              "Phosphate" = "phosphate",
                              "Nitrate" = "nitrate",
                              "Chlorophyll-a" = "chlorophyll",
                              "Secchi depth" = "secchi"
                            ),
                            multiple = TRUE,
                            selected = c("temperature", "salinity")),

                  tags$hr(),
                  tags$h5("Bounding Box (Optional)"),
                  tags$small("Leave blank for all Swedish waters"),

                  fluidRow(
                    column(6,
                      numericInput("shark_bbox_north",
                                 "North (Lat):",
                                 value = NULL,
                                 min = 54, max = 66, step = 0.1)
                    ),
                    column(6,
                      numericInput("shark_bbox_south",
                                 "South (Lat):",
                                 value = NULL,
                                 min = 54, max = 66, step = 0.1)
                    )
                  ),

                  fluidRow(
                    column(6,
                      numericInput("shark_bbox_east",
                                 "East (Lon):",
                                 value = NULL,
                                 min = 10, max = 25, step = 0.1)
                    ),
                    column(6,
                      numericInput("shark_bbox_west",
                                 "West (Lon):",
                                 value = NULL,
                                 min = 10, max = 25, step = 0.1)
                    )
                  ),

                  numericInput("shark_max_env_records",
                             "Max Records:",
                             value = 5000,
                             min = 100, max = 50000, step = 1000),

                  actionButton("shark_query_environmental",
                             "Query Data",
                             icon = icon("download"),
                             class = "btn-success btn-block")
                )
              ),

              # Right column: Results display
              column(8,
                box(
                  title = "Environmental Data Results",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,

                  uiOutput("shark_environmental_status"),
                  br(),

                  DTOutput("shark_environmental_table"),

                  br(),
                  downloadButton("shark_download_environmental",
                               "Download CSV",
                               class = "btn-primary")
                )
              )
            )
          ),

          # ====================================================================
          # TAB 3: SPECIES OCCURRENCE
          # ====================================================================
          tabPanel(
            title = tagList(icon("fish"), " Species Occurrence"),
            value = "shark_occurrence",
            br(),

            fluidRow(
              # Left column: Query panel
              column(4,
                box(
                  title = "Query Species Occurrences",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,

                  textInput("shark_occurrence_species",
                           "Species Name:",
                           placeholder = "e.g., 'Gadus morhua', 'torsk'"),

                  dateRangeInput("shark_occurrence_dates",
                               "Date Range:",
                               start = Sys.Date() - 365*5,  # 5 years
                               end = Sys.Date(),
                               min = "1900-01-01",
                               max = Sys.Date()),

                  numericInput("shark_max_occ_records",
                             "Max Records:",
                             value = 2000,
                             min = 100, max = 10000, step = 500),

                  actionButton("shark_query_occurrence",
                             "Get Occurrences",
                             icon = icon("search"),
                             class = "btn-primary btn-block"),

                  br(),
                  tags$small(HTML("
                    <strong>Note:</strong> Results will be displayed on the map
                    and in the table. Use scientific names for best results.
                  "))
                )
              ),

              # Right column: Results + Map
              column(8,
                box(
                  title = "Occurrence Records",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,

                  uiOutput("shark_occurrence_status"),
                  br(),

                  # Interactive map
                  leafletOutput("shark_occurrence_map", height = "400px"),

                  br(),
                  tags$hr(),

                  # Data table
                  DTOutput("shark_occurrence_table"),

                  br(),
                  downloadButton("shark_download_occurrence",
                               "Download CSV",
                               class = "btn-primary")
                )
              )
            )
          ),

          # ====================================================================
          # TAB 4: QUALITY CONTROL
          # ====================================================================
          tabPanel(
            title = tagList(icon("check-circle"), " Quality Control"),
            value = "shark_qc",
            br(),

            fluidRow(
              # Left column: Upload panel
              column(4,
                box(
                  title = "Upload SHARK Format Data",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,

                  fileInput("shark_qc_file",
                          "Choose File:",
                          accept = c(".csv", ".txt", ".tsv"),
                          placeholder = "Select SHARK format file"),

                  tags$small(HTML("
                    <strong>Accepted formats:</strong> CSV, TXT, TSV<br>
                    <strong>Expected structure:</strong> SHARK standard format
                  ")),

                  br(), br(),

                  actionButton("shark_run_qc",
                             "Run Quality Control",
                             icon = icon("play"),
                             class = "btn-warning btn-block"),

                  br(),

                  checkboxGroupInput("shark_qc_checks",
                    "Quality Checks:",
                    choices = c(
                      "Format validation" = "format",
                      "Data completeness" = "completeness",
                      "Outlier detection" = "outliers",
                      "Coordinate validation" = "coordinates"
                    ),
                    selected = c("format", "completeness", "coordinates")
                  )
                )
              ),

              # Right column: Results panel
              column(8,
                box(
                  title = "Quality Control Results",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  height = "600px",
                  style = "overflow-y: auto;",

                  uiOutput("shark_qc_status"),
                  br(),

                  verbatimTextOutput("shark_qc_results"),

                  br(),
                  uiOutput("shark_qc_warnings"),

                  br(),
                  uiOutput("shark_qc_summary")
                )
              )
            )
          )

        )  # End tabsetPanel
      )  # End main box
    ),  # End main fluidRow

    # Requirements and Installation box (collapsible)
    fluidRow(
      box(
        title = "Requirements & Installation",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        collapsed = TRUE,

        HTML("
          <h5>Required R Package</h5>
          <p>This module requires the SHARK4R package:</p>
          <pre style='background: #f8f9fa; padding: 10px; border-left: 3px solid #007bff;'>
install.packages('SHARK4R')</pre>

          <h5>Package Information</h5>
          <ul>
            <li><strong>Version:</strong> 1.0.2+</li>
            <li><strong>License:</strong> MIT</li>
            <li><strong>Maintainer:</strong> SMHI (Swedish Meteorological and Hydrological Institute)</li>
          </ul>

          <h5>Documentation & Resources</h5>
          <ul>
            <li><a href='https://sharksmhi.github.io/SHARK4R/' target='_blank'>
                SHARK4R Package Documentation</a></li>
            <li><a href='https://www.smhi.se/data/oceanografi/ladda-ner-oceanografiska-observationer' target='_blank'>
                SHARK Database (SMHI)</a></li>
            <li><a href='https://www.artdatabanken.se/vara-datavaror/dyntaxa/' target='_blank'>
                Dyntaxa - Swedish Taxonomic Database</a></li>
          </ul>

          <h5>Typical Use Cases</h5>
          <ol>
            <li><strong>Taxonomy Validation:</strong> Verify Swedish species names using Dyntaxa</li>
            <li><strong>Environmental Context:</strong> Retrieve historical oceanographic data for model regions</li>
            <li><strong>Model Validation:</strong> Compare ECOPATH species with SHARK occurrence records</li>
            <li><strong>Data Quality:</strong> Validate monitoring data before analysis</li>
          </ol>

          <h5>Data Coverage</h5>
          <ul>
            <li><strong>Temporal:</strong> 1900s - present (varies by parameter)</li>
            <li><strong>Spatial:</strong> Swedish waters (Baltic Sea, Skagerrak, Kattegat)</li>
            <li><strong>Parameters:</strong> 100+ environmental and biological variables</li>
          </ul>
        ")
      )
    )

  )  # End tabItem
  # ========================================================================
}
