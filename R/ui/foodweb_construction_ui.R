# UI for Food Web Construction Module
# Focuses on building food webs from trait data

foodweb_construction_ui <- function() {
  tabItem(
    tabName = "foodweb_construction",

    # Header
    fluidRow(
      column(
        width = 10,
        h3(icon("sitemap"), " Food Web Construction"),
        p("Construct trait-based food webs using interaction probability matrices. Edit traits, validate data, and visualize the resulting network.")
      ),
      column(
        width = 2,
        style = "text-align: right; padding-top: 20px;",
        actionButton(
          "foodweb_help_button",
          tagList(icon("question-circle"), " Help & Methodology"),
          class = "btn-info"
        )
      )
    ),

    hr(),

    # Tabbed interface
    tabsetPanel(
      id = "foodweb_main_tabs",
      type = "tabs",

      # ========================================================================
      # TAB 1: DATA INPUT
      # ========================================================================
      tabPanel(
        title = tagList(icon("upload"), " Data Input"),
        value = "input_tab",
        br(),

        fluidRow(
          column(
            width = 12,
            # Check for data from Trait Research
            uiOutput("foodweb_trait_research_data_status")
          )
        ),

        fluidRow(
          # Input method selection
          column(
            width = 3,
            wellPanel(
              h5(icon("database"), " Data Source"),

              radioButtons(
                "foodweb_input_method",
                NULL,
                choices = c(
                  "From Trait Research" = "research",
                  "Upload CSV File" = "upload",
                  "Example Dataset" = "example",
                  "Manual Entry" = "manual"
                ),
                selected = "example"
              ),

              hr(),

              actionButton(
                "foodweb_load_data",
                "Load Data",
                icon = icon("download"),
                class = "btn-primary btn-block"
              )
            ),

            # Quick trait reference
            wellPanel(
              h5("Trait Codes"),
              tags$small(
                tags$dl(
                  tags$dt("MS:"), tags$dd("Size (MS1-MS7)"),
                  tags$dt("FS:"), tags$dd("Foraging (FS0-FS6)"),
                  tags$dt("MB:"), tags$dd("Mobility (MB1-MB5)"),
                  tags$dt("EP:"), tags$dd("Position (EP1-EP4)"),
                  tags$dt("PR:"), tags$dd("Protection (PR0-PR8)")
                )
              )
            )
          ),

          # Input panels (conditional)
          column(
            width = 6,

            # From Trait Research
            conditionalPanel(
              condition = "input.foodweb_input_method == 'research'",
              wellPanel(
                h4(icon("search"), " Use Data from Trait Research"),
                p(class = "text-muted",
                  "Load trait data that was looked up in the Trait Research module."),

                uiOutput("foodweb_research_data_preview"),

                helpText(
                  icon("info-circle"),
                  " If no data is available, first use the Trait Research module to look up species traits."
                )
              )
            ),

            # CSV Upload
            conditionalPanel(
              condition = "input.foodweb_input_method == 'upload'",
              wellPanel(
                h4(icon("file-csv"), " Upload CSV File"),
                p(class = "text-muted", "Upload a CSV file with species trait data."),

                fileInput(
                  "foodweb_file",
                  "Select CSV File:",
                  accept = c(".csv", ".txt"),
                  placeholder = "No file selected"
                ),

                h5("Required Format:"),
                tags$pre("species,MS,FS,MB,EP,PR\nGadus morhua,MS6,FS1,MB5,EP4,PR0\n..."),

                downloadButton(
                  "foodweb_download_template",
                  "Download Template CSV",
                  class = "btn-secondary btn-block"
                )
              )
            ),

            # Example Dataset
            conditionalPanel(
              condition = "input.foodweb_input_method == 'example'",
              wellPanel(
                h4(icon("flask"), " Example Datasets"),
                p(class = "text-muted", "Load a pre-built example dataset."),

                selectInput(
                  "foodweb_example_dataset",
                  "Select Example:",
                  choices = c(
                    "Simple Food Chain (5 species)" = "simple",
                    "Marine Invertebrates (10 species)" = "marine",
                    "Coastal Ecosystem (20 species)" = "complex"
                  ),
                  selected = "simple"
                ),

                htmlOutput("foodweb_example_description")
              )
            ),

            # Manual Entry
            conditionalPanel(
              condition = "input.foodweb_input_method == 'manual'",
              wellPanel(
                h4(icon("keyboard"), " Manual Entry"),
                p(class = "text-muted", "Create a blank template for manual trait entry."),

                numericInput(
                  "foodweb_n_species",
                  "Number of Species:",
                  value = 5,
                  min = 2,
                  max = 100,
                  step = 1
                ),

                helpText(
                  icon("lightbulb"),
                  " Create template, then edit in the 'Trait Editor' tab."
                )
              )
            )
          ),

          # Status column
          column(
            width = 3,
            wellPanel(
              h5(icon("info-circle"), " Data Status"),
              uiOutput("foodweb_data_status"),
              hr(),
              htmlOutput("foodweb_data_quick_stats")
            ),

            wellPanel(
              h5("Workflow"),
              tags$ol(
                tags$li("Load or enter trait data"),
                tags$li("Edit traits in 'Trait Editor'"),
                tags$li("Validate data quality"),
                tags$li("Construct network in 'Network'")
              )
            )
          )
        )
      ),

      # ========================================================================
      # TAB 2: TRAIT EDITOR
      # ========================================================================
      tabPanel(
        title = tagList(icon("edit"), " Trait Editor"),
        value = "editor_tab",
        br(),

        fluidRow(
          column(
            width = 12,
            h4("Edit and Validate Species Trait Data")
          )
        ),

        fluidRow(
          # Editable table
          column(
            width = 8,
            wellPanel(
              h5("Species Trait Table"),
              helpText(
                icon("info-circle"),
                " Click any cell to edit. Changes are saved automatically."
              ),

              div(
                style = "overflow-x: auto;",
                DT::dataTableOutput("foodweb_data_table")
              ),

              br(),

              fluidRow(
                column(
                  width = 4,
                  actionButton(
                    "foodweb_validate",
                    "Validate Data",
                    icon = icon("check-circle"),
                    class = "btn-info btn-block"
                  )
                ),
                column(
                  width = 4,
                  downloadButton(
                    "foodweb_download_data",
                    "Download CSV",
                    class = "btn-secondary btn-block"
                  )
                ),
                column(
                  width = 4,
                  actionButton(
                    "foodweb_reset_data",
                    "Reset to Original",
                    icon = icon("undo"),
                    class = "btn-outline-warning btn-block"
                  )
                )
              )
            )
          ),

          # Validation and stats
          column(
            width = 4,
            wellPanel(
              h5("Validation Results"),
              uiOutput("foodweb_validation_messages")
            ),

            wellPanel(
              h5("Dataset Statistics"),
              htmlOutput("foodweb_data_stats")
            ),

            wellPanel(
              h5("Trait Distribution"),
              plotOutput("foodweb_distribution_plot", height = "200px")
            )
          )
        )
      ),

      # ========================================================================
      # TAB 3: NETWORK CONSTRUCTION
      # ========================================================================
      tabPanel(
        title = tagList(icon("project-diagram"), " Network"),
        value = "network_tab",
        br(),

        fluidRow(
          column(
            width = 12,
            h4("Food Web Network Construction and Visualization")
          )
        ),

        fluidRow(
          # Controls
          column(
            width = 3,
            wellPanel(
              h5("Network Parameters"),

              sliderInput(
                "foodweb_threshold",
                "Interaction Probability Threshold:",
                min = 0,
                max = 1,
                value = 0.05,
                step = 0.01
              ),
              helpText("Higher = fewer interactions (more restrictive)"),

              hr(),

              actionButton(
                "foodweb_construct_network",
                "Construct Food Web",
                icon = icon("sitemap"),
                class = "btn-success btn-block btn-lg"
              ),

              br(),

              downloadButton(
                "foodweb_download_adjacency",
                "Download Adjacency Matrix",
                class = "btn-secondary btn-block"
              ),

              downloadButton(
                "foodweb_download_network",
                "Download Network (RDS)",
                class = "btn-secondary btn-block"
              ),

              downloadButton(
                "foodweb_download_graphml",
                "Download Network (GraphML)",
                class = "btn-secondary btn-block"
              )
            ),

            wellPanel(
              h5("Network Properties"),
              htmlOutput("foodweb_network_stats")
            )
          ),

          # Network visualization
          column(
            width = 9,
            wellPanel(
              h5("Food Web Network"),
              visNetwork::visNetworkOutput("foodweb_network_plot", height = "500px"),
              helpText(
                icon("info-circle"),
                " Drag nodes to rearrange. Zoom with mouse wheel. Click nodes for details."
              )
            ),

            wellPanel(
              h5("Interaction Probability Heatmap"),
              plotOutput("foodweb_probability_heatmap", height = "350px"),
              helpText("Color intensity indicates interaction probability.")
            )
          )
        )
      ),

      # ========================================================================
      # TAB 4: PROBABILITY MATRICES
      # ========================================================================
      tabPanel(
        title = tagList(icon("table"), " Probability Matrices"),
        value = "matrices_tab",
        br(),

        fluidRow(
          column(
            width = 12,
            h4("Interaction Probability Matrices"),
            p("These matrices define the probability of interaction given trait combinations.")
          )
        ),

        fluidRow(
          column(
            width = 12,
            tabsetPanel(
              id = "prob_matrix_tabs",

              tabPanel(
                "MS x MS (Size)",
                br(),
                div(
                  class = "well",
                  h5("Consumer Size x Resource Size"),
                  p("Probability that a consumer of size MSi can consume a resource of size MSj."),
                  tags$ul(
                    tags$li("Diagonal (0.5): Cannibalism or same-size predation"),
                    tags$li("Below diagonal (0.0): Smaller cannot eat larger"),
                    tags$li("Above diagonal: Probability decreases with size difference")
                  )
                ),
                tableOutput("foodweb_prob_matrix_MS_MS")
              ),

              tabPanel(
                "FS x MS (Foraging x Size)",
                br(),
                div(
                  class = "well",
                  h5("Consumer Foraging Strategy x Resource Size"),
                  p("Probability that a consumer with foraging strategy FSi can access a resource of size MSj.")
                ),
                tableOutput("foodweb_prob_matrix_FS_MS")
              ),

              tabPanel(
                "MB x MB (Mobility)",
                br(),
                div(
                  class = "well",
                  h5("Consumer Mobility x Resource Mobility"),
                  p("Probability that a consumer with mobility MBi can capture a resource with mobility MBj.")
                ),
                tableOutput("foodweb_prob_matrix_MB_MB")
              ),

              tabPanel(
                "EP x MS (Position x Size)",
                br(),
                div(
                  class = "well",
                  h5("Consumer Position x Resource Size"),
                  p("Probability that a consumer in position EPi can access a resource of size MSj.")
                ),
                tableOutput("foodweb_prob_matrix_EP_MS")
              ),

              tabPanel(
                "PR x MS (Protection x Size)",
                br(),
                div(
                  class = "well",
                  h5("Resource Protection x Consumer Size"),
                  p("Probability that a consumer of size MSi can overcome protection PRj.")
                ),
                tableOutput("foodweb_prob_matrix_PR_MS")
              )
            )
          )
        )
      ),

      # ========================================================================
      # TAB 5: TRAIT REFERENCE
      # ========================================================================
      tabPanel(
        title = tagList(icon("book"), " Trait Reference"),
        value = "reference_tab",
        br(),

        fluidRow(
          column(
            width = 12,
            h4("Trait Code Reference Guide")
          )
        ),

        fluidRow(
          column(
            width = 12,
            tabsetPanel(
              id = "trait_reference_tabs",

              tabPanel(
                "Size Class (MS)",
                br(),
                htmlOutput("foodweb_trait_ref_MS")
              ),

              tabPanel(
                "Foraging Strategy (FS)",
                br(),
                htmlOutput("foodweb_trait_ref_FS")
              ),

              tabPanel(
                "Mobility (MB)",
                br(),
                htmlOutput("foodweb_trait_ref_MB")
              ),

              tabPanel(
                "Environmental Position (EP)",
                br(),
                htmlOutput("foodweb_trait_ref_EP")
              ),

              tabPanel(
                "Protection (PR)",
                br(),
                htmlOutput("foodweb_trait_ref_PR")
              )
            )
          )
        )
      )
    ),

    # Help Modal
    bsModal(
      id = "foodweb_help_modal",
      title = tagList(icon("question-circle"), " Food Web Construction - Help & Methodology"),
      trigger = "foodweb_help_button",
      size = "large",

      tabsetPanel(
        id = "foodweb_help_tabs",

        tabPanel(
          "Quick Start",
          br(),
          h4("Quick Start Guide"),
          tags$ol(
            tags$li("Load trait data (from Trait Research, CSV, example, or manual entry)"),
            tags$li("Review and edit traits in the 'Trait Editor' tab"),
            tags$li("Validate data to check for issues"),
            tags$li("Go to 'Network' tab and set the probability threshold"),
            tags$li("Click 'Construct Food Web' to generate the network"),
            tags$li("Explore the network visualization and statistics")
          )
        ),

        tabPanel(
          "Methodology",
          br(),
          h4("Trait-Based Food Web Construction"),
          p("The method constructs food webs by calculating interaction probabilities based on five trait dimensions:"),
          tags$ul(
            tags$li(tags$strong("MS (Maximum Size):"), " Determines predator-prey size relationships"),
            tags$li(tags$strong("FS (Foraging Strategy):"), " Defines feeding mode and prey accessibility"),
            tags$li(tags$strong("MB (Mobility):"), " Affects prey capture probability"),
            tags$li(tags$strong("EP (Environmental Position):"), " Determines spatial overlap"),
            tags$li(tags$strong("PR (Protection):"), " Reduces vulnerability to predation")
          ),
          p("The final interaction probability is calculated as the ", tags$strong("minimum"),
            " across all five dimension-specific probabilities.")
        ),

        tabPanel(
          "Threshold",
          br(),
          h4("Probability Threshold"),
          p("The threshold determines which interactions are included in the final network:"),
          tags$ul(
            tags$li(tags$strong("Low threshold (0.01-0.05):"), " More interactions, denser network"),
            tags$li(tags$strong("Medium threshold (0.05-0.15):"), " Balanced network structure"),
            tags$li(tags$strong("High threshold (0.15-0.30):"), " Fewer interactions, sparser network")
          ),
          p("Adjust the threshold based on your research question and the expected connectance of the ecosystem.")
        ),

        tabPanel(
          "References",
          br(),
          h4("Scientific References"),
          tags$ul(
            tags$li("Gravel et al. (2013). Inferring food web structure from predator-prey body size relationships. Methods in Ecology and Evolution."),
            tags$li("Brose et al. (2006). Consumer-resource body-size relationships in natural food webs. Ecology."),
            tags$li("Williams & Martinez (2000). Simple rules yield complex food webs. Nature.")
          )
        )
      )
    )
  )
}
