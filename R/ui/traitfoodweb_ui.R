# UI for Trait-Based Food Web Construction Module (Reorganized with Tabs)

traitfoodweb_ui <- function() {
  tabItem(
    tabName = "traitfoodweb",

    # Header with title and help button
    fluidRow(
      column(
        width = 10,
        h3(icon("dna"), " Trait-Based Food Web Construction"),
        p("Construct food webs using species trait probabilities based on size class, foraging strategy, mobility, environmental position, and protection.")
      ),
      column(
        width = 2,
        style = "text-align: right; padding-top: 20px;",
        actionButton(
          "trait_help_button",
          tagList(icon("question-circle"), " Help & Methodology"),
          class = "btn-info btn-lg"
        )
      )
    ),

    hr(),

    # Tabbed interface
    tabsetPanel(
      id = "trait_main_tabs",
      type = "tabs",

      # ========================================================================
      # TAB 1: AUTOMATED LOOKUP
      # ========================================================================
      tabPanel(
        title = tagList(icon("search"), " Automated Lookup"),
        value = "lookup_tab",
        br(),

        fluidRow(
          column(
            width = 12,
            h4(icon("database"), " Automated Trait Lookup from Online Databases"),
            p("Query multiple online databases to automatically retrieve species trait data. The system queries databases hierarchically and harmonizes raw data into categorical trait classes.")
          )
        ),

        fluidRow(
          # Left column: Species input and database selection
          column(
            width = 5,
            wellPanel(
              h5(icon("list"), " Species List"),
              p(class = "text-muted", "Enter scientific names, one per line."),

              textAreaInput(
                "trait_species_list",
                "Species Names (Scientific):",
                value = "Amphibalanus improvisus\nGammarus spp.\nHediste diversicolor\nMacoma balthica\nMarenzelleria spp.\nMya arenaria\nMytilus edulis trossulus\nSaduria entomon\nBathyporeia pilosa\nCerastoderma glaucum\nCorophium spp.\nHydrobia spp.\nIdotea balthica\nJaera albifrons\nOligochaeta\nOstracoda\nPygospio elegans\nTheodoxus fluviatilis",
                rows = 15,
                placeholder = "Amphibalanus improvisus\nMacoma balthica\nMya arenaria\n..."
              ),

              helpText(
                icon("lightbulb"),
                " Default: 18 Baltic Sea benthic invertebrate species. Use exact scientific names for best results."
              )
            ),

            wellPanel(
              h5(icon("database"), " Database Selection"),
              p(class = "text-muted", "Select which databases to query (checked databases will be queried in hierarchical order)."),

              checkboxGroupInput(
                "trait_databases",
                NULL,
                choices = c(
                  "WoRMS (taxonomy - always recommended)" = "worms",
                  "FishBase (fish traits)" = "fishbase",
                  "SeaLifeBase (marine invertebrates)" = "sealifebase",
                  "BIOTIC (benthic invertebrates)" = "biotic",
                  "freshwaterecology.info (freshwater species)" = "freshwater",
                  "MAREDAT (zooplankton)" = "maredat",
                  "PTDB (phytoplankton)" = "ptdb",
                  "AlgaeBase (algae/phytoplankton)" = "algaebase",
                  "SHARK (Swedish Ocean Archives)" = "shark"
                ),
                selected = c("worms", "fishbase", "sealifebase")
              ),

              actionButton(
                "trait_run_lookup",
                "Run Automated Lookup",
                icon = icon("search"),
                class = "btn-success btn-block btn-lg"
              )
            )
          ),

          # Right column: Information and help
          column(
            width = 7,
            wellPanel(
              h5(icon("info-circle"), " How Automated Lookup Works"),
              tags$ol(
                tags$li(tags$strong("Hierarchical Querying:"), " Databases are queried in priority order (WoRMS → FishBase → SeaLifeBase → BIOTIC → freshwaterecology.info → MAREDAT → PTDB → AlgaeBase → SHARK)"),
                tags$li(tags$strong("Data Merging:"), " Traits from multiple sources are merged, with higher-priority databases taking precedence"),
                tags$li(tags$strong("Harmonization:"), " Raw continuous data (size, trophic level, etc.) is converted to categorical trait classes (MS, FS, MB, EP, PR)"),
                tags$li(tags$strong("Caching:"), " Results are cached for 30 days to minimize API calls"),
                tags$li(tags$strong("Progress Tracking:"), " Detailed progress modal shows which databases are queried and results for each species")
              ),

              hr(),

              h5(icon("database"), " Database Coverage"),
              tags$ul(
                tags$li(tags$strong("WoRMS:"), " Taxonomy and basic classification (all marine species)"),
                tags$li(tags$strong("FishBase:"), " Fish traits (23,000+ species) - size, trophic level, habitat, depth"),
                tags$li(tags$strong("SeaLifeBase:"), " Marine invertebrates (23,000+ species) - crustaceans, mollusks, echinoderms"),
                tags$li(tags$strong("BIOTIC:"), " Benthic invertebrates (requires downloaded file)"),
                tags$li(tags$strong("freshwaterecology.info:"), " Freshwater species (10,000+ species) - requires API key"),
                tags$li(tags$strong("MAREDAT:"), " Zooplankton (requires downloaded file)"),
                tags$li(tags$strong("PTDB:"), " Phytoplankton (requires downloaded file)"),
                tags$li(tags$strong("AlgaeBase:"), " Algae taxonomy (150,000+ species via WoRMS)"),
                tags$li(tags$strong("SHARK:"), " Swedish Ocean Archives - habitat and depth data")
              ),

              hr(),

              tags$div(
                class = "alert alert-warning",
                icon("exclamation-triangle"),
                tags$strong(" Database Requirements:"),
                tags$ul(
                  tags$li("BIOTIC, MAREDAT, PTDB: Require downloaded CSV files in ", tags$code("data/"), " folder"),
                  tags$li("freshwaterecology.info: Requires API key (demo key provided, see ", tags$code("config/api_keys.R.template"), " for custom key)"),
                  tags$li("AlgaeBase: Currently uses WoRMS fallback (no public API available yet)")
                )
              ),

              actionLink(
                "trait_help_database_setup",
                tagList(icon("book"), " View Database Setup Guide →"),
                onclick = "Shiny.setInputValue('trait_help_button', Math.random());"
              )
            ),

            wellPanel(
              h5(icon("graduation-cap"), " Trait Categories"),
              tags$small(
                tags$dl(
                  tags$dt("MS (Maximum Size):"), tags$dd("MS1-MS7 (< 0.1 cm to > 150 cm)"),
                  tags$dt("FS (Foraging Strategy):"), tags$dd("FS0-FS6 (Primary Producer to Filter Feeder)"),
                  tags$dt("MB (Mobility):"), tags$dd("MB1-MB5 (Sessile to Obligate Swimmer)"),
                  tags$dt("EP (Environmental Position):"), tags$dd("EP1-EP4 (Infaunal to Pelagic)"),
                  tags$dt("PR (Protection):"), tags$dd("PR0-PR8 (No Protection to Armoured)")
                )
              ),
              actionLink("trait_view_full_reference_lookup", "View Full Trait Reference →",
                        onclick = "$('#trait_main_tabs').find('a:contains(\\'Trait Reference\\')').click();")
            )
          )
        )
      ),

      # ========================================================================
      # TAB 2: DATA INPUT (CSV/EXAMPLE/MANUAL)
      # ========================================================================
      tabPanel(
        title = tagList(icon("upload"), " Data Input"),
        value = "input_tab",
        br(),

        fluidRow(
          column(
            width = 12,
            h4("Alternative Data Input Methods"),
            p("Upload CSV files, use example datasets, or create manual entries for species trait data.")
          )
        ),

        fluidRow(
          # Input method selection
          column(
            width = 3,
            wellPanel(
              h5("Input Method"),

              radioButtons(
                "trait_input_method",
                NULL,
                choices = c(
                  "Upload CSV File" = "upload",
                  "Example Dataset" = "example",
                  "Manual Entry" = "manual"
                ),
                selected = "example"
              )
            ),

            # Trait code quick reference
            wellPanel(
              h5("Trait Code Quick Reference"),
              tags$small(
                tags$dl(
                  tags$dt("MS (Size):"), tags$dd("MS1-MS7 (< 0.1 cm to > 150 cm)"),
                  tags$dt("FS (Foraging):"), tags$dd("FS0-FS6 (Producer to Filter)"),
                  tags$dt("MB (Mobility):"), tags$dd("MB1-MB5 (Sessile to Swimmer)"),
                  tags$dt("EP (Position):"), tags$dd("EP1-EP4 (Infaunal to Pelagic)"),
                  tags$dt("PR (Protection):"), tags$dd("PR0-PR8 (None to Armoured)")
                )
              ),
              actionLink("trait_view_full_reference", "View Full Reference →")
            )
          ),

          # Input panels (conditional)
          column(
            width = 6,
            # CSV upload
            conditionalPanel(
              condition = "input.trait_input_method == 'upload'",
              wellPanel(
                h4(icon("file-csv"), " Upload CSV File"),
                p(class = "text-muted", "Upload a CSV file with species trait data."),

                fileInput(
                  "trait_file",
                  "Select CSV File:",
                  accept = c(".csv", ".txt"),
                  placeholder = "No file selected"
                ),

                h5("Required Format:"),
                tags$pre("species,MS,FS,MB,EP,PR\nGadus morhua,MS6,FS1,MB5,EP4,PR0\n..."),

                helpText(
                  icon("info-circle"),
                  " CSV must contain columns: species, MS, FS, MB, EP, PR"
                ),

                downloadButton(
                  "trait_download_template",
                  "Download Template CSV",
                  class = "btn-secondary btn-block"
                )
              )
            ),

            # Example dataset
            conditionalPanel(
              condition = "input.trait_input_method == 'example'",
              wellPanel(
                h4(icon("flask"), " Example Datasets"),
                p(class = "text-muted", "Load a pre-built example dataset for demonstration or testing."),

                selectInput(
                  "trait_example_dataset",
                  "Select Example:",
                  choices = c(
                    "Simple Food Chain (5 species)" = "simple",
                    "Marine Invertebrate Community (10 species)" = "marine",
                    "Coastal Ecosystem (20 species)" = "complex"
                  ),
                  selected = "simple"
                ),

                htmlOutput("trait_example_description")
              )
            ),

            # Manual entry
            conditionalPanel(
              condition = "input.trait_input_method == 'manual'",
              wellPanel(
                h4(icon("keyboard"), " Manual Entry"),
                p(class = "text-muted", "Create a blank template for manual trait entry."),

                numericInput(
                  "trait_n_species",
                  "Number of Species:",
                  value = 5,
                  min = 2,
                  max = 100,
                  step = 1
                ),

                actionButton(
                  "trait_create_template",
                  "Create Blank Template",
                  icon = icon("table"),
                  class = "btn-primary btn-block"
                ),

                br(),
                helpText(
                  icon("lightbulb"),
                  " Tip: Create template, then edit in the 'Trait Editor' tab."
                )
              )
            )
          ),

          # Status and quick stats
          column(
            width = 3,
            wellPanel(
              h5("Data Status"),
              uiOutput("trait_data_status"),
              hr(),
              htmlOutput("trait_data_quick_stats")
            ),

            wellPanel(
              h5("Next Steps"),
              tags$ol(
                tags$li("Load or enter trait data"),
                tags$li("Go to 'Trait Editor' tab to review"),
                tags$li("Validate data quality"),
                tags$li("Go to 'Network' tab to construct")
              )
            )
          )
        )
      ),

      # ========================================================================
      # TAB 3: TRAIT EDITOR
      # ========================================================================
      tabPanel(
        title = tagList(icon("edit"), " Trait Editor"),
        value = "editor_tab",
        br(),

        fluidRow(
          column(
            width = 12,
            h4("Edit and Validate Species Trait Data"),
            p("Review, edit, and validate trait assignments before constructing the food web.")
          )
        ),

        fluidRow(
          # Editable data table
          column(
            width = 8,
            wellPanel(
              h5("Species Trait Table"),
              helpText(
                icon("info-circle"),
                " Click any cell to edit. Double-click species name to edit. Changes are saved automatically."
              ),

              div(
                style = "overflow-x: auto;",
                DT::dataTableOutput("trait_data_table")
              ),

              br(),

              fluidRow(
                column(
                  width = 6,
                  actionButton(
                    "trait_validate",
                    "Validate Trait Data",
                    icon = icon("check-circle"),
                    class = "btn-info btn-block"
                  )
                ),
                column(
                  width = 6,
                  downloadButton(
                    "trait_download_data",
                    "Download Trait Data CSV",
                    class = "btn-secondary btn-block"
                  )
                )
              )
            )
          ),

          # Validation messages and statistics
          column(
            width = 4,
            wellPanel(
              h5("Validation Results"),
              uiOutput("trait_validation_messages")
            ),

            wellPanel(
              h5("Dataset Statistics"),
              htmlOutput("trait_data_stats")
            ),

            wellPanel(
              h5("Trait Distribution"),
              plotOutput("trait_distribution_plot", height = "250px")
            )
          )
        )
      ),

      # ========================================================================
      # TAB 4: NETWORK CONSTRUCTION
      # ========================================================================
      tabPanel(
        title = tagList(icon("sitemap"), " Network"),
        value = "network_tab",
        br(),

        fluidRow(
          column(
            width = 12,
            h4("Food Web Network Construction and Visualization"),
            p("Construct the interaction network and explore network properties.")
          )
        ),

        fluidRow(
          # Controls
          column(
            width = 3,
            wellPanel(
              h5("Network Parameters"),

              sliderInput(
                "trait_threshold",
                "Interaction Probability Threshold:",
                min = 0,
                max = 1,
                value = 0.05,
                step = 0.01
              ),
              helpText("Higher = fewer interactions (more restrictive)"),

              hr(),

              actionButton(
                "trait_construct_network",
                "Construct Food Web",
                icon = icon("sitemap"),
                class = "btn-success btn-block btn-lg"
              ),

              br(),

              downloadButton(
                "trait_download_adjacency",
                "Download Adjacency Matrix",
                class = "btn-secondary btn-block"
              ),

              downloadButton(
                "trait_download_network",
                "Download Network Object (RDS)",
                class = "btn-secondary btn-block"
              )
            ),

            wellPanel(
              h5("Network Properties"),
              htmlOutput("trait_network_stats")
            )
          ),

          # Network visualization
          column(
            width = 9,
            wellPanel(
              h5("Food Web Network Visualization"),
              visNetwork::visNetworkOutput("trait_network_plot", height = "500px"),
              br(),
              helpText(
                icon("info-circle"),
                " Tip: Drag nodes to rearrange. Zoom with mouse wheel. Click nodes for details."
              )
            ),

            wellPanel(
              h5("Interaction Probability Heatmap"),
              plotOutput("trait_probability_heatmap", height = "400px"),
              helpText("Color intensity indicates interaction probability between species pairs.")
            )
          )
        )
      ),

      # ========================================================================
      # TAB 5: PROBABILITY MATRICES
      # ========================================================================
      tabPanel(
        title = tagList(icon("table"), " Probability Matrices"),
        value = "matrices_tab",
        br(),

        fluidRow(
          column(
            width = 12,
            h4("Interaction Probability Matrices"),
            p("These matrices define the probability of interaction given trait combinations.",
              "The final interaction probability is calculated as the minimum across all five dimensions.",
              tags$a(href = "#", onclick = "Shiny.setInputValue('trait_help_button', Math.random());",
                    "See Methodology Guide for details →"))
          )
        ),

        fluidRow(
          column(
            width = 12,
            tabsetPanel(
              id = "prob_matrix_tabs",

              tabPanel(
                "MS × MS (Size)",
                br(),
                div(
                  class = "well",
                  h5("Consumer Size × Resource Size"),
                  p("Probability that a consumer of size class MSi can consume a resource of size class MSj."),
                  tags$ul(
                    tags$li("Diagonal values (0.5): Cannibalism or same-size predation"),
                    tags$li("Below diagonal (0.0): Smaller organisms cannot eat larger ones"),
                    tags$li("Above diagonal: Probability decreases with size difference"),
                    tags$li("Optimal prey size: 0.1-0.5 of consumer size")
                  )
                ),
                tableOutput("prob_matrix_MS_MS")
              ),

              tabPanel(
                "FS × MS (Foraging × Size)",
                br(),
                div(
                  class = "well",
                  h5("Consumer Foraging Strategy × Resource Size"),
                  p("Probability that a consumer with foraging strategy FSi can access a resource of size MSj."),
                  tags$ul(
                    tags$li("FS0 (Producer): No consumption (all zeros)"),
                    tags$li("FS1 (Predator): High for medium-large prey"),
                    tags$li("FS4-FS6 (Grazers/Filter feeders): High for small prey"),
                    tags$li("FS2 (Scavenger): Moderate across all sizes")
                  )
                ),
                tableOutput("prob_matrix_FS_MS")
              ),

              tabPanel(
                "MB × MB (Mobility)",
                br(),
                div(
                  class = "well",
                  h5("Consumer Mobility × Resource Mobility"),
                  p("Probability that a consumer with mobility MBi can capture a resource with mobility MBj."),
                  tags$ul(
                    tags$li("MB1 (Sessile) consumers: Limited to slow prey (filtering)"),
                    tags$li("MB5 (Swimmers): Can catch all prey types"),
                    tags$li("Diagonal: Moderate probability (equal speed)"),
                    tags$li("Below diagonal: Higher probability (faster catches slower)")
                  )
                ),
                tableOutput("prob_matrix_MB_MB")
              ),

              tabPanel(
                "EP × MS (Position × Size)",
                br(),
                div(
                  class = "well",
                  h5("Consumer Environmental Position × Resource Size"),
                  p("Probability that a consumer in position EPi can access a resource of size MSj."),
                  tags$ul(
                    tags$li("EP1 (Infaunal): Limited to small prey in sediment"),
                    tags$li("EP4 (Pelagic): Access to all sizes in water column"),
                    tags$li("EP2-EP3 (Benthic/Benthopelagic): Intermediate access"),
                    tags$li("Spatial overlap required for interaction")
                  )
                ),
                tableOutput("prob_matrix_EP_MS")
              ),

              tabPanel(
                "PR × MS (Protection × Size)",
                br(),
                div(
                  class = "well",
                  h5("Resource Protection × Consumer Size"),
                  p("Probability that a consumer of size MSi can overcome protection PRj (reduced by defense)."),
                  tags$ul(
                    tags$li("PR0 (No protection): Maximum vulnerability (1.0)"),
                    tags$li("PR6-PR8 (Hard shell/armor): Strong protection vs small predators"),
                    tags$li("Large predators (MS6-MS7): Can overcome most defenses"),
                    tags$li("Protection effectiveness decreases with predator size")
                  )
                ),
                tableOutput("prob_matrix_PR_MS")
              )
            )
          )
        )
      ),

      # ========================================================================
      # TAB 6: TRAIT REFERENCE
      # ========================================================================
      tabPanel(
        title = tagList(icon("book"), " Trait Reference"),
        value = "reference_tab",
        br(),

        fluidRow(
          column(
            width = 12,
            h4("Trait Code Reference Guide"),
            p("Detailed descriptions of all trait categories and codes.",
              tags$a(href = "#", onclick = "Shiny.setInputValue('trait_help_button', Math.random());",
                    "View Full Methodology Guide →"))
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
                htmlOutput("trait_ref_MS")
              ),

              tabPanel(
                "Foraging Strategy (FS)",
                br(),
                htmlOutput("trait_ref_FS")
              ),

              tabPanel(
                "Mobility (MB)",
                br(),
                htmlOutput("trait_ref_MB")
              ),

              tabPanel(
                "Environmental Position (EP)",
                br(),
                htmlOutput("trait_ref_EP")
              ),

              tabPanel(
                "Protection (PR)",
                br(),
                htmlOutput("trait_ref_PR")
              )
            )
          )
        )
      )
    ),

    # Help Modal (triggered by help button)
    bsModal(
      id = "trait_help_modal",
      title = tagList(icon("question-circle"), " Trait-Based Food Web Construction - Help & Methodology"),
      trigger = "trait_help_button",
      size = "large",

      # Tabbed help content
      tabsetPanel(
        id = "help_tabs",

        tabPanel(
          "Quick Start",
          br(),
          h4("Quick Start Guide"),
          htmlOutput("trait_help_quickstart")
        ),

        tabPanel(
          "Trait Dimensions",
          br(),
          h4("The Five Trait Dimensions"),
          htmlOutput("trait_help_dimensions")
        ),

        tabPanel(
          "Probability Matrices",
          br(),
          h4("How Interaction Probabilities Work"),
          htmlOutput("trait_help_matrices")
        ),

        tabPanel(
          "Automated Lookup",
          br(),
          h4("Automated Trait Lookup"),
          htmlOutput("trait_help_lookup")
        ),

        tabPanel(
          "Best Practices",
          br(),
          h4("Best Practices & Tips"),
          htmlOutput("trait_help_best_practices")
        ),

        tabPanel(
          "Full Methodology",
          br(),
          h4("Complete Methodology Guide"),
          p("For comprehensive documentation, see:"),
          tags$ul(
            tags$li(tags$strong("Methodology Guide:"), " docs/TRAIT_FOODWEB_METHODOLOGY.md"),
            tags$li(tags$strong("Trait Lookup Guide:"), " docs/AUTOMATED_TRAIT_LOOKUP_GUIDE.md"),
            tags$li(tags$strong("Quick Start:"), " docs/TRAIT_LOOKUP_QUICKSTART.md"),
            tags$li(tags$strong("Database Setup:"), " docs/DATABASE_SETUP_GUIDE.md")
          ),
          hr(),
          downloadButton("trait_download_methodology", "Download Full Methodology PDF", class = "btn-primary")
        )
      )
    )
  )
}
