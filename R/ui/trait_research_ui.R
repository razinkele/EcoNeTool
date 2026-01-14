# UI for Trait Research Module
# Split into three tabs: Species List/Lookup Status, Found Traits, Trait Code Reference

trait_research_ui <- function() {
  tabItem(
    tabName = "trait_research",

    # Compact header with title and help button
    fluidRow(
      column(
        width = 9,
        h4(icon("search"), " Trait Research",
           tags$small(class = "text-muted", " - Query species traits from online databases")),
        style = "margin-bottom: 5px;"
      ),
      column(
        width = 3,
        style = "text-align: right;",
        actionButton(
          "trait_research_help_button",
          tagList(icon("question-circle"), " Help"),
          class = "btn-info btn-sm"
        )
      )
    ),

    # Main content - two column layout
    fluidRow(
      # Left column: Species input, databases, and action buttons side by side
      column(
        width = 4,

        # Row with Input Panel and Action Buttons
        fluidRow(
          # Species Input Panel (left)
          column(
            width = 8,
            wellPanel(
              style = "padding: 10px; margin-bottom: 10px;",
              h5(icon("list"), " Species Input", style = "margin-top: 0;"),

              # Input method selection (inline to save space)
              radioButtons(
                "trait_research_input_method",
                NULL,
                choices = c(
                  "Manual" = "manual",
                  "File" = "file",
                  "Test" = "test"
                ),
                selected = "manual",
                inline = TRUE
              ),

              # Manual input
              conditionalPanel(
                condition = "input.trait_research_input_method == 'manual'",
                textAreaInput(
                  "trait_research_species_list",
                  NULL,
                  value = "Gadus morhua\nClupea harengus\nSprattus sprattus\nPlatichthys flesus\nPerca fluviatilis",
                  rows = 6,
                  placeholder = "One species per line..."
                )
              ),

              # File upload
              conditionalPanel(
                condition = "input.trait_research_input_method == 'file'",
                fileInput(
                  "trait_research_species_file",
                  NULL,
                  accept = c(".csv", ".txt"),
                  placeholder = "Upload CSV/TXT"
                ),
                tags$small(class = "text-muted", "One species per line or 'species' column")
              ),

              # Test dataset
              conditionalPanel(
                condition = "input.trait_research_input_method == 'test'",
                selectInput(
                  "trait_research_test_dataset",
                  NULL,
                  choices = c(
                    "Baltic Full (67)" = "baltic_full",
                    "Fish (20)" = "baltic_fish",
                    "Benthos (17)" = "baltic_benthos",
                    "Plankton (17)" = "baltic_plankton",
                    "Top Pred (15)" = "baltic_top_predators"
                  ),
                  selected = "baltic_full"
                ),
                actionButton(
                  "trait_research_load_test",
                  "Load",
                  icon = icon("database"),
                  class = "btn-info btn-sm btn-block"
                )
              )
            )
          ),

          # Action Buttons Panel (right)
          column(
            width = 4,
            wellPanel(
              style = "padding: 10px; margin-bottom: 10px;",
              h5(icon("play"), " Actions", style = "margin-top: 0;"),

              actionButton(
                "trait_research_run_lookup",
                tagList(icon("search"), " Run"),
                class = "btn-success btn-block",
                style = "margin-bottom: 8px;"
              ),

              actionButton(
                "trait_research_clear",
                tagList(icon("trash"), " Clear"),
                class = "btn-outline-secondary btn-block btn-sm"
              ),

              hr(style = "margin: 10px 0;"),

              tags$small(
                class = "text-muted",
                icon("lightbulb"), " Use scientific names"
              )
            )
          )
        ),

        # Database Selection Panel (full width below)
        wellPanel(
          style = "padding: 10px;",
          h5(icon("database"), " Databases", style = "margin-top: 0;"),

          checkboxGroupInput(
            "trait_research_databases",
            NULL,
            choices = c(
              "WoRMS (taxonomy)" = "worms",
              "FishBase (fish)" = "fishbase",
              "SeaLifeBase (inverts)" = "sealifebase",
              "BIOTIC (benthic)" = "biotic",
              "freshwaterecology" = "freshwater",
              "MAREDAT (zooplankton)" = "maredat",
              "PTDB (phytoplankton)" = "ptdb",
              "AlgaeBase" = "algaebase",
              "SHARK (Swedish)" = "shark"
            ),
            selected = c("worms", "fishbase", "sealifebase")
          )
        )
      ),

      # Right column: Tabbed results panel
      column(
        width = 8,

        # Three-tab panel
        tabsetPanel(
          id = "trait_research_tabs",
          type = "tabs",

          # =================================================================
          # TAB 1: Species List & Lookup Status
          # =================================================================
          tabPanel(
            title = tagList(icon("list-check"), " Species & Status"),
            value = "species_status",

            br(),

            fluidRow(
              # Species List Display
              column(
                width = 6,
                wellPanel(
                  h4(icon("list"), " Species List"),
                  p(class = "text-muted", "Species to be looked up:"),

                  div(
                    style = "max-height: 300px; overflow-y: auto; border: 1px solid #ddd; border-radius: 4px; padding: 10px; background-color: #f9f9f9;",
                    uiOutput("trait_research_species_display")
                  ),

                  br(),

                  fluidRow(
                    column(
                      width = 6,
                      tags$span(
                        class = "badge bg-primary",
                        style = "font-size: 14px;",
                        textOutput("trait_research_species_count", inline = TRUE)
                      )
                    ),
                    column(
                      width = 6,
                      style = "text-align: right;",
                      actionButton(
                        "trait_research_validate_names",
                        tagList(icon("check-circle"), " Validate"),
                        class = "btn-outline-info btn-sm"
                      )
                    )
                  )
                )
              ),

              # Lookup Status
              column(
                width = 6,
                wellPanel(
                  h4(icon("tasks"), " Lookup Status"),
                  uiOutput("trait_research_status"),

                  hr(),

                  h5(icon("chart-pie"), " Progress Summary"),
                  uiOutput("trait_research_progress_summary")
                )
              )
            ),

            # Lookup Progress Details
            wellPanel(
              h4(icon("clipboard-list"), " Lookup Progress"),
              p(class = "text-muted", "Real-time progress of database queries."),

              div(
                style = "max-height: 250px; overflow-y: auto;",
                uiOutput("trait_research_progress_log")
              )
            )
          ),

          # =================================================================
          # TAB 2: Found Traits Table & Summary
          # =================================================================
          tabPanel(
            title = tagList(icon("table"), " Found Traits"),
            value = "traits_table",

            br(),

            # Summary Statistics
            wellPanel(
              h4(icon("chart-bar"), " Trait Summary"),

              fluidRow(
                column(
                  width = 3,
                  div(
                    class = "info-box bg-info",
                    style = "min-height: 80px;",
                    tags$span(class = "info-box-icon", icon("fish")),
                    div(
                      class = "info-box-content",
                      tags$span(class = "info-box-text", "Species Found"),
                      tags$span(class = "info-box-number", textOutput("trait_summary_species", inline = TRUE))
                    )
                  )
                ),
                column(
                  width = 3,
                  div(
                    class = "info-box bg-success",
                    style = "min-height: 80px;",
                    tags$span(class = "info-box-icon", icon("check")),
                    div(
                      class = "info-box-content",
                      tags$span(class = "info-box-text", "Complete Traits"),
                      tags$span(class = "info-box-number", textOutput("trait_summary_complete", inline = TRUE))
                    )
                  )
                ),
                column(
                  width = 3,
                  div(
                    class = "info-box bg-warning",
                    style = "min-height: 80px;",
                    tags$span(class = "info-box-icon", icon("exclamation-triangle")),
                    div(
                      class = "info-box-content",
                      tags$span(class = "info-box-text", "Partial Traits"),
                      tags$span(class = "info-box-number", textOutput("trait_summary_partial", inline = TRUE))
                    )
                  )
                ),
                column(
                  width = 3,
                  div(
                    class = "info-box bg-danger",
                    style = "min-height: 80px;",
                    tags$span(class = "info-box-icon", icon("times")),
                    div(
                      class = "info-box-content",
                      tags$span(class = "info-box-text", "Not Found"),
                      tags$span(class = "info-box-number", textOutput("trait_summary_notfound", inline = TRUE))
                    )
                  )
                )
              ),

              # Database coverage summary
              uiOutput("trait_research_summary")
            ),

            # Found Traits Table
            wellPanel(
              h4(icon("table"), " Found Traits Table"),
              p(class = "text-muted",
                "View trait data retrieved from databases. Click column headers to sort."),

              # Export buttons
              fluidRow(
                column(
                  width = 3,
                  downloadButton(
                    "trait_research_download_csv",
                    "Download CSV",
                    class = "btn-primary btn-block"
                  )
                ),
                column(
                  width = 3,
                  downloadButton(
                    "trait_research_download_rds",
                    "Download RDS",
                    class = "btn-secondary btn-block"
                  )
                ),
                column(
                  width = 3,
                  downloadButton(
                    "trait_research_download_excel",
                    "Download Excel",
                    class = "btn-outline-success btn-block"
                  )
                ),
                column(
                  width = 3,
                  actionButton(
                    "trait_research_use_in_foodweb",
                    tagList(icon("arrow-right"), " Use in Food Web"),
                    class = "btn-success btn-block"
                  )
                )
              ),

              br(),

              # Main trait table
              div(
                style = "overflow-x: auto;",
                DT::dataTableOutput("trait_research_table", height = "400px")
              )
            ),

            # Raw data details (collapsible)
            wellPanel(
              h4(
                icon("microscope"),
                " Raw Trait Details",
                actionButton(
                  "trait_research_toggle_raw",
                  icon("chevron-down"),
                  class = "btn-link btn-sm float-right",
                  style = "margin-top: -5px;"
                )
              ),

              conditionalPanel(
                condition = "input.trait_research_toggle_raw % 2 == 1",
                p(class = "text-muted",
                  "Detailed raw data from database queries (before harmonization)."),

                selectInput(
                  "trait_research_raw_species",
                  "Select species to view details:",
                  choices = NULL
                ),

                htmlOutput("trait_research_raw_details")
              )
            )
          ),

          # =================================================================
          # TAB 3: Trait Code Reference
          # =================================================================
          tabPanel(
            title = tagList(icon("book"), " Trait Reference"),
            value = "trait_reference",

            br(),

            # Quick Reference Cards
            fluidRow(
              column(
                width = 6,

                # Size Traits
                wellPanel(
                  h4(icon("ruler"), " MS - Maximum Size"),
                  tags$table(
                    class = "table table-striped table-sm",
                    tags$thead(
                      tags$tr(
                        tags$th("Code"),
                        tags$th("Size Range"),
                        tags$th("Examples")
                      )
                    ),
                    tags$tbody(
                      tags$tr(tags$td("MS1"), tags$td("< 0.1 cm"), tags$td("Bacteria, small phytoplankton")),
                      tags$tr(tags$td("MS2"), tags$td("0.1 - 0.5 cm"), tags$td("Copepods, rotifers")),
                      tags$tr(tags$td("MS3"), tags$td("0.5 - 1 cm"), tags$td("Small amphipods, mysids")),
                      tags$tr(tags$td("MS4"), tags$td("1 - 3 cm"), tags$td("Shrimps, small fish larvae")),
                      tags$tr(tags$td("MS5"), tags$td("3 - 10 cm"), tags$td("Small fish, crabs")),
                      tags$tr(tags$td("MS6"), tags$td("10 - 50 cm"), tags$td("Medium fish, lobsters")),
                      tags$tr(tags$td("MS7"), tags$td("> 50 cm"), tags$td("Large fish, seals"))
                    )
                  )
                ),

                # Foraging Strategy
                wellPanel(
                  h4(icon("utensils"), " FS - Foraging Strategy"),
                  tags$table(
                    class = "table table-striped table-sm",
                    tags$thead(
                      tags$tr(
                        tags$th("Code"),
                        tags$th("Strategy"),
                        tags$th("Description")
                      )
                    ),
                    tags$tbody(
                      tags$tr(tags$td("FS0"), tags$td("Producer"), tags$td("Photosynthetic organisms")),
                      tags$tr(tags$td("FS1"), tags$td("Herbivore"), tags$td("Feeds on plants/algae")),
                      tags$tr(tags$td("FS2"), tags$td("Omnivore"), tags$td("Mixed diet")),
                      tags$tr(tags$td("FS3"), tags$td("Predator"), tags$td("Active hunting")),
                      tags$tr(tags$td("FS4"), tags$td("Scavenger"), tags$td("Feeds on dead matter")),
                      tags$tr(tags$td("FS5"), tags$td("Deposit feeder"), tags$td("Feeds on sediment")),
                      tags$tr(tags$td("FS6"), tags$td("Filter feeder"), tags$td("Filters particles from water"))
                    )
                  )
                )
              ),

              column(
                width = 6,

                # Mobility
                wellPanel(
                  h4(icon("walking"), " MB - Mobility"),
                  tags$table(
                    class = "table table-striped table-sm",
                    tags$thead(
                      tags$tr(
                        tags$th("Code"),
                        tags$th("Mobility"),
                        tags$th("Examples")
                      )
                    ),
                    tags$tbody(
                      tags$tr(tags$td("MB1"), tags$td("Sessile"), tags$td("Barnacles, mussels, sponges")),
                      tags$tr(tags$td("MB2"), tags$td("Limited movement"), tags$td("Sea anemones, some worms")),
                      tags$tr(tags$td("MB3"), tags$td("Crawler"), tags$td("Crabs, sea stars, snails")),
                      tags$tr(tags$td("MB4"), tags$td("Burrower"), tags$td("Lugworms, clams")),
                      tags$tr(tags$td("MB5"), tags$td("Swimmer"), tags$td("Fish, squid, jellyfish"))
                    )
                  )
                ),

                # Ecosystem Position
                wellPanel(
                  h4(icon("layer-group"), " EP - Ecosystem Position"),
                  tags$table(
                    class = "table table-striped table-sm",
                    tags$thead(
                      tags$tr(
                        tags$th("Code"),
                        tags$th("Position"),
                        tags$th("Description")
                      )
                    ),
                    tags$tbody(
                      tags$tr(tags$td("EP1"), tags$td("Infaunal"), tags$td("Lives within sediment")),
                      tags$tr(tags$td("EP2"), tags$td("Epifaunal"), tags$td("Lives on sediment surface")),
                      tags$tr(tags$td("EP3"), tags$td("Demersal"), tags$td("Near-bottom dwelling")),
                      tags$tr(tags$td("EP4"), tags$td("Pelagic"), tags$td("Open water column"))
                    )
                  )
                ),

                # Protection
                wellPanel(
                  h4(icon("shield-alt"), " PR - Protection"),
                  tags$table(
                    class = "table table-striped table-sm",
                    tags$thead(
                      tags$tr(
                        tags$th("Code"),
                        tags$th("Protection"),
                        tags$th("Examples")
                      )
                    ),
                    tags$tbody(
                      tags$tr(tags$td("PR0"), tags$td("None"), tags$td("Jellyfish, soft worms")),
                      tags$tr(tags$td("PR1"), tags$td("Mucus/slime"), tags$td("Hagfish, some fish")),
                      tags$tr(tags$td("PR2"), tags$td("Soft tissue"), tags$td("Sea slugs")),
                      tags$tr(tags$td("PR3"), tags$td("Spines"), tags$td("Sea urchins, some fish")),
                      tags$tr(tags$td("PR4"), tags$td("Tube"), tags$td("Tube worms")),
                      tags$tr(tags$td("PR5"), tags$td("Soft shell"), tags$td("Crabs (molting)")),
                      tags$tr(tags$td("PR6"), tags$td("Hard shell"), tags$td("Mussels, snails")),
                      tags$tr(tags$td("PR7"), tags$td("Scales"), tags$td("Most fish")),
                      tags$tr(tags$td("PR8"), tags$td("Armoured"), tags$td("Lobsters, sturgeon"))
                    )
                  )
                )
              )
            ),

            # Additional information
            wellPanel(
              h4(icon("info-circle"), " About Trait Harmonization"),
              p("Raw trait data from databases is automatically converted to these categorical codes based on the following rules:"),

              tags$ul(
                tags$li(tags$strong("Size (MS):"), " Derived from maximum body length/size measurements"),
                tags$li(tags$strong("Foraging (FS):"), " Inferred from trophic level, diet composition, and feeding type"),
                tags$li(tags$strong("Mobility (MB):"), " Based on locomotion type and attachment status"),
                tags$li(tags$strong("Position (EP):"), " Determined from habitat depth and substrate association"),
                tags$li(tags$strong("Protection (PR):"), " Assessed from body covering and defensive structures")
              ),

              hr(),

              fluidRow(
                column(
                  width = 6,
                  actionButton(
                    "trait_research_full_reference",
                    tagList(icon("external-link-alt"), " Open Full Reference Guide"),
                    class = "btn-info"
                  )
                ),
                column(
                  width = 6,
                  actionButton(
                    "trait_research_download_reference",
                    tagList(icon("download"), " Download Reference PDF"),
                    class = "btn-outline-secondary"
                  )
                )
              )
            )
          )
        )  # End tabsetPanel
      )  # End right column
    ),  # End fluidRow

    # Help Modal
    bsModal(
      id = "trait_research_help_modal",
      title = tagList(icon("question-circle"), " Trait Research Help"),
      trigger = "trait_research_help_button",
      size = "large",

      h4("Overview"),
      p("The Trait Research module allows you to query multiple online databases to retrieve functional trait data for marine and freshwater species."),

      h4("Tabs"),
      tags$ul(
        tags$li(tags$strong("Species & Status:"), " View your species list and monitor lookup progress in real-time"),
        tags$li(tags$strong("Found Traits:"), " Browse and export the retrieved trait data table with summary statistics"),
        tags$li(tags$strong("Trait Reference:"), " Quick reference guide for trait codes (MS, FS, MB, EP, PR)")
      ),

      h4("Workflow"),
      tags$ol(
        tags$li("Enter species names manually or upload a file with species list"),
        tags$li("Select which databases to query"),
        tags$li("Click 'Run Trait Lookup' to start the search"),
        tags$li("Monitor progress in the 'Species & Status' tab"),
        tags$li("View results in the 'Found Traits' tab"),
        tags$li("Export data as CSV/RDS/Excel or use directly in Food Web Construction")
      ),

      h4("Databases"),
      tags$ul(
        tags$li(tags$strong("WoRMS:"), " World Register of Marine Species - taxonomy and classification"),
        tags$li(tags$strong("FishBase:"), " Fish traits - size, trophic level, habitat, depth"),
        tags$li(tags$strong("SeaLifeBase:"), " Marine invertebrates - crustaceans, mollusks, echinoderms"),
        tags$li(tags$strong("BIOTIC:"), " Benthic invertebrate biological traits (local file)"),
        tags$li(tags$strong("freshwaterecology.info:"), " Freshwater species traits (API)"),
        tags$li(tags$strong("MAREDAT:"), " Zooplankton functional traits (local file)"),
        tags$li(tags$strong("PTDB:"), " Phytoplankton traits (local file)"),
        tags$li(tags$strong("AlgaeBase:"), " Algae taxonomy via WoRMS"),
        tags$li(tags$strong("SHARK:"), " Swedish Ocean Archives - occurrence and depth data")
      ),

      h4("Tips"),
      tags$ul(
        tags$li("Always include WoRMS for accurate taxonomic classification"),
        tags$li("Use exact scientific names (e.g., 'Gadus morhua' not 'Atlantic cod')"),
        tags$li("Results are cached for 30 days to speed up repeated queries"),
        tags$li("Click 'Use in Food Web' to transfer trait data to the Food Web Construction module")
      )
    )
  )
}
