# Harmonization Settings UI
# GUI for configuring trait harmonization thresholds and rules

harmonization_settings_ui <- function() {
  tagList(
    h3(icon("sliders-h"), " Harmonization Configuration"),
    p("Configure how raw trait data is converted to categorical trait classes."),
    hr(),
    
    tabsetPanel(
      id = "harm_tabs",
      type = "pills",
      
      # TAB 1: SIZE THRESHOLDS
      tabPanel(
        title = tagList(icon("ruler"), " Size Thresholds"),
        value = "size_tab",
        br(),
        
        fluidRow(
          column(6,
            h4("Maximum Size (MS) Class Boundaries"),
            sliderInput("harm_thresh_MS1_MS2", "MS1/MS2 boundary:", 
                       min = 0.01, max = 0.5, value = 0.1, step = 0.01, post = " cm"),
            sliderInput("harm_thresh_MS2_MS3", "MS2/MS3 boundary:", 
                       min = 0.1, max = 5.0, value = 1.0, step = 0.1, post = " cm"),
            sliderInput("harm_thresh_MS3_MS4", "MS3/MS4 boundary:", 
                       min = 1.0, max = 20.0, value = 5.0, step = 0.5, post = " cm"),
            sliderInput("harm_thresh_MS4_MS5", "MS4/MS5 boundary:", 
                       min = 5.0, max = 50.0, value = 20.0, step = 1.0, post = " cm"),
            sliderInput("harm_thresh_MS5_MS6", "MS5/MS6 boundary:", 
                       min = 20.0, max = 100.0, value = 50.0, step = 5.0, post = " cm"),
            sliderInput("harm_thresh_MS6_MS7", "MS6/MS7 boundary:", 
                       min = 50.0, max = 300.0, value = 150.0, step = 10.0, post = " cm")
          ),
          column(6,
            h4("Size Class Preview"),
            actionButton("harm_preview_size", "Generate Preview", class = "btn-info btn-sm"),
            br(), br(),
            plotOutput("harm_size_distribution_plot", height = "400px")
          )
        )
      ),
      
      # TAB 2: FORAGING PATTERNS
      tabPanel(
        title = tagList(icon("utensils"), " Foraging Patterns"),
        value = "foraging_tab",
        br(),
        h4("Foraging Strategy (FS) Pattern Matching"),
        fluidRow(
          column(6,
            textInput("harm_pattern_FS0", "FS0 - Primary Producer:", 
                     value = "photosyn|autotrop|producer|plant|algae"),
            textInput("harm_pattern_FS1", "FS1 - Predator:", 
                     value = "predat|carnivor|pisciv|hunter"),
            textInput("harm_pattern_FS2", "FS2 - Scavenger:", 
                     value = "scaveng|detritivor|carrion")
          ),
          column(6,
            textInput("harm_pattern_FS4", "FS4 - Grazer:", 
                     value = "graz|herbiv|scraper|browser"),
            textInput("harm_pattern_FS5", "FS5 - Deposit Feeder:", 
                     value = "deposit|sediment|burrower"),
            textInput("harm_pattern_FS6", "FS6 - Filter Feeder:", 
                     value = "filter|suspension|planktivor")
          )
        )
      ),
      
      # TAB 3: TAXONOMIC RULES
      tabPanel(
        title = tagList(icon("dna"), " Taxonomic Rules"),
        value = "taxonomic_tab",
        br(),
        h4("Taxonomic Inference Rules"),
        fluidRow(
          column(4,
            h5("Mobility Rules"),
            checkboxInput("harm_rule_fish_swimmers", "Fish -> MB5", TRUE),
            checkboxInput("harm_rule_bivalves_sessile", "Bivalves -> MB1/MB2", TRUE),
            checkboxInput("harm_rule_gastropods_crawlers", "Gastropods -> MB3", TRUE)
          ),
          column(4,
            h5("Foraging Rules"),
            checkboxInput("harm_rule_phytoplankton_producers", "Phytoplankton -> FS0", TRUE),
            checkboxInput("harm_rule_bivalves_filter", "Bivalves -> FS6", TRUE)
          ),
          column(4,
            h5("Protection Rules"),
            checkboxInput("harm_rule_molluscs_shells", "Molluscs -> PR3", TRUE),
            checkboxInput("harm_rule_arthropods_exo", "Arthropods -> PR3", TRUE)
          )
        )
      ),
      
      # TAB 4: ECOSYSTEM PROFILES
      tabPanel(
        title = tagList(icon("water"), " Ecosystem Profiles"),
        value = "ecosystem_tab",
        br(),
        h4("Ecosystem-Specific Harmonization"),
        fluidRow(
          column(6,
            selectInput("harm_active_profile", "Active Profile:",
                       choices = c(
                         "Arctic/Subarctic (Baltic Sea)" = "arctic",
                         "Temperate (North Sea)" = "temperate",
                         "Tropical/Subtropical" = "tropical"
                       ),
                       selected = "temperate"),
            br(),
            uiOutput("harm_profile_details")
          ),
          column(6,
            h5("Profile Effects"),
            verbatimTextOutput("harm_profile_effects")
          )
        )
      ),
      
      # TAB 5: IMPORT/EXPORT
      tabPanel(
        title = tagList(icon("file-export"), " Import/Export"),
        value = "import_export_tab",
        br(),
        h4("Save and Load Configurations"),
        fluidRow(
          column(6,
            h5("Export Configuration"),
            downloadButton("harm_export_json", "Export as JSON", class = "btn-success")
          ),
          column(6,
            h5("Import Configuration"),
            fileInput("harm_import_json", "Select JSON:", accept = c(".json"))
          )
        )
      )
    ),
    
    hr(),
    
    # ACTION BUTTONS
    fluidRow(
      column(12,
        div(style = "text-align: center;",
          actionButton("harm_save_config", "Apply Changes", class = "btn-success btn-lg"),
          actionButton("harm_reset_defaults", "Reset to Defaults", class = "btn-warning"),
          actionButton("harm_cancel", "Cancel", class = "btn-secondary")
        )
      )
    ),
    
    br(),
    uiOutput("harm_status_message")
  )
}
