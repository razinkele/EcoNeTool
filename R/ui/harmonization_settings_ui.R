# Harmonization Settings UI
# GUI for configuring trait harmonization thresholds and rules

# One text input per configured foraging pattern (inputId harm_pattern_<key>).
# Keys match HARMONIZATION_CONFIG$foraging_patterns exactly. The inputs start
# EMPTY: the server pushes the real patterns from the session config at start,
# so no stale literal (FS0 once carried "plant|algae", removed 2026-07-17
# because it inverted trophic structure) can ever be the source of truth.
HARM_FS_PATTERN_LABELS <- c(
  FS0_primary_producer = "FS0 - Primary producer:",
  FS1_predator = "FS1 - Predator:",
  FS2_scavenger = "FS2 - Scavenger / detritivore:",
  FS3_omnivore = "FS3 - Omnivore:",
  FS4_grazer = "FS4 - Grazer / herbivore:",
  FS5_deposit = "FS5 - Deposit feeder:",
  FS6_filter = "FS6 - Filter / suspension feeder:",
  FS7_xylophagous = "FS7 - Xylophagous:"
)

# The taxonomic rules the harmonize_* code actually reads through
# is_rule_enabled() (R/functions/trait_lookup/harmonization.R). One checkbox
# per rule, inputId harm_rule_<name>; the server wires the same vector.
# tests/testthat/test-ui-inputs-have-handlers.R fails if this list and the
# is_rule_enabled() calls drift apart.
CONSUMED_TAXONOMIC_RULES <- c(
  fish_obligate_swimmers = "Fish -> MB5 (swimmer)",
  cephalopods_swimmers = "Cephalopods -> MB5 (swimmer)",
  bivalves_sessile = "Bivalves -> MB1 (sessile)",
  cnidarians_sessile = "Cnidarians -> MB1 (sessile)",
  phytoplankton_pelagic = "Phytoplankton -> EP1 (pelagic)",
  zooplankton_pelagic = "Copepods / cladocerans -> EP1 (pelagic)",
  infaunal_bivalves = "Bivalves -> EP4 (endobenthic)",
  bivalves_hard_shell = "Bivalves -> PR6 (hard shell)",
  gastropods_hard_shell = "Gastropods -> PR6 (hard shell)",
  crustaceans_exoskeleton = "Crustaceans -> PR8 / PR4 (exoskeleton)",
  echinoderms_calcium_plates = "Echinoderms -> PR5 (calcium plates)"
)

harm_rule_checkboxes <- function(rules) {
  lapply(rules, function(rule) {
    checkboxInput(paste0("harm_rule_", rule), CONSUMED_TAXONOMIC_RULES[[rule]], TRUE)
  })
}

harmonization_settings_ui <- function() {
  fs_keys <- names(HARM_FS_PATTERN_LABELS)
  fs_input <- function(key) textInput(paste0("harm_pattern_", key), HARM_FS_PATTERN_LABELS[[key]], value = "")

  tagList(
    h3(icon("sliders-h"), " Harmonization Configuration"),
    p("Configure how raw trait data is converted to categorical trait classes."),
    hr(),

    tabsetPanel(
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
        p(class = "text-muted",
          "Regular expressions matched (case-insensitive) against feeding text, FS0 first. ",
          "An invalid or empty pattern is ignored and the previous one kept."),
        fluidRow(
          column(6, lapply(fs_keys[1:4], fs_input)),
          column(6, lapply(fs_keys[5:8], fs_input))
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
            harm_rule_checkboxes(c("fish_obligate_swimmers", "cephalopods_swimmers",
                                   "bivalves_sessile", "cnidarians_sessile"))
          ),
          column(4,
            h5("Position Rules"),
            harm_rule_checkboxes(c("phytoplankton_pelagic", "zooplankton_pelagic",
                                   "infaunal_bivalves"))
          ),
          column(4,
            h5("Protection Rules"),
            harm_rule_checkboxes(c("bivalves_hard_shell", "gastropods_hard_shell",
                                   "crustaceans_exoskeleton", "echinoderms_calcium_plates"))
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
                         "Temperate (North Sea)" = "temperate",
                         "Mediterranean" = "mediterranean",
                         "Atlantic NE" = "atlantic_ne",
                         "Arctic/Nordic" = "arctic",
                         "Baltic Sea" = "baltic",
                         "Black Sea" = "black_sea",
                         "Tropical/Subtropical" = "tropical",
                         "Deep Sea" = "deep_sea"
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
            fileInput("harm_import_json", "Select JSON:", accept = c(".json")),
            p(class = "text-muted", "An imported file applies to your session only.")
          )
        )
      )
    ),

    hr(),

    # ACTION BUTTONS
    fluidRow(
      column(12,
        div(style = "text-align: center;",
          actionButton("harm_reset_defaults", "Reset to Defaults", class = "btn-warning"),
          actionButton("harm_save_config", "Save as server default",
                       class = "btn-success", icon = icon("lock")),
          actionButton("harm_reset_server_default", "Reset server default",
                       class = "btn-outline-danger", icon = icon("lock"))
        ),
        p(class = "text-muted", style = "text-align: center; margin-top: 8px;",
          "Changes on this tab apply to your session only. Saving or resetting the server default ",
          "needs an admin unlock (Trait Research > Configure API Keys).")
      )
    ),

    br(),
    uiOutput("harm_status_message")
  )
}
