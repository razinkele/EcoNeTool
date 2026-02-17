#' Metaweb Manager Tab UI
#'
#' Creates the metaweb manager tab (Phase 2).
#'
#' @return A tabItem for metaweb manager
metaweb_ui <- function() {
      # METAWEB MANAGER TAB (MARBEFES WP3.2 Phase 2)
      # ========================================================================
      tabItem(
        tabName = "metaweb_manager",

        fluidRow(
          box(
            title = "Metaweb Manager",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            HTML("
              <h4>Regional Metaweb Management</h4>
              <p><strong>MARBEFES WP3.2 Phase 2:</strong> Assemble and manage regional metawebs for ecological interaction network analysis.</p>
              <p>A <strong>metaweb</strong> contains all documented species and trophic interactions in a region, serving as the basis for extracting local food webs.</p>
              <ul>
                <li><strong>Load:</strong> Import pre-built regional metawebs or custom CSV files</li>
                <li><strong>View:</strong> Visualize metaweb structure and browse species/interactions</li>
                <li><strong>Edit:</strong> Add/remove species and trophic links</li>
                <li><strong>Quality:</strong> Assess link evidence quality (1-4 scale)</li>
                <li><strong>Export:</strong> Download metawebs in CSV or RDS format</li>
              </ul>
            ")
          )
        ),

        fluidRow(
          tabBox(
            width = 12,
            id = "metaweb_tabs",
            selected = "Load Metaweb",

            # ==================================================================
            # PANEL 1: LOAD METAWEB
            # ==================================================================
            tabPanel(
              title = "Load Metaweb",
              icon = icon("database"),

              fluidRow(
                # Left column: Regional metawebs
                column(
                  width = 6,
                  box(
                    title = "Pre-built Regional Metawebs",
                    status = "info",
                    solidHeader = TRUE,
                    width = NULL,
                    HTML("<p>Load metawebs from published literature sources (MARBEFES guidance):</p>"),
                    selectInput(
                      "regional_metaweb",
                      "Select Region:",
                      choices = c(
                        "-- Select a region --" = "",
                        "Arctic: Barents Sea - Arctic (Kortsch 2015)" = "arctic_barents_arctic",
                        "Arctic: Barents Sea - Boreal (Kortsch 2015)" = "arctic_barents_boreal",
                        "Arctic: Kongsfjorden (Farage 2021)" = "arctic_kongsfjorden",
                        "Baltic: Nordström et al. 2015" = "baltic_nordstrom",
                        "Baltic: Kortsch et al. 2021" = "baltic_kortsch",
                        "Baltic: Garrison et al. 2022" = "baltic_garrison",
                        "Atlantic: North Sea (Frelat 2022)" = "atlantic_northsea",
                        "Mediterranean: Albuoy et al. 2014" = "mediterranean"
                      ),
                      selected = ""
                    ),
                    actionButton(
                      "load_regional_btn",
                      "Load Regional Metaweb",
                      class = "btn-primary btn-block",
                      icon = icon("download")
                    ),
                    hr(),
                    HTML("<p><small><strong>Note:</strong> Some metawebs require download from literature sources. See metawebs/README.md for details.</small></p>")
                  )
                ),

                # Right column: Custom import
                column(
                  width = 6,
                  box(
                    title = "Import Custom Metaweb",
                    status = "success",
                    solidHeader = TRUE,
                    width = NULL,
                    HTML("<p>Upload your own metaweb from CSV files:</p>"),
                    fileInput(
                      "metaweb_species_file",
                      "Species CSV File:",
                      accept = c(".csv", ".txt"),
                      placeholder = "species.csv"
                    ),
                    fileInput(
                      "metaweb_interactions_file",
                      "Interactions CSV File:",
                      accept = c(".csv", ".txt"),
                      placeholder = "interactions.csv"
                    ),
                    actionButton(
                      "import_metaweb_btn",
                      "Import Metaweb",
                      class = "btn-success btn-block",
                      icon = icon("upload")
                    ),
                    hr(),
                    HTML("<p><small>Use template files in <code>metawebs/</code> folder as examples.</small></p>")
                  )
                )
              ),

              fluidRow(
                box(
                  title = "Current Metaweb Summary",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  verbatimTextOutput("metaweb_summary")
                )
              )
            ),

            # ==================================================================
            # PANEL 2: VIEW METAWEB
            # ==================================================================
            tabPanel(
              title = "View Metaweb",
              icon = icon("eye"),

              fluidRow(
                box(
                  title = "Metaweb Network Visualization",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  HTML("<p>Interactive visualization of all species and trophic interactions in the metaweb:</p>"),
                  visNetworkOutput("metaweb_network", height = "600px")
                )
              ),

              fluidRow(
                column(
                  width = 6,
                  box(
                    title = "Species List",
                    status = "info",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = NULL,
                    HTML("<p>All species in the metaweb:</p>"),
                    DTOutput("metaweb_species_table")
                  )
                ),
                column(
                  width = 6,
                  box(
                    title = "Trophic Interactions",
                    status = "info",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = NULL,
                    HTML("<p>All documented feeding links:</p>"),
                    DTOutput("metaweb_interactions_table")
                  )
                )
              )
            ),

            # ==================================================================
            # PANEL 3: EDIT METAWEB
            # ==================================================================
            tabPanel(
              title = "Edit Metaweb",
              icon = icon("edit"),

              fluidRow(
                # Add/Remove Species
                column(
                  width = 6,
                  box(
                    title = "Add Species",
                    status = "success",
                    solidHeader = TRUE,
                    width = NULL,
                    textInput("new_species_id", "Species ID:", placeholder = "SP001"),
                    textInput("new_species_name", "Species Name:", placeholder = "Gadus morhua"),
                    selectInput(
                      "new_species_fg",
                      "Functional Group:",
                      choices = c("Fish", "Benthos", "Zooplankton", "Phytoplankton", "Detritus", "Mammal", "Bird", "Other")
                    ),
                    actionButton("add_species_btn", "Add Species", class = "btn-success", icon = icon("plus")),
                    hr(),
                    h5("Remove Species"),
                    selectInput("remove_species_id", "Select Species:", choices = NULL),
                    checkboxInput("remove_species_links", "Remove associated links", value = TRUE),
                    actionButton("remove_species_btn", "Remove Species", class = "btn-danger", icon = icon("trash"))
                  )
                ),

                # Add/Remove Links
                column(
                  width = 6,
                  box(
                    title = "Add Trophic Link",
                    status = "success",
                    solidHeader = TRUE,
                    width = NULL,
                    selectInput("link_predator", "Predator:", choices = NULL),
                    selectInput("link_prey", "Prey:", choices = NULL),
                    selectInput(
                      "link_quality",
                      "Link Quality:",
                      choices = c(
                        "1 - Documented in literature" = "1",
                        "2 - Similar species/region" = "2",
                        "3 - Inferred from traits" = "3",
                        "4 - Expert opinion" = "4"
                      ),
                      selected = "1"
                    ),
                    textInput("link_source", "Source (citation/DOI):", placeholder = "doi:10.1111/example"),
                    textAreaInput("link_notes", "Notes:", height = "60px", placeholder = "Optional notes..."),
                    actionButton("add_link_btn", "Add Link", class = "btn-success", icon = icon("link")),
                    hr(),
                    h5("Remove Trophic Link"),
                    selectInput("remove_link_predator", "Predator:", choices = NULL),
                    selectInput("remove_link_prey", "Prey:", choices = NULL),
                    actionButton("remove_link_btn", "Remove Link", class = "btn-danger", icon = icon("unlink"))
                  )
                )
              ),

              fluidRow(
                box(
                  title = "Edit Status",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  verbatimTextOutput("metaweb_edit_status")
                )
              )
            ),

            # ==================================================================
            # PANEL 4: LINK QUALITY
            # ==================================================================
            tabPanel(
              title = "Link Quality",
              icon = icon("certificate"),

              fluidRow(
                column(
                  width = 6,
                  box(
                    title = "Link Quality Distribution",
                    status = "warning",
                    solidHeader = TRUE,
                    width = NULL,
                    plotOutput("link_quality_plot", height = "400px")
                  ),
                  box(
                    title = "Quality Summary Table",
                    status = "info",
                    solidHeader = TRUE,
                    width = NULL,
                    DTOutput("link_quality_table")
                  )
                ),
                column(
                  width = 6,
                  box(
                    title = "Link Quality Guidelines",
                    status = "info",
                    solidHeader = TRUE,
                    width = NULL,
                    HTML("
                      <h5>MARBEFES Quality Code Definitions:</h5>
                      <div class='alert alert-info' style='margin-top: 10px;'>
                        <ul style='margin-bottom: 0;'>
                          <li><strong>1: Documented</strong> - Peer-reviewed literature for these exact species
                            <br><small>Example: Gut content analysis published for this predator-prey pair</small>
                          </li>
                          <li><strong>2: Similar species</strong> - Documented for similar species or different region
                            <br><small>Example: Same predator, similar prey species documented elsewhere</small>
                          </li>
                          <li><strong>3: Inferred</strong> - Inferred from traits or body size relationships
                            <br><small>Example: Predator and prey body sizes suggest potential link</small>
                          </li>
                          <li><strong>4: Expert opinion</strong> - Expert judgment, not yet validated
                            <br><small>Example: Regional expert suggests link based on experience</small>
                          </li>
                        </ul>
                      </div>
                    ")
                  ),
                  box(
                    title = "Filter by Quality",
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    checkboxGroupInput(
                      "quality_filter",
                      "Show links with quality:",
                      choices = c("1 - Documented" = "1",
                                  "2 - Similar species" = "2",
                                  "3 - Inferred" = "3",
                                  "4 - Expert opinion" = "4"),
                      selected = c("1", "2", "3", "4")
                    ),
                    DTOutput("filtered_links_table")
                  )
                )
              )
            ),

            # ==================================================================
            # PANEL 5: EXPORT
            # ==================================================================
            tabPanel(
              title = "Export",
              icon = icon("download"),

              fluidRow(
                column(
                  width = 6,
                  box(
                    title = "Export Metaweb",
                    status = "success",
                    solidHeader = TRUE,
                    width = NULL,
                    HTML("<p>Download the current metaweb for sharing or backup:</p>"),
                    radioButtons(
                      "export_format",
                      "Export Format:",
                      choices = c(
                        "CSV (2 files: species + interactions)" = "csv",
                        "RDS (R object file)" = "rds"
                      ),
                      selected = "csv"
                    ),
                    textInput("export_filename", "Filename prefix:", value = "metaweb"),
                    downloadButton("download_metaweb", "Download Metaweb", class = "btn-success btn-block"),
                    hr(),
                    HTML("<p><small><strong>CSV format:</strong> Creates two files (species and interactions) compatible with Excel and other tools.</small></p>"),
                    HTML("<p><small><strong>RDS format:</strong> Single R object file preserving all metadata, ideal for R users.</small></p>")
                  )
                ),
                column(
                  width = 6,
                  box(
                    title = "Use Metaweb for Analysis",
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    HTML("<p>Convert the current metaweb to an active network for analysis in other tabs:</p>"),
                    actionButton(
                      "export_to_network_btn",
                      "Use as Active Network",
                      class = "btn-primary btn-block",
                      icon = icon("arrow-right")
                    ),
                    hr(),
                    HTML("<p><small>This will convert the metaweb to an igraph network object and make it available for topological metrics, biomass analysis, energy fluxes, and keystoneness analysis.</small></p>"),
                    verbatimTextOutput("export_status")
                  )
                )
              )
            )
          )
        )
      )

      # ========================================================================
}
