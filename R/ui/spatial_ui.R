#' Spatial Analysis Tab UI
#'
#' Creates the spatial analysis tab (Phase 1).
#'
#' @return A tabItem for spatial analysis
spatial_ui <- function() {
      # SPATIAL ANALYSIS TAB (MARBEFES WP3.2 Phase 1)
      # ========================================================================
      tabItem(
        tabName = "spatial_analysis",

        fluidRow(
          box(
            title = "Phase 1: Spatial Analysis (BBT Support)",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            HTML("
              <p><i class='fas fa-map'></i> <strong>Spatial Food Web Analysis</strong></p>
              <p>Create hexagonal grids, assign species to spatial units, extract local networks, and calculate spatial metrics following MARBEFES WP3.2 guidelines.</p>
            ")
          )
        ),

        fluidRow(
          tabBox(
            width = 12,
            id = "spatial_tabs",

            # TAB 0: Study Area (NEW)
            tabPanel(
              title = "0. Study Area (Optional)",
              icon = icon("map-marked-alt"),

              fluidRow(
                box(
                  title = "Define Study Area Boundary",
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,

                  helpText("Upload a shapefile/GeoPackage or select a BBT polygon to define your study area. The hexagonal grid will be clipped to this boundary."),

                  fluidRow(
                    column(6,
                      tags$div(
                        style = "margin-bottom: 15px;",
                        tags$label(
                          `for` = "spatial_bbt_selector",
                          "Select BBT Polygon ",
                          tags$i(
                            class = "fa fa-info-circle",
                            style = "color: #FF9800; cursor: help;",
                            title = "BBT Polygons: Select a pre-defined BBT (Broad Belt Transect) area or upload your own custom boundary below."
                          )
                        ),
                        selectInput("spatial_bbt_selector",
                                   label = NULL,
                                   choices = c(
                                     "None (upload custom boundary)" = "",
                                     "Archipelago" = "Archipelago",
                                     "Balearic" = "Balearic",
                                     "Bay of Gdansk" = "Bay_of_Gdansk",
                                     "Bay of Biscay" = "BayOfBiscay",
                                     "Heraklion" = "Heraklion",
                                     "Hornsund" = "Hornsund",
                                     "Irish Sea" = "Irish_sea",
                                     "Kongsfjord" = "Kongsfiord",
                                     "Lithuanian" = "Lithuanian",
                                     "North Sea" = "North_Sea",
                                     "Porsangerfjord" = "Porsangerfjord",
                                     "Sardinia" = "Sardinia"
                                   ),
                                   selected = "")
                      ),

                      hr(),

                      tags$div(
                        style = "margin-bottom: 15px;",
                        tags$label(
                          `for` = "spatial_study_area",
                          "Upload Custom Study Area ",
                          tags$i(
                            class = "fa fa-info-circle",
                            style = "color: #2196F3; cursor: help;",
                            title = "File Formats: Shapefile - Select ALL files (.shp, .shx, .dbf, .prj) at once. GeoPackage - Select single .gpkg file. Hold Ctrl (Windows) or Cmd (Mac) to select multiple files."
                          )
                        ),
                        fileInput("spatial_study_area",
                                 label = NULL,
                                 multiple = TRUE,
                                 accept = c(".shp", ".shx", ".dbf", ".prj", ".cpg", ".gpkg"))
                      ),

                      verbatimTextOutput("spatial_study_area_info"),

                      actionButton("spatial_clear_study_area", "Clear Study Area",
                                  icon = icon("times"), class = "btn-warning")
                    ),

                    column(6,
                      box(
                        title = "Study Area Preview",
                        width = 12,
                        status = "primary",
                        solidHeader = TRUE,
                        leafletOutput("spatial_study_area_map", height = "400px")
                      )
                    )
                  ),

                  hr(),
                  helpText(HTML("
                    <strong>Note:</strong> If no study area is provided, the grid will cover the entire bounding box specified in Step 1.
                  "))
                )
              )
            ),

            # TAB 1: Grid Setup
            tabPanel(
              title = "1. Grid Setup",
              icon = icon("th"),

              fluidRow(
                box(
                  title = "Configure Hexagonal Grid",
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,

                  helpText("Configure your spatial grid (1-3km hexagons recommended for BBT)"),

                  fluidRow(
                    column(3,
                      numericInput("spatial_xmin", "X Min (longitude):", value = 15, step = 0.1)
                    ),
                    column(3,
                      numericInput("spatial_ymin", "Y Min (latitude):", value = 54, step = 0.1)
                    ),
                    column(3,
                      numericInput("spatial_xmax", "X Max (longitude):", value = 22, step = 0.1)
                    ),
                    column(3,
                      numericInput("spatial_ymax", "Y Max (latitude):", value = 60, step = 0.1)
                    )
                  ),

                  sliderInput("spatial_cell_size", "Cell Size (meters):",
                              min = 1000, max = 5000, value = 2000, step = 100),

                  actionButton("spatial_create_grid", "Create Grid",
                              icon = icon("th"), class = "btn-primary btn-lg"),

                  hr(),
                  verbatimTextOutput("spatial_grid_info")
                )
              )
            ),

            # TAB 2: Habitat Data (EMODnet)
            tabPanel(
              title = "2. Habitat Data",
              icon = icon("layer-group"),

              fluidRow(
                box(
                  title = "EMODnet Habitat Integration",
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,

                  helpText(HTML("
                    <p>Integrate EMODnet EUSeaMap habitat data with your spatial grid.</p>
                    <p><strong>What this does:</strong></p>
                    <ul>
                      <li>Automatically loads regional habitat data (bbox-filtered for your area)</li>
                      <li>Clips habitat map to your study area boundary</li>
                      <li>Overlays habitat polygons with hexagonal grid cells</li>
                      <li>Calculates habitat statistics for each grid cell</li>
                      <li>Adds EUNIS codes, substrate types, and diversity metrics</li>
                    </ul>
                    <p><em>Note: Habitat loads a small centered area for reliability (avoids data errors).
Click 'Clip Habitat to Study Area' button below to get full coverage.</em></p>
                  ")),

                  fluidRow(
                    column(6,
                      checkboxInput(
                        "spatial_enable_habitat",
                        HTML("<strong>Enable Habitat Analysis</strong>"),
                        value = FALSE
                      ),

                      conditionalPanel(
                        condition = "input.spatial_enable_habitat == true",

                        helpText(HTML("<em>Note: Loads small test area initially. Click 'Clip Habitat' for full coverage.</em>")),

                        actionButton("spatial_clip_habitat",
                                    "1. Clip Habitat to Study Area",
                                    icon = icon("cut"),
                                    class = "btn-primary",
                                    style = "width: 100%; margin-bottom: 10px;"),

                        actionButton("spatial_overlay_habitat",
                                    "2. Overlay with Grid Cells",
                                    icon = icon("th-large"),
                                    class = "btn-success",
                                    style = "width: 100%; margin-bottom: 10px;"),

                        hr(),

                        helpText(HTML("
                          <strong>Grid Cell Attributes Added:</strong>
                          <ul style='font-size: 12px; margin: 5px 0;'>
                            <li><strong>dominant_eunis:</strong> Most common EUNIS code</li>
                            <li><strong>dominant_habitat:</strong> Habitat description</li>
                            <li><strong>dominant_substrate:</strong> Main substrate type</li>
                            <li><strong>habitat_diversity:</strong> Shannon diversity of EUNIS codes</li>
                            <li><strong>n_habitats:</strong> Number of different habitats</li>
                            <li><strong>habitat_area_km2:</strong> Total habitat area in cell</li>
                            <li><strong>substrate_*_pct:</strong> Percentage of each substrate</li>
                          </ul>
                        "))
                      )
                    ),

                    column(6,
                      box(
                        title = "Habitat Processing Status",
                        width = 12,
                        status = "success",
                        solidHeader = TRUE,
                        verbatimTextOutput("spatial_habitat_status")
                      )
                    )
                  )
                )
              )
            ),

            # TAB 3: Species Data
            tabPanel(
              title = "3. Species Data",
              icon = icon("fish"),

              fluidRow(
                box(
                  title = "Upload or Generate Species Data",
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,

                  helpText("Upload or generate species occurrence data (CSV with lon, lat, species columns)"),

                  fileInput("spatial_species_file", "Upload Species Data (CSV):",
                           accept = c(".csv")),

                  actionButton("spatial_generate_sample", "Generate Sample Data",
                              icon = icon("random"), class = "btn-secondary"),

                  hr(),
                  DTOutput("spatial_species_preview")
                )
              )
            ),

            # TAB 4: Network Extraction
            tabPanel(
              title = "4. Network Extraction",
              icon = icon("project-diagram"),

              fluidRow(
                box(
                  title = "Extract Local Networks from Metaweb",
                  width = 12,
                  status = "warning",
                  solidHeader = TRUE,

                  helpText("Select metaweb and extract local networks for each hexagon"),

                  selectInput("spatial_metaweb", "Select Regional Metaweb:",
                             choices = c(
                               "Baltic: Kortsch 2021" = "baltic_kortsch2021",
                               "Arctic: Kongsfjorden" = "kongsfjorden_farage2021",
                               "Atlantic: North Sea" = "north_sea_frelat2022",
                               "Arctic: Barents - Arctic" = "barents_arctic_kortsch2015",
                               "Arctic: Barents - Boreal" = "barents_boreal_kortsch2015"
                             )),

                  actionButton("spatial_extract_networks", "Extract Local Networks",
                              icon = icon("project-diagram"), class = "btn-warning btn-lg"),

                  hr(),
                  verbatimTextOutput("spatial_extraction_info")
                )
              )
            ),

            # TAB 5: Spatial Metrics
            tabPanel(
              title = "5. Spatial Metrics",
              icon = icon("calculator"),

              fluidRow(
                box(
                  title = "Calculate Food Web Metrics",
                  width = 12,
                  status = "success",
                  solidHeader = TRUE,

                  helpText("Calculate food web metrics for each hexagon"),

                  checkboxGroupInput("spatial_metrics", "Select Metrics:",
                                    choices = c(
                                      "Species Richness (S)" = "S",
                                      "Number of Links (L)" = "L",
                                      "Connectance (C)" = "C",
                                      "Link Density (LD)" = "LD",
                                      "Mean Trophic Level" = "meanTL",
                                      "Max Trophic Level" = "maxTL"
                                    ),
                                    selected = c("S", "L", "C", "LD")),

                  actionButton("spatial_calculate_metrics", "Calculate Metrics",
                              icon = icon("calculator"), class = "btn-success btn-lg"),

                  hr(),
                  DTOutput("spatial_metrics_table")
                )
              )
            ),

            # TAB 6: Visualization & Export
            tabPanel(
              title = "6. Visualization & Export",
              icon = icon("map-marked-alt"),

              fluidRow(
                box(
                  title = "Interactive Map",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,

                  helpText("Visualize spatial metrics on interactive map (requires calculated metrics)"),

                  selectInput("spatial_map_metric", "Color by Metric:",
                             choices = list(
                               "Food Web Metrics" = c(
                                 "Species Richness (S)" = "S",
                                 "Number of Links (L)" = "L",
                                 "Connectance (C)" = "C",
                                 "Link Density (LD)" = "LD",
                                 "Mean Trophic Level" = "meanTL",
                                 "Max Trophic Level" = "maxTL"
                               ),
                               "Habitat Attributes" = c(
                                 "Dominant EUNIS Code" = "dominant_eunis",
                                 "Dominant Substrate" = "dominant_substrate",
                                 "Habitat Diversity (Shannon)" = "habitat_diversity",
                                 "Number of Habitats" = "n_habitats",
                                 "Habitat Area (km²)" = "habitat_area_km2"
                               )
                             )),

                  hr(),
                  leafletOutput("spatial_map", height = "600px"),
                  hr(),
                  helpText("Map Layers: Study Area (blue), Grid (gray), Species (green), Metrics (color)")
                )
              ),

              fluidRow(
                box(
                  title = "Export Results",
                  width = 12,
                  status = "secondary",
                  solidHeader = TRUE,

                  downloadButton("spatial_download_metrics", "Download Metrics (CSV)", class = "btn-secondary"),
                  downloadButton("spatial_download_rds", "Download Full Analysis (RDS)", class = "btn-secondary"),

                  hr(),
                  helpText("Download spatial metrics table or complete spatial_foodweb_data object for further analysis in R.")
                )
              )
            )
          )
        )
      )   # End of spatial_analysis tabItem (last one - no comma!)
}
