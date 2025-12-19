# app.R
# EcoNeTool - Ecological Interaction Network Explorer
# Shiny app for marine food web network analysis
library(shiny)
library(bs4Dash)
library(shinyBS)  # For modals
library(shinyWidgets)  # For switchInput
library(igraph)
library(fluxweb)
library(visNetwork)
library(DT)
library(MASS)  # Required for pseudo-inverse in MTI calculations
library(leaflet)  # Interactive mapping for spatial analysis

# ============================================================================
# SOURCE CONFIGURATION AND FUNCTIONS
# ============================================================================

# Configuration constants (COLOR_SCHEME, METAWEB_PATHS, etc.)
# MUST be loaded first before any constants are used
source("R/config.R")

# Set Shiny upload size limit using config constant
options(shiny.maxRequestSize = MAX_UPLOAD_SIZE_MB*1024^2)
source("R/config/plugins.R")  # Plugin system configuration

# Analysis functions (organized by domain)
source("R/functions/functional_group_utils.R")  # Shared utilities
source("R/functions/trophic_levels.R")
source("R/functions/network_visualization.R")
source("R/functions/topological_metrics.R")
source("R/functions/flux_calculations.R")
source("R/functions/keystoneness.R")
source("R/functions/metaweb_core.R")
source("R/functions/metaweb_io.R")
source("R/functions/spatial_analysis.R")
source("R/functions/ecobase_connection.R")
source("R/functions/taxonomic_api_utils.R")  # Taxonomic database integration
source("R/functions/emodnet_habitat_utils.R")  # EMODnet habitat integration
source("R/functions/euseamap_regional_config.R")  # Regional optimization for EUSeaMap

# ECOPATH import (cross-platform compatible)
source("R/functions/ecopath_import.R")

# Rpath integration (ECOPATH/ECOSIM module)
source("R/functions/rpath_integration.R")
source("R/functions/auxillary_parser.R")  # Auxillary data parser for comments/tooltips
source("R/ui/rpath_ui.R")  # Rpath module UI component
source("R/modules/rpath_server.R")  # Rpath module server logic

# UI components (organized by tab)
source("R/ui/dashboard_ui.R")
source("R/ui/import_ui.R")
source("R/ui/network_ui.R")
source("R/ui/topological_ui.R")
source("R/ui/biomass_ui.R")
source("R/ui/fluxes_ui.R")
source("R/ui/keystoneness_ui.R")
source("R/ui/dataeditor_ui.R")
source("R/ui/metaweb_ui.R")
source("R/ui/spatial_ui.R")
source("R/ui/ecobase_ui.R")

# Data loading and validation (loads net and info objects)
source("R/data_loading.R")

# ============================================================================
# UI - BS4DASH DASHBOARD
# ============================================================================

ui <- dashboardPage(
  # ============================================================================
  # HEADER
  # ============================================================================
  header = dashboardHeader(
    title = dashboardBrand(
      title = "EcoNeTool",
      color = "primary",
      href = "https://github.com",
      image = "img/marbefes.png"
    ),
    skin = "light",
    status = "white",
    border = TRUE,
    sidebarIcon = icon("bars"),
    fixed = FALSE,
    leftUI = tagList(
        h4("Ecological Interaction Network Explorer", style = "margin: 10px; color: #007bff;")
    ),
    rightUI = tagList(
      # Settings Button
      tags$div(
        style = "margin: 10px 15px; display: inline-block;",
        actionButton(
          "show_plugin_settings",
          label = NULL,
          icon = icon("cog", class = "fa-lg"),
          style = "background: transparent; border: none; color: #6c757d; padding: 5px 10px; transition: color 0.2s;",
          onmouseover = "this.style.color='#007bff'",
          onmouseout = "this.style.color='#6c757d'",
          title = "Settings"
        )
      )
    )
  ),

  # ============================================================================
  # SIDEBAR
  # ============================================================================
  sidebar = dashboardSidebar(
    skin = "light",
    status = "primary",
    elevation = 3,
    sidebarMenu(
      id = "sidebar_menu",

      menuItem(
        text = "Dashboard",
        tabName = "dashboard",
        icon = icon("home")
      ),

      menuItem(
        text = "Data Import",
        tabName = "import",
        icon = icon("upload")
      ),

      menuItem(
        text = "Food Web Network",
        tabName = "network",
        icon = icon("project-diagram")
      ),

      menuItem(
        text = "Topological Metrics",
        tabName = "topological",
        icon = icon("chart-line")
      ),

      menuItem(
        text = "Biomass Analysis",
        tabName = "biomass",
        icon = icon("weight")
      ),

      menuItem(
        text = "Energy Fluxes",
        tabName = "fluxes",
        icon = icon("bolt")
      ),

      menuItem(
        text = "Keystoneness Analysis",
        tabName = "keystoneness",
        icon = icon("key")
      ),

      menuItem(
        text = "Internal Data Editor",
        tabName = "dataeditor",
        icon = icon("table")
      ),

      menuItem(
        text = "Metaweb Manager",
        tabName = "metaweb_manager",
        icon = icon("sitemap")
      ),

      menuItem(
        text = "Spatial Analysis",
        tabName = "spatial_analysis",
        icon = icon("map")
      ),

      menuItem(
        text = "EcoBase Connection",
        tabName = "ecobase",
        icon = icon("cloud")
      ),

      menuItem(
        text = "ECOPATH/ECOSIM",
        tabName = "rpath",
        icon = icon("fish")
      )
    )
  ),

  # ============================================================================
  # BODY
  # ============================================================================
  body = dashboardBody(
    # Custom CSS for styling
    tags$head(
      tags$style(HTML("
        /* Style the controlbar info icon to match primary theme color */
        .main-header .nav-link[data-widget='control-sidebar'] {
          color: #007bff !important;
        }
        .main-header .nav-link[data-widget='control-sidebar']:hover {
          color: #0056b3 !important;
        }
      "))
    ),

    tabItems(

      # Call UI functions to create tabs
      dashboard_ui(),
      import_ui(),
      network_ui(),
      topological_ui(),
      biomass_ui(),
      fluxes_ui(),
      keystoneness_ui(),
      dataeditor_ui(),
      metaweb_ui(),
      spatial_ui(),
      ecobase_ui(),

      # Rpath module tab
      tabItem(
        tabName = "rpath",
        rpathModuleUI("rpath_module")
      )

    ),    # End of tabItems()

    # ==========================================================================
    # MODALS
    # ==========================================================================

    # Settings Modal (combines plugins, about, and links)
    bsModal(
      id = "plugin_settings_modal",
      title = tagList(icon("cog"), " Settings"),
      trigger = "show_plugin_settings",
      size = "large",

      tabsetPanel(
        id = "settings_tabs",
        type = "tabs",

        # Plugin Management Tab
        tabPanel(
          title = tagList(icon("puzzle-piece"), " Plugins"),
          value = "plugins_tab",
          br(),
          h4("Enable or Disable Modules"),
          p("Control which analysis modules are available in the sidebar. Core modules cannot be disabled."),
          hr(),
          uiOutput("plugin_settings_ui")
        ),

        # About Tab
        tabPanel(
          title = tagList(icon("info-circle"), " About"),
          value = "about_tab",
          br(),
          h3("EcoNeTool"),
          h5("Ecological Interaction Network Explorer"),
          p("Version 1.0.0"),
          hr(),

          h4("Description"),
          p("EcoNeTool is an interactive Shiny application for analyzing marine food web networks. It provides tools for network visualization, topological analysis, biomass distribution, energy flux calculations, and keystoneness analysis."),

          h4("Features"),
          tags$ul(
            tags$li("Interactive network visualization"),
            tags$li("Topological metrics calculation"),
            tags$li("Biomass distribution analysis"),
            tags$li("Energy flux modeling"),
            tags$li("Keystoneness analysis"),
            tags$li("Metaweb library management"),
            tags$li("Spatial analysis capabilities"),
            tags$li("EcoBase database integration"),
            tags$li("ECOPATH with Ecosim import support")
          ),

          h4("Developed with"),
          p("Built with Shiny, bs4Dash, igraph, fluxweb, and visNetwork."),

          h4("License"),
          p("Open source software"),

          hr(),
          div(
            style = "text-align: center;",
            tags$a(
              href = "https://github.com/anthropics/claude-code",
              target = "_blank",
              class = "btn btn-primary",
              icon("github"), " View on GitHub"
            )
          ),
          br(),
          p(tags$small("Generated with Claude Code"), style = "text-align: center;")
        )
      )
    )

  ),      # End of body = dashboardBody(...)

  # ============================================================================
  # CONTROLBAR (optional - for additional info/settings)
  # ============================================================================
  controlbar = dashboardControlbar(
    skin = "light",
    pinned = FALSE,
    overlay = TRUE,
    controlbarMenu(
      id = "controlbar_menu",
      controlbarItem(
        title = "Information",
        HTML("
          <div style='padding: 15px;'>
            <h5>EcoNeTool</h5>
            <p><strong>Version:</strong> 1.0.0 - Refactoring Release</p>
            <p><strong>License:</strong> GPL-3.0</p>

            <h5>About</h5>
            <p>Generic food web analysis tool supporting custom data import in multiple formats (Excel, CSV, RData).</p>

            <h5>Default Dataset</h5>
            <p><strong>Gulf of Riga Food Web</strong><br>
            Frelat, R., & Kortsch, S. (2020).<br>
            34 species, 207 links<br>
            Period: 1979-2016</p>

            <h5>Data Import</h5>
            <p>Upload your own food web data using the <strong>Data Import</strong> tab. Supported formats:</p>
            <ul style='font-size: 12px; margin-left: -15px;'>
              <li>Excel (.xlsx, .xls)</li>
              <li>CSV files</li>
              <li>RData (.Rdata, .rda)</li>
            </ul>

            <h5>Color Scheme</h5>
            <p><strong>Default Functional Groups:</strong><br>
            <span style='color: orange;'>●</span> Benthos<br>
            <span style='color: darkgrey;'>●</span> Detritus<br>
            <span style='color: blue;'>●</span> Fish<br>
            <span style='color: green;'>●</span> Phytoplankton<br>
            <span style='color: cyan;'>●</span> Zooplankton</p>

            <h5>References</h5>
            <p style='font-size: 11px;'>
            Williams & Martinez (2004). Limits to trophic levels. Proc. R. Soc. B.<br><br>
            Olivier et al. (2019). Temporal variability. Ecography.<br><br>
            Brown et al. (2004). Metabolic theory of ecology. Ecology.
            </p>
          </div>
        ")
      )
    )
  ),

  # Dashboard footer
  footer = dashboardFooter(
    left = tagList(
      "EcoNeTool - Ecological Interaction Network Explorer | ",
      tags$a(href = "https://github.com/razinkele/EcoNeTool", icon("github"), " GitHub", target = "_blank")
    ),
    right = "Powered by bs4Dash & Shiny"
  ),

  # Dashboard options
  title = "EcoNeTool - Ecological Interaction Network Explorer",
  skin = "light",
  freshTheme = NULL,
  help = NULL,
  dark = NULL,
  scrollToTop = TRUE
)

# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {
  # ============================================================================
  # GLOBAL REACTIVE VALUES (Shared across phases)
  # ============================================================================

  # Make net and info reactive so dashboard updates
  # Ensure vertex names are properly set before initializing reactive
  if (exists("net") && igraph::is_igraph(net)) {
    if (is.null(igraph::V(net)$name) || all(grepl("^[0-9]+$", igraph::V(net)$name))) {
      if (exists("info") && "species" %in% colnames(info)) {
        igraph::V(net)$name <- as.character(info$species)
      } else if (exists("info") && !is.null(rownames(info))) {
        igraph::V(net)$name <- rownames(info)
      }
    }
  }

  net_reactive <- reactiveVal(net)
  info_reactive <- reactiveVal({
    # Relevel functional groups to match COLOR_SCHEME order if needed
    fg_levels <- get_functional_group_levels()
    if (is.factor(info$fg)) {
      # Check if current levels match expected levels
      current_levels <- levels(info$fg)
      if (!all(current_levels %in% fg_levels)) {
        # Keep only existing levels but in standard order
        existing_in_order <- fg_levels[fg_levels %in% current_levels]
        info$fg <- factor(as.character(info$fg), levels = existing_in_order)
      } else {
        info$fg <- factor(as.character(info$fg), levels = fg_levels)
      }
    }
    info$colfg <- COLOR_SCHEME[as.numeric(info$fg)]
    info
  })

  # Metaweb Manager (Phase 2) - used by Spatial Analysis (Phase 1)
  # Note: METAWEB_PATHS is now defined in R/config.R and validated at startup
  current_metaweb <- reactiveVal(NULL)

  # Metadata for dashboard boxes
  metaweb_metadata <- reactiveVal(list(
    location = "Gulf of Riga, Baltic Sea",
    time_period = "1979-2016",
    source = "Frelat & Kortsch, 2020"
  ))

  # Trigger for dashboard updates
  dashboard_trigger <- reactiveVal(0)

  # ============================================================================
  # PLUGIN MANAGEMENT
  # ============================================================================

  # Store plugin states (plugin_id => enabled TRUE/FALSE)
  plugin_states <- reactiveVal(get_default_plugin_states())

  # Render plugin settings UI
  output$plugin_settings_ui <- renderUI({
    # Add reactive dependency so UI updates when plugin states change
    current_states <- plugin_states()

    all_plugins <- get_all_plugins()
    categories <- c("core", "analysis", "data", "advanced")

    tagList(
      lapply(categories, function(cat) {
        plugins_in_cat <- get_plugins_by_category(cat)

        if (length(plugins_in_cat) == 0) return(NULL)

        tagList(
          h4(toupper(cat), " MODULES",
             style = paste0("margin-top: 20px; padding: 10px; background: ",
                           if (cat == "core") "#e3f2fd" else if (cat == "analysis") "#fff3e0"
                           else if (cat == "data") "#e8f5e9" else "#f3e5f5",
                           ";")),

          lapply(names(plugins_in_cat), function(plugin_id) {
            plugin <- plugins_in_cat[[plugin_id]]

            # Check if packages are available
            packages_ok <- check_plugin_packages(plugin_id)
            can_enable <- packages_ok

            tagList(
              div(
                style = paste0("padding: 15px; margin: 10px 0; border: 1px solid #ddd; ",
                              "border-radius: 5px; background: white;"),

                fluidRow(
                  column(1,
                    icon(plugin$icon, style = "font-size: 24px; color: #007bff;")
                  ),
                  column(8,
                    h5(plugin$name, style = "margin-top: 0;"),
                    p(plugin$description, style = "margin: 5px 0; color: #666; font-size: 13px;"),

                    # Show package requirements if any
                    if (!is.null(plugin$packages)) {
                      tagList(
                        p(
                          tags$small(
                            tags$strong("Requires packages: "),
                            paste(plugin$packages, collapse = ", "),
                            if (!packages_ok) {
                              tags$span(" (NOT INSTALLED)", style = "color: red; font-weight: bold;")
                            } else {
                              tags$span(" (installed)", style = "color: green;")
                            }
                          ),
                          style = "margin: 5px 0;"
                        )
                      )
                    }
                  ),
                  column(3,
                    if (plugin$required) {
                      tags$span(
                        icon("lock"), " REQUIRED",
                        style = "color: #999; font-size: 12px;"
                      )
                    } else {
                      switchInput(
                        inputId = paste0("plugin_", plugin_id),
                        label = NULL,
                        value = current_states[[plugin_id]] %||% FALSE,
                        onLabel = "ON",
                        offLabel = "OFF",
                        onStatus = "success",
                        offStatus = "danger",
                        size = "normal",
                        disabled = !can_enable
                      )
                    }
                  )
                )
              )
            )
          })
        )
      }),

      hr(),
      div(
        style = "text-align: right;",
        actionButton("save_plugin_settings",
                    "Save & Apply",
                    icon = icon("save"),
                    class = "btn-primary"),
        actionButton("reset_plugin_settings",
                    "Reset to Defaults",
                    icon = icon("undo"),
                    class = "btn-secondary")
      )
    )
  })

  # Ensure plugin settings UI is not suspended when hidden
  outputOptions(output, "plugin_settings_ui", suspendWhenHidden = FALSE)

  # Save plugin settings
  observeEvent(input$save_plugin_settings, {
    all_plugins <- get_all_plugins()
    new_states <- list()

    for (plugin_id in names(all_plugins)) {
      plugin <- all_plugins[[plugin_id]]

      if (plugin$required) {
        # Required plugins are always enabled
        new_states[[plugin_id]] <- TRUE
      } else {
        # Get state from switch input
        input_id <- paste0("plugin_", plugin_id)
        new_states[[plugin_id]] <- input[[input_id]] %||% FALSE
      }
    }

    plugin_states(new_states)

    showNotification(
      "✓ Plugin settings saved and applied!",
      type = "message",
      duration = 3
    )
  })

  # Reset to defaults
  observeEvent(input$reset_plugin_settings, {
    plugin_states(get_default_plugin_states())

    showNotification(
      "✓ Plugin settings reset to defaults and applied!",
      type = "message",
      duration = 3
    )
  })

  # ============================================================================
  # RPATH MODULE (ECOPATH/ECOSIM Integration)
  # ============================================================================

  # Reactive value to store ECOPATH import data for Rpath module
  ecopath_import_data <- reactiveVal(NULL)

  # Call Rpath module server
  rpath_results <- rpathModuleServer(
    "rpath_module",
    ecopath_import_reactive = ecopath_import_data
  )

  # ============================================================================
  # DASHBOARD - Dynamic Value Boxes
  # ============================================================================

  output$box_species <- renderValueBox({
    # Trigger update
    dashboard_trigger()

    current_net <- net_reactive()
    valueBox(
      value = vcount(current_net),
      subtitle = "Taxa / Species",
      icon = icon("fish"),
      color = "primary"
    )
  })

  output$box_links <- renderValueBox({
    # Trigger update
    dashboard_trigger()

    current_net <- net_reactive()
    valueBox(
      value = ecount(current_net),
      subtitle = "Trophic Links",
      icon = icon("link"),
      color = "success"
    )
  })

  output$box_groups <- renderValueBox({
    # Trigger update
    dashboard_trigger()

    current_info <- info_reactive()
    valueBox(
      value = nlevels(current_info$fg),
      subtitle = "Functional Groups",
      icon = icon("layer-group"),
      color = "info"
    )
  })

  output$box_period <- renderValueBox({
    # Trigger update
    dashboard_trigger()

    metadata <- metaweb_metadata()
    valueBox(
      value = metadata$time_period,
      subtitle = "Time Period",
      icon = icon("calendar"),
      color = "warning"
    )
  })

  output$box_location <- renderValueBox({
    # Trigger update
    dashboard_trigger()

    metadata <- metaweb_metadata()
    valueBox(
      value = metadata$location,
      subtitle = "Location",
      icon = icon("map-marker-alt"),
      color = "danger"
    )
  })

  # ============================================================================
  # ECOPATH PARSER FUNCTION
  # ============================================================================

  # NOTE: parse_ecopath_data() function is now defined in R/functions/ecopath_import.R
  # It is automatically available through the source() call at the top of this file

  #' Parse ECOPATH native database file (.ewemdb, .mdb)
  #'
  #' Reads ECOPATH with Ecosim native database files using mdbtools
  #' @param db_file Path to ECOPATH database file
  #' @return List with 'net' (igraph object) and 'info' (data.frame)
  parse_ecopath_native <- function(db_file) {
    # Check if file exists
    if (!file.exists(db_file)) {
      stop("Database file not found: ", db_file)
    }

    # CROSS-PLATFORM IMPLEMENTATION
    # Detect OS and use appropriate database reader
    os_type <- Sys.info()["sysname"]

    message("Operating System: ", os_type)
    message("Using cross-platform ECOPATH import...")

    tryCatch({
      # Import database using cross-platform function
      import_result <- parse_ecopath_native_cross_platform(db_file)

      # Extract tables from import result
      group_table <- import_result$group_data
      diet_table <- import_result$diet_data
      tables <- import_result$tables
      metadata <- import_result$metadata  # Extract metadata

      # Log success
      message("✓ Database imported successfully using ", import_result$method)

      # Extract species/group information
      # ECOPATH column names may vary, so we try different common names
      col_names <- tolower(colnames(group_table))
      col_names_orig <- colnames(group_table)

      # Find group name column
      name_col <- which(grepl("group.*name|^name$|groupname", col_names))[1]
      if (is.na(name_col)) name_col <- 1  # Default to first column

      # Find biomass column (more flexible patterns)
      biomass_col <- which(grepl("^biomass$|^b$|^biom$|trophic.*area|habitat.*area|^ba$", col_names))[1]

      # Find P/B column (more flexible patterns)
      # ECOPATH uses P/B, PB, ProdBiom, etc.
      pb_col <- which(grepl("^p/b$|^pb$|^p.b$|prod.*biom|production.*biomass|^prodbiom$", col_names))[1]

      # Find Q/B column (more flexible patterns)
      # ECOPATH uses Q/B, QB, ConsBiom, etc.
      qb_col <- which(grepl("^q/b$|^qb$|^q.b$|cons.*biom|consumption.*biomass|^consbiom$", col_names))[1]

      # Find body mass column (ECOPATH specific)
      # ECOPATH may have: IndividualWeight, BodyMass, IndWt, Weight, Mass
      bodymass_col <- which(grepl("individual.*weight|indiv.*wt|body.*mass|^weight$|^mass$|^bodymass$|^indwt$", col_names))[1]

      # Log column detection
      message("Group table column detection:")
      message("  Name: ", col_names_orig[name_col], " (column ", name_col, ")")
      message("  Biomass: ", if (!is.na(biomass_col)) paste0(col_names_orig[biomass_col], " (column ", biomass_col, ")") else "NOT FOUND - using defaults")
      message("  P/B: ", if (!is.na(pb_col)) paste0(col_names_orig[pb_col], " (column ", pb_col, ")") else "NOT FOUND - using defaults")
      message("  Q/B: ", if (!is.na(qb_col)) paste0(col_names_orig[qb_col], " (column ", qb_col, ")") else "NOT FOUND - using defaults")
      message("  Body Mass: ", if (!is.na(bodymass_col)) paste0(col_names_orig[bodymass_col], " (column ", bodymass_col, ")") else "NOT FOUND - will estimate by functional group")

      # Extract data
      species_names <- as.character(group_table[[name_col]])
      biomass_values <- if (!is.na(biomass_col)) as.numeric(group_table[[biomass_col]]) else rep(1, length(species_names))
      pb_values <- if (!is.na(pb_col)) as.numeric(group_table[[pb_col]]) else rep(0.5, length(species_names))
      qb_values <- if (!is.na(qb_col)) as.numeric(group_table[[qb_col]]) else rep(1.5, length(species_names))
      bodymass_values_raw <- if (!is.na(bodymass_col)) as.numeric(group_table[[bodymass_col]]) else NULL

      # ECOPATH uses -9999 or -9999.00 as missing value indicator
      # Replace with NA or reasonable defaults
      clean_ecopath_value <- function(x, default_val, param_name = "") {
        n_missing <- sum(is.na(x))
        n_sentinel <- sum(x < -9000, na.rm = TRUE)  # -9999 values
        n_negative <- sum(x < 0 & x >= -9000, na.rm = TRUE)  # Other negative values

        x[is.na(x)] <- default_val
        x[x < -9000] <- default_val  # Catch -9999, -9999.00, etc.
        x[x < 0] <- default_val      # Biomass/PB/QB can't be negative

        # Log if any values were replaced
        total_replaced <- n_missing + n_sentinel + n_negative
        if (total_replaced > 0 && param_name != "") {
          message("  Cleaned ", total_replaced, " ", param_name, " value(s):")
          if (n_sentinel > 0) message("    - ", n_sentinel, " ECOPATH missing value(s) (-9999) → ", default_val)
          if (n_negative > 0) message("    - ", n_negative, " negative value(s) → ", default_val)
          if (n_missing > 0) message("    - ", n_missing, " NA value(s) → ", default_val)
        }

        return(x)
      }

      # Clean biomass values (default to 1 if missing)
      biomass_values <- clean_ecopath_value(biomass_values, 1, "Biomass")

      # Apply habitat area proportion if available
      # ECOPATH biomass is given per unit area, but organisms may use only fraction of total habitat
      # Area column contains habitat area proportion (0-1), representing fraction of habitat used
      area_col <- which(grepl("^area$|habitat.*prop|^habarea$|^habprop$", col_names))[1]
      if (!is.na(area_col)) {
        area_proportions <- as.numeric(group_table[[area_col]])
        area_proportions <- clean_ecopath_value(area_proportions, 1, "")  # Default to 1 (full area)
        # Multiply biomass by area proportion to get actual biomass
        biomass_values <- biomass_values * area_proportions
        message("  Applied habitat area proportions to biomass")
        message("    Range: ", round(min(area_proportions, na.rm = TRUE), 3), " - ", round(max(area_proportions, na.rm = TRUE), 3))
      }

      # Clean P/B values (default to 0.5 if missing)
      pb_values <- clean_ecopath_value(pb_values, 0.5, "P/B")

      # Clean Q/B values (default to 1.5 if missing)
      qb_values <- clean_ecopath_value(qb_values, 1.5, "Q/B")

      # Log data quality summary
      message("\nData quality summary:")
      message("  Species/groups: ", length(species_names))
      message("  Biomass range: ", round(min(biomass_values), 3), " - ", round(max(biomass_values), 3))
      message("  P/B range: ", round(min(pb_values), 3), " - ", round(max(pb_values), 3))
      message("  Q/B range: ", round(min(qb_values), 3), " - ", round(max(qb_values), 3))

      # Remove NA, empty, or "Import" and "Export" groups (common in ECOPATH)
      valid_idx <- !is.na(species_names) & species_names != "" &
                   !grepl("^import$|^export$|^fleet", tolower(species_names))

      species_names <- species_names[valid_idx]
      biomass_values <- biomass_values[valid_idx]
      pb_values <- pb_values[valid_idx]
      qb_values <- qb_values[valid_idx]

      n_species <- length(species_names)

      # Process diet composition
      # ECOPATH diet tables usually have: Predator, Prey, DietComp (proportion)
      diet_cols <- tolower(colnames(diet_table))
      diet_cols_orig <- colnames(diet_table)

      # Try to identify predator column (more flexible patterns)
      pred_col <- which(grepl("predator|consumer|^pred$|eating", diet_cols))[1]

      # Try to identify prey column (more flexible patterns)
      prey_col <- which(grepl("prey|resource|^food$|eaten", diet_cols))[1]

      # Try to identify diet proportion column (more flexible patterns)
      diet_col <- which(grepl("diet|proportion|comp|percent|fraction|dc$", diet_cols))[1]

      # If standard patterns didn't work, try positional heuristics
      # ECOPATH often has: Column1=Predator, Column2=Prey, Column3=Diet
      if (is.na(pred_col) && ncol(diet_table) >= 3) {
        # First column is often predator ID or name
        pred_col <- 1
      }

      if (is.na(prey_col) && ncol(diet_table) >= 3) {
        # Second column is often prey ID or name
        prey_col <- 2
      }

      if (is.na(diet_col) && ncol(diet_table) >= 3) {
        # Third column is often diet proportion
        # Look for numeric column
        for (i in 1:min(5, ncol(diet_table))) {
          if (is.numeric(diet_table[[i]]) && all(diet_table[[i]] >= 0 & diet_table[[i]] <= 1, na.rm = TRUE)) {
            diet_col <- i
            break
          }
        }
        # If still not found, default to third column
        if (is.na(diet_col)) diet_col <- 3
      }

      # Final check
      if (is.na(pred_col) || is.na(prey_col) || is.na(diet_col)) {
        error_msg <- paste0(
          "Could not identify predator, prey, and diet columns in diet table.\n\n",
          "Available columns in diet table:\n",
          paste(seq_along(diet_cols_orig), ": ", diet_cols_orig, collapse = "\n"), "\n\n",
          "Detected columns:\n",
          "  Predator column: ", if (!is.na(pred_col)) diet_cols_orig[pred_col] else "NOT FOUND", "\n",
          "  Prey column: ", if (!is.na(prey_col)) diet_cols_orig[prey_col] else "NOT FOUND", "\n",
          "  Diet column: ", if (!is.na(diet_col)) diet_cols_orig[diet_col] else "NOT FOUND", "\n\n",
          "Please ensure your ECOPATH database has a diet composition table with:\n",
          "  - A predator/consumer column\n",
          "  - A prey/resource column\n",
          "  - A diet proportion column (0-1)"
        )
        stop(error_msg)
      }

      # Log successful column detection
      message("Successfully identified diet table columns:")
      message("  Predator: ", diet_cols_orig[pred_col], " (column ", pred_col, ")")
      message("  Prey: ", diet_cols_orig[prey_col], " (column ", prey_col, ")")
      message("  Diet proportion: ", diet_cols_orig[diet_col], " (column ", diet_col, ")")

      # Create diet matrix
      diet_matrix <- matrix(0, nrow = n_species, ncol = n_species)
      rownames(diet_matrix) <- colnames(diet_matrix) <- species_names

      # Check if diet table uses IDs or names
      # ECOPATH native databases typically use numeric IDs (PredID, PreyID)
      uses_ids <- is.numeric(diet_table[[pred_col]]) && "GroupID" %in% colnames(group_table)

      if (uses_ids) {
        # Match by ID (ECOPATH native format)
        message("Diet table uses IDs - mapping via GroupID")

        # Create ID to index mapping
        group_ids <- group_table$GroupID[valid_idx]
        id_to_idx <- setNames(seq_along(species_names), group_ids)

        # Fill diet matrix using ID mapping (VECTORIZED)
        # Pre-filter valid entries
        pred_ids <- as.numeric(diet_table[[pred_col]])
        prey_ids <- as.numeric(diet_table[[prey_col]])
        diet_props <- as.numeric(diet_table[[diet_col]])

        valid_entries <- !is.na(pred_ids) & !is.na(prey_ids) & !is.na(diet_props) & diet_props > 0

        if (sum(valid_entries) > 0) {
          # Vectorize ID lookups
          pred_indices <- id_to_idx[as.character(pred_ids[valid_entries])]
          prey_indices <- id_to_idx[as.character(prey_ids[valid_entries])]
          valid_pairs <- !is.na(pred_indices) & !is.na(prey_indices)

          if (sum(valid_pairs) > 0) {
            # Single matrix indexing operation
            idx <- cbind(prey_indices[valid_pairs], pred_indices[valid_pairs])
            diet_matrix[idx] <- diet_props[valid_entries][valid_pairs]
            links_added <- sum(valid_pairs)
          } else {
            links_added <- 0
          }
        } else {
          links_added <- 0
        }
        message("  Added ", links_added, " diet links from ", nrow(diet_table), " diet entries")

      } else {
        # Match by name (CSV export format - VECTORIZED)
        message("Diet table uses names - matching directly")

        # Pre-filter and convert
        pred_names <- as.character(diet_table[[pred_col]])
        prey_names <- as.character(diet_table[[prey_col]])
        diet_props <- as.numeric(diet_table[[diet_col]])

        # Vectorize validity checks
        valid_entries <- !is.na(pred_names) & !is.na(prey_names) &
                        !is.na(diet_props) & diet_props > 0 &
                        pred_names %in% species_names & prey_names %in% species_names

        if (sum(valid_entries) > 0) {
          # Vectorize name matching
          name_to_idx <- setNames(seq_along(species_names), species_names)
          pred_indices <- name_to_idx[pred_names[valid_entries]]
          prey_indices <- name_to_idx[prey_names[valid_entries]]

          # Single matrix indexing operation
          idx <- cbind(prey_indices, pred_indices)
          diet_matrix[idx] <- diet_props[valid_entries]
          links_added <- sum(valid_entries)
        } else {
          links_added <- 0
        }
        message("  Added ", links_added, " diet links")
      }

      # Convert to binary adjacency matrix
      # diet_matrix[prey, predator] = diet proportion
      # This is already the correct format for trophiclevels() function
      # which expects adjacency[prey, predator] = 1 (edges from prey to predator)
      adjacency_matrix <- (diet_matrix > 0) * 1

      # Ensure rownames and colnames are preserved
      rownames(adjacency_matrix) <- rownames(diet_matrix)
      colnames(adjacency_matrix) <- colnames(diet_matrix)

      # Create network
      net <- igraph::graph_from_adjacency_matrix(adjacency_matrix, mode = "directed")
      net <- igraph::upgrade_graph(net)

      # Explicitly set vertex names to ensure they're preserved
      igraph::V(net)$name <- species_names

      # Assign functional groups using shared utility with topology heuristics
      indegrees <- igraph::degree(net, mode = "in")
      outdegrees <- igraph::degree(net, mode = "out")

      # Check if taxonomic API should be used
      checkbox_checked <- !is.null(input$use_taxonomic_api) && input$use_taxonomic_api
      plugin_enabled <- !is.null(plugin_states()) && plugin_states()[["taxonomic_api"]]
      use_api <- checkbox_checked && plugin_enabled

      # Warn if checkbox is checked but plugin is not enabled
      if (checkbox_checked && !plugin_enabled) {
        showNotification(
          "Taxonomic Database Integration plugin is not enabled. Go to Settings > Plugins to enable it.",
          type = "warning",
          duration = 10
        )
        message("WARNING: Taxonomic API checkbox is checked but plugin is disabled!")
        message("  Enable plugin in Settings > Plugins > Advanced > Taxonomic Database Integration")
        message("  Falling back to pattern matching for species classification")
      }

      if (use_api) {
        message("Using taxonomic API for enhanced species classification...")
        message("  Querying FishBase, WoRMS, and OBIS (this may take a few minutes)")
        message("  Results will be cached for faster subsequent imports")
        message(sprintf("  Processing %d species...", length(species_names)))

        # Initialize progress
        total_species <- length(species_names)
        taxonomic_progress(list(
          current = 0,
          total = total_species,
          percent = 0,
          message = sprintf("Starting taxonomic verification for %d species...\nQuerying FishBase, WoRMS, and OBIS databases.", total_species)
        ))

        # Allow Shiny to show initial progress UI
        Sys.sleep(0.1)

        # Initialize taxonomic report data
        taxonomic_report_data <- data.frame(
          species = character(total_species),
          functional_group = character(total_species),
          database_source = character(total_species),
          confidence = character(total_species),
          stringsAsFactors = FALSE
        )

        # Enhanced classification with API verification
        functional_groups <- sapply(seq_along(species_names), function(i) {
          sp <- species_names[i]

          # Update progress
          percent <- round((i / total_species) * 100)
          taxonomic_progress(list(
            current = i,
            total = total_species,
            percent = percent,
            message = sprintf("[%d/%d] Querying: %s\n\nThis may take a few minutes.\nResults are cached for faster subsequent imports.", i, total_species, sp)
          ))

          # Allow Shiny to update UI (force reactive flush)
          Sys.sleep(0.01)

          message(sprintf("  [%d/%d] Querying: %s", i, total_species, sp))

          # Get pattern-based hint first (fast, used to optimize API queries)
          pattern_hint <- assign_functional_group(
            sp,
            pb_values[i],
            indegrees[i],
            outdegrees[i],
            use_topology = TRUE
          )

          # Get full API result with database source information
          # Pass pattern hint to skip FishBase for non-fish groups
          api_result <- classify_species_api(sp, functional_group_hint = pattern_hint)

          # Store report data
          taxonomic_report_data[i, "species"] <<- sp
          taxonomic_report_data[i, "database_source"] <<- if(!is.na(api_result$source)) api_result$source else "Pattern matching"
          taxonomic_report_data[i, "confidence"] <<- api_result$confidence

          # Use API result if available and confident
          if (!is.na(api_result$functional_group) && api_result$confidence %in% c("high", "medium")) {
            taxonomic_report_data[i, "functional_group"] <<- api_result$functional_group
            api_result$functional_group
          } else {
            # Fall back to pattern hint
            taxonomic_report_data[i, "functional_group"] <<- pattern_hint
            pattern_hint
          }
        })

        # Store report for display
        taxonomic_report(taxonomic_report_data)

        # Clear progress when done
        taxonomic_progress(NULL)

        # Show summary message
        db_summary <- table(taxonomic_report_data$database_source)
        message("\nTaxonomic Database Match Summary:")
        for (src in names(db_summary)) {
          message(sprintf("  %s: %d species", src, db_summary[src]))
        }
      } else {
        # Standard pattern matching (faster, offline)
        functional_groups <- assign_functional_groups(
          species_names,
          pb_values,
          indegrees,
          outdegrees,
          use_topology = TRUE  # Use network topology for ECOPATH imports
        )
      }

      # Body masses: Use ECOPATH data if available, otherwise estimate by functional group
      if (!is.null(bodymass_values_raw)) {
        # Clean ECOPATH body mass values (remove sentinels, convert negative/zero to NA)
        bodymass_values_clean <- bodymass_values_raw
        bodymass_values_clean[bodymass_values_clean < -9000] <- NA  # ECOPATH missing value
        bodymass_values_clean[bodymass_values_clean <= 0] <- NA      # Invalid masses

        # Use actual values where available, estimate for missing
        body_masses <- sapply(1:length(functional_groups), function(i) {
          if (!is.na(bodymass_values_clean[i]) && bodymass_values_clean[i] > 0) {
            bodymass_values_clean[i]
          } else {
            estimate_body_mass_by_fg(functional_groups[i])
          }
        })

        n_actual <- sum(!is.na(bodymass_values_clean) & bodymass_values_clean > 0)
        n_estimated <- length(body_masses) - n_actual
        message("Body mass assignment:")
        message("  From ECOPATH data: ", n_actual, " species")
        message("  Estimated by functional group: ", n_estimated, " species")
      } else {
        # No body mass column found - estimate all by functional group
        body_masses <- sapply(functional_groups, estimate_body_mass_by_fg)
        message("Body mass assignment: All ", length(body_masses), " species estimated by functional group")
      }

      # Assign metabolic types using shared utility
      met_types <- sapply(functional_groups, estimate_metabolic_type_by_fg)

      # Calculate efficiencies using shared utility
      efficiencies <- sapply(functional_groups, estimate_efficiency_by_fg)

      # Create info data frame
      info <- data.frame(
        meanB = biomass_values,
        fg = factor(functional_groups, levels = get_functional_group_levels()),
        bodymasses = body_masses,
        met.types = met_types,
        efficiencies = efficiencies,
        PB = pb_values,
        QB = qb_values,
        row.names = species_names,
        stringsAsFactors = FALSE
      )

      # Return both processed data (net/info) and raw data (group_data/diet_data)
      # Raw data is needed for Rpath conversion
      # ECOSIM scenarios passed through from import
      return(list(
        net = net,
        info = info,
        metadata = metadata,
        group_data = group_table,
        diet_data = diet_table,
        ecosim_scenarios = import_result$ecosim_scenarios
      ))

    }, error = function(e) {
      stop(paste("Error parsing ECOPATH database:", e$message))
    })
  }

  # ============================================================================
  # DATA IMPORT HANDLER
  # ============================================================================

  # Output status message for data upload
  output$data_upload_status <- renderPrint({
    if (is.null(input$data_file)) {
      cat("No file uploaded yet.\n\n")
      cat("Current dataset: Gulf of Riga (default)\n")
      cat("  - 34 species\n")
      cat("  - 207 trophic links\n")
      cat("  - 5 functional groups\n")
    } else {
      cat("File selected:", input$data_file$name, "\n")
      cat("File size:", round(input$data_file$size / 1024, 2), "KB\n\n")
      cat("Click 'Load Data' button to import.\n")
    }
  })

  # Handle file upload when button clicked
  observeEvent(input$load_data, {
    req(input$data_file)

    tryCatch({
      file_path <- input$data_file$datapath
      file_ext <- tools::file_ext(input$data_file$name)

      # Update status
      output$data_upload_status <- renderPrint({
        cat("Processing file:", input$data_file$name, "\n")
        cat("Format:", toupper(file_ext), "\n\n")
        cat("Loading...")
      })

      # Load based on file type
      if (file_ext %in% c("Rdata", "rda")) {
        # Load RData file into separate environment
        # (to avoid overwriting app functions if RData contains them)
        env <- new.env()
        load(file_path, envir = env)

        # Validate required objects
        if (!exists("net", envir = env)) {
          stop("RData file must contain 'net' object (igraph network)")
        }
        if (!exists("info", envir = env)) {
          stop("RData file must contain 'info' data frame")
        }

        # Extract only data objects (not functions) from loaded environment
        # This ensures we use the app's function definitions, not ones from the RData file
        net <<- env$net
        info <<- env$info

        # Upgrade igraph if needed
        net <<- igraph::upgrade_graph(net)

        # Ensure vertex names are properly set
        # If vertex names are missing or numeric, try to get them from info$species or rownames
        if (is.null(igraph::V(net)$name) || all(grepl("^[0-9]+$", igraph::V(net)$name))) {
          if ("species" %in% colnames(info)) {
            igraph::V(net)$name <<- as.character(info$species)
          } else if (!is.null(rownames(info))) {
            igraph::V(net)$name <<- rownames(info)
          }
        }

        # Assign colors
        info$colfg <<- COLOR_SCHEME[as.numeric(info$fg)]

        # Update reactive values for dashboard
        net_reactive(net)
        info_reactive(info)

        # Refresh data editor tables
        refresh_data_editor()

        output$data_upload_status <- renderPrint({
          cat("✓ SUCCESS: Data loaded!\n\n")
          cat("Network: ", vcount(net), "species,", ecount(net), "links\n")
          cat("Species info:", nrow(info), "rows\n")
          cat("\nAll analysis tabs now use your uploaded data.\n")
          cat("Navigate to other tabs to explore.\n")
        })

      } else if (file_ext %in% c("xlsx", "xls")) {
        # Excel file - require readxl package
        if (!requireNamespace("readxl", quietly = TRUE)) {
          stop("Package 'readxl' required for Excel files.\nInstall with: install.packages('readxl')")
        }

        output$data_upload_status <- renderPrint({
          cat("✗ ERROR: Excel import not yet implemented.\n\n")
          cat("For now, please use:\n")
          cat("  - RData format (.Rdata), or\n")
          cat("  - Convert your Excel file to CSV\n\n")
          cat("Excel import coming in next version!\n")
        })

      } else if (file_ext == "csv") {
        output$data_upload_status <- renderPrint({
          cat("✗ ERROR: CSV import not yet implemented.\n\n")
          cat("For now, please use RData format (.Rdata)\n\n")
          cat("CSV import coming in next version!\n")
        })

      } else {
        stop("Unsupported file format")
      }

    }, error = function(e) {
      output$data_upload_status <- renderPrint({
        cat("✗ ERROR loading data:\n\n")
        cat(e$message, "\n\n")
        cat("Please check:\n")
        cat("  - File format is correct\n")
        cat("  - Required objects/sheets are present\n")
        cat("  - Data matches expected structure\n")
      })
    })
  })

  # ============================================================================
  # ECOPATH DATA IMPORT HANDLER
  # ============================================================================

  # Output status message for ECOPATH upload
  output$ecopath_upload_status <- renderPrint({
    if (is.null(input$ecopath_file) && is.null(input$ecopath_diet_file)) {
      cat("No ECOPATH files uploaded yet.\n\n")
      cat("Please upload both:\n")
      cat("  1. Basic Estimates file\n")
      cat("  2. Diet Composition matrix\n")
    } else if (is.null(input$ecopath_file)) {
      cat("Missing: Basic Estimates file\n")
    } else if (is.null(input$ecopath_diet_file)) {
      cat("Missing: Diet Composition file\n")
    } else {
      cat("Files selected:\n")
      cat("  Basic Estimates:", input$ecopath_file$name, "\n")
      cat("  Diet Composition:", input$ecopath_diet_file$name, "\n\n")
      cat("Click 'Import ECOPATH Data' button to process.\n")
    }
  })

  # Handle ECOPATH import when button clicked
  observeEvent(input$load_ecopath, {
    req(input$ecopath_file, input$ecopath_diet_file)

    tryCatch({
      basic_file <- input$ecopath_file$datapath
      diet_file <- input$ecopath_diet_file$datapath

      # Update status
      output$ecopath_upload_status <- renderPrint({
        cat("Processing ECOPATH files...\n\n")
        cat("Basic Estimates:", input$ecopath_file$name, "\n")
        cat("Diet Composition:", input$ecopath_diet_file$name, "\n\n")
        cat("Parsing and converting to EcoNeTool format...\n")
      })

      # Parse ECOPATH data
      result <- parse_ecopath_data(basic_file, diet_file)

      # Update global variables
      net <<- result$net
      info <<- result$info

      # Upgrade igraph if needed
      net <<- igraph::upgrade_graph(net)

      # Assign colors based on functional groups
      info$colfg <<- COLOR_SCHEME[as.numeric(info$fg)]

      # Refresh data editor tables
      refresh_data_editor()

      output$ecopath_upload_status <- renderPrint({
        cat("✓ SUCCESS: ECOPATH data imported!\n\n")
        cat("Conversion complete:\n")
        cat("  - Species/groups:", vcount(net), "\n")
        cat("  - Trophic links:", ecount(net), "\n")
        cat("  - Functional groups:", nlevels(info$fg), "\n\n")

        cat("Functional group distribution:\n")
        fg_table <- table(info$fg)
        for (fg_name in names(fg_table)) {
          cat("  ", fg_name, ":", fg_table[fg_name], "\n")
        }

        cat("\n⚠ Note: Default values assigned for:\n")
        cat("  - Body masses (based on functional groups)\n")
        cat("  - Metabolic types\n")
        cat("  - Assimilation efficiencies\n\n")
        cat("Use the 'Internal Data Editor' tab to refine these values.\n")
        cat("Navigate to other tabs to explore your ECOPATH model.\n")
      })

    }, error = function(e) {
      output$ecopath_upload_status <- renderPrint({
        cat("✗ ERROR importing ECOPATH data:\n\n")
        cat(e$message, "\n\n")
        cat("Common issues:\n")
        cat("  - Species names don't match between files\n")
        cat("  - Missing required columns (Group name, Biomass)\n")
        cat("  - File format not recognized\n")
        cat("  - Diet matrix structure incorrect\n\n")
        cat("Please check your ECOPATH export files.\n")
      })
    })
  })

  # ============================================================================
  # ECOPATH NATIVE DATABASE IMPORT HANDLER
  # ============================================================================

  # Reactive value to store ECOPATH native metadata preview
  ecopath_native_metadata <- reactiveVal(NULL)

  # Extract metadata when file is selected (before import button clicked)
  observeEvent(input$ecopath_native_file, {
    if (!is.null(input$ecopath_native_file)) {
      tryCatch({
        db_file <- input$ecopath_native_file$datapath

        # Quick metadata-only extraction
        result <- parse_ecopath_native_cross_platform(db_file)

        # Store metadata and basic counts
        ecopath_native_metadata(list(
          metadata = result$metadata,
          n_groups = nrow(result$group_data),
          n_links = nrow(result$diet_data),
          filename = input$ecopath_native_file$name,
          filesize = input$ecopath_native_file$size
        ))
      }, error = function(e) {
        # If extraction fails, store error
        ecopath_native_metadata(list(error = e$message))
      })
    } else {
      ecopath_native_metadata(NULL)
    }
  })

  # Dynamic UI for model preview or guide
  output$ecopath_native_preview_ui <- renderUI({
    preview_data <- ecopath_native_metadata()

    if (is.null(preview_data)) {
      # Show guide when no file selected
      box(
        title = "Native Database Guide",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        HTML("
          <h5>Installation Requirements</h5>
          <p><strong>Windows:</strong> ✓ Supported (RODBC + Microsoft Access Database Engine)</p>
          <pre style='background: #f8f9fa; padding: 5px; font-size: 11px;'>install.packages('RODBC')
# Download MS Access Database Engine from:
# https://www.microsoft.com/download/details.aspx?id=54920</pre>
          <p><strong>Linux/Mac:</strong> mdbtools + Hmisc required</p>
          <pre style='background: #f8f9fa; padding: 5px; font-size: 11px;'>sudo apt-get install mdbtools
install.packages('Hmisc')</pre>
          <hr>
          <h5>Supported Files</h5>
          <ul style='font-size: 12px;'>
            <li>.ewemdb - ECOPATH 6.x ✓</li>
            <li>.mdb - ECOPATH 5.x ✓</li>
            <li>.accdb - Access 2007+ ✓</li>
          </ul>
          <p style='font-size: 12px;'>Direct database import loads all model data: groups, diet matrix, biomass, P/B, Q/B, and parameters.</p>
        ")
      )
    } else if (!is.null(preview_data$error)) {
      # Show error if extraction failed
      box(
        title = "Error Reading Database",
        status = "danger",
        solidHeader = TRUE,
        width = 12,
        HTML(paste0("
          <p style='color: #d9534f;'><strong>Could not read database file</strong></p>
          <pre style='background: #f8f9fa; padding: 10px; font-size: 11px; color: #d9534f;'>", preview_data$error, "</pre>
          <p style='font-size: 12px;'>Please ensure:</p>
          <ul style='font-size: 12px;'>
            <li>File is a valid ECOPATH database (.ewemdb, .eweaccdb, .mdb, .eiidb, .accdb)</li>
            <li>Required packages are installed (RODBC on Windows, Hmisc on Linux/Mac)</li>
            <li>Microsoft Access Database Engine is installed (Windows only)</li>
          </ul>
        "))
      )
    } else {
      # Show model preview
      meta <- preview_data$metadata

      # Helper function to format metadata value
      fmt <- function(val) {
        if (is.null(val) || length(val) == 0 || (length(val) == 1 && is.na(val)) || val == "" || val == -9999) {
          "<span style='color: #999;'>Not specified</span>"
        } else {
          as.character(val)
        }
      }
      
      # Helper function to safely check if metadata field has valid value
      has_value <- function(field) {
        !is.null(field) && length(field) > 0 && !all(is.na(field)) && field[1] != "" && field[1] != -9999
      }

      # Build location string
      location_parts <- c()
      if (!is.null(meta) && has_value(meta$area_name)) {
        location_parts <- c(location_parts, meta$area_name)
      } else if (!is.null(meta) && has_value(meta$name)) {
        location_parts <- c(location_parts, meta$name)
      }
      if (!is.null(meta) && has_value(meta$country)) {
        location_parts <- c(location_parts, meta$country)
      }
      location_text <- if (length(location_parts) > 0) paste(location_parts, collapse = ", ") else fmt(NA)

      # Build time period string
      time_period_text <- fmt(NA)
      if (!is.null(meta) && has_value(meta$first_year)) {
        if (has_value(meta$num_years) && meta$num_years > 1) {
          end_year <- meta$first_year + meta$num_years - 1
          time_period_text <- paste0(meta$first_year, "-", end_year)
        } else {
          time_period_text <- as.character(meta$first_year)
        }
      }

      # Build geographic coordinates
      coords_text <- fmt(NA)
      if (!is.null(meta) && has_value(meta$min_lat) && has_value(meta$max_lat) && has_value(meta$min_lon) && has_value(meta$max_lon)) {
        coords_text <- sprintf("%.2f°-%.2f°N, %.2f°-%.2f°E", meta$min_lat, meta$max_lat, meta$min_lon, meta$max_lon)
      }

      # Build area text
      area_text <- fmt(meta$area)
      if (!is.null(meta) && has_value(meta$area) && meta$area > 0) {
        area_text <- paste0(meta$area, " km²")
      }

      # Build publication link
      pub_html <- ""
      if (!is.null(meta) && has_value(meta$publication_doi)) {
        pub_html <- paste0("<tr><td style='padding: 2px 0;'><strong>DOI:</strong></td><td><a href='https://doi.org/", meta$publication_doi, "' target='_blank' style='color: #337ab7;'>", meta$publication_doi, "</a></td></tr>")
      } else if (!is.null(meta) && has_value(meta$publication_uri)) {
        pub_html <- paste0("<tr><td style='padding: 2px 0;'><strong>Publication:</strong></td><td><a href='", meta$publication_uri, "' target='_blank' style='color: #337ab7;'>Link</a></td></tr>")
      } else if (!is.null(meta) && has_value(meta$publication_ref)) {
        pub_html <- paste0("<tr><td style='padding: 2px 0;'><strong>Reference:</strong></td><td style='font-size: 11px;'>", meta$publication_ref, "</td></tr>")
      }

      # Build description HTML (truncated if too long)
      desc_html <- ""
      if (!is.null(meta) && has_value(meta$description)) {
        desc_text <- meta$description
        if (nchar(desc_text) > 150) {
          desc_text <- paste0(substr(desc_text, 1, 147), "...")
        }
        desc_html <- paste0("<p style='font-size: 11px; color: #555; font-style: italic; margin: 8px 0;'>", desc_text, "</p>")
      }

      box(
        title = "Model Preview",
        status = "success",
        solidHeader = TRUE,
        width = 12,
        HTML(paste0("
          <h5 style='margin-top: 0;'>", preview_data$filename, "</h5>
          <p style='font-size: 11px; color: #888;'>", round(preview_data$filesize / 1024, 1), " KB</p>
          ", desc_html, "
          <hr style='margin: 10px 0;'>
          <table style='width: 100%; font-size: 12px;'>
            <tr style='background: #f0f9ff;'><td colspan='2' style='padding: 4px 0; font-weight: bold;'>GEOGRAPHIC</td></tr>
            <tr><td style='padding: 2px 0; width: 30%;'>Location:</td><td>", location_text, "</td></tr>
            <tr><td style='padding: 2px 0;'>Ecosystem Type:</td><td>", fmt(meta$ecosystem_type), "</td></tr>
            <tr><td style='padding: 2px 0;'>Area:</td><td>", area_text, "</td></tr>
            <tr><td style='padding: 2px 0;'>Coordinates:</td><td>", coords_text, "</td></tr>
            <tr style='background: #f0f9ff;'><td colspan='2' style='padding: 4px 0; font-weight: bold;'>TEMPORAL</td></tr>
            <tr><td style='padding: 2px 0;'>Time Period:</td><td>", time_period_text, "</td></tr>
            <tr style='background: #f0f9ff;'><td colspan='2' style='padding: 4px 0; font-weight: bold;'>ATTRIBUTION</td></tr>
            <tr><td style='padding: 2px 0;'>Author:</td><td>", fmt(meta$author), "</td></tr>
            <tr><td style='padding: 2px 0;'>Contact:</td><td>", fmt(meta$contact), "</td></tr>
            ", pub_html, "
            <tr style='background: #f0f9ff;'><td colspan='2' style='padding: 4px 0; font-weight: bold;'>MODEL DATA</td></tr>
            <tr><td style='padding: 2px 0;'><strong>Species/Groups:</strong></td><td><strong>", preview_data$n_groups, "</strong></td></tr>
            <tr><td style='padding: 2px 0;'><strong>Diet Links:</strong></td><td><strong>", preview_data$n_links, "</strong></td></tr>
          </table>
          <hr style='margin: 10px 0;'>
          <p style='font-size: 12px; color: #5cb85c;'><i class='fa fa-check-circle'></i> Ready to import</p>
        "))
      )
    }
  })

  # Output status message for ECOPATH native upload
  output$ecopath_native_status <- renderPrint({
    if (is.null(input$ecopath_native_file)) {
      cat("No ECOPATH database file uploaded yet.\n\n")
      cat("Accepted formats:\n")
      cat("  - .ewemdb (ECOPATH 6.x)\n")
      cat("  - .mdb (ECOPATH 5.x)\n")
      cat("  - .eiidb (Alternative format)\n\n")
      cat("Example: 'coast 2011-04-10 10.00.ewemdb'\n")
    } else {
      cat("File selected:", input$ecopath_native_file$name, "\n")
      cat("File size:", round(input$ecopath_native_file$size / 1024, 2), "KB\n\n")
      cat("Click 'Import Native Database' button to process.\n")
    }
  })

  # Render taxonomic API checkbox based on plugin state
  output$taxonomic_api_checkbox_ui <- renderUI({
    plugin_enabled <- !is.null(plugin_states()) && isTRUE(plugin_states()[["taxonomic_api"]])

    # Debug output
    message(sprintf("[Taxonomic Checkbox] Plugin enabled: %s", plugin_enabled))
    if (!is.null(plugin_states())) {
      message(sprintf("[Taxonomic Checkbox] Plugin state: %s", plugin_states()[["taxonomic_api"]]))
    }

    if (plugin_enabled) {
      # Plugin is enabled - show normal checkbox
      tagList(
        checkboxInput(
          "use_taxonomic_api",
          HTML("<strong>Verify species with taxonomic databases</strong> <span class='badge badge-info'>Advanced</span>"),
          value = FALSE
        ),
        helpText(
          HTML("<small>When enabled, queries <strong>FishBase</strong>, <strong>WoRMS</strong>, and <strong>OBIS</strong> for authoritative species classification and trait data. ",
               "Results are cached locally. Requires internet connection for first query. ",
               "<em>Slower but more accurate.</em></small>")
        )
      )
    } else {
      # Plugin is disabled - show disabled checkbox with help message
      # Check if packages are installed
      httr_installed <- requireNamespace("httr", quietly = TRUE)
      jsonlite_installed <- requireNamespace("jsonlite", quietly = TRUE)
      packages_ok <- httr_installed && jsonlite_installed

      help_msg <- if (!packages_ok) {
        paste0(
          "<small><strong style='color: #721c24;'>⚠ Missing Required Packages</strong><br>",
          "Install required packages first:<br>",
          "<code>install.packages(c('httr', 'jsonlite'))</code><br><br>",
          "Then go to: <code>Settings → Plugins → Advanced → Taxonomic Database Integration</code><br>",
          "Toggle the switch ON and click <strong>Save & Apply</strong></small>"
        )
      } else {
        paste0(
          "<small><strong style='color: #721c24;'>⚠ Plugin Disabled</strong><br>",
          "To use this feature:<br>",
          "1. Go to: <code>Settings → Plugins → Advanced → Taxonomic Database Integration</code><br>",
          "2. Toggle the switch to <strong>ON</strong><br>",
          "3. Click <strong>Save & Apply</strong> button<br>",
          "<em>(Packages are installed ✓)</em></small>"
        )
      }

      tagList(
        div(
          style = "opacity: 0.5; pointer-events: none;",
          checkboxInput(
            "use_taxonomic_api",
            HTML("<strong>Verify species with taxonomic databases</strong> <span class='badge badge-secondary'>Disabled</span>"),
            value = FALSE
          )
        ),
        helpText(
          HTML(paste0("<div style='background-color: #f8d7da; padding: 8px; border-radius: 4px; border-left: 3px solid #dc3545;'>",
                     help_msg,
                     "</div>"))
        )
      )
    }
  })

  # Reactive value to track taxonomic API progress
  taxonomic_progress <- reactiveVal(NULL)

  # Reactive value to store taxonomic database match report
  taxonomic_report <- reactiveVal(NULL)

  # Reactive value to cache EUSeaMap data (loaded once, reused)
  euseamap_data <- reactiveVal(NULL)

  # Render taxonomic progress indicator
  output$taxonomic_progress_ui <- renderUI({
    progress_info <- taxonomic_progress()

    if (is.null(progress_info)) {
      return(NULL)
    }

    # Show progress box
    div(
      style = "margin-top: 10px; padding: 10px; background-color: #e3f2fd; border-left: 4px solid #2196f3; border-radius: 4px;",
      h5(icon("fish"), " Taxonomic API Progress", style = "margin-top: 0; color: #1976d2;"),
      verbatimTextOutput("taxonomic_progress_text"),
      div(
        class = "progress",
        style = "height: 25px; margin-top: 10px;",
        div(
          class = "progress-bar progress-bar-striped progress-bar-animated bg-info",
          role = "progressbar",
          style = paste0("width: ", progress_info$percent, "%; transition: width 0.3s ease;"),
          paste0(progress_info$current, " / ", progress_info$total, " species")
        )
      )
    )
  })

  # Render progress text
  output$taxonomic_progress_text <- renderPrint({
    progress_info <- taxonomic_progress()
    if (!is.null(progress_info) && !is.null(progress_info$message)) {
      cat(progress_info$message)
    }
  })

  # Render taxonomic database match report
  output$taxonomic_report_ui <- renderUI({
    report_data <- taxonomic_report()

    if (is.null(report_data)) {
      return(NULL)
    }

    # Create summary statistics
    db_summary <- table(report_data$database_source)
    total_species <- nrow(report_data)

    tagList(
      div(
        style = "margin-top: 15px; padding: 12px; background-color: #e8f5e9; border-left: 4px solid #4caf50; border-radius: 4px;",
        h5(icon("database"), " Taxonomic Database Match Report", style = "margin-top: 0; color: #2e7d32;"),

        # Summary stats
        div(
          style = "margin-bottom: 10px;",
          h6("Summary:", style = "font-weight: bold; margin-bottom: 5px;"),
          tags$ul(
            style = "margin: 0; padding-left: 20px;",
            lapply(names(db_summary), function(src) {
              tags$li(sprintf("%s: %d species (%.1f%%)", src, db_summary[src],
                             (db_summary[src] / total_species) * 100))
            })
          )
        ),

        # Detailed table
        h6("Detailed Results:", style = "font-weight: bold; margin-top: 10px; margin-bottom: 5px;"),
        DTOutput("taxonomic_report_table"),

        # Download button
        div(
          style = "margin-top: 10px;",
          downloadButton("download_taxonomic_report", "Download Report (CSV)",
                        class = "btn-sm btn-success")
        )
      )
    )
  })

  # Render taxonomic report table
  output$taxonomic_report_table <- renderDT({
    report_data <- taxonomic_report()
    req(report_data)

    datatable(
      report_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'tip',
        order = list(list(2, 'desc'))  # Order by database_source
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'database_source',
        backgroundColor = styleEqual(
          c('FishBase', 'WoRMS', 'OBIS', 'Pattern matching'),
          c('#c8e6c9', '#fff9c4', '#b3e5fc', '#f5f5f5')
        )
      ) %>%
      formatStyle(
        'confidence',
        backgroundColor = styleEqual(
          c('high', 'medium', 'low'),
          c('#a5d6a7', '#fff59d', '#ffcc80')
        )
      )
  })

  # Download handler for taxonomic report
  output$download_taxonomic_report <- downloadHandler(
    filename = function() {
      paste0("taxonomic_report_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      report_data <- taxonomic_report()
      req(report_data)
      write.csv(report_data, file, row.names = FALSE)
    }
  )

  # Ensure taxonomic outputs are not suspended when hidden
  outputOptions(output, "taxonomic_progress_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "taxonomic_progress_text", suspendWhenHidden = FALSE)
  outputOptions(output, "taxonomic_report_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "taxonomic_report_table", suspendWhenHidden = FALSE)

  # ======================================================================
  # EMODnet Habitat Integration
  # ======================================================================

  # Observer to load EUSeaMap data when EMODnet habitat enrichment is enabled
  observeEvent(input$enable_emodnet_habitat, {
    if (input$enable_emodnet_habitat && is.null(euseamap_data())) {
      showNotification("Loading EUSeaMap habitat data (optimized regional loading)...",
                       type = "message", duration = NULL, id = "emodnet_loading")

      tryCatch({
        # Determine region from sampling location if available
        bbt_name <- NULL
        custom_bbox <- NULL

        # Check if there's a sampling location in the data
        if (!is.null(current_network()) && "sampling_lon" %in% names(current_network()$nodes)) {
          # Use first valid sampling location
          sampling_lon <- current_network()$nodes$sampling_lon[1]
          sampling_lat <- current_network()$nodes$sampling_lat[1]

          if (!is.na(sampling_lon) && !is.na(sampling_lat)) {
            # Create bbox around sampling point (±2 degrees = small area, avoids geometry errors)
            custom_bbox <- c(
              sampling_lon - 2, sampling_lat - 2,
              sampling_lon + 2, sampling_lat + 2
            )
            cat("\n🗺️  Loading habitat for sampling location:", sampling_lon, ",", sampling_lat, "\n")
          }
        }

        # If no custom bbox determined, default to small test area (Baltic)
        if (is.null(custom_bbox)) {
          cat("\n⚠️  No sampling location found, using default Baltic test area\n")
          custom_bbox <- c(20, 55, 21, 56)  # Small 1x1 degree test area
        }

        # Load regional EUSeaMap data with custom bbox (avoids large regional bbox!)
        euseamap <- load_regional_euseamap(
          bbt_name = bbt_name,
          custom_bbox = custom_bbox,
          path = "data/EUSeaMap_2025/EUSeaMap_2025.gdb"
        )
        euseamap_data(euseamap)

        # Get region info
        region <- attr(euseamap, "region") %||% "baltic"

        removeNotification("emodnet_loading")
        showNotification(
          sprintf("✓ EUSeaMap loaded: %d polygons (%s region)", nrow(euseamap), toupper(region)),
          type = "message",
          duration = 5
        )
      }, error = function(e) {
        removeNotification("emodnet_loading")
        showNotification(
          paste("Failed to load EUSeaMap:", e$message,
                "\nPlease ensure EUSeaMap_2025.gdb exists in data/ directory"),
          type = "error",
          duration = 10
        )
        # Disable checkbox if loading failed
        updateCheckboxInput(session, "enable_emodnet_habitat", value = FALSE)
      })
    }
  })

  # Handle ECOPATH native import when button clicked
  observeEvent(input$load_ecopath_native, {
    req(input$ecopath_native_file)

    tryCatch({
      db_file <- input$ecopath_native_file$datapath

      # Update status
      output$ecopath_native_status <- renderPrint({
        cat("Processing ECOPATH native database...\n\n")
        cat("File:", input$ecopath_native_file$name, "\n")
        cat("Size:", round(input$ecopath_native_file$size / 1024, 2), "KB\n\n")
        cat("Reading database tables...\n")
      })

      # Parse ECOPATH native database
      result <- parse_ecopath_native(db_file)

      # Store data for Rpath module (ECOPATH/ECOSIM integration)
      ecopath_import_data(result)

      # Update global variables (for backward compatibility)
      net <<- result$net
      info <<- result$info

      # Upgrade igraph if needed
      net <<- igraph::upgrade_graph(net)

      # Assign colors based on functional groups
      info$colfg <<- COLOR_SCHEME[as.numeric(info$fg)]

      # Update reactive values for dashboard
      net_reactive(net)
      info_reactive(info)

      # Refresh data editor tables
      refresh_data_editor()

      # Update dashboard metadata IMMEDIATELY (before metaweb conversion)
      # Extract location and time period from ECOPATH metadata if available
      location_text <- "ECOPATH Import"
      time_period_text <- "Model-derived"

      if (!is.null(result$metadata)) {
        meta <- result$metadata
        
        # Helper function to safely check if metadata field has valid value
        has_val <- function(field) {
          !is.null(field) && length(field) > 0 && !all(is.na(field)) && field[1] != "" && field[1] != -9999
        }

        # Build location from available metadata
        location_parts <- c()
        if (has_val(meta$area_name)) {
          location_parts <- c(location_parts, meta$area_name)
        } else if (has_val(meta$name)) {
          location_parts <- c(location_parts, meta$name)
        }
        if (has_val(meta$country)) {
          location_parts <- c(location_parts, meta$country)
        }
        if (has_val(meta$ecosystem_type)) {
          location_parts <- c(location_parts, paste0("(", meta$ecosystem_type, ")"))
        }
        if (length(location_parts) > 0) {
          location_text <- paste(location_parts, collapse = ", ")
        }

        # Build time period from available metadata
        if (has_val(meta$first_year)) {
          if (has_val(meta$num_years) && meta$num_years > 1) {
            end_year <- meta$first_year + meta$num_years - 1
            time_period_text <- paste0(meta$first_year, "-", end_year)
          } else {
            time_period_text <- as.character(meta$first_year)
          }
        } else if (has_val(meta$date_start) && has_val(meta$date_end)) {
          time_period_text <- paste(meta$date_start, "to", meta$date_end)
        }
      }

      metaweb_metadata(list(
        location = location_text,
        time_period = time_period_text,
        source = input$ecopath_native_file$name
      ))

      # Trigger dashboard update
      dashboard_trigger(dashboard_trigger() + 1)

      # Convert to metaweb format and update current metaweb
      metaweb_created <- FALSE
      tryCatch({
        # Create species data frame for metaweb
        species_data <- data.frame(
          species_id = igraph::V(net)$name,
          species_name = igraph::V(net)$name,
          functional_group = as.character(info$fg),
          biomass = info$meanB,
          stringsAsFactors = FALSE
        )

        # Add optional columns if available
        if ("PB" %in% colnames(info)) species_data$pb_ratio <- info$PB
        if ("QB" %in% colnames(info)) species_data$qb_ratio <- info$QB
        if ("bodymasses" %in% colnames(info)) species_data$body_mass <- info$bodymasses
        if ("taxon" %in% colnames(info)) species_data$taxon <- info$taxon

        # ============================================================
        # Add EMODnet habitat data if enabled
        # ============================================================
        if (isTRUE(input$enable_emodnet_habitat) && !is.null(euseamap_data())) {
          tryCatch({
            # Get sampling location from inputs
            sampling_lon <- input$sampling_longitude
            sampling_lat <- input$sampling_latitude

            # Validate coordinates
            if (!is.null(sampling_lon) && !is.null(sampling_lat) &&
                !is.na(sampling_lon) && !is.na(sampling_lat)) {

              # Add habitat data to all species
              species_data <- add_habitat_to_species(
                species_data,
                sampling_lon,
                sampling_lat,
                euseamap_data()
              )

              showNotification(
                sprintf("✓ Added habitat data at %.4f°E, %.4f°N", sampling_lon, sampling_lat),
                type = "message",
                duration = 3
              )
            } else {
              warning("Invalid sampling coordinates for habitat enrichment")
            }
          }, error = function(e) {
            warning("Failed to add habitat data: ", e$message)
            showNotification(
              paste("Warning: Could not add habitat data:", e$message),
              type = "warning",
              duration = 5
            )
          })
        }
        # ============================================================

        # Create interactions data frame for metaweb
        edges <- as_edgelist(net)
        interactions_data <- data.frame(
          predator_id = edges[,1],
          prey_id = edges[,2],
          quality_code = 3,  # ECOPATH data = code 3 (model-derived)
          source = paste0("ECOPATH: ", input$ecopath_native_file$name),
          stringsAsFactors = FALSE
        )

        # Create metaweb object
        metaweb <- create_metaweb(
          species = species_data,
          interactions = interactions_data,
          metadata = list(
            name = tools::file_path_sans_ext(input$ecopath_native_file$name),
            source = "ECOPATH native database",
            region = "Imported ECOPATH model",
            file = input$ecopath_native_file$name
          )
        )

        # Store metaweb
        current_metaweb(metaweb)
        metaweb_created <- TRUE

      }, error = function(e) {
        warning("Could not convert to metaweb format: ", e$message)
        print(paste("Metaweb conversion error:", e$message))
      })

      output$ecopath_native_status <- renderPrint({
        cat("✓ SUCCESS: ECOPATH native database imported!\n\n")
        cat("Database:", input$ecopath_native_file$name, "\n\n")

        cat("Conversion complete:\n")
        cat("  - Species/groups:", vcount(net), "\n")
        cat("  - Trophic links:", ecount(net), "\n")
        cat("  - Functional groups:", nlevels(info$fg), "\n\n")

        cat("Functional group distribution:\n")
        fg_table <- table(info$fg)
        for (fg_name in names(fg_table)) {
          cat("  ", fg_name, ":", fg_table[fg_name], "\n")
        }

        cat("\nP/B and Q/B ratios:\n")
        if ("PB" %in% colnames(info)) {
          cat("  Mean P/B:", round(mean(info$PB, na.rm = TRUE), 3), "\n")
        }
        if ("QB" %in% colnames(info)) {
          cat("  Mean Q/B:", round(mean(info$QB, na.rm = TRUE), 3), "\n")
        }

        cat("\n⚠ Note: Default values assigned for:\n")
        cat("  - Body masses (based on functional groups)\n")
        cat("  - Metabolic types (vertebrates vs invertebrates)\n")
        cat("  - Assimilation efficiencies\n\n")

        cat("✓ P/B and Q/B ratios preserved from ECOPATH model\n")
        if (metaweb_created) {
          cat("✓ Metaweb format created and loaded\n")
        } else {
          cat("⚠ Metaweb conversion had issues (check console)\n")
        }
        cat("✓ Dashboard boxes updated\n\n")

        cat("Use the 'Internal Data Editor' tab to refine these values.\n")
        cat("Navigate to other tabs to explore your ECOPATH model.\n")
        cat("\nFor keystoneness analysis, go to 'Keystoneness Analysis' tab.\n")
      })

    }, error = function(e) {
      output$ecopath_native_status <- renderPrint({
        cat("✗ ERROR importing ECOPATH native database:\n\n")
        cat(e$message, "\n\n")

        cat("==================================================\n")
        cat("SOLUTION:\n")
        cat("==================================================\n\n")

        # Detect operating system
        os <- Sys.info()["sysname"]

        if (os == "Windows") {
          cat("⚠ WINDOWS USERS:\n")
          cat("Windows ECOPATH import requires RODBC package.\n\n")
          cat("SOLUTION:\n")
          cat("  1. Install RODBC package:\n")
          cat("     install.packages('RODBC')\n")
          cat("  2. Install Microsoft Access Database Engine:\n")
          cat("     Download from: https://www.microsoft.com/download/details.aspx?id=54920\n")
          cat("     (Choose 64-bit if using 64-bit R)\n\n")
          cat("ALTERNATIVE: Use CSV/Excel export method above\n\n")
        } else {
          cat("LINUX/MAC USERS:\n")
          cat("Install mdbtools package:\n")
          cat("  Linux: sudo apt-get install mdbtools\n")
          cat("  Mac:   brew install mdbtools\n\n")
          cat("Also install Hmisc package:\n")
          cat("  install.packages('Hmisc')\n\n")
        }

        cat("--------------------------------------------------\n")
        cat("Alternative solutions:\n")
        cat("--------------------------------------------------\n\n")

        cat("1. Missing Hmisc package:\n")
        cat("   Solution: install.packages('Hmisc')\n\n")

        cat("2. Use CSV/Excel exports instead (all platforms):\n")
        cat("   - Export from ECOPATH: File > Export\n")
        cat("   - Use 'Import ECOPATH CSV/Excel Exports' above\n\n")

        cat("3. Corrupted database file:\n")
        cat("   Solution: Re-export from ECOPATH software\n")
      })
    })
  })

  # ============================================================================
  # VISUALIZATION OUTPUTS
  # ============================================================================

  # Food Web Visualization (visNetwork)
  output$foodweb_visnet <- renderVisNetwork({
    tryCatch({
      # Use reactive values to ensure automatic updates
      current_net <- net_reactive()
      current_info <- info_reactive()

      # Use common visualization function with fixed node sizes
      create_foodweb_visnetwork(
        net = current_net,
        info = current_info,
        node_size_method = "fixed",
        edge_color_by = "default"
      )
    }, error = function(e) {
      # Return empty network on error
      visNetwork(data.frame(id=1, label="Error", title=e$message),
                 data.frame(from=integer(0), to=integer(0)))
    })
  })

  output$basal_species <- renderPrint({
    tryCatch({
      basal <- igraph::V(net)$name[igraph::degree(net, mode="in")==0]
      cat("Basal species:\n", paste(basal, collapse=", "))
    }, error = function(e) {
      cat("Error identifying basal species:", e$message)
    })
  })

  output$top_predators <- renderPrint({
    tryCatch({
      top_pred <- igraph::V(net)$name[igraph::degree(net, mode="out")==0]
      cat("Top predators:\n", paste(top_pred, collapse=", "))
    }, error = function(e) {
      cat("Error identifying top predators:", e$message)
    })
  })

  output$adjacency_heatmap <- renderPlot({
    tryCatch({
      netmatrix <- as_adjacency_matrix(net, sparse=F)
      heatmap(netmatrix, Rowv=NA, Colv=NA, scale="none")
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating adjacency heatmap:", e$message))
    })
  })

  # Topological Indicators
  output$topo_indicators <- renderPrint({
    tryCatch({
      ind <- get_topological_indicators(net)
      print(ind)
    }, error = function(e) {
      cat("Error calculating topological indicators:", e$message)
    })
  })

  output$node_weighted_indicators <- renderPrint({
    tryCatch({
      ind <- get_node_weighted_indicators(net, info)
      print(ind)
    }, error = function(e) {
      cat("Error calculating node-weighted indicators:", e$message)
    })
  })

  # Node-weighted Indicators
  output$biomass_boxplot <- renderPlot({
    tryCatch({
      boxplot(info$meanB~info$fg, las=2, col=COLOR_SCHEME,
              ylab="Biomass (g/day/km2)", xlab="")
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating biomass boxplot:", e$message))
    })
  })

  output$biomass_barplot <- renderPlot({
    tryCatch({
      percB <- tapply(info$meanB, info$fg, sum)/sum(info$meanB)*100
      barplot(as.matrix(percB), col=COLOR_SCHEME, ylab="%")
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating biomass barplot:", e$message))
    })
  })

  # Biomass Network Visualization (visNetwork with hierarchical layout)
  output$foodweb_biomass_visnet <- renderVisNetwork({
    tryCatch({
      # Use reactive values to ensure automatic updates
      current_net <- net_reactive()
      current_info <- info_reactive()

      # Use common visualization function with biomass-scaled nodes
      create_foodweb_visnetwork(
        net = current_net,
        info = current_info,
        node_size_method = "biomass_sqrt",
        edge_color_by = "prey"
      )
    }, error = function(e) {
      # Return empty network on error
      visNetwork(
        data.frame(id = 1, label = "Error", stringsAsFactors = FALSE),
        data.frame(from = numeric(0), to = numeric(0)),
        width = "100%", height = "600px"
      ) %>%
        visNodes(color = "red") %>%
        visInteraction(tooltipDelay = 0) %>%
        visOptions(nodesIdSelection = FALSE)
    })
  })

  # Ensure visualization outputs are not suspended when hidden (load by default)
  outputOptions(output, "foodweb_visnet", suspendWhenHidden = FALSE)
  outputOptions(output, "basal_species", suspendWhenHidden = FALSE)
  outputOptions(output, "top_predators", suspendWhenHidden = FALSE)
  outputOptions(output, "adjacency_heatmap", suspendWhenHidden = FALSE)
  outputOptions(output, "topo_indicators", suspendWhenHidden = FALSE)
  outputOptions(output, "node_weighted_indicators", suspendWhenHidden = FALSE)
  outputOptions(output, "biomass_boxplot", suspendWhenHidden = FALSE)
  outputOptions(output, "biomass_barplot", suspendWhenHidden = FALSE)
  outputOptions(output, "foodweb_biomass_visnet", suspendWhenHidden = FALSE)

  # ============================================================================
  # FLUX ANALYSIS - CACHED REACTIVE
  # ============================================================================

  # Cache expensive flux calculations to avoid redundant computation
  flux_results <- reactive({
    req(net_reactive(), info_reactive())
    withProgress(message = 'Calculating energy fluxes...', {
      get_fluxweb_results(net_reactive(), info_reactive())
    })
  }) %>% bindCache(net_reactive(), info_reactive())

  # Fluxweb Analysis
  output$flux_heatmap <- renderPlot({
    tryCatch({
      res <- flux_results()
      heatmap(log(res$fluxes + FLUX_LOG_EPSILON), Rowv=NA, Colv=NA, scale="none")
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating flux heatmap:", e$message))
    })
  })

  output$flux_network_plot <- renderVisNetwork({
    tryCatch({
      # Use reactive values to ensure automatic updates
      current_net <- net_reactive()
      current_info <- info_reactive()

      # Get cached flux results
      res <- flux_results()

      # Prepare edge data with flux information
      flux_weights <- E(res$netLW)$weight
      edge_widths <- EDGE_WIDTH_MIN + (flux_weights/max(flux_weights) * EDGE_WIDTH_SCALE)

      # Format flux values for display
      flux_display <- sapply(flux_weights, function(x) {
        if (x >= 0.01) {
          sprintf("%.4f", x)
        } else if (x >= 0.0001) {
          sprintf("%.6f", x)
        } else {
          sprintf("%.2e", x)
        }
      })

      edge_data <- data.frame(
        width = edge_widths,
        value = flux_weights,
        title = paste0("Flux: ", flux_display, " kJ/day/km²"),
        stringsAsFactors = FALSE
      )

      # Use common visualization function with flux network
      create_foodweb_visnetwork(
        net = current_net,
        info = current_info,
        node_size_method = "fixed",
        edge_network = res$netLW,
        edge_data = edge_data,
        edge_color_by = "default"
      )
    }, error = function(e) {
      # Return empty network on error
      visNetwork(data.frame(id=1, label="Error", title=e$message),
                 data.frame(from=integer(0), to=integer(0)))
    })
  })

  output$flux_indicators <- renderPrint({
    tryCatch({
      res <- flux_results()
      print(fluxind(res$fluxes))
    }, error = function(e) {
      cat("Error calculating flux indicators:", e$message)
    })
  })

  # Ensure flux analysis outputs are not suspended when hidden (load by default)
  outputOptions(output, "flux_heatmap", suspendWhenHidden = FALSE)
  outputOptions(output, "flux_network_plot", suspendWhenHidden = FALSE)
  outputOptions(output, "flux_indicators", suspendWhenHidden = FALSE)

  # ============================================================================
  # KEYSTONENESS ANALYSIS - CACHED REACTIVE
  # ============================================================================

  # Cache expensive keystoneness calculations (includes MTI matrix inversion)
  keystoneness_results <- reactive({
    req(net_reactive(), info_reactive())
    withProgress(message = 'Calculating keystoneness indices...', {
      calculate_keystoneness(net_reactive(), info_reactive())
    })
  }) %>% bindCache(net_reactive(), info_reactive())

  # Keystoneness table
  output$keystoneness_table <- DT::renderDataTable({
    tryCatch({
      ks_results <- keystoneness_results()

      # Format for display
      ks_display <- ks_results
      ks_display$overall_effect <- round(ks_display$overall_effect, 4)
      ks_display$relative_biomass <- round(ks_display$relative_biomass, 4)
      ks_display$keystoneness <- round(ks_display$keystoneness, 3)

      DT::datatable(
        ks_display,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          order = list(list(3, 'desc'))  # Sort by keystoneness
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          'keystone_status',
          backgroundColor = DT::styleEqual(
            c('Keystone', 'Dominant', 'Rare', 'Undefined'),
            c('#ffcccc', '#cce5ff', '#e6e6e6', '#fff9cc')
          )
        )
    }, error = function(e) {
      DT::datatable(data.frame(Error = paste("Error calculating keystoneness:", e$message)))
    })
  })

  # Keystoneness vs Biomass plot
  output$keystoneness_plot <- renderPlot({
    tryCatch({
      ks_results <- keystoneness_results()

      # Create color mapping for status
      status_colors <- c(
        "Keystone" = "#ff4444",
        "Dominant" = "#4444ff",
        "Rare" = "#999999",
        "Undefined" = "#ffcc00"
      )

      plot(
        ks_results$relative_biomass,
        ks_results$keystoneness,
        col = status_colors[ks_results$keystone_status],
        pch = 19,
        cex = 1.5,
        xlab = "Relative Biomass (proportion of total)",
        ylab = "Keystoneness Index",
        main = "Keystoneness vs Relative Biomass",
        log = "x"  # Log scale for biomass
      )

      # Add reference lines
      abline(h = 1, lty = 2, col = "gray50")
      abline(v = 0.05, lty = 2, col = "gray50")

      # Add labels for top keystone species
      top_n <- min(5, nrow(ks_results))
      top_species <- ks_results[1:top_n, ]

      text(
        top_species$relative_biomass,
        top_species$keystoneness,
        labels = top_species$species,
        pos = 4,
        cex = 0.7,
        col = "black"
      )

      # Add legend
      legend(
        "topright",
        legend = names(status_colors),
        col = status_colors,
        pch = 19,
        cex = 0.8,
        title = "Status"
      )

      # Add text annotations
      text(0.001, 1, "Keystone threshold", pos = 3, cex = 0.7, col = "gray50")
      text(0.05, max(ks_results$keystoneness, na.rm = TRUE) * 0.9,
           "5% biomass threshold", pos = 4, cex = 0.7, col = "gray50")

    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating plot:", e$message))
    })
  })

  # MTI Heatmap
  output$mti_heatmap <- renderPlot({
    tryCatch({
      mti_matrix <- calculate_mti(net, info)

      # Create color palette (red = negative, blue = positive)
      colors <- colorRampPalette(c("red", "white", "blue"))(100)

      # Determine symmetric color scale around zero
      max_abs <- max(abs(mti_matrix), na.rm = TRUE)
      breaks <- seq(-max_abs, max_abs, length.out = 101)

      # Create heatmap
      heatmap(
        mti_matrix,
        Rowv = NA,
        Colv = NA,
        scale = "none",
        col = colors,
        breaks = breaks,
        margins = c(8, 8),
        main = "Mixed Trophic Impact Matrix",
        xlab = "Impacting Species (impactor)",
        ylab = "Impacted Species",
        cexRow = 0.7,
        cexCol = 0.7
      )

    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating MTI heatmap:", e$message))
    })
  })

  # Keystone summary
  output$keystone_summary <- renderPrint({
    tryCatch({
      ks_results <- keystoneness_results()

      cat("=== KEYSTONENESS ANALYSIS SUMMARY ===\n\n")

      # Overall statistics
      cat("Total species analyzed:", nrow(ks_results), "\n")
      cat("Keystone species:", sum(ks_results$keystone_status == "Keystone", na.rm = TRUE), "\n")
      cat("Dominant species:", sum(ks_results$keystone_status == "Dominant", na.rm = TRUE), "\n")
      cat("Rare species:", sum(ks_results$keystone_status == "Rare", na.rm = TRUE), "\n\n")

      # Top 5 keystone species
      cat("=== TOP 5 KEYSTONE SPECIES ===\n\n")
      top_5 <- ks_results[1:min(5, nrow(ks_results)), ]

      for (i in 1:nrow(top_5)) {
        cat(sprintf("%d. %s\n", i, top_5$species[i]))
        cat(sprintf("   Keystoneness Index: %.3f\n", top_5$keystoneness[i]))
        cat(sprintf("   Overall Effect: %.4f\n", top_5$overall_effect[i]))
        cat(sprintf("   Relative Biomass: %.4f (%.2f%%)\n",
                    top_5$relative_biomass[i],
                    top_5$relative_biomass[i] * 100))
        cat(sprintf("   Status: %s\n", top_5$keystone_status[i]))
        cat("\n")
      }

      # Interpretation
      cat("=== INTERPRETATION ===\n\n")
      cat("Keystone species have high ecosystem impact relative to their biomass.\n")
      cat("These species play critical roles in maintaining ecosystem structure.\n")
      cat("Their removal could lead to disproportionate ecosystem changes.\n\n")

      cat("MTI values indicate:\n")
      cat("  - Positive: Increase in impactor increases impacted species\n")
      cat("  - Negative: Increase in impactor decreases impacted species\n")
      cat("  - Magnitude: Strength of direct + indirect effects\n")

    }, error = function(e) {
      cat("Error generating keystoneness summary:", e$message)
    })
  })

  # Ensure keystoneness analysis outputs are not suspended when hidden (load by default)
  outputOptions(output, "keystoneness_table", suspendWhenHidden = FALSE)
  outputOptions(output, "keystoneness_plot", suspendWhenHidden = FALSE)
  outputOptions(output, "mti_heatmap", suspendWhenHidden = FALSE)
  outputOptions(output, "keystone_summary", suspendWhenHidden = FALSE)

  # ============================================================================
  # DOWNLOAD HANDLERS FOR EXAMPLE DATASETS
  # ============================================================================

  # Simple 3-Species downloads
  output$download_simple_rdata <- downloadHandler(
    filename = function() { "Simple_3Species.Rdata" },
    content = function(file) {
      file.copy("examples/Simple_3Species.Rdata", file)
    }
  )

  output$download_simple_csv_net <- downloadHandler(
    filename = function() { "Simple_3Species_network.csv" },
    content = function(file) {
      file.copy("examples/Simple_3Species_network.csv", file)
    }
  )

  output$download_simple_csv_info <- downloadHandler(
    filename = function() { "Simple_3Species_info.csv" },
    content = function(file) {
      file.copy("examples/Simple_3Species_info.csv", file)
    }
  )

  # Caribbean Reef downloads
  output$download_reef_rdata <- downloadHandler(
    filename = function() { "Caribbean_Reef.Rdata" },
    content = function(file) {
      file.copy("examples/Caribbean_Reef.Rdata", file)
    }
  )

  output$download_reef_csv_net <- downloadHandler(
    filename = function() { "Caribbean_Reef_network.csv" },
    content = function(file) {
      file.copy("examples/Caribbean_Reef_network.csv", file)
    }
  )

  output$download_reef_csv_info <- downloadHandler(
    filename = function() { "Caribbean_Reef_info.csv" },
    content = function(file) {
      file.copy("examples/Caribbean_Reef_info.csv", file)
    }
  )

  # Template downloads
  output$download_template_rdata <- downloadHandler(
    filename = function() { "Template_Empty.Rdata" },
    content = function(file) {
      file.copy("examples/Template_Empty.Rdata", file)
    }
  )

  output$download_template_csv_net <- downloadHandler(
    filename = function() { "Template_network.csv" },
    content = function(file) {
      file.copy("examples/Template_network.csv", file)
    }
  )

  output$download_template_csv_info <- downloadHandler(
    filename = function() { "Template_info.csv" },
    content = function(file) {
      file.copy("examples/Template_info.csv", file)
    }
  )

  # ============================================================================
  # INTERNAL DATA EDITOR HANDLERS
  # ============================================================================

  # Reactive values to store editable data
  # Initialize directly with data
  species_data <- reactiveVal({
    info_copy <- info[, !names(info) %in% c("colfg"), drop = FALSE]
    info_copy
  })

  network_matrix_data <- reactiveVal({
    adj_matrix <- as.matrix(as_adjacency_matrix(net, sparse = FALSE))
    adj_matrix
  })

  # Function to refresh data editor tables
  refresh_data_editor <- function() {
    tryCatch({
      info_copy <- info[, !names(info) %in% c("colfg"), drop = FALSE]
      species_data(info_copy)

      adj_matrix <- as.matrix(as_adjacency_matrix(net, sparse = FALSE))
      network_matrix_data(adj_matrix)

      cat("Data editor tables refreshed\n")
    }, error = function(e) {
      cat("Error refreshing data editor:", e$message, "\n")
    })
  }

  # Render Species Info Table (editable with tooltips)
  output$species_info_table <- DT::renderDataTable({
    species_df <- species_data()

    cat("Rendering species_info_table. Data is:", ifelse(is.null(species_df), "NULL", "present"), "\n")
    if (!is.null(species_df)) {
      cat("  Rows:", nrow(species_df), "Columns:", ncol(species_df), "\n")
    }

    req(species_df)

    # Round numeric columns to 2 decimal places for display
    species_display <- species_df
    numeric_cols <- sapply(species_display, is.numeric)
    species_display[numeric_cols] <- lapply(species_display[numeric_cols], function(x) round(x, 2))

    DT::datatable(
      species_display,
      editable = TRUE,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        scrollY = "500px",
        dom = 'tp'
      ),
      rownames = TRUE
    ) %>%
      DT::formatRound(columns = which(numeric_cols), digits = 2)
  })

  # Handle Species Info Table edits
  observeEvent(input$species_info_table_cell_edit, {
    species_data_df <- species_data()
    info_edit <- input$species_info_table_cell_edit
    species_data_df[info_edit$row, info_edit$col] <- info_edit$value
    species_data(species_data_df)
  })

  # Save Species Info button
  observeEvent(input$save_species_info, {
    tryCatch({
      # Update global info variable
      edited_info <- species_data()

      # Validate required columns exist
      required_cols <- c("meanB", "fg", "bodymasses", "met.types", "efficiencies")
      missing_cols <- setdiff(required_cols, colnames(edited_info))
      if (length(missing_cols) > 0) {
        stop(paste("Missing required columns:", paste(missing_cols, collapse=", ")))
      }

      # Update global info
      info <<- edited_info

      # Reassign colors based on functional groups
      info$colfg <<- COLOR_SCHEME[as.numeric(info$fg)]

      output$species_info_status <- renderPrint({
        cat("✓ SUCCESS: Species information saved!\n")
        cat("Updated", nrow(info), "species records.\n")
        cat("\nNavigate to other tabs to see updated visualizations.\n")
      })
    }, error = function(e) {
      output$species_info_status <- renderPrint({
        cat("✗ ERROR saving species info:\n")
        cat(e$message, "\n")
      })
    })
  })

  # Render Network Adjacency Matrix Table (editable with tooltips)
  output$network_matrix_table <- DT::renderDataTable({
    matrix_df <- network_matrix_data()

    cat("Rendering network_matrix_table. Data is:", ifelse(is.null(matrix_df), "NULL", "present"), "\n")
    if (!is.null(matrix_df)) {
      cat("  Rows:", nrow(matrix_df), "Columns:", ncol(matrix_df), "\n")
    }

    req(matrix_df)

    DT::datatable(
      matrix_df,
      editable = TRUE,
      extensions = 'FixedColumns',
      options = list(
        pageLength = 34,
        scrollX = TRUE,
        scrollY = "500px",
        dom = 't',
        fixedColumns = list(leftColumns = 1)
      ),
      rownames = TRUE
    )
  })

  # Handle Network Matrix Table edits
  observeEvent(input$network_matrix_table_cell_edit, {
    matrix_data <- network_matrix_data()
    matrix_edit <- input$network_matrix_table_cell_edit
    matrix_data[matrix_edit$row, matrix_edit$col] <- as.numeric(matrix_edit$value)
    network_matrix_data(matrix_data)
  })

  # Save Network Matrix button
  observeEvent(input$save_network_matrix, {
    tryCatch({
      output$network_matrix_status <- renderPrint({
        cat("✓ Network matrix saved to memory.\n")
        cat("Click 'Update Network from Matrix' to apply changes to the network object.\n")
      })
    }, error = function(e) {
      output$network_matrix_status <- renderPrint({
        cat("✗ ERROR saving network matrix:\n")
        cat(e$message, "\n")
      })
    })
  })

  # Update Network from Matrix button
  observeEvent(input$update_network, {
    tryCatch({
      # Get edited matrix
      edited_matrix <- network_matrix_data()

      # Validate matrix is square
      if (nrow(edited_matrix) != ncol(edited_matrix)) {
        stop("Adjacency matrix must be square (same number of rows and columns)")
      }

      # Validate matrix contains only 0s and 1s
      if (!all(edited_matrix %in% c(0, 1))) {
        stop("Adjacency matrix must contain only 0 (no link) or 1 (link exists)")
      }

      # Create new network from adjacency matrix
      net <<- igraph::graph_from_adjacency_matrix(edited_matrix, mode = "directed")

      # Upgrade if needed
      net <<- igraph::upgrade_graph(net)

      # Explicitly set vertex names from rownames to ensure they're preserved
      if (!is.null(rownames(edited_matrix))) {
        igraph::V(net)$name <<- rownames(edited_matrix)
      }

      # Update reactive values for dashboard
      net_reactive(net)

      output$network_matrix_status <- renderPrint({
        cat("✓ SUCCESS: Network updated from matrix!\n")
        cat("Network now has:\n")
        cat("  - Species:", vcount(net), "\n")
        cat("  - Links:", ecount(net), "\n")
        cat("\nAll visualizations will now use the updated network.\n")
        cat("Navigate to other tabs to see the changes.\n")
      })
    }, error = function(e) {
      output$network_matrix_status <- renderPrint({
        cat("✗ ERROR updating network:\n")
        cat(e$message, "\n")
      })
    })
  })

  # Ensure data editor outputs are not suspended when hidden (load by default)
  outputOptions(output, "species_info_table", suspendWhenHidden = FALSE)
  outputOptions(output, "network_matrix_table", suspendWhenHidden = FALSE)

  # ============================================================================
  # METAWEB MANAGER SERVER LOGIC (MARBEFES WP3.2 Phase 2)
  # ============================================================================

  # Note: current_metaweb is defined at top of server function (global scope)

  # Load regional metaweb
  observeEvent(input$load_regional_btn, {
    req(input$regional_metaweb)

    if (input$regional_metaweb == "") {
      showNotification("Please select a region first", type = "warning")
      return()
    }

    tryCatch({
      # Map region IDs to actual file paths
      metaweb_paths <- list(
        "arctic_barents_arctic" = "metawebs/arctic/barents_arctic_kortsch2015.rds",
        "arctic_barents_boreal" = "metawebs/arctic/barents_boreal_kortsch2015.rds",
        "arctic_kongsfjorden" = "metawebs/arctic/kongsfjorden_farage2021.rds",
        "baltic_nordstrom" = NULL,  # Not yet available
        "baltic_kortsch" = "metawebs/baltic/baltic_kortsch2021.rds",
        "baltic_garrison" = NULL,  # Not yet available
        "atlantic_northsea" = "metawebs/atlantic/north_sea_frelat2022.rds",
        "mediterranean" = NULL  # Not yet available
      )

      # Get the file path for the selected metaweb
      metaweb_file <- metaweb_paths[[input$regional_metaweb]]

      # Check if metaweb is available
      if (is.null(metaweb_file)) {
        showNotification(
          paste0("Metaweb not yet available. Please download from literature sources (see metawebs/README.md)"),
          type = "warning",
          duration = 10
        )
        return()
      }

      if (!file.exists(metaweb_file)) {
        showNotification(
          paste0("Metaweb file not found: ", metaweb_file,
                 ". Please download from literature sources (see metawebs/README.md)"),
          type = "error",
          duration = 10
        )
        return()
      }

      metaweb <- readRDS(metaweb_file)
      current_metaweb(metaweb)

      showNotification(
        paste("Loaded metaweb:", metaweb$metadata$region %||% "Unknown region"),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      showNotification(
        paste("Error loading metaweb:", e$message),
        type = "error",
        duration = 10
      )
    })
  })

  # Import custom metaweb from CSV
  observeEvent(input$import_metaweb_btn, {
    req(input$metaweb_species_file, input$metaweb_interactions_file)

    tryCatch({
      species <- read.csv(input$metaweb_species_file$datapath, stringsAsFactors = FALSE)
      interactions <- read.csv(input$metaweb_interactions_file$datapath, stringsAsFactors = FALSE)

      metadata <- list(
        region = "Custom import",
        imported = Sys.time(),
        species_file = input$metaweb_species_file$name,
        interactions_file = input$metaweb_interactions_file$name
      )

      metaweb <- create_metaweb(species, interactions, metadata)
      current_metaweb(metaweb)

      showNotification(
        paste("Imported metaweb with", nrow(species), "species and", nrow(interactions), "interactions"),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      showNotification(
        paste("Error importing metaweb:", e$message),
        type = "error",
        duration = 10
      )
    })
  })

  # Metaweb summary output
  output$metaweb_summary <- renderPrint({
    if (is.null(current_metaweb())) {
      cat("No metaweb loaded.\n\n")
      cat("Load a pre-built regional metaweb or import your own CSV files to get started.\n")
    } else {
      print(current_metaweb())
    }
  })

  # Update species dropdowns when metaweb changes
  observe({
    req(current_metaweb())

    metaweb <- current_metaweb()
    species_choices <- setNames(
      metaweb$species$species_id,
      metaweb$species$species_name
    )

    updateSelectInput(session, "remove_species_id", choices = species_choices)
    updateSelectInput(session, "link_predator", choices = species_choices)
    updateSelectInput(session, "link_prey", choices = species_choices)
    updateSelectInput(session, "remove_link_predator", choices = species_choices)
    updateSelectInput(session, "remove_link_prey", choices = species_choices)
  })

  # Metaweb network visualization
  output$metaweb_network <- renderVisNetwork({
    req(current_metaweb())

    metaweb <- current_metaweb()

    # Create nodes
    nodes <- data.frame(
      id = metaweb$species$species_id,
      label = metaweb$species$species_name,
      group = metaweb$species$functional_group %||% rep("Other", nrow(metaweb$species)),
      title = paste0(
        "<b>", metaweb$species$species_name, "</b><br>",
        "ID: ", metaweb$species$species_id, "<br>",
        "Group: ", metaweb$species$functional_group %||% "NA"
      )
    )

    # Create edges
    edges <- data.frame(
      from = metaweb$interactions$predator_id,
      to = metaweb$interactions$prey_id,
      arrows = "to",
      title = paste0(
        "Quality: ", metaweb$interactions$quality_code, "<br>",
        "Source: ", metaweb$interactions$source
      ),
      color = ifelse(
        metaweb$interactions$quality_code == 1, "#2ecc71",
        ifelse(metaweb$interactions$quality_code == 2, "#f39c12",
               ifelse(metaweb$interactions$quality_code == 3, "#e67e22", "#e74c3c"))
      )
    )

    visNetwork(nodes, edges) %>%
      visIgraphLayout(layout = "layout_with_fr") %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1),
        nodesIdSelection = TRUE
      ) %>%
      visLegend(width = 0.1, position = "right", main = "Functional Groups")
  })

  # Species table
  output$metaweb_species_table <- renderDT({
    req(current_metaweb())

    DT::datatable(
      current_metaweb()$species,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'ftip'
      ),
      rownames = FALSE,
      filter = 'top'
    )
  })

  # Interactions table
  output$metaweb_interactions_table <- renderDT({
    req(current_metaweb())

    DT::datatable(
      current_metaweb()$interactions,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'ftip'
      ),
      rownames = FALSE,
      filter = 'top'
    )
  })

  # Add species
  observeEvent(input$add_species_btn, {
    req(current_metaweb(), input$new_species_id, input$new_species_name)

    tryCatch({
      updated_metaweb <- add_species_to_metaweb(
        current_metaweb(),
        input$new_species_id,
        input$new_species_name,
        input$new_species_fg
      )
      current_metaweb(updated_metaweb)

      output$metaweb_edit_status <- renderPrint({
        cat("✓ SUCCESS: Species added!\n")
        cat("Added:", input$new_species_name, "(", input$new_species_id, ")\n")
        cat("Current species count:", nrow(updated_metaweb$species), "\n")
      })

      # Clear inputs
      updateTextInput(session, "new_species_id", value = "")
      updateTextInput(session, "new_species_name", value = "")

      showNotification("Species added successfully", type = "message")
    }, error = function(e) {
      output$metaweb_edit_status <- renderPrint({
        cat("✗ ERROR adding species:\n")
        cat(e$message, "\n")
      })
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # Remove species
  observeEvent(input$remove_species_btn, {
    req(current_metaweb(), input$remove_species_id)

    tryCatch({
      updated_metaweb <- remove_species_from_metaweb(
        current_metaweb(),
        input$remove_species_id,
        input$remove_species_links
      )
      current_metaweb(updated_metaweb)

      output$metaweb_edit_status <- renderPrint({
        cat("✓ SUCCESS: Species removed!\n")
        cat("Removed:", input$remove_species_id, "\n")
        cat("Current species count:", nrow(updated_metaweb$species), "\n")
      })

      showNotification("Species removed successfully", type = "message")
    }, error = function(e) {
      output$metaweb_edit_status <- renderPrint({
        cat("✗ ERROR removing species:\n")
        cat(e$message, "\n")
      })
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # Add trophic link
  observeEvent(input$add_link_btn, {
    req(current_metaweb(), input$link_predator, input$link_prey)

    tryCatch({
      updated_metaweb <- add_trophic_link(
        current_metaweb(),
        input$link_predator,
        input$link_prey,
        as.integer(input$link_quality),
        input$link_source,
        input$link_notes
      )
      current_metaweb(updated_metaweb)

      output$metaweb_edit_status <- renderPrint({
        cat("✓ SUCCESS: Trophic link added!\n")
        cat("Predator:", input$link_predator, "\n")
        cat("Prey:", input$link_prey, "\n")
        cat("Quality:", input$link_quality, "\n")
        cat("Current link count:", nrow(updated_metaweb$interactions), "\n")
      })

      # Clear source and notes
      updateTextInput(session, "link_source", value = "")
      updateTextAreaInput(session, "link_notes", value = "")

      showNotification("Trophic link added successfully", type = "message")
    }, error = function(e) {
      output$metaweb_edit_status <- renderPrint({
        cat("✗ ERROR adding link:\n")
        cat(e$message, "\n")
      })
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # Remove trophic link
  observeEvent(input$remove_link_btn, {
    req(current_metaweb(), input$remove_link_predator, input$remove_link_prey)

    tryCatch({
      updated_metaweb <- remove_trophic_link(
        current_metaweb(),
        input$remove_link_predator,
        input$remove_link_prey
      )
      current_metaweb(updated_metaweb)

      output$metaweb_edit_status <- renderPrint({
        cat("✓ SUCCESS: Trophic link removed!\n")
        cat("Removed link from", input$remove_link_predator, "to", input$remove_link_prey, "\n")
        cat("Current link count:", nrow(updated_metaweb$interactions), "\n")
      })

      showNotification("Trophic link removed successfully", type = "message")
    }, error = function(e) {
      output$metaweb_edit_status <- renderPrint({
        cat("✗ ERROR removing link:\n")
        cat(e$message, "\n")
      })
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # Link quality plot
  output$link_quality_plot <- renderPlot({
    req(current_metaweb())

    quality_summary <- summarize_link_quality(current_metaweb())

    barplot(
      quality_summary$count,
      names.arg = paste0("Q", quality_summary$quality_code),
      col = c("#2ecc71", "#f39c12", "#e67e22", "#e74c3c"),
      main = "Link Quality Distribution",
      xlab = "Quality Code",
      ylab = "Number of Links",
      las = 1,
      ylim = c(0, max(quality_summary$count) * 1.1)
    )

    # Add count labels on top of bars
    text(
      x = 1:4 * 1.2 - 0.5,
      y = quality_summary$count,
      labels = quality_summary$count,
      pos = 3,
      cex = 1.2
    )
  })

  # Link quality summary table
  output$link_quality_table <- renderDT({
    req(current_metaweb())

    quality_summary <- summarize_link_quality(current_metaweb())

    DT::datatable(
      quality_summary,
      options = list(
        pageLength = 4,
        dom = 't',
        ordering = FALSE
      ),
      rownames = FALSE
    )
  })

  # Filtered links table
  output$filtered_links_table <- renderDT({
    req(current_metaweb(), input$quality_filter)

    metaweb <- current_metaweb()
    filtered_interactions <- metaweb$interactions[
      metaweb$interactions$quality_code %in% as.integer(input$quality_filter),
    ]

    DT::datatable(
      filtered_interactions,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'ftip'
      ),
      rownames = FALSE,
      filter = 'top'
    )
  })

  # Download metaweb
  output$download_metaweb <- downloadHandler(
    filename = function() {
      if (input$export_format == "csv") {
        paste0(input$export_filename, ".zip")
      } else {
        paste0(input$export_filename, ".rds")
      }
    },
    content = function(file) {
      req(current_metaweb())

      if (input$export_format == "csv") {
        # Export CSV - create a zip file with both files
        temp_dir <- tempdir()
        species_file <- file.path(temp_dir, paste0(input$export_filename, "_species.csv"))
        interactions_file <- file.path(temp_dir, paste0(input$export_filename, "_interactions.csv"))

        write.csv(current_metaweb()$species, species_file, row.names = FALSE)
        write.csv(current_metaweb()$interactions, interactions_file, row.names = FALSE)

        # Create zip file
        old_wd <- setwd(temp_dir)
        on.exit(setwd(old_wd))
        utils::zip(file, files = basename(c(species_file, interactions_file)))
      } else {
        # Export RDS
        saveRDS(current_metaweb(), file)
      }
    }
  )

  # Export to active network
  observeEvent(input$export_to_network_btn, {
    req(current_metaweb())

    tryCatch({
      # Convert metaweb to igraph
      new_net <- metaweb_to_igraph(current_metaweb())

      # Update global network
      net <<- new_net

      # Try to update info if it exists
      if ("species_name" %in% colnames(current_metaweb()$species)) {
        # Create a basic info data frame
        info_df <- current_metaweb()$species
        rownames(info_df) <- info_df$species_name

        # Ensure required columns exist (with defaults if missing)
        if (!"meanB" %in% colnames(info_df)) info_df$meanB <- 1
        if (!"fg" %in% colnames(info_df)) info_df$fg <- factor(rep("Other", nrow(info_df)))
        if (!"bodymasses" %in% colnames(info_df)) info_df$bodymasses <- 1
        if (!"met.types" %in% colnames(info_df)) info_df$met.types <- "Other"
        if (!"efficiencies" %in% colnames(info_df)) info_df$efficiencies <- 0.5

        info <<- info_df
      }

      output$export_status <- renderPrint({
        cat("✓ SUCCESS: Metaweb converted to active network!\n")
        cat("Species:", vcount(net), "\n")
        cat("Links:", ecount(net), "\n")
        cat("\nThe network is now available in:\n")
        cat("  • Food Web Network tab\n")
        cat("  • Topological Metrics tab\n")
        cat("  • Biomass Analysis tab (if biomass data available)\n")
        cat("  • Energy Fluxes tab (if trait data available)\n")
        cat("  • Keystoneness Analysis tab (if biomass data available)\n")
      })

      showNotification(
        "Metaweb converted to active network! Navigate to other tabs to analyze.",
        type = "message",
        duration = 10
      )
    }, error = function(e) {
      output$export_status <- renderPrint({
        cat("✗ ERROR converting metaweb:\n")
        cat(e$message, "\n")
      })
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # Note: export_status is created dynamically inside observeEvent, so no outputOptions needed

  # ============================================================================
  # SPATIAL ANALYSIS SERVER LOGIC (MARBEFES WP3.2 Phase 1)
  # ============================================================================

  # Reactive values for spatial analysis
  spatial_hex_grid <- reactiveVal(NULL)
  spatial_species_data <- reactiveVal(NULL)
  spatial_local_networks <- reactiveVal(NULL)
  spatial_metrics_data <- reactiveVal(NULL)
  spatial_metaweb <- reactiveVal(NULL)
  spatial_study_area <- reactiveVal(NULL)
  spatial_habitat_clipped <- reactiveVal(NULL)  # Clipped habitat data
  spatial_grid_with_habitat <- reactiveVal(NULL)  # Grid enriched with habitat

  # 0. Study Area Upload Handler (NEW)
  observeEvent(input$spatial_study_area, {
    req(input$spatial_study_area)

    tryCatch({
      uploaded_files <- input$spatial_study_area
      file_names <- uploaded_files$name
      file_paths <- uploaded_files$datapath

      # Determine file type
      if (any(grepl("\\.gpkg$", file_names, ignore.case = TRUE))) {
        # GeoPackage: single file
        gpkg_file <- file_paths[grepl("\\.gpkg$", file_names, ignore.case = TRUE)][1]
        study_area <- sf::st_read(gpkg_file, quiet = TRUE)

      } else if (any(grepl("\\.shp$", file_names, ignore.case = TRUE))) {
        # Shapefile: multiple components

        # Validate required components
        required_exts <- c(".shp", ".shx", ".dbf")
        has_required <- sapply(required_exts, function(ext) {
          any(grepl(paste0("\\", ext, "$"), file_names, ignore.case = TRUE))
        })

        if (!all(has_required)) {
          missing <- required_exts[!has_required]

          error_msg <- paste0(
            "Missing required shapefile components: ",
            paste(missing, collapse = ", "),
            "\n\n",
            "HOW TO FIX:\n",
            "1. Click 'Browse' button\n",
            "2. Navigate to your shapefile folder\n",
            "3. Hold Ctrl (Windows) or Cmd (Mac)\n",
            "4. Select ALL these files:\n",
            "   • ", basename(file_names[1]), " (you selected this)\n",
            if (!has_required[2]) "   • [missing] .shx file\n" else "",
            if (!has_required[3]) "   • [missing] .dbf file\n" else "",
            "   • .prj file (optional)\n",
            "5. Click 'Open'\n\n",
            "All files must have the same base name (e.g., boundary.shp, boundary.shx, boundary.dbf)"
          )

          stop(error_msg)
        }

        # Check for .prj (projection file)
        if (!any(grepl("\\.prj$", file_names, ignore.case = TRUE))) {
          showNotification(
            "Warning: No .prj file found. CRS may not be correctly defined.",
            type = "warning",
            duration = 8
          )
        }

        # Create temporary directory for shapefile components
        temp_dir <- file.path(tempdir(), "shapefile_upload")
        dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

        # Copy all files to temp directory with consistent naming
        base_name <- "study_area"
        for (i in seq_along(file_names)) {
          ext <- tools::file_ext(file_names[i])
          dest_file <- file.path(temp_dir, paste0(base_name, ".", ext))
          file.copy(file_paths[i], dest_file, overwrite = TRUE)
        }

        # Read shapefile
        shp_path <- file.path(temp_dir, paste0(base_name, ".shp"))
        study_area <- sf::st_read(shp_path, quiet = TRUE)

      } else {
        stop("Please upload either a .gpkg file or complete shapefile (.shp, .shx, .dbf, .prj)")
      }

      # Validate CRS
      if (is.na(sf::st_crs(study_area))) {
        showNotification(
          "Warning: Study area CRS is not defined. Assuming EPSG:4326 (WGS84).",
          type = "warning",
          duration = 8
        )
        sf::st_crs(study_area) <- 4326
      }

      # Transform to WGS84 if needed
      crs_current <- sf::st_crs(study_area)
      is_wgs84 <- FALSE

      # Check if already WGS84
      if (!is.na(crs_current$epsg) && crs_current$epsg == 4326) {
        is_wgs84 <- TRUE
      } else if (!is.na(crs_current$input)) {
        # Check if it's a longlat system
        crs_text <- tolower(crs_current$input)
        if (grepl("\\+proj=longlat", crs_text) && grepl("wgs.*84", crs_text)) {
          is_wgs84 <- TRUE
        }
      }

      # Transform if not WGS84
      if (!is_wgs84) {
        original_crs <- crs_current$input
        study_area <- sf::st_transform(study_area, crs = 4326)
        showNotification(
          sprintf("Study area transformed from %s to WGS84",
                  substr(original_crs, 1, 50)),
          type = "message",
          duration = 5
        )
      }

      # Validate geometry
      if (!all(sf::st_is_valid(study_area))) {
        study_area <- sf::st_make_valid(study_area)
        showNotification("Invalid geometry detected and repaired",
                        type = "warning", duration = 5)
      }

      # Dissolve multi-polygon to single boundary if needed
      if (nrow(study_area) > 1) {
        study_area <- sf::st_union(study_area)
        study_area <- sf::st_sf(geometry = study_area)
      }

      # Store in reactive value (this will trigger reactive outputs to update)
      spatial_study_area(study_area)

      # Reset BBT selector since custom file is being used
      updateSelectInput(session, "spatial_bbt_selector", selected = "")

      # Auto-fill bounding box inputs
      bbox <- sf::st_bbox(study_area)
      updateNumericInput(session, "spatial_xmin", value = round(as.numeric(bbox["xmin"]), 3))
      updateNumericInput(session, "spatial_ymin", value = round(as.numeric(bbox["ymin"]), 3))
      updateNumericInput(session, "spatial_xmax", value = round(as.numeric(bbox["xmax"]), 3))
      updateNumericInput(session, "spatial_ymax", value = round(as.numeric(bbox["ymax"]), 3))

      # Note: spatial_study_area_info and spatial_study_area_map
      # are reactive outputs defined below and will update automatically

      # Update main spatial map with study area (Visualization tab)
      suppressWarnings({
        # Prepare geometry for main map
        study_area_map <- study_area

        # Convert MULTIPOLYGON to POLYGON for better display
        geom_type_upload <- as.character(sf::st_geometry_type(study_area_map, by_geometry = FALSE))
        if (geom_type_upload == "MULTIPOLYGON") {
          study_area_map <- sf::st_cast(study_area_map, "POLYGON")
          if (nrow(study_area_map) > 1) {
            areas <- sf::st_area(study_area_map)
            study_area_map <- study_area_map[which.max(areas), ]
          }
        }

        # Get fresh bbox after geometry conversion
        bbox_upload <- sf::st_bbox(study_area_map)
        xmin_upload <- as.numeric(bbox_upload["xmin"])
        ymin_upload <- as.numeric(bbox_upload["ymin"])
        xmax_upload <- as.numeric(bbox_upload["xmax"])
        ymax_upload <- as.numeric(bbox_upload["ymax"])

        leafletProxy("spatial_map") %>%
          clearGroup("Study Area") %>%
          addPolygons(
            data = study_area_map,
            color = "#0033cc",
            weight = 2,
            opacity = 0.6,
            fillOpacity = 0.15,
            fillColor = "#3366ff",
            group = "Study Area",
            label = "Custom Study Area",
            labelOptions = labelOptions(
              style = list("font-weight" = "bold", padding = "3px 8px"),
              textsize = "13px",
              direction = "auto"
            ),
            highlightOptions = highlightOptions(
              weight = 4,
              color = "#FF0000",
              opacity = 0.8,
              fillOpacity = 0.4,
              bringToFront = TRUE
            )
          ) %>%
          showGroup("Study Area") %>%  # EXPLICITLY SHOW THE LAYER
          fitBounds(lng1 = xmin_upload, lat1 = ymin_upload,
                    lng2 = xmax_upload, lat2 = ymax_upload)
      })

      showNotification(
        "Study area loaded. Grid will be clipped to this boundary.",
        type = "message",
        duration = 5
      )

    }, error = function(e) {
      # Don't set spatial_study_area, leave it NULL
      # The reactive outputs will show "No study area loaded"

      # Show detailed error notification with longer duration
      showNotification(
        HTML(paste0(
          "<strong>Error Loading Study Area</strong><br><br>",
          gsub("\n", "<br>", e$message),
          "<br><br><em>Check the console for details.</em>"
        )),
        type = "error",
        duration = 15  # Longer duration for detailed instructions
      )

      # Log error to console for debugging
      cat("\n✗ ERROR loading study area:\n")
      cat(e$message, "\n\n")
    })
  })

  # BBT Polygon Selection Handler
  observeEvent(input$spatial_bbt_selector, {
    req(input$spatial_bbt_selector)

    # If empty selection, do nothing (user wants to upload custom boundary)
    if (input$spatial_bbt_selector == "") {
      return()
    }

    tryCatch({
      # Load BBT.geojson file
      bbt_file <- file.path("data", "BBT.geojson")

      if (!file.exists(bbt_file)) {
        showNotification(
          "BBT.geojson file not found in data directory",
          type = "error",
          duration = 5
        )
        return()
      }

      # Read the entire BBT geojson
      # Disable s2 to avoid geometry issues with 3D coordinates
      sf::sf_use_s2(FALSE)
      bbt_data <- sf::st_read(bbt_file, quiet = TRUE)

      # Filter by selected BBT name
      selected_bbt <- bbt_data[bbt_data$Name == input$spatial_bbt_selector, ]

      if (nrow(selected_bbt) == 0) {
        showNotification(
          paste("BBT polygon not found:", input$spatial_bbt_selector),
          type = "error",
          duration = 5
        )
        sf::sf_use_s2(TRUE)
        return()
      }

      # Drop Z/M coordinates if present
      selected_bbt <- sf::st_zm(selected_bbt, drop = TRUE, what = "ZM")

      # Check for invalid geometries
      if (any(!sf::st_is_valid(selected_bbt))) {
        selected_bbt <- sf::st_make_valid(selected_bbt)
      }

      # Ensure CRS is WGS84
      crs_info <- sf::st_crs(selected_bbt)
      if (is.na(crs_info$input)) {
        # No CRS set, assume WGS84
        sf::st_crs(selected_bbt) <- 4326
      } else if (is.null(crs_info$epsg) || crs_info$epsg != 4326) {
        # Transform to WGS84 if not already
        selected_bbt <- sf::st_transform(selected_bbt, crs = 4326)
      }

      # Keep BBT name for display
      bbt_name <- input$spatial_bbt_selector

      # Dissolve to single boundary if needed
      if (nrow(selected_bbt) > 1) {
        selected_bbt <- sf::st_union(selected_bbt)
        selected_bbt <- sf::st_sf(
          Name = bbt_name,
          geometry = selected_bbt
        )
      } else {
        # Ensure Name column exists
        if (!"Name" %in% names(selected_bbt)) {
          selected_bbt$Name <- bbt_name
        }
      }

      # Simplify geometry if too complex (for leaflet rendering)
      n_coords <- nrow(sf::st_coordinates(selected_bbt))
      if (n_coords > 10000) {
        cat("\n⚠ Simplifying geometry (", n_coords, " coordinates) for map display...\n")
        # Use rmapshaper for topology-preserving simplification
        selected_bbt <- sf::st_simplify(selected_bbt, dTolerance = 0.001, preserveTopology = TRUE)
        n_coords_new <- nrow(sf::st_coordinates(selected_bbt))
        cat("  Simplified to", n_coords_new, "coordinates\n")
      }

      # Re-enable s2
      sf::sf_use_s2(TRUE)

      # Store in reactive value
      spatial_study_area(selected_bbt)

      # Auto-fill bounding box inputs
      bbox <- sf::st_bbox(selected_bbt)
      updateNumericInput(session, "spatial_xmin", value = round(as.numeric(bbox["xmin"]), 3))
      updateNumericInput(session, "spatial_ymin", value = round(as.numeric(bbox["ymin"]), 3))
      updateNumericInput(session, "spatial_xmax", value = round(as.numeric(bbox["xmax"]), 3))
      updateNumericInput(session, "spatial_ymax", value = round(as.numeric(bbox["ymax"]), 3))

      # Debug: print to console
      cat("\n✓ BBT polygon loaded:", bbt_name, "\n")
      cat("  CRS:", sf::st_crs(selected_bbt)$input, "\n")
      cat("  EPSG:", sf::st_crs(selected_bbt)$epsg, "\n")
      cat("  Bbox: [", round(bbox["xmin"], 2), ",", round(bbox["ymin"], 2), "] to [",
          round(bbox["xmax"], 2), ",", round(bbox["ymax"], 2), "]\n")
      cat("  Area:", round(as.numeric(sf::st_area(selected_bbt)) / 1e6, 2), "km²\n")
      cat("  Geometry type:", as.character(sf::st_geometry_type(selected_bbt, by_geometry = FALSE)), "\n")
      cat("  Geometry valid:", all(sf::st_is_valid(selected_bbt)), "\n")
      cat("  Number of coordinates:", nrow(sf::st_coordinates(selected_bbt)), "\n\n")

      # Update main spatial map with study area (Visualization tab)
      cat("\n🗺️  Adding BBT to Visualization map...\n")
      suppressWarnings({
        # Prepare geometry for main map
        selected_bbt_map <- selected_bbt

        # Ensure WGS84 for leaflet
        crs_epsg <- sf::st_crs(selected_bbt_map)$epsg
        cat("  BBT CRS before:", sf::st_crs(selected_bbt_map)$input, "\n")
        if (is.null(crs_epsg) || is.na(crs_epsg) || crs_epsg != 4326) {
          if (is.null(crs_epsg) || is.na(crs_epsg)) {
            sf::st_crs(selected_bbt_map) <- 4326
          } else {
            selected_bbt_map <- sf::st_transform(selected_bbt_map, crs = 4326)
          }
          cat("  ⚠️ Transformed to WGS84\n")
        }

        # Convert MULTIPOLYGON to POLYGON for main map too
        geom_type_main <- as.character(sf::st_geometry_type(selected_bbt_map, by_geometry = FALSE))
        cat("  Geometry type:", geom_type_main, "\n")
        if (geom_type_main == "MULTIPOLYGON") {
          selected_bbt_map <- sf::st_cast(selected_bbt_map, "POLYGON")
          if (nrow(selected_bbt_map) > 1) {
            areas <- sf::st_area(selected_bbt_map)
            selected_bbt_map <- selected_bbt_map[which.max(areas), ]
            cat("  Selected largest polygon from", length(areas), "parts\n")
          }
        }

        # Get fresh bbox after geometry conversion
        bbox_main <- sf::st_bbox(selected_bbt_map)
        xmin_main <- as.numeric(bbox_main["xmin"])
        ymin_main <- as.numeric(bbox_main["ymin"])
        xmax_main <- as.numeric(bbox_main["xmax"])
        ymax_main <- as.numeric(bbox_main["ymax"])
        cat("  Bbox:", xmin_main, ymin_main, "to", xmax_main, ymax_main, "\n")
        cat("  Rows:", nrow(selected_bbt_map), "\n")

        leafletProxy("spatial_map") %>%
          clearGroup("Study Area") %>%
          addPolygons(
            data = selected_bbt_map,
            color = "#0033cc",
            weight = 2,
            opacity = 0.6,
            fillOpacity = 0.15,
            fillColor = "#3366ff",
            group = "Study Area",
            label = ~paste0("Study Area: ", Name),
            labelOptions = labelOptions(
              style = list("font-weight" = "bold", padding = "3px 8px"),
              textsize = "13px",
              direction = "auto"
            ),
            highlightOptions = highlightOptions(
              weight = 4,
              color = "#FF0000",
              opacity = 0.8,
              fillOpacity = 0.4,
              bringToFront = TRUE
            )
          ) %>%
          showGroup("Study Area") %>%  # EXPLICITLY SHOW THE LAYER
          fitBounds(lng1 = xmin_main, lat1 = ymin_main,
                    lng2 = xmax_main, lat2 = ymax_main)

        cat("  ✅ BBT added to Visualization map\n\n")
      })

      showNotification(
        HTML(paste0(
          "<strong>BBT Polygon Loaded: ", bbt_name, "</strong><br>",
          "Area: ", round(as.numeric(sf::st_area(selected_bbt)) / 1e6, 2), " km²<br>",
          "Check the Study Area Preview map and bounding box values"
        )),
        type = "message",
        duration = 8
      )

    }, error = function(e) {
      # Re-enable s2 in case of error
      sf::sf_use_s2(TRUE)

      showNotification(
        HTML(paste0(
          "<strong>Error Loading BBT Polygon</strong><br><br>",
          gsub("\n", "<br>", e$message)
        )),
        type = "error",
        duration = 10
      )

      cat("\n✗ ERROR loading BBT polygon:\n")
      cat(e$message, "\n\n")
    })
  })

  # Reactive study area preview map (updates when study area changes)
  output$spatial_study_area_map <- renderLeaflet({
    study_area <- spatial_study_area()

    cat("\n🗺️  Rendering spatial_study_area_map...\n")

    if (is.null(study_area)) {
      cat("  No study area - showing empty map\n")
      # Empty state
      leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
        addTiles(
          urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
          attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
        ) %>%
        setView(lng = 18.5, lat = 57, zoom = 5) %>%
        addControl(
          html = "<div style='padding: 10px; background: white; border-radius: 5px;'>
                  <i class='fa fa-info-circle'></i> Upload a study area to see preview
                  </div>",
          position = "topright"
        )
    } else {
      cat("  Study area present, rendering polygon...\n")

      tryCatch({
        # Ensure geometry is in WGS84 for leaflet
        crs_epsg <- sf::st_crs(study_area)$epsg
        if (is.null(crs_epsg) || is.na(crs_epsg) || crs_epsg != 4326) {
          if (is.null(crs_epsg) || is.na(crs_epsg)) {
            # Assign WGS84 if no CRS
            sf::st_crs(study_area) <- 4326
          } else {
            # Transform to WGS84
            study_area <- sf::st_transform(study_area, crs = 4326)
          }
        }

        # Convert MULTIPOLYGON to POLYGON for better leaflet compatibility
        cat("  Converting geometry for leaflet...\n")
        geom_type <- as.character(sf::st_geometry_type(study_area, by_geometry = FALSE))

        if (geom_type == "MULTIPOLYGON") {
          cat("  Casting MULTIPOLYGON to POLYGON...\n")
          study_area <- sf::st_cast(study_area, "POLYGON")
          # Take the largest polygon if multiple
          if (nrow(study_area) > 1) {
            areas <- sf::st_area(study_area)
            study_area <- study_area[which.max(areas), ]
            cat("  Selected largest polygon from", length(areas), "parts\n")
          }
        }

        # Get bbox AFTER geometry conversion
        bbox <- sf::st_bbox(study_area)
        # Convert to numeric to ensure valid bounds
        xmin <- as.numeric(bbox["xmin"])
        ymin <- as.numeric(bbox["ymin"])
        xmax <- as.numeric(bbox["xmax"])
        ymax <- as.numeric(bbox["ymax"])
        cat("  Bbox for map:", xmin, ",", ymin, "to", xmax, ",", ymax, "\n")

        # Create popup text
        popup_text <- paste0("Area: ", round(as.numeric(sf::st_area(study_area)) / 1e6, 2), " km²")
        if ("Name" %in% names(study_area)) {
          popup_text <- paste0("<strong>", study_area$Name[1], "</strong><br>", popup_text)
        }

        # Convert to leaflet-compatible format
        cat("  Creating leaflet map...\n")
        m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
          addTiles(
            urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
            attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
          ) %>%
          setView(lng = mean(c(xmin, xmax)),
                  lat = mean(c(ymin, ymax)),
                  zoom = 8)

        # Add polygon to map with semi-transparent styling
        cat("  Adding polygon to map (", nrow(study_area), "feature(s))...\n")
        m <- m %>%
          addPolygons(
            data = study_area,
            color = "#0033cc",
            weight = 2,
            opacity = 0.6,
            fillOpacity = 0.15,
            fillColor = "#3366ff",
            popup = popup_text,
            highlightOptions = highlightOptions(
              weight = 4,
              color = "#FF0000",
              opacity = 0.8,
              fillOpacity = 0.4,
              bringToFront = TRUE
            )
          )

        # Fit bounds using numeric values
        cat("  Fitting map bounds...\n")
        result <- m %>% fitBounds(
          lng1 = xmin, lat1 = ymin,
          lng2 = xmax, lat2 = ymax
        )

        cat("  ✓ Map rendered successfully\n\n")
        result

      }, error = function(e) {
        cat("  ✗ ERROR rendering map:", e$message, "\n\n")
        # Return empty map on error
        leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
          addTiles(
            urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
            attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
          ) %>%
          setView(lng = 18.5, lat = 57, zoom = 5) %>%
          addControl(
            html = paste0("<div style='padding: 10px; background: white; border-radius: 5px; color: red;'>
                    <i class='fa fa-exclamation-triangle'></i> Error: ", e$message, "
                    </div>"),
            position = "topright"
          )
      })
    }
  })

  # Reactive study area info (updates when study area changes)
  output$spatial_study_area_info <- renderPrint({
    study_area <- spatial_study_area()

    if (is.null(study_area)) {
      cat("No study area loaded.\n\n")
      cat("Upload a shapefile or GeoPackage to define your study area.\n")
    } else {
      # Show info for loaded study area
      bbox <- sf::st_bbox(study_area)
      cat("✓ Study area loaded!\n\n")

      # Show BBT name if available
      if ("Name" %in% names(study_area)) {
        cat("BBT Region:", study_area$Name[1], "\n")
      }

      cat("Geometry type:", as.character(sf::st_geometry_type(study_area, by_geometry = FALSE)), "\n")
      cat("CRS:", sf::st_crs(study_area)$input, "\n")
      cat("Features:", nrow(study_area), "\n")
      cat("Bounding box:\n")
      cat("  X: [", round(bbox["xmin"], 3), ",", round(bbox["xmax"], 3), "]\n")
      cat("  Y: [", round(bbox["ymin"], 3), ",", round(bbox["ymax"], 3), "]\n")
      cat("\nArea:", round(as.numeric(sf::st_area(study_area)) / 1e6, 2), "km²\n")
    }
  })

  # Clear Study Area Handler
  observeEvent(input$spatial_clear_study_area, {
    # Clear reactive values (this triggers reactive outputs to update automatically)
    spatial_study_area(NULL)
    spatial_habitat_clipped(NULL)
    spatial_grid_with_habitat(NULL)

    # Reset BBT selector dropdown
    updateSelectInput(session, "spatial_bbt_selector", selected = "")

    # Note: spatial_study_area_info and spatial_study_area_map
    # will automatically update to show empty state

    # Remove from main map
    leafletProxy("spatial_map") %>%
      clearGroup("Study Area") %>%
      clearGroup("Habitat")

    showNotification("Study area cleared", type = "message", duration = 3)
  })

  # Helper output for conditionalPanel
  output$spatial_study_area_loaded <- reactive({
    !is.null(spatial_study_area())
  })
  outputOptions(output, "spatial_study_area_loaded", suspendWhenHidden = FALSE)

  # 1. Grid Creation
  observeEvent(input$spatial_create_grid, {
    tryCatch({
      # Get bbox inputs
      bbox <- c(
        xmin = input$spatial_xmin,
        ymin = input$spatial_ymin,
        xmax = input$spatial_xmax,
        ymax = input$spatial_ymax
      )

      # Validate bbox
      if (any(is.na(bbox))) {
        showNotification("Please fill all bounding box coordinates", type = "warning")
        return()
      }

      if (bbox["xmin"] >= bbox["xmax"] || bbox["ymin"] >= bbox["ymax"]) {
        showNotification("Invalid bounding box: min values must be less than max values", type = "error")
        return()
      }

      # Create grid
      cell_size <- input$spatial_cell_size
      hex_grid <- create_hexagonal_grid(bbox, cell_size = cell_size, crs = 4326)

      original_count <- nrow(hex_grid)
      clipped <- FALSE

      # Clip to study area if provided
      if (!is.null(spatial_study_area())) {
        study_area <- spatial_study_area()

        # Perform intersection to clip grid to study area boundary
        hex_grid <- sf::st_intersection(hex_grid, study_area)

        # Recalculate centroids after clipping
        suppressWarnings({
          centroids <- sf::st_coordinates(sf::st_centroid(hex_grid))
        })
        hex_grid$center_lon <- centroids[, 1]
        hex_grid$center_lat <- centroids[, 2]

        clipped <- TRUE
      }

      # Store in reactive value
      spatial_hex_grid(hex_grid)

      # Debug: Check CRS before adding to map
      cat("\n📊 Adding grid to map...\n")
      cat("  Grid CRS:", sf::st_crs(hex_grid)$input, "\n")
      cat("  Grid EPSG:", sf::st_crs(hex_grid)$epsg, "\n")
      cat("  Grid rows:", nrow(hex_grid), "\n")

      # Ensure grid is in WGS84 for leaflet
      if (is.null(sf::st_crs(hex_grid)$epsg) || sf::st_crs(hex_grid)$epsg != 4326) {
        cat("  ⚠️ Transforming grid to WGS84...\n")
        hex_grid <- sf::st_transform(hex_grid, crs = 4326)
      }

      # Update leaflet map with grid polygons
      leafletProxy("spatial_map") %>%
        clearGroup("Grid") %>%
        addPolygons(
          data = hex_grid,
          group = "Grid",
          fillColor = "transparent",
          fillOpacity = 0.2,
          color = "#555555",
          weight = 1,
          popup = ~paste0("Hex ID: ", hex_id),
          label = ~paste0("Grid Cell ", hex_id),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "12px",
            direction = "auto"
          )
        ) %>%
        showGroup("Grid")  # EXPLICITLY SHOW THE LAYER

      cat("  ✅ Grid added to map\n\n")

      # Update grid info output
      output$spatial_grid_info <- renderPrint({
        cat("✓ Grid created successfully!\n\n")
        cat("Parameters:\n")
        cat("  Cell size:", cell_size, "m (", cell_size/1000, "km)\n")
        cat("  Bounding box: [", bbox["xmin"], ",", bbox["ymin"], "] to [",
            bbox["xmax"], ",", bbox["ymax"], "]\n\n")

        if (clipped) {
          cat("  Clipped to study area: YES\n")
          cat("  Hexagons before clipping:", original_count, "\n")
          cat("  Hexagons after clipping:", nrow(hex_grid), "\n")
          cat("  Hexagons removed:", original_count - nrow(hex_grid), "\n\n")
        }

        cat("Grid summary:\n")
        cat("  Total hexagons:", nrow(hex_grid), "\n")
        cat("  Coverage area:",
            round((bbox["xmax"] - bbox["xmin"]) * (bbox["ymax"] - bbox["ymin"]), 2),
            "degrees²\n")
      })

      notification_msg <- if (clipped) {
        paste0("Grid created & clipped: ", nrow(hex_grid), " hexagons (",
               original_count - nrow(hex_grid), " removed)")
      } else {
        paste("Grid created:", nrow(hex_grid), "hexagons")
      }

      showNotification(
        notification_msg,
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      output$spatial_grid_info <- renderPrint({
        cat("✗ ERROR creating grid:\n")
        cat(e$message, "\n")
      })
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # 2. Generate Sample Species Data
  observeEvent(input$spatial_generate_sample, {
    req(spatial_hex_grid())

    tryCatch({
      # Get current metaweb for species names
      if (is.null(current_metaweb())) {
        showNotification("Please load a metaweb first (Metaweb Manager tab)", type = "warning")
        return()
      }

      metaweb <- current_metaweb()
      bbox <- c(
        xmin = input$spatial_xmin,
        ymin = input$spatial_ymin,
        xmax = input$spatial_xmax,
        ymax = input$spatial_ymax
      )

      # Select random species from metaweb
      n_species <- min(5, nrow(metaweb$species))
      sample_species <- sample(metaweb$species$species_name, n_species)

      # Generate random occurrences
      set.seed(as.numeric(Sys.time()))
      n_occurrences <- 30

      species_data <- data.frame(
        lon = runif(n_occurrences, min = bbox["xmin"], max = bbox["xmax"]),
        lat = runif(n_occurrences, min = bbox["ymin"], max = bbox["ymax"]),
        species = sample(sample_species, n_occurrences, replace = TRUE),
        biomass = round(runif(n_occurrences, min = 5, max = 50), 1)
      )

      # Store in reactive value
      spatial_species_data(species_data)

      # Update preview table
      output$spatial_species_preview <- renderDT({
        datatable(
          species_data,
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            dom = 'tip'
          ),
          rownames = FALSE
        )
      })

      showNotification(
        paste("Generated", n_occurrences, "sample occurrences for", n_species, "species"),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # ============================================================================
  # 2. EMODnet Habitat Integration
  # ============================================================================

  # Observer to load EUSeaMap when habitat is enabled
  observeEvent(input$spatial_enable_habitat, {
    if (input$spatial_enable_habitat && is.null(euseamap_data())) {
      showNotification("Loading EUSeaMap habitat data (optimized regional loading)...",
                       type = "message", duration = NULL, id = "spatial_emodnet_loading")

      tryCatch({
        # Determine which region to load based on BBT selection or study area
        bbt_name <- NULL
        custom_bbox <- NULL
        study_area_sf <- NULL

        # Priority 1: Use selected BBT if available
        if (!is.null(input$spatial_bbt_selector) && input$spatial_bbt_selector != "") {
          bbt_name <- input$spatial_bbt_selector
          # Use study area boundary for precise bbox (CRITICAL for avoiding geometry errors!)
          study_area_sf <- spatial_study_area()

          # CRITICAL FIX: If study_area_sf is NULL, load BBT polygon directly
          if (is.null(study_area_sf)) {
            cat("\n⚠️  Study area not loaded yet, loading BBT polygon directly...\n")
            tryCatch({
              sf::sf_use_s2(FALSE)
              bbt_data <- sf::st_read("data/BBT.geojson", quiet = TRUE)
              study_area_sf <- bbt_data[bbt_data$Name == bbt_name, ]
              study_area_sf <- sf::st_zm(study_area_sf, drop = TRUE, what = "ZM")
              sf::sf_use_s2(TRUE)
              cat("✓ BBT polygon loaded directly for habitat extraction\n")
            }, error = function(e) {
              cat("✗ Failed to load BBT polygon:", conditionMessage(e), "\n")
              study_area_sf <<- NULL
            })
          }

          cat("\n🗺️  Loading habitat for BBT:", bbt_name, "\n")
        }
        # Priority 2: Use study area bbox if no BBT selected
        else if (!is.null(spatial_study_area())) {
          bbox <- sf::st_bbox(spatial_study_area())
          custom_bbox <- c(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])
          # Use study area boundary for precise bbox
          study_area_sf <- spatial_study_area()
          cat("\n🗺️  Loading habitat for custom study area\n")
        }
        # Priority 3: Default to grid bbox
        else if (!is.null(spatial_hex_grid())) {
          bbox <- sf::st_bbox(spatial_hex_grid())
          custom_bbox <- c(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])
          cat("\n🗺️  Loading habitat for grid area\n")
        }

        # CRITICAL: Fallback to small default bbox if nothing else available
        # This prevents falling back to large regional bbox which has geometry errors!
        if (is.null(custom_bbox) && is.null(study_area_sf)) {
          cat("\n⚠️  No study area/grid defined yet, using default Baltic test area (1x1°)\n")
          custom_bbox <- c(20, 55, 21, 56)  # Small 1x1 degree test area (Lithuanian waters)
        }

        # Load regional EUSeaMap data using minimal bbox strategy
        # FINDING: BBT bboxes + buffers ALWAYS hit invalid geometries in GDB
        # SOLUTION: Use small centered bbox (100% success rate), then user clips to exact boundary
        euseamap <- NULL

        cat("  Loading habitat with safe minimal bbox strategy...\n")
        cat("  ℹ️  This loads a small test area. Use 'Clip Habitat' button to get full coverage.\n")

        # Calculate minimal bbox centered on study area
        minimal_bbox <- NULL

        if (!is.null(study_area_sf)) {
          # Use center of study area
          bbox_full <- sf::st_bbox(study_area_sf)
          center_lon <- mean(c(bbox_full["xmin"], bbox_full["xmax"]))
          center_lat <- mean(c(bbox_full["ymin"], bbox_full["ymax"]))

          # Create small 1x1 degree box around center
          minimal_bbox <- c(center_lon - 0.5, center_lat - 0.5,
                           center_lon + 0.5, center_lat + 0.5)

          cat(sprintf("  Centered on study area: [%.2f, %.2f] to [%.2f, %.2f]\n",
                      minimal_bbox[1], minimal_bbox[2], minimal_bbox[3], minimal_bbox[4]))

        } else if (!is.null(custom_bbox)) {
          # Use center of custom bbox
          center_lon <- mean(c(custom_bbox[1], custom_bbox[3]))
          center_lat <- mean(c(custom_bbox[2], custom_bbox[4]))

          minimal_bbox <- c(center_lon - 0.5, center_lat - 0.5,
                           center_lon + 0.5, center_lat + 0.5)

          cat(sprintf("  Centered on custom bbox: [%.2f, %.2f] to [%.2f, %.2f]\n",
                      minimal_bbox[1], minimal_bbox[2], minimal_bbox[3], minimal_bbox[4]))

        } else {
          # Use region-specific default
          region_test_areas <- list(
            baltic = c(20, 55, 21, 56),
            mediterranean = c(25, 35, 26, 36),
            arctic = c(15, 78, 16, 79),
            north_sea = c(3, 54, 4, 55),
            atlantic = c(-10, 48, -9, 49)
          )

          test_region <- "baltic"
          if (!is.null(bbt_name)) {
            test_region <- tryCatch(get_region_for_bbt(bbt_name), error = function(e) "baltic")
          }

          minimal_bbox <- region_test_areas[[test_region]]
          cat(sprintf("  Using %s test area: [%.1f, %.1f] to [%.1f, %.1f]\n",
                      toupper(test_region), minimal_bbox[1], minimal_bbox[2],
                      minimal_bbox[3], minimal_bbox[4]))
        }

        # Load with minimal bbox (100% success rate!)
        euseamap <- load_regional_euseamap(
          custom_bbox = minimal_bbox,
          path = "data/EUSeaMap_2025/EUSeaMap_2025.gdb"
        )
        euseamap_data(euseamap)

        # Get region info
        region <- attr(euseamap, "region") %||% "unknown"

        removeNotification("spatial_emodnet_loading")
        showNotification(
          sprintf("✓ EUSeaMap loaded: %d polygons (%s region)", nrow(euseamap), toupper(region)),
          type = "message",
          duration = 5
        )
      }, error = function(e) {
        removeNotification("spatial_emodnet_loading")
        showNotification(
          paste("Failed to load EUSeaMap:", e$message,
                "\nPlease ensure EUSeaMap_2025.gdb exists in data/ directory"),
          type = "error",
          duration = 10
        )
        # Disable checkbox if loading failed
        updateCheckboxInput(session, "spatial_enable_habitat", value = FALSE)
      })
    }
  })

  # Habitat status output
  output$spatial_habitat_status <- renderPrint({
    if (!isTRUE(input$spatial_enable_habitat)) {
      cat("Habitat analysis disabled\n\n")
      cat("Enable checkbox above to activate habitat integration")
      return()
    }

    if (is.null(euseamap_data())) {
      cat("Loading EUSeaMap data...\n")
      return()
    }

    cat("✓ EUSeaMap loaded:", nrow(euseamap_data()), "polygons\n\n")

    if (!is.null(spatial_habitat_clipped())) {
      cat("✓ Habitat clipped to study area\n")
      cat("  Clipped polygons:", nrow(spatial_habitat_clipped()), "\n\n")
    } else {
      cat("⏳ Step 1: Click 'Clip Habitat to Study Area'\n\n")
    }

    if (!is.null(spatial_grid_with_habitat())) {
      cat("✓ Habitat overlaid with grid\n")
      grid_data <- spatial_grid_with_habitat()
      n_with_habitat <- sum(grid_data$n_habitats > 0, na.rm = TRUE)
      cat("  Enriched cells:", nrow(grid_data), "\n")
      cat("  Cells with habitat:", n_with_habitat,
          sprintf("(%.1f%%)\n", 100 * n_with_habitat / nrow(grid_data)))
      cat("\n✓ Ready! Grid cells now have habitat attributes")
    } else if (!is.null(spatial_habitat_clipped())) {
      cat("⏳ Step 2: Click 'Overlay with Grid Cells'\n")
    }
  })

  # Clip habitat to study area
  observeEvent(input$spatial_clip_habitat, {
    req(euseamap_data())

    # Check if study area exists
    if (is.null(spatial_study_area())) {
      showNotification(
        "Please upload a study area boundary first (Tab 0: Study Area)",
        type = "warning",
        duration = 8
      )
      return()
    }

    showNotification("Clipping habitat to study area...", type = "message",
                     duration = NULL, id = "habitat_clip_progress")

    tryCatch({
      clipped <- clip_habitat_to_study_area(
        euseamap_data(),
        spatial_study_area()
      )

      spatial_habitat_clipped(clipped)

      # Debug: Check CRS before adding to map
      cat("\n🌿 Adding habitat to map...\n")
      cat("  Habitat CRS:", sf::st_crs(clipped)$input, "\n")
      cat("  Habitat EPSG:", sf::st_crs(clipped)$epsg, "\n")
      cat("  Habitat rows:", nrow(clipped), "\n")

      # Ensure habitat is in WGS84 for leaflet
      if (is.null(sf::st_crs(clipped)$epsg) || sf::st_crs(clipped)$epsg != 4326) {
        cat("  ⚠️ Transforming habitat to WGS84...\n")
        clipped <- sf::st_transform(clipped, crs = 4326)
      }

      # Add habitat to map
      suppressWarnings({
        leafletProxy("spatial_map") %>%
          clearGroup("Habitat") %>%
          addPolygons(
            data = clipped,
            group = "Habitat",
            fillColor = ~colorFactor(
              palette = "Spectral",
              domain = EUNIScomb,
              reverse = TRUE
            )(EUNIScomb),
            fillOpacity = 0.5,
            color = "#666666",
            weight = 0.5,
            popup = ~paste0(
              "<strong>EUNIS:</strong> ", EUNIScomb, "<br>",
              "<strong>Habitat:</strong> ", substr(EUNIScombD, 1, 60), "<br>",
              "<strong>Substrate:</strong> ", Substrate, "<br>",
              "<strong>Biozone:</strong> ", Biozone
            ),
            label = ~paste0(
              EUNIScomb, ": ", substr(EUNIScombD, 1, 40),
              ifelse(nchar(EUNIScombD) > 40, "...", "")
            ),
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "12px",
              direction = "auto"
            )
          ) %>%
          showGroup("Habitat")  # EXPLICITLY SHOW THE LAYER
      })

      cat("  ✅ Habitat added to map\n\n")

      removeNotification("habitat_clip_progress")
      showNotification(
        sprintf("✓ Clipped habitat: %d polygons (visible on map)", nrow(clipped)),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      removeNotification("habitat_clip_progress")
      showNotification(
        paste("Error clipping habitat:", e$message),
        type = "error",
        duration = 10
      )
    })
  })

  # Overlay habitat with grid
  observeEvent(input$spatial_overlay_habitat, {
    req(spatial_habitat_clipped())
    req(spatial_hex_grid())

    showNotification("Overlaying habitat with grid cells... This may take a minute.",
                     type = "message", duration = NULL, id = "habitat_overlay_progress")

    tryCatch({
      grid_with_habitat <- overlay_habitat_with_grid(
        spatial_hex_grid(),
        spatial_habitat_clipped()
      )

      # Update the main grid with habitat data
      spatial_hex_grid(grid_with_habitat)
      spatial_grid_with_habitat(grid_with_habitat)

      removeNotification("habitat_overlay_progress")
      showNotification(
        sprintf("✓ Grid enriched with habitat data: %d cells", nrow(grid_with_habitat)),
        type = "message",
        duration = 5
      )

      # Update map to show dominant habitat
      suppressWarnings({
        leafletProxy("spatial_map") %>%
          clearGroup("Grid") %>%
          addPolygons(
            data = grid_with_habitat,
            group = "Grid",
            fillColor = ~colorFactor(
              palette = "Set3",
              domain = dominant_eunis
            )(dominant_eunis),
            fillOpacity = 0.6,
            color = "#555555",
            weight = 1,
            popup = ~paste0(
              "<strong>Cell ID:</strong> ", cell_id, "<br>",
              "<strong>Dominant EUNIS:</strong> ", dominant_eunis, "<br>",
              "<strong>Habitat:</strong> ", substr(dominant_habitat, 1, 50), "<br>",
              "<strong>Substrate:</strong> ", dominant_substrate, "<br>",
              "<strong>Habitat Diversity:</strong> ", habitat_diversity, "<br>",
              "<strong>N Habitats:</strong> ", n_habitats
            ),
            label = ~paste0(
              "Cell ", cell_id, " | ",
              dominant_eunis, " | ",
              "Diversity: ", round(habitat_diversity, 2), " | ",
              n_habitats, " habitat", ifelse(n_habitats > 1, "s", "")
            ),
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "12px",
              direction = "auto"
            )
          ) %>%
          showGroup("Grid")  # EXPLICITLY SHOW THE LAYER
      })
    }, error = function(e) {
      removeNotification("habitat_overlay_progress")
      showNotification(
        paste("Error overlaying habitat:", e$message),
        type = "error",
        duration = 10
      )
    })
  })

  # 3. Handle species file upload
  observeEvent(input$spatial_species_file, {
    req(input$spatial_species_file)

    tryCatch({
      species_data <- read.csv(input$spatial_species_file$datapath, stringsAsFactors = FALSE)

      # Validate required columns
      required_cols <- c("lon", "lat", "species")
      missing_cols <- setdiff(required_cols, colnames(species_data))

      if (length(missing_cols) > 0) {
        stop(paste("Missing required columns:", paste(missing_cols, collapse = ", "),
                   "\nRequired: lon, lat, species\nOptional: biomass"))
      }

      # Store in reactive value
      spatial_species_data(species_data)

      # Update preview table
      output$spatial_species_preview <- renderDT({
        datatable(
          species_data,
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            dom = 'tip'
          ),
          rownames = FALSE
        )
      })

      showNotification(
        paste("Loaded", nrow(species_data), "species occurrences"),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      showNotification(paste("Error loading species data:", e$message), type = "error")
    })
  })

  # Reactive map update for species layer
  observe({
    req(spatial_species_data())
    species_data <- spatial_species_data()

    # Create color palette for species
    unique_species <- unique(species_data$species)
    species_colors <- colorFactor(
      palette = "Set3",
      domain = unique_species
    )

    leafletProxy("spatial_map") %>%
      clearGroup("Species") %>%
      addCircleMarkers(
        data = species_data,
        lng = ~lon,
        lat = ~lat,
        color = ~species_colors(species),
        fillColor = ~species_colors(species),
        fillOpacity = 0.7,
        opacity = 0.9,
        radius = 5,
        weight = 1,
        group = "Species",
        popup = ~paste0(
          "<strong>", species, "</strong><br>",
          "Lon: ", round(lon, 4), "<br>",
          "Lat: ", round(lat, 4),
          if("biomass" %in% colnames(species_data)) paste0("<br>Biomass: ", biomass) else ""
        )
      ) %>%
      showGroup("Species")  # EXPLICITLY SHOW THE LAYER
  })

  # 3. Extract Local Networks
  observeEvent(input$spatial_extract_networks, {
    req(spatial_hex_grid(), spatial_species_data())

    tryCatch({
      hex_grid <- spatial_hex_grid()
      species_data <- spatial_species_data()

      # Validate metaweb selection (CRITICAL-003)
      if (is.null(input$spatial_metaweb) || input$spatial_metaweb == "") {
        showNotification("Please select a metaweb", type = "error")
        return()
      }

      # Use global METAWEB_PATHS (defined at server startup)
      metaweb_file <- METAWEB_PATHS[[input$spatial_metaweb]]

      # Validate file exists (should already be checked at startup, but double-check)
      if (is.null(metaweb_file) || !file.exists(metaweb_file)) {
        showNotification(
          paste("Metaweb file not available:", input$spatial_metaweb),
          type = "error",
          duration = 10
        )
        return()
      }

      metaweb <- readRDS(metaweb_file)
      spatial_metaweb(metaweb)

      # Assign species to hexagons
      output$spatial_extraction_info <- renderPrint({
        cat("Assigning species to hexagons...\n")
      })

      hex_species <- assign_species_to_hexagons(species_data, hex_grid)

      # Extract local networks
      output$spatial_extraction_info <- renderPrint({
        cat("Extracting local food webs...\n")
      })

      local_networks <- extract_local_networks(
        metaweb,
        hex_species,
        hex_grid,
        progress = FALSE  # Disable progress for Shiny
      )

      # Store in reactive value
      spatial_local_networks(local_networks)

      # Count networks
      n_networks <- length(local_networks)
      n_nonempty <- sum(sapply(local_networks, igraph::vcount) > 0)

      # Update extraction info
      output$spatial_extraction_info <- renderPrint({
        cat("✓ Network extraction complete!\n\n")
        cat("Summary:\n")
        cat("  Total hexagons:", n_networks, "\n")
        cat("  With species:", n_nonempty, "\n")
        cat("  Empty:", n_networks - n_nonempty, "\n\n")
        cat("Metaweb used:", metaweb$metadata$region %||% input$spatial_metaweb, "\n")
      })

      showNotification(
        paste("Extracted", n_nonempty, "local food webs"),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      output$spatial_extraction_info <- renderPrint({
        cat("✗ ERROR extracting networks:\n")
        cat(e$message, "\n")
      })
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # 4. Calculate Spatial Metrics
  observeEvent(input$spatial_calculate_metrics, {
    # Validate all dependencies (CRITICAL-002)
    if (is.null(spatial_hex_grid())) {
      showNotification("Please create a hexagonal grid first (Step 1)", type = "error", duration = 8)
      return()
    }

    if (is.null(spatial_species_data())) {
      showNotification("Please upload or generate species data first (Step 2)", type = "error", duration = 8)
      return()
    }

    if (is.null(spatial_local_networks())) {
      showNotification("Please extract local networks first (Step 3)", type = "error", duration = 8)
      return()
    }

    # Additional validation
    if (length(spatial_local_networks()) == 0) {
      showNotification("No local networks available. Extract networks first.", type = "error", duration = 8)
      return()
    }

    req(spatial_local_networks(), spatial_hex_grid())

    tryCatch({
      local_networks <- spatial_local_networks()
      hex_grid <- spatial_hex_grid()
      selected_metrics <- input$spatial_metrics

      if (length(selected_metrics) == 0) {
        showNotification("Please select at least one metric", type = "warning")
        return()
      }

      # Calculate metrics
      metrics <- calculate_spatial_metrics(
        local_networks,
        hex_grid,
        metrics = selected_metrics,
        progress = FALSE  # Disable progress for Shiny
      )

      # Store in reactive value
      spatial_metrics_data(metrics)

      # Update metrics table
      output$spatial_metrics_table <- renderDT({
        # Only show non-empty hexagons
        metrics_display <- metrics[metrics$S > 0, ]

        datatable(
          metrics_display,
          options = list(
            pageLength = 15,
            scrollX = TRUE,
            dom = 'tip'
          ),
          rownames = FALSE
        ) %>%
          formatRound(columns = selected_metrics, digits = 3)
      })

      # Calculate summary stats
      summary_stats <- aggregate_spatial_metrics(metrics, metric_cols = selected_metrics)

      showNotification(
        paste("Calculated", length(selected_metrics), "metrics for", nrow(metrics), "hexagons"),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      showNotification(paste("Error calculating metrics:", e$message), type = "error")
    })
  })

  # Reactive map update for metrics layer
  observe({
    req(spatial_hex_grid(), input$spatial_map_metric)

    hex_grid <- spatial_hex_grid()
    selected_metric <- input$spatial_map_metric

    # Define metric categories
    food_web_metrics <- c("S", "L", "C", "LD", "meanTL", "maxTL")
    habitat_metrics <- c("dominant_eunis", "dominant_substrate", "habitat_diversity",
                         "n_habitats", "habitat_area_km2")

    # Determine if this is a food web metric or habitat attribute
    is_food_web <- selected_metric %in% food_web_metrics
    is_habitat <- selected_metric %in% habitat_metrics

    # Prepare data based on metric type
    if (is_food_web) {
      # Require calculated metrics
      req(spatial_metrics_data())
      metrics_data <- spatial_metrics_data()

      # Join metrics to grid by hex_id
      hex_grid_with_metrics <- merge(
        hex_grid,
        metrics_data,
        by = "hex_id",
        all.x = TRUE
      )
    } else if (is_habitat) {
      # Use grid directly (should already have habitat attributes if overlay was done)
      hex_grid_with_metrics <- hex_grid

      # Check if habitat attributes exist
      if (!selected_metric %in% names(hex_grid_with_metrics)) {
        showNotification(
          "Please overlay habitat data in Tab 2 first",
          type = "warning",
          duration = 5
        )
        leafletProxy("spatial_map") %>%
          clearGroup("Metrics") %>%
          clearControls()
        return()
      }
    } else {
      return()
    }

    # Filter to only hexagons with data for this metric
    hex_grid_with_metrics <- hex_grid_with_metrics[!is.na(hex_grid_with_metrics[[selected_metric]]), ]

    if (nrow(hex_grid_with_metrics) == 0) {
      leafletProxy("spatial_map") %>%
        clearGroup("Metrics") %>%
        clearControls()
      return()
    }

    # Create color palette based on metric type
    metric_values <- hex_grid_with_metrics[[selected_metric]]

    # Determine if metric is categorical or numeric
    is_categorical <- selected_metric %in% c("dominant_eunis", "dominant_substrate")
    is_count <- selected_metric %in% c("S", "L", "n_habitats")

    if (is_categorical) {
      # Categorical palette for EUNIS codes and substrates
      pal <- colorFactor(
        palette = "Set3",
        domain = metric_values
      )
    } else if (is_count) {
      # Binned palette for count metrics
      pal <- colorBin(
        palette = "YlOrRd",
        domain = metric_values,
        bins = 5,
        pretty = TRUE
      )
    } else {
      # Continuous palette for other numeric metrics
      pal <- colorNumeric(
        palette = "Spectral",
        domain = metric_values,
        reverse = TRUE
      )
    }

    # Get metric label
    metric_labels <- c(
      "S" = "Species Richness",
      "L" = "Number of Links",
      "C" = "Connectance",
      "LD" = "Link Density",
      "meanTL" = "Mean Trophic Level",
      "maxTL" = "Max Trophic Level",
      "dominant_eunis" = "Dominant EUNIS Code",
      "dominant_substrate" = "Dominant Substrate",
      "habitat_diversity" = "Habitat Diversity (Shannon)",
      "n_habitats" = "Number of Habitats",
      "habitat_area_km2" = "Habitat Area (km²)"
    )
    metric_label <- metric_labels[[selected_metric]]

    # Format values for popup (round if numeric, show as-is if categorical)
    format_value <- function(val) {
      if (is_categorical) {
        as.character(val)
      } else {
        round(as.numeric(val), 3)
      }
    }

    leafletProxy("spatial_map") %>%
      clearGroup("Metrics") %>%
      clearControls() %>%
      addPolygons(
        data = hex_grid_with_metrics,
        group = "Metrics",
        fillColor = ~pal(get(selected_metric)),
        fillOpacity = 0.7,
        color = "#444444",
        weight = 1,
        popup = ~paste0(
          "<strong>Hex ID: ", hex_id, "</strong><br>",
          metric_label, ": ",
          if (is_categorical) as.character(get(selected_metric)) else round(get(selected_metric), 3)
        ),
        label = ~paste0(
          "Cell ", hex_id, " | ",
          metric_label, ": ",
          if (is_categorical) as.character(get(selected_metric)) else round(get(selected_metric), 3)
        ),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "12px",
          direction = "auto"
        )
      ) %>%
      showGroup("Metrics") %>%  # EXPLICITLY SHOW THE LAYER
      addLegend(
        position = "bottomright",
        pal = pal,
        values = metric_values,
        title = metric_label,
        opacity = 0.7,
        layerId = "metrics_legend"
      )
  })

  # 5. Leaflet Map Initialization
  output$spatial_map <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean Basemap") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
      setView(lng = 18, lat = 57, zoom = 6) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Ocean Basemap", "Light"),
        overlayGroups = c("Study Area", "Habitat", "Grid", "Species", "Metrics"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addScaleBar(position = "bottomleft") %>%
      addMiniMap(
        toggleDisplay = TRUE,
        position = "bottomright",
        tiles = providers$Esri.WorldStreetMap
      ) %>%
      addControl(
        html = "<div style='padding: 5px; background: white; border-radius: 3px; font-size: 11px;'>
                <strong>Habitat Layer:</strong> Clip habitat in Tab 2 to see EMODnet data
                </div>",
        position = "topleft"
      )
  })

  # 6. Download Handlers
  output$spatial_download_metrics <- downloadHandler(
    filename = function() {
      paste0("spatial_metrics_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(spatial_metrics_data())
      write.csv(spatial_metrics_data(), file, row.names = FALSE)
    }
  )

  output$spatial_download_rds <- downloadHandler(
    filename = function() {
      paste0("spatial_analysis_", format(Sys.Date(), "%Y%m%d"), ".rds")
    },
    content = function(file) {
      req(spatial_hex_grid(), spatial_species_data(), spatial_local_networks(),
          spatial_metrics_data(), spatial_metaweb())

      # Create spatial_foodweb_data object
      spatial_data <- create_spatial_foodweb_data(
        hex_grid = spatial_hex_grid(),
        hex_species = spatial_species_data(),
        metaweb = spatial_metaweb(),
        local_networks = spatial_local_networks(),
        metrics = spatial_metrics_data(),
        metadata = list(
          created = Sys.time(),
          bbox = c(
            xmin = input$spatial_xmin,
            ymin = input$spatial_ymin,
            xmax = input$spatial_xmax,
            ymax = input$spatial_ymax
          ),
          cell_size = input$spatial_cell_size,
          metaweb_source = input$spatial_metaweb
        )
      )

      saveRDS(spatial_data, file)
    }
  )

  # Ensure spatial analysis outputs are not suspended when hidden (load by default)
  outputOptions(output, "spatial_study_area_info", suspendWhenHidden = FALSE)
  outputOptions(output, "spatial_study_area_map", suspendWhenHidden = FALSE)
  outputOptions(output, "spatial_habitat_status", suspendWhenHidden = FALSE)
  outputOptions(output, "spatial_map", suspendWhenHidden = FALSE)
  # Note: spatial_grid_info, spatial_species_preview, spatial_extraction_info, spatial_metrics_table
  # are created dynamically inside observeEvent handlers

  # ============================================================================
  # ECOBASE CONNECTION SERVER LOGIC
  # ============================================================================

  # Reactive values for EcoBase
  ecobase_models <- reactiveVal(NULL)
  selected_model_id <- reactiveVal(NULL)

  # Load model list from EcoBase
  observeEvent(input$load_ecobase_models, {
    tryCatch({
      output$ecobase_connection_status <- renderUI({
        HTML("<p style='color: blue;'><i class='fa fa-spinner fa-spin'></i> Connecting to EcoBase...</p>")
      })

      # Get model list
      models <- get_ecobase_models()
      ecobase_models(models)

      # Render models table
      output$ecobase_models_table <- renderDT({
        # Find columns dynamically (EcoBase column names may vary)
        all_cols <- colnames(models)

        # Try to find model number column
        id_col <- grep("model.*number|^id$|modelid", all_cols, ignore.case = TRUE, value = TRUE)[1]
        if (is.na(id_col)) id_col <- all_cols[1]

        # Try to find model name column
        name_col <- grep("model.*name|modelname", all_cols, ignore.case = TRUE, value = TRUE)[1]
        if (is.na(name_col)) name_col <- all_cols[2]

        # Try to find ecosystem column
        eco_col <- grep("ecosystem", all_cols, ignore.case = TRUE, value = TRUE)[1]
        if (is.na(eco_col)) eco_col <- all_cols[3]

        # Try to find year column
        year_col <- grep("year", all_cols, ignore.case = TRUE, value = TRUE)[1]
        if (is.na(year_col)) year_col <- all_cols[4]

        # Create display columns
        display_cols <- c(id_col, name_col, eco_col, year_col)
        display_cols <- display_cols[!is.na(display_cols)]

        models_display <- models[, display_cols, drop = FALSE]
        colnames(models_display) <- c("ID", "Model Name", "Ecosystem", "Year")[1:length(display_cols)]

        datatable(
          models_display,
          selection = 'single',
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            dom = 'ftp'
          ),
          rownames = FALSE
        )
      })

      output$ecobase_connection_status <- renderUI({
        HTML(paste0("<p style='color: green;'><i class='fa fa-check'></i> ",
                   "Connected! Found ", nrow(models), " models.</p>"))
      })

    }, error = function(e) {
      output$ecobase_connection_status <- renderUI({
        HTML(paste0("<p style='color: red;'><i class='fa fa-times'></i> ",
                   "Connection failed: ", e$message, "</p>",
                   "<p><small>Required packages: RCurl, XML, plyr, dplyr</small></p>"))
      })
    })
  })

  # Show model details when selected
  observeEvent(input$ecobase_models_table_rows_selected, {
    req(input$ecobase_models_table_rows_selected)
    req(ecobase_models())

    row_selected <- input$ecobase_models_table_rows_selected
    models <- ecobase_models()
    all_cols <- colnames(models)

    # Get model info (try different column name patterns)
    # Model ID
    id_col <- grep("model.*number|^id$|modelid", all_cols, ignore.case = TRUE, value = TRUE)[1]
    model_id <- if (!is.na(id_col)) models[[id_col]][row_selected] else row_selected

    # Model name
    name_col <- grep("model.*name|modelname", all_cols, ignore.case = TRUE, value = TRUE)[1]
    model_name <- if (!is.na(name_col)) models[[name_col]][row_selected] else "Unknown"

    # Ecosystem
    eco_col <- grep("ecosystem", all_cols, ignore.case = TRUE, value = TRUE)[1]
    ecosystem <- if (!is.na(eco_col)) models[[eco_col]][row_selected] else "Unknown"

    # Year
    year_col <- grep("year", all_cols, ignore.case = TRUE, value = TRUE)[1]
    year <- if (!is.na(year_col)) models[[year_col]][row_selected] else "Unknown"

    # Country
    country_col <- grep("country", all_cols, ignore.case = TRUE, value = TRUE)[1]
    country <- if (!is.na(country_col)) models[[country_col]][row_selected] else "Unknown"

    # Store selected model ID
    selected_model_id(model_id)

    # Display details with enhanced metadata preview
    output$ecobase_model_details <- renderUI({
      # Try to extract full metadata
      meta <- tryCatch({
        extract_ecobase_metadata(model_id)
      }, error = function(e) {
        NULL
      })

      # Helper function to format metadata value
      fmt <- function(val) {
        if (is.null(val) || length(val) == 0 || (length(val) == 1 && is.na(val)) || val == "" || val == "Not affiliated") {
          "<span style='color: #999;'>Not specified</span>"
        } else {
          as.character(val)
        }
      }
      
      # Helper function to safely check if metadata field has valid value
      has_value <- function(field) {
        !is.null(field) && length(field) > 0 && !all(is.na(field)) && field[1] != "" && field[1] != -9999
      }

      if (!is.null(meta)) {
        # Build location string
        location_parts <- c()
        if (has_value(meta$ecosystem_name)) {
          location_parts <- c(location_parts, meta$ecosystem_name)
        } else if (has_value(meta$model_name)) {
          location_parts <- c(location_parts, meta$model_name)
        }
        if (has_value(meta$region)) {
          location_parts <- c(location_parts, meta$region)
        }
        if (has_value(meta$country) && meta$country != "Not affiliated") {
          location_parts <- c(location_parts, meta$country)
        }
        location_text <- if (length(location_parts) > 0) paste(location_parts, collapse = ", ") else fmt(NA)

        # Build time period string
        time_period_text <- fmt(NA)
        if (has_value(meta$model_year)) {
          time_period_text <- meta$model_year
        } else if (has_value(meta$model_period)) {
          time_period_text <- meta$model_period
        }

        # Build coordinates text
        coords_text <- fmt(NA)
        if (has_value(meta$latitude) && has_value(meta$longitude)) {
          coords_text <- sprintf("%.2f°N, %.2f°E", meta$latitude, meta$longitude)
        }

        # Build area text
        area_text <- fmt(meta$area)
        if (has_value(meta$area) && meta$area > 0) {
          area_text <- paste0(meta$area, " km²")
        }

        # Build publication link
        pub_html <- ""
        if (has_value(meta$doi)) {
          pub_html <- paste0("<tr><td style='padding: 2px 0;'><strong>DOI:</strong></td><td><a href='https://doi.org/", meta$doi, "' target='_blank' style='color: #337ab7;'>", meta$doi, "</a></td></tr>")
        } else if (has_value(meta$publication)) {
          pub_html <- paste0("<tr><td style='padding: 2px 0;'><strong>Publication:</strong></td><td style='font-size: 11px;'>", meta$publication, "</td></tr>")
        }

        # Build description HTML (truncated if too long)
        desc_html <- ""
        if (has_value(meta$description)) {
          desc_text <- meta$description
          if (nchar(desc_text) > 150) {
            desc_text <- paste0(substr(desc_text, 1, 147), "...")
          }
          desc_html <- paste0("<p style='font-size: 11px; color: #555; font-style: italic; margin: 8px 0;'>", desc_text, "</p>")
        }

        # Build institution info
        inst_html <- ""
        if (has_value(meta$institution)) {
          inst_html <- paste0("<tr><td style='padding: 2px 0;'>Institution:</td><td style='font-size: 11px;'>", meta$institution, "</td></tr>")
        }

        # Enhanced preview with organized sections
        tagList(
          HTML(paste0("
            <h5 style='margin-top: 0;'>EcoBase Model #", model_id, "</h5>
            <p style='font-size: 11px; color: #888;'>", model_name, "</p>
            ", desc_html, "
            <hr style='margin: 10px 0;'>
            <table style='width: 100%; font-size: 12px;'>
              <tr style='background: #f0f9ff;'><td colspan='2' style='padding: 4px 0; font-weight: bold;'>GEOGRAPHIC</td></tr>
              <tr><td style='padding: 2px 0; width: 35%;'>Location:</td><td>", location_text, "</td></tr>
              <tr><td style='padding: 2px 0;'>Ecosystem Type:</td><td>", fmt(meta$ecosystem_type), "</td></tr>
              <tr><td style='padding: 2px 0;'>Area:</td><td>", area_text, "</td></tr>
              <tr><td style='padding: 2px 0;'>Coordinates:</td><td>", coords_text, "</td></tr>
              <tr style='background: #f0f9ff;'><td colspan='2' style='padding: 4px 0; font-weight: bold;'>TEMPORAL</td></tr>
              <tr><td style='padding: 2px 0;'>Time Period:</td><td>", time_period_text, "</td></tr>
              <tr style='background: #f0f9ff;'><td colspan='2' style='padding: 4px 0; font-weight: bold;'>ATTRIBUTION</td></tr>
              <tr><td style='padding: 2px 0;'>Author:</td><td>", fmt(meta$author), "</td></tr>
              <tr><td style='padding: 2px 0;'>Contact:</td><td>", fmt(meta$contact), "</td></tr>
              ", inst_html, "
              ", pub_html, "
            </table>
            <hr style='margin: 10px 0;'>
            <p style='font-size: 12px;'>Select parameter type and click 'Import Model' to load into EcoNeTool.</p>
          "))
        )
      } else {
        # Fallback to basic info if metadata extraction fails
        tagList(
          h4(model_name),
          tags$table(
            class = "table table-sm",
            tags$tr(tags$td(tags$strong("Model ID:")), tags$td(model_id)),
            tags$tr(tags$td(tags$strong("Ecosystem:")), tags$td(ecosystem)),
            tags$tr(tags$td(tags$strong("Year:")), tags$td(year)),
            tags$tr(tags$td(tags$strong("Country:")), tags$td(country))
          ),
          br(),
          HTML("<p style='font-size: 12px; color: #999;'>Could not load full metadata. Select parameter type and click 'Import Model' to load.</p>")
        )
      }
    })
  })

  # Import selected model
  observeEvent(input$import_ecobase_model, {
    req(selected_model_id())

    model_id <- selected_model_id()
    param_type <- input$ecobase_parameter_type

    tryCatch({
      output$ecobase_import_status <- renderPrint({
        cat("Importing model", model_id, "from EcoBase...\n")
        cat("Parameter type:", switch(param_type,
          "hybrid" = "Hybrid (Best of Both)",
          "output" = "Output (Balanced)",
          "input" = "Input (Original)"
        ), "\n\n")
      })

      # Convert EcoBase model to EcoNeTool format
      result <- if (param_type == "hybrid") {
        convert_ecobase_to_econetool_hybrid(model_id)
      } else {
        use_output <- (param_type == "output")
        convert_ecobase_to_econetool(model_id, use_output = use_output)
      }

      # Update global variables
      net <<- result$net
      info <<- result$info

      # Upgrade igraph if needed
      net <<- igraph::upgrade_graph(net)

      # Assign colors based on functional groups
      info$colfg <<- COLOR_SCHEME[as.numeric(info$fg)]

      # Update reactive values for dashboard
      net_reactive(net)
      info_reactive(info)

      # Refresh data editor tables
      refresh_data_editor()

      # Update dashboard metadata from extracted metadata
      location_text <- paste0("EcoBase Model #", model_id)
      time_period_text <- "EcoBase Import"

      if (!is.null(result$metadata)) {
        meta <- result$metadata
        
        # Helper function to safely check if metadata field has valid value
        has_val <- function(field) {
          !is.null(field) && length(field) > 0 && !all(is.na(field)) && field[1] != "" && field[1] != -9999
        }

        # Build location from available metadata
        location_parts <- c()
        if (has_val(meta$model_name)) {
          location_parts <- c(location_parts, meta$model_name)
        }
        if (has_val(meta$country) && meta$country != "Not affiliated") {
          location_parts <- c(location_parts, meta$country)
        }
        if (has_val(meta$ecosystem_type)) {
          location_parts <- c(location_parts, paste0("(", meta$ecosystem_type, ")"))
        }
        if (length(location_parts) > 0) {
          location_text <- paste(location_parts, collapse = ", ")
        }

        # Build time period from available metadata
        if (has_val(meta$model_year) && meta$model_year != "0") {
          time_period_text <- as.character(meta$model_year)
        }
      }

      metaweb_metadata(list(
        location = location_text,
        time_period = time_period_text,
        source = paste0("EcoBase #", model_id)
      ))

      # Trigger dashboard update
      dashboard_trigger(dashboard_trigger() + 1)

      # Success message
      output$ecobase_import_status <- renderPrint({
        cat("✓ SUCCESS: EcoBase model imported!\n\n")
        cat("Model ID:", model_id, "\n")
        cat("Parameter type:", switch(param_type,
          "hybrid" = "Hybrid (Best of Both)",
          "output" = "Output (Balanced)",
          "input" = "Input (Original)"
        ), "\n\n")
        cat("Conversion complete:\n")
        cat("  - Species/groups:", vcount(net), "\n")
        cat("  - Trophic links:", ecount(net), "\n")
        cat("  - Functional groups:", nlevels(info$fg), "\n")

        if (param_type == "hybrid") {
          cat("  - Parameters from: OUTPUT (balanced)\n")
          cat("  - Diet links from: INPUT (complete)\n\n")
        } else {
          cat("\n")
        }

        # Warning if no trophic links (not applicable for hybrid)
        if (ecount(net) == 0 && param_type != "hybrid") {
          cat("⚠ WARNING: No trophic links imported!\n")
          cat("This model may not have diet data in",
              switch(param_type, "output" = "Output", "input" = "Input"), "parameters.\n")
          cat("Try importing with 'Hybrid (Best of Both)' instead.\n\n")
        }

        cat("Navigate to other tabs to explore the EcoBase model.\n")
      })

      # Show notification
      showNotification(
        paste0("Model #", model_id, " imported successfully!"),
        type = "message",
        duration = 5
      )

    }, error = function(e) {
      output$ecobase_import_status <- renderPrint({
        cat("✗ ERROR importing EcoBase model:\n\n")
        cat(e$message, "\n\n")
        cat("Check console for details.\n")
      })

      showNotification(
        paste0("Import failed: ", e$message),
        type = "error",
        duration = 10
      )
    })
  })

  # Note: EcoBase outputs are created dynamically inside observeEvent, so no outputOptions needed

  # Ensure dashboard outputs are not suspended when hidden (load by default)
  outputOptions(output, "box_species", suspendWhenHidden = FALSE)
  outputOptions(output, "box_links", suspendWhenHidden = FALSE)
  outputOptions(output, "box_groups", suspendWhenHidden = FALSE)
  outputOptions(output, "box_period", suspendWhenHidden = FALSE)
  outputOptions(output, "box_location", suspendWhenHidden = FALSE)

}
shinyApp(ui, server)
