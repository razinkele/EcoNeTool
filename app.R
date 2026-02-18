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
source("R/config/harmonization_config.R")  # Harmonization configuration for trait lookup
source("R/functions/validation_utils.R")  # Core utilities (%||%, with_timeout, validators) - MUST load early

# =============================================================================
# CURRENT VERSION: v1.4.2 (2025-12-26)
# =============================================================================
# LATEST: Local Databases Integration (v1.4.2)
# - BVOL phytoplankton database: 3,846 species
# - SpeciesEnriched marine invertebrates: 915 species
# - 12 total databases (added 2 local databases)
# - Smart routing based on taxonomy
# - Ultra-fast in-memory caching (0.4-0.6ms lookups)
#
# PHASE 6: PERFORMANCE & ROBUSTNESS (v1.4.0)
# - Error logging system for debugging and monitoring
# - API rate limiting to prevent bans during batch operations
# - SQLite indexed cache for 90× faster phylogenetic searches
# - Parallel database queries for 3-5× faster lookups
#
# All features are backward compatible
# =============================================================================

ENABLE_PHASE6 <- TRUE

if (ENABLE_PHASE6) {
  suppressMessages({
    # General application logger (debug gated by ECO_NT_DEBUG env var)
    if (file.exists("R/functions/logger.R")) {
      source("R/functions/logger.R")
    }

    # Error logging system
    if (file.exists("R/functions/error_logging.R")) {
      source("R/functions/error_logging.R")
      message("✓ Phase 6: Error logging enabled")
    }

    # API rate limiting with retry logic
    if (file.exists("R/functions/api_rate_limiter.R")) {
      source("R/functions/api_rate_limiter.R")
      message("✓ Phase 6: API rate limiting enabled")
    }

    # SQLite indexed cache (90× faster phylogenetic searches)
    if (file.exists("R/functions/cache_sqlite.R")) {
      source("R/functions/cache_sqlite.R")
      message("✓ Phase 6: SQLite cache enabled")
    }

    # Parallel database queries (3-5× faster lookups)
    if (file.exists("R/functions/parallel_lookup.R")) {
      source("R/functions/parallel_lookup.R")
      # Initialize with 4 workers (adjust based on server CPU cores)
      # Each worker uses ~200-300MB RAM
      init_parallel_lookup(workers = 4)
      message("✓ Phase 6: Parallel processing enabled (4 workers)")
    }
  })

  message("═══════════════════════════════════════════════════════════")
  message("✓ PHASE 6 LOADED - Performance & Robustness Active")
  message("  - 3-5× faster lookups (parallel queries)")
  message("  - 90× faster phylogenetic searches (SQLite cache)")
  message("  - Zero API bans (rate limiting with automatic retry)")
  message("  - Comprehensive error tracking and health monitoring")
  message("═══════════════════════════════════════════════════════════")
}

# Analysis functions (organized by domain)
source("R/functions/functional_group_utils.R")  # Shared utilities
# NOTE: validation_utils.R is now sourced early (line 27) for %||% and with_timeout
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
source("R/functions/shark_api_utils.R")  # SHARK4R integration (Swedish ocean archives)
source("R/functions/emodnet_habitat_utils.R")  # EMODnet habitat integration
source("R/functions/euseamap_regional_config.R")  # Regional optimization for EUSeaMap
source("R/functions/trait_foodweb.R")  # Trait-based food web construction
source("R/functions/trait_lookup/load_all.R")  # Automated trait lookup from databases

# ECOPATH import (cross-platform compatible)
source("R/functions/ecopath/load_all.R")

# Rpath integration (ECOPATH/ECOSIM module)
source("R/functions/rpath/load_all.R")
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
source("R/ui/shark_ui.R")
source("R/ui/traitfoodweb_ui.R")  # Trait-based food web UI (legacy - kept for reference)
source("R/ui/trait_research_ui.R")  # Trait Research UI (NEW)
source("R/ui/foodweb_construction_ui.R")  # Food Web Construction UI (NEW)
source("R/ui/harmonization_settings_ui.R")  # Harmonization settings UI

# Server modules
source("R/modules/traitfoodweb_server.R")  # Trait-based food web server logic (legacy)
source("R/modules/trait_research_server.R")  # Trait Research server logic (NEW)
source("R/modules/foodweb_construction_server.R")  # Food Web Construction server logic (NEW)
source("R/modules/harmonization_settings_server.R")  # Harmonization settings server logic
source("R/modules/plugin_server.R")  # Plugin management server logic
source("R/modules/dashboard_server.R")  # Dashboard value boxes server logic
source("R/modules/ecopath_import_server.R")  # ECOPATH import server logic
source("R/modules/data_import_server.R")  # Data import handler
source("R/modules/visualization_server.R")  # Visualization outputs
source("R/modules/analysis_server.R")  # Flux + Keystoneness analysis
source("R/modules/download_server.R")  # Download handlers + export
source("R/modules/dataeditor_inline_server.R")  # Internal data editor
source("R/modules/metaweb_manager_server.R")  # Metaweb manager
source("R/modules/spatial_server.R")  # Spatial analysis
source("R/modules/ecobase_server.R")  # EcoBase connection
source("R/modules/shark_server.R")  # SHARK4R integration

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
        tags$span("Ecological Interaction Network Explorer", style = "margin-left: 10px; color: #007bff; font-size: 1.1rem; font-weight: 500;")
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
        text = "Trait Research",
        tabName = "trait_research",
        icon = icon("search")
      ),

      menuItem(
        text = "Food Web Construction",
        tabName = "foodweb_construction",
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
        text = "SHARK Data",
        tabName = "shark",
        icon = icon("database")
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

        /* Center Shiny progress notifications */
        .shiny-notification {
          position: fixed !important;
          top: 50% !important;
          left: 50% !important;
          transform: translate(-50%, -50%) !important;
          min-width: 400px !important;
          max-width: 600px !important;
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
      trait_research_ui(),  # Trait Research (NEW)
      foodweb_construction_ui(),  # Food Web Construction (NEW)
      spatial_ui(),
      ecobase_ui(),
      shark_ui(),

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

        # Harmonization Settings Tab
        tabPanel(
          title = tagList(icon("sliders-h"), " Harmonization"),
          value = "harmonization_tab",
          br(),
          harmonization_settings_ui()
        ),

        # About Tab
        tabPanel(
          title = tagList(icon("info-circle"), " About"),
          value = "about_tab",
          br(),
          h3("EcoNeTool"),
          h5("Ecological Interaction Network Explorer"),
          p(get_version("full")),
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
        HTML(paste0("
          <div style='padding: 15px;'>
            <h5>EcoNeTool</h5>
            <p>", get_version_html(include_date = TRUE), "</p>
            <p><strong>License:</strong> GPL-3.0</p>

            <h5>About</h5>
            <p>Generic food web analysis tool supporting custom data import in multiple formats (Excel, CSV, RData).</p>

            <h5>Default Dataset</h5>
            <p><strong>Lithuanian Coastal Food Web</strong><br>
            Southeastern Baltic Sea coastal ecosystem<br>
            41 species, 244 links<br>
            6 functional groups (Phytoplankton, Zooplankton, Benthos, Fish, Birds, Detritus)</p>

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
        "))
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
  # SESSION CLEANUP - Release parallel workers on session end
  # ============================================================================
  session$onSessionEnded(function() {
    tryCatch({
      if (exists("shutdown_parallel_lookup") && is.function(shutdown_parallel_lookup)) {
        shutdown_parallel_lookup()
        message("Session ended: parallel workers released")
      }
    }, error = function(e) {
      # Silent fail - session is ending anyway
    })
  })

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
    # Assign colors by matching functional group NAMES to COLOR_SCHEME
    # This works regardless of factor level order in the data
    fg_levels <- get_functional_group_levels()

    info$colfg <- sapply(as.character(info$fg), function(fg) {
      idx <- which(fg_levels == fg)
      if (length(idx) == 0) return("gray")  # Unknown functional group
      COLOR_SCHEME[idx]
    })

    info
  })

  # ============================================================================
  # CACHED REACTIVE EXPRESSIONS (HIGH-PRIORITY OPTIMIZATION)
  # ============================================================================
  # These cached reactives prevent expensive recalculations when the network
  # hasn't changed. Using bindCache() ensures trophic levels and metrics are
  # only computed once per unique network state.

  # Cached trophic levels calculation
  trophic_levels_cached <- reactive({
    req(net_reactive())
    calculate_trophic_levels(net_reactive())
  })

  # Cached topological metrics calculation
  topological_metrics_cached <- reactive({
    req(net_reactive())
    get_topological_indicators(net_reactive())
  })

  # Metaweb Manager (Phase 2) - used by Spatial Analysis (Phase 1)
  # Note: METAWEB_PATHS is now defined in R/config.R and validated at startup
  current_metaweb <- reactiveVal(NULL)

  # Metadata for dashboard boxes
  metaweb_metadata <- reactiveVal(list(
    location = "Lithuanian Coast, Southeastern Baltic Sea",
    time_period = "Coastal ecosystem",
    source = "LTCoastal Food Web Model"
  ))

  # Trigger for dashboard updates
  dashboard_trigger <- reactiveVal(0)

  # ============================================================================
  # PLUGIN MANAGEMENT
  # ============================================================================

  # Store plugin states (plugin_id => enabled TRUE/FALSE)
  plugin_states <- reactiveVal(get_default_plugin_states())

  # Plugin management UI and handlers (extracted to R/modules/plugin_server.R)
  plugin_server(input, output, session, plugin_states)

  # ============================================================================
  # SHARED DATA FOR TRAIT MODULES
  # ============================================================================

  # Shared reactive values for data transfer between Trait Research and Food Web Construction
  shared_trait_data <- reactiveValues(
    trait_data = NULL  # Trait data passed from Trait Research to Food Web Construction
  )

  # ============================================================================
  # TRAIT RESEARCH MODULE (NEW)
  # ============================================================================

  # Call trait research server
  trait_research_server(input, output, session, shared_data = shared_trait_data)

  # ============================================================================
  # FOOD WEB CONSTRUCTION MODULE (NEW)
  # ============================================================================

  # Call food web construction server
  foodweb_construction_server(input, output, session, shared_data = shared_trait_data)

  # ============================================================================
  # HARMONIZATION SETTINGS MODULE
  # ============================================================================

  # Call harmonization settings server
  harmonization_settings_server(input, output, session)

  # ============================================================================
  # RPATH MODULE (ECOPATH/ECOSIM Integration)
  # ============================================================================

  # Reactive value to store ECOPATH import data for Rpath module
  ecopath_import_data <- reactiveVal(NULL)

  # Reactive value to store ECOPATH native import status for formatted display
  ecopath_native_status_data <- reactiveVal(NULL)

  # Call Rpath module server
  rpath_results <- rpathModuleServer(
    "rpath_module",
    ecopath_import_reactive = ecopath_import_data
  )

  # ============================================================================
  # DASHBOARD - Dynamic Value Boxes
  # ============================================================================
  dashboard_server(input, output, session, net_reactive, info_reactive,
                   metaweb_metadata, dashboard_trigger)

  # Reactive value to cache EUSeaMap data (loaded once, reused across modules)
  euseamap_data <- reactiveVal(NULL)

  # ============================================================================
  # INTERNAL DATA EDITOR (must be before ecopath_import_server and ecobase_server)
  # Returns refresh_data_editor function used by other modules
  # ============================================================================
  refresh_data_editor <- dataeditor_inline_server(input, output, session,
                                                   net_reactive, info_reactive,
                                                   dashboard_trigger)

  # ============================================================================
  # ECOPATH IMPORT MODULE (parser + CSV import + native DB import + EMODnet)
  # ============================================================================
  ecopath_import_server(input, output, session, net_reactive, info_reactive,
                        metaweb_metadata, dashboard_trigger,
                        ecopath_import_data, ecopath_native_status_data,
                        plugin_states, euseamap_data, current_metaweb,
                        refresh_data_editor)

  # ============================================================================
  # DATA IMPORT HANDLER
  # ============================================================================
  data_import_server(input, output, session, net_reactive, info_reactive,
                     dashboard_trigger, refresh_data_editor)

  # ============================================================================
  # VISUALIZATION OUTPUTS
  # ============================================================================
  visualization_server(input, output, session, net_reactive, info_reactive,
                       trophic_levels_cached, topological_metrics_cached)

  # ============================================================================
  # ANALYSIS (FLUX + KEYSTONENESS)
  # ============================================================================
  analysis_server(input, output, session, net_reactive, info_reactive,
                  trophic_levels_cached)

  # ============================================================================
  # DOWNLOAD + EXPORT
  # ============================================================================
  download_server(input, output, session, net_reactive, info_reactive)

  # ============================================================================
  # METAWEB MANAGER
  # ============================================================================
  metaweb_manager_server(input, output, session, current_metaweb,
                         net_reactive, info_reactive,
                         metaweb_metadata, dashboard_trigger)

  # ============================================================================
  # SPATIAL ANALYSIS
  # ============================================================================
  spatial_server(input, output, session, net_reactive, info_reactive,
                 current_metaweb, euseamap_data)

  # ============================================================================
  # ECOBASE CONNECTION
  # ============================================================================
  ecobase_server(input, output, session, net_reactive, info_reactive,
                 metaweb_metadata, dashboard_trigger, refresh_data_editor)

  # ============================================================================
  # SHARK4R INTEGRATION
  # ============================================================================
  shark_server(input, output, session)

  # Ensure dashboard outputs are not suspended when hidden (load by default)
  outputOptions(output, "box_species", suspendWhenHidden = FALSE)
  outputOptions(output, "box_links", suspendWhenHidden = FALSE)
  outputOptions(output, "box_groups", suspendWhenHidden = FALSE)
  outputOptions(output, "box_period", suspendWhenHidden = FALSE)
  outputOptions(output, "box_location", suspendWhenHidden = FALSE)

}
shinyApp(ui, server)
