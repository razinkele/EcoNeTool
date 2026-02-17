#' Plugin Configuration for EcoNeTool
#'
#' Defines available plugins/modules and their default states
#'
#' @return List of plugin configurations

# ==============================================================================
# PLUGIN DEFINITIONS
# ==============================================================================

AVAILABLE_PLUGINS <- list(
  # Core modules (always enabled, not optional)
  core = list(
    dashboard = list(
      name = "Dashboard",
      description = "Overview and quick stats",
      icon = "tachometer-alt",
      required = TRUE,
      enabled = TRUE,
      category = "core"
    ),
    import = list(
      name = "Data Import",
      description = "Import food web data from various sources",
      icon = "upload",
      required = TRUE,
      enabled = TRUE,
      category = "core"
    ),
    network = list(
      name = "Food Web Network",
      description = "Interactive network visualization",
      icon = "project-diagram",
      required = TRUE,
      enabled = TRUE,
      category = "core"
    )
  ),

  # Analysis modules (optional)
  analysis = list(
    topological = list(
      name = "Topological Metrics",
      description = "Network topology analysis and metrics",
      icon = "chart-line",
      required = FALSE,
      enabled = TRUE,
      category = "analysis"
    ),
    biomass = list(
      name = "Biomass Analysis",
      description = "Species biomass distribution and analysis",
      icon = "balance-scale",
      required = FALSE,
      enabled = TRUE,
      category = "analysis"
    ),
    fluxes = list(
      name = "Energy Fluxes",
      description = "Energy flow and flux analysis",
      icon = "bolt",
      required = FALSE,
      enabled = TRUE,
      category = "analysis"
    ),
    keystoneness = list(
      name = "Keystoneness Analysis",
      description = "Identify keystone species",
      icon = "key",
      required = FALSE,
      enabled = TRUE,
      category = "analysis"
    )
  ),

  # Data management modules (optional)
  data = list(
    dataeditor = list(
      name = "Internal Data Editor",
      description = "Edit network and species data",
      icon = "edit",
      required = FALSE,
      enabled = TRUE,
      category = "data"
    ),
    metaweb_manager = list(
      name = "Metaweb Manager",
      description = "Manage metaweb library",
      icon = "layer-group",
      required = FALSE,
      enabled = TRUE,
      category = "data"
    )
  ),

  # Advanced modules (optional, may require additional packages)
  advanced = list(
    spatial_analysis = list(
      name = "Spatial Analysis",
      description = "Geographic and spatial network analysis",
      icon = "map-marked-alt",
      required = FALSE,
      enabled = TRUE,  # Enabled by default (requires leaflet, sp)
      category = "advanced",
      packages = c("leaflet", "sp")
    ),
    ecobase = list(
      name = "EcoBase Connection",
      description = "Connect to EcoBase web service",
      icon = "cloud-download-alt",
      required = FALSE,
      enabled = TRUE,
      category = "advanced",
      packages = c("RCurl", "XML")
    ),
    taxonomic_api = list(
      name = "Taxonomic Database Integration",
      description = "Classify species using FishBase, WoRMS, and OBIS APIs",
      icon = "fish",
      required = FALSE,
      enabled = TRUE,  # Enabled by default (requires internet + packages)
      category = "advanced",
      packages = c("httr", "jsonlite"),
      optional_packages = c("rfishbase"),  # Optional but recommended
      notes = "Queries FishBase, WoRMS, and OBIS for authoritative species classification and trait data. Requires internet connection for first query, then cached locally."
    )
  )
)

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Get Flattened Plugin List
#'
#' @return Named list of all plugins
get_all_plugins <- function() {
  # Flatten the nested list structure
  all_plugins <- list()

  for (category_name in names(AVAILABLE_PLUGINS)) {
    category <- AVAILABLE_PLUGINS[[category_name]]
    for (plugin_id in names(category)) {
      all_plugins[[plugin_id]] <- category[[plugin_id]]
    }
  }

  return(all_plugins)
}

#' Get Plugin by ID
#'
#' @param plugin_id Plugin identifier (e.g., "topological", "spatial_analysis")
#' @return Plugin configuration list or NULL
get_plugin <- function(plugin_id) {
  all_plugins <- get_all_plugins()
  all_plugins[[plugin_id]]
}

#' Check if Plugin is Enabled
#'
#' @param plugin_id Plugin identifier
#' @param plugin_states Current plugin states (reactive value)
#' @return Logical
is_plugin_enabled <- function(plugin_id, plugin_states) {
  if (is.null(plugin_states)) {
    # Use default state
    plugin <- get_plugin(plugin_id)
    return(if (!is.null(plugin)) plugin$enabled else FALSE)
  }

  # Use current state
  if (plugin_id %in% names(plugin_states)) {
    return(plugin_states[[plugin_id]])
  }

  # Fallback to default
  plugin <- get_plugin(plugin_id)
  return(if (!is.null(plugin)) plugin$enabled else FALSE)
}

#' Check if Required Packages are Available
#'
#' @param plugin_id Plugin identifier
#' @return Logical
check_plugin_packages <- function(plugin_id) {
  plugin <- get_plugin(plugin_id)

  if (is.null(plugin) || is.null(plugin$packages)) {
    return(TRUE)  # No package requirements
  }

  # Check all required packages
  all(sapply(plugin$packages, function(pkg) {
    requireNamespace(pkg, quietly = TRUE)
  }))
}

#' Get Default Plugin States
#'
#' @return Named list of plugin_id => enabled status
get_default_plugin_states <- function() {
  all_plugins <- get_all_plugins()
  plugin_ids <- names(all_plugins)

  states <- lapply(seq_along(all_plugins), function(i) {
    plugin_id <- plugin_ids[i]
    plugin <- all_plugins[[i]]

    # Disable if required packages are missing
    if (!is.null(plugin$packages) && !check_plugin_packages(plugin_id)) {
      return(FALSE)
    }
    return(plugin$enabled)
  })

  names(states) <- plugin_ids
  return(states)
}

#' Get Plugins by Category
#'
#' @param category Category name ("core", "analysis", "data", "advanced")
#' @return List of plugins in category
get_plugins_by_category <- function(category) {
  all_plugins <- get_all_plugins()
  Filter(function(p) !is.null(p$category) && p$category == category, all_plugins)
}
