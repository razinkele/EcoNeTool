# ==============================================================================
# RPATH MODULE - UI COMPONENT
# ==============================================================================
# UI component for ECOPATH/ECOSIM modeling functionality
#
# This module provides the user interface for:
#   - Mass balance modeling (Ecopath)
#   - Mixed Trophic Impact (MTI) analysis
#   - Dynamic simulations (Ecosim)
#   - Sensitivity analysis
#   - Management scenarios
#   - Results visualization and export
#
# The UI is dynamically generated based on Rpath package availability.
# If Rpath is not installed, installation instructions are displayed.
#
# Dependencies:
#   - Shiny
#   - shinydashboard
#   - shinyWidgets
#
# Server Component:
#   See R/modules/rpath_server.R for server-side logic
#
# ==============================================================================

#' Rpath Module UI
#'
#' Creates the user interface for ECOPATH/ECOSIM functionality
#'
#' @param id Character string. The namespace ID for this module instance.
#'
#' @return A tagList containing the module UI elements
#'
#' @details
#' The UI consists of two main components:
#' 1. **rpath_status**: Shows Rpath package installation status or instructions
#' 2. **rpath_content**: Main content area (tabs for different analyses)
#'
#' The content area is only shown if the Rpath package is installed.
#'
#' @examples
#' \dontrun{
#' # In your Shiny UI:
#' rpathModuleUI("rpath_module")
#' }
#'
#' @export
rpathModuleUI <- function(id) {
  ns <- NS(id)

  tagList(
    # Header
    fluidRow(
      column(12,
        h3(icon("fish"), "ECOPATH/ECOSIM Modeling"),
        p("Run mass-balance models and dynamic simulations using the Rpath package (NOAA-EDAB)")
      )
    ),

    # Installation status check (shows error message if Rpath not installed)
    uiOutput(ns("rpath_status")),

    # Main content area (tabs for different analyses)
    # Only shown if Rpath is installed
    uiOutput(ns("rpath_content"))
  )
}
