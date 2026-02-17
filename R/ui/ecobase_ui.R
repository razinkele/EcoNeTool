#' EcoBase Connection UI
#'
#' Creates the EcoBase tab for downloading Ecopath models from EcoBase web service
#'
#' @return A tabItem for EcoBase connection
ecobase_ui <- function() {
  # ========================================================================
  # ECOBASE CONNECTION TAB
  # ========================================================================
  tabItem(
    tabName = "ecobase",

    fluidRow(
      box(
        title = "EcoBase - Online Ecopath Model Repository",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        HTML("
          <h4>Connect to EcoBase Web Service</h4>
          <p>EcoBase is an online repository of Ecopath with Ecosim (EwE) models from around the world.</p>
          <p><strong>Website:</strong> <a href='http://sirs.agrocampus-ouest.fr/EcoBase/' target='_blank'>http://sirs.agrocampus-ouest.fr/EcoBase/</a></p>
          <p>Use this interface to browse and download published Ecopath models directly into EcoNeTool.</p>
        ")
      )
    ),

    fluidRow(
      # Left column: Model browser
      column(6,
        box(
          title = "1. Browse Available Models",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          height = "600px",

          actionButton("load_ecobase_models", "Load Model List from EcoBase",
                      icon = icon("download"), class = "btn-primary"),
          br(), br(),

          uiOutput("ecobase_connection_status"),
          br(),

          DTOutput("ecobase_models_table")
        )
      ),

      # Right column: Model details and import
      column(6,
        box(
          title = "2. Model Details",
          status = "success",
          solidHeader = TRUE,
          width = 12,

          uiOutput("ecobase_model_details"),
          br(),

          radioButtons(
            "ecobase_parameter_type",
            "Parameter Type:",
            choices = c(
              "Hybrid (Best of Both)" = "hybrid",
              "Output (Balanced)" = "output",
              "Input (Original)" = "input"
            ),
            selected = "hybrid",
            inline = FALSE
          ),

          HTML("<p style='font-size: 12px; color: #666; margin-top: -10px;'>
            <strong>Hybrid:</strong> Balanced parameters (Output) + Complete diet links (Input) - Recommended!<br>
            <strong>Output:</strong> Mass-balanced, may lack diet data for some models<br>
            <strong>Input:</strong> Original parameters with complete diet composition
          </p>"),

          actionButton("import_ecobase_model", "Import Model to EcoNeTool",
                      icon = icon("cloud-download-alt"), class = "btn-success"),
          br(), br(),

          verbatimTextOutput("ecobase_import_status")
        ),

        box(
          title = "Requirements",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,

          HTML("
            <h5>Required R Packages</h5>
            <p>EcoBase connection requires additional packages:</p>
            <pre style='background: #f8f9fa; padding: 10px;'>
install.packages(c('RCurl', 'XML', 'plyr', 'dplyr'))</pre>

            <h5>Internet Connection</h5>
            <p>Active internet connection required to access EcoBase web service.</p>

            <h5>Model Types</h5>
            <ul style='font-size: 13px;'>
              <li><strong>Output (Balanced):</strong> Mass-balanced parameters from EwE output</li>
              <li><strong>Input (Original):</strong> Original input parameters before balancing</li>
            </ul>

            <h5>What Gets Imported</h5>
            <ul style='font-size: 13px;'>
              <li>Species/functional groups</li>
              <li>Biomass values</li>
              <li>P/B ratios (Production/Biomass)</li>
              <li>Q/B ratios (Consumption/Biomass)</li>
              <li>Ecotrophic efficiency (EE)</li>
              <li>Diet composition (trophic links)</li>
            </ul>
          ")
        )
      )
    )
  )
  # ========================================================================
}
