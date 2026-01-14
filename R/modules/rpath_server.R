# ==============================================================================
# RPATH MODULE - SERVER COMPONENT
# ==============================================================================
# Server-side logic for ECOPATH/ECOSIM modeling functionality
#
# This module provides the server logic for:
#   - Importing and converting ECOPATH data to Rpath format
#   - Running mass balance models (Ecopath)
#   - Calculating Mixed Trophic Impacts (MTI)
#   - Running dynamic simulations (Ecosim)
#   - Performing sensitivity analyses
#   - Testing management scenarios
#   - Generating visualizations and exports
#
# Dependencies:
#   - Rpath package (NOAA-EDAB)
#   - R/functions/rpath/ (conversion, balance, simulation functions)
#   - R/functions/ecopath/ (ECOPATH database import)
#   - R/functions/auxillary_parser.R (comments/tooltips parsing)
#   - DT (interactive tables)
#   - data.table (efficient data manipulation)
#
# UI Component:
#   See R/ui/rpath_ui.R for user interface elements
#
# ==============================================================================

rpathModuleServer <- function(id, ecopath_import_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values to store Rpath results
    rpath_values <- reactiveValues(
      params = NULL,
      ecopath_model = NULL,
      mti = NULL,
      ecosim = NULL,
      status = "not_started"
    )

    # Note: Integration functions are loaded globally in app.R
    # No need to source rpath_integration.R here as it's already available

    # Check Rpath installation status
    rpath_installed <- reactive({
      requireNamespace("Rpath", quietly = TRUE)
    })

    # Display installation status
    output$rpath_status <- renderUI({
      if (!rpath_installed()) {
        box(
          title = "Rpath Package Not Installed",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          icon = icon("exclamation-triangle"),
          HTML("
            <p><strong>The Rpath package is required for ECOPATH/ECOSIM functionality.</strong></p>
            <h4>Installation Instructions:</h4>
            <ol>
              <li><strong>Install Rtools</strong> (Windows users only):
                <ul>
                  <li>Download from: <a href='https://cran.r-project.org/bin/windows/Rtools/' target='_blank'>https://cran.r-project.org/bin/windows/Rtools/</a></li>
                  <li>Choose version matching your R version (e.g., Rtools44 for R 4.4.x)</li>
                  <li>Run installer with default settings</li>
                  <li>Restart R/RStudio</li>
                </ul>
              </li>
              <li><strong>Install Rpath package</strong> (in R console):
                <pre style='background: #f8f9fa; padding: 10px; margin: 10px 0;'>
install.packages('remotes')
remotes::install_github('noaa-edab/Rpath', build_vignettes = TRUE)</pre>
              </li>
              <li><strong>Reload the application</strong></li>
            </ol>
            <p><strong>Documentation:</strong> <a href='https://noaa-edab.github.io/Rpath/' target='_blank'>https://noaa-edab.github.io/Rpath/</a></p>
            <p><em>Note: Installation may take 5-10 minutes. Requires internet connection.</em></p>
          ")
        )
      }
    })

    # Main content (only shown if Rpath is installed)
    output$rpath_content <- renderUI({
      if (!rpath_installed()) {
        return(NULL)
      }

      tabBox(
        id = ns("rpath_tabs"),
        width = 12,

        # Tab 1: Model Setup
        tabPanel(
          title = tagList(icon("database"), "Model Setup"),
          value = "setup",

          fluidRow(
            column(6,
              box(
                title = "Import Status",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                uiOutput(ns("import_status"))
              )
            ),
            column(6,
              box(
                title = "Convert to Rpath",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                textInput(ns("model_name"), "Model Name", value = "EcoNeTool Model"),
                actionButton(ns("btn_convert"), "Convert to Rpath Format",
                           icon = icon("exchange-alt"), class = "btn-primary"),
                br(), br(),
                uiOutput(ns("conversion_status"))
              )
            )
          )
        ),

        # Tab 2: Group Parameters Editor
        tabPanel(
          title = tagList(icon("table"), "Group Parameters"),
          value = "group_params",

          fluidRow(
            column(12,
              box(
                title = "Group Parameters - Inspect & Edit",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                HTML("
                  <p><strong>Edit group parameters before balancing.</strong> Each group needs at least 3 out of 4 parameters:</p>
                  <ul>
                    <li><strong>Biomass (B):</strong> tons/km²</li>
                    <li><strong>P/B:</strong> Production/Biomass ratio (per year)</li>
                    <li><strong>Q/B:</strong> Consumption/Biomass ratio (per year)</li>
                    <li><strong>EE:</strong> Ecotrophic Efficiency (0-1)</li>
                  </ul>
                  <p><strong>Type:</strong> 0=Consumer, 1=Producer, 2=Detritus, 3=Fleet</p>
                "),
                br(),
                uiOutput(ns("group_validation_summary")),
                br(),
                DT::dataTableOutput(ns("group_params_table")),
                br(),
                actionButton(ns("btn_save_groups"), "Save Changes",
                           icon = icon("save"), class = "btn-primary"),
                actionButton(ns("btn_reset_groups"), "Reset to Original",
                           icon = icon("undo"), class = "btn-secondary")
              )
            )
          )
        ),

        # Tab 3: Diet Matrix Editor
        tabPanel(
          title = tagList(icon("utensils"), "Diet Matrix"),
          value = "diet_matrix",

          fluidRow(
            column(12,
              box(
                title = "Diet Matrix - Inspect & Edit",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                HTML("
                  <p><strong>Edit diet composition matrix.</strong> Values represent the proportion of each prey in the predator's diet.</p>
                  <ul>
                    <li><strong>Rows:</strong> Prey groups (what is eaten)</li>
                    <li><strong>Columns:</strong> Predator groups (who eats)</li>
                    <li><strong>Values:</strong> Diet proportion (0-1)</li>
                    <li><strong>Important:</strong> Each column (predator) should sum to ≤ 1.0</li>
                  </ul>
                "),
                br(),
                uiOutput(ns("diet_validation_summary")),
                br(),
                DT::dataTableOutput(ns("diet_matrix_table")),
                br(),
                actionButton(ns("btn_save_diet"), "Save Changes",
                           icon = icon("save"), class = "btn-primary"),
                actionButton(ns("btn_reset_diet"), "Reset to Original",
                           icon = icon("undo"), class = "btn-secondary")
              )
            )
          )
        ),

        # Tab 4: Calibration/Pedigree
        tabPanel(
          title = tagList(icon("certificate"), "Calibration"),
          value = "calibration",

          fluidRow(
            column(12,
              box(
                title = "Calibration/Pedigree Quality Assessment",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                HTML("
                  <p><strong>Data quality and confidence assessment for model parameters.</strong></p>
                  <p>Pedigree levels indicate the reliability of parameter estimates:</p>
                  <ul>
                    <li><strong>High:</strong> Direct measurements, well-documented data</li>
                    <li><strong>Medium:</strong> Regional estimates, indirect measurements</li>
                    <li><strong>Low:</strong> Expert estimates, assumptions</li>
                  </ul>
                  <p><em>Variables tracked: Biomass, P/B, Q/B, Diet Composition, Catch</em></p>
                "),
                br(),
                DT::dataTableOutput(ns("calibration_table")),
                br(),
                downloadButton(ns("download_calibration"), "Download Calibration Data",
                             class = "btn-info")
              )
            )
          )
        ),

        # Tab 5: Comments/Notes
        tabPanel(
          title = tagList(icon("comment"), "Comments/Notes"),
          value = "comments",

          fluidRow(
            column(12,
              box(
                title = "Parameter Comments, Tooltips, and Source Citations",
                status = "warning",
                solidHeader = TRUE,
                width = 12,
                HTML("
                  <p><strong>Detailed comments, notes, and source citations for parameter values.</strong></p>
                  <p>This table shows:</p>
                  <ul>
                    <li><strong>Entity:</strong> Which group or fleet the comment refers to</li>
                    <li><strong>Parameter:</strong> Which parameter (Biomass, P/B, Q/B, Diet, etc.)</li>
                    <li><strong>Remark:</strong> Comments, notes, source citations, and methodology</li>
                  </ul>
                  <p><em>Source: Auxillary table from EwE database</em></p>
                "),
                br(),
                DT::dataTableOutput(ns("comments_table")),
                br(),
                downloadButton(ns("download_comments"), "Download Comments Data",
                             class = "btn-warning")
              )
            )
          )
        ),

        # Tab 6: Mass Balance (Ecopath)
        tabPanel(
          title = tagList(icon("balance-scale"), "Mass Balance"),
          value = "ecopath",

          fluidRow(
            column(4,
              box(
                title = "Run Ecopath",
                status = "success",
                solidHeader = TRUE,
                width = 12,
                checkboxInput(ns("balance_model"), "Auto-balance model", value = TRUE),
                actionButton(ns("btn_run_ecopath"), "Run Mass Balance",
                           icon = icon("play"), class = "btn-success"),
                br(), br(),
                uiOutput(ns("ecopath_status"))
              ),
              box(
                title = "Model Summary",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                verbatimTextOutput(ns("ecopath_summary"))
              )
            ),
            column(8,
              box(
                title = "Balanced Model Results",
                status = "success",
                solidHeader = TRUE,
                width = 12,
                HTML("
                  <p><strong>Results from mass-balanced ECOPATH model.</strong></p>
                  <p>Key outputs:</p>
                  <ul>
                    <li><strong>EE (Ecotrophic Efficiency):</strong> Proportion of production consumed or caught (0-1)</li>
                    <li><strong>GE (Gross Efficiency):</strong> P/Q ratio (production / consumption)</li>
                    <li><strong>TL (Trophic Level):</strong> Calculated position in food web</li>
                    <li><strong>P (Production):</strong> Total production (t/km²/year)</li>
                    <li><strong>Q (Consumption):</strong> Total consumption (t/km²/year)</li>
                  </ul>
                  <p><em>Values are calculated by Ecopath mass-balance algorithm</em></p>
                  <div style='margin-top: 10px; padding: 10px; background-color: #f8f9fa; border-left: 4px solid #17a2b8;'>
                    <strong>Quality Check Legend:</strong><br>
                    <span style='background-color: #ffebee; padding: 2px 6px; margin-right: 10px;'>■</span> Critical Issue (EE > 1.0, GE > 1.0, negative values)<br>
                    <span style='background-color: #fff3cd; padding: 2px 6px; margin-right: 10px;'>■</span> Warning (EE > 0.95, GE > 0.5, TL > 5.0)<br>
                    <span style='background-color: #e8f4f8; padding: 2px 6px; margin-right: 10px;'>■</span> Note (EE < 0.05, very low biomass)
                  </div>
                "),
                br(),
                DT::dataTableOutput(ns("ecopath_results_table")),
                br(),
                downloadButton(ns("download_ecopath_results"), "Download Results",
                             class = "btn-success")
              )
            )
          )
        ),

        # Tab 3: Mixed Trophic Impacts
        tabPanel(
          title = tagList(icon("project-diagram"), "MTI Analysis"),
          value = "mti",

          fluidRow(
            column(3,
              box(
                title = "Calculate MTI",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                actionButton(ns("btn_calc_mti"), "Calculate MTI",
                           icon = icon("calculator"), class = "btn-info"),
                br(), br(),
                uiOutput(ns("mti_status"))
              )
            ),
            column(9,
              box(
                title = "Mixed Trophic Impact Matrix",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                plotOutput(ns("mti_heatmap"), height = "600px")
              )
            )
          )
        ),

        # Tab 4: Ecosim Simulation
        tabPanel(
          title = tagList(icon("chart-line"), "Ecosim"),
          value = "ecosim",

          fluidRow(
            column(4,
              box(
                title = "Simulation Setup",
                status = "warning",
                solidHeader = TRUE,
                width = 12,
                uiOutput(ns("scenario_selector")),
                numericInput(ns("sim_years"), "Simulation Years", value = 50, min = 1, max = 200),
                selectInput(ns("sim_method"), "Integration Method",
                          choices = c("RK4" = "RK4", "AB" = "AB", "Euler" = "Euler"),
                          selected = "RK4"),
                actionButton(ns("btn_run_ecosim"), "Run Simulation",
                           icon = icon("rocket"), class = "btn-warning"),
                br(), br(),
                uiOutput(ns("ecosim_status"))
              ),
              box(
                title = "Visualization Options",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                collapsed = TRUE,
                selectInput(ns("plot_type"), "Plot Type",
                          choices = c("Biomass" = "biomass",
                                    "Catch" = "catch",
                                    "Relative Biomass" = "relative"),
                          selected = "biomass"),
                uiOutput(ns("group_selector")),
                checkboxInput(ns("show_legend"), "Show Legend", value = TRUE),
                actionButton(ns("btn_update_plot"), "Update Plot",
                           icon = icon("sync"), class = "btn-info btn-sm")
              )
            ),
            column(8,
              box(
                title = "Biomass Dynamics",
                status = "warning",
                solidHeader = TRUE,
                width = 12,
                plotOutput(ns("ecosim_biomass_plot"), height = "500px")
              ),
              box(
                title = "Export Results",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                collapsed = TRUE,
                fluidRow(
                  column(6,
                    downloadButton(ns("download_biomass_csv"), "Biomass (CSV)",
                                 class = "btn-primary btn-sm")
                  ),
                  column(6,
                    downloadButton(ns("download_simulation_rds"), "Full Results (RDS)",
                                 class = "btn-primary btn-sm")
                  )
                )
              )
            )
          )
        ),

        # Tab 5: Model Diagnostics
        tabPanel(
          title = tagList(icon("stethoscope"), "Diagnostics"),
          value = "diagnostics",

          fluidRow(
            column(6,
              box(
                title = "Model Quality Metrics",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                actionButton(ns("btn_calc_diagnostics"), "Calculate Diagnostics",
                           icon = icon("calculator"), class = "btn-info"),
                br(), br(),
                uiOutput(ns("diagnostics_summary"))
              )
            ),
            column(6,
              box(
                title = "Export Model Data",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                downloadButton(ns("download_ecopath_params"), "Ecopath Parameters (CSV)"),
                br(), br(),
                downloadButton(ns("download_diet_matrix"), "Diet Matrix (CSV)"),
                br(), br(),
                downloadButton(ns("download_model_summary"), "Model Summary (TXT)")
              )
            )
          ),
          fluidRow(
            column(12,
              box(
                title = "Trophic Structure",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                plotOutput(ns("trophic_pyramid"), height = "400px")
              )
            )
          )
        ),

        # Tab 6: Sensitivity Analysis
        tabPanel(
          title = tagList(icon("sliders-h"), "Sensitivity"),
          value = "sensitivity",

          fluidRow(
            column(4,
              box(
                title = "Parameter Sensitivity Test",
                status = "success",
                solidHeader = TRUE,
                width = 12,
                HTML("<p>Test how sensitive the model is to parameter changes.</p>"),
                uiOutput(ns("sensitivity_param_selector")),
                numericInput(ns("sensitivity_range"), "Variation Range (%)",
                           value = 20, min = 5, max = 50, step = 5),
                numericInput(ns("sensitivity_steps"), "Number of Steps",
                           value = 10, min = 5, max = 30, step = 1),
                actionButton(ns("btn_run_sensitivity"), "Run Sensitivity Analysis",
                           icon = icon("chart-area"), class = "btn-success"),
                br(), br(),
                uiOutput(ns("sensitivity_status"))
              )
            ),
            column(8,
              box(
                title = "Sensitivity Results",
                status = "success",
                solidHeader = TRUE,
                width = 12,
                plotOutput(ns("sensitivity_plot"), height = "500px")
              )
            )
          )
        ),

        # Tab 7: Management Scenarios
        tabPanel(
          title = tagList(icon("ship"), "Scenarios"),
          value = "scenarios",

          fluidRow(
            column(6,
              box(
                title = "Fishing Scenario Builder",
                status = "danger",
                solidHeader = TRUE,
                width = 12,
                HTML("<p><strong>Create and test fishing management scenarios</strong></p>"),
                uiOutput(ns("fleet_selector")),
                sliderInput(ns("effort_multiplier"), "Fishing Effort Multiplier",
                          min = 0, max = 2, value = 1, step = 0.1),
                numericInput(ns("scenario_years"), "Scenario Duration (years)",
                           value = 50, min = 10, max = 100),
                actionButton(ns("btn_run_scenario"), "Run Fishing Scenario",
                           icon = icon("ship"), class = "btn-danger"),
                br(), br(),
                uiOutput(ns("scenario_status"))
              )
            ),
            column(6,
              box(
                title = "Scenario Results",
                status = "danger",
                solidHeader = TRUE,
                width = 12,
                plotOutput(ns("scenario_plot"), height = "400px")
              )
            )
          ),
          fluidRow(
            column(12,
              box(
                title = "Scenario Comparison",
                status = "warning",
                solidHeader = TRUE,
                width = 12,
                HTML("
                  <p><strong>Compare multiple management scenarios:</strong></p>
                  <ul>
                    <li>Baseline (current effort)</li>
                    <li>Reduced effort (conservation)</li>
                    <li>Increased effort (exploitation)</li>
                    <li>Fleet-specific restrictions</li>
                  </ul>
                  <p><em>Run scenarios above to enable comparison.</em></p>
                ")
              )
            )
          )
        )
      )
    })

    # Import status display
    output$import_status <- renderUI({
      ecopath_data <- ecopath_import_reactive()

      if (is.null(ecopath_data)) {
        HTML("
          <p><i class='fa fa-info-circle'></i> No ECOPATH data imported yet.</p>
          <p>Import an ECOPATH database from the <strong>Import Data</strong> tab.</p>
        ")
      } else {
        n_groups <- nrow(ecopath_data$group_data)
        n_diet <- nrow(ecopath_data$diet_data)

        # Check for ECOSIM scenarios
        ecosim_info <- ""
        if (!is.null(ecopath_data$ecosim_scenarios)) {
          n_scenarios <- nrow(ecopath_data$ecosim_scenarios$scenarios)
          ecosim_info <- paste0("
            <li><strong>ECOSIM scenarios:</strong> ", n_scenarios, " <span style='color: #f39c12;'>⚡</span></li>
          ")
        }

        HTML(paste0("
          <p><i class='fa fa-check-circle' style='color: green;'></i> ECOPATH data loaded</p>
          <ul>
            <li><strong>Groups:</strong> ", n_groups, "</li>
            <li><strong>Diet links:</strong> ", n_diet, "</li>
            ", ecosim_info, "
          </ul>
          <p style='color: green;'><em>Ready for conversion to Rpath format.</em></p>
        "))
      }
    })

    # ECOSIM Scenario selector
    output$scenario_selector <- renderUI({
      ecopath_data <- ecopath_import_reactive()

      # Check if ECOSIM scenarios are available
      if (!is.null(ecopath_data) && !is.null(ecopath_data$ecosim_scenarios)) {
        scenarios <- ecopath_data$ecosim_scenarios$scenarios

        # Create choices for selectInput
        scenario_choices <- setNames(
          scenarios$ScenarioID,
          paste0(scenarios$ScenarioID, ". ", scenarios$ScenarioName)
        )

        # Add "None (Default)" option
        scenario_choices <- c("Default (No scenario)" = 0, scenario_choices)

        tagList(
          selectInput(
            ns("selected_scenario"),
            HTML("<strong>ECOSIM Scenario</strong> <span style='color: #f39c12;'>⚡</span>"),
            choices = scenario_choices,
            selected = 0
          ),
          helpText(icon("info-circle"), "Select an imported ECOSIM scenario to apply its parameters (vulnerabilities, forcing functions).")
        )
      } else {
        # No scenarios available
        helpText(icon("info-circle"), "No ECOSIM scenarios available. Import a database with scenarios to use this feature.")
      }
    })

    # Store original params for reset functionality
    original_params <- reactiveVal(NULL)

    # Convert to Rpath format
    observeEvent(input$btn_convert, {
      ecopath_data <- ecopath_import_reactive()

      if (is.null(ecopath_data)) {
        showNotification("Please import ECOPATH data first", type = "error")
        return()
      }

      tryCatch({
        # Clear old results BEFORE creating new ones to free memory
        rpath_values$params <- NULL
        rpath_values$ecopath_model <- NULL
        rpath_values$mti <- NULL
        rpath_values$ecosim <- NULL
        gc()  # Trigger garbage collection

        showNotification("Converting to Rpath format...", type = "message")

        rpath_values$params <- convert_ecopath_to_rpath(
          ecopath_data,
          model_name = input$model_name
        )

        rpath_values$status <- "converted"

        # Store original params for reset functionality
        original_params(list(
          model = as.data.frame(rpath_values$params$model),
          diet = as.data.frame(rpath_values$params$diet)
        ))

        showNotification("Conversion successful!", type = "message", duration = 3)

      }, error = function(e) {
        showNotification(paste("Conversion error:", e$message), type = "error", duration = 10)
      })
    })

    # ===========================================================================
    # GROUP PARAMETERS EDITOR
    # ===========================================================================

    # Display group parameters table (editable)
    output$group_params_table <- DT::renderDataTable({
      # Force reactivity - wait for params to be available
      req(rpath_values$params)

      # Also require that status is "converted" to ensure conversion is complete
      req(rpath_values$status == "converted")

      # Debug output (only shown when ECO_NT_DEBUG=true)
      if (exists("log_debug")) log_debug("Rpath", "Rendering group_params_table...")

      tryCatch({
        # Check if params exists and has model component
        if (is.null(rpath_values$params) || is.null(rpath_values$params$model)) {
          if (exists("log_debug")) log_debug("Rpath", "ERROR: params or params$model is NULL")
          return(DT::datatable(
            data.frame(Message = "Please convert ECOPATH data to Rpath format first"),
            options = list(dom = 't'),
            rownames = FALSE
          ))
        }

        # Get model data
        model <- rpath_values$params$model
        if (exists("log_debug")) log_debug("Rpath", "Model has", nrow(model), "rows")

        # Validate model has required columns
        required_cols <- c("Group", "Type", "Biomass", "PB", "QB", "EE")
        if (!all(required_cols %in% names(model))) {
          missing <- setdiff(required_cols, names(model))
          message(sprintf("  ERROR: Missing columns: %s", paste(missing, collapse=", ")))
          return(DT::datatable(
            data.frame(Message = paste("Model data is incomplete. Missing:", paste(missing, collapse=", "))),
            options = list(dom = 't'),
            rownames = FALSE
          ))
        }

        # Get relevant columns for editing with explicit type conversion
        df <- data.frame(
          Group = as.character(model$Group),
          Type = as.integer(model$Type),
          Biomass = round(as.numeric(model$Biomass), 4),
          PB = round(as.numeric(model$PB), 4),
          QB = round(as.numeric(model$QB), 4),
          EE = round(as.numeric(model$EE), 4),
          stringsAsFactors = FALSE
        )

        message(sprintf("  Created data frame: %d rows × %d columns", nrow(df), ncol(df)))
        message("  Rendering DT::datatable...")

        DT::datatable(
          df,
          editable = list(target = 'cell', disable = list(columns = c(0))),  # Group name not editable
          options = list(
            pageLength = 25,
            scrollX = TRUE,
            dom = 'tp',  # Simplified: table and pagination only
            columnDefs = list(
              list(className = 'dt-center', targets = 1:5)  # 0-indexed: columns 1-5
            )
          ),
          rownames = FALSE,
          caption = sprintf("Edit parameters for %d groups (detritus only needs Biomass)", nrow(df))
        )
      }, error = function(e) {
        message(sprintf("  ERROR in group_params_table: %s", conditionMessage(e)))
        DT::datatable(
          data.frame(Error = paste("Error rendering table:", conditionMessage(e))),
          options = list(dom = 't'),
          rownames = FALSE
        )
      })
    })

    # Validate and show summary
    output$group_validation_summary <- renderUI({
      if (is.null(rpath_values$params)) {
        return(NULL)
      }

      model <- rpath_values$params$model
      issues <- list()

      for (i in 1:nrow(model)) {
        if (model$Type[i] == 0) {
          # CONSUMERS: Need 3 out of 4 parameters
          n_params <- sum(!is.na(c(model$Biomass[i], model$PB[i], model$QB[i], model$EE[i])))

          if (n_params < 3) {
            issues <- c(issues, paste0(model$Group[i], " (consumer: only ", n_params, " parameters)"))
          }
        } else if (model$Type[i] == 1) {
          # PRODUCERS: Need Biomass + P/B (autotrophs don't need Q/B)
          has_biomass <- !is.na(model$Biomass[i])
          has_pb <- !is.na(model$PB[i])

          if (!has_biomass || !has_pb) {
            missing <- c()
            if (!has_biomass) missing <- c(missing, "Biomass")
            if (!has_pb) missing <- c(missing, "P/B")
            issues <- c(issues, paste0(model$Group[i], " (producer missing: ", paste(missing, collapse=", "), ")"))
          }
        } else if (model$Type[i] == 2) {
          # DETRITUS: Only needs Biomass
          if (is.na(model$Biomass[i])) {
            issues <- c(issues, paste0(model$Group[i], " (detritus missing Biomass)"))
          }
        }
      }

      if (length(issues) > 0) {
        HTML(paste0("
          <div class='alert alert-warning'>
            <i class='fa fa-exclamation-triangle'></i> <strong>Validation Issues (", length(issues), " groups):</strong>
            <ul>",
            paste0("<li>", issues, "</li>", collapse = ""),
            "</ul>
            <p><em>Requirements: Consumers need 3/4 parameters (B, P/B, Q/B, EE). Producers need Biomass + P/B. Detritus needs only Biomass.</em></p>
          </div>
        "))
      } else {
        HTML("<div class='alert alert-success'><i class='fa fa-check-circle'></i> <strong>All groups have sufficient parameters!</strong></div>")
      }
    })

    # Handle cell edits in group parameters table
    observeEvent(input$group_params_table_cell_edit, {
      info <- input$group_params_table_cell_edit

      row <- info$row
      col <- info$col
      value <- info$value

      # Column mapping (0-indexed from DT)
      cols <- c("Group", "Type", "Biomass", "PB", "QB", "EE")
      col_name <- cols[col + 1]

      if (col_name == "Group") return()  # Don't edit group names

      # Convert value to numeric
      new_value <- as.numeric(value)

      # Validate
      if (col_name == "Type" && !new_value %in% c(0, 1, 2, 3)) {
        showNotification("Type must be 0, 1, 2, or 3", type = "error")
        return()
      }

      if (col_name %in% c("PB", "QB") && !is.na(new_value) && new_value <= 0) {
        showNotification(paste(col_name, "must be > 0"), type = "error")
        return()
      }

      if (col_name == "EE" && !is.na(new_value) && (new_value < 0 || new_value > 1)) {
        showNotification("EE must be between 0 and 1", type = "error")
        return()
      }

      # Update the parameter
      rpath_values$params$model[[col_name]][row] <- new_value

      showNotification(paste("Updated", col_name, "for group", row), type = "message", duration = 2)
    })

    # Save changes button
    observeEvent(input$btn_save_groups, {
      showNotification("Group parameters saved!", type = "message", duration = 3)
      # Parameters are already updated in rpath_values$params via cell edits
    })

    # Reset button
    observeEvent(input$btn_reset_groups, {
      if (!is.null(original_params())) {
        rpath_values$params$model <- original_params()$model
        showNotification("Group parameters reset to original values", type = "message", duration = 3)
      }
    })

    # ===========================================================================
    # DIET MATRIX EDITOR
    # ===========================================================================

    # Display diet matrix table (editable)
    output$diet_matrix_table <- DT::renderDataTable({
      if (is.null(rpath_values$params)) {
        return(data.frame(Message = "Please convert ECOPATH data to Rpath format first"))
      }

      # Get diet matrix and round values
      df <- as.data.frame(rpath_values$params$diet)
      for (col in names(df)) {
        if (col != "Group" && is.numeric(df[[col]])) {
          df[[col]] <- round(df[[col]], 4)
        }
      }

      DT::datatable(
        df,
        editable = list(target = 'cell', disable = list(columns = c(0))),  # Group column not editable
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          scrollY = "400px",
          dom = 't<"bottom"ip>',
          columnDefs = list(
            list(className = 'dt-center', targets = 1:(ncol(df)-1))
          )
        ),
        rownames = FALSE
      )
    })

    # Validate diet matrix
    output$diet_validation_summary <- renderUI({
      if (is.null(rpath_values$params)) {
        return(NULL)
      }

      diet <- rpath_values$params$diet
      diet_cols <- names(diet)[names(diet) != "Group"]

      issues <- list()

      for (col in diet_cols) {
        col_sum <- sum(diet[[col]], na.rm = TRUE)
        if (col_sum > 1.01) {
          issues <- c(issues, paste0(col, " (sum = ", round(col_sum, 3), ")"))
        }
      }

      if (length(issues) > 0) {
        HTML(paste0("
          <div class='alert alert-warning'>
            <i class='fa fa-exclamation-triangle'></i> <strong>Diet Sum Issues (", length(issues), " predators):</strong>
            <ul>",
            paste0("<li>", issues, "</li>", collapse = ""),
            "</ul>
            <p><em>Each predator column should sum to ≤ 1.0</em></p>
          </div>
        "))
      } else {
        HTML("<div class='alert alert-success'><i class='fa fa-check-circle'></i> <strong>All predator diets are valid!</strong></div>")
      }
    })

    # Handle cell edits in diet matrix
    observeEvent(input$diet_matrix_table_cell_edit, {
      info <- input$diet_matrix_table_cell_edit

      row <- info$row + 1  # R uses 1-indexed
      col <- info$col + 1  # DT is 0-indexed, R is 1-indexed
      value <- info$value

      if (col == 1) return()  # Don't edit Group column

      # Convert value to numeric
      new_value <- as.numeric(value)

      # Validate
      if (!is.na(new_value) && (new_value < 0 || new_value > 1)) {
        showNotification("Diet values must be between 0 and 1", type = "error")
        return()
      }

      # Update the diet matrix (using data.table format)
      col_name <- names(rpath_values$params$diet)[col]
      rpath_values$params$diet[[col_name]][row] <- new_value

      showNotification(paste("Updated diet value"), type = "message", duration = 2)
    })

    # Save diet changes
    observeEvent(input$btn_save_diet, {
      showNotification("Diet matrix saved!", type = "message", duration = 3)
      # Diet is already updated in rpath_values$params via cell edits
    })

    # Reset diet matrix
    observeEvent(input$btn_reset_diet, {
      if (!is.null(original_params())) {
        rpath_values$params$diet <- data.table::as.data.table(original_params()$diet)
        showNotification("Diet matrix reset to original values", type = "message", duration = 3)
      }
    })

    # Calibration/Pedigree table
    output$calibration_table <- DT::renderDataTable({
      ecopath_data <- ecopath_import_reactive()

      if (is.null(ecopath_data)) {
        return(DT::datatable(
          data.frame(Message = "No ECOPATH data imported yet"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      # Check if pedigree data exists
      if (is.null(ecopath_data$pedigree_data) || nrow(ecopath_data$pedigree_data) == 0) {
        return(DT::datatable(
          data.frame(Message = "No pedigree/calibration data found in this database"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      tryCatch({
        pedigree <- ecopath_data$pedigree_data
        groups <- ecopath_data$group_data
        pedigree_levels <- ecopath_data$pedigree_levels

        # Merge group names
        if ("GroupID" %in% colnames(pedigree) && "GroupID" %in% colnames(groups)) {
          pedigree <- merge(pedigree,
                           groups[, c("GroupID", "GroupName"), drop = FALSE],
                           by = "GroupID",
                           all.x = TRUE)
        }

        # Create readable parameter names
        param_names <- list(
          "BiomassAreaInput" = "Biomass",
          "PBInput" = "P/B",
          "QBInput" = "Q/B",
          "DietComp" = "Diet Composition",
          "TCatchInput" = "Catch"
        )

        # Map variable names to readable names
        if ("VarName" %in% colnames(pedigree)) {
          pedigree$Parameter <- sapply(pedigree$VarName, function(x) {
            if (x %in% names(param_names)) param_names[[x]] else x
          })
        }

        # Get confidence level descriptions if available
        if (!is.null(pedigree_levels) && "LevelID" %in% colnames(pedigree) && "LevelID" %in% colnames(pedigree_levels)) {
          pedigree <- merge(pedigree,
                           pedigree_levels[, c("LevelID", "LevelName", "Confidence"), drop = FALSE],
                           by = "LevelID",
                           all.x = TRUE)
        }

        # Create display dataframe
        df <- data.frame(
          Group = if ("GroupName" %in% colnames(pedigree)) pedigree$GroupName else pedigree$GroupID,
          Parameter = if ("Parameter" %in% colnames(pedigree)) pedigree$Parameter else pedigree$VarName,
          Level_ID = pedigree$LevelID,
          stringsAsFactors = FALSE
        )

        # Add confidence if available
        if ("Confidence" %in% colnames(pedigree)) {
          df$Confidence <- pedigree$Confidence
        }

        # Add level name if available
        if ("LevelName" %in% colnames(pedigree)) {
          df$Level_Name <- pedigree$LevelName
        }

        # Sort by group and parameter
        df <- df[order(df$Group, df$Parameter), ]

        DT::datatable(
          df,
          options = list(
            pageLength = 25,
            scrollX = TRUE,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel'),
            columnDefs = list(
              list(className = 'dt-center', targets = 2:ncol(df)-1)
            )
          ),
          rownames = FALSE,
          caption = sprintf("Pedigree/calibration data for %d entries across %d groups",
                          nrow(df),
                          length(unique(df$Group)))
        )
      }, error = function(e) {
        message(sprintf("ERROR in calibration_table: %s", conditionMessage(e)))
        DT::datatable(
          data.frame(Error = paste("Error rendering calibration table:", conditionMessage(e))),
          options = list(dom = 't'),
          rownames = FALSE
        )
      })
    })

    # Download calibration data
    output$download_calibration <- downloadHandler(
      filename = function() {
        paste0("calibration_pedigree_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        ecopath_data <- ecopath_import_reactive()

        if (!is.null(ecopath_data) && !is.null(ecopath_data$pedigree_data)) {
          pedigree <- ecopath_data$pedigree_data
          groups <- ecopath_data$group_data
          pedigree_levels <- ecopath_data$pedigree_levels

          # Merge group names
          if ("GroupID" %in% colnames(pedigree) && "GroupID" %in% colnames(groups)) {
            pedigree <- merge(pedigree,
                             groups[, c("GroupID", "GroupName"), drop = FALSE],
                             by = "GroupID",
                             all.x = TRUE)
          }

          # Merge pedigree levels
          if (!is.null(pedigree_levels) && "LevelID" %in% colnames(pedigree)) {
            pedigree <- merge(pedigree,
                             pedigree_levels,
                             by = "LevelID",
                             all.x = TRUE)
          }

          write.csv(pedigree, file, row.names = FALSE)
        }
      }
    )

    # Comments/Notes table (from Auxillary table)
    output$comments_table <- DT::renderDataTable({
      ecopath_data <- ecopath_import_reactive()

      if (is.null(ecopath_data)) {
        return(DT::datatable(
          data.frame(Message = "No ECOPATH data imported yet"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      # Check if auxillary data exists
      if (is.null(ecopath_data$auxillary_data) || nrow(ecopath_data$auxillary_data) == 0) {
        return(DT::datatable(
          data.frame(Message = "No comments/tooltips data found in this database"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      tryCatch({
        # Source parser functions
        if (!exists("organize_auxillary_data", mode = "function")) {
          source("R/functions/auxillary_parser.R", local = TRUE)
        }

        # Organize auxillary data
        df <- organize_auxillary_data(
          ecopath_data$auxillary_data,
          ecopath_data$group_data,
          ecopath_data$fleet_data
        )

        if (nrow(df) == 0) {
          return(DT::datatable(
            data.frame(Message = "No comments with text found"),
            options = list(dom = 't'),
            rownames = FALSE
          ))
        }

        # Create display table (without DBID and ValueID_Full for cleaner view)
        display_df <- df[, c("Entity_Type", "Entity_Name", "Parameter", "Remark")]
        colnames(display_df) <- c("Type", "Entity", "Parameter", "Comment/Note")

        DT::datatable(
          display_df,
          options = list(
            pageLength = 25,
            scrollX = TRUE,
            scrollY = "500px",
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel'),
            columnDefs = list(
              list(className = 'dt-left', targets = 3)  # Left-align comments
            )
          ),
          rownames = FALSE,
          caption = sprintf("Comments and source citations for %d parameters across %d entities",
                          nrow(df),
                          length(unique(df$Entity_Name)))
        )
      }, error = function(e) {
        message(sprintf("ERROR in comments_table: %s", conditionMessage(e)))
        DT::datatable(
          data.frame(Error = paste("Error rendering comments table:", conditionMessage(e))),
          options = list(dom = 't'),
          rownames = FALSE
        )
      })
    })

    # Download comments data
    output$download_comments <- downloadHandler(
      filename = function() {
        paste0("comments_tooltips_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        ecopath_data <- ecopath_import_reactive()

        if (!is.null(ecopath_data) && !is.null(ecopath_data$auxillary_data)) {
          # Source parser functions
          if (!exists("organize_auxillary_data", mode = "function")) {
            source("R/functions/auxillary_parser.R", local = TRUE)
          }

          # Organize data
          df <- organize_auxillary_data(
            ecopath_data$auxillary_data,
            ecopath_data$group_data,
            ecopath_data$fleet_data
          )

          write.csv(df, file, row.names = FALSE)
        }
      }
    )

    # ===========================================================================
    # ECOPATH MASS BALANCE
    # ===========================================================================

    # Conversion status
    output$conversion_status <- renderUI({
      if (!is.null(rpath_values$params)) {
        HTML(paste0("
          <p style='color: green;'><i class='fa fa-check-circle'></i> <strong>Conversion successful!</strong></p>
          <p>Ready for mass balance modeling.</p>
        "))
      }
    })

    # Run Ecopath mass balance
    observeEvent(input$btn_run_ecopath, {
      if (is.null(rpath_values$params)) {
        showNotification("Please convert data to Rpath format first", type = "error")
        return()
      }

      tryCatch({
        showNotification("Running mass balance model...", type = "message")

        rpath_values$ecopath_model <- run_ecopath_balance(
          rpath_values$params,
          balance = input$balance_model
        )

        rpath_values$status <- "balanced"

        showNotification("Mass balance complete!", type = "message", duration = 3)

      }, error = function(e) {
        showNotification(paste("Ecopath error:", e$message), type = "error", duration = 10)
      })
    })

    # Ecopath status
    output$ecopath_status <- renderUI({
      if (!is.null(rpath_values$ecopath_model)) {
        HTML("<p style='color: green;'><i class='fa fa-check-circle'></i> <strong>Model balanced!</strong></p>")
      }
    })

    # Ecopath summary
    output$ecopath_summary <- renderPrint({
      if (is.null(rpath_values$ecopath_model)) {
        cat("No model results yet.\n\nRun mass balance to see results.")
      } else {
        print(summary(rpath_values$ecopath_model))
      }
    })

    # Ecopath results table
    output$ecopath_results_table <- DT::renderDataTable({
      req(rpath_values$ecopath_model)

      model <- rpath_values$ecopath_model

      # Map type values to names
      type_names <- c("Consumer", "Producer", "Detritus", "Fleet")
      type_display <- sapply(model$type, function(t) {
        if (is.na(t)) return("Unknown")
        type_names[t + 1]
      })

      # Calculate Production and Consumption
      production <- model$Biomass * model$PB
      consumption <- model$Biomass * model$QB

      # Create results data frame (keep numeric for formatStyle)
      results_df <- data.frame(
        Group = model$Group,
        Type = type_display,
        Biomass = round(model$Biomass, 3),
        PB = round(model$PB, 3),
        QB = round(model$QB, 3),
        EE = round(model$EE, 3),
        GE = round(model$GE, 3),
        TL = round(model$TL, 2),
        Production = round(production, 3),
        Consumption = round(consumption, 3),
        stringsAsFactors = FALSE
      )

      # Render with DT and conditional formatting
      dt <- DT::datatable(
        results_df,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          scrollY = "500px",
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          columnDefs = list(
            list(className = 'dt-center', targets = 1:9)
          )
        ),
        rownames = FALSE,
        class = "display compact",
        caption = "ECOPATH Mass-Balance Model Results"
      ) %>%
      # EE (Ecotrophic Efficiency) highlighting
      DT::formatStyle(
        'EE',
        backgroundColor = DT::styleInterval(
          cuts = c(0.05, 0.95, 1.0),
          values = c('#e8f4f8', 'white', '#fff3cd', '#ffebee')
        )
      ) %>%
      # GE (Gross Efficiency) highlighting
      DT::formatStyle(
        'GE',
        backgroundColor = DT::styleInterval(
          cuts = c(0, 0.5, 1.0),
          values = c('#ffebee', 'white', '#fff3cd', '#ffebee')
        )
      ) %>%
      # TL (Trophic Level) highlighting
      DT::formatStyle(
        'TL',
        backgroundColor = DT::styleInterval(
          cuts = c(5.0),
          values = c('white', '#fff3cd')
        )
      ) %>%
      # Biomass highlighting (negative or very low values)
      DT::formatStyle(
        'Biomass',
        backgroundColor = DT::styleInterval(
          cuts = c(0, 0.001),
          values = c('#ffebee', '#e8f4f8', 'white')
        )
      )

      return(dt)
    })

    # Download ecopath results
    output$download_ecopath_results <- downloadHandler(
      filename = function() {
        paste0("ecopath_results_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(rpath_values$ecopath_model)

        model <- rpath_values$ecopath_model

        # Map type values to names
        type_names <- c("Consumer", "Producer", "Detritus", "Fleet")
        type_display <- sapply(model$type, function(t) {
          if (is.na(t)) return("Unknown")
          type_names[t + 1]
        })

        # Calculate Production and Consumption
        production <- model$Biomass * model$PB
        consumption <- model$Biomass * model$QB

        results_df <- data.frame(
          Group = model$Group,
          Type = type_display,
          Biomass = model$Biomass,
          PB = model$PB,
          QB = model$QB,
          EE = model$EE,
          GE = model$GE,
          TL = model$TL,
          Production = production,
          Consumption = consumption,
          stringsAsFactors = FALSE
        )

        write.csv(results_df, file, row.names = FALSE)
      }
    )

    # Calculate MTI
    observeEvent(input$btn_calc_mti, {
      if (is.null(rpath_values$params)) {
        showNotification("Please convert data to Rpath format first", type = "error")
        return()
      }

      if (is.null(rpath_values$ecopath_model)) {
        showNotification("Please run mass balance first", type = "error")
        return()
      }

      tryCatch({
        showNotification("Calculating Mixed Trophic Impacts...", type = "message")

        # MTI requires both params and balanced model
        rpath_values$mti <- calculate_rpath_mti(rpath_values$params, rpath_values$ecopath_model)

        showNotification("MTI calculation complete!", type = "message", duration = 3)

      }, error = function(e) {
        showNotification(paste("MTI error:", e$message), type = "error", duration = 10)
      })
    })

    # MTI status
    output$mti_status <- renderUI({
      if (!is.null(rpath_values$mti)) {
        HTML("<p style='color: green;'><i class='fa fa-check-circle'></i> <strong>MTI calculated!</strong></p>")
      }
    })

    # MTI heatmap
    output$mti_heatmap <- renderPlot({
      if (is.null(rpath_values$mti)) {
        plot.new()
        text(0.5, 0.5, "Calculate MTI to see results", cex = 1.5)
      } else {
        heatmap(
          rpath_values$mti,
          main = "Mixed Trophic Impacts",
          xlab = "Impacted Group",
          ylab = "Impacting Group",
          col = colorRampPalette(c("red", "white", "blue"))(100),
          scale = "none",
          margins = c(10, 10)
        )
      }
    })

    # Run Ecosim simulation
    observeEvent(input$btn_run_ecosim, {
      if (is.null(rpath_values$ecopath_model)) {
        showNotification("Please run mass balance first", type = "error")
        return()
      }

      tryCatch({
        # Get ECOPATH data for scenario parameters
        ecopath_data <- ecopath_import_reactive()

        # Check if a scenario is selected
        selected_scenario_id <- as.numeric(input$selected_scenario)
        use_scenario <- !is.null(selected_scenario_id) && selected_scenario_id > 0

        if (use_scenario && !is.null(ecopath_data$ecosim_scenarios)) {
          # Get scenario data
          scenarios <- ecopath_data$ecosim_scenarios$scenarios
          scenario_data <- scenarios[scenarios$ScenarioID == selected_scenario_id, ]

          if (nrow(scenario_data) > 0) {
            scenario_name <- scenario_data$ScenarioName[1]
            showNotification(
              paste("Running", input$sim_years, "year simulation with scenario:", scenario_name),
              type = "message"
            )
          } else {
            showNotification("Selected scenario not found. Using default parameters.", type = "warning")
            use_scenario <- FALSE
          }
        } else {
          showNotification(paste("Running", input$sim_years, "year simulation (default parameters)..."),
                          type = "message")
        }

        # Setup scenario
        if (use_scenario) {
          # Apply ECOSIM scenario parameters
          scenario <- setup_ecosim_scenario_with_data(
            rpath_values$ecopath_model,
            ecosim_data = ecopath_data$ecosim_scenarios,
            scenario_id = selected_scenario_id,
            years = input$sim_years,
            method = input$sim_method
          )
        } else {
          # Use default parameters
          scenario <- setup_ecosim_scenario(
            rpath_values$ecopath_model,
            years = input$sim_years,
            method = input$sim_method
          )
        }

        # Run simulation
        rpath_values$ecosim <- run_ecosim_simulation(scenario)

        showNotification("Simulation complete!", type = "message", duration = 3)

      }, error = function(e) {
        showNotification(paste("Ecosim error:", e$message), type = "error", duration = 10)
      })
    })

    # Ecosim status
    output$ecosim_status <- renderUI({
      if (!is.null(rpath_values$ecosim)) {
        HTML(paste0("
          <p style='color: green;'><i class='fa fa-check-circle'></i> <strong>Simulation complete!</strong></p>
          <p>Years simulated: ", input$sim_years, "</p>
        "))
      }
    })

    # Ecosim biomass plot
    output$ecosim_biomass_plot <- renderPlot({
      if (is.null(rpath_values$ecosim)) {
        plot.new()
        text(0.5, 0.5, "Run simulation to see results", cex = 1.5)
      } else {
        plot_ecosim_results(rpath_values$ecosim, type = "biomass")
      }
    })

    # ===========================================================================
    # ADVANCED ECOSIM VISUALIZATION
    # ===========================================================================

    # Group selector for custom plots
    output$group_selector <- renderUI({
      if (is.null(rpath_values$params)) {
        return(helpText("Run model first"))
      }

      groups <- rpath_values$params$model$Group[rpath_values$params$model$Type < 3]

      selectInput(
        ns("selected_groups"),
        "Select Groups to Plot",
        choices = groups,
        selected = groups[1:min(5, length(groups))],
        multiple = TRUE
      )
    })

    # Update plot on button click
    observeEvent(input$btn_update_plot, {
      if (is.null(rpath_values$ecosim)) return()

      # Trigger plot update by changing a reactive value
      rpath_values$plot_trigger <- runif(1)
    })

    # Download biomass CSV
    output$download_biomass_csv <- downloadHandler(
      filename = function() {
        paste0("ecosim_biomass_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        if (!is.null(rpath_values$ecosim)) {
          biomass_data <- extract_ecosim_biomass(rpath_values$ecosim)
          write.csv(biomass_data, file, row.names = FALSE)
        }
      }
    )

    # Download full simulation RDS
    output$download_simulation_rds <- downloadHandler(
      filename = function() {
        paste0("ecosim_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      },
      content = function(file) {
        if (!is.null(rpath_values$ecosim)) {
          saveRDS(rpath_values$ecosim, file)
        }
      }
    )

    # ===========================================================================
    # MODEL DIAGNOSTICS
    # ===========================================================================

    # Calculate and display diagnostics
    observeEvent(input$btn_calc_diagnostics, {
      if (is.null(rpath_values$ecopath_model)) {
        showNotification("Please run mass balance first", type = "error")
        return()
      }

      tryCatch({
        showNotification("Calculating model diagnostics...", type = "message")

        # Store diagnostics
        model <- rpath_values$params$model

        # Calculate key metrics
        total_biomass <- sum(model$Biomass[model$Type < 3], na.rm = TRUE)
        mean_tl <- mean(model$TL[model$Type < 3], na.rm = TRUE)
        total_pp <- sum(model$Biomass[model$Type == 1] * model$PB[model$Type == 1], na.rm = TRUE)

        rpath_values$diagnostics <- list(
          total_biomass = total_biomass,
          mean_trophic_level = mean_tl,
          primary_production = total_pp,
          n_groups = sum(model$Type < 3),
          n_producers = sum(model$Type == 1),
          n_consumers = sum(model$Type == 0)
        )

        showNotification("Diagnostics calculated!", type = "message", duration = 3)

      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    # Display diagnostics summary
    output$diagnostics_summary <- renderUI({
      if (is.null(rpath_values$diagnostics)) {
        return(HTML("<p><em>Click 'Calculate Diagnostics' to see model quality metrics.</em></p>"))
      }

      diag <- rpath_values$diagnostics

      HTML(paste0("
        <div class='alert alert-success'>
          <h4><i class='fa fa-check-circle'></i> Model Summary</h4>
          <table class='table table-condensed'>
            <tr><td><strong>Total Biomass:</strong></td><td>", round(diag$total_biomass, 2), " t/km²</td></tr>
            <tr><td><strong>Mean Trophic Level:</strong></td><td>", round(diag$mean_trophic_level, 2), "</td></tr>
            <tr><td><strong>Primary Production:</strong></td><td>", round(diag$primary_production, 2), " t/km²/year</td></tr>
            <tr><td><strong>Number of Groups:</strong></td><td>", diag$n_groups, "</td></tr>
            <tr><td><strong>Producers:</strong></td><td>", diag$n_producers, "</td></tr>
            <tr><td><strong>Consumers:</strong></td><td>", diag$n_consumers, "</td></tr>
          </table>
        </div>
      "))
    })

    # Trophic pyramid plot
    output$trophic_pyramid <- renderPlot({
      if (is.null(rpath_values$params)) {
        plot.new()
        text(0.5, 0.5, "Run model to see trophic structure", cex = 1.5)
        return()
      }

      model <- rpath_values$params$model
      living <- model[model$Type < 3, ]

      # Calculate biomass by trophic level
      tl_bins <- cut(living$TL, breaks = seq(1, ceiling(max(living$TL, na.rm = TRUE)), by = 0.5))
      biomass_by_tl <- tapply(living$Biomass, tl_bins, sum, na.rm = TRUE)

      barplot(
        rev(biomass_by_tl),
        main = "Trophic Pyramid",
        xlab = "Biomass (t/km²)",
        ylab = "Trophic Level",
        horiz = TRUE,
        col = colorRampPalette(c("#2ecc71", "#f39c12", "#e74c3c"))(length(biomass_by_tl)),
        las = 1
      )
    })

    # Download handlers for diagnostics
    output$download_ecopath_params <- downloadHandler(
      filename = function() {
        paste0("ecopath_parameters_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        if (!is.null(rpath_values$params)) {
          write.csv(rpath_values$params$model, file, row.names = FALSE)
        }
      }
    )

    output$download_diet_matrix <- downloadHandler(
      filename = function() {
        paste0("diet_matrix_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        if (!is.null(rpath_values$params)) {
          write.csv(rpath_values$params$diet, file, row.names = FALSE)
        }
      }
    )

    output$download_model_summary <- downloadHandler(
      filename = function() {
        paste0("model_summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
      },
      content = function(file) {
        if (!is.null(rpath_values$ecopath_model)) {
          sink(file)
          print(summary(rpath_values$ecopath_model))
          sink()
        }
      }
    )

    # ===========================================================================
    # SENSITIVITY ANALYSIS
    # ===========================================================================

    # Parameter selector for sensitivity analysis
    output$sensitivity_param_selector <- renderUI({
      if (is.null(rpath_values$params)) {
        return(helpText("Convert model first"))
      }

      groups <- rpath_values$params$model$Group[rpath_values$params$model$Type < 3]

      tagList(
        selectInput(ns("sens_group"), "Select Group", choices = groups),
        selectInput(ns("sens_parameter"), "Select Parameter",
                   choices = c("Biomass" = "B", "P/B" = "PB", "Q/B" = "QB"))
      )
    })

    # Run sensitivity analysis
    observeEvent(input$btn_run_sensitivity, {
      if (is.null(rpath_values$ecopath_model)) {
        showNotification("Please run mass balance first", type = "error")
        return()
      }

      req(input$sens_group, input$sens_parameter)

      tryCatch({
        showNotification("Running sensitivity analysis...", type = "message")

        rpath_values$sensitivity <- run_sensitivity_analysis(
          rpath_values$ecopath_model,
          group = input$sens_group,
          parameter = input$sens_parameter,
          range = input$sensitivity_range,
          steps = input$sensitivity_steps
        )

        showNotification("Sensitivity analysis complete!", type = "message", duration = 3)

      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
      })
    })

    # Sensitivity status
    output$sensitivity_status <- renderUI({
      if (!is.null(rpath_values$sensitivity)) {
        HTML("<p style='color: green;'><i class='fa fa-check-circle'></i> <strong>Analysis complete!</strong></p>")
      }
    })

    # Sensitivity plot
    output$sensitivity_plot <- renderPlot({
      if (is.null(rpath_values$sensitivity)) {
        plot.new()
        text(0.5, 0.5, "Run sensitivity analysis to see results", cex = 1.5)
      } else {
        # Plot sensitivity results
        sens <- rpath_values$sensitivity

        par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
        plot(
          sens$param_values,
          sens$total_biomass,
          type = "b",
          pch = 19,
          col = "#3498db",
          lwd = 2,
          main = paste("Sensitivity:", input$sens_group, input$sens_parameter),
          xlab = paste(input$sens_parameter, "Value"),
          ylab = "Total System Biomass (t/km²)",
          cex.main = 1.3,
          cex.lab = 1.1
        )
        grid()
        abline(v = sens$baseline_value, col = "red", lty = 2, lwd = 2)
        legend("topright", legend = "Baseline", col = "red", lty = 2, lwd = 2)
      }
    })

    # ===========================================================================
    # FISHING SCENARIOS
    # ===========================================================================

    # Fleet selector
    output$fleet_selector <- renderUI({
      if (is.null(rpath_values$params)) {
        return(helpText("Convert model first"))
      }

      fleets <- rpath_values$params$model$Group[rpath_values$params$model$Type == 3]

      if (length(fleets) == 0) {
        return(helpText("No fleets found in model"))
      }

      selectInput(ns("selected_fleet"), "Select Fleet",
                 choices = fleets,
                 selected = fleets[1])
    })

    # Run fishing scenario
    observeEvent(input$btn_run_scenario, {
      if (is.null(rpath_values$ecopath_model)) {
        showNotification("Please run mass balance first", type = "error")
        return()
      }

      req(input$selected_fleet)

      tryCatch({
        showNotification(paste("Running fishing scenario:",
                             input$effort_multiplier, "× effort..."),
                       type = "message")

        rpath_values$fishing_scenario <- evaluate_fishing_scenario(
          rpath_values$ecopath_model,
          fleet = input$selected_fleet,
          effort_multiplier = input$effort_multiplier,
          years = input$scenario_years
        )

        showNotification("Scenario complete!", type = "message", duration = 3)

      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
      })
    })

    # Scenario status
    output$scenario_status <- renderUI({
      if (!is.null(rpath_values$fishing_scenario)) {
        HTML(paste0("
          <p style='color: green;'><i class='fa fa-check-circle'></i> <strong>Scenario complete!</strong></p>
          <p>Effort multiplier: ", input$effort_multiplier, "×</p>
          <p>Duration: ", input$scenario_years, " years</p>
        "))
      }
    })

    # Scenario plot
    output$scenario_plot <- renderPlot({
      if (is.null(rpath_values$fishing_scenario)) {
        plot.new()
        text(0.5, 0.5, "Run fishing scenario to see results", cex = 1.5)
      } else {
        plot_ecosim_results(rpath_values$fishing_scenario, type = "biomass")
      }
    })

    # Return reactive values for external access if needed
    return(rpath_values)
  })
}
