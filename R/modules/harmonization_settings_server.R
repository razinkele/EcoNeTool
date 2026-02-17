# Harmonization Settings Server Module

harmonization_settings_server <- function(input, output, session) {
  
  rv <- reactiveValues(
    config = HARMONIZATION_CONFIG,
    unsaved_changes = FALSE
  )
  
  # INITIALIZE
  observe({
    if (file.exists("config/harmonization_custom.json")) {
      rv$config <- load_harmonization_config("config/harmonization_custom.json")
    }
    updateSliderInput(session, "harm_thresh_MS1_MS2", value = rv$config$size_thresholds$MS1_MS2)
    updateSliderInput(session, "harm_thresh_MS2_MS3", value = rv$config$size_thresholds$MS2_MS3)
    updateSliderInput(session, "harm_thresh_MS3_MS4", value = rv$config$size_thresholds$MS3_MS4)
    updateSliderInput(session, "harm_thresh_MS4_MS5", value = rv$config$size_thresholds$MS4_MS5)
    updateSliderInput(session, "harm_thresh_MS5_MS6", value = rv$config$size_thresholds$MS5_MS6)
    updateSliderInput(session, "harm_thresh_MS6_MS7", value = rv$config$size_thresholds$MS6_MS7)
  })
  
  # UPDATE CONFIG
  observe({
    rv$config$size_thresholds$MS1_MS2 <- input$harm_thresh_MS1_MS2
    rv$config$size_thresholds$MS2_MS3 <- input$harm_thresh_MS2_MS3
    rv$config$size_thresholds$MS3_MS4 <- input$harm_thresh_MS3_MS4
    rv$config$size_thresholds$MS4_MS5 <- input$harm_thresh_MS4_MS5
    rv$config$size_thresholds$MS5_MS6 <- input$harm_thresh_MS5_MS6
    rv$config$size_thresholds$MS6_MS7 <- input$harm_thresh_MS6_MS7
    rv$unsaved_changes <- TRUE
  })
  
  # SAVE CONFIGURATION
  observeEvent(input$harm_save_config, {
    tryCatch({
      assign("HARMONIZATION_CONFIG", rv$config, envir = globalenv())
      save_harmonization_config(rv$config, "config/harmonization_custom.json")
      rv$unsaved_changes <- FALSE
      
      output$harm_status_message <- renderUI({
        div(class = "alert alert-success", icon("check-circle"), " Configuration saved!")
      })
      showNotification("Configuration saved!", type = "message", duration = 3)
    }, error = function(e) {
      output$harm_status_message <- renderUI({
        div(class = "alert alert-danger", icon("exclamation-circle"), " Error: ", e$message)
      })
    })
  })
  
  # RESET TO DEFAULTS
  observeEvent(input$harm_reset_defaults, {
    source("R/config/harmonization_config.R", local = FALSE)
    rv$config <- HARMONIZATION_CONFIG
    updateSliderInput(session, "harm_thresh_MS1_MS2", value = 0.1)
    updateSliderInput(session, "harm_thresh_MS2_MS3", value = 1.0)
    updateSliderInput(session, "harm_thresh_MS3_MS4", value = 5.0)
    updateSliderInput(session, "harm_thresh_MS4_MS5", value = 20.0)
    updateSliderInput(session, "harm_thresh_MS5_MS6", value = 50.0)
    updateSliderInput(session, "harm_thresh_MS6_MS7", value = 150.0)
    rv$unsaved_changes <- TRUE
    showNotification("Reset to defaults", type = "warning", duration = 3)
  })
  
  # SIZE DISTRIBUTION PREVIEW
  output$harm_size_distribution_plot <- renderPlot({
    req(input$harm_preview_size)
    
    isolate({
      cache_dir <- "cache/taxonomy"
      if (!dir.exists(cache_dir)) {
        plot.new()
        text(0.5, 0.5, "No cached data available", cex = 1.5)
        return()
      }
      
      cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
      sample_files <- sample(cache_files, min(100, length(cache_files)))
      sizes <- numeric()
      
      for (file in sample_files) {
        data <- tryCatch(readRDS(file), error = function(e) NULL)
        if (!is.null(data) && !is.null(data$max_length_cm)) {
          sizes <- c(sizes, data$max_length_cm)
        }
      }
      
      hist(log10(sizes + 0.01), breaks = 30, col = "lightblue", border = "white",
           main = paste("Size Distribution (", length(sizes), " species)"),
           xlab = "log10(Size in cm)", ylab = "Frequency")
      
      abline(v = log10(rv$config$size_thresholds$MS1_MS2), col = "red", lwd = 2, lty = 2)
      abline(v = log10(rv$config$size_thresholds$MS2_MS3), col = "orange", lwd = 2, lty = 2)
      abline(v = log10(rv$config$size_thresholds$MS3_MS4), col = "yellow", lwd = 2, lty = 2)
      abline(v = log10(rv$config$size_thresholds$MS4_MS5), col = "green", lwd = 2, lty = 2)
      abline(v = log10(rv$config$size_thresholds$MS5_MS6), col = "blue", lwd = 2, lty = 2)
      abline(v = log10(rv$config$size_thresholds$MS6_MS7), col = "purple", lwd = 2, lty = 2)
    })
  })
  
  # ECOSYSTEM PROFILE DETAILS
  output$harm_profile_details <- renderUI({
    req(input$harm_active_profile)
    profile <- rv$config$profiles[[input$harm_active_profile]]
    tagList(
      h5("Description:"),
      p(profile$description),
      h5("Size Multiplier:"),
      p(paste0(profile$size_multiplier, "x"))
    )
  })
  
  # EXPORT
  output$harm_export_json <- downloadHandler(
    filename = function() {
      paste0("harmonization_config_", Sys.Date(), ".json")
    },
    content = function(file) {
      export_config_json(file, rv$config)
    }
  )
  
  # IMPORT
  observeEvent(input$harm_import_json, {
    req(input$harm_import_json)
    tryCatch({
      imported_config <- import_config_json(input$harm_import_json$datapath)
      rv$config <- imported_config
      rv$unsaved_changes <- TRUE
      showNotification("Configuration imported!", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Import failed:", e$message), type = "error", duration = 5)
    })
  })
}
