#' Download Handlers and Export Server Module
#'
#' Handles download handlers for example datasets and metaweb export.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param net_reactive ReactiveVal holding the igraph network
#' @param info_reactive ReactiveVal holding the species info data frame
download_server <- function(input, output, session, net_reactive, info_reactive) {

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
  # EXPORT CURRENT METAWEB
  # ============================================================================

  output$download_current_metaweb <- downloadHandler(
    filename = function() {
      # Get filename from input, add .Rdata extension
      filename <- input$export_metaweb_name
      if (is.null(filename) || filename == "") {
        filename <- "my_metaweb"
      }
      # Remove any existing extension and add .Rdata
      filename <- tools::file_path_sans_ext(filename)
      paste0(filename, ".Rdata")
    },
    content = function(file) {
      tryCatch({
        # Get current network and info
        current_net <- net_reactive()
        current_info <- info_reactive()

        # Use export function
        export_metaweb_rda(current_net, current_info, file)

        showNotification(
          paste0("Metaweb exported successfully: ", basename(file)),
          type = "message",
          duration = 5
        )

      }, error = function(e) {
        showNotification(
          paste0("Export failed: ", e$message),
          type = "error",
          duration = 10
        )
        # Still create an empty file so download doesn't fail
        save(list = character(0), file = file)
      })
    }
  )
}
