# Test Progress Indicator for Taxonomic API
# This creates a minimal test app to verify progress updates work

library(shiny)

ui <- fluidPage(
  titlePanel("Progress Indicator Test"),

  sidebarLayout(
    sidebarPanel(
      actionButton("start", "Start Long Process"),
      hr(),
      h4("Test Parameters:"),
      numericInput("num_items", "Number of items:", 10, min = 5, max = 50)
    ),

    mainPanel(
      h3("Progress Status:"),
      verbatimTextOutput("debug_output"),
      uiOutput("progress_ui"),
      hr(),
      verbatimTextOutput("results")
    )
  )
)

server <- function(input, output, session) {

  # Reactive value for progress
  progress_data <- reactiveVal(NULL)

  # Reactive value for results
  results_list <- reactiveVal(NULL)

  # Debug output
  output$debug_output <- renderPrint({
    prog <- progress_data()
    if (is.null(prog)) {
      cat("[DEBUG] No progress data\n")
    } else {
      cat(sprintf("[DEBUG] Progress: %d/%d (%d%%) - Item: %s\n",
                  prog$current,
                  prog$total,
                  prog$percent,
                  prog$item_name))
    }
  })

  # Progress UI
  output$progress_ui <- renderUI({
    prog <- progress_data()
    if (is.null(prog)) return(NULL)

    tagList(
      div(
        style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 5px; border-left: 4px solid #0066cc;",

        h4(style = "margin-top: 0; color: #333;",
           icon("spinner", class = "fa-spin"),
           " Processing in Progress"),

        div(
          class = "progress",
          style = "height: 25px; margin-bottom: 15px;",
          div(
            class = "progress-bar progress-bar-striped active",
            role = "progressbar",
            style = sprintf("width: %d%%; background-color: #0066cc;", prog$percent),
            strong(sprintf("%d%%", prog$percent))
          )
        ),

        div(
          style = "margin-bottom: 10px;",
          strong("Current Item: "),
          code(style = "background-color: #fff; padding: 2px 6px;", prog$item_name)
        ),

        div(
          style = "margin-bottom: 10px;",
          strong("Progress: "),
          sprintf("Item %d of %d", prog$current, prog$total)
        ),

        div(
          style = "margin-top: 10px; padding: 8px; background-color: #fff; border-radius: 3px;",
          icon("info-circle"),
          " ",
          prog$message
        )
      )
    )
  })

  # Results output
  output$results <- renderPrint({
    res <- results_list()
    if (!is.null(res)) {
      cat("=== COMPLETED ===\n")
      cat(sprintf("Processed %d items\n", length(res)))
      cat("\nResults:\n")
      print(res)
    }
  })

  # Long process simulation
  observeEvent(input$start, {
    num <- input$num_items

    cat("\n=== STARTING LONG PROCESS ===\n")
    cat(sprintf("Processing %d items...\n", num))

    # Clear previous results
    results_list(NULL)

    # Initialize progress
    progress_data(list(
      current = 0,
      total = num,
      percent = 0,
      item_name = "Starting...",
      message = sprintf("Initializing process for %d items", num)
    ))

    cat("[TEST] Initial progress set\n")
    Sys.sleep(0.5)  # Give UI time to render

    # Process items
    results <- list()
    for (i in 1:num) {
      item_name <- paste0("Item_", i)

      # Update progress
      percent <- round((i / num) * 100)
      progress_data(list(
        current = i,
        total = num,
        percent = percent,
        item_name = item_name,
        message = sprintf("Processing item %d of %d. Please wait...", i, num)
      ))

      cat(sprintf("[TEST] Progress updated: %d/%d (%d%%) - %s\n", i, num, percent, item_name))

      # Simulate work
      Sys.sleep(0.5)

      # Store result
      results[[i]] <- list(
        name = item_name,
        value = runif(1),
        timestamp = Sys.time()
      )
    }

    # Clear progress
    progress_data(NULL)

    # Show results
    results_list(results)

    cat("[TEST] Process completed\n")
  })
}

# Run the test app
shinyApp(ui = ui, server = server)
