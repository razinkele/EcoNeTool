# Simple Test: Verify Progress$new() works in observeEvent
# Run this to verify progress updates work correctly

library(shiny)

ui <- fluidPage(
  titlePanel("Simple Progress Test"),
  actionButton("start", "Start Process (10 items)"),
  hr(),
  verbatimTextOutput("status")
)

server <- function(input, output, session) {

  output$status <- renderPrint({
    cat("Click 'Start Process' button to test progress indicator.\n")
    cat("You should see a progress bar appear at the top-right of the window.\n")
  })

  observeEvent(input$start, {
    cat("\n=== TEST STARTED ===\n")

    # Create Shiny Progress object
    progress <- Progress$new()
    on.exit(progress$close())

    progress$set(message = "Processing", value = 0)

    for (i in 1:10) {
      # Update progress
      progress$set(
        value = i / 10,
        detail = sprintf("Item %d of 10", i)
      )

      cat(sprintf("[TEST] Progress: %d/10\n", i))

      # Simulate work
      Sys.sleep(0.5)
    }

    cat("[TEST] COMPLETED\n")

    showNotification("Process completed successfully!", type = "message")
  })
}

cat("\n===========================================\n")
cat("PROGRESS INDICATOR TEST\n")
cat("===========================================\n\n")
cat("INSTRUCTIONS:\n")
cat("1. Click 'Start Process' button\n")
cat("2. Watch for progress bar in top-right corner\n")
cat("3. Progress should update from 0% to 100%\n")
cat("4. Check R console for [TEST] messages\n\n")
cat("If progress bar updates in real-time: ✓ WORKING\n")
cat("If progress bar stuck or appears only at end: ✗ NOT WORKING\n\n")

shinyApp(ui, server)
