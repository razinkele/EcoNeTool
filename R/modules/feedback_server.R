# Feedback module - submit handler + (Task 5) gated admin panel.
# Direct pattern: receives input/output/session; outputs on the shared object.

feedback_server <- function(input, output, session) {

  # Boot: surface feedback-store health in warnings (shiny-server logs).
  local({
    s <- tryCatch(feedback_summary(), error = function(e) NULL)
    if (is.null(s)) {
      warning("[feedback] store unreachable at boot", call. = FALSE)
    } else {
      warning(sprintf("[feedback] store OK at boot: open=%d total=%d", s$open, s$total),
              call. = FALSE)
    }
  })

  # ---- Submit ----------------------------------------------------------------
  observeEvent(input$feedback_submit, {
    desc <- input$feedback_description
    if (is.null(desc) || !nzchar(trimws(desc))) {
      showNotification("Please describe the bug or suggestion.", type = "warning")
      return()
    }
    res <- feedback_insert(
      type            = input$feedback_type,
      description     = desc,
      submitter_email = input$feedback_email,
      app_version     = tryCatch(
        get_version("short"),
        error = function(e) {
          warning(sprintf("[feedback_server] get_version failed: %s", conditionMessage(e)),
                  call. = FALSE)
          NA_character_
        }
      ),
      tab             = input$sidebar_menu,
      session_id      = session$token
    )
    if (isTRUE(res$success)) {
      showNotification("Thanks - your feedback was saved.", type = "message")
      updateTextAreaInput(session, "feedback_description", value = "")
      updateTextInput(session, "feedback_email", value = "")
      toggleModal(session, "feedback_modal", toggle = "close")
    } else {
      # Keep the modal open + fields intact so the user never loses their report.
      showNotification(
        "Couldn't save your feedback - please try again; if it keeps failing, email the maintainer.",
        type = "error", duration = 10)
    }
  })

  # ---- Admin (URL-key gated; fail closed) ------------------------------------
  is_admin <- reactive({
    key <- Sys.getenv("FEEDBACK_ADMIN_KEY", "")
    if (!nzchar(key)) return(FALSE)                      # unset env -> no admin
    qkey <- tryCatch(
      shiny::parseQueryString(session$clientData$url_search)$admin,
      error = function(e) NULL)
    isTRUE(identical(key, qkey))                          # nzchar guard ran first
  })

  feedback_refresh <- reactiveVal(0)

  output$feedback_admin_ui <- renderUI({
    if (!is_admin()) return(NULL)                         # panel not sent to non-admins
    tagList(
      fluidRow(
        column(4, valueBoxOutput("feedback_box_open", width = 12)),
        column(4, valueBoxOutput("feedback_box_addressed", width = 12)),
        column(4, valueBoxOutput("feedback_box_total", width = 12))
      ),
      selectInput("feedback_filter", "Show:",
                  c("Open" = "open", "Addressed" = "addressed", "All" = "all"),
                  selected = "open"),
      DT::dataTableOutput("feedback_admin_table"),
      tags$hr(),
      textInput("feedback_resolution", "Resolution note (for selected row):"),
      actionButton("feedback_mark", "Mark selected addressed",
                   icon = icon("check"), class = "btn-primary")
    )
  })

  .fb_admin_data <- reactive({
    feedback_refresh()
    req(is_admin())
    st <- if (identical(input$feedback_filter, "all")) NULL else input$feedback_filter
    feedback_list(status = st, include_email = TRUE)
  })

  output$feedback_admin_table <- DT::renderDataTable({
    df <- .fb_admin_data()
    DT::datatable(df, escape = TRUE, selection = "single", rownames = FALSE,
                  options = list(pageLength = 15, scrollX = TRUE))
  })

  observeEvent(input$feedback_mark, {
    req(is_admin())
    sel <- input$feedback_admin_table_rows_selected
    df <- .fb_admin_data()
    if (length(sel) != 1 || nrow(df) == 0 || sel > nrow(df)) {
      showNotification("Select one row first.", type = "warning"); return()
    }
    note <- input$feedback_resolution
    res <- feedback_mark_addressed(
      id = df$id[sel],
      resolution_note = if (!is.null(note) && nzchar(note)) note else NA_character_,
      addressed_by = "admin")
    if (isTRUE(res$success)) {
      showNotification("Marked addressed.", type = "message")
      feedback_refresh(feedback_refresh() + 1)
    } else {
      showNotification("Couldn't mark addressed (see server log).", type = "error")
    }
  })

  output$feedback_box_open <- renderValueBox({
    req(is_admin())
    valueBox(feedback_summary()$open, "Open", icon = icon("inbox"), color = "warning")
  })
  output$feedback_box_addressed <- renderValueBox({
    req(is_admin())
    valueBox(feedback_summary()$addressed, "Addressed", icon = icon("check"), color = "success")
  })
  output$feedback_box_total <- renderValueBox({
    req(is_admin())
    valueBox(feedback_summary()$total, "Total", icon = icon("list"), color = "primary")
  })
}
