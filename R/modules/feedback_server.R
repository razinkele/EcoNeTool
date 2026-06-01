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
}
