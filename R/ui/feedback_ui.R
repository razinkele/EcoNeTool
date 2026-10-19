# =============================================================================
# Feedback Dashboard tab (port of the MarineSABRES feedback management)
# =============================================================================
# The body is rendered server-side (output$feedback_dashboard_ui) because what
# a session may see is decided per session: everyone gets the counts and the
# Date / Type / Title / Status list; admins (FEEDBACK_ADMIN_KEY in the URL, or
# a session unlocked with the admin password) also get descriptions, the
# detail view, resolve actions and Duplicate Detection.
# tabName stays "feedback_admin" so existing bookmarks keep working.
# =============================================================================

feedback_ui <- function() {
  tabItem(
    tabName = "feedback_admin",
    tags$h3(style = "margin-bottom: 20px;", icon("chart-bar"), " Feedback Dashboard"),
    uiOutput("feedback_dashboard_ui")
  )
}

#' Body of the Feedback Dashboard (called from renderUI)
#' @param admin TRUE to include the admin-only Duplicate Detection tab
feedback_dashboard_body <- function(admin = FALSE) {
  vbox <- function(id) column(width = 3, valueBoxOutput(id, width = 12))

  dashboard_tab <- tabPanel(
    title = tagList(icon("list"), " Feedback Dashboard"), value = "dashboard",
    fluidRow(
      vbox("feedback_vbox_total"), vbox("feedback_vbox_bugs"),
      vbox("feedback_vbox_suggestions"), vbox("feedback_vbox_recent")
    ),
    fluidRow(
      vbox("feedback_vbox_github"), vbox("feedback_vbox_local"),
      vbox("feedback_vbox_resolved")
    ),
    fluidRow(
      column(width = 3,
             selectInput("feedback_status_filter", "Status:",
                         choices = c("Open" = "open", "Resolved" = "resolved", "All" = "all"),
                         selected = "open")),
      column(width = 3,
             tags$div(style = "margin-top: 32px;",
                      actionButton("feedback_refresh", "Refresh", icon = icon("sync-alt"),
                                   class = "btn-outline-secondary btn-sm")))
    ),
    fluidRow(
      column(width = 12,
             if (admin) {
               tags$p(class = "text-muted",
                      "Click a row for full details; use Mark addressed to resolve an open report.")
             },
             DT::dataTableOutput("feedback_table"))
    )
  )

  duplicates_tab <- if (admin) {
    tabPanel(
      title = tagList(icon("copy"), " Duplicate Detection"), value = "duplicates",
      fluidRow(
        column(width = 4,
               box(width = 12, status = "warning", solidHeader = TRUE,
                   title = "Similarity threshold",
                   sliderInput("feedback_similarity_threshold", label = NULL,
                               min = 0.5, max = 0.95, value = 0.7, step = 0.05),
                   actionButton("feedback_find_duplicates", "Find duplicates",
                                icon = icon("search"), class = "btn-warning btn-block"),
                   tags$br(),
                   actionButton("feedback_recalculate", "Reload & recalculate",
                                icon = icon("sync-alt"),
                                class = "btn-outline-secondary btn-block"),
                   uiOutput("feedback_scale_note"))),
        column(width = 8,
               box(width = 12, status = "warning", solidHeader = TRUE,
                   title = "Candidate duplicates",
                   DT::dataTableOutput("feedback_duplicates_table")))
      )
    )
  }

  do.call(tabBox, c(
    list(id = "feedback_tabs", width = 12, side = "left", status = "primary",
         solidHeader = FALSE, collapsible = FALSE),
    Filter(Negate(is.null), list(dashboard_tab, duplicates_tab))
  ))
}
