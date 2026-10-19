# Feedback module - submit handler + Feedback Dashboard (MarineSABRES port).
# Direct pattern: receives input/output/session; outputs on the shared object.
#
# Every admin-only observer re-checks .feedback_admin_allowed() itself: input
# IDs are client-controlled, so hiding a button is not authorisation (a
# browser console can fire Shiny.setInputValue('feedback_do_mark', 1)).

#' May this session use the admin tools of the Feedback Dashboard?
#'
#' Fail closed. Either the FEEDBACK_ADMIN_KEY URL key (?admin=<key>) or a
#' session unlocked with the admin password - strict: an instance without
#' ECONETOOL_ADMIN_PASSWORD_HASH grants nothing through the password path.
#' @keywords internal
.feedback_admin_allowed <- function(url_search, unlocked) {
  key <- Sys.getenv("FEEDBACK_ADMIN_KEY", "")
  if (nzchar(key)) {
    qkey <- tryCatch(shiny::parseQueryString(url_search %||% "")$admin,
                     error = function(e) NULL)
    if (isTRUE(identical(key, qkey))) return(TRUE)
  }
  exists("admin_authorized_strict", mode = "function") &&
    isTRUE(admin_authorized_strict(unlocked))
}

#' HTML-escape the user-text columns of a DT display frame
#'
#' The admin tables use escape = FALSE so the GitHub-link / action-button
#' columns render as HTML; every column holding user-submitted text must
#' therefore be escaped explicitly.
#' @param df display data.frame
#' @param cols columns to escape (missing ones are ignored)
escape_feedback_display_cols <- function(df, cols) {
  for (cn in intersect(cols, names(df))) {
    df[[cn]] <- htmltools::htmlEscape(as.character(df[[cn]]))
  }
  df
}

#' Title for display; legacy rows (pre-dashboard) have none
#' @keywords internal
.feedback_display_title <- function(title) {
  ifelse(is.na(title) | !nzchar(title), "(untitled)", title)
}

feedback_server <- function(input, output, session, net_reactive = NULL) {

  # Boot: surface feedback-store health in warnings (shiny-server logs).
  local({
    s <- tryCatch(feedback_summary(), error = function(e) {
      warning(sprintf("[feedback] store unreachable at boot: %s",
                      conditionMessage(e)), call. = FALSE)
      NULL
    })
    if (is.null(s)) {
      warning("[feedback] store unreachable at boot", call. = FALSE)
    } else {
      warning(sprintf("[feedback] store OK at boot: open=%d total=%d", s$open, s$total),
              call. = FALSE)
    }
  })

  app_version <- function() {
    tryCatch(
      get_version("short"),
      error = function(e) {
        warning(sprintf("[feedback_server] get_version failed: %s", conditionMessage(e)),
                call. = FALSE)
        NA_character_
      }
    )
  }

  current_context <- function() {
    net <- if (is.function(net_reactive)) tryCatch(net_reactive(), error = function(e) NULL)
    feedback_collect_context(input, net = net, app_version = app_version())
  }

  feedback_refresh <- reactiveVal(0)
  bump <- function() feedback_refresh(isolate(feedback_refresh()) + 1)

  # ---- Submit ----------------------------------------------------------------
  output$feedback_context_display <- renderUI({
    input$feedback_open
    ctx <- isolate(current_context())
    tags$pre(style = "font-size: 11px; white-space: pre-wrap; margin: 0;", paste(
      sprintf("Version: %s", ctx$app_version),
      sprintf("Page: %s", ctx$current_tab),
      sprintf("Species in loaded web: %d", ctx$species_count),
      sprintf("Links in loaded web: %d", ctx$link_count),
      sprintf("Browser: %s", ctx$browser_info),
      sep = "\n"))
  })

  last_submit_time <- reactiveVal(NULL)

  observeEvent(input$feedback_submit, {
    last <- last_submit_time()
    if (!is.null(last) && difftime(Sys.time(), last, units = "secs") < 30) {
      showNotification("Please wait 30 seconds before sending another report.",
                       type = "warning")
      return()
    }
    title <- trimws(input$feedback_title %||% "")
    desc <- input$feedback_description
    if (!nzchar(title)) {
      showNotification("Please give your report a short title.", type = "warning")
      return()
    }
    if (is.null(desc) || !nzchar(trimws(desc))) {
      showNotification("Please describe the bug or suggestion.", type = "warning")
      return()
    }
    type <- input$feedback_type %||% "bug"
    steps <- if (identical(type, "bug")) input$feedback_steps else NA_character_

    res <- feedback_submit(
      type        = type,
      title       = title,
      description = desc,
      steps       = steps,
      email       = input$feedback_email,
      context     = current_context(),
      session_id  = session$token
    )
    if (isTRUE(res$success)) {
      last_submit_time(Sys.time())
      showNotification(
        if (isTRUE(res$github_success)) {
          tagList("Thanks - your feedback was saved and filed as an issue. ",
                  tags$a(href = res$github_url, target = "_blank", rel = "noopener", "View"))
        } else {
          "Thanks - your feedback was saved."
        },
        type = "message", duration = 8)
      updateTextInput(session, "feedback_title", value = "")
      updateTextAreaInput(session, "feedback_description", value = "")
      updateTextAreaInput(session, "feedback_steps", value = "")
      updateTextInput(session, "feedback_email", value = "")
      toggleModal(session, "feedback_modal", toggle = "close")
      bump()
    } else {
      # Keep the modal open + fields intact so the user never loses their report.
      showNotification(
        "Couldn't save your feedback - please try again; if it keeps failing, email the maintainer.",
        type = "error", duration = 10)
    }
  })

  # ---- Who is admin ------------------------------------------------------------
  # A reactiveVal (not a reactive) so the dashboard only re-renders when the
  # answer changes. Re-evaluated on navigation and after a password unlock
  # (plugin_server sets session$userData$admin_unlocked, which isn't reactive).
  admin_now <- function() {
    .feedback_admin_allowed(isolate(session$clientData$url_search),
                            session$userData$admin_unlocked)
  }
  is_admin <- reactiveVal(FALSE)
  observe({
    session$clientData$url_search
    input$sidebar_menu
    input$admin_unlock
    is_admin(admin_now())
  })

  # ---- Data --------------------------------------------------------------------
  fb_data <- reactive({
    feedback_refresh()
    feedback_list(include_email = is_admin())
  })

  fb_summary <- reactive({
    feedback_refresh()
    feedback_summary()
  })

  filtered <- reactive({
    df <- fb_data()
    if (nrow(df) == 0L) return(df)
    sel <- input$feedback_status_filter %||% "open"
    if (identical(sel, "open")) df <- df[df$status == "open", , drop = FALSE]
    else if (identical(sel, "resolved")) df <- df[df$status != "open", , drop = FALSE]
    df
  })

  observeEvent(input$feedback_refresh, bump())

  # ---- Dashboard ---------------------------------------------------------------
  output$feedback_dashboard_ui <- renderUI(feedback_dashboard_body(admin = is_admin()))

  vbox <- function(field, subtitle, icon_name, color) {
    renderValueBox(valueBox(fb_summary()[[field]], subtitle, icon = icon(icon_name),
                            color = color))
  }
  output$feedback_vbox_total       <- vbox("total", "Total reports", "comment-dots", "primary")
  output$feedback_vbox_bugs        <- vbox("bugs", "Bug reports", "bug", "danger")
  output$feedback_vbox_suggestions <- vbox("suggestions", "Suggestions", "lightbulb", "success")
  output$feedback_vbox_recent      <- vbox("recent", "Last 7 days", "calendar-week", "info")
  output$feedback_vbox_github      <- vbox("github", "GitHub submitted", "github", "secondary")
  output$feedback_vbox_local       <- vbox("local_only", "Local only", "hdd", "warning")
  output$feedback_vbox_resolved    <- vbox("resolved", "Resolved", "check-double", "success")

  status_style <- function(dt) {
    DT::formatStyle(dt, "Status", backgroundColor = DT::styleEqual(
      c("open", "addressed", "wont_fix", "duplicate"),
      c("#e9ecef", "#d4edda", "#e2e3e5", "#fff3cd")))
  }

  output$feedback_table <- DT::renderDataTable({
    df <- filtered()
    if (nrow(df) == 0L) {
      return(DT::datatable(data.frame(message = "No feedback to show."),
                           options = list(dom = "t"), rownames = FALSE))
    }
    base <- data.frame(
      Date   = substr(df$created_at, 1, 10),
      Type   = df$type,
      Title  = .feedback_display_title(df$title),
      Status = df$status,
      stringsAsFactors = FALSE
    )
    # Columns shown to everyone: date / type / title / status. No description,
    # email, GitHub link or actions for non-admins (privacy).
    if (!is_admin()) {
      return(DT::datatable(base, escape = TRUE, selection = "none", rownames = FALSE,
                           options = list(pageLength = 15, order = list(list(0, "desc")),
                                          dom = "lfrtip", scrollX = TRUE)) |>
               status_style())
    }

    url_ok <- !is.na(df$github_url) & grepl("^https://", df$github_url)
    github_html <- ifelse(
      url_ok,
      paste0('<a href="', htmltools::htmlEscape(df$github_url),
             '" target="_blank" rel="noopener"><i class="fa fa-check-circle text-success"></i></a>'),
      '<i class="fa fa-times-circle text-muted"></i>')
    # Per-row button carries the store id (NOT a DataTable row index) so it
    # stays correct under filtering; stopPropagation keeps the click from also
    # selecting the row (which opens the detail modal).
    action_html <- ifelse(
      df$status == "open",
      sprintf(paste0('<button class="btn btn-xs btn-success" onclick="event.stopPropagation();',
                     " Shiny.setInputValue('feedback_mark_addr_click',",
                     ' {id: %d, rand: Math.random()})">Mark addressed</button>'),
              as.integer(df$id)),
      "")
    desc <- ifelse(is.na(df$description), "", df$description)
    disp <- data.frame(
      Date        = base$Date,
      Type        = base$Type,
      Title       = base$Title,
      Description = ifelse(nchar(desc) > 100, paste0(substr(desc, 1, 100), "…"), desc),
      Status      = base$Status,
      GitHub      = github_html,
      Action      = action_html,
      stringsAsFactors = FALSE
    )
    # escape = FALSE is needed for the GitHub/Action HTML columns only;
    # user-submitted text is escaped by hand.
    disp <- escape_feedback_display_cols(disp, c("Title", "Description"))
    DT::datatable(disp, escape = FALSE, selection = "single", rownames = FALSE,
                  options = list(pageLength = 15, order = list(list(0, "desc")),
                                 dom = "lfrtip", scrollX = TRUE)) |>
      status_style()
  })

  # ---- Row click -> detail modal (admin) ---------------------------------------
  observeEvent(input$feedback_table_rows_selected, {
    req(admin_now())
    idx <- input$feedback_table_rows_selected
    df <- isolate(filtered())
    req(length(idx) == 1L, idx <= nrow(df))
    row <- df[idx, , drop = FALSE]
    field <- function(label, value) {
      v <- as.character(value)
      tagList(tags$dt(label), tags$dd(if (is.na(v) || !nzchar(v)) tags$em("—") else v))
    }
    showModal(modalDialog(
      title = tagList(icon("file-alt"), sprintf(" Feedback #%d", as.integer(row$id))),
      tags$dl(
        field("Date", row$created_at),
        field("Type", row$type),
        field("Title", .feedback_display_title(row$title)),
        field("Description", row$description),
        field("Steps to reproduce", row$steps),
        field("Email", row$submitter_email),
        field("App version", row$app_version),
        field("Page", row$tab),
        field("Status", row$status),
        field("Resolution note", row$resolution_note),
        field("Fix reference", row$fix_commit),
        field("Duplicate of", row$duplicate_of),
        tags$dt("GitHub"),
        tags$dd(if (!is.na(row$github_url) && grepl("^https://", row$github_url)) {
          tags$a(href = row$github_url, target = "_blank", rel = "noopener", row$github_url)
        } else {
          tags$em("—")
        })
      ),
      footer = modalButton("Close"), easyClose = TRUE, size = "l"
    ))
  })

  # ---- Mark addressed / won't fix / duplicate (admin) ----------------------------
  pending_mark_id <- reactiveVal(NULL)

  observeEvent(input$feedback_mark_addr_click, {
    req(admin_now())
    id <- suppressWarnings(as.integer(input$feedback_mark_addr_click$id))
    req(length(id) == 1L, !is.na(id))
    pending_mark_id(id)
    showModal(modalDialog(
      title = sprintf("Resolve feedback #%d", id),
      selectInput("feedback_mark_status", "Status:",
                  choices = c("Addressed" = "addressed", "Won't fix" = "wont_fix",
                              "Duplicate" = "duplicate"),
                  selected = "addressed"),
      textAreaInput("feedback_mark_note", "Note:", width = "100%"),
      textInput("feedback_mark_fix_ref", "Fix reference (commit / PR):", width = "100%"),
      footer = tagList(modalButton("Cancel"),
                       actionButton("feedback_do_mark", "Save", class = "btn-success"))
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$feedback_do_mark, {
    if (!admin_now()) {
      warning("[feedback] feedback_do_mark fired without admin rights; refusing", call. = FALSE)
      return()
    }
    id <- pending_mark_id()
    req(!is.null(id))
    res <- feedback_mark_resolved(id,
                                  status = input$feedback_mark_status %||% "addressed",
                                  note = input$feedback_mark_note,
                                  fix_ref = input$feedback_mark_fix_ref,
                                  resolved_by = "admin")
    pending_mark_id(NULL)
    removeModal()
    if (isTRUE(res$success)) {
      showNotification(sprintf("Feedback #%d resolved.", id), type = "message")
      bump()
    } else {
      showNotification("Couldn't update the report (see server log).", type = "error")
    }
  }, ignoreInit = TRUE)

  # ---- Duplicate detection (admin) ---------------------------------------------
  dup_pairs <- reactiveVal(NULL)

  output$feedback_scale_note <- renderUI({
    if (nrow(fb_data()) > 500L) {
      tags$div(class = "alert alert-info", style = "margin-top: 10px; font-size: 12px;",
               icon("info-circle"),
               " More than 500 reports: only the 500 most recent are compared.")
    }
  })

  detect <- function() {
    df <- isolate(fb_data())
    pairs <- withProgress(message = "Finding duplicates", value = 0.3, {
      tryCatch(
        find_duplicate_pairs(df, threshold = isolate(input$feedback_similarity_threshold) %||% 0.7),
        error = function(e) {
          warning(sprintf("[feedback] duplicate detection failed: %s", conditionMessage(e)),
                  call. = FALSE)
          showNotification("Duplicate detection failed (see server log).", type = "error")
          NULL
        }
      )
    })
    dup_pairs(pairs)
  }

  observeEvent(input$feedback_find_duplicates, {
    req(admin_now())
    detect()
  })

  observeEvent(input$feedback_recalculate, {
    req(admin_now())
    bump()
    detect()
  })

  output$feedback_duplicates_table <- DT::renderDataTable({
    pairs <- dup_pairs()
    if (is.null(pairs) || nrow(pairs) == 0L) {
      return(DT::datatable(
        data.frame(message = if (is.null(pairs)) "Run Find duplicates to compare reports."
                             else "No candidate duplicates at this threshold."),
        options = list(dom = "t"), rownames = FALSE))
    }
    df <- isolate(fb_data())
    label <- function(id) {
      k <- match(id, df$id)
      if (is.na(k)) return(sprintf("[#%d]", id))
      txt <- if (is.na(df$title[k]) || !nzchar(df$title[k])) df$description[k] else df$title[k]
      sprintf("[#%d] %s", id, substr(txt, 1, 60))
    }
    buttons <- sprintf(paste0(
      '<button class="btn btn-xs btn-warning"',
      " onclick=\"Shiny.setInputValue('feedback_mark_dup',",
      ' {id: %d, dup_of: %d, rand: Math.random()})">Mark B duplicate of A</button>'),
      pairs$id_b, pairs$id_a)
    disp <- data.frame(
      Report_A   = vapply(pairs$id_a, label, character(1)),
      Report_B   = vapply(pairs$id_b, label, character(1)),
      Similarity = round(pairs$similarity, 3),
      Action     = buttons,
      stringsAsFactors = FALSE
    )
    disp <- escape_feedback_display_cols(disp, c("Report_A", "Report_B"))
    DT::datatable(disp, colnames = c("Report A", "Report B", "Similarity", "Action"),
                  escape = FALSE, rownames = FALSE, selection = "none",
                  options = list(pageLength = 10, dom = "lfrtip",
                                 order = list(list(2, "desc")), scrollX = TRUE))
  })

  observeEvent(input$feedback_mark_dup, {
    if (!admin_now()) {
      warning("[feedback] feedback_mark_dup fired without admin rights; refusing", call. = FALSE)
      return()
    }
    id <- suppressWarnings(as.integer(input$feedback_mark_dup$id))
    orig <- suppressWarnings(as.integer(input$feedback_mark_dup$dup_of))
    req(length(id) == 1L, length(orig) == 1L)
    res <- feedback_mark_duplicate(id, duplicate_of = orig, resolved_by = "admin")
    if (isTRUE(res$success)) {
      showNotification(sprintf("Feedback #%d marked as a duplicate of #%d.", id, orig),
                       type = "message")
      bump()
      detect()
    } else {
      showNotification("Couldn't mark the duplicate (see server log).", type = "error")
    }
  }, ignoreInit = TRUE)

  invisible(list(data = fb_data, filtered = filtered, summary = fb_summary,
                 dup_pairs = dup_pairs, is_admin = is_admin))
}
