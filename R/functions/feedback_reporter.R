# =============================================================================
# Feedback reporter - submit orchestration + optional GitHub issue mirroring
# =============================================================================
# Ported from MarineSABRES (functions/feedback_reporter.R). The durable record
# is always the SQLite store (feedback_store.R); a GitHub issue is a best-effort
# extra, dormant unless ECONETOOL_GITHUB_TOKEN is set on the server.
#
# GitHub issues are PUBLIC on a public repo: the issue carries title,
# description, steps and system context only - never the submitter's email.
# =============================================================================

#' System context attached to a feedback submission
#'
#' @param input Shiny input (or a plain list in tests).
#' @param net igraph of the loaded food web, or NULL.
#' @param app_version Version string (get_version("short") in the app).
#' @return list(app_version, current_tab, browser_info, species_count,
#'   link_count, timestamp)
#' @export
feedback_collect_context <- function(input, net = NULL, app_version = NA_character_) {
  scalar_or_unknown <- function(x) {
    if (is.null(x) || length(x) != 1L || is.na(x) || !nzchar(x)) "unknown" else as.character(x)
  }
  counts <- tryCatch(
    if (inherits(net, "igraph")) {
      c(igraph::vcount(net), igraph::ecount(net))
    } else {
      c(0L, 0L)
    },
    error = function(e) {
      warning(sprintf("[feedback_reporter] network size unavailable: %s",
                      conditionMessage(e)), call. = FALSE)
      c(0L, 0L)
    }
  )
  list(
    app_version   = scalar_or_unknown(app_version),
    current_tab   = scalar_or_unknown(input$sidebar_menu),
    browser_info  = substr(scalar_or_unknown(input$feedback_browser_info), 1, 300),
    species_count = as.integer(counts[1]),
    link_count    = as.integer(counts[2]),
    timestamp     = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  )
}

#' Markdown body for the mirrored GitHub issue (no email, by construction)
#' @export
feedback_issue_body <- function(description, steps, context) {
  parts <- paste0("## Description\n\n", description)
  if (!is.null(steps) && !is.na(steps) && nzchar(trimws(steps))) {
    parts <- c(parts, paste0("\n\n## Steps to Reproduce\n\n", steps))
  }
  context_json <- tryCatch(
    jsonlite::toJSON(context, auto_unbox = TRUE, pretty = TRUE),
    error = function(e) {
      warning(sprintf("[feedback_reporter] context serialisation failed: %s",
                      conditionMessage(e)), call. = FALSE)
      "{}"
    }
  )
  parts <- c(parts, paste0("\n\n<details>\n<summary>System Context</summary>\n\n",
                           "```json\n", context_json, "\n```\n\n</details>"))
  paste(parts, collapse = "")
}

#' POST a new issue to GitHub (best effort)
#'
#' Reads ECONETOOL_GITHUB_TOKEN (and optional ECONETOOL_GITHUB_REPO,
#' "owner/name", default razinkele/EcoNeTool). The token is never logged.
#' @return list(url, number) on success, NULL when unconfigured or on error.
#' @export
feedback_github_issue <- function(title, body, labels = character(0)) {
  token <- Sys.getenv("ECONETOOL_GITHUB_TOKEN", unset = "")
  if (!nzchar(token)) return(NULL)
  repo <- Sys.getenv("ECONETOOL_GITHUB_REPO", unset = "razinkele/EcoNeTool")
  api_url <- sprintf("https://api.github.com/repos/%s/issues", repo)
  tryCatch({
    resp <- httr::POST(
      api_url,
      httr::add_headers(Authorization = paste("Bearer", token),
                        Accept = "application/vnd.github+json"),
      body = jsonlite::toJSON(list(title = title, body = body, labels = as.list(labels)),
                              auto_unbox = TRUE),
      encode = "raw",
      httr::timeout(10)
    )
    status <- httr::status_code(resp)
    if (!status %in% c(200L, 201L)) {
      warning(sprintf("[feedback_reporter] GitHub issue creation returned HTTP %d", status),
              call. = FALSE)
      return(NULL)
    }
    parsed <- httr::content(resp, as = "parsed", type = "application/json")
    list(url = parsed$html_url %||% NA_character_, number = parsed$number %||% NA_integer_)
  }, error = function(e) {
    warning(sprintf("[feedback_reporter] GitHub issue creation failed: %s",
                    conditionMessage(e)), call. = FALSE)
    NULL
  })
}

#' Save a submission to the store, then mirror it to GitHub if configured
#'
#' The store write comes first and alone decides success: a GitHub outage
#' never loses a report.
#' @return list(success, id, error, github_success, github_url)
#' @export
feedback_submit <- function(type, title, description, steps = NA, email = NA,
                            context = list(), session_id = NA, db_path = NULL) {
  res <- feedback_insert(type = type, description = description,
                         submitter_email = email,
                         app_version = context$app_version %||% NA_character_,
                         tab = context$current_tab %||% NA_character_,
                         session_id = session_id, title = title, steps = steps,
                         db_path = db_path)
  out <- list(success = isTRUE(res$success), id = res$id, error = res$error,
              github_success = FALSE, github_url = NA_character_)
  if (!out$success) return(out)

  labels <- switch(type,
                   bug = c("bug", "user-reported"),
                   suggestion = c("enhancement", "user-reported"),
                   c("feedback", "user-reported"))
  gh <- feedback_github_issue(substr(title, 1, 200), feedback_issue_body(description, steps, context),
                              labels)
  if (!is.null(gh) && isTRUE(grepl("^https://", gh$url))) {
    out$github_success <- TRUE
    out$github_url <- gh$url
    set <- feedback_set_github_url(res$id, gh$url, db_path = db_path)
    if (!isTRUE(set$success)) {
      warning(sprintf("[feedback_reporter] issue created but URL not stored for id=%s: %s",
                      res$id, set$error), call. = FALSE)
    }
  }
  out
}
