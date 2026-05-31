# =============================================================================
# Feedback store - parameterized SQLite helpers (no Shiny dependency)
# =============================================================================
# Durable bug/suggestion store. ALL SQL is parameterized (DBI params) - never
# interpolate user text into SQL. DB lives at a fixed absolute path OUTSIDE the
# deploy tree (env FEEDBACK_DB_PATH) so deploys can't wipe it.
# =============================================================================

#' Resolve the feedback DB path (absolute, outside the deploy tree)
#' @keywords internal
feedback_db_path <- function() {
  p <- Sys.getenv("FEEDBACK_DB_PATH", unset = "")
  if (nzchar(p)) p else "feedback_dev.db"   # dev default (gitignored)
}

#' Open a WAL connection and ensure the schema exists
#' @keywords internal
.feedback_con <- function(db_path) {
  d <- dirname(db_path)
  if (nzchar(d) && !dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL")
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
  DBI::dbExecute(con, paste(
    "CREATE TABLE IF NOT EXISTS feedback (",
    "  id INTEGER PRIMARY KEY AUTOINCREMENT,",
    "  created_at TEXT NOT NULL,",
    "  type TEXT NOT NULL,",
    "  description TEXT NOT NULL,",
    "  submitter_email TEXT DEFAULT NULL,",
    "  app_version TEXT DEFAULT NULL,",
    "  tab TEXT DEFAULT NULL,",
    "  session_id TEXT DEFAULT NULL,",
    "  status TEXT NOT NULL DEFAULT 'open',",
    "  addressed_at TEXT DEFAULT NULL,",
    "  addressed_by TEXT DEFAULT NULL,",
    "  fix_commit TEXT DEFAULT NULL,",
    "  resolution_note TEXT DEFAULT NULL )"))
  con
}

#' Current UTC timestamp (ISO-8601)
#' @keywords internal
.feedback_now <- function() format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

#' Insert a feedback row (status='open')
#' @return list(success, id, error)
#' @export
feedback_insert <- function(type, description, submitter_email = NA,
                            app_version = NA, tab = NA, session_id = NA,
                            db_path = NULL) {
  result <- list(success = FALSE, id = NA_integer_, error = NA_character_)
  if (!isTRUE(type %in% c("bug", "suggestion"))) {
    result$error <- "invalid type"; return(result)
  }
  if (is.null(description) || !nzchar(trimws(description))) {
    result$error <- "empty description"; return(result)
  }
  description <- substr(description, 1, 5000)
  submitter_email <- if (!is.na(submitter_email) && nzchar(submitter_email)) {
    substr(submitter_email, 1, 254)
  } else NA_character_
  path <- if (is.null(db_path)) feedback_db_path() else db_path
  tryCatch({
    con <- .feedback_con(path)
    on.exit({
      tryCatch(DBI::dbExecute(con, "PRAGMA wal_checkpoint(TRUNCATE)"), error = function(e) NULL)
      DBI::dbDisconnect(con)
    }, add = TRUE)
    DBI::dbExecute(con, paste(
      "INSERT INTO feedback",
      "(created_at, type, description, submitter_email, app_version, tab, session_id, status)",
      "VALUES (?, ?, ?, ?, ?, ?, ?, 'open')"),
      params = list(.feedback_now(), type, description, submitter_email,
                    app_version, tab, session_id))
    result$id <- as.integer(DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id)
    result$success <- TRUE
  }, error = function(e) {
    warning(sprintf("[feedback_insert] failed (db=%s): %s", path, conditionMessage(e)),
            call. = FALSE)
    result$error <<- conditionMessage(e)
  })
  result
}

#' List feedback rows (newest first). include_email=FALSE omits PII (default).
#' @return data.frame (empty data.frame on error)
#' @export
feedback_list <- function(status = NULL, include_email = FALSE, db_path = NULL) {
  path <- if (is.null(db_path)) feedback_db_path() else db_path
  tryCatch({
    con <- .feedback_con(path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    cols <- c("id", "created_at", "type", "description",
              if (include_email) "submitter_email",
              "app_version", "tab", "session_id", "status",
              "addressed_at", "addressed_by", "fix_commit", "resolution_note")
    cols <- intersect(cols, DBI::dbListFields(con, "feedback"))
    base <- sprintf("SELECT %s FROM feedback", paste(cols, collapse = ", "))
    if (!is.null(status)) {
      DBI::dbGetQuery(con, paste(base, "WHERE status = ? ORDER BY created_at DESC"),
                      params = list(status))
    } else {
      DBI::dbGetQuery(con, paste(base, "ORDER BY created_at DESC"))
    }
  }, error = function(e) {
    warning(sprintf("[feedback_list] failed (db=%s): %s", path, conditionMessage(e)),
            call. = FALSE)
    data.frame()
  })
}

#' Mark a feedback row addressed. 0 rows matched -> failure (not silent success).
#' @return list(success, error)
#' @export
feedback_mark_addressed <- function(id, fix_commit = NA, resolution_note = NA,
                                    addressed_by = "skill", db_path = NULL) {
  result <- list(success = FALSE, error = NA_character_)
  path <- if (is.null(db_path)) feedback_db_path() else db_path
  tryCatch({
    con <- .feedback_con(path)
    on.exit({
      tryCatch(DBI::dbExecute(con, "PRAGMA wal_checkpoint(TRUNCATE)"), error = function(e) NULL)
      DBI::dbDisconnect(con)
    }, add = TRUE)
    n <- DBI::dbExecute(con, paste(
      "UPDATE feedback SET status = 'addressed', addressed_at = ?, addressed_by = ?,",
      "fix_commit = ?, resolution_note = ? WHERE id = ?"),
      params = list(.feedback_now(), addressed_by,
                    if (is.na(fix_commit)) NA_character_ else fix_commit,
                    if (is.na(resolution_note)) NA_character_ else resolution_note,
                    as.integer(id)))
    if (n == 0L) result$error <- sprintf("no feedback row with id=%s", id)
    else result$success <- TRUE
  }, error = function(e) {
    warning(sprintf("[feedback_mark_addressed] failed (db=%s): %s", path, conditionMessage(e)),
            call. = FALSE)
    result$error <<- conditionMessage(e)
  })
  result
}

#' Delete a feedback row (GDPR deletion path). 0 rows -> failure.
#' @return list(success, error)
#' @export
feedback_delete <- function(id, db_path = NULL) {
  result <- list(success = FALSE, error = NA_character_)
  path <- if (is.null(db_path)) feedback_db_path() else db_path
  tryCatch({
    con <- .feedback_con(path)
    on.exit({
      tryCatch(DBI::dbExecute(con, "PRAGMA wal_checkpoint(TRUNCATE)"), error = function(e) NULL)
      DBI::dbDisconnect(con)
    }, add = TRUE)
    n <- DBI::dbExecute(con, "DELETE FROM feedback WHERE id = ?", params = list(as.integer(id)))
    if (n == 0L) result$error <- sprintf("no feedback row with id=%s", id)
    else result$success <- TRUE
  }, error = function(e) {
    warning(sprintf("[feedback_delete] failed (db=%s): %s", path, conditionMessage(e)),
            call. = FALSE)
    result$error <<- conditionMessage(e)
  })
  result
}

#' Counts by status.
#' @return list(open, addressed, total)
#' @export
feedback_summary <- function(db_path = NULL) {
  path <- if (is.null(db_path)) feedback_db_path() else db_path
  tryCatch({
    con <- .feedback_con(path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    q <- DBI::dbGetQuery(con, "SELECT status, COUNT(*) AS n FROM feedback GROUP BY status")
    open <- sum(q$n[q$status == "open"]); addr <- sum(q$n[q$status == "addressed"])
    list(open = as.integer(open), addressed = as.integer(addr),
         total = as.integer(open + addr))
  }, error = function(e) {
    warning(sprintf("[feedback_summary] failed (db=%s): %s", path, conditionMessage(e)),
            call. = FALSE)
    list(open = 0L, addressed = 0L, total = 0L)
  })
}
