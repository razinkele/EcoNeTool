# =============================================================================
# Feedback store - parameterized SQLite helpers (no Shiny dependency)
# =============================================================================
# Durable bug/suggestion store. ALL SQL is parameterized (DBI params) - never
# interpolate user text into SQL. DB lives at a fixed absolute path OUTSIDE the
# deploy tree (env FEEDBACK_DB_PATH) so deploys can't wipe it.
# =============================================================================

#' Feedback types accepted from the submit modal
FEEDBACK_TYPES <- c("bug", "suggestion", "general")

#' Resolution statuses. "open" is the insert default; the rest are set by an
#' admin (Feedback Dashboard) or the triage skill.
FEEDBACK_STATUSES <- c("open", "addressed", "wont_fix", "duplicate")

#' Columns added after the first release of the store (Feedback Dashboard).
#' Nullable TEXT so the original INSERT/SELECTs keep working unchanged.
FEEDBACK_ADDED_COLUMNS <- c("title", "steps", "github_url", "duplicate_of")

#' Resolve the feedback DB path (absolute, outside the deploy tree)
#' @keywords internal
feedback_db_path <- function() {
  p <- Sys.getenv("FEEDBACK_DB_PATH", unset = "")
  if (nzchar(p)) p else "feedback_dev.db"   # dev default (gitignored)
}

#' Open a WAL connection and ensure the schema exists
#'
#' Shared-writer invariant: this store is written by two OS users - the app
#' (runs as `shiny`) inserts; the triage skill (runs as `razinka`) marks
#' addressed / deletes. Both are in a shared group. SQLite's default file mode
#' (umask 022 -> 644) is owner-writable only, so whichever user CREATES the DB
#' or its WAL sidecars locks the other out with "attempt to write a readonly
#' database" (prod incident 2026-06-01). We therefore create the DB, WAL, and
#' SHM files group-writable (umask 002 -> 664) and setgid the directory so the
#' group is inherited. Sys.umask/Sys.chmod are POSIX no-ops on Windows (dev),
#' which is harmless - the invariant only matters on the multi-user server.
#' @keywords internal
.feedback_con <- function(db_path) {
  d <- dirname(db_path)
  if (nzchar(d) && !dir.exists(d)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    try(Sys.chmod(d, mode = "2775"), silent = TRUE)  # setgid + group-writable
  }
  old_umask <- Sys.umask("0002")                      # group-writable new files
  on.exit(Sys.umask(old_umask), add = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  ok <- FALSE
  on.exit(if (!ok) DBI::dbDisconnect(con), add = TRUE)
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
  .feedback_migrate(con)
  # Widen an already-existing owner-only DB when we own it (no-op otherwise).
  try(Sys.chmod(db_path, mode = "0664"), silent = TRUE)
  ok <- TRUE
  con
}

#' Add the Feedback Dashboard columns to a store created before they existed
#'
#' CREATE TABLE IF NOT EXISTS never alters an existing table, so the live DB
#' needs an explicit ALTER. Column names come from FEEDBACK_ADDED_COLUMNS (a
#' constant), never from user input. A failed ALTER warns and continues:
#' feedback_list() intersects with dbListFields(), so reads still work.
#' @keywords internal
.feedback_migrate <- function(con) {
  missing <- setdiff(FEEDBACK_ADDED_COLUMNS, DBI::dbListFields(con, "feedback"))
  for (col in missing) {
    tryCatch(
      DBI::dbExecute(con, sprintf("ALTER TABLE feedback ADD COLUMN %s TEXT DEFAULT NULL", col)),
      error = function(e) {
        warning(sprintf("[feedback_store] migration: adding column '%s' failed: %s",
                        col, conditionMessage(e)), call. = FALSE)
      }
    )
  }
  invisible(missing)
}

#' Current UTC timestamp (ISO-8601)
#' @keywords internal
.feedback_now <- function() format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

#' Optional free-text field: NA when blank, otherwise capped at max_chars
#' @keywords internal
.feedback_opt_text <- function(x, max_chars) {
  if (is.null(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) return(NA_character_)
  substr(as.character(x), 1, max_chars)
}

#' Run one parameterized UPDATE/DELETE that must touch exactly one row
#'
#' 0 rows matched -> failure (not silent success).
#' @return list(success, error)
#' @keywords internal
.feedback_write_one <- function(path, label, id, sql, params) {
  result <- list(success = FALSE, error = NA_character_)
  tryCatch({
    con <- .feedback_con(path)
    on.exit({
      tryCatch(DBI::dbExecute(con, "PRAGMA wal_checkpoint(TRUNCATE)"), error = function(e) {
        warning(sprintf("[feedback_store] WAL checkpoint cleanup failed: %s",
                        conditionMessage(e)), call. = FALSE)
      })
      DBI::dbDisconnect(con)
    }, add = TRUE)
    n <- DBI::dbExecute(con, sql, params = params)
    if (n == 0L) result$error <- sprintf("no feedback row with id=%s", id)
    else result$success <- TRUE
  }, error = function(e) {
    warning(sprintf("[%s] failed (db=%s): %s", label, path, conditionMessage(e)),
            call. = FALSE)
    result$error <<- conditionMessage(e)
  })
  result
}

#' Coerce an id argument; NA when it is not integer-coercible
#' @keywords internal
.feedback_id <- function(id) {
  if (is.null(id) || length(id) != 1L) return(NA_integer_)
  suppressWarnings(as.integer(id))
}

#' Insert a feedback row (status='open')
#' @return list(success, id, error)
#' @export
feedback_insert <- function(type, description, submitter_email = NA,
                            app_version = NA, tab = NA, session_id = NA,
                            title = NA, steps = NA, db_path = NULL) {
  result <- list(success = FALSE, id = NA_integer_, error = NA_character_)
  if (!isTRUE(type %in% FEEDBACK_TYPES)) {
    result$error <- "invalid type"; return(result)
  }
  if (is.null(description) || !nzchar(trimws(description))) {
    result$error <- "empty description"; return(result)
  }
  description <- substr(description, 1, 5000)
  submitter_email <- .feedback_opt_text(submitter_email, 254)
  title <- .feedback_opt_text(title, 200)
  steps <- .feedback_opt_text(steps, 2000)
  path <- if (is.null(db_path)) feedback_db_path() else db_path
  tryCatch({
    con <- .feedback_con(path)
    on.exit({
      tryCatch(DBI::dbExecute(con, "PRAGMA wal_checkpoint(TRUNCATE)"), error = function(e) {
        warning(sprintf("[feedback_store] WAL checkpoint cleanup failed: %s",
                        conditionMessage(e)), call. = FALSE)
      })
      DBI::dbDisconnect(con)
    }, add = TRUE)
    DBI::dbExecute(con, paste(
      "INSERT INTO feedback",
      "(created_at, type, description, submitter_email, app_version, tab, session_id,",
      "title, steps, status)",
      "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 'open')"),
      params = list(.feedback_now(), type, description, submitter_email,
                    app_version, tab, session_id, title, steps))
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
    cols <- c("id", "created_at", "type", "title", "description", "steps",
              if (include_email) "submitter_email",
              "app_version", "tab", "session_id", "status",
              "addressed_at", "addressed_by", "fix_commit", "resolution_note",
              "github_url", "duplicate_of")
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
    data.frame(id = integer(0), created_at = character(0), type = character(0),
               title = character(0), description = character(0), steps = character(0),
               app_version = character(0), tab = character(0),
               session_id = character(0), status = character(0), addressed_at = character(0),
               addressed_by = character(0), fix_commit = character(0),
               resolution_note = character(0), github_url = character(0),
               duplicate_of = character(0), stringsAsFactors = FALSE)
  })
}

#' Set a feedback row's resolution status (addressed / wont_fix / duplicate)
#'
#' The status is whitelisted server-side: it arrives from a client-side select
#' input, which a browser console can set to anything.
#' @return list(success, error)
#' @export
feedback_mark_resolved <- function(id, status = "addressed", note = NA, fix_ref = NA,
                                   resolved_by = "admin", db_path = NULL) {
  path <- if (is.null(db_path)) feedback_db_path() else db_path
  id_int <- .feedback_id(id)
  if (is.na(id_int)) {
    return(list(success = FALSE,
                error = sprintf("id must be integer-coercible, got: %s", paste(id, collapse = ","))))
  }
  if (!isTRUE(status %in% setdiff(FEEDBACK_STATUSES, "open"))) {
    return(list(success = FALSE, error = sprintf("invalid status: %s", paste(status, collapse = ","))))
  }
  .feedback_write_one(path, "feedback_mark_resolved", id, paste(
    "UPDATE feedback SET status = ?, addressed_at = ?, addressed_by = ?,",
    "fix_commit = ?, resolution_note = ? WHERE id = ?"),
    params = list(status, .feedback_now(), as.character(resolved_by),
                  .feedback_opt_text(fix_ref, 200), .feedback_opt_text(note, 2000),
                  id_int))
}

#' Mark a feedback row addressed. 0 rows matched -> failure (not silent success).
#'
#' Kept for the triage skill (.claude/skills/feedback), which calls it by name
#' over SSH; a thin wrapper over feedback_mark_resolved().
#' @return list(success, error)
#' @export
feedback_mark_addressed <- function(id, fix_commit = NA, resolution_note = NA,
                                    addressed_by = "skill", db_path = NULL) {
  feedback_mark_resolved(id, status = "addressed", note = resolution_note,
                         fix_ref = fix_commit, resolved_by = addressed_by,
                         db_path = db_path)
}

#' Mark a feedback row as a duplicate of another (status='duplicate')
#' @return list(success, error)
#' @export
feedback_mark_duplicate <- function(id, duplicate_of, resolved_by = "admin", db_path = NULL) {
  path <- if (is.null(db_path)) feedback_db_path() else db_path
  id_int <- .feedback_id(id)
  orig_int <- .feedback_id(duplicate_of)
  if (is.na(id_int) || is.na(orig_int)) {
    return(list(success = FALSE, error = "id and duplicate_of must be integer-coercible"))
  }
  if (id_int == orig_int) {
    return(list(success = FALSE, error = "a row cannot duplicate itself"))
  }
  .feedback_write_one(path, "feedback_mark_duplicate", id, paste(
    "UPDATE feedback SET status = 'duplicate', duplicate_of = ?, addressed_at = ?,",
    "addressed_by = ? WHERE id = ? AND EXISTS (SELECT 1 FROM feedback WHERE id = ?)"),
    params = list(paste0("id:", orig_int), .feedback_now(), as.character(resolved_by),
                  id_int, orig_int))
}

#' Record the GitHub issue a feedback row was mirrored to (https only)
#' @return list(success, error)
#' @export
feedback_set_github_url <- function(id, url, db_path = NULL) {
  path <- if (is.null(db_path)) feedback_db_path() else db_path
  id_int <- .feedback_id(id)
  if (is.na(id_int)) {
    return(list(success = FALSE, error = "id must be integer-coercible"))
  }
  if (!isTRUE(grepl("^https://", url))) {
    return(list(success = FALSE, error = "github_url must start with https://"))
  }
  .feedback_write_one(path, "feedback_set_github_url", id,
                      "UPDATE feedback SET github_url = ? WHERE id = ?",
                      params = list(substr(url, 1, 500), id_int))
}

#' Delete a feedback row (GDPR deletion path). 0 rows -> failure.
#' @return list(success, error)
#' @export
feedback_delete <- function(id, db_path = NULL) {
  path <- if (is.null(db_path)) feedback_db_path() else db_path
  id_int <- .feedback_id(id)
  if (is.na(id_int)) {
    return(list(success = FALSE,
                error = sprintf("id must be integer-coercible, got: %s", paste(id, collapse = ","))))
  }
  .feedback_write_one(path, "feedback_delete", id,
                      "DELETE FROM feedback WHERE id = ?", params = list(id_int))
}

#' Dashboard counts.
#'
#' `resolved` is every non-open row (addressed + wont_fix + duplicate);
#' `addressed` counts only status 'addressed' (kept for the boot log line).
#' `recent` is rows created in the last 7 days.
#' @return list(total, open, addressed, resolved, bugs, suggestions, recent,
#'   github, local_only)
#' @export
feedback_summary <- function(db_path = NULL) {
  path <- if (is.null(db_path)) feedback_db_path() else db_path
  zero <- list(total = 0L, open = 0L, addressed = 0L, resolved = 0L, bugs = 0L,
               suggestions = 0L, recent = 0L, github = 0L, local_only = 0L)
  tryCatch({
    con <- .feedback_con(path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    cutoff <- format(as.POSIXct(Sys.time(), tz = "UTC") - 7 * 86400, "%Y-%m-%dT%H:%M:%SZ")
    q <- DBI::dbGetQuery(con, paste(
      "SELECT COUNT(*) AS total,",
      "SUM(status = 'open') AS open,",
      "SUM(status = 'addressed') AS addressed,",
      "SUM(status != 'open') AS resolved,",
      "SUM(type = 'bug') AS bugs,",
      "SUM(type = 'suggestion') AS suggestions,",
      "SUM(created_at >= ?) AS recent,",
      "SUM(github_url IS NOT NULL AND github_url LIKE 'https://%') AS github",
      "FROM feedback"), params = list(cutoff))
    out <- lapply(q[1, ], function(v) if (is.na(v)) 0L else as.integer(v))
    out$local_only <- out$total - out$github
    out[names(zero)]
  }, error = function(e) {
    warning(sprintf("[feedback_summary] failed (db=%s): %s", path, conditionMessage(e)),
            call. = FALSE)
    zero
  })
}
