# Unit tests for the parameterized feedback store (no Shiny, temp DB).
app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")

local_db <- function() {
  f <- tempfile(fileext = ".db")
  withr::defer(unlink(c(f, paste0(f, "-wal"), paste0(f, "-shm"))), envir = parent.frame())
  f
}

test_that("feedback_insert validates, stores, and round-trips via feedback_list", {
  skip_if_not_installed("RSQLite")
  source(file.path(app_root, "R/functions/feedback_store.R"), local = TRUE)
  db <- local_db()

  # invalid type / empty description -> structured failure, never stop()
  expect_false(feedback_insert("spam", "x", db_path = db)$success)
  expect_false(feedback_insert("bug", "   ", db_path = db)$success)

  ins <- feedback_insert("bug", "the cod plot is blank",
                         submitter_email = "a@b.org", app_version = "1.4.2",
                         tab = "trait_research", session_id = "tok1", db_path = db)
  expect_true(ins$success)
  expect_true(is.numeric(ins$id) && ins$id >= 1)

  all <- feedback_list(db_path = db)               # default: no email column
  expect_s3_class(all, "data.frame")
  expect_equal(nrow(all), 1)
  expect_equal(all$type, "bug")
  expect_equal(all$status, "open")
  expect_false("submitter_email" %in% names(all))  # minimized by default

  with_email <- feedback_list(include_email = TRUE, db_path = db)
  expect_equal(with_email$submitter_email, "a@b.org")
})

test_that("feedback store is injection-safe (parameterized)", {
  skip_if_not_installed("RSQLite")
  source(file.path(app_root, "R/functions/feedback_store.R"), local = TRUE)
  db <- local_db()
  payload <- "'); DROP TABLE feedback;--"
  ins <- feedback_insert("suggestion", payload, db_path = db)
  expect_true(ins$success)
  got <- feedback_list(db_path = db)
  expect_equal(got$description, payload)            # stored verbatim
  expect_equal(nrow(got), 1)                        # table NOT dropped
})

test_that("feedback_insert caps over-long fields", {
  skip_if_not_installed("RSQLite")
  source(file.path(app_root, "R/functions/feedback_store.R"), local = TRUE)
  db <- local_db()
  feedback_insert("bug", strrep("x", 6000),
                  submitter_email = strrep("e", 400), db_path = db)
  got <- feedback_list(include_email = TRUE, db_path = db)
  expect_equal(nchar(got$description), 5000)
  expect_equal(nchar(got$submitter_email), 254)
})

test_that("mark_addressed sets status, fails loudly on a missing id", {
  skip_if_not_installed("RSQLite")
  source(file.path(app_root, "R/functions/feedback_store.R"), local = TRUE)
  db <- local_db()
  id <- feedback_insert("bug", "x", db_path = db)$id

  ok <- feedback_mark_addressed(id, fix_commit = "abc123",
                                resolution_note = "fixed in abc123",
                                addressed_by = "skill", db_path = db)
  expect_true(ok$success)
  addr <- feedback_list(status = "addressed", db_path = db)
  expect_equal(nrow(addr), 1)
  expect_equal(addr$fix_commit, "abc123")
  expect_equal(addr$addressed_by, "skill")
  expect_true(nzchar(addr$addressed_at))

  # 0-row update must NOT silently succeed
  miss <- feedback_mark_addressed(99999L, db_path = db)
  expect_false(miss$success)
  expect_match(miss$error, "no feedback row")
})

test_that("feedback_delete removes a row; summary counts", {
  skip_if_not_installed("RSQLite")
  source(file.path(app_root, "R/functions/feedback_store.R"), local = TRUE)
  db <- local_db()
  id1 <- feedback_insert("bug", "a", db_path = db)$id
  feedback_insert("suggestion", "b", db_path = db)
  feedback_mark_addressed(id1, db_path = db)

  s <- feedback_summary(db_path = db)
  expect_equal(s$total, 2); expect_equal(s$open, 1); expect_equal(s$addressed, 1)

  expect_true(feedback_delete(id1, db_path = db)$success)
  expect_false(feedback_delete(id1, db_path = db)$success)   # already gone
  expect_equal(feedback_summary(db_path = db)$total, 1)
})

test_that("feedback_mark_addressed rejects non-integer id (FIX 3)", {
  skip_if_not_installed("RSQLite")
  source(file.path(app_root, "R/functions/feedback_store.R"), local = TRUE)
  db <- local_db()
  res <- feedback_mark_addressed("abc", db_path = db)
  expect_false(res$success)
  expect_match(res$error, "integer-coercible")
})

test_that("feedback_list on fresh empty db returns typed data.frame (FIX 2)", {
  skip_if_not_installed("RSQLite")
  source(file.path(app_root, "R/functions/feedback_store.R"), local = TRUE)
  db <- local_db()
  df <- feedback_list(db_path = db)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0L)
  expect_true(all(c("id", "type", "status", "description") %in% names(df)))
})

test_that(".feedback_con creates a group-writable store (shared-writer invariant)", {
  # The app writes as OS user `shiny`; the triage skill writes as `razinka`.
  # Both are in the shared `shiny` group. If the DB / WAL sidecars are created
  # owner-writable only (644), whichever user creates them locks the other out
  # with "attempt to write a readonly database" (prod incident 2026-06-01).
  # The store must create files group-writable (664). POSIX modes only.
  skip_on_os("windows")
  skip_if_not_installed("RSQLite")
  source(file.path(app_root, "R/functions/feedback_store.R"), local = TRUE)
  db <- local_db()
  con <- .feedback_con(db)
  DBI::dbExecute(con, "PRAGMA wal_checkpoint(TRUNCATE)")
  DBI::dbDisconnect(con)
  group_write <- as.integer(as.octmode("020"))
  for (f in c(db, paste0(db, "-wal"), paste0(db, "-shm"))) {
    if (!file.exists(f)) next
    mode_i <- as.integer(file.mode(f))
    expect_true(bitwAnd(mode_i, group_write) != 0L,
                info = sprintf("%s mode %s lacks group-write", basename(f),
                               as.character(as.octmode(mode_i))))
  }
})

# ---- Feedback Dashboard (port of the MarineSABRES feedback management) ------

test_that(".feedback_con migrates a pre-dashboard DB in place, keeping old rows", {
  # The live store predates the title/steps/github_url/duplicate_of columns.
  # CREATE TABLE IF NOT EXISTS never adds columns, so the store must ALTER.
  skip_if_not_installed("RSQLite")
  source(file.path(app_root, "R/functions/feedback_store.R"), local = TRUE)
  db <- local_db()
  old <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(old, paste(
    "CREATE TABLE feedback (id INTEGER PRIMARY KEY AUTOINCREMENT,",
    "created_at TEXT NOT NULL, type TEXT NOT NULL, description TEXT NOT NULL,",
    "submitter_email TEXT DEFAULT NULL, app_version TEXT DEFAULT NULL,",
    "tab TEXT DEFAULT NULL, session_id TEXT DEFAULT NULL,",
    "status TEXT NOT NULL DEFAULT 'open', addressed_at TEXT DEFAULT NULL,",
    "addressed_by TEXT DEFAULT NULL, fix_commit TEXT DEFAULT NULL,",
    "resolution_note TEXT DEFAULT NULL )"))
  DBI::dbExecute(old, paste("INSERT INTO feedback (created_at, type, description)",
                            "VALUES ('2026-06-01T00:00:00Z', 'bug', 'legacy row')"))
  DBI::dbDisconnect(old)

  con <- .feedback_con(db)
  fields <- DBI::dbListFields(con, "feedback")
  DBI::dbDisconnect(con)
  expect_true(all(c("title", "steps", "github_url", "duplicate_of") %in% fields))

  got <- feedback_list(db_path = db)
  expect_equal(got$description, "legacy row")
  expect_true(is.na(got$title))
  expect_true(feedback_insert("bug", "new row", title = "New", db_path = db)$success)
  expect_equal(nrow(feedback_list(db_path = db)), 2L)
})

test_that("feedback_insert stores title, steps and the 'general' type, capping lengths", {
  skip_if_not_installed("RSQLite")
  source(file.path(app_root, "R/functions/feedback_store.R"), local = TRUE)
  db <- local_db()
  ins <- feedback_insert("general", "desc", title = strrep("t", 300),
                         steps = strrep("s", 2500), db_path = db)
  expect_true(ins$success)
  got <- feedback_list(db_path = db)
  expect_equal(got$type, "general")
  expect_equal(nchar(got$title), 200)
  expect_equal(nchar(got$steps), 2000)
})

test_that("feedback_mark_resolved whitelists status and fails loudly on a missing id", {
  skip_if_not_installed("RSQLite")
  source(file.path(app_root, "R/functions/feedback_store.R"), local = TRUE)
  db <- local_db()
  id <- feedback_insert("bug", "x", db_path = db)$id

  bad <- feedback_mark_resolved(id, status = "deleted", db_path = db)
  expect_false(bad$success)
  expect_match(bad$error, "invalid status")
  expect_equal(feedback_list(db_path = db)$status, "open")

  ok <- feedback_mark_resolved(id, status = "wont_fix", note = "by design",
                               fix_ref = "PR #9", resolved_by = "admin", db_path = db)
  expect_true(ok$success)
  row <- feedback_list(db_path = db)
  expect_equal(row$status, "wont_fix")
  expect_equal(row$resolution_note, "by design")
  expect_equal(row$fix_commit, "PR #9")
  expect_equal(row$addressed_by, "admin")

  expect_false(feedback_mark_resolved(99999L, db_path = db)$success)
})

test_that("feedback_mark_duplicate records the original and sets status 'duplicate'", {
  skip_if_not_installed("RSQLite")
  source(file.path(app_root, "R/functions/feedback_store.R"), local = TRUE)
  db <- local_db()
  a <- feedback_insert("bug", "plot blank", db_path = db)$id
  b <- feedback_insert("bug", "plot is blank", db_path = db)$id
  expect_true(feedback_mark_duplicate(b, duplicate_of = a, db_path = db)$success)
  row <- feedback_list(db_path = db)
  row <- row[row$id == b, ]
  expect_equal(row$status, "duplicate")
  expect_equal(row$duplicate_of, paste0("id:", a))
  expect_false(feedback_mark_duplicate(b, duplicate_of = b, db_path = db)$success)
  expect_false(feedback_mark_duplicate(99999L, duplicate_of = a, db_path = db)$success)
})

test_that("feedback_set_github_url only accepts https URLs", {
  skip_if_not_installed("RSQLite")
  source(file.path(app_root, "R/functions/feedback_store.R"), local = TRUE)
  db <- local_db()
  id <- feedback_insert("bug", "x", db_path = db)$id
  expect_false(feedback_set_github_url(id, "javascript:alert(1)", db_path = db)$success)
  url <- "https://github.com/razinkele/EcoNeTool/issues/1"
  expect_true(feedback_set_github_url(id, url, db_path = db)$success)
  expect_equal(feedback_list(db_path = db)$github_url, url)
})

test_that("feedback_summary returns the dashboard counts", {
  skip_if_not_installed("RSQLite")
  source(file.path(app_root, "R/functions/feedback_store.R"), local = TRUE)
  db <- local_db()
  b1 <- feedback_insert("bug", "a", db_path = db)$id
  feedback_insert("bug", "b", db_path = db)
  s1 <- feedback_insert("suggestion", "c", db_path = db)$id
  feedback_insert("general", "d", db_path = db)
  feedback_mark_resolved(b1, status = "wont_fix", db_path = db)
  feedback_set_github_url(s1, "https://github.com/o/r/issues/2", db_path = db)
  # Back-date one row past the 7-day window.
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(con, "UPDATE feedback SET created_at = '2020-01-01T00:00:00Z' WHERE id = ?",
                 params = list(s1))
  DBI::dbDisconnect(con)

  s <- feedback_summary(db_path = db)
  expect_equal(s$total, 4L)
  expect_equal(s$bugs, 2L)
  expect_equal(s$suggestions, 1L)
  expect_equal(s$recent, 3L)
  expect_equal(s$github, 1L)
  expect_equal(s$local_only, 3L)
  expect_equal(s$open, 3L)
  expect_equal(s$resolved, 1L)
  expect_equal(s$addressed, 0L)
})
