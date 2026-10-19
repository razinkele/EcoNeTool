# =============================================================================
# feedback_server(): submit modal + Feedback Dashboard (MarineSABRES port)
# =============================================================================
# Flat input/output pattern, so testServer() wraps it in a plain server
# function; the module returns its reactives (`mod`) for the test body.
# Admin = FEEDBACK_ADMIN_KEY in the URL, or a session unlocked through the
# admin password (admin_auth.R). testServer cannot change url_search, so the
# admin tests use the password path.

local({
  root <- get_app_root()
  source(file.path(root, "R/functions/admin_auth.R"), local = FALSE)
  source(file.path(root, "R/functions/feedback_store.R"), local = FALSE)
  source(file.path(root, "R/functions/feedback_analyzer.R"), local = FALSE)
  source(file.path(root, "R/functions/feedback_reporter.R"), local = FALSE)
  source(file.path(root, "R/ui/feedback_ui.R"), local = FALSE)
  source(file.path(root, "R/modules/feedback_server.R"), local = FALSE)
})
suppressPackageStartupMessages({
  library(shiny)
  library(bs4Dash)
  library(shinyBS)  # toggleModal() in the submit handler
})

local_store <- function(env = parent.frame()) {
  db <- tempfile(fileext = ".db")
  withr::local_envvar(FEEDBACK_DB_PATH = db, ECONETOOL_GITHUB_TOKEN = "",
                      FEEDBACK_ADMIN_KEY = "", .local_envir = env)
  withr::defer(unlink(c(db, paste0(db, "-wal"), paste0(db, "-shm"))), envir = env)
  db
}

# get_version() comes from R/config.R in the app; stub it here.
assign("get_version", function(...) "test", envir = globalenv())

app <- function(input, output, session) {
  mod <- feedback_server(input, output, session)
}

as_admin <- function(session) {
  session$userData$admin_unlocked <- TRUE
  session$setInputs(sidebar_menu = "feedback_admin")
}

test_that("feedback_ui is the feedback_admin tab with the dashboard placeholder", {
  ui <- as.character(feedback_ui())
  expect_match(ui, "tab-feedback_admin")
  expect_match(ui, "Feedback Dashboard")
  expect_match(ui, "feedback_dashboard_ui")
})

test_that(".feedback_admin_allowed fails closed and accepts key or strict unlock", {
  withr::local_envvar(FEEDBACK_ADMIN_KEY = "", ECONETOOL_ADMIN_PASSWORD_HASH = "")
  expect_false(.feedback_admin_allowed("?admin=", NULL))
  expect_false(.feedback_admin_allowed("", TRUE))          # gate unset: strict refuses
  withr::local_envvar(FEEDBACK_ADMIN_KEY = "s3cret")
  expect_true(.feedback_admin_allowed("?admin=s3cret", NULL))
  expect_false(.feedback_admin_allowed("?admin=wrong", NULL))
  withr::local_envvar(FEEDBACK_ADMIN_KEY = "", ECONETOOL_ADMIN_PASSWORD_HASH = "configured")
  expect_true(.feedback_admin_allowed("", TRUE))
  expect_false(.feedback_admin_allowed("", FALSE))
})

test_that("submit stores title, steps and type; empty title is refused", {
  db <- local_store()
  testServer(app, {
    session$setInputs(feedback_type = "bug", feedback_title = "", feedback_description = "x",
                      feedback_submit = 1)
    expect_equal(nrow(feedback_list(db_path = db)), 0L)

    session$setInputs(feedback_title = "Blank plot", feedback_description = "plot is blank",
                      feedback_steps = "1. open", feedback_email = "", feedback_submit = 2)
    row <- feedback_list(db_path = db)
    expect_equal(nrow(row), 1L)
    expect_equal(row$title, "Blank plot")
    expect_equal(row$steps, "1. open")

    # A second submit inside 30 s is rate-limited
    session$setInputs(feedback_title = "Again", feedback_description = "again",
                      feedback_submit = 3)
    expect_equal(nrow(feedback_list(db_path = db)), 1L)
  })
})

test_that("steps are dropped for non-bug feedback", {
  db <- local_store()
  testServer(app, {
    session$setInputs(feedback_type = "suggestion", feedback_title = "Idea",
                      feedback_description = "add export", feedback_steps = "stale",
                      feedback_submit = 1)
    expect_true(is.na(feedback_list(db_path = db)$steps))
  })
})

test_that("non-admins see the list without descriptions; status filter applies", {
  db <- local_store()
  a <- feedback_insert("bug", "secret detail", title = "Crash", db_path = db)$id
  feedback_insert("suggestion", "more", title = "Idea", db_path = db)
  feedback_mark_resolved(a, status = "addressed", db_path = db)
  testServer(app, {
    expect_false(mod$is_admin())
    session$setInputs(feedback_status_filter = "open")
    expect_equal(mod$filtered()$title, "Idea")
    session$setInputs(feedback_status_filter = "resolved")
    expect_equal(mod$filtered()$title, "Crash")
    session$setInputs(feedback_status_filter = "all")
    expect_equal(nrow(mod$filtered()), 2L)
    expect_false("submitter_email" %in% names(mod$filtered()))
    expect_false(grepl("Duplicate Detection", as.character(output$feedback_dashboard_ui$html)))
    expect_false(grepl("secret detail", as.character(output$feedback_table)))
    expect_equal(mod$summary()$total, 2L)
    expect_match(as.character(output$feedback_vbox_total$html), "Total reports")
  })
})

test_that("admin actions are refused for a non-admin session", {
  db <- local_store()
  id <- feedback_insert("bug", "x", title = "t", db_path = db)$id
  testServer(app, {
    session$setInputs(feedback_mark_addr_click = list(id = id, rand = 1))
    session$setInputs(feedback_mark_status = "addressed", feedback_mark_note = "n",
                      feedback_mark_fix_ref = "", feedback_do_mark = 1)
    expect_equal(feedback_list(db_path = db)$status, "open")
  })
})

test_that("admin can mark addressed with note + fix ref, and the list refreshes", {
  db <- local_store()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = "configured")
  id <- feedback_insert("bug", "x", title = "t", db_path = db)$id
  testServer(app, {
    as_admin(session)
    expect_true(mod$is_admin())
    expect_true(grepl("Duplicate Detection", as.character(output$feedback_dashboard_ui$html)))
    session$setInputs(feedback_status_filter = "open")
    expect_equal(nrow(mod$filtered()), 1L)
    session$setInputs(feedback_mark_addr_click = list(id = id, rand = 1))
    session$setInputs(feedback_mark_status = "wont_fix", feedback_mark_note = "by design",
                      feedback_mark_fix_ref = "PR #1", feedback_do_mark = 1)
    row <- feedback_list(db_path = db)
    expect_equal(row$status, "wont_fix")
    expect_equal(row$resolution_note, "by design")
    expect_equal(nrow(mod$filtered()), 0L)
  })
})

test_that("admin duplicate detection finds the pair and marks the newer one", {
  db <- local_store()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = "configured")
  a <- feedback_insert("bug", "the cod biomass plot is blank", title = "Blank plot", db_path = db)$id
  b <- feedback_insert("bug", "the cod biomass plot is blank", title = "Blank plot", db_path = db)$id
  feedback_insert("suggestion", "please add a csv export button", title = "Export", db_path = db)
  testServer(app, {
    as_admin(session)
    session$setInputs(feedback_similarity_threshold = 0.7, feedback_find_duplicates = 1)
    pairs <- mod$dup_pairs()
    expect_equal(nrow(pairs), 1L)
    expect_equal(c(pairs$id_a, pairs$id_b), c(a, b))

    session$setInputs(feedback_mark_dup = list(id = b, dup_of = a, rand = 1))
    row <- feedback_list(db_path = db)
    expect_equal(row$status[row$id == b], "duplicate")
    expect_equal(row$duplicate_of[row$id == b], paste0("id:", a))
    expect_equal(nrow(mod$dup_pairs()), 0L)
  })
})

test_that("escape_feedback_display_cols escapes user-text columns only", {
  df <- data.frame(Title = "<script>x</script>", Action = "<button>ok</button>",
                   stringsAsFactors = FALSE)
  out <- escape_feedback_display_cols(df, c("Title", "Missing"))
  expect_equal(out$Title, "&lt;script&gt;x&lt;/script&gt;")
  expect_equal(out$Action, "<button>ok</button>")
})
