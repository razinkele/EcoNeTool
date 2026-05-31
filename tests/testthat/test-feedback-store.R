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
