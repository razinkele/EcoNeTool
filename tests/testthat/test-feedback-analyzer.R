# Unit tests for duplicate detection + the GitHub/issue helpers behind the
# Feedback Dashboard (ported from MarineSABRES; rows keyed by store id).
app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
source(file.path(app_root, "R/functions/feedback_analyzer.R"), local = TRUE)
source(file.path(app_root, "R/functions/feedback_reporter.R"), local = TRUE)
if (requireNamespace("RSQLite", quietly = TRUE)) {
  source(file.path(app_root, "R/functions/feedback_store.R"), local = TRUE)
}

fb_df <- function(title, description, duplicate_of = rep(NA_character_, length(title))) {
  data.frame(id = seq_along(title), title = title, description = description,
             duplicate_of = duplicate_of, stringsAsFactors = FALSE)
}

# ---- compute_text_similarity ------------------------------------------------

test_that("compute_text_similarity returns 1.0 for identical texts", {
  mat <- compute_text_similarity(rep("the quick brown fox jumps over the lazy dog", 2))
  expect_equal(dim(mat), c(2L, 2L))
  expect_equal(mat[1L, 2L], 1.0, tolerance = 1e-9)
  expect_equal(mat[2L, 1L], 1.0, tolerance = 1e-9)
})

test_that("compute_text_similarity returns near 0 for unrelated texts", {
  mat <- compute_text_similarity(c("marine ecology coral reef biodiversity",
                                   "software programming compiler binary tree"))
  expect_lt(mat[1L, 2L], 0.3)
})

test_that("compute_text_similarity returns an NxN matrix with unit diagonal", {
  mat <- compute_text_similarity(c("fish population decline",
                                   "overfishing impact assessment",
                                   "climate change ocean temperature"))
  expect_equal(dim(mat), c(3L, 3L))
  expect_equal(unname(diag(mat)), rep(1.0, 3), tolerance = 1e-9)
})

test_that("compute_text_similarity returns a 0-matrix for a single entry", {
  mat <- compute_text_similarity("only one document here")
  expect_equal(dim(mat), c(1L, 1L))
  expect_equal(mat[1L, 1L], 0.0)
})

test_that("compute_text_similarity handles all-stopword text without NaN", {
  mat <- compute_text_similarity(c("the and or but a an", "is are was were be"))
  expect_equal(dim(mat), c(2L, 2L))
  expect_false(any(is.na(mat)))
})

# ---- find_duplicate_pairs ---------------------------------------------------

test_that("find_duplicate_pairs finds known duplicates above threshold", {
  df <- fb_df(c("App crashes on startup", "App crashes on startup", "Suggestion for UI"),
              c("App crashes immediately when opened", "App crashes immediately when opened",
                "The interface could be improved"))
  pairs <- find_duplicate_pairs(df, threshold = 0.7)
  expect_s3_class(pairs, "data.frame")
  expect_equal(nrow(pairs), 1L)
  expect_setequal(c(pairs$id_a, pairs$id_b), c(1L, 2L))
})

test_that("find_duplicate_pairs returns an empty frame with fewer than 2 entries", {
  cols <- c("id_a", "id_b", "similarity")
  p0 <- find_duplicate_pairs(fb_df(character(0), character(0)))
  p1 <- find_duplicate_pairs(fb_df("Only entry", "desc"))
  expect_equal(nrow(p0), 0L)
  expect_equal(nrow(p1), 0L)
  expect_true(all(cols %in% names(p0)))
  expect_true(all(cols %in% names(p1)))
})

test_that("find_duplicate_pairs excludes rows already marked duplicate", {
  df <- fb_df(c("App crashes on startup", "App crashes on startup", "Network error occurs"),
              c("App crashes immediately when opened", "App crashes immediately when opened",
                "Network disconnects randomly"),
              duplicate_of = c(NA, "id:1", NA))
  pairs <- find_duplicate_pairs(df, threshold = 0.7)
  expect_false(any(pairs$id_a == 2L | pairs$id_b == 2L))
})

test_that("find_duplicate_pairs uses the description when a legacy row has no title", {
  df <- fb_df(c(NA, "Blank cod plot"),
              c("the cod biomass plot is blank", "the cod biomass plot is blank"))
  expect_equal(nrow(find_duplicate_pairs(df, threshold = 0.7)), 1L)
})

# ---- reporter: context, issue body, GitHub ----------------------------------

test_that("feedback_collect_context reports version, tab, browser and web size", {
  input <- list(sidebar_menu = "network", feedback_browser_info = "TestAgent/1.0")
  net <- igraph::make_graph(c("A", "B", "B", "C"), directed = TRUE)
  ctx <- feedback_collect_context(input, net = net, app_version = "1.7.0")
  expect_equal(ctx$app_version, "1.7.0")
  expect_equal(ctx$current_tab, "network")
  expect_equal(ctx$browser_info, "TestAgent/1.0")
  expect_equal(ctx$species_count, 3L)
  expect_equal(ctx$link_count, 2L)

  bare <- feedback_collect_context(list(), net = NULL, app_version = NA)
  expect_equal(bare$current_tab, "unknown")
  expect_equal(bare$species_count, 0L)
})

test_that("feedback_issue_body never contains the submitter email", {
  ctx <- list(app_version = "1.7.0", current_tab = "network")
  body <- feedback_issue_body("it broke", "1. click", ctx)
  expect_match(body, "## Description")
  expect_match(body, "## Steps to Reproduce")
  expect_false(grepl("@", body, fixed = TRUE))
  expect_false(grepl("Steps to Reproduce", feedback_issue_body("idea", "", ctx)))
})

test_that("feedback_github_issue is dormant without ECONETOOL_GITHUB_TOKEN", {
  withr::local_envvar(ECONETOOL_GITHUB_TOKEN = "")
  expect_null(feedback_github_issue("t", "b", "bug"))
})

test_that("feedback_submit stores the row even when GitHub is not configured", {
  skip_if_not_installed("RSQLite")
  withr::local_envvar(ECONETOOL_GITHUB_TOKEN = "")
  db <- tempfile(fileext = ".db")
  withr::defer(unlink(c(db, paste0(db, "-wal"), paste0(db, "-shm"))))
  res <- feedback_submit("bug", title = "Blank plot", description = "the plot is blank",
                         steps = "open it", email = "a@b.org",
                         context = list(app_version = "1.7.0", current_tab = "network"),
                         session_id = "tok", db_path = db)
  expect_true(res$success)
  expect_false(res$github_success)
  row <- feedback_list(include_email = TRUE, db_path = db)
  expect_equal(row$title, "Blank plot")
  expect_equal(row$steps, "open it")
  expect_equal(row$submitter_email, "a@b.org")
  expect_equal(row$tab, "network")
  expect_true(is.na(row$github_url))
})

test_that("feedback_submit records the issue URL when GitHub succeeds", {
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".db")
  withr::defer(unlink(c(db, paste0(db, "-wal"), paste0(db, "-shm"))))
  url <- "https://github.com/razinkele/EcoNeTool/issues/42"
  # The helpers are sourced into this file's env (not a package), so swap the
  # binding feedback_submit() resolves to and restore it afterwards.
  env <- environment(feedback_submit)
  real <- get("feedback_github_issue", envir = env)
  assign("feedback_github_issue",
         function(title, body, labels) list(url = url, number = 42L), envir = env)
  withr::defer(assign("feedback_github_issue", real, envir = env))
  res <- feedback_submit("suggestion", title = "Idea", description = "add a thing",
                         context = list(), db_path = db)
  expect_true(res$github_success)
  expect_equal(feedback_list(db_path = db)$github_url, url)
})
