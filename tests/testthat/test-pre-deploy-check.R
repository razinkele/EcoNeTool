# =============================================================================
# deployment/pre-deploy-check.R parses all of R/ (F83)
# =============================================================================
# The syntax check used to parse only app.R and run_app.R, so a syntax error
# in any sourced module passed the gate. The script itself runs top-level
# code (setwd(".."), package checks, quit()), so the test evaluates only the
# collect_r_syntax_errors() definition taken from the script's parse tree.

load_syntax_checker <- function() {
  script <- file.path(get_app_root(), "deployment", "pre-deploy-check.R")
  exprs <- parse(script, keep.source = FALSE)
  is_def <- vapply(exprs, function(e) {
    is.call(e) && identical(e[[1]], as.name("<-")) &&
      identical(e[[2]], as.name("collect_r_syntax_errors"))
  }, logical(1))
  if (sum(is_def) != 1L) {
    stop("pre-deploy-check.R must define collect_r_syntax_errors() exactly once at top level")
  }
  env <- new.env(parent = baseenv())
  # Safe: evaluates one function *definition* from the repo's own script into
  # a baseenv() child; nothing in the script body runs.
  eval(exprs[[which(is_def)]], env)
  env$collect_r_syntax_errors
}

make_tree <- function(files) {
  root <- tempfile("predeploy_")
  for (rel in names(files)) {
    dir.create(dirname(file.path(root, rel)), recursive = TRUE, showWarnings = FALSE)
    writeLines(files[[rel]], file.path(root, rel))
  }
  root
}

test_that("a syntax error anywhere under R/ is reported, including nested dirs", {
  check <- load_syntax_checker()
  root <- make_tree(c(
    "app.R" = "x <- 1",
    "R/functions/ok.R" = "f <- function() 1",
    "R/modules/bad.R" = "{",
    "R/functions/trait_lookup/bad2.R" = "g <- function( {"
  ))
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  res <- check(root)
  expect_setequal(names(res$errors), c("R/modules/bad.R", "R/functions/trait_lookup/bad2.R"))
  expect_true(all(c("app.R", "R/functions/ok.R") %in% res$files))
  expect_gte(length(res$errors), 1L)
})

test_that("safeBackup copies are skipped", {
  check <- load_syntax_checker()
  root <- make_tree(c("app.R" = "x <- 1", "R/modules/x_safeBackup.R" = "{"))
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  res <- check(root)
  expect_length(res$errors, 0L)
  expect_false(any(grepl("safeBackup", res$files)))
})

test_that("the real tree parses, trait_lookup included", {
  check <- load_syntax_checker()
  res <- check(get_app_root())
  expect_equal(length(res$errors), 0L, info = paste(names(res$errors), collapse = ", "))
  expect_true("R/functions/trait_lookup/orchestrator.R" %in% res$files)
  expect_true("app.R" %in% res$files)
})

test_that("processx is a required package (in-app rebuild, F-d)", {
  code <- readLines(file.path(get_app_root(), "deployment", "pre-deploy-check.R"), warn = FALSE)
  start <- grep("^required_packages <- c\\(", code)
  expect_length(start, 1L)
  end <- start + which(grepl("^\\)", code[-seq_len(start)]))[1]
  block <- paste(code[start:end], collapse = "\n")
  expect_match(block, "\"processx\"", fixed = TRUE)
})

test_that("the script turns every parse error into an ERROR check", {
  code <- readLines(file.path(get_app_root(), "deployment", "pre-deploy-check.R"), warn = FALSE)
  code <- code[!grepl("^\\s*#", code)]
  expect_true(any(grepl('collect_r_syntax_errors(".")', code, fixed = TRUE)))
  expect_true(any(grepl('print_check(paste("Syntax:", f), "ERROR"', code, fixed = TRUE)))
})
