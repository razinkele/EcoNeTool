# =============================================================================
# CI parses all of R/ and runs the offline testthat suite (F82)
# =============================================================================
# The parse steps in ci.yml and r-check.yml used a fixed, non-recursive
# directory list that skipped R/functions/trait_lookup/, and no job ran
# testthat on push/PR. These tests run each workflow's own parse script
# against a scratch tree and check the offline job's wiring.

read_workflow <- function(file) {
  yaml::read_yaml(file.path(get_app_root(), ".github", "workflows", file))
}

workflow_step_run <- function(file, job, step) {
  steps <- read_workflow(file)$jobs[[job]]$steps
  hit <- Filter(function(s) identical(s$name, step), steps)
  if (length(hit) != 1L) stop(sprintf("%s: job %s has no single step '%s'", file, job, step))
  hit[[1]]$run
}

# Runs an Rscript step body with `root` as working directory; returns the
# exit status (0 = success).
run_step_in <- function(script, root) {
  q <- function(x) if (.Platform$OS.type == "windows") shQuote(x, type = "cmd") else shQuote(x)
  f <- tempfile(fileext = ".R")
  writeLines(script, f)
  on.exit(unlink(f), add = TRUE)
  out <- withr::with_dir(root, suppressWarnings(
    system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", q(f)), stdout = TRUE, stderr = TRUE)
  ))
  status <- attr(out, "status")
  if (is.null(status)) 0L else status
}

scratch_tree <- function(bad_file = NULL) {
  root <- tempfile("ci_parse_")
  dir.create(file.path(root, "R", "functions", "trait_lookup"), recursive = TRUE)
  writeLines("x <- 1", file.path(root, "app.R"))
  writeLines("f <- function() 1", file.path(root, "R", "functions", "trait_lookup", "ok.R"))
  if (!is.null(bad_file)) {
    dir.create(dirname(file.path(root, bad_file)), recursive = TRUE, showWarnings = FALSE)
    writeLines("{", file.path(root, bad_file))
  }
  root
}

parse_steps <- list(
  list(file = "ci.yml", job = "r-syntax", step = "Parse-check all R files"),
  list(file = "r-check.yml", job = "r-validation", step = "Validate all R file syntax")
)

test_that("both workflow parse steps fail on a syntax error in R/functions/trait_lookup/", {
  skip_if_not_installed("yaml")
  for (ps in parse_steps) {
    script <- workflow_step_run(ps$file, ps$job, ps$step)
    clean <- scratch_tree()
    bad <- scratch_tree("R/functions/trait_lookup/broken.R")
    on.exit(unlink(c(clean, bad), recursive = TRUE), add = TRUE)
    expect_equal(run_step_in(script, clean), 0L, info = paste(ps$file, "clean tree"))
    expect_gt(run_step_in(script, bad), 0L, label = paste(ps$file, "exit status with a broken trait_lookup file"))
  }
})

test_that("ci.yml runs the offline testthat suite and CI Status depends on it", {
  skip_if_not_installed("yaml")
  wf <- read_workflow("ci.yml")
  job <- wf$jobs[["testthat-offline"]]
  expect_false(is.null(job), info = "no testthat-offline job in ci.yml")
  expect_equal(job[["timeout-minutes"]], 20L)
  expect_null(job$env$RUN_LIVE_TESTS)

  runs <- paste(
    vapply(job$steps, function(s) if (is.null(s$run)) "" else s$run, character(1)),
    collapse = "\n"
  )
  expect_match(runs, "testthat::test_dir(", fixed = TRUE)
  expect_match(runs, '"tests/testthat"', fixed = TRUE)
  expect_match(runs, "stop_on_failure = TRUE", fixed = TRUE)
  expect_false(grepl("RUN_LIVE_TESTS", runs, fixed = TRUE))

  expect_true("testthat-offline" %in% unlist(wf$jobs[["ci-status"]]$needs))
  status_run <- wf$jobs[["ci-status"]]$steps[[1]]$run
  expect_match(status_run, "needs.testthat-offline.result", fixed = TRUE)
})

# Packages confirmed (2026-09-27, F-A) to be genuinely required - not merely
# transitively installed - by the offline suite: their loading namespaces
# were derived empirically by running the full offline suite once and
# diffing loadedNamespaces() against installed.packages(priority = c("base",
# "recommended")). data.table is called unguarded (no requireNamespace /
# skip_if) by make_diet() in test-deep-analysis-fixes.R, used by 3
# test_that() blocks - if data.table is missing, those blocks error rather
# than skip. This guard pins that package's presence in the install list so
# it can't silently regress back out.
test_that("testthat-offline installs packages the suite unguardedly needs", {
  skip_if_not_installed("yaml")
  wf <- read_workflow("ci.yml")
  job <- wf$jobs[["testthat-offline"]]
  install_step <- Filter(function(s) identical(s$name, "Install R dependencies"), job$steps)
  expect_equal(length(install_step), 1L, info = "no single 'Install R dependencies' step")
  pkgs <- install_step[[1]]$with$packages

  required <- c("data.table")
  for (p in required) {
    expect_match(pkgs, paste0("any::", p), fixed = TRUE, label = paste("package list contains", p))
  }
})

# The nightly live job runs the same test_dir() as the offline job plus the
# live tests, so it needs at least the offline job's R packages - and the
# system libraries those packages build against (sf needs GDAL/GEOS/PROJ/
# udunits). Run 36293106958 failed on master with "no package called 'DT'"
# because the nightly list had drifted behind (M6).
job_r_packages <- function(file, job) {
  steps <- read_workflow(file)$jobs[[job]]$steps
  step <- Filter(function(s) identical(s$name, "Install R dependencies"), steps)
  if (length(step) != 1L) stop(sprintf("%s/%s: no single 'Install R dependencies' step", file, job))
  pkgs <- trimws(strsplit(step[[1]]$with$packages, "\n", fixed = TRUE)[[1]])
  sub("^any::", "", pkgs[nzchar(pkgs)])
}

job_apt_packages <- function(file, job) {
  run <- workflow_step_run(file, job, "Install system dependencies")
  tokens <- strsplit(gsub("\\\\", " ", run), "\\s+")[[1]]
  grep("^lib[[:alnum:]._+-]+$", tokens, value = TRUE)
}

# test-rebuild-observer.R runs trait_research_server() under testServer();
# the module builds plotly / DT outputs at start-up and launches the build
# with processx. PR #13 CI errored with "there is no package called
# 'plotly'" because the install list lacked it.
test_that("testthat-offline installs every package trait_research_server() calls via ::", {
  skip_if_not_installed("yaml")
  offline <- job_r_packages("ci.yml", "testthat-offline")
  src <- readLines(file.path(get_app_root(), "R/modules/trait_research_server.R"), warn = FALSE)
  src <- src[!grepl("^\\s*#", src)]
  used <- unique(sub("::$", "", unlist(regmatches(src, gregexpr("\\b[A-Za-z][A-Za-z0-9.]*::", src)))))
  base_pkgs <- rownames(installed.packages(priority = "base"))
  needed <- setdiff(used, base_pkgs)
  expect_true(all(c("plotly", "DT", "processx") %in% needed), info = "premise: the module uses these")
  expect_equal(setdiff(needed, offline), character(0))
})

test_that("the nightly live job installs every package the offline job installs", {
  skip_if_not_installed("yaml")
  offline <- job_r_packages("ci.yml", "testthat-offline")
  nightly <- job_r_packages("nightly-live-tests.yml", "live-tests")
  expect_true(all(c("DT", "data.table") %in% offline), info = "premise: offline list has DT and data.table")
  expect_equal(setdiff(offline, nightly), character(0))

  offline_apt <- job_apt_packages("ci.yml", "testthat-offline")
  nightly_apt <- job_apt_packages("nightly-live-tests.yml", "live-tests")
  expect_gt(length(offline_apt), 0L)
  expect_equal(setdiff(offline_apt, nightly_apt), character(0))
})
