# =============================================================================
# Shiny must NOT autoload R/*.R before app.R
# =============================================================================
# Shiny (>= 1.5) sources every top-level R/*.R file, alphabetically, BEFORE
# app.R whenever the app directory has an R/ folder. That ran R/config.R and
# R/data_loading.R ahead of R/functions/validation_utils.R, so data_loading.R's
# source-time load_baltic_foodweb() could not see helpers defined there
# (startup halted: could not find function "safe_load_rdata"), and the
# default web was loaded twice per start. app.R sources everything itself, in
# order; R/_disable_autoload.R is Shiny's documented opt-out.

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")

test_that("R/_disable_autoload.R exists so app.R controls the source order", {
  expect_true(file.exists(file.path(app_root, "R", "_disable_autoload.R")))
})

test_that("app.R itself sources every top-level R/*.R file", {
  app <- readLines(file.path(app_root, "app.R"), warn = FALSE)
  top <- setdiff(basename(list.files(file.path(app_root, "R"), pattern = "\\.R$")),
                 "_disable_autoload.R")
  top <- top[!grepl("safeBackup", top)]
  for (f in top) {
    expect_true(any(grepl(sprintf('source\\("R/%s"\\)', f), app, fixed = FALSE)),
                info = sprintf("R/%s is not sourced by app.R; with autoload off it would never load", f))
  }
})
