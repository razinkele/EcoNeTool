library(testthat)
app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")

test_that("API key saving uses JSON, not source()'d R code", {
  server_text <- readLines(file.path(app_root, "R/modules/plugin_server.R"))
  server_joined <- paste(server_text, collapse = "\n")
  expect_true(grepl("write_json", server_joined),
              info = "Must use jsonlite::write_json, not paste0 interpolation")
})
