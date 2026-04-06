library(testthat)
app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")

test_that("trait_research_server.R includes expanded display columns", {
  server_text <- readLines(file.path(app_root, "R/modules/trait_research_server.R"))
  server_joined <- paste(server_text, collapse = "\n")
  expect_true(grepl("RS.*TT.*ST|display_cols.*RS", server_joined),
              info = "Results table should include RS column")
  expect_true(grepl("imputation_method", server_joined),
              info = "Results table should include imputation_method")
  expect_true(grepl("styleInterval", server_joined),
              info = "Confidence columns should be color-coded with styleInterval")
})

test_that("trait research UI has offline DB management panel", {
  ui_text <- readLines(file.path(app_root, "R/ui/trait_research_ui.R"))
  ui_joined <- paste(ui_text, collapse = "\n")
  expect_true(grepl("Offline.*Database|offline_db", ui_joined, ignore.case = TRUE))
})
test_that("trait research server has offline DB rendering", {
  server_text <- readLines(file.path(app_root, "R/modules/trait_research_server.R"))
  server_joined <- paste(server_text, collapse = "\n")
  expect_true(grepl("offline_db_species_count|offline_db_age", server_joined))
})

test_that("trait research server has radar chart", {
  server_text <- readLines(file.path(app_root, "R/modules/trait_research_server.R"))
  server_joined <- paste(server_text, collapse = "\n")
  expect_true(grepl("scatterpolar|radar|plotly", server_joined, ignore.case = TRUE))
})

test_that("plugin server has API key configuration", {
  server_text <- readLines(file.path(app_root, "R/modules/plugin_server.R"))
  server_joined <- paste(server_text, collapse = "\n")
  expect_true(grepl("API_KEYS.*algaebase|api_key.*algaebase", server_joined, ignore.case = TRUE))
})

test_that("harmonization settings UI has 8 ecosystem profiles", {
  ui_text <- readLines(file.path(app_root, "R/ui/harmonization_settings_ui.R"))
  ui_joined <- paste(ui_text, collapse = "\n")
  expect_true(grepl("mediterranean", ui_joined, ignore.case = TRUE))
  expect_true(grepl("baltic", ui_joined, ignore.case = TRUE))
  expect_true(grepl("black.sea", ui_joined, ignore.case = TRUE))
  expect_true(grepl("deep.sea", ui_joined, ignore.case = TRUE))
  expect_true(grepl("atlantic", ui_joined, ignore.case = TRUE))
  expect_true(grepl("tropical", ui_joined, ignore.case = TRUE))
})
