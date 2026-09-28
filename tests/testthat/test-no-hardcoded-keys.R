# Guard: no API-key-shaped literal (UUID) may be committed in app code or
# config templates. Keys belong in the gitignored config/api_keys.json.

local({
  root <- get_app_root()
  source(file.path(root, "R/functions/validation_utils.R"), local = FALSE)
})

test_that("no UUID-shaped API key literal is committed in R code or config templates", {
  root <- app_path(".")
  files <- c(
    list.files(file.path(root, "R"), pattern = "\\.R$", recursive = TRUE, full.names = TRUE),
    file.path(root, "app.R"),
    list.files(file.path(root, "config"), pattern = "\\.template$", full.names = TRUE)
  )
  # OneDrive conflict copies (gitignored *-safeBackup-*) are not part of the app.
  files <- files[!grepl("safeBackup", basename(files), fixed = TRUE)]
  expect_gt(length(files), 10)
  uuid <- "[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}"
  offenders <- character(0)
  for (f in files) {
    hits <- grep(uuid, readLines(f, warn = FALSE), value = FALSE)
    if (length(hits) > 0) offenders <- c(offenders, sprintf("%s:%s", basename(f), paste(hits, collapse = ",")))
  }
  expect_identical(offenders, character(0))
})

test_that("the freshwaterecology key defaults to empty so get_api_key() reports it unset", {
  cfg <- readLines(app_path("R/config.R"), warn = FALSE)
  line <- grep("freshwaterecology_key\\s*=", cfg, value = TRUE)
  expect_length(line, 1)
  expect_match(line, "freshwaterecology_key\\s*=\\s*\"\"")
})
