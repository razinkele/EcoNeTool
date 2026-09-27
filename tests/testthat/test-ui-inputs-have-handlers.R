# Spec B section 4.1 (F75): the Harmonization tab shipped seven rule
# checkboxes, six FS pattern inputs, a profile-effects panel and a Cancel
# button that no server code ever read. These guards pin the UI to the config
# keys and rules the harmonizer actually uses.

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")

test_that("the rule checkboxes are exactly the rules the harmonize_* code reads", {
  source(file.path(app_root, "R/ui/harmonization_settings_ui.R"), local = FALSE)
  files <- list.files(file.path(app_root, "R"), pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  code <- unlist(lapply(files, readLines, warn = FALSE))
  code <- code[!startsWith(trimws(code), "#")]
  calls <- regmatches(code, gregexpr('is_rule_enabled\\("[A-Za-z0-9_]+"\\)', code))
  read_rules <- unique(sub('is_rule_enabled\\("(.*)"\\)', "\\1", unlist(calls)))

  expect_gt(length(read_rules), 5L)
  expect_setequal(names(get0("CONSUMED_TAXONOMIC_RULES", ifnotfound = character())), read_rules)
})

test_that("the FS pattern inputs cover every configured foraging pattern", {
  source(file.path(app_root, "R/config/harmonization_config.R"), local = FALSE)
  source(file.path(app_root, "R/ui/harmonization_settings_ui.R"), local = FALSE)
  expect_setequal(names(get0("HARM_FS_PATTERN_LABELS", ifnotfound = character())),
                  names(HARMONIZATION_CONFIG$foraging_patterns))
})

rendered_harm_ids <- function() {
  suppressPackageStartupMessages(library(shiny)) # the UI builders are unqualified, as in app.R
  source(file.path(app_root, "R/ui/harmonization_settings_ui.R"), local = FALSE)
  html <- as.character(harmonization_settings_ui())
  hits <- regmatches(html, gregexpr('\\sid="harm_[A-Za-z0-9_]+"', html))[[1]]
  ids <- unique(sub('^\\sid="(.*)"$', "\\1", hits))
  # fileInput() adds a "<id>_progress" bar div of its own; it is not an input.
  ids[!grepl("_progress$", ids)]
}

server_src <- function() {
  paste(readLines(file.path(app_root, "R/modules/harmonization_settings_server.R"), warn = FALSE),
        collapse = "\n")
}

test_that("every harm_* element in the harmonization UI has a server consumer", {
  skip_if_not_installed("shiny")
  ids <- rendered_harm_ids()
  src <- server_src()

  used <- regmatches(src, gregexpr("(input|output)\\$harm_[A-Za-z0-9_]+", src))[[1]]
  used <- unique(sub("^(input|output)\\$", "", used))
  generated <- c(
    paste0("harm_rule_", names(get0("CONSUMED_TAXONOMIC_RULES", ifnotfound = character()))),
    paste0("harm_pattern_", names(get0("HARM_FS_PATTERN_LABELS", ifnotfound = character())))
  )

  expect_gt(length(ids), 20L)
  expect_identical(setdiff(ids, c(used, generated)), character(0))
})

test_that("generated widget IDs are wired from the same vectors the UI uses", {
  src <- server_src()
  expect_true(grepl('paste0("harm_rule_", rule)', src, fixed = TRUE))
  expect_true(grepl('paste0("harm_pattern_", key)', src, fixed = TRUE))
  expect_true(grepl("names(CONSUMED_TAXONOMIC_RULES)", src, fixed = TRUE))
  expect_true(grepl("names(HARM_FS_PATTERN_LABELS)", src, fixed = TRUE))
})
