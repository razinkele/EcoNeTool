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
