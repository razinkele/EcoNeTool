# Spec B section 4.1 (F75): the Harmonization tab shipped seven rule
# checkboxes, six FS pattern inputs, a profile-effects panel and a Cancel
# button that no server code ever read. These guards pin the UI to the config
# keys and rules the harmonizer actually uses.

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")

test_that("the rule checkboxes are exactly the rules the harmonize_* code reads", {
  source(file.path(app_root, "R/config/harmonization_config.R"), local = FALSE)
  source(file.path(app_root, "R/ui/harmonization_settings_ui.R"), local = FALSE)
  files <- list.files(file.path(app_root, "R"), pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  code <- unlist(lapply(files, readLines, warn = FALSE))
  code <- code[!startsWith(trimws(code), "#")]
  calls <- regmatches(code, gregexpr('is_rule_enabled\\("[A-Za-z0-9_]+"\\)', code))
  literal_rules <- sub('is_rule_enabled\\("(.*)"\\)', "\\1", unlist(calls))
  # Trait vocab v2: the MB/EP/PR rules are data (TRAIT_VOCAB$taxon_rules)
  # and apply_taxon_rules() checks each rule's `flag` via is_rule_enabled().
  flag_rules <- unlist(lapply(TRAIT_VOCAB$taxon_rules, function(rules) lapply(rules, `[[`, "flag")))
  read_rules <- unique(c(literal_rules, flag_rules))

  expect_gt(length(read_rules), 5L)
  expect_setequal(names(get0("CONSUMED_TAXONOMIC_RULES", ifnotfound = character())), read_rules)
})

test_that("the FS pattern inputs cover every configured foraging pattern", {
  source(file.path(app_root, "R/config/harmonization_config.R"), local = FALSE)
  source(file.path(app_root, "R/ui/harmonization_settings_ui.R"), local = FALSE)
  expect_setequal(names(get0("HARM_FS_PATTERN_LABELS", ifnotfound = character())),
                  names(HARMONIZATION_CONFIG$foraging_patterns))
})

render_harm_ui <- function() {
  suppressPackageStartupMessages(library(shiny)) # the UI builders are unqualified, as in app.R
  # The UI reads HARM_THRESHOLD_RANGES / HARMONIZATION_CONFIG, as app.R
  # sources the config before R/ui/.
  source(file.path(app_root, "R/config/harmonization_config.R"), local = FALSE)
  source(file.path(app_root, "R/ui/harmonization_settings_ui.R"), local = FALSE)
  as.character(harmonization_settings_ui())
}

rendered_harm_ids <- function(dedupe = TRUE) {
  html <- render_harm_ui()
  hits <- regmatches(html, gregexpr('\\sid="harm_[A-Za-z0-9_]+"', html))[[1]]
  ids <- sub('^\\sid="(.*)"$', "\\1", hits)
  if (dedupe) ids <- unique(ids)
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

# Final fix wave (F-b): the rendered rule checkboxes and FS inputs are exactly
# the wired vectors - no rule dropped from a column, none rendered twice.
test_that("rendered harm_rule_* / harm_pattern_* ids equal the wired vectors exactly", {
  skip_if_not_installed("shiny")
  ids <- rendered_harm_ids(dedupe = FALSE)
  rule_ids <- ids[startsWith(ids, "harm_rule_")]
  pattern_ids <- ids[startsWith(ids, "harm_pattern_")]
  expected_rules <- paste0("harm_rule_", names(CONSUMED_TAXONOMIC_RULES))
  expected_patterns <- paste0("harm_pattern_", names(HARM_FS_PATTERN_LABELS))

  expect_setequal(rule_ids, expected_rules)
  expect_length(rule_ids, length(CONSUMED_TAXONOMIC_RULES))
  expect_setequal(pattern_ids, expected_patterns)
  expect_length(pattern_ids, length(HARM_FS_PATTERN_LABELS))
})

# Final fix wave (I3): the sliders and the validator share HARM_THRESHOLD_RANGES,
# so the browser can never clamp or snap a value the validator accepted.
test_that("each threshold slider's min/max/step and default come from the shared constants", {
  skip_if_not_installed("shiny")
  html <- render_harm_ui()
  for (key in HARM_THRESHOLD_KEYS) {
    id <- paste0("harm_thresh_", key)
    tag <- regmatches(html, regexpr(sprintf('<input[^>]*id="%s"[^>]*>', id), html))
    expect_length(tag, 1L)
    attr_num <- function(name) {
      as.numeric(sub(sprintf('.*\\s%s="([^"]*)".*', name), "\\1", tag))
    }
    rng <- HARM_THRESHOLD_RANGES[[key]]
    expect_equal(attr_num("data-min"), rng[["min"]], info = key)
    expect_equal(attr_num("data-max"), rng[["max"]], info = key)
    expect_equal(attr_num("data-step"), rng[["step"]], info = key)
    expect_equal(attr_num("data-from"), HARMONIZATION_CONFIG$size_thresholds[[key]], info = key)
    # Labels are unchanged: "MS1/MS2 boundary:" etc.
    expect_true(grepl(sprintf('<label[^>]*for="%s"[^>]*>%s boundary:</label>', id, sub("_", "/", key)), html),
                info = key)
  }
})
