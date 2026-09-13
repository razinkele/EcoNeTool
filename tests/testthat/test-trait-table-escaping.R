# =============================================================================
# Trait research table: stored XSS via escape = FALSE (#30)
# =============================================================================
# The results table renders provenance badges, so it needs HTML in the trait
# columns. It got that with a blanket escape = FALSE, which also unescaped
# `species` and `sources` - both carrying values straight from a user-uploaded
# CSV. A species name of `<img src=x onerror=...>` is then stored in the
# session's results and executed in the browser of anyone viewing that table.
#
# Two independent defences are required, because they cover different columns:
#   1. Escape every column that does NOT need HTML (species, sources, ...).
#   2. Escape the interpolated values inside the badge HTML itself, since
#      those columns must stay unescaped.

source_app_dependencies()

local({
  root <- get_app_root()
  source(file.path(root, "R/modules/trait_research_server.R"), local = FALSE)
})

PAYLOAD <- '<img src=x onerror="alert(1)">'

# ---------------------------------------------------------------------------
# Defence 2: the badge builder must not interpolate raw HTML
# ---------------------------------------------------------------------------

test_that("format_trait_badge escapes HTML in the trait value", {
  html <- format_trait_badge(PAYLOAD, "FishBase")

  expect_false(grepl("<img", html, fixed = TRUE),
               info = "raw <img> survived into the rendered cell")
  expect_true(grepl("&lt;img", html, fixed = TRUE))
})

test_that("format_trait_badge escapes HTML in the source label", {
  html <- format_trait_badge("MS3", PAYLOAD)

  expect_false(grepl("<img", html, fixed = TRUE))
  expect_true(grepl("&lt;img", html, fixed = TRUE))
})

test_that("format_trait_badge still emits the badge markup for clean input", {
  html <- format_trait_badge("MS3", "FishBase")

  expect_true(grepl("<strong>MS3</strong>", html, fixed = TRUE))
  expect_true(grepl("<span style=", html, fixed = TRUE))
  expect_true(grepl("FishBase", html, fixed = TRUE))
})

test_that("format_trait_badge handles NA and empty values unchanged", {
  expect_true(grepl("-", format_trait_badge(NA_character_, "FishBase"), fixed = TRUE))
  expect_true(grepl("-", format_trait_badge("", "FishBase"), fixed = TRUE))
})

test_that("source_badge_color never returns attacker-controlled text", {
  # The colour is interpolated into a style attribute; it must come from the
  # fixed palette, never from the source string.
  col <- source_badge_color(PAYLOAD)

  expect_match(col, "^#[0-9a-fA-F]{6}$")
  expect_equal(unname(col), "#9e9e9e")
})

# ---------------------------------------------------------------------------
# Defence 1: only the badge columns may skip escaping
# ---------------------------------------------------------------------------

test_that("badge_escape_columns escapes the user-controlled columns", {
  display_df <- data.frame(
    species = "a", MS = "b", FS = "c", sources = "d", confidence = 1,
    stringsAsFactors = FALSE
  )
  idx <- badge_escape_columns(display_df, c("MS", "FS"))

  expect_equal(sort(idx), c(1L, 4L, 5L))          # species, sources, confidence
  expect_false(2L %in% idx)                        # MS keeps its HTML
  expect_false(3L %in% idx)                        # FS keeps its HTML
})

test_that("badge_escape_columns escapes everything when no badge columns exist", {
  display_df <- data.frame(species = "a", sources = "b", stringsAsFactors = FALSE)
  expect_equal(badge_escape_columns(display_df, character(0)), c(1L, 2L))
})

test_that("badge_escape_columns ignores badge names absent from the frame", {
  display_df <- data.frame(species = "a", MS = "b", stringsAsFactors = FALSE)
  expect_equal(badge_escape_columns(display_df, c("MS", "ST", "TT")), 1L)
})

# ---------------------------------------------------------------------------
# Regression guard
# ---------------------------------------------------------------------------

test_that("trait_research_server.R does not blanket-disable escaping", {
  code <- readLines(app_path("R/modules/trait_research_server.R"), warn = FALSE)
  code <- code[!startsWith(trimws(code), "#")]

  offenders <- grep("escape *= *FALSE", code, value = TRUE)

  expect_equal(length(offenders), 0L,
               label = paste("blanket escape=FALSE:",
                             paste(trimws(offenders), collapse = " | ")))
})
