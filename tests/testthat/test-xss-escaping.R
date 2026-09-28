# F3 / F54 (deep analysis 2026-09-26, spec B section 4.3): EcoBase metadata
# and uploaded .ewemdb metadata, the upload's file name and parser/connection
# errors were pasted into HTML(). They must render as text, and no href may
# carry anything but http(s) (DOIs are rebuilt on https://doi.org/).

app_root <- get_app_root()

source_xss_modules <- function(env = parent.frame()) {
  # The modules call shiny, DT and bs4Dash (box) unqualified, as app.R attaches
  # them. Attach them only for the calling test.
  for (pkg in c("shiny", "DT", "bs4Dash")) withr::local_package(pkg, .local_envir = env)
  source(file.path(app_root, "R/functions/validation_utils.R"), local = FALSE)
  source(file.path(app_root, "R/modules/ecobase_server.R"), local = FALSE)
  source(file.path(app_root, "R/modules/ecopath_import_server.R"), local = FALSE)
}

# Assign `fn` to `nm` in globalenv for the calling test, restoring (or
# removing) whatever was there before. One call per name, so each deferred
# restore captures its own `nm` (a for-loop would defer the last name only).
local_global_mock <- function(nm, fn, env = parent.frame()) {
  had <- exists(nm, envir = globalenv(), inherits = FALSE)
  old <- if (had) get(nm, envir = globalenv()) else NULL
  assign(nm, fn, envir = globalenv())
  withr::defer(
    if (had) assign(nm, old, envir = globalenv()) else rm(list = nm, envir = globalenv()),
    envir = env
  )
}

html_of <- function(x) paste(as.character(htmltools::renderTags(x)$html), collapse = "\n")

payload_img <- "<img src=x onerror=alert(1)>"
payload_script <- "<script>alert(1)</script>"

evil_ecobase_meta <- list(
  model_name = payload_script,
  ecosystem_name = payload_img,
  description = payload_img,
  author = payload_script,
  contact = "<a href='javascript:alert(1)'>mail</a>",
  institution = "<b onmouseover=alert(1)>Inst</b>",
  ecosystem_type = payload_img,
  doi = "10.1000/x' onmouseover='a",
  # Non-numeric where numbers are expected: must not break the panel
  latitude = "<i>north</i>", longitude = "east",
  area = "<b>big</b>"
)

# ---------------------------------------------------------------------------
# Shared helpers (R/functions/validation_utils.R)
# ---------------------------------------------------------------------------

test_that("safe_href passes only single http(s) URLs", {
  source(file.path(app_root, "R/functions/validation_utils.R"), local = FALSE)
  expect_identical(safe_href("https://example.org/a"), "https://example.org/a")
  expect_identical(safe_href("HTTP://example.org"), "HTTP://example.org")
  expect_null(safe_href("javascript:alert(1)"))
  expect_null(safe_href(" javascript:alert(1)"))
  expect_null(safe_href("data:text/html,<script>alert(1)</script>"))
  expect_null(safe_href("//evil.example"))
  expect_null(safe_href(NA_character_))
  expect_null(safe_href(c("https://a", "https://b")))
  expect_null(safe_href(NULL))
})

test_that("safe_doi_href builds doi.org links for valid DOIs only", {
  source(file.path(app_root, "R/functions/validation_utils.R"), local = FALSE)
  expect_identical(safe_doi_href("10.1016/j.ecolmodel.2004.02.001"),
                   "https://doi.org/10.1016/j.ecolmodel.2004.02.001")
  expect_identical(safe_doi_href("https://doi.org/10.1000/abc"), "https://doi.org/10.1000/abc")
  expect_identical(safe_doi_href("doi: 10.1000/abc"), "https://doi.org/10.1000/abc")
  expect_null(safe_doi_href("10.1000/x' onmouseover='a"))
  expect_null(safe_doi_href("javascript:alert(1)"))
  expect_null(safe_doi_href("not a doi"))
  expect_null(safe_doi_href(NA_character_))
})

test_that("meta_text returns plain text for values and a span for missing ones", {
  source(file.path(app_root, "R/functions/validation_utils.R"), local = FALSE)
  expect_identical(meta_text("Baltic"), "Baltic")
  expect_identical(meta_text(12.5), "12.5")
  for (missing in list(NULL, NA, "", -9999, character(0))) {
    expect_match(html_of(meta_text(missing)), "Not specified")
  }
  expect_match(html_of(meta_text("Not affiliated", missing = c("", "Not affiliated"))), "Not specified")
  # Escaped exactly once by the builder, never pre-escaped
  expect_identical(html_of(htmltools::tags$td(meta_text("a & b"))), "<td>a &amp; b</td>")
})

# ---------------------------------------------------------------------------
# EcoBase metadata panel (R/modules/ecobase_server.R)
# ---------------------------------------------------------------------------

test_that("EcoBase metadata renders as text, with no injected tags or links", {
  skip_if_not_installed("withr")
  skip_if_not_installed("bs4Dash")
  skip_if_not_installed("DT")
  source_xss_modules()

  html <- html_of(ecobase_metadata_panel(evil_ecobase_meta, model_id = 7, model_name = payload_script))

  expect_match(html, "&lt;img", fixed = TRUE)
  expect_false(grepl("<img src=x", html, fixed = TRUE))
  expect_false(grepl("<script>", html, fixed = TRUE))
  expect_false(grepl("<b onmouseover", html, fixed = TRUE))
  expect_false(grepl("href=\"javascript", html, fixed = TRUE))
  expect_false(grepl("<a ", html, fixed = TRUE))  # the invalid DOI yields no link
  expect_match(html, "<td>10.1000/x' onmouseover='a</td>", fixed = TRUE)  # text, not an attribute
})

test_that("a valid EcoBase DOI becomes a doi.org link", {
  skip_if_not_installed("withr")
  skip_if_not_installed("bs4Dash")
  skip_if_not_installed("DT")
  source_xss_modules()

  html <- html_of(ecobase_metadata_panel(list(doi = "10.1000/abc"), model_id = 1, model_name = "M"))
  expect_match(html, "href=\"https://doi.org/10.1000/abc\"", fixed = TRUE)
})

test_that("the EcoBase connection error renders the message as text", {
  skip_if_not_installed("withr")
  skip_if_not_installed("bs4Dash")
  skip_if_not_installed("DT")
  source_xss_modules()

  html <- html_of(ecobase_connection_error_ui(paste("HTTP 500:", payload_script)))
  expect_match(html, "&lt;script&gt;", fixed = TRUE)
  expect_false(grepl("<script>", html, fixed = TRUE))
})

test_that("the EcoBase details output escapes metadata end to end", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  skip_if_not_installed("bs4Dash")
  skip_if_not_installed("DT")
  source_xss_modules()
  local_global_mock("get_ecobase_models", function() {
    data.frame(model.number = "<svg onload=alert(3)>", model.name = payload_script,
               ecosystem = "x", year = 2000)
  })
  local_global_mock("extract_ecobase_metadata", function(model_id) evil_ecobase_meta)

  mod <- ecobase_server
  formals(mod)[c("net_reactive", "info_reactive", "metaweb_metadata",
                 "dashboard_trigger", "refresh_data_editor")] <- list(NULL)
  shiny::testServer(mod, {
    session$setInputs(load_ecobase_models = 1)
    session$setInputs(ecobase_models_table_rows_selected = 1)
    html <- output$ecobase_model_details$html
    expect_match(html, "&lt;img", fixed = TRUE)
    expect_false(grepl("<img src=x", html, fixed = TRUE))
    expect_false(grepl("<script>", html, fixed = TRUE))
    expect_false(grepl("<svg", html, fixed = TRUE))  # the model id from the list table
  })
})
