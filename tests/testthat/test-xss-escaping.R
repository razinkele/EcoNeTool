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

test_that("meta_description treats the EwE -9999 sentinel as missing (F-b)", {
  source(file.path(app_root, "R/functions/validation_utils.R"), local = FALSE)
  for (missing in list(NULL, NA, "", -9999, "-9999", character(0))) {
    expect_null(meta_description(missing))
  }
  expect_match(html_of(meta_description("A <b>model</b>")), "A &lt;b&gt;model&lt;/b&gt;", fixed = TRUE)
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

# ---------------------------------------------------------------------------
# EwE (.ewemdb) preview (R/modules/ecopath_import_server.R)
# ---------------------------------------------------------------------------

evil_preview <- list(
  metadata = list(
    name = payload_img,
    description = payload_img,
    author = payload_script,
    contact = payload_img,
    ecosystem_type = payload_script,
    publication_uri = "javascript:alert(1)"
  ),
  n_groups = 3, n_links = 4,
  filename = "<img src=x onerror=alert(2)>.ewemdb",
  filesize = 2048
)

test_that("EwE preview renders metadata and file name as text and drops javascript: hrefs", {
  skip_if_not_installed("withr")
  skip_if_not_installed("bs4Dash")
  skip_if_not_installed("DT")
  source_xss_modules()

  html <- html_of(ewe_preview_panel(evil_preview))

  expect_match(html, "&lt;img src=x onerror=alert(2)&gt;.ewemdb", fixed = TRUE)
  expect_false(grepl("<img src=x", html, fixed = TRUE))
  expect_false(grepl("<script>", html, fixed = TRUE))
  expect_false(grepl("href=\"javascript", html, fixed = TRUE))
  expect_false(grepl("<a ", html, fixed = TRUE))
  expect_match(html, "javascript:alert(1)", fixed = TRUE)  # shown as text only
})

test_that("a safe EwE publication URL is linked, and a valid DOI wins over it", {
  skip_if_not_installed("withr")
  skip_if_not_installed("bs4Dash")
  skip_if_not_installed("DT")
  source_xss_modules()

  uri_only <- list(metadata = list(publication_uri = "https://example.org/paper"),
                   n_groups = 1, n_links = 0, filename = "m.ewemdb", filesize = 1024)
  expect_match(html_of(ewe_preview_panel(uri_only)), "href=\"https://example.org/paper\"", fixed = TRUE)

  both <- uri_only
  both$metadata$publication_doi <- "10.1000/abc"
  expect_match(html_of(ewe_preview_panel(both)), "href=\"https://doi.org/10.1000/abc\"", fixed = TRUE)
})

test_that("an EwE preview with no metadata still renders", {
  skip_if_not_installed("withr")
  skip_if_not_installed("bs4Dash")
  skip_if_not_installed("DT")
  source_xss_modules()

  html <- html_of(ewe_preview_panel(list(metadata = NULL, n_groups = 0, n_links = 0,
                                         filename = "empty.ewemdb", filesize = 0)))
  expect_match(html, "empty.ewemdb", fixed = TRUE)
  expect_match(html, "Not specified", fixed = TRUE)
})

test_that("the EwE parser error renders as text", {
  skip_if_not_installed("withr")
  skip_if_not_installed("bs4Dash")
  skip_if_not_installed("DT")
  source_xss_modules()

  html <- html_of(ewe_preview_error_panel(paste("bad table", payload_script)))
  expect_match(html, "&lt;script&gt;alert(1)&lt;/script&gt;", fixed = TRUE)
  expect_false(grepl("<script>", html, fixed = TRUE))
})

test_that("the EwE preview output escapes an uploaded file end to end", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  skip_if_not_installed("bs4Dash")
  skip_if_not_installed("DT")
  source_xss_modules()
  local_global_mock("parse_ecopath_native_cross_platform", function(db_file) {
    list(metadata = evil_preview$metadata,
         group_data = data.frame(g = 1:3), diet_data = data.frame(d = 1:4))
  })

  mod <- ecopath_import_server
  formals(mod)[c("net_reactive", "info_reactive", "metaweb_metadata", "dashboard_trigger",
                 "ecopath_import_data", "ecopath_native_status_data", "plugin_states",
                 "euseamap_data", "current_metaweb", "refresh_data_editor")] <- list(NULL)
  shiny::testServer(mod, {
    session$setInputs(ecopath_native_file = list(
      name = evil_preview$filename, size = 2048, datapath = tempfile()
    ))
    html <- output$ecopath_native_preview_ui$html
    expect_match(html, "&lt;img src=x onerror=alert(2)&gt;", fixed = TRUE)
    expect_false(grepl("<img src=x", html, fixed = TRUE))
    expect_false(grepl("href=\"javascript", html, fixed = TRUE))
  })
})

# ---------------------------------------------------------------------------
# Source guard: no dynamic HTML() left in the two modules
# ---------------------------------------------------------------------------

test_that("the two modules paste nothing dynamic into HTML() except the EcoBase model count", {
  for (f in c("R/modules/ecobase_server.R", "R/modules/ecopath_import_server.R")) {
    code <- readLines(file.path(app_root, f), warn = FALSE)
    code <- code[!grepl("^\\s*#", code)]  # whole-line comments only; '#' also starts CSS colours
    hits <- grep("HTML\\((paste0?|sprintf)\\(", code)
    calls <- vapply(hits, function(i) paste(code[i:min(i + 1L, length(code))], collapse = " "), "")
    offenders <- calls[!grepl("Connected! Found \", nrow(models)", calls, fixed = TRUE)]
    expect_length(offenders, 0)
  }
})
