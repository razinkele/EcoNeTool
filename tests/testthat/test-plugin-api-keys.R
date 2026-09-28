# F6 (deep analysis 2026-09-26, spec B section 4.3): saving the API key
# modal with the (never pre-filled) password field left blank overwrote the
# stored AlgaeBase password with "", and the freshwater key was shipped to
# the browser in a plain textInput.

app_root <- get_app_root()

source_plugin_module <- function(env = parent.frame()) {
  withr::local_package("shiny", .local_envir = env)
  source(file.path(app_root, "R/functions/validation_utils.R"), local = FALSE)
  source(file.path(app_root, "R/functions/admin_auth.R"), local = FALSE)
  source(file.path(app_root, "R/modules/plugin_server.R"), local = FALSE)
}

# Point API_KEYS / API_KEYS_JSON / API_KEYS_FILE at a scratch store holding
# known secrets for the calling test; restore the real globals afterwards.
local_key_store <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  keys <- list2env(list(algaebase_username = "old_user",
                        algaebase_password = "old_pass",
                        freshwaterecology_key = "old_key"), parent = emptyenv())
  store <- list(API_KEYS = keys,
                API_KEYS_JSON = file.path(dir, "config", "api_keys.json"),
                API_KEYS_FILE = file.path(dir, "config", "api_keys.R"))
  for (nm in names(store)) local_global_value(nm, store[[nm]], env)
  store
}

local_global_value <- function(nm, value, env) {
  had <- exists(nm, envir = globalenv(), inherits = FALSE)
  old <- if (had) get(nm, envir = globalenv()) else NULL
  assign(nm, value, envir = globalenv())
  withr::defer(
    if (had) assign(nm, old, envir = globalenv()) else rm(list = nm, envir = globalenv()),
    envir = env
  )
}

plugin_test_module <- function() {
  mod <- plugin_server
  formals(mod)$plugin_states <- NULL
  mod
}

submit_keys <- function(session, user, pass, fresh) {
  session$setInputs(api_key_algaebase_user = user, api_key_algaebase_pass = pass,
                    api_key_freshwater = fresh)
  session$setInputs(save_api_keys = 1)
}

test_that("blank secret fields keep the stored secrets (JSON and in-memory)", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  skip_if_not_installed("jsonlite")
  source_plugin_module()
  store <- local_key_store()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = "")  # gate off: modal usable as before

  shiny::testServer(plugin_test_module(), {
    submit_keys(session, user = "new_user", pass = "", fresh = "   ")
  })

  saved <- jsonlite::fromJSON(store$API_KEYS_JSON)
  expect_identical(saved$algaebase_password, "old_pass")
  expect_identical(saved$freshwaterecology_key, "old_key")
  expect_identical(saved$algaebase_username, "new_user")
  expect_identical(store$API_KEYS$algaebase_password, "old_pass")
  expect_identical(store$API_KEYS$freshwaterecology_key, "old_key")
  expect_identical(store$API_KEYS$algaebase_username, "new_user")
})

test_that("a typed secret replaces the stored one", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  skip_if_not_installed("jsonlite")
  source_plugin_module()
  store <- local_key_store()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = "")

  shiny::testServer(plugin_test_module(), {
    submit_keys(session, user = "old_user", pass = "new_pass", fresh = "")
  })

  saved <- jsonlite::fromJSON(store$API_KEYS_JSON)
  expect_identical(saved$algaebase_password, "new_pass")
  expect_identical(saved$freshwaterecology_key, "old_key")
  expect_identical(store$API_KEYS$algaebase_password, "new_pass")
  expect_length(list.files(dirname(store$API_KEYS_JSON), pattern = "\\.tmp\\."), 0)
})

test_that("the key file is owner-only (0600)", {
  skip_on_os("windows")  # Sys.chmod is a no-op there
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  skip_if_not_installed("jsonlite")
  source_plugin_module()
  store <- local_key_store()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = "")

  shiny::testServer(plugin_test_module(), {
    submit_keys(session, user = "u", pass = "p", fresh = "k")
  })

  expect_identical(as.character(file.info(store$API_KEYS_JSON)$mode), "600")
})

test_that("a locked session still cannot save, and nothing is written", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_plugin_module()
  store <- local_key_store()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = paste0(
    "econetool1$12$00112233445566778899aabbccddeeff$",
    "00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff"
  ))

  shiny::testServer(plugin_test_module(), {
    expect_warning(submit_keys(session, user = "x", pass = "y", fresh = "z"),
                   "save_api_keys fired without an unlocked session")
  })
  expect_false(file.exists(store$API_KEYS_JSON))
  expect_identical(store$API_KEYS$algaebase_password, "old_pass")
})

test_that("the modal never sends a stored secret to the browser", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_plugin_module()
  keys <- list(algaebase_username = "old_user", algaebase_password = "old_pass",
               freshwaterecology_key = "old_key")

  html <- as.character(htmltools::renderTags(api_key_modal_dialog(keys))$html)

  expect_false(grepl("old_pass", html, fixed = TRUE))
  expect_false(grepl("old_key", html, fixed = TRUE))
  expect_match(html, "value=\"old_user\"", fixed = TRUE)
  expect_match(html, "id=\"api_key_freshwater\" type=\"password\"", fixed = TRUE)
  expect_match(html, "id=\"api_key_algaebase_pass\" type=\"password\"", fixed = TRUE)
  expect_match(html, "leave blank to keep", fixed = TRUE)
})

test_that("merge_api_key_submission keeps blank, NA and NULL secrets", {
  source_plugin_module()
  stored <- list(algaebase_username = "u", algaebase_password = "p", freshwaterecology_key = "k")
  for (blank in list(NULL, NA_character_, "", "  \t")) {
    merged <- merge_api_key_submission(stored, "u2", blank, blank)
    expect_identical(merged$algaebase_password, "p")
    expect_identical(merged$freshwaterecology_key, "k")
    expect_identical(merged$algaebase_username, "u2")
  }
  expect_identical(merge_api_key_submission(list(), NULL, NULL, NULL),
                   list(algaebase_username = "", algaebase_password = "", freshwaterecology_key = ""))
})
