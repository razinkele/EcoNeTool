# F19 wiring (spec B section 4.3): the "Rebuild Database" observer in
# R/modules/trait_research_server.R must go through the admin gate and the
# process-wide lock, hand the lock token to the child Rscript, and release
# the lock once the child is done. testServer() drives the module's plain
# (non-moduleServer) server function directly.

app_root <- get_app_root()

source_rebuild_module <- function(env = parent.frame()) {
  # The module renders bs4Dash value boxes at start-up; attach bs4Dash only
  # for the calling test so no other test file sees it on the search path.
  withr::local_package("bs4Dash", .local_envir = env)
  source(file.path(app_root, "R/functions/validation_utils.R"), local = FALSE)
  source(file.path(app_root, "R/functions/admin_auth.R"), local = FALSE)
  source(file.path(app_root, "R/functions/offline_db_rebuild.R"), local = FALSE)
  source(file.path(app_root, "R/modules/trait_research_server.R"), local = FALSE)
}

# testServer() refuses `args` for a plain (non-moduleServer) function, so give
# shared_data a default instead. The module's locals (offline_rebuild_process,
# rebuild_lock_dir, ...) stay visible inside the testServer block.
rebuild_test_module <- function() {
  mod <- trait_research_server
  formals(mod)$shared_data <- NULL
  mod
}

# A scratch app root (app.R + R/functions marker, so app_path() accepts it)
# whose fake build script sleeps FAKE_BUILD_SLEEP seconds, records the token it
# was handed and the token in the lock's owner file, then prints the summary
# line the poller parses. Unlike the real script it never releases the lock,
# so any release observed here was done by the Shiny side.
local_fake_app_root <- function(env = parent.frame()) {
  root <- normalizePath(withr::local_tempdir(.local_envir = env), winslash = "/")
  dir.create(file.path(root, "R", "functions"), recursive = TRUE)
  dir.create(file.path(root, "scripts", "initialization"), recursive = TRUE)
  dir.create(file.path(root, "cache"))
  writeLines("# marker", file.path(root, "app.R"))
  writeLines(c(
    'Sys.sleep(as.numeric(Sys.getenv("FAKE_BUILD_SLEEP", "0")))',
    'tok <- Sys.getenv("ECONETOOL_REBUILD_LOCK_TOKEN")',
    'owner <- readLines("cache/offline_traits.db.lock/owner")',
    'writeLines(c(tok, sub("^token=", "", grep("^token=", owner, value = TRUE))), "token_seen.txt")',
    'cat("Total species in database: 7", "\\n")'
  ), file.path(root, "scripts", "initialization", "build_offline_trait_db.R"))
  withr::local_options(econetool.app_root = root, .local_envir = env)
  root
}

# Any non-blank value switches the gate on; verification is never reached
# because these tests set session$userData$admin_unlocked directly.
gate_hash <- paste0("econetool1$12$00112233445566778899aabbccddeeff$",
                    "00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff")

test_that("with the gate unset a click is refused, warns, and starts nothing", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  skip_if_not_installed("bs4Dash")
  source_rebuild_module()
  root <- local_fake_app_root()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = "")

  shiny::testServer(rebuild_test_module(), {
    session$userData$admin_unlocked <- TRUE  # even an "unlocked" flag cannot open an unset gate
    expect_warning(session$setInputs(rebuild_offline_db = 1),
                   "rebuild_offline_db refused: admin gate not configured")
    expect_null(offline_rebuild_process())
  })
  expect_false(dir.exists(file.path(root, "cache", "offline_traits.db.lock")))
  expect_false(file.exists(file.path(root, "token_seen.txt")))
})

test_that("with the gate set, a locked session is refused", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  skip_if_not_installed("bs4Dash")
  source_rebuild_module()
  root <- local_fake_app_root()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = gate_hash)

  shiny::testServer(rebuild_test_module(), {
    expect_warning(session$setInputs(rebuild_offline_db = 1), "without an unlocked session")
    expect_null(offline_rebuild_process())
  })
  expect_false(dir.exists(file.path(root, "cache", "offline_traits.db.lock")))
  expect_false(file.exists(file.path(root, "token_seen.txt")))
})

test_that("a click while another build holds the lock starts nothing and leaves that lock alone", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  skip_if_not_installed("bs4Dash")
  source_rebuild_module()
  root <- local_fake_app_root()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = gate_hash)
  lock_dir <- file.path(root, "cache", "offline_traits.db.lock")
  held <- acquire_rebuild_lock(lock_dir)

  shiny::testServer(rebuild_test_module(), {
    session$userData$admin_unlocked <- TRUE
    session$setInputs(rebuild_offline_db = 1)
    expect_null(offline_rebuild_process())
  })
  expect_false(file.exists(file.path(root, "token_seen.txt")))
  expect_identical(.read_rebuild_lock_token(lock_dir), held$token)
})

test_that("an unlocked admin's build receives the lock token and the lock is released after it exits", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  skip_if_not_installed("bs4Dash")
  skip_if_not_installed("processx")
  source_rebuild_module()
  root <- local_fake_app_root()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = gate_hash)
  lock_dir <- file.path(root, "cache", "offline_traits.db.lock")

  shiny::testServer(rebuild_test_module(), {
    session$userData$admin_unlocked <- TRUE
    session$setInputs(rebuild_offline_db = 1)
    # The fake build can finish inside the same flush; wait only if it has not.
    proc <- offline_rebuild_process()
    if (!is.null(proc)) proc$wait(30000)
    session$elapse(2100)
    expect_null(offline_rebuild_process())
  })

  seen <- readLines(file.path(root, "token_seen.txt"))
  expect_length(seen, 2L)
  expect_match(seen[1], "^[0-9]+-")
  expect_identical(seen[1], seen[2])
  expect_false(dir.exists(lock_dir))
})

test_that("closing the tab while the build runs leaves the lock to the build", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  skip_if_not_installed("bs4Dash")
  skip_if_not_installed("processx")
  source_rebuild_module()
  root <- local_fake_app_root()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = gate_hash, FAKE_BUILD_SLEEP = "20")
  lock_dir <- file.path(root, "cache", "offline_traits.db.lock")
  proc <- NULL

  shiny::testServer(rebuild_test_module(), {
    session$userData$admin_unlocked <- TRUE
    session$setInputs(rebuild_offline_db = 1)
    proc <<- offline_rebuild_process()
    expect_true(proc$is_alive())
    session$close()
  })
  withr::defer(if (proc$is_alive()) proc$kill())

  expect_true(dir.exists(lock_dir))
})

test_that("the build is not supervised, so the worker exiting does not kill it (I1)", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  skip_if_not_installed("bs4Dash")
  skip_if_not_installed("processx")
  source_rebuild_module()
  root <- local_fake_app_root()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = gate_hash, FAKE_BUILD_SLEEP = "20")
  proc <- NULL

  shiny::testServer(rebuild_test_module(), {
    session$userData$admin_unlocked <- TRUE
    session$setInputs(rebuild_offline_db = 1)
    proc <<- offline_rebuild_process()
  })
  withr::defer(if (proc$is_alive()) proc$kill())

  # shiny-server's idle timeout ends the R worker ~5 s after the last tab
  # closes; a supervised child would be killed with it, mid-build.
  expect_false(proc$is_supervised())
})

test_that("closing the tab after the build died (before the poller ran) releases the lock", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  skip_if_not_installed("bs4Dash")
  skip_if_not_installed("processx")
  source_rebuild_module()
  root <- local_fake_app_root()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = gate_hash, FAKE_BUILD_SLEEP = "20")
  lock_dir <- file.path(root, "cache", "offline_traits.db.lock")

  shiny::testServer(rebuild_test_module(), {
    session$userData$admin_unlocked <- TRUE
    session$setInputs(rebuild_offline_db = 1)
    proc <- offline_rebuild_process()
    proc$kill()  # a crash: the child never reaches its own release
    expect_true(dir.exists(lock_dir))
    session$close()
  })

  expect_false(dir.exists(lock_dir))
})

test_that("a click before the previous completion was handled starts nothing (F-a)", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  skip_if_not_installed("bs4Dash")
  skip_if_not_installed("processx")
  source_rebuild_module()
  root <- local_fake_app_root()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = gate_hash, FAKE_BUILD_SLEEP = "2")
  lock_dir <- file.path(root, "cache", "offline_traits.db.lock")

  shiny::testServer(rebuild_test_module(), {
    session$userData$admin_unlocked <- TRUE
    session$setInputs(rebuild_offline_db = 1)
    proc <- offline_rebuild_process()
    skip_if(is.null(proc), "fake build finished inside the first flush; cannot reach the window")
    first_token <- rebuild_state$token
    proc$wait(30000)
    # The child exited and released its lock (the real script does this in
    # its finalizer), but the 2 s poller has not handled the completion yet.
    release_rebuild_lock(lock_dir, first_token)
    expect_false(dir.exists(lock_dir))

    session$setInputs(rebuild_offline_db = 2)
    expect_identical(offline_rebuild_process(), proc)
    expect_identical(rebuild_state$token, first_token)
    expect_false(dir.exists(lock_dir))  # no second build took the lock

    session$elapse(2100)
    expect_null(offline_rebuild_process())
  })
})

test_that("an unreadable rebuild log warns instead of failing silently (F-e)", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  skip_if_not_installed("bs4Dash")
  source_rebuild_module()
  local_fake_app_root()
  not_a_file <- withr::local_tempdir()  # exists, but readLines() on it errors

  shiny::testServer(rebuild_test_module(), {
    w <- testthat::capture_warnings(res <- read_rebuild_log(not_a_file))
    expect_identical(res, character(0))
    expect_true(any(grepl("[rebuild] could not read build log", w, fixed = TRUE)),
                info = paste(w, collapse = " | "))
  })
})

test_that("app.R sources offline_db_rebuild.R before the trait research module", {
  app_lines <- readLines(file.path(app_root, "app.R"), warn = FALSE)
  # The module calls offline_db_lock_path() at start-up, so an unsourced
  # helper would error inside server() and take down every session.
  helper <- which(startsWith(app_lines, 'source("R/functions/offline_db_rebuild.R")'))
  module <- which(startsWith(app_lines, 'source("R/modules/trait_research_server.R")'))
  expect_length(helper, 1L)
  expect_length(module, 1L)
  expect_lt(helper, module)
})
