# F19 (deep analysis 2026-09-26, spec B section 4.3): the "Rebuild Database"
# button had no admin gate, only a per-session in-progress guard, and the
# build script deleted the live cache/offline_traits.db before a build that
# can stop(). These tests pin the admin gate, the process-wide lock and the
# build-into-tmp-then-rename install.

app_root <- get_app_root()
source(file.path(app_root, "R/functions/validation_utils.R"), local = FALSE)
source(file.path(app_root, "R/functions/admin_auth.R"), local = FALSE)
source(file.path(app_root, "R/functions/offline_db_rebuild.R"), local = FALSE)

local_lock_dir <- function(env = parent.frame()) {
  root <- tempfile("rebuild_lock_")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE), envir = env)
  file.path(root, "cache", "offline_traits.db.lock")
}

gate_hash <- paste0("econetool1$12$00112233445566778899aabbccddeeff$",
                    "00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff")

# ---------------------------------------------------------------------------
# Lock primitives
# ---------------------------------------------------------------------------

test_that("a second acquire fails while the first holds the lock", {
  lock_dir <- local_lock_dir()
  first <- acquire_rebuild_lock(lock_dir)
  expect_true(first$acquired)
  expect_true(dir.exists(lock_dir))
  expect_match(first$token, "^[0-9]+-")

  second <- acquire_rebuild_lock(lock_dir)
  expect_false(second$acquired)
  expect_null(second$token)
  expect_match(second$message, "^Rebuild already running \\(started [0-9]{2}:[0-9]{2}\\)$")
})

test_that("a stale lock (mtime 2 h back) is reclaimed with a warning", {
  lock_dir <- local_lock_dir()
  old <- acquire_rebuild_lock(lock_dir)
  Sys.setFileTime(lock_dir, Sys.time() - 2 * 3600)

  expect_warning(fresh <- acquire_rebuild_lock(lock_dir), "reclaiming stale lock")
  expect_true(fresh$acquired)
  expect_false(identical(fresh$token, old$token))
})

test_that("acquiring the lock sweeps stale orphaned tmp builds and keeps fresh ones (I1)", {
  lock_dir <- local_lock_dir()
  cache <- dirname(lock_dir)
  dir.create(cache, recursive = TRUE)
  stale_tmp <- file.path(cache, "offline_traits.db.tmp.123")
  fresh_tmp <- file.path(cache, "offline_traits.db.tmp.456")
  live_db <- file.path(cache, "offline_traits.db")
  writeLines("orphaned build", stale_tmp)
  writeLines("a build in progress", fresh_tmp)
  writeLines("LIVE DB", live_db)
  Sys.setFileTime(stale_tmp, Sys.time() - 2 * 3600)
  Sys.setFileTime(live_db, Sys.time() - 2 * 3600)

  expect_warning(lock <- acquire_rebuild_lock(lock_dir), "offline_traits\\.db\\.tmp\\.123")
  expect_true(lock$acquired)
  expect_false(file.exists(stale_tmp))
  expect_true(file.exists(fresh_tmp))
  expect_true(file.exists(live_db))
})

test_that("an adopting child never sweeps tmp builds", {
  lock_dir <- local_lock_dir()
  parent <- acquire_rebuild_lock(lock_dir)
  stale_tmp <- file.path(dirname(lock_dir), "offline_traits.db.tmp.123")
  writeLines("orphaned build", stale_tmp)
  Sys.setFileTime(stale_tmp, Sys.time() - 2 * 3600)

  expect_silent(child <- acquire_rebuild_lock(lock_dir, inherit_token = parent$token))
  expect_true(child$acquired)
  expect_true(file.exists(stale_tmp))
})

test_that("release removes the lock only for the owning token", {
  lock_dir <- local_lock_dir()
  lock <- acquire_rebuild_lock(lock_dir)

  expect_false(release_rebuild_lock(lock_dir, "someone-else"))
  expect_true(dir.exists(lock_dir))
  expect_false(release_rebuild_lock(lock_dir, NULL))
  expect_true(dir.exists(lock_dir))

  expect_true(release_rebuild_lock(lock_dir, lock$token))
  expect_false(dir.exists(lock_dir))
  expect_false(release_rebuild_lock(lock_dir, lock$token))
  expect_true(acquire_rebuild_lock(lock_dir)$acquired)
})

test_that("a child adopts the lock with the parent's token and nothing else", {
  lock_dir <- local_lock_dir()
  parent <- acquire_rebuild_lock(lock_dir)

  child <- acquire_rebuild_lock(lock_dir, inherit_token = parent$token)
  expect_true(child$acquired)
  expect_identical(child$token, parent$token)

  stranger <- acquire_rebuild_lock(lock_dir, inherit_token = "not-the-token")
  expect_false(stranger$acquired)
})

test_that("an unwritable lock location is reported, not mistaken for a running build", {
  lock_dir <- local_lock_dir()
  blocker <- dirname(lock_dir)
  writeLines("a file where the cache directory should be", blocker)

  expect_warning(res <- acquire_rebuild_lock(lock_dir), "cannot create lock directory")
  expect_false(res$acquired)
  expect_match(res$message, "Cannot create the rebuild lock")
})

test_that("the lock is group-writable so the other OS user can reclaim it", {
  skip_on_os("windows")  # Sys.chmod is a no-op there
  old_umask <- Sys.umask("0022")  # the Shiny process's default umask
  withr::defer(Sys.umask(old_umask))
  lock_dir <- local_lock_dir()
  acquire_rebuild_lock(lock_dir)

  expect_identical(as.character(file.info(lock_dir)$mode), "775")
  expect_identical(as.character(file.info(file.path(lock_dir, "owner"))$mode), "664")
})

# ---------------------------------------------------------------------------
# request_offline_rebuild(): admin gate first, then the lock
# ---------------------------------------------------------------------------

test_that("rebuild is refused and nothing is written when the admin gate is unset", {
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = "")
  lock_dir <- local_lock_dir()
  launched <- FALSE

  expect_warning(
    res <- request_offline_rebuild(TRUE, function(token) launched <<- TRUE, lock_dir = lock_dir),
    "\\[admin auth\\] rebuild_offline_db refused: admin gate not configured"
  )
  expect_identical(res$status, "refused")
  expect_match(res$message, "^Admin gate not configured on this instance")
  expect_false(launched)
  expect_false(dir.exists(lock_dir))
})

test_that("rebuild is refused for a locked session when the gate is set", {
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = gate_hash)
  lock_dir <- local_lock_dir()
  launched <- FALSE

  expect_warning(
    res <- request_offline_rebuild(NULL, function(token) launched <<- TRUE, lock_dir = lock_dir),
    "without an unlocked session"
  )
  expect_identical(res$status, "refused")
  expect_match(res$message, "^Unlock via Trait Research > Configure API Keys first")
  expect_false(launched)
  expect_false(dir.exists(lock_dir))
})

test_that("an unlocked admin starts the build with the lock token; a second request is busy", {
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = gate_hash)
  lock_dir <- local_lock_dir()
  seen_token <- NULL

  res <- request_offline_rebuild(TRUE, function(token) {
    seen_token <<- token
    "fake-process"
  }, lock_dir = lock_dir)
  expect_identical(res$status, "started")
  expect_identical(res$proc, "fake-process")
  expect_identical(res$token, seen_token)
  expect_identical(.read_rebuild_lock_token(lock_dir), seen_token)

  again <- request_offline_rebuild(TRUE, function(token) stop("must not launch"), lock_dir = lock_dir)
  expect_identical(again$status, "busy")
  expect_match(again$message, "^Rebuild already running")
})

test_that("a launch failure releases the lock and reports the error", {
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = gate_hash)
  lock_dir <- local_lock_dir()

  expect_warning(
    res <- request_offline_rebuild(TRUE, function(token) stop("Rscript not found"), lock_dir = lock_dir),
    "failed to start the build process: Rscript not found"
  )
  expect_identical(res$status, "failed")
  expect_match(res$message, "Rscript not found")
  expect_false(dir.exists(lock_dir))
})

# ---------------------------------------------------------------------------
# finalize_offline_db_build(): rename, with a copy fallback
# ---------------------------------------------------------------------------

test_that("finalize replaces the live DB with the build and removes the tmp file", {
  skip_if_not_installed("RSQLite")
  dir <- withr::local_tempdir()
  db_path <- file.path(dir, "offline_traits.db")
  tmp_path <- paste0(db_path, ".tmp.123")
  writeLines("OLD LIVE DB", db_path)
  con <- DBI::dbConnect(RSQLite::SQLite(), tmp_path)
  DBI::dbExecute(con, "CREATE TABLE t (x INTEGER)")
  DBI::dbExecute(con, "INSERT INTO t VALUES (42)")

  finalize_offline_db_build(con, tmp_path, db_path)

  expect_false(DBI::dbIsValid(con))
  expect_false(file.exists(tmp_path))
  con2 <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  withr::defer(DBI::dbDisconnect(con2))
  expect_equal(DBI::dbGetQuery(con2, "SELECT x FROM t")$x, 42L)
})

test_that("finalize falls back to copy when the rename fails", {
  dir <- withr::local_tempdir()
  db_path <- file.path(dir, "offline_traits.db")
  tmp_path <- paste0(db_path, ".tmp.123")
  writeLines("OLD LIVE DB", db_path)
  writeLines("NEW BUILD", tmp_path)

  expect_warning(
    finalize_offline_db_build(NULL, tmp_path, db_path, rename = function(from, to) FALSE),
    "copying instead"
  )
  expect_identical(readLines(db_path), "NEW BUILD")
  expect_false(file.exists(tmp_path))
})

# ---------------------------------------------------------------------------
# Fix round 1 (task review of f9bba93)
# ---------------------------------------------------------------------------

test_that("release with the old token fails after a stale reclaim; the new lock stays", {
  lock_dir <- local_lock_dir()
  old <- acquire_rebuild_lock(lock_dir)
  Sys.setFileTime(lock_dir, Sys.time() - 2 * 3600)

  expect_warning(fresh <- acquire_rebuild_lock(lock_dir), "reclaiming stale lock")
  expect_true(fresh$acquired)

  expect_false(release_rebuild_lock(lock_dir, old$token))
  expect_true(dir.exists(lock_dir))
  expect_identical(.read_rebuild_lock_token(lock_dir), fresh$token)
})

test_that(".reclaim_stale_lock leaves a lock recreated in the meantime untouched", {
  lock_dir <- local_lock_dir()
  acquire_rebuild_lock(lock_dir)
  Sys.setFileTime(lock_dir, Sys.time() - 2 * 3600)

  # Simulate the winning side: it reclaims the stale directory and installs
  # a fresh lock in its place.
  expect_true(.reclaim_stale_lock(lock_dir, REBUILD_LOCK_STALE_MINS))
  fresh <- acquire_rebuild_lock(lock_dir)
  expect_true(fresh$acquired)

  # Simulate the losing side: it decided the (now-stale) lock needed
  # reclaiming before the winner recreated it, and only gets to the rename
  # after the winner's fresh lock is already in place. It must not steal it.
  losing <- .reclaim_stale_lock(lock_dir, REBUILD_LOCK_STALE_MINS)
  expect_false(losing)
  expect_true(dir.exists(lock_dir))
  expect_identical(.read_rebuild_lock_token(lock_dir), fresh$token)

  again <- acquire_rebuild_lock(lock_dir)
  expect_false(again$acquired)
  expect_match(again$message, "^Rebuild already running")
})

test_that("an unreadable owner file warns instead of failing silently", {
  lock_dir <- local_lock_dir()
  dir.create(lock_dir, recursive = TRUE)
  # A directory where the owner file should be: readLines() on it errors,
  # unlike a genuinely missing owner file, which is a normal, silent state.
  dir.create(file.path(lock_dir, "owner"))

  expect_warning(tok <- .read_rebuild_lock_token(lock_dir), "could not read lock owner")
  expect_identical(tok, NA_character_)
})

test_that("a missing owner file stays silent (normal state, not an error)", {
  lock_dir <- local_lock_dir()
  dir.create(lock_dir, recursive = TRUE)
  expect_silent(tok <- .read_rebuild_lock_token(lock_dir))
  expect_identical(tok, NA_character_)
})

test_that("refusal messages are built from B1's ADMIN_STRICT_MSG_* constants", {
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = "")
  lock_dir <- local_lock_dir()
  expect_warning(
    unset_res <- request_offline_rebuild(TRUE, function(token) NULL, lock_dir = lock_dir),
    "admin gate not configured"
  )
  expect_true(grepl(ADMIN_STRICT_MSG_UNSET, unset_res$message, fixed = TRUE))

  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = gate_hash)
  lock_dir2 <- local_lock_dir()
  expect_warning(
    locked_res <- request_offline_rebuild(NULL, function(token) NULL, lock_dir = lock_dir2),
    "without an unlocked session"
  )
  expect_true(grepl(ADMIN_STRICT_MSG_LOCKED, locked_res$message, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# The build script itself, run as a child Rscript in a scratch project root
# ---------------------------------------------------------------------------

# Scratch project root holding exactly what build_offline_trait_db.R sources,
# a cache/ with a sentinel "live DB", and a data/ with only the given
# ontology CSV (every other source is skipped because its file is absent).
local_build_root <- function(ontology_lines, env = parent.frame()) {
  root <- tempfile("offline_build_")
  withr::defer(unlink(root, recursive = TRUE), envir = env)
  for (rel in c("R/config/harmonization_config.R",
                "R/functions/validation_utils.R",
                "R/functions/trait_lookup/harmonization.R",
                "R/functions/offline_db_rebuild.R",
                "scripts/initialization/build_offline_trait_db.R")) {
    dir.create(file.path(root, dirname(rel)), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(app_root, rel), file.path(root, rel))
  }
  dir.create(file.path(root, "cache"))
  dir.create(file.path(root, "data"))
  writeLines(ontology_lines, file.path(root, "data", "ontology_traits.csv"))
  writeLines("SENTINEL LIVE DB - must survive a failed build", file.path(root, "cache", "offline_traits.db"))
  root
}

run_build <- function(root, token = "") {
  processx::run(
    file.path(R.home("bin"), "Rscript"),
    args = "scripts/initialization/build_offline_trait_db.R",
    wd = root, error_on_status = FALSE, timeout = 120,
    env = c("current", ECONETOOL_REBUILD_LOCK_TOKEN = token)
  )
}

good_ontology <- c(
  "taxon_name,aphia_id,trait_category,trait_name,trait_modality,trait_score",
  "Gadus morhua,126436,feeding,feeding_mode,predator,3",
  "Gadus morhua,126436,mobility,mobility,swimmer,3"
)

test_that("a build that stop()s on ontology drift leaves the live DB byte-identical", {
  skip_if_not_installed("processx")
  skip_if_not_installed("RSQLite")
  root <- local_build_root(c("wrong,columns", "a,b"))
  db_path <- file.path(root, "cache", "offline_traits.db")
  before <- readBin(db_path, "raw", file.size(db_path))

  res <- run_build(root)

  expect_false(res$status == 0)
  expect_match(res$stderr, "Ontology CSV missing required columns")
  expect_true(file.exists(db_path))
  expect_identical(readBin(db_path, "raw", file.size(db_path)), before)
  expect_length(list.files(file.path(root, "cache"), pattern = "\\.tmp\\."), 0)
  expect_false(dir.exists(file.path(root, "cache", "offline_traits.db.lock")))
})

test_that("a successful build installs a readable DB and releases the lock", {
  skip_if_not_installed("processx")
  skip_if_not_installed("RSQLite")
  root <- local_build_root(good_ontology)
  db_path <- file.path(root, "cache", "offline_traits.db")

  res <- run_build(root)

  expect_equal(res$status, 0, info = res$stderr)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  withr::defer(DBI::dbDisconnect(con))
  expect_true("species_traits" %in% DBI::dbListTables(con))
  expect_identical(DBI::dbGetQuery(con, "SELECT species FROM species_traits")$species, "Gadus morhua")
  expect_length(list.files(file.path(root, "cache"), pattern = "\\.tmp\\."), 0)
  expect_false(dir.exists(file.path(root, "cache", "offline_traits.db.lock")))
})

test_that("a console build refuses to run while another build holds the lock", {
  skip_if_not_installed("processx")
  skip_if_not_installed("RSQLite")
  root <- local_build_root(good_ontology)
  db_path <- file.path(root, "cache", "offline_traits.db")
  before <- readBin(db_path, "raw", file.size(db_path))
  lock_dir <- file.path(root, "cache", "offline_traits.db.lock")
  held <- acquire_rebuild_lock(lock_dir)

  res <- run_build(root)

  expect_false(res$status == 0)
  expect_match(res$stderr, "Rebuild already running")
  expect_identical(readBin(db_path, "raw", file.size(db_path)), before)
  expect_identical(.read_rebuild_lock_token(lock_dir), held$token)
})

test_that("a Shiny-launched build adopts the parent's lock and releases it when done", {
  skip_if_not_installed("processx")
  skip_if_not_installed("RSQLite")
  root <- local_build_root(good_ontology)
  lock_dir <- file.path(root, "cache", "offline_traits.db.lock")
  held <- acquire_rebuild_lock(lock_dir)

  res <- run_build(root, token = held$token)

  expect_equal(res$status, 0, info = res$stderr)
  expect_false(dir.exists(lock_dir))
})
