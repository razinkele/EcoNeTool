# =============================================================================
# OFFLINE TRAIT DB REBUILD: admin gate, process-wide lock, atomic install
# =============================================================================
# Used by the "Rebuild Database" button (R/modules/trait_research_server.R)
# and by scripts/initialization/build_offline_trait_db.R, so a console build
# and an in-app build exclude each other.
#
# The lock is a DIRECTORY, not an R flag: the build runs in a separate
# Rscript process, and dir.create() fails atomically when the directory
# already exists. An `owner` file inside it records pid, start time and a
# random token. The Shiny session hands the token to the child through
# ECONETOOL_REBUILD_LOCK_TOKEN; the child adopts the lock and releases it
# when it exits. release_rebuild_lock() only removes a lock whose token
# matches, so a late release can never delete somebody else's lock.
#
# No package dependencies beyond DBI (only for finalize_offline_db_build).
# =============================================================================

REBUILD_LOCK_STALE_MINS <- 60
REBUILD_LOCK_TOKEN_ENV <- "ECONETOOL_REBUILD_LOCK_TOKEN"

#' Path of the offline-DB rebuild lock directory
#' @return Character path, resolved independently of getwd().
offline_db_lock_path <- function() {
  app_path("cache/offline_traits.db.lock")
}

.read_rebuild_lock_token <- function(lock_dir) {
  owner <- file.path(lock_dir, "owner")
  # A MISSING owner file is a normal state (no lock, or lock still being
  # written) and stays silent. An owner file that EXISTS but cannot be read
  # (e.g. permission trouble, or something odd sitting at that path) is not
  # normal, so it warns rather than degrading silently to "no token".
  if (!file.exists(owner)) return(NA_character_)
  lines <- tryCatch(readLines(owner, warn = FALSE), error = function(e) {
    warning(sprintf("[rebuild lock] could not read lock owner '%s': %s",
                    owner, conditionMessage(e)), call. = FALSE)
    character(0)
  })
  tok <- sub("^token=", "", grep("^token=", lines, value = TRUE))
  if (length(tok) == 1L && nzchar(tok)) tok else NA_character_
}

#' Atomically move a stale lock aside, verifying it is still stale
#'
#' Two requesters can both observe the same stale lock and both decide to
#' reclaim it. `file.rename()` on the same source is atomic, so only one of
#' two truly concurrent renames can succeed - the loser's rename simply fails
#' because the source is already gone, and it is left treating the lock as
#' held (see `acquire_rebuild_lock()`'s dir.create() fallback below).
#'
#' There is a narrower window this alone would not close: a caller can win
#' the rename right after another process has already reclaimed the same
#' stale lock AND recreated a fresh one at `lock_dir` - the "loser" then
#' renames away a perfectly live lock. To guard against that, after a
#' successful rename this re-checks the age of what actually got moved; if
#' it turns out fresh after all, it is put back and treated as a loss.
#'
#' @param lock_dir Lock directory path.
#' @param stale_after_mins Passed through from acquire_rebuild_lock().
#' @return TRUE when this caller reclaimed (and discarded) a genuinely stale
#'   lock; FALSE when it lost the race (nothing was removed).
.reclaim_stale_lock <- function(lock_dir, stale_after_mins = REBUILD_LOCK_STALE_MINS) {
  aside <- paste0(lock_dir, ".stale.", Sys.getpid(), ".", basename(tempfile("")))
  if (!isTRUE(suppressWarnings(file.rename(lock_dir, aside)))) {
    return(invisible(FALSE))
  }
  age_mins <- as.numeric(difftime(Sys.time(), file.mtime(aside), units = "mins"))
  if (!is.na(age_mins) && age_mins <= stale_after_mins) {
    # We captured a lock someone else just (re)created - not ours to take.
    file.rename(aside, lock_dir)
    return(invisible(FALSE))
  }
  unlink(aside, recursive = TRUE)
  invisible(TRUE)
}

#' Delete orphaned `<db>.tmp.<pid>` builds left next to the DB
#'
#' A build killed hard (SIGKILL, OOM, reboot) never reaches its finalizer, so
#' its tmp file stays behind. Called only by a caller that has just acquired
#' the lock (fresh or reclaimed) - so no build is running - and even then
#' only files older than the stale window go; a fresh tmp is never touched.
#'
#' @param lock_dir Lock directory path; the DB is `sub(".lock$", "", lock_dir)`.
#' @param stale_after_mins Age threshold, as for the lock.
#' @return The removed paths, invisibly.
.sweep_stale_tmp_builds <- function(lock_dir, stale_after_mins = REBUILD_LOCK_STALE_MINS) {
  db_base <- sub("\\.lock$", "", basename(lock_dir))
  pattern <- paste0("^", gsub(".", "\\.", db_base, fixed = TRUE), "\\.tmp\\.")
  tmps <- list.files(dirname(lock_dir), pattern = pattern, full.names = TRUE)
  if (length(tmps) == 0L) return(invisible(character(0)))
  age_mins <- as.numeric(difftime(Sys.time(), file.mtime(tmps), units = "mins"))
  stale <- tmps[!is.na(age_mins) & age_mins > stale_after_mins]
  if (length(stale) == 0L) return(invisible(character(0)))
  unlink(stale)
  warning(sprintf("[rebuild lock] removed orphaned tmp build(s) older than %d min: %s",
                  as.integer(stale_after_mins), paste(basename(stale), collapse = ", ")),
          call. = FALSE)
  invisible(stale)
}

#' Acquire the process-wide offline-DB rebuild lock
#'
#' @param lock_dir Lock directory path.
#' @param stale_after_mins A lock older than this (directory mtime) is
#'   treated as abandoned: warning(), then reclaimed.
#' @param inherit_token Token handed down by the process that already holds
#'   the lock. When it matches the lock's owner token the caller adopts the
#'   lock instead of failing on it.
#' @return list(acquired = logical(1), token = character(1) or NULL,
#'   message = character(1) or NULL).
acquire_rebuild_lock <- function(lock_dir = offline_db_lock_path(),
                                 stale_after_mins = REBUILD_LOCK_STALE_MINS,
                                 inherit_token = "") {
  if (nzchar(inherit_token) &&
        identical(.read_rebuild_lock_token(lock_dir), inherit_token)) {
    return(list(acquired = TRUE, token = inherit_token, message = NULL))
  }

  dir.create(dirname(lock_dir), recursive = TRUE, showWarnings = FALSE)

  if (dir.exists(lock_dir)) {
    age_mins <- as.numeric(difftime(Sys.time(), file.mtime(lock_dir), units = "mins"))
    if (!is.na(age_mins) && age_mins > stale_after_mins) {
      warning(sprintf("[rebuild lock] reclaiming stale lock %s (%.0f min old)",
                      lock_dir, age_mins), call. = FALSE)
      # Rename-aside is the sole arbiter of who gets to reclaim (see
      # .reclaim_stale_lock()); a losing caller here simply falls through to
      # the dir.create() below, which then reports the winner's fresh lock
      # as "already running" instead of erroring.
      .reclaim_stale_lock(lock_dir, stale_after_mins)
    }
  }

  if (!dir.create(lock_dir, showWarnings = FALSE)) {
    if (!dir.exists(lock_dir)) {
      warning(sprintf("[rebuild lock] cannot create lock directory %s", lock_dir),
              call. = FALSE)
      return(list(acquired = FALSE, token = NULL,
                  message = "Cannot create the rebuild lock (is cache/ writable?)"))
    }
    started <- file.mtime(lock_dir)
    return(list(
      acquired = FALSE, token = NULL,
      message = sprintf("Rebuild already running (started %s)",
                        if (is.na(started)) "unknown" else format(started, "%H:%M"))
    ))
  }

  .sweep_stale_tmp_builds(lock_dir, stale_after_mins)

  # tempfile() draws from the C-level RNG, so this never disturbs a user's
  # set.seed() stream in the Shiny process.
  token <- paste0(Sys.getpid(), "-", basename(tempfile("")))
  owner <- file.path(lock_dir, "owner")
  writeLines(c(paste0("pid=", Sys.getpid()),
               paste0("started=", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
               paste0("token=", token)),
             owner)
  # Console builds run as the developer, the app as `shiny` (same group).
  # Group-writable so either side can reclaim a stale lock the other left.
  # No-op on Windows.
  # use_umask = FALSE: the default applies the umask, turning 0775 into 0755.
  Sys.chmod(lock_dir, mode = "0775", use_umask = FALSE)
  Sys.chmod(owner, mode = "0664", use_umask = FALSE)
  list(acquired = TRUE, token = token, message = NULL)
}

#' Release the rebuild lock if (and only if) `token` owns it
#'
#' Accepted read-token-then-unlink race: between the token check and the
#' unlink another process could in principle reclaim the lock as stale and
#' install its own, which this would then delete. Left unfixed by design -
#' it requires the owner to have been stalled for more than
#' REBUILD_LOCK_STALE_MINS (60 minutes), which is far longer than this
#' function's own execution time.
#'
#' @return TRUE when a lock was removed, FALSE otherwise (invisibly).
release_rebuild_lock <- function(lock_dir = offline_db_lock_path(), token = NULL) {
  if (is.null(token) || !dir.exists(lock_dir)) return(invisible(FALSE))
  if (!identical(.read_rebuild_lock_token(lock_dir), token)) return(invisible(FALSE))
  unlink(lock_dir, recursive = TRUE)
  invisible(!dir.exists(lock_dir))
}

#' Decide and start an in-app offline-DB rebuild
#'
#' Pure of Shiny: the observer maps the returned status to notifications.
#'
#' @param unlocked The session's admin unlock flag
#'   (session$userData$admin_unlocked).
#' @param launch function(token) that starts the build and returns the
#'   process handle. Only called once the gate and the lock both pass.
#' @param lock_dir,stale_after_mins Passed to acquire_rebuild_lock().
#' @return list(status = "refused" | "busy" | "failed" | "started",
#'   message = character(1) or NULL, proc = handle or NULL,
#'   token = character(1) or NULL).
request_offline_rebuild <- function(unlocked, launch,
                                    lock_dir = offline_db_lock_path(),
                                    stale_after_mins = REBUILD_LOCK_STALE_MINS) {
  if (!admin_authorized_strict(unlocked)) {
    if (!admin_gate_enabled()) {
      warning("[admin auth] rebuild_offline_db refused: admin gate not configured on this instance",
              call. = FALSE)
      # Built from B1's ADMIN_STRICT_MSG_UNSET (R/functions/admin_auth.R),
      # with the rebuild-specific suffix appended locally - not a copy.
      return(list(status = "refused", proc = NULL, token = NULL,
                  message = paste0(ADMIN_STRICT_MSG_UNSET,
                                   "; the offline database cannot be rebuilt from the app")))
    }
    warning("[admin auth] rebuild_offline_db fired without an unlocked session; refusing",
            call. = FALSE)
    # Built from B1's ADMIN_STRICT_MSG_LOCKED, likewise with a local suffix.
    return(list(status = "refused", proc = NULL, token = NULL,
                message = paste0(ADMIN_STRICT_MSG_LOCKED, ", then rebuild the database")))
  }

  lock <- acquire_rebuild_lock(lock_dir, stale_after_mins = stale_after_mins)
  if (!isTRUE(lock$acquired)) {
    return(list(status = "busy", proc = NULL, token = NULL, message = lock$message))
  }

  launch_error <- NULL
  proc <- tryCatch(launch(lock$token), error = function(e) {
    launch_error <<- conditionMessage(e)
    warning(sprintf("[rebuild] failed to start the build process: %s", launch_error),
            call. = FALSE)
    NULL
  })
  if (is.null(proc)) {
    release_rebuild_lock(lock_dir, lock$token)
    return(list(status = "failed", proc = NULL, token = NULL,
                message = paste("Failed to start rebuild:", launch_error %||% "no process")))
  }
  list(status = "started", proc = proc, token = lock$token, message = NULL)
}

#' Install a freshly built offline DB over the live one
#'
#' Disconnects first (Windows cannot rename an open SQLite file), widens the
#' mode so the app user can migrate the schema, then renames. file.rename()
#' replaces an existing target on Linux and Windows alike, so there is no
#' copy fallback: copying over the live DB could leave it half-written. If
#' the rename fails the tmp build is removed and the live DB is untouched.
#'
#' @param con DBI connection to `tmp_path` (or NULL).
#' @param tmp_path The completed build.
#' @param db_path The live DB path.
#' @param rename Injectable for tests; defaults to file.rename.
#' @return db_path, invisibly. stop()s if the rename failed.
finalize_offline_db_build <- function(con, tmp_path, db_path, rename = file.rename) {
  if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con)
  try(Sys.chmod(tmp_path, mode = "0664", use_umask = FALSE), silent = TRUE)
  if (!isTRUE(suppressWarnings(rename(tmp_path, db_path)))) {
    unlink(tmp_path)
    stop(sprintf("could not rename %s over %s; the live DB is unchanged", tmp_path, db_path),
         call. = FALSE)
  }
  try(Sys.chmod(db_path, mode = "0664", use_umask = FALSE), silent = TRUE)
  invisible(db_path)
}
