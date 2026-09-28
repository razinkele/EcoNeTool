# B3: Security and Admin Gating (F19, F3, F54, F6) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Only an unlocked admin can rebuild the offline trait DB, concurrent or failed rebuilds can no longer destroy or race on `cache/offline_traits.db`, third-party and uploaded metadata render as text instead of HTML, and saving the API-key modal with an empty secret field keeps the stored secret; ship as the next PATCH (expected 1.5.3) and deploy.

**Architecture:** A new dependency-free file `R/functions/offline_db_rebuild.R` holds the rebuild decision (`request_offline_rebuild()`: strict admin gate first, then a process-wide directory lock, then launch), the token-owned lock (`acquire_rebuild_lock()` / `release_rebuild_lock()`), and the atomic install (`finalize_offline_db_build()`). Both the Shiny observer and `scripts/initialization/build_offline_trait_db.R` use it; the Shiny session hands its lock token to the child Rscript through `ECONETOOL_REBUILD_LOCK_TOKEN`, and the child builds into `offline_traits.db.tmp.<pid>` and renames it over the live DB only at the end. For F3/F54 the two metadata panels become file-scope helpers built only from `htmltools` tag builders (which escape by construction), with shared helpers `safe_href()`, `safe_doi_href()` and `meta_*()` in `R/functions/validation_utils.R`. For F6 the modal and the save path move to file-scope helpers that never pre-fill secrets and keep a stored secret when its field is blank, writing the JSON tmp-then-rename with mode 0600.

**Tech Stack:** R 4.4.1, shiny 1.11.1 (`testServer`, `MockShinySession`), htmltools 0.5.8.1, bs4Dash 2.3.5, DT 0.34.0, processx 3.8.6, DBI/RSQLite, jsonlite, testthat 3.3.2, withr 3.0.2. Windows dev box (Git Bash), Linux deploy target (laguna.ku.lt, shiny-server).

**Spec:** `docs/superpowers/specs/2026-09-26-fix-b-platform-safety-design.md` section 4.3 (Phase B3), section 5 (rows `test-rebuild-lock.R`, `test-xss-escaping.R`, `test-plugin-api-keys.R`), section 6 (rollout row B3, "B1/B3 prerequisite"), section 8 items 2, 8, 9, 10, 11, and Appendix A (F54 `solution_html` stays); plus `docs/superpowers/specs/2026-09-26-fix-overview.md` (merge order row 7, "Shared rules for every PR").

> **Execution rulings (controller, 2026-09-27) — these override the tasks below where they conflict:**
> 1. **Release flow:** the fix PR is squash-merged WITHOUT the version bump / CHANGELOG regeneration. The plan's version/CHANGELOG task then runs as a separate release PR cut from the updated master: bump to 1.5.3 (VERSION, `R/config.R` fallback, app.R header, README via `scripts/version_bump.R`), normalise CRLF to LF, set `GIT_BRANCH=master`, regenerate CHANGELOG, re-insert `docs/releases/1.5.0-results-changed.md` under `[1.5.0]`, strip the extra trailing newline, merge it, then `git tag -a v1.5.3` on the merge commit and push the tag. This keeps CHANGELOG hashes valid under squash merges and guarantees the tag the next plan expects exists.
> 2. **Locked-session refusal text** is `Unlock via Trait Research > Configure API Keys first` (the unlock button lives on the Trait Research tab).
> 3. Deploys use the deploy scripts as they exist on master at deploy time (after B2 merges, the hardened scripts); every outward step still STOPs for the user.

## Global Constraints

- Branch `fix/b3-security-admin`, cut from `master` **after B1 has merged** (B3 "Depends on B1 (`admin_authorized_strict`)"). Merge order is B2 -> B1 -> B3, so the version is the next PATCH: **1.5.3** if master reads 1.5.2 (Task 7 re-derives it from `VERSION`).
- Consumed interface from B1, used exactly as delivered and never redefined here: `admin_authorized_strict(unlocked) -> logical(1)` in `R/functions/admin_auth.R`, TRUE only when `admin_gate_enabled() && isTRUE(unlocked)`. B1's refusal texts are "Admin gate not configured on this instance; server defaults are read-only" (gate unset) and "Unlock via Trait Research > Configure API Keys first" (gate set, session locked), each with `warning("[admin auth] ...")`. B3 reuses the same two conditions with rebuild-specific wording (Task 1). The scratch verification of this plan used a stand-in with exactly that one-line body appended to a scratch copy of `admin_auth.R`; this plan does not add it.
- Hash unset means fail CLOSED for the rebuild (spec 4.1/4.3). `admin_authorized()` (fail-open, API-key modal) is unchanged (spec section 3 non-goal).
- `ecopath_import_server.R` `HTML(status_data$solution_html)` stays: it is static server text (spec Appendix A). Static `HTML("...")` literals are also left alone.
- CLAUDE.md conventions: `warning()` not `message()` in error handlers; `<<-` for outer-scope mutation from an error closure; `app_path()` for runtime paths, never a wd-relative `source()` under `R/`; `skip_if()` / `skip_if_not_installed()` / `skip_on_os()`, never `if (cond) expect_*()`.
- lintr: 120-char lines, `<-` assignment, no tabs, no trailing whitespace. Parse-check every edited `.R` file.
- Tests: single file `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/<file>')"` (`set_max_fails(Inf)` matters: testthat otherwise stops after 10 failures and the fail-before counts below would be truncated). Full suite `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_dir('tests/testthat')"`. `tests/run_all_tests.R` needs `dggridR` and cannot run locally; do not use it. Do not run heavy R jobs in parallel (16 GB RAM).
- Create new files and multi-line replacements with the Write/Edit tools. Do not generate R code through shell heredocs: in this harness heredocs have been observed to drop backslashes (`"^\\s*#"` arrived as `"^\s*#"`, a parse error).
- `git add` explicit paths only; never stage the untracked `WBGIFSV5ISSUE70.pdf` or anything under `config/`. Every commit message ends with `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`.
- Outward-facing steps (push, PR, anything on laguna) are marked **STOP - ask the user**. `razinka` has no passwordless sudo; sudo steps are written as USER instructions and never automated. Production `data/` (~3.1 GB) must never be deleted.

## Deviations from the spec (and why)

1. **New file `R/functions/offline_db_rebuild.R` instead of adding the lock to `R/functions/cache_sqlite.R`.** `cache_sqlite.R` `stop()`s at source time when RSQLite is missing and is sourced only inside the `ENABLE_PHASE6` / `file.exists()` block of `app.R`; the rebuild observer runs in every session, so its helpers must be sourced unconditionally (right after `admin_auth.R`).
2. **Token-owned lock with hand-over.** The spec has the Shiny observer acquire the lock and the build script "also call `acquire_rebuild_lock()` itself" - as written the child would always find the parent's lock and refuse. The owner file carries a random token; the child adopts the lock when `ECONETOOL_REBUILD_LOCK_TOKEN` matches it, and `release_rebuild_lock()` only removes a lock whose token matches, so a late release from one side can never delete a lock the other side (or a console build) now holds.
3. **`reg.finalizer(onexit = TRUE)` instead of `on.exit()` in the build script.** Verified 2026-09-27 with a probe script: a top-level `on.exit(cat("TOPLEVEL on.exit ran\n"), add = TRUE)` in an `Rscript` never runs (the call just prints `NULL`), while a finalizer registered with `onexit = TRUE` ran after `stop("boom")` / "Execution halted". The existing `on.exit(dbDisconnect(con), add = TRUE)` in the script has therefore never done anything.
4. **Child output goes to temp files, not pipes, and `cleanup = FALSE`.** With `stdout = "|"` nothing reads the pipes until the child exits; a full pipe buffer stalls the child while it holds the lock, and a closed tab garbage-collects the process object and kills the child. Files plus `cleanup = FALSE` let a started build finish and release its own lock; `supervise = TRUE` still reaps it if the R process dies.
   - *Superseded by final review I1: `supervise = FALSE`* - shiny-server ends the worker ~5 s after the last tab closes, and a supervised child died with it; the child's finalizer releases the lock and tmp file, and `acquire_rebuild_lock()` sweeps tmp builds older than the stale window.
5. **Lock is taken after `Sys.umask("0002")`, and `acquire_rebuild_lock()` chmods the lock dir 0775 / owner 0664.** Console builds run as `razinka`, the app as `shiny` (the script's own comment); a 0755 lock left by one could not be reclaimed by the other.
6. **`fmt()` returns plain text (or a `tags$span`), not `htmlEscape()`d text.** The panels are rebuilt with `tags$` builders, which escape their children; pre-escaping would double-escape (`a &amp;amp; b`). The test "meta_text returns plain text ..." pins single escaping.
7. **Extra helpers beyond `safe_href()`:** `safe_doi_href()` (the spec's DOI rule, also accepting `doi:` / `https://doi.org/` prefixes so real-world DOIs keep their link), `meta_text()`, `meta_row()`, `meta_section_row()`, `meta_publication_row()`, `meta_description()`, and file-scope panel builders `ecobase_metadata_panel()`, `ecobase_connection_error_ui()`, `ewe_preview_panel()`, `ewe_preview_error_panel()` so the panels are unit-testable.
8. **F6 helpers at file scope** (`api_key_modal_dialog()`, `merge_api_key_submission()`, `write_api_keys_json()`) so the modal content is testable (MockShinySession does not expose modal UI).
9. **New platform skips:** `test-rebuild-lock.R` "the lock is group-writable ..." and `test-plugin-api-keys.R` "the key file is owner-only (0600)" use `skip_on_os("windows")` because `Sys.chmod` is a no-op on Windows. Acceptance criterion 11 says the skip count grows only for newly gated live tests; these two are platform skips (+2 locally, 0 on Linux CI), not gated or broken tests.

## Review Focus

- **Admin closes the tab while a rebuild runs:** the build keeps going and releases its own lock; if the child is already dead when the session ends (crash before it could release), `onSessionEnded` releases the lock. Pinned by Task 3 tests "closing the tab while the build runs leaves the lock to the build" and "closing the tab after the build died (before the poller ran) releases the lock".
- **Mixed OS users on laguna (`razinka` console build vs `shiny` app):** either side must be able to reclaim a stale lock left by the other. Pinned by Task 1 test "the lock is group-writable so the other OS user can reclaim it" (Linux only).
- **A lock abandoned by a hard kill (SIGKILL, OOM, server reboot):** nothing can release it at once; after 60 minutes the next request reclaims it with a warning instead of refusing forever. Pinned by Task 1 test "a stale lock (mtime 2 h back) is reclaimed with a warning". (Accepted: up to 60 minutes of "Rebuild already running" after such a crash.)
- **Real-world DOI and URL forms:** `doi:10...` and `https://doi.org/10...` keep their link; `javascript:`, `data:`, protocol-relative and whitespace-prefixed URLs never become an href. Pinned by Task 4 tests "safe_href passes only single http(s) URLs" and "safe_doi_href builds doi.org links for valid DOIs only".
- **Metadata with missing or wrongly-typed fields** (no metadata at all, text where EcoBase has numbers, a hostile model id from the list table): the panel still renders, shows "Not specified", and escapes everything. Pinned by Task 4 tests "EcoBase metadata renders as text ..." (non-numeric latitude/area) and "the EcoBase details output escapes metadata end to end" (`<svg>` model id), and Task 5 test "an EwE preview with no metadata still renders".

---

## File Structure

| File | Change | Responsibility |
|---|---|---|
| `R/functions/offline_db_rebuild.R` | Create | Rebuild decision (gate, lock, launch), token-owned directory lock, atomic install of a finished build |
| `scripts/initialization/build_offline_trait_db.R` | Modify (4 edits) | Adopt/acquire the lock, build into `offline_traits.db.tmp.<pid>`, finalizer cleanup, rename over the live DB at the end |
| `R/modules/trait_research_server.R` | Modify (rebuild block, ~110 lines) | Observer routes through `request_offline_rebuild()`, passes the token, logs to files, releases on completion / session end |
| `app.R` | Modify (1 line after `admin_auth.R`) | Source `offline_db_rebuild.R` unconditionally |
| `R/functions/validation_utils.R` | Append | `safe_href`, `safe_doi_href`, `meta_text`, `meta_row`, `meta_section_row`, `meta_publication_row`, `meta_description` |
| `R/modules/ecobase_server.R` | Modify (prepend helpers, 2 edits) | EcoBase connection error and metadata panel built from tags |
| `R/modules/ecopath_import_server.R` | Modify (prepend helpers, 1 edit) | EwE preview and error panel built from tags |
| `R/modules/plugin_server.R` | Modify (prepend helpers, 2 edits) | Modal never pre-fills secrets; blank secret keeps stored; atomic 0600 JSON write |
| `tests/testthat/test-rebuild-lock.R` | Create | Lock, gate decision, finalize, and the build script run as a child Rscript |
| `tests/testthat/test-rebuild-observer.R` | Create | testServer wiring of the rebuild observer, session end, app.R sourcing guard |
| `tests/testthat/test-xss-escaping.R` | Create | Helpers, both panels (unit + testServer end to end), source guard on `HTML(paste...)` |
| `tests/testthat/test-plugin-api-keys.R` | Create | F6 keep-secret, replace, 0600, locked refusal, modal content |
| `CONTRIBUTING.md` | Modify (1 bullet) | Convention: third-party text only through tag builders; rebuild lock |
| `VERSION`, `R/config.R`, `app.R`, `README.md`, `CHANGELOG.md` | Modify | Version 1.5.3 and CHANGELOG section |

Not touched (out of scope, noted for reviewers): `offline_db_summary()` and `output$offline_db_contents` in `trait_research_server.R` still open the wd-relative `"cache/offline_traits.db"`; the lock itself uses `app_path()`.

---

### Task 0: Branch, preconditions and baseline

**Files:** none modified.

**Interfaces:**
- Consumes: B1 merged on `master` (`admin_authorized_strict()` present).
- Produces: branch `fix/b3-security-admin`; recorded `BASELINE pass N fail 0 skip M` for Task 7.

- [ ] **Step 1: Confirm B1 (and B2) are merged and create the branch**

```bash
cd "/c/Users/arturas.baziukas/OneDrive - ku.lt/HORIZON_EUROPE/MARBEFES/Traits/Networks/EcoNeTool"
git checkout master && git pull --ff-only
grep -n "^admin_authorized_strict <- function" R/functions/admin_auth.R
grep -n "^VERSION=" VERSION
git tag -l "v1.5.*"
git checkout -b fix/b3-security-admin
```

Expected: the `admin_authorized_strict` definition is printed (if it is missing, B1 has not merged: **STOP** and tell the user; every B3 test sources `admin_auth.R` and needs it). `VERSION=1.5.2` is the expected value (write down whatever it is; Task 7 uses PATCH+1). Record which `v1.5.*` tags exist; Task 7 needs `v1.5.2` (see Task 7 Step 3).

- [ ] **Step 2: Record the baseline full-suite counts**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "r <- as.data.frame(testthat::test_dir('tests/testthat', reporter = 'summary', stop_on_failure = FALSE)); cat('BASELINE pass', sum(r\$passed), 'fail', sum(r\$failed), 'skip', sum(r\$skipped), 'error', sum(r\$error), '\n')"
```

Expected: `fail 0 ... error 0`. Write the `BASELINE pass N ... skip M` line into your task notes. If the baseline is red, stop and report; do not start B3 on a red suite.

---

### Task 1: Rebuild gate, token-owned lock and atomic install helpers (F19, part 1)

**Files:**
- Create: `R/functions/offline_db_rebuild.R`
- Create: `tests/testthat/test-rebuild-lock.R` (first part; Task 2 appends the rest)

**Interfaces:**
- Consumes: `app_path()`, `%||%` (`R/functions/validation_utils.R`); `admin_gate_enabled()`, `admin_authorized_strict(unlocked)` (`R/functions/admin_auth.R`, the latter from B1); `DBI` (only inside `finalize_offline_db_build`).
- Produces (used by Tasks 2 and 3):
  - `REBUILD_LOCK_STALE_MINS` = `60`; `REBUILD_LOCK_TOKEN_ENV` = `"ECONETOOL_REBUILD_LOCK_TOKEN"`.
  - `offline_db_lock_path() -> character(1)` = `app_path("cache/offline_traits.db.lock")`.
  - `acquire_rebuild_lock(lock_dir = offline_db_lock_path(), stale_after_mins = REBUILD_LOCK_STALE_MINS, inherit_token = "") -> list(acquired = logical(1), token = character(1) | NULL, message = character(1) | NULL)`. Busy message: `"Rebuild already running (started HH:MM)"`.
  - `release_rebuild_lock(lock_dir = offline_db_lock_path(), token = NULL) -> invisible(logical(1))`, removes the lock only when `token` matches the owner file.
  - `.read_rebuild_lock_token(lock_dir) -> character(1)` (NA when absent); used by tests.
  - `request_offline_rebuild(unlocked, launch, lock_dir = offline_db_lock_path(), stale_after_mins = REBUILD_LOCK_STALE_MINS) -> list(status = "refused" | "busy" | "failed" | "started", message, proc, token)`; `launch` is `function(token)` returning the process handle.
  - `finalize_offline_db_build(con, tmp_path, db_path, rename = file.rename) -> invisible(db_path)`.

- [ ] **Step 1: Write the failing test (first part of the file)**

Create `tests/testthat/test-rebuild-lock.R` with exactly this content:

```r
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

```

- [ ] **Step 2: Run it and verify it fails**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/test-rebuild-lock.R')"
```

Expected: the file errors at load, `cannot open file '.../R/functions/offline_db_rebuild.R': No such file or directory` (`[ FAIL 1 | ... | PASS 0 ]` or an error report). That is the missing unit.

- [ ] **Step 3: Implement `R/functions/offline_db_rebuild.R`**

Create it with exactly this content:

```r
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
  if (!file.exists(owner)) return(NA_character_)
  lines <- tryCatch(readLines(owner, warn = FALSE), error = function(e) character(0))
  tok <- sub("^token=", "", grep("^token=", lines, value = TRUE))
  if (length(tok) == 1L && nzchar(tok)) tok else NA_character_
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
      unlink(lock_dir, recursive = TRUE)
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
  Sys.chmod(lock_dir, mode = "0775")
  Sys.chmod(owner, mode = "0664")
  list(acquired = TRUE, token = token, message = NULL)
}

#' Release the rebuild lock if (and only if) `token` owns it
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
      return(list(status = "refused", proc = NULL, token = NULL,
                  message = paste("Admin gate not configured on this instance;",
                                  "the offline database cannot be rebuilt from the app")))
    }
    warning("[admin auth] rebuild_offline_db fired without an unlocked session; refusing",
            call. = FALSE)
    return(list(status = "refused", proc = NULL, token = NULL,
                message = "Unlock via Trait Research > Configure API Keys first, then rebuild the database"))
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
#' mode so the app user can migrate the schema, then renames; if the rename
#' fails (e.g. the live DB is open on Windows) falls back to copy + remove.
#'
#' @param con DBI connection to `tmp_path` (or NULL).
#' @param tmp_path The completed build.
#' @param db_path The live DB path.
#' @param rename Injectable for tests; defaults to file.rename.
#' @return db_path, invisibly. stop()s if neither rename nor copy worked,
#'   leaving the live DB untouched.
finalize_offline_db_build <- function(con, tmp_path, db_path, rename = file.rename) {
  if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con)
  try(Sys.chmod(tmp_path, mode = "0664"), silent = TRUE)
  if (!isTRUE(suppressWarnings(rename(tmp_path, db_path)))) {
    warning(sprintf("[rebuild] rename %s -> %s failed; copying instead", tmp_path, db_path),
            call. = FALSE)
    if (!isTRUE(file.copy(tmp_path, db_path, overwrite = TRUE))) {
      stop(sprintf("could not install %s over %s; the live DB is unchanged", tmp_path, db_path),
           call. = FALSE)
    }
    unlink(tmp_path)
  }
  try(Sys.chmod(db_path, mode = "0664"), silent = TRUE)
  invisible(db_path)
}
```

- [ ] **Step 4: Parse-check, lint, run the test**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='R/functions/offline_db_rebuild.R'); parse(file='tests/testthat/test-rebuild-lock.R'); cat('OK\n')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "print(lintr::lint('R/functions/offline_db_rebuild.R')); print(lintr::lint('tests/testthat/test-rebuild-lock.R'))"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/test-rebuild-lock.R')"
```

Expected: `OK`; lintr prints no lints for either file apart from the repo-wide `object_usage_linter` noise (none were reported on the scratch copy); test `[ FAIL 0 | WARN 0 | SKIP 1 | PASS 49 ]` on Windows (verified on a scratch copy; the skip is the Linux-only permission test - on Linux SKIP 0 | PASS 51).

Note: "the lock is group-writable ..." skips on Windows before its body runs, so the plan author never executed it; it is first exercised on Linux (B2's CI testthat job). Read its CI result rather than assuming it passes; if it fails there, fix it before merging.

- [ ] **Step 5: Commit**

```bash
git add R/functions/offline_db_rebuild.R tests/testthat/test-rebuild-lock.R
git commit -m "$(cat <<'EOF'
feat(rebuild): admin gate, token-owned lock and atomic install helpers (F19)

request_offline_rebuild() refuses unless admin_authorized_strict() passes
(fail closed when ECONETOOL_ADMIN_PASSWORD_HASH is unset), then takes a
process-wide directory lock whose owner token can be handed to the child
build. finalize_offline_db_build() installs a finished tmp build by rename,
falling back to copy.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 2: Build script builds into a tmp file under the lock (F19, part 2)

**Files:**
- Modify: `scripts/initialization/build_offline_trait_db.R` (4 edits: after the `harmonization.R` source line; section 4 "Create cache/ directory and database"; the `dbConnect` right after `Sys.umask("0002")`; the chmod block at the end)
- Modify: `tests/testthat/test-rebuild-lock.R` (append)

**Interfaces:**
- Consumes: Task 1 `acquire_rebuild_lock()`, `release_rebuild_lock()`, `finalize_offline_db_build()`, `REBUILD_LOCK_TOKEN_ENV`.
- Produces: a script that (a) exits non-zero with "Rebuild already running (started HH:MM)" if another build holds `cache/offline_traits.db.lock`, (b) adopts the lock when `ECONETOOL_REBUILD_LOCK_TOKEN` matches its owner token, (c) never touches `cache/offline_traits.db` until the build has finished, and (d) always removes its tmp file and releases its lock at process exit. Task 3 relies on (b) and on the child releasing the lock itself.

- [ ] **Step 1: Append the failing build-script tests**

Append this to the end of `tests/testthat/test-rebuild-lock.R`:

```r
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
```

The helper copies exactly the five files the script sources (Task 2 adds `offline_db_rebuild.R` to that list) into a scratch project root; `data/` holds only the given ontology CSV, so Sources 2-10 skip themselves ("Skipped: file not found"). Each child run takes about 2 s.

- [ ] **Step 2: Run the file and verify the new tests fail**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/test-rebuild-lock.R')"
```

Expected (verified on a scratch copy of the pre-fix script): `[ FAIL 5 | WARN 0 | SKIP 1 | PASS 61 ]` on Windows:
- "a build that stop()s on ontology drift ..." - `readBin(db_path, ...)` is not identical to `before` (`Lengths (32768, 48) differ`): the old script deleted the live DB before the build and left a half-built one.
- "a console build refuses to run ..." - three failures: exit status 0, stderr lacks "Rebuild already running", and the sentinel DB was overwritten.
- "a Shiny-launched build adopts ..." - `dir.exists(lock_dir)` is TRUE: the old script ignores the lock.
"a successful build installs a readable DB ..." passes pre-fix (guard for the happy path).

- [ ] **Step 3: Edit 1 - source the helper**

In `scripts/initialization/build_offline_trait_db.R`, replace:

```r
source(file.path(project_root, "R", "functions", "trait_lookup", "harmonization.R"))
```

with:

```r
source(file.path(project_root, "R", "functions", "trait_lookup", "harmonization.R"))
# Rebuild lock + atomic install, shared with the in-app "Rebuild Database"
# button (F19), so console and in-app builds exclude each other.
source(file.path(project_root, "R", "functions", "offline_db_rebuild.R"))
```

- [ ] **Step 4: Edit 2 - stop deleting the live DB up front**

Replace:

```r
db_path <- file.path(cache_dir, "offline_traits.db")
cat("Database path:", db_path, "\n")

# Remove old database if it exists (fresh build)
if (file.exists(db_path)) {
  file.remove(db_path)
  cat("Removed existing database for fresh build.\n")
}
```

with:

```r
db_path <- file.path(cache_dir, "offline_traits.db")
cat("Database path:", db_path, "\n")

# F19: the live DB is NOT removed here any more. The build goes into a
# per-process tmp file (below) and replaces the live DB only once it has
# finished, so a stop() mid-build leaves the live DB intact.
```

- [ ] **Step 5: Edit 3 - lock, tmp path, exit finalizer, connect to the tmp file**

These two lines sit directly below `Sys.umask("0002")`. Replace:

```r
con <- dbConnect(RSQLite::SQLite(), db_path)
on.exit(dbDisconnect(con), add = TRUE)
```

with:

```r
# F19: take the process-wide rebuild lock (after the umask above, so the lock
# directory is group-writable and the other OS user can reclaim it). An
# in-app build hands its token down through ECONETOOL_REBUILD_LOCK_TOKEN and
# this process adopts that lock; a console build acquires its own and stops
# if another build holds it.
lock_dir <- file.path(cache_dir, "offline_traits.db.lock")
build_lock <- acquire_rebuild_lock(lock_dir,
                                   inherit_token = Sys.getenv(REBUILD_LOCK_TOKEN_ENV))
if (!isTRUE(build_lock$acquired)) {
  stop(build_lock$message, call. = FALSE)
}

# F19: build into a per-process tmp file; finalize_offline_db_build() renames
# it over the live DB at the very end.
tmp_path <- paste0(db_path, ".tmp.", Sys.getpid())
if (file.exists(tmp_path)) unlink(tmp_path)

# Cleanup at process exit, success or failure: disconnect, drop an unfinished
# tmp build, release the lock. A top-level on.exit() is a no-op in an Rscript
# (it never runs), so the old on.exit(dbDisconnect(con)) here did nothing; a
# finalizer with onexit = TRUE runs even after a stop().
.build_state <- new.env()
.build_state$con <- NULL
reg.finalizer(.build_state, function(st) {
  if (!is.null(st$con) && DBI::dbIsValid(st$con)) try(DBI::dbDisconnect(st$con), silent = TRUE)
  if (file.exists(tmp_path)) unlink(tmp_path)
  release_rebuild_lock(lock_dir, build_lock$token)
}, onexit = TRUE)

con <- dbConnect(RSQLite::SQLite(), tmp_path)
.build_state$con <- con
```

- [ ] **Step 6: Edit 4 - install the finished build**

Near the end of the file (after the "Trait coverage" block, before `cat("\nDatabase saved to:", db_path, "\n")`), replace:

```r
# Belt-and-suspenders: widen to group-writable even if umask was overridden,
# so the app user (`shiny`) can migrate the schema at runtime. POSIX-only.
try(Sys.chmod(db_path, mode = "0664"), silent = TRUE)
```

with:

```r
# F19: install the finished build over the live DB. Disconnects first
# (Windows cannot rename an open SQLite file), widens the mode to 0664 so the
# app user (`shiny`) can migrate the schema at runtime, renames, and falls
# back to copy + remove if the rename fails.
finalize_offline_db_build(con, tmp_path, db_path)
```

No other line of the script changes (all inserts still use `con`; `db_path` is still printed at the end).

- [ ] **Step 7: Parse-check and run the test file**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='scripts/initialization/build_offline_trait_db.R'); cat('OK\n')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/test-rebuild-lock.R')"
```

Expected: `OK`; `[ FAIL 0 | WARN 0 | SKIP 1 | PASS 66 ]` on Windows (Linux: SKIP 0 | PASS 68) (about 15 s).

- [ ] **Step 8: Commit**

```bash
git add scripts/initialization/build_offline_trait_db.R tests/testthat/test-rebuild-lock.R
git commit -m "$(cat <<'EOF'
fix(rebuild): build offline DB into a tmp file under the rebuild lock (F19)

The build script deleted cache/offline_traits.db before a build that can
stop() (e.g. ontology drift), leaving no DB, and nothing stopped two builds
from writing the same file. It now takes (or adopts) the rebuild lock,
builds into offline_traits.db.tmp.<pid>, and renames it over the live DB
only when finished. Cleanup runs from a reg.finalizer(onexit = TRUE):
top-level on.exit() never runs in an Rscript, so the old disconnect was
dead code.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 3: Rebuild observer goes through the gate and the lock (F19, part 3)

**Files:**
- Create: `tests/testthat/test-rebuild-observer.R`
- Modify: `R/modules/trait_research_server.R` (the block from `  # Background process handle for async rebuild` to the end of `observeEvent(input$rebuild_offline_db, ...)`)
- Modify: `app.R` (one line after `source("R/functions/admin_auth.R")`)

**Interfaces:**
- Consumes: Task 1 `offline_db_lock_path()`, `request_offline_rebuild()`, `release_rebuild_lock()`, `REBUILD_LOCK_TOKEN_ENV`; Task 2's script behaviour (adopts the token, releases its own lock). `session$userData$admin_unlocked` is set by `plugin_server.R`'s unlock modal (unchanged).
- Produces: module locals `offline_rebuild_process` (reactiveVal, unchanged name), `rebuild_lock_dir`, `rebuild_state` (env with `token`, `out`, `err`); the tests read `offline_rebuild_process()` inside `testServer`.

- [ ] **Step 1: Write the failing test file**

Create `tests/testthat/test-rebuild-observer.R` with exactly this content:

```r
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
```

Notes for the implementer: `testServer()` refuses `args` for a plain (non-`moduleServer`) server function ("Arguments were provided to a server function"), hence `rebuild_test_module()` gives `shared_data` a default instead. `withr::local_package("bs4Dash")` is needed because the module registers `renderValueBox()` at start-up; it is detached when the test ends.

- [ ] **Step 2: Run it and verify it fails**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/test-rebuild-observer.R')"
```

Expected (verified on a scratch copy of the pre-fix module): `[ FAIL 7 | WARN 8 | SKIP 0 | PASS 11 ]`. The two refusal tests fail with "Expected `session$setInputs(rebuild_offline_db = 1)` to produce warnings" (the old observer has no gate); the token test errors `cannot open the connection` on `token_seen.txt` (the old observer looks for the wd-relative script, finds nothing under `tests/testthat/`, and starts no build); the two session-end tests error `attempt to apply non-function` (no process was started); and the app.R guard fails (`helper` has length 0) and errors in `expect_lt`. The "another build holds the lock" test passes pre-fix (the old code starts nothing there either); it guards against a regression that would launch anyway.

- [ ] **Step 3: Source the helper from `app.R`**

In `app.R`, replace:

```r
source("R/functions/admin_auth.R")        # Optional password gate for the API key modal
```

with:

```r
source("R/functions/admin_auth.R")        # Optional password gate for the API key modal
source("R/functions/offline_db_rebuild.R")  # Offline trait DB rebuild: admin gate, lock, atomic install (F19)
```

It must stay outside the `ENABLE_PHASE6` block: the module calls `offline_db_lock_path()` at start-up, so an unsourced helper would error inside `server()` for every session.

- [ ] **Step 4: Replace the rebuild block in `R/modules/trait_research_server.R`**

Replace this block (it starts at `  # Background process handle for async rebuild` and ends with the `  })` that closes `observeEvent(input$rebuild_offline_db, { ... })`, just above `  output$offline_db_contents <- DT::renderDataTable({`):

```r
  # Background process handle for async rebuild
  offline_rebuild_process <- reactiveVal(NULL)

  # Single session-level poller for rebuild completion. req() suspends
  # the observer when no process is in flight, so it costs nothing while
  # idle. Created ONCE at module init; previous design created a fresh
  # observe() inside every observeEvent click, which leaked observers
  # under repeated clicks since they had no destroy hook.
  observe({
    proc <- offline_rebuild_process()
    req(proc)
    if (proc$is_alive()) {
      invalidateLater(2000, session)
      return()
    }

    # Process finished - handle completion. Wrap the body in tryCatch so a
    # rendering / notification failure doesn't leave the process reactive
    # in a "finished but unhandled" state forever.
    tryCatch(isolate({
      exit_status <- proc$get_exit_status()
      removeNotification("rebuild_progress")

      if (exit_status == 0) {
        output_text <- tryCatch(
          proc$read_all_output_lines(),
          error = function(e) character(0)
        )
        output_text <- paste(output_text, collapse = "\n")
        total_match <- regmatches(output_text,
          regexpr("Total species in database: [0-9]+", output_text))
        summary <- if (length(total_match) > 0) total_match else "Build complete"

        showNotification(
          HTML(paste0("<b>Offline database rebuilt!</b><br>", summary)),
          type = "message", duration = 8
        )
      } else {
        stderr_text <- tryCatch(
          paste(proc$read_all_error_lines(), collapse = "\n"),
          error = function(e) ""
        )
        err_lines <- strsplit(stderr_text, "\n")[[1]]
        err_lines <- err_lines[nchar(trimws(err_lines)) > 0]
        last_err <- if (length(err_lines) > 0) {
          tail(err_lines, 1)
        } else {
          "Unknown error"
        }
        showNotification(
          HTML(paste0("<b>Rebuild failed</b> (exit ", exit_status, ")<br>",
                      htmltools::htmlEscape(last_err))),
          type = "error", duration = 15
        )
      }

      offline_db_trigger(offline_db_trigger() + 1)
      offline_rebuild_process(NULL)
    }), error = function(e) {
      warning(sprintf("[rebuild observer] handler error: %s", conditionMessage(e)), call. = FALSE)
      isolate(offline_rebuild_process(NULL))
    })
  })

  observeEvent(input$rebuild_offline_db, {
    # Prevent double-click while rebuilding
    proc <- offline_rebuild_process()
    if (!is.null(proc) && proc$is_alive()) {
      showNotification("Rebuild already in progress...", type = "warning")
      return()
    }

    build_script <- "scripts/initialization/build_offline_trait_db.R"
    if (!file.exists(build_script)) {
      showNotification(
        paste("Build script not found:", build_script),
        type = "error"
      )
      return()
    }

    showNotification(
      HTML("<b>Rebuilding offline trait database...</b><br>This may take a minute."),
      type = "message", duration = NULL, id = "rebuild_progress"
    )

    tryCatch({
      # Run the build script as a background Rscript process. The
      # session-level observer above takes over once we set the process
      # reactive — no nested observe() needed here.
      rscript_path <- file.path(R.home("bin"), "Rscript")
      proc <- processx::process$new(
        rscript_path,
        args = normalizePath(build_script),
        wd = normalizePath("."),
        stdout = "|", stderr = "|",
        supervise = TRUE
      )
      offline_rebuild_process(proc)
    }, error = function(e) {
      removeNotification("rebuild_progress")
      showNotification(
        paste("Failed to start rebuild:", e$message),
        type = "error"
      )
    })
  })
```

with:

```r
  # Background process handle for async rebuild
  offline_rebuild_process <- reactiveVal(NULL)

  # F19: what THIS session's rebuild holds - the lock token and the child's
  # log files. A plain environment, not a reactive: only the exit poller and
  # onSessionEnded read it. The child adopts the lock through the token and
  # releases it itself when it exits; the releases below are token-checked
  # no-ops then, and only matter if the child died before it could release.
  rebuild_lock_dir <- offline_db_lock_path()
  rebuild_state <- new.env()
  rebuild_state$token <- NULL
  rebuild_state$out <- NULL
  rebuild_state$err <- NULL

  release_session_rebuild_lock <- function() {
    if (!is.null(rebuild_state$token)) {
      release_rebuild_lock(rebuild_lock_dir, rebuild_state$token)
      rebuild_state$token <- NULL
    }
  }

  read_rebuild_log <- function(path) {
    if (is.null(path) || !file.exists(path)) return(character(0))
    tryCatch(readLines(path, warn = FALSE), error = function(e) character(0))
  }

  # A closed tab must not leave the lock behind. Release only when the build
  # is no longer running: a live child keeps building (cleanup = FALSE) and
  # releases the lock itself when it exits.
  session$onSessionEnded(function() {
    proc <- isolate(offline_rebuild_process())
    if (is.null(proc) || !proc$is_alive()) release_session_rebuild_lock()
  })

  # Single session-level poller for rebuild completion. req() suspends
  # the observer when no process is in flight, so it costs nothing while
  # idle. Created ONCE at module init; previous design created a fresh
  # observe() inside every observeEvent click, which leaked observers
  # under repeated clicks since they had no destroy hook.
  observe({
    proc <- offline_rebuild_process()
    req(proc)
    if (proc$is_alive()) {
      invalidateLater(2000, session)
      return()
    }

    # Process finished - handle completion. Wrap the body in tryCatch so a
    # rendering / notification failure doesn't leave the process reactive
    # in a "finished but unhandled" state forever. `finally` releases this
    # session's lock on success, failure and handler error alike.
    tryCatch(isolate({
      exit_status <- proc$get_exit_status()
      removeNotification("rebuild_progress")

      if (isTRUE(exit_status == 0)) {
        output_text <- paste(read_rebuild_log(rebuild_state$out), collapse = "\n")
        total_match <- regmatches(output_text,
          regexpr("Total species in database: [0-9]+", output_text))
        summary <- if (length(total_match) > 0) total_match else "Build complete"

        showNotification(
          HTML(paste0("<b>Offline database rebuilt!</b><br>", summary)),
          type = "message", duration = 8
        )
      } else {
        err_lines <- read_rebuild_log(rebuild_state$err)
        err_lines <- err_lines[nchar(trimws(err_lines)) > 0]
        last_err <- if (length(err_lines) > 0) {
          tail(err_lines, 1)
        } else {
          "Unknown error"
        }
        showNotification(
          HTML(paste0("<b>Rebuild failed</b> (exit ", exit_status, ")<br>",
                      htmltools::htmlEscape(last_err))),
          type = "error", duration = 15
        )
      }

      offline_db_trigger(offline_db_trigger() + 1)
      offline_rebuild_process(NULL)
    }), error = function(e) {
      warning(sprintf("[rebuild observer] handler error: %s", conditionMessage(e)), call. = FALSE)
      isolate(offline_rebuild_process(NULL))
    }, finally = {
      release_session_rebuild_lock()
      unlink(c(rebuild_state$out, rebuild_state$err))
    })
  })

  # F19: admin gate first (fail closed when ECONETOOL_ADMIN_PASSWORD_HASH is
  # unset), then the process-wide lock, then the child Rscript. The decision
  # lives in request_offline_rebuild() (R/functions/offline_db_rebuild.R) so
  # it is unit-tested without a session.
  observeEvent(input$rebuild_offline_db, {
    out_file <- tempfile("offline_rebuild_", fileext = ".out")
    err_file <- tempfile("offline_rebuild_", fileext = ".err")

    res <- request_offline_rebuild(
      unlocked = session$userData$admin_unlocked,
      lock_dir = rebuild_lock_dir,
      launch = function(token) {
        build_script <- app_path("scripts/initialization/build_offline_trait_db.R")
        if (!file.exists(build_script)) {
          stop("build script not found: ", build_script, call. = FALSE)
        }
        # Logs go to files, not pipes: nothing reads a pipe until the build
        # ends, and a full pipe buffer (or a closed tab) would stall or kill
        # the child while it holds the lock. cleanup = FALSE lets the build
        # finish if the tab is closed; supervise = TRUE still reaps it if the
        # whole R process dies.
        processx::process$new(
          file.path(R.home("bin"), "Rscript"),
          args = build_script,
          wd = app_path(),
          env = c("current", stats::setNames(token, REBUILD_LOCK_TOKEN_ENV)),
          stdout = out_file, stderr = err_file,
          supervise = TRUE, cleanup = FALSE
        )
      }
    )

    if (!identical(res$status, "started")) {
      unlink(c(out_file, err_file))
      showNotification(res$message,
                       type = if (identical(res$status, "busy")) "warning" else "error",
                       duration = 8)
      return()
    }

    rebuild_state$token <- res$token
    rebuild_state$out <- out_file
    rebuild_state$err <- err_file
    offline_rebuild_process(res$proc)
    showNotification(
      HTML("<b>Rebuilding offline trait database...</b><br>This may take a minute."),
      type = "message", duration = NULL, id = "rebuild_progress"
    )
  })
```

What changed and why (for the reviewer):
- The gate is the first thing the click does (inside `request_offline_rebuild()`); the old per-session "already in progress" check is superseded by the process-wide lock, which also covers other sessions and console builds.
- The launch closure builds the script path with `app_path()` (the old wd-relative `"scripts/initialization/build_offline_trait_db.R"` + `normalizePath(".")`), passes the token in `env`, and logs to temp files.
- The poller reads the log files; its `finally` releases this session's token (a no-op when the child already released) and deletes the logs. The exit-status test is `isTRUE(exit_status == 0)`.
- `session$onSessionEnded` releases only if the child is no longer running.

- [ ] **Step 5: Parse-check and run the tests**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='R/modules/trait_research_server.R'); parse(file='app.R'); parse(file='tests/testthat/test-rebuild-observer.R'); cat('OK\n')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/test-rebuild-observer.R'); testthat::test_file('tests/testthat/test-deep-analysis-fixes.R'); testthat::test_file('tests/testthat/test-config-paths.R')"
```

Expected: `OK`; `test-rebuild-observer.R` `[ FAIL 0 | WARN 7 | SKIP 0 | PASS 23 ]` (about 25 s) (the warnings are `package '...' was built under R version 4.4.3` notices from attaching shiny/bs4Dash); `test-deep-analysis-fixes.R` and `test-config-paths.R` `FAIL 0` (no bare relative `source()` was added under `R/`).

- [ ] **Step 6: Commit**

```bash
git add app.R R/modules/trait_research_server.R tests/testthat/test-rebuild-observer.R
git commit -m "$(cat <<'EOF'
fix(trait-research): gate and lock the offline DB rebuild button (F19)

Anyone could start a rebuild, and the only guard was per session. The
button now requires an unlocked admin session (and a configured admin
hash), takes the process-wide rebuild lock, hands its token to the child
build, logs to files instead of unread pipes, and releases the lock when
the child is done or when the session ends after the child died.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 4: Safe rendering helpers and the EcoBase panel (F3)

**Files:**
- Modify: `R/functions/validation_utils.R` (append at end of file, after `app_path()`)
- Modify: `R/modules/ecobase_server.R` (prepend helpers; 2 edits)
- Create: `tests/testthat/test-xss-escaping.R` (first part; Task 5 appends)

**Interfaces:**
- Consumes: htmltools (`tags`), shiny (`tagList`, `tags` attached by `app.R`).
- Produces (Task 5 uses them): `safe_href(url) -> character(1) | NULL`; `safe_doi_href(doi) -> character(1) | NULL`; `meta_text(val, missing = c("", "-9999")) -> character(1) | shiny.tag`; `meta_row(label, value, label_style = "padding: 2px 0;", value_style = NULL) -> shiny.tag`; `meta_section_row(title)`; `meta_publication_row(doi = NULL, uri = NULL, ref = NULL, ref_label = "Reference:") -> shiny.tag | NULL`; `meta_description(desc, max_chars = 150) -> shiny.tag | NULL`. In `ecobase_server.R`: `ecobase_connection_error_ui(msg)`, `ecobase_metadata_panel(meta, model_id, model_name)`.

- [ ] **Step 1: Write the failing test (first part of the file)**

Create `tests/testthat/test-xss-escaping.R` with exactly this content:

```r
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

```

- [ ] **Step 2: Run it and verify it fails**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/test-xss-escaping.R')"
```

Expected (verified on a scratch copy): `[ FAIL 7 | WARN 12 | SKIP 0 | PASS 0 ]`: six `could not find function` errors (`safe_href`, `safe_doi_href`, `meta_text`, `ecobase_metadata_panel` x2, `ecobase_connection_error_ui`), and the end-to-end EcoBase test errors inside the OLD panel code with `invalid format '%.2f'; use format %s for character objects` - the pre-fix panel crashes on a non-numeric latitude before it even reaches the unescaped HTML. The 12 warnings are `package '...' was built under R version 4.4.3` notices from attaching shiny, DT and bs4Dash.

- [ ] **Step 3: Append the helpers to `R/functions/validation_utils.R`**

Append at the very end of the file (after the closing `}` of `app_path()`):

```r

# =============================================================================
# SAFE RENDERING OF THIRD-PARTY TEXT (F3 / F54)
# =============================================================================
# EcoBase metadata and uploaded .ewemdb metadata are attacker-controllable.
# Render them only through htmltools tag builders (tags$td(x) escapes x) and
# never paste them into HTML(). These helpers return plain character or tag
# objects, never pre-escaped strings, so nothing is escaped twice.

#' Return `url` only if it is a single http(s) URL, else NULL
#'
#' Blocks javascript:, data:, vbscript: and relative hrefs. The caller still
#' passes the result to tags$a(href = ...), which escapes quotes.
#' @param url Candidate URL.
#' @return `url` or NULL.
safe_href <- function(url) {
  if (is.character(url) && length(url) == 1 && !is.na(url) &&
        grepl("^https?://", url, ignore.case = TRUE)) url else NULL
}

#' Build a https://doi.org/ link target from a DOI, or NULL
#'
#' Accepts a bare DOI or one prefixed with doi: / https://doi.org/, then
#' requires the Crossref shape `10.<4-9 digits>/<no whitespace>`.
#' @param doi Candidate DOI.
#' @return Character href or NULL.
safe_doi_href <- function(doi) {
  if (!is.character(doi) || length(doi) != 1 || is.na(doi)) return(NULL)
  bare <- sub("^(https?://(dx\\.)?doi\\.org/|doi:\\s*)", "", trimws(doi), ignore.case = TRUE)
  if (!grepl("^10\\.\\d{4,9}/\\S+$", bare, perl = TRUE)) return(NULL)
  paste0("https://doi.org/", utils::URLencode(bare, reserved = FALSE))
}

#' Metadata value as escaped-by-construction content
#'
#' @param val Metadata value (any type).
#' @param missing Values that mean "not specified" (compared as character).
#' @return The value as character (tags$ builders escape it), or a grey
#'   "Not specified" span.
meta_text <- function(val, missing = c("", "-9999")) {
  if (is.null(val) || length(val) == 0 || all(is.na(val)) ||
        as.character(val[1]) %in% missing) {
    return(htmltools::tags$span(style = "color: #999;", "Not specified"))
  }
  as.character(val[1])
}

#' One label/value row of a metadata preview table
#' @param label Static label text.
#' @param value Character or tag; escaped by the builder.
#' @param label_style,value_style Inline CSS.
meta_row <- function(label, value, label_style = "padding: 2px 0;", value_style = NULL) {
  htmltools::tags$tr(
    htmltools::tags$td(style = label_style, label),
    htmltools::tags$td(style = value_style, value)
  )
}

#' Section header row (GEOGRAPHIC, TEMPORAL, ...) of a metadata preview table
meta_section_row <- function(title) {
  htmltools::tags$tr(
    style = "background: #f0f9ff;",
    htmltools::tags$td(colspan = "2", style = "padding: 4px 0; font-weight: bold;", title)
  )
}

#' Publication row: DOI link, else safe URL link, else escaped text, else NULL
#'
#' @param doi,uri,ref Candidate DOI, URL and free-text reference.
#' @param ref_label Label for the free-text row.
#' @return A tags$tr or NULL.
meta_publication_row <- function(doi = NULL, uri = NULL, ref = NULL, ref_label = "Reference:") {
  has <- function(v) !is.null(v) && length(v) > 0 && !all(is.na(v)) && nzchar(as.character(v[1]))
  link <- function(href, text) {
    htmltools::tags$a(href = href, target = "_blank", rel = "noopener noreferrer",
                      style = "color: #337ab7;", text)
  }
  if (has(doi)) {
    href <- safe_doi_href(as.character(doi[1]))
    value <- if (is.null(href)) as.character(doi[1]) else link(href, as.character(doi[1]))
    return(meta_row(htmltools::tags$strong("DOI:"), value))
  }
  if (has(uri)) {
    href <- safe_href(as.character(uri[1]))
    value <- if (is.null(href)) as.character(uri[1]) else link(href, "Link")
    return(meta_row(htmltools::tags$strong("Publication:"), value))
  }
  if (has(ref)) {
    return(meta_row(htmltools::tags$strong(ref_label), as.character(ref[1]),
                    value_style = "font-size: 11px;"))
  }
  NULL
}

#' Italic, truncated description paragraph, or NULL
meta_description <- function(desc, max_chars = 150) {
  if (is.null(desc) || length(desc) == 0 || all(is.na(desc)) || !nzchar(as.character(desc[1]))) {
    return(NULL)
  }
  text <- as.character(desc[1])
  if (nchar(text) > max_chars) text <- paste0(substr(text, 1, max_chars - 3), "...")
  htmltools::tags$p(style = "font-size: 11px; color: #555; font-style: italic; margin: 8px 0;", text)
}
```

- [ ] **Step 4: Add the panel builders to the top of `R/modules/ecobase_server.R`**

Insert this block at the very top of the file, above `#' EcoBase Connection Server Module` (followed by one blank line):

```r
# =============================================================================
# SAFE RENDERING HELPERS (F3)
# =============================================================================
# At file scope so they are unit-testable without a session. EcoBase fields
# are third-party text: they only ever reach the page through htmltools tag
# builders, which escape them. Never paste them into HTML().

#' Connection-failure message for the EcoBase status panel
#' @param msg Error text (escaped by the tag builder).
ecobase_connection_error_ui <- function(msg) {
  tagList(
    tags$p(style = "color: red;", tags$i(class = "fa fa-times"), " Connection failed: ", msg),
    tags$p(tags$small("Required packages: RCurl, XML, plyr, dplyr"))
  )
}

#' EcoBase model metadata preview
#' @param meta List from extract_ecobase_metadata().
#' @param model_id,model_name Values from the model list table.
#' @return A tagList; all metadata is escaped by construction.
ecobase_metadata_panel <- function(meta, model_id, model_name) {
  has_value <- function(field) {
    !is.null(field) && length(field) > 0 && !all(is.na(field)) && field[1] != "" && field[1] != -9999
  }
  fmt <- function(val) meta_text(val, missing = c("", "Not affiliated"))

  location_parts <- character(0)
  if (has_value(meta$ecosystem_name)) {
    location_parts <- c(location_parts, as.character(meta$ecosystem_name[1]))
  } else if (has_value(meta$model_name)) {
    location_parts <- c(location_parts, as.character(meta$model_name[1]))
  }
  if (has_value(meta$region)) {
    location_parts <- c(location_parts, as.character(meta$region[1]))
  }
  if (has_value(meta$country) && meta$country[1] != "Not affiliated") {
    location_parts <- c(location_parts, as.character(meta$country[1]))
  }
  location_text <- if (length(location_parts) > 0) paste(location_parts, collapse = ", ") else fmt(NA)

  time_period_text <- fmt(NA)
  if (has_value(meta$model_year)) {
    time_period_text <- as.character(meta$model_year[1])
  } else if (has_value(meta$model_period)) {
    time_period_text <- as.character(meta$model_period[1])
  }

  coords_text <- fmt(NA)
  lat <- if (has_value(meta$latitude)) suppressWarnings(as.numeric(meta$latitude[1])) else NA_real_
  lon <- if (has_value(meta$longitude)) suppressWarnings(as.numeric(meta$longitude[1])) else NA_real_
  if (!is.na(lat) && !is.na(lon)) {
    coords_text <- sprintf("%.2f deg N, %.2f deg E", lat, lon)
  }

  area_text <- fmt(meta$area)
  area_num <- if (has_value(meta$area)) suppressWarnings(as.numeric(meta$area[1])) else NA_real_
  if (!is.na(area_num) && area_num > 0) {
    area_text <- paste0(meta$area[1], " km2")
  }

  tagList(
    tags$h5(style = "margin-top: 0;", paste0("EcoBase Model #", model_id)),
    tags$p(style = "font-size: 11px; color: #888;", as.character(model_name)),
    meta_description(meta$description),
    tags$hr(style = "margin: 10px 0;"),
    tags$table(
      style = "width: 100%; font-size: 12px;",
      meta_section_row("GEOGRAPHIC"),
      meta_row("Location:", location_text, label_style = "padding: 2px 0; width: 35%;"),
      meta_row("Ecosystem Type:", fmt(meta$ecosystem_type)),
      meta_row("Area:", area_text),
      meta_row("Coordinates:", coords_text),
      meta_section_row("TEMPORAL"),
      meta_row("Time Period:", time_period_text),
      meta_section_row("ATTRIBUTION"),
      meta_row("Author:", fmt(meta$author)),
      meta_row("Contact:", fmt(meta$contact)),
      if (has_value(meta$institution)) {
        meta_row("Institution:", as.character(meta$institution[1]), value_style = "font-size: 11px;")
      },
      meta_publication_row(
        doi = if (has_value(meta$doi)) meta$doi,
        ref = if (has_value(meta$publication)) meta$publication,
        ref_label = "Publication:"
      )
    ),
    tags$hr(style = "margin: 10px 0;"),
    tags$p(style = "font-size: 12px;",
           "Select parameter type and click 'Import Model' to load into EcoNeTool.")
  )
}

```

- [ ] **Step 5: Render the connection error as text**

In `ecobase_server.R`, replace:

```r

    }, error = function(e) {
      output$ecobase_connection_status <- renderUI({
        HTML(paste0("<p style='color: red;'><i class='fa fa-times'></i> ",
                   "Connection failed: ", e$message, "</p>",
                   "<p><small>Required packages: RCurl, XML, plyr, dplyr</small></p>"))
      })
    })
```

with:

```r
    }, error = function(e) {
      warning(sprintf("[ecobase] loading the model list failed: %s", conditionMessage(e)),
              call. = FALSE)
      # F3: the error text can carry server-supplied content; render it as text.
      err_msg <- conditionMessage(e)
      output$ecobase_connection_status <- renderUI({
        ecobase_connection_error_ui(err_msg)
      })
    })
```

- [ ] **Step 6: Render the metadata panel through the builder**

Replace this block (from `    output$ecobase_model_details <- renderUI({` down to the `        )` that closes the `tagList(HTML(paste0(...)))`, directly above `      } else {`):

```r

    output$ecobase_model_details <- renderUI({
      meta <- tryCatch({
        extract_ecobase_metadata(model_id)
      }, error = function(e) {
        NULL
      })

      fmt <- function(val) {
        if (is.null(val) || length(val) == 0 || (length(val) == 1 && is.na(val)) || val == "" || val == "Not affiliated") {
          "<span style='color: #999;'>Not specified</span>"
        } else {
          as.character(val)
        }
      }

      has_value <- function(field) {
        !is.null(field) && length(field) > 0 && !all(is.na(field)) && field[1] != "" && field[1] != -9999
      }

      if (!is.null(meta)) {
        location_parts <- c()
        if (has_value(meta$ecosystem_name)) {
          location_parts <- c(location_parts, meta$ecosystem_name)
        } else if (has_value(meta$model_name)) {
          location_parts <- c(location_parts, meta$model_name)
        }
        if (has_value(meta$region)) {
          location_parts <- c(location_parts, meta$region)
        }
        if (has_value(meta$country) && meta$country != "Not affiliated") {
          location_parts <- c(location_parts, meta$country)
        }
        location_text <- if (length(location_parts) > 0) paste(location_parts, collapse = ", ") else fmt(NA)

        time_period_text <- fmt(NA)
        if (has_value(meta$model_year)) {
          time_period_text <- meta$model_year
        } else if (has_value(meta$model_period)) {
          time_period_text <- meta$model_period
        }

        coords_text <- fmt(NA)
        if (has_value(meta$latitude) && has_value(meta$longitude)) {
          coords_text <- sprintf("%.2f deg N, %.2f deg E", meta$latitude, meta$longitude)
        }

        area_text <- fmt(meta$area)
        if (has_value(meta$area) && meta$area > 0) {
          area_text <- paste0(meta$area, " km2")
        }

        pub_html <- ""
        if (has_value(meta$doi)) {
          pub_html <- paste0("<tr><td style='padding: 2px 0;'><strong>DOI:</strong></td><td><a href='https://doi.org/", meta$doi, "' target='_blank' style='color: #337ab7;'>", meta$doi, "</a></td></tr>")
        } else if (has_value(meta$publication)) {
          pub_html <- paste0("<tr><td style='padding: 2px 0;'><strong>Publication:</strong></td><td style='font-size: 11px;'>", meta$publication, "</td></tr>")
        }

        desc_html <- ""
        if (has_value(meta$description)) {
          desc_text <- meta$description
          if (nchar(desc_text) > 150) {
            desc_text <- paste0(substr(desc_text, 1, 147), "...")
          }
          desc_html <- paste0("<p style='font-size: 11px; color: #555; font-style: italic; margin: 8px 0;'>", desc_text, "</p>")
        }

        inst_html <- ""
        if (has_value(meta$institution)) {
          inst_html <- paste0("<tr><td style='padding: 2px 0;'>Institution:</td><td style='font-size: 11px;'>", meta$institution, "</td></tr>")
        }

        tagList(
          HTML(paste0("
            <h5 style='margin-top: 0;'>EcoBase Model #", model_id, "</h5>
            <p style='font-size: 11px; color: #888;'>", model_name, "</p>
            ", desc_html, "
            <hr style='margin: 10px 0;'>
            <table style='width: 100%; font-size: 12px;'>
              <tr style='background: #f0f9ff;'><td colspan='2' style='padding: 4px 0; font-weight: bold;'>GEOGRAPHIC</td></tr>
              <tr><td style='padding: 2px 0; width: 35%;'>Location:</td><td>", location_text, "</td></tr>
              <tr><td style='padding: 2px 0;'>Ecosystem Type:</td><td>", fmt(meta$ecosystem_type), "</td></tr>
              <tr><td style='padding: 2px 0;'>Area:</td><td>", area_text, "</td></tr>
              <tr><td style='padding: 2px 0;'>Coordinates:</td><td>", coords_text, "</td></tr>
              <tr style='background: #f0f9ff;'><td colspan='2' style='padding: 4px 0; font-weight: bold;'>TEMPORAL</td></tr>
              <tr><td style='padding: 2px 0;'>Time Period:</td><td>", time_period_text, "</td></tr>
              <tr style='background: #f0f9ff;'><td colspan='2' style='padding: 4px 0; font-weight: bold;'>ATTRIBUTION</td></tr>
              <tr><td style='padding: 2px 0;'>Author:</td><td>", fmt(meta$author), "</td></tr>
              <tr><td style='padding: 2px 0;'>Contact:</td><td>", fmt(meta$contact), "</td></tr>
              ", inst_html, "
              ", pub_html, "
            </table>
            <hr style='margin: 10px 0;'>
            <p style='font-size: 12px;'>Select parameter type and click 'Import Model' to load into EcoNeTool.</p>
          "))
        )
```

with:

```r
    output$ecobase_model_details <- renderUI({
      meta <- tryCatch({
        extract_ecobase_metadata(model_id)
      }, error = function(e) {
        warning(sprintf("[ecobase] metadata for model %s unavailable: %s",
                        model_id, conditionMessage(e)), call. = FALSE)
        NULL
      })

      if (!is.null(meta)) {
        # F3: every EcoBase field is rendered as escaped text via tag builders.
        ecobase_metadata_panel(meta, model_id, model_name)
```

The `} else { tagList(h4(model_name), tags$table(...)) }` fallback below is unchanged (already tag-built).

- [ ] **Step 7: Parse-check, lint the new code, run the test**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='R/functions/validation_utils.R'); parse(file='R/modules/ecobase_server.R'); parse(file='tests/testthat/test-xss-escaping.R'); cat('OK\n')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "l <- lintr::lint('R/modules/ecobase_server.R'); print(l[vapply(l, function(z) z\$line_number <= 100, TRUE)]); print(lintr::lint('R/functions/validation_utils.R'))"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/test-xss-escaping.R')"
```

Expected: `OK`; no lints in the new helper block or in `validation_utils.R` (the remaining `ecobase_server.R` lints further down are pre-existing: indentation at the `ecobase_server <- function(` continuation line and the `"hybrid" = ` switch, and the 120+ char `HTML("<p style='font-size: 12px; color: #999;'>Could not load...` line); test `[ FAIL 0 | WARN 12 | SKIP 0 | PASS 39 ]` (warnings: the "built under" notices).

- [ ] **Step 8: Commit**

```bash
git add R/functions/validation_utils.R R/modules/ecobase_server.R tests/testthat/test-xss-escaping.R
git commit -m "$(cat <<'EOF'
fix(ecobase): render EcoBase metadata and errors as text (F3)

The model preview pasted EcoBase description, author, contact,
institution, model name and a doi into HTML(), and the connection error
pasted e$message. The panel is now built with htmltools tags, which escape
by construction; DOIs are validated and rebuilt on https://doi.org/, and
only http(s) URLs ever become an href (safe_href / safe_doi_href in
validation_utils.R).

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 5: EwE (.ewemdb) preview renders as text (F54)

**Files:**
- Modify: `R/modules/ecopath_import_server.R` (prepend helpers; 1 edit in `output$ecopath_native_preview_ui`)
- Modify: `tests/testthat/test-xss-escaping.R` (append)

**Interfaces:**
- Consumes: Task 4 helpers `meta_text`, `meta_row`, `meta_section_row`, `meta_publication_row`, `meta_description` (and through them `safe_href`, `safe_doi_href`).
- Produces: `ewe_preview_error_panel(error)`, `ewe_preview_panel(preview_data)` where `preview_data` is `list(metadata, n_groups, n_links, filename, filesize)` (as stored by the `input$ecopath_native_file` observer, unchanged).

- [ ] **Step 1: Append the failing tests**

Append this to the end of `tests/testthat/test-xss-escaping.R`:

```r
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
```

- [ ] **Step 2: Run the file and verify the new tests fail**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/test-xss-escaping.R')"
```

Expected (verified on a scratch copy): `[ FAIL 7 | WARN 27 | SKIP 0 | PASS 41 ]`: four `could not find function` errors (`ewe_preview_panel` x3, `ewe_preview_error_panel`); the end-to-end EwE test fails twice on the old code (`html` does not contain `&lt;img src=x onerror=alert(2)&gt;` and does contain `<img src=x` - the uploaded file name was injected raw); and the source guard fails (`offenders` has length 1: `ecopath_import_server.R` still has `HTML(paste0(`).

- [ ] **Step 3: Add the panel builders to the top of `R/modules/ecopath_import_server.R`**

Insert this block at the very top of the file, above `#' ECOPATH Import Server Logic` (followed by one blank line). The `°` and `²` characters are literal UTF-8, as elsewhere in this file:

```r
# =============================================================================
# SAFE RENDERING HELPERS (F54)
# =============================================================================
# At file scope so they are unit-testable without a session. An uploaded
# .ewemdb is user-controlled: its metadata, its file name and parser errors
# only reach the page through htmltools tag builders, which escape them.

#' Body of the "Error Reading Database" box
#' @param error Parser error text (escaped by the tag builder).
ewe_preview_error_panel <- function(error) {
  tagList(
    tags$p(style = "color: #d9534f;", tags$strong("Could not read database file")),
    tags$pre(style = "background: #f8f9fa; padding: 10px; font-size: 11px; color: #d9534f;",
             as.character(error)),
    tags$p(style = "font-size: 12px;", "Please ensure:"),
    tags$ul(
      style = "font-size: 12px;",
      tags$li("File is a valid ECOPATH database (.ewemdb, .eweaccdb, .mdb, .eiidb, .accdb)"),
      tags$li("Required packages are installed (RODBC on Windows, Hmisc on Linux/Mac)"),
      tags$li("Microsoft Access Database Engine is installed (Windows only)")
    )
  )
}

#' Body of the "Model Preview" box for an uploaded EwE database
#' @param preview_data list(metadata, n_groups, n_links, filename, filesize).
#' @return A tagList; all metadata is escaped by construction.
ewe_preview_panel <- function(preview_data) {
  meta <- preview_data$metadata
  has_value <- function(field) {
    !is.null(field) && length(field) > 0 && !all(is.na(field)) && field[1] != "" && field[1] != -9999
  }
  fmt <- function(val) meta_text(val, missing = c("", "-9999"))
  num <- function(field) if (has_value(field)) suppressWarnings(as.numeric(field[1])) else NA_real_

  location_parts <- character(0)
  if (has_value(meta$area_name)) {
    location_parts <- c(location_parts, as.character(meta$area_name[1]))
  } else if (has_value(meta$name)) {
    location_parts <- c(location_parts, as.character(meta$name[1]))
  }
  if (has_value(meta$country)) {
    location_parts <- c(location_parts, as.character(meta$country[1]))
  }
  location_text <- if (length(location_parts) > 0) paste(location_parts, collapse = ", ") else fmt(NA)

  time_period_text <- fmt(NA)
  first_year <- num(meta$first_year)
  if (!is.na(first_year)) {
    num_years <- num(meta$num_years)
    time_period_text <- if (!is.na(num_years) && num_years > 1) {
      paste0(first_year, "-", first_year + num_years - 1)
    } else {
      as.character(first_year)
    }
  }

  coords_text <- fmt(NA)
  bbox <- c(num(meta$min_lat), num(meta$max_lat), num(meta$min_lon), num(meta$max_lon))
  if (!anyNA(bbox)) {
    coords_text <- sprintf("%.2f°-%.2f°N, %.2f°-%.2f°E", bbox[1], bbox[2], bbox[3], bbox[4])
  }

  area_text <- fmt(meta$area)
  area_num <- num(meta$area)
  if (!is.na(area_num) && area_num > 0) {
    area_text <- paste0(meta$area[1], " km²")
  }

  filesize_kb <- suppressWarnings(round(as.numeric(preview_data$filesize) / 1024, 1))

  tagList(
    tags$h5(style = "margin-top: 0;", as.character(preview_data$filename %||% "")),
    tags$p(style = "font-size: 11px; color: #888;", paste0(filesize_kb, " KB")),
    meta_description(meta$description),
    tags$hr(style = "margin: 10px 0;"),
    tags$table(
      style = "width: 100%; font-size: 12px;",
      meta_section_row("GEOGRAPHIC"),
      meta_row("Location:", location_text, label_style = "padding: 2px 0; width: 30%;"),
      meta_row("Ecosystem Type:", fmt(meta$ecosystem_type)),
      meta_row("Area:", area_text),
      meta_row("Coordinates:", coords_text),
      meta_section_row("TEMPORAL"),
      meta_row("Time Period:", time_period_text),
      meta_section_row("ATTRIBUTION"),
      meta_row("Author:", fmt(meta$author)),
      meta_row("Contact:", fmt(meta$contact)),
      meta_publication_row(
        doi = if (has_value(meta$publication_doi)) meta$publication_doi,
        uri = if (has_value(meta$publication_uri)) meta$publication_uri,
        ref = if (has_value(meta$publication_ref)) meta$publication_ref
      ),
      meta_section_row("MODEL DATA"),
      meta_row(tags$strong("Species/Groups:"), tags$strong(as.character(preview_data$n_groups))),
      meta_row(tags$strong("Diet Links:"), tags$strong(as.character(preview_data$n_links)))
    ),
    tags$hr(style = "margin: 10px 0;"),
    tags$p(style = "font-size: 12px; color: #5cb85c;",
           tags$i(class = "fa fa-check-circle"), " Ready to import")
  )
}

```

- [ ] **Step 4: Replace the error and preview branches of `output$ecopath_native_preview_ui`**

Replace this block (from `    } else if (!is.null(preview_data$error)) {` to the `    }` that closes the if/else chain, directly above the `  })` that closes `renderUI`; the `if (is.null(preview_data))` guide branch above it is static and stays):

```r
    } else if (!is.null(preview_data$error)) {
      # Show error if extraction failed
      box(
        title = "Error Reading Database",
        status = "danger",
        solidHeader = TRUE,
        width = 12,
        HTML(paste0("
          <p style='color: #d9534f;'><strong>Could not read database file</strong></p>
          <pre style='background: #f8f9fa; padding: 10px; font-size: 11px; color: #d9534f;'>", preview_data$error, "</pre>
          <p style='font-size: 12px;'>Please ensure:</p>
          <ul style='font-size: 12px;'>
            <li>File is a valid ECOPATH database (.ewemdb, .eweaccdb, .mdb, .eiidb, .accdb)</li>
            <li>Required packages are installed (RODBC on Windows, Hmisc on Linux/Mac)</li>
            <li>Microsoft Access Database Engine is installed (Windows only)</li>
          </ul>
        "))
      )
    } else {
      # Show model preview
      meta <- preview_data$metadata

      # Helper function to format metadata value
      fmt <- function(val) {
        if (is.null(val) || length(val) == 0 || (length(val) == 1 && is.na(val)) || val == "" || val == -9999) {
          "<span style='color: #999;'>Not specified</span>"
        } else {
          as.character(val)
        }
      }

      # Helper function to safely check if metadata field has valid value
      has_value <- function(field) {
        !is.null(field) && length(field) > 0 && !all(is.na(field)) && field[1] != "" && field[1] != -9999
      }

      # Build location string
      location_parts <- c()
      if (!is.null(meta) && has_value(meta$area_name)) {
        location_parts <- c(location_parts, meta$area_name)
      } else if (!is.null(meta) && has_value(meta$name)) {
        location_parts <- c(location_parts, meta$name)
      }
      if (!is.null(meta) && has_value(meta$country)) {
        location_parts <- c(location_parts, meta$country)
      }
      location_text <- if (length(location_parts) > 0) paste(location_parts, collapse = ", ") else fmt(NA)

      # Build time period string
      time_period_text <- fmt(NA)
      if (!is.null(meta) && has_value(meta$first_year)) {
        if (has_value(meta$num_years) && meta$num_years > 1) {
          end_year <- meta$first_year + meta$num_years - 1
          time_period_text <- paste0(meta$first_year, "-", end_year)
        } else {
          time_period_text <- as.character(meta$first_year)
        }
      }

      # Build geographic coordinates
      coords_text <- fmt(NA)
      if (!is.null(meta) && has_value(meta$min_lat) && has_value(meta$max_lat) && has_value(meta$min_lon) && has_value(meta$max_lon)) {
        coords_text <- sprintf("%.2f°-%.2f°N, %.2f°-%.2f°E", meta$min_lat, meta$max_lat, meta$min_lon, meta$max_lon)
      }

      # Build area text
      area_text <- fmt(meta$area)
      if (!is.null(meta) && has_value(meta$area) && meta$area > 0) {
        area_text <- paste0(meta$area, " km²")
      }

      # Build publication link
      pub_html <- ""
      if (!is.null(meta) && has_value(meta$publication_doi)) {
        pub_html <- paste0("<tr><td style='padding: 2px 0;'><strong>DOI:</strong></td><td><a href='https://doi.org/", meta$publication_doi, "' target='_blank' style='color: #337ab7;'>", meta$publication_doi, "</a></td></tr>")
      } else if (!is.null(meta) && has_value(meta$publication_uri)) {
        pub_html <- paste0("<tr><td style='padding: 2px 0;'><strong>Publication:</strong></td><td><a href='", meta$publication_uri, "' target='_blank' style='color: #337ab7;'>Link</a></td></tr>")
      } else if (!is.null(meta) && has_value(meta$publication_ref)) {
        pub_html <- paste0("<tr><td style='padding: 2px 0;'><strong>Reference:</strong></td><td style='font-size: 11px;'>", meta$publication_ref, "</td></tr>")
      }

      # Build description HTML (truncated if too long)
      desc_html <- ""
      if (!is.null(meta) && has_value(meta$description)) {
        desc_text <- meta$description
        if (nchar(desc_text) > 150) {
          desc_text <- paste0(substr(desc_text, 1, 147), "...")
        }
        desc_html <- paste0("<p style='font-size: 11px; color: #555; font-style: italic; margin: 8px 0;'>", desc_text, "</p>")
      }

      box(
        title = "Model Preview",
        status = "success",
        solidHeader = TRUE,
        width = 12,
        HTML(paste0("
          <h5 style='margin-top: 0;'>", preview_data$filename, "</h5>
          <p style='font-size: 11px; color: #888;'>", round(preview_data$filesize / 1024, 1), " KB</p>
          ", desc_html, "
          <hr style='margin: 10px 0;'>
          <table style='width: 100%; font-size: 12px;'>
            <tr style='background: #f0f9ff;'><td colspan='2' style='padding: 4px 0; font-weight: bold;'>GEOGRAPHIC</td></tr>
            <tr><td style='padding: 2px 0; width: 30%;'>Location:</td><td>", location_text, "</td></tr>
            <tr><td style='padding: 2px 0;'>Ecosystem Type:</td><td>", fmt(meta$ecosystem_type), "</td></tr>
            <tr><td style='padding: 2px 0;'>Area:</td><td>", area_text, "</td></tr>
            <tr><td style='padding: 2px 0;'>Coordinates:</td><td>", coords_text, "</td></tr>
            <tr style='background: #f0f9ff;'><td colspan='2' style='padding: 4px 0; font-weight: bold;'>TEMPORAL</td></tr>
            <tr><td style='padding: 2px 0;'>Time Period:</td><td>", time_period_text, "</td></tr>
            <tr style='background: #f0f9ff;'><td colspan='2' style='padding: 4px 0; font-weight: bold;'>ATTRIBUTION</td></tr>
            <tr><td style='padding: 2px 0;'>Author:</td><td>", fmt(meta$author), "</td></tr>
            <tr><td style='padding: 2px 0;'>Contact:</td><td>", fmt(meta$contact), "</td></tr>
            ", pub_html, "
            <tr style='background: #f0f9ff;'><td colspan='2' style='padding: 4px 0; font-weight: bold;'>MODEL DATA</td></tr>
            <tr><td style='padding: 2px 0;'><strong>Species/Groups:</strong></td><td><strong>", preview_data$n_groups, "</strong></td></tr>
            <tr><td style='padding: 2px 0;'><strong>Diet Links:</strong></td><td><strong>", preview_data$n_links, "</strong></td></tr>
          </table>
          <hr style='margin: 10px 0;'>
          <p style='font-size: 12px; color: #5cb85c;'><i class='fa fa-check-circle'></i> Ready to import</p>
        "))
      )
    }
```

with:

```r
    } else if (!is.null(preview_data$error)) {
      # Show error if extraction failed. F54: the parser error can quote file
      # content, so it goes in as escaped text.
      box(
        title = "Error Reading Database",
        status = "danger",
        solidHeader = TRUE,
        width = 12,
        ewe_preview_error_panel(preview_data$error)
      )
    } else {
      # Show model preview. F54: every .ewemdb field and the upload's file
      # name are rendered as escaped text via tag builders.
      box(
        title = "Model Preview",
        status = "success",
        solidHeader = TRUE,
        width = 12,
        ewe_preview_panel(preview_data)
      )
    }
```

- [ ] **Step 5: Parse-check and run the tests**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='R/modules/ecopath_import_server.R'); parse(file='tests/testthat/test-xss-escaping.R'); cat('OK\n')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "l <- lintr::lint('R/modules/ecopath_import_server.R'); print(l[vapply(l, function(z) z\$line_number <= 110, TRUE)])"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/test-xss-escaping.R')"
```

Expected: `OK`; no lints in the new helper block (lines 1-110; the file's other lints are pre-existing); test `[ FAIL 0 | WARN 27 | SKIP 0 | PASS 56 ]`.

- [ ] **Step 6: Commit**

```bash
git add R/modules/ecopath_import_server.R tests/testthat/test-xss-escaping.R
git commit -m "$(cat <<'EOF'
fix(ecopath-import): render uploaded EwE metadata and errors as text (F54)

The .ewemdb preview pasted file metadata, a publication_uri href, the
upload's file name and the parser error into HTML(). The preview and error
panels are now built with htmltools tags; publication links pass
safe_href()/safe_doi_href(). A source guard fails if either module pastes
anything dynamic into HTML() again.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 6: API-key modal keeps stored secrets (F6)

**Files:**
- Modify: `R/modules/plugin_server.R` (prepend helpers; 2 edits)
- Create: `tests/testthat/test-plugin-api-keys.R`

**Interfaces:**
- Consumes: globals `API_KEYS` (environment), `API_KEYS_JSON`, `API_KEYS_FILE` (`R/config.R`, unchanged); `admin_authorized()` (unchanged, fail-open when the gate is unset).
- Produces: `api_key_modal_dialog(stored) -> shiny.tag` (modal); `merge_api_key_submission(stored, username, password, freshwater_key) -> list(algaebase_username, algaebase_password, freshwaterecology_key)`; `write_api_keys_json(keys_list, path) -> invisible(path)`.

- [ ] **Step 1: Write the failing test file**

Create `tests/testthat/test-plugin-api-keys.R` with exactly this content:

```r
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
```

- [ ] **Step 2: Run it and verify it fails**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/test-plugin-api-keys.R')"
```

Expected (verified on a scratch copy of the pre-fix module): `[ FAIL 7 | ... | SKIP 1 | PASS 8 ]` on Windows - the blank submission stored `""` for the password and the freshwater key (JSON and `API_KEYS`, 4 failures), the typed-password test sees the freshwater key overwritten with `""` (1 failure), and `api_key_modal_dialog` / `merge_api_key_submission` do not exist (2 errors). The skip is the 0600 test (Windows).

- [ ] **Step 3: Add the helpers to the top of `R/modules/plugin_server.R`**

Insert this block at the very top of the file, above `#' Plugin Management Server Module` (followed by one blank line):

```r
# =============================================================================
# API KEY FORM HELPERS (F6)
# =============================================================================
# At file scope so they are unit-testable without a session.

#' The "API Key Configuration" modal
#'
#' Secrets are never sent to the browser: both secret fields are password
#' inputs that start empty, and leaving one blank keeps the stored value
#' (see merge_api_key_submission()). Only the username is pre-filled.
#'
#' @param stored API_KEYS environment (or a list) holding the current keys.
api_key_modal_dialog <- function(stored) {
  keep <- "(unchanged - leave blank to keep)"
  modalDialog(
    title = "API Key Configuration", size = "m",
    textInput("api_key_algaebase_user", "AlgaeBase Username:",
              value = stored$algaebase_username %||% ""),
    passwordInput("api_key_algaebase_pass", "AlgaeBase Password:", value = "",
                  placeholder = keep),
    hr(),
    passwordInput("api_key_freshwater", "freshwaterecology.info API Key:", value = "",
                  placeholder = keep),
    tags$p(class = "text-muted",
           "Keys saved to config/api_keys.json (gitignored). AlgaeBase: register at algaebase.org."),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("save_api_keys", "Save Keys", class = "btn-primary", icon = icon("save"))
    )
  )
}

#' Merge an API-key form submission into the stored keys
#'
#' @param stored API_KEYS environment (or a list).
#' @param username Submitted username; written as given (blank clears it).
#' @param password,freshwater_key Submitted secrets; NULL, NA, empty or
#'   whitespace-only keeps the stored value.
#' @return list(algaebase_username, algaebase_password, freshwaterecology_key).
merge_api_key_submission <- function(stored, username, password, freshwater_key) {
  keep_if_blank <- function(new, old) {
    if (is.null(new) || length(new) != 1 || is.na(new) || !nzchar(trimws(new))) old %||% "" else new
  }
  list(
    algaebase_username = username %||% "",
    algaebase_password = keep_if_blank(password, stored$algaebase_password),
    freshwaterecology_key = keep_if_blank(freshwater_key, stored$freshwaterecology_key)
  )
}

#' Write the API keys JSON atomically with owner-only permissions
#'
#' Writes a sibling tmp file, chmods it 0600, then renames it over `path`
#' (copy + remove if the rename fails), so a crash never leaves a truncated
#' key file and the secrets are never world-readable. chmod is a no-op on
#' Windows.
#' @param keys_list Named list of keys.
#' @param path Destination (API_KEYS_JSON).
write_api_keys_json <- function(keys_list, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(path, ".tmp.", Sys.getpid())
  jsonlite::write_json(keys_list, tmp, auto_unbox = TRUE, pretty = TRUE)
  Sys.chmod(tmp, mode = "0600")
  if (!isTRUE(suppressWarnings(file.rename(tmp, path)))) {
    ok <- file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
    if (!isTRUE(ok)) stop("could not write ", path, call. = FALSE)
  }
  Sys.chmod(path, mode = "0600")
  invisible(path)
}

```

- [ ] **Step 4: Build the modal from the helper**

Replace:

```r

  show_api_key_modal <- function() {
    showModal(modalDialog(
      title = "API Key Configuration", size = "m",
      textInput("api_key_algaebase_user", "AlgaeBase Username:",
                value = if (exists("API_KEYS")) API_KEYS$algaebase_username %||% "" else ""),
      passwordInput("api_key_algaebase_pass", "AlgaeBase Password:", value = ""),
      hr(),
      textInput("api_key_freshwater", "freshwaterecology.info API Key:",
                value = if (exists("API_KEYS")) API_KEYS$freshwaterecology_key %||% "" else ""),
      tags$p(class = "text-muted",
             "Keys saved to config/api_keys.json (gitignored). AlgaeBase: register at algaebase.org."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_api_keys", "Save Keys", class = "btn-primary", icon = icon("save"))
      )
    ))
  }
```

with:

```r
  show_api_key_modal <- function() {
    showModal(api_key_modal_dialog(if (exists("API_KEYS")) API_KEYS else list()))
  }
```

- [ ] **Step 5: Merge the submission and write atomically**

Inside `observeEvent(input$save_api_keys, { ... })`, below the `jsonlite` check, replace:

```r
    # Use JSON format to avoid R code injection via source()
    keys_list <- list(
      algaebase_username = input$api_key_algaebase_user,
      algaebase_password = input$api_key_algaebase_pass,
      freshwaterecology_key = input$api_key_freshwater
    )
    jsonlite::write_json(keys_list, API_KEYS_JSON, auto_unbox = TRUE, pretty = TRUE)

    # Remove old vulnerable .R format if it exists
    old_file <- API_KEYS_FILE
    if (file.exists(old_file)) {
      file.remove(old_file)
      message("Removed legacy config/api_keys.R (replaced by config/api_keys.json)")
    }

    # Update in-memory API_KEYS. The env is a process-wide reference type,
    # so direct $<- mutates in place; <<- was unsafe because it could touch
    # whichever frame happened to bind the symbol first.
    if (exists("API_KEYS", envir = .GlobalEnv) && is.environment(API_KEYS)) {
      API_KEYS$algaebase_username    <- input$api_key_algaebase_user
      API_KEYS$algaebase_password    <- input$api_key_algaebase_pass
      API_KEYS$freshwaterecology_key <- input$api_key_freshwater
    }
```

with:

```r
    # Use JSON format to avoid R code injection via source(). F6: a blank
    # secret field means "keep the stored value" - the modal never pre-fills
    # secrets, so an untouched field must not erase them.
    stored <- if (exists("API_KEYS", envir = .GlobalEnv) && is.environment(API_KEYS)) API_KEYS else list()
    keys_list <- merge_api_key_submission(
      stored,
      username = input$api_key_algaebase_user,
      password = input$api_key_algaebase_pass,
      freshwater_key = input$api_key_freshwater
    )
    write_api_keys_json(keys_list, API_KEYS_JSON)

    # Remove old vulnerable .R format if it exists
    old_file <- API_KEYS_FILE
    if (file.exists(old_file)) {
      file.remove(old_file)
      message("Removed legacy config/api_keys.R (replaced by config/api_keys.json)")
    }

    # Update in-memory API_KEYS, only the fields that changed. The env is a
    # process-wide reference type, so direct [[<- mutates in place; <<- was
    # unsafe because it could touch whichever frame bound the symbol first.
    if (is.environment(stored)) {
      for (field in names(keys_list)) {
        if (!identical(stored[[field]], keys_list[[field]])) stored[[field]] <- keys_list[[field]]
      }
    }
```

`removeModal()` and the success notification below stay.

- [ ] **Step 6: Parse-check and run the tests**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='R/modules/plugin_server.R'); parse(file='tests/testthat/test-plugin-api-keys.R'); cat('OK\n')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/test-plugin-api-keys.R'); testthat::test_file('tests/testthat/test-admin-auth.R')"
```

Expected: `OK`; `test-plugin-api-keys.R` `[ FAIL 0 | ... | SKIP 1 | PASS 32 ]` on Windows (on Linux: SKIP 0, PASS 33); `test-admin-auth.R` `FAIL 0`. As in Task 1, the 0600 test was never executed by the plan author (Windows skip); check its result on Linux CI.

- [ ] **Step 7: Commit**

```bash
git add R/modules/plugin_server.R tests/testthat/test-plugin-api-keys.R
git commit -m "$(cat <<'EOF'
fix(plugins): keep stored API secrets when the modal field is left blank (F6)

The password field is never pre-filled, so saving the modal without
retyping it overwrote the stored AlgaeBase password with "". The
freshwater key was also shipped to the browser in a textInput. Both
secrets are now empty password inputs where blank means "keep"; the JSON is
written tmp-then-rename with mode 0600, and API_KEYS is updated only for
fields that changed.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 7: Convention note, full suite, version 1.5.3, CHANGELOG, PR

**Files:**
- Modify: `CONTRIBUTING.md`, `VERSION`, `R/config.R` (`load_version_info()` fallback), `app.R` (header comment), `README.md` (3 version strings), `CHANGELOG.md` (regenerated)

**Interfaces:**
- Consumes: all task commits (the CHANGELOG generator reads git history).
- Produces: `VERSION=1.5.3` (PATCH+1 of what Task 0 recorded), `## [1.5.3] - <date>` at the CHANGELOG head with every earlier hand-written release note still in place. Task 8 greps for these.

- [ ] **Step 1: Add the convention to `CONTRIBUTING.md`**

In the "Trait-pipeline & concurrency patterns" section, insert a new bullet directly after the paragraph that ends with:

```
  `config/api_keys.json` and `.Renviron` are both gitignored. Check
  before committing if you touch that area.
```

(if B1 added its own bullet right after it, put this one after B1's):

```
- **Third-party and uploaded text reaches the page only through tag
  builders.** EcoBase metadata, `.ewemdb` metadata, upload file names and
  error messages go into `tags$td(x)`, `tags$p(x)` etc., which escape
  them - never into `HTML(paste0(...))`. Links go through `safe_href()`
  (http/https only) or `safe_doi_href()` (rebuilt on https://doi.org/),
  both in `R/functions/validation_utils.R`. `tests/testthat/test-xss-escaping.R`
  fails if `ecobase_server.R` or `ecopath_import_server.R` pastes anything
  dynamic into `HTML()`.

- **The offline trait DB is rebuilt only under the rebuild lock.** The
  in-app button needs `admin_authorized_strict()` (so nothing happens
  while `ECONETOOL_ADMIN_PASSWORD_HASH` is unset); the lock is the
  directory `cache/offline_traits.db.lock`, shared with console runs of
  `scripts/initialization/build_offline_trait_db.R`, and a build writes
  `offline_traits.db.tmp.<pid>` and renames it over the live DB only when
  it has finished. Use `request_offline_rebuild()` /
  `acquire_rebuild_lock()` from `R/functions/offline_db_rebuild.R`; do not
  delete or write the live DB directly.
```

- [ ] **Step 2: Run the full suite**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "r <- as.data.frame(testthat::test_dir('tests/testthat', reporter = 'summary', stop_on_failure = FALSE)); cat('AFTER pass', sum(r\$passed), 'fail', sum(r\$failed), 'skip', sum(r\$skipped), 'error', sum(r\$error), '\n')"
```

Expected: `fail 0 error 0`; `pass` = BASELINE + 177 on Windows (66 + 23 + 56 + 32; on Linux 180); `skip` = BASELINE + 2 on Windows (the two `skip_on_os("windows")` permission tests; + 0 on Linux). Verified on a scratch copy of master @ 595e517 plus the B1 stand-in: baseline `pass 1518 fail 0 skip 40`, after `pass 1694 fail 0 skip 42` with a revision of `test-xss-escaping.R` that had one expectation fewer (55; the final file has 56), i.e. +177 with the final files. The scratch copy has no untracked `data/`, so its absolute numbers sit below the real repo's (~1524 / 38 on master); compare deltas, not absolutes.

- [ ] **Step 3: Check the tags the CHANGELOG generator needs (STOP if missing)**

`scripts/generate_changelog.R` rebuilds the whole file from `v*` tags and labels every commit since the newest tag as `--version`. If `v1.5.1` / `v1.5.2` were never tagged, B2's and B1's sections would be merged into `[1.5.3]` and their headings would disappear.

```bash
git tag -l "v1.5.*"
grep -n -m3 "^## \[" CHANGELOG.md
```

Expected: `v1.5.0`, `v1.5.1`, `v1.5.2` all listed, and the CHANGELOG head is `## [1.5.2] - ...`. If a tag for a version that has a CHANGELOG section is missing, **STOP - ask the user** whether to tag it retroactively (as was done for `v1.4.4` in `c11da14`) on its release commit. Do not tag on your own.

- [ ] **Step 4: Bump the version strings**

Use PATCH+1 of the value recorded in Task 0 (1.5.3 if it was 1.5.2) everywhere below, and today's date (`2026-MM-DD` of the release day) for every date.

`VERSION`: set these lines (leave `STATUS`, `MAJOR`, `MINOR`, the git and deploy lines as they are):
```
VERSION=1.5.3
VERSION_NAME=Security and Admin Gating
RELEASE_DATE=<release date>
```
and
```
PATCH=3
```

`R/config.R`, inside `load_version_info()`, the fallback `version_info <- list(...)`: set `VERSION = "1.5.3"`, `VERSION_NAME = "Security and Admin Gating"`, `RELEASE_DATE = "<release date>"`, `PATCH = 3` (keep `STATUS = "stable"`, `MAJOR = 1`, `MINOR = 5`).

`app.R`: the header line `# CURRENT VERSION: v1.5.2 (...)` becomes `# CURRENT VERSION: v1.5.3 (<release date>)`.

`README.md`: `  version = {1.5.2},` -> `  version = {1.5.3},`; `**Current Version**: 1.5.2` -> `**Current Version**: 1.5.3`; `**Last Updated**: ...` -> `**Last Updated**: <release date>`; `<!-- VERSION:1.5.2 -->` -> `<!-- VERSION:1.5.3 -->`.

```bash
grep -n "^VERSION=\|^VERSION_NAME=\|^RELEASE_DATE=\|^PATCH=" VERSION
grep -n 'VERSION = "\|VERSION_NAME = "\|RELEASE_DATE = "\|PATCH = ' R/config.R | head -4
grep -n "CURRENT VERSION" app.R
grep -n "1\.5\.[0-9]" README.md
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='R/config.R'); parse(file='app.R'); cat('OK\n')"
```

Expected: every line shows 1.5.3 / the release date / `PATCH=3` / `PATCH = 3`; README shows `1.5.3` three times and no `1.5.2`; `OK`.

- [ ] **Step 5: Regenerate the CHANGELOG and re-insert the hand-written release notes**

The generator drops hand-written text; `docs/releases/<version>-*.md` is the canonical copy (today: `docs/releases/1.5.0-results-changed.md`, which belongs directly under `## [1.5.0] - ...`). This helper is a throwaway, not a repo file: save it outside the repo with the Write tool, e.g. as `<scratchpad>/reinsert_release_notes.R`:

```r
# Re-insert hand-written release notes (docs/releases/<version>-*.md) under
# their "## [<version>]" headings after scripts/generate_changelog.R has
# regenerated CHANGELOG.md from git history (which drops them).
x <- readLines("CHANGELOG.md", warn = FALSE)
notes <- list.files("docs/releases", pattern = "^[0-9]+\\.[0-9]+\\.[0-9]+-.+\\.md$", full.names = TRUE)
for (f in notes) {
  ver <- sub("-.*$", "", basename(f))
  h <- grep(paste0("^## \\[", gsub(".", "\\.", ver, fixed = TRUE), "\\] - "), x)
  if (length(h) != 1) stop("expected one '## [", ver, "]' heading, found ", length(h))
  first <- readLines(f, n = 1, warn = FALSE)
  if (any(x[seq(h, min(h + 3, length(x)))] == first)) next  # already present
  x <- append(x, c(readLines(f, warn = FALSE), ""), after = h + 1)
  cat("re-inserted", basename(f), "under [", ver, "]\n")
}
while (length(x) > 0 && x[length(x)] == "") x <- x[-length(x)]  # one trailing newline (end-of-file-fixer)
con <- file("CHANGELOG.md", "wb")
writeLines(x, con, sep = "\n")
close(con)
```

Then:

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/generate_changelog.R --version 1.5.3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" "<scratchpad>/reinsert_release_notes.R"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" "<scratchpad>/reinsert_release_notes.R"   # second run must print nothing
sed -i 's/\r$//' CHANGELOG.md
git diff CHANGELOG.md | grep '^-' | grep -v '^---'
grep -n -m2 "^## \[" CHANGELOG.md
grep -n "^### Results changed" CHANGELOG.md
```

Expected: the first reinsert run prints `re-inserted 1.5.0-results-changed.md under [ 1.5.0 ]` (and one line per any other `docs/releases/*.md` a later PR added); the second prints nothing (idempotent). The `-` line list is empty or contains only `[1.5.2]: .../compare/v1.5.1...HEAD`-style footer links that now point at a tag (verified on a clone at 595e517: regenerate + re-insert reproduced the committed CHANGELOG exactly, apart from a new `### Maintenance` line for the release commit under the previous version and the footer links). The head is `## [1.5.3] - <date>` then the previous version; `### Results changed` appears under `## [1.5.0]`. If any other `-` line appears (lost text), run `git checkout -- CHANGELOG.md` and instead paste the output of `scripts/generate_changelog.R --preview --version 1.5.3` above the current head section by hand.

- [ ] **Step 6: Commit**

```bash
git add CONTRIBUTING.md VERSION R/config.R app.R README.md CHANGELOG.md
git commit -m "$(cat <<'EOF'
chore(release): 1.5.3 - security and admin gating (B3)

Version 1.5.3 in VERSION, the R/config.R fallback, the app.R header and
README. CHANGELOG regenerated with the hand-written 1.5.0 release notes
re-inserted. CONTRIBUTING documents the tag-builder rule for third-party
text and the offline-DB rebuild lock.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

- [ ] **Step 7: Push and open the PR (STOP - ask the user before running)**

```bash
git push -u origin fix/b3-security-admin
gh pr create --base master --title "fix(security): B3 admin-gated offline DB rebuild, escaped metadata, API-key modal (F19, F3, F54, F6), v1.5.3" --body "$(cat <<'EOF'
Implements spec B section 4.3 (Phase B3).

- **F19** Rebuild Database requires `admin_authorized_strict()` (refused while `ECONETOOL_ADMIN_PASSWORD_HASH` is unset), takes a process-wide token-owned lock (`cache/offline_traits.db.lock`, shared with console builds), and the build script writes `offline_traits.db.tmp.<pid>` and renames it over the live DB only when finished. A build that stop()s leaves the live DB byte-identical.
- **F3 / F54** EcoBase metadata, `.ewemdb` metadata, upload file names and errors render through htmltools tag builders; hrefs are http(s) only, DOIs are rebuilt on https://doi.org/. `solution_html` (static) is unchanged.
- **F6** The API-key modal never pre-fills secrets; a blank secret field keeps the stored value; the JSON is written tmp-then-rename with mode 0600.
- New tests: `test-rebuild-lock.R`, `test-rebuild-observer.R`, `test-xss-escaping.R`, `test-plugin-api-keys.R` (each failed on the pre-fix code; see the plan for counts).

Deviations from the spec (new `offline_db_rebuild.R` instead of `cache_sqlite.R`, token hand-over, finalizer instead of top-level `on.exit`, log files instead of pipes) are listed with reasons in `docs/superpowers/plans/2026-09-27-b3-security-admin-gating.md`.

After deploy the rebuild button stays refused on laguna until `ECONETOOL_ADMIN_PASSWORD_HASH` is set in `/srv/shiny-server/EcoNeTool/.Renviron` (by design).

After merge, consider tagging `v1.5.3`.

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

Expected: PR URL printed; CI green (B2 added the offline testthat job, so the new tests run there too; on Linux the two permission tests run instead of skipping - check that they pass).

**Ask the user how to merge (STOP).** PRs #5-#8 were squash-merged, and a squash replaces the branch SHAs that the in-branch `[1.5.3]` CHANGELOG section cites. Options: merge this PR with a merge commit (as B0 #4 was), or squash it and cut the release (Task 7 Steps 3-6) as a follow-up PR on master (as 1.5.0 #8 was). Do not merge without the user's choice.

---

### Task 8: Deploy 1.5.3 to laguna.ku.lt and set the admin hash

Every step here touches production or the shared server. **Each is marked STOP: show the user the exact command and wait for explicit confirmation.** Deploy from the merged `master`.

**Files:** none in the repo. Remote: `/home/razinka/EcoNeTool_staging/`, `/srv/shiny-server/EcoNeTool/`.

**Interfaces:**
- Consumes: Task 3 (`offline_db_rebuild.R` sourced from `app.R`), Task 7 (`VERSION=1.5.3`).
- Produces: production on 1.5.3; with the user's hash in place, an admin can rebuild the offline DB.

- [ ] **Step 1: Pre-deploy check (local, read-only)**

```bash
git checkout master && git pull --ff-only
git log -1 --oneline
cd deployment && "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" pre-deploy-check.R; cd ..
```

Expected: the B3 merge is HEAD; the check reports no errors (it must run from inside `deployment/`).

- [ ] **Step 2: Upload to staging (STOP - ask the user before running)**

Use the B2-hardened scripts. Clear staging first (the overview's shared rule), then upload code only:

```bash
ssh razinka@laguna.ku.lt "rm -rf /home/razinka/EcoNeTool_staging && echo STAGING_CLEARED"
powershell ./deploy-windows.ps1 -SkipData -NoSudo
ssh razinka@laguna.ku.lt "ls /home/razinka/EcoNeTool_staging/config/ 2>/dev/null"
```

Expected: `STAGING_CLEARED`; the upload completes; the staging `config/` listing contains no `api_keys.R`, `api_keys.json` or `harmonization_custom.json` (B2 excludes them). If any of those three is listed, remove it from staging before Step 3 (`rm -f /home/razinka/EcoNeTool_staging/config/{api_keys.R,api_keys.json,harmonization_custom.json}`), and tell the user B2's exclusion did not hold. Ignore any `sudo` / `rm -rf /srv/shiny-server/EcoNeTool/*` suggestion the script prints.

- [ ] **Step 3: Copy over the live tree and reload (STOP - ask the user before running)**

```bash
ssh razinka@laguna.ku.lt "cp -rT /home/razinka/EcoNeTool_staging /srv/shiny-server/EcoNeTool && touch /srv/shiny-server/EcoNeTool/restart.txt"
```

Expected: silent success. `cp -rT` copies contents without deleting siblings, so live `data/`, `cache/` and `.Renviron` survive.

- [ ] **Step 4: Verify (read-only; still show the user before running)**

```bash
ssh razinka@laguna.ku.lt "grep -c 'offline_db_rebuild.R' /srv/shiny-server/EcoNeTool/app.R; grep -c 'request_offline_rebuild' /srv/shiny-server/EcoNeTool/R/modules/trait_research_server.R; grep -c 'ecobase_metadata_panel' /srv/shiny-server/EcoNeTool/R/modules/ecobase_server.R; grep -c 'merge_api_key_submission' /srv/shiny-server/EcoNeTool/R/modules/plugin_server.R; grep '^VERSION=' /srv/shiny-server/EcoNeTool/VERSION; stat -c %y /srv/shiny-server/EcoNeTool/restart.txt; ls /srv/shiny-server/EcoNeTool/data | head -3; ls -d /srv/shiny-server/EcoNeTool/cache/offline_traits.db.lock 2>&1; grep -c ECONETOOL_ADMIN_PASSWORD_HASH /srv/shiny-server/EcoNeTool/.Renviron"
curl -sL -o /dev/null -w '%{http_code}\n' http://laguna.ku.lt/EcoNeTool/
```

Expected: `1` (app.R sources the helper once), then non-zero counts for the three module symbols; `VERSION=1.5.3`; a fresh `restart.txt` mtime; a non-empty `data/` listing; `No such file or directory` for the lock; the `.Renviron` count (`0` today: the hash is not set on laguna as of 2026-09-27); `200` (`-L` follows the 301 to https).

- [ ] **Step 5: USER step - set the admin password hash (not automated)**

While `ECONETOOL_ADMIN_PASSWORD_HASH` is unset (the Step 4 count was `0`), the Rebuild Database button refuses by design ("Admin gate not configured on this instance; ..."). Ask the user to do this themselves; never generate, see or store the password:

1. Locally, in R from the repo root: `source("R/functions/admin_auth.R"); set_admin_password("<a long passphrase>")` - it prints one line `ECONETOOL_ADMIN_PASSWORD_HASH=econetool1$12$...` and writes nothing.
2. Append that line to `/srv/shiny-server/EcoNeTool/.Renviron` (the file exists; razinka can write the app directory).
3. `touch /srv/shiny-server/EcoNeTool/restart.txt`.

If B1's deploy already did this, skip it (Step 4's count is then `1`).

- [ ] **Step 6: Production smoke test (user or browser automation, with the user's go-ahead)**

On `https://laguna.ku.lt/EcoNeTool/`, Trait Research tab:
1. Without unlocking, click **Rebuild Database**: an error notification ("Unlock via Trait Research > Configure API Keys first, then rebuild the database", or the "Admin gate not configured" text if Step 5 was skipped); nothing is built.
2. Click **Configure API Keys**, unlock with the admin password, close the modal (Cancel), click **Rebuild Database**: "Rebuilding offline trait database..." then, about a minute later, "Offline database rebuilt! Total species in database: N". Afterwards `ssh razinka@laguna.ku.lt "ls -la /srv/shiny-server/EcoNeTool/cache/ | grep offline_traits"` shows `offline_traits.db` (mode `-rw-rw-r--`) and no `.lock` / `.tmp.` entries.
3. Open **Configure API Keys** again: both secret fields are empty password boxes with "(unchanged - leave blank to keep)"; the page source does not contain the stored freshwater key.

---

## Self-Review

1. **Spec coverage.** 4.3 F19: gate via `admin_authorized_strict(session$userData$admin_unlocked)` with B1's two refusal conditions -> Task 1 (`request_offline_rebuild`) + Task 3; lock directory `app_path("cache/offline_traits.db.lock")` via atomic `dir.create()`, owner file with pid + timestamp, 60-minute stale reclaim with `warning()`, "Rebuild already running (started HH:MM)" -> Task 1; exit observer releases on success/failure/handler error, `onSessionEnded` releases only if owned and dead -> Task 3; build into `db_path.tmp.<pid>`, script acquires the lock itself, `dbDisconnect` before rename, chmod 0664, rename with copy fallback, guarded disconnect, live DB untouched on `stop()` -> Tasks 1-2 (deviations 1-5 explain the differences). F3/F54: `safe_href`, `fmt` escaping, tag-built panels, DOI regex + `URLencode`, `publication_uri` via `safe_href`, `ecobase_server` `e$message` and `ecopath_import_server` error in `tags$pre`, model name / filename / description / author / contact / institution as text, `solution_html` untouched -> Tasks 4-5. F6: both secrets `passwordInput(value = "")` with placeholder `"(unchanged - leave blank to keep)"`, keys built from stored values with blank/whitespace keeping them, username written as given, mutate only changed fields, tmp-then-rename + `Sys.chmod(API_KEYS_JSON, "0600")` -> Task 6. Section 5 rows: lock (second acquire, stale 2 h, release) and atomic build (pre-existing DB byte-identical after `stop()`) -> Tasks 1-2; XSS EcoBase `&lt;img`, preview `javascript:` and `<script>` error, DOI `10.1000/x' onmouseover='a` -> Tasks 4-5; plugin keep/replace -> Task 6. Section 6: PATCH+1, CHANGELOG, CONTRIBUTING line, B1/B3 prerequisite (hash) -> Tasks 7-8. Section 8 items 2 (rebuild half; B1 owns the save half), 8, 9, 10, 11 -> Tasks 1-7.
2. **Placeholder scan.** Every code step carries complete code, byte-identical to the scratch files that were run. `<release date>` and `<scratchpad>` are values the executor fills at run time (the date of the release; their own scratch directory), not missing design.
3. **Type consistency.** `acquire_rebuild_lock()` / `release_rebuild_lock()` / `request_offline_rebuild()` / `finalize_offline_db_build()` / `offline_db_lock_path()` / `REBUILD_LOCK_TOKEN_ENV` are defined in Task 1 and used with the same signatures in Tasks 2-3; `meta_*`, `safe_href`, `safe_doi_href` are defined in Task 4 and used in Task 5; `api_key_modal_dialog` / `merge_api_key_submission` / `write_api_keys_json` are defined and used in Task 6.
4. **Review Focus.** Each of the five lines names the test that pins it (Tasks 1, 3, 4, 5).
