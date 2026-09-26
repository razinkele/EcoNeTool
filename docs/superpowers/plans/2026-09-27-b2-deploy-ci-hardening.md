# B2: Deploy and CI Hardening Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make every deploy path keep the server's own state (dotfiles, `data/`, `cache/`, `r-libs/`, `config/` runtime files, `models/`), never upload runtime config, write backups outside the served tree, make the pre-deploy gate and CI parse all of `R/`, run the offline testthat suite in CI, and gate the layer2c live calls. Ship it as v1.5.1 and deploy it with the new scripts.

**Architecture:** The three deploy scripts are not executed by the suite, so they are pinned by source guards that read **code lines only**: a new `tests/testthat/helper-deploy.R` strips comments and joins `\` continuations before any assertion, which fixes F86 (the old guard passed because a *comment* said `.Renviron`). Each script task adds its guard block to `test-deploy-preserve.R` (red), then fixes the script (green). The pre-deploy syntax check becomes a function `collect_r_syntax_errors()` that the test evaluates on its own. CI changes are tested by running each workflow's own parse script (read with `yaml`) against a scratch tree. F84 (live gating) lands before F82 (the CI job), so the new CI job never depends on five upstream APIs.

**Tech Stack:** R 4.4.1, testthat 3.3.2, withr, yaml (installed locally), bash (Git Bash on Windows), PowerShell 7 (`pwsh` 7.6 locally; preinstalled on GitHub `ubuntu-latest`), GitHub Actions (`r-lib/actions/setup-r@v2`, `setup-r-dependencies@v2`). Deploy target laguna.ku.lt (shiny-server, no rsync, razinka has no passwordless sudo).

**Spec:** `docs/superpowers/specs/2026-09-26-fix-b-platform-safety-design.md` section 4.2 (Phase B2: F86, F84, F81, F4, F5, F7, F83, F82), section 5 (B2 rows), section 6 (rollout, "B2 manual server steps"), section 7 (CI runtime risk), section 8 items 5, 6, 7, 11; plus `docs/superpowers/specs/2026-09-26-fix-overview.md` (merge order row 5, shared rules). Server facts below were verified read-only on 2026-09-27 and override the spec where they differ.

> **Execution rulings (controller, 2026-09-27) — these override the tasks below where they conflict:**
> 1. **Release flow:** the fix PR is squash-merged WITHOUT the version bump / CHANGELOG regeneration. The plan's version/CHANGELOG task then runs as a separate release PR cut from the updated master: bump to 1.5.1 (VERSION, `R/config.R` fallback, app.R header, README via `scripts/version_bump.R`), normalise CRLF to LF, set `GIT_BRANCH=master`, regenerate CHANGELOG, re-insert `docs/releases/1.5.0-results-changed.md` under `[1.5.0]`, strip the extra trailing newline, merge it, then `git tag -a v1.5.1` on the merge commit and push the tag. This keeps CHANGELOG hashes valid under squash merges and guarantees the tag the next plan expects exists.
> 2. **Locked-session refusal text** is `Unlock via Trait Research > Configure API Keys first` (the unlock button lives on the Trait Research tab).
> 3. Deploys use the deploy scripts as they exist on master at deploy time (after B2 merges, the hardened scripts); every outward step still STOPs for the user.

## Global Constraints

- Version for this PR: **1.5.1** (overview: "B2: deploy and CI hardening | patch"; merge order B2 -> B1 -> B3). `VERSION`, `R/config.R` fallback, `app.R` header and README markers all read 1.5.1 after Task 10.
- Order inside the PR (spec 4.2): "Tests come first, and the CI job comes last so it does not flake on F84."
- F81: "`PRESERVE_ITEMS=("r-libs" "cache" "restart.txt" "data" "config" "models")`" ... "Replace the per-directory `rsync -av --exclude ...` with `cp -rT "$SRC" "$DEST/$ITEM"`" ... "Drop the `*.csv` exclude" ... "Remove `data` and `config` from `CRITICAL_ITEMS`".
- F4: live tree `$preserve = "! -name '.*' ! -name data ! -name cache ! -name r-libs ! -name models ! -name config"`; "For `-NoSudo` ... set `$preserve = ""` and wipe staging entirely"; "Add `"models/"` to `$DEPLOY_ITEMS`".
- F5: "Add `config/api_keys.json`, `config/api_keys.R` and `config/harmonization_custom.json` to `EXCLUDE_PATTERNS` in `deploy.sh` and `deploy-windows.ps1`"; "The ps1 `Test-ShouldExclude` must match on relative path". This plan also excludes `.Renviron` (task brief).
- F7: "`BACKUP_DIR=/srv/shiny-server-data/EcoNeTool/backups` in all three scripts ...; `-NoSudo` keeps `/home/$User/backups`. Backups are `tar -czf` plus `chmod 600` everywhere; the ps1 `cp -r` backup ... is removed." Reference conf gets `directory_index off;`.
- Controller ruling (2026-09-27, added after the spec): deploy scripts must **never** overwrite `/etc/shiny-server/shiny-server.conf` - laguna is a shared server hosting ~30 other apps (BowTie, osmose, marxan, ...). Where a script needs to tell the user about conf changes, it prints the `location /EcoNeTool` snippet from the repo conf and asks for a manual, reviewed edit.
- F83: parse "`app.R`, `run_app.R` and `list.files("R", "\\.R$", recursive = TRUE)` minus `safeBackup`"; failures use "`print_check(..., "ERROR")`, which already leads to `quit(status = 1)`".
- F82: both workflows "parse root `*.R` plus `R/` recursively"; job `testthat-offline` "leaves `RUN_LIVE_TESTS` unset, runs `testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)` with a 20 min timeout, and is added to `ci-status` needs".
- F84: delete the local `skip_if_offline`; `skip_if_no_live_tests()` on every network-reaching test; "Wrap live calls in `with_timeout(..., timeout = 15)`"; argument-validation tests stay ungated; EMODnet gated only when `Btrait` is installed.
- Server facts (verified 2026-09-27): razinka has **no passwordless sudo**, so every sudo step is a **USER runs this** instruction, never automated. `/srv/shiny-server/backups` does **not** exist. A stray `/srv/shiny-server/EcoNeTool.bak.20260510_192058` (razinka:shiny, 2775, full old tree incl. `config/api_keys.R` and `data/`) sits inside site_dir. The live `/etc/shiny-server/shiny-server.conf` `location /` block has `directory_index on;` (around line 216). `/srv/shiny-server-data/EcoNeTool` exists (feedback DB). `http://laguna.ku.lt/EcoNeTool/` 301-redirects to https: use `curl -L`. Production `data/` is ~3.1 GB and must never be deleted. rsync is not installed on laguna.
- Error handlers use `warning()`, not `message()`; mutating outer scope from an error closure uses `<<-`. Tests: never `if (cond) expect_*()`; use `skip_if()` with an actionable reason.
- lintr: 120-char lines, `<-`, no tabs, no trailing whitespace. Parse-check every edited `.R` file: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='<path>'); cat('OK\n')"`. `bash -n` every edited `.sh`; PowerShell parse every edited `.ps1` (command in Task 4).
- Test commands: single file `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/<file>')"`; full suite `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_dir('tests/testthat')"` (a few minutes; do not run other heavy R jobs in parallel - 16 GB RAM). `tests/run_all_tests.R` needs `dggridR` and cannot run locally.
- Branch `fix/b2-deploy-ci` from `master` (HEAD `595e517`, v1.5.0). Every commit message ends with `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`. `git add` explicit paths only; never stage the untracked `WBGIFSV5ISSUE70.pdf`.
- Deploy and server steps are outward-facing: each is marked **STOP - ask the user** and must not run without the user's explicit go-ahead for that step. sudo steps are for the user to run.

## Review Focus

- **`-NoSudo -DryRun` must delete nothing.** The new staging wipe goes through `Invoke-RemoteCommand`, whose `$DryRun` branch only logs. Pinned by Task 4 test "deploy-windows.ps1 empties staging on every upload path, and only staging" (asserts the `rm -rf` is an `Invoke-RemoteCommand "..."` call).
- **Non-default `-User` (or an empty one).** The staging-path guard must accept `/home/alice/EcoNeTool_staging` and refuse `/srv/shiny-server/EcoNeTool`, a subdirectory, and `/home//EcoNeTool_staging`. Pinned by the same Task 4 test (regex cases).
- **A `#` inside a quoted string** (for example `Write-Host "   # cp -rT ..."`) is cut by `code_lines()` like a comment. Expected: it can only hide text from a guard, never create a "keep". Pinned by the Task 1 helper self-test (last expectation).
- **Live-tree backup on the non-NoSudo ps1 path vs the 60 s `Invoke-RemoteCommand` limit.** A tar of the 3.1 GB `data/` would time out and abort the deploy. Expected: backups exclude `data/`. Pinned by Task 4 test "deploy-windows.ps1 backups are tar archives outside site_dir" (`--exclude=$appLeaf/data`).
- **The offline suite on Linux.** `testthat-offline` has never run on `ubuntu-latest`; a Windows-only assumption or a missing package turns CI red. Not pinnable locally; Task 11 Step 3 watches the first run and STOPs on any red instead of merging.

Known and deliberately left out (not B2 scope): CLAUDE.md's "Deploy target ... via rsync+ssh" line is stale; `deployment/deploy.sh` and `deployment/force-reload.sh` still restart the whole shiny-server (`systemctl stop/start`) rather than touching `restart.txt`. (The `/etc/shiny-server/shiny-server.conf` overwrite is now fixed by Task 6.)

---

## File Structure

| File | Change | Responsibility |
|---|---|---|
| `tests/testthat/helper-deploy.R` | Create (Task 1) | `code_lines()`, `script_array()`, `protected_*()`, `backup_dirs()`, `DEPLOY_PROTECTED`, `RUNTIME_CONFIG_FILES` |
| `tests/testthat/test-deploy-preserve.R` | Rewrite (Task 1), append (Tasks 3, 4, 5, 6) | Comment-proof guards for all three deploy scripts |
| `tests/testthat/test-trait-lookup-unit.R` | Modify (Task 1) | `if (...) expect_*` -> `skip_if()` (F86 second half) |
| `tests/testthat/test-layer2c-api-databases.R` | Rewrite (Task 2) | Live gating + `with_timeout()` (F84) |
| `tests/testthat/test-live-gating-guard.R` | Create (Task 2) | Source guard for F84 |
| `deployment/deploy.sh` | Modify (Task 3) | F81, F5, F7 |
| `deployment/shiny-server.conf` | Modify (Task 3) | Reference copy: `directory_index off;` |
| `deploy-windows.ps1` | Modify (Task 4) | F4, F5, F7, staging wipe on every upload path |
| `deploy.sh` | Modify (Task 5) | F5, F7 (excludes and guards only; spec non-goal: making it work without rsync) |
| `deployment/deploy.sh` | Modify (Task 6) | Never write `/etc/shiny-server/`; print the `location /EcoNeTool` block instead |
| `deployment/pre-deploy-check.R` | Modify (Task 7) | F83 |
| `tests/testthat/test-pre-deploy-check.R` | Create (Task 7) | F83 |
| `.github/workflows/ci.yml` | Modify (Task 8) | Recursive parse + `testthat-offline` job + ci-status |
| `.github/workflows/r-check.yml` | Modify (Task 8) | Recursive parse |
| `tests/testthat/test-ci-workflows.R` | Create (Task 8) | Runs the workflows' parse scripts against scratch trees |
| `CONTRIBUTING.md` | Modify (Task 9) | Convention line for the code-line guard helper; CI note on live tests |
| `VERSION`, `app.R`, `README.md`, `R/config.R`, `CHANGELOG.md` | Modify (Task 10) | 1.5.1 |

## Verified while writing this plan (2026-09-27, scratch copies, not the repo)

- `test-deploy-preserve.R` (final form, all blocks): **26 failures + 1 error on the current scripts, 0 after** the Task 3-5 edits. Removing `! -name '.*'` (or `".*"` for `deploy.sh`) from any script turns its block red (spec acceptance 5).
- Task 6 (shared-conf guard), on scratch copies with Tasks 1-5 applied: 2 failures before (the three `/etc/shiny-server/` writes in `deployment/deploy.sh` - backup `cp`, `cp` of the repo conf, `cat >` heredoc - and the missing snippet print), 0 after; the new `sed -n` range prints exactly the repo conf's `location /EcoNeTool { ... }` block. Only `deployment/deploy.sh` writes there today (`deploy.sh`, `deploy-windows.ps1`, `deployment/force-reload.sh`, `deployment/verify-deployment.sh`, `create_regional_files.sh` do not); the guard covers every `*.sh`/`*.ps1` at the root and in `deployment/`.
- `bash -n` passes on both edited `.sh`; `[Parser]::ParseFile` reports 0 errors on the edited `.ps1`.
- The `deployment/deploy.sh` wipe + copy was replayed on a scratch tree: `.Renviron`, `data/x.csv`, `config/api_keys.json` survived; stale top-level files went; `config/` received only `api_keys.R.template`; `models/` was copied.
- `test-live-gating-guard.R`: 25 failures on the current layer2c file, 0 on the rewrite. Rewritten layer2c offline: 18 tests, 0 failed, **11 skipped (was 1)**, under 1 s (was 20 s of HTTP); with `RUN_LIVE_TESTS=true`: 0 failed.
- `test-trait-lookup-unit.R` after the `skip_if` rewrite: 26 tests, 0 failed, 0 skipped (fixtures have `success = TRUE`, `isMarine`, `strategy`, `max_length_cm`, `trophic_level`).
- `test-pre-deploy-check.R`: errors on the current script (function missing); passes after. The edited script run against the real repo: `✓ Syntax: 91 R files parse`, exit 0.
- `test-ci-workflows.R`: on the current workflows both parse steps **exit 0 with a broken `R/functions/trait_lookup/broken.R`** (proves F82) and the job checks fail; all pass after Task 8.

---

### Task 0: Branch, plan, baseline

**Files:** none modified.

**Interfaces:**
- Consumes: nothing.
- Produces: branch `fix/b2-deploy-ci`; recorded baseline PASS / FAIL / SKIP counts used in Tasks 9 and 11.

- [ ] **Step 1: Branch from master**

```bash
cd "/c/Users/arturas.baziukas/OneDrive - ku.lt/HORIZON_EUROPE/MARBEFES/Traits/Networks/EcoNeTool"
git checkout master && git pull
git checkout -b fix/b2-deploy-ci
git log --oneline -1
```
Expected: `595e517 chore(release): 1.5.0 ...` (or a later master commit).

- [ ] **Step 2: Commit this plan on the branch**

`docs/superpowers/plans/` is gitignored (`.gitignore:98`); earlier plans reached master through a docs merge. This plan is force-added as the branch's first commit so it travels with the PR.

```bash
git add -f docs/superpowers/plans/2026-09-27-b2-deploy-ci-hardening.md
git commit -m "docs(plan): B2 deploy and CI hardening

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

- [ ] **Step 3: Record the baseline**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "r <- as.data.frame(testthat::test_dir('tests/testthat', reporter = 'silent', stop_on_failure = FALSE)); cat('PASS', sum(r\$nb) - sum(r\$failed), 'FAIL', sum(r\$failed), 'SKIP', sum(r\$skipped), 'ERR', sum(r\$error), '\n')"
```
Expected: about `FAIL 0`, `SKIP 38`, `ERR 0` (master had ~1524 pass / 0 fail / 38 skip). Write the numbers down.

---

### Task 1: F86 - comment-proof guard helper, test rewrite, `skip_if` in the unit tests

**Files:**
- Create: `tests/testthat/helper-deploy.R`
- Rewrite: `tests/testthat/test-deploy-preserve.R`
- Modify: `tests/testthat/test-trait-lookup-unit.R` (tests at lines 53-69, 95-152, 294-322)

**Interfaces:**
- Consumes: `get_app_root()` from `tests/testthat/helper-fixtures.R`.
- Produces (used by Tasks 3-6, all in `helper-deploy.R`):
  - `code_lines(path) -> character` (comment-stripped, continuation-joined, non-blank lines)
  - `script_array(lines, name) -> character` (quoted items of `NAME=(...)` or `$NAME = @(...)`; `stop()`s unless defined exactly once)
  - `protected_deployment_sh(path)`, `protected_windows_ps1(path)`, `protected_root_sh(path) -> character` (names kept by each script)
  - `backup_dirs(path) -> character` (every `BACKUP_DIR` value, `SHINY_SERVER_ROOT` / `APP_NAME` expanded)
  - `DEPLOY_PROTECTED <- c(".*", "data", "cache", "r-libs", "models", "config")`
  - `RUNTIME_CONFIG_FILES <- c("config/api_keys.R", "config/api_keys.json", "config/harmonization_custom.json", ".Renviron")`
  - In `test-deploy-preserve.R`: `deploy_file(rel) -> character` (absolute path under the app root).

- [ ] **Step 1: Rewrite `tests/testthat/test-deploy-preserve.R` (tests first)**

Replace the whole file with:

```r
# =============================================================================
# Deploy scripts must not destroy server-only state (F81, F4, F5, F7, F86)
# =============================================================================
# Three scripts can deploy EcoNeTool: deploy.sh (rsync, unusable against
# laguna, which has no rsync), deployment/deploy.sh (run as root on the
# server) and deploy-windows.ps1 (the path actually used, with -NoSudo).
# The suite does not execute them, so these are source guards over
# code_lines() (helper-deploy.R): comments are stripped first, so a comment
# mentioning `.Renviron` can no longer make a guard pass.

deploy_file <- function(rel) file.path(get_app_root(), rel)

# --- helper self-test --------------------------------------------------------

test_that("code_lines() ignores comments, so a commented-out keep does not count", {
  fixture <- tempfile(fileext = ".sh")
  on.exit(unlink(fixture), add = TRUE)
  writeLines(c(
    "#!/bin/bash",
    "# PRESERVE_ITEMS=(\"r-libs\" \"cache\" \"data\" \"config\" \"models\")",
    "# find /srv/shiny-server/EcoNeTool -mindepth 1 ! -name '.*' -exec rm -rf {} +",
    "PRESERVE_ITEMS=(\"r-libs\" \"cache\")  # keep .Renviron data config models too",
    "find /srv/shiny-server/EcoNeTool -mindepth 1 -maxdepth 1 \"${FIND_KEEP[@]}\" -exec rm -rf {} +",
    "echo \"${#PRESERVE_ITEMS[@]} kept\"",
    "cp -rT \"$SRC\" \\",
    "      \"$DEST/$ITEM\"",
    "echo \"   # shown to the user\""
  ), fixture)

  # The pre-F86 guard grepped the raw text and would have passed:
  raw <- paste(readLines(fixture), collapse = "\n")
  expect_true(grepl("-name '.*'", raw, fixed = TRUE))

  keep <- protected_deployment_sh(fixture)
  expect_setequal(keep, c("r-libs", "cache"))
  expect_false(".*" %in% keep)
  expect_false(all(DEPLOY_PROTECTED %in% keep))

  code <- code_lines(fixture)
  # `${#arr[@]}` is not a comment
  expect_true(any(grepl("${#PRESERVE_ITEMS[@]}", code, fixed = TRUE)))
  # continuation lines are joined into one logical line
  expect_true(any(grepl("cp -rT \"\\$SRC\"\\s+\"\\$DEST/\\$ITEM\"", code)))
  # Known limitation: a " #" inside a quoted string is cut like a comment.
  # That can only hide text from a guard, never invent a keep.
  expect_true("echo \"" %in% code)
})

# --- all three scripts -------------------------------------------------------

test_that("no deploy script wipes the live tree with rm -rf .../EcoNeTool/*", {
  for (rel in c("deploy.sh", "deployment/deploy.sh", "deploy-windows.ps1")) {
    offenders <- grep("rm\\s+-rf\\s+\\S*EcoNeTool/\\*", code_lines(deploy_file(rel)), value = TRUE)
    expect_equal(length(offenders), 0L, info = paste(rel, ":", paste(offenders, collapse = " | ")))
  }
})
```

(The old four tests only covered `deployment/deploy.sh` with raw greps; Task 3 replaces them with stricter code-line versions. Until Task 3, `r-libs`/`cache`/dotfiles/`models` for that script are guarded only by the wipe test above - acceptable for two commits.)

- [ ] **Step 2: Run it to verify it fails**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-deploy-preserve.R')"`
Expected: the first test errors with `could not find function "protected_deployment_sh"` (the helper does not exist yet); the wipe test errors with `could not find function "code_lines"`.

- [ ] **Step 3: Create `tests/testthat/helper-deploy.R`**

```r
# =============================================================================
# Test helpers: deploy-script guards (F86)
# =============================================================================
# The deploy scripts (deploy.sh, deployment/deploy.sh, deploy-windows.ps1) are
# not executed by the suite, so their tests are source guards. A plain grep
# over the whole file also matches comments, and a guard that passes because
# a *comment* mentions `.Renviron` protects nothing. Every guard therefore
# reads the scripts through code_lines(), which drops comments first.

# Code lines of a sh or ps1 script: full-line `#` comments and trailing
# ` # ...` comments removed, blank lines dropped. A `#` counts as a comment
# only at line start or after whitespace, so `${#arr[@]}` survives. sh
# continuation lines (ending in `\`) are joined into one logical line. The
# scripts use no `<# #>` block comments; heredoc bodies are kept as text.
code_lines <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- lines[!grepl("^\\s*#", lines)]
  lines <- sub("\\s+#.*$", "", lines)
  lines <- lines[nzchar(trimws(lines))]
  out <- character(0)
  buf <- ""
  for (ln in lines) {
    if (grepl("\\\\\\s*$", ln)) {
      buf <- paste0(buf, sub("\\\\\\s*$", " ", ln))
    } else {
      out <- c(out, paste0(buf, ln))
      buf <- ""
    }
  }
  if (nzchar(buf)) out <- c(out, buf)
  out
}

# Quoted items of a sh array `NAME=( ... )` or a ps1 array `$NAME = @( ... )`,
# single- or multi-line, read from code lines only.
script_array <- function(lines, name) {
  start <- grep(sprintf("^\\s*\\$?%s\\s*=\\s*@?\\(", name), lines)
  if (length(start) != 1L) {
    stop(sprintf("array %s: expected 1 definition, found %d", name, length(start)))
  }
  end <- start
  while (!grepl("\\)\\s*$", lines[end])) {
    end <- end + 1L
  }
  body <- paste(lines[start:end], collapse = "\n")
  items <- regmatches(body, gregexpr('"[^"]*"', body))[[1]]
  gsub('"', "", items, fixed = TRUE)
}

# Top-level names each deploy path must never delete or overwrite from the
# local tree: server-only state (.Renviron, data/ ~3.1 GB, cache/ with
# offline_traits.db, r-libs/ with icesSAG, config/ runtime files) plus the
# tracked models/ that a wipe would otherwise lose.
DEPLOY_PROTECTED <- c(".*", "data", "cache", "r-libs", "models", "config")

# Runtime config that exists only on the server (or only on a dev box) and
# must never be uploaded.
RUNTIME_CONFIG_FILES <- c("config/api_keys.R", "config/api_keys.json",
                          "config/harmonization_custom.json", ".Renviron")

# deployment/deploy.sh: names kept by the find-based wipe.
protected_deployment_sh <- function(path) {
  code <- code_lines(path)
  keep <- script_array(code, "PRESERVE_ITEMS")
  find_line <- grep("find /srv/shiny-server/EcoNeTool .*-exec rm -rf", code, value = TRUE)
  if (length(find_line) == 1L && grepl("! -name '.*'", find_line, fixed = TRUE)) {
    keep <- c(keep, ".*")
  }
  keep
}

# deploy-windows.ps1: names kept by the find-based wipe of the LIVE tree
# (the non-empty `$preserve = "..."`; staging uses `$preserve = ""`).
protected_windows_ps1 <- function(path) {
  code <- code_lines(path)
  line <- grep('^\\s*\\$preserve\\s*=\\s*"!', code, value = TRUE)
  if (length(line) != 1L) {
    return(character(0))
  }
  names <- regmatches(line, gregexpr("-name\\s+'?[^'\"[:space:]]+'?", line))[[1]]
  gsub("^-name\\s+|'", "", names)
}

# deploy.sh (rsync --delete): excluded paths are neither sent nor deleted on
# the receiver. Normalise "/data/", "cache/*", "r-libs" to bare names.
protected_root_sh <- function(path) {
  pats <- script_array(code_lines(path), "EXCLUDE_PATTERNS")
  unique(sub("/\\*?$", "", sub("^/", "", pats)))
}

# Right-hand sides of every BACKUP_DIR assignment (sh and ps1), with the
# shiny-server root and app-name variables expanded.
backup_dirs <- function(path) {
  code <- code_lines(path)
  rhs <- sub('^\\s*\\$?BACKUP_DIR\\s*=\\s*"([^"]*)".*$', "\\1",
             grep('^\\s*\\$?BACKUP_DIR\\s*=\\s*"', code, value = TRUE))
  rhs <- gsub("\\$\\{?SHINY_SERVER_ROOT\\}?", "/srv/shiny-server", rhs)
  gsub("\\$\\{?APP_NAME\\}?", "EcoNeTool", rhs)
}
```

- [ ] **Step 4: Run it to verify it passes**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-deploy-preserve.R')"`
Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 10 ]`.

- [ ] **Step 5: Replace the if-gated expectations in `tests/testthat/test-trait-lookup-unit.R`**

These tests are green today, so there is no red step; the change makes a bad fixture visible as a skip with a reason instead of an "empty test". Replace each named `test_that` block (match on the whole block) with the version below.

`"WoRMS returns marine flag for marine species"` (currently lines 53-60):
```r
test_that("WoRMS returns marine flag for marine species", {
  result <- load_fixture("worms_gadus_morhua")

  skip_if(!isTRUE(result$success) || is.null(result$traits$isMarine),
          "worms_gadus_morhua fixture has no isMarine flag; refresh with capture_fixtures.R")
  expect_true(result$traits$isMarine %in% c(TRUE, 1),
              info = "Gadus morhua should be flagged as marine")
})
```

`"WoRMS lookup uses multiple strategies"` (lines 62-69):
```r
test_that("WoRMS lookup uses multiple strategies", {
  result <- load_fixture("worms_gadus_morhua")

  skip_if(is.null(result$strategy),
          "worms_gadus_morhua fixture has no strategy field; refresh with capture_fixtures.R")
  expect_true(is.character(result$strategy))
})
```

`"FishBase returns traits for Atlantic cod"` (lines 95-115):
```r
test_that("FishBase returns traits for Atlantic cod", {
  result <- load_fixture("fishbase_gadus_morhua")

  skip_if(!isTRUE(result$success),
          "fishbase_gadus_morhua fixture has success=FALSE; refresh with capture_fixtures.R")
  traits <- result$traits
  skip_if(is.null(traits$max_length_cm) || is.null(traits$trophic_level),
          "fishbase_gadus_morhua fixture lacks max_length_cm/trophic_level; refresh with capture_fixtures.R")

  # Cod should have reasonable size data
  expect_gt(traits$max_length_cm, 50,
            label = "Cod max length should be > 50 cm")
  expect_lt(traits$max_length_cm, 250,
            label = "Cod max length should be < 250 cm")

  # Cod trophic level should be around 4
  expect_gt(traits$trophic_level, 3.0)
  expect_lt(traits$trophic_level, 5.0)
})
```

`"FishBase returns traits for herring"` (lines 117-137):
```r
test_that("FishBase returns traits for herring", {
  result <- load_fixture("fishbase_clupea_harengus")

  expect_valid_fishbase_result(result)

  skip_if(!isTRUE(result$success),
          "fishbase_clupea_harengus fixture has success=FALSE; refresh with capture_fixtures.R")
  traits <- result$traits
  skip_if(is.null(traits$max_length_cm) || is.null(traits$trophic_level),
          "fishbase_clupea_harengus fixture lacks max_length_cm/trophic_level; refresh with capture_fixtures.R")

  # Herring should be smaller than cod
  expect_gt(traits$max_length_cm, 15)
  expect_lt(traits$max_length_cm, 60)

  # Herring trophic level should be lower (planktivore)
  expect_gt(traits$trophic_level, 2.5)
  expect_lt(traits$trophic_level, 4.5)
})
```

`"FishBase trait list has expected fields"` (lines 139-152):
```r
test_that("FishBase trait list has expected fields", {
  result <- load_fixture("fishbase_gadus_morhua")

  skip_if(!isTRUE(result$success),
          "fishbase_gadus_morhua fixture has success=FALSE; refresh with capture_fixtures.R")
  trait_names <- names(result$traits)

  # These are the key fields FishBase should provide
  expected_fields <- c("max_length_cm", "trophic_level")
  for (field in expected_fields) {
    expect_true(field %in% trait_names,
                info = paste("FishBase should return", field))
  }
})
```

`"fish species routes to FishBase, not SeaLifeBase"` (lines 294-311; same anti-pattern, not in the spec's line list - see Self-review):
```r
test_that("fish species routes to FishBase, not SeaLifeBase", {
  worms_cod <- load_fixture("worms_gadus_morhua")

  # Verify the routing logic: Chordata + Actinopterygii -> FishBase
  skip_if(!isTRUE(worms_cod$success),
          "worms_gadus_morhua fixture has success=FALSE; refresh with capture_fixtures.R")
  phylum <- tolower(worms_cod$traits$phylum)
  class <- tolower(worms_cod$traits$class)

  expect_equal(phylum, "chordata")
  fish_classes <- c("actinopterygii", "actinopteri", "elasmobranchii",
                    "holocephali", "myxini", "petromyzonti",
                    "teleostei", "chondrichthyes", "osteichthyes")
  expect_true(class %in% fish_classes,
              info = paste("Class", class, "should trigger FishBase routing"))
})
```

`"mollusc species routes to SeaLifeBase, not FishBase"` (lines 313-322):
```r
test_that("mollusc species routes to SeaLifeBase, not FishBase", {
  worms_mussel <- load_fixture("worms_mytilus_edulis")

  skip_if(!isTRUE(worms_mussel$success),
          "worms_mytilus_edulis fixture has success=FALSE; refresh with capture_fixtures.R")
  phylum <- tolower(worms_mussel$traits$phylum)
  invertebrate_phyla <- c("mollusca", "arthropoda", "annelida", "echinodermata",
                          "cnidaria", "porifera")
  expect_true(phylum %in% invertebrate_phyla,
              info = paste("Phylum", phylum, "should trigger SeaLifeBase routing"))
})
```

The remaining `if (!is.na(result$MS))` style checks in "trait codes are valid format when present" and the `"source"`/`"confidence"` tests are per-field optional checks inside a test that already asserts other things; they stay (out of F86's list).

- [ ] **Step 6: Run the unit file**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-trait-lookup-unit.R')"`
Expected: `FAIL 0`, `SKIP 0` (same as before: the fixtures satisfy every precondition). Then:
```bash
grep -n "^  if (result\$success\|^  if (worms_\|^  if (!is.null(result\$strategy" tests/testthat/test-trait-lookup-unit.R
```
Expected: no output.

- [ ] **Step 7: Parse-check and commit**

```bash
for f in tests/testthat/helper-deploy.R tests/testthat/test-deploy-preserve.R tests/testthat/test-trait-lookup-unit.R; do "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='$f'); cat('OK\n')"; done
git add tests/testthat/helper-deploy.R tests/testthat/test-deploy-preserve.R tests/testthat/test-trait-lookup-unit.R
git commit -m "test(deploy): comment-proof deploy guards; skip_if in trait unit tests (F86)

code_lines() strips comments and joins continuations before any
deploy-script assertion; the old guard passed because a comment
mentioned .Renviron.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 2: F84 - gate layer2c live calls, bound them, guard it

**Files:**
- Create: `tests/testthat/test-live-gating-guard.R`
- Rewrite: `tests/testthat/test-layer2c-api-databases.R`

**Interfaces:**
- Consumes: `skip_if_no_live_tests()`, `skip_if_offline(host = "www.marinespecies.org")`, `get_app_root()` (helper-fixtures.R); `with_timeout(expr, timeout = 10, on_timeout = NULL, verbose = FALSE)` from `R/functions/validation_utils.R` (returns `on_timeout`, i.e. NULL, on a time-limit error).
- Produces: nothing used by later tasks. Task 8's CI job relies on this file making no HTTP when `RUN_LIVE_TESTS` is unset.

- [ ] **Step 1: Write the guard `tests/testthat/test-live-gating-guard.R`**

```r
# =============================================================================
# Source guard: network-reaching layer2c tests are live-gated (F84)
# =============================================================================
# test-layer2c-api-databases.R used to define its own skip_if_offline()
# (shadowing helper-fixtures.R) and ran real HTTP in "structure" tests with no
# RUN_LIVE_TESTS check, so the offline suite depended on five upstream APIs.
# This guard parses the file and fails if a test that calls a network lookup
# loses its gate or its timeout.

layer2c_path <- function() {
  file.path(get_app_root(), "tests", "testthat", "test-layer2c-api-databases.R")
}

# test_that() calls at top level: description -> body expression
layer2c_tests <- function() {
  exprs <- as.list(parse(layer2c_path(), keep.source = FALSE))
  calls <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("test_that")), exprs)
  stats::setNames(lapply(calls, function(e) e[[3]]),
                  vapply(calls, function(e) as.character(e[[2]]), character(1)))
}

NETWORK_LOOKUPS <- c("lookup_worms_traits_api", "lookup_polytraits", "lookup_emodnet_traits",
                     "lookup_obis_traits", "lookup_traitbank")

# Tests that call a lookup but return before any I/O (argument validation,
# or EMODnet without Btrait). Keep this list short and exact.
UNGATED_OK <- c(
  "lookup_worms_traits_api returns correct structure with NULL aphia_id",
  "lookup_worms_traits_api returns FALSE for non-positive aphia_id",
  "lookup_worms_traits_api returns FALSE for non-numeric aphia_id",
  "lookup_emodnet_traits returns FALSE gracefully without Btrait"
)

test_that("layer2c does not redefine skip_if_offline()", {
  exprs <- as.list(parse(layer2c_path(), keep.source = FALSE))
  redefines <- vapply(exprs, function(e) {
    is.call(e) && as.character(e[[1]]) %in% c("<-", "=", "<<-") &&
      identical(e[[2]], as.name("skip_if_offline"))
  }, logical(1))
  expect_false(any(redefines))
})

test_that("every layer2c test that calls a network lookup is live-gated and time-bounded", {
  tests <- layer2c_tests()
  expect_true(all(UNGATED_OK %in% names(tests)),
              info = "UNGATED_OK names a test that no longer exists; update the list")

  calls_lookup <- vapply(tests, function(b) any(NETWORK_LOOKUPS %in% all.names(b)), logical(1))
  gated <- names(tests)[calls_lookup & !names(tests) %in% UNGATED_OK]
  expect_gte(length(gated), 12L)

  for (nm in gated) {
    used <- all.names(tests[[nm]])
    expect_true("skip_if_no_live_tests" %in% used, info = paste("no skip_if_no_live_tests():", nm))
    expect_true("with_timeout" %in% used, info = paste("no with_timeout():", nm))
  }
})
```

- [ ] **Step 2: Run it to verify it fails**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-live-gating-guard.R')"`
Expected: `FAIL 25` (1 for the local `skip_if_offline`, 24 missing gate/timeout expectations across 12 tests).

- [ ] **Step 3: Replace `tests/testthat/test-layer2c-api-databases.R` entirely**

Changes versus today: the local `skip_if_offline` (L14-21) is gone; every network-reaching test starts with `skip_if_no_live_tests()` and wraps the call in `with_timeout(..., timeout = LIVE_TIMEOUT)` (15 s) followed by `skip_if(is.null(res), ...)`; the three pre-existing live tests keep `skip_if_offline()` (now the helper's version) after the gate; the OBIS `if (...) skip(...)` and `if (res$success) expect_*` become `skip_if()` and an unconditional `expect_true()`; the Btrait `if` in "returns FALSE gracefully" becomes `skip_if()`.

```r
library(testthat)

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
source(file.path(app_root, "R/config.R"))
source(file.path(app_root, "R/functions/validation_utils.R"))
source(file.path(app_root, "R/functions/functional_group_utils.R"))
source(file.path(app_root, "R/config/harmonization_config.R"))
source(file.path(app_root, "R/functions/trait_lookup/harmonization.R"))
source(file.path(app_root, "R/functions/trait_lookup/api_trait_databases.R"))

# Every test that can reach the network is gated by skip_if_no_live_tests()
# (helper-fixtures.R) and bounds the call with with_timeout() (F84). The
# offline suite runs on every push/PR in CI; these run nightly with
# RUN_LIVE_TESTS=true. skip_if_offline() is the helper-fixtures.R version:
# a local redefinition used to shadow it. test-live-gating-guard.R enforces
# all of this.
LIVE_TIMEOUT <- 15

# =============================================================================
# 0. All 5 functions exist
# =============================================================================
test_that("all 5 API lookup functions are defined", {
  expect_true(exists("lookup_worms_traits_api"),  info = "lookup_worms_traits_api missing")
  expect_true(exists("lookup_polytraits"),         info = "lookup_polytraits missing")
  expect_true(exists("lookup_emodnet_traits"),     info = "lookup_emodnet_traits missing")
  expect_true(exists("lookup_obis_traits"),        info = "lookup_obis_traits missing")
  expect_true(exists("lookup_traitbank"),          info = "lookup_traitbank missing")
})

# =============================================================================
# 1. lookup_worms_traits_api - argument validation (returns before any HTTP)
# =============================================================================
test_that("lookup_worms_traits_api returns correct structure with NULL aphia_id", {
  res <- lookup_worms_traits_api(species_name = "Nereis diversicolor", aphia_id = NULL)
  expect_type(res, "list")
  expect_named(res, c("species", "source", "success", "traits"), ignore.order = TRUE)
  expect_equal(res$source,  "WoRMS_Traits")
  expect_false(res$success)
  expect_type(res$traits, "list")
})

test_that("lookup_worms_traits_api returns FALSE for non-positive aphia_id", {
  res <- lookup_worms_traits_api(species_name = "Test", aphia_id = -1)
  expect_false(res$success)
  res2 <- lookup_worms_traits_api(species_name = "Test", aphia_id = 0)
  expect_false(res2$success)
})

test_that("lookup_worms_traits_api returns FALSE for non-numeric aphia_id", {
  res <- lookup_worms_traits_api(species_name = "Test", aphia_id = "abc")
  expect_false(res$success)
})

test_that("lookup_worms_traits_api live lookup for AphiaID 126436 (Gadus morhua)", {
  skip_if_no_live_tests()
  skip_if_offline()
  skip_if_not_installed("worrms")
  res <- with_timeout(lookup_worms_traits_api(
    species_name = "Gadus morhua",
    aphia_id     = 126436,
    timeout      = 10
  ), timeout = LIVE_TIMEOUT)
  skip_if(is.null(res), "WoRMS did not answer within 15 s")
  expect_type(res, "list")
  expect_equal(res$source, "WoRMS_Traits")
  # Even if WoRMS returns no attributes, structure must be intact
  expect_type(res$traits,  "list")
  expect_type(res$success, "logical")
})

# =============================================================================
# 2. lookup_polytraits - structure (HTTP even for a nonexistent name)
# =============================================================================
test_that("lookup_polytraits returns correct structure", {
  skip_if_no_live_tests()
  res <- with_timeout(lookup_polytraits("XXXXXXNONEXISTENT_SPECIES_ZZZZ", timeout = 2),
                      timeout = LIVE_TIMEOUT)
  skip_if(is.null(res), "PolyTraits did not answer within 15 s")
  expect_type(res, "list")
  expect_named(res, c("species", "source", "success", "traits"), ignore.order = TRUE)
  expect_equal(res$source,  "PolyTraits")
  expect_false(res$success)
  expect_type(res$traits, "list")
})

test_that("lookup_polytraits species field matches input", {
  skip_if_no_live_tests()
  res <- with_timeout(lookup_polytraits("Hediste diversicolor", timeout = 1), timeout = LIVE_TIMEOUT)
  skip_if(is.null(res), "PolyTraits did not answer within 15 s")
  expect_equal(res$species, "Hediste diversicolor")
})

test_that("lookup_polytraits live lookup for a known polychaete", {
  skip_if_no_live_tests()
  skip_if_offline()
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
  res <- with_timeout(lookup_polytraits("Nereis diversicolor", timeout = 10), timeout = LIVE_TIMEOUT)
  skip_if(is.null(res), "PolyTraits did not answer within 15 s")
  expect_type(res, "list")
  expect_equal(res$source, "PolyTraits")
  expect_type(res$traits,  "list")
  expect_type(res$success, "logical")
})

# =============================================================================
# 3. lookup_emodnet_traits - structure
# =============================================================================
test_that("lookup_emodnet_traits returns correct structure (Btrait may be absent)", {
  # Without Btrait the function returns before any I/O. With it,
  # Btrait::getTrait() may fetch, so that case is a live test.
  if (requireNamespace("Btrait", quietly = TRUE)) skip_if_no_live_tests()
  res <- with_timeout(lookup_emodnet_traits("Abra alba"), timeout = LIVE_TIMEOUT)
  skip_if(is.null(res), "EMODnet did not answer within 15 s")
  expect_type(res, "list")
  expect_named(res, c("species", "source", "success", "traits"), ignore.order = TRUE)
  expect_equal(res$source,  "EMODnet")
  # Success depends on Btrait being installed; we only check shape
  expect_type(res$success, "logical")
  expect_type(res$traits,  "list")
})

test_that("lookup_emodnet_traits returns FALSE gracefully without Btrait", {
  skip_if(requireNamespace("Btrait", quietly = TRUE),
          "Btrait is installed - graceful-degradation test not applicable")
  res <- lookup_emodnet_traits("Abra alba")
  expect_false(res$success)
})

# =============================================================================
# 4. lookup_obis_traits - structure (HTTP even for a nonexistent name)
# =============================================================================
test_that("lookup_obis_traits returns correct structure", {
  skip_if_no_live_tests()
  res <- with_timeout(lookup_obis_traits("XXXXXXNONEXISTENT_SPECIES_ZZZZ", timeout = 5),
                      timeout = LIVE_TIMEOUT)
  skip_if(is.null(res), "OBIS did not answer within 15 s")
  expect_type(res, "list")
  expect_named(res, c("species", "source", "success", "traits"), ignore.order = TRUE)
  expect_equal(res$source,  "OBIS")
  expect_type(res$success, "logical")
  expect_type(res$traits,  "list")
})

test_that("lookup_obis_traits species field matches input", {
  skip_if_no_live_tests()
  res <- with_timeout(lookup_obis_traits("Fake species", timeout = 1), timeout = LIVE_TIMEOUT)
  skip_if(is.null(res), "OBIS did not answer within 15 s")
  expect_equal(res$species, "Fake species")
})

test_that("lookup_obis_traits live lookup for Abra alba", {
  skip_if_no_live_tests()
  skip_if_offline()
  skip_if_not_installed("robis")
  # OBIS API is very slow and can exceed R's C-level elapsed time limit.
  # Only run when ECONETOOL_TEST_OBIS_LIVE=true is set.
  skip_if(!identical(Sys.getenv("ECONETOOL_TEST_OBIS_LIVE"), "true"),
          "OBIS live test skipped by default (set ECONETOOL_TEST_OBIS_LIVE=true to enable)")
  res <- with_timeout(lookup_obis_traits("Abra alba", timeout = 10), timeout = LIVE_TIMEOUT)
  skip_if(is.null(res), "OBIS did not answer within 15 s")
  expect_type(res, "list")
  expect_equal(res$source, "OBIS")
  expect_type(res$traits,  "list")
  expect_type(res$success, "logical")
  expect_true(!isTRUE(res$success) || length(res$traits) > 0,
              info = "a successful OBIS lookup must carry traits")
})

# =============================================================================
# 5. lookup_traitbank - structure (HTTP even for a nonexistent name)
# =============================================================================
test_that("lookup_traitbank returns correct structure", {
  skip_if_no_live_tests()
  res <- with_timeout(lookup_traitbank("XXXXXXNONEXISTENT_SPECIES_ZZZZ", timeout = 2),
                      timeout = LIVE_TIMEOUT)
  skip_if(is.null(res), "TraitBank did not answer within 15 s")
  expect_type(res, "list")
  expect_named(res, c("species", "source", "success", "traits"), ignore.order = TRUE)
  expect_equal(res$source,  "TraitBank")
  expect_type(res$success, "logical")
  expect_type(res$traits,  "list")
})

test_that("lookup_traitbank species field matches input", {
  skip_if_no_live_tests()
  res <- with_timeout(lookup_traitbank("Fake species xyz", timeout = 1), timeout = LIVE_TIMEOUT)
  skip_if(is.null(res), "TraitBank did not answer within 15 s")
  expect_equal(res$species, "Fake species xyz")
})

test_that("lookup_traitbank live lookup for Abra alba", {
  skip_if_no_live_tests()
  skip_if_offline()
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
  res <- with_timeout(lookup_traitbank("Abra alba", timeout = 10), timeout = LIVE_TIMEOUT)
  skip_if(is.null(res), "TraitBank did not answer within 15 s")
  expect_type(res, "list")
  expect_equal(res$source, "TraitBank")
  expect_type(res$traits,  "list")
  expect_type(res$success, "logical")
})

# =============================================================================
# 6. Return-value invariants (all functions)
# =============================================================================
test_that("all functions always return the four required list fields", {
  skip_if_no_live_tests()
  required_fields <- c("species", "source", "success", "traits")

  results <- with_timeout(list(
    worms      = lookup_worms_traits_api("X", aphia_id = NULL),
    polytraits = lookup_polytraits("X", timeout = 1),
    emodnet    = lookup_emodnet_traits("X"),
    obis       = lookup_obis_traits("X", timeout = 1),
    traitbank  = lookup_traitbank("X", timeout = 1)
  ), timeout = LIVE_TIMEOUT)
  skip_if(is.null(results), "lookups did not answer within 15 s")

  for (nm in names(results)) {
    r <- results[[nm]]
    expect_named(r, required_fields, ignore.order = TRUE,
                 info = paste(nm, "missing required fields"))
    expect_true(is.logical(r$success),
                info = paste(nm, "$success should be logical"))
    expect_true(is.list(r$traits),
                info = paste(nm, "$traits should be a list"))
  }
})

test_that("orchestrator has API database routing flags", {
  orch_text <- readLines(file.path(app_root, "R/functions/trait_lookup/orchestrator.R"))
  orch_joined <- paste(orch_text, collapse = "\n")
  expect_true(grepl("query_worms_attrs", orch_joined), info = "Missing query_worms_attrs")
  expect_true(grepl("query_polytraits", orch_joined), info = "Missing query_polytraits")
  expect_true(grepl("query_emodnet", orch_joined), info = "Missing query_emodnet")
  expect_true(grepl("query_obis", orch_joined), info = "Missing query_obis")
  expect_true(grepl("query_traitbank", orch_joined), info = "Missing query_traitbank")
})
```

- [ ] **Step 4: Run guard, offline file and live file**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-live-gating-guard.R')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer2c-api-databases.R')"
RUN_LIVE_TESTS=true "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer2c-api-databases.R')"
```
Expected: guard `FAIL 0`. Offline layer2c `FAIL 0`, `SKIP 11` (was `SKIP 1`; +10 are the newly gated tests), finishes in about a second. Live run `FAIL 0` (a few skips for `skip_if_offline()` / missing `robis` / OBIS env var are normal).

- [ ] **Step 5: Parse-check and commit**

```bash
for f in tests/testthat/test-live-gating-guard.R tests/testthat/test-layer2c-api-databases.R; do "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='$f'); cat('OK\n')"; done
git add tests/testthat/test-live-gating-guard.R tests/testthat/test-layer2c-api-databases.R
git commit -m "test(layer2c): gate network calls behind RUN_LIVE_TESTS with 15 s timeouts (F84)

Drops the local skip_if_offline() that shadowed the helper. The offline
suite now makes no HTTP from this file; a parse-level guard keeps it so.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 3: F81 / F5 / F7 - `deployment/deploy.sh` and the reference conf

**Files:**
- Modify: `deployment/deploy.sh` (backup block ~L174-184, wipe block ~L186-205, `CRITICAL_ITEMS` ~L208-225, copy loop ~L230-243, after the loop, shiny-server.conf heredoc ~L349-351)
- Modify: `deployment/shiny-server.conf:26-28`
- Test: `tests/testthat/test-deploy-preserve.R` (append)

**Interfaces:**
- Consumes: `code_lines()`, `script_array()`, `protected_deployment_sh()`, `backup_dirs()`, `DEPLOY_PROTECTED` (Task 1), `deploy_file()` (test file).
- Produces: nothing new.

- [ ] **Step 1: Append the failing tests to `tests/testthat/test-deploy-preserve.R`**

```r

# --- deployment/deploy.sh (F81, F5, F7) --------------------------------------

test_that("deployment/deploy.sh keeps dotfiles, data, cache, r-libs, models and config", {
  keep <- protected_deployment_sh(deploy_file("deployment/deploy.sh"))
  missing <- setdiff(c(DEPLOY_PROTECTED, "restart.txt"), keep)
  expect_equal(missing, character(0), info = "not preserved by the find-based wipe")
})

test_that("deployment/deploy.sh never copies data/ or config/ over the live tree", {
  code <- code_lines(deploy_file("deployment/deploy.sh"))
  items <- script_array(code, "CRITICAL_ITEMS")
  expect_false("data" %in% items)
  expect_false("config" %in% items)
  expect_true("models" %in% items, info = "models/ is tracked and loaded by ml_trait_prediction.R")
  # only the template goes into the preserved config/
  expect_true(any(grepl("config/api_keys.R.template", code, fixed = TRUE)))
})

test_that("deployment/deploy.sh copies with cp -rT, not rsync, and keeps *.csv", {
  code <- code_lines(deploy_file("deployment/deploy.sh"))
  expect_false(any(grepl("\\brsync\\b", code)), info = "rsync is not installed on laguna")
  expect_true(any(grepl("cp -rT \"$SRC\" \"$DEST/$ITEM\"", code, fixed = TRUE)))
  expect_false(any(grepl("--exclude='*.csv'", code, fixed = TRUE)))
})

test_that("deployment/deploy.sh writes tar backups outside site_dir, mode 600", {
  path <- deploy_file("deployment/deploy.sh")
  dirs <- backup_dirs(path)
  expect_equal(dirs, "/srv/shiny-server-data/EcoNeTool/backups")
  code <- code_lines(path)
  expect_true(any(grepl("tar -czf", code, fixed = TRUE)))
  expect_true(any(grepl("chmod 600", code, fixed = TRUE)))
})

test_that("the reference shiny-server.conf has no directory index anywhere", {
  conf <- code_lines(deploy_file("deployment/shiny-server.conf"))
  expect_false(any(grepl("directory_index\\s+on", conf)))
  # the fallback heredoc in deployment/deploy.sh writes the same file
  expect_false(any(grepl("directory_index\\s+on", code_lines(deploy_file("deployment/deploy.sh")))))
})
```

- [ ] **Step 2: Run to verify it fails**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-deploy-preserve.R')"`
Expected: `FAIL 11` - keep list misses `data`/`config`/`models`; `data`/`config` in `CRITICAL_ITEMS`; no template copy; `rsync` present, no `cp -rT`, `*.csv` excluded; backup dir `/srv/shiny-server/backups/EcoNeTool`, no `chmod 600`; `directory_index on` in both files.

- [ ] **Step 3: Edit `deployment/deploy.sh`**

3a. Backup block. Replace:
```bash
    # Backup existing deployment before removing
    BACKUP_DIR="/srv/shiny-server/backups/EcoNeTool"
    TIMESTAMP=$(date +%Y%m%d_%H%M%S)
    mkdir -p "$BACKUP_DIR"
    if [ -f /srv/shiny-server/EcoNeTool/app.R ]; then
        print_status "Creating timestamped backup..."
        tar -czf "${BACKUP_DIR}/EcoNeTool_${TIMESTAMP}.tar.gz" -C /srv/shiny-server EcoNeTool
        print_success "Backup created: EcoNeTool_${TIMESTAMP}.tar.gz"
```
with:
```bash
    # Backup existing deployment before removing. The backup directory is
    # OUTSIDE site_dir (/srv/shiny-server): everything under site_dir is
    # served by shiny-server's `location /`, and a copy that contains app.R
    # and config/api_keys.R would even run as an app. Mode 600: the tar
    # holds the server's API keys.
    BACKUP_DIR="/srv/shiny-server-data/EcoNeTool/backups"
    TIMESTAMP=$(date +%Y%m%d_%H%M%S)
    mkdir -p "$BACKUP_DIR"
    chmod 700 "$BACKUP_DIR"
    if [ -f /srv/shiny-server/EcoNeTool/app.R ]; then
        print_status "Creating timestamped backup..."
        tar -czf "${BACKUP_DIR}/EcoNeTool_${TIMESTAMP}.tar.gz" -C /srv/shiny-server EcoNeTool
        chmod 600 "${BACKUP_DIR}/EcoNeTool_${TIMESTAMP}.tar.gz"
        print_success "Backup created: ${BACKUP_DIR}/EcoNeTool_${TIMESTAMP}.tar.gz"
```
(The two lines after it - `# Keep last 5 backups` and the `ls -t *.tar.gz ... | xargs -r rm` - stay.)

3b. Wipe block. Replace:
```bash
    # Dotfiles are preserved explicitly. The old glob never matched them, so a
    # find-based delete that removed .Renviron would be a regression
    # introduced by this very fix.
    print_status "Removing old deployment contents (preserving server state)..."
    PRESERVE_ITEMS=("r-libs" "cache" "restart.txt")
```
with:
```bash
    # Dotfiles are preserved explicitly. The old glob never matched them, so a
    # find-based delete that removed .Renviron would be a regression
    # introduced by this very fix.
    #
    # data/ (~3.1 GB, managed out of band) and config/ (runtime state:
    # api_keys.json, api_keys.R, harmonization_custom.json) are server-only
    # too: deleting them and copying the local tree back replaced production
    # data and keys with whatever the deploying checkout held (F81, F5).
    # models/ is kept so a failed copy below cannot leave the ML tier empty.
    print_status "Removing old deployment contents (preserving server state)..."
    PRESERVE_ITEMS=("r-libs" "cache" "restart.txt" "data" "config" "models")
```

3c. `CRITICAL_ITEMS`. Replace:
```bash
        "metawebs"
        "data"
        "config"
        # Tracked in git
```
with:
```bash
        "metawebs"
        # Tracked in git
```

3d. Copy loop. Replace:
```bash
        SRC="$APP_DIR/$ITEM"
        DEST="/srv/shiny-server/EcoNeTool/"
        if [ -e "$SRC" ]; then
            print_status "Copying $ITEM..."
            if [ -d "$SRC" ]; then
                # Exclude large/sensitive data files to match root deploy.sh
                rsync -av --exclude='*.zip' --exclude='*.csv' --exclude='*.ewemdb' \
                      --exclude='*.eweaccdb' --exclude='*.accdb' --exclude='*.xml' \
                      --exclude='*.doc' --exclude='.claude' \
                      "$SRC" "$DEST" 2>err.log
            else
                cp -vf "$SRC" "$DEST" 2>err.log
            fi
```
with:
```bash
        SRC="$APP_DIR/$ITEM"
        DEST="/srv/shiny-server/EcoNeTool"
        if [ -e "$SRC" ]; then
            print_status "Copying $ITEM..."
            if [ -d "$SRC" ]; then
                # cp -rT copies the directory CONTENTS into DEST/ITEM and
                # never deletes siblings. rsync is not installed on laguna,
                # so the old rsync call failed after the wipe above. No *.csv
                # exclude: metawebs/ ships as CSV.
                cp -rT "$SRC" "$DEST/$ITEM" 2>err.log
            else
                cp -vf "$SRC" "$DEST/" 2>err.log
            fi
```
(This also removes the false "match root deploy.sh" comment.)

3e. After the copy loop. Replace:
```bash
    # Summary of errors and warnings
    if [ ${#ERRORS[@]} -gt 0 ]; then
```
with:
```bash
    # config/ is preserved above. Ship only the key template into it, never
    # a local api_keys.R / api_keys.json / harmonization_custom.json (F5).
    mkdir -p /srv/shiny-server/EcoNeTool/config
    if cp -f "$APP_DIR/config/api_keys.R.template" /srv/shiny-server/EcoNeTool/config/; then
        print_success "Copied: config/api_keys.R.template"
    else
        ERRORS+=("config/api_keys.R.template: copy failed")
    fi

    # Summary of errors and warnings
    if [ ${#ERRORS[@]} -gt 0 ]; then
```

3f. Heredoc fallback conf. Replace:
```bash
    # When a user visits the base URL rather than a particular application,
    # an index of the applications available in this directory will be shown.
    directory_index on;
  }
```
with:
```bash
    # No directory index: it would list everything under site_dir.
    directory_index off;
  }
```

- [ ] **Step 4: Edit `deployment/shiny-server.conf`**

Replace:
```
    # When a user visits the base URL rather than a particular application,
    # an index of the applications available in this directory will be shown.
    directory_index on;
```
with:
```
    # No directory index: it would list everything under site_dir, including
    # any stray backup copy of an app. Reference copy only - the live
    # /etc/shiny-server/shiny-server.conf is changed by hand (sudo).
    directory_index off;
```

- [ ] **Step 5: Run the tests and syntax-check the script**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-deploy-preserve.R')"
bash -n deployment/deploy.sh && echo BASH_OK
```
Expected: `FAIL 0` for the Task 1 + Task 3 blocks; `BASH_OK`.

- [ ] **Step 6: Replay the wipe + copy on a scratch tree (no server involved)**

```bash
T="$(mktemp -d)"; mkdir -p "$T/live/data" "$T/live/config" "$T/live/cache" "$T/live/r-libs" "$T/live/R" "$T/src/R/functions" "$T/src/models" "$T/src/config"
echo s > "$T/live/.Renviron"; echo d > "$T/live/data/x.csv"; echo k > "$T/live/config/api_keys.json"; echo o > "$T/live/R/old.R"; echo x > "$T/live/stale.txt"
echo n > "$T/src/R/functions/new.R"; echo m > "$T/src/models/trait_ml_models.rds"; echo t > "$T/src/config/api_keys.R.template"; echo l > "$T/src/config/api_keys.R"
PRESERVE_ITEMS=("r-libs" "cache" "restart.txt" "data" "config" "models"); FIND_KEEP=(); for KEEP in "${PRESERVE_ITEMS[@]}"; do FIND_KEEP+=(! -name "$KEEP"); done
find "$T/live" -mindepth 1 -maxdepth 1 ! -name '.*' "${FIND_KEEP[@]}" -exec rm -rf {} +
for ITEM in R models; do cp -rT "$T/src/$ITEM" "$T/live/$ITEM"; done; mkdir -p "$T/live/config"; cp -f "$T/src/config/api_keys.R.template" "$T/live/config/"
(cd "$T/live" && find . -type f | sort); rm -rf "$T"
```
Expected exactly: `./.Renviron`, `./R/functions/new.R`, `./config/api_keys.R.template`, `./config/api_keys.json`, `./data/x.csv`, `./models/trait_ml_models.rds` (no `stale.txt`, no `R/old.R`, no local `api_keys.R`).

- [ ] **Step 7: Commit**

```bash
git add deployment/deploy.sh deployment/shiny-server.conf tests/testthat/test-deploy-preserve.R
git commit -m "fix(deploy): deployment/deploy.sh keeps data/config/models, cp -rT, backups outside site_dir (F81, F5, F7)

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 4: F4 / F5 / F7 - `deploy-windows.ps1`

**Files:**
- Modify: `deploy-windows.ps1` (L45-51 paths, L68-78 `$DEPLOY_ITEMS`, L81-113 `$EXCLUDE_PATTERNS`, `New-RemoteBackup` L321-348, `Deploy-Application` start L353-356, `Test-ShouldExclude` L368-381, extract block L482-490, NoSudo "NEXT STEPS" L692-695)
- Test: `tests/testthat/test-deploy-preserve.R` (append)

**Interfaces:**
- Consumes: `code_lines()`, `script_array()`, `protected_windows_ps1()`, `backup_dirs()`, `DEPLOY_PROTECTED`, `RUNTIME_CONFIG_FILES` (Task 1).
- Produces: the deploy behaviour Task 12 relies on: `-NoSudo` empties `/home/<User>/EcoNeTool_staging` before every upload; staging never contains `config/api_keys.R`, `config/api_keys.json`, `config/harmonization_custom.json`, `.Renviron`; `models/` is shipped; backups are `<BACKUP_DIR>/EcoNeTool_backup_<ts>.tar.gz`, mode 600.

- [ ] **Step 1: Append the failing tests to `tests/testthat/test-deploy-preserve.R`**

The pwsh test **runs** wherever `pwsh` is on PATH - locally (pwsh 7.6) and on GitHub `ubuntu-latest` (preinstalled). The Windows-style cases pass on Linux too: the new inner-slash branch normalises `\` to `/`, and `.Renviron` matches via the existing `*\$pattern` check.

```r

# --- deploy-windows.ps1 (F4, F5, F7) -----------------------------------------

test_that("deploy-windows.ps1 keeps dotfiles, data, cache, r-libs, models and config on the live tree", {
  keep <- protected_windows_ps1(deploy_file("deploy-windows.ps1"))
  expect_equal(setdiff(DEPLOY_PROTECTED, keep), character(0))
})

test_that("deploy-windows.ps1 -NoSudo wipes staging completely", {
  code <- code_lines(deploy_file("deploy-windows.ps1"))
  # staging holds no live state; anything left there is cp -rT'd over live
  expect_true(any(grepl('^\\s*\\$preserve\\s*=\\s*""\\s*$', code)))
})

test_that("deploy-windows.ps1 ships models/ and never uploads runtime config", {
  code <- code_lines(deploy_file("deploy-windows.ps1"))
  expect_true("models/" %in% script_array(code, "DEPLOY_ITEMS"))
  excl <- script_array(code, "EXCLUDE_PATTERNS")
  expect_equal(setdiff(RUNTIME_CONFIG_FILES, excl), character(0))
})

test_that("deploy-windows.ps1 backups are tar archives outside site_dir", {
  path <- deploy_file("deploy-windows.ps1")
  dirs <- backup_dirs(path)
  expect_setequal(dirs, c("/home/$User/backups", "/srv/shiny-server-data/EcoNeTool/backups"))
  code <- code_lines(path)
  expect_false(any(grepl("cp\\s+-r\\s", code)), info = "a cp -r backup is a live, runnable app copy")
  expect_true(any(grepl("tar -czf", code, fixed = TRUE)))
  expect_true(any(grepl("chmod 600", code, fixed = TRUE)))
  # data/ (~3.1 GB) would overrun Invoke-RemoteCommand's 60 s limit
  expect_true(any(grepl("tar --exclude=$appLeaf/data -czf", code, fixed = TRUE)))
})

test_that("deploy-windows.ps1 empties staging on every upload path, and only staging", {
  code <- code_lines(deploy_file("deploy-windows.ps1"))
  # Through Invoke-RemoteCommand, so -DryRun only logs it
  expect_true(any(grepl('Invoke-RemoteCommand "rm -rf $APP_DEPLOY_PATH && mkdir -p $APP_DEPLOY_PATH"',
                        code, fixed = TRUE)))

  guard <- grep("-notmatch '", code, value = TRUE, fixed = TRUE)
  expect_length(guard, 1L)
  rx <- sub("^.*-notmatch '([^']+)'.*$", "\\1", guard)
  expect_true(grepl(rx, "/home/razinka/EcoNeTool_staging", perl = TRUE))
  expect_true(grepl(rx, "/home/alice/EcoNeTool_staging", perl = TRUE), info = "-User alice")
  expect_false(grepl(rx, "/srv/shiny-server/EcoNeTool", perl = TRUE))
  expect_false(grepl(rx, "/home/razinka/EcoNeTool_staging/data", perl = TRUE))
  expect_false(grepl(rx, "/home//EcoNeTool_staging", perl = TRUE), info = "empty -User")
})

test_that("deploy-windows.ps1 Test-ShouldExclude drops runtime config by relative path only", {
  pwsh <- Sys.which("pwsh")
  skip_if(!nzchar(pwsh), "pwsh (PowerShell 7) not on PATH")
  q <- function(x) if (.Platform$OS.type == "windows") shQuote(x, type = "cmd") else shQuote(x)

  # Evaluate the script's own $EXCLUDE_PATTERNS and Test-ShouldExclude
  # (nested in Deploy-Application) without running the deploy.
  runner <- tempfile(fileext = ".ps1")
  on.exit(unlink(runner), add = TRUE)
  writeLines(c(
    "param([string]$Script, [string]$Paths)",
    "$ast = [System.Management.Automation.Language.Parser]::ParseFile($Script, [ref]$null, [ref]$null)",
    "$arr = $ast.Find({ param($n) $n -is [System.Management.Automation.Language.AssignmentStatementAst] -and",
    "  $n.Left.Extent.Text -eq '$EXCLUDE_PATTERNS' }, $true)",
    "$fn = $ast.Find({ param($n) $n -is [System.Management.Automation.Language.FunctionDefinitionAst] -and",
    "  $n.Name -eq 'Test-ShouldExclude' }, $true)",
    "Invoke-Expression $arr.Extent.Text",
    "Invoke-Expression $fn.Extent.Text",
    "foreach ($p in $Paths.Split('|')) { '{0}={1}' -f $p, (Test-ShouldExclude $p) }"
  ), runner)

  cases <- c(
    "C:\\repo\\config\\api_keys.R"                = "True",
    "C:\\repo\\config\\api_keys.json"             = "True",
    "C:\\repo\\config\\harmonization_custom.json" = "True",
    "/repo/config/api_keys.R"                     = "True",
    "C:\\repo\\config\\.Renviron"                 = "True",
    "C:\\repo\\config\\api_keys.R.template"       = "False",
    "C:\\repo\\R\\functions\\api_keys.R"          = "False",
    "C:\\repo\\models\\trait_ml_models.rds"       = "False",
    "C:\\repo\\R\\modules\\plugin_server.R"       = "False"
  )
  out <- system2(pwsh, c("-NoProfile", "-NonInteractive", "-File", q(runner),
                         "-Script", q(normalizePath(deploy_file("deploy-windows.ps1"))),
                         "-Paths", q(paste(names(cases), collapse = "|"))),
                 stdout = TRUE, stderr = TRUE)
  got <- sub("^.*=", "", out)
  names(got) <- sub("=[^=]*$", "", out)
  expect_equal(got[names(cases)], cases)
})
```

- [ ] **Step 2: Run to verify it fails**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-deploy-preserve.R')"`
Expected: the six ps1 tests fail or error (live `$preserve` lacks `.*`/`models`/`config`; no `$preserve = ""`; no `models/`; no runtime excludes; backup under `/srv/shiny-server/backups` via `cp -r`; no staging wipe - `expect_length(guard, 1L)` fails and the regex step errors; `Test-ShouldExclude` returns `False` for `config\api_keys.R`). The Task 1 and Task 3 blocks stay green.

- [ ] **Step 3: Edit `deploy-windows.ps1`**

4a. Paths. Replace:
```powershell
    $APP_DEPLOY_PATH = "$SHINY_SERVER_ROOT/$APP_NAME"
    $BACKUP_DIR = "$SHINY_SERVER_ROOT/backups"
}
```
with:
```powershell
    $APP_DEPLOY_PATH = "$SHINY_SERVER_ROOT/$APP_NAME"
    # Outside site_dir (/srv/shiny-server): everything under site_dir is
    # served by shiny-server, and a backup holds config/api_keys.* (F7).
    $BACKUP_DIR = "/srv/shiny-server-data/$APP_NAME/backups"
}
```

4b. `$DEPLOY_ITEMS`. Replace:
```powershell
    "data/",
    "config/"
)
```
with:
```powershell
    "data/",
    "config/",
    # Tracked in git and loaded at runtime by ml_trait_prediction.R.
    "models/"
)
```

4c. `$EXCLUDE_PATTERNS`. Replace:
```powershell
    "archive/",
    "data_conversion/"
)
```
with:
```powershell
    "archive/",
    "data_conversion/",
    # Runtime config: server-only state or a developer's local keys. Never
    # uploaded, so `cp -rT staging live` cannot overwrite the server's copy
    # (F5). Patterns with an inner '/' match one relative path.
    "config/api_keys.R",
    "config/api_keys.json",
    "config/harmonization_custom.json",
    ".Renviron"
)
```

4d. `New-RemoteBackup`. Replace:
```powershell
    $backupName = "${APP_NAME}_backup_$TIMESTAMP"
    $backupPath = "$BACKUP_DIR/$backupName"
    $sudoPrefix = if ($NoSudo) { "" } else { "sudo " }

    # Create backup directory
    Invoke-RemoteCommand "${sudoPrefix}mkdir -p $BACKUP_DIR"

    # Check if app exists
    $appExists = Invoke-RemoteCommand "test -d $APP_DEPLOY_PATH && echo 'yes' || echo 'no'" -IgnoreError

    if ($appExists.Trim() -eq "yes") {
        Invoke-RemoteCommand "${sudoPrefix}cp -r $APP_DEPLOY_PATH $backupPath"
        Write-Log "Backup created: $backupPath" "SUCCESS"

        # Keep only last 5 backups
        Invoke-RemoteCommand "cd $BACKUP_DIR && ls -t | tail -n +6 | xargs -r ${sudoPrefix}rm -rf" -IgnoreError
```
with:
```powershell
    $backupName = "${APP_NAME}_backup_$TIMESTAMP.tar.gz"
    $backupPath = "$BACKUP_DIR/$backupName"
    $sudoPrefix = if ($NoSudo) { "" } else { "sudo " }
    $appParent = $APP_DEPLOY_PATH -replace '/[^/]+$', ''
    $appLeaf = $APP_DEPLOY_PATH -replace '^.*/', ''

    # Create backup directory (private: backups hold config/api_keys.*)
    Invoke-RemoteCommand "${sudoPrefix}mkdir -p $BACKUP_DIR && ${sudoPrefix}chmod 700 $BACKUP_DIR"

    # Check if app exists
    $appExists = Invoke-RemoteCommand "test -d $APP_DEPLOY_PATH && echo 'yes' || echo 'no'" -IgnoreError

    if ($appExists.Trim() -eq "yes") {
        # A tar archive, not a `cp -r` copy: a copied tree is a runnable app
        # wherever it lands (F7). data/ (~3.1 GB) is left out: it is managed
        # out of band and would overrun Invoke-RemoteCommand's 60 s limit.
        Invoke-RemoteCommand "${sudoPrefix}tar --exclude=$appLeaf/data -czf $backupPath -C $appParent $appLeaf && ${sudoPrefix}chmod 600 $backupPath"
        Write-Log "Backup created: $backupPath" "SUCCESS"

        # Keep only last 5 backups
        Invoke-RemoteCommand "cd $BACKUP_DIR && ls -t ${APP_NAME}_backup_*.tar.gz | tail -n +6 | xargs -r ${sudoPrefix}rm -f" -IgnoreError
```
(With `-NoSudo` this archives the previous staging upload, i.e. the code last copied live - a cheap code rollback source. It runs before Deploy-Application empties staging.)

4e. `Deploy-Application` start. Replace:
```powershell
    # Ensure deploy directory exists
    Invoke-RemoteCommand "${sudoPrefix}mkdir -p $APP_DEPLOY_PATH"

    # Get files to deploy
```
with:
```powershell
    if ($NoSudo) {
        # Staging holds no live state. Empty it completely - dotfiles, data/
        # and config/ included - on EVERY upload path (tar and scp), so the
        # follow-up `cp -rT staging live` copies only this upload and never a
        # stale data/ or a stripped-then-reuploaded config/api_keys.R.
        if ($APP_DEPLOY_PATH -notmatch '^/home/[^/]+/EcoNeTool_staging$') {
            throw "Refusing to clear unexpected staging path: $APP_DEPLOY_PATH"
        }
        Invoke-RemoteCommand "rm -rf $APP_DEPLOY_PATH && mkdir -p $APP_DEPLOY_PATH"
    } else {
        # Ensure deploy directory exists
        Invoke-RemoteCommand "${sudoPrefix}mkdir -p $APP_DEPLOY_PATH"
    }

    # Get files to deploy
```

4f. `Test-ShouldExclude`. Replace:
```powershell
            $name = Split-Path -Leaf $Path
            $fullPath = $Path

            foreach ($pattern in $EXCLUDE_PATTERNS) {
                # Check filename against pattern
```
with:
```powershell
            $name = Split-Path -Leaf $Path
            $fullPath = $Path
            $unixPath = $Path -replace '\\', '/'

            foreach ($pattern in $EXCLUDE_PATTERNS) {
                # A pattern with an inner '/' (config/api_keys.R) names one
                # relative path: match it against the end of the full path
                # only. Trailing-slash directory patterns (cache/) keep the
                # old component matching below.
                if ($pattern.TrimEnd('/').Contains('/')) {
                    if ($unixPath -like "*/$pattern") { return $true }
                    continue
                }
                # Check filename against pattern
```

4g. Extract block. Replace:
```powershell
                    # Extract on server.
                    # Clean stale top-level entries BUT preserve persistent
                    # server-only state that lives under the app dir and is NOT
                    # in the tar: data/ (incl. data/feedback/feedback.db),
                    # cache/ (offline_traits.db), and r-libs/ (app-local R
                    # packages e.g. icesSAG). A blanket `rm -rf APP/*` would
                    # silently wipe user feedback, the offline DB, and icesSAG.
                    $preserve = "! -name data ! -name cache ! -name r-libs"
```
with:
```powershell
                    # Extract on server.
                    # Live tree: clean stale top-level entries BUT preserve
                    # server-only state that is NOT in the tar: dotfiles
                    # (.Renviron with the admin hash), data/ (~3.1 GB), cache/
                    # (offline_traits.db), r-libs/ (icesSAG), config/ (runtime
                    # keys and harmonization_custom.json) and models/. A
                    # blanket `rm -rf APP/*` would wipe them (F4).
                    # Staging (-NoSudo) holds no live state and was emptied at
                    # the start of Deploy-Application, so nothing is kept.
                    if ($NoSudo) {
                        $preserve = ""
                    } else {
                        $preserve = "! -name '.*' ! -name data ! -name cache ! -name r-libs ! -name models ! -name config"
                    }
```
(The next line, `Invoke-RemoteCommand "${sudoPrefix}find $APP_DEPLOY_PATH -mindepth 1 -maxdepth 1 $preserve -exec rm -rf {} + && ..."`, is unchanged.)

4h. NoSudo "NEXT STEPS". Replace:
```powershell
            Write-Host "   # (offline_traits.db) and r-libs/ survive. Do NOT 'rm -rf' the tree." -ForegroundColor DarkGray
```
with:
```powershell
            Write-Host "   # (offline_traits.db) and r-libs/ survive. Do NOT 'rm -rf' the tree." -ForegroundColor DarkGray
            Write-Host "   # Staging was emptied before this upload and holds no runtime" -ForegroundColor DarkGray
            Write-Host "   # config (api_keys.*, harmonization_custom.json): nothing to strip." -ForegroundColor DarkGray
```

- [ ] **Step 4: Run the tests and parse the script**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-deploy-preserve.R')"
pwsh -NoProfile -Command '$e=$null; [void][System.Management.Automation.Language.Parser]::ParseFile((Resolve-Path "deploy-windows.ps1").Path, [ref]$null, [ref]$e); "PS parse errors: $($e.Count)"'
```
Expected: `FAIL 0` (Tasks 1, 3, 4 blocks); `PS parse errors: 0`.

- [ ] **Step 5: Dry run against the server (read-only) - STOP - ask the user**

`-DryRun` still opens an SSH connection (connection test), so ask first.
```bash
powershell ./deploy-windows.ps1 -SkipData -NoSudo -DryRun
```
Expected in the output (in dry-run `Invoke-RemoteCommand` returns `""`, so the backup's "app exists" check is false and the extract step, which sits inside `if (-not $DryRun)`, does not print):
- `[DRY-RUN] Would execute: mkdir -p /home/razinka/backups && chmod 700 /home/razinka/backups`
- `No existing deployment to backup`
- `[DRY-RUN] Would execute: rm -rf /home/razinka/EcoNeTool_staging && mkdir -p /home/razinka/EcoNeTool_staging`
- `Staged: config/ (1 files, 2 excluded)` (only the template; the counts depend on which local runtime files exist) and `Staged: models/ (1 files, 0 excluded)`

No `tar` line appears in dry-run - that is expected, not a bug. Nothing is changed on the server.

- [ ] **Step 6: Commit**

```bash
git add deploy-windows.ps1 tests/testthat/test-deploy-preserve.R
git commit -m "fix(deploy): ps1 keeps dotfiles/config/models, empties staging, never uploads runtime config (F4, F5, F7)

-NoSudo now clears /home/<user>/EcoNeTool_staging itself on every upload
path, so the manual rm -rf staging and strip steps are no longer needed.
Backups are tar.gz (mode 600) under /srv/shiny-server-data/EcoNeTool/backups
or /home/<user>/backups, never a cp -r copy under site_dir.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 5: F5 / F7 - root `deploy.sh` (excludes and guards only)

**Files:**
- Modify: `deploy.sh` (L39 `BACKUP_DIR`, L57-70 `FILES`, L108-120 excludes, `create_backup` L367-377 and L390-400)
- Test: `tests/testthat/test-deploy-preserve.R` (append)

**Interfaces:**
- Consumes: `protected_root_sh()`, `backup_dirs()`, `code_lines()`, `RUNTIME_CONFIG_FILES` (Task 1).
- Produces: nothing.

- [ ] **Step 1: Append the failing tests**

```r

# --- deploy.sh (root; F5, F7) ------------------------------------------------

test_that("deploy.sh rsync --delete excludes server state and runtime config", {
  prot <- protected_root_sh(deploy_file("deploy.sh"))
  # config/ itself ships (templates); its runtime files are excluded below
  expect_equal(setdiff(c(".*", "data", "cache", "r-libs", "models"), prot), character(0))
  expect_equal(setdiff(RUNTIME_CONFIG_FILES, prot), character(0))
})

test_that("deploy.sh backups live outside site_dir", {
  path <- deploy_file("deploy.sh")
  expect_equal(backup_dirs(path), "/srv/shiny-server-data/EcoNeTool/backups")
  expect_true(any(grepl("chmod 600", code_lines(path), fixed = TRUE)))
})
```

- [ ] **Step 2: Run to verify it fails**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-deploy-preserve.R')"`
Expected: `FAIL 4` (missing `.*` and `data`; missing the three config paths; backup dir `/srv/shiny-server/backups/EcoNeTool`; no `chmod 600`).

- [ ] **Step 3: Edit `deploy.sh`**

5a. Replace `BACKUP_DIR="${SHINY_SERVER_ROOT}/backups/${APP_NAME}"` with:
```bash
# Outside site_dir: everything under ${SHINY_SERVER_ROOT} is served by
# shiny-server, and a backup holds config/api_keys.* (F7).
BACKUP_DIR="/srv/shiny-server-data/EcoNeTool/backups"
```

5b. In `FILES`, replace:
```bash
  "metawebs/"
  "data/"
  "config/"
)
```
with:
```bash
  "metawebs/"
  "config/"
)
```

5c. In `EXCLUDE_PATTERNS`, replace:
```bash
  ".Renviron"
  "r-libs"
  "r-libs/*"
```
with:
```bash
  ".Renviron"
  ".*"
  "r-libs"
  "r-libs/*"
  # data/ (~3.1 GB) is managed out of band; with --delete, shipping the
  # local data/ would delete every server-only file in it.
  "/data/"
  # Runtime config: server-only keys and settings, or a developer's local
  # copies. Excluded paths are neither sent nor deleted by --delete (F5).
  "config/api_keys.R"
  "config/api_keys.json"
  "config/harmonization_custom.json"
```
(`"models/*"` stays excluded: this script never shipped `models/`, and it cannot run against laguna anyway - spec non-goal.)

5d. In `create_backup`, local branch, replace:
```bash
    mkdir -p "${BACKUP_DIR}" || {
```
with:
```bash
    mkdir -p "${BACKUP_DIR}" && chmod 700 "${BACKUP_DIR}" || {
```
and replace:
```bash
      cd "${SHINY_SERVER_ROOT}" && tar -czf "${backup_path}" "${APP_NAME}" || {
```
with:
```bash
      cd "${SHINY_SERVER_ROOT}" && tar -czf "${backup_path}" "${APP_NAME}" && chmod 600 "${backup_path}" || {
```

5e. Remote branch, replace:
```bash
    ssh "${SERVER_USER}@${SERVER_HOST}" "mkdir -p ${BACKUP_DIR}" || {
```
with:
```bash
    ssh "${SERVER_USER}@${SERVER_HOST}" "mkdir -p ${BACKUP_DIR} && chmod 700 ${BACKUP_DIR}" || {
```
and replace:
```bash
"cd ${SHINY_SERVER_ROOT} && tar -czf ${backup_path} ${APP_NAME}" || {
```
with:
```bash
"cd ${SHINY_SERVER_ROOT} && tar -czf ${backup_path} ${APP_NAME} && chmod 600 ${backup_path}" || {
```

- [ ] **Step 4: Run tests and syntax check**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-deploy-preserve.R')"
bash -n deploy.sh && echo BASH_OK
```
Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 44 ]` (15 tests; the pwsh test skips only where `pwsh` is missing); `BASH_OK`.

- [ ] **Step 5: Commit**

```bash
git add deploy.sh tests/testthat/test-deploy-preserve.R
git commit -m "fix(deploy): root deploy.sh excludes data/, dotfiles and runtime config; backups outside site_dir (F5, F7)

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

- [ ] **Step 6: Acceptance check (spec 8.5) on the committed tree, then revert**

Run this only after Step 5's commit, so `git checkout -- <file>` restores the committed version. One script at a time, by hand (no commit):
1. `deployment/deploy.sh`: in the `find /srv/shiny-server/EcoNeTool ...` command, delete `! -name '.*' ` from its second line.
2. `deploy-windows.ps1`: in the live `$preserve = "..."` line, delete `! -name '.*' `.
3. `deploy.sh`: delete the `  ".*"` line from `EXCLUDE_PATTERNS`.

After each edit run `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-deploy-preserve.R')"` - expected: that script's "keeps dotfiles ..." / "rsync --delete excludes ..." test FAILS - then restore with `git checkout -- <file>` and confirm `git diff --stat` is empty before the next one.

---

### Task 6: No deploy script writes the shared Shiny Server config (controller ruling)

**Files:**
- Modify: `deployment/deploy.sh` (the "Configure Shiny Server" block after the R package install, originally L317-363; it already carries Task 3f's `directory_index off;`)
- Test: `tests/testthat/test-deploy-preserve.R` (append)

Only `deployment/deploy.sh` writes under `/etc/shiny-server/` today (verified with `grep -rn "/etc/shiny-server" --include=*.sh --include=*.ps1`): a `cp` backup of the live conf, a `cp` of the repo conf over it, and a `cat >` heredoc fallback. `deploy.sh`, `deploy-windows.ps1`, `deployment/force-reload.sh` and `deployment/verify-deployment.sh` do not; the guard covers them (and any future `*.sh`/`*.ps1` at the root or in `deployment/`) so it stays that way.

**Interfaces:**
- Consumes: `code_lines()` (Task 1), `deploy_file()` (test file), `get_app_root()` (helper-fixtures.R).
- Produces (test file only): `writes_etc_shiny(lines) -> logical`, `deploy_scripts() -> character` (relative paths).

- [ ] **Step 1: Append the failing tests to `tests/testthat/test-deploy-preserve.R`**

```r

# --- no script writes the shared Shiny Server config -------------------------

# TRUE for a code line that writes under /etc/shiny-server/: a copy/move/tee/
# install/link/rsync whose command line names it, a `>`/`>>` redirect into
# it, or `sed -i` on it. Lines that only print or read it (echo, Write-Host,
# grep, sed -n) are not writes.
writes_etc_shiny <- function(lines) {
  cmd <- "(^|[;&|(]\\s*|\\bsudo\\s+)(cp|mv|tee|install|ln|rsync|truncate)\\b[^#]*/etc/shiny-server/"
  grepl(cmd, lines, perl = TRUE) |
    grepl(">>?\\s*[\"']?/etc/shiny-server/", lines, perl = TRUE) |
    grepl("\\bsed\\s+(-[a-zA-Z]*i|--in-place)[^#]*/etc/shiny-server/", lines, perl = TRUE)
}

deploy_scripts <- function() {
  root <- get_app_root()
  c(list.files(root, "\\.(sh|ps1)$"),
    file.path("deployment", list.files(file.path(root, "deployment"), "\\.(sh|ps1)$")))
}

test_that("writes_etc_shiny() flags writes and ignores prints and reads", {
  writes <- c(
    "cp \"$DEPLOY_DIR/shiny-server.conf\" /etc/shiny-server/shiny-server.conf",
    "    sudo cp x /etc/shiny-server/shiny-server.conf",
    "cat > /etc/shiny-server/shiny-server.conf <<'EOF'",
    "echo x | sudo tee /etc/shiny-server/shiny-server.conf",
    "sudo sed -i 's/on;/off;/' /etc/shiny-server/shiny-server.conf",
    "Invoke-RemoteCommand \"sudo cp /tmp/c /etc/shiny-server/shiny-server.conf\""
  )
  reads <- c(
    "echo \"  sudo nano /etc/shiny-server/shiny-server.conf\"",
    "grep -n directory_index /etc/shiny-server/shiny-server.conf",
    "sed -n '/location \\/EcoNeTool {/,/^  }/p' \"$DEPLOY_DIR/shiny-server.conf\"",
    "Write-Host \"edit /etc/shiny-server/shiny-server.conf by hand\""
  )
  expect_equal(writes_etc_shiny(writes), rep(TRUE, length(writes)))
  expect_equal(writes_etc_shiny(reads), rep(FALSE, length(reads)))
})

test_that("no deploy script writes to /etc/shiny-server/ (shared server)", {
  scripts <- deploy_scripts()
  expect_true(all(c("deploy.sh", "deploy-windows.ps1", "deployment/deploy.sh",
                    "deployment/force-reload.sh") %in% scripts))
  for (rel in scripts) {
    code <- code_lines(deploy_file(rel))
    offenders <- code[writes_etc_shiny(code)]
    expect_equal(length(offenders), 0L, info = paste(rel, ":", paste(trimws(offenders), collapse = " | ")))
  }
})

test_that("deployment/deploy.sh prints the EcoNeTool location block for a manual edit", {
  code <- code_lines(deploy_file("deployment/deploy.sh"))
  expect_true(any(grepl("sed -n '/location \\/EcoNeTool {/,/^  }/p' \"$DEPLOY_DIR/shiny-server.conf\"",
                        code, fixed = TRUE)))
  conf <- readLines(deploy_file("deployment/shiny-server.conf"), warn = FALSE)
  start <- grep("location /EcoNeTool {", conf, fixed = TRUE)
  expect_length(start, 1L)
  expect_true(any(conf[start:length(conf)] == "  }"), info = "the sed range needs a closing '  }' line")
})
```

- [ ] **Step 2: Run to verify it fails**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-deploy-preserve.R')"`
Expected: `FAIL 2` - "no deploy script writes to /etc/shiny-server/" lists `deployment/deploy.sh : cp /etc/shiny-server/shiny-server.conf /etc/shiny-server/shiny-server.conf.backup | cp "$DEPLOY_DIR/shiny-server.conf" /etc/shiny-server/shiny-server.conf | cat > /etc/shiny-server/shiny-server.conf <<'EOF'`; the snippet test finds no `sed -n` print. The detector self-test and all earlier blocks pass.

- [ ] **Step 3: Replace the conf block in `deployment/deploy.sh`**

Replace everything from the line `    # Configure Shiny Server` down to and including `    print_success "Shiny Server configured"` - that is:
```bash
    # Configure Shiny Server
    print_status "Configuring Shiny Server..."

    # Backup original config
    if [ -f /etc/shiny-server/shiny-server.conf ]; then
        cp /etc/shiny-server/shiny-server.conf /etc/shiny-server/shiny-server.conf.backup
        print_success "Original config backed up"
    fi

    # Create or copy new config
    if [ -f "$DEPLOY_DIR/shiny-server.conf" ]; then
        cp "$DEPLOY_DIR/shiny-server.conf" /etc/shiny-server/shiny-server.conf
    else
        print_status "Creating default shiny-server.conf..."
        cat > /etc/shiny-server/shiny-server.conf <<'EOF'
```
... through the heredoc body (as left by Task 3f, with `directory_index off;`), the closing `EOF`, `    fi`, a blank line and `    print_success "Shiny Server configured"` - with:
```bash
    # Shiny Server configuration is NEVER written by a deploy script.
    # laguna is a shared server (~30 apps: BowTie, osmose, marxan, ...);
    # copying deployment/shiny-server.conf over the live file would drop
    # every other app's location block. Print the EcoNeTool block from the
    # repo copy instead, for a manual, reviewed edit.
    print_warning "Shiny Server config NOT modified (shared server)."
    echo "If the live config lacks an EcoNeTool location, add this block by hand"
    echo "(sudo, reviewed), then reload shiny-server:"
    echo ""
    sed -n '/location \/EcoNeTool {/,/^  }/p' "$DEPLOY_DIR/shiny-server.conf"
    echo ""
```
The next block (`    # Clear Shiny Server caches`) is unchanged. This also removes the heredoc that Task 3f edited; Task 3's "no directory index anywhere" test keeps passing (nothing left to match).

- [ ] **Step 4: Run the tests, syntax-check, and see the snippet**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-deploy-preserve.R')"
bash -n deployment/deploy.sh && echo BASH_OK
sed -n '/location \/EcoNeTool {/,/^  }/p' deployment/shiny-server.conf
grep -rn "/etc/shiny-server" --include=*.sh --include=*.ps1 . | grep -v "^./archive/"
```
Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 56 ]` (18 tests; the pwsh test skips only where `pwsh` is missing); `BASH_OK`; the printed block runs from `  location /EcoNeTool {` to its closing `  }`; the final grep prints nothing.

- [ ] **Step 5: Commit**

```bash
git add deployment/deploy.sh tests/testthat/test-deploy-preserve.R
git commit -m "fix(deploy): never overwrite /etc/shiny-server/shiny-server.conf (shared server)

laguna hosts ~30 other Shiny apps; copying the repo conf over the live
one would drop their location blocks. deployment/deploy.sh now prints the
location /EcoNeTool block for a manual, reviewed edit. A guard fails if
any root or deployment/ *.sh / *.ps1 writes under /etc/shiny-server/.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 7: F83 - pre-deploy check parses all of `R/`

**Files:**
- Modify: `deployment/pre-deploy-check.R:240-257` (the `r_files <- c("app.R")` block under `[4] Checking R Syntax...`)
- Create: `tests/testthat/test-pre-deploy-check.R`

**Interfaces:**
- Consumes: `print_check(name, status, message)` (defined at the top of the script; `"ERROR"` increments `errors`, which leads to `quit(status = 1)`).
- Produces: top-level `collect_r_syntax_errors(root = ".") -> list(files = character, errors = named list of messages)` in `deployment/pre-deploy-check.R`.

- [ ] **Step 1: Write the failing test `tests/testthat/test-pre-deploy-check.R`**

```r
# =============================================================================
# deployment/pre-deploy-check.R parses all of R/ (F83)
# =============================================================================
# The syntax check used to parse only app.R and run_app.R, so a syntax error
# in any sourced module passed the gate. The script itself runs top-level
# code (setwd(".."), package checks, quit()), so the test evaluates only the
# collect_r_syntax_errors() definition taken from the script's parse tree.

load_syntax_checker <- function() {
  script <- file.path(get_app_root(), "deployment", "pre-deploy-check.R")
  exprs <- parse(script, keep.source = FALSE)
  is_def <- vapply(exprs, function(e) {
    is.call(e) && identical(e[[1]], as.name("<-")) &&
      identical(e[[2]], as.name("collect_r_syntax_errors"))
  }, logical(1))
  if (sum(is_def) != 1L) {
    stop("pre-deploy-check.R must define collect_r_syntax_errors() exactly once at top level")
  }
  env <- new.env(parent = baseenv())
  # Safe: evaluates one function *definition* from the repo's own script into
  # a baseenv() child; nothing in the script body runs.
  eval(exprs[[which(is_def)]], env)
  env$collect_r_syntax_errors
}

make_tree <- function(files) {
  root <- tempfile("predeploy_")
  for (rel in names(files)) {
    dir.create(dirname(file.path(root, rel)), recursive = TRUE, showWarnings = FALSE)
    writeLines(files[[rel]], file.path(root, rel))
  }
  root
}

test_that("a syntax error anywhere under R/ is reported, including nested dirs", {
  check <- load_syntax_checker()
  root <- make_tree(c(
    "app.R" = "x <- 1",
    "R/functions/ok.R" = "f <- function() 1",
    "R/modules/bad.R" = "{",
    "R/functions/trait_lookup/bad2.R" = "g <- function( {"
  ))
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  res <- check(root)
  expect_setequal(names(res$errors), c("R/modules/bad.R", "R/functions/trait_lookup/bad2.R"))
  expect_true(all(c("app.R", "R/functions/ok.R") %in% res$files))
  expect_gte(length(res$errors), 1L)
})

test_that("safeBackup copies are skipped", {
  check <- load_syntax_checker()
  root <- make_tree(c("app.R" = "x <- 1", "R/modules/x_safeBackup.R" = "{"))
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  res <- check(root)
  expect_length(res$errors, 0L)
  expect_false(any(grepl("safeBackup", res$files)))
})

test_that("the real tree parses, trait_lookup included", {
  check <- load_syntax_checker()
  res <- check(get_app_root())
  expect_equal(length(res$errors), 0L, info = paste(names(res$errors), collapse = ", "))
  expect_true("R/functions/trait_lookup/orchestrator.R" %in% res$files)
  expect_true("app.R" %in% res$files)
})

test_that("the script turns every parse error into an ERROR check", {
  code <- readLines(file.path(get_app_root(), "deployment", "pre-deploy-check.R"), warn = FALSE)
  code <- code[!grepl("^\\s*#", code)]
  expect_true(any(grepl('collect_r_syntax_errors(".")', code, fixed = TRUE)))
  expect_true(any(grepl('print_check(paste("Syntax:", f), "ERROR"', code, fixed = TRUE)))
})
```

- [ ] **Step 2: Run to verify it fails**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-pre-deploy-check.R')"`
Expected: three tests error with "pre-deploy-check.R must define collect_r_syntax_errors() exactly once at top level"; the fourth fails twice.

- [ ] **Step 3: Replace the syntax block in `deployment/pre-deploy-check.R`**

Replace:
```r
r_files <- c("app.R")
if (file.exists("run_app.R")) {
  r_files <- c(r_files, "run_app.R")
}
for (file in r_files) {
  if (file.exists(file)) {
    result <- tryCatch({
      parse(file)
      TRUE
    }, error = function(e) {
      print_check(paste("Syntax:", file), "ERROR", e$message)
      FALSE
    })
    if (result) {
      print_check(paste("Syntax:", file), "PASS")
    }
  }
}
```
with:
```r
# Parse app.R, run_app.R and every R/**/*.R (F83). Only app.R and run_app.R
# were parsed before, so a syntax error in a sourced module reached laguna
# and broke the app at startup. Returns the files checked and a named list
# of parse errors (file -> message). tests/testthat/test-pre-deploy-check.R
# evaluates this definition on its own, so keep it self-contained.
collect_r_syntax_errors <- function(root = ".") {
  files <- c("app.R", "run_app.R",
             file.path("R", list.files(file.path(root, "R"), pattern = "\\.R$", recursive = TRUE)))
  files <- files[file.exists(file.path(root, files))]
  files <- files[!grepl("safeBackup", files, fixed = TRUE)]
  errors <- list()
  for (f in files) {
    msg <- tryCatch({
      parse(file.path(root, f))
      NULL
    }, error = function(e) conditionMessage(e))
    if (!is.null(msg)) errors[[f]] <- msg
  }
  list(files = files, errors = errors)
}

syntax <- collect_r_syntax_errors(".")
for (f in names(syntax$errors)) {
  print_check(paste("Syntax:", f), "ERROR", syntax$errors[[f]])
}
if (length(syntax$errors) == 0) {
  print_check(sprintf("Syntax: %d R files parse", length(syntax$files)), "PASS")
}
```

- [ ] **Step 4: Run the test, then the real script**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-pre-deploy-check.R')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='deployment/pre-deploy-check.R'); cat('OK\n')"
cd deployment && "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" pre-deploy-check.R | grep -n "Syntax\|DEPLOYMENT"; echo "exit=${PIPESTATUS[0]}"; cd ..
```
Expected: `FAIL 0`; `OK`; `✓ Syntax: 91 R files parse` (or the current count), `DEPLOYMENT POSSIBLE WITH WARNINGS` (existing warnings), `exit=0`.

- [ ] **Step 5: Commit**

```bash
git add deployment/pre-deploy-check.R tests/testthat/test-pre-deploy-check.R
git commit -m "fix(deploy): pre-deploy check parses every R/**/*.R (F83)

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 8: F82 - CI parses all of `R/` and runs the offline suite (last code task)

**Files:**
- Modify: `.github/workflows/ci.yml` (r-syntax step L149-157; new job after `r-syntax`; `ci-status` L262-274)
- Modify: `.github/workflows/r-check.yml` (L92-99)
- Create: `tests/testthat/test-ci-workflows.R`

**Interfaces:**
- Consumes: Task 2 (the offline suite makes no layer2c HTTP).
- Produces: CI job id `testthat-offline` (name "testthat (offline)"), required by `ci-status`.

- [ ] **Step 1: Write the failing test `tests/testthat/test-ci-workflows.R`**

```r
# =============================================================================
# CI parses all of R/ and runs the offline testthat suite (F82)
# =============================================================================
# The parse steps in ci.yml and r-check.yml used a fixed, non-recursive
# directory list that skipped R/functions/trait_lookup/, and no job ran
# testthat on push/PR. These tests run each workflow's own parse script
# against a scratch tree and check the offline job's wiring.

read_workflow <- function(file) {
  yaml::read_yaml(file.path(get_app_root(), ".github", "workflows", file))
}

workflow_step_run <- function(file, job, step) {
  steps <- read_workflow(file)$jobs[[job]]$steps
  hit <- Filter(function(s) identical(s$name, step), steps)
  if (length(hit) != 1L) stop(sprintf("%s: job %s has no single step '%s'", file, job, step))
  hit[[1]]$run
}

# Runs an Rscript step body with `root` as working directory; returns the
# exit status (0 = success).
run_step_in <- function(script, root) {
  q <- function(x) if (.Platform$OS.type == "windows") shQuote(x, type = "cmd") else shQuote(x)
  f <- tempfile(fileext = ".R")
  writeLines(script, f)
  on.exit(unlink(f), add = TRUE)
  out <- withr::with_dir(root, suppressWarnings(
    system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", q(f)), stdout = TRUE, stderr = TRUE)
  ))
  status <- attr(out, "status")
  if (is.null(status)) 0L else status
}

scratch_tree <- function(bad_file = NULL) {
  root <- tempfile("ci_parse_")
  dir.create(file.path(root, "R", "functions", "trait_lookup"), recursive = TRUE)
  writeLines("x <- 1", file.path(root, "app.R"))
  writeLines("f <- function() 1", file.path(root, "R", "functions", "trait_lookup", "ok.R"))
  if (!is.null(bad_file)) {
    dir.create(dirname(file.path(root, bad_file)), recursive = TRUE, showWarnings = FALSE)
    writeLines("{", file.path(root, bad_file))
  }
  root
}

parse_steps <- list(
  list(file = "ci.yml", job = "r-syntax", step = "Parse-check all R files"),
  list(file = "r-check.yml", job = "r-validation", step = "Validate all R file syntax")
)

test_that("both workflow parse steps fail on a syntax error in R/functions/trait_lookup/", {
  skip_if_not_installed("yaml")
  for (ps in parse_steps) {
    script <- workflow_step_run(ps$file, ps$job, ps$step)
    clean <- scratch_tree()
    bad <- scratch_tree("R/functions/trait_lookup/broken.R")
    on.exit(unlink(c(clean, bad), recursive = TRUE), add = TRUE)
    expect_equal(run_step_in(script, clean), 0L, info = paste(ps$file, "clean tree"))
    expect_gt(run_step_in(script, bad), 0L, label = paste(ps$file, "exit status with a broken trait_lookup file"))
  }
})

test_that("ci.yml runs the offline testthat suite and CI Status depends on it", {
  skip_if_not_installed("yaml")
  wf <- read_workflow("ci.yml")
  job <- wf$jobs[["testthat-offline"]]
  expect_false(is.null(job), info = "no testthat-offline job in ci.yml")
  expect_equal(job[["timeout-minutes"]], 20L)
  expect_null(job$env$RUN_LIVE_TESTS)

  runs <- paste(vapply(job$steps, function(s) s$run %||% "", character(1)), collapse = "\n")
  expect_match(runs, "testthat::test_dir(", fixed = TRUE)
  expect_match(runs, '"tests/testthat"', fixed = TRUE)
  expect_match(runs, "stop_on_failure = TRUE", fixed = TRUE)
  expect_false(grepl("RUN_LIVE_TESTS", runs, fixed = TRUE))

  expect_true("testthat-offline" %in% unlist(wf$jobs[["ci-status"]]$needs))
  status_run <- wf$jobs[["ci-status"]]$steps[[1]]$run
  expect_match(status_run, "needs.testthat-offline.result", fixed = TRUE)
})
```

- [ ] **Step 2: Run to verify it fails**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-ci-workflows.R')"`
Expected: failures include `ci.yml exit status with a broken trait_lookup file > 0L ... 0 <= 0` and the same for `r-check.yml` (the current parse steps pass a broken `R/functions/trait_lookup/broken.R`), plus the missing `testthat-offline` job / needs.

- [ ] **Step 3: Edit `.github/workflows/ci.yml`**

7a. In the `r-syntax` job, replace:
```yaml
          r_files <- list.files(
            c(".", "R", "R/config", "R/functions", "R/functions/ecopath",
              "R/functions/rpath", "R/modules", "R/ui"),
            pattern = "\\.R$",
            full.names = TRUE,
            recursive = FALSE
          )
          # Exclude backup files
```
with:
```yaml
          # Root *.R plus everything under R/, recursively. A fixed directory
          # list silently skipped R/functions/trait_lookup/ (F82).
          r_files <- c(
            list.files(".", pattern = "\\.R$", full.names = TRUE),
            list.files("R", pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
          )
          # Exclude backup files
```

7b. Insert a new job between the end of `r-syntax` (its last line is `        shell: Rscript {0}`) and the `# R linting (runs only when R files change)` banner. Replace:
```yaml
            cat("All R files have valid syntax\n")
          }
        shell: Rscript {0}

  # ===========================================================================
  # R linting (runs only when R files change)
```
with:
```yaml
            cat("All R files have valid syntax\n")
          }
        shell: Rscript {0}

  # ===========================================================================
  # Offline testthat suite (F82). RUN_LIVE_TESTS is deliberately unset, so
  # every live-API test skips; the nightly workflow runs those.
  # ===========================================================================
  testthat-offline:
    name: testthat (offline)
    runs-on: ubuntu-latest
    timeout-minutes: 20
    steps:
      - name: Checkout code
        uses: actions/checkout@v4

      - name: Set up R
        uses: r-lib/actions/setup-r@v2
        with:
          r-version: '4.4.1'
          use-public-rspm: true

      - name: Install system dependencies
        run: |
          sudo apt-get update
          sudo apt-get install -y \
            libcurl4-openssl-dev \
            libssl-dev \
            libxml2-dev \
            libfontconfig1-dev \
            libfreetype6-dev \
            libpng-dev \
            libgdal-dev \
            libgeos-dev \
            libproj-dev \
            libudunits2-dev

      - name: Install R dependencies
        uses: r-lib/actions/setup-r-dependencies@v2
        with:
          packages: |
            any::testthat
            any::worrms
            any::rfishbase
            any::httr
            any::jsonlite
            any::igraph
            any::dplyr
            any::tidyr
            any::readr
            any::readxl
            any::RCurl
            any::XML
            any::plyr
            any::ape
            any::icesVocab
            any::icesDatras
            any::randomForest
            any::DBI
            any::RSQLite
            any::digest
            any::openssl
            any::shiny
            any::withr
            any::yaml
            any::sf
            any::visNetwork
            any::fluxweb
            any::R6
            any::MASS
            any::leaflet
            any::DT
            any::bs4Dash
            any::shinyWidgets
            any::shinyBS
            any::future
            any::future.apply

      - name: Run offline testthat suite (fail on any test failure)
        run: |
          testthat::test_dir(
            "tests/testthat",
            reporter = "summary",
            stop_on_failure = TRUE
          )
        shell: Rscript {0}

  # ===========================================================================
  # R linting (runs only when R files change)
```
(The package list is the nightly job's list plus `withr` and `yaml` (used by the new tests) and `sf`, `visNetwork`, `fluxweb`, `R6`, `MASS`, `leaflet`, `DT`, `bs4Dash`, `shinyWidgets`, `shinyBS`, `future`, `future.apply`, which `app.R` / `R/functions/*.R` load with `library()`. The last group is pre-emptive, not verified per test file: each missing package would cost a ~10 min CI round-trip, and RSPM binaries make extras cheap. Task 11 Step 3 remains the backstop. The system libraries add the `sf` stack from `r-check.yml`.)

7c. In `ci-status`, replace:
```yaml
    needs: [pre-commit, repo-hygiene, shellcheck, r-syntax, version-drift]
```
with:
```yaml
    needs: [pre-commit, repo-hygiene, shellcheck, r-syntax, testthat-offline, version-drift]
```
then replace:
```yaml
          echo "R syntax: ${{ needs.r-syntax.result }}"

          if [ "${{ needs.pre-commit.result }}" == "failure" ] || \
             [ "${{ needs.repo-hygiene.result }}" == "failure" ] || \
             [ "${{ needs.r-syntax.result }}" == "failure" ]; then
```
with:
```yaml
          echo "R syntax: ${{ needs.r-syntax.result }}"
          echo "testthat (offline): ${{ needs.testthat-offline.result }}"

          if [ "${{ needs.pre-commit.result }}" == "failure" ] || \
             [ "${{ needs.repo-hygiene.result }}" == "failure" ] || \
             [ "${{ needs.r-syntax.result }}" == "failure" ] || \
             [ "${{ needs.testthat-offline.result }}" != "success" ]; then
```
(`!= "success"` also fails CI Status when the job is cancelled or times out.)

- [ ] **Step 4: Edit `.github/workflows/r-check.yml`**

Replace:
```yaml
          r_files <- list.files(
            c(".", "R", "R/config", "R/functions", "R/functions/ecopath",
              "R/functions/rpath", "R/modules", "R/ui"),
            pattern = "\\.R$",
            full.names = TRUE,
            recursive = FALSE
          )
          r_files <- r_files[!grepl("safeBackup", r_files)]
```
with:
```yaml
          # Root *.R plus everything under R/, recursively (F82).
          r_files <- c(
            list.files(".", pattern = "\\.R$", full.names = TRUE),
            list.files("R", pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
          )
          r_files <- r_files[!grepl("safeBackup", r_files)]
```

- [ ] **Step 5: Run the test and validate the YAML**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-ci-workflows.R')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "invisible(lapply(c('.github/workflows/ci.yml','.github/workflows/r-check.yml'), yaml::read_yaml)); cat('YAML OK\n')"
```
Expected: `FAIL 0`; `YAML OK`.

- [ ] **Step 6: Commit**

```bash
git add .github/workflows/ci.yml .github/workflows/r-check.yml tests/testthat/test-ci-workflows.R
git commit -m "ci: parse R/ recursively and run the offline testthat suite on push/PR (F82)

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 9: CONTRIBUTING convention and full local suite

**Files:**
- Modify: `CONTRIBUTING.md` (section "Trait-pipeline & concurrency patterns": the "Live-API tests" bullet, and a new bullet before `## Commit Messages`)

**Interfaces:** none.

- [ ] **Step 1: Edit `CONTRIBUTING.md`**

Replace:
```markdown
- **Live-API tests gate behind `RUN_LIVE_TESTS=true`** and run on the
  nightly workflow (`.github/workflows/nightly-live-tests.yml`).
  Inside the gate use `with_timeout()` (in `R/functions/validation_utils.R`)
  so a slow upstream day doesn't hang the whole run.
```
with:
```markdown
- **Live-API tests gate behind `RUN_LIVE_TESTS=true`** and run on the
  nightly workflow (`.github/workflows/nightly-live-tests.yml`).
  Inside the gate use `with_timeout()` (in `R/functions/validation_utils.R`)
  so a slow upstream day doesn't hang the whole run. CI's
  `testthat-offline` job runs the suite on every PR with the variable
  unset, so an ungated HTTP call shows up there as a flaky failure.
```
Then replace:
```markdown
  `config/api_keys.json` and `.Renviron` are both gitignored. Check
  before committing if you touch that area.

## Commit Messages
```
with:
```markdown
  `config/api_keys.json` and `.Renviron` are both gitignored. Check
  before committing if you touch that area.

- **Deploy-script guards read code lines only.** Assertions about
  `deploy.sh`, `deployment/deploy.sh` or `deploy-windows.ps1` go through
  `code_lines()` / `script_array()` in `tests/testthat/helper-deploy.R`,
  which drop comments and join `\` continuations first. A raw `grepl()`
  over the file also matches comments: the old guard "passed" because a
  comment mentioned `.Renviron`. Every deploy path must keep dotfiles,
  `data/`, `cache/`, `r-libs/`, `models/` and `config/` on the server,
  never upload `config/api_keys.*`, `config/harmonization_custom.json` or
  `.Renviron`, and write backups as `tar.gz` (mode 600) to
  `/srv/shiny-server-data/EcoNeTool/backups` - never under
  `/srv/shiny-server/`, which shiny-server serves. No deploy script
  writes anything under `/etc/shiny-server/`: laguna is shared with ~30
  other apps, so conf changes are printed as a snippet and applied by
  hand after review.

## Commit Messages
```

- [ ] **Step 2: Full local suite**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "r <- as.data.frame(testthat::test_dir('tests/testthat', reporter = 'silent', stop_on_failure = FALSE)); cat('PASS', sum(r\$nb) - sum(r\$failed), 'FAIL', sum(r\$failed), 'SKIP', sum(r\$skipped), 'ERR', sum(r\$error), '\n')"
```
Expected: `FAIL 0`, `ERR 0`, `SKIP` = Task 0 baseline + 10 (all ten in `test-layer2c-api-databases.R`; spec 8.11 "the skip count does not grow except for newly gated live tests"). PASS grows by the new tests.

- [ ] **Step 3: Lint the new and edited R files**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "for (f in c('tests/testthat/helper-deploy.R','tests/testthat/test-deploy-preserve.R','tests/testthat/test-live-gating-guard.R','tests/testthat/test-layer2c-api-databases.R','tests/testthat/test-pre-deploy-check.R','tests/testthat/test-ci-workflows.R','tests/testthat/test-trait-lookup-unit.R')) print(lintr::lint(f))"
```
Expected: only `object_usage_linter` notes about `get_app_root` (defined in a helper), as in the existing test files. `lintr::lint('deployment/pre-deploy-check.R')` reports 5 pre-existing `indentation_linter` lines (136, 171, 460, 513, 523 before the edit) and nothing on the new block.

- [ ] **Step 4: Commit**

```bash
git add CONTRIBUTING.md
git commit -m "docs(contributing): deploy-guard convention and CI offline suite note (B2)

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 10: Version 1.5.1 and CHANGELOG

**Files:**
- Modify: `VERSION`, `app.R` (`# CURRENT VERSION:` line), `README.md` (version markers) via `scripts/version_bump.R`
- Modify: `R/config.R` (`load_version_info()` fallback list; `version_bump.R` does not touch it)
- Modify: `CHANGELOG.md` (regenerate, then re-insert the 1.5.0 hand-written section)

**Interfaces:** none.

- [ ] **Step 1: Bump**

```bash
grep -n "^VERSION=" VERSION   # expect VERSION=1.5.0
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/version_bump.R --version 1.5.1 --name "Deploy and CI Hardening" --dry-run
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/version_bump.R --version 1.5.1 --name "Deploy and CI Hardening"
grep -n "^VERSION=\|^PATCH=\|^VERSION_NAME=" VERSION; grep -n "CURRENT VERSION" app.R; grep -n "1\.5\.1" README.md
```
Expected: `VERSION=1.5.1`, `PATCH=1`, `VERSION_NAME=Deploy and CI Hardening`; `# CURRENT VERSION: v1.5.1 (<today>)`; README markers read 1.5.1.

- [ ] **Step 2: Align the `R/config.R` fallback**

In `load_version_info()` replace:
```r
    VERSION = "1.5.0",
    VERSION_NAME = "Network Science Correctness",
    RELEASE_DATE = "2026-09-27",
    STATUS = "stable",
    MAJOR = 1,
    MINOR = 5,
    PATCH = 0
```
with:
```r
    VERSION = "1.5.1",
    VERSION_NAME = "Deploy and CI Hardening",
    RELEASE_DATE = "<today, YYYY-MM-DD - same as VERSION's RELEASE_DATE>",
    STATUS = "stable",
    MAJOR = 1,
    MINOR = 5,
    PATCH = 1
```
Then: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='R/config.R'); cat('OK\n')"` and `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-config-paths.R')"`. Expected: `OK`; `FAIL 0`.

- [ ] **Step 3: Regenerate the CHANGELOG, then re-insert the 1.5.0 "Results changed" section**

`scripts/generate_changelog.R` rebuilds the whole file from git history and **erases** the hand-written `### Results changed` block under `## [1.5.0]`. Its canonical source is `docs/releases/1.5.0-results-changed.md` (72 lines, starts with `### Results changed`).

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/generate_changelog.R --version 1.5.1
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "
cl <- readLines('CHANGELOG.md', warn = FALSE)
rc <- readLines('docs/releases/1.5.0-results-changed.md', warn = FALSE)
h <- grep('## [1.5.0]', cl, fixed = TRUE)
stopifnot(length(h) == 1L, !any(grepl('### Results changed', cl, fixed = TRUE)))
writeLines(c(cl[1:h], '', rc, cl[(h + 1):length(cl)]), 'CHANGELOG.md')
cat('inserted after line', h, '\n')
"
grep -nF -e "## [1.5." -e "### Results changed" CHANGELOG.md
```
Expected: `## [1.5.1] - <today>` first, then `## [1.5.0] - 2026-09-27`, then `### Results changed` exactly once, two lines below the `[1.5.0]` heading. Open the `[1.5.1]` section and check it lists the B2 commits (Tasks 1-9). If the generator did not group a commit sensibly, leave it - do not hand-edit generated entries.

- [ ] **Step 4: Commit**

```bash
git add VERSION app.R README.md R/config.R CHANGELOG.md
git commit -m "chore(release): 1.5.1 - deploy and CI hardening

CHANGELOG regenerated; the hand-written 1.5.0 'Results changed' section
is re-inserted from docs/releases/1.5.0-results-changed.md (the generator
erases it on every run).

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 11: Push, PR, first CI run

**Interfaces:** none.

- [ ] **Step 1: Push and open the PR - STOP - ask the user before pushing**

```bash
git push -u origin fix/b2-deploy-ci
gh pr create --base master --title "fix(deploy,ci): B2 deploy and CI hardening (F86, F84, F81, F4, F5, F7, F83, F82) - 1.5.1" --body "$(cat <<'EOF'
Spec B section 4.2 - docs/superpowers/specs/2026-09-26-fix-b-platform-safety-design.md
Plan - docs/superpowers/plans/2026-09-27-b2-deploy-ci-hardening.md

- Guards read comment-stripped code lines (helper-deploy.R); all three deploy scripts covered (F86).
- Every deploy path keeps dotfiles, data/, cache/, r-libs/, models/, config/; runtime config (api_keys.*, harmonization_custom.json, .Renviron) is never uploaded (F81, F4, F5).
- deploy-windows.ps1 -NoSudo empties staging itself on every upload path: the manual `rm -rf staging` and strip steps are gone.
- Backups: tar.gz, mode 600, under /srv/shiny-server-data/EcoNeTool/backups (or /home/<user>/backups); reference conf directory_index off (F7). Server cleanup of the stray /srv/shiny-server/EcoNeTool.bak.20260510_192058 and the live directory_index are manual sudo steps (plan Task 12).
- No deploy script writes /etc/shiny-server/ any more (controller ruling: shared server); deployment/deploy.sh prints the location /EcoNeTool block for a manual edit, and a guard covers every root/deployment *.sh and *.ps1.
- pre-deploy-check.R parses every R/**/*.R (F83); CI parses R/ recursively and runs the offline testthat suite (F82).
- layer2c network tests gated by RUN_LIVE_TESTS with 15 s timeouts (F84).

Deviations: ps1 live backups exclude data/ (60 s remote-command limit); root deploy.sh also excludes ".*" and /data/; two more if-gated unit tests converted to skip_if.
Behaviour change on first deploy: models/ now ships with the ps1, so the ML trait tier becomes available in production if it was missing.

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

- [ ] **Step 2: Watch CI**

```bash
gh pr checks --watch
```
Expected: `CI Status`, `testthat (offline)`, `R syntax validation`, `R Validation` green.

- [ ] **Step 3: If `testthat (offline)` is red - STOP - report to the user, do not merge**

This job has never run on Linux. Read the log (`gh run view --log-failed`) and classify:
- `there is no package called 'X'` -> add `any::X` to the job's `packages:` list, commit `ci: install X for the offline suite`, push, re-watch.
- A Windows-only assumption in a test (paths, drive letters, an example file only present locally) -> fix the test with `skip_on_os()` / `skip_if(!file.exists(...), "<reason>")`, never with `if (...) expect_*`; separate commit.
- Package installation alone exceeds the 20 min limit -> spec 7 fallback: report to the user with timings before changing anything (split by file, keep `stop_on_failure`).
- A genuine failure in app code -> stop and report; it is out of B2 scope.

- [ ] **Step 4: After the user merges - tag**

```bash
git checkout master && git pull && git tag v1.5.1 && git push origin v1.5.1
```

---

### Task 12: Server cleanup (USER, sudo) and deploy 1.5.1 with the new scripts (EACH STEP: STOP - ask the user)

Nothing in this task runs without the user's explicit go-ahead for that step. Steps marked **USER** need sudo; razinka has no passwordless sudo, so the user runs them in their own SSH session. Spec 6 "B2 manual server steps" step 2 ("Move `/srv/shiny-server/backups/*`") does not apply: that directory does not exist. The real F7 object is the stray backup inside site_dir plus `directory_index on`.

- [ ] **Step 1 (STOP - ask the user): Read-only survey**

```bash
ssh razinka@laguna.ku.lt "ls -ld /srv/shiny-server-data/EcoNeTool /srv/shiny-server-data/EcoNeTool/backups /srv/shiny-server/EcoNeTool.bak.* 2>&1; du -sh /srv/shiny-server/EcoNeTool.bak.* /srv/shiny-server/EcoNeTool.bak.*/data 2>/dev/null; df -h /srv; grep -n 'directory_index\|site_dir' /etc/shiny-server/shiny-server.conf; ls -la /srv/shiny-server/EcoNeTool/ /srv/shiny-server/EcoNeTool/config/; ls /srv/shiny-server/EcoNeTool/models 2>&1; ls -la /home/razinka/backups 2>&1 | tail -5"
```
Record: whether `backups/` exists (expected: no), the size of the stray dir and its `data/`, free space, the `directory_index on;` line number in the `location /` block (about 216), the live `config/` listing, and whether live `models/` exists (if it does not, this deploy is the first to ship it - the ML trait tier becomes available in production).

- [ ] **Step 2 (USER runs, sudo): Backup directory outside site_dir**

```bash
sudo mkdir -p /srv/shiny-server-data/EcoNeTool/backups
sudo chown razinka:shiny /srv/shiny-server-data/EcoNeTool/backups
sudo chmod 700 /srv/shiny-server-data/EcoNeTool/backups
```
(Owner razinka so on-server runs of `deploy.sh` without sudo can write there; root-run `deployment/deploy.sh` can write anyway.)

- [ ] **Step 3 (USER runs, sudo): Remove the stray backup from site_dir**

It holds a full old tree including `config/api_keys.R`; with `app.R` inside site_dir, shiny-server would even run it at `:3838/EcoNeTool.bak.20260510_192058/`. Pick one:

Option A - keep a private archive (code + config; its old `data/` is superseded by the live `data/`):
```bash
sudo tar --exclude=EcoNeTool.bak.20260510_192058/data -czf /srv/shiny-server-data/EcoNeTool/backups/EcoNeTool.bak.20260510_192058.tar.gz -C /srv/shiny-server EcoNeTool.bak.20260510_192058
sudo chmod 600 /srv/shiny-server-data/EcoNeTool/backups/EcoNeTool.bak.20260510_192058.tar.gz
sudo tar -tzf /srv/shiny-server-data/EcoNeTool/backups/EcoNeTool.bak.20260510_192058.tar.gz | grep -c '^EcoNeTool.bak.20260510_192058/app.R$'
sudo rm -rf /srv/shiny-server/EcoNeTool.bak.20260510_192058
```
(the `grep -c` must print `1` before the `rm`; drop the `--exclude` if the old `data/` should be kept too - check free space from Step 1 first).

Option B - it is not needed (4.5 months old, pre-1.4.5):
```bash
sudo rm -rf /srv/shiny-server/EcoNeTool.bak.20260510_192058
```

- [ ] **Step 4 (USER runs, sudo): Turn off the site-root directory index**

```bash
sudo cp /etc/shiny-server/shiny-server.conf /etc/shiny-server/shiny-server.conf.bak-$(date +%Y%m%d)
sudo grep -n 'directory_index\|site_dir\|location' /etc/shiny-server/shiny-server.conf
sudo nano +216 /etc/shiny-server/shiny-server.conf
```
In the `location / { site_dir /srv/shiny-server; ... }` block only, change `directory_index on;` to `directory_index off;` (leave other apps' blocks alone). Then reload, not restart (about 20 apps share the server):
```bash
sudo systemctl reload shiny-server
systemctl is-active shiny-server
```
Expected: `active`. If `reload` reports "Job type reload is not applicable", use `sudo kill -HUP $(pidof shiny-server)` (shiny-server re-reads its config on SIGHUP) and check `systemctl is-active shiny-server` again. This hides the index at `laguna.ku.lt:3838/`, which is internal only (nginx proxies only the listed apps); tell the user in case someone uses that page.

- [ ] **Step 5 (STOP - ask the user): Verify the F7 cleanup**

```bash
ssh razinka@laguna.ku.lt "ls -d /srv/shiny-server/*.bak* 2>/dev/null | wc -l; curl -s -o /dev/null -w '%{http_code}\n' http://localhost:3838/; curl -s -o /dev/null -w '%{http_code}\n' http://localhost:3838/EcoNeTool.bak.20260510_192058/; ls -ld /srv/shiny-server-data/EcoNeTool/backups; ls -la /srv/shiny-server-data/EcoNeTool/backups"
curl -s -o /dev/null -w "%{http_code}\n" https://laguna.ku.lt/backups/
curl -sL -o /dev/null -w "%{http_code}\n" http://laguna.ku.lt/EcoNeTool/
```
Expected: `0` stray dirs; neither `:3838/` nor the old backup URL returns `200`; backups dir `drwx------ razinka shiny`; public `/backups/` not `200`; the app `200`.

- [ ] **Step 6 (STOP - ask the user): Pre-deploy check, from inside `deployment/`**

```bash
cd deployment && "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" pre-deploy-check.R; echo "exit=$?"; cd ..
```
Expected: `✓ Syntax: <n> R files parse`, no `✗` lines, `exit=0`.

- [ ] **Step 7 (STOP - ask the user): Upload code only with the NEW script**

No manual `rm -rf /home/razinka/EcoNeTool_staging` and no strip step: the script empties staging and never uploads runtime config.
```bash
powershell ./deploy-windows.ps1 -SkipData -NoSudo
```
Expected: `Backup created: /home/razinka/backups/EcoNeTool_backup_<ts>.tar.gz`, `Staged: config/ (1 files, ...)`, `Staged: models/ (1 files, ...)`, `DEPLOYMENT SUCCESSFUL`, and the NEXT STEPS text saying there is nothing to strip.

- [ ] **Step 8 (STOP - ask the user): Check staging before copying**

```bash
ssh razinka@laguna.ku.lt "cd /home/razinka/EcoNeTool_staging && ls -A && ls -A config/ && test ! -e config/api_keys.R && test ! -e config/api_keys.json && test ! -e config/harmonization_custom.json && test ! -e .Renviron && test ! -d data && test -f models/trait_ml_models.rds && grep '^VERSION=' VERSION && echo STAGING_OK; ls -la /home/razinka/backups | tail -3"
```
Expected: `config/` lists only `api_keys.R.template`; `VERSION=1.5.1`; `STAGING_OK`; the new `EcoNeTool_backup_<ts>.tar.gz` is `-rw-------`. Any failed `test` stops the chain before `STAGING_OK` - then do not copy; report.

(Old `cp -r` backup directories from earlier deploys may still sit in `/home/razinka/backups/`; the new rotation only touches `*.tar.gz`. Offer the user to delete them: `ssh razinka@laguna.ku.lt "ls -d /home/razinka/backups/EcoNeTool_backup_*/"` then `rm -rf` the listed dirs after they confirm.)

- [ ] **Step 9 (STOP - ask the user): Copy into place and reload**

```bash
ssh razinka@laguna.ku.lt "cp -rT /home/razinka/EcoNeTool_staging /srv/shiny-server/EcoNeTool && touch /srv/shiny-server/EcoNeTool/restart.txt"
```
Never `rm -rf /srv/shiny-server/EcoNeTool/*` (it would delete the live ~3.1 GB `data/`).

- [ ] **Step 10: Verify live**

```bash
ssh razinka@laguna.ku.lt "grep '^VERSION=' /srv/shiny-server/EcoNeTool/VERSION; ls -la /srv/shiny-server/EcoNeTool/config/ /srv/shiny-server/EcoNeTool/.Renviron /srv/shiny-server/EcoNeTool/models/; du -sh /srv/shiny-server/EcoNeTool/data; stat -c '%y' /srv/shiny-server/EcoNeTool/restart.txt"
curl -sL -o /dev/null -w "%{http_code}\n" http://laguna.ku.lt/EcoNeTool/
```
Expected: `VERSION=1.5.1`; `config/` unchanged from the Step 1 listing apart from `api_keys.R.template`'s mtime; `.Renviron` still present; `models/trait_ml_models.rds` present; `data/` about 3.1 GB; `restart.txt` time is now; HTTP `200`. Open the app once and load a dataset to confirm it starts.

Reminder for B1/B3 (not this task): `ECONETOOL_ADMIN_PASSWORD_HASH` is not set in the live `.Renviron`; after B1/B3 strict-gated actions are refused until the user sets it via `set_admin_password()`.

---

## Self-review (done while writing)

- **Spec coverage (4.2):** F86 -> Task 1 (helper, rewritten guard, `skip_if` at the five listed lines plus nested ifs). F84 -> Task 2 (local `skip_if_offline` deleted; gate on the tests at L58, L76, L86, L91, L127, L136, L141, L162, L171, L176, L190; EMODnet L105 gated when `Btrait` is installed; L37/L46/L53/L115/L212 ungated; `with_timeout(..., 15)`). F81 -> Task 3. F4 -> Task 4 (live `$preserve`, `-NoSudo` `$preserve = ""` plus a wipe that also covers the scp path, `models/`). F5 -> Tasks 3, 4, 5 (`Test-ShouldExclude` relative-path case in 4f). F7 -> Tasks 3, 4, 5 (script side) and Task 12 Steps 2-5 (server side, USER sudo). F83 -> Task 7. F82 -> Task 8. Section 5 B2 rows -> Tasks 1, 2, 7 plus the per-script blocks in 3-5. Controller ruling (never write `/etc/shiny-server/`) -> Task 6 (guard over every root and `deployment/` `*.sh`/`*.ps1`, plus the snippet print). Section 6 (VERSION, CHANGELOG, CONTRIBUTING line) -> Tasks 9-10; deploy -> Task 12. Section 8: item 5 -> Task 5 Step 6; item 6 -> Tasks 7-8; item 7 -> Task 2; item 11 -> Task 9 Step 2.
- **Deviations from the spec, with reasons:**
  - Per-script test blocks sharing `DEPLOY_PROTECTED` / `RUNTIME_CONFIG_FILES` instead of one table: each script task owns a red-green cycle and every commit stays green.
  - `code_lines()` also joins `\` continuations: `deployment/deploy.sh`'s `find` spans two lines, and the dotfile keep sits on the second.
  - Spec says "Heredocs are not used in these scripts"; both `.sh` files have one (help text, fallback conf). Their bodies are treated as code text; the fallback conf's `directory_index on` is fixed too (Task 3f).
  - `.Renviron` added to the runtime excludes (task brief), and `".*"` plus `/data/` to root `deploy.sh`: with `rsync --delete`, excluding only `.Renviron` leaves every other server dotfile deletable, and shipping local `data/` deletes server-only data files.
  - ps1 backups exclude `data/` (60 s `Invoke-RemoteCommand` limit, see Review Focus); `deployment/deploy.sh` (runs on the server, no timeout) keeps a full tar as the spec says.
  - The ps1 `-NoSudo` staging wipe happens at the start of `Deploy-Application` (both upload paths), not only in the tar branch where `$preserve` lives; `$preserve = ""` is still set there as the spec says.
  - `Test-ShouldExclude` uses `continue` for inner-slash patterns, so `config/api_keys.R` never matches by leaf name (`R/functions/api_keys.R` stays shipped).
  - Two routing tests in `test-trait-lookup-unit.R` (L294, L313) have the same whole-test `if (success)` gate as the spec's list and are converted too.
  - `expect_gte(length(gated), 12L)` is exact today (12 gated tests); it catches a parse that finds no tests.
  - pre-deploy F83 is a function in the script so the test can run it without the script's top-level `setwd("..")` / `quit()`.
  - Root `deploy.sh` keeps `"models/*"` excluded (it never shipped `models/`; spec non-goal: making it work without rsync).
  - Task 6 is not in the spec: it implements the 2026-09-27 controller ruling. It removes the Task 3f heredoc it edited (Task 3 stays green on its own commit), and it also drops the `cp` backup of the live conf into `/etc/shiny-server/` - itself a write there.
- **Placeholders:** none. `<today, YYYY-MM-DD ...>` in Task 10 Step 2 and `<ts>`/`<n>` in Task 12 are values known only at execution time.
- **Type/name consistency:** `code_lines`, `script_array`, `protected_deployment_sh`, `protected_windows_ps1`, `protected_root_sh`, `backup_dirs`, `DEPLOY_PROTECTED`, `RUNTIME_CONFIG_FILES`, `deploy_file` are defined in Task 1 and used unchanged in Tasks 3-6; `writes_etc_shiny()` and `deploy_scripts()` live only in Task 6's block. `collect_r_syntax_errors(root)` returns `list(files, errors)` in Task 7 and the test reads exactly those. Job id `testthat-offline` is the same in the YAML, `needs`, the status script and the test.
- **Review Focus:** five items, each pinned by a named test in Tasks 1 and 4, except CI-on-Linux, which Task 11 Step 3 handles as a STOP.
