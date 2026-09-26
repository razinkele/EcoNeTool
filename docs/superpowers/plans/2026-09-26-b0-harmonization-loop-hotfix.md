# B0: Harmonization Slider Loop Hotfix (F1/F76) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Stop the harmonization-settings observers from re-triggering each other forever (one slider move pins the single R process on laguna), ship it as v1.4.5 with all version strings aligned, and deploy it.

**Architecture:** `R/modules/harmonization_settings_server.R` loads `config/harmonization_custom.json` once, eagerly, at module start (outside any reactive). A run-once `observeEvent(TRUE, ..., once = TRUE)` pushes those thresholds to the six sliders. The UPDATE observer depends only on the six slider inputs: it reads `rv$config` under `isolate()` and writes it once. The fix is proven by the repo's first `shiny::testServer()` tests, which bound the number of JSON loads with a counting shim that `stop()`s after 5 calls, so the pre-fix infinite loop becomes a fast test failure.

**Tech Stack:** R 4.4.1, shiny 1.11.1 (`testServer`, `MockShinySession`), testthat 3.3.2, withr, jsonlite. Windows dev box (Git Bash), Linux deploy target (laguna.ku.lt, shiny-server).

**Spec:** `docs/superpowers/specs/2026-09-26-fix-b-platform-safety-design.md` section 4.0 (Phase B0), section 5 (first table row), section 6 ("B0 specifics") and section 8 item 1; plus `docs/superpowers/specs/2026-09-26-fix-overview.md` (merge order row 1, shared rules for every PR).

## Global Constraints

- Version for this PR: **1.4.5**. "`VERSION` reads 1.4.4 today, while `R/config.R:294` reads 1.4.2 and the CHANGELOG head reads 1.4.3. B0 aligns all three." `app.R`'s header comment is aligned too.
- Scope (spec 4.0): "No other changes: no gating and no UI changes. The PR diff is this file, one test file, `VERSION` and `CHANGELOG.md`." This plan adds exactly two more files for the version alignment: `R/config.R` (fallback list) and `app.R` (header comment).
- Error handling unchanged: "`load_harmonization_config` already warns and returns defaults when the JSON is unparseable."
- Never write `HARMONIZATION_CONFIG` into `globalenv()` from the module; per-session state lives in `session$userData$harm_config` (read through `get_harm_config()`).
- Error handlers use `warning()`, not `message()`; mutating outer scope from an error closure uses `<<-`.
- Tests: never `if (cond) expect_*()`; use `skip_if()` / `skip_if_not_installed()` with a reason.
- lintr: 120-char lines, `<-` assignment, no tabs, no trailing whitespace. Parse-check every edited `.R` file.
- Test commands: single file `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/<file>')"`; full suite `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_dir('tests/testthat')"`. The legacy `tests/run_all_tests.R` needs `dggridR` and cannot run locally; do not use it.
- Branch: `fix/b0-harmonization-loop` from `master`. Every commit message ends with `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`.
- `git add` explicit paths only. The working tree has an unrelated untracked `WBGIFSV5ISSUE70.pdf`; never stage it.
- Deploy steps are outward-facing: each one is marked **STOP - ask the user before running** and must not be executed without explicit user confirmation in the conversation.

## Review Focus

- **Browser start-up echo:** a real browser first reports the UI's built-in slider values (MS3_MS4 = 5), then the file value (7) after `updateSliderInput()` round-trips. Expected: the session config settles on 7 with exactly one JSON load. (Known, pre-existing and out of B0 scope: `rv$unsaved_changes` flips to TRUE at start-up, and for one flush `session$userData$harm_config` holds the UI defaults. Not a loop; B1 adds the equal-value early return.) Pinned by Task 1 test "browser start-up echo ...".
- **Unparseable `harmonization_custom.json`:** the session starts on the built-in defaults, the parse warning is emitted, the file is read at most once, and sliders still work. Pinned by Task 1 test "an unparseable server-default file ...".
- **No `harmonization_custom.json` at all (fresh install, or production if the user decides to remove the live copy - `cp -rT` never deletes it):** defaults, zero loads, sliders work. Pinned by Task 1 test "without a server-default file ...".
- **Partial JSON (a threshold key missing):** `updateSliderInput(value = NULL)` is a no-op, the session does not error, and the first slider move fills all six thresholds. Pinned by Task 1 test "a server-default file missing a threshold ...".
- **Concurrent sessions:** a slider move in one session must not change the process-wide `HARMONIZATION_CONFIG` or another session's `harm_config` (the PR9-alpha contamination class). Pinned by Task 1 test "a slider move in one session leaves ...".

Also known and deliberately left for B1 (not tested here): JSON import does not push values to the sliders or to `session$userData`, and "Reset to Defaults" uses hard-coded built-ins rather than the loaded file.

---

## File Structure

| File | Change | Responsibility |
|---|---|---|
| `R/modules/harmonization_settings_server.R` | Modify lines 1-46 (everything above `  # SAVE CONFIGURATION.`) | Eager one-time config load, run-once slider push, isolated UPDATE observer |
| `tests/testthat/test-harmonization-settings-server.R` | Create | First `testServer` tests; bounded-load regression for F1/F76 plus Review Focus cases |
| `VERSION` | Modify | 1.4.4 -> 1.4.5 |
| `R/config.R` | Modify lines 293-301 (`load_version_info()` fallback list) | 1.4.2 -> 1.4.5 |
| `app.R` | Modify line 46 (header comment) | v1.4.4 -> v1.4.5 |
| `CHANGELOG.md` | Regenerate with `scripts/generate_changelog.R --version 1.4.5` | New `## [1.4.5]` head section |

Deploy scripts are NOT modified (that is B2). Task 3 works around their pre-B2 defects by hand.

---

### Task 0: Branch and baseline

**Files:** none modified (the untracked plan file is committed on the branch).

**Interfaces:**
- Consumes: nothing.
- Produces: branch `fix/b0-harmonization-loop` containing the specs and this plan; recorded baseline suite counts (PASS / FAIL / SKIP) that Task 1 and Task 2 compare against.

- [ ] **Step 1: Create the branch from master and bring in the specs**

The specs and plans were merged to `master` before execution (the user decided on 2026-09-26 that the docs branch goes in first, so fix PRs stay code-only). Branch straight from master.

```bash
cd "/c/Users/arturas.baziukas/OneDrive - ku.lt/HORIZON_EUROPE/MARBEFES/Traits/Networks/EcoNeTool"
git checkout master
git checkout -b fix/b0-harmonization-loop
```

Expected: `docs/superpowers/specs/2026-09-26-fix-b-platform-safety-design.md` and this plan exist on the branch.

- [ ] **Step 2: Confirm the plan is already tracked**

The plan reached master with the docs merge (force-added, because `docs/superpowers/plans/` is in `.gitignore:98`).

```bash
git ls-files docs/superpowers/plans/2026-09-26-b0-harmonization-loop-hotfix.md
```

Expected: the path is printed. Nothing to commit.

- [ ] **Step 3: Record the baseline full-suite counts**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "r <- as.data.frame(testthat::test_dir('tests/testthat', reporter = 'summary', stop_on_failure = FALSE)); cat('BASELINE pass', sum(r\$passed), 'fail', sum(r\$failed), 'skip', sum(r\$skipped), 'error', sum(r\$error), '\n')"
```

Expected: `fail 0 ... error 0` (the deep-analysis remediation left the suite green; not re-verified while writing this plan). Write the `BASELINE pass N fail 0 skip M` line into your task notes; Task 1 Step 6 expects `pass N + 23`, `fail 0`, `skip M`. If the baseline already has failures, stop and report them - do not start B0 on a red suite.

---

### Task 1: Fix the F1/F76 observer loop (test first)

**Files:**
- Create: `tests/testthat/test-harmonization-settings-server.R`
- Modify: `R/modules/harmonization_settings_server.R:1-46`

**Interfaces:**
- Consumes (unchanged, from `R/config/harmonization_config.R`): globals `HARMONIZATION_CONFIG` (list with `$size_thresholds$MS1_MS2 ... $MS6_MS7`, built-in MS3_MS4 = 5.0), `HARMONIZATION_CONFIG_FILE` (character path), `load_harmonization_config(file = HARMONIZATION_CONFIG_FILE)` -> list (warns and returns `HARMONIZATION_CONFIG` on unparseable JSON), `save_harmonization_config(config, file)` (writes JSON, emits a `message()`).
- Consumes: module signature `harmonization_settings_server(input, output, session)` - a plain server function called from `app.R:697`, NOT a `moduleServer()` module. `shiny::testServer(harmonization_settings_server, { ... })` drives it directly (no `id` argument); inside the block, `session`, `input`, and the module's local `rv` are in scope.
- Produces: `session$userData$harm_config` is set before the first flush (seeded from the JSON when present), and afterwards mirrors the six slider inputs. Slider input IDs: `harm_thresh_MS1_MS2`, `harm_thresh_MS2_MS3`, `harm_thresh_MS3_MS4`, `harm_thresh_MS4_MS5`, `harm_thresh_MS5_MS6`, `harm_thresh_MS6_MS7`. `rv$config` / `rv$unsaved_changes` keep their names (the SAVE, RESET, preview, profile, export and import handlers below line 46 are untouched).

- [ ] **Step 1: Write the failing test file**

Create `tests/testthat/test-harmonization-settings-server.R` with exactly this content:

```r
# Regression tests for F1/F76 (deep analysis 2026-09-26, spec B section 4.0).
# Pre-B0 the INITIALIZE observe() in harmonization_settings_server.R re-read
# config/harmonization_custom.json and read rv$config reactively, while the
# UPDATE observe() wrote rv$config from the sliders. Each observer
# re-triggered the other, so one slider move pinned the single R process.
# These are the repo's first shiny::testServer() tests; testServer accepts
# the module's plain function(input, output, session) signature directly.

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")

source_harm_module <- function() {
  source(file.path(app_root, "R/functions/validation_utils.R"), local = FALSE)
  source(file.path(app_root, "R/config/harmonization_config.R"), local = FALSE)
  source(file.path(app_root, "R/modules/harmonization_settings_server.R"), local = FALSE)
}

# Point HARMONIZATION_CONFIG_FILE at `path` and wrap load_harmonization_config
# in a counter that stop()s once it has been called more than `max_loads`
# times. The stop turns the pre-fix infinite observer loop into a fast test
# failure instead of a hung R process. Both globals are restored when the
# calling test_that() block exits.
local_harm_loader <- function(path, max_loads = 5L, env = parent.frame()) {
  old_file <- get("HARMONIZATION_CONFIG_FILE", envir = globalenv())
  old_loader <- get("load_harmonization_config", envir = globalenv())
  calls <- new.env()
  calls$n <- 0L
  assign("HARMONIZATION_CONFIG_FILE", path, envir = globalenv())
  assign("load_harmonization_config", function(file = HARMONIZATION_CONFIG_FILE) {
    calls$n <- calls$n + 1L
    if (calls$n > max_loads) {
      stop(sprintf("load_harmonization_config called %d times: observer loop", calls$n))
    }
    old_loader(file)
  }, envir = globalenv())
  withr::defer({
    assign("HARMONIZATION_CONFIG_FILE", old_file, envir = globalenv())
    assign("load_harmonization_config", old_loader, envir = globalenv())
  }, envir = env)
  calls
}

# Write a server-default JSON whose MS3_MS4 differs from the built-in 5.
write_harm_json <- function(ms3_ms4, env = parent.frame()) {
  path <- tempfile("harm_cfg_", fileext = ".json")
  cfg <- HARMONIZATION_CONFIG
  cfg$size_thresholds$MS3_MS4 <- ms3_ms4
  suppressMessages(save_harmonization_config(cfg, path))
  withr::defer(unlink(path), envir = env)
  path
}

set_all_thresholds <- function(session, ms3_ms4) {
  session$setInputs(
    harm_thresh_MS1_MS2 = 0.1, harm_thresh_MS2_MS3 = 1.0,
    harm_thresh_MS3_MS4 = ms3_ms4, harm_thresh_MS4_MS5 = 20.0,
    harm_thresh_MS5_MS6 = 50.0, harm_thresh_MS6_MS7 = 150.0
  )
}

test_that("slider change settles and is not reverted by the server-default file", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  calls <- local_harm_loader(write_harm_json(ms3_ms4 = 7))

  shiny::testServer(harmonization_settings_server, {
    expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4, 7,
                 info = "session config is seeded from the server-default JSON")
    set_all_thresholds(session, ms3_ms4 = 9)
    expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4, 9,
                 info = "slider value must survive; pre-B0 the JSON reload reverted it to 7")
    expect_equal(shiny::isolate(rv$config$size_thresholds$MS3_MS4), 9)
  })

  expect_lte(calls$n, 1L)
})

test_that("repeated slider moves never reload the server-default file", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  calls <- local_harm_loader(write_harm_json(ms3_ms4 = 7))

  shiny::testServer(harmonization_settings_server, {
    for (v in c(8, 9, 10, 7, 12)) set_all_thresholds(session, ms3_ms4 = v)
    expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4, 12)
  })

  expect_equal(calls$n, 1L)
})

test_that("without a server-default file the session starts from the built-in defaults", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  calls <- local_harm_loader(file.path(tempdir(), "no_such_harm_cfg.json"))

  shiny::testServer(harmonization_settings_server, {
    expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4,
                 HARMONIZATION_CONFIG$size_thresholds$MS3_MS4)
    set_all_thresholds(session, ms3_ms4 = 9)
    expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4, 9)
  })

  expect_equal(calls$n, 0L)
})

test_that("an unparseable server-default file warns once and falls back to defaults", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  bad <- tempfile("harm_bad_", fileext = ".json")
  writeLines("{ not valid json", bad)
  withr::defer(unlink(bad))
  calls <- local_harm_loader(bad)

  expect_warning(
    shiny::testServer(harmonization_settings_server, {
      expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4,
                   HARMONIZATION_CONFIG$size_thresholds$MS3_MS4)
      set_all_thresholds(session, ms3_ms4 = 9)
      expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4, 9)
    }),
    "could not parse"
  )
  expect_lte(calls$n, 1L)
})

test_that("browser start-up echo (UI defaults, then the file value) settles on the file value", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  calls <- local_harm_loader(write_harm_json(ms3_ms4 = 7))

  shiny::testServer(harmonization_settings_server, {
    # A real browser first reports the sliderInput() defaults from the UI
    # (MS3_MS4 = 5), then the value pushed by updateSliderInput() (7).
    set_all_thresholds(session, ms3_ms4 = 5)
    set_all_thresholds(session, ms3_ms4 = 7)
    expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4, 7)
  })

  expect_equal(calls$n, 1L)
})

test_that("a server-default file missing a threshold still starts and accepts slider moves", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  path <- tempfile("harm_partial_", fileext = ".json")
  cfg <- HARMONIZATION_CONFIG
  cfg$size_thresholds$MS6_MS7 <- NULL
  suppressMessages(save_harmonization_config(cfg, path))
  withr::defer(unlink(path))
  calls <- local_harm_loader(path)

  shiny::testServer(harmonization_settings_server, {
    expect_null(session$userData$harm_config$size_thresholds$MS6_MS7)
    set_all_thresholds(session, ms3_ms4 = 9)
    expect_equal(session$userData$harm_config$size_thresholds$MS6_MS7, 150)
    expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4, 9)
  })

  expect_equal(calls$n, 1L)
})

test_that("a slider move in one session leaves the process-wide default and other sessions alone", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  calls <- local_harm_loader(write_harm_json(ms3_ms4 = 7))
  default_before <- HARMONIZATION_CONFIG

  other_cfg <- NULL
  shiny::testServer(harmonization_settings_server, {
    other_cfg <<- session$userData$harm_config
  })
  shiny::testServer(harmonization_settings_server, {
    set_all_thresholds(session, ms3_ms4 = 9)
    expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4, 9)
  })

  expect_identical(HARMONIZATION_CONFIG, default_before)
  expect_equal(other_cfg$size_thresholds$MS3_MS4, 7)
  expect_equal(calls$n, 2L)
})
```

Notes for the implementer:
- `local_harm_loader()` replaces two globals and restores them with `withr::defer(..., envir = env)`, i.e. when the calling `test_that()` block exits. The module finds `load_harmonization_config` and `HARMONIZATION_CONFIG_FILE` lexically in `globalenv()` (it is `source()`d with `local = FALSE`), so the shim is what it calls.
- `source_harm_module()` sources `validation_utils.R` first so `HARMONIZATION_CONFIG_FILE` is repo-absolute when restored; `test-config-paths.R:130` checks that property.

- [ ] **Step 2: Run the new test and verify it fails**

Run:
```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-harmonization-settings-server.R')"
```

Expected (verified on a scratch copy of the pre-fix module at `7a96789`; finishes in about 7 s instead of hanging): `[ FAIL 11 | WARN 6 | SKIP 0 | PASS 12 ]`. Details:
- Five warnings read `Error in load_harmonization_config: load_harmonization_config called 6 times: observer loop` (testServer reports the observer error as a warning; the counting shim is what stops the infinite loop). The sixth warning is `package 'shiny' was built under R version 4.4.3` (harmless; appears the first time `testServer` attaches shiny in a process).
- Failures include `Expected calls$n <= 1L. Actual comparison: 6 > 1` and `Expected ...MS3_MS4 to equal 12` (got 8: the reloaded JSON value / a stale slider value won).
- The first assertion ("seeded from the server-default JSON", expected 7) also fails pre-fix: seeding used to happen inside an `observe()`, i.e. only after the first flush. That is part of the spec's behaviour change ("reads ... once per session at module start"), not a test bug.
- Test "without a server-default file ..." passes pre-fix (no file means no reload, so no loop). It is a guard for the fallback path, not a regression test.

- [ ] **Step 3: Implement the fix**

In `R/modules/harmonization_settings_server.R`, replace lines 1-46 - from `# Harmonization Settings Server Module` down to and including the `  })` that closes the UPDATE `observe({ ... })` (the line just above the blank line and `  # SAVE CONFIGURATION. Pre-PR9α also did`) - with exactly:

```r
# Harmonization Settings Server Module

harmonization_settings_server <- function(input, output, session) {

  # Read the server-default JSON ONCE per session, outside any reactive.
  # Pre-B0 (F1/F76) this load lived in an observe() that also read
  # rv$config, while the slider observer below wrote rv$config: the two
  # observers re-triggered each other forever and pinned the R process.
  initial_cfg <- if (file.exists(HARMONIZATION_CONFIG_FILE)) {
    load_harmonization_config(HARMONIZATION_CONFIG_FILE)
  } else {
    HARMONIZATION_CONFIG
  }

  rv <- reactiveValues(
    config = initial_cfg,
    unsaved_changes = FALSE
  )

  # Seed the per-session harm config so the harmonize_* helpers (which
  # read through get_harm_config() -> session$userData) see the server
  # default from page load. Never write HARMONIZATION_CONFIG to globalenv:
  # that contaminated every concurrent session (pre-PR9α).
  session$userData$harm_config <- initial_cfg

  # INITIALIZE: push the loaded thresholds to the widgets once. No
  # reactive read of rv$config, so nothing can re-trigger this.
  observeEvent(TRUE, {
    thr <- initial_cfg$size_thresholds
    updateSliderInput(session, "harm_thresh_MS1_MS2", value = thr$MS1_MS2)
    updateSliderInput(session, "harm_thresh_MS2_MS3", value = thr$MS2_MS3)
    updateSliderInput(session, "harm_thresh_MS3_MS4", value = thr$MS3_MS4)
    updateSliderInput(session, "harm_thresh_MS4_MS5", value = thr$MS4_MS5)
    updateSliderInput(session, "harm_thresh_MS5_MS6", value = thr$MS5_MS6)
    updateSliderInput(session, "harm_thresh_MS6_MS7", value = thr$MS6_MS7)
  }, once = TRUE)

  # UPDATE CONFIG. Depends on the six slider inputs only: rv$config is
  # read under isolate() and written once, so the write cannot re-trigger
  # this observer. Mirror into session$userData so the helpers see the
  # user's thresholds for THIS session immediately; cross-session
  # persistence still routes through the JSON save below.
  observe({
    req(input$harm_thresh_MS1_MS2, input$harm_thresh_MS2_MS3,
        input$harm_thresh_MS3_MS4, input$harm_thresh_MS4_MS5,
        input$harm_thresh_MS5_MS6, input$harm_thresh_MS6_MS7)
    cfg <- isolate(rv$config)
    cfg$size_thresholds$MS1_MS2 <- input$harm_thresh_MS1_MS2
    cfg$size_thresholds$MS2_MS3 <- input$harm_thresh_MS2_MS3
    cfg$size_thresholds$MS3_MS4 <- input$harm_thresh_MS3_MS4
    cfg$size_thresholds$MS4_MS5 <- input$harm_thresh_MS4_MS5
    cfg$size_thresholds$MS5_MS6 <- input$harm_thresh_MS5_MS6
    cfg$size_thresholds$MS6_MS7 <- input$harm_thresh_MS6_MS7
    rv$config <- cfg
    session$userData$harm_config <- cfg
    rv$unsaved_changes <- TRUE
  })
```

Leave everything from `  # SAVE CONFIGURATION.` to the end of the file unchanged.

What changed and why (for the reviewer):
- The JSON load moved out of the INITIALIZE `observe()` to module top, before `rv` exists: it runs once per session and has no reactive dependencies.
- INITIALIZE is `observeEvent(TRUE, ..., once = TRUE)` and reads only the plain local `initial_cfg`, never `rv$config`, so nothing can invalidate it.
- UPDATE takes `cfg <- isolate(rv$config)`, so its only dependencies are the six inputs. Its write to `rv$config` can no longer re-trigger itself or INITIALIZE.

- [ ] **Step 4: Parse-check and lint the edited files**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='R/modules/harmonization_settings_server.R'); parse(file='tests/testthat/test-harmonization-settings-server.R'); cat('OK\n')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "print(lintr::lint('R/modules/harmonization_settings_server.R')); print(lintr::lint('tests/testthat/test-harmonization-settings-server.R'))"
```

Expected: `OK`. lintr: no line-length, assignment, whitespace or trailing-whitespace lints. The only non-`object_usage` lint on the module is the pre-existing `commented_code_linter` hit on the `#   assign("HARMONIZATION_CONFIG", ...)` comment in the SAVE block (untouched). `object_usage_linter` "no visible global function definition for 'observe'" etc. are the repo-wide Shiny noise and are expected.

- [ ] **Step 5: Run the new test and verify it passes**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-harmonization-settings-server.R')"
```

Expected (verified on a scratch copy with this exact fix): `[ FAIL 0 | WARN 1 | SKIP 0 | PASS 23 ]`. The single warning is the `package 'shiny' was built under R version 4.4.3` notice; there must be no `observer loop` warning.

- [ ] **Step 6: Run the neighbouring guards and the full suite**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-config-paths.R'); testthat::test_file('tests/testthat/test-session-isolation.R'); testthat::test_file('tests/testthat/test-deep-analysis-fixes.R')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "r <- as.data.frame(testthat::test_dir('tests/testthat', reporter = 'summary', stop_on_failure = FALSE)); cat('AFTER pass', sum(r\$passed), 'fail', sum(r\$failed), 'skip', sum(r\$skipped), 'error', sum(r\$error), '\n')"
```

Expected: the three guard files report `FAIL 0` (`test-config-paths.R:138` still finds no literal `"config/harmonization_custom.json"` in the module; the fix uses the constant; `test-deep-analysis-fixes.R` still finds no bare relative `source()` under `R/`). Full suite: `fail 0`, `pass` = baseline + 23, `skip` = baseline.

- [ ] **Step 7: Commit**

```bash
git add R/modules/harmonization_settings_server.R tests/testthat/test-harmonization-settings-server.R
git commit -m "$(cat <<'EOF'
fix(harmonization): load settings once so slider observers cannot loop (F1/F76)

The INITIALIZE observe() re-read config/harmonization_custom.json and read
rv$config reactively while the UPDATE observe() wrote rv$config from the
sliders, so the two re-triggered each other forever and one slider move
pinned the single R process. The JSON is now loaded once at module start,
pushed to the sliders by a run-once observeEvent, and the UPDATE observer
reads rv$config under isolate(). Adds the repo's first testServer tests,
which bound the JSON loads and fail fast on the old code.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 2: Align every version string on 1.4.5 and add the CHANGELOG entry

**Files:**
- Modify: `VERSION`
- Modify: `R/config.R:293-301`
- Modify: `app.R:46`
- Modify: `CHANGELOG.md` (regenerated head section)

**Interfaces:**
- Consumes: Task 1's commit (the CHANGELOG generator reads git history, so the fix commit must exist before Step 4).
- Produces: `VERSION=1.4.5`, fallback `load_version_info()$VERSION == "1.4.5"`, `## [1.4.5] - <date>` at the CHANGELOG head. Task 3 greps for these on the server.

- [ ] **Step 1: Confirm the current drift**

```bash
grep -n "^VERSION=\|^VERSION_NAME=\|^RELEASE_DATE=\|^PATCH=" VERSION
grep -n 'VERSION = "\|VERSION_NAME = "\|RELEASE_DATE = "\|PATCH = ' R/config.R | head -4
grep -n "CURRENT VERSION" app.R
grep -n -m1 "^## \[" CHANGELOG.md
```

Expected: `VERSION=1.4.4` / `PATCH=4`; `R/config.R` 294 `VERSION = "1.4.2"`, 300 `PATCH = 2`; `app.R:46:# CURRENT VERSION: v1.4.4 (2026-04-14)`; `CHANGELOG.md:10:## [1.4.3] - 2026-04-11`. If the lines moved, locate them by content, not number.

- [ ] **Step 2: Edit `VERSION`**

Replace these lines (the rest of the file is unchanged; `GIT_BRANCH=master`, `DEPLOYED_BY=`, `DEPLOYED_ON=` stay):

```
VERSION=1.4.4
VERSION_NAME=Trait Research Stability
RELEASE_DATE=2026-04-14
```
with
```
VERSION=1.4.5
VERSION_NAME=Harmonization Loop Hotfix
RELEASE_DATE=2026-09-26
```

and
```
PATCH=4
```
with
```
PATCH=5
```

Leave `GIT_COMMIT`, `GIT_BRANCH` and `BUILD_DATE` as they are (no deploy script sets them and the spec does not ask for them).

If the release is cut on a later day, use that day in `RELEASE_DATE`, `R/config.R` and `app.R` consistently.

- [ ] **Step 3: Edit the `R/config.R` fallback and the `app.R` header**

In `R/config.R` inside `load_version_info()`, replace:

```r
  version_info <- list(
    VERSION = "1.4.2",
    VERSION_NAME = "Local Databases Integration + Performance & Robustness",
    RELEASE_DATE = "2025-12-26",
    STATUS = "stable",
    MAJOR = 1,
    MINOR = 4,
    PATCH = 2
  )
```
with
```r
  version_info <- list(
    VERSION = "1.4.5",
    VERSION_NAME = "Harmonization Loop Hotfix",
    RELEASE_DATE = "2026-09-26",
    STATUS = "stable",
    MAJOR = 1,
    MINOR = 4,
    PATCH = 5
  )
```

In `app.R`, replace:
```r
# CURRENT VERSION: v1.4.4 (2026-04-14)
```
with
```r
# CURRENT VERSION: v1.4.5 (2026-09-26)
```

Do NOT touch `tests/testthat/test-feedback-store.R:20` (`app_version = "1.4.2"` is a fixture literal, unrelated).

Parse-check:
```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='R/config.R'); parse(file='app.R'); cat('OK\n')"
```
Expected: `OK`.

- [ ] **Step 4: Regenerate the CHANGELOG head section**

`CHANGELOG.md` is generated by `scripts/generate_changelog.R` (the `auto-changelog.yml` workflow only posts PR previews; nothing overwrites the file on push). The generator is additive: it rebuilds from `v*` tags and labels everything since the latest tag (`v1.4.3`) as the given version. 1.4.4 was never tagged, so the new `[1.4.5]` section covers all commits since `v1.4.3`, including this fix. It writes CRLF on Windows, and `.gitattributes` / the `mixed-line-ending --fix=lf` hook want LF, so normalise.

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/generate_changelog.R --version 1.4.5
sed -i 's/\r$//' CHANGELOG.md
git diff --stat CHANGELOG.md
git diff CHANGELOG.md | grep '^-' | grep -v '^---' | wc -l
grep -n -m1 "^## \[" CHANGELOG.md
grep -n "load settings once so slider observers cannot loop" CHANGELOG.md
```

Expected: only insertions (the `-` line count is `0`); the first heading is `## [1.4.5] - 2026-09-26`; the fix commit appears under `### Fixed` as `- **harmonization:** load settings once so slider observers cannot loop (F1/F76) (<sha>)`. The generator also adds a `- **release:** v1.4.3 (773cc93)` line and a `[1.4.5]: .../compare/v1.4.3...HEAD` link at the bottom; both are expected. If the `-` count is not 0, run `git checkout -- CHANGELOG.md` and instead paste the output of `--preview --version 1.4.5` above the `## [1.4.3]` line by hand.

- [ ] **Step 5: Verify alignment and the full suite**

```bash
grep -n "^VERSION=" VERSION
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "source('R/config.R'); cat(ECONETOOL_VERSION\$VERSION, '\n')"
grep -n 'VERSION = "1.4.5"\|PATCH = 5' R/config.R
grep -n "CURRENT VERSION" app.R
grep -n -m1 "^## \[" CHANGELOG.md
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "r <- as.data.frame(testthat::test_dir('tests/testthat', reporter = 'summary', stop_on_failure = FALSE)); cat('pass', sum(r\$passed), 'fail', sum(r\$failed), 'skip', sum(r\$skipped), '\n')"
```

Expected: `VERSION=1.4.5`; the `Rscript` line prints `EcoNeTool 1.4.5 - Harmonization Loop Hotfix` then `1.4.5` (a deprecation notice about `config/api_keys.R` may precede it - unrelated); the grep shows `VERSION = "1.4.5",` and `PATCH = 5`; `v1.4.5 (2026-09-26)`; `## [1.4.5] - 2026-09-26`; suite counts identical to Task 1 Step 6.

- [ ] **Step 6: Commit**

```bash
git add VERSION R/config.R app.R CHANGELOG.md
git commit -m "$(cat <<'EOF'
chore(release): 1.4.5 - align VERSION, config fallback, app header, CHANGELOG

VERSION said 1.4.4, the load_version_info() fallback 1.4.2 and the
CHANGELOG head 1.4.3. All now read 1.4.5 (B0 harmonization loop hotfix).

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

- [ ] **Step 7: Push and open the PR (STOP - ask the user before running)**

```bash
git push -u origin fix/b0-harmonization-loop
gh pr create --base master --title "fix(harmonization): B0 hotfix for the slider observer loop (F1/F76), v1.4.5" --body "$(cat <<'EOF'
Implements spec B section 4.0 (Phase B0). One slider move on the Harmonization tab re-triggered two observers forever and pinned the single R process.

- Load `config/harmonization_custom.json` once at module start; run-once slider push; UPDATE observer reads `rv$config` under `isolate()`.
- First `shiny::testServer` tests: a counting shim on `load_harmonization_config` stops after 5 calls, so the old code fails fast (11 failures) instead of hanging; 23 passes after the fix.
- Version aligned on 1.4.5 (VERSION, `R/config.R` fallback, `app.R` header, CHANGELOG).

Note: CI does not run testthat yet (that is B2), so green CI does not exercise the new test; it was run locally (`test_file` and `test_dir`).

After merge, consider tagging `v1.4.5` (the version-drift CI job only warns without it).

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

Expected: PR URL printed. CI `r-syntax`, `pre-commit`, `repo-hygiene` green; `version-drift` may warn "VERSION file (1.4.5) differs from latest tag (v1.4.3)" - informational only. Tagging `v1.4.5` after merge is the user's decision.

---

### Task 3: Deploy B0 to laguna.ku.lt

Every step below touches production or the shared server. **Each is marked STOP: show the user the exact command and wait for explicit confirmation before running it.** Deploy from the merged `master` (or from the PR branch only if the user says so).

**Files:** none in the repo. Remote: `/home/razinka/EcoNeTool_staging/`, `/srv/shiny-server/EcoNeTool/`.

**Interfaces:**
- Consumes: Task 1 module (`observeEvent(TRUE` marker string in the deployed file), Task 2 `VERSION=1.4.5`.
- Produces: production running 1.4.5 with no `config/harmonization_custom.json` and no `config/api_keys.R` shipped from the dev box.

- [ ] **Step 1: Pre-deploy check (local, read-only; runs from inside `deployment/`)**

```bash
git checkout master && git pull --ff-only
git log -1 --oneline   # expect the B0 merge / chore(release): 1.4.5
cd deployment && "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" pre-deploy-check.R; cd ..
```

Expected: the script reports no errors (it does `setwd("..")`, so it must be run from `deployment/`). Fix anything it flags before continuing.

- [ ] **Step 2: Record production config state and clear staging (STOP - ask the user before running)**

With `-NoSudo`, `deploy-windows.ps1` uses staging as its deploy path and its own cleanup (`find ... ! -name data ! -name cache ! -name r-libs -exec rm -rf`) keeps `data/`, `cache/` and `r-libs/` in staging. With `-SkipData`, a stale `staging/data/` from an earlier deploy would then be `cp -rT`'d over the live `data/` (spec overview "Shared rules", spec B F5). Remove staging entirely; the script recreates it with `mkdir -p`.

```bash
ssh razinka@laguna.ku.lt "ls -la /srv/shiny-server/EcoNeTool/config/; cat /srv/shiny-server/EcoNeTool/VERSION | grep '^VERSION='; rm -rf /home/razinka/EcoNeTool_staging && echo STAGING_CLEARED"
```

Expected: a listing of the live `config/` (record whether `harmonization_custom.json` was present: that file is what armed the loop in production), `VERSION=1.4.4` (or whatever is live), and `STAGING_CLEARED`.

- [ ] **Step 3: Upload code to staging (STOP - ask the user before running)**

```bash
powershell ./deploy-windows.ps1 -SkipData -NoSudo
```

Expected: the upload completes into `/home/razinka/EcoNeTool_staging/`. The script's final `sudo` / `rm -rf /srv/shiny-server/EcoNeTool/*` suggestions must be ignored - do NOT run them (they would delete the live `data/`).

- [ ] **Step 4: Strip dev-box runtime config, copy over live, reload (STOP - ask the user before running)**

The pre-B2 ps1 ships the whole local `config/`, which contains the untracked `config/harmonization_custom.json` and `config/api_keys.R`. Strip both from staging, then copy the contents over the live tree without deleting siblings (`cp -rT`), then reload only this app.

```bash
ssh razinka@laguna.ku.lt "ls -la /srv/shiny-server/EcoNeTool/config/; \
  rm -f /home/razinka/EcoNeTool_staging/config/harmonization_custom.json \
        /home/razinka/EcoNeTool_staging/config/api_keys.R && \
  cp -rT /home/razinka/EcoNeTool_staging /srv/shiny-server/EcoNeTool && \
  touch /srv/shiny-server/EcoNeTool/restart.txt"
```

Expected: the `ls` output, then silent success (exit 0).

- [ ] **Step 5: Verify the deployment (read-only; still show the user before running)**

```bash
ssh razinka@laguna.ku.lt "grep -c 'observeEvent(TRUE' /srv/shiny-server/EcoNeTool/R/modules/harmonization_settings_server.R; grep -c 'isolate(rv\$config)' /srv/shiny-server/EcoNeTool/R/modules/harmonization_settings_server.R; grep '^VERSION=' /srv/shiny-server/EcoNeTool/VERSION; stat -c %y /srv/shiny-server/EcoNeTool/restart.txt; ls /srv/shiny-server/EcoNeTool/data | head -3"
curl -s -o /dev/null -w '%{http_code}\n' http://laguna.ku.lt/EcoNeTool/
```

Expected: `1`, `1`, `VERSION=1.4.5`, a `restart.txt` mtime of a few seconds ago, a non-empty `data/` listing (live data survived), and `200`.

- [ ] **Step 6: Manual production smoke test (user or browser automation, with the user's go-ahead)**

Open `http://laguna.ku.lt/EcoNeTool/` in two browser tabs. In tab A open the Harmonization settings tab and drag the MS3/MS4 slider several times. Expected: tab A stays responsive and the slider stays where it was dropped (it does not snap back); tab B keeps responding to navigation during and after the drags (acceptance criterion 1: "a slider move leaves `/EcoNeTool/` responsive for other sessions"). Report the Step 2 finding (whether production had `harmonization_custom.json`) to the user. Stripping it from staging does not remove a live copy (`cp -rT` never deletes siblings). After B0 a live copy is harmless (read once per session, no loop); whether to delete it is the user's decision - do not `rm` it unasked.

---

## Self-Review

1. **Spec coverage.** Spec 4.0 hoisted load -> Task 1 Step 3 (`initial_cfg`); `observeEvent(TRUE, ..., once = TRUE)` INITIALIZE -> Task 1 Step 3; UPDATE `cfg <- isolate(rv$config)`, one `rv$config <- cfg`, one `session$userData$harm_config <- cfg` -> Task 1 Step 3; "no gating, no UI changes" -> only lines 1-46 of the module change. Spec 5 row "slider change settles" (temp JSON with MS3_MS4 = 7, swapped `HARMONIZATION_CONFIG_FILE` restored on exit, counter that `stop()`s after 5 calls, `setInputs` all six sliders with MS3_MS4 = 9, `counter <= 1`, `harm_config$...$MS3_MS4 == 9`) -> test 1. Overview versions (VERSION 1.4.4, `R/config.R:294` 1.4.2, CHANGELOG 1.4.3 -> 1.4.5) -> Task 2; "every PR bumps VERSION and adds a CHANGELOG entry" -> Task 2 Steps 2 and 4. Spec 6 deploy steps plus "B0 specifics" (strip the two config files, `cp -rT`, `touch restart.txt`, `curl` 200, slider move) and the overview's "clear staging before each upload" -> Task 3. Spec 8 item 1 (fails on `7a96789`, passes after; production stays responsive) -> Task 1 Steps 2/5 and Task 3 Step 6. Overview shared rules (TDD, `test_dir`, parse checks, re-check line numbers) -> Global Constraints, Task 1, Task 2 Step 1.
2. **Placeholder scan.** No TBD/TODO/fill-in values; every code step carries the exact code (the test file and module block are byte-identical to the scratch copies that were run).
3. **Type consistency.** Helper names `source_harm_module()`, `local_harm_loader(path, max_loads, env)`, `write_harm_json(ms3_ms4, env)`, `set_all_thresholds(session, ms3_ms4)` are defined once in the test file and used consistently; module locals `initial_cfg`, `rv`, `cfg`; input IDs match `R/ui/harmonization_settings_ui.R:23-33`.
4. **Review Focus.** Each of the five lines has a test in Task 1's file: start-up echo, unparseable JSON, missing file, partial JSON, and cross-session / global isolation.
