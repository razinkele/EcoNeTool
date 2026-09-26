# B1: Harmonization Settings Safety (F2, F8, F75, F72) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the Harmonization tab safe on the shared laguna process: every widget is session-only and actually reaches the config the harmonizer reads, only an unlocked admin can change the server default (fail-closed when no admin hash is configured), JSON import/export works, and the shared trait cache is keyed on the effective config. Ship it as v1.5.2 and deploy it.

**Architecture:** A new fail-closed `admin_authorized_strict()` in `R/functions/admin_auth.R` gates the two server-default buttons. `R/config/harmonization_config.R` gains one shared validator used by the loader, JSON import and the save button, plus an atomic tmp-then-rename save. The settings module keeps B0's no-loop shape (load once, run-once push, observers read `rv$config` only under `isolate()`), routes every write through one `set_session_config()` helper, drives all widgets from `push_config_to_widgets(cfg)`, and returns early on echoes. The trait cache envelope carries `config_hash = harm_config_hash()`, and readers pass their own hash to `read_cache_field()`.

**Tech Stack:** R 4.4.1, shiny 1.11.1 (`testServer`, `MockShinySession`), testthat 3.3.2, withr, jsonlite 2.0.0, digest 0.6.39 (already a hard dependency of htmltools and used by `R/functions/error_logging.R`; no new package). Windows dev box (Git Bash); Linux deploy target (laguna.ku.lt, shiny-server).

**Spec:** `docs/superpowers/specs/2026-09-26-fix-b-platform-safety-design.md` section 4.1 (Phase B1: F2, F8, F75, F72), section 5 (all rows marked B1 plus `test-admin-auth.R` "strict gate"), section 6 (rollout, "B1/B3 prerequisite"), section 7 (echo-loop risk, cache hit rate) and section 8 items 2, 3, 4 and 11. Also `docs/superpowers/specs/2026-09-26-fix-overview.md` (merge order row 6, shared rules).

> **Execution rulings (controller, 2026-09-27) — these override the tasks below where they conflict:**
> 1. **Release flow:** the fix PR is squash-merged WITHOUT the version bump / CHANGELOG regeneration. The plan's version/CHANGELOG task then runs as a separate release PR cut from the updated master: bump to 1.5.2 (VERSION, `R/config.R` fallback, app.R header, README via `scripts/version_bump.R`), normalise CRLF to LF, set `GIT_BRANCH=master`, regenerate CHANGELOG, re-insert `docs/releases/1.5.0-results-changed.md` under `[1.5.0]`, strip the extra trailing newline, merge it, then `git tag -a v1.5.2` on the merge commit and push the tag. This keeps CHANGELOG hashes valid under squash merges and guarantees the tag the next plan expects exists.
> 2. **Locked-session refusal text** is `Unlock via Trait Research > Configure API Keys first` (the unlock button lives on the Trait Research tab).
> 3. Deploys use the deploy scripts as they exist on master at deploy time (after B2 merges, the hardened scripts); every outward step still STOPs for the user.

## Global Constraints

- Branch: `fix/b1-harmonization-safety`, cut from `master` **after B2 has merged** (merge order B2 -> B1 -> B3). Every commit message ends with `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`.
- Version: "Every PR bumps `VERSION`" to PATCH+1 at merge. Expected: master holds 1.5.1 after B2, so B1 is **1.5.2**. If `VERSION` reads anything else when Task 6 starts, STOP and ask the user.
- Behaviour decision (spec 4.1): "Anyone can move sliders, edit patterns, toggle rules, pick a profile, import JSON, and download their config. All of this is session-only (`session$userData$harm_config`, read through `get_harm_config()`)." "'Save as server default' (relabelled from 'Apply Changes') and 'Reset server default' write `HARMONIZATION_CONFIG_FILE` and require `admin_authorized_strict()`. 'Reset to Defaults' stays session-only and ungated."
- Fail-closed rule (spec 4.1): "Hash unset means fail CLOSED ... 'Admin gate not configured on this instance; server defaults are read-only' plus `warning("[admin auth] ...")`." "If the gate is set but the session is locked, the user sees 'Unlock via Trait Research > Configure API Keys first'." `admin_authorized()` is unchanged (spec non-goal).
- Keep B0's no-loop structure: config loaded once outside any reactive; `observeEvent(TRUE, ..., once = TRUE)` push; the slider observer reads `rv$config` only under `isolate()`. The B0 counting-shim `testServer` tests must keep passing (one assertion changes by spec, see Task 2).
- Never write `HARMONIZATION_CONFIG` into `globalenv()`; read configs through `get_harm_config()` (the one deliberate exception, `harm_default_config_hash()`, mirrors `get_harm_config()`'s own global fallback and never writes).
- CLAUDE.md conventions: `warning()` not `message()` in error handlers; `<<-` in error closures that mutate outer state; `app_path()`; no `if (cond) expect_*()` (use `skip_if()`); `<-` assignment; 120-char lines; no tabs or trailing whitespace. Parse-check every edited `.R` file.
- Test commands (from the repo root): one file `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/<file>')"`; full suite `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_dir('tests/testthat')"`. `tests/run_all_tests.R` needs `dggridR` and cannot run locally. The machine has 16 GB RAM: never run two suites in parallel.
- Tests must never write `config/harmonization_custom.json`: the dev box has an untracked, stale copy (its FS0 still contains `plant|algae|phytoplankton|diatom`; from Task 2 on it is rejected at load, so a dev run of the app warns and uses the built-in defaults). Every test points `HARMONIZATION_CONFIG_FILE` at a temp path.
- There is no local `.Renviron`, so on the dev box "Save as server default" and "Reset server default" are refused by design. That is expected; do not create one to "fix" it.
- `git add` explicit paths only. Never stage the untracked `WBGIFSV5ISSUE70.pdf` or `config/harmonization_custom.json`.
- Outward-facing steps (push, PR, tag, anything on laguna) are marked **STOP - ask the user** and must not run without explicit confirmation. Steps needing sudo are **USER** instructions, never automated (razinka has no passwordless sudo).

## Review Focus

- **Browser start-up echo of empty FS inputs:** the FS text inputs render with `value = ""`, so a real browser reports `""` before the start-up push lands. An empty pattern matches every text (everything would become FS0). Expected: the empty echo is ignored and the config keeps its patterns. Pinned by Task 5 test "an FS pattern edit reaches the consumer after the debounce; bad input keeps the old value".
- **Overlapping slider ranges:** the MS2/MS3 slider goes up to 5 cm and the MS3/MS4 slider down to 1 cm, so the UI itself can produce non-increasing boundaries. Expected: the session keeps working, but "Save as server default" refuses with the validator's "strictly increasing" message and writes nothing. Pinned by Task 5 test "an invalid session config (overlapping sliders) is not saved, even by an admin".
- **One session's unlock must not authorize another live session** (`session$userData$admin_unlocked` is per session; a global would leak). Pinned by Task 5 test "one session's unlock does not authorize another live session" (two `MockShinySession`s alive at once).
- **Server default not writable** (production `config/` is razinka-owned; the app runs as `shiny`). Expected: `warning("[harmonization] save failed: ...")`, a visible "Save failed" status, no partial file. Pinned by Task 5 test "a save that cannot write warns and reports the failure"; the real permission is fixed by a USER step in Task 8.
- **Cache envelopes written before B1 (no `config_hash`):** Expected: a miss, so the first lookup of each species after the upgrade re-queries once, then caches with the hash. Pinned by Task 3 test "read_cache_field treats a different or missing config hash as a miss".

---

## Interfaces produced (B3 and later phases rely on these)

B3's plan (written in parallel) depends on **exactly** the first three items. Everything else is B1-internal but stable.

| Name | Signature / value | Where |
|---|---|---|
| `admin_authorized_strict` | `admin_authorized_strict(unlocked) -> logical(1)`; body `admin_gate_enabled() && isTRUE(unlocked)`: TRUE only when `ECONETOOL_ADMIN_PASSWORD_HASH` is non-blank AND `unlocked` is exactly TRUE | `R/functions/admin_auth.R` |
| Refusal text, gate unset | `"Admin gate not configured on this instance; server defaults are read-only"` (constant `ADMIN_STRICT_MSG_UNSET`) | same |
| Refusal text, gate set, session locked | `"Unlock via Trait Research > Configure API Keys first"` (constant `ADMIN_STRICT_MSG_LOCKED`) | same |
| `admin_strict_refusal` | `admin_strict_refusal(unlocked, action = "admin action") -> NULL \| character(1)`: NULL when allowed; otherwise emits `warning(sprintf("[admin auth] %s refused: %s", action, msg), call. = FALSE)` and returns `msg` (one of the two texts above). B3 MAY use it; B3 may equally emit its own `warning("[admin auth] ...")` with the same two texts | same |
| `HARM_THRESHOLD_KEYS` | `c("MS1_MS2", "MS2_MS3", "MS3_MS4", "MS4_MS5", "MS5_MS6", "MS6_MS7")` | `R/config/harmonization_config.R` |
| `harm_pattern_compiles` | `harm_pattern_compiles(pattern) -> logical(1)` | same |
| `HARM_FS0_DIET_NOUNS` | `c("plant", "algae", "phytoplankton", "diatom", "dinoflagellate", "seaweed", "macroalgae")` | same |
| `validate_harmonization_config` | `validate_harmonization_config(cfg) -> list(ok = logical(1), errors = character(), config = list)`; also rejects an FS0 pattern matching any `HARM_FS0_DIET_NOUNS` word (error text contains "FS0_primary_producer matches diet nouns") | same |
| `save_harmonization_config` | `save_harmonization_config(config = HARMONIZATION_CONFIG, file = HARMONIZATION_CONFIG_FILE) -> invisible(file)`; tmp-then-rename; stops on failure | same |
| `load_harmonization_config` | unchanged signature; now returns the validated, default-filled config; warns and returns `HARMONIZATION_CONFIG` on parse or validation failure | same |
| `export_config_json` | `export_config_json(file, config = HARMONIZATION_CONFIG) -> invisible(TRUE)` | same |
| `import_config_json` | `import_config_json(file) -> list` (validated config); stops with joined validation errors; never assigns globals | same |
| `harm_config_hash` | `harm_config_hash(cfg = get_harm_config()) -> character(1) \| NULL` (xxhash64 of the JSON text, `last_modified` and `version` dropped) | `R/functions/trait_lookup/harmonization.R` |
| `harm_default_config_hash` | `harm_default_config_hash() -> character(1) \| NULL` | same |
| `read_cache_field` | `read_cache_field(cache_file, field, max_age_days = 30, config_hash = NULL)`; non-NULL `config_hash` makes a different or missing envelope hash a miss | `R/functions/validation_utils.R` |
| `HARM_FS_PATTERN_LABELS` | named chr, names = `names(HARMONIZATION_CONFIG$foraging_patterns)` | `R/ui/harmonization_settings_ui.R` |
| `CONSUMED_TAXONOMIC_RULES` | named chr, names = every rule passed literally to `is_rule_enabled()` under `R/` (11) | same |

## Deviations from the spec (with reasons)

1. `validate_harmonization_config()` returns `list(ok, errors, config)`, not `list(ok, errors)`. The spec requires missing keys to be filled with `modifyList` before validation; returning the normalised config is how that fill reaches the loader, import and save.
2. `admin_strict_refusal()` and the two `ADMIN_STRICT_MSG_*` constants are additions (one place for the refusal text + warning). The B3 contract is only `admin_authorized_strict()` and the two exact strings.
3. The validator also rejects an **empty** foraging pattern (spec: "a length-1 string that compiles"). An empty regex compiles but matches every text.
4. `tabsetPanel(id = "harm_tabs")` loses its `id`: it was an input nothing read, and the dead-UI guard (spec: "Every `harm_*` inputId ... appears as `input$<id>`") would flag it. Removed together with `harm_cancel` and the five unconsumed rule checkboxes.
5. `R/functions/parallel_lookup.R` also passes the config hash (the spec lists only the orchestrator and `trait_research_server.R` readers). It is the third reader of the same envelope; leaving it unkeyed would reopen F72 for batch lookups.
6. The B0 test "a server-default file missing a threshold still starts and accepts slider moves" asserted `expect_null(...$MS6_MS7)` at start. B1's loader fills missing keys from the defaults (spec: "Missing keys are filled from `HARMONIZATION_CONFIG`"), so the assertion becomes `== 150` (Task 2).
7. The spec's FS-seeding test reads `input$harm_pattern_FS0_primary_producer` inside `testServer`. `testServer` never feeds `update*Input()` messages back into `input` (verified: the value stays NULL), so the test records the messages instead through a hand-built `MockShinySession` whose `sendInputMessage` is replaced (`start_harm_session()` in the test file). Do not "simplify" it back to `testServer`.
8. Spec Test 10 rewrite in `tests/test_harmonization_phase1.R`: done, but that legacy script already cannot reach Test 10 (Test 7 sources the long-gone `R/functions/trait_lookup.R`) and nothing runs it. Parse-check only.
9. Controller ruling (2026-09-27), beyond the spec: `validate_harmonization_config()` rejects an FS0 (primary producer) pattern that matches any of `HARM_FS0_DIET_NOUNS` (plant, algae, phytoplankton, diatom, dinoflagellate, seaweed, macroalgae). Production's `config/harmonization_custom.json` still carries the pre-2026-07-17 FS0 `photosyn|autotrop|producer|plant|algae|phytoplankton|diatom|dinoflagellate`; with this rule it falls back to the built-in defaults with a `warning()` at load (never a crash), import rejects it, and "Save as server default" refuses it. The session FS widget only checks that a pattern compiles, so a user may still try diet nouns in their own session; they can never reach the server default.

## Open questions for the user (do not block execution)

- Controller ruling (2026-09-27): the locked-session refusal text is "Unlock via Trait Research > Configure API Keys first" (the real location of the unlock button, `R/ui/trait_research_ui.R:19`). The spec's "Plugins > API Key Configuration" named a place that does not exist. B1 and B3 use this exact string.
- Resolved by controller ruling (deviation 9): FS0 patterns matching diet nouns are rejected.
- Production may hold a stale `config/harmonization_custom.json` (Task 8 Step 2 checks). After the admin hash is set, "Reset server default" removes it; whether to do so is your call.

---

## File Structure

| File | Change | Responsibility |
|---|---|---|
| `R/functions/admin_auth.R` | Modify (insert after `admin_authorized()`) | Fail-closed gate, refusal texts, refusal helper |
| `tests/testthat/test-admin-auth.R` | Modify (append) | Strict-gate tests |
| `R/config/harmonization_config.R` | Modify (replace `save_`/`load_harmonization_config`, add validator, import/export) | Config validation and file I/O |
| `tests/testthat/test-harmonization-config-io.R` | Create | Validator, import/export, loader, atomic save |
| `tests/test_harmonization_phase1.R` | Modify (Test 10 only) | Stop expecting import to mutate the global |
| `R/functions/trait_lookup/harmonization.R` | Modify (insert after `get_harm_config()`) | `harm_config_hash()`, `harm_default_config_hash()` |
| `R/functions/validation_utils.R` | Modify (`read_cache_field`) | `config_hash` argument |
| `R/functions/trait_lookup/orchestrator.R` | Modify (reader ~L245, offline writer ~L425, pipeline writer ~L1736) | Stamp and check the hash |
| `R/modules/trait_research_server.R` | Modify (cache block ~L361-399) | Read the cache through `read_cache_field(..., config_hash =)` |
| `R/functions/parallel_lookup.R` | Modify (~L369-382) | Same for the batch reader |
| `tests/testthat/test-trait-cache-config-hash.R` | Create | Hash stability, cache miss on mismatch, two-session isolation |
| `R/ui/harmonization_settings_ui.R` | Replace | Config-keyed FS inputs (empty literals), consumed-rule checkboxes, new buttons |
| `tests/testthat/test-ui-inputs-have-handlers.R` | Create (Task 4), append (Task 5) | Dead-UI guard |
| `R/modules/harmonization_settings_server.R` | Replace | Session-only widgets, strict-gated server default, import/export |
| `tests/testthat/test-harmonization-settings-server.R` | Modify (Task 2), replace (Task 5) | B0 loop tests + B1 behaviour + deferred B0 minors |
| `CONTRIBUTING.md` | Modify (Tasks 1, 3, 5) | Strict gate, config hash, harm_* consumer rule |
| `VERSION`, `R/config.R`, `app.R`, `README.md`, `CHANGELOG.md` | Modify (Task 6) | 1.5.2 |

`trait_research_server.R` is also edited by B3 (rebuild observer near L1110-1212); B1 touches only the lookup cache block near L361-399, so the hunks do not overlap.

---

### Task 0: Branch and baseline

**Files:** none.

**Interfaces:**
- Consumes: master with B2 merged.
- Produces: branch `fix/b1-harmonization-safety`; baseline PASS / FAIL / SKIP counts that Task 6 compares against.

- [ ] **Step 1: Confirm B2 is merged and cut the branch**

```bash
cd "/c/Users/arturas.baziukas/OneDrive - ku.lt/HORIZON_EUROPE/MARBEFES/Traits/Networks/EcoNeTool"
git checkout master && git pull --ff-only
git log --oneline -5
grep '^VERSION=' VERSION
git tag -l 'v1.5.*'
git checkout -b fix/b1-harmonization-safety
```

Expected: the log shows the B2 merge; `VERSION=1.5.1`. If B2 is not merged, STOP and ask the user (B1 must deploy with B2's safe scripts). Record whether `v1.5.1` is tagged (Task 6 Step 4 needs it).

- [ ] **Step 2: Confirm this plan is tracked**

`docs/superpowers/plans/` is in `.gitignore`; plans reach master force-added with the docs merge.

```bash
git ls-files docs/superpowers/plans/2026-09-27-b1-harmonization-safety.md
```

Expected: the path is printed. If it is empty, run `git add -f docs/superpowers/plans/2026-09-27-b1-harmonization-safety.md` and commit it as `docs(plans): add B1 harmonization safety plan` (with the Co-Authored-By line).

- [ ] **Step 3: Re-read the files this plan edits**

Line numbers in the spec are stale and B2 may have moved things. Before each task, confirm every "replace this exact text" block below still matches (`grep -n` the first line). If a block no longer matches, adapt the edit to the current text and note it in the task's commit message.

- [ ] **Step 4: Record the baseline suite**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "r <- as.data.frame(testthat::test_dir('tests/testthat', reporter = 'summary', stop_on_failure = FALSE)); cat('pass', sum(r\$passed), 'fail', sum(r\$failed), 'skip', sum(r\$skipped), '\n')"
```

Expected: `fail 0` (master before B2 was about 1524 pass / 0 fail / 38 skip; B2 adds tests). Write the three numbers down.

---

### Task 1: Fail-closed admin gate

**Files:**
- Modify: `R/functions/admin_auth.R` (insert after `admin_authorized()`, around line 201-203)
- Modify: `tests/testthat/test-admin-auth.R` (append)
- Modify: `CONTRIBUTING.md` (one bullet above `## Commit Messages`)

**Interfaces:**
- Consumes: `admin_gate_enabled(record = Sys.getenv(ADMIN_PASSWORD_ENV))` (existing).
- Produces: `admin_authorized_strict(unlocked) -> logical(1)`; `ADMIN_STRICT_MSG_UNSET`; `ADMIN_STRICT_MSG_LOCKED`; `admin_strict_refusal(unlocked, action = "admin action") -> NULL | character(1)` (warns on refusal).

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-admin-auth.R` (it already defines `with_admin_hash(value, code)`):

```r

# ---------------------------------------------------------------------------
# admin_authorized_strict: the fail-CLOSED gate for server-wide writes (B1)
# ---------------------------------------------------------------------------
# admin_authorized() fails OPEN when no hash is configured, so the API-key
# modal keeps working on unconfigured instances. Saving the harmonization
# server default (B1) and rebuilding the offline DB (B3) must not: with no
# hash configured they are refused outright.

test_that("admin_authorized_strict refuses everything when the gate is off", {
  with_admin_hash(NULL, {
    expect_identical(admin_authorized_strict(TRUE), FALSE)
    expect_identical(admin_authorized_strict(FALSE), FALSE)
    expect_identical(admin_authorized_strict(NULL), FALSE)
  })
  with_admin_hash("", expect_identical(admin_authorized_strict(TRUE), FALSE))
  with_admin_hash("   ", expect_identical(admin_authorized_strict(TRUE), FALSE))
})

test_that("admin_authorized_strict needs the gate on AND an unlocked session", {
  # admin_gate_enabled() only checks that the record is non-blank.
  with_admin_hash("econetool1$12$00$00", {
    expect_identical(admin_authorized_strict(TRUE), TRUE)
    expect_identical(admin_authorized_strict(FALSE), FALSE)
    expect_identical(admin_authorized_strict(NULL), FALSE)
    expect_identical(admin_authorized_strict(NA), FALSE)
    expect_identical(admin_authorized_strict("yes"), FALSE)
  })
})

test_that("admin_strict_refusal returns the user-facing reason and warns", {
  with_admin_hash(NULL, {
    expect_warning(msg <- admin_strict_refusal(TRUE, "save server default"),
                   "[admin auth] save server default refused", fixed = TRUE)
    expect_identical(msg, "Admin gate not configured on this instance; server defaults are read-only")
  })
  with_admin_hash("econetool1$12$00$00", {
    expect_warning(msg <- admin_strict_refusal(NULL, "save server default"),
                   "[admin auth]", fixed = TRUE)
    expect_identical(msg, "Unlock via Trait Research > Configure API Keys first")
    expect_silent(ok <- admin_strict_refusal(TRUE, "save server default"))
    expect_null(ok)
  })
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-admin-auth.R')"`
Expected: `[ FAIL 3 | WARN ... | SKIP 0 | PASS 55 ]` - the three new tests error with `could not find function "admin_authorized_strict"` / `"admin_strict_refusal"`.

- [ ] **Step 3: Implement**

In `R/functions/admin_auth.R`, replace:

```r
admin_authorized <- function(unlocked) {
  !admin_gate_enabled() || isTRUE(unlocked)
}
```

with:

```r
admin_authorized <- function(unlocked) {
  !admin_gate_enabled() || isTRUE(unlocked)
}

#' May this session change SERVER-WIDE state? (fail-closed)
#'
#' admin_authorized() fails OPEN when no hash is configured, so the API-key
#' modal keeps working on instances nobody has set up. Actions that change
#' what every session sees - saving the harmonization server default (B1),
#' rebuilding the offline trait DB (B3) - must not: on an unconfigured
#' instance they are refused outright. For local development, set a hash with
#' set_admin_password() in .Renviron, or run the build script from a console.
#'
#' @param unlocked The session's unlock flag, normally
#'   \code{session$userData$admin_unlocked}. Only TRUE counts.
#' @return TRUE only when the gate is configured AND this session unlocked it.
#' @export
admin_authorized_strict <- function(unlocked) {
  admin_gate_enabled() && isTRUE(unlocked)
}

# User-facing refusal texts for strict-gated actions. B3 shows the same two
# texts for the offline-DB rebuild, so keep them word-for-word.
ADMIN_STRICT_MSG_UNSET <- "Admin gate not configured on this instance; server defaults are read-only"
ADMIN_STRICT_MSG_LOCKED <- "Unlock via Trait Research > Configure API Keys first"

#' Why is a strict-gated action refused? (NULL when it is allowed)
#'
#' Emits warning("[admin auth] ...") on refusal - warning, not message,
#' because production shiny-server.conf keeps no message() logs.
#'
#' @param unlocked The session's unlock flag.
#' @param action Short label for the log line, e.g. "save server default".
#' @return NULL when admin_authorized_strict(unlocked) is TRUE, otherwise
#'   ADMIN_STRICT_MSG_UNSET (no hash configured) or ADMIN_STRICT_MSG_LOCKED.
#' @export
admin_strict_refusal <- function(unlocked, action = "admin action") {
  if (admin_authorized_strict(unlocked)) {
    return(NULL)
  }
  msg <- if (admin_gate_enabled()) ADMIN_STRICT_MSG_LOCKED else ADMIN_STRICT_MSG_UNSET
  warning(sprintf("[admin auth] %s refused: %s", action, msg), call. = FALSE)
  msg
}
```

- [ ] **Step 4: Parse-check and run the tests**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='R/functions/admin_auth.R'); cat('OK\n')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-admin-auth.R')"
```

Expected: `OK`; `[ FAIL 0 | WARN ... | SKIP 0 | PASS 71 ]`.

- [ ] **Step 5: Document the convention**

In `CONTRIBUTING.md`, replace the line `## Commit Messages` with:

```markdown
- **Server-wide writes use the strict (fail-closed) gate.**
  `admin_authorized()` fails OPEN when `ECONETOOL_ADMIN_PASSWORD_HASH` is
  unset, which is right for the API-key modal only. Anything that changes
  what every session sees (the harmonization server default, the offline DB
  rebuild) must call `admin_authorized_strict(session$userData$admin_unlocked)`
  - or `admin_strict_refusal(unlocked, "<action>")`, which also warns and
  returns the user-facing reason. With no hash configured these actions are
  refused ("Admin gate not configured on this instance; server defaults are
  read-only"). For local work, put a `set_admin_password()` line in
  `.Renviron` or run the build script from a console.

## Commit Messages
```

- [ ] **Step 6: Commit**

```bash
git add R/functions/admin_auth.R tests/testthat/test-admin-auth.R CONTRIBUTING.md
git commit -m "$(cat <<'EOF'
feat(admin): add fail-closed admin_authorized_strict gate (F2)

admin_authorized() stays fail-open for the API-key modal. Server-wide
writes get admin_authorized_strict(): TRUE only when the hash is set AND
the session unlocked. admin_strict_refusal() returns the user-facing
reason and warns "[admin auth] ...".

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 2: Shared validator, atomic save, JSON import/export (F2, F8)

**Files:**
- Modify: `R/config/harmonization_config.R` (the `save_harmonization_config` and `load_harmonization_config` definitions at the end of the file)
- Create: `tests/testthat/test-harmonization-config-io.R`
- Modify: `tests/test_harmonization_phase1.R` (Test 10)
- Modify: `tests/testthat/test-harmonization-settings-server.R` (one B0 test)

**Interfaces:**
- Consumes: `HARMONIZATION_CONFIG`, `HARMONIZATION_CONFIG_FILE` (existing).
- Produces: `HARM_THRESHOLD_KEYS`; `HARM_FS0_DIET_NOUNS`; `harm_pattern_compiles(pattern) -> logical(1)`; `validate_harmonization_config(cfg) -> list(ok, errors, config)` (rejects a diet-noun FS0); `save_harmonization_config(config, file) -> invisible(file)` (tmp + rename, stops on failure); `load_harmonization_config(file)` (validated; warns and returns defaults on parse or validation failure; the "could not parse" wording is kept); `export_config_json(file, config) -> invisible(TRUE)`; `import_config_json(file) -> list` (stops on invalid).

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-harmonization-config-io.R`:

```r
# Spec B section 4.1 (F2, F8): the shared harmonization-config validator,
# JSON import/export, and the server-default loader/saver. Pure functions:
# no Shiny session needed.

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
source(file.path(app_root, "R/functions/validation_utils.R"), local = FALSE)
source(file.path(app_root, "R/config/harmonization_config.R"), local = FALSE)

with_threshold <- function(key, value) {
  cfg <- HARMONIZATION_CONFIG
  cfg$size_thresholds[[key]] <- value
  cfg
}

errors_of <- function(cfg) paste(validate_harmonization_config(cfg)$errors, collapse = " | ")

test_that("the built-in defaults validate", {
  v <- validate_harmonization_config(HARMONIZATION_CONFIG)
  expect_true(v$ok)
  expect_identical(v$errors, character(0))
  expect_identical(v$config$size_thresholds, HARMONIZATION_CONFIG$size_thresholds)
})

test_that("non-increasing, infinite, negative and non-numeric thresholds are rejected", {
  expect_false(validate_harmonization_config(with_threshold("MS3_MS4", 0.5))$ok)
  expect_false(validate_harmonization_config(with_threshold("MS3_MS4", 1.0))$ok) # equals MS2_MS3
  expect_false(validate_harmonization_config(with_threshold("MS6_MS7", Inf))$ok)
  expect_false(validate_harmonization_config(with_threshold("MS1_MS2", -0.1))$ok)
  expect_false(validate_harmonization_config(with_threshold("MS1_MS2", "0.1"))$ok)
  expect_match(errors_of(with_threshold("MS3_MS4", 0.5)), "strictly increasing")
  expect_match(errors_of(with_threshold("MS6_MS7", Inf)), "MS6_MS7")
})

test_that("uncompilable and empty foraging patterns are rejected", {
  cfg <- HARMONIZATION_CONFIG
  cfg$foraging_patterns$FS1_predator <- "("
  expect_false(validate_harmonization_config(cfg)$ok)
  expect_match(errors_of(cfg), "FS1_predator")
  cfg$foraging_patterns$FS1_predator <- ""
  expect_false(validate_harmonization_config(cfg)$ok)
  expect_true(harm_pattern_compiles("predat|hunter"))
  expect_false(harm_pattern_compiles("predat("))
  expect_false(harm_pattern_compiles(c("a", "b")))
})

test_that("an FS0 pattern that matches diet nouns is rejected (the 2026-07-17 inversion)", {
  # The string production held in 2026-09: diet nouns make every herbivore
  # and planktivore a primary producer, because FS0 is tested first.
  stale <- "photosyn|autotrop|producer|plant|algae|phytoplankton|diatom|dinoflagellate"
  cfg <- HARMONIZATION_CONFIG
  cfg$foraging_patterns$FS0_primary_producer <- stale
  v <- validate_harmonization_config(cfg)
  expect_false(v$ok)
  expect_match(paste(v$errors, collapse = " | "), "FS0_primary_producer matches diet nouns")
  expect_match(paste(v$errors, collapse = " | "), "phytoplankton")

  cfg$foraging_patterns$FS0_primary_producer <- "producer|seaweed"
  expect_false(validate_harmonization_config(cfg)$ok)

  # The built-in default names no diet noun and passes.
  expect_identical(HARMONIZATION_CONFIG$foraging_patterns$FS0_primary_producer,
                   "photosyn|autotroph|autotrop|primary.?produc|producer")
  expect_true(validate_harmonization_config(HARMONIZATION_CONFIG)$ok)
})

test_that("a stale server file with a diet-noun FS0 loads as the defaults, with a warning", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  cfg <- HARMONIZATION_CONFIG
  cfg$foraging_patterns$FS0_primary_producer <- "photosyn|autotrop|producer|plant|algae|phytoplankton|diatom|dinoflagellate"
  export_config_json(tmp, cfg)
  expect_warning(got <- load_harmonization_config(tmp), "diet nouns")
  expect_identical(got, HARMONIZATION_CONFIG)
  expect_error(import_config_json(tmp), "diet nouns")
})

test_that("non-logical taxonomic rules and unknown profiles are rejected", {
  cfg <- HARMONIZATION_CONFIG
  cfg$taxonomic_rules$bivalves_sessile <- "yes"
  expect_false(validate_harmonization_config(cfg)$ok)
  expect_match(errors_of(cfg), "bivalves_sessile")

  cfg <- HARMONIZATION_CONFIG
  cfg$active_profile <- "atlantis"
  expect_false(validate_harmonization_config(cfg)$ok)
  expect_match(errors_of(cfg), "atlantis")
})

test_that("unknown top-level keys are dropped and missing keys are filled from the defaults", {
  v <- validate_harmonization_config(list(size_thresholds = list(MS3_MS4 = 6), evil = "x"))
  expect_true(v$ok)
  expect_null(v$config$evil)
  expect_equal(v$config$size_thresholds$MS3_MS4, 6)
  expect_equal(v$config$size_thresholds$MS6_MS7, 150)
  expect_identical(v$config$foraging_patterns, HARMONIZATION_CONFIG$foraging_patterns)
})

test_that("a config that is not a named list is rejected", {
  expect_false(validate_harmonization_config(list(1, 2))$ok)
  expect_false(validate_harmonization_config("x")$ok)
  expect_false(validate_harmonization_config(NULL)$ok)
})

test_that("export then import round-trips and never touches the global default", {
  global_before <- HARMONIZATION_CONFIG
  cfg <- with_threshold("MS3_MS4", 7.5)
  cfg$taxonomic_rules$bivalves_sessile <- FALSE
  cfg$active_profile <- "baltic"
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  expect_true(export_config_json(tmp, cfg))
  got <- import_config_json(tmp)

  expect_equal(got$size_thresholds, cfg$size_thresholds)
  expect_equal(got$foraging_patterns, cfg$foraging_patterns)
  expect_equal(got$taxonomic_rules, cfg$taxonomic_rules)
  expect_identical(got$active_profile, "baltic")
  expect_identical(HARMONIZATION_CONFIG, global_before)
})

test_that("import stops with the validation errors for an invalid file", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  export_config_json(tmp, with_threshold("MS3_MS4", 0.5))
  expect_error(import_config_json(tmp), "strictly increasing")
})

test_that("import stops on unparseable JSON", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("{ nope", tmp)
  expect_error(import_config_json(tmp))
})

test_that("load_harmonization_config warns and returns the defaults for an invalid server file", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  export_config_json(tmp, with_threshold("MS3_MS4", 0.5))
  expect_warning(got <- load_harmonization_config(tmp), "invalid config")
  expect_identical(got, HARMONIZATION_CONFIG)
})

test_that("load_harmonization_config fills a partial server file from the defaults", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  writeLines('{"size_thresholds": {"MS3_MS4": 6}}', tmp)
  got <- load_harmonization_config(tmp)
  expect_equal(got$size_thresholds$MS3_MS4, 6)
  expect_equal(got$size_thresholds$MS6_MS7, 150)
  expect_identical(got$foraging_patterns, HARMONIZATION_CONFIG$foraging_patterns)
})

test_that("save replaces the file through a temp file and leaves nothing behind", {
  dir <- tempfile("harm_save_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  target <- file.path(dir, "harmonization_custom.json")

  suppressMessages(save_harmonization_config(with_threshold("MS3_MS4", 6), target))
  suppressMessages(save_harmonization_config(with_threshold("MS3_MS4", 8), target))

  expect_equal(load_harmonization_config(target)$size_thresholds$MS3_MS4, 8)
  expect_identical(list.files(dir), "harmonization_custom.json")
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-harmonization-config-io.R')"`
Expected: `[ FAIL 13 | WARN ... | SKIP 0 | PASS 4 ]` - errors `could not find function "validate_harmonization_config"` / `"export_config_json"` (including both diet-noun FS0 tests), and the partial-file test fails because `MS6_MS7` is NULL. Only the unparseable-JSON and atomic-save tests pass (the latter is a regression pin).

- [ ] **Step 3: Implement**

In `R/config/harmonization_config.R`, replace:

```r
save_harmonization_config <- function(config = HARMONIZATION_CONFIG,
                                      file = HARMONIZATION_CONFIG_FILE) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  json_data <- jsonlite::toJSON(config, pretty = TRUE, auto_unbox = TRUE)
  writeLines(json_data, file)
  message("✓ Harmonization configuration saved to: ", file)
}

load_harmonization_config <- function(file = HARMONIZATION_CONFIG_FILE) {
  if (!file.exists(file)) return(HARMONIZATION_CONFIG)
  tryCatch({
    jsonlite::fromJSON(file, simplifyVector = FALSE)
  }, error = function(e) {
    # Falling back to defaults is right; doing it silently is not - the user
    # would see their saved settings quietly revert with no explanation.
    warning(sprintf("[harmonization] could not parse '%s', using defaults: %s",
                    file, conditionMessage(e)), call. = FALSE)
    HARMONIZATION_CONFIG
  })
}
```

with:

```r
# The six size-class boundaries in MS order. The validator, the slider module
# and the tests all iterate this one vector.
HARM_THRESHOLD_KEYS <- c("MS1_MS2", "MS2_MS3", "MS3_MS4", "MS4_MS5", "MS5_MS6", "MS6_MS7")

# Diet nouns an FS0 (primary producer) pattern must never match. They were
# removed from FS0 on 2026-07-17: FS0 is tested before FS1-FS6 on the pasted
# feeding text, so a consumer whose diet mentions algae or diatoms was coded
# as an autotroph, inverting the base of the food web. The validator rejects
# any FS0 pattern matching one of these, so a stale server-default file can
# never bring the inversion back.
HARM_FS0_DIET_NOUNS <- c("plant", "algae", "phytoplankton", "diatom", "dinoflagellate",
                         "seaweed", "macroalgae")

#' Does a foraging pattern compile the way harmonize_foraging_strategy() uses it?
#'
#' A length-1, non-blank string that grepl(..., ignore.case = TRUE) accepts.
#' Blank is refused because an empty pattern matches every text. The tryCatch
#' is a validation probe, not a swallowed failure: FALSE is reported to the
#' caller as a validation error.
#'
#' @param pattern Candidate regular expression.
#' @return TRUE or FALSE.
harm_pattern_compiles <- function(pattern) {
  if (!is.character(pattern) || length(pattern) != 1L || is.na(pattern) ||
        !nzchar(trimws(pattern))) {
    return(FALSE)
  }
  tryCatch({
    suppressWarnings(grepl(pattern, "", ignore.case = TRUE))
    TRUE
  }, error = function(e) FALSE)
}

#' Validate (and normalise) a harmonization config
#'
#' Shared by the server-default loader, JSON import and the "Save as server
#' default" button (spec B section 4.1, F2/F8). Unknown top-level keys are
#' dropped; missing keys are filled from HARMONIZATION_CONFIG with
#' utils::modifyList() BEFORE the checks run.
#'
#' @param cfg A config list (e.g. from jsonlite::fromJSON(simplifyVector = FALSE)).
#' @return list(ok = logical(1), errors = character(), config = <normalised list>).
#'   `config` is HARMONIZATION_CONFIG when `cfg` is not a named list.
validate_harmonization_config <- function(cfg) {
  if (!is.list(cfg) || is.null(names(cfg))) {
    return(list(ok = FALSE, errors = "config must be a JSON object (a named list)",
                config = HARMONIZATION_CONFIG))
  }
  cfg <- utils::modifyList(HARMONIZATION_CONFIG, cfg[intersect(names(cfg), names(HARMONIZATION_CONFIG))])
  errors <- character()

  thr <- if (is.list(cfg$size_thresholds)) cfg$size_thresholds else list()
  vals <- vapply(HARM_THRESHOLD_KEYS, function(k) {
    v <- thr[[k]]
    if (is.numeric(v) && length(v) == 1L && is.finite(v) && v > 0) as.numeric(v) else NA_real_
  }, numeric(1))
  if (anyNA(vals)) {
    errors <- c(errors, sprintf("size_thresholds: %s must be a finite number > 0",
                                paste(HARM_THRESHOLD_KEYS[is.na(vals)], collapse = ", ")))
  } else if (any(diff(vals) <= 0)) {
    errors <- c(errors, "size_thresholds must be strictly increasing from MS1_MS2 to MS6_MS7")
  }

  pats <- if (is.list(cfg$foraging_patterns)) cfg$foraging_patterns else list()
  bad_pats <- names(pats)[!vapply(pats, harm_pattern_compiles, logical(1))]
  if (length(pats) == 0L || length(bad_pats) > 0L) {
    errors <- c(errors, sprintf("foraging_patterns: not a valid non-empty regular expression: %s",
                                paste(bad_pats, collapse = ", ")))
  }
  fs0 <- pats$FS0_primary_producer
  if (harm_pattern_compiles(fs0)) {
    diet_hits <- HARM_FS0_DIET_NOUNS[grepl(fs0, HARM_FS0_DIET_NOUNS, ignore.case = TRUE)]
    if (length(diet_hits) > 0L) {
      errors <- c(errors, sprintf(paste0(
        "foraging_patterns: FS0_primary_producer matches diet nouns (%s); FS0 is tested first, ",
        "so consumers whose diet mentions them would be coded as producers"),
        paste(diet_hits, collapse = ", ")))
    }
  }

  rules <- if (is.list(cfg$taxonomic_rules)) cfg$taxonomic_rules else list()
  is_flag <- vapply(rules, function(x) is.logical(x) && length(x) == 1L && !is.na(x), logical(1))
  if (length(rules) == 0L || !all(is_flag)) {
    errors <- c(errors, sprintf("taxonomic_rules: must be TRUE or FALSE: %s",
                                paste(names(rules)[!is_flag], collapse = ", ")))
  }

  profile <- cfg$active_profile
  if (!is.character(profile) || length(profile) != 1L || !profile %in% names(cfg$profiles)) {
    errors <- c(errors, sprintf("active_profile '%s' is not one of: %s",
                                paste(format(profile), collapse = ","),
                                paste(names(cfg$profiles), collapse = ", ")))
  }

  list(ok = length(errors) == 0L, errors = errors, config = cfg)
}

save_harmonization_config <- function(config = HARMONIZATION_CONFIG,
                                      file = HARMONIZATION_CONFIG_FILE) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  json_data <- jsonlite::toJSON(config, pretty = TRUE, auto_unbox = TRUE, digits = NA)
  # Write beside the target, then rename over it, so a crash or a full disk
  # mid-write can never leave a truncated server default behind.
  tmp <- paste0(file, ".tmp")
  writeLines(json_data, tmp)
  if (!file.rename(tmp, file)) {
    unlink(tmp)
    stop(sprintf("could not move '%s' into place", tmp), call. = FALSE)
  }
  message("✓ Harmonization configuration saved to: ", file)
  invisible(file)
}

load_harmonization_config <- function(file = HARMONIZATION_CONFIG_FILE) {
  if (!file.exists(file)) return(HARMONIZATION_CONFIG)
  raw <- tryCatch({
    jsonlite::fromJSON(file, simplifyVector = FALSE)
  }, error = function(e) {
    # Falling back to defaults is right; doing it silently is not - the user
    # would see their saved settings quietly revert with no explanation.
    warning(sprintf("[harmonization] could not parse '%s', using defaults: %s",
                    file, conditionMessage(e)), call. = FALSE)
    NULL
  })
  if (is.null(raw)) return(HARMONIZATION_CONFIG)
  checked <- validate_harmonization_config(raw)
  if (!checked$ok) {
    warning(sprintf("[harmonization] invalid config in '%s', using defaults: %s",
                    file, paste(checked$errors, collapse = "; ")), call. = FALSE)
    return(HARMONIZATION_CONFIG)
  }
  checked$config
}

#' Write a config to a JSON file (the "Export as JSON" download)
#'
#' @param file Destination path.
#' @param config Config list; defaults to the process-wide default.
#' @return invisible(TRUE).
export_config_json <- function(file, config = HARMONIZATION_CONFIG) {
  jsonlite::write_json(config, file, auto_unbox = TRUE, pretty = TRUE, digits = NA)
  invisible(TRUE)
}

#' Read and validate a config JSON file (the "Import" upload)
#'
#' Never assigns a global: the caller decides where the config goes (the
#' settings module puts it in the session only).
#'
#' @param file Path to a JSON file.
#' @return The validated, normalised config list. Stops with the joined
#'   validation errors when the file is invalid, or with the parse error.
import_config_json <- function(file) {
  raw <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  checked <- validate_harmonization_config(raw)
  if (!checked$ok) {
    stop(paste(checked$errors, collapse = "; "), call. = FALSE)
  }
  checked$config
}
```

Notes for the implementer: `save_harmonization_config()` deliberately does NOT validate (test helpers write partial files through it; the save button validates first). `file.rename()` replaces an existing target on both Windows and Linux (verified on the dev box).

- [ ] **Step 4: Update the one B0 test whose premise the loader changes**

In `tests/testthat/test-harmonization-settings-server.R`, replace:

```r
test_that("a server-default file missing a threshold still starts and accepts slider moves", {
```

with:

```r
test_that("a server-default file missing a threshold is filled from the defaults at start", {
```

and, inside that same test, replace:

```r
  shiny::testServer(harmonization_settings_server, {
    expect_null(session$userData$harm_config$size_thresholds$MS6_MS7)
```

with:

```r
  shiny::testServer(harmonization_settings_server, {
    # B1: load_harmonization_config() now runs the shared validator, which
    # fills missing keys from HARMONIZATION_CONFIG (pre-B1 this was NULL).
    expect_equal(session$userData$harm_config$size_thresholds$MS6_MS7, 150)
```

- [ ] **Step 5: Rewrite Test 10 of the legacy phase-1 script**

In `tests/test_harmonization_phase1.R`, replace:

```r
  # Modify config
  HARMONIZATION_CONFIG$size_thresholds$MS2_MS3 <- 2.0

  # Import from JSON
  success_import <- import_config_json(file = test_json)
  stopifnot("Import succeeded" = success_import)
  stopifnot("Config restored" =
    HARMONIZATION_CONFIG$size_thresholds$MS2_MS3 == 1.0
  )
```

with:

```r
  # Import returns the validated config; it never assigns a global (the
  # pre-PR9α pattern this test used to expect).
  global_before <- HARMONIZATION_CONFIG
  imported <- import_config_json(file = test_json)
  stopifnot("Import returns a config" = is.list(imported))
  stopifnot("Thresholds round-trip" =
    isTRUE(all.equal(unlist(imported$size_thresholds), unlist(HARMONIZATION_CONFIG$size_thresholds)))
  )
  stopifnot("Global default untouched" = identical(HARMONIZATION_CONFIG, global_before))
```

This script is legacy and unrun (its Test 7 sources the removed `R/functions/trait_lookup.R`), so it is only parse-checked.

- [ ] **Step 6: Parse-check and run**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='R/config/harmonization_config.R'); parse(file='tests/test_harmonization_phase1.R'); cat('OK\n')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-harmonization-config-io.R')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-harmonization-settings-server.R')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-config-paths.R')"
```

Expected: `OK`; config-io `[ FAIL 0 | ... | PASS 52 ]`; the B0 server file `FAIL 0` (its unparseable-JSON test still sees the "could not parse" warning); test-config-paths `FAIL 0` (its "same resolved path" test compares the unchanged `file =` formals).

- [ ] **Step 7: Commit**

```bash
git add R/config/harmonization_config.R tests/testthat/test-harmonization-config-io.R tests/test_harmonization_phase1.R tests/testthat/test-harmonization-settings-server.R
git commit -m "$(cat <<'EOF'
fix(harmonization): validate server config, atomic save, JSON import/export (F2, F8)

validate_harmonization_config() drops unknown top-level keys, fills missing
ones from the defaults and checks thresholds (finite, > 0, strictly
increasing), FS regexes, rule flags and the active profile. An FS0
pattern matching a diet noun (plant, algae, phytoplankton, ...) is
rejected, so a stale server default cannot reinvert producers. The loader
warns and falls back on an invalid file; save writes <file>.tmp and
renames. export_config_json()/import_config_json() were called by the
module but never defined; import returns the config and never assigns
a global.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 3: Key the trait cache on the effective config (F72)

**Files:**
- Modify: `R/functions/trait_lookup/harmonization.R` (after `get_harm_config()`, ~L66-75)
- Modify: `R/functions/validation_utils.R` (`read_cache_field`, ~L481-500)
- Modify: `R/functions/trait_lookup/orchestrator.R` (~L240-245, ~L421-426, ~L1735-1740)
- Modify: `R/modules/trait_research_server.R` (~L361-399)
- Modify: `R/functions/parallel_lookup.R` (~L369-382)
- Create: `tests/testthat/test-trait-cache-config-hash.R`
- Modify: `CONTRIBUTING.md`

**Interfaces:**
- Consumes: `get_harm_config()`; `export_config_json()`, `import_config_json()` (Task 2); in its session test, the B0 module `harmonization_settings_server` (any version) and `admin_auth.R`.
- Produces: `harm_config_hash(cfg = get_harm_config()) -> character(1) | NULL`; `harm_default_config_hash() -> character(1) | NULL`; `read_cache_field(cache_file, field, max_age_days = 30, config_hash = NULL)`; envelopes in `cache/taxonomy/<species>.rds` with a `config_hash` field.

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-trait-cache-config-hash.R`:

```r
# Spec B section 4.1 (F72): harmonized trait codes are cached per species in
# cache/taxonomy/<species>.rds and shared by every session, but they depend on
# the session's harmonization config. The envelope now carries the effective
# config hash, and a reader that passes its own hash treats a different (or
# missing) hash as a cache miss.

source_app_dependencies()

write_envelope <- function(path, hash) {
  envelope <- list(traits = data.frame(species = "Gadus morhua", MS = "MS6", stringsAsFactors = FALSE),
                   timestamp = Sys.time())
  if (!is.null(hash)) envelope$config_hash <- hash
  saveRDS(envelope, path)
}

test_that("harm_config_hash ignores last_modified and version", {
  b <- HARMONIZATION_CONFIG
  b$last_modified <- as.Date("2001-01-01")
  b$version <- "0.0.1"
  expect_identical(harm_config_hash(b), harm_config_hash(HARMONIZATION_CONFIG))
})

test_that("harm_config_hash changes when a threshold changes", {
  b <- HARMONIZATION_CONFIG
  b$size_thresholds$MS3_MS4 <- 6
  expect_false(identical(harm_config_hash(b), harm_config_hash(HARMONIZATION_CONFIG)))
})

test_that("harm_config_hash survives a JSON round-trip (integer/double drift)", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  export_config_json(tmp, HARMONIZATION_CONFIG)
  expect_identical(harm_config_hash(import_config_json(tmp)), harm_config_hash(HARMONIZATION_CONFIG))
})

test_that("outside Shiny the default hash is the process default's hash", {
  expect_identical(harm_config_hash(), harm_config_hash(HARMONIZATION_CONFIG))
  expect_identical(harm_default_config_hash(), harm_config_hash(HARMONIZATION_CONFIG))
  expect_type(harm_config_hash(), "character")
  expect_length(harm_config_hash(), 1L)
})

test_that("read_cache_field treats a different or missing config hash as a miss", {
  f <- tempfile(fileext = ".rds")
  on.exit(unlink(f), add = TRUE)

  write_envelope(f, "abc")
  expect_null(read_cache_field(f, "traits", config_hash = "x"))
  expect_equal(read_cache_field(f, "traits", config_hash = "abc")$MS, "MS6")
  expect_equal(read_cache_field(f, "traits")$MS, "MS6") # no hash asked: unchanged behaviour

  write_envelope(f, NULL) # envelope written before B1
  expect_null(read_cache_field(f, "traits", config_hash = "abc"))
})

test_that("two sessions with different MS3_MS4 do not share cached harmonized codes", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  root <- get_app_root()
  source(file.path(root, "R/functions/admin_auth.R"), local = FALSE)
  source(file.path(root, "R/ui/harmonization_settings_ui.R"), local = FALSE)
  source(file.path(root, "R/modules/harmonization_settings_server.R"), local = FALSE)

  old_file <- get("HARMONIZATION_CONFIG_FILE", envir = globalenv())
  old_worms <- get("lookup_worms_traits", envir = globalenv())
  assign("HARMONIZATION_CONFIG_FILE", file.path(tempdir(), "no_such_harm_cfg.json"), envir = globalenv())
  # A cache miss falls through to the live WoRMS lookup first; make it loud.
  assign("lookup_worms_traits", function(...) stop("CACHE MISS: live lookup reached"), envir = globalenv())
  withr::defer({
    assign("HARMONIZATION_CONFIG_FILE", old_file, envir = globalenv())
    assign("lookup_worms_traits", old_worms, envir = globalenv())
  })

  cache_dir <- tempfile("taxonomy_")
  dir.create(cache_dir)
  withr::defer(unlink(cache_dir, recursive = TRUE))
  species <- "Gadus morhua"

  # Session A (default settings) owns the cached row.
  shiny::testServer(harmonization_settings_server, {
    write_envelope(file.path(cache_dir, "Gadus_morhua.rds"), harm_config_hash())
    got <- suppressMessages(lookup_species_traits(species, cache_dir = cache_dir))
    expect_equal(got$MS, "MS6")
  })

  # Session B moved MS3/MS4: its codes can differ, so A's row must not be served.
  shiny::testServer(harmonization_settings_server, {
    session$setInputs(
      harm_thresh_MS1_MS2 = 0.1, harm_thresh_MS2_MS3 = 1.0,
      harm_thresh_MS3_MS4 = 9, harm_thresh_MS4_MS5 = 20.0,
      harm_thresh_MS5_MS6 = 50.0, harm_thresh_MS6_MS7 = 150.0
    )
    expect_error(suppressMessages(lookup_species_traits(species, cache_dir = cache_dir)),
                 "CACHE MISS")
  })
})

test_that("every trait-cache writer stamps the hash and every reader passes one", {
  orch <- readLines(app_path("R/functions/trait_lookup/orchestrator.R"), warn = FALSE)
  orch <- paste(orch[!startsWith(trimws(orch), "#")], collapse = "\n")
  expect_true(grepl('read_cache_field(cache_file, "traits", config_hash = harm_config_hash())', orch, fixed = TRUE))
  expect_true(grepl("config_hash = harm_default_config_hash()", orch, fixed = TRUE))
  # One reader plus the full-pipeline writer (cache_data <- list(...)).
  n_session_hash <- lengths(regmatches(orch, gregexpr("config_hash = harm_config_hash()", orch, fixed = TRUE)))
  expect_gte(n_session_hash, 2L)

  srv <- readLines(app_path("R/modules/trait_research_server.R"), warn = FALSE)
  srv <- srv[!startsWith(trimws(srv), "#")]
  expect_false(any(grepl("readRDS(cache_file)", srv, fixed = TRUE)))
  expect_true(any(grepl('read_cache_field(cache_file, "traits", config_hash = cfg_hash)', srv, fixed = TRUE)))

  par <- readLines(app_path("R/functions/parallel_lookup.R"), warn = FALSE)
  expect_true(any(grepl('read_cache_field(cache_file, "traits", config_hash = cfg_hash)', par, fixed = TRUE)))
})
```

Why the WoRMS shim: after a cache miss, `lookup_species_traits()` calls `lookup_worms_traits()` before anything else, so replacing it with a `stop()` turns "miss" into an observable error without any network.

- [ ] **Step 2: Run the tests to verify they fail**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-trait-cache-config-hash.R')"`
Expected: `[ FAIL 12 | ... | PASS 0 ]` - `could not find function "harm_config_hash"`, `unused argument (config_hash = "x")`, and the guard test's `expect_true` failures.

- [ ] **Step 3: Add the hash helpers**

In `R/functions/trait_lookup/harmonization.R`, replace:

```r
  if (!exists("HARMONIZATION_CONFIG", envir = .GlobalEnv)) return(NULL)
  get("HARMONIZATION_CONFIG", envir = .GlobalEnv)
}
```

(the end of `get_harm_config()`, the first match in the file) with:

```r
  if (!exists("HARMONIZATION_CONFIG", envir = .GlobalEnv)) return(NULL)
  get("HARMONIZATION_CONFIG", envir = .GlobalEnv)
}


#' Hash of the effective harmonization config (trait-cache key, F72)
#'
#' Harmonized codes in cache/taxonomy/<species>.rds depend on the config that
#' produced them, and every session shares those files. Writers stamp this
#' hash into the envelope; readers pass their own hash to read_cache_field()
#' and treat a mismatch as a miss. `last_modified` and `version` are dropped
#' (they do not change any code). The JSON text is hashed rather than the R
#' object, so 150L after a JSON round trip hashes like 150.
#'
#' @param cfg Config list; defaults to this session's config.
#' @return Character(1) xxhash64 digest, or NULL when no config is loaded.
harm_config_hash <- function(cfg = get_harm_config()) {
  if (is.null(cfg)) return(NULL)
  cfg$last_modified <- NULL
  cfg$version <- NULL
  json <- jsonlite::toJSON(cfg, auto_unbox = TRUE, digits = NA)
  digest::digest(as.character(json), algo = "xxhash64", serialize = FALSE)
}


#' Hash of the process-wide default config
#'
#' For cache writes whose codes were NOT harmonized with the session config:
#' offline-DB rows were harmonized at build time with the defaults. Reads the
#' same global fallback as get_harm_config() and never writes it.
#'
#' @return Character(1) digest, or NULL when no config is loaded.
harm_default_config_hash <- function() {
  if (!exists("HARMONIZATION_CONFIG", envir = .GlobalEnv)) return(NULL)
  harm_config_hash(get("HARMONIZATION_CONFIG", envir = .GlobalEnv))
}
```

- [ ] **Step 4: Teach `read_cache_field()` the hash**

In `R/functions/validation_utils.R`, replace:

```r
#' @param max_age_days Maximum cache age in days (default 30).
#' @return The field's value, or NULL if absent/stale/missing/unreadable.
#' @export
read_cache_field <- function(cache_file, field, max_age_days = 30) {
```

with:

```r
#' @param max_age_days Maximum cache age in days (default 30).
#' @param config_hash Optional harm_config_hash() of the reader's session. When
#'   given, an envelope stamped with a different hash - or with none, i.e.
#'   written before B1 - is a miss (spec B F72: harmonized codes depend on the
#'   session's harmonization config).
#' @return The field's value, or NULL if absent/stale/missing/unreadable/foreign-config.
#' @export
read_cache_field <- function(cache_file, field, max_age_days = 30, config_hash = NULL) {
```

and, in the same function, replace:

```r
  if (difftime(Sys.time(), cached$timestamp, units = "days") >= max_age_days) {
    return(NULL)
  }
  cached[[field]]
```

with:

```r
  if (difftime(Sys.time(), cached$timestamp, units = "days") >= max_age_days) {
    return(NULL)
  }
  if (!is.null(config_hash) && !identical(cached$config_hash, config_hash)) {
    return(NULL)
  }
  cached[[field]]
```

- [ ] **Step 5: Stamp and check the hash in the orchestrator**

In `R/functions/trait_lookup/orchestrator.R`, replace (reader, ~L242-245):

```r
  # as a miss instead of returning NULL traits (deep-analysis #4).
  if (!is.null(cache_dir) && dir.exists(cache_dir)) {
    cache_file <- file.path(cache_dir, paste0(gsub(" ", "_", species_name), ".rds"))
    cached_traits <- read_cache_field(cache_file, "traits")
```

with:

```r
  # as a miss instead of returning NULL traits (deep-analysis #4). The config
  # hash makes a row harmonized under another session's settings a miss (F72).
  if (!is.null(cache_dir) && dir.exists(cache_dir)) {
    cache_file <- file.path(cache_dir, paste0(gsub(" ", "_", species_name), ".rds"))
    cached_traits <- read_cache_field(cache_file, "traits", config_hash = harm_config_hash())
```

Replace (offline-DB writer, ~L424-425):

```r
        cache_file <- file.path(cache_dir, paste0(gsub(" ", "_", species_name), ".rds"))
        saveRDS(list(traits = result, timestamp = Sys.time()), cache_file)
```

with:

```r
        cache_file <- file.path(cache_dir, paste0(gsub(" ", "_", species_name), ".rds"))
        # Offline-DB codes were harmonized at build time with the defaults.
        saveRDS(list(traits = result, timestamp = Sys.time(),
                     config_hash = harm_default_config_hash()), cache_file)
```

Replace (full-pipeline writer, ~L1738-1742):

```r
      species = species_name,
      timestamp = Sys.time()
    )

    # Include raw traits for reference
```

with:

```r
      species = species_name,
      timestamp = Sys.time(),
      config_hash = harm_config_hash()
    )

    # Include raw traits for reference
```

- [ ] **Step 6: Route the Trait Research cache read through the helper**

In `R/modules/trait_research_server.R`, replace:

```r
    tryCatch({
      results_list <- list()
      raw_list <- list()

      for (i in seq_along(species_list)) {
```

with:

```r
    tryCatch({
      results_list <- list()
      raw_list <- list()
      # This session's harmonization settings key the shared trait cache (F72).
      cfg_hash <- harm_config_hash()

      for (i in seq_along(species_list)) {
```

and replace:

```r
        # Check cache
        cache_file <- file.path(cache_dir, paste0(gsub(" ", "_", species), ".rds"))
        if (file.exists(cache_file)) {
          cached <- readRDS(cache_file)
          cache_age_days <- as.numeric(difftime(Sys.time(), cached$timestamp %||% Sys.time(), units = "days"))
          # Shape guard: a classify_species_api {data,...} envelope collides on
          # the same filename; without !is.null(cached$traits) we'd cache NULL
          # trait rows and misalign the results list (deep-analysis #4).
          if (cache_age_days < 30 && !is.null(cached$traits)) {
            cat("  -> Using cached data\n")
            results_list[[i]] <- cached$traits
            # cached$raw_data is only present in legacy caches written by the
            # old re-save here; orchestrator-written caches don't carry it.
            # Synthesise a minimal raw_data summary from the traits row so the
            # raw-details UI still has something to render either way.
            raw_list[[species]] <- if (!is.null(cached$raw_data)) {
              cached$raw_data
            } else {
              list(species = species,
                   source = if (is.data.frame(cached$traits)) cached$traits$source else NA)
            }
            next
          }
        }
```

with:

```r
        # Check cache. read_cache_field() applies the 30-day TTL, the shape
        # guard (a classify_species_api {data,...} envelope collides on the
        # same filename - deep-analysis #4) and the config-hash match (F72).
        cache_file <- file.path(cache_dir, paste0(gsub(" ", "_", species), ".rds"))
        cached_traits <- read_cache_field(cache_file, "traits", config_hash = cfg_hash)
        if (!is.null(cached_traits)) {
          cat("  -> Using cached data\n")
          results_list[[i]] <- cached_traits
          # raw_data is only present in legacy caches written by the old
          # re-save here; orchestrator-written caches don't carry it.
          # Synthesise a minimal raw_data summary from the traits row so the
          # raw-details UI still has something to render either way.
          cached_raw <- read_cache_field(cache_file, "raw_data", config_hash = cfg_hash)
          raw_list[[species]] <- if (!is.null(cached_raw)) {
            cached_raw
          } else {
            list(species = species,
                 source = if (is.data.frame(cached_traits)) cached_traits$source else NA)
          }
          next
        }
```

(Behaviour note: legacy envelopes carry no hash, so `cached_raw` is always NULL for them and the synthesised summary is used - harmless. An envelope with no timestamp used to count as fresh here; it is now a miss, matching the orchestrator.)

- [ ] **Step 7: Same for the batch reader**

In `R/functions/parallel_lookup.R`, replace:

```r
  # Parallel batch processing
  results <- future_lapply(seq_along(species_list), function(i) {
```

with:

```r
  # The caller's harmonization settings key the shared trait cache (F72).
  # Computed here, in the calling process: a worker has no Shiny session, so
  # harm_config_hash() inside it would always see the process default.
  cfg_hash <- harm_config_hash()

  # Parallel batch processing
  results <- future_lapply(seq_along(species_list), function(i) {
```

and replace:

```r
      cached <- read_cache_field(cache_file, "traits")
```

with:

```r
      cached <- read_cache_field(cache_file, "traits", config_hash = cfg_hash)
```

- [ ] **Step 8: Parse-check and run**

```bash
for f in R/functions/trait_lookup/harmonization.R R/functions/validation_utils.R R/functions/trait_lookup/orchestrator.R R/modules/trait_research_server.R R/functions/parallel_lookup.R; do "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "invisible(parse(file='$f')); cat('OK $f\n')"; done
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-trait-cache-config-hash.R')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-deep-analysis-fixes.R')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-trait-research-datras.R')"
```

Expected: five `OK` lines; config-hash `[ FAIL 0 | ... | PASS 19 ]`; test-deep-analysis-fixes `FAIL 0` (its `read_cache_field` and parallel_lookup guards still hold); test-trait-research-datras `FAIL 0`.

- [ ] **Step 9: Document the convention**

In `CONTRIBUTING.md`, replace the line `## Commit Messages` with:

```markdown
- **Trait-cache envelopes carry `config_hash`.** Harmonized codes depend on
  the session's harmonization settings, and every session shares
  `cache/taxonomy/<species>.rds`. A writer stamps `config_hash =
  harm_config_hash()` (or `harm_default_config_hash()` when the codes were
  harmonized with the defaults, e.g. offline-DB rows); a reader passes
  `config_hash = harm_config_hash()` to `read_cache_field()`, which treats a
  different or missing hash as a miss. Compute the hash in the calling
  process, never inside a `future` worker (a worker has no Shiny session).

## Commit Messages
```

- [ ] **Step 10: Commit**

```bash
git add R/functions/trait_lookup/harmonization.R R/functions/validation_utils.R R/functions/trait_lookup/orchestrator.R R/modules/trait_research_server.R R/functions/parallel_lookup.R tests/testthat/test-trait-cache-config-hash.R CONTRIBUTING.md
git commit -m "$(cat <<'EOF'
fix(traits): key the shared trait cache on the harmonization config hash (F72)

Cached harmonized codes were shared by every session regardless of its
settings. Envelopes now carry config_hash (session hash for pipeline
results, default hash for offline-DB rows) and every reader passes its
own hash to read_cache_field(); a different or missing hash is a miss.
Pre-B1 envelopes therefore re-query once after the upgrade.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 4: Harmonization UI keyed to the real config (F75, UI half)

**Files:**
- Replace: `R/ui/harmonization_settings_ui.R`
- Create: `tests/testthat/test-ui-inputs-have-handlers.R`

**Interfaces:**
- Consumes: `HARMONIZATION_CONFIG$foraging_patterns` names; the `is_rule_enabled("<literal>")` calls under `R/`.
- Produces: `HARM_FS_PATTERN_LABELS` (names = the 8 FS config keys); `CONSUMED_TAXONOMIC_RULES` (names = the 11 consumed rules); inputs `harm_pattern_<FS key>` (value `""`), `harm_rule_<rule>`, buttons `harm_save_config` ("Save as server default"), `harm_reset_server_default`, `harm_reset_defaults`. Removed: `harm_pattern_FS0..FS6`, `harm_rule_fish_swimmers`, `harm_rule_gastropods_crawlers`, `harm_rule_phytoplankton_producers`, `harm_rule_bivalves_filter`, `harm_rule_molluscs_shells`, `harm_rule_arthropods_exo`, `harm_cancel`, the `harm_tabs` tabset id. (`harm_rule_bivalves_sessile` keeps its ID.)

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-ui-inputs-have-handlers.R`:

```r
# Spec B section 4.1 (F75): the Harmonization tab shipped seven rule
# checkboxes, six FS pattern inputs, a profile-effects panel and a Cancel
# button that no server code ever read. These guards pin the UI to the config
# keys and rules the harmonizer actually uses.

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")

test_that("the rule checkboxes are exactly the rules the harmonize_* code reads", {
  source(file.path(app_root, "R/ui/harmonization_settings_ui.R"), local = FALSE)
  files <- list.files(file.path(app_root, "R"), pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  code <- unlist(lapply(files, readLines, warn = FALSE))
  code <- code[!startsWith(trimws(code), "#")]
  calls <- regmatches(code, gregexpr('is_rule_enabled\\("[A-Za-z0-9_]+"\\)', code))
  read_rules <- unique(sub('is_rule_enabled\\("(.*)"\\)', "\\1", unlist(calls)))

  expect_gt(length(read_rules), 5L)
  expect_setequal(names(get0("CONSUMED_TAXONOMIC_RULES", ifnotfound = character())), read_rules)
})

test_that("the FS pattern inputs cover every configured foraging pattern", {
  source(file.path(app_root, "R/config/harmonization_config.R"), local = FALSE)
  source(file.path(app_root, "R/ui/harmonization_settings_ui.R"), local = FALSE)
  expect_setequal(names(get0("HARM_FS_PATTERN_LABELS", ifnotfound = character())),
                  names(HARMONIZATION_CONFIG$foraging_patterns))
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-ui-inputs-have-handlers.R')"`
Expected: `FAIL 2` - both `expect_setequal` calls report the empty vector against the 11 rules / 8 FS keys.

- [ ] **Step 3: Replace the UI file**

Replace the whole of `R/ui/harmonization_settings_ui.R` with:

```r
# Harmonization Settings UI
# GUI for configuring trait harmonization thresholds and rules

# One text input per configured foraging pattern (inputId harm_pattern_<key>).
# Keys match HARMONIZATION_CONFIG$foraging_patterns exactly. The inputs start
# EMPTY: the server pushes the real patterns from the session config at start,
# so no stale literal (FS0 once carried "plant|algae", removed 2026-07-17
# because it inverted trophic structure) can ever be the source of truth.
HARM_FS_PATTERN_LABELS <- c(
  FS0_primary_producer = "FS0 - Primary producer:",
  FS1_predator = "FS1 - Predator:",
  FS2_scavenger = "FS2 - Scavenger / detritivore:",
  FS3_omnivore = "FS3 - Omnivore:",
  FS4_grazer = "FS4 - Grazer / herbivore:",
  FS5_deposit = "FS5 - Deposit feeder:",
  FS6_filter = "FS6 - Filter / suspension feeder:",
  FS7_xylophagous = "FS7 - Xylophagous:"
)

# The taxonomic rules the harmonize_* code actually reads through
# is_rule_enabled() (R/functions/trait_lookup/harmonization.R). One checkbox
# per rule, inputId harm_rule_<name>; the server wires the same vector.
# tests/testthat/test-ui-inputs-have-handlers.R fails if this list and the
# is_rule_enabled() calls drift apart.
CONSUMED_TAXONOMIC_RULES <- c(
  fish_obligate_swimmers = "Fish -> MB5 (swimmer)",
  cephalopods_swimmers = "Cephalopods -> MB5 (swimmer)",
  bivalves_sessile = "Bivalves -> MB1 (sessile)",
  cnidarians_sessile = "Cnidarians -> MB1 (sessile)",
  phytoplankton_pelagic = "Phytoplankton -> EP1 (pelagic)",
  zooplankton_pelagic = "Copepods / cladocerans -> EP1 (pelagic)",
  infaunal_bivalves = "Bivalves -> EP4 (endobenthic)",
  bivalves_hard_shell = "Bivalves -> PR6 (hard shell)",
  gastropods_hard_shell = "Gastropods -> PR6 (hard shell)",
  crustaceans_exoskeleton = "Crustaceans -> PR8 / PR4 (exoskeleton)",
  echinoderms_calcium_plates = "Echinoderms -> PR5 (calcium plates)"
)

harm_rule_checkboxes <- function(rules) {
  lapply(rules, function(rule) {
    checkboxInput(paste0("harm_rule_", rule), CONSUMED_TAXONOMIC_RULES[[rule]], TRUE)
  })
}

harmonization_settings_ui <- function() {
  fs_keys <- names(HARM_FS_PATTERN_LABELS)
  fs_input <- function(key) textInput(paste0("harm_pattern_", key), HARM_FS_PATTERN_LABELS[[key]], value = "")

  tagList(
    h3(icon("sliders-h"), " Harmonization Configuration"),
    p("Configure how raw trait data is converted to categorical trait classes."),
    hr(),

    tabsetPanel(
      type = "pills",

      # TAB 1: SIZE THRESHOLDS
      tabPanel(
        title = tagList(icon("ruler"), " Size Thresholds"),
        value = "size_tab",
        br(),

        fluidRow(
          column(6,
            h4("Maximum Size (MS) Class Boundaries"),
            sliderInput("harm_thresh_MS1_MS2", "MS1/MS2 boundary:",
                       min = 0.01, max = 0.5, value = 0.1, step = 0.01, post = " cm"),
            sliderInput("harm_thresh_MS2_MS3", "MS2/MS3 boundary:",
                       min = 0.1, max = 5.0, value = 1.0, step = 0.1, post = " cm"),
            sliderInput("harm_thresh_MS3_MS4", "MS3/MS4 boundary:",
                       min = 1.0, max = 20.0, value = 5.0, step = 0.5, post = " cm"),
            sliderInput("harm_thresh_MS4_MS5", "MS4/MS5 boundary:",
                       min = 5.0, max = 50.0, value = 20.0, step = 1.0, post = " cm"),
            sliderInput("harm_thresh_MS5_MS6", "MS5/MS6 boundary:",
                       min = 20.0, max = 100.0, value = 50.0, step = 5.0, post = " cm"),
            sliderInput("harm_thresh_MS6_MS7", "MS6/MS7 boundary:",
                       min = 50.0, max = 300.0, value = 150.0, step = 10.0, post = " cm")
          ),
          column(6,
            h4("Size Class Preview"),
            actionButton("harm_preview_size", "Generate Preview", class = "btn-info btn-sm"),
            br(), br(),
            plotOutput("harm_size_distribution_plot", height = "400px")
          )
        )
      ),

      # TAB 2: FORAGING PATTERNS
      tabPanel(
        title = tagList(icon("utensils"), " Foraging Patterns"),
        value = "foraging_tab",
        br(),
        h4("Foraging Strategy (FS) Pattern Matching"),
        p(class = "text-muted",
          "Regular expressions matched (case-insensitive) against feeding text, FS0 first. ",
          "An invalid or empty pattern is ignored and the previous one kept."),
        fluidRow(
          column(6, lapply(fs_keys[1:4], fs_input)),
          column(6, lapply(fs_keys[5:8], fs_input))
        )
      ),

      # TAB 3: TAXONOMIC RULES
      tabPanel(
        title = tagList(icon("dna"), " Taxonomic Rules"),
        value = "taxonomic_tab",
        br(),
        h4("Taxonomic Inference Rules"),
        fluidRow(
          column(4,
            h5("Mobility Rules"),
            harm_rule_checkboxes(c("fish_obligate_swimmers", "cephalopods_swimmers",
                                   "bivalves_sessile", "cnidarians_sessile"))
          ),
          column(4,
            h5("Position Rules"),
            harm_rule_checkboxes(c("phytoplankton_pelagic", "zooplankton_pelagic",
                                   "infaunal_bivalves"))
          ),
          column(4,
            h5("Protection Rules"),
            harm_rule_checkboxes(c("bivalves_hard_shell", "gastropods_hard_shell",
                                   "crustaceans_exoskeleton", "echinoderms_calcium_plates"))
          )
        )
      ),

      # TAB 4: ECOSYSTEM PROFILES
      tabPanel(
        title = tagList(icon("water"), " Ecosystem Profiles"),
        value = "ecosystem_tab",
        br(),
        h4("Ecosystem-Specific Harmonization"),
        fluidRow(
          column(6,
            selectInput("harm_active_profile", "Active Profile:",
                       choices = c(
                         "Temperate (North Sea)" = "temperate",
                         "Mediterranean" = "mediterranean",
                         "Atlantic NE" = "atlantic_ne",
                         "Arctic/Nordic" = "arctic",
                         "Baltic Sea" = "baltic",
                         "Black Sea" = "black_sea",
                         "Tropical/Subtropical" = "tropical",
                         "Deep Sea" = "deep_sea"
                       ),
                       selected = "temperate"),
            br(),
            uiOutput("harm_profile_details")
          ),
          column(6,
            h5("Profile Effects"),
            verbatimTextOutput("harm_profile_effects")
          )
        )
      ),

      # TAB 5: IMPORT/EXPORT
      tabPanel(
        title = tagList(icon("file-export"), " Import/Export"),
        value = "import_export_tab",
        br(),
        h4("Save and Load Configurations"),
        fluidRow(
          column(6,
            h5("Export Configuration"),
            downloadButton("harm_export_json", "Export as JSON", class = "btn-success")
          ),
          column(6,
            h5("Import Configuration"),
            fileInput("harm_import_json", "Select JSON:", accept = c(".json")),
            p(class = "text-muted", "An imported file applies to your session only.")
          )
        )
      )
    ),

    hr(),

    # ACTION BUTTONS
    fluidRow(
      column(12,
        div(style = "text-align: center;",
          actionButton("harm_reset_defaults", "Reset to Defaults", class = "btn-warning"),
          actionButton("harm_save_config", "Save as server default",
                       class = "btn-success", icon = icon("lock")),
          actionButton("harm_reset_server_default", "Reset server default",
                       class = "btn-outline-danger", icon = icon("lock"))
        ),
        p(class = "text-muted", style = "text-align: center; margin-top: 8px;",
          "Changes on this tab apply to your session only. Saving or resetting the server default ",
          "needs an admin unlock (Trait Research > Configure API Keys).")
      )
    ),

    br(),
    uiOutput("harm_status_message")
  )
}
```

- [ ] **Step 4: Parse-check and run**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='R/ui/harmonization_settings_ui.R'); cat('OK\n')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-ui-inputs-have-handlers.R')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer4-ui.R')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-harmonization-settings-server.R')"
```

Expected: `OK`; ui-inputs `FAIL 0` (2 tests); test-layer4-ui `FAIL 0` (its "8 ecosystem profiles" test greps this file); the B0 server file `FAIL 0` (the old module still works with the new UI; the new widgets are simply not wired until Task 5).

- [ ] **Step 5: Commit**

```bash
git add R/ui/harmonization_settings_ui.R tests/testthat/test-ui-inputs-have-handlers.R
git commit -m "$(cat <<'EOF'
fix(harmonization): key harmonization widgets to the real config (F75)

FS pattern inputs now use the config keys (adds FS3/FS7) and start empty,
so the stale FS0 "plant|algae" literal can never be the source of truth.
The rule checkboxes are generated from the 11 rules is_rule_enabled()
actually reads; the 5 unread rule boxes, Cancel and the unused tabset id
are removed. "Apply Changes" becomes "Save as server default" and a
"Reset server default" button is added (wired in the next commit).

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 5: Session-only settings module with a strict-gated server default (F2, F8, F75 server half, B0 minors)

**Files:**
- Replace: `R/modules/harmonization_settings_server.R`
- Replace: `tests/testthat/test-harmonization-settings-server.R`
- Modify: `tests/testthat/test-ui-inputs-have-handlers.R` (append)
- Modify: `CONTRIBUTING.md`

**Interfaces:**
- Consumes: `admin_strict_refusal()` (Task 1); `HARM_THRESHOLD_KEYS`, `harm_pattern_compiles()`, `validate_harmonization_config()`, `save_harmonization_config()`, `load_harmonization_config()`, `export_config_json()`, `import_config_json()` (Task 2); `harm_config_hash()` (Task 3); `HARM_FS_PATTERN_LABELS`, `CONSUMED_TAXONOMIC_RULES` (Task 4); `apply_size_adjustment()`, `get_harm_config()` (existing).
- Produces: `harmonization_settings_server(input, output, session)` with module-local `rv` (`config`, `saved_hash`, `unsaved_changes`), `set_session_config(cfg)`, `push_config_to_widgets(cfg)`; outputs `harm_status_message`, `harm_profile_effects`, `harm_profile_details`, `harm_size_distribution_plot`, `harm_export_json`.

- [ ] **Step 1: Replace the test file**

Replace the whole of `tests/testthat/test-harmonization-settings-server.R` with the content below. The seven B0 tests are kept (the unparseable-JSON test now asserts exactly one warning and one load; the partial-file test carries the Task 2 change; the concurrent-session test now keeps both sessions alive). **Why `start_harm_session()` exists:** `testServer()` never feeds `update*Input()` messages back into `input` (the value stays NULL), and it cannot keep two sessions alive at once. The helper builds a `MockShinySession` by hand, replaces its `sendInputMessage` with a recorder, and runs the module in it. Do not replace it with `testServer`.

```r
# Regression tests for F1/F76 (deep analysis 2026-09-26, spec B section 4.0).
# Pre-B0 the INITIALIZE observe() in harmonization_settings_server.R re-read
# config/harmonization_custom.json and read rv$config reactively, while the
# UPDATE observe() wrote rv$config from the sliders. Each observer
# re-triggered the other, so one slider move pinned the single R process.
# These are the repo's first shiny::testServer() tests; testServer accepts
# the module's plain function(input, output, session) signature directly.
#
# B1 (spec B section 4.1) adds: the strict admin gate on the two server-default
# buttons (F2), JSON import/export (F8), the wired FS pattern / rule / profile
# widgets (F75), and the B0 deferred minors (RESET echo, exactly-one warning,
# a real two-live-sessions test, unsaved_changes on page load).

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")

source_harm_module <- function() {
  suppressPackageStartupMessages(library(shiny)) # the UI/module code is unqualified, as in app.R
  source(file.path(app_root, "R/functions/validation_utils.R"), local = FALSE)
  source(file.path(app_root, "R/functions/admin_auth.R"), local = FALSE)
  source(file.path(app_root, "R/config/harmonization_config.R"), local = FALSE)
  source(file.path(app_root, "R/functions/trait_lookup/harmonization.R"), local = FALSE)
  source(file.path(app_root, "R/ui/harmonization_settings_ui.R"), local = FALSE)
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

# A path that does not exist yet, removed again when the test exits.
new_harm_path <- function(env = parent.frame()) {
  path <- tempfile("harm_target_", fileext = ".json")
  withr::defer(unlink(c(path, paste0(path, ".tmp"))), envir = env)
  path
}

set_all_thresholds <- function(session, ms3_ms4, ms2_ms3 = 1.0) {
  session$setInputs(
    harm_thresh_MS1_MS2 = 0.1, harm_thresh_MS2_MS3 = ms2_ms3,
    harm_thresh_MS3_MS4 = ms3_ms4, harm_thresh_MS4_MS5 = 20.0,
    harm_thresh_MS5_MS6 = 50.0, harm_thresh_MS6_MS7 = 150.0
  )
}

# admin_gate_enabled() only checks that the record is non-blank, so any
# non-blank string switches the gate on without needing openssl.
GATE_ON <- "econetool1$12$00$00"

# Start the module in a hand-built MockShinySession. Unlike testServer() this
# records every update*Input() message the module sends (testServer drops
# them) and lets two sessions stay alive at the same time.
start_harm_session <- function(env = parent.frame()) {
  s <- shiny::MockShinySession$new()
  sent <- new.env()
  sent$msgs <- list()
  s$sendInputMessage <- function(inputId, message) sent$msgs[[inputId]] <- message
  shiny::isolate(shiny::withReactiveDomain(s, harmonization_settings_server(s$input, s$output, s)))
  s$flushReact()
  withr::defer(s$close(), envir = env)
  list(session = s, sent = sent)
}

read_json_file <- function(path) jsonlite::fromJSON(path, simplifyVector = FALSE)

# ---------------------------------------------------------------------------
# B0: the observer loop (F1/F76)
# ---------------------------------------------------------------------------

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

test_that("an unparseable server-default file warns exactly once and falls back to defaults", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  bad <- tempfile("harm_bad_", fileext = ".json")
  writeLines("{ not valid json", bad)
  withr::defer(unlink(bad))
  calls <- local_harm_loader(bad)

  seen <- character()
  withCallingHandlers(
    shiny::testServer(harmonization_settings_server, {
      expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4,
                   HARMONIZATION_CONFIG$size_thresholds$MS3_MS4)
      set_all_thresholds(session, ms3_ms4 = 9)
      expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4, 9)
    }),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(seen, 1L)
  expect_match(seen, "could not parse")
  expect_equal(calls$n, 1L)
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

test_that("a server-default file missing a threshold is filled from the defaults at start", {
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
    # B1: load_harmonization_config() now runs the shared validator, which
    # fills missing keys from HARMONIZATION_CONFIG (pre-B1 this was NULL).
    expect_equal(session$userData$harm_config$size_thresholds$MS6_MS7, 150)
    set_all_thresholds(session, ms3_ms4 = 9)
    expect_equal(session$userData$harm_config$size_thresholds$MS6_MS7, 150)
    expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4, 9)
  })

  expect_equal(calls$n, 1L)
})

test_that("two live sessions keep independent configs and the global default is untouched", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  calls <- local_harm_loader(write_harm_json(ms3_ms4 = 7))
  default_before <- HARMONIZATION_CONFIG

  a <- start_harm_session()
  b <- start_harm_session()
  set_all_thresholds(b$session, ms3_ms4 = 9)
  set_all_thresholds(a$session, ms3_ms4 = 8)

  # Both sessions are still alive here; each sees only its own move.
  expect_equal(a$session$userData$harm_config$size_thresholds$MS3_MS4, 8)
  expect_equal(b$session$userData$harm_config$size_thresholds$MS3_MS4, 9)
  expect_identical(HARMONIZATION_CONFIG, default_before)
  expect_equal(calls$n, 2L)
})

# ---------------------------------------------------------------------------
# B1 / F2: server-default save and reset are strictly admin-gated
# ---------------------------------------------------------------------------

test_that("save is refused, with a warning and no file, when the admin gate is unset", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = "")
  target <- new_harm_path()
  local_harm_loader(target)

  shiny::testServer(harmonization_settings_server, {
    session$userData$admin_unlocked <- TRUE # even a claimed unlock must not help
    expect_warning(session$setInputs(harm_save_config = 1),
                   "Admin gate not configured on this instance", fixed = TRUE)
    expect_match(output$harm_status_message$html, "server defaults are read-only", fixed = TRUE)
  })
  expect_false(file.exists(target))
})

test_that("save is refused when the gate is set but the session is locked", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = GATE_ON)
  target <- new_harm_path()
  local_harm_loader(target)

  shiny::testServer(harmonization_settings_server, {
    expect_warning(session$setInputs(harm_save_config = 1),
                   "Unlock via Trait Research > Configure API Keys first", fixed = TRUE)
  })
  expect_false(file.exists(target))
})

test_that("save by an unlocked admin writes a file that round-trips", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = GATE_ON)
  target <- new_harm_path()
  calls <- local_harm_loader(target)

  shiny::testServer(harmonization_settings_server, {
    session$userData$admin_unlocked <- TRUE
    set_all_thresholds(session, ms3_ms4 = 9)
    expect_true(shiny::isolate(rv$unsaved_changes))
    suppressMessages(session$setInputs(harm_save_config = 1))
    expect_false(shiny::isolate(rv$unsaved_changes))
  })
  expect_true(file.exists(target))
  expect_false(file.exists(paste0(target, ".tmp")))
  expect_equal(calls$n, 0L)
  expect_equal(load_harmonization_config(target)$size_thresholds$MS3_MS4, 9)
})

test_that("an FS0 pattern with diet nouns is never saved as the server default", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = GATE_ON)
  target <- new_harm_path()
  local_harm_loader(target)

  shiny::testServer(harmonization_settings_server, {
    session$userData$admin_unlocked <- TRUE
    session$setInputs(harm_pattern_FS0_primary_producer = "photosyn|producer|plant|algae")
    session$elapse(600)
    session$setInputs(harm_save_config = 1)
    expect_match(output$harm_status_message$html, "diet nouns", fixed = TRUE)
  })
  expect_false(file.exists(target))
})

test_that("an invalid session config (overlapping sliders) is not saved, even by an admin", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = GATE_ON)
  target <- new_harm_path()
  local_harm_loader(target)

  shiny::testServer(harmonization_settings_server, {
    session$userData$admin_unlocked <- TRUE
    # The MS2/MS3 slider goes up to 5 and the MS3/MS4 slider down to 1, so the
    # UI itself can produce non-increasing boundaries.
    set_all_thresholds(session, ms3_ms4 = 2, ms2_ms3 = 4)
    session$setInputs(harm_save_config = 1)
    expect_match(output$harm_status_message$html, "strictly increasing", fixed = TRUE)
  })
  expect_false(file.exists(target))
})

test_that("a save that cannot write warns and reports the failure", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = GATE_ON)
  blocker <- tempfile("harm_blocker_")
  writeLines("a file, not a directory", blocker)
  withr::defer(unlink(blocker))
  local_harm_loader(file.path(blocker, "harmonization_custom.json"))

  shiny::testServer(harmonization_settings_server, {
    session$userData$admin_unlocked <- TRUE
    expect_warning(suppressMessages(session$setInputs(harm_save_config = 1)),
                   "[harmonization] save failed", fixed = TRUE)
    expect_match(output$harm_status_message$html, "Save failed", fixed = TRUE)
  })
})

test_that("reset server default is gated and, when unlocked, removes the file", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  path <- write_harm_json(ms3_ms4 = 7)
  local_harm_loader(path)

  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = "")
  shiny::testServer(harmonization_settings_server, {
    session$userData$admin_unlocked <- TRUE
    expect_warning(session$setInputs(harm_reset_server_default = 1), "[admin auth]", fixed = TRUE)
  })
  expect_true(file.exists(path))

  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = GATE_ON)
  shiny::testServer(harmonization_settings_server, {
    session$userData$admin_unlocked <- TRUE
    session$setInputs(harm_reset_server_default = 1)
    expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4, 7,
                 info = "resetting the server default does not change this session")
  })
  expect_false(file.exists(path))
})

test_that("one session's unlock does not authorize another live session", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = GATE_ON)
  target <- new_harm_path()
  local_harm_loader(target)

  admin <- start_harm_session()
  other <- start_harm_session()
  admin$session$userData$admin_unlocked <- TRUE

  expect_warning(other$session$setInputs(harm_save_config = 1), "Unlock via Trait Research", fixed = TRUE)
  expect_false(file.exists(target))
  suppressMessages(admin$session$setInputs(harm_save_config = 1))
  expect_true(file.exists(target))
})

# ---------------------------------------------------------------------------
# B1 / F8: JSON import (session-only) and export (anyone)
# ---------------------------------------------------------------------------

test_that("import is session-only and pushes the imported values to the widgets", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  target <- new_harm_path()
  local_harm_loader(target)
  upload <- tempfile("harm_upload_", fileext = ".json")
  withr::defer(unlink(upload))
  cfg <- HARMONIZATION_CONFIG
  cfg$size_thresholds$MS3_MS4 <- 11
  cfg$taxonomic_rules$bivalves_sessile <- FALSE
  export_config_json(upload, cfg)

  h <- start_harm_session()
  h$sent$msgs <- list()
  h$session$setInputs(harm_import_json = list(name = "mine.json", datapath = upload))

  expect_equal(h$session$userData$harm_config$size_thresholds$MS3_MS4, 11)
  expect_false(h$session$userData$harm_config$taxonomic_rules$bivalves_sessile)
  expect_identical(h$sent$msgs$harm_thresh_MS3_MS4$value, "11")
  expect_false(h$sent$msgs$harm_rule_bivalves_sessile$value)
  expect_false(file.exists(target))
  # Outside any session the process-wide default is untouched.
  expect_equal(get_harm_config()$size_thresholds$MS3_MS4, 5)
})

test_that("an invalid import keeps the session config and warns", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  local_harm_loader(new_harm_path())
  upload <- tempfile("harm_upload_", fileext = ".json")
  withr::defer(unlink(upload))
  cfg <- HARMONIZATION_CONFIG
  cfg$size_thresholds$MS3_MS4 <- 0.5 # below MS2_MS3
  export_config_json(upload, cfg)

  shiny::testServer(harmonization_settings_server, {
    expect_warning(session$setInputs(harm_import_json = list(name = "bad.json", datapath = upload)),
                   "[harmonization] import failed", fixed = TRUE)
    expect_equal(session$userData$harm_config$size_thresholds$MS3_MS4, 5)
  })
})

test_that("anyone can download the session config as JSON", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = GATE_ON) # locked session
  local_harm_loader(new_harm_path())

  shiny::testServer(harmonization_settings_server, {
    set_all_thresholds(session, ms3_ms4 = 9)
    downloaded <- output$harm_export_json
    expect_true(file.exists(downloaded))
    expect_equal(import_config_json(downloaded)$size_thresholds$MS3_MS4, 9)
  })
})

# ---------------------------------------------------------------------------
# B1 / F75: every widget reaches the config the harmonize_* code reads
# ---------------------------------------------------------------------------

test_that("the widgets are seeded from the config at start, not from UI literals", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  path <- tempfile("harm_cfg_", fileext = ".json")
  withr::defer(unlink(path))
  cfg <- HARMONIZATION_CONFIG
  cfg$taxonomic_rules$cnidarians_sessile <- FALSE
  cfg$active_profile <- "baltic"
  suppressMessages(save_harmonization_config(cfg, path))
  local_harm_loader(path)

  h <- start_harm_session()
  fs0 <- h$sent$msgs$harm_pattern_FS0_primary_producer$value
  expect_identical(fs0, HARMONIZATION_CONFIG$foraging_patterns$FS0_primary_producer)
  expect_false(grepl("algae|plant", fs0))
  expect_identical(h$sent$msgs$harm_pattern_FS7_xylophagous$value,
                   HARMONIZATION_CONFIG$foraging_patterns$FS7_xylophagous)
  expect_false(h$sent$msgs$harm_rule_cnidarians_sessile$value)
  expect_true(h$sent$msgs$harm_rule_fish_obligate_swimmers$value)
  expect_identical(h$sent$msgs$harm_active_profile$value, "baltic")

  # The UI literal can never be the source of truth: its FS values are empty.
  html <- as.character(harmonization_settings_ui())
  fs_tags <- regmatches(html, gregexpr('<input id="harm_pattern_[A-Za-z0-9_]+"[^>]*>', html))[[1]]
  expect_length(fs_tags, length(HARM_FS_PATTERN_LABELS))
  expect_true(all(grepl('value=""', fs_tags, fixed = TRUE)))
  expect_false(grepl("algae", html))
})

test_that("a rule checkbox reaches is_rule_enabled() in its own session only", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  local_harm_loader(new_harm_path())

  shiny::testServer(harmonization_settings_server, {
    expect_true(shiny::isolate(is_rule_enabled("fish_obligate_swimmers")))
    session$setInputs(harm_rule_fish_obligate_swimmers = FALSE)
    expect_false(shiny::isolate(is_rule_enabled("fish_obligate_swimmers")))
  })
  expect_true(is_rule_enabled("fish_obligate_swimmers"))
})

test_that("an FS pattern edit reaches the consumer after the debounce; bad input keeps the old value", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  local_harm_loader(new_harm_path())
  old <- HARMONIZATION_CONFIG$foraging_patterns$FS1_predator

  shiny::testServer(harmonization_settings_server, {
    session$setInputs(harm_pattern_FS1_predator = "predat|hunter|ambush")
    session$elapse(600)
    expect_identical(session$userData$harm_config$foraging_patterns$FS1_predator,
                     "predat|hunter|ambush")
    expect_identical(shiny::isolate(harmonize_foraging_strategy("ambush feeder")), "FS1")

    session$setInputs(harm_pattern_FS1_predator = "predat(")
    session$elapse(600)
    expect_identical(session$userData$harm_config$foraging_patterns$FS1_predator,
                     "predat|hunter|ambush")

    # A browser reports the UI's empty value before the push lands; an empty
    # pattern would match every text, so it must be ignored.
    session$setInputs(harm_pattern_FS0_primary_producer = "")
    session$elapse(600)
    expect_identical(session$userData$harm_config$foraging_patterns$FS0_primary_producer,
                     HARMONIZATION_CONFIG$foraging_patterns$FS0_primary_producer)
  })
  expect_identical(HARMONIZATION_CONFIG$foraging_patterns$FS1_predator, old)
})

test_that("the profile select reaches apply_size_adjustment() and the effects panel", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  local_harm_loader(new_harm_path())

  shiny::testServer(harmonization_settings_server, {
    session$setInputs(harm_active_profile = "arctic")
    expect_identical(session$userData$harm_config$active_profile, "arctic")
    expect_equal(shiny::isolate(apply_size_adjustment(10)), 12)
    expect_match(output$harm_profile_effects, "arctic", fixed = TRUE)
    expect_match(output$harm_profile_effects, "1.2", fixed = TRUE)

    session$setInputs(harm_active_profile = "atlantis") # not a profile: ignored
    expect_identical(session$userData$harm_config$active_profile, "arctic")
  })
})

# ---------------------------------------------------------------------------
# B0 deferred minors: RESET echo and unsaved_changes on page load
# ---------------------------------------------------------------------------

test_that("Reset to Defaults is session-only, pushes the widgets and its echo settles", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  path <- write_harm_json(ms3_ms4 = 7)
  calls <- local_harm_loader(path)

  h <- start_harm_session()
  set_all_thresholds(h$session, ms3_ms4 = 9)
  h$sent$msgs <- list()
  h$session$setInputs(harm_reset_defaults = 1)

  expect_equal(h$session$userData$harm_config$size_thresholds$MS3_MS4, 5)
  expect_identical(h$sent$msgs$harm_thresh_MS3_MS4$value, "5")
  # The browser echoes the pushed values back; nothing reloads or re-pushes.
  h$sent$msgs <- list()
  set_all_thresholds(h$session, ms3_ms4 = 5)
  expect_equal(h$session$userData$harm_config$size_thresholds$MS3_MS4, 5)
  expect_length(h$sent$msgs, 0L)
  expect_equal(calls$n, 1L)
  expect_equal(read_json_file(path)$size_thresholds$MS3_MS4, 7) # server file untouched
})

test_that("page load does not flag unsaved changes; a real move does", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  source_harm_module()
  local_harm_loader(write_harm_json(ms3_ms4 = 7))

  shiny::testServer(harmonization_settings_server, {
    set_all_thresholds(session, ms3_ms4 = 5) # UI literal first
    set_all_thresholds(session, ms3_ms4 = 7) # then the pushed file value
    expect_false(shiny::isolate(rv$unsaved_changes))
    set_all_thresholds(session, ms3_ms4 = 9)
    expect_true(shiny::isolate(rv$unsaved_changes))
  })
})
```

- [ ] **Step 2: Append the dead-UI guard that needs the server**

Append to `tests/testthat/test-ui-inputs-have-handlers.R`:

```r

rendered_harm_ids <- function() {
  suppressPackageStartupMessages(library(shiny)) # the UI builders are unqualified, as in app.R
  source(file.path(app_root, "R/ui/harmonization_settings_ui.R"), local = FALSE)
  html <- as.character(harmonization_settings_ui())
  hits <- regmatches(html, gregexpr('\\sid="harm_[A-Za-z0-9_]+"', html))[[1]]
  ids <- unique(sub('^\\sid="(.*)"$', "\\1", hits))
  # fileInput() adds a "<id>_progress" bar div of its own; it is not an input.
  ids[!grepl("_progress$", ids)]
}

server_src <- function() {
  paste(readLines(file.path(app_root, "R/modules/harmonization_settings_server.R"), warn = FALSE),
        collapse = "\n")
}

test_that("every harm_* element in the harmonization UI has a server consumer", {
  skip_if_not_installed("shiny")
  ids <- rendered_harm_ids()
  src <- server_src()

  used <- regmatches(src, gregexpr("(input|output)\\$harm_[A-Za-z0-9_]+", src))[[1]]
  used <- unique(sub("^(input|output)\\$", "", used))
  generated <- c(
    paste0("harm_rule_", names(get0("CONSUMED_TAXONOMIC_RULES", ifnotfound = character()))),
    paste0("harm_pattern_", names(get0("HARM_FS_PATTERN_LABELS", ifnotfound = character())))
  )

  expect_gt(length(ids), 20L)
  expect_identical(setdiff(ids, c(used, generated)), character(0))
})

test_that("generated widget IDs are wired from the same vectors the UI uses", {
  src <- server_src()
  expect_true(grepl('paste0("harm_rule_", rule)', src, fixed = TRUE))
  expect_true(grepl('paste0("harm_pattern_", key)', src, fixed = TRUE))
  expect_true(grepl("names(CONSUMED_TAXONOMIC_RULES)", src, fixed = TRUE))
  expect_true(grepl("names(HARM_FS_PATTERN_LABELS)", src, fixed = TRUE))
})
```

- [ ] **Step 3: Run the tests to verify they fail**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-harmonization-settings-server.R')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-ui-inputs-have-handlers.R')"
```

Expected (old module still in place): the server file ends `FAIL 34 | ... | PASS 52` - the seven B0 tests, the download test and the RESET-echo test pass (regression pins); 14 B1 tests fail (e.g. the diet-noun FS0 save test finds "Configuration saved!" and a written file; "save is refused ... gate is unset" writes nothing but emits no `[admin auth]` warning; "a rule checkbox reaches is_rule_enabled()" still sees TRUE; the seeding test finds no `harm_pattern_FS0_primary_producer` message). The ui file fails its two new tests: `setdiff` lists `harm_reset_server_default`, the `harm_pattern_*` and `harm_rule_*` IDs and `harm_profile_effects`, and the `paste0(...)` greps are FALSE.

- [ ] **Step 4: Replace the module**

Replace the whole of `R/modules/harmonization_settings_server.R` with:

```r
# Harmonization Settings Server Module
#
# Everything on this tab is SESSION-ONLY: sliders, FS patterns, rule
# checkboxes, the profile and JSON import change session$userData$harm_config
# (read by the harmonize_* helpers through get_harm_config()) and nothing
# else. Only "Save as server default" and "Reset server default" touch the
# process-wide HARMONIZATION_CONFIG_FILE, and both require
# admin_authorized_strict() (spec B section 4.1, F2).
#
# Loop safety (B0, F1/F76): widget observers read rv$config only under
# isolate() and write it only through set_session_config(). Each returns early
# when the incoming value already equals the config, so the browser's echo of
# a push_config_to_widgets() update settles in one round.

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
    # Hash of the current server default; unsaved_changes compares with it,
    # so the page-load echo of the widgets does not count as a change.
    saved_hash = harm_config_hash(initial_cfg),
    unsaved_changes = FALSE
  )

  # Seed the per-session harm config so the harmonize_* helpers (which
  # read through get_harm_config() -> session$userData) see the server
  # default from page load. Never write HARMONIZATION_CONFIG to globalenv:
  # that contaminated every concurrent session (pre-PR9α).
  session$userData$harm_config <- initial_cfg

  # The only writer of the session config.
  set_session_config <- function(cfg) {
    rv$config <- cfg
    session$userData$harm_config <- cfg
    rv$unsaved_changes <- !identical(harm_config_hash(cfg), isolate(rv$saved_hash))
  }

  # Drive every widget from a config: sliders, FS pattern inputs, rule
  # checkboxes and the profile select. Called at session start, after import
  # and after Reset to Defaults.
  push_config_to_widgets <- function(cfg) {
    for (key in HARM_THRESHOLD_KEYS) {
      updateSliderInput(session, paste0("harm_thresh_", key), value = cfg$size_thresholds[[key]])
    }
    for (key in names(HARM_FS_PATTERN_LABELS)) {
      updateTextInput(session, paste0("harm_pattern_", key), value = cfg$foraging_patterns[[key]] %||% "")
    }
    for (rule in names(CONSUMED_TAXONOMIC_RULES)) {
      updateCheckboxInput(session, paste0("harm_rule_", rule), value = isTRUE(cfg$taxonomic_rules[[rule]]))
    }
    updateSelectInput(session, "harm_active_profile", selected = cfg$active_profile %||% "temperate")
  }

  show_status <- function(alert_class, icon_name, text) {
    output$harm_status_message <- renderUI({
      div(class = paste("alert", alert_class), icon(icon_name), " ", text)
    })
  }

  # Strict admin gate for the two server-default buttons. Returns TRUE (and
  # tells the user why) when the action must not run.
  refuse_unless_admin <- function(action) {
    refusal <- admin_strict_refusal(session$userData$admin_unlocked, action)
    if (is.null(refusal)) return(FALSE)
    show_status("alert-warning", "lock", refusal)
    showNotification(refusal, type = "warning", duration = 6)
    TRUE
  }

  # INITIALIZE: push the loaded config to every widget once. No reactive
  # read of rv$config, so nothing can re-trigger this.
  observeEvent(TRUE, {
    push_config_to_widgets(initial_cfg)
  }, once = TRUE)

  # UPDATE CONFIG (sliders). Depends on the six slider inputs only: rv$config
  # is read under isolate() and written once, so the write cannot re-trigger
  # this observer. An echo that matches the config is ignored.
  observe({
    req(input$harm_thresh_MS1_MS2, input$harm_thresh_MS2_MS3,
        input$harm_thresh_MS3_MS4, input$harm_thresh_MS4_MS5,
        input$harm_thresh_MS5_MS6, input$harm_thresh_MS6_MS7)
    cfg <- isolate(rv$config)
    incoming <- list(
      MS1_MS2 = input$harm_thresh_MS1_MS2, MS2_MS3 = input$harm_thresh_MS2_MS3,
      MS3_MS4 = input$harm_thresh_MS3_MS4, MS4_MS5 = input$harm_thresh_MS4_MS5,
      MS5_MS6 = input$harm_thresh_MS5_MS6, MS6_MS7 = input$harm_thresh_MS6_MS7
    )
    unchanged <- vapply(HARM_THRESHOLD_KEYS, function(k) {
      isTRUE(all.equal(cfg$size_thresholds[[k]], incoming[[k]]))
    }, logical(1))
    if (all(unchanged)) return()
    cfg$size_thresholds[HARM_THRESHOLD_KEYS] <- incoming[HARM_THRESHOLD_KEYS]
    set_session_config(cfg)
  })

  # FS PATTERNS. Debounced so a half-typed regex is not applied per keystroke.
  lapply(names(HARM_FS_PATTERN_LABELS), function(key) {
    input_id <- paste0("harm_pattern_", key)
    pattern_in <- debounce(reactive(input[[input_id]]), 500)
    observeEvent(pattern_in(), {
      value <- pattern_in()
      # The browser reports the UI's empty value before the start-up push
      # lands, and an empty pattern would match every text: ignore it.
      if (!is.character(value) || length(value) != 1L || !nzchar(trimws(value))) return()
      cfg <- isolate(rv$config)
      if (identical(cfg$foraging_patterns[[key]], value)) return()
      if (!harm_pattern_compiles(value)) {
        showNotification(sprintf("Invalid pattern for %s - keeping the previous one.", key),
                         type = "error", duration = 5)
        return()
      }
      cfg$foraging_patterns[[key]] <- value
      set_session_config(cfg)
    })
  })

  # TAXONOMIC RULES: one observer per rule the harmonize_* code reads.
  lapply(names(CONSUMED_TAXONOMIC_RULES), function(rule) {
    input_id <- paste0("harm_rule_", rule)
    observeEvent(input[[input_id]], {
      value <- isTRUE(input[[input_id]])
      cfg <- isolate(rv$config)
      if (identical(isTRUE(cfg$taxonomic_rules[[rule]]), value)) return()
      cfg$taxonomic_rules[[rule]] <- value
      set_session_config(cfg)
    })
  })

  # ECOSYSTEM PROFILE
  observeEvent(input$harm_active_profile, {
    value <- input$harm_active_profile
    cfg <- isolate(rv$config)
    if (identical(cfg$active_profile, value) || !value %in% names(cfg$profiles)) return()
    cfg$active_profile <- value
    set_session_config(cfg)
  })

  # SAVE AS SERVER DEFAULT (admin only). Pre-PR9α this also did
  #   assign("HARMONIZATION_CONFIG", rv$config, envir = globalenv())
  # which contaminated every concurrent Shiny session. Pre-B1 anyone could
  # overwrite the server default with an unvalidated config (F2).
  observeEvent(input$harm_save_config, {
    if (refuse_unless_admin("save server default")) return()
    checked <- validate_harmonization_config(isolate(rv$config))
    if (!checked$ok) {
      show_status("alert-danger", "exclamation-circle",
                  paste("Not saved:", paste(checked$errors, collapse = "; ")))
      return()
    }
    tryCatch({
      save_harmonization_config(checked$config, HARMONIZATION_CONFIG_FILE)
      rv$saved_hash <- harm_config_hash(checked$config)
      rv$unsaved_changes <- FALSE
      show_status("alert-success", "check-circle", "Saved as the server default for new sessions.")
      showNotification("Server default saved", type = "message", duration = 3)
    }, error = function(e) {
      warning(sprintf("[harmonization] save failed: %s", conditionMessage(e)), call. = FALSE)
      show_status("alert-danger", "exclamation-circle", paste("Save failed:", conditionMessage(e)))
    })
  })

  # RESET SERVER DEFAULT (admin only): delete the file, so new sessions start
  # from the built-in HARMONIZATION_CONFIG. This session keeps its settings.
  observeEvent(input$harm_reset_server_default, {
    if (refuse_unless_admin("reset server default")) return()
    tryCatch({
      if (file.exists(HARMONIZATION_CONFIG_FILE) && !file.remove(HARMONIZATION_CONFIG_FILE)) {
        stop(sprintf("could not remove '%s'", HARMONIZATION_CONFIG_FILE), call. = FALSE)
      }
      rv$saved_hash <- harm_config_hash(HARMONIZATION_CONFIG)
      rv$unsaved_changes <- !identical(harm_config_hash(isolate(rv$config)), rv$saved_hash)
      show_status("alert-success", "check-circle",
                  "Server default reset: new sessions start from the built-in defaults.")
    }, error = function(e) {
      warning(sprintf("[harmonization] reset server default failed: %s", conditionMessage(e)),
              call. = FALSE)
      show_status("alert-danger", "exclamation-circle", paste("Reset failed:", conditionMessage(e)))
    })
  })

  # RESET TO DEFAULTS (session only, ungated). Rolls THIS session back to the
  # built-in HARMONIZATION_CONFIG - not to the server-default file - and
  # pushes every widget. The widgets' echo then equals rv$config and returns
  # early, so the reset settles in one round. The global default is never
  # written (pre-PR9α this re-sourced harmonization_config.R into globalenv).
  observeEvent(input$harm_reset_defaults, {
    set_session_config(HARMONIZATION_CONFIG)
    push_config_to_widgets(HARMONIZATION_CONFIG)
    showNotification("This session now uses the built-in defaults", type = "warning", duration = 3)
  })

  # SIZE DISTRIBUTION PREVIEW
  output$harm_size_distribution_plot <- renderPlot({
    req(input$harm_preview_size)

    isolate({
      cache_dir <- "cache/taxonomy"
      if (!dir.exists(cache_dir)) {
        plot.new()
        text(0.5, 0.5, "No cached data available", cex = 1.5)
        return()
      }

      cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
      sample_files <- sample(cache_files, min(100, length(cache_files)))
      sizes <- numeric()

      for (file in sample_files) {
        data <- tryCatch(readRDS(file), error = function(e) NULL)
        if (!is.null(data) && !is.null(data$max_length_cm)) {
          sizes <- c(sizes, data$max_length_cm)
        }
      }

      hist(log10(sizes + 0.01), breaks = 30, col = "lightblue", border = "white",
           main = paste("Size Distribution (", length(sizes), " species)"),
           xlab = "log10(Size in cm)", ylab = "Frequency")

      abline(v = log10(rv$config$size_thresholds$MS1_MS2), col = "red", lwd = 2, lty = 2)
      abline(v = log10(rv$config$size_thresholds$MS2_MS3), col = "orange", lwd = 2, lty = 2)
      abline(v = log10(rv$config$size_thresholds$MS3_MS4), col = "yellow", lwd = 2, lty = 2)
      abline(v = log10(rv$config$size_thresholds$MS4_MS5), col = "green", lwd = 2, lty = 2)
      abline(v = log10(rv$config$size_thresholds$MS5_MS6), col = "blue", lwd = 2, lty = 2)
      abline(v = log10(rv$config$size_thresholds$MS6_MS7), col = "purple", lwd = 2, lty = 2)
    })
  })

  # ECOSYSTEM PROFILE DETAILS
  output$harm_profile_details <- renderUI({
    req(input$harm_active_profile)
    profile <- rv$config$profiles[[input$harm_active_profile]]
    tagList(
      h5("Description:"),
      p(profile$description),
      h5("Size Multiplier:"),
      p(paste0(profile$size_multiplier, "x"))
    )
  })

  # PROFILE EFFECTS: the length at which each MS boundary bites under the
  # active profile. apply_size_adjustment() multiplies a measured length by
  # the profile multiplier before it is compared with the thresholds, so a
  # boundary at T cm is reached by a measured length of T / multiplier.
  output$harm_profile_effects <- renderText({
    cfg <- rv$config
    mult <- apply_size_adjustment(1)
    thr <- unlist(cfg$size_thresholds[HARM_THRESHOLD_KEYS])
    paste(c(
      sprintf("Profile '%s': measured sizes are multiplied by %s", cfg$active_profile %||% "temperate",
              format(mult)),
      "Measured length at each boundary:",
      sprintf("  %s: %s cm", names(thr), format(signif(thr / mult, 3)))
    ), collapse = "\n")
  })

  # EXPORT: anyone may download their own session config.
  output$harm_export_json <- downloadHandler(
    filename = function() {
      paste0("harmonization_config_", Sys.Date(), ".json")
    },
    content = function(file) {
      export_config_json(file, isolate(rv$config))
    }
  )

  # IMPORT: validated, then applied to this session only.
  observeEvent(input$harm_import_json, {
    req(input$harm_import_json)
    tryCatch({
      imported <- import_config_json(input$harm_import_json$datapath)
      set_session_config(imported)
      push_config_to_widgets(imported)
      show_status("alert-info", "file-import",
                  "Imported into this session only. An admin can save it as the server default.")
      showNotification("Configuration imported for this session", type = "message", duration = 3)
    }, error = function(e) {
      warning(sprintf("[harmonization] import failed: %s", conditionMessage(e)), call. = FALSE)
      showNotification(paste("Import failed:", conditionMessage(e)), type = "error", duration = 8)
    })
  })
}
```

Design notes for the implementer (do not "fix" these):
- Do NOT add `ignoreInit = TRUE` to the widget observers. In `testServer` the first `setInputs()` counts as the init run and would be silently dropped (verified), which breaks the tests; the equal-value early returns already make the start-up echo harmless.
- `apply_size_adjustment(1)` in `harm_profile_effects` reads the same config through `get_harm_config()`; `rv$config` is touched first so the render re-runs when it changes, and `set_session_config()` always writes both together.
- `refuse_unless_admin()` must run before validation, so a locked user learns nothing about the config's validity.

- [ ] **Step 5: Parse-check and run**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='R/modules/harmonization_settings_server.R'); parse(file='tests/testthat/test-harmonization-settings-server.R'); parse(file='tests/testthat/test-ui-inputs-have-handlers.R'); cat('OK\n')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-harmonization-settings-server.R')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-ui-inputs-have-handlers.R')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-trait-cache-config-hash.R')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-config-paths.R')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-session-isolation.R')"
```

Expected: `OK`; server `[ FAIL 0 | ... | PASS 88 ]`; ui-inputs `[ FAIL 0 | ... | PASS 9 ]`; config-hash `PASS 19`; test-config-paths `FAIL 0` (its "uses the constant, not a literal" guard); test-session-isolation `FAIL 0`.

- [ ] **Step 6: Lint the new files**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "for (f in c('R/modules/harmonization_settings_server.R','R/ui/harmonization_settings_ui.R','R/config/harmonization_config.R','R/functions/admin_auth.R')) { l <- as.data.frame(lintr::lint(f)); l <- l[!l\$linter %in% c('object_usage_linter','return_linter','commented_code_linter'), ]; cat(f, nrow(l), '\n') }"
```

Expected: `0` for each file. (`object_usage_linter` fires on every Shiny module in this repo because the functions are sourced into globalenv; `return_linter` and `commented_code_linter` hits are pre-existing style.)

- [ ] **Step 7: Document the convention**

In `CONTRIBUTING.md`, replace the line `## Commit Messages` with:

```markdown
- **Every `harm_*` widget needs a server consumer.** The Harmonization tab
  once shipped rule checkboxes and FS inputs nothing read.
  `tests/testthat/test-ui-inputs-have-handlers.R` renders the UI and fails
  for any `harm_*` id the module does not read as `input$<id>`/`output$<id>`
  or wire from `CONSUMED_TAXONOMIC_RULES` / `HARM_FS_PATTERN_LABELS`. Drive
  widgets from the session config with `push_config_to_widgets(cfg)`, write
  it only through `set_session_config(cfg)`, and return early when an
  incoming value already equals `isolate(rv$config)` (echo safety, B0).

## Commit Messages
```

- [ ] **Step 8: Commit**

```bash
git add R/modules/harmonization_settings_server.R tests/testthat/test-harmonization-settings-server.R tests/testthat/test-ui-inputs-have-handlers.R CONTRIBUTING.md
git commit -m "$(cat <<'EOF'
fix(harmonization): session-only settings, strict-gated server default (F2, F8, F75)

Sliders, FS patterns, rule checkboxes, the profile and JSON import now
change only this session's config; every widget is pushed from it and
echo-safe. "Save as server default" and "Reset server default" require
admin_authorized_strict() and refuse (with a warning) when no admin
hash is configured. Save validates first; export works for everyone.
Folds in the B0 minors: RESET echo test, exactly-one-warning check, a
real two-live-sessions test, and no unsaved flag on page load.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 6: Release 1.5.2 (version strings, CHANGELOG, full suite)

**Files:**
- Modify: `VERSION`, `app.R` (header comment), `README.md` (3 version lines) - via `scripts/version_bump.R`
- Modify: `R/config.R` (`load_version_info()` fallback)
- Modify: `CHANGELOG.md` - via `scripts/generate_changelog.R`, then re-insert `docs/releases/1.5.0-results-changed.md`

**Interfaces:**
- Consumes: Tasks 1-5 committed.
- Produces: `VERSION=1.5.2` everywhere; `## [1.5.2]` CHANGELOG section.

- [ ] **Step 1: Confirm the starting version**

```bash
grep '^VERSION=' VERSION
grep -n 'VERSION = "\|VERSION_NAME = "\|RELEASE_DATE = "\|PATCH = ' R/config.R
```

Expected: `VERSION=1.5.1` and the matching four fallback lines in `R/config.R` (B2 set them). If `VERSION` is not 1.5.1, STOP and ask the user which version B1 gets.

- [ ] **Step 2: Bump VERSION, app.R and README**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/version_bump.R --version 1.5.2 --name "Harmonization Settings Safety"
sed -i 's/\r$//' VERSION
git diff --stat
```

Expected: `VERSION` (VERSION, VERSION_NAME, RELEASE_DATE, PATCH=2 lines), `app.R` (one `# CURRENT VERSION: v1.5.2 (<today>)` line) and `README.md` (bibtex `version = {1.5.2}`, `**Current Version**: 1.5.2`, `<!-- VERSION:1.5.2 -->`, and `**Last Updated**` if the date changed). `version_bump.R` writes VERSION with CRLF; the `sed` restores LF (the `mixed-line-ending --fix=lf` hook wants LF). If `version_bump.R` also rewrote `GIT_COMMIT`/`BUILD_DATE`, keep what it wrote.

- [ ] **Step 3: Bump the `R/config.R` fallback**

`version_bump.R` does not touch `R/config.R`. In `load_version_info()`, edit the four lines found in Step 1 so they read (use today's date):

```r
    VERSION = "1.5.2",
    VERSION_NAME = "Harmonization Settings Safety",
    RELEASE_DATE = "<today, YYYY-MM-DD, same as VERSION's RELEASE_DATE>",
```

and

```r
    PATCH = 2
```

(`MAJOR = 1` and `MINOR = 5` stay.) Then:

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='R/config.R'); parse(file='app.R'); cat('OK\n')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "source('R/config.R'); cat(ECONETOOL_VERSION\$VERSION, '\n')"
```

Expected: `OK`, then `1.5.2` (a startup banner line may precede it).

- [ ] **Step 4: Regenerate the CHANGELOG head and restore the 1.5.0 "Results changed" block**

`scripts/generate_changelog.R` rebuilds every section from the `v*` tags and labels all commits since the latest tag as the given version. It drops the hand-written `### Results changed` block of `[1.5.0]` (the text lives in `docs/releases/1.5.0-results-changed.md`), so re-insert it. If `v1.5.1` is not tagged (Task 0 Step 1), B2's commits will also land under `[1.5.2]` and any hand-written B2 section would be dropped: in that case STOP and ask the user whether to tag the B2 merge commit `v1.5.1` first (tagging and pushing a tag is outward-facing).

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/generate_changelog.R --version 1.5.2
sed -i 's/\r$//' CHANGELOG.md
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "f <- 'CHANGELOG.md'; cl <- readLines(f); rc <- readLines('docs/releases/1.5.0-results-changed.md'); i <- which(startsWith(cl, '## [1.5.0] ')); stopifnot(length(i) == 1, cl[i + 1] == '', !any(cl == '### Results changed')); cl <- append(cl, c(rc, ''), after = i + 1); con <- file(f, 'wb'); writeLines(cl, con); close(con); cat('INSERTED', length(rc), 'lines\n')"
git diff CHANGELOG.md | grep '^-' | grep -v '^---'
grep -n -m1 '^## \[' CHANGELOG.md
grep -n '^### Results changed' CHANGELOG.md
```

Expected: `INSERTED 72 lines`; the only removed lines are link lines at the bottom (e.g. `-[1.5.0]: .../compare/v1.4.5...HEAD`, now `...v1.4.5...v1.5.0`, and the previous head's link) - no removed prose; the first heading is `## [1.5.2] - <today>` with the five B1 commits under `### Added` / `### Fixed` (the `feat(admin)` commit under Added); `### Results changed` appears exactly once, under `## [1.5.0]`. A `### Maintenance` line for the 1.5.0 release commit is added to `[1.5.0]` - expected. If any other prose line shows as removed, restore it from `git show HEAD:CHANGELOG.md` the same way.

- [ ] **Step 5: Full suite**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "r <- as.data.frame(testthat::test_dir('tests/testthat', reporter = 'summary', stop_on_failure = FALSE)); cat('pass', sum(r\$passed), 'fail', sum(r\$failed), 'skip', sum(r\$skipped), '\n')"
```

Expected: `fail 0`; skip count equal to the Task 0 baseline; pass count about 159 higher than the baseline (admin +16, config-io +52, cache-hash +19, ui-inputs +9, settings-server 88 instead of 23).

- [ ] **Step 6: Commit**

```bash
git add VERSION app.R README.md R/config.R CHANGELOG.md
git commit -m "$(cat <<'EOF'
chore(release): 1.5.2 - harmonization settings safety

VERSION, R/config.R fallback, app.R header, README and CHANGELOG at 1.5.2.
The 1.5.0 "Results changed" block is re-inserted after regeneration.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 7: Push and open the PR (STOP - ask the user before running)

**Files:** none.

**Interfaces:**
- Consumes: Tasks 1-6.
- Produces: a PR against `master`.

- [ ] **Step 1: Push and create the PR (STOP - ask the user before running)**

```bash
git push -u origin fix/b1-harmonization-safety
gh pr create --base master --title "fix(harmonization): B1 harmonization settings safety (F2, F8, F75, F72), v1.5.2" --body "$(cat <<'EOF'
Implements spec B section 4.1 (Phase B1).

- **F2** Server-default save/reset require the new fail-closed `admin_authorized_strict()`; with `ECONETOOL_ADMIN_PASSWORD_HASH` unset they are refused ("Admin gate not configured on this instance; server defaults are read-only") and warn `[admin auth] ...`. Save validates first and writes tmp-then-rename.
- **F8** `export_config_json()` / `import_config_json()` implemented; import is session-only; anyone can download their config.
- **F75** Every `harm_*` widget reaches the config (FS inputs keyed to the real config names, seeded from config, never the stale `plant|algae` literal; checkboxes = the 11 rules `is_rule_enabled()` reads; profile effects rendered); dead controls removed; guard test added.
- **F72** Trait cache envelopes carry `config_hash`; readers pass the session hash.
- B0 minors: RESET echo test, exactly-one warning, real two-live-sessions test, no unsaved flag on page load.

**Deploy prerequisites (user):** set `ECONETOOL_ADMIN_PASSWORD_HASH` in production `.Renviron`, and make `config/` writable by the app user - until then saving the server default is refused by design. Existing cache envelopes have no hash, so each species re-queries once after deploy.

Interfaces B3 relies on: `admin_authorized_strict(unlocked) -> logical(1)` and the two refusal texts (see the plan's "Interfaces produced").

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

Expected: PR URL printed; CI (including B2's testthat job) green. `version-drift` may warn that 1.5.2 differs from the latest tag - informational; tagging `v1.5.2` after merge is the user's decision.

---

### Task 8: Deploy 1.5.2 to laguna.ku.lt

Every step touches production or the shared server. **Each is marked STOP: show the user the exact command and wait for explicit confirmation.** USER steps are run by the user (sudo or secrets). Deploy from the merged `master`.

**Files:** none in the repo. Remote: `/home/razinka/EcoNeTool_staging/`, `/srv/shiny-server/EcoNeTool/`.

**Interfaces:**
- Consumes: merged master at 1.5.2.
- Produces: production on 1.5.2 with a working strict gate.

- [ ] **Step 1: Pre-deploy check (local)**

```bash
git checkout master && git pull --ff-only
git log -1 --oneline
cd deployment && "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" pre-deploy-check.R; cd ..
```

Expected: the B1 merge / release commit; the check reports no errors (it must run from inside `deployment/`).

- [ ] **Step 2: Record production state and clear staging (STOP - ask the user before running)**

Read-only checks, then the staging wipe (B2's deploy may already do it; doing it again is harmless).

```bash
ssh razinka@laguna.ku.lt "grep '^VERSION=' /srv/shiny-server/EcoNeTool/VERSION; stat -c '%U:%G %a %n' /srv/shiny-server/EcoNeTool/config; ls -la /srv/shiny-server/EcoNeTool/config/; grep -n 'run_as' /etc/shiny-server/shiny-server.conf | head -3; id"
ssh razinka@laguna.ku.lt "grep -c '^ECONETOOL_ADMIN_PASSWORD_HASH=' /srv/shiny-server/EcoNeTool/.Renviron; true"
ssh razinka@laguna.ku.lt "f=/srv/shiny-server/EcoNeTool/config/harmonization_custom.json; if [ -f \$f ]; then grep -o 'FS0_primary_producer.*' \$f; else echo NO_SERVER_DEFAULT_FILE; fi"
ssh razinka@laguna.ku.lt "rm -rf /home/razinka/EcoNeTool_staging && echo STAGING_CLEARED"
```

Expected and what to record for the user: the live version (1.5.1); the owner/mode of `config/` and the app's `run_as` user (normally `shiny`) - if `config/` is not writable by that user, Step 6 is needed; the `.Renviron` count (`0` today: gate not configured, so Step 7 is needed); whether a server-default file exists and whether its FS0 contains `plant` or `algae` (the controller saw production's FS0 = `photosyn|autotrop|producer|plant|algae|phytoplankton|diatom|dinoflagellate`; after this deploy it is rejected at load - Step 9); `STAGING_CLEARED`.

- [ ] **Step 3: Upload code to staging (STOP - ask the user before running)**

```bash
powershell ./deploy-windows.ps1 -SkipData -NoSudo
```

Expected: upload into `/home/razinka/EcoNeTool_staging/`. Ignore any printed `sudo` / `rm -rf /srv/shiny-server/EcoNeTool/*` suggestion (it would delete the live `data/`).

- [ ] **Step 4: Strip runtime files, copy over live, reload (STOP - ask the user before running)**

```bash
ssh razinka@laguna.ku.lt "cd /home/razinka/EcoNeTool_staging && rm -f config/harmonization_custom.json config/api_keys.R config/api_keys.json && test ! -e .Renviron && cp -rT /home/razinka/EcoNeTool_staging /srv/shiny-server/EcoNeTool && touch /srv/shiny-server/EcoNeTool/restart.txt && echo DEPLOYED"
```

Expected: `DEPLOYED`. `test ! -e .Renviron` aborts the copy if a local `.Renviron` was ever uploaded (it would overwrite production's). `cp -rT` never deletes siblings, so the live `data/`, `.Renviron` and any live `config/harmonization_custom.json` survive.

- [ ] **Step 5: Verify (read-only; still show the user before running)**

```bash
ssh razinka@laguna.ku.lt "grep -c 'admin_strict_refusal' /srv/shiny-server/EcoNeTool/R/modules/harmonization_settings_server.R; grep -c 'admin_authorized_strict <- function' /srv/shiny-server/EcoNeTool/R/functions/admin_auth.R; grep '^VERSION=' /srv/shiny-server/EcoNeTool/VERSION; stat -c %y /srv/shiny-server/EcoNeTool/restart.txt; ls /srv/shiny-server/EcoNeTool/data | head -3"
curl -sL -o /dev/null -w '%{http_code}\n' http://laguna.ku.lt/EcoNeTool/
```

Expected: `1`, `1`, `VERSION=1.5.2`, a fresh `restart.txt` mtime, a non-empty `data/` listing, `200` (`-L` follows the http -> https redirect). Then open the app, go to the Harmonization tab, move a slider (the page stays responsive) and click "Save as server default": expected refusal "Admin gate not configured on this instance; server defaults are read-only" (the gate is not set yet - correct by design).

- [ ] **Step 6: USER - make `config/` writable by the app user (only if Step 2 showed it is not)**

The save writes `config/harmonization_custom.json.tmp` and renames it; reset removes the file. Both need write permission on the `config/` directory for the `run_as` user (normally `shiny`). The user runs (with sudo if razinka cannot change the group):

```bash
sudo chgrp shiny /srv/shiny-server/EcoNeTool/config
sudo chmod 2775 /srv/shiny-server/EcoNeTool/config
stat -c '%U:%G %a' /srv/shiny-server/EcoNeTool/config
```

Expected: `<owner>:shiny 2775`. Later deploys keep this (`cp -rT` does not change an existing directory's mode).

- [ ] **Step 7: USER - set the admin password hash**

Production `.Renviron` exists but has no `ECONETOOL_ADMIN_PASSWORD_HASH`, so strict actions are refused until this is done. The user, not the agent, handles the password:

1. Locally, in R from the repo root: `source("R/functions/admin_auth.R"); set_admin_password("<a long passphrase>")`. It prints one line `ECONETOOL_ADMIN_PASSWORD_HASH=econetool1$12$...` and writes nothing.
2. On laguna, append that exact line to `/srv/shiny-server/EcoNeTool/.Renviron` with an editor (e.g. `nano`), keeping the `$` characters literal. Do not `echo` it through a double-quoted shell string.
3. Reload: `touch /srv/shiny-server/EcoNeTool/restart.txt`.
4. Check (prints a count, not the secret): `grep -c '^ECONETOOL_ADMIN_PASSWORD_HASH=econetool1' /srv/shiny-server/EcoNeTool/.Renviron` -> `1`.

- [ ] **Step 8: Production smoke test (user, or browser automation with the user's go-ahead)**

In a fresh browser session: Harmonization tab -> "Save as server default" -> expected "Unlock via Trait Research > Configure API Keys first". Then Trait Research -> "Configure API Keys" -> enter the password (unlock). Back on Harmonization -> "Save as server default" -> "Saved as the server default for new sessions." Confirm on the server (read-only; show the user first):

```bash
ssh razinka@laguna.ku.lt "ls -la /srv/shiny-server/EcoNeTool/config/; ls /srv/shiny-server/EcoNeTool/config/*.tmp 2>/dev/null | wc -l"
```

Expected: a fresh `harmonization_custom.json` and `0` leftover `.tmp` files. In a second browser (not unlocked), the save must still be refused. Also export JSON from the locked browser (download works for everyone).

- [ ] **Step 9: Stale server default decision (user)**

If Step 2 showed a server-default file whose FS0 contains `plant` or `algae`, tell the user: before B1 every session was seeded with that inverted FS0 pattern; from 1.5.2 on the validator rejects the file, so every session starts from the built-in defaults (whole file, not just FS0) and each session start logs `[harmonization] invalid config ... diet nouns`. To stop the warnings, an unlocked admin clicks "Reset server default" (removes the file), optionally followed by a fresh "Save as server default". Do not remove it unasked.

---

## Self-Review

1. **Spec coverage.** 4.1 behaviour (session-only widgets; relabelled "Save as server default"; "Reset server default"; ungated "Reset to Defaults") -> Tasks 4-5. Fail-closed helper, both refusal texts, `warning("[admin auth] ...")`, `admin_authorized()` unchanged -> Task 1 (+ Task 5 wiring). Validator (six keys, finite, > 0, strictly increasing; compiling patterns; logical rules; known profile; unknown keys dropped; `modifyList` fill) -> Task 2; controller ruling: FS0 diet-noun rejection (validator, load fallback with warning, import, save refusal) -> Task 2 (+ Task 5 save test). Loader runs the validator -> Task 2. F8 `export_config_json(... auto_unbox, pretty, digits = NA)`, `import_config_json(fromJSON(simplifyVector = FALSE) -> modifyList -> validator)`, server import sets session only and calls `push_config_to_widgets()`, `error =` warns then notifies -> Tasks 2, 5; Test 10 rewrite -> Task 2 Step 5. F2 gate -> validate -> save; tmp + rename; `[harmonization] save failed` warning; reset-server-default gate + `file.remove` -> Tasks 2, 5. F75 table: FS inputs keyed to config names incl. FS3/FS7, seeded with `updateTextInput`, UI `value = ""`, 500 ms debounce, regex validator, invalid keeps old + notification -> Tasks 4-5; `fish_obligate_swimmers` re-ID and `bivalves_sessile` wired, five unread boxes removed, nine consumed rules added from one vector with an `lapply` of `observeEvent`s -> Tasks 4-5; profile wired with `isolate`; `harm_profile_effects` renderer reusing `apply_size_adjustment`; `harm_cancel` removed -> Tasks 4-5; `push_config_to_widgets()` at start/import/reset with equal-value early returns -> Task 5. F72 hash helper (drops `last_modified`/`version`, xxhash64 of JSON text), `read_cache_field(config_hash =)`, writers at the pipeline and offline paths, reader at the orchestrator, `trait_research_server` via `read_cache_field` incl. the `raw_data` fallback -> Task 3. Spec 5 B1 test rows -> Tasks 1-5 (the FS-seeding row via recorded messages, deviation 7). Spec 6 (VERSION, CHANGELOG, CONTRIBUTING lines for strict gate and config hash; B1 prerequisite hash) -> Tasks 1, 3, 6, 8. Spec 8 items 2, 3, 4, 11 -> Tasks 5, 4-5, 3, 6. B0 deferred minors (stale RESET comment, RESET-echo test, exactly-one warning, real two-session test, unsaved on page load) -> Task 5.
2. **Placeholder scan.** No TBD/TODO. The only value left to the executor is today's date in Task 6 Step 3 (it must equal what `version_bump.R` wrote into `VERSION`).
3. **Type consistency.** `admin_authorized_strict(unlocked)`, `admin_strict_refusal(unlocked, action)`, `validate_harmonization_config(cfg)$ok/$errors/$config`, `harm_config_hash(cfg)`, `harm_default_config_hash()`, `read_cache_field(..., config_hash =)`, `HARM_THRESHOLD_KEYS`, `HARM_FS0_DIET_NOUNS`, `HARM_FS_PATTERN_LABELS`, `CONSUMED_TAXONOMIC_RULES`, `set_session_config(cfg)`, `push_config_to_widgets(cfg)` are spelled identically in every task and test; all code blocks were run on a scratch copy of master (595e517): each task's tests fail with the stated counts before its implementation and pass after, in task order. A differential `test_dir` run on that scratch copy (no `data/`, so some environment failures) gave identical fail/error/skip counts per file before and after all five tasks; only the five B1 test files changed, adding exactly 150 passes (measured before the FS0 diet-noun ruling; that ruling adds 9 config-io and 2 server passes, re-verified red-then-green on the scratch copy: config-io FAIL 13/PASS 4 -> PASS 52, server FAIL 34/PASS 52 -> PASS 88).
4. **Review Focus.** Each of the five lines has a test in its owning task (Task 5 x4, Task 3 x1).
