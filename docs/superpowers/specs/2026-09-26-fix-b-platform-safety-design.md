# Fix B: Platform Safety (Harmonization Settings, Deploy/CI, Admin Gating)

- **Status:** Design, approved decisions baked in; not yet implemented
- **Date:** 2026-09-26
- **Source:** `docs/econetool-deep-analysis-2026-09-26.md`, remediation batches 3, 4 and 11
- **Prior art:** `docs/econetool-deep-analysis-2026-07-17.md` finding #19 (same bug as F1/F76)
- **Baseline:** master @ `7a96789`, `VERSION=1.4.4`
- **Sequencing:** Phase B0 ships first as a standalone hotfix, before sub-project A. B1-B3 follow after A.

## 1. Goal and context

The report found 18 defects that share one theme: the platform around the science code
is not safe for a shared, single-process Shiny deployment.

1. **Harmonization settings.** One slider move can pin the only R process (DoS), anyone
   can overwrite server defaults, half the tab is dead UI, and the trait cache ignores
   per-session settings.
2. **Deploy and CI.** Scripts delete or overwrite server-only state and put secret-bearing
   backups in the served tree. Guard tests pass on comments, CI never runs testthat, and
   the pre-deploy gate parses two files.
3. **Security.** Anonymous users can rebuild the offline DB (racy), two panels render
   untrusted text as raw HTML, and the API key modal erases a stored password.

The goal is to close all confirmed findings with a failing test first, in four PRs,
without changing scientific output under default settings.

## 2. Scope

All 18 findings were re-checked against master on 2026-09-26. Line numbers are current.

| ID | Sev | Current location | Status | Defect (one line) |
|----|-----|------------------|--------|-------------------|
| F1 | high | `R/modules/harmonization_settings_server.R:18-29` | CONFIRMED | INITIALIZE `observe()` reloads the JSON and reads `rv$config` reactively |
| F76 | high | `harmonization_settings_server.R:34-46` | CONFIRMED | UPDATE observer reads and writes `rv$config`, so the two observers loop forever |
| F2 | medium | `harmonization_settings_server.R:53-68` (write at :56) | CONFIRMED | "Apply Changes" writes the process-wide JSON with no gate and no validation |
| F8 | low | `harmonization_settings_server.R:141,149` | CONFIRMED | `export_config_json` and `import_config_json` are not defined anywhere under `R/` |
| F75 | low | `R/ui/harmonization_settings_ui.R:53-65,80-92,105,122,154` | CONFIRMED | FS pattern inputs, 7 rule checkboxes, profile select and `harm_cancel` are never read |
| F72 | medium | `R/modules/trait_research_server.R:377-384`; `R/functions/trait_lookup/orchestrator.R:243-246,422-425,1671-1675` | CONFIRMED | Harmonized codes are cached per species, shared by all sessions, and ignore the session config |
| F81 | high | `deployment/deploy.sh:198-204,234-240` | CONFIRMED | Preserves only r-libs/cache/restart.txt, deletes data/ and config/, then restores data/ with `*.csv` excluded |
| F4 | medium | `deploy-windows.ps1:489-490`, `:68-78` | CONFIRMED | Non-NoSudo `find` deletes dotfiles (.Renviron) and models/, and models/ is never shipped |
| F5 | medium | `deploy.sh:69,73-120`; `deploy-windows.ps1:77,81-113`; `deployment/deploy.sh:219-220` | CONFIRMED | config/ is shipped whole. Local `config/api_keys.R` and `config/harmonization_custom.json` exist and reach laguna via `cp -rT` |
| F7 | medium | `deploy.sh:39`; `deployment/deploy.sh:175`; `deploy-windows.ps1:50,340`; `deployment/shiny-server.conf:21,28` | CONFIRMED (exposure unverified) | Backups live in `/srv/shiny-server/backups` under `site_dir` with `directory_index on`. The ps1 backups are `cp -r` live app copies |
| F83 | medium | `deployment/pre-deploy-check.R:240-256` | CONFIRMED | Syntax check parses only `app.R` and `run_app.R` |
| F82 | medium | `.github/workflows/ci.yml:149-155`; `r-check.yml:97-103` | CONFIRMED | Parse list is non-recursive and skips `R/functions/trait_lookup`. No testthat job runs on push/PR |
| F84 | medium | `tests/testthat/test-layer2c-api-databases.R:14-21,76-89,127-139,162-174,190-210` | CONFIRMED | A local `skip_if_offline` shadows the helper, "structure" tests do real HTTP, and nothing checks `RUN_LIVE_TESTS` |
| F86 | low | `tests/testthat/test-deploy-preserve.R:37,48`; `test-trait-lookup-unit.R:56,66,98,122,142` | CONFIRMED | Greps include comments. `deploy.sh` and `deploy-windows.ps1` are unguarded. `expect_*` calls are if-gated |
| F19 | high | `trait_research_server.R:1170-1212`; `scripts/initialization/build_offline_trait_db.R:139-141,153,279` | CONFIRMED | No admin gate, a per-session in-progress guard, and `file.remove(db_path)` before a build that can `stop()` |
| F3 | medium | `R/modules/ecobase_server.R:71,111-117,157,160,168,172,177-195` | CONFIRMED | EcoBase metadata (description, author, doi in href, institution, model_name) and `e$message` are pasted into `HTML()` |
| F54 | medium | `R/modules/ecopath_import_server.R:966-968,981-987,1033-1037,1045,1055-1056` | CONFIRMED | .ewemdb metadata, `publication_uri` href, filename and parser error are pasted into `HTML()` |
| F6 | low | `R/modules/plugin_server.R:169,171,264-270,282-286` | CONFIRMED | Empty password field overwrites the stored AlgaeBase password. Freshwater key is shown in a `textInput` |

## 3. Non-goals

- Re-harmonizing offline-DB rows per session (F72 covers only the per-species RDS cache).
- Changing `admin_authorized()` semantics, or refactoring the plugin_server unlock modal.
- Making root `deploy.sh` work without rsync. It gets excludes and guards only.
- nginx config (not in the repo; one manual check in rollout). Findings outside batches 3/4/11.

## 4. Design

### 4.0 Phase B0: hotfix for F1/F76 (standalone PR)

**Behaviour change:** the harmonization tab reads `config/harmonization_custom.json`
once per session at module start. Moving a slider updates the session config once and
settles.

`R/modules/harmonization_settings_server.R`:
- Hoist the load out of any reactive, at module top before `rv` is created:
  `initial_cfg <- if (file.exists(HARMONIZATION_CONFIG_FILE)) load_harmonization_config(HARMONIZATION_CONFIG_FILE) else HARMONIZATION_CONFIG`.
  Seed `rv$config` and `session$userData$harm_config` from `initial_cfg`.
- Replace the INITIALIZE `observe()` (L18-29) with
  `observeEvent(TRUE, { ...six updateSliderInput from initial_cfg... }, once = TRUE)`.
  It has no reactive read of `rv$config`.
- UPDATE observer (L34-46): `cfg <- isolate(rv$config)`, assign the six thresholds to
  `cfg$size_thresholds`, then do one `rv$config <- cfg` and one
  `session$userData$harm_config <- cfg`.
- No other changes: no gating and no UI changes. The PR diff is this file, one test
  file, `VERSION` and `CHANGELOG.md`.

**Error handling:** unchanged. `load_harmonization_config` already warns and returns
defaults when the JSON is unparseable.

### 4.1 Phase B1: harmonization settings safety (F2, F8, F75, F72)

**Behaviour change (the "session-only for users" decision):**
- Anyone can move sliders, edit patterns, toggle rules, pick a profile, import JSON, and
  download their config. All of this is session-only (`session$userData$harm_config`,
  read through `get_harm_config()`).
- "Save as server default" (relabelled from "Apply Changes") and "Reset server default"
  write `HARMONIZATION_CONFIG_FILE` and require `admin_authorized_strict()`. "Reset to
  Defaults" stays session-only and ungated.

**Fail-closed rule (new helper, `R/functions/admin_auth.R`):**
```r
admin_authorized_strict <- function(unlocked) {
  admin_gate_enabled() && isTRUE(unlocked)
}
```
**Hash unset means fail CLOSED:** server-default saves and DB rebuilds are refused, with
"Admin gate not configured on this instance; server defaults are read-only" plus
`warning("[admin auth] ...")`. For local dev, use `set_admin_password()` with
`.Renviron`, or run the build script from a console. `admin_authorized()` is unchanged.
If the gate is set but the session is locked, the user sees "Unlock via Plugins > API Key
Configuration first", which sets the shared `session$userData$admin_unlocked`.

**Shared validator (F2, F8).** New function `validate_harmonization_config(cfg)` in
`R/config/harmonization_config.R`. It returns `list(ok, errors)`:
- `size_thresholds`: all six keys are present, numeric, finite, > 0 and strictly
  increasing in MS order.
- `foraging_patterns`: every value is a length-1 string that compiles
  (`tryCatch(grepl(p, ""), error = ...)`).
- `taxonomic_rules`: every value is logical length 1.
- `active_profile %in% names(profiles)`.
- Unknown top-level keys are dropped. Missing keys are filled from `HARMONIZATION_CONFIG`
  through `utils::modifyList(HARMONIZATION_CONFIG, cfg)` before validation.

`load_harmonization_config()` runs it too: an invalid file gives `warning()` plus defaults.

**F8. Wire import/export, because the backend (jsonlite) exists.** In
`R/config/harmonization_config.R`:
- `export_config_json(file, config)` calls `write_json(..., auto_unbox = TRUE, pretty = TRUE, digits = NA)`
  and returns `invisible(TRUE)`.
- `import_config_json(file)` runs `fromJSON(simplifyVector = FALSE)`, then `modifyList`,
  then the validator, and returns the config. It never assigns globals, and it stops
  with the joined validation errors.
- The server import sets `rv$config` and `session$userData$harm_config`, then calls
  `push_config_to_widgets()`. Its `error =` calls `warning()` and then `showNotification`.
- `tests/test_harmonization_phase1.R:150-175` (Test 10) already calls both functions,
  but it expects the import to mutate global `HARMONIZATION_CONFIG`, which is the
  pattern PR9α banned. Rewrite it to assert on the returned config.

**F2. Gated save.**
- `harm_save_config` checks `admin_authorized_strict(session$userData$admin_unlocked)`,
  then validates, then saves.
- `save_harmonization_config()` writes `<file>.tmp` and then `file.rename`s.
- The L63-67 error handler gains `warning("[harmonization] save failed: ...")`, as the
  CLAUDE.md convention requires.
- New `harm_reset_server_default` has the same gate and calls
  `file.remove(HARMONIZATION_CONFIG_FILE)`.

**F75. Dead UI, handled per input (wire if a consumer exists, otherwise remove):**

| UI input | Consumer today | Action |
|----------|----------------|--------|
| `harm_pattern_FS0..FS6` (ui:53-65) | `harmonization.R:558` reads `cfg$foraging_patterns` | **Wire.** Key the inputs to the real config names (`FS0_primary_producer`, ...), add the missing FS3/FS7, and set their values from `rv$config` at session start with `updateTextInput`. The UI literal defaults are stale: FS0 still contains `plant\|algae`, which were removed on 2026-07-17 because they inverted trophic structure. Set the UI `value = ""` so the literals can never be the source of truth. Apply edits through `observeEvent` with debounce (500 ms) and the regex validator. An invalid regex keeps the old value and shows a notification |
| `harm_rule_fish_swimmers` | `is_rule_enabled("fish_obligate_swimmers")` harmonization.R:652 | **Wire** (re-ID to `harm_rule_fish_obligate_swimmers`) |
| `harm_rule_bivalves_sessile` | `is_rule_enabled("bivalves_sessile")` :662 | **Wire** |
| `harm_rule_gastropods_crawlers`, `_phytoplankton_producers`, `_bivalves_filter`, `_molluscs_shells`, `_arthropods_exo` | none (`gastropods_crawlers`, `phytoplankton_primary_producers`, `bivalves_filter_feeders`, `molluscs_have_shells`, `arthropods_exoskeleton` are never passed to `is_rule_enabled`) | **Remove** |
| (new) checkboxes for consumed rules | `cephalopods_swimmers`:666, `cnidarians_sessile`:683, `phytoplankton_pelagic`:758, `zooplankton_pelagic`:770, `infaunal_bivalves`:780, `bivalves_hard_shell`:882, `gastropods_hard_shell`:886, `crustaceans_exoskeleton`:900, `echinoderms_calcium_plates`:911 | **Add.** Generate them from a single vector `CONSUMED_TAXONOMIC_RULES` in the UI file, with IDs `harm_rule_<name>`. One `lapply` of `observeEvent`s writes `cfg$taxonomic_rules[[name]]` using `isolate` |
| `harm_active_profile` (ui:105) | `harmonization.R:449` reads `cfg$active_profile` | **Wire.** Write it into the session cfg with `isolate` |
| `harm_profile_effects` (ui:122) | no renderer | **Wire** a `renderText` that lists the effective thresholds after the profile adjustment (reuse `apply_size_adjustment` logic) |
| `harm_cancel` (ui:154) | none | **Remove** |

All wired handlers use the B0 pattern: `cfg <- isolate(rv$config)`, then mutate, then
assign once. A single helper `push_config_to_widgets(cfg)` drives sliders, text inputs,
checkboxes and the profile select. It is called at session start, after import and
after reset. To prevent echo, each widget observer compares the incoming value with
`isolate(rv$config)` and returns early when they are equal.

**F72. Cache keyed on the effective config hash.** The hash goes in the envelope, not the
filename. A filename scheme would break `phylogenetic_imputation.R:142` and
`harmonization_settings_server.R:99`, which `list.files(cache_dir, "\\.rds$")`.
- New helper in `R/functions/trait_lookup/harmonization.R`:
  `harm_config_hash(cfg = get_harm_config())`. It drops `last_modified` and `version`,
  then computes `digest::digest(as.character(jsonlite::toJSON(cfg, auto_unbox = TRUE, digits = NA)), algo = "xxhash64", serialize = FALSE)`.
  Hashing the JSON text avoids integer/double drift after a JSON round-trip.
- `read_cache_field(cache_file, field, max_age_days = 30, config_hash = NULL)` in
  `validation_utils.R:486`. When `config_hash` is non-NULL and
  `!identical(cached$config_hash, config_hash)`, it returns NULL. An envelope with no
  hash counts as a miss.
- The writer at `orchestrator.R:1675+` adds `config_hash = harm_config_hash()`. The
  offline-DB writer at `:425` adds `harm_config_hash(HARMONIZATION_CONFIG)`, because
  those codes were harmonized at build time with the defaults. The reader at `:245`
  passes the session hash.
- `trait_research_server.R:377-384` drops its inline `readRDS` and age check in favour
  of `read_cache_field(..., "traits", config_hash = harm_config_hash())`. The legacy
  `raw_data` fallback stays, reading `cached$raw_data` with a separate
  `read_cache_field(..., "raw_data", config_hash = ...)`.
- Consequence: when two sessions use different configs, whichever wrote last owns the
  file, so each session may recompute. That is the correct trade (correctness over hit
  rate), and it matters only with non-default settings.

### 4.2 Phase B2: deploy and CI hardening (F86, F84, F81, F4, F5, F7, F83, F82)

The phase lands in this order within one PR. Tests come first, and the CI job comes last
so it does not flake on F84.

**F86. Guard tests.**
- A new helper `code_lines(path)` in `tests/testthat/helper-deploy.R` strips full-line
  and trailing `#` comments for sh and ps1. Heredocs are not used in these scripts.
- `test-deploy-preserve.R` is rewritten as a table-driven test over `deploy.sh`,
  `deployment/deploy.sh` and `deploy-windows.ps1`, asserting on code lines only (see §5).
- `test-trait-lookup-unit.R:56,66,98,122,142`: replace `if (cond) { expect_* }` with
  `skip_if(!isTRUE(cond), "<reason>")`.

**F84. Live-call gating.**
- `test-layer2c-api-databases.R`: delete the local `skip_if_offline` (L14-21) so the
  helper-fixtures version is used.
- Add `skip_if_no_live_tests()` to every test that reaches the network: the tests at
  L76, L86, L127, L136, L162, L171, L190 and the live tests at L58, L91, L141, L176.
- Wrap live calls in `with_timeout(..., timeout = 15)`.
- Pure argument-validation tests (L37, L46, L53, L212) stay ungated. The EMODnet tests
  (L105, L115) get `skip_if_no_live_tests()` when `Btrait` is installed, because
  `Btrait::getTrait` may fetch.

**F81. `deployment/deploy.sh`.**
- `PRESERVE_ITEMS=("r-libs" "cache" "restart.txt" "data" "config" "models")`. Dotfiles
  are already kept by `! -name '.*'` at L204.
- Replace the per-directory `rsync -av --exclude ...` (L234-240) with
  `cp -rT "$SRC" "$DEST/$ITEM"`. rsync is absent on laguna, so today every directory
  copy fails after the wipe.
- Drop the `*.csv` exclude.
- Remove `data` and `config` from `CRITICAL_ITEMS`. data/ is managed out of band (about
  3.1 GB). config/ holds runtime state.
- Fix the false "match root deploy.sh" comment.

**F4. `deploy-windows.ps1:489`.**
- For the live tree (non-NoSudo):
  `$preserve = "! -name '.*' ! -name data ! -name cache ! -name r-libs ! -name models ! -name config"`.
- For `-NoSudo`, `$APP_DEPLOY_PATH` is the staging dir (L45-46), so set `$preserve = ""`
  and wipe staging entirely. Staging holds no live state. Otherwise stale staged
  `data/` or `config/` files survive and are `cp -rT`'d over live.
- Add `"models/"` to `$DEPLOY_ITEMS` (tracked; loaded by `ml_trait_prediction.R`).

**F5. Runtime config files are never shipped.**
- Add `config/api_keys.json`, `config/api_keys.R` and `config/harmonization_custom.json`
  to `EXCLUDE_PATTERNS` in `deploy.sh` and `deploy-windows.ps1` (both match full relative
  paths).
- `deployment/deploy.sh` preserves config/ (F81) and copies only
  `config/api_keys.R.template` into it.
- The ps1 `Test-ShouldExclude` must match on relative path, so add a case for patterns
  containing `/`.
- In `deploy.sh`, the rsync `--delete` no longer removes the server's `api_keys.json`
  because excluded paths are protected on the receiver.

**F7. Backups out of the served tree.**
- `BACKUP_DIR=/srv/shiny-server-data/EcoNeTool/backups` in all three scripts (exists; holds
  `feedback.db`); `-NoSudo` keeps `/home/$User/backups`. Backups are `tar -czf` plus
  `chmod 600` everywhere; the ps1 `cp -r` backup (L340), a live app copy under
  site_dir, is removed.
- `deployment/shiny-server.conf:28` gets `directory_index off;` (reference copy; the
  server change is a manual step in §6).

**F83.** `pre-deploy-check.R:240`: parse `app.R`, `run_app.R` and
`list.files("R", "\\.R$", recursive = TRUE)` minus `safeBackup`. Failures use the
existing `print_check(..., "ERROR")`, which already leads to `quit(status = 1)`.

**F82.** `ci.yml:149-155` and `r-check.yml:97-103` parse root `*.R` plus `R/`
recursively. A new `ci.yml` job `testthat-offline` copies the nightly
`setup-r-dependencies` block, leaves `RUN_LIVE_TESTS` unset, runs
`testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)` with a
20 min timeout, and is added to `ci-status` needs.

### 4.3 Phase B3: security and admin gating (F19, F3, F54, F6)

**F19. Rebuild offline DB.**
- `trait_research_server.R:1170` observer:
  1. Require `admin_authorized_strict(session$userData$admin_unlocked)`. Refusal
     messages are the same as B1.
  2. Acquire a process-wide lock with a new `acquire_rebuild_lock()` in
     `R/functions/cache_sqlite.R`. The lock is the directory
     `app_path("cache/offline_traits.db.lock")`, created with `dir.create()`, which
     fails atomically if the directory already exists. An `owner` file inside it holds
     `Sys.getpid()` and a timestamp. The lock has to be a file-system object rather
     than an R flag because the build runs in a separate Rscript. If the lock exists
     and is older than 60 min, it is treated as stale: `warning()` and then reclaim it.
     Otherwise the user sees "Rebuild already running (started HH:MM)".
  3. Start processx.
- The exit observer (L1110-1166) calls `release_rebuild_lock()` on success, failure and
  handler error. A `session$onSessionEnded` hook releases the lock only if this session
  owns it and the process is dead.
- `build_offline_trait_db.R`:
  - Build into `tmp_path <- paste0(db_path, ".tmp.", Sys.getpid())` in place of the
    `file.remove(db_path)` at L139, so concurrent builds never share a file.
  - The script also calls `acquire_rebuild_lock()` itself (sourcing `cache_sqlite.R`),
    so console builds and Shiny builds exclude each other. It releases the lock with
    `on.exit`.
  - After the final coverage query: `dbDisconnect(con)` before the rename (Windows
    blocks renaming an open SQLite file), then `Sys.chmod(tmp_path, "0664")`, then
    `file.rename(tmp_path, db_path)`. If the rename fails, `file.copy(overwrite = TRUE)`
    and then remove the tmp.
  - The `on.exit(dbDisconnect)` becomes guarded with `if (DBI::dbIsValid(con))`.
  - On `stop()` for ontology drift (L279), the live DB is untouched, and sessions keep
    fast offline lookups during a build.

**F3 / F54. Escape at the render boundary.** One shared helper goes in
`R/functions/validation_utils.R`:
```r
safe_href <- function(url) {
  if (is.character(url) && length(url) == 1 && grepl("^https?://", url, ignore.case = TRUE)) url else NULL
}
```
- `fmt()` in both files returns `htmltools::htmlEscape(as.character(val))`. The
  "Not specified" span stays literal.
- Rebuild the preview panels with `tags$table`, `tags$tr` and `tags$td`, which escape by
  construction. Drop the `HTML(paste0(...))` blocks at `ecobase_server.R:177-195` and
  `ecopath_import_server.R:1055-1074`.
- DOI: validate with `^10\\.\\d{4,9}/\\S+$`. Build the href as
  `paste0("https://doi.org/", utils::URLencode(doi, reserved = FALSE))`. If validation
  fails, show escaped text with no link.
- `publication_uri` (`ecopath_import_server.R:1035`): use `safe_href()`. If it returns
  NULL, render escaped text only.
- Error text: `ecobase_server.R:71` gets `htmlEscape(e$message)`, and
  `ecopath_import_server.R:966-968` gets `tags$pre(preview_data$error)`.
- `model_name` (ecobase L178), `preview_data$filename` (L1056), description, author,
  contact and institution all become escaped text nodes.
- `ecopath_import_server.R:1570 HTML(status_data$solution_html)` stays, because it is
  static server text (see Appendix).

**F6. API key modal (`plugin_server.R`).**
- L169 and L171: both secrets become `passwordInput(..., value = "")` with placeholder
  `"(unchanged - leave blank to keep)"`. Stop pre-filling the freshwater key.
- L264-270: build `keys_list` from the stored values. An empty or whitespace field
  keeps `API_KEYS$<field>`. The username keeps `textInput` and is written as given.
- L282-286: mutate `API_KEYS` only for the fields that changed.
- Write the JSON with the same tmp-then-rename pattern and `Sys.chmod(API_KEYS_JSON, "0600")`.

## 5. Testing strategy

Every fix starts with a failing test. testServer is new to this repo; it accepts a plain
`function(input, output, session)`, so no `moduleServer` refactor is needed.

| File | Case | Asserts |
|------|------|---------|
| `tests/testthat/test-harmonization-settings-server.R` (new, B0) | "slider change settles" | Write a temp JSON with `MS3_MS4 = 7`. Swap `HARMONIZATION_CONFIG_FILE` in globalenv and restore it with `on.exit`. Shim `load_harmonization_config` with a counter that `stop()`s after 5 calls, which turns the pre-fix hang into a fast failure. In `testServer`, `setInputs` all six sliders with `MS3_MS4 = 9`. Assert `counter <= 1` and `session$userData$harm_config$size_thresholds$MS3_MS4 == 9` |
| same (B1) | "save refused when gate unset" | `withr::local_envvar(ECONETOOL_ADMIN_PASSWORD_HASH = "")`. Clicking save writes no file |
| same (B1) | "save refused when locked" | Gate set, `admin_unlocked` NULL, so no file is written |
| same (B1) | "save allowed when unlocked" | Gate set and unlocked, so the file is written and round-trips through `load_harmonization_config` |
| same (B1) | "import is session-only" | Import a valid JSON. The session cfg changes, the server file does not exist, and `get_harm_config()` outside the session returns the global default |
| same (B1) | "rule checkbox reaches consumer" | Untick `harm_rule_fish_obligate_swimmers`, then `isolate(is_rule_enabled("fish_obligate_swimmers"))` in the session is FALSE |
| same (B1) | "FS inputs seeded from config" | After start, `input$harm_pattern_FS0_primary_producer` equals `HARMONIZATION_CONFIG$foraging_patterns$FS0_primary_producer` and does not contain `algae` |
| `tests/testthat/test-harmonization-config-io.R` (new, B1) | validator | Rejects non-increasing thresholds, `Inf`, a negative value, the uncompilable regex `"("`, and an unknown profile. Accepts the defaults |
| same | export/import round-trip | `export_config_json(tmp, cfg)`, then `import_config_json(tmp)`, is identical on validated keys, and global `HARMONIZATION_CONFIG` is untouched |
| same | invalid server file | `load_harmonization_config` on bad thresholds gives a warning and returns the defaults (`expect_warning`) |
| `tests/testthat/test-ui-inputs-have-handlers.R` (new, B1) | dead UI guard | Every `harm_*` inputId in `harmonization_settings_ui.R` appears as `input$<id>` or in the generated-ID vector in the server |
| `tests/testthat/test-trait-cache-config-hash.R` (new, B1) | hash | `harm_config_hash` is stable across `last_modified` and changes when `MS3_MS4` changes |
| same | cache miss on mismatch | `read_cache_field(f, "traits", config_hash = "x")` returns NULL for an envelope with a different hash or no hash |
| `tests/testthat/test-deploy-preserve.R` (rewritten, B2) | per script | Code lines only. Each of the three scripts protects `.*`, `data`, `cache`, `r-libs`, `models`, `config`. None contains `rm -rf .../EcoNeTool/*`. `deploy.sh` and ps1 exclude the three runtime config paths. No backup path starts with `/srv/shiny-server/backups`. The ps1 backup does not use `cp -r` |
| same | comment-proofing | Feed `code_lines()` a fixture where `.Renviron` appears only in a comment. The dotfile assertion fails |
| `tests/testthat/test-pre-deploy-check.R` (new, B2) | parses R/ | Source the syntax block against a temp tree containing `R/modules/bad.R` with `{`. The error count is at least 1 |
| `tests/testthat/test-live-gating-guard.R` (new, B2) | source guard | Parse `test-layer2c-api-databases.R`. Every `test_that` body calling a network `lookup_*` contains `skip_if_no_live_tests()`, and the file defines no local `skip_if_offline` |
| `tests/testthat/test-admin-auth.R` (extend, B1) | strict gate | `admin_authorized_strict(TRUE)` is FALSE when the env var is empty, and TRUE when it is set and unlocked |
| `tests/testthat/test-rebuild-lock.R` (new, B3) | lock | A second acquire fails. A stale lock (mtime set 2 h back) is reclaimed with a warning. Release removes it |
| same | atomic build | Run the build script's final block against a temp dir with a pre-existing DB, simulating `stop()` before the rename. The original DB is byte-identical |
| `tests/testthat/test-xss-escaping.R` (new, B3) | EcoBase panel | Render with `meta$description = "<img src=x onerror=alert(1)>"`. The HTML contains `&lt;img` and not `<img src=x` |
| same | preview panel | `publication_uri = "javascript:alert(1)"` produces no `href="javascript`. The error string with `<script>` is escaped |
| same | DOI | `10.1000/x' onmouseover='a` yields no link |
| `tests/testthat/test-plugin-api-keys.R` (new, B3) | keep secret | Seed `API_KEYS$algaebase_password = "old"` and submit empty. The JSON and env still hold `"old"`. Submitting `"new"` stores `"new"` |

Every run: `testthat::test_dir("tests/testthat")` (legacy `run_all_tests.R` needs `dggridR`) and parse checks on each edited file. No
`if (...) expect_*`; use `skip_if` with a reason.

## 6. Rollout

| PR | Content | Version | Depends on |
|----|---------|---------|-----------|
| B0 | F1/F76 hotfix | 1.4.5 | none; ships before sub-project A |
| (A) | sub-project A (separate spec) | - | B0 |
| B2 | Deploy and CI (lands before B1/B3 so their deploys use safe scripts) | PATCH+1 at merge | A merged |
| B1 | Harmonization safety | PATCH+1 at merge | B0, A |
| B3 | Security and admin gating | PATCH+1 at merge | B1 (`admin_authorized_strict`) |

Every PR bumps `VERSION` (the version-drift job compares it with the latest tag), adds a
`CHANGELOG.md` entry, and adds a `CONTRIBUTING.md` line where a convention changes (B1:
strict gate, config hash; B2: code-line guard helper).

**Deploy steps.** Every deploy follows the documented workflow:
1. `cd deployment && Rscript pre-deploy-check.R`
2. `powershell ./deploy-windows.ps1 -SkipData -NoSudo`

**B0 specifics.** The pre-B2 ps1 still ships local `config/harmonization_custom.json`
and `config/api_keys.R`, so they must be stripped from staging before copying:
```
ssh razinka@laguna.ku.lt "ls -la /srv/shiny-server/EcoNeTool/config/; \
  rm -f /home/razinka/EcoNeTool_staging/config/harmonization_custom.json \
        /home/razinka/EcoNeTool_staging/config/api_keys.R && \
  cp -rT /home/razinka/EcoNeTool_staging /srv/shiny-server/EcoNeTool && \
  touch /srv/shiny-server/EcoNeTool/restart.txt"
```
The `ls` records whether production had the loop trigger file. Verify with
`curl -s -o /dev/null -w '%{http_code}' http://laguna.ku.lt/EcoNeTool/` (expect 200),
then move a slider in production and confirm the page stays responsive.

**B2 manual server steps (sudo, one time):**
1. `mkdir -p /srv/shiny-server-data/EcoNeTool/backups && chmod 700`.
2. Move `/srv/shiny-server/backups/*` there.
3. Set `directory_index off` in `/etc/shiny-server/shiny-server.conf` and reload.
4. Check `curl -I http://laguna.ku.lt:3838/backups/` and
   `curl -I http://laguna.ku.lt/backups/`. Both must not return 200.

**B1/B3 prerequisite:** confirm `ECONETOOL_ADMIN_PASSWORD_HASH` is set in
`/srv/shiny-server/EcoNeTool/.Renviron`. If it is not, run `set_admin_password()`
locally and add the line. Without it, save and rebuild are refused by design.

## 7. Risks and open questions

- **Echo loops in B1 wiring.** Widget, config and widget cycles are the same class as
  F1. Mitigation: the `isolate` pattern, early return on equal values, and the testServer
  bounded-observer test extended to text inputs and checkboxes.
- **Cache hit rate (F72).** Users with non-default settings will see more API calls, and
  sessions with different configs overwrite each other's files. This is acceptable and
  documented.
- **Stale lock / rename.** A crashed R process leaves the lock; the 60-minute reclaim
  handles it (builds take about 1 min). A Windows rename over an open DB can fail, so
  there is a copy fallback. On laguna (Linux) rename is atomic.
- **CI runtime.** The offline job adds about 10 min. If it is too slow, split by file;
  keep `stop_on_failure`.
- **Open question:** should B2 also delete the root `deploy.sh`, which is unusable
  against laguna without rsync, rather than maintain it? Default: keep it and guard it.
  Its removal would be a separate decision.

## 8. Acceptance criteria

1. B0: the new testServer test fails on `7a96789` (the counter stop fires) and passes
   after the fix. In production, a slider move leaves `/EcoNeTool/` responsive for other
   sessions.
2. With `ECONETOOL_ADMIN_PASSWORD_HASH` unset, neither server-default save nor rebuild
   writes any file. Both emit a `warning()`.
3. Every `harm_*` input in the UI has a server consumer, enforced by
   `test-ui-inputs-have-handlers.R`. No FS pattern default contains `plant` or `algae`.
4. Two testServer sessions with different `MS3_MS4` do not share cached harmonized codes.
5. All three deploy scripts pass the comment-stripped guard test, and removing
   `! -name '.*'` from any of them turns it red.
6. `pre-deploy-check.R` exits 1 on a syntax error in any `R/**/*.R`. CI parses
   `R/functions/trait_lookup/*` and runs offline testthat on push/PR.
7. Every network-reaching test in `test-layer2c` is gated by `skip_if_no_live_tests()` (source guard).
8. A rebuild that `stop()`s mid-way leaves the previous `offline_traits.db` intact. A
   second concurrent rebuild is refused.
9. The XSS payloads in `test-xss-escaping.R` render as text. No `javascript:` href is
   produced.
10. Saving the API modal with blank secret fields preserves the stored secrets.
11. The full suite (`testthat::test_dir("tests/testthat")`) has 0 failures, and the skip count does not
    grow except for newly gated live tests.

## Appendix A. Excluded or adjusted items

No finding in batches 3, 4 or 11 was NOT-REPRODUCED. These sub-claims were narrowed:

- **F54, `ecopath_import_server.R:1570` `HTML(status_data$solution_html)`:** not a
  defect. `solution_html` is built from static strings at L1873-1905; only
  `error_message` is dynamic, and it already renders through `tags$pre`. It stays as is.
- **F84:** there is no `setTimeLimit` (the timeouts are `httr::timeout()`), but the ungated
  HTTP stands. `test-ices-lookups.R:8` shadows `skip_if_no_live_tests` with an equivalent
  body, so its gate works; that is out of scope.
- **F5, key deletion via `rsync --delete`:** reachable only through root `deploy.sh`,
  which cannot run against laguna (no rsync). The shipping half of F5 is live on the
  real path, because `config/api_keys.R` and `config/harmonization_custom.json` exist
  locally and are untracked.
- **F4 / F7:** F4 is reachable only without `-NoSudo` (fixed anyway; the guard now covers
  ps1). F7's nginx exposure is not verifiable from the repo, so the B2 `curl -I` step
  checks it.
