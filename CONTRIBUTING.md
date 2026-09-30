# Contributing to EcoNeTool

Thank you for your interest in contributing to EcoNeTool! This guide covers the process for reporting bugs, proposing features, and submitting code changes.

## Reporting Bugs

1. Search [existing issues](https://github.com/razinkele/EcoNeTool/issues) first
2. Open a new issue with:
   - Steps to reproduce
   - Expected vs. actual behavior
   - R version and OS
   - Relevant error messages or screenshots

## Proposing Features

Open a GitHub issue with the "enhancement" label. Describe:
- The problem your feature solves
- How it fits into the existing workflow
- Any marine ecology context that helps understand the use case

## Development Setup

### Prerequisites

- R >= 4.0.0
- Required R packages (see `app.R` library calls)
- Git with conventional commit support

### Local Development

```bash
# Clone the repository
git clone https://github.com/razinkele/EcoNeTool.git
cd EcoNeTool

# Install dependencies
Rscript deployment/install_dependencies.R

# Run the app
Rscript run_app.R

# Run tests
Rscript tests/run_all_tests.R
```

## Code Style

- **R functions:** `snake_case`
- **Assignment:** `<-` (not `=`)
- **Line length:** 120 characters max
- **Indentation:** 2 spaces (no tabs)
- **No trailing whitespace**
- **Linting:** enforced by [lintr](https://github.com/r-lib/lintr) (see `.lintr`)

## Trait-pipeline & concurrency patterns

These are non-obvious conventions established by recent silent-failure
fixes. Follow them or risk re-introducing bugs we already paid to find.

- **Read `HARMONIZATION_CONFIG` via `get_harm_config()`**, not from
  `globalenv()` directly. The accessor (defined in
  `R/functions/trait_lookup/harmonization.R`) prefers the per-session
  override stored in `session$userData$harm_config`. Pre-PR9α the
  slider's writeback to `globalenv()` contaminated every concurrent
  Shiny session. The same fallback pattern works outside Shiny (build
  scripts, console).

- **Confidence values are numeric `[0, 1]`.** Use the helpers
  `confidence_to_num("high")` -> `1.0` and `confidence_to_label(0.7)`
  -> `"high"` (defined in `harmonization.R`). The mapping is
  `none = 0.0 / low = 0.33 / medium = 0.66 / high = 1.0`. Don't
  re-define a local mapping inline.

- **`warning()`, not `message()`, for swallowed errors.** Production
  `shiny-server.conf` has `preserve_logs` commented out, so
  `message()` output is invisible. Warnings reach the Shiny error
  reporter, console, tests, and the nightly live-test workflow.

- **`<<-` (not `<-`) inside `error = function(e)` closures.** When
  the handler needs to mutate an outer-scope variable like
  `result$error <<- conditionMessage(e)`. Plain `<-` only mutates the
  closure-local copy and the change is silently lost.

- **Tests: never gate `expect_*` behind `if (precondition)`.** Use
  `skip_if(!precondition, "actionable reason")` instead. The
  if-guarded pattern lets a failing fixture / dependency silently
  hide its impact (testthat reports "empty test" with a tiny footnote
  nobody reads).

- **Live-API tests gate behind `RUN_LIVE_TESTS=true`** and run on the
  nightly workflow (`.github/workflows/nightly-live-tests.yml`).
  Inside the gate use `with_timeout()` (in `R/functions/validation_utils.R`)
  so a slow upstream day doesn't hang the whole run. CI's
  `testthat-offline` job runs the suite on every PR with the variable
  unset, so an ungated HTTP call shows up there as a flaky failure.

- **`data/external_traits/*.csv` are intentional header-only stubs.**
  The build script's Sources 7-10 read them and gracefully insert 0
  rows when empty. Per `data/external_traits/README.md`, users
  populate them per the per-source download URL. Don't "fix" the
  empty CSVs.

- **Schema additions in `scripts/initialization/build_offline_trait_db.R`
  must be backward-compatible.** Add new columns as nullable
  (`TEXT` / `REAL DEFAULT 0.0`) so existing INSERT statements keep
  working unchanged. The lookup in
  `R/functions/trait_lookup/orchestrator.R` uses defensive
  `DBI::dbListFields()` to handle old + new schemas.

- **Never `source()` a working-directory-relative path at runtime.**
  Use `app_path()` (in `R/functions/validation_utils.R`):

  ```r
  source(app_path("R/functions/uncertainty_quantification.R"))
  ```

  A bare `source("R/functions/...")` only resolves when the working
  directory is the repo root. Inside a function body the wd at call
  time depends on the caller - under testthat it is `tests/testthat/`,
  so the `source()` fails and the feature degrades silently. This bug
  hid uncertainty quantification from every nightly run for weeks.
  `app_path()` checks `getOption("econetool.app_root")`, then walks up
  from `getwd()` for the `app.R` + `R/functions` marker, then falls
  back to `getwd()`.

  The `R/functions/*/load_all.R` files are the deliberate exception:
  they run only at startup, sourced by `app.R` from the repo root.
  A regression test in `tests/testthat/test-deep-analysis-fixes.R`
  enforces this for every other file under `R/`.

- **API key configuration can be password-gated.** The "API Key
  Configuration" modal (`R/modules/plugin_server.R`) is protected when
  `ECONETOOL_ADMIN_PASSWORD_HASH` is set, and behaves exactly as before
  when it is not, so local development needs no setup.

  To enable it on a deployment:

  ```r
  # locally - prints a pasteable line, writes nothing
  source("R/functions/admin_auth.R")
  set_admin_password("a long passphrase")
  ```

  Put the printed `ECONETOOL_ADMIN_PASSWORD_HASH=...` line in an
  `.Renviron` in the app directory on the server, then `touch
  restart.txt`. Only the derived key is stored - never the password.
  Hashing is `openssl::bcrypt_pbkdf` at 12 rounds with a random
  16-byte salt, compared in constant time.

  **Gate every observer that touches protected state, not just the
  one that draws the modal.** Shiny input IDs are client-controlled, so
  `Shiny.setInputValue('save_api_keys', 1)` from a browser console
  reaches the save handler without any dialog ever opening. Both
  `show_api_keys` (read) and `save_api_keys` (write) call
  `admin_authorized(session$userData$admin_unlocked)`; a new protected
  action must call it too.

  The unlock is held in `session$userData$admin_unlocked`, never a
  global: a global would leak one user's unlock to every concurrent
  session in the same R process, the same hazard `get_harm_config()`
  exists to avoid. Attempts are capped at 5 per session and every
  failure is logged with `warning()`.

  `config/api_keys.json` and `.Renviron` are both gitignored. Check
  before committing if you touch that area.

- **Never commit API keys or other secrets,** not even as defaults or
  template examples; keys live only in the gitignored
  `config/api_keys.json` (set via the API-key modal).
  `tests/testthat/test-no-hardcoded-keys.R` fails on any UUID-shaped
  literal in `R/`, `app.R` or `config/*.template`.

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

- **Deploy-script guards read code lines only.** Assertions about
  `deploy.sh`, `deployment/deploy.sh` or `deploy-windows.ps1` go through
  `code_lines()` / `script_array()` in `tests/testthat/helper-deploy.R`,
  which drop comments and join `\` continuations first. A raw `grepl()`
  over the file also matches comments: the old guard "passed" because a
  comment mentioned `.Renviron`. Every deploy path must keep dotfiles,
  `data/`, `cache/`, `r-libs/`, `models/` and `config/` on the server,
  never upload `config/api_keys.*`, `config/harmonization_custom.json` or
  `.Renviron`, and write backups as `tar.gz` (mode 600, without `data/`) to
  `/srv/shiny-server-data/EcoNeTool/backups` (or `/home/$User/backups`
  for `deploy-windows.ps1 -NoSudo`) - never under `/srv/shiny-server/`,
  which shiny-server serves. `deploy-windows.bat` is a retired stub; any
  `.bat`/`.cmd` deploy script must stay one. `deploy-windows.ps1` ships
  `scripts/initialization/build_offline_trait_db.R` and, file by file, the
  tracked `data/` inputs it reads; a guard test derives that list from the
  script, so a new build input must be added to `DEPLOY_ITEMS`. No deploy script
  writes anything under `/etc/shiny-server/`: laguna is shared with ~30
  other apps, so conf changes are printed as a snippet and applied by
  hand after review.
- **Server-wide writes use the strict (fail-closed) gate.**
  `admin_authorized()` fails OPEN when `ECONETOOL_ADMIN_PASSWORD_HASH` is
  unset, which is right for the API-key modal only. Anything that changes
  what every session sees (the harmonization server default, the offline DB
  rebuild) must call `admin_authorized_strict(session$userData$admin_unlocked)`,
  or `admin_strict_refusal(unlocked, "<action>")`, which also warns and
  returns the user-facing reason. With no hash configured these actions are
  refused ("Admin gate not configured on this instance; server defaults are
  read-only"). For local work, put a `set_admin_password()` line in
  `.Renviron` or run the build script from a console.
- **Trait-cache envelopes carry `config_hash`.** Harmonized codes depend on
  the session's harmonization settings, and every session shares
  `cache/taxonomy/<species>.rds`. A writer stamps `config_hash =
  harm_config_hash()` (or `harm_default_config_hash()` when the codes were
  harmonized with the defaults, e.g. offline-DB rows); a reader passes
  `config_hash = harm_config_hash()` to `read_cache_field()`, which treats a
  different or missing hash as a miss. Compute the hash in the calling
  process, never inside a `future` worker (a worker has no Shiny session).
- **Every `harm_*` widget needs a server consumer.** The Harmonization tab
  once shipped rule checkboxes and FS inputs nothing read.
  `tests/testthat/test-ui-inputs-have-handlers.R` renders the UI and fails
  for any `harm_*` id the module does not read as `input$<id>`/`output$<id>`
  or wire from `CONSUMED_TAXONOMIC_RULES` / `HARM_FS_PATTERN_LABELS`. Drive
  widgets from the session config with `push_config_to_widgets(cfg)`, write
  it only through `set_session_config(cfg)`, and return early when an
  incoming value already equals `isolate(rv$config)` (echo safety, B0).
- **The rule-checkbox start-up guard needs static checkboxes.** Each
  `harm_rule_*` observer ignores the FIRST report if it is `TRUE`, because
  that is the UI literal (`checkboxInput(..., TRUE)`) arriving before the
  start-up push. This is only correct while the checkboxes are bound at
  session init by the static `harmonization_settings_ui()`. Do not move them
  into `renderUI()`/`insertUI()` without replacing the guard: rendered later,
  the first report can follow the push and be a real click.
- **The trait vocabulary lives only in `TRAIT_VOCAB`.** MS/FS/MB/EP/PR
  codes, their labels, the default MB/EP/PR text patterns, their precedence
  and the taxonomic rules are in `TRAIT_VOCAB`
  (`R/config/harmonization_config.R`); read them through `get_trait_vocab()`,
  `trait_codes()`, `trait_code_label()`, `classify_by_patterns(text, trait)`
  and `apply_taxon_rules(taxonomy, trait)`, never through a private regex or
  a literal code list. Sessions and saved JSON may tune `*_patterns` only.
  `tests/testthat/test-trait-vocabulary.R` fails on a habitat, mobility or
  protection word inside a `grepl("...")` in the MB/EP/PR code. When a code
  changes meaning, bump `trait_vocab_version`: offline DBs
  (`metadata.trait_vocab_version`), cache envelopes and the ML model are
  then ignored until rebuilt or retrained, and the production offline DB
  must be rebuilt after the deploy.
- **Normalise database fields where they are read; batch lookups go
  through the safe wrapper.** A rank or column a database does not report
  arrives as `character(0)`, and a zero-length value makes `&&` / `if`
  error. Read taxonomy and other scalar fields with `.scalar_chr(x)`
  (`R/functions/validation_utils.R`; NA for NULL, zero-length, NA or "").
  Loops over species call `lookup_species_traits_safely()`, never
  `lookup_species_traits()` directly, so one bad taxon becomes a warning
  and an error row instead of aborting the batch. When a lookup fix changes
  values the cache already holds, bump `TRAIT_LOOKUP_REVISION`
  (`harmonization.R`).
- **List every `SHARK4R::` call in `SHARK4R_FUNCTIONS`**
  (`R/functions/shark_api_utils.R`). SHARK4R 1.2.0 dropped every function
  the SHARK tab called, and nothing noticed until production.
  `test-shark-rewire.R` now fails when a `SHARK4R::` call is missing from
  the list or from `getNamespaceExports("SHARK4R")`, and CI installs
  SHARK4R so the test runs. The SHARK wrappers return a status (`found` /
  `not_found` / `no_key` / `error`, or `ok` / `empty` / `error`) instead
  of NULL, so the tab can say why nothing came back.

## Commit Messages

We use [Conventional Commits](https://www.conventionalcommits.org/). This drives our automatic versioning and changelog generation.

### Format

```
type(scope): description

[optional body]
[optional footer]
```

### Types

| Type | Description | Version Bump |
|------|-------------|-------------|
| `feat` | New feature | minor |
| `fix` | Bug fix | patch |
| `perf` | Performance improvement | patch |
| `refactor` | Code restructuring | patch |
| `test` | Adding/updating tests | patch |
| `docs` | Documentation only | patch |
| `chore` | Maintenance tasks | patch |
| `ci` | CI/CD changes | patch |
| `style` | Code style (formatting) | patch |
| `security` | Security fix | patch |

### Examples

```
feat(traits): add FishBase trait pre-fetcher for offline knowledge base
fix(ui): use bs4Dash status names for valueBox colors
perf(traits): remove redundant individual DB calls from trait research
test(traits): add multi-regional species tests
docs: update README badges
```

### Breaking Changes

Add `!` after type or include `BREAKING CHANGE` in the footer for major version bumps:

```
feat(api)!: redesign trait lookup return format

BREAKING CHANGE: lookup_species_traits() now returns a list instead of a data frame
```

## Pull Request Process

1. Fork the repository
2. Create a feature branch: `git checkout -b feat/my-feature`
3. Write conventional commits
4. Ensure all tests pass: `Rscript tests/run_all_tests.R`
5. Ensure lint passes: `Rscript -e "lintr::lint('your_file.R')"`
6. Open a PR against `master`
7. CI must pass before merge

## Testing

- Write tests for new functionality in `tests/`
- Run the full suite before submitting: `Rscript tests/run_all_tests.R`
- For specific test files: `Rscript -e "testthat::test_file('tests/test_file.R')"`

## Releases

Releases are managed by the maintainers using `scripts/release.R`. Contributors do not need to update version numbers or changelogs — the automated system handles this from conventional commits.

## Code of Conduct

By participating in this project, you agree to abide by our [Code of Conduct](CODE_OF_CONDUCT.md).
