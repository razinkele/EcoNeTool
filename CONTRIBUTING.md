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
  so a slow upstream day doesn't hang the whole run.

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
