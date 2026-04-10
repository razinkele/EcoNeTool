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
