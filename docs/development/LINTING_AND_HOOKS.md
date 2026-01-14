# Linting and Pre-commit Hooks

This document describes the code quality tools configured for EcoNeTool.

## Quick Start

```bash
# Install pre-commit (requires Python)
pip install pre-commit

# Install hooks (run once after cloning)
pre-commit install

# Run all checks manually
pre-commit run --all-files

# Update hooks to latest versions
pre-commit autoupdate
```

## Configuration Files

| File | Purpose |
|------|---------|
| `.pre-commit-config.yaml` | Pre-commit hook definitions |
| `.lintr` | R linting rules (lintr package) |
| `.editorconfig` | Editor formatting consistency |

## Pre-commit Hooks

The following hooks run automatically on `git commit`:

### File Hygiene
- **trailing-whitespace**: Remove trailing spaces
- **end-of-file-fixer**: Ensure files end with newline
- **check-yaml**: Validate YAML syntax
- **check-json**: Validate JSON syntax
- **check-added-large-files**: Warn on files > 1MB
- **check-merge-conflict**: Detect unresolved conflicts
- **mixed-line-ending**: Enforce LF line endings

### Shell Scripts
- **shellcheck**: Lint bash scripts for common issues

### Repository Hygiene
- **no-debug-prints**: Prevent `DEBUG:` strings in commits
- **no-rproj-user**: Prevent IDE files from being tracked
- **no-large-binaries**: Warn when staging binary data files

## R Linting

The `.lintr` file configures R code linting. To run manually:

```r
# Install lintr
install.packages("lintr")

# Lint a file
lintr::lint("app.R")

# Lint entire project
lintr::lint_dir("R/")
```

### Current Rules

- Line length: 120 characters (generous for Shiny UI code)
- Assignment: Use `<-` not `=`
- Whitespace: No tabs, no trailing whitespace
- Some strict rules disabled for existing codebase compatibility

## EditorConfig

The `.editorconfig` file ensures consistent formatting across editors:

- **Indentation**: 2 spaces for R/YAML/JSON, 4 spaces for shell scripts
- **Line endings**: LF (Unix-style), except `.bat` files
- **Charset**: UTF-8
- **Final newline**: Yes

Most modern editors support EditorConfig natively or via plugins.

## Skipping Hooks

If you need to bypass hooks temporarily:

```bash
# Skip all hooks for one commit
git commit --no-verify -m "WIP: work in progress"

# Skip specific hook
SKIP=shellcheck git commit -m "message"
```

**Note**: Use sparingly. Hooks exist to maintain code quality.

## CI Integration

For GitHub Actions, add to your workflow:

```yaml
- name: Run pre-commit
  uses: pre-commit/action@v3.0.0
```

Or run manually in CI:

```yaml
- name: Install pre-commit
  run: pip install pre-commit

- name: Run checks
  run: pre-commit run --all-files
```

## Troubleshooting

### Hook fails on existing code

Some hooks may fail on existing code that doesn't meet standards. Options:

1. Fix the issues (recommended)
2. Add files to exclusions in `.pre-commit-config.yaml`
3. Skip temporarily with `--no-verify`

### shellcheck not found

Install shellcheck:
- **macOS**: `brew install shellcheck`
- **Ubuntu**: `apt install shellcheck`
- **Windows**: `scoop install shellcheck` or via WSL

### R lintr not working

The R lintr hook requires R and the lintr package installed locally.
The hook is commented out by default. Uncomment in `.pre-commit-config.yaml` if R is available.
