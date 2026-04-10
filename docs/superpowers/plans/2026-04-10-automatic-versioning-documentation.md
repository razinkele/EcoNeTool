# Automatic Versioning & Documentation System — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a pure-R automatic versioning system with CHANGELOG generation, API reference generation, modern README, user manual, and GitHub Actions integration — targeting version 1.4.3.

**Architecture:** Four standalone R scripts (`version_bump.R`, `generate_changelog.R`, `generate_api_reference.R`, `release.R`) orchestrate the release process. All scripts use only base R — zero external package dependencies. GitHub Actions workflows call these scripts for CI-driven releases.

**Tech Stack:** R (base only), GitHub Actions, GitHub-flavored Markdown, Mermaid diagrams

**Spec:** `docs/superpowers/specs/2026-04-10-automatic-versioning-documentation-design.md`

**Parallelism:** Tasks 1–3 and 5–7 are independent and can be executed by parallel subagents. Task 4 depends on 1–3. Task 8 depends on 4. Task 9 depends on all.

---

## Task 1: CHANGELOG Generator Script

**Files:**
- Create: `scripts/generate_changelog.R`

This script parses the full git history, groups commits by version tags, categorizes them by conventional commit type, and writes `CHANGELOG.md`.

- [ ] **Step 1: Create `scripts/generate_changelog.R`**

```r
#!/usr/bin/env Rscript
# =============================================================================
# generate_changelog.R — Generate CHANGELOG.md from git commit history
# =============================================================================
# Usage:
#   Rscript scripts/generate_changelog.R                      # Full regeneration
#   Rscript scripts/generate_changelog.R --preview             # Preview unreleased only
#   Rscript scripts/generate_changelog.R --version 1.4.3       # Label unreleased as version
#   Rscript scripts/generate_changelog.R --output FILE         # Custom output path
#
# Pure base R — zero external dependencies.
# =============================================================================

# --- CLI argument parsing ---------------------------------------------------

parse_changelog_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  opts <- list(preview = FALSE, output = "CHANGELOG.md", version = NULL)
  i <- 1
  while (i <= length(args)) {
    if (args[i] == "--preview") {
      opts$preview <- TRUE
    } else if (args[i] == "--output" && i < length(args)) {
      i <- i + 1
      opts$output <- args[i]
    } else if (args[i] == "--version" && i < length(args)) {
      i <- i + 1
      opts$version <- args[i]
    }
    i <- i + 1
  }
  opts
}

# --- Git helpers -------------------------------------------------------------

get_repo_url <- function() {
  url <- trimws(system("git remote get-url origin 2>/dev/null", intern = TRUE))
  if (length(url) == 0 || url == "") return(NULL)
  # Convert SSH to HTTPS and strip .git suffix

  url <- sub("^git@github\\.com:", "https://github.com/", url)
  url <- sub("\\.git$", "", url)
  url
}

get_all_tags <- function() {
  # Get tags with their commit hashes, sorted by commit date
  raw <- system(
    "git tag -l \"v*\" --sort=-version:refname --format=\"%(refname:short) %(objectname:short)\"",
    intern = TRUE
  )
  if (length(raw) == 0 || (length(raw) == 1 && raw[1] == "")) return(data.frame())
  parts <- strsplit(raw, " ")
  data.frame(
    tag = vapply(parts, `[`, character(1), 1),
    hash = vapply(parts, `[`, character(1), 2),
    stringsAsFactors = FALSE
  )
}

get_tag_date <- function(tag) {
  date_str <- trimws(system(
    sprintf("git log -1 --format=%%ai %s 2>/dev/null", tag),
    intern = TRUE
  ))
  if (length(date_str) == 0) return(Sys.Date())
  as.Date(substr(date_str, 1, 10))
}

get_commits <- function(from = NULL, to = "HEAD") {
  # Use marker-based format to avoid delimiter collisions in subject/body
  start_marker <- "---COMMIT_START---"
  end_marker <- "---COMMIT_END---"
  # Format: each commit block is START\nhash\ndate\nsubject\n---BODY---\nbody\nEND
  fmt <- sprintf(
    "--format=%s%%n%%h%%n%%ai%%n%%s%%n---BODY---%%n%%b%%n%s",
    start_marker, end_marker
  )
  if (is.null(from)) {
    cmd <- sprintf("git log %s %s --reverse", fmt, to)
  } else {
    cmd <- sprintf("git log %s %s..%s --reverse", fmt, from, to)
  }
  raw <- system(cmd, intern = TRUE)
  raw_text <- paste(raw, collapse = "\n")
  if (raw_text == "") return(data.frame())

  # Split into commit blocks
  blocks <- strsplit(raw_text, start_marker)[[1]]
  blocks <- trimws(blocks)
  blocks <- blocks[blocks != ""]

  result <- lapply(blocks, function(block) {
    # Remove end marker
    block <- sub(paste0("\\s*", end_marker, "\\s*$"), "", block)
    lines <- strsplit(block, "\n")[[1]]
    lines <- lines[lines != ""]

    if (length(lines) < 3) return(NULL)

    hash <- lines[1]
    date_str <- lines[2]
    subject <- lines[3]

    # Body is everything after the ---BODY--- marker
    body_start <- which(lines == "---BODY---")
    body <- ""
    if (length(body_start) > 0 && body_start[1] < length(lines)) {
      body_lines <- lines[(body_start[1] + 1):length(lines)]
      body <- paste(body_lines, collapse = "\n")
    }

    data.frame(
      hash = hash,
      date = as.Date(substr(date_str, 1, 10)),
      subject = subject,
      body = body,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, Filter(Negate(is.null), result))
}

# --- Commit parsing ----------------------------------------------------------

# Conventional commit regex
CC_REGEX <- "^(feat|fix|perf|refactor|test|docs|chore|ci|build|security|style)(\\(.+?\\))?(!)?:\\s*(.+)$"

# Category display order
CATEGORY_ORDER <- c(
  "BREAKING CHANGES", "Added", "Performance", "Changed", "Fixed",
  "Security", "Testing", "Documentation", "Maintenance", "Style", "Other"
)

# Prefix -> category mapping
PREFIX_MAP <- list(
  feat = "Added",
  fix = "Fixed",
  perf = "Performance",
  refactor = "Changed",
  test = "Testing",
  docs = "Documentation",
  chore = "Maintenance",
  ci = "Maintenance",
  build = "Maintenance",
  security = "Security",
  style = "Style"
)

parse_commit <- function(subject, body = "") {
  m <- regmatches(subject, regexec(CC_REGEX, subject))[[1]]

  if (length(m) > 0 && m[1] != "") {
    prefix <- m[2]
    scope <- gsub("[()]", "", m[3])  # Remove parens
    breaking <- m[4] == "!"
    description <- m[5]
    category <- PREFIX_MAP[[prefix]]
    if (is.null(category)) category <- "Other"
  } else {
    # Non-conventional commit fallback
    scope <- ""
    breaking <- FALSE
    description <- subject
    if (grepl("^(Add|Initial commit|Reorganize)", subject, ignore.case = FALSE)) {
      category <- "Added"
    } else if (grepl("^Fix", subject, ignore.case = FALSE)) {
      category <- "Fixed"
    } else if (grepl("^(Clean up|Update)", subject, ignore.case = FALSE)) {
      category <- "Changed"
    } else {
      category <- "Other"
    }
  }

  # Check body for BREAKING CHANGE
  if (!breaking && grepl("BREAKING CHANGE", body, fixed = TRUE)) {
    breaking <- TRUE
  }

  list(
    category = category,
    scope = scope,
    description = description,
    breaking = breaking
  )
}

# --- Formatting --------------------------------------------------------------

format_commit_line <- function(parsed, hash) {
  if (parsed$scope != "") {
    sprintf("- **%s:** %s (%s)", parsed$scope, parsed$description, hash)
  } else {
    sprintf("- %s (%s)", parsed$description, hash)
  }
}

format_version_section <- function(version_label, date, commits_df) {
  if (is.null(commits_df) || nrow(commits_df) == 0) return("")

  # Parse all commits
  parsed <- lapply(seq_len(nrow(commits_df)), function(i) {
    p <- parse_commit(commits_df$subject[i], commits_df$body[i])
    p$hash <- commits_df$hash[i]
    p
  })

  # Group by category
  by_category <- list()
  breaking_lines <- character(0)

  for (p in parsed) {
    line <- format_commit_line(p, p$hash)
    if (p$breaking) {
      breaking_lines <- c(breaking_lines, line)
    }
    cat_name <- p$category
    by_category[[cat_name]] <- c(by_category[[cat_name]], line)
  }

  # Build section
  lines <- character(0)
  lines <- c(lines, sprintf("## [%s] - %s", version_label, format(date)))
  lines <- c(lines, "")

  # Breaking changes first
  if (length(breaking_lines) > 0) {
    lines <- c(lines, "### BREAKING CHANGES", "")
    lines <- c(lines, breaking_lines, "")
  }

  # Categories in order
  for (cat_name in CATEGORY_ORDER) {
    if (cat_name == "BREAKING CHANGES") next
    if (!is.null(by_category[[cat_name]])) {
      lines <- c(lines, sprintf("### %s", cat_name), "")
      lines <- c(lines, by_category[[cat_name]], "")
    }
  }

  paste(lines, collapse = "\n")
}

# --- Main entry point --------------------------------------------------------

generate_changelog <- function(opts = parse_changelog_args()) {
  cat("Generating CHANGELOG...\n")

  repo_url <- get_repo_url()
  tags <- get_all_tags()

  # Build version sections
  sections <- character(0)
  compare_links <- character(0)

  # Determine label for unreleased commits
  # --version flag overrides "Unreleased" (used during release before tag exists)
  unreleased_label <- if (!is.null(opts$version)) opts$version else "Unreleased"
  unreleased_date <- if (!is.null(opts$version)) Sys.Date() else Sys.Date()

  if (nrow(tags) == 0) {
    # No tags — all commits go into one section
    commits <- get_commits(from = NULL, to = "HEAD")
    if (!is.null(commits) && nrow(commits) > 0) {
      sections <- c(sections, format_version_section(unreleased_label, unreleased_date, commits))
    }
  } else {
    # Commits after latest tag → Unreleased (or version label)
    unreleased <- get_commits(from = tags$tag[1], to = "HEAD")
    if (!is.null(unreleased) && nrow(unreleased) > 0) {
      sections <- c(sections, format_version_section(unreleased_label, unreleased_date, unreleased))
      if (!is.null(repo_url)) {
        if (unreleased_label == "Unreleased") {
          compare_links <- c(compare_links,
            sprintf("[Unreleased]: %s/compare/%s...HEAD", repo_url, tags$tag[1]))
        } else {
          compare_links <- c(compare_links,
            sprintf("[%s]: %s/compare/%s...HEAD", unreleased_label, repo_url, tags$tag[1]))
        }
      }
    }

    # Each tag range
    for (i in seq_len(nrow(tags))) {
      tag <- tags$tag[i]
      version <- sub("^v", "", tag)
      tag_date <- get_tag_date(tag)

      if (i < nrow(tags)) {
        # Commits between this tag and the previous (older) one
        commits <- get_commits(from = tags$tag[i + 1], to = tag)
        if (!is.null(repo_url)) {
          compare_links <- c(compare_links,
            sprintf("[%s]: %s/compare/%s...%s", version, repo_url, tags$tag[i + 1], tag))
        }
      } else {
        # Oldest tag — all commits before it
        commits <- get_commits(from = NULL, to = tag)
        if (!is.null(repo_url)) {
          compare_links <- c(compare_links,
            sprintf("[%s]: %s/releases/tag/%s", version, repo_url, tag))
        }
      }

      if (!is.null(commits) && nrow(commits) > 0) {
        sections <- c(sections, format_version_section(version, tag_date, commits))
      }
    }
  }

  if (opts$preview) {
    # Preview mode: only show first section (unreleased or latest)
    cat("\n--- CHANGELOG PREVIEW ---\n\n")
    if (length(sections) > 0) {
      cat(sections[1])
    } else {
      cat("No unreleased changes found.\n")
    }
    cat("\n--- END PREVIEW ---\n")
    return(invisible(NULL))
  }

  # Assemble full changelog
  header <- paste(
    "# Changelog",
    "",
    "All notable changes to EcoNeTool will be documented in this file.",
    "",
    "The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),",
    "and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).",
    "",
    "> Auto-generated by `scripts/generate_changelog.R` from git commit history.",
    "",
    sep = "\n"
  )

  body <- paste(sections, collapse = "\n---\n\n")

  footer <- ""
  if (length(compare_links) > 0) {
    footer <- paste(c("", "---", "", compare_links, ""), collapse = "\n")
  }

  content <- paste(header, body, footer, sep = "\n")

  writeLines(content, opts$output)
  cat(sprintf("CHANGELOG written to %s (%d version sections)\n",
    opts$output, length(sections)))
  invisible(opts$output)
}

# Run if called directly
if (identical(environment(), globalenv())) {
  generate_changelog()
}
```

- [ ] **Step 2: Test the CHANGELOG generator**

Run:
```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/generate_changelog.R --preview
```
Expected: Prints a preview of unreleased changes (all commits since no tags exist yet).

Then run full generation:
```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/generate_changelog.R
```
Expected: Creates `CHANGELOG.md` with all 119+ commits grouped into an "Unreleased" section (since no tags exist yet). Verify the file exists and has content.

- [ ] **Step 3: Verify CHANGELOG output quality**

Open `CHANGELOG.md` and verify:
1. Header has "Keep a Changelog" and "Semantic Versioning" links
2. Conventional commits are categorized correctly (feat → Added, fix → Fixed, etc.)
3. Non-conventional early commits (e.g., "Initial commit", "Add hover tooltips") are classified via fallback
4. Scopes appear in bold: `**traits:** description`
5. Each entry has a short hash in parentheses

- [ ] **Step 4: Commit**

```bash
git add scripts/generate_changelog.R
git commit -m "feat(versioning): add CHANGELOG generator from git history"
```

---

## Task 2: Version Bump Script

**Files:**
- Create: `scripts/version_bump.R`

This script calculates the next version from conventional commits and updates all version references across the codebase.

- [ ] **Step 1: Create `scripts/version_bump.R`**

```r
#!/usr/bin/env Rscript
# =============================================================================
# version_bump.R — Calculate version bump and update all version references
# =============================================================================
# Usage:
#   Rscript scripts/version_bump.R                        # Auto-detect from commits
#   Rscript scripts/version_bump.R --bump patch            # Force bump type
#   Rscript scripts/version_bump.R --version 2.0.0         # Force exact version
#   Rscript scripts/version_bump.R --name "Release Name"   # Set VERSION_NAME
#   Rscript scripts/version_bump.R --dry-run               # Preview only
#
# Pure base R — zero external dependencies.
# =============================================================================

# --- CLI argument parsing ---------------------------------------------------

parse_bump_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  opts <- list(
    bump = NULL,       # "patch", "minor", "major", or NULL for auto-detect
    version = NULL,    # Explicit version override
    name = NULL,       # VERSION_NAME override
    dry_run = FALSE
  )
  i <- 1
  while (i <= length(args)) {
    if (args[i] == "--bump" && i < length(args)) {
      i <- i + 1
      opts$bump <- args[i]
    } else if (args[i] == "--version" && i < length(args)) {
      i <- i + 1
      opts$version <- args[i]
    } else if (args[i] == "--name" && i < length(args)) {
      i <- i + 1
      opts$name <- args[i]
    } else if (args[i] == "--dry-run") {
      opts$dry_run <- TRUE
    }
    i <- i + 1
  }
  opts
}

# --- Version reading/writing ------------------------------------------------

read_version_file <- function(path = "VERSION") {
  if (!file.exists(path)) stop("VERSION file not found: ", path)
  lines <- readLines(path, warn = FALSE)
  info <- list()
  for (line in lines) {
    if (grepl("^\\s*#", line) || grepl("^\\s*$", line)) next
    if (grepl("=", line)) {
      parts <- strsplit(line, "=", fixed = TRUE)[[1]]
      if (length(parts) >= 2) {
        key <- trimws(parts[1])
        value <- trimws(paste(parts[-1], collapse = "="))
        info[[key]] <- value
      }
    }
  }
  info
}

write_version_file <- function(info, path = "VERSION") {
  lines <- c(
    "# EcoNeTool Version Information",
    "#",
    "# This file tracks the version number and release information",
    "# Format: MAJOR.MINOR.PATCH",
    "# See CHANGELOG.md for detailed change history",
    "",
    sprintf("VERSION=%s", info$VERSION),
    sprintf("VERSION_NAME=%s", info$VERSION_NAME),
    sprintf("RELEASE_DATE=%s", info$RELEASE_DATE),
    sprintf("STATUS=%s", info$STATUS),
    "",
    "# Version components",
    sprintf("MAJOR=%s", info$MAJOR),
    sprintf("MINOR=%s", info$MINOR),
    sprintf("PATCH=%s", info$PATCH),
    "",
    "# Git information (if available)",
    "# Set by deployment scripts",
    sprintf("GIT_COMMIT=%s", info$GIT_COMMIT),
    sprintf("GIT_BRANCH=%s", info$GIT_BRANCH),
    sprintf("BUILD_DATE=%s", info$BUILD_DATE),
    "",
    "# Deployment information",
    sprintf("DEPLOYED_BY=%s", if (!is.null(info$DEPLOYED_BY)) info$DEPLOYED_BY else ""),
    sprintf("DEPLOYED_ON=%s", if (!is.null(info$DEPLOYED_ON)) info$DEPLOYED_ON else "")
  )
  writeLines(lines, path)
}

# --- Bump calculation -------------------------------------------------------

CC_REGEX <- "^(feat|fix|perf|refactor|test|docs|chore|ci|build|security|style)(\\(.+?\\))?(!)?:\\s*(.+)$"

detect_bump_type <- function() {
  # Get latest tag

  latest_tag <- trimws(system(
    "git describe --tags --abbrev=0 --match=\"v*\" 2>/dev/null",
    intern = TRUE
  ))
  if (length(latest_tag) == 0 || latest_tag == "") {
    latest_tag <- NULL
  }

  # Get commits since tag (or all commits)
  if (is.null(latest_tag)) {
    raw <- system("git log --format=%s%n%b HEAD", intern = TRUE)
  } else {
    raw <- system(sprintf("git log --format=%%s%%n%%b %s..HEAD", latest_tag), intern = TRUE)
  }

  if (length(raw) == 0) return("patch")

  combined <- paste(raw, collapse = "\n")

  # Check for breaking changes
  if (grepl("BREAKING CHANGE", combined, fixed = TRUE)) return("major")

  # Check for ! in conventional commit subjects
  subjects <- system(
    if (is.null(latest_tag)) "git log --format=%s HEAD"
    else sprintf("git log --format=%%s %s..HEAD", latest_tag),
    intern = TRUE
  )
  for (s in subjects) {
    m <- regmatches(s, regexec(CC_REGEX, s))[[1]]
    if (length(m) > 0 && m[4] == "!") return("major")
  }

  # Check for feat: commits → minor
  for (s in subjects) {
    if (grepl("^feat(\\(|:)", s)) return("minor")
  }

  # Default: patch
  "patch"
}

calculate_new_version <- function(current, bump_type) {
  major <- as.integer(current$MAJOR)
  minor <- as.integer(current$MINOR)
  patch <- as.integer(current$PATCH)

  if (bump_type == "major") {
    major <- major + 1L
    minor <- 0L
    patch <- 0L
  } else if (bump_type == "minor") {
    major <- major
    minor <- minor + 1L
    patch <- 0L
  } else {
    patch <- patch + 1L
  }

  list(MAJOR = major, MINOR = minor, PATCH = patch,
       VERSION = sprintf("%d.%d.%d", major, minor, patch))
}

auto_version_name <- function() {
  # Try to auto-generate from feat: commit scopes
  latest_tag <- trimws(system(
    "git describe --tags --abbrev=0 --match=\"v*\" 2>/dev/null",
    intern = TRUE
  ))
  if (length(latest_tag) == 0 || latest_tag == "") {
    subjects <- system("git log --format=%s HEAD", intern = TRUE)
  } else {
    subjects <- system(sprintf("git log --format=%%s %s..HEAD", latest_tag), intern = TRUE)
  }

  scopes <- character(0)
  for (s in subjects) {
    m <- regmatches(s, regexec("^feat\\((.+?)\\)", s))[[1]]
    if (length(m) > 0 && m[2] != "") {
      scopes <- c(scopes, m[2])
    }
  }

  if (length(scopes) == 0) return("Maintenance Release")

  # Title-case the most common scopes
  scope_counts <- sort(table(scopes), decreasing = TRUE)
  top_scopes <- names(scope_counts)[1:min(3, length(scope_counts))]
  top_scopes <- vapply(top_scopes, function(s) {
    paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
  }, character(1))

  paste(top_scopes, collapse = " & ")
}

# --- File updaters -----------------------------------------------------------

update_app_r <- function(new_version, release_date, dry_run = FALSE) {
  path <- "app.R"
  if (!file.exists(path)) {
    warning("app.R not found, skipping")
    return(invisible(NULL))
  }
  content <- readLines(path, warn = FALSE)

  pattern <- "^# CURRENT VERSION: v"
  idx <- grep(pattern, content)
  if (length(idx) == 0) {
    warning("Could not find '# CURRENT VERSION:' line in app.R")
    return(invisible(NULL))
  }

  new_line <- sprintf("# CURRENT VERSION: v%s (%s)", new_version, release_date)

  if (dry_run) {
    cat(sprintf("  app.R line %d: '%s' -> '%s'\n", idx[1], content[idx[1]], new_line))
    return(invisible(NULL))
  }

  content[idx[1]] <- new_line
  writeLines(content, path)
  cat(sprintf("  Updated app.R line %d\n", idx[1]))
}

update_readme <- function(old_version, new_version, release_date, dry_run = FALSE) {
  path <- "README.md"
  if (!file.exists(path)) {
    warning("README.md not found, skipping")
    return(invisible(NULL))
  }
  content <- readLines(path, warn = FALSE)
  original <- content

  # Update BibTeX version
  content <- gsub(
    sprintf("version = \\{%s\\}", gsub("\\.", "\\\\.", old_version)),
    sprintf("version = {%s}", new_version),
    content
  )

  # Update "Current Version" line
  content <- gsub(
    "\\*\\*Current Version\\*\\*:\\s*[0-9]+\\.[0-9]+\\.[0-9]+",
    sprintf("**Current Version**: %s", new_version),
    content
  )

  # Update "Last Updated" line
  content <- gsub(
    "\\*\\*Last Updated\\*\\*:\\s*[0-9]{4}-[0-9]{2}-[0-9]{2}",
    sprintf("**Last Updated**: %s", release_date),
    content
  )

  # Update HTML version comments
  content <- gsub(
    "<!-- VERSION:[0-9]+\\.[0-9]+\\.[0-9]+ -->",
    sprintf("<!-- VERSION:%s -->", new_version),
    content
  )

  if (dry_run) {
    changed <- which(content != original)
    if (length(changed) > 0) {
      cat(sprintf("  README.md: %d lines would change\n", length(changed)))
    } else {
      cat("  README.md: no version references found to update\n")
    }
    return(invisible(NULL))
  }

  writeLines(content, path)
  changed <- which(content != original)
  cat(sprintf("  Updated README.md (%d lines changed)\n", length(changed)))
}

# --- Main entry point --------------------------------------------------------

version_bump <- function(opts = parse_bump_args()) {
  current <- read_version_file()
  old_version <- current$VERSION
  cat(sprintf("Current version: %s\n", old_version))

  # Determine new version
  if (!is.null(opts$version)) {
    # Explicit version override
    parts <- strsplit(opts$version, "\\.")[[1]]
    if (length(parts) != 3) stop("Version must be in MAJOR.MINOR.PATCH format")
    new <- list(
      MAJOR = as.integer(parts[1]),
      MINOR = as.integer(parts[2]),
      PATCH = as.integer(parts[3]),
      VERSION = opts$version
    )
    bump_type <- "explicit"
  } else {
    bump_type <- if (!is.null(opts$bump)) opts$bump else detect_bump_type()
    new <- calculate_new_version(current, bump_type)
  }

  new_version <- new$VERSION
  release_date <- format(Sys.Date())
  version_name <- if (!is.null(opts$name)) opts$name else auto_version_name()

  cat(sprintf("Bump type: %s\n", bump_type))
  cat(sprintf("New version: %s\n", new_version))
  cat(sprintf("Version name: %s\n", version_name))
  cat(sprintf("Release date: %s\n", release_date))

  if (opts$dry_run) {
    cat("\n--- DRY RUN (no files modified) ---\n\n")
    cat("Files that would be updated:\n")
    cat("  VERSION\n")
    update_app_r(new_version, release_date, dry_run = TRUE)
    update_readme(old_version, new_version, release_date, dry_run = TRUE)
    return(invisible(list(version = new_version, name = version_name, date = release_date)))
  }

  # Get git info
  git_commit <- trimws(system("git rev-parse --short HEAD 2>/dev/null", intern = TRUE))
  git_branch <- trimws(system("git rev-parse --abbrev-ref HEAD 2>/dev/null", intern = TRUE))
  if (length(git_commit) == 0) git_commit <- ""
  if (length(git_branch) == 0) git_branch <- ""

  # Update VERSION file
  current$VERSION <- new_version
  current$MAJOR <- as.character(new$MAJOR)
  current$MINOR <- as.character(new$MINOR)
  current$PATCH <- as.character(new$PATCH)
  current$VERSION_NAME <- version_name
  current$RELEASE_DATE <- release_date
  current$STATUS <- "stable"
  current$GIT_COMMIT <- git_commit
  current$GIT_BRANCH <- git_branch
  current$BUILD_DATE <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

  write_version_file(current)
  cat("  Updated VERSION\n")

  # Update app.R
  update_app_r(new_version, release_date)

  # Update README.md
  update_readme(old_version, new_version, release_date)

  cat(sprintf("\nVersion bumped: %s -> %s\n", old_version, new_version))
  invisible(list(version = new_version, name = version_name, date = release_date))
}

# Run if called directly
if (identical(environment(), globalenv())) {
  version_bump()
}
```

- [ ] **Step 2: Test version bump in dry-run mode**

Run:
```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/version_bump.R --bump patch --dry-run
```
Expected: Prints "Current version: 1.4.2", "Bump type: patch", "New version: 1.4.3" and lists files that would be updated, without modifying anything. Verify VERSION file still says 1.4.2.

- [ ] **Step 3: Test auto-detection mode (dry-run)**

Run:
```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/version_bump.R --dry-run
```
Expected: Auto-detects "minor" bump type (because there are `feat:` commits since no tag exists). Reports "New version: 1.5.0". This is correct auto-detection behavior — the release script will override with `--bump patch` per user decision.

- [ ] **Step 4: Commit**

```bash
git add scripts/version_bump.R
git commit -m "feat(versioning): add version bump script with auto-detection"
```

---

## Task 3: API Reference Generator Script

**Files:**
- Create: `scripts/generate_api_reference.R`

Parses Roxygen2 `#'` comment blocks from all R source files and generates `docs/API_REFERENCE.md`.

**Important context:** Not all functions have `@export` tags. Many important functions (like `calculate_mti`, `get_topological_indicators`) have no Roxygen2 docs at all. The generator captures all functions that DO have `#'` comment blocks — approximately 178 documented functions across 40 files.

- [ ] **Step 1: Create `scripts/generate_api_reference.R`**

```r
#!/usr/bin/env Rscript
# =============================================================================
# generate_api_reference.R — Generate API reference from Roxygen2 comments
# =============================================================================
# Usage:
#   Rscript scripts/generate_api_reference.R
#   Rscript scripts/generate_api_reference.R --output docs/API_REFERENCE.md
#
# Parses #' comment blocks from R source files. Pure base R.
# =============================================================================

# --- CLI argument parsing ---------------------------------------------------

parse_api_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  opts <- list(output = "docs/API_REFERENCE.md")
  i <- 1
  while (i <= length(args)) {
    if (args[i] == "--output" && i < length(args)) {
      i <- i + 1
      opts$output <- args[i]
    }
    i <- i + 1
  }
  opts
}

# --- Domain mapping ----------------------------------------------------------

# Maps file paths to domain categories for organized output.
# Paths are relative to project root.
DOMAIN_MAP <- list(
  "Network Analysis" = c(
    "R/functions/topological_metrics.R",
    "R/functions/trophic_levels.R",
    "R/functions/keystoneness.R",
    "R/functions/flux_calculations.R",
    "R/functions/network_visualization.R"
  ),
  "Data Import/Export" = c(
    "R/functions/ecopath/",
    "R/functions/rpath/",
    "R/functions/metaweb_core.R",
    "R/functions/metaweb_io.R",
    "R/functions/ecobase_connection.R",
    "R/data_loading.R"
  ),
  "Trait Lookup" = c(
    "R/functions/trait_lookup/",
    "R/functions/local_trait_databases.R",
    "R/functions/trait_foodweb.R",
    "R/functions/trait_help_content.R",
    "R/functions/parallel_lookup.R"
  ),
  "Trait Imputation" = c(
    "R/functions/ml_trait_prediction.R",
    "R/functions/bhpmf_imputation.R",
    "R/functions/rphylopars_imputation.R",
    "R/functions/phylogenetic_imputation.R"
  ),
  "Spatial Analysis" = c(
    "R/functions/spatial_analysis.R",
    "R/functions/emodnet_habitat_utils.R",
    "R/functions/euseamap_regional_config.R"
  ),
  "Taxonomic APIs" = c(
    "R/functions/taxonomic_api_utils.R",
    "R/functions/shark_api_utils.R"
  ),
  "Shiny Modules" = c(
    "R/modules/",
    "R/ui/"
  ),
  "Configuration" = c(
    "R/config.R",
    "R/config/"
  ),
  "Utilities" = c(
    "R/functions/validation_utils.R",
    "R/functions/error_logging.R",
    "R/functions/cache_sqlite.R",
    "R/functions/api_rate_limiter.R",
    "R/functions/logger.R",
    "R/functions/auxillary_parser.R",
    "R/functions/functional_group_utils.R",
    "R/functions/uncertainty_quantification.R"
  )
)

get_domain_for_file <- function(filepath) {
  # Normalize path separators
  fp <- gsub("\\\\", "/", filepath)

  for (domain in names(DOMAIN_MAP)) {
    for (pattern in DOMAIN_MAP[[domain]]) {
      # Directory match (pattern ends with /)
      if (grepl("/$", pattern)) {
        if (startsWith(fp, pattern)) return(domain)
      } else {
        if (fp == pattern) return(domain)
      }
    }
  }
  "Other"
}

# --- Roxygen2 parser ---------------------------------------------------------

find_r_files <- function() {
  dirs <- c("R", "R/functions", "R/functions/ecopath", "R/functions/rpath",
            "R/functions/trait_lookup", "R/modules", "R/ui", "R/config")
  dirs <- dirs[dir.exists(dirs)]

  files <- unlist(lapply(dirs, function(d) {
    list.files(d, pattern = "\\.R$", full.names = TRUE, recursive = FALSE)
  }))

  # Exclude backup files and load_all.R
  files <- files[!grepl("(safeBackup|load_all\\.R)", files)]
  # Normalize separators
  gsub("\\\\", "/", files)
}

parse_roxygen_block <- function(lines, block_end_line) {
  # Walk backwards from block_end_line to find the start of the #' block
  start <- block_end_line
  while (start > 1 && grepl("^#'", lines[start - 1])) {
    start <- start - 1
  }

  block_lines <- lines[start:block_end_line]
  # Strip #' prefix
  stripped <- sub("^#'\\s?", "", block_lines)

  # Parse tags
  result <- list(
    title = "",
    description = "",
    params = list(),
    return_val = "",
    export = FALSE,
    examples = "",
    details = "",
    family = "",
    seealso = ""
  )

  current_tag <- "title"
  current_value <- character(0)

  flush_tag <- function() {
    val <- trimws(paste(current_value, collapse = " "))
    if (current_tag == "title" && result$title == "") {
      result$title <<- val
    } else if (current_tag == "description") {
      result$description <<- val
    } else if (current_tag == "param") {
      # First word is param name, rest is description
      parts <- strsplit(val, "\\s+", perl = TRUE)[[1]]
      if (length(parts) >= 1) {
        pname <- parts[1]
        pdesc <- paste(parts[-1], collapse = " ")
        result$params[[pname]] <<- pdesc
      }
    } else if (current_tag == "return") {
      result$return_val <<- val
    } else if (current_tag == "export") {
      result$export <<- TRUE
    } else if (current_tag == "details") {
      result$details <<- val
    } else if (current_tag == "family") {
      result$family <<- val
    } else if (current_tag == "seealso") {
      result$seealso <<- val
    } else if (current_tag == "examples") {
      result$examples <<- trimws(paste(current_value, collapse = "\n"))
    }
  }

  for (s in stripped) {
    tag_match <- regmatches(s, regexec("^@(\\w+)\\s*(.*)", s))[[1]]
    if (length(tag_match) > 0 && tag_match[1] != "") {
      # Flush previous tag
      flush_tag()
      current_tag <- tag_match[2]
      current_value <- if (tag_match[3] != "") tag_match[3] else character(0)
    } else {
      # Continuation of current tag (or title/description before first tag)
      if (s == "" && current_tag == "title" && length(current_value) > 0) {
        # Blank line after title switches to description
        flush_tag()
        current_tag <- "description"
        current_value <- character(0)
      } else {
        current_value <- c(current_value, s)
      }
    }
  }
  flush_tag()

  # If no explicit title, use first non-empty line
  if (result$title == "" && length(stripped) > 0) {
    result$title <- trimws(stripped[1])
  }

  result
}

extract_function_signature <- function(lines, after_line) {
  # Look for function definition after the roxygen block
  for (i in (after_line + 1):min(after_line + 5, length(lines))) {
    if (grepl("<-\\s*function\\s*\\(", lines[i])) {
      # Extract function name
      name_match <- regmatches(lines[i], regexec("^([a-zA-Z_.][a-zA-Z0-9_.]*)", lines[i]))[[1]]
      func_name <- if (length(name_match) > 0) name_match[2] else ""

      # Extract params from signature (may span multiple lines)
      sig_lines <- lines[i]
      j <- i
      while (!grepl("\\)", sig_lines) && j < min(i + 20, length(lines))) {
        j <- j + 1
        sig_lines <- paste(sig_lines, trimws(lines[j]))
      }
      # Extract just the params part
      params_match <- regmatches(sig_lines, regexec("function\\s*\\((.*)\\)", sig_lines))[[1]]
      params_str <- if (length(params_match) > 0) trimws(params_match[2]) else ""

      return(list(name = func_name, params_str = params_str, line = i))
    }
  }
  NULL
}

parse_function_params <- function(params_str) {
  if (params_str == "") return(list())
  # Split by comma, respecting nested parens/quotes
  params <- list()
  depth <- 0
  current <- ""
  in_string <- FALSE
  string_char <- ""

  for (ch in strsplit(params_str, "")[[1]]) {
    if (in_string) {
      current <- paste0(current, ch)
      if (ch == string_char) in_string <- FALSE
    } else if (ch %in% c("\"", "'")) {
      current <- paste0(current, ch)
      in_string <- TRUE
      string_char <- ch
    } else if (ch == "(") {
      depth <- depth + 1
      current <- paste0(current, ch)
    } else if (ch == ")") {
      depth <- depth - 1
      current <- paste0(current, ch)
    } else if (ch == "," && depth == 0) {
      params <- c(params, list(trimws(current)))
      current <- ""
    } else {
      current <- paste0(current, ch)
    }
  }
  if (trimws(current) != "") params <- c(params, list(trimws(current)))

  # Parse each param into name and default
  lapply(params, function(p) {
    if (grepl("=", p)) {
      parts <- strsplit(p, "=", fixed = TRUE)[[1]]
      list(name = trimws(parts[1]), default = trimws(paste(parts[-1], collapse = "=")))
    } else {
      list(name = trimws(p), default = NULL)
    }
  })
}

# --- Document generation -----------------------------------------------------

parse_all_files <- function() {
  files <- find_r_files()
  all_functions <- list()

  for (filepath in files) {
    lines <- readLines(filepath, warn = FALSE)
    domain <- get_domain_for_file(filepath)

    # Find all roxygen block endings (last #' line before non-#' content)
    in_block <- FALSE
    block_end <- NULL

    for (i in seq_along(lines)) {
      is_roxygen <- grepl("^#'", lines[i])

      if (is_roxygen) {
        in_block <- TRUE
        block_end <- i
      } else if (in_block) {
        # Block just ended — parse it
        roxygen <- parse_roxygen_block(lines, block_end)
        sig <- extract_function_signature(lines, block_end)

        if (!is.null(sig) && sig$name != "") {
          # Parse default values from signature
          sig_params <- parse_function_params(sig$params_str)

          all_functions <- c(all_functions, list(list(
            name = sig$name,
            file = filepath,
            line = sig$line,
            domain = domain,
            roxygen = roxygen,
            sig_params = sig_params
          )))
        }

        in_block <- FALSE
        block_end <- NULL
      }
    }
  }

  all_functions
}

format_function_entry <- function(func) {
  rox <- func$roxygen
  lines <- character(0)

  lines <- c(lines, sprintf("### %s()", func$name))
  lines <- c(lines, "")

  if (rox$title != "") {
    lines <- c(lines, rox$title)
    lines <- c(lines, "")
  }

  if (rox$description != "") {
    lines <- c(lines, rox$description)
    lines <- c(lines, "")
  }

  # Parameters table
  if (length(rox$params) > 0 || length(func$sig_params) > 0) {
    lines <- c(lines, "**Parameters:**", "")
    lines <- c(lines, "| Name | Default | Description |")
    lines <- c(lines, "|------|---------|-------------|")

    # Build defaults lookup from signature
    defaults <- list()
    for (sp in func$sig_params) {
      defaults[[sp$name]] <- if (!is.null(sp$default)) sp$default else "\u2014"
    }

    # Use roxygen params as primary, fall back to signature
    param_names <- union(names(rox$params), names(defaults))
    for (pname in param_names) {
      desc <- if (!is.null(rox$params[[pname]])) rox$params[[pname]] else ""
      def <- if (!is.null(defaults[[pname]])) defaults[[pname]] else "\u2014"
      lines <- c(lines, sprintf("| `%s` | `%s` | %s |", pname, def, desc))
    }
    lines <- c(lines, "")
  }

  if (rox$return_val != "") {
    lines <- c(lines, sprintf("**Returns:** %s", rox$return_val))
    lines <- c(lines, "")
  }

  # Source link
  lines <- c(lines, sprintf("**Source:** `%s:%d`", func$file, func$line))
  lines <- c(lines, "")

  paste(lines, collapse = "\n")
}

generate_api_reference <- function(opts = parse_api_args()) {
  cat("Generating API Reference...\n")

  all_funcs <- parse_all_files()
  cat(sprintf("Found %d documented functions across %d files\n",
    length(all_funcs),
    length(unique(vapply(all_funcs, function(f) f$file, character(1))))))

  # Read version info
  version <- "unknown"
  if (file.exists("VERSION")) {
    vinfo <- read_version_file("VERSION")
    version <- vinfo$VERSION
  }

  # Group by domain
  by_domain <- list()
  for (func in all_funcs) {
    d <- func$domain
    by_domain[[d]] <- c(by_domain[[d]], list(func))
  }

  # Domain display order
  domain_order <- c(
    "Network Analysis", "Data Import/Export", "Trait Lookup", "Trait Imputation",
    "Spatial Analysis", "Taxonomic APIs", "Shiny Modules", "Configuration",
    "Utilities", "Other"
  )

  # Build document
  lines <- character(0)

  # Header
  lines <- c(lines,
    "# EcoNeTool API Reference",
    "",
    "> Auto-generated from source code \u2014 do not edit manually.",
    sprintf("> Generated: %s | Version: %s", Sys.Date(), version),
    "> Regenerate: `Rscript scripts/generate_api_reference.R`",
    ""
  )

  # Table of contents
  lines <- c(lines, "## Table of Contents", "")
  for (domain in domain_order) {
    if (!is.null(by_domain[[domain]])) {
      anchor <- gsub("[^a-z0-9 ]", "", tolower(domain))
      anchor <- gsub(" ", "-", anchor)
      count <- length(by_domain[[domain]])
      lines <- c(lines, sprintf("- [%s](#%s) (%d functions)", domain, anchor, count))
    }
  }
  lines <- c(lines, "", "---", "")

  # Domain sections
  for (domain in domain_order) {
    funcs <- by_domain[[domain]]
    if (is.null(funcs)) next

    lines <- c(lines, sprintf("## %s", domain), "")

    # Sort functions alphabetically within domain
    func_names <- vapply(funcs, function(f) f$name, character(1))
    funcs <- funcs[order(func_names)]

    for (func in funcs) {
      lines <- c(lines, format_function_entry(func))
    }

    lines <- c(lines, "---", "")
  }

  content <- paste(lines, collapse = "\n")

  # Ensure output directory exists
  out_dir <- dirname(opts$output)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  writeLines(content, opts$output)
  cat(sprintf("API Reference written to %s (%d functions in %d domains)\n",
    opts$output, length(all_funcs), length(by_domain)))
  invisible(opts$output)
}

# Reuse version reader from version_bump.R if not already defined
if (!exists("read_version_file")) {
  read_version_file <- function(path = "VERSION") {
    if (!file.exists(path)) return(list(VERSION = "unknown"))
    lines <- readLines(path, warn = FALSE)
    info <- list()
    for (line in lines) {
      if (grepl("^\\s*#", line) || grepl("^\\s*$", line)) next
      if (grepl("=", line)) {
        parts <- strsplit(line, "=", fixed = TRUE)[[1]]
        if (length(parts) >= 2) {
          info[[trimws(parts[1])]] <- trimws(paste(parts[-1], collapse = "="))
        }
      }
    }
    info
  }
}

# Run if called directly
if (identical(environment(), globalenv())) {
  generate_api_reference()
}
```

- [ ] **Step 2: Test the API reference generator**

Run:
```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/generate_api_reference.R
```
Expected: Creates `docs/API_REFERENCE.md`. Prints count of documented functions found. Verify file exists and contains organized domain sections with function entries.

- [ ] **Step 3: Verify API reference output**

Open `docs/API_REFERENCE.md` and check:
1. Header has version and generation date
2. Table of contents lists all domains with function counts
3. Functions are grouped by domain
4. Each function has: title, parameters table, return value, source file:line
5. No empty domains (skip domains with 0 documented functions)

- [ ] **Step 4: Commit**

```bash
git add scripts/generate_api_reference.R docs/API_REFERENCE.md
git commit -m "feat(versioning): add API reference generator from Roxygen2 comments"
```

---

## Task 4: Release Orchestrator Script

**Files:**
- Create: `scripts/release.R`

**Depends on:** Tasks 1, 2, 3

This script orchestrates the full release: bump version, generate changelog, generate API ref, commit, and tag.

- [ ] **Step 1: Create `scripts/release.R`**

```r
#!/usr/bin/env Rscript
# =============================================================================
# release.R — Release orchestrator for EcoNeTool
# =============================================================================
# Usage:
#   Rscript scripts/release.R --bump patch                         # Patch release
#   Rscript scripts/release.R --bump minor --name "New Features"   # Minor release
#   Rscript scripts/release.R --version 2.0.0                      # Explicit version
#   Rscript scripts/release.R --bump patch --dry-run               # Preview only
#
# Orchestrates: version_bump.R -> generate_changelog.R ->
#               generate_api_reference.R -> git commit -> git tag
#
# Pure base R — zero external dependencies.
# =============================================================================

parse_release_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  opts <- list(
    bump = NULL,
    version = NULL,
    name = NULL,
    dry_run = FALSE
  )
  i <- 1
  while (i <= length(args)) {
    if (args[i] == "--bump" && i < length(args)) {
      i <- i + 1
      opts$bump <- args[i]
    } else if (args[i] == "--version" && i < length(args)) {
      i <- i + 1
      opts$version <- args[i]
    } else if (args[i] == "--name" && i < length(args)) {
      i <- i + 1
      opts$name <- args[i]
    } else if (args[i] == "--dry-run") {
      opts$dry_run <- TRUE
    }
    i <- i + 1
  }
  opts
}

run_release <- function(opts = parse_release_args()) {
  cat("=============================================================================\n")
  cat("EcoNeTool Release Process\n")
  cat("=============================================================================\n\n")

  # Step 1: Validate clean working tree
  cat("[1/8] Checking working tree...\n")
  status <- system("git status --porcelain", intern = TRUE)
  if (length(status) > 0 && !opts$dry_run) {
    cat("ERROR: Working tree is not clean. Commit or stash changes first.\n")
    cat("Uncommitted changes:\n")
    cat(paste("  ", status, collapse = "\n"), "\n")
    stop("Dirty working tree", call. = FALSE)
  }

  # Step 2: Source helper scripts
  cat("[2/8] Loading release scripts...\n")
  source("scripts/version_bump.R", local = TRUE)
  source("scripts/generate_changelog.R", local = TRUE)
  source("scripts/generate_api_reference.R", local = TRUE)

  # Step 3: Calculate and apply version bump
  cat("[3/8] Bumping version...\n")
  bump_opts <- list(
    bump = opts$bump,
    version = opts$version,
    name = opts$name,
    dry_run = opts$dry_run
  )
  bump_result <- version_bump(bump_opts)
  new_version <- bump_result$version

  # Step 4: Generate CHANGELOG (pass version so it labels correctly before tag exists)
  cat("\n[4/8] Generating CHANGELOG...\n")
  if (opts$dry_run) {
    generate_changelog(list(preview = TRUE, output = "CHANGELOG.md", version = new_version))
  } else {
    generate_changelog(list(preview = FALSE, output = "CHANGELOG.md", version = new_version))
  }

  # Step 5: Generate API Reference
  cat("\n[5/8] Generating API Reference...\n")
  if (!opts$dry_run) {
    generate_api_reference(list(output = "docs/API_REFERENCE.md"))
  } else {
    cat("  [dry-run] Would regenerate docs/API_REFERENCE.md\n")
  }

  if (opts$dry_run) {
    cat("\n=============================================================================\n")
    cat(sprintf("DRY RUN COMPLETE — would release v%s\n", new_version))
    cat("=============================================================================\n")
    cat("\nNo files were modified. Run without --dry-run to execute.\n")
    return(invisible(NULL))
  }

  # Step 6: Stage all modified files
  cat("\n[6/8] Staging files...\n")
  files_to_stage <- c("VERSION", "app.R", "README.md", "CHANGELOG.md",
                       "docs/API_REFERENCE.md")
  for (f in files_to_stage) {
    if (file.exists(f)) {
      system(sprintf("git add \"%s\"", f))
      cat(sprintf("  Staged: %s\n", f))
    }
  }

  # Step 7: Commit
  cat("\n[7/8] Committing...\n")
  commit_msg <- sprintf("chore(release): v%s", new_version)
  ret <- system(sprintf("git commit -m \"%s\"", commit_msg))
  if (ret != 0) stop("git commit failed (exit code ", ret, ")", call. = FALSE)
  cat(sprintf("  Committed: %s\n", commit_msg))

  # Step 8: Create annotated tag (use temp file for message to avoid quoting issues)
  cat("\n[8/8] Creating tag...\n")
  tag_name <- sprintf("v%s", new_version)
  tag_msg <- sprintf("Release %s - %s", new_version, bump_result$name)
  tag_msg_file <- tempfile(fileext = ".txt")
  writeLines(tag_msg, tag_msg_file)
  ret <- system(sprintf("git tag -a %s -F \"%s\"", tag_name, tag_msg_file))
  file.remove(tag_msg_file)
  if (ret != 0) stop("git tag failed (exit code ", ret, ")", call. = FALSE)
  cat(sprintf("  Tagged: %s\n", tag_name))

  cat("\n=============================================================================\n")
  cat(sprintf("Release v%s complete!\n", new_version))
  cat("=============================================================================\n")
  cat("\nTo publish:\n")
  cat("  git push --follow-tags\n")
  cat("\nOr push tag separately:\n")
  cat(sprintf("  git push origin %s\n", tag_name))
}

# Run if called directly
if (identical(environment(), globalenv())) {
  run_release()
}
```

- [ ] **Step 2: Test release in dry-run mode**

Run:
```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/release.R --bump patch --name "Versioning & Documentation" --dry-run
```
Expected: Prints the full release preview — version bump, changelog preview, API ref note — without modifying any files. Ends with "DRY RUN COMPLETE".

- [ ] **Step 3: Commit**

```bash
git add scripts/release.R
git commit -m "feat(versioning): add release orchestrator script"
```

---

## Task 5: GitHub Companion Files

**Files:**
- Create: `SECURITY.md`
- Create: `CONTRIBUTING.md`
- Create: `CODE_OF_CONDUCT.md`

These are independent of the R scripts and can be done in parallel with Tasks 1–4.

- [ ] **Step 1: Create `SECURITY.md`**

```markdown
# Security Policy

## Supported Versions

| Version | Supported          |
|---------|--------------------|
| Latest  | :white_check_mark: |
| < Latest | :x:               |

Only the latest release receives security updates.

## Reporting a Vulnerability

**Do NOT open a public GitHub issue for security vulnerabilities.**

Instead, please report security issues by emailing the project maintainers directly. Include:

1. Description of the vulnerability
2. Steps to reproduce
3. Potential impact
4. Suggested fix (if any)

### Response Timeline

- **Acknowledgment:** within 3 business days
- **Initial assessment:** within 7 business days
- **Fix or mitigation:** depends on severity

### Non-sensitive bugs

For bugs that are not security-sensitive, please use [GitHub Issues](https://github.com/razinkele/EcoNeTool/issues).

## Scope

This policy applies to the EcoNeTool R Shiny application and its deployment infrastructure. Third-party APIs (WoRMS, FishBase, OBIS, EMODnet) have their own security policies.
```

- [ ] **Step 2: Create `CONTRIBUTING.md`**

```markdown
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
```

- [ ] **Step 3: Create `CODE_OF_CONDUCT.md`**

Use the Contributor Covenant v2.1. Create the file with the standard text from https://www.contributor-covenant.org/version/2/1/code_of_conduct/ — the full text should be placed in `CODE_OF_CONDUCT.md`. The contact method should reference the GitHub repository issues as the reporting mechanism.

- [ ] **Step 4: Commit all companion files**

```bash
git add SECURITY.md CONTRIBUTING.md CODE_OF_CONDUCT.md
git commit -m "docs: add SECURITY.md, CONTRIBUTING.md, and CODE_OF_CONDUCT.md"
```

---

## Task 6: README.md Full Rewrite

**Files:**
- Modify: `README.md` (full rewrite)

Independent of Tasks 1–4. Requires companion files from Task 5 to exist for links to work, but the README can be written first.

**Important context for the implementer:** The current README is at `README.md` (584 lines). You are completely rewriting it. Read the current README first to understand what content to preserve (authors, citation, scientific references, data format details, functional group colors). Then write the new version using all modern GitHub markdown features.

- [ ] **Step 1: Read current README.md to extract content to preserve**

Read the full current `README.md`. Note these sections to carry forward:
- Scientific references and citation BibTeX (lines 506–528)
- Authors and funding acknowledgments (lines 532–548)
- License details (lines 551–556)
- Functional group color table (currently in the Data Format section)
- Required data columns table
- Default dataset info (Lithuanian Coastal Food Web)

- [ ] **Step 2: Write the new README.md**

Rewrite `README.md` with the structure defined in spec Section 5.2. The new README must include:

1. **Centered header** with `<div align="center">` containing title and shields.io badges:
   - CI status badge: `[![CI](https://github.com/razinkele/EcoNeTool/actions/workflows/ci.yml/badge.svg)](https://github.com/razinkele/EcoNeTool/actions/workflows/ci.yml)`
   - Release badge: `[![Release](https://img.shields.io/github/v/release/razinkele/EcoNeTool)](https://github.com/razinkele/EcoNeTool/releases)`
   - License badge: `[![License: GPL-3.0](https://img.shields.io/badge/License-GPL%203.0-blue.svg)](LICENSE)`
   - R version badge: `[![R >= 4.0.0](https://img.shields.io/badge/R-%E2%89%A5%204.0.0-blue.svg)](https://www.r-project.org/)`
   - Last commit badge: `[![Last Commit](https://img.shields.io/github/last-commit/razinkele/EcoNeTool)](https://github.com/razinkele/EcoNeTool/commits)`
   - Issues badge: `[![Issues](https://img.shields.io/github/issues/razinkele/EcoNeTool)](https://github.com/razinkele/EcoNeTool/issues)`

2. **Overview** (3-4 sentences) — what it does, who it's for, MARBEFES context

3. **Live Demo** — `> [!NOTE]` admonition with link to `http://laguna.ku.lt:3838/EcoNeTool/`

4. **Key Features** — table format with Category and Feature columns covering: Network Analysis, Trait Research (12 databases), Spatial Analysis, Rpath/Ecosim, Metawebs, Data Import/Export

5. **Quick Start** — `> [!TIP]` admonition with 3 methods (Rscript, RStudio, Docker placeholder)

6. **Installation** — `<details>` collapsible with prerequisites, install script, manual package install, verification

7. **Architecture** — Mermaid flowchart:
   ```mermaid
   flowchart LR
     A[Data Import] --> B[Network Analysis]
     A --> C[Trait Enrichment]
     B --> D[Visualization]
     C --> D
     D --> E[Export/Report]
     C --> F[Spatial Analysis]
     F --> D
   ```

8. **Project Structure** — `<details>` collapsible directory tree (from current README, updated)

9. **Data Format** — `<details>` collapsible with required columns table, supported formats, functional groups

10. **Analysis Features** — subsections: Network Metrics, Trait Research, Spatial, Rpath

11. **Documentation** — links to: `docs/API_REFERENCE.md`, `docs/USER_MANUAL.md`, `CHANGELOG.md`, `deployment/README.md`

12. **Deployment** — `<details>` collapsible with `> [!WARNING]` for prerequisites

13. **Contributing** — link to `CONTRIBUTING.md`, brief code style note

14. **Citation** — BibTeX with `version = {1.4.2}` (will be auto-updated by version_bump.R), `year = {2025}`

15. **Authors & Acknowledgments** — preserved from current README

16. **License** — GPL-3.0 + CC-BY-SA-4.0

17. **Contact & Support** — GitHub issues, project links

18. **Version footer** — `**Current Version**: 1.4.2` with `<!-- VERSION:1.4.2 -->` marker

- [ ] **Step 3: Verify README renders correctly**

Check that:
- No raw HTML or broken markdown syntax
- Mermaid diagram is valid (test by viewing on GitHub or using a markdown previewer)
- All collapsible `<details>` sections have proper `<summary>` tags
- Badge URLs are correct
- All internal links point to existing files

- [ ] **Step 4: Commit**

```bash
git add README.md
git commit -m "docs: rewrite README with modern GitHub format"
```

---

## Task 7: User Manual

**Files:**
- Create: `docs/USER_MANUAL.md`

Independent of all other tasks. This is hand-written content for marine scientists.

**Important context for the implementer:** You need to read the actual Shiny UI files to understand what each tab does. The 15 tabs are defined in `app.R` (sidebar menu, lines 218–310). UI definitions are in `R/ui/*.R` files. Key analysis functions are in `R/functions/`. Read enough to write accurate descriptions of each tab's purpose and workflow.

- [ ] **Step 1: Research the dashboard tabs**

Read these files to understand what each of the 15 tabs does:
- `app.R` lines 218–362 (sidebar menu items and tab item bindings)
- All files in `R/ui/` (one per tab)
- Key function files for understanding analysis outputs

Note the 15 tabs:
1. Dashboard (overview/home)
2. Data Import
3. Food Web Network
4. Topological Metrics
5. Biomass Analysis
6. Energy Fluxes
7. Keystoneness Analysis
8. Internal Data Editor
9. Metaweb Manager
10. Trait Research
11. Food Web Construction
12. Spatial Analysis
13. EcoBase Connection
14. SHARK Data
15. ECOPATH/ECOSIM (Rpath)

- [ ] **Step 2: Write `docs/USER_MANUAL.md`**

Write the full user manual following the structure in spec Section 8.2. The manual should be written for marine ecologists, not R developers. Use plain language. Avoid code references. Focus on:
- What each feature does scientifically
- How to use it through the dashboard UI
- How to interpret results
- Common workflows (import → analyze → export)

Cover all 11 sections defined in the spec:
1. Getting Started
2. Importing Data
3. Dashboard Overview (all 15 tabs)
4. Network Analysis
5. Trait Research
6. Rpath Integration
7. Metaweb Management
8. Spatial Analysis
9. Exporting Results
10. Troubleshooting
11. Glossary

The Glossary should include: connectance, generality, vulnerability, omnivory index, trophic level, ecotrophic efficiency (EE), production/biomass (P/B), consumption/biomass (Q/B), Mixed Trophic Impact (MTI), keystoneness, functional group, metaweb, modality, harmonization.

- [ ] **Step 3: Commit**

```bash
git add docs/USER_MANUAL.md
git commit -m "docs: add user manual for marine scientists"
```

---

## Task 8: GitHub Actions Workflows

**Files:**
- Create: `.github/workflows/release.yml`
- Create: `.github/workflows/auto-changelog.yml`
- Modify: `.github/workflows/ci.yml`

**Depends on:** Tasks 1–4 (scripts must exist for workflows to call them)

- [ ] **Step 1: Create `.github/workflows/release.yml`**

```yaml
# =============================================================================
# EcoNeTool — Release Workflow
# =============================================================================
# Manually triggered to create a new release.
# Runs the release.R script, pushes commit + tag, creates GitHub Release.
# =============================================================================

name: Release

on:
  workflow_dispatch:
    inputs:
      bump_type:
        description: 'Version bump type'
        required: true
        type: choice
        options:
          - patch
          - minor
          - major
      version_override:
        description: 'Explicit version (e.g., 2.0.0) — overrides bump_type'
        required: false
        type: string
      release_name:
        description: 'Release name (e.g., "Trait Provenance")'
        required: false
        type: string

permissions:
  contents: write

jobs:
  release:
    name: Create Release
    runs-on: ubuntu-latest
    steps:
      - name: Checkout code
        uses: actions/checkout@v4
        with:
          fetch-depth: 0  # Full history for tags and changelog

      - name: Set up R
        uses: r-lib/actions/setup-r@v2
        with:
          r-version: '4.4.1'

      - name: Configure git
        run: |
          git config user.name "github-actions[bot]"
          git config user.email "github-actions[bot]@users.noreply.github.com"

      - name: Run release script
        run: |
          ARGS=()
          if [ -n "${{ inputs.version_override }}" ]; then
            ARGS+=(--version "${{ inputs.version_override }}")
          else
            ARGS+=(--bump "${{ inputs.bump_type }}")
          fi
          if [ -n "${{ inputs.release_name }}" ]; then
            ARGS+=(--name "${{ inputs.release_name }}")
          fi
          Rscript scripts/release.R "${ARGS[@]}"
        shell: bash

      - name: Push commit and tag
        run: git push --follow-tags

      - name: Extract changelog for release body
        id: changelog
        run: |
          # Extract the first version section from CHANGELOG.md
          VERSION=$(grep -m1 "^## \[" CHANGELOG.md | sed 's/## \[\(.*\)\].*/\1/')
          # Extract content between first and second ## [ headers
          awk '/^## \[/{n++} n==1{print} n==2{exit}' CHANGELOG.md | tail -n +2 > release_body.md
          echo "version=$VERSION" >> $GITHUB_OUTPUT

      - name: Create GitHub Release
        env:
          GH_TOKEN: ${{ github.token }}
        run: |
          gh release create "v${{ steps.changelog.outputs.version }}" \
            --title "v${{ steps.changelog.outputs.version }}" \
            --notes-file release_body.md
```

- [ ] **Step 2: Create `.github/workflows/auto-changelog.yml`**

```yaml
# =============================================================================
# EcoNeTool — PR Changelog Preview
# =============================================================================
# Posts a preview of changelog entries for commits in a pull request.
# =============================================================================

name: PR Changelog Preview

on:
  pull_request:
    branches: [master, main]

permissions:
  contents: read
  pull-requests: write

jobs:
  changelog-preview:
    name: Changelog Preview
    runs-on: ubuntu-latest
    steps:
      - name: Checkout code
        uses: actions/checkout@v4
        with:
          fetch-depth: 0

      - name: Set up R
        uses: r-lib/actions/setup-r@v2
        with:
          r-version: '4.4.1'

      - name: Generate changelog preview
        run: |
          Rscript scripts/generate_changelog.R --preview > changelog_preview.txt 2>&1

      - name: Build comment body file
        run: |
          {
            echo "### Changelog Preview"
            echo ""
            echo "These entries will be added to CHANGELOG.md when this PR is included in a release:"
            echo ""
            echo '```'
            cat changelog_preview.txt
            echo '```'
            echo ""
            echo "*Auto-generated by [generate_changelog.R](../blob/master/scripts/generate_changelog.R)*"
          } > comment_body.md

      - name: Post or update PR comment
        env:
          GH_TOKEN: ${{ github.token }}
        run: |
          PR_NUM=${{ github.event.pull_request.number }}

          # Find existing changelog preview comment (using REST API for numeric IDs)
          COMMENT_ID=$(gh api "repos/${{ github.repository }}/issues/${PR_NUM}/comments" \
            --jq '.[] | select(.body | startswith("### Changelog Preview")) | .id' | head -1)

          if [ -n "$COMMENT_ID" ]; then
            gh api "repos/${{ github.repository }}/issues/comments/${COMMENT_ID}" \
              -X PATCH -F "body=@comment_body.md"
          else
            gh pr comment "$PR_NUM" --body-file comment_body.md
          fi
```

- [ ] **Step 3: Add version drift detection to `.github/workflows/ci.yml`**

Add a new job to the existing `ci.yml` file, **before** the `ci-status` job. Insert after the `lintr` job block (after line 221):

```yaml

  # ===========================================================================
  # Version drift detection
  # ===========================================================================
  version-drift:
    name: Version drift check
    runs-on: ubuntu-latest
    if: github.event_name == 'push'
    steps:
      - name: Checkout code
        uses: actions/checkout@v4
        with:
          fetch-depth: 0

      - name: Check VERSION vs latest tag
        run: |
          # Read version from VERSION file
          FILE_VERSION=$(grep "^VERSION=" VERSION | cut -d= -f2)
          echo "VERSION file: $FILE_VERSION"

          # Get latest tag
          LATEST_TAG=$(git describe --tags --abbrev=0 --match="v*" 2>/dev/null || echo "none")
          echo "Latest tag: $LATEST_TAG"

          if [ "$LATEST_TAG" = "none" ]; then
            echo "::notice::No version tags found. Consider creating an initial tag: git tag -a v$FILE_VERSION -m 'Release $FILE_VERSION'"
          else
            TAG_VERSION=${LATEST_TAG#v}
            if [ "$FILE_VERSION" != "$TAG_VERSION" ]; then
              echo "::warning::VERSION file ($FILE_VERSION) differs from latest tag ($LATEST_TAG). Unreleased changes detected."
            else
              echo "VERSION file matches latest tag"
            fi
          fi
```

Also update the `ci-status` job `needs` array to include `version-drift`:

Change line 229:
```yaml
    needs: [pre-commit, repo-hygiene, shellcheck, r-syntax]
```
to:
```yaml
    needs: [pre-commit, repo-hygiene, shellcheck, r-syntax, version-drift]
```

The `version-drift` job should NOT cause CI failure — it's informational only (uses `::warning::` and `::notice::`, not `exit 1`).

- [ ] **Step 4: Commit all workflow files**

```bash
git add .github/workflows/release.yml .github/workflows/auto-changelog.yml .github/workflows/ci.yml
git commit -m "ci: add release workflow, PR changelog preview, and version drift detection"
```

---

## Task 9: Execute First Release (v1.4.3)

**Depends on:** All previous tasks (1–8)

This task runs the release process to bump to v1.4.3, creating the first version tag and fully generated CHANGELOG.

- [ ] **Step 1: Verify all scripts work in dry-run**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/release.R --bump patch --name "Versioning & Documentation" --dry-run
```
Expected: Full dry-run output showing v1.4.3, changelog preview, no files modified.

- [ ] **Step 2: Run the actual release**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/release.R --bump patch --name "Versioning & Documentation"
```
Expected: Updates VERSION to 1.4.3, updates app.R version line, updates README version references, regenerates CHANGELOG.md from full git history, regenerates docs/API_REFERENCE.md, creates commit `chore(release): v1.4.3`, creates annotated tag `v1.4.3`.

- [ ] **Step 3: Verify the release**

Run these checks:
```bash
# Verify VERSION file
grep "^VERSION=" VERSION
# Expected: VERSION=1.4.3

# Verify git tag
git tag -l "v1.4.3"
# Expected: v1.4.3

# Verify tag is annotated
git show v1.4.3 --no-patch
# Expected: Shows tagger, date, and tag message

# Verify CHANGELOG has version sections
head -20 CHANGELOG.md
# Expected: Header + first version section

# Verify API_REFERENCE.md exists and has content
wc -l docs/API_REFERENCE.md
# Expected: Several hundred lines

# Verify app.R version line
grep "CURRENT VERSION" app.R
# Expected: # CURRENT VERSION: v1.4.3 (2026-04-10)
```

- [ ] **Step 4: Print push command for user**

Do NOT push automatically. Print:
```
Release v1.4.3 complete. To publish:
  git push --follow-tags
```
