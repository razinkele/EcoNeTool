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

safe_system <- function(cmd) {
  tryCatch(
    system(cmd, intern = TRUE, ignore.stderr = TRUE),
    warning = function(w) character(0),
    error = function(e) character(0)
  )
}

detect_bump_type <- function() {
  latest_tag <- trimws(safe_system("git describe --tags --abbrev=0 --match=\"v*\""))
  if (length(latest_tag) == 0 || latest_tag == "") {
    latest_tag <- NULL
  }

  if (is.null(latest_tag)) {
    raw <- safe_system("git log --format=%s%n%b HEAD")
  } else {
    raw <- safe_system(sprintf("git log --format=%%s%%n%%b %s..HEAD", latest_tag))
  }

  if (length(raw) == 0) return("patch")

  combined <- paste(raw, collapse = "\n")

  if (grepl("BREAKING CHANGE", combined, fixed = TRUE)) return("major")

  if (is.null(latest_tag)) {
    subjects <- safe_system("git log --format=%s HEAD")
  } else {
    subjects <- safe_system(sprintf("git log --format=%%s %s..HEAD", latest_tag))
  }
  for (s in subjects) {
    m <- regmatches(s, regexec(CC_REGEX, s))[[1]]
    if (length(m) > 0 && m[4] == "!") return("major")
  }

  for (s in subjects) {
    if (grepl("^feat(\\(|:)", s)) return("minor")
  }

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
    minor <- minor + 1L
    patch <- 0L
  } else {
    patch <- patch + 1L
  }

  list(MAJOR = major, MINOR = minor, PATCH = patch,
       VERSION = sprintf("%d.%d.%d", major, minor, patch))
}

auto_version_name <- function() {
  latest_tag <- trimws(safe_system("git describe --tags --abbrev=0 --match=\"v*\""))
  if (length(latest_tag) == 0 || latest_tag == "") {
    subjects <- safe_system("git log --format=%s HEAD")
  } else {
    subjects <- safe_system(sprintf("git log --format=%%s %s..HEAD", latest_tag))
  }

  scopes <- character(0)
  for (s in subjects) {
    m <- regmatches(s, regexec("^feat\\((.+?)\\)", s))[[1]]
    if (length(m) > 0 && m[2] != "") {
      scopes <- c(scopes, m[2])
    }
  }

  if (length(scopes) == 0) return("Maintenance Release")

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

  content <- gsub(
    sprintf("version = \\{%s\\}", gsub("\\.", "\\\\.", old_version)),
    sprintf("version = {%s}", new_version),
    content
  )

  content <- gsub(
    "\\*\\*Current Version\\*\\*:\\s*[0-9]+\\.[0-9]+\\.[0-9]+",
    sprintf("**Current Version**: %s", new_version),
    content
  )

  content <- gsub(
    "\\*\\*Last Updated\\*\\*:\\s*[0-9]{4}-[0-9]{2}-[0-9]{2}",
    sprintf("**Last Updated**: %s", release_date),
    content
  )

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

  if (!is.null(opts$version)) {
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
  release_date <- format(Sys.Date(), "%Y-%m-%d")
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

  git_commit <- trimws(safe_system("git rev-parse --short HEAD"))
  git_branch <- trimws(safe_system("git rev-parse --abbrev-ref HEAD"))
  if (length(git_commit) == 0) git_commit <- ""
  if (length(git_branch) == 0) git_branch <- ""

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

  update_app_r(new_version, release_date)
  update_readme(old_version, new_version, release_date)

  cat(sprintf("\nVersion bumped: %s -> %s\n", old_version, new_version))
  invisible(list(version = new_version, name = version_name, date = release_date))
}

if (identical(environment(), globalenv())) {
  version_bump()
}
