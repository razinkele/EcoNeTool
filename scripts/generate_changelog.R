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
  url <- tryCatch(
    trimws(system("git remote get-url origin", intern = TRUE, ignore.stderr = TRUE)),
    warning = function(w) "",
    error = function(e) ""
  )
  if (length(url) == 0 || url == "") return(NULL)
  url <- sub("^git@github\\.com:", "https://github.com/", url)
  url <- sub("\\.git$", "", url)
  url
}

get_all_tags <- function() {
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
  date_str <- tryCatch(
    trimws(system(
      sprintf("git log -1 --format=%%ai %s", tag),
      intern = TRUE, ignore.stderr = TRUE
    )),
    warning = function(w) character(0),
    error = function(e) character(0)
  )
  if (length(date_str) == 0) return(Sys.Date())
  as.Date(substr(date_str, 1, 10))
}

get_commits <- function(from = NULL, to = "HEAD") {
  start_marker <- "---COMMIT_START---"
  end_marker <- "---COMMIT_END---"
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

  blocks <- strsplit(raw_text, start_marker)[[1]]
  blocks <- trimws(blocks)
  blocks <- blocks[blocks != ""]

  result <- lapply(blocks, function(block) {
    block <- sub(paste0("\\s*", end_marker, "\\s*$"), "", block)
    lines <- strsplit(block, "\n")[[1]]
    lines <- lines[lines != ""]

    if (length(lines) < 3) return(NULL)

    hash <- lines[1]
    date_str <- lines[2]
    subject <- lines[3]

    # Validate date format (YYYY-MM-DD) to skip malformed blocks
    date_part <- substr(date_str, 1, 10)
    parsed_date <- tryCatch(as.Date(date_part), error = function(e) NA)
    if (is.na(parsed_date)) return(NULL)

    body_start <- which(lines == "---BODY---")
    body <- ""
    if (length(body_start) > 0 && body_start[1] < length(lines)) {
      body_lines <- lines[(body_start[1] + 1):length(lines)]
      body <- paste(body_lines, collapse = "\n")
    }

    data.frame(
      hash = hash,
      date = parsed_date,
      subject = subject,
      body = body,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, Filter(Negate(is.null), result))
}

# --- Commit parsing ----------------------------------------------------------

CC_REGEX <- "^(feat|fix|perf|refactor|test|docs|chore|ci|build|security|style)(\\(.+?\\))?(!)?:\\s*(.+)$"

CATEGORY_ORDER <- c(
  "BREAKING CHANGES", "Added", "Performance", "Changed", "Fixed",
  "Security", "Testing", "Documentation", "Maintenance", "Style", "Other"
)

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
    scope <- gsub("[()]", "", m[3])
    breaking <- m[4] == "!"
    description <- m[5]
    category <- PREFIX_MAP[[prefix]]
    if (is.null(category)) category <- "Other"
  } else {
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

  parsed <- lapply(seq_len(nrow(commits_df)), function(i) {
    p <- parse_commit(commits_df$subject[i], commits_df$body[i])
    p$hash <- commits_df$hash[i]
    p
  })

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

  lines <- character(0)
  lines <- c(lines, sprintf("## [%s] - %s", version_label, format(date)))
  lines <- c(lines, "")

  if (length(breaking_lines) > 0) {
    lines <- c(lines, "### BREAKING CHANGES", "")
    lines <- c(lines, breaking_lines, "")
  }

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

  sections <- character(0)
  compare_links <- character(0)

  unreleased_label <- if (!is.null(opts$version)) opts$version else "Unreleased"
  unreleased_date <- if (!is.null(opts$version)) Sys.Date() else Sys.Date()

  if (nrow(tags) == 0) {
    commits <- get_commits(from = NULL, to = "HEAD")
    if (!is.null(commits) && nrow(commits) > 0) {
      sections <- c(sections, format_version_section(unreleased_label, unreleased_date, commits))
    }
  } else {
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

    for (i in seq_len(nrow(tags))) {
      tag <- tags$tag[i]
      version <- sub("^v", "", tag)
      tag_date <- get_tag_date(tag)

      if (i < nrow(tags)) {
        commits <- get_commits(from = tags$tag[i + 1], to = tag)
        if (!is.null(repo_url)) {
          compare_links <- c(compare_links,
            sprintf("[%s]: %s/compare/%s...%s", version, repo_url, tags$tag[i + 1], tag))
        }
      } else {
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
    cat("\n--- CHANGELOG PREVIEW ---\n\n")
    if (length(sections) > 0) {
      cat(sections[1])
    } else {
      cat("No unreleased changes found.\n")
    }
    cat("\n--- END PREVIEW ---\n")
    return(invisible(NULL))
  }

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
