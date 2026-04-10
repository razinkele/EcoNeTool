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

  # Guard: must be run from project root
  if (!file.exists("VERSION") || !file.exists("scripts/version_bump.R")) {
    stop("release.R must be run from the project root directory. ",
         "Current directory: ", getwd(), call. = FALSE)
  }

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

  # Wrap post-bump steps so we can give recovery instructions if they fail
  tryCatch({
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
  }, error = function(e) {
    if (!opts$dry_run) {
      cat("\nERROR: Release aborted during post-bump steps.\n")
      cat("Version files have been modified but NOT committed.\n")
      cat("\nTo restore:\n")
      cat("  git checkout -- VERSION app.R README.md\n")
      cat(sprintf("\nOr re-run with explicit version to resume: --version %s\n", new_version))
    }
    stop(e)
  })

  if (opts$dry_run) {
    cat("\n=============================================================================\n")
    cat(sprintf("DRY RUN COMPLETE \u2014 would release v%s\n", new_version))
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

if (identical(environment(), globalenv())) {
  run_release()
}
