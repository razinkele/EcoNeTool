# =============================================================================
# Deploy scripts must not destroy server-only state (F81, F4, F5, F7, F86)
# =============================================================================
# Three scripts can deploy EcoNeTool: deploy.sh (rsync, unusable against
# laguna, which has no rsync), deployment/deploy.sh (run as root on the
# server) and deploy-windows.ps1 (the path actually used, with -NoSudo).
# The suite does not execute them, so these are source guards over
# code_lines() (helper-deploy.R): comments are stripped first, so a comment
# mentioning `.Renviron` can no longer make a guard pass.

deploy_file <- function(rel) file.path(get_app_root(), rel)

# --- helper self-test --------------------------------------------------------

test_that("code_lines() ignores comments, so a commented-out keep does not count", {
  fixture <- tempfile(fileext = ".sh")
  on.exit(unlink(fixture), add = TRUE)
  writeLines(c(
    "#!/bin/bash",
    "# PRESERVE_ITEMS=(\"r-libs\" \"cache\" \"data\" \"config\" \"models\")",
    "# find /srv/shiny-server/EcoNeTool -mindepth 1 ! -name '.*' -exec rm -rf {} +",
    "PRESERVE_ITEMS=(\"r-libs\" \"cache\")  # keep .Renviron data config models too",
    "find /srv/shiny-server/EcoNeTool -mindepth 1 -maxdepth 1 \"${FIND_KEEP[@]}\" -exec rm -rf {} +",
    "echo \"${#PRESERVE_ITEMS[@]} kept\"",
    "cp -rT \"$SRC\" \\",
    "      \"$DEST/$ITEM\"",
    "echo \"   # shown to the user\""
  ), fixture)

  # The pre-F86 guard grepped the raw text and would have passed:
  raw <- paste(readLines(fixture), collapse = "\n")
  expect_true(grepl("-name '.*'", raw, fixed = TRUE))

  keep <- protected_deployment_sh(fixture)
  expect_setequal(keep, c("r-libs", "cache"))
  expect_false(".*" %in% keep)
  expect_false(all(DEPLOY_PROTECTED %in% keep))

  code <- code_lines(fixture)
  # `${#arr[@]}` is not a comment
  expect_true(any(grepl("${#PRESERVE_ITEMS[@]}", code, fixed = TRUE)))
  # continuation lines are joined into one logical line
  expect_true(any(grepl("cp -rT \"\\$SRC\"\\s+\"\\$DEST/\\$ITEM\"", code)))
  # Known limitation: a " #" inside a quoted string is cut like a comment.
  # That can only hide text from a guard, never invent a keep.
  expect_true("echo \"" %in% code)
})

# --- all three scripts -------------------------------------------------------

test_that("no deploy script wipes the live tree with rm -rf .../EcoNeTool/*", {
  for (rel in c("deploy.sh", "deployment/deploy.sh", "deploy-windows.ps1")) {
    offenders <- grep("rm\\s+-rf\\s+\\S*EcoNeTool/\\*", code_lines(deploy_file(rel)), value = TRUE)
    expect_equal(length(offenders), 0L, info = paste(rel, ":", paste(offenders, collapse = " | ")))
  }
})
