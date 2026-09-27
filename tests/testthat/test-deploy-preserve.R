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

# --- deployment/deploy.sh (F81, F5, F7) --------------------------------------

test_that("deployment/deploy.sh keeps dotfiles, data, cache, r-libs, models and config", {
  keep <- protected_deployment_sh(deploy_file("deployment/deploy.sh"))
  missing <- setdiff(c(DEPLOY_PROTECTED, "restart.txt"), keep)
  expect_equal(missing, character(0), info = "not preserved by the find-based wipe")
})

test_that("deployment/deploy.sh never copies data/ or config/ over the live tree", {
  code <- code_lines(deploy_file("deployment/deploy.sh"))
  items <- script_array(code, "CRITICAL_ITEMS")
  expect_false("data" %in% items)
  expect_false("config" %in% items)
  expect_true("models" %in% items, info = "models/ is tracked and loaded by ml_trait_prediction.R")
  # only the template goes into the preserved config/
  expect_true(any(grepl("config/api_keys.R.template", code, fixed = TRUE)))
})

test_that("deployment/deploy.sh copies with cp -rT, not rsync, and keeps *.csv", {
  code <- code_lines(deploy_file("deployment/deploy.sh"))
  expect_false(any(grepl("\\brsync\\b", code)), info = "rsync is not installed on laguna")
  expect_true(any(grepl("cp -rT \"$SRC\" \"$DEST/$ITEM\"", code, fixed = TRUE)))
  expect_false(any(grepl("--exclude='*.csv'", code, fixed = TRUE)))
})

test_that("deployment/deploy.sh writes tar backups outside site_dir, mode 600", {
  path <- deploy_file("deployment/deploy.sh")
  dirs <- backup_dirs(path)
  expect_equal(dirs, "/srv/shiny-server-data/EcoNeTool/backups")
  code <- code_lines(path)
  expect_true(any(grepl("tar -czf", code, fixed = TRUE)))
  expect_true(any(grepl("chmod 600", code, fixed = TRUE)))
})

test_that("the reference shiny-server.conf has no directory index anywhere", {
  conf <- code_lines(deploy_file("deployment/shiny-server.conf"))
  expect_false(any(grepl("directory_index\\s+on", conf)))
  # the fallback heredoc in deployment/deploy.sh writes the same file
  expect_false(any(grepl("directory_index\\s+on", code_lines(deploy_file("deployment/deploy.sh")))))
})
