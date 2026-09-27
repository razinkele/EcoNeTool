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
    "FIND_KEEP=()",
    "for KEEP in \"${PRESERVE_ITEMS[@]}\"; do",
    "    FIND_KEEP+=(! -name \"$KEEP\")",
    "done",
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

# --- keep lists only count when the delete actually uses them ----------------

write_fixture <- function(lines, ext) {
  path <- tempfile(fileext = ext)
  writeLines(lines, path)
  path
}

test_that("protected_deployment_sh() ignores PRESERVE_ITEMS the find line does not use", {
  sh <- c(
    "PRESERVE_ITEMS=(\"r-libs\" \"cache\" \"data\" \"config\" \"models\")",
    "FIND_KEEP=()",
    "for KEEP in \"${PRESERVE_ITEMS[@]}\"; do",
    "    FIND_KEEP+=(! -name \"$KEEP\")",
    "done"
  )
  find_head <- "find /srv/shiny-server/EcoNeTool -mindepth 1 -maxdepth 1 \\"
  find_no_keep <- c(find_head, "     ! -name '.*' -exec rm -rf {} +")
  find_keep <- c(find_head, "     ! -name '.*' \"${FIND_KEEP[@]}\" -exec rm -rf {} +")
  unused <- write_fixture(c(sh, find_no_keep), ".sh")
  no_loop <- write_fixture(c(sh[1], find_keep), ".sh")
  used <- write_fixture(c(sh, find_keep), ".sh")
  on.exit(unlink(c(unused, no_loop, used)), add = TRUE)

  expect_equal(protected_deployment_sh(unused), ".*")
  expect_equal(protected_deployment_sh(no_loop), ".*")
  expect_setequal(protected_deployment_sh(used), c(".*", "r-libs", "cache", "data", "config", "models"))
})

test_that("protected_windows_ps1() ignores a $preserve the find command does not use", {
  preserve <- "$preserve = \"! -name '.*' ! -name data ! -name cache ! -name r-libs ! -name models ! -name config\""
  find_no_keep <- "Invoke-RemoteCommand \"${sudoPrefix}find $APP_DEPLOY_PATH -mindepth 1 -exec rm -rf {} +\""
  find_keep <- "Invoke-RemoteCommand \"${sudoPrefix}find $APP_DEPLOY_PATH -mindepth 1 $preserve -exec rm -rf {} +\""
  unused <- write_fixture(c(preserve, find_no_keep), ".ps1")
  used <- write_fixture(c(preserve, find_keep), ".ps1")
  on.exit(unlink(c(unused, used)), add = TRUE)

  expect_equal(protected_windows_ps1(unused), character(0))
  expect_setequal(protected_windows_ps1(used), DEPLOY_PROTECTED)
})

test_that("protected_root_sh() ignores EXCLUDE_PATTERNS that rsync does not receive", {
  sh <- c(
    "EXCLUDE_PATTERNS=(",
    "  \"/data/\"",
    "  \"r-libs\"",
    ")",
    "  local exclude_opts=\"\"",
    "  for pattern in \"${EXCLUDE_PATTERNS[@]}\"; do",
    "    exclude_opts+=\"--exclude='${pattern}' \"",
    "  done",
    "    local rsync_cmd=\"rsync -avz --progress --delete\""
  )
  unused <- write_fixture(c(sh, "    rsync_cmd+=\" ./\""), ".sh")
  used <- write_fixture(c(sh, "    rsync_cmd+=\" ${exclude_opts}\"", "    rsync_cmd+=\" ./\""), ".sh")
  on.exit(unlink(c(unused, used)), add = TRUE)

  expect_equal(protected_root_sh(unused), character(0))
  expect_setequal(protected_root_sh(used), c("data", "r-libs"))
})

# --- deployment/deploy.sh: failed copies and backup size ----------------------

test_that("deployment/deploy.sh records a failed copy instead of aborting under set -e", {
  code <- code_lines(deploy_file("deployment/deploy.sh"))
  expect_true(any(grepl("^\\s*set -e", code)), info = "premise: the script runs under set -e")
  copies <- grep("cp -(rT|vf) \"\\$SRC\"", code, value = TRUE)
  expect_length(copies, 2L)
  # a bare cp would exit the script mid-restore, before COPY_STATUS is read
  guarded <- grepl("^\\s*(if|elif)\\s", copies) | grepl("\\|\\|", copies)
  expect_true(all(guarded), info = paste(copies[!guarded], collapse = " | "))
  # ... and the recorded errors still end the run with a non-zero status
  err_check <- grep("if \\[ \\$\\{#ERRORS\\[@\\]\\} -eq 0 \\]", code)
  expect_length(err_check, 1L)
  expect_true(any(grepl("^\\s*exit 1\\s*$", code[err_check + 1:4])))
})

test_that("deployment/deploy.sh backups leave out data/", {
  code <- code_lines(deploy_file("deployment/deploy.sh"))
  tar_line <- grep("tar -czf", code, value = TRUE)
  expect_length(tar_line, 1L)
  # -C /srv/shiny-server EcoNeTool: members are EcoNeTool/..., so the
  # exclude must name EcoNeTool/data and come before the member argument
  excl <- regexpr("--exclude=EcoNeTool/data", tar_line, fixed = TRUE)
  member <- regexpr("-C /srv/shiny-server EcoNeTool", tar_line, fixed = TRUE)
  expect_true(excl > 0 && member > 0 && excl < member, info = tar_line)
})

# --- deploy-windows.ps1 (F4, F5, F7) -----------------------------------------

test_that("deploy-windows.ps1 keeps dotfiles, data, cache, r-libs, models and config on the live tree", {
  keep <- protected_windows_ps1(deploy_file("deploy-windows.ps1"))
  expect_equal(setdiff(DEPLOY_PROTECTED, keep), character(0))
})

test_that("deploy-windows.ps1 -NoSudo wipes staging completely", {
  code <- code_lines(deploy_file("deploy-windows.ps1"))
  # staging holds no live state; anything left there is cp -rT'd over live
  expect_true(any(grepl('^\\s*\\$preserve\\s*=\\s*""\\s*$', code)))
})

test_that("deploy-windows.ps1 ships models/ and never uploads runtime config", {
  code <- code_lines(deploy_file("deploy-windows.ps1"))
  expect_true("models/" %in% script_array(code, "DEPLOY_ITEMS"))
  excl <- script_array(code, "EXCLUDE_PATTERNS")
  expect_equal(setdiff(RUNTIME_CONFIG_FILES, excl), character(0))
})

test_that("deploy-windows.ps1 backups are tar archives outside site_dir", {
  path <- deploy_file("deploy-windows.ps1")
  dirs <- backup_dirs(path)
  expect_setequal(dirs, c("/home/$User/backups", "/srv/shiny-server-data/EcoNeTool/backups"))
  code <- code_lines(path)
  expect_false(any(grepl("cp\\s+-r\\s", code)), info = "a cp -r backup is a live, runnable app copy")
  expect_true(any(grepl("tar -czf", code, fixed = TRUE)))
  expect_true(any(grepl("chmod 600", code, fixed = TRUE)))
  # data/ (~3.1 GB) would overrun Invoke-RemoteCommand's 60 s limit
  expect_true(any(grepl("tar --exclude=$appLeaf/data -czf", code, fixed = TRUE)))
})

test_that("deploy-windows.ps1 empties staging on every upload path, and only staging", {
  code <- code_lines(deploy_file("deploy-windows.ps1"))
  # Through Invoke-RemoteCommand, so -DryRun only logs it
  expect_true(any(grepl('Invoke-RemoteCommand "rm -rf $APP_DEPLOY_PATH && mkdir -p $APP_DEPLOY_PATH"',
                        code, fixed = TRUE)))

  guard <- grep("-notmatch '", code, value = TRUE, fixed = TRUE)
  expect_length(guard, 1L)
  rx <- sub("^.*-notmatch '([^']+)'.*$", "\\1", guard)
  expect_true(grepl(rx, "/home/razinka/EcoNeTool_staging", perl = TRUE))
  expect_true(grepl(rx, "/home/alice/EcoNeTool_staging", perl = TRUE), info = "-User alice")
  expect_false(grepl(rx, "/srv/shiny-server/EcoNeTool", perl = TRUE))
  expect_false(grepl(rx, "/home/razinka/EcoNeTool_staging/data", perl = TRUE))
  expect_false(grepl(rx, "/home//EcoNeTool_staging", perl = TRUE), info = "empty -User")
})

test_that("deploy-windows.ps1 Test-ShouldExclude drops runtime config by relative path only", {
  pwsh <- Sys.which("pwsh")
  skip_if(!nzchar(pwsh), "pwsh (PowerShell 7) not on PATH")
  q <- function(x) if (.Platform$OS.type == "windows") shQuote(x, type = "cmd") else shQuote(x)

  # Evaluate the script's own $EXCLUDE_PATTERNS and Test-ShouldExclude
  # (nested in Deploy-Application) without running the deploy.
  runner <- tempfile(fileext = ".ps1")
  on.exit(unlink(runner), add = TRUE)
  writeLines(c(
    "param([string]$Script, [string]$Paths)",
    "$ast = [System.Management.Automation.Language.Parser]::ParseFile($Script, [ref]$null, [ref]$null)",
    "$arr = $ast.Find({ param($n) $n -is [System.Management.Automation.Language.AssignmentStatementAst] -and",
    "  $n.Left.Extent.Text -eq '$EXCLUDE_PATTERNS' }, $true)",
    "$fn = $ast.Find({ param($n) $n -is [System.Management.Automation.Language.FunctionDefinitionAst] -and",
    "  $n.Name -eq 'Test-ShouldExclude' }, $true)",
    "Invoke-Expression $arr.Extent.Text",
    "Invoke-Expression $fn.Extent.Text",
    "foreach ($p in $Paths.Split('|')) { '{0}={1}' -f $p, (Test-ShouldExclude $p) }"
  ), runner)

  cases <- c(
    "C:\\repo\\config\\api_keys.R"                = "True",
    "C:\\repo\\config\\api_keys.json"             = "True",
    "C:\\repo\\config\\harmonization_custom.json" = "True",
    "/repo/config/api_keys.R"                     = "True",
    "C:\\repo\\config\\.Renviron"                 = "True",
    "C:\\repo\\config\\api_keys.R.template"       = "False",
    "C:\\repo\\R\\functions\\api_keys.R"          = "False",
    "C:\\repo\\models\\trait_ml_models.rds"       = "False",
    "C:\\repo\\R\\modules\\plugin_server.R"       = "False"
  )
  out <- system2(pwsh, c("-NoProfile", "-NonInteractive", "-File", q(runner),
                         "-Script", q(normalizePath(deploy_file("deploy-windows.ps1"))),
                         "-Paths", q(paste(names(cases), collapse = "|"))),
                 stdout = TRUE, stderr = TRUE)
  got <- sub("^.*=", "", out)
  names(got) <- sub("=[^=]*$", "", out)
  expect_equal(got[names(cases)], cases)
})
