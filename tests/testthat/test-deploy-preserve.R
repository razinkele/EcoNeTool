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
  expect_true(any(grepl('Invoke-RemoteCommand "rm -rf $APP_DEPLOY_PATH && mkdir -p $APP_DEPLOY_PATH && echo STAGING_CLEARED"',
                        code, fixed = TRUE)))

  guard <- grep("-notmatch '", code, value = TRUE, fixed = TRUE)
  expect_length(guard, 1L)
  rx <- sub("^.*-notmatch '([^']+)'.*$", "\\1", guard)
  expect_true(grepl(rx, "/home/razinka/EcoNeTool_staging", perl = TRUE))
  expect_true(grepl(rx, "/home/alice/EcoNeTool_staging", perl = TRUE), info = "-User alice")
  expect_false(grepl(rx, "/srv/shiny-server/EcoNeTool", perl = TRUE))
  expect_false(grepl(rx, "/home/razinka/EcoNeTool_staging/data", perl = TRUE))
  expect_false(grepl(rx, "/home//EcoNeTool_staging", perl = TRUE), info = "empty -User")
  # M1: the path is interpolated into remote shell commands
  expect_false(grepl(rx, "/home/a;b/EcoNeTool_staging", perl = TRUE), info = "shell metacharacter in -User")
  expect_false(grepl(rx, "/home/a b/EcoNeTool_staging", perl = TRUE), info = "space in -User")
  expect_false(grepl(rx, "/home/$(id)/EcoNeTool_staging", perl = TRUE), info = "substitution in -User")

  # ... and -User itself is validated with the same character class
  user <- grep("^\\s*\\[string\\]\\$User\\s*=", code)
  expect_length(user, 1L)
  expect_true(any(grepl("[ValidatePattern('^[a-z_][a-z0-9_-]*$')]", code[user + c(-1L, 0L)], fixed = TRUE)),
              info = "-User needs [ValidatePattern('^[a-z_][a-z0-9_-]*$')]")
})

# Invoke-RemoteCommand does not check ssh's exit status (I2). A failed
# staging wipe or extract would leave old staging files that Test-Deployment
# accepts, and the follow-up `cp -rT` would push them live. Each command
# must echo a success marker as its last `&&` step, its output must be
# captured, and a check on the next code lines must throw when the marker is
# missing (outside -DryRun, where Invoke-RemoteCommand returns "").
test_that("deploy-windows.ps1 throws when the staging wipe or the extract prints no success marker", {
  code <- code_lines(deploy_file("deploy-windows.ps1"))
  for (marker in c("STAGING_CLEARED", "EXTRACT_OK")) {
    cmd <- grep(sprintf("^\\s*\\$\\w+\\s*=\\s*Invoke-RemoteCommand \".*&& echo %s\"\\s*$", marker), code)
    expect_length(cmd, 1L)
    if (length(cmd) != 1L) next
    var <- sub("^\\s*(\\$\\w+)\\s*=.*$", "\\1", code[cmd])
    check <- code[(cmd + 1L):min(length(code), cmd + 3L)]
    # the `if` on the very next code line, with a throw directly under it
    cond <- grepl("^\\s*if\\s*\\(", check[1]) &&
      grepl("-not $DryRun", check[1], fixed = TRUE) &&
      grepl(sprintf("-not ((%s | Out-String) -match '%s')", var, marker), check[1], fixed = TRUE)
    expect_true(cond, info = paste(marker, "check:", check[1]))
    expect_true(grepl("^\\s*throw\\b", check[2]) || grepl("\\{\\s*throw\\b", check[1]),
                info = paste(marker, "no throw under the check:", paste(check, collapse = " | ")))
  }
})

# The local archive was a fixed %TEMP%\econetool_deploy.tar.gz, removed only
# after a successful upload, and the git-bash tar exit code was ignored: a
# failed tar left the previous run's archive in place, Test-Path accepted it
# and a stale upload went out (I3).
test_that("deploy-windows.ps1 never uploads a stale local tar archive", {
  code <- code_lines(deploy_file("deploy-windows.ps1"))
  def <- grep('^\\s*\\$tarFile\\s*=\\s*Join-Path \\$env:TEMP "econetool_deploy_\\$TIMESTAMP\\.tar\\.gz"\\s*$', code)
  expect_length(def, 1L)
  try_line <- grep("^\\s*try\\s*\\{\\s*$", code)
  temp_def <- grep("^\\s*\\$tempDir\\s*=\\s*Join-Path \\$env:TEMP", code)
  expect_length(temp_def, 1L)
  deploy_try <- min(try_line[try_line > temp_def])
  expect_true(length(def) == 1L && def < deploy_try,
              info = "$tarFile must be set before the try, so finally never removes $null")

  tar_call <- grep("^\\s*& \\$gitBash -c \\$tarCommand", code)
  expect_length(tar_call, 1L)
  pre_rm <- grep("^\\s*Remove-Item \\$tarFile -Force -ErrorAction SilentlyContinue\\s*$", code)
  expect_true(any(pre_rm < tar_call), info = "remove any old archive before building the new one")
  expect_match(code[tar_call + 1L], "^\\s*if \\(\\$LASTEXITCODE -ne 0\\) \\{ throw \"tar failed")

  fin <- grep("\\}\\s*finally\\s*\\{", code)
  expect_length(fin, 1L)
  fin_body <- code[fin + seq_len(3L)]
  expect_true(any(grepl("^\\s*Remove-Item -Path \\$tarFile -Force -ErrorAction SilentlyContinue\\s*$", fin_body)),
              info = paste(fin_body, collapse = " | "))
})

# data/ (~3-5 GB locally) used to ship unless -SkipData was given (M4). It
# is managed out of band on the server, so shipping it is now opt-in via
# -IncludeData; -SkipData stays accepted as a no-op for old command lines.
test_that("deploy-windows.ps1 leaves data/ out unless -IncludeData is given", {
  code <- code_lines(deploy_file("deploy-windows.ps1"))
  expect_false("data/" %in% script_array(code, "DEPLOY_ITEMS"))
  expect_true(any(grepl("^\\s*\\[switch\\]\\$IncludeData\\b", code)))
  expect_true(any(grepl("^\\s*\\[switch\\]\\$SkipData\\b", code)), info = "-SkipData must still be accepted")
  data_lines <- grep('"data/"', code, value = TRUE, fixed = TRUE)
  expect_gt(length(data_lines), 0L)
  expect_true(all(grepl("$IncludeData", data_lines, fixed = TRUE)),
              info = paste(data_lines, collapse = " | "))
})

test_that("deploy-windows.ps1 Get-FilesToDeploy adds data/ only with -IncludeData", {
  pwsh <- Sys.which("pwsh")
  skip_if(!nzchar(pwsh), "pwsh (PowerShell 7) not on PATH")
  q <- function(x) if (.Platform$OS.type == "windows") shQuote(x, type = "cmd") else shQuote(x)

  # Scratch project with every default item plus data/
  root <- tempfile("deploy_items_")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  for (d in c("R", "www", "examples", "metawebs", "data", "config", "models")) {
    dir.create(file.path(root, d), recursive = TRUE)
  }
  for (f in c("app.R", "run_app.R", "VERSION")) writeLines("x", file.path(root, f))

  runner <- tempfile(fileext = ".ps1")
  on.exit(unlink(runner), add = TRUE)
  writeLines(c(
    "param([string]$Script, [string]$Root, [string]$Mode)",
    "$ast = [System.Management.Automation.Language.Parser]::ParseFile($Script, [ref]$null, [ref]$null)",
    "foreach ($v in '$DEPLOY_ITEMS', '$EXCLUDE_PATTERNS') {",
    "  $a = $ast.Find({ param($n) $n -is [System.Management.Automation.Language.AssignmentStatementAst] -and",
    "    $n.Left.Extent.Text -eq $v }, $true)",
    "  Invoke-Expression $a.Extent.Text",
    "}",
    "$fn = $ast.Find({ param($n) $n -is [System.Management.Automation.Language.FunctionDefinitionAst] -and",
    "  $n.Name -eq 'Get-FilesToDeploy' }, $true)",
    "Invoke-Expression $fn.Extent.Text",
    "function Write-Log { param($Message, $Level) }",
    "$PROJECT_ROOT = $Root",
    "$IncludeData = $Mode -eq 'include'",
    "$SkipData = $Mode -eq 'skip'",
    "(Get-FilesToDeploy | ForEach-Object { $_.RelativePath }) -join ','"
  ), runner)

  items <- function(mode) {
    out <- system2(pwsh, c("-NoProfile", "-NonInteractive", "-File", q(runner),
                           "-Script", q(normalizePath(deploy_file("deploy-windows.ps1"))),
                           "-Root", q(normalizePath(root)), "-Mode", mode),
                   stdout = TRUE, stderr = TRUE)
    strsplit(tail(out, 1L), ",", fixed = TRUE)[[1]]
  }
  default <- items("default")
  expect_true(all(c("app.R", "R/", "config/", "models/") %in% default), info = paste(default, collapse = ","))
  expect_false("data/" %in% default)
  expect_false("data/" %in% items("skip"))
  expect_true("data/" %in% items("include"))
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

# --- deploy.sh (root; F5, F7) ------------------------------------------------

test_that("deploy.sh rsync --delete excludes server state and runtime config", {
  prot <- protected_root_sh(deploy_file("deploy.sh"))
  # config/ itself ships (templates); its runtime files are excluded below
  expect_equal(setdiff(c(".*", "data", "cache", "r-libs", "models"), prot), character(0))
  expect_equal(setdiff(RUNTIME_CONFIG_FILES, prot), character(0))
})

test_that("deploy.sh backups live outside site_dir", {
  path <- deploy_file("deploy.sh")
  expect_equal(backup_dirs(path), "/srv/shiny-server-data/EcoNeTool/backups")
  expect_true(any(grepl("chmod 600", code_lines(path), fixed = TRUE)))
})

# --- no script writes the shared Shiny Server config -------------------------

# TRUE for a code line that writes under /etc/shiny-server/: a copy/move/tee/
# install/link/rsync/truncate/remove/chmod/chown whose command line names
# it (indented or not, with or without sudo), a `>`/`>>` redirect into it,
# or `sed -i` on it. Lines that only print or read it (echo, Write-Host,
# grep, sed -n) are not writes.
writes_etc_shiny <- function(lines) {
  cmd <- "(^\\s*|[;&|(]\\s*|\\bsudo\\s+)(cp|mv|tee|install|ln|rsync|truncate|rm|chmod|chown)\\b[^#]*/etc/shiny-server/"
  grepl(cmd, lines, perl = TRUE) |
    grepl(">>?\\s*[\"']?/etc/shiny-server/", lines, perl = TRUE) |
    grepl("\\bsed\\s+(-[a-zA-Z]*i|--in-place)[^#]*/etc/shiny-server/", lines, perl = TRUE)
}

deploy_scripts <- function() {
  root <- get_app_root()
  rx <- "\\.(sh|ps1|bat|cmd)$"
  c(list.files(root, rx),
    file.path("deployment", list.files(file.path(root, "deployment"), rx)))
}

# deploy-windows.bat was a fourth, unguarded deploy path (I1): it wiped the
# live tree with `sudo rm -rf`, tarred and shipped local data/ and config/,
# and restarted every app on the server. Any .bat/.cmd deploy script may
# only be a stub that points at deploy-windows.ps1 and exits non-zero.
test_that("every .bat/.cmd deploy script is a retired stub that touches nothing", {
  scripts <- grep("(^|/)deploy[^/]*\\.(bat|cmd)$", deploy_scripts(), value = TRUE)
  expect_true("deploy-windows.bat" %in% scripts)
  for (rel in scripts) {
    txt <- readLines(deploy_file(rel), warn = FALSE)
    expect_true(any(grepl("^\\s*exit /b 1\\s*$", txt, ignore.case = TRUE)), info = rel)
    expect_true(any(grepl("deploy-windows.ps1", txt, fixed = TRUE)), info = rel)
    for (bad in c("rm -rf", "scp", "ssh", "tar ", "/etc/shiny-server", "systemctl")) {
      expect_false(any(grepl(bad, tolower(txt), fixed = TRUE)), info = paste(rel, "contains", bad))
    }
  }
})

test_that("writes_etc_shiny() flags writes and ignores prints and reads", {
  writes <- c(
    "cp \"$DEPLOY_DIR/shiny-server.conf\" /etc/shiny-server/shiny-server.conf",
    "    sudo cp x /etc/shiny-server/shiny-server.conf",
    "cat > /etc/shiny-server/shiny-server.conf <<'EOF'",
    "echo x | sudo tee /etc/shiny-server/shiny-server.conf",
    "sudo sed -i 's/on;/off;/' /etc/shiny-server/shiny-server.conf",
    "Invoke-RemoteCommand \"sudo cp /tmp/c /etc/shiny-server/shiny-server.conf\"",
    # F-A regression: indented, non-sudo lines (the original bug's exact
    # shape) were not flagged because the cmd anchor required `cp` at
    # column 0.
    "    cp \"$DEPLOY_DIR/shiny-server.conf\" /etc/shiny-server/shiny-server.conf",
    "    mv x /etc/shiny-server/shiny-server.conf",
    "    install -m 644 x /etc/shiny-server/shiny-server.conf",
    # F-B: rm, chmod, chown targeting /etc/shiny-server/ are also writes
    # (deleting or changing permissions on the shared conf/dir).
    "rm -rf /etc/shiny-server/shiny-server.conf",
    "chmod 644 /etc/shiny-server/shiny-server.conf",
    "chown shiny:shiny /etc/shiny-server/shiny-server.conf"
  )
  reads <- c(
    "echo \"  sudo nano /etc/shiny-server/shiny-server.conf\"",
    "grep -n directory_index /etc/shiny-server/shiny-server.conf",
    "sed -n '/location \\/EcoNeTool {/,/^  }/p' \"$DEPLOY_DIR/shiny-server.conf\"",
    "Write-Host \"edit /etc/shiny-server/shiny-server.conf by hand\""
  )
  expect_equal(writes_etc_shiny(writes), rep(TRUE, length(writes)))
  expect_equal(writes_etc_shiny(reads), rep(FALSE, length(reads)))
})

test_that("no deploy script writes to /etc/shiny-server/ (shared server)", {
  scripts <- deploy_scripts()
  expect_true(all(c("deploy.sh", "deploy-windows.ps1", "deployment/deploy.sh",
                    "deployment/force-reload.sh") %in% scripts))
  for (rel in scripts) {
    code <- code_lines(deploy_file(rel))
    offenders <- code[writes_etc_shiny(code)]
    expect_equal(length(offenders), 0L, info = paste(rel, ":", paste(trimws(offenders), collapse = " | ")))
  }
})

test_that("deployment/deploy.sh prints the EcoNeTool location block for a manual edit", {
  code <- code_lines(deploy_file("deployment/deploy.sh"))
  expect_true(any(grepl("sed -n '/location \\/EcoNeTool {/,/^  }/p' \"$DEPLOY_DIR/shiny-server.conf\"",
                        code, fixed = TRUE)))
  conf <- readLines(deploy_file("deployment/shiny-server.conf"), warn = FALSE)
  start <- grep("location /EcoNeTool {", conf, fixed = TRUE)
  expect_length(start, 1L)
  expect_true(any(conf[start:length(conf)] == "  }"), info = "the sed range needs a closing '  }' line")
})

test_that("deployment/deploy.sh guards the location snippet print so a missing repo conf warns instead of aborting under set -e", {
  code <- code_lines(deploy_file("deployment/deploy.sh"))
  sed_idx <- grep("sed -n '/location \\/EcoNeTool {/,/^  }/p' \"$DEPLOY_DIR/shiny-server.conf\"",
                  code, fixed = TRUE)
  expect_length(sed_idx, 1L)
  before <- code[max(1L, sed_idx - 5L):sed_idx]
  expect_true(any(grepl('if \\[ -f "\\$DEPLOY_DIR/shiny-server\\.conf" \\]', before)),
              info = "sed -n must run only when $DEPLOY_DIR/shiny-server.conf exists")
  after <- code[sed_idx:min(length(code), sed_idx + 5L)]
  expect_true(any(grepl("print_warning", after)),
              info = "the else branch (missing repo conf) must warn, not abort")
})
