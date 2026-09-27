# =============================================================================
# Test helpers: deploy-script guards (F86)
# =============================================================================
# The deploy scripts (deploy.sh, deployment/deploy.sh, deploy-windows.ps1) are
# not executed by the suite, so their tests are source guards. A plain grep
# over the whole file also matches comments, and a guard that passes because
# a *comment* mentions `.Renviron` protects nothing. Every guard therefore
# reads the scripts through code_lines(), which drops comments first.

# Code lines of a sh or ps1 script: full-line `#` comments and trailing
# ` # ...` comments removed, blank lines dropped. A `#` counts as a comment
# only at line start or after whitespace, so `${#arr[@]}` survives. sh
# continuation lines (ending in `\`) are joined into one logical line. The
# scripts use no `<# #>` block comments; heredoc bodies are kept as text.
code_lines <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- lines[!grepl("^\\s*#", lines)]
  lines <- sub("\\s+#.*$", "", lines)
  lines <- lines[nzchar(trimws(lines))]
  out <- character(0)
  buf <- ""
  for (ln in lines) {
    if (grepl("\\\\\\s*$", ln)) {
      buf <- paste0(buf, sub("\\\\\\s*$", " ", ln))
    } else {
      out <- c(out, paste0(buf, ln))
      buf <- ""
    }
  }
  if (nzchar(buf)) out <- c(out, buf)
  out
}

# Quoted items of a sh array `NAME=( ... )` or a ps1 array `$NAME = @( ... )`,
# single- or multi-line, read from code lines only.
script_array <- function(lines, name) {
  start <- grep(sprintf("^\\s*\\$?%s\\s*=\\s*@?\\(", name), lines)
  if (length(start) != 1L) {
    stop(sprintf("array %s: expected 1 definition, found %d", name, length(start)))
  }
  end <- start
  while (!grepl("\\)\\s*$", lines[end])) {
    end <- end + 1L
  }
  body <- paste(lines[start:end], collapse = "\n")
  items <- regmatches(body, gregexpr('"[^"]*"', body))[[1]]
  gsub('"', "", items, fixed = TRUE)
}

# Top-level names each deploy path must never delete or overwrite from the
# local tree: server-only state (.Renviron, data/ ~3.1 GB, cache/ with
# offline_traits.db, r-libs/ with icesSAG, config/ runtime files) plus the
# tracked models/ that a wipe would otherwise lose.
DEPLOY_PROTECTED <- c(".*", "data", "cache", "r-libs", "models", "config")

# Runtime config that exists only on the server (or only on a dev box) and
# must never be uploaded.
RUNTIME_CONFIG_FILES <- c("config/api_keys.R", "config/api_keys.json",
                          "config/harmonization_custom.json", ".Renviron")

# deployment/deploy.sh: names kept by the find-based wipe. PRESERVE_ITEMS
# only counts when it reaches the delete: the loop must turn it into
# FIND_KEEP and the find line must pass "${FIND_KEEP[@]}". Otherwise a
# well-populated but unused array would keep the guard green while the
# find deleted data/, config/, r-libs/, ...
protected_deployment_sh <- function(path) {
  code <- code_lines(path)
  find_line <- grep("find /srv/shiny-server/EcoNeTool .*-exec rm -rf", code, value = TRUE)
  if (length(find_line) != 1L) {
    return(character(0))
  }
  keep <- character(0)
  loop_builds_keep <- any(grepl("for KEEP in \"${PRESERVE_ITEMS[@]}\"", code, fixed = TRUE)) &&
    any(grepl("FIND_KEEP+=(! -name \"$KEEP\")", code, fixed = TRUE))
  if (loop_builds_keep && grepl("\"${FIND_KEEP[@]}\"", find_line, fixed = TRUE)) {
    keep <- script_array(code, "PRESERVE_ITEMS")
  }
  if (grepl("! -name '.*'", find_line, fixed = TRUE)) {
    keep <- c(keep, ".*")
  }
  keep
}

# deploy-windows.ps1: names kept by the find-based wipe of the LIVE tree
# (the non-empty `$preserve = "..."`; staging uses `$preserve = ""`). Counts
# only when a `find ... $preserve ... -exec rm -rf` command uses it.
protected_windows_ps1 <- function(path) {
  code <- code_lines(path)
  line <- grep('^\\s*\\$preserve\\s*=\\s*"!', code, value = TRUE)
  if (length(line) != 1L) {
    return(character(0))
  }
  if (!any(grepl("find\\s.*\\$preserve\\s.*-exec rm -rf", code))) {
    return(character(0))
  }
  names <- regmatches(line, gregexpr("-name\\s+'?[^'\"[:space:]]+'?", line))[[1]]
  gsub("^-name\\s+|'", "", names)
}

# deploy.sh (rsync --delete): excluded paths are neither sent nor deleted on
# the receiver. Normalise "/data/", "cache/*", "r-libs" to bare names.
# Counts only when the patterns reach rsync: the loop must build
# exclude_opts from EXCLUDE_PATTERNS and every rsync_cmd must append it.
protected_root_sh <- function(path) {
  code <- code_lines(path)
  loop_builds_opts <- any(grepl("for pattern in \"${EXCLUDE_PATTERNS[@]}\"", code, fixed = TRUE)) &&
    any(grepl("exclude_opts+=\"--exclude=", code, fixed = TRUE))
  n_rsync <- sum(grepl("rsync_cmd=\"rsync", code, fixed = TRUE))
  n_used <- sum(grepl("rsync_cmd+=\" ${exclude_opts}\"", code, fixed = TRUE))
  if (!loop_builds_opts || n_rsync == 0L || n_used < n_rsync) {
    return(character(0))
  }
  pats <- script_array(code, "EXCLUDE_PATTERNS")
  unique(sub("/\\*?$", "", sub("^/", "", pats)))
}

# Right-hand sides of every BACKUP_DIR assignment (sh and ps1), with the
# shiny-server root and app-name variables expanded.
backup_dirs <- function(path) {
  code <- code_lines(path)
  rhs <- sub('^\\s*\\$?BACKUP_DIR\\s*=\\s*"([^"]*)".*$', "\\1",
             grep('^\\s*\\$?BACKUP_DIR\\s*=\\s*"', code, value = TRUE))
  rhs <- gsub("\\$\\{?SHINY_SERVER_ROOT\\}?", "/srv/shiny-server", rhs)
  gsub("\\$\\{?APP_NAME\\}?", "EcoNeTool", rhs)
}
