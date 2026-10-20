# Conventions guard (deep-analysis fix D2: D2.2, D2.3, D2.7, D2.8).
#
# CLAUDE.md conventions - warning() in error handlers, <<- for outer-scope
# mutation in error closures - were applied file by file and kept regressing.
# These AST checks make them mechanical. Production keeps no message() output
# (preserve_logs is off), so a handler that neither warns nor stops hides the
# failure. Offenders are reported as file:line.
#
# Allow-list a deliberate swallow with a `# swallow-ok: <reason>` comment
# inside the handler, and a deliberate local assignment in a handler with
# `# closure-ok: <reason>` (reason: at least 10 characters).

app_root <- get_app_root()

r_files <- function(dirs) {
  files <- unlist(lapply(dirs, function(d) {
    list.files(file.path(app_root, d), pattern = "[.][Rr]$", recursive = TRUE, full.names = TRUE)
  }))
  sort(unique(files))
}

rel <- function(f) {
  sub(paste0("^", app_root, "/?"), "", normalizePath(f, winslash = "/", mustWork = FALSE))
}

is_call_to <- function(e, name) is.call(e) && is.symbol(e[[1]]) && identical(as.character(e[[1]]), name)

# Function names called anywhere inside `expr`, `pkg::fn` reported as `fn`.
called_functions <- function(expr) {
  out <- character()
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    fn <- e[[1]]
    if (is.symbol(fn)) {
      out <<- c(out, as.character(fn))
    } else if (is.call(fn) && (is_call_to(fn, "::") || is_call_to(fn, ":::"))) {
      out <<- c(out, as.character(fn[[3]]))
    }
    for (a in as.list(e)[-1]) if (!missing(a)) walk(a)
  }
  walk(expr)
  out
}

srcref_text <- function(src, sr) if (is.null(sr)) "" else paste(src[sr[1]:sr[3]], collapse = "\n")

has_marker <- function(txt, tag) grepl(paste0("# ", tag, ": .{10,}"), txt)

parse_with_src <- function(f) {
  exprs <- tryCatch(parse(f, keep.source = TRUE), error = function(e) {
    stop(sprintf("conventions guard cannot parse %s: %s", rel(f), conditionMessage(e)), call. = FALSE)
  })
  list(exprs = exprs, src = readLines(f, warn = FALSE))
}

# Every `tryCatch(..., error = function(e) ...)` handler in a file.
error_handlers <- function(f) {
  p <- parse_with_src(f)
  found <- list()
  visit <- function(e) {
    if (!is.call(e)) return(invisible())
    if (is_call_to(e, "tryCatch") && "error" %in% names(e)) {
      h <- e[["error"]]
      if (is.call(h) && is_call_to(h, "function")) {
        sr <- attr(h, "srcref")
        if (is.null(sr) && length(h) >= 4) sr <- h[[4]]
        found[[length(found) + 1L]] <<- list(handler = h, line = if (is.null(sr)) NA_integer_ else sr[1],
                                             text = srcref_text(p$src, sr))
      }
    }
    for (a in as.list(e)[-1]) if (!missing(a)) visit(a)
  }
  for (e in p$exprs) visit(e)
  found
}

surfacing_calls <- c("warning", "stop", "abort", "warn", "signalCondition", "cli_abort", "cli_warn")

# --------------------------------------------------------------------------
# D2.2 - handlers under R/functions must surface failures or be marked
# --------------------------------------------------------------------------

test_that("every R/functions error handler surfaces the failure or is marked # swallow-ok:", {
  offenders <- character()
  for (f in r_files("R/functions")) {
    for (h in error_handlers(f)) {
      surfaced <- any(called_functions(h$handler[[3]]) %in% surfacing_calls)
      if (!surfaced && !has_marker(h$text, "swallow-ok")) offenders <- c(offenders, sprintf("%s:%d", rel(f), h$line))
    }
  }
  expect(length(offenders) == 0L,
         sprintf("%d error handler(s) neither warn()/stop() nor carry '# swallow-ok: <reason>':\n%s",
                 length(offenders), paste(offenders, collapse = "\n")))
})

# --------------------------------------------------------------------------
# D2.3 - closure assignment direction (F15, F23)
# --------------------------------------------------------------------------

# Base name of an assignment target: result$error -> "result", x[[1]] -> "x".
target_base <- function(lhs) {
  while (is.call(lhs) && length(lhs) >= 2) lhs <- lhs[[2]]  # x$a, x[[i]], names(x), attr(x, "k") ...
  if (is.symbol(lhs)) as.character(lhs) else NA_character_
}

# Assignments in `expr`, not descending into nested function literals.
assignments <- function(expr, ops) {
  out <- character()
  walk <- function(e) {
    if (!is.call(e) || is_call_to(e, "function")) return(invisible())
    if (is.symbol(e[[1]]) && as.character(e[[1]]) %in% ops && length(e) >= 3) out <<- c(out, target_base(e[[2]]))
    for (a in as.list(e)[-1]) if (!missing(a)) walk(a)
  }
  walk(expr)
  out[!is.na(out)]
}

# For each function literal: its frame locals, and every tryCatch inside it
# (not inside nested functions) with its body and error handler.
closure_sites <- function(f) {
  p <- parse_with_src(f)
  sites <- list()
  scan_frame <- function(fun_body) {
    locals <- assignments(fun_body, c("<-", "="))
    visit <- function(e) {
      if (!is.call(e) || is_call_to(e, "function")) return(invisible())
      if (is_call_to(e, "tryCatch")) {
        h <- if ("error" %in% names(e)) e[["error"]] else NULL
        sr <- if (is.call(h)) attr(h, "srcref") %||% (if (length(h) >= 4) h[[4]] else NULL) else NULL
        body_arg <- if (length(e) >= 2 && (is.null(names(e)) || !nzchar(names(e)[2]))) e[[2]] else NULL
        sites[[length(sites) + 1L]] <<- list(locals = locals, body = body_arg, handler = h,
                                             line = if (is.null(sr)) NA_integer_ else sr[1],
                                             text = srcref_text(p$src, sr))
      }
      for (a in as.list(e)[-1]) if (!missing(a)) visit(a)
    }
    visit(fun_body)
  }
  find_functions <- function(e, top = FALSE) {
    if (!is.call(e)) return(invisible())
    if (is_call_to(e, "function")) scan_frame(e[[3]])
    for (a in as.list(e)[-1]) if (!missing(a)) find_functions(a)
  }
  # Top-level script code counts as a frame too (scripts/).
  for (e in p$exprs) find_functions(e)
  scan_frame(as.call(c(as.name("{"), as.list(p$exprs))))
  sites
}

closure_files <- function() c(r_files(c("R", "scripts")), file.path(app_root, "app.R"))

test_that("(a) no <<- in a tryCatch body to a name the enclosing frame assigns with <- (F15)", {
  offenders <- character()
  for (f in closure_files()) {
    for (s in closure_sites(f)) {
      if (is.null(s$body)) next
      hit <- intersect(assignments(s$body, "<<-"), s$locals)
      if (length(hit) > 0) {
        offenders <- c(offenders, sprintf("%s:%s (%s)", rel(f), s$line %||% "?", paste(hit, collapse = ", ")))
      }
    }
  }
  expect(length(offenders) == 0L,
         sprintf("<<- inside a tryCatch body targets a frame local (use <-):\n%s", paste(offenders, collapse = "\n")))
})

test_that("(b) no plain <- in an error handler to an enclosing-frame local that is then lost (F23)", {
  reference_names <- c("output", "rv", "session", "values", "e")
  offenders <- character()
  for (f in closure_files()) {
    for (s in closure_sites(f)) {
      h <- s$handler
      if (!is.call(h) || !is_call_to(h, "function")) next
      body <- h[[3]]
      last <- if (is_call_to(body, "{") && length(body) > 1) body[[length(body)]] else body
      returned <- if (is.symbol(last)) as.character(last) else NA_character_
      hit <- setdiff(intersect(assignments(body, c("<-", "=")), s$locals), c(reference_names, returned))
      if (length(hit) > 0 && !has_marker(s$text, "closure-ok")) {
        offenders <- c(offenders, sprintf("%s:%s (%s)", rel(f), s$line, paste(hit, collapse = ", ")))
      }
    }
  }
  expect(length(offenders) == 0L,
         sprintf("plain <- in an error handler only changes the handler's copy (use <<-):\n%s",
                 paste(offenders, collapse = "\n")))
})

# --------------------------------------------------------------------------
# D2.7 / D2.8 / F39 - named pins
# --------------------------------------------------------------------------

test_that("app.R starts no future plan (F31)", {
  app <- paste(readLines(file.path(app_root, "app.R"), warn = FALSE), collapse = "\n")
  expect_false(grepl("init_parallel_lookup(", app, fixed = TRUE))
  expect_false(grepl("shutdown_parallel_lookup", app, fixed = TRUE))
})

test_that("scripts no longer source the removed monolithic trait_lookup.R (F23)", {
  for (f in r_files("scripts")) {
    expect_false(any(grepl("trait_lookup.R\"", readLines(f, warn = FALSE), fixed = TRUE)), info = rel(f))
  }
})

test_that("the ML model path goes through ml_models_path() only (F39)", {
  # The bare literal may appear once: ml_models_path()'s fallback when
  # app_path() is not loaded. ml_models_available() used to check it directly.
  src <- readLines(file.path(app_root, "R/functions/ml_trait_prediction.R"), warn = FALSE)
  expect_identical(sum(grepl("\"models/trait_ml_models.rds\"", src, fixed = TRUE)), 1L)
  expect_true(any(grepl("ml_models_path <- function()", src, fixed = TRUE)))
})
