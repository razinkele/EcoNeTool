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
  fp <- gsub("\\\\", "/", filepath)

  for (domain in names(DOMAIN_MAP)) {
    for (pattern in DOMAIN_MAP[[domain]]) {
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

  files <- files[!grepl("(safeBackup|load_all\\.R)", files)]
  gsub("\\\\", "/", files)
}

parse_roxygen_block <- function(lines, block_end_line) {
  start <- block_end_line
  while (start > 1 && grepl("^#'", lines[start - 1])) {
    start <- start - 1
  }

  block_lines <- lines[start:block_end_line]
  stripped <- sub("^#'\\s?", "", block_lines)

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
      flush_tag()
      current_tag <- tag_match[2]
      current_value <- if (tag_match[3] != "") tag_match[3] else character(0)
    } else {
      if (s == "" && current_tag == "title" && length(current_value) > 0) {
        flush_tag()
        current_tag <- "description"
        current_value <- character(0)
      } else {
        current_value <- c(current_value, s)
      }
    }
  }
  flush_tag()

  if (result$title == "" && length(stripped) > 0) {
    result$title <- trimws(stripped[1])
  }

  result
}

extract_function_signature <- function(lines, after_line) {
  for (i in (after_line + 1):min(after_line + 5, length(lines))) {
    if (grepl("<-\\s*function\\s*\\(", lines[i])) {
      name_match <- regmatches(lines[i], regexec("^([a-zA-Z_.][a-zA-Z0-9_.]*)", lines[i]))[[1]]
      func_name <- if (length(name_match) > 0) name_match[2] else ""

      sig_lines <- lines[i]
      j <- i
      while (!grepl("\\)", sig_lines) && j < min(i + 20, length(lines))) {
        j <- j + 1
        sig_lines <- paste(sig_lines, trimws(lines[j]))
      }
      params_match <- regmatches(sig_lines, regexec("function\\s*\\((.*)\\)", sig_lines))[[1]]
      params_str <- if (length(params_match) > 0) trimws(params_match[2]) else ""

      return(list(name = func_name, params_str = params_str, line = i))
    }
  }
  NULL
}

parse_function_params <- function(params_str) {
  if (params_str == "") return(list())
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

    in_block <- FALSE
    block_end <- NULL

    for (i in seq_along(lines)) {
      is_roxygen <- grepl("^#'", lines[i])

      if (is_roxygen) {
        in_block <- TRUE
        block_end <- i
      } else if (in_block) {
        roxygen <- parse_roxygen_block(lines, block_end)
        sig <- extract_function_signature(lines, block_end)

        if (!is.null(sig) && sig$name != "") {
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

  if (length(rox$params) > 0 || length(func$sig_params) > 0) {
    lines <- c(lines, "**Parameters:**", "")
    lines <- c(lines, "| Name | Default | Description |")
    lines <- c(lines, "|------|---------|-------------|")

    defaults <- list()
    for (sp in func$sig_params) {
      defaults[[sp$name]] <- if (!is.null(sp$default)) sp$default else "\u2014"
    }

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

  lines <- c(lines, sprintf("**Source:** `%s:%d`", func$file, func$line))
  lines <- c(lines, "")

  paste(lines, collapse = "\n")
}

read_version_for_api <- function(path = "VERSION") {
  if (!file.exists(path)) return("unknown")
  lines <- readLines(path, warn = FALSE)
  for (line in lines) {
    if (grepl("^VERSION=", line)) {
      return(trimws(sub("^VERSION=", "", line)))
    }
  }
  "unknown"
}

generate_api_reference <- function(opts = parse_api_args()) {
  cat("Generating API Reference...\n")

  all_funcs <- parse_all_files()
  cat(sprintf("Found %d documented functions across %d files\n",
    length(all_funcs),
    length(unique(vapply(all_funcs, function(f) f$file, character(1))))))

  version <- read_version_for_api()

  by_domain <- list()
  for (func in all_funcs) {
    d <- func$domain
    by_domain[[d]] <- c(by_domain[[d]], list(func))
  }

  domain_order <- c(
    "Network Analysis", "Data Import/Export", "Trait Lookup", "Trait Imputation",
    "Spatial Analysis", "Taxonomic APIs", "Shiny Modules", "Configuration",
    "Utilities", "Other"
  )

  lines <- character(0)

  lines <- c(lines,
    "# EcoNeTool API Reference",
    "",
    "> Auto-generated from source code \u2014 do not edit manually.",
    sprintf("> Generated: %s | Version: %s", format(Sys.Date(), "%Y-%m-%d"), version),
    "> Regenerate: `Rscript scripts/generate_api_reference.R`",
    ""
  )

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

  for (domain in domain_order) {
    funcs <- by_domain[[domain]]
    if (is.null(funcs)) next

    lines <- c(lines, sprintf("## %s", domain), "")

    func_names <- vapply(funcs, function(f) f$name, character(1))
    funcs <- funcs[order(func_names)]

    for (func in funcs) {
      lines <- c(lines, format_function_entry(func))
    }

    lines <- c(lines, "---", "")
  }

  content <- paste(lines, collapse = "\n")

  out_dir <- dirname(opts$output)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  writeLines(content, opts$output)
  cat(sprintf("API Reference written to %s (%d functions in %d domains)\n",
    opts$output, length(all_funcs), length(by_domain)))
  invisible(opts$output)
}

if (identical(environment(), globalenv())) {
  generate_api_reference()
}
