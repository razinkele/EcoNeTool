#' Validation Utilities
#'
#' Shared validation functions to reduce code duplication across the codebase.
#' These utilities provide consistent error messages and validation patterns.
#'
#' @name validation_utils
NULL

# =============================================================================
# NULL COALESCING OPERATOR (UNIFIED)
# =============================================================================

#' Null/NA Coalescing Operator
#'
#' Returns y if x is NULL or a single NA value, otherwise returns x.
#' This is the single canonical definition used across the entire codebase.
#'
#' @param x Value to check
#' @param y Default value to return if x is NULL or NA
#' @return x if not NULL/NA, otherwise y
#' @export
#' @examples
#' NULL %||% "default"     # Returns "default"
#' NA %||% "default"       # Returns "default"
#' "value" %||% "default"  # Returns "value"
#' c(NA, 1) %||% "default" # Returns c(NA, 1) - only single NA triggers default
`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}

# =============================================================================
# TIMEOUT EXECUTION (UNIFIED)
# =============================================================================

#' Execute Expression with Timeout
#'
#' Executes an expression with a timeout limit. This is the single canonical
#' implementation used across the entire codebase.
#'
#' @param expr Expression to evaluate
#' @param timeout Timeout in seconds (default: 10)
#' @param on_timeout Value to return on timeout (default: NULL)
#' @param verbose Logical, whether to print timeout messages (default: FALSE)
#' @return Result of expr, or on_timeout value if timeout occurs
#' @export
#' @examples
#' \dontrun{
#' # Simple usage
#' result <- with_timeout(slow_function(), timeout = 5)
#'
#' # With custom timeout value and verbose output
#' result <- with_timeout(api_call(), timeout = 30, on_timeout = list(), verbose = TRUE)
#' }
with_timeout <- function(expr, timeout = 10, on_timeout = NULL, verbose = FALSE) {
  tryCatch({
    # Set time limits
    setTimeLimit(cpu = timeout, elapsed = timeout, transient = TRUE)
    on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE), add = TRUE)

    # Execute expression
    result <- expr
    return(result)

  }, error = function(e) {
    # Reset time limits
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)

    # Check if it's a timeout error
    if (grepl("time limit|timeout|elapsed", e$message, ignore.case = TRUE)) {
      if (verbose) {
        message(sprintf("Timeout after %ds", timeout))
      }
      return(on_timeout)
    }

    # Re-throw non-timeout errors
    stop(e)
  })
}

# =============================================================================
# NULL/NA HANDLING UTILITIES
# =============================================================================

#' Check if Value is Valid (not NULL and not NA)
#'
#' Returns TRUE if the value is not NULL and not NA (for scalars) or
#' has at least one non-NA value (for vectors/lists).
#'
#' @param x Value to check
#' @return Logical indicating if value is usable
#' @export
#' @examples
#' is_valid_value(NULL)        # FALSE
#' is_valid_value(NA)          # FALSE
#' is_valid_value("value")     # TRUE
#' is_valid_value(c(NA, 1))    # TRUE (has at least one non-NA)
is_valid_value <- function(x) {
  if (is.null(x)) return(FALSE)
  if (length(x) == 0) return(FALSE)
  if (length(x) == 1 && is.na(x)) return(FALSE)
  # For vectors, return TRUE if at least one non-NA value
  if (length(x) > 1) return(any(!is.na(x)))
  return(TRUE)
}

#' Safely Get Value with Default
#'
#' Extracts a value from a list/data.frame column, returning default if
#' the value is NULL, NA, or the column doesn't exist.
#'
#' @param data List or data.frame to extract from
#' @param key Column/element name
#' @param default Default value if extraction fails (default: NULL)
#' @return Extracted value or default
#' @export
#' @examples
#' safe_get(list(a = 1, b = NA), "a")        # Returns 1
#' safe_get(list(a = 1, b = NA), "b", 0)     # Returns 0 (b is NA)
#' safe_get(list(a = 1), "c", "default")     # Returns "default"
safe_get <- function(data, key, default = NULL) {
  if (is.null(data)) return(default)

  value <- tryCatch({
    if (is.data.frame(data)) {
      if (key %in% colnames(data)) data[[key]][1] else NULL
    } else if (is.list(data)) {
      data[[key]]
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (is_valid_value(value)) value else default
}

#' Coalesce Multiple Values
#'
#' Returns the first non-NULL, non-NA value from the arguments.
#' Similar to SQL COALESCE or dplyr::coalesce.
#'
#' @param ... Values to check in order
#' @return First valid value, or NULL if none found
#' @export
#' @examples
#' coalesce_values(NULL, NA, "value", "other")  # Returns "value"
#' coalesce_values(NA, NA, NA)                   # Returns NULL
coalesce_values <- function(...) {
  args <- list(...)
  for (arg in args) {
    if (is_valid_value(arg)) return(arg)
  }
  return(NULL)
}

#' Validate Required Package
#'
#' Checks if a package is installed and stops with helpful error message if not.
#'
#' @param pkg_name Character string with package name
#' @param purpose Optional character string describing what the package is needed for
#' @return TRUE invisibly if package is available
#' @export
#' @examples
#' \dontrun{
#' validate_package("sf", "spatial analysis")
#' validate_package("igraph")
#' }
validate_package <- function(pkg_name, purpose = NULL) {
  if (!requireNamespace(pkg_name, quietly = TRUE)) {
    if (!is.null(purpose)) {
      stop(sprintf(
        "Package '%s' is required for %s. Please install it with: install.packages('%s')",
        pkg_name, purpose, pkg_name
      ), call. = FALSE)
    } else {
      stop(sprintf(
        "Package '%s' is required. Please install it with: install.packages('%s')",
        pkg_name, pkg_name
      ), call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Validate Multiple Required Packages
#'
#' Checks if multiple packages are installed and stops with helpful error message if any are missing.
#'
#' @param pkg_names Character vector of package names
#' @param purpose Optional character string describing what the packages are needed for
#' @return TRUE invisibly if all packages are available
#' @export
#' @examples
#' \dontrun{
#' validate_packages(c("httr", "jsonlite"), "API calls")
#' }
validate_packages <- function(pkg_names, purpose = NULL) {
  missing_pkgs <- pkg_names[!sapply(pkg_names, requireNamespace, quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    pkg_list <- paste0("'", missing_pkgs, "'", collapse = ", ")
    install_cmd <- sprintf("install.packages(c(%s))", pkg_list)

    if (!is.null(purpose)) {
      stop(sprintf(
        "The following packages are required for %s: %s\nInstall them with: %s",
        purpose, pkg_list, install_cmd
      ), call. = FALSE)
    } else {
      stop(sprintf(
        "The following packages are required: %s\nInstall them with: %s",
        pkg_list, install_cmd
      ), call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Validate Required Parameter
#'
#' Checks if a parameter is provided and not NULL.
#'
#' @param param The parameter value to check
#' @param param_name Character string with parameter name (for error message)
#' @param allow_na Logical, whether NA values are allowed (default: FALSE)
#' @return TRUE invisibly if parameter is valid
#' @export
#' @examples
#' \dontrun{
#' validate_parameter(net, "net")
#' validate_parameter(value, "value", allow_na = TRUE)
#' }
validate_parameter <- function(param, param_name, allow_na = FALSE) {
  # Check if missing (using substitute to detect missing in parent frame)
  if (substitute(missing(param), parent.frame()) || is.null(param)) {
    stop(sprintf(
      "Parameter '%s' is required and cannot be NULL",
      param_name
    ), call. = FALSE)
  }

  # Check for NA if not allowed
  if (!allow_na && any(is.na(param))) {
    stop(sprintf(
      "Parameter '%s' contains NA values which are not allowed",
      param_name
    ), call. = FALSE)
  }

  invisible(TRUE)
}

#' Validate Data Frame with Required Columns
#'
#' Checks if a data frame has all required columns.
#'
#' @param df Data frame to validate
#' @param required_cols Character vector of required column names
#' @param df_name Optional name of data frame for error message (default: "Data frame")
#' @return TRUE invisibly if all columns are present
#' @export
#' @examples
#' \dontrun{
#' validate_dataframe(info, c("species", "bodymass", "metabolic_type"), "info")
#' }
validate_dataframe <- function(df, required_cols, df_name = "Data frame") {
  if (!is.data.frame(df)) {
    stop(sprintf("%s must be a data frame", df_name), call. = FALSE)
  }

  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    col_list <- paste0("'", missing_cols, "'", collapse = ", ")
    stop(sprintf(
      "%s is missing required columns: %s",
      df_name, col_list
    ), call. = FALSE)
  }

  invisible(TRUE)
}

#' Validate igraph Network Object
#'
#' Checks if an object is a valid igraph network.
#'
#' @param net Network object to validate
#' @param require_directed Logical, whether network must be directed (default: TRUE)
#' @param min_vertices Integer, minimum number of vertices required (default: 1)
#' @return TRUE invisibly if network is valid
#' @export
#' @examples
#' \dontrun{
#' validate_network(net, require_directed = TRUE, min_vertices = 2)
#' }
validate_network <- function(net, require_directed = TRUE, min_vertices = 1) {
  validate_package("igraph", "network operations")

  if (!igraph::is_igraph(net)) {
    stop("Object must be an igraph network", call. = FALSE)
  }

  if (require_directed && !igraph::is_directed(net)) {
    stop("Network must be directed", call. = FALSE)
  }

  n_vertices <- igraph::vcount(net)
  if (n_vertices < min_vertices) {
    stop(sprintf(
      "Network must have at least %d vertices (has %d)",
      min_vertices, n_vertices
    ), call. = FALSE)
  }

  invisible(TRUE)
}

#' Validate Bounding Box
#'
#' Checks if a bounding box has valid coordinates.
#'
#' @param xmin Minimum x coordinate (longitude)
#' @param ymin Minimum y coordinate (latitude)
#' @param xmax Maximum x coordinate (longitude)
#' @param ymax Maximum y coordinate (latitude)
#' @return TRUE invisibly if bbox is valid
#' @export
#' @examples
#' \dontrun{
#' validate_bbox(10, 54, 25, 66)
#' }
validate_bbox <- function(xmin, ymin, xmax, ymax) {
  # Check for NA values
  if (anyNA(c(xmin, ymin, xmax, ymax))) {
    stop("Invalid bbox: contains NA values", call. = FALSE)
  }

  # Check for numeric values
  if (!all(is.numeric(c(xmin, ymin, xmax, ymax)))) {
    stop("Invalid bbox: all coordinates must be numeric", call. = FALSE)
  }

  # Check min < max
  if (xmin >= xmax) {
    stop(sprintf(
      "Invalid bbox: xmin (%.4f) must be < xmax (%.4f)",
      xmin, xmax
    ), call. = FALSE)
  }

  if (ymin >= ymax) {
    stop(sprintf(
      "Invalid bbox: ymin (%.4f) must be < ymax (%.4f)",
      ymin, ymax
    ), call. = FALSE)
  }

  # Check for reasonable latitude values
  if (ymin < -90 || ymax > 90) {
    stop(sprintf(
      "Invalid bbox: latitude values must be between -90 and 90 (got ymin=%.4f, ymax=%.4f)",
      ymin, ymax
    ), call. = FALSE)
  }

  # Check for reasonable longitude values
  if (xmin < -180 || xmax > 180) {
    stop(sprintf(
      "Invalid bbox: longitude values must be between -180 and 180 (got xmin=%.4f, xmax=%.4f)",
      xmin, xmax
    ), call. = FALSE)
  }

  invisible(TRUE)
}

#' Validate Numeric Range
#'
#' Checks if a numeric value is within a specified range.
#'
#' @param value Numeric value to check
#' @param param_name Character string with parameter name
#' @param min Minimum allowed value (inclusive)
#' @param max Maximum allowed value (inclusive)
#' @param allow_na Logical, whether NA is allowed (default: FALSE)
#' @return TRUE invisibly if value is valid
#' @export
#' @examples
#' \dontrun{
#' validate_numeric_range(cell_size, "cell_size", min = 0.001, max = 10)
#' }
validate_numeric_range <- function(value, param_name, min = -Inf, max = Inf, allow_na = FALSE) {
  # Check for NA
  if (is.na(value)) {
    if (!allow_na) {
      stop(sprintf("Parameter '%s' cannot be NA", param_name), call. = FALSE)
    }
    return(invisible(TRUE))
  }

  # Check numeric
  if (!is.numeric(value) || length(value) != 1) {
    stop(sprintf(
      "Parameter '%s' must be a single numeric value",
      param_name
    ), call. = FALSE)
  }

  # Check range
  if (value < min || value > max) {
    if (is.finite(min) && is.finite(max)) {
      stop(sprintf(
        "Parameter '%s' must be between %s and %s (got %s)",
        param_name, min, max, value
      ), call. = FALSE)
    } else if (is.finite(min)) {
      stop(sprintf(
        "Parameter '%s' must be >= %s (got %s)",
        param_name, min, value
      ), call. = FALSE)
    } else if (is.finite(max)) {
      stop(sprintf(
        "Parameter '%s' must be <= %s (got %s)",
        param_name, max, value
      ), call. = FALSE)
    }
  }

  invisible(TRUE)
}

#' Validate File Exists
#'
#' Checks if a file exists and is readable.
#'
#' @param file_path Character string with file path
#' @param param_name Optional parameter name for error message
#' @return TRUE invisibly if file exists
#' @export
#' @examples
#' \dontrun{
#' validate_file_exists("data/network.RData", "data_file")
#' }
validate_file_exists <- function(file_path, param_name = "file") {
  if (!file.exists(file_path)) {
    stop(sprintf(
      "File does not exist: %s",
      file_path
    ), call. = FALSE)
  }

  # Check if readable
  if (file.access(file_path, mode = 4) != 0) {
    stop(sprintf(
      "File exists but is not readable: %s",
      file_path
    ), call. = FALSE)
  }

  invisible(TRUE)
}
