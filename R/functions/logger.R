# =============================================================================
# General Application Logger
# =============================================================================
#
# Simple, configurable logging for debug output throughout the application.
# Use this for general debugging - for trait lookup specific logging,
# see error_logging.R instead.
#
# Configuration:
#   Set environment variable ECO_NT_DEBUG=true to enable debug output
#   Or call: Sys.setenv(ECO_NT_DEBUG = "true")
#
# Usage:
#   log_debug("Component", "Processing item:", item_name)
#   log_info("Startup", "Application initialized")
#   log_warn("Config", "Using default value for missing setting")
#
# Version: 1.1.2
# =============================================================================

#' Check if Debug Mode is Enabled
#'
#' @return Logical. TRUE if ECO_NT_DEBUG environment variable is "true"
#' @keywords internal
is_debug_enabled <- function() {
  debug_env <- Sys.getenv("ECO_NT_DEBUG", "false")
  tolower(debug_env) %in% c("true", "1", "yes")
}

#' Log Debug Message
#'
#' Outputs debug messages only when ECO_NT_DEBUG=true.
#' Use for detailed diagnostic information during development.
#'
#' @param component Character. Component/module name (e.g., "Spatial", "Rpath")
#' @param ... Additional message parts (concatenated with spaces)
#'
#' @examples
#' log_debug("Spatial", "Loading BBT polygon:", bbt_name)
#' log_debug("Rpath", "Converting", n_groups, "groups")
#'
log_debug <- function(component, ...) {
  if (is_debug_enabled()) {
    msg <- paste(...)
    timestamp <- format(Sys.time(), "%H:%M:%S")
    message(sprintf("[%s] [DEBUG] [%s] %s", timestamp, component, msg))
  }
  invisible(NULL)
}

#' Log Info Message
#'
#' Outputs informational messages. Always shown (not gated by debug flag).
#' Use for important status updates the user should see.
#'
#' @param component Character. Component/module name
#' @param ... Additional message parts
#'
#' @examples
#' log_info("App", "Application started successfully")
#'
log_info <- function(component, ...) {
  msg <- paste(...)
  timestamp <- format(Sys.time(), "%H:%M:%S")
  message(sprintf("[%s] [INFO] [%s] %s", timestamp, component, msg))
  invisible(NULL)
}

#' Log Warning Message
#'
#' Outputs warning messages for non-fatal issues.
#' Always shown (not gated by debug flag).
#'
#' @param component Character. Component/module name
#' @param ... Additional message parts
#'
#' @examples
#' log_warn("Config", "Missing config file, using defaults")
#'
log_warn <- function(component, ...) {
  msg <- paste(...)
  timestamp <- format(Sys.time(), "%H:%M:%S")
  message(sprintf("[%s] [WARN] [%s] %s", timestamp, component, msg))
  invisible(NULL)
}

#' Log Detailed Debug Information
#'
#' Outputs multi-line debug information (only when debug enabled).
#' Useful for logging object properties, CRS info, etc.
#'
#' @param component Character. Component/module name
#' @param title Character. Title for the debug block
#' @param ... Named arguments to display as key: value pairs
#'
#' @examples
#' log_debug_details("Spatial", "Loaded polygon",
#'   name = bbt_name,
#'   crs = sf::st_crs(polygon)$input,
#'   rows = nrow(polygon)
#' )
#'
log_debug_details <- function(component, title, ...) {
  if (is_debug_enabled()) {
    timestamp <- format(Sys.time(), "%H:%M:%S")
    message(sprintf("[%s] [DEBUG] [%s] %s", timestamp, component, title))

    details <- list(...)
    if (length(details) > 0) {
      for (name in names(details)) {
        value <- details[[name]]
        message(sprintf("         %s: %s", name, as.character(value)))
      }
    }
  }
  invisible(NULL)
}

#' Enable Debug Logging at Runtime
#'
#' Convenience function to enable/disable debug logging.
#'
#' @param enabled Logical. TRUE to enable, FALSE to disable
#'
#' @examples
#' set_debug_mode(TRUE)  # Enable debug output
#' set_debug_mode(FALSE) # Disable debug output
#'
set_debug_mode <- function(enabled = TRUE) {
  if (enabled) {
    Sys.setenv(ECO_NT_DEBUG = "true")
    message("Debug logging enabled (ECO_NT_DEBUG=true)")
  } else {
    Sys.setenv(ECO_NT_DEBUG = "false")
    message("Debug logging disabled")
  }
  invisible(NULL)
}
