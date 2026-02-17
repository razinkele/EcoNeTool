# =============================================================================
# Error Logging and Recovery System
# =============================================================================
#
# This module provides comprehensive error tracking for the trait lookup system.
# Logs all failures to enable debugging, API reliability tracking, and system
# monitoring.
#
# Features:
# - Structured error logging to CSV
# - Session tracking
# - Error analysis and reporting
# - Optional console debugging
#
# Version: 1.4.0 (Phase 6)
# Date: 2025-12-25
# =============================================================================

# Global session ID (initialized on module load)
SESSION_ID <- paste0(
  format(Sys.time(), "%Y%m%d_%H%M%S"),
  "_",
  substr(digest::digest(Sys.time()), 1, 8)
)

# =============================================================================
# ERROR LOGGING
# =============================================================================

#' Log an Error to File
#'
#' Records errors to a CSV log file for later analysis. Creates log directory
#' if it doesn't exist.
#'
#' @param source Character. Source of error (e.g., "FishBase", "ML_prediction")
#' @param species Character. Species name being looked up
#' @param error_message Character. Error message from tryCatch
#' @param error_type Character. Type of error (default: "lookup_failure")
#' @param additional_info List. Additional context (optional)
#'
#' @return NULL (invisible). Side effect: writes to log file
#'
#' @examples
#' tryCatch({
#'   result <- risky_function()
#' }, error = function(e) {
#'   log_error("FishBase", "Gadus morhua", e$message)
#' })
#'
log_error <- function(source,
                     species,
                     error_message,
                     error_type = "lookup_failure",
                     additional_info = NULL) {

  # Create log directory if needed
  log_dir <- "logs"
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  # Create log entry
  log_entry <- data.frame(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    session_id = SESSION_ID,
    source = source,
    species = species,
    error_type = error_type,
    error_message = as.character(error_message),
    user = Sys.info()["user"],
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    stringsAsFactors = FALSE
  )

  # Add additional info if provided
  if (!is.null(additional_info)) {
    for (key in names(additional_info)) {
      log_entry[[key]] <- as.character(additional_info[[key]])
    }
  }

  # Append to log file
  log_file <- file.path(log_dir, "trait_lookup_errors.csv")

  if (file.exists(log_file)) {
    # Append to existing log
    tryCatch({
      write.table(log_entry, log_file,
                 append = TRUE,
                 sep = ",",
                 row.names = FALSE,
                 col.names = FALSE,
                 quote = TRUE)
    }, error = function(e) {
      # If append fails, print to console
      message("[LOG ERROR] Could not write to log file: ", e$message)
    })
  } else {
    # Create new log with headers
    tryCatch({
      write.csv(log_entry, log_file, row.names = FALSE)
    }, error = function(e) {
      message("[LOG ERROR] Could not create log file: ", e$message)
    })
  }

  # Console output if debug mode enabled
  if (getOption("trait_lookup_debug", FALSE)) {
    message(sprintf("[ERROR] %s - %s - %s: %s",
                   format(Sys.time(), "%H:%M:%S"),
                   source,
                   species,
                   error_message))
  }

  invisible(NULL)
}


#' Log a Warning
#'
#' Records warnings (non-fatal issues) to a separate log file.
#'
#' @param source Character. Source of warning
#' @param species Character. Species name
#' @param warning_message Character. Warning message
#' @param warning_type Character. Type of warning (default: "data_quality")
#'
#' @return NULL (invisible)
#'
log_warning <- function(source,
                       species,
                       warning_message,
                       warning_type = "data_quality") {

  log_dir <- "logs"
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  log_entry <- data.frame(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    session_id = SESSION_ID,
    source = source,
    species = species,
    warning_type = warning_type,
    warning_message = as.character(warning_message),
    stringsAsFactors = FALSE
  )

  log_file <- file.path(log_dir, "trait_lookup_warnings.csv")

  if (file.exists(log_file)) {
    write.table(log_entry, log_file, append = TRUE, sep = ",",
               row.names = FALSE, col.names = FALSE, quote = TRUE)
  } else {
    write.csv(log_entry, log_file, row.names = FALSE)
  }

  if (getOption("trait_lookup_debug", FALSE)) {
    message(sprintf("[WARNING] %s - %s: %s", source, species, warning_message))
  }

  invisible(NULL)
}


#' Log Successful Lookup
#'
#' Records successful lookups for performance tracking and usage statistics.
#'
#' @param species Character. Species name
#' @param sources_used Character vector. Data sources that provided data
#' @param lookup_time_ms Numeric. Time taken in milliseconds
#' @param traits_found Integer. Number of traits successfully retrieved
#'
#' @return NULL (invisible)
#'
log_success <- function(species,
                       sources_used,
                       lookup_time_ms,
                       traits_found = 5) {

  log_dir <- "logs"
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  log_entry <- data.frame(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    session_id = SESSION_ID,
    species = species,
    sources = paste(sources_used, collapse = "|"),
    lookup_time_ms = lookup_time_ms,
    traits_found = traits_found,
    stringsAsFactors = FALSE
  )

  log_file <- file.path(log_dir, "trait_lookup_success.csv")

  if (file.exists(log_file)) {
    write.table(log_entry, log_file, append = TRUE, sep = ",",
               row.names = FALSE, col.names = FALSE, quote = TRUE)
  } else {
    write.csv(log_entry, log_file, row.names = FALSE)
  }

  invisible(NULL)
}


# =============================================================================
# ERROR ANALYSIS
# =============================================================================

#' Analyze Error Log
#'
#' Generates summary statistics from error log for debugging and monitoring.
#'
#' @param log_file Character. Path to error log (default: "logs/trait_lookup_errors.csv")
#' @param recent_days Integer. Only analyze errors from last N days (default: all)
#'
#' @return List with error statistics
#'
#' @examples
#' summary <- analyze_error_log()
#' print(summary$by_source)
#' print(summary$most_common_errors)
#'
analyze_error_log <- function(log_file = "logs/trait_lookup_errors.csv",
                              recent_days = NULL) {

  if (!file.exists(log_file)) {
    message("No error log found at: ", log_file)
    return(list(
      total_errors = 0,
      message = "No errors logged"
    ))
  }

  # Read log
  errors <- tryCatch({
    read.csv(log_file, stringsAsFactors = FALSE)
  }, error = function(e) {
    message("Could not read error log: ", e$message)
    return(NULL)
  })

  if (is.null(errors) || nrow(errors) == 0) {
    return(list(total_errors = 0, message = "Empty error log"))
  }

  # Convert timestamp
  errors$timestamp <- as.POSIXct(errors$timestamp)

  # Filter by date if requested
  if (!is.null(recent_days)) {
    cutoff <- Sys.time() - (recent_days * 24 * 3600)
    errors <- errors[errors$timestamp >= cutoff, ]
  }

  # Generate summary
  summary <- list(
    total_errors = nrow(errors),
    date_range = c(min(errors$timestamp), max(errors$timestamp)),

    by_source = sort(table(errors$source), decreasing = TRUE),
    by_error_type = sort(table(errors$error_type), decreasing = TRUE),
    by_date = table(as.Date(errors$timestamp)),
    by_session = table(errors$session_id),

    most_common_errors = head(
      sort(table(errors$error_message), decreasing = TRUE),
      10
    ),

    most_problematic_species = head(
      sort(table(errors$species), decreasing = TRUE),
      10
    ),

    error_rate_by_hour = table(format(errors$timestamp, "%H")),

    recent_errors = if (nrow(errors) > 0) {
      tail(errors[order(errors$timestamp), c("timestamp", "source", "species", "error_message")], 10)
    } else {
      NULL
    }
  )

  return(summary)
}


#' Analyze Success Log
#'
#' Generates performance statistics from success log.
#'
#' @param log_file Character. Path to success log
#' @param recent_days Integer. Only analyze recent N days
#'
#' @return List with performance statistics
#'
analyze_success_log <- function(log_file = "logs/trait_lookup_success.csv",
                               recent_days = NULL) {

  if (!file.exists(log_file)) {
    return(list(total_lookups = 0, message = "No success log found"))
  }

  successes <- read.csv(log_file, stringsAsFactors = FALSE)
  successes$timestamp <- as.POSIXct(successes$timestamp)

  if (!is.null(recent_days)) {
    cutoff <- Sys.time() - (recent_days * 24 * 3600)
    successes <- successes[successes$timestamp >= cutoff, ]
  }

  summary <- list(
    total_lookups = nrow(successes),
    mean_lookup_time_ms = mean(successes$lookup_time_ms, na.rm = TRUE),
    median_lookup_time_ms = median(successes$lookup_time_ms, na.rm = TRUE),
    min_lookup_time_ms = min(successes$lookup_time_ms, na.rm = TRUE),
    max_lookup_time_ms = max(successes$lookup_time_ms, na.rm = TRUE),

    mean_traits_found = mean(successes$traits_found, na.rm = TRUE),

    lookups_per_day = table(as.Date(successes$timestamp)),

    source_usage = {
      all_sources <- unlist(strsplit(successes$sources, "\\|"))
      sort(table(all_sources), decreasing = TRUE)
    },

    fastest_lookups = head(
      successes[order(successes$lookup_time_ms), c("species", "lookup_time_ms", "sources")],
      5
    ),

    slowest_lookups = head(
      successes[order(-successes$lookup_time_ms), c("species", "lookup_time_ms", "sources")],
      5
    )
  )

  return(summary)
}


#' Generate System Health Report
#'
#' Combines error and success logs to create comprehensive health report.
#'
#' @param recent_days Integer. Analyze last N days (default: 7)
#' @param output_file Character. Optional file to save report (default: console)
#'
#' @return List with health metrics
#'
generate_health_report <- function(recent_days = 7, output_file = NULL) {

  errors <- analyze_error_log(recent_days = recent_days)
  successes <- analyze_success_log(recent_days = recent_days)

  total_operations <- errors$total_errors + successes$total_lookups
  success_rate <- if (total_operations > 0) {
    successes$total_lookups / total_operations
  } else {
    NA
  }

  report <- list(
    period = paste("Last", recent_days, "days"),
    generated_at = Sys.time(),

    overall = list(
      total_operations = total_operations,
      successful_lookups = successes$total_lookups,
      failed_lookups = errors$total_errors,
      success_rate = success_rate
    ),

    performance = list(
      mean_lookup_time_ms = successes$mean_lookup_time_ms,
      median_lookup_time_ms = successes$median_lookup_time_ms,
      mean_traits_found = successes$mean_traits_found
    ),

    reliability = list(
      most_reliable_sources = if (length(errors$by_source) > 0) {
        names(head(sort(errors$by_source), 3))
      } else {
        character(0)
      },
      most_used_sources = names(head(successes$source_usage, 5))
    ),

    issues = list(
      top_error_types = names(head(errors$by_error_type, 3)),
      problematic_species = names(head(errors$most_problematic_species, 5)),
      recent_errors = errors$recent_errors
    ),

    recommendations = generate_recommendations(errors, successes)
  )

  # Print to console
  print_health_report(report)

  # Save to file if requested
  if (!is.null(output_file)) {
    saveRDS(report, output_file)
    message("Health report saved to: ", output_file)
  }

  invisible(report)
}


#' Generate Recommendations
#'
#' @keywords internal
generate_recommendations <- function(errors, successes) {
  recommendations <- character()

  # Check success rate
  total <- errors$total_errors + successes$total_lookups
  if (total > 0) {
    success_rate <- successes$total_lookups / total
    if (success_rate < 0.9) {
      recommendations <- c(recommendations,
        paste0("Success rate is low (", round(success_rate * 100, 1),
              "%). Consider investigating top error sources."))
    }
  }

  # Check performance
  if (!is.null(successes$mean_lookup_time_ms) &&
      successes$mean_lookup_time_ms > 1000) {
    recommendations <- c(recommendations,
      paste0("Average lookup time is high (",
            round(successes$mean_lookup_time_ms, 0),
            "ms). Consider enabling cache or parallel queries."))
  }

  # Check error concentration
  if (length(errors$by_source) > 0 && max(errors$by_source) > 10) {
    top_source <- names(errors$by_source)[1]
    recommendations <- c(recommendations,
      paste0("Source '", top_source, "' has many errors (",
            errors$by_source[1], "). Check API status or credentials."))
  }

  if (length(recommendations) == 0) {
    recommendations <- "System operating normally. No issues detected."
  }

  return(recommendations)
}


#' Print Health Report to Console
#'
#' @keywords internal
print_health_report <- function(report) {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("TRAIT LOOKUP SYSTEM HEALTH REPORT\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("Period:", report$period, "\n")
  cat("Generated:", format(report$generated_at, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("\n")

  cat("OVERALL STATISTICS\n")
  cat("───────────────────────────────────────────────────────────────\n")
  cat(sprintf("Total operations:    %d\n", report$overall$total_operations))
  cat(sprintf("Successful lookups:  %d (%.1f%%)\n",
             report$overall$successful_lookups,
             report$overall$success_rate * 100))
  cat(sprintf("Failed lookups:      %d (%.1f%%)\n",
             report$overall$failed_lookups,
             (1 - report$overall$success_rate) * 100))
  cat("\n")

  cat("PERFORMANCE\n")
  cat("───────────────────────────────────────────────────────────────\n")
  cat(sprintf("Mean lookup time:    %.0f ms\n", report$performance$mean_lookup_time_ms))
  cat(sprintf("Median lookup time:  %.0f ms\n", report$performance$median_lookup_time_ms))
  cat(sprintf("Mean traits found:   %.1f / 5\n", report$performance$mean_traits_found))
  cat("\n")

  cat("RELIABILITY\n")
  cat("───────────────────────────────────────────────────────────────\n")
  cat("Most used sources:\n")
  for (i in seq_along(report$reliability$most_used_sources)) {
    cat(sprintf("  %d. %s\n", i, report$reliability$most_used_sources[i]))
  }
  cat("\n")

  if (length(report$issues$top_error_types) > 0) {
    cat("TOP ISSUES\n")
    cat("───────────────────────────────────────────────────────────────\n")
    for (i in seq_along(report$issues$top_error_types)) {
      cat(sprintf("  %d. %s\n", i, report$issues$top_error_types[i]))
    }
    cat("\n")
  }

  cat("RECOMMENDATIONS\n")
  cat("───────────────────────────────────────────────────────────────\n")
  for (rec in report$recommendations) {
    cat("• ", rec, "\n")
  }
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("\n")
}


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Enable Debug Mode
#'
#' Turns on console logging for all errors and warnings.
#'
#' @param enabled Logical. TRUE to enable, FALSE to disable
#'
#' @examples
#' enable_debug_logging(TRUE)
#' # ... errors now printed to console
#' enable_debug_logging(FALSE)
#'
enable_debug_logging <- function(enabled = TRUE) {
  options(trait_lookup_debug = enabled)
  if (enabled) {
    message("Debug logging enabled")
  } else {
    message("Debug logging disabled")
  }
}


#' Clear Log Files
#'
#' Removes all log files. Use with caution!
#'
#' @param confirm Logical. Must be TRUE to actually clear logs
#'
clear_logs <- function(confirm = FALSE) {
  if (!confirm) {
    message("Set confirm=TRUE to actually clear logs")
    return(invisible(FALSE))
  }

  log_files <- c(
    "logs/trait_lookup_errors.csv",
    "logs/trait_lookup_warnings.csv",
    "logs/trait_lookup_success.csv"
  )

  removed <- 0
  for (file in log_files) {
    if (file.exists(file)) {
      unlink(file)
      removed <- removed + 1
    }
  }

  message("Removed ", removed, " log files")
  invisible(TRUE)
}


# =============================================================================
# EXPORT
# =============================================================================

# Main functions exported for use in other modules:
# - log_error()
# - log_warning()
# - log_success()
# - analyze_error_log()
# - analyze_success_log()
# - generate_health_report()
# - enable_debug_logging()
