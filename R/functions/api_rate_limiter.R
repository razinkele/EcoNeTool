# =============================================================================
# API Rate Limiter
# =============================================================================
#
# This module provides rate limiting for external API calls to prevent
# hitting API limits and getting banned during batch processing.
#
# Features:
# - Token bucket algorithm for smooth rate limiting
# - Per-API rate limit configuration
# - Automatic retry with exponential backoff
# - Request tracking and statistics
#
# Version: 1.4.0 (Phase 6)
# Date: 2025-12-25
# =============================================================================

# Load R6 for OOP (install if needed)
if (!requireNamespace("R6", quietly = TRUE)) {
  install.packages("R6")
}
library(R6)


# =============================================================================
# RATE LIMITER CLASS
# =============================================================================

#' RateLimiter Class
#'
#' Implements token bucket rate limiting algorithm. Each API call consumes
#' one token. Tokens regenerate over time. If no tokens available, waits.
#'
#' @field max_requests Maximum requests allowed in time window
#' @field time_window Time window in seconds
#' @field requests Vector of request timestamps
#' @field total_requests Total requests made (lifetime)
#' @field total_waits Total waits performed
#'
#' @examples
#' limiter <- RateLimiter$new(max_requests = 100, time_window = 3600)
#' limiter$acquire()  # Blocks if rate limit exceeded
#' # ... make API call
#'
RateLimiter <- R6Class("RateLimiter",
  public = list(
    max_requests = NULL,
    time_window = NULL,
    requests = NULL,
    total_requests = 0,
    total_waits = 0,
    api_name = NULL,

    #' Initialize Rate Limiter
    #'
    #' @param max_requests Integer. Max requests per time window
    #' @param time_window Numeric. Time window in seconds (default: 3600 = 1 hour)
    #' @param api_name Character. Name of API for logging
    #'
    initialize = function(max_requests = 100,
                         time_window = 3600,
                         api_name = "API") {
      self$max_requests <- max_requests
      self$time_window <- time_window
      self$api_name <- api_name
      self$requests <- c()
      self$total_requests <- 0
      self$total_waits <- 0

      message(sprintf("[%s] Rate limiter initialized: %d requests / %.0f seconds",
                     api_name, max_requests, time_window))
    },

    #' Acquire Permission to Make API Call
    #'
    #' Blocks until a request can be made without exceeding rate limit.
    #'
    #' @return TRUE when permission granted
    #'
    acquire = function() {
      now <- Sys.time()

      # Remove expired requests (outside time window)
      cutoff <- now - self$time_window
      self$requests <- self$requests[self$requests > cutoff]

      # Check if we can make request
      if (length(self$requests) >= self$max_requests) {
        # Calculate wait time
        oldest_request <- min(self$requests)
        wait_time <- as.numeric(oldest_request + self$time_window - now, units = "secs")

        if (wait_time > 0) {
          self$total_waits <- self$total_waits + 1

          message(sprintf("[%s] Rate limit reached (%d/%d). Waiting %.1f seconds...",
                         self$api_name,
                         length(self$requests),
                         self$max_requests,
                         wait_time))

          Sys.sleep(wait_time + 0.1)  # Add small buffer

          # Recursive call after waiting
          return(self$acquire())
        }
      }

      # Record request
      self$requests <- c(self$requests, now)
      self$total_requests <- self$total_requests + 1

      return(TRUE)
    },

    #' Get Current Request Count
    #'
    #' @return Integer. Number of requests in current time window
    #'
    get_current_count = function() {
      now <- Sys.time()
      cutoff <- now - self$time_window
      self$requests <- self$requests[self$requests > cutoff]
      return(length(self$requests))
    },

    #' Get Remaining Requests
    #'
    #' @return Integer. Number of requests available before limit
    #'
    get_remaining = function() {
      return(self$max_requests - self$get_current_count())
    },

    #' Get Statistics
    #'
    #' @return List with rate limiter statistics
    #'
    get_stats = function() {
      return(list(
        api_name = self$api_name,
        max_requests = self$max_requests,
        time_window = self$time_window,
        current_count = self$get_current_count(),
        remaining = self$get_remaining(),
        total_requests = self$total_requests,
        total_waits = self$total_waits,
        utilization = self$get_current_count() / self$max_requests
      ))
    },

    #' Print Status
    #'
    print_status = function() {
      stats <- self$get_stats()
      cat(sprintf("\n[%s Rate Limiter]\n", stats$api_name))
      cat(sprintf("  Limit: %d requests / %.0f seconds\n",
                 stats$max_requests, stats$time_window))
      cat(sprintf("  Current: %d/%d (%.1f%% used)\n",
                 stats$current_count, stats$max_requests,
                 stats$utilization * 100))
      cat(sprintf("  Remaining: %d\n", stats$remaining))
      cat(sprintf("  Lifetime requests: %d\n", stats$total_requests))
      cat(sprintf("  Times waited: %d\n", stats$total_waits))
      cat("\n")
    },

    #' Reset Limiter
    #'
    reset = function() {
      self$requests <- c()
      self$total_requests <- 0
      self$total_waits <- 0
      message(sprintf("[%s] Rate limiter reset", self$api_name))
    }
  )
)


# =============================================================================
# PRE-CONFIGURED RATE LIMITERS
# =============================================================================

# Global rate limiters for common APIs
# These are initialized lazily (only when first used)

#' Get FishBase Rate Limiter
#'
#' FishBase API limit: ~100 requests per hour (unofficial, conservative estimate)
#'
get_fishbase_limiter <- function() {
  if (!exists(".fishbase_limiter", envir = .GlobalEnv)) {
    assign(".fishbase_limiter",
          RateLimiter$new(max_requests = 100, time_window = 3600, api_name = "FishBase"),
          envir = .GlobalEnv)
  }
  return(get(".fishbase_limiter", envir = .GlobalEnv))
}


#' Get SeaLifeBase Rate Limiter
#'
#' SeaLifeBase API limit: Same as FishBase (~100/hour)
#'
get_sealifebase_limiter <- function() {
  if (!exists(".sealifebase_limiter", envir = .GlobalEnv)) {
    assign(".sealifebase_limiter",
          RateLimiter$new(max_requests = 100, time_window = 3600, api_name = "SeaLifeBase"),
          envir = .GlobalEnv)
  }
  return(get(".sealifebase_limiter", envir = .GlobalEnv))
}


#' Get WoRMS Rate Limiter
#'
#' WoRMS API limit: ~50 requests per minute (conservative)
#'
get_worms_limiter <- function() {
  if (!exists(".worms_limiter", envir = .GlobalEnv)) {
    assign(".worms_limiter",
          RateLimiter$new(max_requests = 50, time_window = 60, api_name = "WoRMS"),
          envir = .GlobalEnv)
  }
  return(get(".worms_limiter", envir = .GlobalEnv))
}


#' Get AlgaeBase Rate Limiter
#'
#' AlgaeBase API limit: Unknown, use conservative 60/hour
#'
get_algaebase_limiter <- function() {
  if (!exists(".algaebase_limiter", envir = .GlobalEnv)) {
    assign(".algaebase_limiter",
          RateLimiter$new(max_requests = 60, time_window = 3600, api_name = "AlgaeBase"),
          envir = .GlobalEnv)
  }
  return(get(".algaebase_limiter", envir = .GlobalEnv))
}


#' Get freshwaterecology.info Rate Limiter
#'
#' freshwaterecology API limit: Unknown, use 100/hour
#'
get_freshwater_limiter <- function() {
  if (!exists(".freshwater_limiter", envir = .GlobalEnv)) {
    assign(".freshwater_limiter",
          RateLimiter$new(max_requests = 100, time_window = 3600, api_name = "freshwaterecology"),
          envir = .GlobalEnv)
  }
  return(get(".freshwater_limiter", envir = .GlobalEnv))
}


#' Get SHARK Rate Limiter
#'
#' SHARK API limit: Unknown, use 100/hour
#'
get_shark_limiter <- function() {
  if (!exists(".shark_limiter", envir = .GlobalEnv)) {
    assign(".shark_limiter",
          RateLimiter$new(max_requests = 100, time_window = 3600, api_name = "SHARK"),
          envir = .GlobalEnv)
  }
  return(get(".shark_limiter", envir = .GlobalEnv))
}


# =============================================================================
# RETRY LOGIC WITH EXPONENTIAL BACKOFF
# =============================================================================

#' Execute API Call with Retry Logic
#'
#' Wraps an API call with automatic retry and exponential backoff for
#' rate limit errors, network errors, and temporary failures.
#'
#' @param api_call Function. The API call to execute (should return result or NULL)
#' @param max_retries Integer. Maximum number of retries (default: 3)
#' @param initial_delay Numeric. Initial delay in seconds (default: 1)
#' @param backoff_factor Numeric. Multiply delay by this each retry (default: 2)
#' @param rate_limiter RateLimiter. Optional rate limiter to use
#' @param api_name Character. Name of API for logging
#'
#' @return Result of api_call, or NULL if all retries failed
#'
#' @examples
#' result <- api_call_with_retry(
#'   api_call = function() rfishbase::species("Gadus morhua"),
#'   rate_limiter = get_fishbase_limiter(),
#'   api_name = "FishBase"
#' )
#'
api_call_with_retry <- function(api_call,
                                max_retries = 3,
                                initial_delay = 1,
                                backoff_factor = 2,
                                rate_limiter = NULL,
                                api_name = "API") {

  attempt <- 0
  delay <- initial_delay

  while (attempt <= max_retries) {
    attempt <- attempt + 1

    tryCatch({
      # Use rate limiter if provided
      if (!is.null(rate_limiter)) {
        rate_limiter$acquire()
      }

      # Make API call
      result <- api_call()

      # Success!
      if (attempt > 1) {
        message(sprintf("[%s] Success after %d attempts", api_name, attempt))
      }

      return(result)

    }, error = function(e) {
      error_msg <- as.character(e$message)

      # Check if rate limit error
      is_rate_limit <- grepl("rate limit|too many requests|429", error_msg, ignore.case = TRUE)

      # Check if network error
      is_network <- grepl("timeout|connection|network|could not resolve", error_msg, ignore.case = TRUE)

      # Check if temporary error
      is_temporary <- grepl("503|502|500|temporarily", error_msg, ignore.case = TRUE)

      # Log error
      if (exists("log_error")) {
        log_error(api_name, "unknown_species", error_msg,
                 error_type = if (is_rate_limit) "rate_limit"
                             else if (is_network) "network"
                             else if (is_temporary) "temporary"
                             else "api_error")
      }

      # Decide whether to retry
      should_retry <- (is_rate_limit || is_network || is_temporary) && attempt <= max_retries

      if (should_retry) {
        message(sprintf("[%s] Error on attempt %d/%d: %s",
                       api_name, attempt, max_retries + 1, error_msg))
        message(sprintf("[%s] Retrying in %.1f seconds...", api_name, delay))

        Sys.sleep(delay)
        delay <- delay * backoff_factor  # Exponential backoff

        # Continue to next iteration (retry)
        return(NULL)  # Will be caught and retried
      } else {
        # Don't retry - return NULL
        if (attempt > max_retries) {
          message(sprintf("[%s] Failed after %d attempts: %s",
                         api_name, max_retries + 1, error_msg))
        }
        return(NULL)
      }
    })
  }

  # All retries exhausted
  return(NULL)
}


# =============================================================================
# RATE LIMITER STATISTICS
# =============================================================================

#' Get All Rate Limiter Statistics
#'
#' Returns statistics for all active rate limiters.
#'
#' @return List of rate limiter statistics
#'
get_all_limiter_stats <- function() {
  stats <- list()

  limiter_names <- c(
    ".fishbase_limiter",
    ".sealifebase_limiter",
    ".worms_limiter",
    ".algaebase_limiter",
    ".freshwater_limiter",
    ".shark_limiter"
  )

  for (name in limiter_names) {
    if (exists(name, envir = .GlobalEnv)) {
      limiter <- get(name, envir = .GlobalEnv)
      stats[[gsub("^\\.", "", gsub("_limiter$", "", name))]] <- limiter$get_stats()
    }
  }

  return(stats)
}


#' Print All Rate Limiter Statistics
#'
print_all_limiter_stats <- function() {
  stats <- get_all_limiter_stats()

  if (length(stats) == 0) {
    message("No active rate limiters")
    return(invisible(NULL))
  }

  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("API RATE LIMITER STATISTICS\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("\n")

  for (api in names(stats)) {
    s <- stats[[api]]
    cat(sprintf("%s\n", s$api_name))
    cat("───────────────────────────────────────────────────────────────\n")
    cat(sprintf("  Limit:         %d requests / %.0fs\n", s$max_requests, s$time_window))
    cat(sprintf("  Current usage: %d/%d (%.1f%%)\n",
               s$current_count, s$max_requests, s$utilization * 100))
    cat(sprintf("  Remaining:     %d\n", s$remaining))
    cat(sprintf("  Total requests:%d\n", s$total_requests))
    cat(sprintf("  Times waited:  %d\n", s$total_waits))
    cat("\n")
  }

  cat("═══════════════════════════════════════════════════════════════\n")
  cat("\n")
}


#' Reset All Rate Limiters
#'
#' Clears all rate limiter request histories.
#'
reset_all_limiters <- function() {
  limiter_names <- c(
    ".fishbase_limiter",
    ".sealifebase_limiter",
    ".worms_limiter",
    ".algaebase_limiter",
    ".freshwater_limiter",
    ".shark_limiter"
  )

  count <- 0
  for (name in limiter_names) {
    if (exists(name, envir = .GlobalEnv)) {
      limiter <- get(name, envir = .GlobalEnv)
      limiter$reset()
      count <- count + 1
    }
  }

  message(sprintf("Reset %d rate limiters", count))
}


# =============================================================================
# EXPORT
# =============================================================================

# Main functions exported for use in other modules:
# - RateLimiter (class)
# - get_fishbase_limiter()
# - get_sealifebase_limiter()
# - get_worms_limiter()
# - get_algaebase_limiter()
# - get_freshwater_limiter()
# - get_shark_limiter()
# - api_call_with_retry()
# - get_all_limiter_stats()
# - print_all_limiter_stats()
# - reset_all_limiters()
