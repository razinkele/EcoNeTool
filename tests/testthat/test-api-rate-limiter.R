# api_call_with_retry() backoff (PR #45 review): the error handler updated a
# closure-local `delay`, so every retry waited initial_delay.
library(testthat)

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
source(file.path(app_root, "R/functions/api_rate_limiter.R"))

test_that("api_call_with_retry() grows the delay between retries", {
  msgs <- character()
  withCallingHandlers(
    res <- api_call_with_retry(function() stop("503 temporarily unavailable"),
                               max_retries = 2, initial_delay = 0.1, backoff_factor = 3,
                               api_name = "test"),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_null(res)
  delays <- regmatches(msgs, regexpr("Retrying in [0-9.]+", msgs))
  expect_identical(delays, c("Retrying in 0.1", "Retrying in 0.3"))
})
