# =============================================================================
# Source guard: network-reaching layer2c tests are live-gated (F84)
# =============================================================================
# test-layer2c-api-databases.R used to define its own skip_if_offline()
# (shadowing helper-fixtures.R) and ran real HTTP in "structure" tests with no
# RUN_LIVE_TESTS check, so the offline suite depended on five upstream APIs.
# This guard parses the file and fails if a test that calls a network lookup
# loses its gate or its timeout.

layer2c_path <- function() {
  file.path(get_app_root(), "tests", "testthat", "test-layer2c-api-databases.R")
}

# test_that() calls at top level: description -> body expression
layer2c_tests <- function() {
  exprs <- as.list(parse(layer2c_path(), keep.source = FALSE))
  calls <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("test_that")), exprs)
  stats::setNames(lapply(calls, function(e) e[[3]]),
                  vapply(calls, function(e) as.character(e[[2]]), character(1)))
}

NETWORK_LOOKUPS <- c("lookup_worms_traits_api", "lookup_polytraits", "lookup_emodnet_traits",
                     "lookup_obis_traits", "lookup_traitbank")

# Tests that call a lookup but return before any I/O (argument validation,
# or EMODnet without Btrait). Keep this list short and exact.
UNGATED_OK <- c(
  "lookup_worms_traits_api returns correct structure with NULL aphia_id",
  "lookup_worms_traits_api returns FALSE for non-positive aphia_id",
  "lookup_worms_traits_api returns FALSE for non-numeric aphia_id",
  "lookup_emodnet_traits returns FALSE gracefully without Btrait"
)

test_that("layer2c does not redefine skip_if_offline()", {
  exprs <- as.list(parse(layer2c_path(), keep.source = FALSE))
  redefines <- vapply(exprs, function(e) {
    is.call(e) && as.character(e[[1]]) %in% c("<-", "=", "<<-") &&
      identical(e[[2]], as.name("skip_if_offline"))
  }, logical(1))
  expect_false(any(redefines))
})

test_that("every layer2c test that calls a network lookup is live-gated and time-bounded", {
  tests <- layer2c_tests()
  expect_true(all(UNGATED_OK %in% names(tests)),
              info = "UNGATED_OK names a test that no longer exists; update the list")

  calls_lookup <- vapply(tests, function(b) any(NETWORK_LOOKUPS %in% all.names(b)), logical(1))
  gated <- names(tests)[calls_lookup & !names(tests) %in% UNGATED_OK]
  expect_gte(length(gated), 12L)

  for (nm in gated) {
    used <- all.names(tests[[nm]])
    expect_true("skip_if_no_live_tests" %in% used, info = paste("no skip_if_no_live_tests():", nm))
    expect_true("with_timeout" %in% used, info = paste("no with_timeout():", nm))
  }
})
