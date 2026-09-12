# =============================================================================
# Admin password gate for the API key configuration modal
# =============================================================================
# The gate protects R/modules/plugin_server.R's "API Key Configuration" modal
# on deployed instances. Logic lives in pure functions so it is testable
# without a Shiny session.

source_app_dependencies()

local({
  root <- get_app_root()
  source(file.path(root, "R/functions/admin_auth.R"), local = FALSE)
})

ENV_VAR <- "ECONETOOL_ADMIN_PASSWORD_HASH"

with_admin_hash <- function(value, code) {
  old <- Sys.getenv(ENV_VAR, unset = NA)
  on.exit({
    if (is.na(old)) Sys.unsetenv(ENV_VAR) else Sys.setenv(ECONETOOL_ADMIN_PASSWORD_HASH = old)
  }, add = TRUE)
  if (is.null(value)) Sys.unsetenv(ENV_VAR) else Sys.setenv(ECONETOOL_ADMIN_PASSWORD_HASH = value)
  force(code)
}

# ---------------------------------------------------------------------------
# hash_admin_password / verify_admin_password
# ---------------------------------------------------------------------------

test_that("a hashed password verifies against itself", {
  skip_if_no_package("openssl")

  record <- hash_admin_password("correct horse battery staple")
  expect_true(verify_admin_password("correct horse battery staple", record))
})

test_that("a wrong password does not verify", {
  skip_if_no_package("openssl")

  record <- hash_admin_password("correct horse battery staple")
  expect_false(verify_admin_password("Correct Horse Battery Staple", record))
  expect_false(verify_admin_password("", record))
  expect_false(verify_admin_password("correct horse battery stapl", record))
})

test_that("the same password hashes differently each time (random salt)", {
  skip_if_no_package("openssl")

  a <- hash_admin_password("hunter2")
  b <- hash_admin_password("hunter2")
  expect_false(identical(a, b))
  # ...but both still verify
  expect_true(verify_admin_password("hunter2", a))
  expect_true(verify_admin_password("hunter2", b))
})

test_that("the record is a single self-describing string", {
  skip_if_no_package("openssl")

  record <- hash_admin_password("hunter2", rounds = 8L)
  expect_length(record, 1L)
  parts <- strsplit(record, "$", fixed = TRUE)[[1]]
  expect_equal(length(parts), 4L)
  expect_equal(parts[1], "econetool1")
  expect_equal(parts[2], "8")
  expect_match(parts[3], "^[0-9a-f]+$")
  expect_match(parts[4], "^[0-9a-f]+$")
})

test_that("an empty password is refused at hash time", {
  skip_if_no_package("openssl")

  expect_error(hash_admin_password(""), "empty")
  expect_error(hash_admin_password("   "), "empty")
})

# ---------------------------------------------------------------------------
# Malformed records must fail closed, and say so
# ---------------------------------------------------------------------------

test_that("malformed records fail closed with a warning", {
  skip_if_no_package("openssl")

  bad <- list(
    "",
    "not-a-record",
    "econetool1$12$deadbeef",                    # too few fields
    "econetool1$12$deadbeef$cafe$extra",         # too many fields
    "bcrypt$12$deadbeef$cafe",                   # unknown scheme
    "econetool1$notanumber$deadbeef$cafe",       # bad rounds
    "econetool1$12$nothex$cafe",                 # bad salt hex
    "econetool1$12$deadbeef$nothex"              # bad key hex
  )

  for (record in bad) {
    expect_warning(result <- verify_admin_password("hunter2", record),
                   regexp = "admin", ignore.case = TRUE)
    expect_false(result)
  }
})

test_that("verify returns FALSE rather than erroring on NA or NULL", {
  skip_if_no_package("openssl")

  expect_false(suppressWarnings(verify_admin_password("hunter2", NA_character_)))
  expect_false(suppressWarnings(verify_admin_password("hunter2", NULL)))
})

# ---------------------------------------------------------------------------
# admin_gate_enabled
# ---------------------------------------------------------------------------

test_that("the gate is disabled when the environment variable is unset", {
  with_admin_hash(NULL, {
    expect_false(admin_gate_enabled())
  })
})

test_that("the gate is disabled when the environment variable is blank", {
  with_admin_hash("", expect_false(admin_gate_enabled()))
  with_admin_hash("   ", expect_false(admin_gate_enabled()))
})

test_that("the gate is enabled when a hash is configured", {
  skip_if_no_package("openssl")

  with_admin_hash(hash_admin_password("hunter2"), {
    expect_true(admin_gate_enabled())
  })
})

test_that("verify_admin_password reads the environment by default", {
  skip_if_no_package("openssl")

  with_admin_hash(hash_admin_password("hunter2"), {
    expect_true(verify_admin_password("hunter2"))
    expect_false(verify_admin_password("wrong"))
  })
})

# ---------------------------------------------------------------------------
# Constant-time comparison
# ---------------------------------------------------------------------------

test_that("constant-time comparison agrees with identical() on raw vectors", {
  a <- as.raw(c(1, 2, 3, 4))
  b <- as.raw(c(1, 2, 3, 4))
  c_ <- as.raw(c(1, 2, 3, 5))
  d <- as.raw(c(1, 2, 3))

  expect_true(constant_time_equal(a, b))
  expect_false(constant_time_equal(a, c_))
  expect_false(constant_time_equal(a, d))   # length mismatch
})

# ---------------------------------------------------------------------------
# set_admin_password
# ---------------------------------------------------------------------------

test_that("set_admin_password prints a pasteable env line and writes nothing", {
  skip_if_no_package("openssl")

  before <- list.files(app_path("config"), all.files = TRUE)
  out <- capture.output(record <- set_admin_password("hunter2"))
  after <- list.files(app_path("config"), all.files = TRUE)

  expect_equal(before, after)
  expect_true(any(grepl("ECONETOOL_ADMIN_PASSWORD_HASH", out, fixed = TRUE)))
  expect_true(verify_admin_password("hunter2", record))
})
