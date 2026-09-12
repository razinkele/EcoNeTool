# =============================================================================
# ADMIN PASSWORD GATE
# =============================================================================
# Protects the "API Key Configuration" modal in R/modules/plugin_server.R on
# deployed instances. The gate is OPT-IN: with no hash configured it stays
# disabled and the modal behaves exactly as it always has, so local
# development and existing deployments are unaffected.
#
# Enable it by setting one environment variable on the server:
#
#   ECONETOOL_ADMIN_PASSWORD_HASH=econetool1$12$<salt-hex>$<key-hex>
#
# Generate the value with set_admin_password("your password"), which prints a
# pasteable line and never writes it to disk.
#
# Only the derived key is ever stored. The plaintext password exists solely in
# the Shiny input during a single unlock attempt.
# =============================================================================

ADMIN_PASSWORD_ENV <- "ECONETOOL_ADMIN_PASSWORD_HASH"

# Record format identifier. Bump if the KDF or layout ever changes, so old
# records are rejected loudly instead of silently mis-verified.
ADMIN_HASH_SCHEME <- "econetool1"

# bcrypt_pbkdf work factor. 12 rounds measures ~0.15s: slow enough to make
# offline guessing expensive, fast enough for an interactive prompt.
ADMIN_HASH_ROUNDS <- 12L
ADMIN_HASH_SIZE <- 32L
ADMIN_SALT_BYTES <- 16L

# ---------------------------------------------------------------------------
# Internal hex helpers (raw <-> lowercase hex, no dependencies)
# ---------------------------------------------------------------------------

.raw_to_hex <- function(r) {
  paste(as.character(r), collapse = "")
}

.hex_to_raw <- function(h) {
  if (!is.character(h) || length(h) != 1L || is.na(h)) {
    return(NULL)
  }
  if (!grepl("^([0-9a-f]{2})+$", h)) {
    return(NULL)
  }
  starts <- seq(1L, nchar(h), by = 2L)
  as.raw(strtoi(substring(h, starts, starts + 1L), base = 16L))
}

#' Compare two raw vectors without leaking content through timing
#'
#' Comparison cost depends only on the inputs' lengths, never on where the
#' first differing byte sits, so an attacker cannot narrow a guess byte by
#' byte. Unequal lengths return FALSE immediately - that length is already
#' public, being fixed by ADMIN_HASH_SIZE.
#'
#' @param a,b Raw vectors.
#' @return TRUE when both are raw, equal length, and byte-identical.
#' @export
constant_time_equal <- function(a, b) {
  if (!is.raw(a) || !is.raw(b)) {
    return(FALSE)
  }
  if (length(a) != length(b)) {
    return(FALSE)
  }
  # Vectorised XOR with no early exit: every byte is always examined.
  isTRUE(sum(bitwXor(as.integer(a), as.integer(b))) == 0L)
}

#' Hash an admin password into a storable record
#'
#' @param password Character scalar. Must not be empty or whitespace only.
#' @param rounds Integer bcrypt_pbkdf work factor.
#' @return Character scalar, formatted as
#'   \code{econetool1$<rounds>$<salt-hex>$<key-hex>}.
#' @export
#' @examples
#' \dontrun{
#' hash_admin_password("a long passphrase")
#' }
hash_admin_password <- function(password, rounds = ADMIN_HASH_ROUNDS) {
  if (!is.character(password) || length(password) != 1L || is.na(password) ||
        !nzchar(trimws(password))) {
    stop("admin password must not be empty", call. = FALSE)
  }
  if (!requireNamespace("openssl", quietly = TRUE)) {
    stop("the 'openssl' package is required to hash the admin password",
         call. = FALSE)
  }

  rounds <- as.integer(rounds)
  if (is.na(rounds) || rounds < 1L) {
    stop("rounds must be a positive integer", call. = FALSE)
  }

  salt <- openssl::rand_bytes(ADMIN_SALT_BYTES)
  key <- openssl::bcrypt_pbkdf(password, salt = salt,
                               size = ADMIN_HASH_SIZE, rounds = rounds)

  sprintf("%s$%d$%s$%s", ADMIN_HASH_SCHEME, rounds,
          .raw_to_hex(salt), .raw_to_hex(key))
}

#' Is the admin gate switched on?
#'
#' @param record The stored record; defaults to the environment variable.
#' @return TRUE when a non-blank record is configured.
#' @export
admin_gate_enabled <- function(record = Sys.getenv(ADMIN_PASSWORD_ENV)) {
  is.character(record) && length(record) == 1L && !is.na(record) &&
    nzchar(trimws(record))
}

#' Verify a candidate password against the configured record
#'
#' Fails closed: any malformed, truncated, or unknown-scheme record returns
#' FALSE and warns. warning() rather than message() because production
#' shiny-server.conf has preserve_logs commented out, so messages vanish.
#'
#' @param password Character scalar supplied by the user.
#' @param record The stored record; defaults to the environment variable.
#' @return TRUE only on an exact match.
#' @export
verify_admin_password <- function(password,
                                  record = Sys.getenv(ADMIN_PASSWORD_ENV)) {
  if (!admin_gate_enabled(record)) {
    warning("[admin auth] no admin password hash configured; denying unlock",
            call. = FALSE)
    return(FALSE)
  }
  # An empty submission is a user typo, not a misconfiguration: reject it
  # quietly rather than letting the KDF fail and emit an alarming warning.
  if (!is.character(password) || length(password) != 1L || is.na(password) ||
        !nzchar(password)) {
    return(FALSE)
  }
  if (!requireNamespace("openssl", quietly = TRUE)) {
    warning("[admin auth] the 'openssl' package is unavailable; denying unlock",
            call. = FALSE)
    return(FALSE)
  }

  parts <- strsplit(trimws(record), "$", fixed = TRUE)[[1]]
  if (length(parts) != 4L) {
    warning("[admin auth] malformed password record: expected 4 fields, got ",
            length(parts), call. = FALSE)
    return(FALSE)
  }
  if (!identical(parts[1], ADMIN_HASH_SCHEME)) {
    warning(sprintf("[admin auth] unknown password record scheme '%s'", parts[1]),
            call. = FALSE)
    return(FALSE)
  }

  rounds <- suppressWarnings(as.integer(parts[2]))
  if (is.na(rounds) || rounds < 1L) {
    warning(sprintf("[admin auth] invalid rounds in password record: '%s'",
                    parts[2]), call. = FALSE)
    return(FALSE)
  }

  salt <- .hex_to_raw(parts[3])
  expected <- .hex_to_raw(parts[4])
  if (is.null(salt) || is.null(expected)) {
    warning("[admin auth] password record contains malformed hex", call. = FALSE)
    return(FALSE)
  }

  actual <- tryCatch(
    openssl::bcrypt_pbkdf(password, salt = salt,
                          size = length(expected), rounds = rounds),
    error = function(e) {
      warning(sprintf("[admin auth] key derivation failed: %s",
                      conditionMessage(e)), call. = FALSE)
      NULL
    }
  )
  if (is.null(actual)) {
    return(FALSE)
  }

  constant_time_equal(actual, expected)
}

#' May this session act on protected configuration?
#'
#' The single decision every protected observer must consult. Shiny input IDs
#' are client-controlled: gating only the observer that *renders* a form
#' leaves the observer that *acts* on it reachable directly, e.g.
#' \code{Shiny.setInputValue('save_api_keys', 1)} from a browser console. So
#' the read path and the write path both call this, not just the one that
#' draws the modal.
#'
#' @param unlocked The session's unlock flag, normally
#'   \code{session$userData$admin_unlocked}. Anything other than TRUE is
#'   treated as locked.
#' @return TRUE when the gate is off, or when this session has unlocked it.
#' @export
admin_authorized <- function(unlocked) {
  !admin_gate_enabled() || isTRUE(unlocked)
}

#' Print a pasteable environment line for a new admin password
#'
#' Deliberately writes nothing: silently creating a secrets file is worse than
#' one manual paste. Run this locally, then set the variable on the server
#' (for shiny-server, an .Renviron in the app directory works).
#'
#' @param password Character scalar.
#' @param rounds Integer work factor.
#' @return The record, invisibly.
#' @export
#' @examples
#' \dontrun{
#' set_admin_password("a long passphrase")
#' }
set_admin_password <- function(password, rounds = ADMIN_HASH_ROUNDS) {
  record <- hash_admin_password(password, rounds = rounds)

  cat("\nAdd this line to the server environment",
      "(e.g. .Renviron in the app directory):\n\n")
  cat(sprintf("%s=%s\n\n", ADMIN_PASSWORD_ENV, record))
  cat("Keep .Renviron out of git - it is listed in .gitignore.\n")
  cat("Restart the app (touch restart.txt) for the gate to take effect.\n\n")

  invisible(record)
}
