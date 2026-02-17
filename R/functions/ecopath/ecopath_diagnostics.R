# ==============================================================================
# ECOPATH IMPORT TESTING
# ==============================================================================
# Testing and validation utilities for ECOPATH import functionality
#
# Features:
#   - System compatibility check
#   - Package availability verification
#   - Database connection testing
#   - ODBC driver listing (Windows)
#
# ==============================================================================

test_ecopath_import <- function(db_file = NULL) {
  #' Test ECOPATH Import Functionality
  #'
  #' Checks if required packages are installed and tests database connection
  #'
  #' @param db_file Optional path to test database
  #' @export

  cat("================================================================================\n")
  cat("ECOPATH Import Test\n")
  cat("================================================================================\n\n")

  # Check OS
  os_type <- Sys.info()["sysname"]
  cat("Operating System:", os_type, "\n")
  cat("R Version:", R.version.string, "\n")
  cat("R Architecture:", R.version$arch, "\n\n")

  # Check package availability
  if (os_type == "Windows") {
    cat("Checking RODBC package... ")
    if (requireNamespace("RODBC", quietly = TRUE)) {
      cat("✓ Installed\n")

      # Check ODBC drivers
      cat("\nAvailable ODBC drivers:\n")
      tryCatch({
        drivers <- RODBC::odbcDataSources()
        if (length(drivers) > 0) {
          print(drivers)
        } else {
          cat("  (None found - but this is normal if using file-based connections)\n")
        }
      }, error = function(e) {
        cat("  Could not list drivers:", e$message, "\n")
      })
    } else {
      cat("✗ NOT installed\n")
      cat("  Install with: install.packages('RODBC')\n")
      return(invisible(FALSE))
    }
  } else {
    cat("Checking Hmisc package... ")
    if (requireNamespace("Hmisc", quietly = TRUE)) {
      cat("✓ Installed\n")
    } else {
      cat("✗ NOT installed\n")
      cat("  Install with: install.packages('Hmisc')\n")
      return(invisible(FALSE))
    }

    cat("Checking mdbtools... ")
    mdb_check <- system("which mdb-tables", intern = TRUE)
    if (length(mdb_check) > 0) {
      cat("✓ Installed\n")
    } else {
      cat("✗ NOT installed\n")
      cat("  Install with: sudo apt-get install mdbtools (Ubuntu/Debian)\n")
      return(invisible(FALSE))
    }
  }

  # Test with file if provided
  if (!is.null(db_file)) {
    cat("\n================================================================================\n")
    cat("Testing database file:", db_file, "\n")
    cat("================================================================================\n\n")

    if (!file.exists(db_file)) {
      cat("✗ File not found\n")
      return(invisible(FALSE))
    }

    tryCatch({
      result <- parse_ecopath_native_cross_platform(db_file)

      cat("\n✓ Success!\n\n")
      cat("Group data: ", nrow(result$group_data), " rows, ",
          ncol(result$group_data), " columns\n")
      cat("Diet data: ", nrow(result$diet_data), " rows, ",
          ncol(result$diet_data), " columns\n")
      cat("Method: ", result$method, "\n")

      return(invisible(TRUE))

    }, error = function(e) {
      cat("\n✗ Error:", e$message, "\n")
      return(invisible(FALSE))
    })
  }

  cat("\n✓ Prerequisites check complete\n")
  return(invisible(TRUE))
}

# ==============================================================================
# USAGE EXAMPLES
# ==============================================================================

# Example 1: Test system compatibility
# test_ecopath_import()

# Example 2: Test with specific file
# test_ecopath_import("coast 2011-04-10 10.00.ewemdb")

# Example 3: Import ECOPATH file
# result <- parse_ecopath_native_cross_platform("coast 2011-04-10 10.00.ewemdb")
# View(result$group_data)
# View(result$diet_data)
