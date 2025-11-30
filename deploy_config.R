# Deployment Configuration for laguna.ku.lt
# This file contains all configuration settings for deployment

# ==============================================================================
# SERVER CONFIGURATION
# ==============================================================================

# Server details
SERVER_HOST <- "laguna.ku.lt"
SERVER_USER <- Sys.getenv("SHINY_SERVER_USER", "your_username")  # Set in environment
SERVER_PORT <- 22

# Shiny server paths
SHINY_SERVER_ROOT <- "/srv/shiny-server"
APP_NAME <- "EcoNeTool"
APP_DEPLOY_PATH <- file.path(SHINY_SERVER_ROOT, APP_NAME)

# ==============================================================================
# DEPLOYMENT SETTINGS
# ==============================================================================

# Files to deploy (relative to project root)
FILES_TO_DEPLOY <- c(
  "app.R",                    # Main application
  "plotfw.R",                 # Plotting functions
  "BalticFW.Rdata",           # Default data
  "README.md",                # Documentation
  "BS4DASH_README.md",        # bs4Dash documentation
  "QUICK_START_BS4DASH.md",  # Quick start guide
  "LAUNCH_GUIDE.md",          # Launch guide
  "IMPROVEMENTS.md",          # Code improvements doc
  "FIXES_APPLIED.md",         # Bug fixes doc
  "CHANGELOG_v2.1.md"         # Version changelog
)

# Directories to deploy
DIRS_TO_DEPLOY <- c(
  # Add any subdirectories if needed
)

# Files to exclude from deployment
EXCLUDE_PATTERNS <- c(
  "*.Rproj",
  "*.Rproj.user",
  "*.Rhistory",
  "*.RData",
  ".git",
  ".gitignore",
  "*backup*",
  "*test*.R",
  "deploy_*.R",
  "install_*.R",
  "check_*.R"
)

# ==============================================================================
# PACKAGE DEPENDENCIES
# ==============================================================================

# Required R packages on server
REQUIRED_PACKAGES <- c(
  "shiny",
  "bs4Dash",
  "igraph",
  "fluxweb",
  "visNetwork"
)

# Optional packages (for future features)
OPTIONAL_PACKAGES <- c(
  "readxl",      # Excel import
  "readr",       # CSV import
  "DT"           # Data tables
)

# ==============================================================================
# APPLICATION SETTINGS
# ==============================================================================

# App metadata
APP_VERSION <- "2.1"
APP_TITLE <- "EcoNeTool - Food Web Explorer"
APP_DESCRIPTION <- "Interactive food web analysis tool"

# Server settings
SHINY_PORT <- 3838  # Default Shiny server port
APP_URL <- paste0("http://", SERVER_HOST, ":", SHINY_PORT, "/", APP_NAME, "/")

# ==============================================================================
# BACKUP SETTINGS
# ==============================================================================

# Backup before deployment
CREATE_BACKUP <- TRUE
BACKUP_DIR <- file.path(SHINY_SERVER_ROOT, "backups", APP_NAME)
BACKUP_TIMESTAMP <- format(Sys.time(), "%Y%m%d_%H%M%S")

# ==============================================================================
# LOGGING
# ==============================================================================

# Log file location
LOG_DIR <- "deployment_logs"
LOG_FILE <- file.path(LOG_DIR, paste0("deploy_", Sys.Date(), ".log"))

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Get server connection string
#' @return Character string for SSH connection
get_server_connection <- function() {
  paste0(SERVER_USER, "@", SERVER_HOST)
}

#' Get full app URL
#' @return Character string with full URL
get_app_url <- function() {
  APP_URL
}

#' Check if required packages are listed
#' @return Logical indicating if all packages are present
check_required_packages <- function() {
  missing <- REQUIRED_PACKAGES[!REQUIRED_PACKAGES %in% installed.packages()[,"Package"]]
  if (length(missing) > 0) {
    message("Missing required packages: ", paste(missing, collapse=", "))
    return(FALSE)
  }
  return(TRUE)
}

#' Create log directory if it doesn't exist
create_log_dir <- function() {
  if (!dir.exists(LOG_DIR)) {
    dir.create(LOG_DIR, recursive = TRUE)
    message("Created log directory: ", LOG_DIR)
  }
}

#' Log deployment message
#' @param msg Message to log
#' @param level Log level (INFO, WARN, ERROR)
log_message <- function(msg, level = "INFO") {
  create_log_dir()
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s: %s\n", timestamp, level, msg)
  cat(log_entry, file = LOG_FILE, append = TRUE)
  cat(log_entry)
}

# ==============================================================================
# VALIDATION
# ==============================================================================

#' Validate deployment configuration
#' @return Logical indicating if configuration is valid
validate_config <- function() {
  valid <- TRUE

  # Check server user is set
  if (SERVER_USER == "your_username") {
    log_message("SERVER_USER not configured. Set SHINY_SERVER_USER environment variable.", "ERROR")
    valid <- FALSE
  }

  # Check required files exist
  for (file in FILES_TO_DEPLOY) {
    if (!file.exists(file)) {
      log_message(paste("Required file not found:", file), "WARN")
    }
  }

  # Check required packages
  if (!check_required_packages()) {
    log_message("Some required packages are missing locally", "WARN")
  }

  return(valid)
}

# ==============================================================================
# PRINT CONFIGURATION (for verification)
# ==============================================================================

print_config <- function() {
  cat("\n")
  cat(strrep("=", 60), "\n")
  cat("DEPLOYMENT CONFIGURATION\n")
  cat(strrep("=", 60), "\n\n")

  cat("SERVER:\n")
  cat("  Host:     ", SERVER_HOST, "\n")
  cat("  User:     ", SERVER_USER, "\n")
  cat("  Port:     ", SERVER_PORT, "\n")
  cat("  Path:     ", APP_DEPLOY_PATH, "\n\n")

  cat("APPLICATION:\n")
  cat("  Name:     ", APP_NAME, "\n")
  cat("  Version:  ", APP_VERSION, "\n")
  cat("  URL:      ", get_app_url(), "\n\n")

  cat("FILES TO DEPLOY:\n")
  for (file in FILES_TO_DEPLOY) {
    status <- if (file.exists(file)) "[OK]" else "[MISSING]"
    cat("  ", status, " ", file, "\n")
  }
  cat("\n")

  cat("PACKAGES REQUIRED:\n")
  cat("  ", paste(REQUIRED_PACKAGES, collapse=", "), "\n\n")

  cat("BACKUP:\n")
  cat("  Enabled:  ", CREATE_BACKUP, "\n")
  cat("  Dir:      ", BACKUP_DIR, "\n\n")

  cat(strrep("=", 60), "\n\n")
}

# Print configuration when sourced
if (interactive()) {
  print_config()
}
