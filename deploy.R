# ==============================================================================
# EcoNeTool R-based Deployment Script
# ==============================================================================
# Cross-platform deployment script using R
# Works on Windows, Mac, and Linux
#
# Usage:
#   Rscript deploy.R
#
# Or from R console:
#   source("deploy.R")
#   deploy_to_server()
#
# ==============================================================================
Sys.setenv(SHINY_SERVER_USER = "razinka")
# Load configuration
source("deploy_config.R")

# ==============================================================================
# DEPLOYMENT FUNCTIONS
# ==============================================================================

#' Main deployment function
#' @param dry_run Logical, if TRUE, show what would be done without doing it
#' @param no_backup Logical, if TRUE, skip backup creation
deploy_to_server <- function(dry_run = FALSE, no_backup = FALSE) {

  cat("\n")
  cat("================================================================\n")
  cat("  EcoNeTool Deployment to laguna.ku.lt\n")
  cat("  Version: 2.1\n")
  cat("  Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("================================================================\n\n")

  if (dry_run) {
    cat("*** DRY RUN MODE - No changes will be made ***\n\n")
  }

  # Create log directory
  create_log_dir()

  log_message("Deployment started")
  log_message(paste("Target:", get_server_connection()))

  # Step 1: Validate configuration
  cat("\n[1/6] Validating configuration...\n")
  if (!validate_config()) {
    if (!dry_run) {
      stop("Configuration validation failed. Please fix errors and try again.")
    } else {
      log_message("Configuration has issues (continuing in dry-run mode)", "WARN")
    }
  }
  log_message("Configuration validated")

  # Step 2: Test SSH connection
  cat("\n[2/6] Testing SSH connection...\n")
  if (!test_ssh_connection(dry_run)) {
    stop("Cannot connect to server. Please check SSH configuration.")
  }
  log_message("SSH connection successful")

  # Step 3: Create backup
  if (!no_backup && !dry_run) {
    cat("\n[3/6] Creating backup...\n")
    create_server_backup()
  } else {
    cat("\n[3/6] Skipping backup\n")
    if (dry_run) {
      log_message("Backup skipped (dry-run mode)")
    } else {
      log_message("Backup skipped (no-backup flag)")
    }
  }

  # Step 4: Deploy files
  cat("\n[4/6] Deploying files...\n")
  deploy_files_rsync(dry_run)
  log_message("Files deployed")

  # Step 5: Install packages (if not dry run)
  if (!dry_run) {
    cat("\n[5/6] Installing R packages...\n")
    install_server_packages()
  } else {
    cat("\n[5/6] Skipping package installation (dry-run)\n")
  }

  # Step 6: Restart server (if not dry run)
  if (!dry_run) {
    cat("\n[6/6] Restarting Shiny Server...\n")
    restart_server()
  } else {
    cat("\n[6/6] Skipping server restart (dry-run)\n")
  }

  # Summary
  cat("\n")
  cat("================================================================\n")
  cat("  Deployment Complete!\n")
  cat("================================================================\n\n")

  if (!dry_run) {
    cat("Application URL:", get_app_url(), "\n\n")
    cat("Next steps:\n")
    cat("  1. Visit", get_app_url(), "in your browser\n")
    cat("  2. Test all features\n")
    cat("  3. Check logs if needed\n\n")

    log_message("Deployment completed successfully")
  } else {
    cat("Dry run completed - no changes were made\n\n")
    log_message("Dry run completed")
  }

  invisible(TRUE)
}

#' Test SSH connection to server
#' @param dry_run Logical
#' @return Logical indicating success
test_ssh_connection <- function(dry_run = FALSE) {
  log_message("Testing SSH connection")

  # Build SSH command (allow password authentication)
  ssh_cmd <- sprintf('ssh -o ConnectTimeout=10 %s "echo Connection successful"',
                     get_server_connection())

  result <- tryCatch({
    system(ssh_cmd, intern = TRUE, ignore.stderr = TRUE)
  }, error = function(e) {
    NULL
  })

  if (is.null(result) || length(result) == 0) {
    log_message("SSH connection failed", "ERROR")
    cat("\nSSH connection failed. Please check:\n")
    cat("  1. SHINY_SERVER_USER environment variable is set\n")
    cat("  2. SSH key-based authentication is configured\n")
    cat("  3. Server is reachable\n\n")
    cat("Try manually: ssh", get_server_connection(), "\n\n")
    return(FALSE)
  }

  cat("  ✓ SSH connection successful\n")
  return(TRUE)
}

#' Create backup on server
create_server_backup <- function() {
  log_message("Creating backup on server")

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_name <- paste0(APP_NAME, "_", timestamp, ".tar.gz")

  # Create backup directory
  cmd1 <- sprintf('ssh %s "mkdir -p %s"',
                  get_server_connection(),
                  BACKUP_DIR)
  system(cmd1)

  # Check if app exists
  cmd_check <- sprintf('ssh %s "[ -d %s ] && echo exists || echo new"',
                       get_server_connection(),
                       APP_DEPLOY_PATH)
  exists <- system(cmd_check, intern = TRUE)

  if (length(exists) > 0 && exists[1] == "exists") {
    # Create backup
    log_message("Backing up existing application")
    cmd2 <- sprintf('ssh %s "cd %s && tar -czf %s/%s %s"',
                    get_server_connection(),
                    SHINY_SERVER_ROOT,
                    BACKUP_DIR,
                    backup_name,
                    APP_NAME)
    system(cmd2)
    cat("  ✓ Backup created:", backup_name, "\n")
    log_message(paste("Backup created:", backup_name))

    # Clean old backups (keep last 5)
    cmd3 <- sprintf('ssh %s "cd %s && ls -t | tail -n +6 | xargs -r rm"',
                    get_server_connection(),
                    BACKUP_DIR)
    system(cmd3)
  } else {
    cat("  ℹ No existing application to backup (first deployment)\n")
    log_message("No existing application to backup")
  }
}

#' Deploy files using rsync
#' @param dry_run Logical
deploy_files_rsync <- function(dry_run = FALSE) {
  log_message("Deploying files with rsync")

  # Check if rsync is available
  rsync_check <- tryCatch({
    system("rsync --version", intern = TRUE, ignore.stderr = TRUE)
  }, error = function(e) NULL)

  if (is.null(rsync_check)) {
    log_message("rsync not available, falling back to manual copy", "WARN")
    deploy_files_manual(dry_run)
    return(invisible(FALSE))
  }

  # Create app directory on server
  cmd_mkdir <- sprintf('ssh %s "mkdir -p %s"',
                       get_server_connection(),
                       APP_DEPLOY_PATH)
  system(cmd_mkdir)

  # Build exclude options
  exclude_opts <- paste(paste0("--exclude='", EXCLUDE_PATTERNS, "'"), collapse = " ")

  # Build rsync command
  dry_run_flag <- if (dry_run) "--dry-run" else ""

  rsync_cmd <- sprintf(
    'rsync -avz --progress --delete %s %s -e "ssh -p %d" ./ %s:%s/',
    dry_run_flag,
    exclude_opts,
    SERVER_PORT,
    get_server_connection(),
    APP_DEPLOY_PATH
  )

  cat("  Executing rsync...\n")
  log_message(paste("Command:", rsync_cmd))

  # Execute rsync
  result <- system(rsync_cmd)

  if (result == 0) {
    if (!dry_run) {
      cat("  ✓ Files deployed successfully\n")
      log_message("Files deployed successfully")
    } else {
      cat("  ✓ Dry run completed (no files transferred)\n")
    }
    return(invisible(TRUE))
  } else {
    log_message("rsync failed", "ERROR")
    stop("File deployment failed")
  }
}

#' Deploy files manually (fallback when rsync not available)
#' @param dry_run Logical
deploy_files_manual <- function(dry_run = FALSE) {
  log_message("Using manual file copy (SCP)")

  if (dry_run) {
    cat("  Would copy files using SCP\n")
    return(invisible(TRUE))
  }

  # Create app directory on server
  cat("  Creating directory on server...\n")
  mkdir_cmd <- sprintf('ssh %s "mkdir -p %s"',
                       get_server_connection(),
                       APP_DEPLOY_PATH)
  system(mkdir_cmd)

  # Copy each file individually
  cat("  Copying files to server...\n")
  for (file in FILES_TO_DEPLOY) {
    if (file.exists(file)) {
      # Convert Windows path to Unix-style for scp
      file_unix <- gsub("\\\\", "/", file)

      scp_cmd <- sprintf('scp "%s" %s:%s/',
                         file_unix,
                         get_server_connection(),
                         APP_DEPLOY_PATH)

      result <- system(scp_cmd, ignore.stdout = TRUE, ignore.stderr = FALSE)

      if (result == 0) {
        cat("    ✓", file, "\n")
      } else {
        cat("    ✗", file, "(failed)\n")
        log_message(paste("Failed to copy:", file), "WARN")
      }
    } else {
      cat("    ⚠", file, "(not found)\n")
      log_message(paste("File not found:", file), "WARN")
    }
  }

  cat("  ✓ Files deployed\n")
  log_message("Files deployed using SCP")
}

#' Install R packages on server
install_server_packages <- function() {
  log_message("Installing R packages on server")

  packages_str <- paste(sprintf("'%s'", REQUIRED_PACKAGES), collapse = ", ")

  install_script <- paste0(
    "packages <- c(", packages_str, "); ",
    "new_packages <- packages[!(packages %in% installed.packages()[,'Package'])]; ",
    "if(length(new_packages)) { ",
    "cat('Installing packages:', paste(new_packages, collapse=', '), '\\n'); ",
    "install.packages(new_packages, repos='https://cloud.r-project.org'); ",
    "cat('Installation complete\\n'); ",
    "} else { ",
    "cat('All required packages already installed\\n'); ",
    "}"
  )

  cmd <- sprintf('ssh %s "Rscript -e \\"%s\\""',
                 get_server_connection(),
                 gsub('"', '\\\\"', install_script))

  result <- system(cmd)

  if (result == 0) {
    cat("  ✓ Packages installed\n")
    log_message("Packages installed successfully")
  } else {
    log_message("Package installation had warnings", "WARN")
    cat("  ⚠ Package installation completed with warnings\n")
  }
}

#' Restart Shiny Server
restart_server <- function() {
  log_message("Restarting Shiny Server")

  # Try systemctl first
  cmd1 <- sprintf('ssh %s "sudo systemctl restart shiny-server 2>/dev/null && echo success || echo failed"',
                  get_server_connection())

  result <- system(cmd1, intern = TRUE)

  if (length(result) > 0 && grepl("success", result[1])) {
    cat("  ✓ Shiny Server restarted (systemctl)\n")
    log_message("Server restarted successfully")
  } else {
    # Try service command
    cmd2 <- sprintf('ssh %s "sudo service shiny-server restart 2>/dev/null && echo success || echo failed"',
                    get_server_connection())

    result2 <- system(cmd2, intern = TRUE)

    if (length(result2) > 0 && grepl("success", result2[1])) {
      cat("  ✓ Shiny Server restarted (service)\n")
      log_message("Server restarted successfully")
    } else {
      cat("  ⚠ Could not restart Shiny Server automatically\n")
      cat("    You may need to restart manually:\n")
      cat("    ssh", get_server_connection(), "\n")
      cat("    sudo systemctl restart shiny-server\n")
      log_message("Could not restart server automatically", "WARN")
    }
  }

  # Wait for server to start
  cat("  Waiting for server to start...\n")
  Sys.sleep(5)
}

# ==============================================================================
# CONVENIENCE FUNCTIONS
# ==============================================================================

#' Run dry-run deployment
dry_run_deployment <- function() {
  deploy_to_server(dry_run = TRUE, no_backup = TRUE)
}

#' Quick deployment (no backup)
quick_deploy <- function() {
  deploy_to_server(dry_run = FALSE, no_backup = TRUE)
}

# ==============================================================================
# INTERACTIVE MODE
# ==============================================================================

if (interactive()) {
  cat("\n")
  cat("EcoNeTool Deployment Script Loaded\n")
  cat("===================================\n\n")
  cat("Available functions:\n")
  cat("  deploy_to_server()       - Full deployment with backup\n")
  cat("  dry_run_deployment()     - Test deployment (no changes)\n")
  cat("  quick_deploy()           - Deploy without backup\n")
  cat("  validate_config()        - Check configuration\n")
  cat("  test_ssh_connection()    - Test server connection\n\n")

  cat("Configuration:\n")
  cat("  Server:  ", SERVER_HOST, "\n")
  cat("  User:    ", SERVER_USER, "\n")
  cat("  App:     ", APP_NAME, "\n")
  cat("  URL:     ", get_app_url(), "\n\n")

  if (SERVER_USER == "your_username") {
    cat("⚠ WARNING: SERVER_USER not configured!\n")
    cat("Set environment variable: Sys.setenv(SHINY_SERVER_USER='your_username')\n\n")
  }

  cat("To deploy, run: deploy_to_server()\n\n")
}

# ==============================================================================
# COMMAND LINE MODE
# ==============================================================================

# If running from command line (Rscript)
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) > 0 && args[1] == "--help") {
    cat("EcoNeTool Deployment Script\n\n")
    cat("Usage: Rscript deploy.R [--dry-run] [--no-backup]\n\n")
    cat("Options:\n")
    cat("  --dry-run      Show what would be deployed without making changes\n")
    cat("  --no-backup    Skip backup creation\n")
    cat("  --help         Show this help message\n\n")
    quit(status = 0)
  }

  dry_run <- "--dry-run" %in% args
  no_backup <- "--no-backup" %in% args

  deploy_to_server(dry_run = dry_run, no_backup = no_backup)
}
