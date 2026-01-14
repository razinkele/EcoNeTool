#!/bin/bash
# ============================================================================
# EcoNeTool - Quick Deployment Script
# ============================================================================
#
# This script automates the deployment of EcoNeTool to a Shiny Server
#
# Usage:
#   sudo ./deploy.sh [--docker|--shiny-server]
#
# ============================================================================

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging configuration
LOG_DIR="/var/log/econetool"
LOG_FILE="${LOG_DIR}/deployment.log"

# Create log directory if it doesn't exist
if [ "$EUID" -eq 0 ]; then
    mkdir -p "$LOG_DIR"
    chmod 755 "$LOG_DIR"
fi

# Function to log messages
log_message() {
    local level="$1"
    shift
    local message="$@"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    if [ -w "$LOG_FILE" ] || [ "$EUID" -eq 0 ]; then
        echo "[$timestamp] [$level] $message" >> "$LOG_FILE" 2>/dev/null || true
    fi
}

# Function to print status messages
print_status() {
    echo -e "${BLUE}==>${NC} $1"
    log_message "INFO" "$1"
}

print_success() {
    echo -e "${GREEN}✓${NC} $1"
    log_message "SUCCESS" "$1"
}

print_error() {
    echo -e "${RED}✗${NC} $1"
    log_message "ERROR" "$1"
}

print_warning() {
    echo -e "${YELLOW}⚠${NC} $1"
    log_message "WARNING" "$1"
}

# Get the absolute path to the deployment directory
DEPLOY_DIR="$(cd "$(dirname "$0")" && pwd)"
APP_DIR="$(cd "$(dirname "$0")/.." && pwd)"

# Default deployment method
DEPLOY_METHOD="${1:---shiny-server}"

echo "================================================================================"
echo " EcoNeTool - Deployment Script"
echo "================================================================================"
echo ""
log_message "DEPLOY" "Deployment started by user: $(whoami)"
log_message "DEPLOY" "Deployment method: $DEPLOY_METHOD"
log_message "DEPLOY" "Application directory: $APP_DIR"

# Run pre-deployment checks
print_status "Running pre-deployment checks..."
if command -v Rscript &> /dev/null; then
    cd "$DEPLOY_DIR"
    if Rscript pre-deploy-check.R; then
        print_success "Pre-deployment checks passed"
    else
        print_warning "Pre-deployment checks found issues"
        read -p "Continue with deployment anyway? (y/N) " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            print_error "Deployment cancelled"
            exit 1
        fi
    fi
else
    print_warning "Rscript not found, skipping pre-deployment checks"
fi

# Check if running as root for shiny-server deployment
if [[ "$DEPLOY_METHOD" == "--shiny-server" ]] && [[ $EUID -ne 0 ]]; then
   echo -e "${RED}ERROR: This script must be run as root for Shiny Server deployment${NC}"
   echo "Please run: sudo ./deploy.sh"
   exit 1
fi

# ============================================================================
# Docker Deployment
# ============================================================================

deploy_docker() {
    print_status "Starting Docker deployment..."

    # Check if Docker is installed
    if ! command -v docker &> /dev/null; then
        print_error "Docker is not installed"
        echo "Please install Docker first: https://docs.docker.com/get-docker/"
        exit 1
    fi

    # Check if Docker Compose is installed
    if ! command -v docker-compose &> /dev/null; then
        print_error "Docker Compose is not installed"
        echo "Please install Docker Compose first"
        exit 1
    fi

    print_success "Docker and Docker Compose found"

    # Build Docker image
    print_status "Building Docker image..."
    docker-compose build

    # Start containers
    print_status "Starting containers..."
    docker-compose up -d

    # Wait for container to be ready
    print_status "Waiting for application to start..."
    sleep 10

    # Check if container is running
    if docker-compose ps | grep -q "Up"; then
        print_success "Docker deployment successful!"
        echo ""
        echo "Application is running at: http://localhost:3838"
        echo ""
        echo "Useful commands:"
        echo "  - View logs: docker-compose logs -f"
        echo "  - Stop app: docker-compose down"
        echo "  - Restart: docker-compose restart"
    else
        print_error "Container failed to start"
        echo "Check logs with: docker-compose logs"
        exit 1
    fi
}

# ============================================================================
# Shiny Server Deployment
# ============================================================================

deploy_shiny_server() {
    print_status "Starting Shiny Server deployment..."

    # Ensure app directory exists and is writable
    if [ ! -d /srv/shiny-server/EcoNeTool ]; then
        print_status "Creating deployment directory..."
        mkdir -p /srv/shiny-server/EcoNeTool
        chmod 775 /srv/shiny-server/EcoNeTool
    fi
    print_status "Deployment directory status:"
    ls -ld /srv/shiny-server/EcoNeTool

    # Remove old deployment contents
    print_status "Removing old deployment contents..."
    rm -rf /srv/shiny-server/EcoNeTool/*

    # List of critical files and directories to copy
    CRITICAL_ITEMS=(
        "app.R"
        "BalticFW.Rdata"
        "run_app.R"
        "README.md"
        "LICENSE"
        "VERSION"
        "CHANGELOG.md"
        "R"
        "R/functions"
        "www"
        "examples"
        "metawebs"
        "output"
        "docs"
        "scripts"
        "cache"
        "archive"
        "tests"
    )

    ERRORS=()
    WARNINGS=()

    for ITEM in "${CRITICAL_ITEMS[@]}"; do
        SRC="$APP_DIR/$ITEM"
        DEST="/srv/shiny-server/EcoNeTool/"
        if [ -e "$SRC" ]; then
            print_status "Copying $ITEM..."
            if [ -d "$SRC" ]; then
                cp -rv "$SRC" "$DEST" 2>err.log
            else
                cp -vf "$SRC" "$DEST" 2>err.log
            fi
            COPY_STATUS=$?
            if [ $COPY_STATUS -eq 0 ]; then
                print_success "Copied: $ITEM"
                log_message "DEPLOY" "Copied $ITEM from $APP_DIR"
            else
                print_error "Failed to copy $ITEM"
                log_message "ERROR" "Failed to copy $ITEM from $APP_DIR"
                print_error "Copy error details:"
                cat err.log
                ERRORS+=("$ITEM: $(cat err.log)")
            fi
            rm -f err.log
        else
            print_warning "$ITEM not found in source directory"
            log_message "WARNING" "$ITEM not found at $SRC"
            WARNINGS+=("$ITEM not found")
        fi
    done

    # Summary of errors and warnings
    if [ ${#ERRORS[@]} -gt 0 ]; then
        echo -e "${RED}Deployment encountered errors:${NC}"
        for ERR in "${ERRORS[@]}"; do
            echo -e "${RED}✗ $ERR${NC}"
        done
    fi
    if [ ${#WARNINGS[@]} -gt 0 ]; then
        echo -e "${YELLOW}Deployment warnings:${NC}"
        for WARN in "${WARNINGS[@]}"; do
            echo -e "${YELLOW}⚠ $WARN${NC}"
        done
    fi
    if [ ${#ERRORS[@]} -eq 0 ]; then
        print_success "All critical files and directories copied successfully"
    else
        print_error "Some critical files or directories failed to copy. See above."
        exit 1
    fi

    # Copy optional R files if they exist
    if [ -f "$APP_DIR/run_app.R" ]; then
        if cp -v "$APP_DIR/run_app.R" /srv/shiny-server/EcoNeTool/; then
            print_success "Copied: run_app.R (optional)"
            log_message "DEPLOY" "Copied run_app.R from $APP_DIR"
        else
            print_warning "Failed to copy run_app.R (optional)"
            log_message "WARNING" "Failed to copy run_app.R from $APP_DIR"
        fi
    fi

    # Copy www directory (contains images and web assets)
    if [ -d "$APP_DIR/www" ]; then
        print_status "Copying www directory..."
        if cp -rv "$APP_DIR/www" /srv/shiny-server/EcoNeTool/; then
            print_success "Copied: www directory"
            log_message "DEPLOY" "Copied www directory from $APP_DIR"
        else
            print_warning "Failed to copy www directory"
            log_message "WARNING" "Failed to copy www directory from $APP_DIR"
        fi
    fi

    # Copy examples directory (contains example datasets)
    if [ -d "$APP_DIR/examples" ]; then
        print_status "Copying examples directory..."
        if cp -rv "$APP_DIR/examples" /srv/shiny-server/EcoNeTool/; then
            print_success "Copied: examples directory"
            log_message "DEPLOY" "Copied examples directory from $APP_DIR"
        else
            print_warning "Failed to copy examples directory"
            log_message "WARNING" "Failed to copy examples directory from $APP_DIR"
        fi
    fi

    # Copy metawebs directory (contains metaweb data)
    if [ -d "$APP_DIR/metawebs" ]; then
        print_status "Copying metawebs directory..."
        if cp -rv "$APP_DIR/metawebs" /srv/shiny-server/EcoNeTool/; then
            print_success "Copied: metawebs directory"
            log_message "DEPLOY" "Copied metawebs directory from $APP_DIR"
        else
            print_warning "Failed to copy metawebs directory"
            log_message "WARNING" "Failed to copy metawebs directory from $APP_DIR"
        fi
    fi

    # Copy R directory (contains all modular application code)
    if [ -d "$APP_DIR/R" ]; then
        print_status "Copying R directory..."
        if cp -rv "$APP_DIR/R" /srv/shiny-server/EcoNeTool/; then
            print_success "Copied: R directory (config, functions, modules, UI)"
            log_message "DEPLOY" "Copied R directory from $APP_DIR"
        else
            print_error "Failed to copy R directory (CRITICAL)"
            log_message "ERROR" "Failed to copy R directory from $APP_DIR"
            exit 1
        fi
    else
        print_error "R directory not found (CRITICAL)"
        log_message "ERROR" "R directory not found at $APP_DIR/R"
        exit 1
    fi

    # Set permissions
    print_status "Setting file permissions..."
    if chown -R shiny:shiny /srv/shiny-server/EcoNeTool; then
        print_success "Permissions set"
        log_message "DEPLOY" "Set permissions for shiny:shiny"
    else
        print_error "Failed to set permissions"
        log_message "ERROR" "Failed to set permissions"
        exit 1
    fi

    print_success "Application files deployed"
    log_message "DEPLOY" "All application files deployed successfully"

    # Configure Shiny Server
    print_status "Configuring Shiny Server..."

    # Backup original config
    if [ -f /etc/shiny-server/shiny-server.conf ]; then
        cp /etc/shiny-server/shiny-server.conf /etc/shiny-server/shiny-server.conf.backup
        print_success "Original config backed up"
    fi

    # Create or copy new config
    if [ -f "$DEPLOY_DIR/shiny-server.conf" ]; then
        cp "$DEPLOY_DIR/shiny-server.conf" /etc/shiny-server/shiny-server.conf
    else
        print_status "Creating default shiny-server.conf..."
        cat > /etc/shiny-server/shiny-server.conf <<'EOF'
# Shiny Server configuration file for EcoNeTool

# Instruct Shiny Server to run applications as the user "shiny"
run_as shiny;

# Define a server that listens on port 3838
server {
  listen 3838;

  # Define a location at the base URL
  location / {
    # Host the directory of Shiny Apps stored in this directory
    site_dir /srv/shiny-server;

    # Log all Shiny output to files in this directory
    log_dir /var/log/shiny-server;

    # When a user visits the base URL rather than a particular application,
    # an index of the applications available in this directory will be shown.
    directory_index on;
  }

  # Define EcoNeTool specific location
  location /EcoNeTool {
    app_dir /srv/shiny-server/EcoNeTool;
    log_dir /var/log/shiny-server;
  }
}
EOF
    fi

    print_success "Shiny Server configured"

    # Clear Shiny Server caches
    print_status "Clearing Shiny Server caches..."
    rm -rf /var/lib/shiny-server/bookmarks/* 2>/dev/null || true
    rm -rf /srv/shiny-server/EcoNeTool/.Rhistory 2>/dev/null || true
    rm -rf /srv/shiny-server/EcoNeTool/.RData 2>/dev/null || true

    # Restart Shiny Server
    print_status "Restarting Shiny Server..."
    systemctl stop shiny-server
    sleep 2
    systemctl start shiny-server

    # Wait for server to start
    sleep 5

    # Check if Shiny Server is running
    if systemctl is-active --quiet shiny-server; then
        print_success "Deployment successful!"
        log_message "DEPLOY" "Deployment completed successfully"
        log_message "DEPLOY" "Application running at: http://$(hostname -I | awk '{print $1}'):3838/EcoNeTool"
        echo ""
        echo "Application is running at: http://$(hostname -I | awk '{print $1}'):3838/EcoNeTool"
        echo ""
        echo "⚠️  IMPORTANT: Clear your browser cache to see changes!"
        echo "   - Hard reload: Ctrl+Shift+R (Linux/Windows) or Cmd+Shift+R (Mac)"
        echo "   - Or open in incognito/private browsing mode"
        echo ""
        echo "Useful commands:"
        echo "  - Check status: sudo systemctl status shiny-server"
        echo "  - View logs: sudo tail -f /var/log/shiny-server.log"
        echo "  - Deployment log: sudo tail -f $LOG_FILE"
        echo "  - Force reload: cd deployment && sudo ./force-reload.sh"
        echo "  - Verify deployment: cd deployment && sudo ./verify-deployment.sh"
    else
        print_error "Shiny Server failed to start"
        log_message "DEPLOY" "Deployment failed - Shiny Server not running"
        echo "Check logs with: sudo journalctl -u shiny-server -n 50"
        exit 1
    fi
}

# ============================================================================
# Main execution
# ============================================================================

case "$DEPLOY_METHOD" in
    --docker)
        deploy_docker
        ;;
    --shiny-server)
        deploy_shiny_server
        ;;
    *)
        echo "Usage: $0 [--docker|--shiny-server]"
        echo ""
        echo "Deployment methods:"
        echo "  --docker         Deploy using Docker"
        echo "  --shiny-server   Deploy to Shiny Server (default)"
        exit 1
        ;;
esac

echo ""
echo "================================================================================"
echo " Deployment Complete!"
echo "================================================================================"
echo ""
