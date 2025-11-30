#!/bin/bash
# Quick fix script to clean up and manually deploy EcoNeTool

set -e

# Configuration
APP_DIR="/srv/shiny-server/EcoNeTool"
SOURCE_DIR="/home/razinka/OneDrive/HORIZON_EUROPE/MARBEFES/Traits/Networks/EcoNeTool"
BACKUP_DIR="/srv/shiny-server/backups/EcoNeTool"

echo "=================================================="
echo "EcoNeTool Manual Deployment Fix"
echo "=================================================="
echo ""

# Step 1: Clean up wrong files in app directory
echo "Step 1: Cleaning up app directory..."
cd "$APP_DIR"
rm -f *.tar.gz
rm -f test_app_server.R
echo "✓ Removed tar.gz backups and test files"

# Step 2: Create proper backup directory
echo ""
echo "Step 2: Creating proper backup directory..."
mkdir -p "$BACKUP_DIR"
echo "✓ Backup directory created: $BACKUP_DIR"

# Step 3: Move existing backups to correct location
echo ""
echo "Step 3: Moving backups to correct location..."
if ls /srv/shiny-server/backups/*.tar.gz 1> /dev/null 2>&1; then
    mv /srv/shiny-server/backups/*.tar.gz "$BACKUP_DIR/" 2>/dev/null || true
    echo "✓ Moved backups to proper directory"
else
    echo "✓ No backups to move"
fi

# Step 4: Copy required files
echo ""
echo "Step 4: Copying application files..."
cd "$SOURCE_DIR"

FILES=(
    "app.R"
    "plotfw.R"
    "BalticFW.Rdata"
    "README.md"
    "BS4DASH_README.md"
    "QUICK_START_BS4DASH.md"
    "LAUNCH_GUIDE.md"
    "IMPROVEMENTS.md"
    "FIXES_APPLIED.md"
    "CHANGELOG_v2.1.md"
)

for file in "${FILES[@]}"; do
    if [ -f "$file" ]; then
        cp "$file" "$APP_DIR/"
        echo "  ✓ Copied: $file"
    else
        echo "  ⚠ Missing: $file"
    fi
done

# Copy examples directory
if [ -d "examples" ]; then
    cp -r "examples" "$APP_DIR/"
    echo "  ✓ Copied: examples/ directory"
else
    echo "  ⚠ Missing: examples/ directory"
fi

# Step 5: Set correct permissions
echo ""
echo "Step 5: Setting permissions..."
chown -R shiny:shiny "$APP_DIR"
chmod -R 755 "$APP_DIR"
chmod 644 "$APP_DIR"/*.R "$APP_DIR"/*.Rdata 2>/dev/null || true
echo "✓ Permissions set (owner: shiny:shiny, mode: 755/644)"

# Step 6: Verify deployment
echo ""
echo "Step 6: Verifying deployment..."
cd "$APP_DIR"
echo "Files in $APP_DIR:"
ls -lh
echo ""

# Check for app.R
if [ -f "$APP_DIR/app.R" ]; then
    echo "✓ app.R exists"
else
    echo "✗ app.R is missing!"
    exit 1
fi

# Step 7: Restart Shiny Server
echo ""
echo "Step 7: Restarting Shiny Server..."
systemctl restart shiny-server
echo "✓ Shiny Server restarted"

echo ""
echo "=================================================="
echo "Deployment Fix Complete!"
echo "=================================================="
echo ""
echo "Application URL: http://laguna.ku.lt:3838/EcoNeTool/"
echo ""
echo "Next steps:"
echo "  1. Visit the URL above to test the app"
echo "  2. If you see errors, check logs:"
echo "     sudo tail -f /var/log/shiny-server.log"
echo ""
