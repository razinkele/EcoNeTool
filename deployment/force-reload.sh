#!/bin/bash
# Force reload Shiny Server app - clears all caches

set -e

echo "========================================"
echo "Force Reload EcoNeTool"
echo "========================================"
echo ""

# Check if running as root
if [ "$EUID" -ne 0 ]; then
    echo "❌ Error: This script must be run with sudo"
    echo "Usage: sudo ./force-reload.sh"
    exit 1
fi

APP_DIR=$(dirname "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)")

echo "1. Stopping Shiny Server..."
systemctl stop shiny-server

echo "2. Clearing Shiny bookmarks and cache..."
rm -rf /var/lib/shiny-server/bookmarks/*
rm -rf /srv/shiny-server/EcoNeTool/.Rhistory
rm -rf /srv/shiny-server/EcoNeTool/.RData

echo "3. Re-deploying files..."

# Copy with error checking
echo "   Copying app.R..."
if cp -v "$APP_DIR/app.R" /srv/shiny-server/EcoNeTool/; then
    echo "   ✓ app.R copied successfully"
else
    echo "   ✗ Failed to copy app.R"
    exit 1
fi

echo "   Copying functions.R..."
if cp -v "$APP_DIR/functions.R" /srv/shiny-server/EcoNeTool/; then
    echo "   ✓ functions.R copied successfully"
else
    echo "   ✗ Failed to copy functions.R"
    exit 1
fi

echo "   Copying BalticFW.Rdata..."
if cp -v "$APP_DIR/BalticFW.Rdata" /srv/shiny-server/EcoNeTool/; then
    echo "   ✓ BalticFW.Rdata copied successfully"
else
    echo "   ✗ Failed to copy BalticFW.Rdata"
    exit 1
fi

if [ -f "$APP_DIR/run_app.R" ]; then
    echo "   Copying run_app.R (optional)..."
    if cp -v "$APP_DIR/run_app.R" /srv/shiny-server/EcoNeTool/; then
        echo "   ✓ run_app.R copied successfully"
    else
        echo "   ⚠ Failed to copy run_app.R (optional)"
    fi
fi

echo "4. Setting permissions..."
chown -R shiny:shiny /srv/shiny-server/EcoNeTool

echo "5. Starting Shiny Server..."
systemctl start shiny-server

echo "6. Waiting for server to start..."
sleep 3

if systemctl is-active --quiet shiny-server; then
    echo ""
    echo "✅ Force reload complete!"
    echo ""
    echo "Next steps:"
    echo "  1. Clear your browser cache (Ctrl+Shift+R or Cmd+Shift+R)"
    echo "  2. Or open in incognito/private window"
    echo "  3. Reload the app page"
    echo ""
else
    echo "❌ Error: Shiny Server failed to start"
    echo "Check logs: sudo journalctl -u shiny-server -n 50"
    exit 1
fi
