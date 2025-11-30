#!/bin/bash
# Diagnostic script to find EcoNeTool startup error
# Run on server: ssh razinka@laguna.ku.lt 'bash -s' < diagnose_server_error.sh

echo "======================================"
echo "EcoNeTool Diagnostic"
echo "======================================"
echo ""

echo "1. Checking file permissions..."
ls -la /srv/shiny-server/EcoNeTool/ | head -15

echo ""
echo "2. Checking if data file exists and is readable..."
if [ -f /srv/shiny-server/EcoNeTool/BalticFW.Rdata ]; then
  echo "✓ BalticFW.Rdata exists"
  file /srv/shiny-server/EcoNeTool/BalticFW.Rdata
else
  echo "✗ BalticFW.Rdata NOT FOUND"
fi

echo ""
echo "3. Checking latest error logs..."
echo "Recent app-specific logs:"
ls -lt /var/log/shiny-server/EcoNeTool*.log 2>/dev/null | head -3

echo ""
echo "4. Trying to read latest error log (may need sudo)..."
LATEST_LOG=$(ls -t /var/log/shiny-server/EcoNeTool*.log 2>/dev/null | head -1)
if [ -n "$LATEST_LOG" ]; then
  echo "Latest log: $LATEST_LOG"
  sudo cat "$LATEST_LOG" 2>/dev/null || echo "Need sudo password to read log"
else
  echo "No EcoNeTool logs found"
fi

echo ""
echo "5. Testing if R can load the app..."
cd /srv/shiny-server/EcoNeTool
R --vanilla --quiet -e "source('app.R')" 2>&1 | head -20

echo ""
echo "======================================"
echo "If you see errors above, that's the problem!"
echo "======================================"
