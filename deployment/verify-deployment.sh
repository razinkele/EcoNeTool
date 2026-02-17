#!/bin/bash
# Verify what's actually deployed on the Shiny Server

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo "========================================"
echo "EcoNeTool Deployment Verification"
echo "========================================"
echo ""

# Check if running as root
if [ "$EUID" -ne 0 ]; then
    echo -e "${RED}❌ Error: This script must be run with sudo${NC}"
    echo "Usage: sudo ./verify-deployment.sh"
    exit 1
fi

DEPLOY_DIR="/srv/shiny-server/EcoNeTool"
SOURCE_DIR="$(cd "$(dirname "$0")/.." && pwd)"

echo -e "${BLUE}═══════════════════════════════════════════${NC}"
echo -e "${BLUE}1. SHINY SERVER STATUS${NC}"
echo -e "${BLUE}═══════════════════════════════════════════${NC}"
if systemctl is-active --quiet shiny-server; then
    echo -e "   ${GREEN}✅ Shiny Server is running${NC}"
    echo -e "   ${GREEN}✅ Status: $(systemctl is-active shiny-server)${NC}"
else
    echo -e "   ${RED}❌ Shiny Server is NOT running${NC}"
    echo "   Run: sudo systemctl start shiny-server"
fi
echo ""

echo -e "${BLUE}═══════════════════════════════════════════${NC}"
echo -e "${BLUE}2. DEPLOYMENT DIRECTORY${NC}"
echo -e "${BLUE}═══════════════════════════════════════════${NC}"
if [ -d "$DEPLOY_DIR" ]; then
    echo -e "   ${GREEN}✅ Deployment directory exists${NC}"
    echo "   Location: $DEPLOY_DIR"
    echo ""
    echo "   Files in deployment directory:"
    ls -lh "$DEPLOY_DIR" | tail -n +2 | while read line; do
        echo "   $line"
    done
    echo ""
    echo "   Disk usage:"
    du -sh "$DEPLOY_DIR"
else
    echo -e "   ${RED}❌ Deployment directory does NOT exist: $DEPLOY_DIR${NC}"
    exit 1
fi
echo ""

echo -e "${BLUE}═══════════════════════════════════════════${NC}"
echo -e "${BLUE}3. REQUIRED FILES CHECK${NC}"
echo -e "${BLUE}═══════════════════════════════════════════${NC}"

# Check app.R
if [ -f "$DEPLOY_DIR/app.R" ]; then
    echo -e "   ${GREEN}✅ app.R exists${NC}"
    echo "      Size: $(stat -c%s "$DEPLOY_DIR/app.R" | numfmt --to=iec-i --suffix=B --format="%.2f")"
    echo "      Modified: $(stat -c "%y" "$DEPLOY_DIR/app.R" | cut -d'.' -f1)"
else
    echo -e "   ${RED}❌ app.R is MISSING${NC}"
fi

# Check functions.R
if [ -f "$DEPLOY_DIR/functions.R" ]; then
    echo -e "   ${GREEN}✅ functions.R exists${NC}"
    echo "      Size: $(stat -c%s "$DEPLOY_DIR/functions.R" | numfmt --to=iec-i --suffix=B --format="%.2f")"
    echo "      Modified: $(stat -c "%y" "$DEPLOY_DIR/functions.R" | cut -d'.' -f1)"
else
    echo -e "   ${RED}❌ functions.R is MISSING${NC}"
fi

# Check BalticFW.Rdata
if [ -f "$DEPLOY_DIR/BalticFW.Rdata" ]; then
    echo -e "   ${GREEN}✅ BalticFW.Rdata exists${NC}"
    echo "      Size: $(stat -c%s "$DEPLOY_DIR/BalticFW.Rdata" | numfmt --to=iec-i --suffix=B --format="%.2f")"
    echo "      Modified: $(stat -c "%y" "$DEPLOY_DIR/BalticFW.Rdata" | cut -d'.' -f1)"
else
    echo -e "   ${RED}❌ BalticFW.Rdata is MISSING${NC}"
fi

# Check run_app.R (optional)
if [ -f "$DEPLOY_DIR/run_app.R" ]; then
    echo -e "   ${GREEN}✅ run_app.R exists (optional)${NC}"
    echo "      Size: $(stat -c%s "$DEPLOY_DIR/run_app.R" | numfmt --to=iec-i --suffix=B --format="%.2f")"
    echo "      Modified: $(stat -c "%y" "$DEPLOY_DIR/run_app.R" | cut -d'.' -f1)"
fi
echo ""

echo -e "${BLUE}═══════════════════════════════════════════${NC}"
echo -e "${BLUE}4. VERSION CHECK${NC}"
echo -e "${BLUE}═══════════════════════════════════════════${NC}"

# Check for old plotfw.R reference
if [ -f "$DEPLOY_DIR/app.R" ]; then
    if grep -q "source.*plotfw\.R" "$DEPLOY_DIR/app.R"; then
        echo -e "   ${RED}❌ OLD VERSION DETECTED${NC}"
        echo "   app.R still references plotfw.R (file doesn't exist)"
        echo "   Lines with plotfw.R reference:"
        grep -n "source.*plotfw\.R" "$DEPLOY_DIR/app.R" | sed 's/^/      /'
    else
        echo -e "   ${GREEN}✅ No plotfw.R reference (correct)${NC}"
    fi

    # Check for functions.R sourcing
    if grep -q "source.*functions\.R" "$DEPLOY_DIR/app.R"; then
        echo -e "   ${GREEN}✅ app.R sources functions.R (v1.0.0+)${NC}"
    else
        echo -e "   ${YELLOW}⚠️  Warning: app.R doesn't source functions.R${NC}"
    fi

    # Check for plotfw extraction from data (old method)
    if grep -q "plotfw <<- data_env\\\$plotfw" "$DEPLOY_DIR/app.R"; then
        echo -e "   ${YELLOW}⚠️  Old method: Extracting plotfw from BalticFW.Rdata${NC}"
    fi
fi
echo ""

echo -e "${BLUE}═══════════════════════════════════════════${NC}"
echo -e "${BLUE}5. SOURCE vs DEPLOYED COMPARISON${NC}"
echo -e "${BLUE}═══════════════════════════════════════════${NC}"

# Compare app.R
if [ -f "$SOURCE_DIR/app.R" ] && [ -f "$DEPLOY_DIR/app.R" ]; then
    SOURCE_TIME=$(stat -c %Y "$SOURCE_DIR/app.R")
    DEPLOY_TIME=$(stat -c %Y "$DEPLOY_DIR/app.R")

    echo "   app.R:"
    echo "      Source: $(stat -c "%y" "$SOURCE_DIR/app.R" | cut -d'.' -f1)"
    echo "      Deployed: $(stat -c "%y" "$DEPLOY_DIR/app.R" | cut -d'.' -f1)"

    if [ $SOURCE_TIME -gt $DEPLOY_TIME ]; then
        echo -e "      ${YELLOW}⚠️  Source is NEWER than deployed version${NC}"
        SECONDS_DIFF=$((SOURCE_TIME - DEPLOY_TIME))
        MINUTES_DIFF=$((SECONDS_DIFF / 60))
        echo "      Difference: $MINUTES_DIFF minutes"
    elif [ $SOURCE_TIME -eq $DEPLOY_TIME ]; then
        echo -e "      ${GREEN}✅ Versions match (same timestamp)${NC}"
    else
        echo -e "      ${GREEN}✅ Deployed version is up to date${NC}"
    fi
fi
echo ""

# Compare functions.R
if [ -f "$SOURCE_DIR/functions.R" ] && [ -f "$DEPLOY_DIR/functions.R" ]; then
    SOURCE_TIME=$(stat -c %Y "$SOURCE_DIR/functions.R")
    DEPLOY_TIME=$(stat -c %Y "$DEPLOY_DIR/functions.R")

    echo "   functions.R:"
    echo "      Source: $(stat -c "%y" "$SOURCE_DIR/functions.R" | cut -d'.' -f1)"
    echo "      Deployed: $(stat -c "%y" "$DEPLOY_DIR/functions.R" | cut -d'.' -f1)"

    if [ $SOURCE_TIME -gt $DEPLOY_TIME ]; then
        echo -e "      ${YELLOW}⚠️  Source is NEWER than deployed version${NC}"
        SECONDS_DIFF=$((SOURCE_TIME - DEPLOY_TIME))
        MINUTES_DIFF=$((SECONDS_DIFF / 60))
        echo "      Difference: $MINUTES_DIFF minutes"
    elif [ $SOURCE_TIME -eq $DEPLOY_TIME ]; then
        echo -e "      ${GREEN}✅ Versions match (same timestamp)${NC}"
    else
        echo -e "      ${GREEN}✅ Deployed version is up to date${NC}"
    fi
fi
echo ""

echo -e "${BLUE}═══════════════════════════════════════════${NC}"
echo -e "${BLUE}6. FILE PERMISSIONS${NC}"
echo -e "${BLUE}═══════════════════════════════════════════${NC}"

if [ -f "$DEPLOY_DIR/app.R" ]; then
    OWNER=$(stat -c "%U:%G" "$DEPLOY_DIR/app.R")
    PERMS=$(stat -c "%a" "$DEPLOY_DIR/app.R")

    echo "   Owner: $OWNER"
    echo "   Permissions: $PERMS"

    if [ "$OWNER" = "shiny:shiny" ]; then
        echo -e "   ${GREEN}✅ Correct ownership (shiny:shiny)${NC}"
    else
        echo -e "   ${YELLOW}⚠️  Unexpected ownership (expected shiny:shiny)${NC}"
    fi
fi
echo ""

echo -e "${BLUE}═══════════════════════════════════════════${NC}"
echo -e "${BLUE}7. RECENT SHINY SERVER LOGS${NC}"
echo -e "${BLUE}═══════════════════════════════════════════${NC}"

echo "   Checking for recent errors..."
ERRORS=$(journalctl -u shiny-server --since "5 minutes ago" --no-pager -n 50 | grep -i "error\|fail\|fatal" | head -5)

if [ -n "$ERRORS" ]; then
    echo -e "   ${YELLOW}⚠️  Recent errors found:${NC}"
    echo "$ERRORS" | sed 's/^/      /'
else
    echo -e "   ${GREEN}✅ No recent errors${NC}"
fi
echo ""

echo -e "${BLUE}═══════════════════════════════════════════${NC}"
echo -e "${BLUE}8. DEPLOYMENT LOG${NC}"
echo -e "${BLUE}═══════════════════════════════════════════${NC}"

if [ -f "/var/log/econetool/deployment.log" ]; then
    echo -e "   ${GREEN}✅ Deployment log exists${NC}"
    echo "   Last 5 deployment entries:"
    tail -5 /var/log/econetool/deployment.log | sed 's/^/      /'
else
    echo -e "   ${YELLOW}⚠️  No deployment log found${NC}"
fi
echo ""

echo -e "${BLUE}═══════════════════════════════════════════${NC}"
echo -e "${BLUE}SUMMARY${NC}"
echo -e "${BLUE}═══════════════════════════════════════════${NC}"

# Count issues
ISSUES=0

if ! systemctl is-active --quiet shiny-server; then
    ISSUES=$((ISSUES + 1))
fi

if [ ! -f "$DEPLOY_DIR/app.R" ] || [ ! -f "$DEPLOY_DIR/functions.R" ] || [ ! -f "$DEPLOY_DIR/BalticFW.Rdata" ]; then
    ISSUES=$((ISSUES + 1))
fi

if [ -f "$SOURCE_DIR/app.R" ] && [ -f "$DEPLOY_DIR/app.R" ]; then
    SOURCE_TIME=$(stat -c %Y "$SOURCE_DIR/app.R")
    DEPLOY_TIME=$(stat -c %Y "$DEPLOY_DIR/app.R")
    if [ $SOURCE_TIME -gt $DEPLOY_TIME ]; then
        ISSUES=$((ISSUES + 1))
    fi
fi

if [ $ISSUES -eq 0 ]; then
    echo -e "${GREEN}✅ No issues detected. Deployment appears healthy.${NC}"
else
    echo -e "${YELLOW}⚠️  $ISSUES issue(s) detected. Review details above.${NC}"
    echo ""
    echo "Recommended actions:"
    if [ ! -f "$DEPLOY_DIR/functions.R" ]; then
        echo "   - Run: sudo ./force-reload.sh"
    fi
    if [ -f "$SOURCE_DIR/app.R" ] && [ -f "$DEPLOY_DIR/app.R" ]; then
        SOURCE_TIME=$(stat -c %Y "$SOURCE_DIR/app.R")
        DEPLOY_TIME=$(stat -c %Y "$DEPLOY_DIR/app.R")
        if [ $SOURCE_TIME -gt $DEPLOY_TIME ]; then
            echo "   - Source is newer. Run: sudo ./deploy.sh --shiny-server"
        fi
    fi
fi

echo ""
echo "========================================"
echo "Verification complete"
echo "========================================"
