#!/bin/bash
# Create Regional EUSeaMap Files
# Splits the large GDB into smaller regional .gpkg files for faster loading

echo ""
echo "========================================"
echo "  EUSeaMap Regional File Creator"
echo "========================================"
echo ""
echo "This script will split EUSeaMap_2025.gdb into 5 regional files:"
echo "  - Baltic Sea"
echo "  - North Sea"
echo "  - Atlantic"
echo "  - Arctic"
echo "  - Mediterranean"
echo ""
echo "Benefits:"
echo "  - 10-20x faster loading (1-2 sec vs 15-20 sec)"
echo "  - Smaller file sizes (20-40 MB vs 500+ MB)"
echo "  - Reduced memory usage"
echo ""
echo "This is a ONE-TIME operation that takes ~2 minutes."
echo ""
read -p "Press Enter to continue or Ctrl+C to cancel..."

echo ""
echo "Running R script..."
echo ""

Rscript scripts/create_regional_euseamap_files.R

if [ $? -ne 0 ]; then
    echo ""
    echo "ERROR: Script failed!"
    echo ""
    echo "Possible issues:"
    echo "  1. EUSeaMap_2025.gdb not found in data/EUSeaMap_2025/"
    echo "  2. R or required packages not installed"
    echo "  3. Insufficient disk space"
    echo ""
    exit 1
fi

echo ""
echo "========================================"
echo "  SUCCESS!"
echo "========================================"
echo ""
echo "Regional files created in: data/EUSeaMap_2025/regional/"
echo ""
echo "The app will now use these files automatically for MUCH faster loading!"
echo ""
