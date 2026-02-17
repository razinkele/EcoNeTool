# Scripts Directory

Utility scripts for EcoNeTool setup and maintenance.

---

## create_regional_euseamap_files.R

**Purpose:** Splits the large EUSeaMap_2025.gdb into 5 optimized regional .gpkg files

**Performance Gain:** 10-20x faster habitat data loading

**Usage:**
```r
# From R console
source("scripts/create_regional_euseamap_files.R")

# From command line
Rscript scripts/create_regional_euseamap_files.R
```

**Or use the wrapper scripts:**
- Windows: `create_regional_files.bat`
- Linux/Mac: `./create_regional_files.sh`

**What it does:**
1. Loads full EUSeaMap_2025.gdb (~30-60 seconds)
2. Splits into 5 regional subsets:
   - Baltic Sea
   - North Sea
   - Atlantic
   - Arctic
   - Mediterranean
3. Saves as separate .gpkg files in `data/EUSeaMap_2025/regional/`
4. Validates all output files

**Output:**
```
data/EUSeaMap_2025/regional/
├── EUSeaMap_baltic.gpkg          (~25 MB)
├── EUSeaMap_north_sea.gpkg       (~30 MB)
├── EUSeaMap_atlantic.gpkg        (~40 MB)
├── EUSeaMap_arctic.gpkg          (~15 MB)
└── EUSeaMap_mediterranean.gpkg   (~20 MB)
```

**Requirements:**
- R with sf package installed
- EUSeaMap_2025.gdb in `data/EUSeaMap_2025/`
- ~1 GB free disk space during processing
- ~2 minutes runtime

**Documentation:**
See `docs/REGIONAL_FILES_SETUP.md` for complete guide.

---

## Future Scripts

This directory will contain additional utility scripts for:
- Data validation
- Metaweb conversion tools
- Batch processing utilities
- Deployment helpers

---

**Note:** All scripts are designed to be run from the project root directory.
