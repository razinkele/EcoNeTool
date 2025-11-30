# ECOPATH Import - Success!

## Summary

Your ECOPATH database (`coast 2011-04-10 10.00.ewemdb`) has been successfully extracted and converted for use in EcoNeTool.

### Database Information
- **Original file**: coast 2011-04-10 10.00.ewemdb (2.8 MB)
- **Species/Groups**: 51
- **Diet interactions**: 2,601
- **Format**: Ecopath with Ecosim 6.x database

---

## Generated Files

Two CSV files are now ready to import into your app:

### 1. ECOPATH_Basic_Estimates.csv
Contains species/group information with:
- Group names
- Biomass (B)
- Production/Biomass ratio (P/B)
- Consumption/Biomass ratio (Q/B)
- Ecotrophic Efficiency (EE)
- Production, Consumption, etc.

**Note**: Values of `-9999` are ECOPATH placeholders for calculated/missing values. The app will handle these appropriately.

### 2. ECOPATH_Diet_Matrix.csv
Diet composition matrix (51 × 51):
- **Rows**: Prey species
- **Columns**: Predator species
- **Values**: Diet proportions (0-1)

---

## How to Import into EcoNeTool

### Method 1: Using the Shiny App

1. **Launch the app** (if not already running):
   ```R
   shiny::runApp("app.R")
   ```

2. **Navigate to "Input Food Web" tab**

3. **Find "Import ECOPATH CSV/Excel Exports" section** (right side)

4. **Upload files**:
   - Basic Estimates File → Select `ECOPATH_Basic_Estimates.csv`
   - Diet Composition Matrix → Select `ECOPATH_Diet_Matrix.csv`

5. **Click "Import Exported Files"**

6. **Verify import**:
   - Check the status message
   - Should show: "✓ SUCCESS: ECOPATH data imported!"
   - Navigate to other tabs to explore your food web

### Method 2: Direct R Import (for testing)

```R
# Read the files
basic <- read.csv("ECOPATH_Basic_Estimates.csv")
diet <- read.csv("ECOPATH_Diet_Matrix.csv")

# View the data
View(basic)
View(diet)

# Check dimensions
dim(basic)  # Should be 51 x 9
dim(diet)   # Should be 51 x 52 (prey column + 51 predators)
```

---

## Food Web Species (Sample)

The database includes 51 species/groups such as:
- Atlantic salmon (adults & juveniles)
- Baltic herring (adults & juveniles)
- Bream (adults & juveniles)
- Phytoplankton
- Mesozooplankton
- Mysids
- Macrophytobenthos
- Detritus
- ... and many more

---

## Troubleshooting

### If import fails in the app:

1. **Check file encoding**: Ensure CSV files are UTF-8
2. **Verify column names**: Should match expected format
3. **Check for special characters**: Group names should be clean
4. **Missing values**: -9999 values are normal in ECOPATH exports

### If you need to re-extract:

```bash
# Re-run extraction
Rscript extract_ecopath_windows.R

# Re-run conversion
Rscript convert_ecopath_to_app_format.R
```

---

## Technical Notes

### Windows Compatibility
This extraction used **RODBC** (Windows-native database access), which doesn't require `mdbtools`. The method:
- Reads .ewemdb files directly via Windows ODBC drivers
- Exports raw tables (EcopathGroup, EcopathDietComp)
- Converts to EcoNeTool-compatible format

### Alternative Methods (if needed)
See `WINDOWS_MDBTOOLS_GUIDE.md` for:
- WSL + mdbtools installation
- MSYS2 setup
- Online converters
- Direct ODBC access in R

---

## Next Steps

1. ✓ Extract database → **DONE**
2. ✓ Convert to app format → **DONE**
3. ⏩ **Import into app** (follow instructions above)
4. ⏩ **Explore your food web**:
   - View network visualization
   - Analyze trophic levels
   - Calculate ecosystem metrics
   - Perform keystoneness analysis
   - Run sensitivity analyses

---

## Files Created

```
✓ coast 2011-04-10 10.00_EcopathGroup.csv      (raw database export)
✓ coast 2011-04-10 10.00_EcopathDietComp.csv   (raw database export)
✓ ECOPATH_Basic_Estimates.csv                  (formatted for app)
✓ ECOPATH_Diet_Matrix.csv                      (formatted for app)
✓ extract_ecopath_windows.R                    (extraction script)
✓ convert_ecopath_to_app_format.R              (conversion script)
✓ WINDOWS_MDBTOOLS_GUIDE.md                    (alternative methods)
```

---

**Ready to import!** 🎉

Your ECOPATH food web is now ready to be imported into EcoNeTool.
