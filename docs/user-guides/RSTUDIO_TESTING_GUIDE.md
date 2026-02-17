# RStudio Testing Guide - Phase C Refactoring

**Purpose:** Test the refactored EcoNeTool app in RStudio Desktop
**Expected Outcome:** App launches successfully, all 10 tabs work correctly
**Time Required:** 15-20 minutes

## Prerequisites

✅ RStudio Desktop installed
✅ R version 4.4.1 (or compatible)
✅ All required packages installed (verified: 47/47 checks passed)
✅ Working directory set to EcoNeTool project folder

---

## Step 1: Open RStudio

1. Launch **RStudio Desktop**
2. Navigate to File → Open Project (or Open File)
3. Browse to your project directory:
   ```
   C:\Users\DELL\OneDrive - ku.lt\HORIZON_EUROPE\MARBEFES\Traits\Networks\EcoNeTool
   ```
4. If you have an `.Rproj` file, open that. Otherwise, just set the working directory:
   ```r
   setwd("C:/Users/DELL/OneDrive - ku.lt/HORIZON_EUROPE/MARBEFES/Traits/Networks/EcoNeTool")
   ```

---

## Step 2: Run Component Tests

**Test the refactored components systematically:**

1. In RStudio, open the file `test_app_rstudio.R`
   - File → Open File → `test_app_rstudio.R`

2. Click the **"Source"** button (top right of editor pane)
   - Alternative: Press `Ctrl+Shift+S` (Windows) or `Cmd+Shift+S` (Mac)

3. Watch the Console output. You should see:
   ```
   ================================================================================
   EcoNeTool - RStudio Test Script
   ================================================================================

   Testing Phase A, B, C Refactored Components...

   1. Testing core packages...
      ✓ shiny
      ✓ bs4Dash
      ✓ igraph
      ✓ fluxweb
      ✓ MASS

   2. Testing configuration (Phase A)...
      ✓ R/config.R loaded
      ✓ COLOR_SCHEME: orange, darkgrey, blue, green, cyan
      ✓ Found 5 metawebs

   3. Testing function files (Phase B)...
      ✓ trophic_levels.R
      ✓ network_visualization.R
      ✓ topological_metrics.R
      ✓ flux_calculations.R
      ✓ keystoneness.R
      ✓ metaweb_core.R
      ✓ metaweb_io.R
      ✓ spatial_analysis.R

   4. Testing UI files (Phase C)...
      ✓ dashboard_ui.R
      ✓ import_ui.R
      ✓ network_ui.R
      ✓ topological_metrics_ui.R
      ✓ biomass_ui.R
      ✓ fluxes_ui.R
      ✓ keystoneness_ui.R
      ✓ dataeditor_ui.R
      ✓ metaweb_ui.R
      ✓ spatial_ui.R

   5. Testing data loading (Phase A)...
      ✓ R/data_loading.R loaded
      ✓ Network has 34 species
      ✓ Network has 207 trophic links

   6. Testing main app.R structure...
      ✓ app.R syntax valid
      ✓ app.R loaded
      ✓ UI object created
      ✓ Server function defined

   ================================================================================
   ✅ ALL TESTS PASSED
   ================================================================================
   ```

4. **If all tests pass:** Proceed to Step 3
5. **If any test fails:** Note the error and see Troubleshooting section below

---

## Step 3: Launch the App

1. In RStudio, open the file `app.R`
   - File → Open File → `app.R`

2. Look for the **"Run App"** button
   - Top right corner of the editor pane
   - Green triangle icon with "Run App" text

3. Click **"Run App"**

4. **Expected behavior:**
   - New window/browser tab opens
   - EcoNeTool dashboard appears
   - Sidebar shows 10 menu items
   - No error messages in RStudio Console

5. **Alternative launch methods:**
   ```r
   # In RStudio Console:
   shiny::runApp("app.R")

   # Or with specific options:
   shiny::runApp("app.R", launch.browser = TRUE, port = 3838)
   ```

---

## Step 4: Systematic Feature Testing

Use the **Testing Checklist** to verify all features work correctly.

### Tab 1: Dashboard ✓
- [ ] Opens without errors
- [ ] Shows 4 value boxes (34 Taxa, 207 Links, 5 Groups, 1979-2016)
- [ ] Displays functional groups with colored dots
- [ ] Quick Start guide visible

### Tab 2: Data Import ✓
- [ ] Opens without errors
- [ ] 5 sub-tabs present (General, ECOPATH Native, ECOPATH CSV, Format Guide, Examples)
- [ ] File upload interfaces visible
- [ ] Format guide displays correctly

### Tab 3: Food Web Network ✓
- [ ] Opens without errors
- [ ] 2 sub-tabs (Interactive Network, Adjacency Matrix)
- [ ] Network plot displays
- [ ] Species colored by functional groups
- [ ] Adjacency matrix shows

### Tab 4: Topological Metrics ✓
- [ ] Opens without errors
- [ ] Metrics table displays
- [ ] Shows: S, C, G, V, ShortPath, TL, Omni
- [ ] Values are reasonable

### Tab 5: Biomass Analysis ✓
- [ ] Opens without errors
- [ ] Biomass distribution plot visible
- [ ] Node-weighted indicators calculated
- [ ] Shows: nwC, nwG, nwV, nwTL

### Tab 6: Energy Fluxes ✓
- [ ] Opens without errors
- [ ] 3 sub-tabs (Flux Network, Flux Heatmap, Flux Indicators)
- [ ] Flux calculations complete
- [ ] Plots display correctly
- [ ] No calculation errors

### Tab 7: Keystoneness Analysis ✓
- [ ] Opens without errors
- [ ] MTI matrix calculated
- [ ] Keystoneness index table displays
- [ ] Species classified (Keystone/Dominant/Rare)
- [ ] Plot shows keystoneness vs biomass

### Tab 8: Internal Data Editor ✓
- [ ] Opens without errors
- [ ] Data table displays
- [ ] Editing interface functional
- [ ] Can view species information

### Tab 9: Metaweb Manager (Phase 2) ✓
- [ ] Opens without errors
- [ ] 5 sub-tabs visible
- [ ] Library shows 5 metawebs
- [ ] Load metaweb button works
- [ ] Metaweb visualization displays

### Tab 10: Spatial Analysis (Phase 1) ✓
- [ ] Opens without errors
- [ ] 5 sub-tabs (Grid Setup, Species Data, Network Extraction, Metrics, Visualization)
- [ ] Grid parameters configurable
- [ ] Species upload interface present
- [ ] Workflow steps in order

---

## Step 5: Performance Check

Monitor RStudio Console for:

1. **No errors** (red text)
2. **No warnings** about missing functions
3. **Normal memory usage**
4. **Responsive UI** (tabs switch quickly)

---

## Troubleshooting

### Issue 1: "Object not found" errors

**Symptom:**
```
Error: object 'dashboard_ui' not found
```

**Solution:**
1. Check that all UI files are in `R/ui/` directory
2. Verify files were sourced correctly
3. Re-run `test_app_rstudio.R` to identify which file failed
4. Check file names match source statements in app.R (lines 30-39)

---

### Issue 2: "Function not found" errors

**Symptom:**
```
Error: could not find function "create_metaweb"
```

**Solution:**
1. Check that all function files are in `R/functions/` directory
2. Verify function files were sourced correctly
3. Run in Console:
   ```r
   source("R/functions/metaweb_core.R")
   ```
4. Check if function exists:
   ```r
   exists("create_metaweb")
   ```

---

### Issue 3: Package loading errors

**Symptom:**
```
Error: package 'sf' is not available
```

**Solution:**
1. Install missing package:
   ```r
   install.packages("sf")
   ```
2. For spatial packages on Windows:
   ```r
   install.packages("sf", type = "binary")
   install.packages("sp", type = "binary")
   ```
3. Restart R session: Session → Restart R

---

### Issue 4: Data file not found

**Symptom:**
```
Error: Data file not found: BalticFW.Rdata
```

**Solution:**
1. Verify working directory:
   ```r
   getwd()
   ```
2. Check file exists:
   ```r
   file.exists("BalticFW.Rdata")
   ```
3. Set correct working directory if needed

---

### Issue 5: Metaweb files missing

**Symptom:**
```
Warning: Missing metaweb files: baltic_kortsch2021
```

**Solution:**
This is just a warning. The app will work, but Phase 2 features need metaweb files.
To fix:
1. Check `metawebs/` directory exists
2. Ensure .rds files are present
3. Files should be in subdirectories: `metawebs/baltic/`, `metawebs/arctic/`, etc.

---

## Performance Expectations

**Load Time:**
- Initial app load: 5-10 seconds (loading packages and data)
- Subsequent tab switches: < 1 second
- Complex calculations (Energy Fluxes, Keystoneness): 2-5 seconds

**Memory Usage:**
- Expected: 200-500 MB RAM
- With spatial analysis: up to 1 GB RAM
- Alert if > 2 GB (possible memory leak)

**Responsiveness:**
- UI should remain responsive during calculations
- Progress indicators should appear for long operations
- No freezing or hanging

---

## Success Criteria

Your app is working correctly if:

✅ All 10 tabs open without errors
✅ All visualizations display
✅ All calculations complete successfully
✅ No red error messages in Console
✅ App remains responsive
✅ Can switch between tabs smoothly
✅ Data loads correctly (34 species, 207 links)

---

## After Testing

### If Everything Works ✅

Congratulations! Your Phase C refactoring is successful. Next steps:

1. **Document any observations:**
   - Performance notes
   - UI responsiveness
   - Any warnings (non-critical)

2. **Consider deployment:**
   - Upload to Shiny Server
   - Deploy to RStudio Connect
   - Publish to shinyapps.io

3. **Optional: Proceed to Phase D:**
   - Convert to Shiny modules
   - 8-12 hour effort
   - Further improve architecture

### If You Encounter Issues ❌

1. **Note the exact error message**
2. **Check which step failed in test_app_rstudio.R**
3. **Review Troubleshooting section**
4. **Check the error occurs in pre-refactored code too:**
   ```r
   # Test with backup
   source("app.R.backup_phase_c")
   ```
5. **Report if issue is related to refactoring**

---

## Additional Testing Tips

### Console Commands for Debugging

```r
# Check loaded packages
(.packages())

# Check loaded functions
ls()

# Check specific object exists
exists("dashboard_ui")

# View function source
dashboard_ui

# Test function manually
test_ui <- dashboard_ui()

# Check data
str(net)
str(info)

# Memory usage
pryr::mem_used()  # If pryr package installed
```

### RStudio Features to Use

1. **Environment pane:** Check loaded objects
2. **Files pane:** Verify file structure
3. **Console:** Run test commands
4. **Viewer pane:** See Shiny app
5. **Help pane:** Look up function documentation

---

## Testing Checklist Summary

Print or use this quick checklist:

```
[ ] RStudio opened
[ ] Working directory set
[ ] test_app_rstudio.R sourced
[ ] All component tests passed
[ ] app.R opened
[ ] "Run App" clicked
[ ] App launched in browser/window
[ ] Dashboard tab ✓
[ ] Data Import tab ✓
[ ] Food Web Network tab ✓
[ ] Topological Metrics tab ✓
[ ] Biomass Analysis tab ✓
[ ] Energy Fluxes tab ✓
[ ] Keystoneness Analysis tab ✓
[ ] Internal Data Editor tab ✓
[ ] Metaweb Manager tab ✓
[ ] Spatial Analysis tab ✓
[ ] No errors in Console
[ ] App responsive
[ ] Performance acceptable
```

---

## Contact & Support

**If you need help:**

1. Review `ENVIRONMENT_FIX_GUIDE.md` for detailed solutions
2. Check `PHASE_C_TEST_REPORT.md` for validation evidence
3. Consult `PHASE_C_COMPLETE.md` for refactoring details

**RStudio Community:**
- https://community.rstudio.com/
- Tag: [shiny], [r], [bs4dash]

---

## Conclusion

Testing in RStudio is the recommended approach for Windows systems. The refactored code has been thoroughly validated (47/47 checks) and should work perfectly in RStudio.

**Expected Outcome:** ✅ App launches successfully, all features work

Good luck with your testing! 🚀
