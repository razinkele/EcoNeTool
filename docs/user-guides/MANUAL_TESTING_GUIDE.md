# Manual Testing Guide - EcoNeTool

**Version:** Post Phase 1 & 2 Optimizations
**Date:** 2025-12-08
**Purpose:** Verify all changes work correctly in real-world usage

---

## Quick Start

### Starting the Application

**Option 1: Using run_app.R (Recommended)**
```r
# From R Console or RStudio:
source("run_app.R")
```

**Option 2: Direct app.R**
```r
# From R Console or RStudio:
shiny::runApp()
```

**Option 3: Command Line**
```bash
# From Windows Command Prompt or PowerShell:
Rscript run_app.R
```

**Expected Output:**
```
Loading required package: shiny
Listening on http://127.0.0.1:XXXX
```

The app should open automatically in your default browser. If not, copy the URL and paste it into your browser.

---

## Testing Checklist

### ✅ Phase 0: Basic Startup (2 minutes)

**Goal:** Verify app starts without errors

- [ ] **Step 1:** Start the app using one of the methods above
- [ ] **Step 2:** Check R console for errors
  - ✅ Expected: No red error messages
  - ⚠️ OK: Yellow warnings about package versions (harmless)
  - ❌ Problem: Red errors or app doesn't start
- [ ] **Step 3:** Confirm app opens in browser
  - ✅ Expected: Dashboard appears with sidebar menu
  - ❌ Problem: Blank page or error message

**Initial Observations:**
- App title: "EcoNeTool - Ecological Interaction Network Explorer"
- Sidebar menu visible with 12 menu items
- Default data loaded (Gulf of Riga)
- Dashboard shows 5 value boxes with statistics

---

### ✅ Phase 1: Dashboard & Navigation (3 minutes)

**Goal:** Verify basic UI and navigation work

#### Test 1.1: Dashboard Value Boxes
- [ ] Navigate to: **Dashboard** tab
- [ ] Verify 5 value boxes display:
  - Taxa/Species (should show ~34)
  - Trophic Links (should show ~207)
  - Functional Groups (should show ~5)
  - Time Period (should show "1979-2016")
  - Location (should show "Gulf of Riga, Baltic Sea")

#### Test 1.2: Sidebar Navigation
- [ ] Click through each menu item:
  - [ ] Dashboard
  - [ ] Data Import
  - [ ] Food Web Network
  - [ ] Topological Metrics
  - [ ] Biomass Analysis
  - [ ] Energy Fluxes ⚡ **(Phase 1 optimization - should be faster)**
  - [ ] Keystoneness Analysis ⚡ **(Phase 1 optimization - should be faster)**
  - [ ] Internal Data Editor
  - [ ] Metaweb Manager
  - [ ] Spatial Analysis
  - [ ] EcoBase Connection
  - [ ] ECOPATH/ECOSIM

**Expected:** All tabs load without errors, smooth transitions

---

### ✅ Phase 2: Network Visualization (5 minutes)

**Goal:** Test basic network display

#### Test 2.1: Static Network View
- [ ] Navigate to: **Food Web Network** tab
- [ ] Verify network visualization appears
  - Nodes: Colored circles representing species
  - Edges: Arrows showing predator-prey relationships
  - Interactive: Can drag nodes, zoom, pan

#### Test 2.2: Network Statistics
- [ ] Check "Network Statistics" box shows:
  - Total nodes
  - Total edges
  - Network density
  - Average degree

**Expected:** Network renders clearly, statistics match Dashboard values

---

### ✅ Phase 3: CRITICAL - Flux Analysis (Phase 1 Optimization) (10 minutes)

**Goal:** Verify Phase 1 reactive caching works correctly

#### Test 3.1: Initial Load (First Time)
- [ ] Navigate to: **Energy Fluxes** tab
- [ ] **Watch console for progress message:** "Calculating energy fluxes..."
- [ ] Time the load (should be ~5-6 seconds for first load)
- [ ] Verify THREE outputs appear:
  - [ ] Flux heatmap (log-scaled energy flows)
  - [ ] Flux network visualization
  - [ ] Flux indicators (text output with metrics)

**Expected Performance:**
- First load: ~5-6 seconds (with progress indicator)
- All three outputs should appear together

#### Test 3.2: Cached Load (Immediate)
- [ ] Switch to a different tab (e.g., Dashboard)
- [ ] Switch back to **Energy Fluxes** tab
- [ ] **Time the load** (should be instant - < 0.5 seconds)
- [ ] Verify all three outputs reappear instantly

**✅ SUCCESS CRITERIA:**
- Second load is 90%+ faster than first load
- No "Calculating energy fluxes..." message on second load
- Outputs appear instantly from cache

**❌ IF SLOW ON SECOND LOAD:**
- Phase 1 caching not working - report issue

#### Test 3.3: Stress Test (Multiple Switches)
- [ ] Switch between tabs 5 times rapidly:
  - Energy Fluxes → Dashboard → Energy Fluxes → Network → Energy Fluxes...
- [ ] Verify instant loading each time you return to Energy Fluxes

**Expected:** Instant response every time after initial load

---

### ✅ Phase 4: CRITICAL - Keystoneness Analysis (Phase 1 Optimization) (10 minutes)

**Goal:** Verify Phase 1 keystoneness caching works

#### Test 4.1: Initial Load
- [ ] Navigate to: **Keystoneness Analysis** tab
- [ ] **Watch console for progress:** "Calculating keystoneness indices..."
- [ ] Time the load (should be ~4-5 seconds for first load)
- [ ] Verify TWO outputs appear:
  - [ ] Keystoneness table (species with keystoneness indices)
  - [ ] Keystoneness vs Biomass plot (log scale)

**Expected:**
- First load: ~4-5 seconds
- Table shows all species with keystoneness values
- Plot shows species colored by status (Keystone/Dominant/Rare/Undefined)

#### Test 4.2: Cached Load
- [ ] Switch to another tab
- [ ] Switch back to **Keystoneness Analysis**
- [ ] **Time the load** (should be instant)

**✅ SUCCESS CRITERIA:**
- Second load: < 0.5 seconds (instant)
- No recalculation, immediate display
- Table and plot appear instantly

#### Test 4.3: Table Interaction
- [ ] Click column headers to sort
- [ ] Use search box to filter species
- [ ] Change page length (10/15/25 rows)

**Expected:** Table is interactive and responsive

---

### ✅ Phase 5: ECOPATH Import (Phase 2 Organization) (15 minutes)

**Goal:** Test ECOPATH import with updated file paths

#### Test 5.1: Import Tab Access
- [ ] Navigate to: **Data Import** tab
- [ ] Locate: "ECOPATH Native Database Import" section
- [ ] Verify: File upload button present

#### Test 5.2: Test with Example File
If you have an ECOPATH database file (.mdb, .accdb, .ewemdb):

- [ ] Click "Browse" and select the file
- [ ] **Watch console** for import progress messages
- [ ] Verify metadata preview appears:
  - Number of groups
  - Number of diet links
  - Model information

**Expected:**
- Import completes without errors
- Metadata displayed correctly
- Console shows no red errors

**Note:** ECOPATH import uses the reorganized `R/functions/ecopath_import.R` file. If this works, Phase 2 path updates are correct.

#### Test 5.3: ECOPATH/ECOSIM Module (if Rpath installed)
- [ ] Navigate to: **ECOPATH/ECOSIM** tab
- [ ] Check if Rpath package is detected
  - If installed: Module interface appears
  - If not installed: Installation instructions shown

If Rpath is installed:
- [ ] Try converting ECOPATH data to Rpath format
- [ ] Verify no errors about missing files
- [ ] **Watch console** - should NOT see repeated "loading" messages

**Expected:**
- Clean module loading (Phase 2 memory leak fix)
- No redundant file loading messages
- Functions accessible from `R/functions/rpath_integration.R`

---

### ✅ Phase 6: Topological Metrics (5 minutes)

**Goal:** Basic functionality check

- [ ] Navigate to: **Topological Metrics** tab
- [ ] Verify outputs:
  - [ ] Degree distribution histogram
  - [ ] Topological indicators (text output)
  - [ ] Node-weighted indicators (text output)

**Expected:**
- All outputs render
- Metrics calculated correctly
- No errors in console

---

### ✅ Phase 7: Biomass Analysis (5 minutes)

**Goal:** Check biomass visualizations

- [ ] Navigate to: **Biomass Analysis** tab
- [ ] Verify THREE outputs:
  - [ ] Biomass boxplot (by functional group)
  - [ ] Biomass barplot (percentage distribution)
  - [ ] Biomass network visualization

**Expected:**
- All three visualizations render
- Colors match functional groups
- Network shows nodes sized by biomass

---

### ✅ Phase 8: Data Editor (5 minutes)

**Goal:** Test internal data editing

- [ ] Navigate to: **Internal Data Editor** tab
- [ ] Verify editable table appears
- [ ] Try editing a value:
  - Click on a cell
  - Change the value
  - Press Enter
- [ ] Verify "Update Network" button appears
- [ ] Click "Update Network"

**Expected:**
- Table is editable
- Changes can be saved
- Network updates without crashing

---

### ✅ Phase 9: Memory Leak Test (10 minutes)

**Goal:** Verify Phase 1 & 2 memory improvements

#### Test 9.1: Baseline Memory
- [ ] Open Task Manager (Ctrl+Shift+Esc)
- [ ] Find "R" or "Rscript" process
- [ ] Note initial memory usage: _______ MB

#### Test 9.2: Flux Tab Stress Test
- [ ] Navigate to: Energy Fluxes
- [ ] Wait for calculation
- [ ] Switch away and back 10 times
- [ ] Check memory usage: _______ MB
- [ ] **Calculate increase:** _______ MB

**✅ SUCCESS:** Memory increase < 50 MB
**⚠️ WARNING:** Memory increase 50-100 MB
**❌ PROBLEM:** Memory increase > 100 MB (leak detected)

#### Test 9.3: Keystoneness Stress Test
- [ ] Navigate to: Keystoneness Analysis
- [ ] Wait for calculation
- [ ] Switch away and back 10 times
- [ ] Check memory usage: _______ MB

**Expected:** Minimal memory increase (caching should prevent accumulation)

#### Test 9.4: ECOPATH Module Test (if Rpath installed)
If you tested ECOPATH import earlier:
- [ ] Navigate to: ECOPATH/ECOSIM tab
- [ ] Note memory: _______ MB
- [ ] Convert data 3 times (click convert button repeatedly)
- [ ] Check memory: _______ MB

**✅ SUCCESS:** Memory stable or increases < 30 MB
**❌ PROBLEM:** Memory increases significantly with each conversion

---

### ✅ Phase 10: Performance Verification (10 minutes)

**Goal:** Confirm Phase 1 speed improvements

#### Test 10.1: Timed Operations

| Operation | First Load (seconds) | Cached Load (seconds) | Improvement |
|-----------|---------------------|----------------------|-------------|
| Energy Fluxes | _____ | _____ | _____ % |
| Keystoneness | _____ | _____ | _____ % |
| Network Viz | _____ | _____ | N/A |
| Topological | _____ | _____ | N/A |

**Target Performance:**
- Energy Fluxes: First ~5s, Cached <0.5s (90%+ faster)
- Keystoneness: First ~4s, Cached <0.5s (90%+ faster)

#### Test 10.2: Responsiveness
- [ ] Click through all tabs quickly
- [ ] Resize browser window
- [ ] Zoom in/out on network visualizations
- [ ] Interact with tables (sort, search, page)

**Expected:** Smooth, responsive UI with no lag

---

## Reporting Results

### If Everything Works ✅

**Congratulations!** The reorganization is successful. You can:
1. Proceed with Phase 3 (more performance improvements)
2. Deploy to production as-is
3. Share with collaborators

### If Issues Found ❌

**Please report with this information:**

1. **What were you testing?**
   - Phase/Test number
   - Specific feature

2. **What did you expect?**
   - Expected behavior

3. **What actually happened?**
   - Actual behavior
   - Error messages (copy from console)
   - Screenshots if relevant

4. **Can you reproduce it?**
   - Yes/No
   - Steps to reproduce

5. **System information:**
   ```r
   # Run in R console:
   sessionInfo()
   ```

---

## Known Issues (Expected)

### Harmless Warnings (Can Ignore)
- "package 'X' was built under R version Y.Y.Y" - Version mismatch, safe to ignore
- Yellow warning messages in console - Usually safe

### Expected Behaviors
- First load of Flux/Keystoneness tabs: Slower (calculating)
- Subsequent loads: Instant (cached)
- Network stabilization: Takes 2-3 seconds to settle
- Large networks (>100 nodes): May render slowly

### Not Issues
- Some tabs slower than others: Expected (complexity varies)
- Memory gradually increases: Normal R behavior (up to ~500 MB is fine)
- Network nodes move on hover: Intended interactive feature

---

## Quick Reference Card

### Fast Test (10 minutes)
1. Start app
2. Navigate to Energy Fluxes (wait for load)
3. Navigate away and back (should be instant)
4. Navigate to Keystoneness (wait for load)
5. Navigate away and back (should be instant)
6. If both are instant on return: ✅ Phase 1 working

### Full Test (60 minutes)
Complete all 10 phases above

### Memory Test (5 minutes)
1. Note memory in Task Manager
2. Navigate to Energy Fluxes
3. Switch away/back 10 times
4. Check memory increase
5. Should be < 50 MB increase

---

## Tips for Testing

1. **Keep R Console Visible**
   - Watch for error messages
   - Progress indicators show Phase 1 optimizations working

2. **Use Browser DevTools (Optional)**
   - Press F12
   - Console tab shows JavaScript errors (if any)
   - Network tab shows loading times

3. **Test with Real Data (Optional)**
   - Import your own network data
   - Verify import still works (Phase 2 file moves)

4. **Compare Before/After (If Possible)**
   - If you have an old version, compare speed
   - Flux/Keystoneness should be noticeably faster

---

## Expected Test Duration

| Test Phase | Time Required |
|------------|---------------|
| Basic Startup | 2 minutes |
| Dashboard & Navigation | 3 minutes |
| Network Visualization | 5 minutes |
| Flux Analysis (Critical) | 10 minutes |
| Keystoneness (Critical) | 10 minutes |
| ECOPATH Import | 15 minutes |
| Topological Metrics | 5 minutes |
| Biomass Analysis | 5 minutes |
| Data Editor | 5 minutes |
| Memory Leak Test | 10 minutes |
| Performance Verification | 10 minutes |
| **TOTAL** | **80 minutes** |

**Minimum test:** 30 minutes (Phases 0-5)
**Recommended test:** 60 minutes (All phases)

---

## After Testing

Once you complete your manual testing:

1. **Share results** - Report any issues or confirm everything works
2. **Decide next step:**
   - Proceed with Phase 3 (performance enhancements)
   - Deploy to production
   - Additional testing

---

**Happy Testing!** 🧪

The app should feel faster (Phase 1) and the code should be cleaner (Phase 2), but functionality should be identical to before.
