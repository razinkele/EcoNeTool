# Fix: "could not find function 'trophiclevels'" Error

## Date
2025-11-27

## Issue Summary

**Error Message:**
```
In the flux-weighted network Error creating flux network plot, could not find function 'trophiclevels'
```

**Location:** Energy Fluxes tab → Flux-weighted Network plot

**Severity:** Critical - prevented flux network visualization from working

---

## Root Cause Analysis

### The Problem

The application was experiencing a function scoping issue due to conflicting function definitions:

1. **BalticFW.Rdata contents:**
   - Contains BOTH data (net, info) AND functions (trophiclevels, plotfw, fluxind)
   - These functions were from an older version of the code

2. **app.R structure:**
   - Defined its own versions of `trophiclevels()` and `plotfw()` functions
   - Then loaded BalticFW.Rdata with `load(DATA_FILE)`
   - This **overwrote** the app.R function definitions with older versions from the data file

3. **The conflict:**
   - app.R's trophiclevels() was defined first (lines 120-162)
   - Loading BalticFW.Rdata replaced it with the data file's version
   - The data file's version had environment/scoping issues
   - When fluxind() tried to call trophiclevels(), it couldn't find it or got the wrong version

### Why It Failed

The old approach loaded BalticFW.Rdata directly into the global environment:
```r
load(DATA_FILE)  # Overwrites any existing objects with same names!
```

This caused:
- Function definitions in app.R to be overwritten
- Scoping issues with function calls
- Unpredictable behavior depending on load order

---

## The Solution

### Key Changes

#### 1. Load Data into Separate Environment

**Before:**
```r
load(DATA_FILE)
# This overwrote app.R's functions!
```

**After:**
```r
data_env <- new.env()
load(DATA_FILE, envir = data_env)

# Extract ONLY data objects, not functions
net <<- data_env$net
info <<- data_env$info
```

**Benefits:**
- App functions remain intact
- No naming conflicts
- Predictable behavior
- Data file can contain functions without issues

#### 2. Added fluxind() Function to app.R

Since we're no longer loading functions from BalticFW.Rdata, we needed to implement `fluxind()` ourselves.

**Added to app.R (lines 299-383):**
```r
#' Calculate link-weighted flux indicators
#'
#' Computes Shannon diversity-based indicators from an energy flux matrix.
fluxind <- function(fluxes, loop = FALSE) {
  # Implementation based on Bersier et al. (2002)
  # Calculates lwC, lwG, lwV from flux matrix
  ...
}
```

#### 3. Applied Same Fix to Data Upload Handler

Updated the RData file upload handler to use the same approach:
```r
# Load into separate environment
env <- new.env()
load(file_path, envir = env)

# Extract only data objects
net <<- env$net
info <<- env$info
```

---

## Files Modified

### app.R

**Lines 47-68:** Data loading section
- Changed from direct `load()` to environment-based loading
- Extract only net and info objects
- Removed fluxind environment fix (no longer needed)

**Lines 299-383:** Added fluxind() function
- Complete implementation of link-weighted flux indicators
- Documented with roxygen2 comments
- Based on Bersier et al. (2002) methodology

**Lines 1119-1144:** Data upload handler
- Updated RData upload to use environment-based loading
- Ensures uploaded data doesn't overwrite app functions

---

## Testing

### Test Results

Created comprehensive test suite: `test_final_fix.R`

**All tests passed:**
```
✓ App trophiclevels() function works
✓ App fluxind() function works
✓ Flux network plot generates successfully
✓ Flux indicators calculate correctly
```

### Test Scenarios Covered

1. **Function isolation:** Verified app functions are used, not data file functions
2. **Trophic levels calculation:** Confirmed trophiclevels() works correctly
3. **Flux indicators:** Verified fluxind() produces correct results
4. **Flux network plot:** The critical test - plot generates without errors
5. **End-to-end workflow:** Complete flux analysis pipeline works

---

## Technical Details

### Why This Approach is Better

#### Old Approach Problems:
- ❌ Function definitions could be overwritten
- ❌ Unpredictable behavior with multiple data loads
- ❌ Tight coupling between data file and code
- ❌ Hard to maintain and debug
- ❌ User-uploaded data could break app if it contained functions

#### New Approach Benefits:
- ✅ Complete separation of data and code
- ✅ App functions always take precedence
- ✅ Predictable, reproducible behavior
- ✅ Works with any RData file structure
- ✅ Easy to maintain and extend
- ✅ User uploads can't break app functionality

### Function Ownership

**App-defined functions** (always used):
- `trophiclevels(net)` - Calculate trophic levels
- `plotfw(...)` - Plot food web network
- `fluxind(fluxes, loop)` - Calculate flux indicators
- `get_topological_indicators(net)` - Topological metrics
- `get_node_weighted_indicators(net, info)` - Biomass-weighted metrics
- `get_fluxweb_results(net, info)` - Energy flux calculations

**Data file provides** (only data loaded):
- `net` - igraph network object
- `info` - data frame with species information

---

## Impact

### What's Fixed
✅ Flux-weighted network plot now renders correctly
✅ Flux indicators display properly
✅ No more "trophiclevels not found" errors
✅ All Energy Fluxes tab features functional

### What's Improved
✅ Better code architecture (separation of concerns)
✅ More maintainable codebase
✅ Safer data loading mechanism
✅ Better support for user-uploaded data
✅ Clearer function ownership

### Backward Compatibility
✅ Works with existing BalticFW.Rdata
✅ Works with user-uploaded RData files
✅ No changes needed to existing workflows
✅ All existing features remain functional

---

## Related Code

### fluxind() Implementation Details

The `fluxind()` function calculates Shannon diversity-based indicators:

**Metrics calculated:**
- **lwC** - Link-weighted connectance (effective link density)
- **lwG** - Link-weighted generality (effective number of prey per predator)
- **lwV** - Link-weighted vulnerability (effective number of predators per prey)

**Method:**
1. Calculate Shannon diversity indices for inflows and outflows
2. Compute effective numbers of prey/predators per species
3. Weight by species' contribution to total flux
4. Aggregate to whole-network metrics

**Reference:**
Bersier, L. F., Banašek-Richter, C., & Cattin, M. F. (2002). Quantitative descriptors of food-web matrices. Ecology, 83(9), 2394-2407.

---

## Verification Steps

### For Users

If you encounter the error again:

1. **Clear R environment:**
   ```r
   rm(list = ls())
   ```

2. **Restart Shiny app:**
   ```r
   shiny::runApp()
   ```

3. **Test flux network plot:**
   - Navigate to Energy Fluxes tab
   - Check that "Flux-weighted Network" plot displays
   - Verify no error messages appear

4. **Check flux indicators:**
   - Scroll to "Link-weighted Flux Indicators" section
   - Verify lwC, lwG, lwV values are displayed

### For Developers

Run comprehensive test:
```bash
Rscript test_final_fix.R
```

Expected output:
```
✓ App trophiclevels():   ✓ PASS
✓ App fluxind():         ✓ PASS
✓ Flux Network Plot:     ✓ PASS
✓ Flux Indicators:       ✓ PASS
```

---

## Lessons Learned

### Best Practices Established

1. **Never load external RData directly into GlobalEnv**
   - Always use `load(..., envir = new.env())`
   - Selectively extract needed objects

2. **Separate data from code**
   - Data files should contain only data
   - Functions should be defined in source code

3. **Make function ownership explicit**
   - Document which functions come from where
   - Avoid naming conflicts

4. **Test function scoping**
   - Verify functions are found when called
   - Check function versions are correct

### Code Review Checklist

For future data loading:
- [ ] Load into separate environment
- [ ] Extract only needed objects
- [ ] Don't overwrite existing functions
- [ ] Document data file contents
- [ ] Test with both default and user data

---

## Future Improvements

### Recommended Enhancements

1. **Data file validation:**
   - Add checks for required objects
   - Warn if data file contains functions
   - Document expected data file structure

2. **Function testing:**
   - Add unit tests for all helper functions
   - Test with various network structures
   - Validate edge cases

3. **Documentation:**
   - Create data file format specification
   - Document function dependencies
   - Add examples for custom data

4. **Error messages:**
   - More informative error messages
   - Guidance for fixing common issues
   - Link to documentation

---

## Summary

**Problem:** Function scoping issue caused "trophiclevels not found" error

**Cause:** BalticFW.Rdata overwrote app.R function definitions

**Solution:**
1. Load data into separate environment
2. Extract only data objects (net, info)
3. Added fluxind() to app.R
4. Use app's own function definitions

**Result:** All flux analysis features work correctly

**Status:** ✅ **FIXED AND TESTED**

---

**Fixed by:** Claude Code
**Date:** 2025-11-27
**Version:** 2.1.1
**Test Status:** All tests passing
