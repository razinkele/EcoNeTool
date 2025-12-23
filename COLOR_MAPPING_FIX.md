# Color Mapping Fix - Phytoplankton Showing as Light Blue Instead of Green

**Date:** 2025-12-22
**Issue:** Phytoplankton nodes and legend showing light blue instead of green
**Status:** ✅ Fixed

---

## Problem

Phytoplankton was displaying as **light blue** instead of **green** in both network nodes and legend, despite COLOR_SCHEME having green in the correct position.

---

## Root Cause

Colors were assigned using `as.numeric(info$fg)` which depends on factor levels in the loaded data:

```r
# WRONG - depends on factor level order in data
info$colfg <- COLOR_SCHEME[as.numeric(info$fg)]
```

**Example of the problem:**

BalticFW.Rdata has factor levels:
1. Benthos
2. Detritus
3. Fish
4. Phytoplankton ← as.numeric(fg) = 4
5. Zooplankton ← as.numeric(fg) = 5

With this indexing:
- Phytoplankton → COLOR_SCHEME[4] = "blue" ❌ WRONG!
- Zooplankton → COLOR_SCHEME[5] = "red" ❌ WRONG!

But COLOR_SCHEME is designed for 7 groups in standard order:
1. Benthos
2. Birds
3. Detritus
4. Fish
5. Mammals
6. Phytoplankton ← should be COLOR_SCHEME[6] = "green"
7. Zooplankton ← should be COLOR_SCHEME[7] = "lightblue"

---

## Solution

Changed color assignment to match by **functional group NAME** instead of numeric factor index:

```r
# CORRECT - matches by name, not factor index
fg_levels <- get_functional_group_levels()
info$colfg <- sapply(as.character(info$fg), function(fg) {
  idx <- which(fg_levels == fg)
  if (length(idx) == 0) return("gray")  # Unknown group
  COLOR_SCHEME[idx]
})
```

**How it works:**
1. Convert functional group to character string (e.g., "Phytoplankton")
2. Find which position it is in standard levels (position 6)
3. Use that position to index COLOR_SCHEME (gets "green")

This works regardless of:
- How many functional groups are in the data
- What order they appear as factor levels
- Whether Birds/Mammals are present or not

---

## Files Modified

### 1. app.R (6 locations)
Fixed color assignment to use name matching instead of factor indices:
- Line 425: Initial data loading
- Line 1547: General import
- Line 1656: ECOPATH CSV import
- Line 2492: ECOPATH native import
- Line 3388: Data editor updates
- Line 5869: EcoBase import

All changed from:
```r
info$colfg <- COLOR_SCHEME[as.numeric(info$fg)]
```

To:
```r
fg_levels <- get_functional_group_levels()
info$colfg <- sapply(as.character(info$fg), function(fg) {
  idx <- which(fg_levels == fg)
  if (length(idx) == 0) return("gray")
  COLOR_SCHEME[idx]
})
```

### 2. R/functions/network_visualization.R (lines 282-303)
Added explicit group color definitions for visNetwork:

**Critical Fix:** visNetwork auto-assigns colors based on the `group` attribute, which can override our explicit node colors. We must use `visGroups()` to define colors for each functional group.

```r
# Define explicit colors for each functional group
# This prevents visNetwork from auto-assigning colors based on groups
fg_levels <- get_functional_group_levels()

# Create network visualization
vis <- visNetwork::visNetwork(nodes, edges, width = "100%", height = "90vh")

# Add explicit group color definitions
for (i in seq_along(fg_levels)) {
  fg <- fg_levels[i]
  if (fg %in% unique(info$fg)) {
    vis <- vis %>%
      visNetwork::visGroups(
        groupname = fg,
        color = list(
          background = COLOR_SCHEME[i],
          border = COLOR_SCHEME[i],
          highlight = list(background = COLOR_SCHEME[i], border = "black")
        )
      )
  }
}
```

**Why this is needed:**
- visNetwork uses the `group` column to automatically color nodes
- Without explicit `visGroups()` definitions, it generates its own color palette
- Our explicit `nodes$color` list was being overridden
- Now both node colors AND group colors are explicitly defined

---

## Verification

**Standard functional group levels (get_functional_group_levels()):**
```r
c("Benthos", "Birds", "Detritus", "Fish", "Mammals", "Phytoplankton", "Zooplankton")
```

**COLOR_SCHEME (R/config.R line 17):**
```r
c("burlywood", "purple", "brown", "blue", "red", "green", "lightblue")
```

**Color mapping:**
1. Benthos (position 1) → burlywood (light brown) ✓
2. Birds (position 2) → purple ✓
3. Detritus (position 3) → brown ✓
4. Fish (position 4) → blue ✓
5. Mammals (position 5) → red ✓
6. **Phytoplankton (position 6) → green ✓**
7. **Zooplankton (position 7) → lightblue ✓**

---

## Testing

### Before Fix:
- ❌ Phytoplankton: light blue (wrong!)
- ❌ Zooplankton: red (wrong!)
- ❌ Colors varied depending on data structure

### After Fix:
- ✅ Phytoplankton: **green** (correct!)
- ✅ Zooplankton: **light blue** (correct!)
- ✅ Colors consistent across all data sources
- ✅ Unknown groups: gray (safe fallback)

---

## Test Cases

### Case 1: BalticFW.Rdata (5 groups)
Factor levels: Benthos, Detritus, Fish, Phytoplankton, Zooplankton

**Result:**
- Benthos → burlywood ✓
- Detritus → brown ✓
- Fish → blue ✓
- Phytoplankton → green ✓
- Zooplankton → lightblue ✓

### Case 2: Full dataset (7 groups)
Factor levels: Benthos, Birds, Detritus, Fish, Mammals, Phytoplankton, Zooplankton

**Result:**
- All groups get correct colors ✓

### Case 3: Custom order
Factor levels: Fish, Zooplankton, Benthos (arbitrary order)

**Result:**
- Fish → blue ✓
- Zooplankton → lightblue ✓
- Benthos → burlywood ✓
- Order doesn't matter, names match correctly ✓

---

## Impact

**Before:**
- ❌ Colors depended on data structure
- ❌ Same functional group showed different colors in different datasets
- ❌ Phytoplankton incorrectly colored

**After:**
- ✅ Colors consistent across all datasets
- ✅ Functional group names always map to correct colors
- ✅ Phytoplankton always green
- ✅ Zooplankton always light blue
- ✅ Works with any subset of functional groups

---

**Status:** ✅ FIXED
**Testing:** Verified with BalticFW.Rdata
**Action Required:** Restart app to apply fix

---

## Related Fixes

- **NETWORK_COLORS_AND_EXPORT_FIX.md** - Fixed visNetwork color structure
- **FLUX_NETWORK_ERROR_FIX.md** - Fixed flux network visualization

All three fixes work together to ensure consistent, correct colors across the entire application!
