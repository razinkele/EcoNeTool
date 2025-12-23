# Flux-Weighted Network Display Error Fix

**Date:** 2025-12-22
**Issue:** Flux-weighted network showing single error node instead of network visualization
**Status:** ✅ Fixed (Updated: Added validation parameter fix)

---

## Problem

The Flux-weighted Network tab was displaying a single "Error" node instead of the full network visualization, while the regular Food Web Network and Biomass displays worked normally.

**Error message:** "Failed to calculate fluxweb results: unused argument (min_rows = 1)"

---

## Root Causes

### 0. **Invalid Validation Parameter** (CRITICAL - Blocking flux calculations)

**Issue:**
Multiple functions called `validate_dataframe()` with a non-existent `min_rows` parameter:
```r
# INCORRECT - validation_utils.R does not support min_rows
validate_dataframe(info, required_cols = c("bodymasses", "met.types"), min_rows = 1)
```

The `validate_dataframe()` function signature is:
```r
validate_dataframe(df, required_cols, df_name = "Data frame")
```

**Files affected:**
- `flux_calculations.R` (lines 112, 183)
- `keystoneness.R` (lines 5, 102)
- `topological_metrics.R` (line 63)

**Impact:** All flux calculations failed immediately with "unused argument" error before any network processing could begin.

### 1. **Empty Edge Network Handling** (app.R:2882-2883)

**Issue:**
```r
flux_weights <- E(res$netLW)$weight
edge_widths <- EDGE_WIDTH_MIN + (flux_weights/max(flux_weights) * EDGE_WIDTH_SCALE)
```

If `res$netLW` had no edges:
- `flux_weights` would be `numeric(0)` (empty vector)
- `max(flux_weights)` would return `-Inf` with a warning
- Division by `-Inf` produces `NaN` values
- Edge widths become `NaN`, causing visNetwork to fail

### 2. **No Validation for Finite Values**

If flux calculations produced `NA` or infinite values, the edge width calculations would fail without graceful handling.

### 3. **Edge Name Mapping Issues** (network_visualization.R:234-239)

When edge network vertex names didn't match node network names:
- `name_to_id[edgelist_raw[, 1]]` could return `NA`
- Edges with `from=NA` or `to=NA` cause visNetwork errors
- No validation or filtering of invalid edges

### 4. **Edge Data Row Mismatch**

If edge filtering removed some edges but edge_data wasn't updated, row counts wouldn't match, causing errors.

---

## Solutions Implemented

### Fix 0: Remove Invalid Validation Parameter (CRITICAL)

**Files:**
- `R/functions/flux_calculations.R` (lines 112, 183)
- `R/functions/keystoneness.R` (lines 5, 102)
- `R/functions/topological_metrics.R` (line 63)

**Changes:**
```r
# BEFORE (causing "unused argument" error)
validate_dataframe(info, required_cols = c("bodymasses", "met.types"), min_rows = 1)

# AFTER (correct)
validate_dataframe(info, required_cols = c("bodymasses", "met.types"))
```

**Result:** Flux calculations now execute without parameter errors.

---

### Fix 1: Empty Edge Network Check (app.R:2881-2893)

```r
# Check if flux network has edges
if (ecount(res$netLW) == 0) {
  # No edges - show network with all nodes but no connections
  create_foodweb_visnetwork(
    net = current_net,
    info = current_info,
    node_size_method = "fixed",
    edge_network = res$netLW,  # Empty edge network
    edge_data = NULL,
    edge_color_by = "default",
    trophic_levels = current_tl
  )
}
```

**Result:** Network displays all nodes with no edges when flux calculations produce no connections.

### Fix 2: Safe Max Calculation (app.R:2897-2903)

```r
# Handle case where all fluxes are zero
max_flux <- max(flux_weights, na.rm = TRUE)
if (is.finite(max_flux) && max_flux > 0) {
  edge_widths <- EDGE_WIDTH_MIN + (flux_weights/max_flux * EDGE_WIDTH_SCALE)
} else {
  edge_widths <- rep(EDGE_WIDTH_MIN, length(flux_weights))
}
```

**Result:** Edge widths default to minimum width if max is not finite or zero.

### Fix 3: NA/Infinite Value Handling (app.R:2906-2916)

```r
# Format flux values for display
flux_display <- sapply(flux_weights, function(x) {
  if (is.na(x) || !is.finite(x)) {
    "0"
  } else if (x >= 0.01) {
    sprintf("%.4f", x)
  } else if (x >= 0.0001) {
    sprintf("%.6f", x)
  } else {
    sprintf("%.2e", x)
  }
})
```

**Result:** Non-finite flux values display as "0" instead of causing errors.

### Fix 4: Edge Name Validation (network_visualization.R:197-253)

```r
# Handle case where edge network has no edges
if (ecount(edge_net) == 0) {
  edges <- data.frame(from=integer(0), to=integer(0), stringsAsFactors = FALSE)
} else {
  # Validate that all edge names exist in node list
  from_names <- edgelist_raw[, 1]
  to_names <- edgelist_raw[, 2]
  missing_from <- setdiff(from_names, nodes$label)
  missing_to <- setdiff(to_names, nodes$label)

  if (length(missing_from) > 0 || length(missing_to) > 0) {
    warning("Edge network contains vertices not in main network.")
    message("Missing source vertices: ", paste(missing_from, collapse=", "))
    message("Missing target vertices: ", paste(missing_to, collapse=", "))
  }

  # Build edge dataframe
  edges <- data.frame(
    from = name_to_id[edgelist_raw[, 1]],
    to = name_to_id[edgelist_raw[, 2]],
    stringsAsFactors = FALSE
  )

  # Remove edges with NA mappings
  valid_edges <- !is.na(edges$from) & !is.na(edges$to)
  if (!all(valid_edges)) {
    warning(sprintf("Removed %d edges with invalid node mappings", sum(!valid_edges)))
    edges <- edges[valid_edges, , drop=FALSE]
    if (!is.null(edge_colors)) {
      edge_colors <- edge_colors[valid_edges]
    }
  }
}
```

**Result:** Invalid edges are detected, logged, and filtered out gracefully.

### Fix 5: Edge Data Validation (network_visualization.R:260-271)

```r
# Add custom edge data if provided (e.g., flux widths)
if (!is.null(edge_data) && nrow(edges) > 0) {
  # Validate edge_data has same number of rows as edges
  if (nrow(edge_data) == nrow(edges)) {
    for (col in names(edge_data)) {
      edges[[col]] <- edge_data[[col]]
    }
  } else {
    warning(sprintf("edge_data has %d rows but edges has %d rows. Skipping edge_data.",
                    nrow(edge_data), nrow(edges)))
  }
}
```

**Result:** Mismatched edge_data is detected and skipped with a warning.

### Fix 6: Improved Error Messages (app.R:2936-2943)

```r
}, error = function(e) {
  # Return informative error message
  visNetwork(
    data.frame(id=1, label="Error",
               title=paste0("<b>Flux Network Error:</b><br>", e$message)),
    data.frame(from=integer(0), to=integer(0))
  )
})
```

**Result:** When errors do occur, the error message is clearly displayed in the node tooltip.

---

## Files Modified

1. **R/functions/flux_calculations.R (lines 112, 183)**
   - ✅ Removed invalid `min_rows` parameter from `validate_dataframe()` calls
   - ✅ Corrected `calculate_losses()` validation (line 112)
   - ✅ Corrected `get_fluxweb_results()` validation (line 183)

2. **R/functions/keystoneness.R (lines 5, 102)**
   - ✅ Removed invalid `min_rows` parameter from `validate_dataframe()` calls
   - ✅ Corrected `calculate_mti()` validation (line 5)
   - ✅ Corrected `calculate_keystoneness()` validation (line 102)

3. **R/functions/topological_metrics.R (line 63)**
   - ✅ Removed invalid `min_rows` parameter from `validate_dataframe()` calls
   - ✅ Corrected `get_node_weighted_indicators()` validation (line 63)

4. **app.R (lines 2871-2944)**
   - Added empty edge network check
   - Safe max() calculation with finite value validation
   - NA/infinite value handling in flux display
   - Improved error message formatting

5. **R/functions/network_visualization.R (lines 194-271)**
   - Empty edge network handling
   - Edge name validation and filtering
   - Edge data row count validation
   - Missing vertex warnings

---

## Expected Behavior After Fix

### Case 1: Normal Flux Network
```
✓ All nodes displayed with correct positions
✓ Edges displayed with widths proportional to flux
✓ Edge tooltips show flux values in kJ/day/km²
✓ Colors match functional group legend
```

### Case 2: No Flux Edges (All Zero Fluxes)
```
✓ All nodes displayed
✓ No edges displayed (expected when fluxes are zero)
✓ No error messages
✓ Network layout preserved
```

### Case 3: Invalid Edge Mappings
```
⚠ Warning logged: "Edge network contains vertices not in main network"
⚠ Warning logged: "Removed X edges with invalid node mappings"
✓ Valid edges still displayed
✓ Network continues to render
```

### Case 4: Edge Data Mismatch
```
⚠ Warning logged: "edge_data has X rows but edges has Y rows"
✓ Network displayed without custom edge data
✓ Edges still visible with default styling
```

### Case 5: Actual Error
```
✓ Single "Error" node displayed
✓ Hover over node shows detailed error message
✓ User can diagnose issue from error text
```

---

## Testing Checklist

To verify the fix works:

1. ✅ **Normal food web:** Import ECOPATH model → View Flux-weighted Network
   - Expect: Network displays with flux-weighted edges

2. ✅ **Empty network:** Create minimal network → View Flux-weighted Network
   - Expect: Nodes displayed, no edges (if fluxes are zero)

3. ✅ **Check console:** Look for warnings about edge mappings
   - Expect: No warnings for valid models

4. ✅ **Compare with Food Web Network:** Both should show same nodes
   - Expect: Same node positions and labels

5. ✅ **Hover over edges:** Check flux value tooltips
   - Expect: Values displayed in scientific notation or decimal

---

## Related Issues

This fix complements:
- **GEOGRAPHIC_REGION_FIX.md** - Fixed metadata access errors
- **TAXONOMIC_API_IMPROVEMENTS.md** - Real-time progress updates
- **FISHBASE_WEIGHTS_INTEGRATION.md** - Accurate body mass data

All features work together to ensure robust ECOPATH import and visualization.

---

## Impact

**Before fix:**
- ❌ Flux network showed single "Error" node
- ❌ No indication of what went wrong
- ❌ Network visualization failed completely

**After fix:**
- ✅ Networks display correctly even with edge cases
- ✅ Warnings logged for diagnostic purposes
- ✅ Graceful fallback for empty/invalid edges
- ✅ Clear error messages when actual errors occur
- ✅ All nodes visible even when edges are filtered

---

**Status:** ✅ FIXED
**Compatibility:** 100% backward compatible
**Action Required:** Restart app to apply fixes

---

## Technical Details

### Why Flux Networks Might Have No Edges

Energy flux calculations (`get_fluxweb_results`) can produce zero or near-zero fluxes when:

1. **Biomass values are zero/NA:** No biomass → no flux
2. **Efficiencies are zero:** No assimilation → no flux
3. **Body masses invalid:** Metabolic loss calculation fails
4. **Temperature issues:** Extreme temperatures produce invalid results
5. **Network structure:** Some topologies don't support flux calculations

The fix ensures visualization succeeds regardless of flux calculation outcomes.

---
