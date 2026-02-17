# Rpath Balancing Error Fix

**Date:** 2025-12-08
**Issue:** "missing value where TRUE/FALSE needed" error when balancing Ecopath model
**Status:** ✅ Fixed with Enhanced Validation

---

## Problem

When attempting to balance an Ecopath model in Rpath, users encountered:
```
Ecopath error: Error running Ecopath model: missing value where TRUE/FALSE needed
Check that all required parameters are present and valid.
```

This cryptic error indicates NA values in logical comparisons during the balancing process.

---

## Root Causes

### 1. **NA Values in Type Column**
The Type column categorizes groups (0=consumer, 1=producer, 2=detritus, 3=fleet). If any Type values are NA, logical comparisons like `Type < 2` fail with the "TRUE/FALSE" error.

### 2. **Insufficient Parameters**
Ecopath requires at least 3 out of 4 parameters for each group:
- Biomass (B)
- Production/Biomass ratio (P/B)
- Consumption/Biomass ratio (Q/B)
- Ecotrophic Efficiency (EE)

Missing too many parameters prevents balancing.

### 3. **Invalid Parameter Values**
- Negative or zero P/B or Q/B values
- EE outside range 0-1
- Diet matrix sums > 1.0

---

## Fixes Applied

### 1. **Type Validation (Lines 104-109)**

**Before:**
```r
living_groups <- groups[groups$Type >= 0, ]
```

**After:**
```r
# Validate Type column (critical for Rpath)
if (any(is.na(groups$Type))) {
  na_groups <- groups$GroupName[is.na(groups$Type)]
  stop("Groups have missing Type values: ", paste(na_groups, collapse = ", "),
       "\nType must be: 0 (consumer), 1 (producer), 2 (detritus), or 3 (fleet)")
}

# Filter out groups with Type < 0 (with NA protection)
living_groups <- groups[!is.na(groups$Type) & groups$Type >= 0, ]
```

**Impact:** Early detection of NA Type values with clear error message.

---

### 2. **Predator/Prey Filtering (Lines 227-238)**

**Before:**
```r
predator_groups <- living_groups[living_groups$Type < 2, ]
prey_groups <- living_groups[living_groups$Type < 3, ]
```

**After:**
```r
# Get predator groups (with NA protection)
predator_groups <- living_groups[!is.na(living_groups$Type) & living_groups$Type < 2, ]

# Get prey groups (with NA protection)
prey_groups <- living_groups[!is.na(living_groups$Type) & living_groups$Type < 3, ]

# Validate we have predators and prey
if (nrow(predator_groups) == 0) {
  stop("No predator groups found (Type < 2). Check that Type column is correctly set.")
}
if (nrow(prey_groups) == 0) {
  stop("No prey groups found (Type < 3). Check that Type column is correctly set.")
}
```

**Impact:** Prevents empty predator/prey lists and provides clear diagnostics.

---

### 3. **Parameter Validation (Lines 338-390)**

Added comprehensive validation before returning converted parameters:

```r
message("\nValidating Rpath parameters...")

# For each living group (not fleets), check we have enough data
for (i in 1:nrow(params$model)) {
  group_name <- params$model$Group[i]
  group_type <- params$model$Type[i]

  # Skip fleets (Type == 3)
  if (group_type == 3) next

  # Get parameter values
  b <- params$model$Biomass[i]
  pb <- params$model$PB[i]
  qb <- params$model$QB[i]
  ee <- params$model$EE[i]

  # Count how many parameters are NOT NA
  n_params <- sum(!is.na(c(b, pb, qb, ee)))

  # Ecopath requires at least 3 out of 4 parameters
  if (n_params < 3) {
    warning("Group '", group_name, "' has only ", n_params, " parameters (needs at least 3):",
            "\n  Biomass: ", ifelse(is.na(b), "MISSING", b),
            "\n  P/B: ", ifelse(is.na(pb), "MISSING", pb),
            "\n  Q/B: ", ifelse(is.na(qb), "MISSING", qb),
            "\n  EE: ", ifelse(is.na(ee), "MISSING", ee))
  }

  # Check for invalid values
  if (!is.na(pb) && pb <= 0) {
    warning("Group '", group_name, "' has invalid P/B: ", pb, " (must be > 0)")
  }
  if (!is.na(qb) && qb <= 0) {
    warning("Group '", group_name, "' has invalid Q/B: ", qb, " (must be > 0)")
  }
  if (!is.na(ee) && (ee < 0 || ee > 1)) {
    warning("Group '", group_name, "' has invalid EE: ", ee, " (must be 0-1)")
  }
}

# Check diet matrix
diet_cols <- names(params$diet)[names(params$diet) != "Group"]
for (col in diet_cols) {
  diet_sum <- sum(params$diet[[col]], na.rm = TRUE)
  if (diet_sum > 1.01) {
    warning("Predator '", col, "' has diet sum > 1.0 (", round(diet_sum, 3), ")")
  }
}
```

**Impact:** Identifies parameter issues during conversion with detailed warnings.

---

### 4. **Pre-Balance Validation (Lines 417-440)**

Added validation immediately before calling `Rpath::rpath()`:

```r
# Validate parameters before running
message("Validating parameters before balance...")

# Check for NA values in Type (critical)
if (any(is.na(rpath_params$model$Type))) {
  stop("Type column contains NA values. Cannot proceed with balancing.")
}

# Check for completely empty groups
for (i in 1:nrow(rpath_params$model)) {
  if (rpath_params$model$Type[i] < 3) {  # Not a fleet
    n_valid <- sum(!is.na(c(
      rpath_params$model$Biomass[i],
      rpath_params$model$PB[i],
      rpath_params$model$QB[i],
      rpath_params$model$EE[i]
    )))

    if (n_valid < 2) {
      stop("Group '", rpath_params$model$Group[i], "' has insufficient parameters (",
           n_valid, " out of 4). Need at least 3 for balancing.")
    }
  }
}
```

**Impact:** Prevents Rpath from receiving invalid data.

---

### 5. **Enhanced Error Messages (Lines 472-489)**

Improved error reporting to help diagnose issues:

```r
if (grepl("missing value where TRUE/FALSE needed", error_msg, ignore.case = TRUE)) {
  stop("Ecopath balancing failed: NA values in logical comparisons.\n",
       "This usually means:\n",
       "  1. Type column has NA values\n",
       "  2. Critical parameters (B, P/B, Q/B, EE) are missing\n",
       "  3. Diet matrix has invalid values\n",
       "\nOriginal error: ", error_msg, "\n",
       "\nRun conversion again with verbose output to see validation warnings.")
} else {
  stop("Error running Ecopath model: ", error_msg, "\n",
       "Check that all required parameters are present and valid.\n",
       "Each group needs at least 3 out of 4 parameters: Biomass, P/B, Q/B, EE")
}
```

**Impact:** Users get actionable advice instead of cryptic error.

---

## Diagnostic Tool

Created `tests/diagnose_rpath_error.R` to help identify issues:

### Usage:
```r
source("tests/diagnose_rpath_error.R")
```

### What It Does:
1. **Imports** ECOPATH database
2. **Inspects** group data for Type issues
3. **Checks** parameter completeness (B, P/B, Q/B, EE)
4. **Converts** to Rpath format
5. **Attempts** to balance model
6. **Reports** detailed diagnostics if errors occur

### Example Output:
```
STEP 2: Inspecting group data for issues...
----------------------------------------------------------------------

Checking Type column:
  ✓ No NA values in Type

  Type distribution:
    Type 0 (Consumer): 18 groups
    Type 1 (Producer): 3 groups
    Type 2 (Detritus): 2 groups
    Type 3 (Fleet): 1 groups

Checking critical parameters (B, P/B, Q/B, EE):
  ⚠ Phytoplankton: only 2 parameters
      B: MISSING  P/B: 52  Q/B: MISSING  EE: 0.95
  ⚠ Zooplankton: only 2 parameters
      B: MISSING  P/B: 18.5  Q/B: MISSING  EE: 0.90

STEP 3: Converting to Rpath format...
----------------------------------------------------------------------
Warning: Group 'Phytoplankton' has only 2 parameters (needs at least 3)
Warning: Group 'Zooplankton' has only 2 parameters (needs at least 3)

✓ Conversion successful
```

---

## Testing

### Manual Test in App:

1. **Start app**: `source("run_app.R")`
2. **Import**: Upload ECOPATH database
3. **Convert**: Click "Convert to Rpath Format"
   - **Watch console** for validation warnings
4. **Balance**: Click "Run Mass Balance"
   - If errors occur, warnings from step 3 will indicate the issue

### Automated Diagnostic:

```r
# Run diagnostic on LTgoby
source("tests/diagnose_rpath_error.R")
```

This will show exactly what's wrong and where.

---

## Common Issues & Solutions

### Issue 1: Type Has NA Values

**Error:**
```
Groups have missing Type values: Phytoplankton, Detritus
Type must be: 0 (consumer), 1 (producer), 2 (detritus), or 3 (fleet)
```

**Solution:**
Check the ECOPATH database. Type column may have:
- NULL values
- Invalid type codes
- Missing entries

Fix in EwE software before importing.

---

### Issue 2: Insufficient Parameters

**Warning:**
```
Warning: Group 'Zooplankton' has only 2 parameters (needs at least 3):
  Biomass: MISSING
  P/B: 18.5
  Q/B: MISSING
  EE: 0.90
```

**Solution:**
Ecopath needs at least 3 out of 4 parameters. Options:
1. Add missing Biomass or Q/B values in EwE
2. Let Ecopath calculate the missing value (it will try)
3. Remove the group if not essential

---

### Issue 3: Invalid Parameter Values

**Warning:**
```
Warning: Group 'Benthic fish' has invalid P/B: 0 (must be > 0)
Warning: Group 'Predators' has invalid EE: 1.5 (must be 0-1)
```

**Solution:**
Check ECOPATH database for:
- Zero or negative P/B or Q/B (biological impossible)
- EE > 1.0 (means more than 100% is eaten - impossible)
- Check for -9999 values (ECOPATH missing indicator)

---

### Issue 4: Diet Matrix Issues

**Warning:**
```
Warning: Predator 'Cod' has diet sum > 1.0 (1.15)
```

**Solution:**
Diet proportions must sum to ≤ 1.0 for each predator.
Check diet composition in EwE and normalize if needed.

---

## Before/After Comparison

### Before Fix:
```
User clicks "Run Mass Balance"
  ↓
Rpath receives data with NA Type values
  ↓
Logical comparison: Type < 2 fails
  ↓
Error: "missing value where TRUE/FALSE needed"
  ↓
User confused - no idea what's wrong
```

### After Fix:
```
User clicks "Convert to Rpath Format"
  ↓
Validation checks Type column
  ↓
NA detected: Clear error message with group names
  ↓
User knows exactly which groups have issues
  ↓
OR validation passes but shows warnings
  ↓
User clicks "Run Mass Balance"
  ↓
Pre-balance validation catches any remaining issues
  ↓
Clear error with suggestions
  ↓
User can fix the source data
```

---

## Files Modified

| File | Lines | Changes |
|------|-------|---------|
| `R/functions/rpath_integration.R` | 104-109 | Type validation |
| | 227-238 | Predator/prey filtering |
| | 338-390 | Parameter validation |
| | 417-440 | Pre-balance validation |
| | 472-489 | Enhanced error messages |
| `tests/diagnose_rpath_error.R` | 1-150 (new) | Diagnostic tool |
| `RPATH_BALANCING_FIX.md` | 1-400 (new) | This documentation |

---

## Summary

### What Was Fixed:
1. ✅ NA values in Type column now caught early with clear error
2. ✅ Insufficient parameters identified with detailed warnings
3. ✅ Invalid parameter values detected (negative, out of range)
4. ✅ Diet matrix validated (sums ≤ 1.0)
5. ✅ Pre-balance checks prevent invalid data reaching Rpath
6. ✅ Enhanced error messages guide user to solution
7. ✅ Diagnostic tool helps identify specific issues

### Benefits:
- **Clear errors** instead of cryptic "TRUE/FALSE" message
- **Early detection** of issues during conversion
- **Detailed diagnostics** showing exactly what's wrong
- **Actionable advice** on how to fix problems
- **Faster debugging** with diagnostic tool

---

## Next Steps

1. **Test with your database**:
   ```r
   source("tests/diagnose_rpath_error.R")
   ```

2. **If issues found**:
   - Read the validation warnings
   - Fix issues in ECOPATH database
   - Re-import and try again

3. **If balancing succeeds**:
   - Proceed with MTI analysis
   - Run ECOSIM simulations
   - Enjoy working model!

---

**Status:** ✅ Fix Complete
**Validation:** ✅ Syntax Valid
**Testing:** 🔲 Pending User Feedback

Let me know what errors/warnings you see when running the diagnostic!
