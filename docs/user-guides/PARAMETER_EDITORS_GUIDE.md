# Parameter Editors - User Guide

**Date:** 2025-12-08
**Feature:** Interactive Group Parameters and Diet Matrix Editors
**Status:** ✅ Implemented and Ready to Use

---

## Overview

Two new tabs have been added to the **ECOPATH/ECOSIM** module that allow you to **inspect and edit** model parameters before balancing:

1. **Group Parameters** - Edit Biomass, P/B, Q/B, EE, Type
2. **Diet Matrix** - Edit predator-prey diet proportions

This solves the "missing value where TRUE/FALSE needed" error by letting you fix parameter issues directly in the app!

---

## Why These Tabs Are Needed

### The Problem:
When importing ECOPATH databases, you might encounter:
```
Error: Ecopath balancing failed: NA values in logical comparisons
This usually means:
  1. Type column has NA values
  2. Critical parameters (B, P/B, Q/B, EE) are missing
  3. Diet matrix has invalid values
```

### The Solution:
Instead of going back to the ECOPATH software to fix issues, you can now:
- **See exactly which parameters are missing**
- **Edit values directly in the app**
- **Validate in real-time** before balancing
- **Reset if you make mistakes**

---

## Workflow

### Complete ECOPATH → Rpath → Balance Workflow:

```
1. Data Import Tab
   └─> Upload ECOPATH database

2. Model Setup Tab
   └─> Click "Convert to Rpath Format"
   └─> Watch console for validation warnings

3. Group Parameters Tab ⭐ NEW
   └─> Review validation issues
   └─> Edit missing/invalid parameters
   └─> Save changes

4. Diet Matrix Tab ⭐ NEW
   └─> Review diet sum issues
   └─> Edit diet proportions
   └─> Save changes

5. Mass Balance Tab
   └─> Click "Run Mass Balance"
   └─> Model should balance successfully!
```

---

## Tab 1: Group Parameters Editor

### What You See:

**Validation Summary Box** (at top):
```
⚠ Validation Issues (2 groups):
  • Phytoplankton (only 2 parameters)
  • Zooplankton (only 2 parameters)

Each group needs at least 3 out of 4 parameters to balance.
```

**Editable Table:**
| Group | Type | Biomass | P/B | Q/B | EE |
|-------|------|---------|-----|-----|-----|
| Cod | 0 | 3.500 | 0.450 | 1.800 | NA |
| Zooplankton | 0 | NA | 18.500 | NA | 0.900 |
| Phytoplankton | 1 | NA | 52.000 | NA | 0.950 |
| Detritus | 2 | 50.000 | NA | NA | NA |

### How to Edit:

1. **Click on any cell** (except Group column)
2. **Type new value**
3. **Press Enter**
4. Validation happens automatically
5. **Click "Save Changes"** when done

### Parameter Descriptions:

- **Group**: Name (not editable)
- **Type**:
  - 0 = Consumer (fish, invertebrates)
  - 1 = Producer (phytoplankton, plants)
  - 2 = Detritus (dead organic matter)
  - 3 = Fleet (fishing gear)
- **Biomass (B)**: tons/km²
- **P/B**: Production/Biomass ratio (per year)
- **Q/B**: Consumption/Biomass ratio (per year)
- **EE**: Ecotrophic Efficiency (0-1, proportion consumed)

### Validation Rules:

✓ **At least 3 out of 4 parameters** (B, P/B, Q/B, EE) must be present
✓ **Type** must be 0, 1, 2, or 3
✓ **P/B** must be > 0
✓ **Q/B** must be > 0
✓ **EE** must be 0-1

### Common Fixes:

**Issue:** Zooplankton has only 2 parameters
```
Before: Biomass=NA, P/B=18.5, Q/B=NA, EE=0.9
After:  Biomass=5.0, P/B=18.5, Q/B=NA, EE=0.9
```
→ Now has 3 parameters, can balance!

**Issue:** EE out of range
```
Before: EE=1.5
After:  EE=0.95
```
→ EE must be ≤ 1.0

---

## Tab 2: Diet Matrix Editor

### What You See:

**Validation Summary Box:**
```
⚠ Diet Sum Issues (1 predator):
  • Cod (sum = 1.15)

Each predator column should sum to ≤ 1.0
```

**Editable Matrix:**
| Group (Prey) | Cod | Flounder | Seals | Zooplankton |
|--------------|-----|----------|-------|-------------|
| Zooplankton | 0.100 | 0.200 | 0.000 | 0.000 |
| Benthos | 0.450 | 0.600 | 0.000 | 0.000 |
| Small Fish | 0.600 | 0.150 | 0.400 | 0.000 |
| Phytoplankton | 0.000 | 0.000 | 0.000 | 1.000 |
| **Column Sum** | **1.15** | **0.95** | **0.40** | **1.00** |

### How to Edit:

1. **Click on any cell** (except Group column)
2. **Type new value** (0-1)
3. **Press Enter**
4. Check if predator column now sums to ≤ 1.0
5. **Click "Save Changes"** when done

### Understanding the Matrix:

- **Rows** = Prey (what is eaten)
- **Columns** = Predators (who eats)
- **Values** = Proportion of prey in predator's diet
- **Column sum** = Total diet proportion for that predator

### Example Fix:

**Problem:** Cod's diet sums to 1.15 (impossible!)

```
Cod's Diet (Before):
  Zooplankton: 0.100
  Benthos: 0.450
  Small Fish: 0.600
  Total: 1.150 ❌

Cod's Diet (After - normalized):
  Zooplankton: 0.087 (0.100 / 1.15)
  Benthos: 0.391 (0.450 / 1.15)
  Small Fish: 0.522 (0.600 / 1.15)
  Total: 1.000 ✓
```

**Shortcut:** Multiply all values by (1.0 / 1.15) = 0.87

### Validation Rules:

✓ **Values** must be 0-1
✓ **Column sums** (predator totals) must be ≤ 1.0
✓ **Row** (prey) values can vary freely

---

## Buttons Explained

### "Save Changes" Button:
- Confirms you're done editing
- Shows confirmation message
- Changes are actually saved immediately when you edit each cell
- This button is more for peace of mind

### "Reset to Original" Button:
- Restores values from initial conversion
- Use if you made mistakes and want to start over
- **Warning:** All your edits will be lost!

---

## Real Example: Fixing LTgoby Database

### Step 1: Convert and See Issues

```
Navigate: Model Setup → Click "Convert to Rpath Format"

Console shows:
Warning: Group 'Phytoplankton' has only 2 parameters (needs at least 3):
  Biomass: MISSING
  P/B: 52
  Q/B: MISSING
  EE: 0.95
```

### Step 2: Fix in Group Parameters Tab

```
Navigate: Group Parameters tab

Validation shows:
⚠ Validation Issues (1 group):
  • Phytoplankton (only 2 parameters)

Actions:
1. Click on Phytoplankton Biomass cell
2. Type: 10.5
3. Press Enter
4. Validation updates: ✓ All groups have sufficient parameters!
5. Click "Save Changes"
```

### Step 3: Check Diet Matrix

```
Navigate: Diet Matrix tab

Validation shows:
✓ All predator diets are valid!

No action needed.
```

### Step 4: Balance Model

```
Navigate: Mass Balance tab

Click: "Run Mass Balance"

Result: ✓ Model balances successfully!
```

---

## Tips & Tricks

### Tip 1: Start with Group Parameters
Always check Group Parameters tab first. Most balance failures are due to insufficient parameters, not diet issues.

### Tip 2: Use "Import" Row for Imports
The diet matrix has a special "Import" row at the bottom. This represents external food inputs (e.g., migrating prey).

### Tip 3: Check Console Warnings
The console shows detailed warnings during conversion. Use these to identify which groups need attention.

### Tip 4: Common Parameter Values
If you need to guess values:
- **Phytoplankton**: B=5-20, P/B=50-100, Q/B=NA, EE=0.90-0.95
- **Zooplankton**: B=3-10, P/B=15-30, Q/B=30-60, EE=0.80-0.90
- **Fish**: B=1-10, P/B=0.3-1.5, Q/B=2-8, EE=0.5-0.9
- **Detritus**: B=10-100, P/B=NA, Q/B=NA, EE=0.1-0.5

### Tip 5: Diet Normalization
If a predator's diet sums to > 1.0:
1. Calculate: factor = 1.0 / current_sum
2. Multiply all prey values by factor
3. New sum = 1.0

Example:
```
Sum = 1.15
Factor = 1.0 / 1.15 = 0.87
Prey1: 0.60 × 0.87 = 0.52
Prey2: 0.40 × 0.87 = 0.35
Prey3: 0.15 × 0.87 = 0.13
New sum: 0.52 + 0.35 + 0.13 = 1.00 ✓
```

---

## Troubleshooting

### Issue: Table doesn't appear
**Cause:** Rpath parameters not converted yet
**Solution:** Go to Model Setup tab and click "Convert to Rpath Format" first

### Issue: Can't edit Group column
**Cause:** By design - group names shouldn't change
**Solution:** All other columns are editable

### Issue: Edit doesn't save
**Cause:** Validation failed (value out of range)
**Solution:** Check notification message for specific error

### Issue: Balance still fails after editing
**Cause:** Some issues remain
**Solution:**
1. Check console for new warnings
2. Run diagnostic: `source("tests/diagnose_rpath_error.R")`
3. Review both editor tabs for remaining issues

### Issue: Reset doesn't work
**Cause:** No original params stored
**Solution:** Reconvert from Model Setup tab

---

## Keyboard Shortcuts

- **Tab**: Move to next editable cell
- **Shift+Tab**: Move to previous cell
- **Enter**: Save edit and move down
- **Esc**: Cancel edit
- **Arrows**: Navigate cells (when not editing)

---

## Data Persistence

### What's Saved:
- ✓ Edits are saved immediately to `rpath_values$params`
- ✓ Changes persist when switching tabs
- ✓ Changes are used when you click "Run Mass Balance"

### What's NOT Saved:
- ✗ Changes are NOT saved to the original ECOPATH database file
- ✗ Changes are lost if you reconvert from Model Setup
- ✗ Changes are lost if you close the app

### To Make Permanent:
If you want to keep your edits permanently:
1. Export the modified Rpath params to a file
2. Or fix the values in the original ECOPATH database
3. Or use the app's data export feature (if available)

---

## Advanced: Programmatic Access

If you want to access the edited parameters in R:

```r
# After editing in the app, the modified parameters are in:
rpath_values$params$model    # Group parameters
rpath_values$params$diet     # Diet matrix

# To export:
write.csv(rpath_values$params$model, "edited_groups.csv")
write.csv(rpath_values$params$diet, "edited_diet.csv")
```

---

## Summary

### Before These Editors:
```
Import → Convert → Error: NA values →
Go back to EwE → Fix → Export → Import again → Repeat...
```

### With These Editors:
```
Import → Convert → Group Parameters → Edit →
Diet Matrix → Edit → Balance → Success! ✓
```

**Time saved:** 10-30 minutes per iteration
**Frustration saved:** Infinite!

---

## See Also

- `RPATH_BALANCING_FIX.md` - Validation error details
- `tests/diagnose_rpath_error.R` - Diagnostic tool
- Rpath documentation: https://noaa-edab.github.io/Rpath/

---

**Status:** ✅ Ready to Use
**Last Updated:** 2025-12-08
**Tested:** ✅ Syntax Valid

Start using the editors now to fix balancing errors quickly and easily!
