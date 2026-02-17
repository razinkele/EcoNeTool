# How to Fix Phytoplankton Parameter Issue

**Error:** "Group 'Phytoplankton' has only 2 parameters (Biomass, PB). Needs at least 3."

---

## Quick Fix

Phytoplankton currently has:
- ✅ Biomass: 26.7
- ✅ P/B: 200
- ❌ Q/B: **MISSING**
- ❌ EE: **MISSING**

You need to add **either** Q/B **or** EE to reach 3 parameters.

---

## Option 1: Add Ecotrophic Efficiency (EE) - RECOMMENDED

**EE** = Proportion of production consumed by predators or exported (0 to 1)

**For Phytoplankton:**
- If heavily grazed by zooplankton: EE = 0.85 to 0.95
- If moderately grazed: EE = 0.70 to 0.85
- If lightly grazed: EE = 0.50 to 0.70

**Recommended:** Use EE = **0.85** (typical for phytoplankton)

### Steps:
1. Navigate to: **ECOPATH/ECOSIM → Group Parameters** tab
2. Find the **Phytoplankton** row in the table
3. Click on the **EE** cell (last column)
4. Type: **0.85**
5. Press **Enter**
6. Click **"Save Changes"** button
7. Go to **"Mass Balance"** tab
8. Click **"Mass Balance"** button
9. ✅ Should succeed!

---

## Option 2: Add Consumption/Biomass (Q/B)

**Q/B** = Annual consumption rate (for consumers)

**Note:** Phytoplankton is a producer (Type 1), so it typically doesn't have Q/B in Ecopath. Producers don't "consume" in the traditional sense - they photosynthesize.

**Recommendation:** Use Option 1 (add EE) instead.

However, if your model requires Q/B for some reason:
- For primary producers in some models: Q/B can be set to a small value or left as NA
- Some Ecopath models use Q/B for producers to represent nutrient uptake

---

## Understanding the 3/4 Requirement

### Why Does Ecopath Need 3 Parameters?

Ecopath solves the mass-balance equation:
```
Production = Consumption + Export + Predation + Accumulation
```

For each group, it can calculate **one missing parameter** from the other three:
- **Biomass (B)** - Standing stock (t/km²)
- **P/B** - Production/Biomass ratio (annual turnover rate)
- **Q/B** - Consumption/Biomass ratio (for consumers)
- **EE** - Ecotrophic Efficiency (proportion of production utilized)

You provide 3, Ecopath calculates the 4th.

### For Phytoplankton

**What you have:**
- Biomass = 26.7 t/km² (measured or estimated)
- P/B = 200 /year (typical for phytoplankton: high turnover)

**What you need to add (choose ONE):**
- **EE** = How much is grazed by zooplankton (RECOMMENDED)
- **Q/B** = Usually NA for producers (not recommended)

**What Ecopath will calculate:**
- The missing 4th parameter

---

## Typical Phytoplankton Values

| Parameter | Typical Range | LTgoby Value | Status |
|-----------|---------------|--------------|--------|
| Biomass | 10-100 t/km² | 26.7 | ✅ Present |
| P/B | 50-300 /year | 200 | ✅ Present |
| Q/B | NA (producer) | NA | ❌ Missing |
| EE | 0.7-0.95 | NA | ❌ Missing |

**Recommendation:** Add EE = 0.85

---

## After Adding the Parameter

Once you add EE for Phytoplankton:

1. ✅ Validation will pass (3/3 parameters)
2. ✅ Mass balance will run
3. ✅ Ecopath will calculate the 4th parameter
4. ✅ You can proceed to ECOSIM, MTI analysis, etc.

---

## Troubleshooting

### "Table is not showing up"

Check R console for debug messages:
```
Rendering group_params_table...
  Model has 25 rows
  Created data frame: 25 rows × 6 columns
```

If you see these but no table:
- Switch to another tab and back
- Refresh browser (Ctrl+R or Cmd+R)
- Check browser console (F12) for errors

### "Can't edit the cell"

Make sure:
- You clicked on the correct cell (EE column)
- The table has `editable` icon next to values
- You're not trying to edit the Group name (locked)

### "Save Changes doesn't work"

After editing:
1. Press **Enter** to confirm cell edit
2. Wait for cell to update
3. Click **"Save Changes"** button
4. Look for success notification

---

## Alternative: Edit Before Import

If the table isn't working, you can edit the source database:

1. Open **LTgoby.eweaccdb** in EwE6
2. Find Phytoplankton in Ecopath groups
3. Set EE = 0.85
4. Save the database
5. Re-import into EcoNeTool

---

## Expected Outcome

After adding EE = 0.85:

**Before:**
```
❌ Phytoplankton: 2 parameters (Biomass, PB)
```

**After:**
```
✅ Phytoplankton: 3 parameters (Biomass, PB, EE)
✅ Mass balance successful
✅ Model ready for analysis
```

---

## Why This Happened

The LTgoby database has incomplete parameters for Phytoplankton. This is common when:
- Database is being developed/tested
- Parameters are imported from different sources
- Some groups are placeholders

The validation is working correctly by catching this issue before attempting to balance the model.

---

## Visual Guide

```
Step 1: Navigate to Group Parameters tab
  ┌─────────────────────────────────────────────┐
  │ ECOPATH/ECOSIM                              │
  │ ┌───────────────────────────────────────┐   │
  │ │ [Model Setup] [Group Parameters] ... │   │
  │ └───────────────────────────────────────┘   │
  └─────────────────────────────────────────────┘

Step 2: Find Phytoplankton row and edit EE
  ┌─────────────────────────────────────────────────────┐
  │ Group          Type  Biomass   PB    QB    EE       │
  │ Detritus       2     1.80      NA    NA    NA       │
  │ Phytoplankton  1     26.70   200.00  NA   [0.85] ← │
  │ Zooplankton    0     3.68     25.00  83    NA       │
  └─────────────────────────────────────────────────────┘
                                              Click here,
                                              type 0.85,
                                              press Enter

Step 3: Save and balance
  [Save Changes] ← Click this

  Then go to Mass Balance tab:
  [Mass Balance] ← Click this

  ✅ Success! Model balanced
```

---

## Need Help?

If you're still having issues:

1. **Check if table is visible:**
   - Look for debug messages in R console
   - Try switching tabs back and forth

2. **Check what parameters are shown:**
   - Does Phytoplankton row show Biomass=26.7, PB=200?
   - Are QB and EE showing as NA?

3. **Try the automated fix:**
   ```r
   # In R console:
   rpath_values$params$model$EE[2] <- 0.85  # Row 2 is Phytoplankton
   ```

4. **Or manually update the database:**
   - Edit LTgoby.eweaccdb in EwE6
   - Set Phytoplankton EE = 0.85
   - Re-import

---

**Remember:** The error is **correct** - Phytoplankton genuinely needs another parameter. Once you add EE, the model will balance successfully!
