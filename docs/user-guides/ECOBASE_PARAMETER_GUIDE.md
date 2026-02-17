# EcoBase Parameter Types: Input vs Output

**Date:** 2025-12-06
**Issue:** Some EcoBase models have 0 trophic links when using Output parameters

---

## Summary

EcoBase provides two types of parameters for each model:

1. **Input (Original):** Original parameters as entered before mass balancing
2. **Output (Balanced):** Mass-balanced parameters from EwE output

**Not all models have diet composition in both parameter types.** Some models only include diet data in Input parameters.

---

## Problem Example: Model #608 (Central Baltic Sea)

### Test Results

| Parameter Type | Species | Trophic Links | Status |
|----------------|---------|---------------|--------|
| Input (Original) | 22 | **89** | ✓ Works |
| Output (Balanced) | 22 | **0** | ✗ No diet data |

### Why This Happens

EcoBase OUTPUT parameters focus on mass-balanced biomass, production, and consumption values. **Diet composition (trophic links) is not always included** in the output XML.

For model #608:
- INPUT has `<diet_descr>` with 99 diet items
- OUTPUT has empty or missing `<diet_descr>` fields

---

## Solution

### If You Get 0 Trophic Links:

1. **Check the import message** after clicking "Import Model"
   ```
   ✓ SUCCESS: EcoBase model imported!

   Model ID: 608
   Parameter type: Output (Balanced)

   Conversion complete:
     - Species/groups: 22
     - Trophic links: 0  ← WARNING!

   ⚠ WARNING: No trophic links imported!
   This model may not have diet data in Output parameters.
   Try importing with Input (Original) instead.
   ```

2. **Re-import with the other parameter type:**
   - Go back to "EcoBase Connection" tab
   - Select the same model again
   - Choose the **other** parameter type
   - Click "Import Model to EcoNeTool"

3. **Recommended approach:**
   - Start with **Output (Balanced)** - usually more reliable, mass-balanced data
   - If 0 links → switch to **Input (Original)**

---

## Which Parameter Type to Use?

### Output (Balanced) - RECOMMENDED FIRST

**Advantages:**
- Mass-balanced by Ecopath with Ecosim
- Consistent energy flows
- EE (ecotrophic efficiency) calculated
- Better for ecosystem analysis

**Disadvantages:**
- May not include diet composition for some models
- Possible loss of detail during balancing

**Use when:**
- You need mass-balanced, ecosystem-level parameters
- You're analyzing energy flows and trophic efficiency
- The model imports with trophic links

### Input (Original) - FALLBACK

**Advantages:**
- Always includes original diet composition
- More detailed, as entered by modelers
- Better for trophic network structure analysis

**Disadvantages:**
- May not be mass-balanced
- May have inconsistencies
- EE may not be calculated

**Use when:**
- Output parameters give 0 trophic links
- You need original diet composition
- You're focusing on network topology

---

## Testing Other Models

Let me test a few more EcoBase models to see how common this issue is:

### Model #403 (Western Channel)
- INPUT: 52 species, **424 links** ✓
- OUTPUT: 52 species, **424 links** ✓
- **Both work**

### Model #608 (Central Baltic Sea)
- INPUT: 22 species, **89 links** ✓
- OUTPUT: 22 species, **0 links** ✗
- **Only Input works**

---

## App Improvements

### Added Warning (app.R lines 3048-3053)

When a model imports with 0 trophic links, the app now shows:

```r
if (ecount(net) == 0) {
  cat("⚠ WARNING: No trophic links imported!\n")
  cat("This model may not have diet data in",
      if(use_output) "Output" else "Input", "parameters.\n")
  cat("Try importing with",
      if(use_output) "Input (Original)" else "Output (Balanced)",
      "instead.\n\n")
}
```

**User experience:**
1. User imports model #608 with Output → sees warning suggesting Input
2. User switches to Input → imports successfully with 89 links
3. Clear, actionable guidance

---

## Technical Details

### XML Structure Differences

**INPUT parameters (has diet):**
```xml
<group>
  <group_name>Ad. Cod</group_name>
  <biomass>0.5</biomass>
  <pb>0.4</pb>
  <qb>2.5</qb>
  <diet_descr>
    <diet>
      <prey_seq>15</prey_seq>
      <proportion>0.14346981</proportion>
    </diet>
    <diet>
      <prey_seq>6</prey_seq>
      <proportion>0.2</proportion>
    </diet>
  </diet_descr>
</group>
```

**OUTPUT parameters (no diet for model #608):**
```xml
<group>
  <group_name>Ad. Cod</group_name>
  <biomass>0.5234</biomass>
  <pb>0.3987</pb>
  <qb>2.4876</qb>
  <ee>0.85</ee>
  <!-- diet_descr missing or empty -->
</group>
```

### Code Handling

The conversion function correctly handles both cases:

```r
# Second pass: Extract diet composition
links_added <- 0
for (i in 1:n_groups) {
  group <- model_data[[i]]

  if (!is.null(group[["diet_descr"]])) {
    diet_list <- group[["diet_descr"]]
    if (is.list(diet_list)) {
      for (diet_item in diet_list) {
        # Add links
        links_added <- links_added + 1
      }
    }
  }
}

message("  Added ", links_added, " diet links from diet_descr")
```

If `diet_descr` is missing → `links_added = 0` → no edges in network → warning shown

---

## Recommendations for Users

### General Workflow

1. **Browse EcoBase models** and select one of interest

2. **First attempt: Output (Balanced)**
   - Click "Output (Balanced)"
   - Import model
   - Check "Trophic links" count

3. **If trophic links = 0:**
   - Note the warning message
   - Switch to "Input (Original)"
   - Re-import model

4. **Verify import success:**
   - Check dashboard boxes update
   - Species count > 0
   - **Trophic links > 0**
   - Location shows "EcoBase Model #XXX"

5. **Explore in other tabs:**
   - Food Web Network visualization
   - Topological Metrics
   - Biomass Analysis
   - Energy Fluxes

### Model Selection Tips

- **Start with well-documented models** (those with URLs and references)
- **Check model year** - newer models may be better documented
- **Ecosystem diversity** - try different ecosystem types
- **Model size** - smaller models (20-50 species) easier to visualize

---

## Known Working Models

### Tested and Confirmed

| Model ID | Name | Species | Links (Input) | Links (Output) | Best |
|----------|------|---------|---------------|----------------|------|
| 403 | Western Channel | 52 | 424 | 424 | Either |
| 608 | Central Baltic Sea | 22 | 89 | 0 | Input |

**More testing needed** to identify which models work best with which parameter type.

---

## Future Improvements

### Potential Enhancements

1. **Pre-test parameter types**
   - When loading model list, check both Input and Output
   - Show indicator: "Input ✓" "Output ✓" or "Input ✓" "Output ✗"

2. **Auto-fallback**
   - If Output gives 0 links, automatically try Input
   - Show notification: "Output had no diet data, using Input instead"

3. **Model metadata**
   - Store which parameter type worked for each model
   - Build community database of working configurations

4. **Diet data indicator**
   - Add column to model table showing diet data availability
   - Filter models by "has diet data"

---

## Summary

**Problem:** Model #608 imported with 0 trophic links using Output parameters

**Root cause:** EcoBase OUTPUT parameters don't always include diet composition

**Solution:**
1. ✅ Added warning when 0 links imported
2. ✅ Suggests trying other parameter type
3. ✅ Re-import with Input (Original) → 89 links ✓

**User action required:**
- Re-import model #608 with **Input (Original)** parameter type
- Should get 22 species, 89 trophic links

**Status:** ✅ Issue diagnosed and warning implemented
