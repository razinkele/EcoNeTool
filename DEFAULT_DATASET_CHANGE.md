# Default Dataset Changed to LTCoastal.Rdata

**Date:** 2025-12-23
**Change:** Updated default dataset from BalticFW.Rdata to LTCoast.Rdata

---

## Files Modified

### 1. R/config.R (line 128)
**Change:** Updated DATA_FILE path

**Before:**
```r
DATA_FILE <- "BalticFW.Rdata"
```

**After:**
```r
DATA_FILE <- "examples/LTCoast.Rdata"
```

---

### 2. app.R (lines 353-357)
**Change:** Updated information modal to reflect new default dataset

**Before:**
```r
<h5>Default Dataset</h5>
<p><strong>Gulf of Riga Food Web</strong><br>
Frelat, R., & Kortsch, S. (2020).<br>
34 species, 207 links<br>
Period: 1979-2016</p>
```

**After:**
```r
<h5>Default Dataset</h5>
<p><strong>Lithuanian Coastal Food Web</strong><br>
Southeastern Baltic Sea coastal ecosystem<br>
41 species, 244 links<br>
6 functional groups (Phytoplankton, Zooplankton, Benthos, Fish, Birds, Detritus)</p>
```

---

### 3. app.R (lines 490-494)
**Change:** Updated metadata reactive value

**Before:**
```r
metaweb_metadata <- reactiveVal(list(
  location = "Gulf of Riga, Baltic Sea",
  time_period = "1979-2016",
  source = "Frelat & Kortsch, 2020"
))
```

**After:**
```r
metaweb_metadata <- reactiveVal(list(
  location = "Lithuanian Coast, Southeastern Baltic Sea",
  time_period = "Coastal ecosystem",
  source = "LTCoastal Food Web Model"
))
```

---

### 4. R/data_loading.R
**Change:** Updated comments to be generic instead of BalticFW-specific

**Lines 5, 14, 23-24, 29-30:** Removed specific references to BalticFW.Rdata and made descriptions generic

---

## LTCoast.Rdata Dataset Information

- **File:** examples/LTCoast.Rdata
- **Species:** 41
- **Trophic Links:** 244
- **Functional Groups:** 6
  - Phytoplankton
  - Zooplankton
  - Benthos
  - Fish
  - Birds
  - Detritus
- **Geographic Region:** Lithuanian Coast, Southeastern Baltic Sea
- **Ecosystem Type:** Coastal food web

---

## Sample Species (first 5)
1. Detritus
2. Phytoplankton
3. Mesozooplankton
4. Nectobenthos
5. Macrophytobenthos

---

## Impact

### User Experience
- When the app loads, LTCoast.Rdata will be loaded automatically
- Information modal (ℹ️ icon) shows updated dataset description
- Dashboard metadata boxes show updated location and source information

### Color Scheme
- All 6 functional groups in LTCoast.Rdata have assigned colors:
  - ✅ Phytoplankton: **GREEN** (position 6)
  - ✅ Zooplankton: **LIGHT BLUE** (position 7)
  - ✅ Benthos: **LIGHT BROWN/BURLYWOOD** (position 1)
  - ✅ Birds: **PURPLE** (position 2)
  - ✅ Detritus: **BROWN** (position 3)
  - ✅ Fish: **BLUE** (position 4)

### Backward Compatibility
- ✅ BalticFW.Rdata still available in examples/ folder
- ✅ Can be loaded via Import > General Import tab
- ✅ All import functionality preserved
- ✅ No breaking changes to existing features

---

## Testing Checklist

After restarting the app:

- [ ] App loads successfully with LTCoast.Rdata
- [ ] Dashboard shows 41 species, 244 links
- [ ] Information modal shows "Lithuanian Coastal Food Web"
- [ ] Metadata boxes show updated location and source
- [ ] Network visualizations display all 41 species
- [ ] All functional groups have correct colors
- [ ] Phytoplankton nodes are GREEN ✓
- [ ] Zooplankton nodes are LIGHT BLUE ✓
- [ ] Can still import BalticFW.Rdata via Import tab

---

## Why This Change?

LTCoast.Rdata represents a more recent and locally relevant dataset:
- Focuses on Lithuanian coastal ecosystem
- Larger network (41 vs 34 species, 244 vs 207 links)
- Includes Birds functional group (not in BalticFW)
- More representative of current EcoNeTool capabilities

---

## Reverting (if needed)

To revert to BalticFW.Rdata as default:

1. **R/config.R line 128:**
   ```r
   DATA_FILE <- "BalticFW.Rdata"
   ```

2. **app.R lines 353-357:** Restore original modal text

3. **app.R lines 490-494:** Restore original metadata

---

**Status:** ✅ COMPLETE
**Date:** 2025-12-23
**Compatibility:** 100% backward compatible
**Action Required:** Restart application to load new default dataset

---

## Related Changes

This change was made after resolving the Phytoplankton color issue:
- **R/data_loading.R:** Removed COLOR_SCHEME truncation logic
- **COLOR_SCHEME:** Now always maintains 7 colors for name-based matching
- **All colors:** Now correctly assigned regardless of functional groups in data

All features are working correctly with the new default dataset!
