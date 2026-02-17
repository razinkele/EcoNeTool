# 🎉 EcoNeTool v1.0.19 Release Notes

**Release Date**: December 8, 2025
**Version**: 1.0.19
**Code Name**: "visNetwork Label & Topology Fix"
**Status**: Stable

---

## 🌟 Overview

This release includes **critical fixes** for network graph visualization issues where node labels were displaying numeric IDs instead of species names, and subsequent topology preservation improvements.

---

## 🐛 Critical Fixes

### 1. visNetwork Node Label Display (CRITICAL)

**Issue**: All network graphs were displaying node IDs (1, 2, 3...) instead of species names.

**Root Cause**:
- Nodes data was being passed to visNetwork as a **list-of-lists** instead of a proper data frame
- visNetwork couldn't parse the nested list structure
- Fell back to displaying node IDs as a default behavior
- Tooltips worked correctly because they directly accessed `V(net)$name`

**Solution**:
- Rewrote `create_foodweb_visnetwork()` to create nodes as a proper **data frame**
- Structure: `data.frame(id, label, group, color, value, shape, x, y, fixed, title)`
- visNetwork now correctly reads the `label` column and displays species names

**Impact**:
- ✅ Interactive Network tab now shows species names
- ✅ Biomass Network tab now shows species names
- ✅ Flux Network tab now shows species names
- ✅ All network dropdowns now show species names

---

### 2. Network Graph Topology Preservation

**Issue**: After fixing labels, graph topology became broken with poor node positioning.

**Root Cause**:
- `fixed` parameter was set as simple boolean: `nodes$fixed <- TRUE`
- This completely locked all node positions (both X and Y)
- Physics engine couldn't adjust spacing, resulting in overlapping nodes

**Solution**:
- Changed `fixed` to a **list-column** structure:
  ```r
  nodes$fixed <- I(lapply(1:nrow(nodes), function(i) list(y = TRUE, x = FALSE)))
  ```
- **Y positions**: FIXED by trophic level (maintains vertical layering)
- **X positions**: FREE for physics engine (optimal horizontal spacing)

**Impact**:
- ✅ Proper trophic level stratification (vertical)
- ✅ Optimal horizontal node spacing (no overlap)
- ✅ Readable, aesthetically pleasing layouts
- ✅ Interactive physics simulation for fine-tuning

---

### 3. Vertex Name Persistence

**Issue**: Vertex names weren't being reliably preserved when creating networks from matrices.

**Solution**: Added explicit `V(net)$name <- species_names` in multiple locations:
- CSV import functions
- ECOPATH native database import
- ECOPATH Windows import
- EcoBase import (2 locations)
- Network matrix editor
- Flux network creation
- Data loading module
- RData upload handler

**Impact**:
- ✅ Guaranteed species name persistence across all data sources
- ✅ Consistent behavior regardless of import method
- ✅ No more numeric vertex names in any workflow

---

## 🔧 Technical Improvements

### visNetwork Data Structure
- **Nodes**: Must be a data.frame (not list)
- **Label Column**: `label` displays on nodes
- **Title Column**: `title` shows in tooltips (HTML supported)
- **Fixed Column**: Must be list-column with `x` and `y` components
- **ID Column**: Numeric IDs for edge connections

### Enhanced Font Styling
```r
font = list(
  size = 14,
  face = 'arial',
  color = '#000000',
  strokeWidth = 0,
  strokeColor = '#ffffff'
)
```

### Improved Node Selection
- Custom dropdown styling
- "Select species" label
- Proper width and height settings

---

## 📁 Files Modified

### Core Functions
- `R/functions/network_visualization.R` (Lines 144-174, 230-238, 268-272)
- `R/functions/flux_calculations.R` (Lines 203-218)
- `R/functions/ecobase_connection.R` (2 locations)
- `R/data_loading.R` (Lines 55-63)

### Application Logic
- `app.R` - 5 locations:
  - Line 383-392: Initialization vertex name check
  - Line 736-744: CSV import
  - Line 1071-1080: ECOPATH native import
  - Line 1250-1258: RData upload handler
  - Line 2422-2428: Network matrix editor

---

## 🧪 Testing

Three new test scripts created:

### 1. `test_vertex_names.R`
Verifies vertex names are properly set in igraph networks.
```
✓ SUCCESS: Vertex names are PROPER SPECIES NAMES
```

### 2. `test_visnetwork_labels.R`
Validates visNetwork label display in data structure.
```
✓ SUCCESS: Node labels are SPECIES NAMES
First 10: "Synchaeta", "Autotroph", "Mixotroph"...
```

### 3. `test_node_structure.R`
Checks node data frame structure and fixed parameter.
```
✓ All structure checks passed!
Fixed: {x: FALSE, y: TRUE}
```

---

## 📚 Documentation

### New Documentation Files
- `VERTEX_NAMES_FIX.md` - Comprehensive vertex name persistence guide
- `VISNETWORK_FIX_COMPLETE.md` - visNetwork label solution details
- `TOPOLOGY_FIX_COMPLETE.md` - Topology preservation explanation
- `RELEASE_NOTES_v1.0.19.md` - This file

### Updated Documentation
- `CHANGELOG.md` - Complete version 1.0.19 entry
- `VERSION` - Bumped to 1.0.19
- `README.md` - Added "Recent Updates" section

---

## 🚀 Upgrade Instructions

### For Users Running Locally

1. **Pull latest changes** (if using git):
   ```bash
   git pull origin master
   ```

2. **Stop the app** (if running):
   - Press ESC or click Stop button in RStudio

3. **Restart the app**:
   ```r
   shiny::runApp()
   ```

4. **Clear browser cache** (if needed):
   - Hard refresh: Ctrl+F5 (Windows) or Cmd+Shift+R (Mac)

5. **Verify the fix**:
   - Navigate to: Food Web Network → Interactive Network
   - Nodes should display species names (e.g., "Synchaeta", "Acartia")
   - Trophic levels should be properly layered vertically
   - Nodes should be well-spaced horizontally

### For Production Deployments

1. **Run pre-deployment checks**:
   ```bash
   cd deployment
   Rscript pre-deploy-check.R
   ```

2. **Deploy to server**:
   ```bash
   sudo ./deploy.sh --shiny-server
   ```

3. **Verify deployment**:
   ```bash
   ./verify-deployment.sh
   ```

4. **Force reload** (if needed):
   ```bash
   ./force-reload.sh
   ```

---

## ✅ Verification Checklist

After upgrading, verify:

- [ ] Network graphs display species names (not numbers)
- [ ] Trophic levels are properly layered vertically
- [ ] Nodes have good horizontal spacing (no overlap)
- [ ] Tooltips show full species information
- [ ] Node selection dropdown shows species names
- [ ] All three network tabs work correctly:
  - [ ] Interactive Network
  - [ ] Biomass Network (in Biomass Analysis tab)
  - [ ] Flux Network (in Energy Fluxes tab)

---

## 🔗 Related Issues

- Network nodes showing numbers instead of titles
- Graph topology broken after label fix
- Vertex names not persisting across imports

---

## 📊 Statistics

- **Files Modified**: 8
- **Lines Changed**: ~150
- **Functions Updated**: 3
- **Test Scripts Created**: 3
- **Documentation Files**: 4 new, 3 updated
- **Bug Severity**: Critical (affecting all network visualizations)
- **Impact**: High (core visualization feature)

---

## 🙏 Acknowledgments

Special thanks for identifying that:
- Tooltips showed correct names (proving vertex names were set)
- Only the graph display was broken (narrowing to visNetwork issue)
- This insight led directly to discovering the data structure problem

---

## 📞 Support

- **Issues**: [GitHub Issues](https://github.com/razinkele/EcoNeTool/issues)
- **Documentation**: See `docs/` directory
- **Changelog**: [CHANGELOG.md](CHANGELOG.md)

---

## 🔮 Next Steps

Future improvements planned:
- Additional network layout options
- Customizable node size and color schemes
- Export network visualizations as images
- Enhanced zoom and pan controls

---

**Happy Analyzing! 🌊🦈🐟**
