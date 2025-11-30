# EcoNeTool - Version 2.1 Changelog

## Overview

Version 2.1 transforms EcoNeTool from a Baltic-specific tool into a **generic Food Web Explorer** that supports custom data import in multiple formats.

**Latest Update:** Version 2.1.1 (2025-11-27) - Critical bug fix for flux network visualization

---

## 🔧 Version 2.1.1 - Bug Fix Release

**Date:** 2025-11-27

### Critical Fix: Flux Network Visualization Error

**Issue:** "could not find function 'trophiclevels'" error in Energy Fluxes tab
- **Severity:** Critical
- **Location:** Flux-weighted Network plot
- **Impact:** Prevented flux analysis visualization from working

**Root Cause:**
- BalticFW.Rdata contained both data AND functions (trophiclevels, plotfw, fluxind)
- Loading the data file overwrote app.R's function definitions
- This caused scoping issues and function conflicts

**Solution:**
1. ✅ Load BalticFW.Rdata into separate environment (not GlobalEnv)
2. ✅ Extract only data objects (net, info), not functions
3. ✅ Added fluxind() implementation to app.R
4. ✅ Updated data upload handler with same approach
5. ✅ All app functions now take precedence over data file functions

**Files Modified:**
- `app.R` (lines 47-68, 299-383, 1119-1144)
- Added comprehensive documentation in `FIX_TROPHICLEVELS_ERROR.md`
- Created test suite: `test_final_fix.R`

**Test Results:** ✅ All tests passing
- App trophiclevels() works correctly
- App fluxind() works correctly
- Flux network plot generates successfully
- Flux indicators calculate correctly

**Benefits:**
- 🎯 Better code architecture (separation of data and code)
- 🎯 More maintainable and debuggable
- 🎯 Safer for user-uploaded data
- 🎯 Prevents future function conflicts

---

## 🎯 Major Changes

### 1. **Generic Branding** ✅
**Changed:** "Baltic Food Web Explorer" → "Food Web Explorer"

**Locations Updated:**
- Application title (header)
- Page title (browser tab)
- Footer
- Dashboard welcome message
- Controlbar information
- All documentation references

**Why:** Makes the tool applicable to any marine/terrestrial food web, not just Baltic Sea

---

### 2. **Data Import Feature** ✅ NEW!

#### New Menu Item
- Added **"Data Import"** to sidebar navigation
- Icon: 📤 (upload)
- Position: Second item (after Dashboard, before Network)

#### Comprehensive Format Documentation

**Excel Format (.xlsx, .xls):**
- Sheet 1: Network (adjacency matrix)
  - Square matrix with species names as rows/columns
  - 1 = predation link (row eats column)
  - 0 = no link
- Sheet 2: Species_Info
  - Required columns: species, fg, meanB, losses, efficiencies
  - Optional columns: bodymasses, taxon, nbY
- Complete table examples included
- Column descriptions and units

**CSV Format (.csv):**
- Two files required:
  - network.csv (adjacency matrix)
  - species_info.csv (species attributes)
- Sample file contents with proper formatting
- Comma-separated values

**RData Format (.Rdata, .rda):**
- R workspace with two objects:
  - `net`: igraph object
  - `info`: data.frame
- Complete R code example to create file
- Fully functional import

#### File Upload Interface
- File input widget (supports .xlsx, .xls, .csv, .Rdata, .rda)
- "Load Data" button
- Real-time status display
- Maximum file size: 10 MB

#### Server-Side Functionality
- **RData Import**: FULLY FUNCTIONAL ✅
  - Loads net and info objects
  - Validates structure
  - Upgrades igraph if needed
  - Updates all visualizations
  - Success/error messages

- **Excel Import**: Documented (placeholder for future)
  - Format fully documented
  - Stub implementation with helpful message

- **CSV Import**: Documented (placeholder for future)
  - Format fully documented
  - Stub implementation with helpful message

#### Data Validation
- Checks for required objects/columns
- Validates species name matching
- Confirms positive biomass values
- Error messages with troubleshooting tips

#### User Workflow
1. Launch app (default: Gulf of Riga data)
2. Navigate to "Data Import" tab
3. Read format documentation
4. Upload .Rdata file OR use default
5. Click "Load Data"
6. Explore your data in all tabs
7. Refresh page to reset

---

## 📝 Updated Content

### Dashboard (Home Page)
**Before:**
```
Baltic Food Web Explorer
Gulf of Riga food web (1979-2016)
```

**After:**
```
Food Web Explorer
Current Dataset: Gulf of Riga (default)
Features: Data Import + 4 analysis modules
```

**Changes:**
- Generic introduction
- Mentions custom data import capability
- Lists current dataset as "default" (not only option)
- Navigation instructions include Data Import

### Controlbar (Info Panel)
**Before:**
```
Version: 2.0
Data Source: Gulf of Riga
```

**After:**
```
Version: 2.1
About: Generic food web analysis tool
Default Dataset: Gulf of Riga
Data Import: Supported formats listed
```

**Changes:**
- Emphasizes generic nature
- Lists Gulf of Riga as default (not exclusive)
- Documents import capability
- Lists supported formats

---

## 🎨 UI Enhancements

### New Tab: Data Import
```
┌─────────────────────────────────────────────┐
│ Data Import & Format Guide                  │
├─────────────────────────────────────────────┤
│ Import Your Own Food Web Data               │
│ Supports: Excel, CSV, RData                 │
└─────────────────────────────────────────────┘

┌─────────────────────────────────────────────┐
│ Supported File Formats           [Collapse] │
├─────────────────────────────────────────────┤
│ 1. Excel Format                              │
│    - Sheet 1: Network (adjacency matrix)    │
│    - Sheet 2: Species_Info                  │
│    [Table examples]                          │
│                                              │
│ 2. CSV Format                                │
│    - network.csv                             │
│    - species_info.csv                        │
│    [Code examples]                           │
│                                              │
│ 3. RData Format                              │
│    - net + info objects                      │
│    [R code example]                          │
└─────────────────────────────────────────────┘

┌──────────────────────┐ ┌──────────────────────┐
│ Upload Your Data     │ │ Data Validation      │
│ [File Input]         │ │ Requirements:        │
│ [Load Data Button]   │ │ ✓ Names match        │
│ Status: ...          │ │ ✓ Square matrix      │
└──────────────────────┘ └──────────────────────┘

┌─────────────────────────────────────────────┐
│ Example Datasets                  [Collapse]│
├─────────────────────────────────────────────┤
│ Templates and samples                        │
└─────────────────────────────────────────────┘
```

---

## 🔧 Technical Implementation

### New Constants
```r
# None added - uses existing configuration
```

### New Functions
```r
# Server-side handlers
output$data_upload_status <- renderPrint({ ... })
observeEvent(input$load_data, { ... })
```

### New UI Components
```r
fileInput("data_file", ...)
actionButton("load_data", ...)
verbatimTextOutput("data_upload_status")
```

### File Structure
```
app.R
├── Configuration (unchanged)
├── Data Loading (unchanged - default)
├── Helper Functions (unchanged)
├── UI
│   ├── Header (updated title)
│   ├── Sidebar
│   │   ├── Dashboard
│   │   ├── Data Import (NEW!)
│   │   ├── Food Web Network
│   │   ├── Topological Metrics
│   │   ├── Biomass Analysis
│   │   └── Energy Fluxes
│   ├── Body
│   │   ├── Dashboard Tab (updated text)
│   │   ├── Data Import Tab (NEW! ~200 lines)
│   │   ├── Network Tab (unchanged)
│   │   ├── Topological Tab (unchanged)
│   │   ├── Biomass Tab (unchanged)
│   │   └── Fluxes Tab (unchanged)
│   ├── Controlbar (updated info)
│   └── Footer (updated title)
└── Server
    ├── Data Import Handler (NEW! ~100 lines)
    └── Visualization Outputs (unchanged)
```

---

## 📊 File Format Specifications

### Excel (.xlsx, .xls)

**Sheet 1: Network**
```
        Species_A  Species_B  Species_C
Species_A    0         1          0
Species_B    0         0          1
Species_C    0         0          0
```

**Sheet 2: Species_Info**
```
species         fg              meanB    losses    efficiencies
Species_A       Fish            1250.5   0.12      0.85
Species_B       Zooplankton     850.2    0.08      0.75
Species_C       Phytoplankton   2100.0   0.05      0.40
```

### CSV

**network.csv**
```csv
species,Species_A,Species_B,Species_C
Species_A,0,1,0
Species_B,0,0,1
Species_C,0,0,0
```

**species_info.csv**
```csv
species,fg,meanB,losses,efficiencies
Species_A,Fish,1250.5,0.12,0.85
Species_B,Zooplankton,850.2,0.08,0.75
Species_C,Phytoplankton,2100.0,0.05,0.40
```

### RData

```r
library(igraph)

# Create network
adj_matrix <- matrix(...)
net <- graph_from_adjacency_matrix(adj_matrix, mode='directed')

# Create species info
info <- data.frame(
  species = c(...),
  fg = factor(c(...)),
  meanB = c(...),
  losses = c(...),
  efficiencies = c(...)
)

# Save
save(net, info, file='my_foodweb.Rdata')
```

---

## ✅ Testing Results

### All Tests Passed
```
[1/6] Application loaded ✓
[2/6] UI structure valid ✓
[3/6] Menu items present ✓
[4/6] Default data loaded ✓
[5/6] Configuration updated ✓
[6/6] New features validated ✓
```

### Validation
- ✅ Syntax check passed
- ✅ No errors on startup
- ✅ All menu items functional
- ✅ Default data loads correctly
- ✅ File upload UI present
- ✅ Server handlers defined
- ✅ RData import works
- ✅ Error handling functional
- ✅ Status messages clear

---

## 📚 Documentation Updates

### New Files
- `test_v2.1.R` - Comprehensive test suite
- `CHANGELOG_v2.1.md` - This file

### Updated Files
- `app.R` - Main application (+200 lines for import tab, +100 lines for server)

### Documentation Needs (Future)
- Update `BS4DASH_README.md` with Data Import instructions
- Update `QUICK_START_BS4DASH.md` with upload workflow
- Update `LAUNCH_GUIDE.md` with new tab description
- Create data format template files (Excel, CSV)

---

## 🚀 Usage

### Launch Application
```r
library(shiny)
runApp()
```

### Use Default Data
- App launches with Gulf of Riga dataset pre-loaded
- Navigate to analysis tabs directly
- No upload required

### Import Custom Data
1. Click "Data Import" in sidebar
2. Read format documentation
3. Prepare your data (Excel/CSV/RData)
4. Click "Choose File"
5. Select your .Rdata file
6. Click "Load Data"
7. Check status message
8. Navigate to analysis tabs

### Reset to Default
- Refresh browser page (F5 or Ctrl+R)
- App reloads with Gulf of Riga data

---

## 🔮 Future Enhancements

### Planned for v2.2
- [ ] Full Excel import implementation (readxl)
- [ ] Full CSV import implementation
- [ ] Data validation visualizations
- [ ] Example file downloads
- [ ] Import history/recent files
- [ ] Data preview before loading

### Planned for v2.3
- [ ] Multiple dataset management
- [ ] Dataset comparison tools
- [ ] Export analysis results
- [ ] Custom color schemes
- [ ] Functional group customization

---

## 🐛 Known Limitations

### Current Version (2.1)
- **Excel Import**: Documented but not implemented (placeholder)
- **CSV Import**: Documented but not implemented (placeholder)
- **RData Only**: Only RData format fully functional
- **Single Dataset**: Can only work with one dataset at a time
- **No Persistence**: Uploaded data lost on page refresh

### Workarounds
- Convert Excel/CSV to RData using provided R code
- Save your .Rdata file for quick re-upload
- Use RStudio to prepare data before upload

---

## 📊 Statistics

### Lines of Code Added
- UI (Data Import Tab): ~200 lines
- Server (Upload Handler): ~100 lines
- Documentation (in-app): ~150 lines
- Total: ~450 lines of new code

### Files Modified
- `app.R`: Updated (1 file)

### Files Created
- `test_v2.1.R`: New test suite
- `CHANGELOG_v2.1.md`: This document

### Features Added
- Data import documentation: 3 formats
- File upload interface: 1 widget
- Upload button: 1 action
- Status display: 1 output
- Server handlers: 2 functions
- Menu item: 1 navigation item

---

## 🎓 Learning Resources

### For Users
- See in-app documentation (Data Import tab)
- Check example datasets (coming soon)
- Read format specifications above

### For Developers
- See `app.R` lines 597-794 (UI)
- See `app.R` lines 1065-1166 (Server)
- Check error handling patterns
- Review validation logic

---

## 📞 Support

### Issues
- RData import not working? Check object names (net, info)
- Excel/CSV not working? Use RData format (Excel/CSV coming in v2.2)
- Validation errors? Check required columns match specs
- Upload fails? Check file size < 10 MB

### Contact
- Check documentation in Data Import tab
- Review format examples
- Consult technical specifications in this file

---

## 📄 Version History

| Version | Date | Description |
|---------|------|-------------|
| 1.0 | 2024 | Initial release (Baltic-specific) |
| 1.1 | 2025-11-27 | Code quality improvements |
| 2.0 | 2025-11-27 | bs4Dash interface |
| 2.0.1 | 2025-11-27 | Bug fixes (menuItem, igraph) |
| **2.1** | **2025-11-27** | **Generic tool + Data Import** |

---

## ✨ Summary

Version 2.1 successfully transforms EcoNeTool into a **generic, extensible platform** for food web analysis. Users can now:

✅ Use the default Gulf of Riga dataset
✅ Upload their own RData files
✅ Read comprehensive format documentation
✅ Validate their data before analysis
✅ Work with any marine or terrestrial food web

The application maintains **100% backward compatibility** while adding powerful new import capabilities.

---

**Version**: 2.1
**Status**: ✅ Production Ready
**Release Date**: 2025-11-27
**Next Version**: 2.2 (Excel/CSV import)
