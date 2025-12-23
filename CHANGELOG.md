# Changelog

All notable changes to EcoNeTool will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.20] - 2025-12-23

### Fixed - Critical Color Scheme Issues
- **Phytoplankton and Zooplankton Colors**
  - Fixed Phytoplankton displaying as light blue instead of green
  - Fixed Zooplankton displaying as red instead of light blue
  - Root cause: R/data_loading.R was truncating COLOR_SCHEME from 7 to 5 colors based on data
  - Root cause: .RData workspace file cached old 5-color scheme
  - Solution: Removed COLOR_SCHEME adjustment logic - always maintain full 7-color scheme
  - Solution: Color matching now by functional group NAME, not factor index
  - All functional groups now display with correct colors across all network visualizations

- **visNetwork Color Integration**
  - Fixed legend colors not matching node colors
  - Added explicit node color assignment with visNetwork list structure
  - Added visGroups() definitions for consistency
  - Removed auto-color assignment by visNetwork
  - Result: Legend and nodes use identical colors throughout app

### Fixed - Flux Network Visualization
- **Validation Errors**
  - Fixed "unused argument (min_rows = 1)" error in flux calculations
  - Removed invalid min_rows parameter from validate_dataframe() calls
  - Files modified: flux_calculations.R, keystoneness.R, topological_metrics.R

- **Empty Edge Network Handling**
  - Added check for empty flux networks (ecount == 0)
  - Safe max() calculation with is.finite() validation
  - Proper handling of NA/infinite flux values
  - Edge name validation and filtering
  - Informative error messages instead of single "Error" node display

### Changed - Default Dataset
- **Lithuanian Coastal Food Web (LTCoast.Rdata)**
  - Changed default from BalticFW.Rdata (Gulf of Riga) to LTCoast.Rdata
  - 41 species (vs 34), 244 links (vs 207)
  - 6 functional groups: Phytoplankton, Zooplankton, Benthos, Fish, Birds, Detritus
  - Updated information modal and metadata throughout app
  - BalticFW still available via Import tab

### Added - Metaweb Export Feature
- **Export Current Network to RData**
  - New export_metaweb_rda() function in metaweb_io.R
  - Export UI in Import > General Import tab
  - Custom filename input
  - Compatible with BalticFW.Rdata format
  - Preserves all network and species information
  - Removes dynamic columns (colfg) before export

### Improved - Code Organization
- **Modular Architecture**
  - Removed debug output from production code
  - Cleaned up color assignment logic across 6 locations in app.R
  - Improved comments and documentation in config files
  - Better separation of concerns in network visualization

### Documentation
- Updated README.md with v1.0.20 information
- Created detailed fix documentation:
  - PHYTOPLANKTON_COLOR_FINAL_FIX.md - Complete root cause analysis
  - DEFAULT_DATASET_CHANGE.md - LTCoast dataset change
  - SESSION_COMPLETE_2025-12-23.md - Complete session summary
- Updated CHANGELOG.md with all changes

## [1.1.0] - 2025-12-18

### Added - EMODnet Habitat Integration
- **Complete EMODnet EUSeaMap Habitat Integration**
  - Integrated EMODnet EUSeaMap 2025 broad-scale habitat maps (Baltic Sea coverage)
  - Added habitat enrichment for species data with EUNIS codes, substrate types, and depth zones
  - Created two-step workflow: (1) Clip habitat to study area, (2) Overlay with grid cells
  - Habitat visualization on maps with color-coded EUNIS types (Spectral palette)
  - Real-time habitat statistics: dominant EUNIS, substrate composition, Shannon diversity
  - Supports 592K+ habitat polygons with bounding box filtering for performance

- **New R Functions** (`R/functions/emodnet_habitat_utils.R`)
  - `load_euseamap()` - Load EUSeaMap with bbox filtering and geometry validation
  - `get_habitat_at_location()` - Query habitat at coordinates
  - `add_habitat_to_species()` - Add habitat columns to species dataframes
  - `clip_habitat_to_study_area()` - Clip habitat polygons to study boundary
  - `overlay_habitat_with_grid()` - Calculate habitat stats per grid cell (13 new attributes)
  - `print_euseamap_summary()` - Display layer information

- **Grid Cell Habitat Attributes** (13 new columns)
  - `dominant_eunis` - Most common EUNIS habitat code (area-weighted)
  - `dominant_habitat` - EUNIS habitat description
  - `dominant_substrate` - Main substrate type (Sand/Mud/Rock/Mixed/Other)
  - `dominant_biozone` - Biological zone classification
  - `dominant_energy` - Energy level (wave/current exposure)
  - `n_habitats` - Number of different habitat types in cell
  - `habitat_diversity` - Shannon diversity index of EUNIS codes
  - `habitat_area_km2` - Total habitat area within cell
  - `substrate_sand_pct` - Percentage sand substrate
  - `substrate_mud_pct` - Percentage mud substrate
  - `substrate_rock_pct` - Percentage rock substrate
  - `substrate_mixed_pct` - Percentage mixed substrate
  - `substrate_other_pct` - Percentage other substrate types

### Added - Spatial Analysis Enhancements
- **BBT (Broad Belt Transect) Polygon Selector**
  - Pre-defined BBT areas: Archipelago, Balearic, Bay of Gdansk, Bay of Biscay, Heraklion, Hornsund, Irish Sea, Kongsfjord, Lithuanian, North Sea, Porsangerfjord, Sardinia
  - One-click selection of standardized sampling areas
  - Auto-loads study area boundary when BBT selected

- **Study Area Preview Map** (Tab 0)
  - Interactive preview map showing uploaded study area boundary
  - Real-time display updates when file uploaded
  - Blue boundary with area calculation in popup
  - Empty state with helpful message when no area loaded

- **Study Area Information Display**
  - Reactive info output showing geometry type, CRS, features, bounding box, and area
  - Auto-updates when study area changes
  - CRS type recognition and display
  - Bounding box coordinates with 3 decimal precision

- **Habitat Layer Visualization**
  - EMODnet habitat polygons displayed on main spatial map after clipping
  - Color-coded by EUNIS habitat type (rainbow Spectral palette)
  - Interactive popups with EUNIS code, habitat description, substrate, and biozone
  - Toggleable "Habitat" layer in map layer control
  - 50% opacity for overlay compatibility

- **Enhanced Map Metrics Selector**
  - Grouped metrics: Food Web Metrics and Habitat Attributes
  - New habitat visualization options:
    - Dominant EUNIS Code
    - Dominant Substrate
    - Habitat Diversity (Shannon)
    - Number of Habitats
    - Habitat Area (km²)

### Added - Taxonomic Integration
- **Species-Level Habitat Data** (Import module)
  - EMODnet habitat enrichment for species during ECOPATH import
  - Location-based habitat assignment (user specifies sampling coordinates)
  - Default location: Gdansk Bay (54.5189°N, 18.6466°E)
  - Adds 9 habitat columns to species data during import

### Fixed - CRS & Geometry Handling
- **Enhanced CRS Detection and Transformation**
  - Improved CRS detection for projected systems without EPSG codes
  - Now detects and transforms TM Baltic (+proj=tmerc) automatically
  - Handles Estonian (EPSG:3301), Finnish (EPSG:3067), Swedish (EPSG:3006) systems
  - Checks both EPSG code and proj string for WGS84 identification
  - Shows clear notification: "Study area transformed from X to WGS84"

- **Geometry Validation** (`emodnet_habitat_utils.R`)
  - Added GDAL config options to handle invalid geometries:
    - `OGR_GEOMETRY_ACCEPT_UNCLOSED_RING=YES`
    - `OGR_SKIP_INVALID_GEOMETRIES=YES`
  - Disabled S2 spherical geometry for complex polygon handling
  - Using `check_ring_dir=FALSE` for permissive geometry reading
  - Validates and removes invalid geometries after loading
  - Reports invalid geometry count and percentage removed

- **Study Area Loading** (Spatial Analysis module)
  - Fixed EPSG comparison issue (handles NA EPSG codes)
  - CRS transformation now works for all coordinate systems
  - Proper handling of multi-polygon features (auto-dissolve to single boundary)
  - Geometry validation with `st_make_valid()` for problematic features

### Fixed - UI/UX Improvements
- **Study Area Reactive Outputs**
  - Fixed non-reactive study area info and preview map
  - Outputs now truly reactive - update automatically when file uploaded
  - Single source of truth for each output (no duplicate renders)
  - Proper reactive dependency on `spatial_study_area()` value

- **Suppressed Warnings**
  - Suppressed CRS warnings from leaflet (sf layer datum messages)
  - Suppressed jsonlite named vector warnings (8+ warnings per upload)
  - Clean console output for better user experience

- **Improved UI Layout** (Spatial Analysis Tab 0)
  - Replaced yellow warning box with blue info box for file formats
  - Moved file format help to tooltip (less visual clutter)
  - Study area info displays below file input (better information flow)
  - Preview map in right pane for immediate visual feedback

### Changed - Performance Optimizations
- **EMODnet Data Loading**
  - Bounding box filtering reduces data from 3.6M to 592K polygons (Baltic Sea)
  - Default bbox: 10-31°E, 53-66°N (configurable)
  - Load time: ~20 seconds (one-time, cached in session)
  - Memory usage: ~825 MB (Baltic Sea subset)

- **Habitat Overlay Processing**
  - Area-weighted statistics for dominant values (not count-based)
  - Shannon diversity calculated from area coverage
  - Substrate standardization to 5 categories
  - Processing time: ~1-2 minutes for typical grid (depends on cell count)

- **Spatial Query Optimization**
  - Using `st_filter()` instead of `st_intersection()` for point queries
  - Query time: ~8 seconds per location
  - Planar geometry (non-S2) for faster processing
  - Spatial indexing for efficient intersection

### Documentation
- **New Documentation Files** (13 files created)
  - `EMODNET_INTEGRATION_COMPLETE.md` - Complete EMODnet integration guide
  - `EMODNET_INTEGRATION_QUICKSTART.md` - Step-by-step implementation
  - `TAXONOMIC_HABITAT_QUICKSTART.md` - Quick reference guide
  - `SPATIAL_HABITAT_INTEGRATION_COMPLETE.md` - Spatial module integration
  - `SPATIAL_UI_IMPROVEMENTS_COMPLETE.md` - UI/UX improvements
  - `CRS_WARNINGS_FIX.md` - CRS transformation fixes
  - `STUDY_AREA_REACTIVE_FIX.md` - Reactive outputs fix
  - `SPATIAL_BOUNDARY_FIX.md` - Study area loading fix
  - Plus 5 additional technical documentation files

### Technical Details
- **New Dependencies**
  - `sf` package (spatial features) - required for EMODnet integration
  - EUSeaMap_2025.gdb (2 GB) - optional but recommended for habitat data

- **New Reactive Values**
  - `euseamap_data` - Cached EMODnet habitat layer
  - `spatial_habitat_clipped` - Habitat clipped to study area
  - `spatial_grid_with_habitat` - Grid enriched with habitat attributes

- **Files Modified**
  - `app.R` - Added EMODnet observers, reactive values, habitat visualization
  - `R/ui/spatial_ui.R` - Added Tab 2 (Habitat Data), BBT selector, improved layout
  - `R/ui/import_ui.R` - Added EMODnet checkbox and location inputs
  - `R/functions/emodnet_habitat_utils.R` - New file (687 lines)

### Migration Notes
- **For Existing Users**
  - EMODnet features are optional - app works without EUSeaMap data
  - If using habitat features, download EUSeaMap_2025.zip and extract to `data/`
  - Install `sf` package: `install.packages("sf")`
  - Existing spatial workflows unchanged - new features additive only

- **Data Requirements**
  - EUSeaMap_2025.gdb required for habitat integration (~2 GB download)
  - Study areas in any CRS now supported (auto-transforms to WGS84)
  - Shapefiles must include all components (.shp, .shx, .dbf, .prj)

## [1.0.19] - 2025-12-08

### Fixed
- **visNetwork Node Label Display** (CRITICAL FIX)
  - Fixed network graphs displaying node IDs (1, 2, 3...) instead of species names
  - Root cause: nodes data was passed as list-of-lists instead of data frame
  - visNetwork couldn't parse the nested list structure, fell back to displaying node IDs
  - Solution: Converted nodes structure to proper data frame with columns
  - **Result**: All network graphs now correctly display species names on nodes

- **Network Graph Topology Preservation**
  - Fixed broken topology caused by incorrect `fixed` parameter
  - Changed from `fixed = TRUE` (locked all positions) to proper list-column structure
  - `fixed = list(y = TRUE, x = FALSE)` - Y fixed by trophic level, X free for spacing
  - **Result**: Proper trophic level layering (vertical) with optimal horizontal spacing

- **Vertex Name Persistence**
  - Added explicit vertex name assignment in all network creation functions
  - Ensures species names are preserved when creating networks from adjacency matrices
  - Applied to: CSV imports, ECOPATH imports, EcoBase imports, flux calculations, network editor
  - **Files Modified**: `app.R` (5 locations), `R/data_loading.R`, `R/functions/ecobase_connection.R` (2x), `R/functions/flux_calculations.R`

### Changed
- **R/functions/network_visualization.R** (Lines 144-174)
  - Rewrote `create_foodweb_visnetwork()` nodes structure from list-of-lists to data frame
  - Added proper `fixed` column as list-column: `I(lapply(...))`
  - Enhanced font configuration for better label visibility
  - Improved node selection dropdown configuration

- **Enhanced Label Styling**
  - Added explicit font settings: size=14, face='arial', color='#000000'
  - Added font stroke for better readability on colored backgrounds
  - Configured node selection dropdown with custom styling

### Technical Details
- **visNetwork Data Structure Requirements**:
  - Nodes must be a data frame, not a list
  - `fixed` column must contain list elements, not boolean values
  - Each `fixed` element must specify `x` and `y` separately
  - `label` column displays on nodes, `title` column shows in tooltips

- **Trophic Level Layout**:
  - Y positions calculated from trophic levels (fixed)
  - X positions calculated by grouping similar trophic levels (initial)
  - Physics engine (BarnesHut) adjusts X positions for optimal spacing
  - Prevents node overlap while maintaining vertical trophic structure

### Testing
- Created `test_vertex_names.R` - Verifies vertex names are set correctly
- Created `test_visnetwork_labels.R` - Validates visNetwork label display
- Created `test_node_structure.R` - Checks node data frame structure and fixed parameter
- All tests pass with species names displayed correctly

### Documentation
- Added `VERTEX_NAMES_FIX.md` - Documents vertex name persistence fixes
- Added `VISNETWORK_FIX_COMPLETE.md` - Details visNetwork label solution
- Added `TOPOLOGY_FIX_COMPLETE.md` - Explains topology preservation fix

## [1.0.18] - 2025-12-08

### Added
- **Rpath Shiny Module** (`R/modules/rpath_module.R`)
  - Complete modular integration of ECOPATH/ECOSIM functionality
  - Self-contained UI and server logic using Shiny modules pattern
  - Automatic Rpath package detection with installation guidance
  - 5 integrated tabs: Model Setup, Mass Balance, MTI Analysis, Ecosim, Scenarios
  - Smart status indicators and error handling
  - Reactive data pipeline with progress notifications
  - Ready for integration into main app (see `RPATH_MODULE_INTEGRATION.md`)

- **Rpath Module Integration Guide** (`RPATH_MODULE_INTEGRATION.md`)
  - Complete integration instructions
  - Code examples for app.R integration
  - Testing checklist and troubleshooting
  - Customization guidelines
  - Performance considerations

### Fixed
- **Network Graph Node Labels**
  - Fixed nodes showing numbers instead of species names
  - Added explicit font configuration: `font = list(size = 14)`
  - Now displays species labels clearly on all network visualizations

### Changed
- **R/functions/network_visualization.R** (Line 225)
  - Added font size parameter to visNodes configuration
  - Ensures node labels are visible and readable

## [1.0.17] - 2025-12-08

### Fixed
- **Fish Species Misclassification**
  - Fixed fish species being incorrectly classified as benthos
  - Added "iformes" pattern to catch fish orders (Cypriniformes, Perciformes, etc.)
  - Added "goby" singular pattern (was only matching "gobies" plural)
  - Added "cyprin", "percidae", "cyprinidae" patterns for common fish families
  - Now correctly classifies: Sand goby, Round goby, Cypriniformes, Perciformes

### Changed
- **Network Visualization - All Graphs**
  - Removed navigation widgets from all network visualizations
  - Changed to straight edges (was curved)
  - Hardcoded these settings in `create_foodweb_visnetwork()`

- **R/functions/network_visualization.R** (Lines 92-102, 221, 253)
  - Removed `show_navigation_buttons` and `straight_edges` parameters
  - Always uses straight edges: `smooth = FALSE`
  - Always hides navigation: `navigationButtons = FALSE`

- **R/functions/functional_group_utils.R** (Lines 71-75)
  - Enhanced fish detection pattern with order and family names
  - Now catches taxonomic names (orders, families) that were falling through

- **app.R** (Lines 1862-1874)
  - Replaced biomass network inline code with `create_foodweb_visnetwork()` call
  - Reduced code from ~150 lines to ~12 lines
  - Now uses same visualization approach as other network graphs

- **app.R** (Lines 1778-1783, 1933-1940)
  - Removed obsolete `show_navigation_buttons` and `straight_edges` parameters
  - Simplified function calls

## [1.0.16] - 2025-12-08

### Added
- **Common Network Visualization Function**
  - Created `create_foodweb_visnetwork()` in R/functions/network_visualization.R
  - Unified visualization approach across all network graphs
  - Supports flexible node sizing (biomass_sqrt, biomass_log, fixed)
  - Supports flexible edge coloring (prey, predator, fixed, default)
  - Consistent trophic level positioning and physics parameters

- **Multistanza Group Support in ECOPATH Import**
  - Now reads both `EcopathGroup` and `stanzaEcopathGroup` tables
  - Merges regular and multistanza groups (species with multiple life stages)
  - Adds `is_multistanza` indicator column to group data
  - Preserves all stanza-specific attributes (growth, mortality, etc.)

### Changed
- **app.R** (Lines 1771-1791)
  - Replaced Food Web Network visualization with `create_foodweb_visnetwork()`
  - Reduced code from ~100 lines to ~15 lines
  - Maintains curved edges and navigation buttons

- **app.R** (Lines 2033-2080)
  - Replaced Energy Fluxes visualization with `create_foodweb_visnetwork()`
  - Passes flux network and edge data for weighted visualization
  - Reduced code from ~125 lines to ~47 lines

- **ecopath_windows_import.R** (Lines 77-136)
  - Enhanced to read both regular and stanza group tables
  - Intelligently merges tables with different column sets
  - Reports multistanza group count in console output

## [1.0.15] - 2025-12-08

### Added
- **ECOPATH Import Extension Support**
  - Added `.eweaccdb` file extension support for ECOPATH native database imports
  - Updated file input accept list: `.ewemdb`, `.eweaccdb`, `.mdb`, `.eiidb`, `.accdb`
  - Updated UI help text to reflect new supported extension

### Changed
- **R/ui/import_ui.R** (Lines 100, 107)
  - Extended ECOPATH file type support to include EwE Access database format (.eweaccdb)

## [1.0.0] - 2024-12-01

### 🎉 Major Refactoring Release

This release introduces significant code improvements with no breaking changes to functionality.

### Added
- **functions.R** - New modular analysis functions file
  - All analysis and visualization functions extracted from app.R
  - Comprehensive Roxygen2 documentation for all functions
  - Logical grouping by function type (trophic, topological, flux, keystone)
- **Deployment Scripts**
  - `verify-deployment.sh` - Check what's actually deployed on server
  - `force-reload.sh` - Nuclear option for clearing all caches and redeploying
  - Enhanced `deploy.sh` with automatic cache clearing
- **Documentation**
  - Enhanced main README.md with comprehensive guides
  - REFACTORING.md - Detailed refactoring notes
  - CHANGELOG.md - This file
  - Deployment troubleshooting guide
- **Validation**
  - Enhanced `pre-deploy-check.R` to validate both app.R and functions.R
  - Better function detection and validation logic

### Changed
- **app.R** - Streamlined to ~1200 lines (from ~2700)
  - Removed all function definitions (~600 lines)
  - Added `source("functions.R")` to load analysis functions
  - Added `library(MASS)` for matrix operations
  - Removed `plotfw` extraction from BalticFW.Rdata
  - Now contains only UI, server logic, and configuration
- **Data Loading**
  - BalticFW.Rdata functions no longer extracted
  - All functions now defined in functions.R
  - Only data objects (`net` and `info`) loaded from .Rdata file
- **Deployment Process**
  - `deploy.sh` now includes cache clearing before restart
  - Uses `stop` + `start` instead of `restart` for cleaner reload
  - Shows browser cache warning in success message
- **Documentation**
  - deployment/README.md - Updated with new scripts and troubleshooting
  - Added "Seeing Old Version After Deployment?" section

### Removed
- Removed duplicate function definitions from app.R
- Removed `plotfw.R` references (file never existed)
- Removed extraction of functions from BalticFW.Rdata

### Fixed
- Fixed issue where `plotfw.R` was referenced but didn't exist
- Fixed deployment caching issues causing old versions to display
- Fixed pre-deploy-check not finding functions after refactoring

### Technical Details

#### Code Organization
```
Before: app.R (2700 lines)
        - Configuration
        - Data loading
        - Functions
        - UI
        - Server

After:  app.R (1200 lines)         functions.R (1100 lines)
        - Configuration             - Trophic calculations
        - Data loading              - Visualization
        - UI                        - Topological metrics
        - Server                    - Flux-based metrics
                                    - Keystone analysis
```

#### Functions Module Structure
- **Trophic Level Calculations**
  - `trophiclevels()` - Iterative method
  - `trophiclevels_shortpath()` - Shortest path method
- **Visualization**
  - `plotfw()` - Food web plotting with trophic layout
- **Network Metrics**
  - `get_topological_indicators()` - Qualitative metrics (S, C, G, V, TL, Omni)
  - `get_node_weighted_indicators()` - Biomass-weighted metrics (nwC, nwG, nwV, nwTL)
- **Flux Analysis**
  - `calculate_losses()` - Metabolic losses calculation
  - `get_fluxweb_results()` - Energy flux calculations
  - `fluxind()` - Link-weighted indicators (lwC, lwG, lwV)
- **Keystone Species**
  - `calculate_mti()` - Mixed Trophic Impact matrix
  - `calculate_keystoneness()` - Keystoneness index

### Migration Notes
- No changes required for end users
- Functionality remains identical
- For developers: new functions should be added to functions.R

---

## [0.9.0] - 2024-11-27

### Initial Release

- Interactive Shiny dashboard for marine food web analysis
- Gulf of Riga dataset (34 taxa, 207 links)
- Network visualization with igraph and visNetwork
- Topological metrics calculation
- Biomass-weighted analysis
- Energy flux calculations using fluxweb
- Keystoneness analysis with MTI
- Data import/export functionality
- Internal data editor
- Deployment scripts for Shiny Server

---

## Release Types

- **Major (X.0.0)**: Breaking changes, major feature additions
- **Minor (1.X.0)**: New features, refactoring, enhancements
- **Patch (1.0.X)**: Bug fixes, documentation updates

## Categories

- **Added**: New features
- **Changed**: Changes in existing functionality
- **Deprecated**: Soon-to-be removed features
- **Removed**: Removed features
- **Fixed**: Bug fixes
- **Security**: Security improvements

---

[1.0.0]: https://github.com/razinkele/EcoNeTool/releases/tag/v1.0.0
[0.9.0]: https://github.com/razinkele/EcoNeTool/releases/tag/v0.9.0
