# Example Datasets for EcoNeTool

## Summary of Changes

This document describes the example datasets that have been added to EcoNeTool to fulfill the promises made in the Data Import tab.

## What Was Created

### 1. Example Datasets

Three complete food web datasets have been created in the `examples/` directory:

#### Simple 3-Species Chain
- **File:** `examples/Simple_3Species.Rdata`
- **Description:** Basic linear food chain for testing
- **Species:** 3 (Phytoplankton → Zooplankton → Fish)
- **Links:** 2 trophic interactions
- **Purpose:** Perfect for beginners to understand the data format
- **Also Available As CSV:**
  - `Simple_3Species_network.csv` - Adjacency matrix
  - `Simple_3Species_info.csv` - Species attributes

#### Caribbean Reef Food Web
- **File:** `examples/Caribbean_Reef.Rdata`
- **Description:** Realistic tropical reef ecosystem
- **Species:** 10 species across 4 functional groups
  - Phytoplankton (2 species: Phytoplankton, Macroalgae)
  - Zooplankton (1 species)
  - Benthos (2 species: Sea Urchin, Octopus)
  - Fish (5 species: Parrotfish, Damselfish, Snapper, Grouper, Barracuda)
- **Links:** 18 trophic interactions
- **Purpose:** More complex example showing realistic food web structure
- **Also Available As CSV:**
  - `Caribbean_Reef_network.csv` - Adjacency matrix
  - `Caribbean_Reef_info.csv` - Species attributes

#### Empty Template
- **File:** `examples/Template_Empty.Rdata`
- **Description:** Blank template for users to modify
- **Species:** 3 placeholder species with proper structure
- **Purpose:** Starting point for creating custom food webs
- **Also Available As CSV:**
  - `Template_network.csv` - Empty adjacency matrix template
  - `Template_info.csv` - Species info template with all required columns

### 2. Updated Application Features

#### New Download Buttons (app.R lines 928-994)
The Data Import tab now includes **9 download buttons** organized in 3 columns:
- 3 download buttons per dataset (RData + 2 CSV files)
- Buttons are styled with Bootstrap classes (btn-primary, btn-secondary)
- Box is now expanded by default (collapsed = FALSE) so users see it immediately

#### Server-Side Download Handlers (app.R lines 1718-1787)
Added 9 `downloadHandler()` functions to enable file downloads:
- `download_simple_rdata`, `download_simple_csv_net`, `download_simple_csv_info`
- `download_reef_rdata`, `download_reef_csv_net`, `download_reef_csv_info`
- `download_template_rdata`, `download_template_csv_net`, `download_template_csv_info`

### 3. Deployment Updates

#### Updated fix_deployment.sh
Modified to include the `examples/` directory when deploying:
- Copies all files in `examples/` to the server
- Maintains directory structure
- Sets proper permissions

## File Structure

```
EcoNeTool/
├── app.R (updated with download buttons)
├── examples/
│   ├── README.md
│   ├── Simple_3Species.Rdata
│   ├── Simple_3Species_network.csv
│   ├── Simple_3Species_info.csv
│   ├── Caribbean_Reef.Rdata
│   ├── Caribbean_Reef_network.csv
│   ├── Caribbean_Reef_info.csv
│   ├── Template_Empty.Rdata
│   ├── Template_network.csv
│   └── Template_info.csv
├── create_example_datasets.R (script to regenerate examples)
└── fix_deployment.sh (updated deployment script)
```

## How Users Will Use These Files

### Workflow 1: Download and Test
1. User visits the **Data Import** tab
2. Clicks on "Example Datasets - Download" box
3. Downloads one of the RData files (e.g., "Simple_3Species.Rdata")
4. Uploads it back to the app using the file input
5. Clicks "Load Data" button
6. Explores the food web in other tabs

### Workflow 2: Learn the Format
1. Downloads both CSV files for an example (e.g., Caribbean_Reef_network.csv + Caribbean_Reef_info.csv)
2. Opens them in Excel/LibreOffice to see the structure
3. Uses as a template for their own data
4. Modifies and saves as new CSV files
5. Plans to upload once CSV import is implemented

### Workflow 3: Create Custom Data
1. Downloads the Template files
2. Modifies species names, attributes, and feeding links
3. Saves as RData using R (following template structure)
4. Uploads to EcoNeTool

## Technical Details

### Data Format
All RData files contain two objects:
- `net` - An igraph object (directed graph)
- `info` - A data.frame with required columns:
  - `species` - Species name
  - `fg` - Functional group (factor with levels: Benthos, Detritus, Fish, Phytoplankton, Zooplankton)
  - `meanB` - Mean biomass (g/km²)
  - `bodymasses` - Body mass (g)
  - `met.types` - Metabolic type ("invertebrates", "ectotherm vertebrates", "Other")
  - `efficiencies` - Assimilation efficiency (0-1)

### Adjacency Matrix Format (CSV)
- Square matrix where rows and columns represent species
- Row species eats column species
- 1 = feeding link exists
- 0 = no feeding link
- First column and first row contain species names

### Species Info Format (CSV)
- Each row represents one species
- Columns as described above
- Species names must match exactly with network matrix

## Testing

To verify the examples work correctly:

```r
# Test loading Simple 3-Species
load("examples/Simple_3Species.Rdata")
library(igraph)
plot(net)
print(info)

# Test loading Caribbean Reef
load("examples/Caribbean_Reef.Rdata")
plot(net)
print(info)
```

## Deployment Checklist

To deploy these changes to the server:

- [x] Create example datasets
- [x] Update app.R with download buttons
- [x] Add download handlers to server function
- [x] Update fix_deployment.sh to include examples/
- [ ] Run fix_deployment.sh on server
- [ ] Test downloads work on deployed app
- [ ] Verify examples load correctly when uploaded back

## Future Enhancements

1. **Excel Format Support**
   - Implement Excel file reading in app.R
   - Create Excel templates with multiple sheets

2. **CSV Format Support**
   - Implement CSV file reading in app.R (currently shows "not yet implemented")
   - Allow uploading two separate CSV files

3. **Additional Examples**
   - More diverse ecosystems (freshwater, terrestrial, pelagic)
   - Different sizes (micro: 5 species, large: 50+ species)
   - Seasonal/temporal variants

4. **Interactive Example Builder**
   - Web form to create simple food webs
   - Export as RData/CSV
   - Visual network editor

## Files Modified

1. `app.R` - Added download UI and server handlers
2. `fix_deployment.sh` - Added examples directory copying
3. `create_example_datasets.R` - NEW: Script to generate examples
4. `examples/` - NEW: Directory with all example files
5. `EXAMPLE_DATASETS_README.md` - NEW: This documentation

## Regenerating Examples

If you need to recreate the example datasets:

```bash
Rscript create_example_datasets.R
```

This will regenerate all files in the `examples/` directory.

---

**Created:** 2025-11-28
**Author:** Claude Code
**Version:** 1.0
