# Regional Metaweb Library

This directory contains pre-built metawebs from published literature sources, as referenced in the MARBEFES WP3.2 Guidance Document (Draft v2, 2024).

## Directory Structure

```
metawebs/
├── arctic/          # Arctic marine ecosystems
├── baltic/          # Baltic Sea
├── atlantic/        # Atlantic Ocean (North Sea, etc.)
├── mediterranean/   # Mediterranean Sea
└── README.md        # This file
```

## Available Metawebs

### Arctic Region

#### 1. Barents Sea (Planque et al. 2014)
- **File**: `arctic/barents_sea_planque2014.rds`
- **Citation**: Planque, B., Lindstrøm, U., & Subbey, S. (2014). Non-deterministic modelling of food-web dynamics. *Ecology*, 95(7), 1797.
- **DOI**: https://doi.org/10.1890/13-1062.1
- **Data source**: https://doi.org/10.1890/13-1062.1

#### 2. Kongsfjorden (Farage et al. 2021)
- **File**: `arctic/kongsfjorden_farage2021.rds`
- **Citation**: Farage, C., Edler, D., Eklöf, A., Rosvall, M., & Pilosof, S. (2021). Identifying flow modules in ecological networks using Infomap. *Methods in Ecology and Evolution*, 12(5), 778-786.
- **DOI**: https://doi.org/10.1111/2041-210X.13569
- **Data source**: https://doi.org/10.1111/2041-210X.13569

### Baltic Sea

#### 3. Baltic Sea (Nordström et al. 2015)
- **File**: `baltic/baltic_nordstrom2015.rds`
- **Citation**: Nordström, M. C., Aarnio, K., Törnroos, A., & Bonsdorff, E. (2015). Nestedness of trophic links and biological traits in a marine food web. *Ecosphere*, 6(12), art308.
- **DOI**: https://doi.org/10.1890/ES14-00515.1
- **Data source**: https://doi.org/10.1890/ES14-00515.1

#### 4. Baltic Sea (Kortsch et al. 2021)
- **File**: `baltic/baltic_kortsch2021.rds`
- **Citation**: Kortsch, S., Frelat, R., Pecuchet, L., Olivier, P., Putnis, I., Bonsdorff, E., Ojaveer, H., Jurgensone, I., Strāķe, S., Rubene, G., Krūze, Ē., & Nordström, M.C. (2021). Disentangling temporal food web dynamics facilitates understanding of ecosystem functioning. *Journal of Animal Ecology*, 90(5), 1205-1216.
- **DOI**: https://doi.org/10.1111/1365-2656.13447
- **Data source**: https://github.com/rfrelat/BalticFoodWeb
- **Notes**: This metaweb is already partially available in EcoNeTool's default dataset (BalticFW.Rdata)

#### 5. Baltic Sea (Garrison et al. 2022)
- **File**: `baltic/baltic_garrison2022.rds`
- **Citation**: Garrison, L. K., Olivier, P., Kraufvelin, P., Wikström, S. A., & Nordström, M. C. (2022). Food web dynamics across environmental gradients: A test of trophic theory in temperate rocky reefs. *Ecology and Evolution*, 12(5), e8975.
- **DOI**: https://doi.org/10.1002/ece3.8975
- **Data source**: https://doi.org/10.1002/ece3.8975

### Atlantic Ocean

#### 6. North Sea (Frelat et al. 2022)
- **File**: `atlantic/north_sea_frelat2022.rds`
- **Citation**: Frelat, R., Kortsch, S., & Nordström, M. C. (2022). North Sea Food Web. GitHub repository.
- **URL**: https://github.com/rfrelat/NorthSeaFoodWeb
- **Data source**: https://github.com/rfrelat/NorthSeaFoodWeb

### Mediterranean Sea

#### 7. Mediterranean Sea (Albuoy et al. 2014)
- **File**: `mediterranean/mediterranean_albuoy2014.rds`
- **Citation**: Albouy, C., Guilhaumon, F., Leprieur, F., Ben Rais Lasram, F., Somot, S., Aznar, R., Velez, L., Le Loc'h, F., & Mouillot, D. (2013). Projected climate change and the changing biogeography of coastal Mediterranean fishes. *Journal of Biogeography*, 40(3), 534-547.
- **DOI**: https://doi.org/10.1111/gcb.12467
- **Data source**: Supplementary materials from publication

## File Format

Metawebs are stored as R objects (class `metaweb`) in RDS format. Each metaweb contains:

1. **Species list** (`species` data frame):
   - `species_id`: Unique identifier
   - `species_name`: Scientific name
   - `functional_group`: Taxonomic/functional classification
   - Additional trait columns (optional)

2. **Interactions** (`interactions` data frame):
   - `predator_id`: ID of predator species
   - `prey_id`: ID of prey species
   - `quality_code`: Link quality (1-4 scale)
   - `source`: Citation or DOI
   - `notes`: Additional information (optional)

3. **Metadata** (`metadata` list):
   - `region`: Geographic region
   - `time_period`: Temporal coverage
   - `citation`: Full citation
   - `doi`: Digital Object Identifier
   - Additional metadata as needed

## Link Quality Codes

Following MARBEFES guidance (Section 2.3):

| Code | Description | Example |
|------|-------------|---------|
| 1 | Documented in peer-reviewed literature for these exact species | Gut content analysis published for predator-prey pair |
| 2 | Documented for similar species or different region | Same predator, similar prey species documented elsewhere |
| 3 | Inferred from traits (body size, habitat, feeding mode) | Predator body size and prey size suggest potential link |
| 4 | Expert opinion, not validated | Regional expert suggests link based on experience |

## Usage in EcoNeTool

### Load a pre-built metaweb:
```r
# Load Baltic Sea metaweb from Kortsch et al. 2021
metaweb <- load_regional_metaweb("baltic_kortsch")

# View summary
print(metaweb)

# Convert to igraph network
net <- metaweb_to_igraph(metaweb)
```

### List available metawebs:
```r
available <- list_available_metawebs()
print(available)
```

### In the Shiny app:
1. Go to "Metaweb Manager" tab
2. Select "Load Metaweb" panel
3. Choose region from dropdown
4. Click "Load Regional Metaweb"

## Creating Your Own Metaweb

If your BBT region is not covered by existing metawebs:

1. **Compile species list**: All documented species in your region
2. **Document trophic links**: From literature, gut content data, expert knowledge
3. **Assign quality codes**: Rate each link 1-4 based on evidence
4. **Create CSV files**: Use template format (see `metaweb_template_*.csv`)
5. **Import**: Use `import_metaweb_csv()` function

### Template files:

**species_template.csv**:
```csv
species_id,species_name,functional_group
SP001,Gadus morhua,Fish
SP002,Clupea harengus,Fish
SP003,Calanus finmarchicus,Zooplankton
```

**interactions_template.csv**:
```csv
predator_id,prey_id,quality_code,source
SP001,SP002,1,"Gut content: Smith et al. 2020 doi:10.xxxx"
SP001,SP003,2,"Similar species: Jones et al. 2019"
```

## Data Sources and Download Instructions

Most metawebs are available as supplementary materials from the published papers. To populate this library:

1. **Download from Dryad/Zenodo**: Many papers deposit data in public repositories
2. **Extract from GitHub**: Some are available in GitHub repositories (e.g., North Sea)
3. **Contact authors**: Request data directly if not publicly available
4. **Extract from papers**: Reconstruct from tables/appendices in publications

### Automated download script (future work):
```r
# Download all available metawebs
source("metawebs/download_metawebs.R")
download_all_metawebs()
```

## Contributing New Metawebs

To add a new metaweb to the library:

1. Create metaweb object using `create_metaweb()`
2. Validate with `validate_metaweb()`
3. Save as RDS: `export_metaweb_rds(metaweb, "metawebs/region/name.rds")`
4. Document in this README
5. Update `load_regional_metaweb()` function in `functions.R`

## MARBEFES BBT Integration

For MARBEFES partners:

1. **Phase 1 (M1-M6)**: Load appropriate regional metaweb for your BBT
2. **Phase 2 (M6-M12)**: Customize with BBT-specific species and links
3. **Phase 3 (M12-M18)**: Use for spatial analysis across hexagonal units

## References

All metawebs cite the original sources. Please cite appropriately when using:

### Arctic:
- Planque et al. (2014) *Ecology* 95:1797
- Farage et al. (2021) *Methods in Ecology and Evolution* 12:778-786

### Baltic:
- Nordström et al. (2015) *Ecosphere* 6:art308
- Kortsch et al. (2021) *Journal of Animal Ecology* 90:1205-1216
- Garrison et al. (2022) *Ecology and Evolution* 12:e8975

### Atlantic:
- Frelat et al. (2022) GitHub: rfrelat/NorthSeaFoodWeb

### Mediterranean:
- Albouy et al. (2014) *Global Change Biology* 20:1947-1958

## Support

For questions about metawebs:
- MARBEFES task lead: Marie C. Nordström (Åbo Akademi University)
- EcoNeTool: https://github.com/razinkele/EcoNeTool/issues

---

**Last updated**: 2024-12-02
**MARBEFES WP3.2 Phase 2 Implementation**
