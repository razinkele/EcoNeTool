# North Sea Food Web (Frelat et al. 2022) - Download Instructions

## Source
Frelat, R., et al. (2022). Food-web structure and community composition: a comparison across space and time in the North Sea. Ecography, 2: e05945.
DOI: 10.1111/ecog.05945

## GitHub Repository
**Direct access**: https://github.com/rfrelat/NorthSeaFoodWeb
**Archive**: Zenodo (GNU GPL v3.0+)

## Download Options

### Option 1: Git Clone
```bash
cd metawebs/atlantic
git clone https://github.com/rfrelat/NorthSeaFoodWeb.git
```

### Option 2: Download ZIP
1. Visit: https://github.com/rfrelat/NorthSeaFoodWeb
2. Click 'Code' → 'Download ZIP'
3. Extract to `metawebs/atlantic/NorthSeaFoodWeb/`

## Repository Contents
- Food web data (species, interactions)
- Spatial and temporal network data
- R scripts for analysis
- Community composition data

## Conversion
Look for files like:
- `FW_<region>.csv` or similar
- Species lists
- Interaction matrices

Then convert:
```r
source('convert_northsea_metaweb.R')
```

