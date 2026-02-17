# Baltic Sea Food Web (Nordström et al. 2015) - Download Instructions

## Source
Nordström, M.C., et al. (2015). Nestedness of trophic links and biological traits in a marine food web. Ecosphere, 6(12), art308.
DOI: https://doi.org/10.1890/ES14-00515.1

## Supplementary Data
Available at: http://dx.doi.org/10.1890/ES14-00515.1.sm

## Study Area
Northern Baltic Sea - shallow subtidal trophic network
- Benthic macroinvertebrates
- Fish
- Highly nested feeding interactions

## Download Steps

1. Visit the journal article page: https://esajournals.onlinelibrary.wiley.com/doi/10.1890/ES14-00515.1

2. Look for 'Supporting Information' or 'Supplementary Materials' section

3. Download data files (likely Excel or CSV format)

4. Files may include:
   - Species list with traits
   - Interaction matrix
   - Biological trait data

## Conversion
Once downloaded, adapt the conversion script:
```r
# Load data
species <- read.csv('nordstrom_species.csv')
interactions <- read.csv('nordstrom_interactions.csv')

# Convert to metaweb
source('functions.R')
metaweb <- create_metaweb(species, interactions, metadata)
saveRDS(metaweb, 'metawebs/baltic/baltic_nordstrom2015.rds')
```

