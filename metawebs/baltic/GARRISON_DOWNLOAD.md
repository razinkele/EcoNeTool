# Baltic Sea Food Web (Garrison et al. 2022) - Download Instructions

## Source
Garrison, L.K., et al. (2022). Temporal and spatial changes in benthic invertebrate trophic networks along a taxonomic richness gradient. Ecology and Evolution, 12(6), e8975.
DOI: https://doi.org/10.1002/ece3.8975

## Study Details
- 20 years of benthic monitoring data (1980–1989 and 2010–2019)
- Swedish coast of Baltic Sea and Skagerrak
- Benthic invertebrate trophic interactions
- Temporal and spatial comparisons

## Download Steps

1. Visit: https://onlinelibrary.wiley.com/doi/10.1002/ece3.8975

2. Click on 'Supporting Information' tab below the abstract

3. Download supplementary data files:
   - May include species lists
   - Interaction matrices
   - Temporal data

4. Files are typically in Excel (.xlsx) or CSV format

## Conversion
Once downloaded:
```r
# Example conversion
library(readxl)

# Read supplementary data
species <- read_excel('ece38975-sup-0001-tables1.xlsx', sheet = 'Species')
interactions <- read_excel('ece38975-sup-0001-tables1.xlsx', sheet = 'Interactions')

# Convert to metaweb format
source('functions.R')
metaweb <- create_metaweb(species, interactions, metadata)
saveRDS(metaweb, 'metawebs/baltic/baltic_garrison2022.rds')
```

