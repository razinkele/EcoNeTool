# Mediterranean Sea Food Web (Albuoy et al. 2014) - Download Instructions

## Source
Albouy, C., et al. (2014). From projected species distribution to food-web structure under climate change. Global Change Biology, 20(3), 730-741.
DOI: https://doi.org/10.1111/gcb.12467

## Study Details
- 256 endemic and native Mediterranean fish species
- Climate change projections to 2080-2099
- Food web structure under future climate scenarios
- Continental shelf food webs

## Download Options

### Option 1: Wiley Online Library (may require institutional access)
Visit: https://onlinelibrary.wiley.com/doi/10.1111/gcb.12467
Look for 'Supporting Information' section

### Option 2: HAL Open Archive (Open Access)
Visit: https://hal.archives-ouvertes.fr/hal-02548308
Download full text and supplementary materials

### Option 3: Contact Authors
If data not readily available, contact:
- Camille Albouy
- Or check author's ResearchGate/institutional page

## Expected Data
- Mediterranean fish species list
- Trophic interaction matrix
- Species distribution data
- Food web topology

## Conversion
Once downloaded:
```r
# Load data
species <- read.csv('mediterranean_species.csv')
interactions <- read.csv('mediterranean_interactions.csv')

# Convert to metaweb
source('functions.R')

metadata <- list(
  region = "Mediterranean Sea",
  citation = "Albuoy et al. 2014 GCB",
  doi = "https://doi.org/10.1111/gcb.12467"
)

metaweb <- create_metaweb(species, interactions, metadata)
saveRDS(metaweb, 'metawebs/mediterranean/mediterranean_albuoy2014.rds')
```

## Notes
This paper focuses on climate change impacts, so the metaweb may represent
current or projected future conditions. Check supplementary materials for
details on which food web topology is provided.

