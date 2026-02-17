# populate_metaweb_library.R
# Download and convert regional metawebs from published literature sources
# MARBEFES WP3.2 Phase 2 - Regional Metaweb Library

library(httr)

# Source functions
source("functions.R")

cat("==================================================================\n")
cat("MARBEFES Regional Metaweb Library - Population Script\n")
cat("==================================================================\n\n")

# ============================================================================
# 1. ARCTIC: BARENTS SEA (Planque et al. 2014)
# ============================================================================

cat("1. Downloading Barents Sea metaweb (Planque et al. 2014)...\n")

# Data source: ESA Ecological Archives E095-124
base_url <- "http://www.esapubs.org/archive/ecol/E095/124/"

tryCatch({
  # Download SpeciesList.txt
  species_url <- paste0(base_url, "SpeciesList.txt")
  species_response <- GET(species_url)

  if (status_code(species_response) == 200) {
    species_file <- "metawebs/arctic/barents_species_raw.txt"
    writeBin(content(species_response, "raw"), species_file)
    cat("  ✓ Downloaded species list\n")

    # Read and process
    barents_species <- read.delim(species_file, stringsAsFactors = FALSE)

    # Download PairwiseList.txt
    interactions_url <- paste0(base_url, "PairwiseList.txt")
    interactions_response <- GET(interactions_url)

    if (status_code(interactions_response) == 200) {
      interactions_file <- "metawebs/arctic/barents_interactions_raw.txt"
      writeBin(content(interactions_response, "raw"), interactions_file)
      cat("  ✓ Downloaded interactions list\n")

      # Read and process
      barents_interactions <- read.delim(interactions_file, stringsAsFactors = FALSE)

      # Convert to metaweb format
      cat("  Converting to metaweb format...\n")

      # Create species data frame
      species_data <- data.frame(
        species_id = barents_species$ABBREVIATION,
        species_name = barents_species$TROPHOSPECIES,
        functional_group = gsub("^[0-9]+_", "", barents_species$GROUPS),
        phylum = barents_species$PHYLUM_SUBPYLUM,
        class = barents_species$CLASS,
        order = barents_species$ORDER,
        family = barents_species$FAMILY,
        stringsAsFactors = FALSE
      )

      # Create interactions data frame
      interactions_data <- data.frame(
        predator_id = barents_interactions$PREDATOR,
        prey_id = barents_interactions$PREY,
        quality_code = barents_interactions$QUALITY,
        source = "Planque et al. 2014 - ESA Ecological Archives E095-124",
        stringsAsFactors = FALSE
      )

      # Create metadata
      metadata <- list(
        region = "Arctic - Barents Sea",
        time_period = "Compilation from multiple studies",
        citation = "Planque, B., et al. (2014). Who eats whom in the Barents Sea: a food web topology from plankton to whales. Ecology, 95(7), 1797.",
        doi = "https://doi.org/10.1890/13-1062.1",
        data_source = "ESA Ecological Archives E095-124",
        url = "http://www.esapubs.org/archive/ecol/E095/124/",
        notes = "Comprehensive food web with 244 taxa and 1589 trophic links"
      )

      # Create metaweb
      barents_metaweb <- create_metaweb(species_data, interactions_data, metadata)

      # Validate
      validate_metaweb(barents_metaweb)

      # Save
      saveRDS(barents_metaweb, "metawebs/arctic/barents_sea_planque2014.rds")
      export_metaweb_csv(
        barents_metaweb,
        "metawebs/arctic/barents_sea_planque2014_species.csv",
        "metawebs/arctic/barents_sea_planque2014_interactions.csv"
      )

      cat("  ✓ Barents Sea metaweb created successfully!\n")
      print(barents_metaweb)
      cat("\n")

    } else {
      cat("  ✗ Error downloading interactions:", status_code(interactions_response), "\n\n")
    }
  } else {
    cat("  ✗ Error downloading species:", status_code(species_response), "\n\n")
  }
}, error = function(e) {
  cat("  ✗ Error:", e$message, "\n\n")
})

# ============================================================================
# 2. ARCTIC: KONGSFJORDEN (Farage et al. 2021 / Jacob et al. 2011)
# ============================================================================

cat("2. Kongsfjorden metaweb (Farage et al. 2021)...\n")
cat("  Data source: GitHub repository or Jacob et al. 2011 supplementary\n")
cat("  262 species, 1544 feeding interactions\n")
cat("  ⚠ Manual download required from:\n")
cat("     - GitHub: https://github.com/Ecological-Complexity-Lab/infomap_ecology_package\n")
cat("     - Or: Jacob et al. (2011) supplementary materials\n")
cat("  See: metawebs/arctic/KONGSFJORDEN_DOWNLOAD.md for instructions\n\n")

# Create download instructions
kongsfjorden_instructions <- "# Kongsfjorden Food Web - Download Instructions

## Source
Farage et al. (2021) Methods in Ecology and Evolution 12:778-786
DOI: https://doi.org/10.1111/2041-210X.13569

## Original Data
Jacob et al. (2011) and Cirtwill & Eklöf (2018)
- 262 species
- 1,544 feeding interactions

## Download Options

### Option 1: GitHub Repository
Visit: https://github.com/Ecological-Complexity-Lab/infomap_ecology_package

Look for:
- Example data files
- Kongsfjorden food web data
- Files likely named: `kongsfjorden_nodes.csv`, `kongsfjorden_edges.csv` or similar

### Option 2: Zenodo Archive
Visit: https://zenodo.org/records/4494946
Download the R package and extract example data

### Option 3: Journal Supplementary Materials
Visit: https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13569
Download supplementary materials from the article page

## Conversion
Once downloaded, use the metaweb conversion script:
```r
source('convert_kongsfjorden_metaweb.R')
```
"

writeLines(kongsfjorden_instructions, "metawebs/arctic/KONGSFJORDEN_DOWNLOAD.md")
cat("  ✓ Download instructions created\n\n")

# ============================================================================
# 3. BALTIC: NORDSTRÖM ET AL. 2015
# ============================================================================

cat("3. Baltic Sea metaweb (Nordström et al. 2015)...\n")
cat("  Data source: Ecosphere supplementary materials\n")
cat("  ⚠ Manual download required from:\n")
cat("     - DOI: https://doi.org/10.1890/ES14-00515.1\n")
cat("     - Supplementary: http://dx.doi.org/10.1890/ES14-00515.1.sm\n")
cat("  See: metawebs/baltic/NORDSTROM_DOWNLOAD.md for instructions\n\n")

nordstrom_instructions <- "# Baltic Sea Food Web (Nordström et al. 2015) - Download Instructions

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
"

writeLines(nordstrom_instructions, "metawebs/baltic/NORDSTROM_DOWNLOAD.md")
cat("  ✓ Download instructions created\n\n")

# ============================================================================
# 4. BALTIC: KORTSCH ET AL. 2021 (Already have this!)
# ============================================================================

cat("4. Baltic Sea metaweb (Kortsch et al. 2021)...\n")
if (file.exists("metawebs/baltic/baltic_kortsch2021.rds")) {
  cat("  ✓ Already available!\n")
  kortsch_metaweb <- readRDS("metawebs/baltic/baltic_kortsch2021.rds")
  print(kortsch_metaweb)
  cat("\n")
} else {
  cat("  ⚠ Not found - run create_example_metaweb.R first\n\n")
}

# ============================================================================
# 5. BALTIC: GARRISON ET AL. 2022
# ============================================================================

cat("5. Baltic Sea metaweb (Garrison et al. 2022)...\n")
cat("  Data source: Ecology and Evolution supplementary materials\n")
cat("  ⚠ Manual download required from:\n")
cat("     - DOI: https://doi.org/10.1002/ece3.8975\n")
cat("  See: metawebs/baltic/GARRISON_DOWNLOAD.md for instructions\n\n")

garrison_instructions <- "# Baltic Sea Food Web (Garrison et al. 2022) - Download Instructions

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
"

writeLines(garrison_instructions, "metawebs/baltic/GARRISON_DOWNLOAD.md")
cat("  ✓ Download instructions created\n\n")

# ============================================================================
# 6. ATLANTIC: NORTH SEA (Frelat et al. 2022 - GitHub)
# ============================================================================

cat("6. North Sea metaweb (Frelat et al. 2022)...\n")
cat("  Data source: GitHub repository\n")
cat("  URL: https://github.com/rfrelat/NorthSeaFoodWeb\n")
cat("  ⚠ Requires git clone or manual download\n\n")

northsea_instructions <- "# North Sea Food Web (Frelat et al. 2022) - Download Instructions

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
"

writeLines(northsea_instructions, "metawebs/atlantic/NORTHSEA_DOWNLOAD.md")
cat("  ✓ Download instructions created\n\n")

# ============================================================================
# 7. MEDITERRANEAN: ALBUOY ET AL. 2014
# ============================================================================

cat("7. Mediterranean Sea metaweb (Albuoy et al. 2014)...\n")
cat("  Data source: Global Change Biology supplementary materials\n")
cat("  ⚠ Manual download required (may require institutional access)\n")
cat("     - DOI: https://doi.org/10.1111/gcb.12467\n")
cat("     - Alternative: HAL open archive\n")
cat("  See: metawebs/mediterranean/ALBUOY_DOWNLOAD.md for instructions\n\n")

albuoy_instructions <- "# Mediterranean Sea Food Web (Albuoy et al. 2014) - Download Instructions

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
  region = \"Mediterranean Sea\",
  citation = \"Albuoy et al. 2014 GCB\",
  doi = \"https://doi.org/10.1111/gcb.12467\"
)

metaweb <- create_metaweb(species, interactions, metadata)
saveRDS(metaweb, 'metawebs/mediterranean/mediterranean_albuoy2014.rds')
```

## Notes
This paper focuses on climate change impacts, so the metaweb may represent
current or projected future conditions. Check supplementary materials for
details on which food web topology is provided.
"

writeLines(albuoy_instructions, "metawebs/mediterranean/ALBUOY_DOWNLOAD.md")
cat("  ✓ Download instructions created\n\n")

# ============================================================================
# SUMMARY
# ============================================================================

cat("==================================================================\n")
cat("SUMMARY\n")
cat("==================================================================\n\n")

cat("Metaweb Library Status:\n\n")

cat("✓ AVAILABLE:\n")
cat("  1. Baltic: Kortsch et al. 2021 (34 species, 207 links)\n")
if (file.exists("metawebs/arctic/barents_sea_planque2014.rds")) {
  cat("  2. Arctic: Barents Sea Planque 2014 (244 taxa, 1589 links)\n")
}
cat("\n")

cat("⚠ REQUIRES MANUAL DOWNLOAD:\n")
if (!file.exists("metawebs/arctic/barents_sea_planque2014.rds")) {
  cat("  1. Arctic: Barents Sea (Planque 2014)\n")
}
cat("  2. Arctic: Kongsfjorden (Farage 2021) - 262 species\n")
cat("  3. Baltic: Nordström 2015\n")
cat("  4. Baltic: Garrison 2022\n")
cat("  5. Atlantic: North Sea (Frelat 2022) - GitHub\n")
cat("  6. Mediterranean: Albuoy 2014\n")
cat("\n")

cat("📁 Download instructions created in each region directory\n")
cat("📖 See metawebs/README.md for detailed information\n\n")

cat("Next steps:\n")
cat("1. Download metawebs following instructions in */DOWNLOAD.md files\n")
cat("2. Convert to standard format using template scripts\n")
cat("3. Test loading in EcoNeTool Metaweb Manager\n")
cat("4. Update metawebs/README.md with availability status\n\n")

cat("For assistance with data access, contact:\n")
cat("  - MARBEFES task lead: Marie C. Nordström (ABO/UH)\n")
cat("  - Paper authors (contact info in papers)\n")
cat("  - Institutional library for paywalled content\n\n")

cat("==================================================================\n")
cat("✓ Metaweb library population script complete!\n")
cat("==================================================================\n")
