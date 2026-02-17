# Automated Trait Lookup Guide

## Overview

The Automated Trait Lookup system retrieves species traits from multiple online and local databases, following a best-practice hierarchical workflow. This system eliminates manual trait assignment by automatically querying taxonomic, morphological, and ecological databases and harmonizing the results into the categorical trait classes used for food web construction.

## Workflow

### Database Hierarchy

The system queries databases in the following priority order:

1. **WoRMS** (World Register of Marine Species)
   - Taxonomic classification
   - Basic habitat information
   - Marine/brackish/freshwater flags
   - Always queried first for taxonomic context

2. **FishBase** (for fish species)
   - Maximum body length
   - Body weight
   - Trophic level
   - Feeding type
   - Depth range
   - Habitat preferences
   - Body shape

3. **BIOTIC** (for benthic invertebrates)
   - Maximum length
   - Feeding mode
   - Living habit
   - Mobility
   - Substratum affinity
   - Skeleton type

4. **MAREDAT** (for zooplankton)
   - Body size (ESD)
   - Taxonomic group
   - Trophic level

5. **PTDB** (Phytoplankton Traits Database)
   - Cell volume
   - Growth form
   - Taxonomic class
   - Automatically assigns primary producer traits

### Trait Harmonization

Raw trait values are automatically converted to categorical classes:

#### Size Class (MS)
- **MS1:** < 0.1 cm (< 1 mm) - microplankton, bacteria
- **MS2:** 0.1 - 1 cm - mesoplankton, small invertebrates
- **MS3:** 1 - 5 cm - small fish, large invertebrates
- **MS4:** 5 - 20 cm - medium fish, crabs
- **MS5:** 20 - 50 cm - large fish
- **MS6:** 50 - 150 cm - very large fish
- **MS7:** > 150 cm - marine mammals, large sharks

#### Foraging Strategy (FS)
Pattern matching on feeding descriptions:
- **FS0:** photosyn*/autotrop*/producer → Primary producer
- **FS1:** predat*/carnivor*/pisciv* → Predator
- **FS2:** scaveng*/detritivor* → Scavenger
- **FS4:** graz*/herbiv*/scraper → Grazer
- **FS5:** deposit*/sediment* → Deposit feeder
- **FS6:** filter*/suspension*/planktivor* → Filter feeder

Fallback: Uses trophic level if available

#### Mobility (MB)
Pattern matching and taxonomic inference:
- **MB1:** Sessile - attached organisms (mussels, barnacles, sponges)
- **MB2:** Passive floater - drifters (jellyfish, phytoplankton)
- **MB3:** Crawler-burrower - slow benthic (crabs, worms, snails)
- **MB4:** Facultative swimmer - can swim when needed
- **MB5:** Obligate swimmer - fish, cephalopods, copepods

Taxonomic rules:
- Bivalvia → MB1
- Cephalopoda → MB5
- Gastropoda → MB3
- Copepoda → MB5
- Cnidaria → MB2
- Porifera → MB1
- Fish (Actinopterygii) → MB5

#### Environmental Position (EP)
Based on habitat descriptions and depth:
- **EP1:** Infaunal - within sediment
- **EP2:** Epibenthic - on seafloor surface
- **EP3:** Benthopelagic - near-bottom water column
- **EP4:** Pelagic - open water column

Depth rules:
- Average depth < 50m → EP1 or EP2
- Average depth > 200m → EP3
- Phytoplankton → EP4 (need light)

#### Protection (PR)
Skeletal/armor inference:
- **PR0:** None - soft-bodied organisms
- **PR2:** Tube - tube-dwelling polychaetes
- **PR3:** Burrow - burrow-dwelling
- **PR5:** Soft shell - thin exoskeleton
- **PR6:** Hard shell - thick calcareous shell
- **PR7:** Few spines - spiny defense
- **PR8:** Armoured - heavy exoskeleton

Taxonomic rules:
- Bivalvia/Gastropoda → PR6 (hard shell)
- Cephalopoda → PR0 (soft-bodied)
- Malacostraca (crabs/lobsters) → PR8 (armoured)
- Small arthropods → PR5 (soft exoskeleton)
- Echinodermata → PR7 (spines)
- Cnidaria → PR0 (soft-bodied)
- Porifera → PR7 (spicules)

## Using the System

### In the EcoNeTool App

1. **Navigate to Trait Food Web tab**

2. **Select "Automated lookup" input method**

3. **Enter species names** (one per line):
   ```
   Gadus morhua
   Clupea harengus
   Mytilus edulis
   Cancer pagurus
   Homarus gammarus
   ```

4. **Select databases to query:**
   - WoRMS: Always recommended (provides taxonomic context)
   - FishBase: Required for fish species
   - BIOTIC: Optional, for invertebrates (requires local file)
   - MAREDAT: Optional, for zooplankton (requires local file)
   - PTDB: Optional, for phytoplankton (requires local file)

5. **Click "Run Automated Lookup"**

6. **Wait for results:**
   - Progress messages appear
   - Each species is queried sequentially
   - Results cached to `cache/taxonomy/`

7. **Review results in the data table:**
   - Green rows: Complete (all 5 traits found)
   - Yellow rows: Partial (some traits missing)
   - Red rows: No data found

8. **Edit missing traits manually** (if needed)

9. **Click "Construct Food Web"**

### Programmatic Usage

```r
# Load functions
source("R/functions/trait_lookup.R")

# Single species lookup
result <- lookup_species_traits("Gadus morhua")
print(result)

# Batch lookup
species_list <- c(
  "Gadus morhua",
  "Clupea harengus",
  "Mytilus edulis",
  "Cancer pagurus",
  "Homarus gammarus"
)

traits <- batch_lookup_traits(
  species_list,
  cache_dir = "cache/taxonomy"
)

print(traits)

# Use results for food web construction
source("R/functions/trait_foodweb.R")
foodweb <- construct_trait_foodweb(traits, threshold = 0.05)
```

## Database Setup

### Online Databases (No Setup Required)

- **WoRMS:** Accessed via `worrms` R package
- **FishBase:** Accessed via `rfishbase` R package

Install packages:
```r
install.packages(c("worrms", "rfishbase"))
```

### Local Databases (Optional)

#### BIOTIC Database
1. Download from: https://www.dassh.ac.uk/lifehistory/
2. Save as: `data/biotic_traits.csv`
3. Required columns: `Species`, `Max_Length_mm`, `Feeding_mode`, `Mobility`, `Living_habit`, `Substratum`, `Skeleton`

#### MAREDAT Database
1. Download from: https://doi.pangaea.de/10.1594/PANGAEA.777398
2. Save as: `data/maredat_zooplankton.csv`
3. Required columns: `Species`, `ESD_um`, `Group`, `Trophic_level`

#### PTDB Database
1. Download from: https://github.com/simonabarbaglia/PTDB
2. Save as: `data/ptdb_phytoplankton.csv`
3. Required columns: `Species`, `Cell_volume_um3`, `Growth_form`, `Class`

## Caching System

### How Caching Works

- **Location:** `cache/taxonomy/`
- **Format:** One RDS file per species (`Gadus_morhua.rds`)
- **Duration:** 30 days
- **Contents:** Harmonized traits + timestamp

### Benefits

- Faster lookups for repeated queries
- Reduces API load
- Works offline after first lookup
- Preserves manual edits

### Cache Management

```r
# Clear cache for a species
file.remove("cache/taxonomy/Gadus_morhua.rds")

# Clear all cache
unlink("cache/taxonomy/*.rds")

# Check cache age
cache_file <- "cache/taxonomy/Gadus_morhua.rds"
cache <- readRDS(cache_file)
print(cache$timestamp)
```

## Confidence Levels

The system assigns confidence levels based on trait completeness:

- **High:** All 5 traits found (MS, FS, MB, EP, PR)
- **Medium:** 3-4 traits found
- **Low:** 1-2 traits found
- **None:** No traits found

Edit low-confidence species manually before constructing the food web.

## Example Workflows

### Workflow 1: North Atlantic Fish Community

```r
# Define species list
species <- c(
  "Gadus morhua",       # Atlantic cod
  "Melanogrammus aeglefinus",  # Haddock
  "Pleuronectes platessa",     # Plaice
  "Clupea harengus",    # Herring
  "Sprattus sprattus",  # Sprat
  "Calanus finmarchicus",      # Copepod
  "Euphausiidae",       # Krill (family level)
  "Phaeocystis"         # Phytoplankton (genus level)
)

# Automated lookup (uses WoRMS + FishBase + MAREDAT + PTDB)
traits <- batch_lookup_traits(species, cache_dir = "cache/taxonomy")

# Review confidence
table(traits$confidence)

# Manually edit any low-confidence species
# (Use the app's interactive table or edit the data frame)

# Construct food web
library(igraph)
foodweb <- construct_trait_foodweb(traits, threshold = 0.05)
g <- trait_foodweb_to_igraph(traits, threshold = 0.05)

# Analyze
plot(g, vertex.size = 8, edge.arrow.size = 0.3)
```

### Workflow 2: Baltic Sea Benthic Community

```r
# Benthic invertebrates
species <- c(
  "Mytilus trossulus",
  "Macoma balthica",
  "Cerastoderma glaucum",
  "Marenzelleria viridis",
  "Hediste diversicolor",
  "Gammarus oceanicus",
  "Monoporeia affinis",
  "Saduria entomon"
)

# Use BIOTIC database for better benthic coverage
traits <- batch_lookup_traits(
  species,
  biotic_file = "data/biotic_traits.csv",
  cache_dir = "cache/taxonomy"
)

# Check which traits are missing
apply(traits[, c("MS", "FS", "MB", "EP", "PR")], 2, function(x) sum(is.na(x)))

# Fill missing traits manually or from literature
traits$PR[is.na(traits$PR)] <- "PR6"  # Assume shells for bivalves

# Construct network
foodweb <- construct_trait_foodweb(traits, threshold = 0.20)
```

### Workflow 3: Plankton Food Web

```r
# Mixed plankton community
species <- c(
  # Phytoplankton
  "Skeletonema costatum",
  "Thalassiosira nordenskioeldii",
  "Chaetoceros decipiens",
  "Emiliania huxleyi",

  # Zooplankton
  "Acartia tonsa",
  "Centropages hamatus",
  "Temora longicornis",
  "Pseudocalanus elongatus",
  "Evadne nordmanni",
  "Pleopsis polyphemoides"
)

# Use plankton databases
traits <- batch_lookup_traits(
  species,
  maredat_file = "data/maredat_zooplankton.csv",
  ptdb_file = "data/ptdb_phytoplankton.csv",
  cache_dir = "cache/taxonomy"
)

# Plankton should mostly be EP4 (pelagic) and small (MS1-MS2)
summary(as.factor(traits$MS))
summary(as.factor(traits$EP))

# Construct
foodweb <- construct_trait_foodweb(traits, threshold = 0.05)
```

## Troubleshooting

### No traits found for any species

**Problem:** All lookups return `confidence = "none"`

**Solutions:**
1. Check internet connection (WoRMS and FishBase require online access)
2. Verify species names are correct scientific names
3. Check spelling and taxonomy (use WoRMS.org to verify)
4. Try genus-level names if species-level fails
5. Install required packages: `install.packages(c("worrms", "rfishbase"))`

### Traits incomplete for some species

**Problem:** Some species have `confidence = "low"` or missing traits

**Solutions:**
1. Manually edit the trait table in the app
2. Consult literature and field guides
3. Use taxonomic inference (e.g., all bivalves have hard shells → PR6)
4. Download optional databases (BIOTIC, MAREDAT, PTDB) for better coverage
5. Use genus- or family-level averages from literature

### FishBase queries fail

**Problem:** Error: `Package 'rfishbase' not installed`

**Solution:**
```r
install.packages("rfishbase")
```

**Problem:** FishBase returns no data for known fish species

**Solutions:**
1. Check species name in FishBase.org
2. Try alternative names (synonyms)
3. Use genus name
4. FishBase may not have all species - use manual entry

### Local database files not found

**Problem:** `Error: BIOTIC file not found`

**Solutions:**
1. Download the database (see Database Setup section)
2. Place file in `data/` folder
3. Check file name exactly matches:
   - `data/biotic_traits.csv`
   - `data/maredat_zooplankton.csv`
   - `data/ptdb_phytoplankton.csv`
4. Or uncheck the database in the app if you don't have it

### Incorrect trait assignments

**Problem:** Automated harmonization assigns wrong trait class

**Solutions:**
1. Review the harmonization rules in `R/functions/trait_lookup.R`
2. Manually edit incorrect traits in the app's data table
3. Adjust harmonization thresholds if needed:
   ```r
   # Edit harmonize_size_class() function
   # Modify size breakpoints to match your ecosystem
   ```
4. Submit feedback/issue if systematic errors occur

### API rate limits

**Problem:** `Error: Too many requests`

**Solutions:**
1. Lookups include 0.5s delay between species (see `batch_lookup_traits()`)
2. Use cache - second lookup is instant
3. For large species lists (>50), split into batches
4. Wait a few minutes and retry

## Best Practices

1. **Always enable WoRMS:** Provides taxonomic context for all species

2. **Enable FishBase for fish:** Essential for accurate fish traits

3. **Use caching:** Speeds up repeated analyses

4. **Review results:** Check confidence levels before constructing networks

5. **Manual verification:** Spot-check automated assignments against literature

6. **Document sources:** Note which databases were used in your methods

7. **Validate results:** Compare automated vs manual traits for a subset

8. **Update cache periodically:** Re-run lookups every few months for database updates

9. **Local databases improve coverage:** Download BIOTIC/MAREDAT/PTDB if working with invertebrates/plankton

10. **Taxonomic consistency:** Use accepted names from WoRMS.org

## Advanced: Customizing Harmonization

### Modifying Size Classes

Edit `harmonize_size_class()` in `R/functions/trait_lookup.R`:

```r
harmonize_size_class <- function(size_cm) {
  # Adjust thresholds for your ecosystem
  if (size_cm < 0.05) {  # Smaller MS1 threshold
    return("MS1")
  } else if (size_cm < 0.5) {  # Adjusted MS2 threshold
    return("MS2")
  }
  # ... etc
}
```

### Adding Custom Pattern Matching

Edit `harmonize_foraging_strategy()`:

```r
# Add new pattern
if (grepl("ambush|sit.and.wait", feeding_lower)) {
  return("FS1")  # Ambush predators
}
```

### Taxonomic Overrides

Create a lookup table for species-specific overrides:

```r
# Custom overrides (before automated harmonization)
overrides <- data.frame(
  species = c("Homarus gammarus", "Cancer pagurus"),
  MS = c("MS5", "MS4"),
  PR = c("PR8", "PR8"),  # Both heavily armoured
  stringsAsFactors = FALSE
)

# Merge with automated results
traits <- merge(auto_traits, overrides, by = "species", all.x = TRUE, suffixes = c("_auto", ""))
traits$MS <- ifelse(is.na(traits$MS), traits$MS_auto, traits$MS)
```

## Scientific Basis

The automated lookup follows established best practices:

- **Olivier et al. (2019)** - Trait categorization scheme
- **Brose et al. (2006)** - Body size ratios in food webs
- **Eklöf et al. (2013)** - Trait dimensionality
- **Costello et al. (2015)** - WoRMS as taxonomic backbone
- **Froese & Pauly (2023)** - FishBase as authoritative fish database
- **Bremner et al. (2006)** - BIOTIC functional traits

## References

### Databases

- **WoRMS:** https://www.marinespecies.org/
- **FishBase:** https://www.fishbase.org/
- **BIOTIC:** https://www.dassh.ac.uk/lifehistory/
- **MAREDAT:** https://doi.pangaea.de/10.1594/PANGAEA.777398
- **PTDB:** https://github.com/simonabarbaglia/PTDB

### R Packages

- `worrms`: https://cran.r-project.org/package=worrms
- `rfishbase`: https://cran.r-project.org/package=rfishbase

### Literature

- Olivier et al. (2019) Exploring the temporal variability of a food web using long-term biomonitoring data. *Ecography* 42:2107-2121
- Brose et al. (2006) Consumer–resource body-size relationships in natural food webs. *Ecology Letters* 9:1299-1302
- Eklöf et al. (2013) The dimensionality of ecological networks. *Ecology Letters* 16:577-583

## Support

For questions or issues:
1. Check this guide
2. Review `R/functions/trait_lookup.R` source code
3. Open issue on GitHub
4. Consult database documentation

## Future Enhancements

Planned improvements:
- Integration with additional databases (SeaLifeBase, AlgaeBase)
- Machine learning trait prediction
- Uncertainty quantification
- Phylogenetic trait imputation
- Habitat-specific harmonization rules
- GUI for customizing harmonization thresholds
