# Database Setup Guide for Automated Trait Lookup

## Overview

This guide explains how to set up the required and optional databases for the Automated Trait Lookup system in EcoNeTool.

## Required Databases (Online, No Setup)

### 1. WoRMS (World Register of Marine Species)

**Access:** Online API via `worrms` R package

**Installation:**
```r
install.packages("worrms")
```

**No additional setup required** - queries are made directly to https://www.marinespecies.org/rest/

**Coverage:** All marine species (240,000+ accepted names)

**Data provided:**
- Taxonomic classification (phylum, class, order, family)
- Authority and publication year
- Marine/brackish/freshwater flags
- Synonyms and common names

**Usage limits:** None (free academic use)

---

### 2. FishBase

**Access:** Online API via `rfishbase` R package

**Installation:**
```r
install.packages("rfishbase")
```

**No additional setup required** - queries are made to https://fishbase.org/

**Coverage:** 35,000+ fish species

**Data provided:**
- Maximum length and weight
- Trophic level
- Feeding type/mode
- Depth range
- Habitat preferences (neritic, pelagic, etc.)
- Body shape
- Geographic distribution

**Usage limits:** None (free academic use, but be respectful with query frequency)

---

## Optional Databases (Local Files)

### 3. BIOTIC Database

**Purpose:** Marine invertebrate functional traits

**Coverage:** 3,000+ benthic species (mostly NE Atlantic)

**Setup:**

#### Step 1: Download

Visit: https://www.dassh.ac.uk/lifehistory/

Or direct download:
- MarLIN Biology and Sensitivity Key Information (full dataset)
- Export as CSV

#### Step 2: Prepare File

1. Open downloaded file
2. Ensure columns include:
   - `Species` (scientific name)
   - `Max_Length_mm` (maximum length in mm)
   - `Feeding_mode` (feeding strategy)
   - `Mobility` (mobility type)
   - `Living_habit` (habitat/behavior)
   - `Substratum` (substrate preference)
   - `Skeleton` (skeletal structure)

3. Save as CSV

#### Step 3: Install in EcoNeTool

```bash
# Create data directory if not exists
mkdir -p data/

# Move file
mv path/to/downloaded_file.csv data/biotic_traits.csv
```

**File path:** `data/biotic_traits.csv`

**Expected size:** ~2-5 MB

**Example rows:**
```csv
Species,Max_Length_mm,Feeding_mode,Mobility,Living_habit,Substratum,Skeleton
Mytilus edulis,100,Suspension feeder,Sessile,Epifaunal,Rock,Calcareous shell
Cancer pagurus,250,Scavenger/Predator,Crawling,Epibenthic,Mixed,Chitinous exoskeleton
```

---

### 4. MAREDAT (Marine Ecosystem Data)

**Purpose:** Zooplankton traits and distributions

**Coverage:** Global ocean, 200+ taxonomic groups

**Setup:**

#### Step 1: Download

Visit: https://doi.pangaea.de/10.1594/PANGAEA.777398

Or:
1. Go to PANGAEA data portal
2. Search for "MAREDAT zooplankton"
3. Download: "Global dataset of zooplankton biomass and abundance"
4. Format: CSV/Tab-separated

#### Step 2: Prepare File

Required columns:
- `Species` (scientific name or taxonomic group)
- `ESD_um` (Equivalent Spherical Diameter in micrometers)
- `Group` (taxonomic group: Copepoda, Cladocera, etc.)
- `Trophic_level` (optional, if available)

#### Step 3: Install

```bash
mv path/to/maredat_download.csv data/maredat_zooplankton.csv
```

**File path:** `data/maredat_zooplankton.csv`

**Expected size:** ~10-50 MB

**Example rows:**
```csv
Species,ESD_um,Group,Trophic_level
Calanus finmarchicus,1200,Copepoda,2.5
Acartia tonsa,800,Copepoda,2.3
Evadne nordmanni,600,Cladocera,2.0
```

---

### 5. PTDB (Phytoplankton Traits Database)

**Purpose:** Phytoplankton cell size, morphology, and physiology

**Coverage:** 200+ species, mostly marine

**Setup:**

#### Step 1: Download

Visit: https://github.com/simonabarbaglia/PTDB

```bash
# Clone repository
git clone https://github.com/simonabarbaglia/PTDB.git

# Or download ZIP from GitHub
```

#### Step 2: Extract Data

The PTDB repository contains multiple files. You need the main trait database:

```bash
# Find the main data file (usually ptdb_data.csv or similar)
cd PTDB/
ls *.csv
```

#### Step 3: Prepare File

Required columns:
- `Species` (scientific name)
- `Cell_volume_um3` (cell volume in cubic micrometers)
- `Growth_form` (morphology: solitary, colonial, chain-forming)
- `Class` (taxonomic class: Bacillariophyceae, Dinophyceae, etc.)

#### Step 4: Install

```bash
cp PTDB/ptdb_data.csv /path/to/EcoNeTool/data/ptdb_phytoplankton.csv
```

**File path:** `data/ptdb_phytoplankton.csv`

**Expected size:** ~1-5 MB

**Example rows:**
```csv
Species,Cell_volume_um3,Growth_form,Class
Skeletonema costatum,250,Chain-forming,Bacillariophyceae
Thalassiosira nordenskioeldii,1500,Solitary,Bacillariophyceae
Emiliania huxleyi,100,Solitary,Prymnesiophyceae
```

---

## Directory Structure

After setup, your EcoNeTool directory should look like:

```
EcoNeTool/
├── data/
│   ├── biotic_traits.csv              [OPTIONAL]
│   ├── maredat_zooplankton.csv        [OPTIONAL]
│   └── ptdb_phytoplankton.csv         [OPTIONAL]
├── cache/
│   └── taxonomy/                       [AUTO-CREATED]
│       ├── Gadus_morhua.rds
│       ├── Clupea_harengus.rds
│       └── ...
└── ...
```

---

## Verification

### Check Database Availability

Run in R console:

```r
# Check WoRMS
library(worrms)
test_worms <- wm_records_name("Gadus morhua")
print(test_worms[[1]]$scientificname)  # Should print "Gadus morhua"

# Check FishBase
library(rfishbase)
test_fb <- species("Gadus morhua")
print(test_fb$Species)  # Should print "Gadus morhua"

# Check BIOTIC
biotic_file <- "data/biotic_traits.csv"
if (file.exists(biotic_file)) {
  biotic <- read.csv(biotic_file)
  cat("BIOTIC: OK,", nrow(biotic), "species\n")
} else {
  cat("BIOTIC: NOT FOUND (optional)\n")
}

# Check MAREDAT
maredat_file <- "data/maredat_zooplankton.csv"
if (file.exists(maredat_file)) {
  maredat <- read.csv(maredat_file)
  cat("MAREDAT: OK,", nrow(maredat), "records\n")
} else {
  cat("MAREDAT: NOT FOUND (optional)\n")
}

# Check PTDB
ptdb_file <- "data/ptdb_phytoplankton.csv"
if (file.exists(ptdb_file)) {
  ptdb <- read.csv(ptdb_file)
  cat("PTDB: OK,", nrow(ptdb), "species\n")
} else {
  cat("PTDB: NOT FOUND (optional)\n")
}
```

Expected output:
```
[1] "Gadus morhua"
[1] "Gadus morhua"
BIOTIC: OK, 3457 species
MAREDAT: OK, 8932 records
PTDB: OK, 234 species
```

Or if optional databases not installed:
```
[1] "Gadus morhua"
[1] "Gadus morhua"
BIOTIC: NOT FOUND (optional)
MAREDAT: NOT FOUND (optional)
PTDB: NOT FOUND (optional)
```

---

## Database Coverage by Organism Type

| Organism Type | Best Databases | Coverage |
|--------------|----------------|----------|
| **Fish** | WoRMS + FishBase | Excellent |
| **Benthic invertebrates** | WoRMS + BIOTIC | Good |
| **Zooplankton** | WoRMS + MAREDAT | Good |
| **Phytoplankton** | WoRMS + PTDB | Moderate |
| **Marine mammals** | WoRMS + FishBase* | Fair |
| **Seabirds** | WoRMS | Fair (limited traits) |
| **Cephalopods** | WoRMS + FishBase* | Fair |
| **Crustaceans** | WoRMS + BIOTIC | Good |
| **Molluscs** | WoRMS + BIOTIC | Good |
| **Polychaetes** | WoRMS + BIOTIC | Good |

*FishBase includes some non-fish marine species

---

## Troubleshooting

### Cannot install R packages

**Problem:** `install.packages("worrms")` fails

**Solutions:**

1. Check R version: `R.version.string` (need R ≥ 3.5)
2. Update R: https://cran.r-project.org/
3. Install dependencies:
   ```r
   install.packages("httr")
   install.packages("jsonlite")
   install.packages("curl")
   ```
4. Retry: `install.packages("worrms")`

### Downloaded file has wrong columns

**Problem:** CSV file doesn't have expected column names

**Solutions:**

1. Check if different version/format
2. Rename columns manually:
   ```r
   # Read file
   data <- read.csv("downloaded_file.csv")

   # View column names
   colnames(data)

   # Rename as needed
   colnames(data)[1] <- "Species"
   colnames(data)[5] <- "Max_Length_mm"

   # Save corrected version
   write.csv(data, "data/biotic_traits.csv", row.names = FALSE)
   ```

### File path issues on Windows

**Problem:** `data/biotic_traits.csv` not found on Windows

**Solutions:**

1. Use full path:
   ```r
   biotic_file <- "C:/Users/YourName/Documents/EcoNeTool/data/biotic_traits.csv"
   ```

2. Or check working directory:
   ```r
   getwd()  # Shows current directory
   setwd("C:/path/to/EcoNeTool")
   ```

3. Use file.path():
   ```r
   biotic_file <- file.path(getwd(), "data", "biotic_traits.csv")
   ```

### Database files too large

**Problem:** MAREDAT file is very large (>100 MB)

**Solutions:**

1. Filter to your region:
   ```r
   full_data <- read.csv("maredat_full.csv")

   # Filter to North Atlantic (example)
   filtered <- subset(full_data,
                     Latitude > 40 & Latitude < 70 &
                     Longitude > -50 & Longitude < 30)

   write.csv(filtered, "data/maredat_zooplankton.csv", row.names = FALSE)
   ```

2. Or sample species:
   ```r
   # Get unique species and their mean traits
   species_summary <- aggregate(. ~ Species, data = full_data, FUN = mean)
   write.csv(species_summary, "data/maredat_zooplankton.csv", row.names = FALSE)
   ```

---

## Alternative Data Sources

If primary databases are unavailable, try:

### For invertebrates:
- **CATAMI:** http://catami.org/ (Australian)
- **TRAITS:** https://www.univie.ac.at/traits/ (freshwater, some marine)
- **WORMS attributes:** Some species have trait data in WoRMS

### For plankton:
- **COPEPEDIA:** http://copepedia.org/ (copepods)
- **ITIS:** https://www.itis.gov/ (North American)

### For fish:
- **SeaLifeBase:** https://www.sealifebase.org/ (non-fish marine life, by FishBase team)
- **AquaMaps:** https://www.aquamaps.org/ (distribution data)

---

## Updating Databases

### WoRMS and FishBase

These are always up-to-date (queried live from online APIs)

### Local databases

Update periodically (recommended: annually):

```bash
# Re-download latest versions
cd data/

# Backup old versions
mv biotic_traits.csv biotic_traits_old.csv
mv maredat_zooplankton.csv maredat_zooplankton_old.csv

# Download new versions
# (follow setup instructions above)

# Clear cache to force re-lookup
rm -rf ../cache/taxonomy/*.rds
```

---

## Data Quality

### Expected data quality by source:

| Database | Accuracy | Completeness | Update Frequency |
|----------|----------|--------------|------------------|
| WoRMS | Very High | High | Monthly |
| FishBase | High | High | Quarterly |
| BIOTIC | High | Medium | Annually |
| MAREDAT | Medium | Medium | Static (2013) |
| PTDB | Medium | Low | Occasional |

### Quality checks:

1. **Taxonomic verification:** Always use WoRMS as taxonomic backbone
2. **Cross-validation:** Compare FishBase vs BIOTIC for species in both
3. **Literature check:** Spot-check automated assignments against primary literature
4. **Expert review:** Have domain experts review trait assignments for key species

---

## License and Citations

### WoRMS
- **License:** CC-BY
- **Citation:** WoRMS Editorial Board (2023). World Register of Marine Species. http://www.marinespecies.org

### FishBase
- **License:** CC-BY-NC
- **Citation:** Froese, R. and D. Pauly. Editors. (2023). FishBase. www.fishbase.org

### BIOTIC
- **License:** Open Government Licence
- **Citation:** Available from DASSH website

### MAREDAT
- **License:** CC-BY 3.0
- **Citation:** Moriarty, R. and M. O'Brien (2013). Distribution of mesozooplankton biomass in the global ocean. PANGAEA.

### PTDB
- **License:** Check repository
- **Citation:** Cite the GitHub repository or associated publications

**Always cite databases used in your publications!**

---

## Support

For database-specific issues:
- **WoRMS:** https://www.marinespecies.org/contact.php
- **FishBase:** https://www.fishbase.org/ContactUs.php
- **BIOTIC:** https://www.dassh.ac.uk/contact/
- **MAREDAT:** https://www.pangaea.de/contact/
- **PTDB:** Open issue on GitHub

For EcoNeTool trait lookup issues:
- Check this guide
- Review automated trait lookup guide
- Open issue on EcoNeTool GitHub

---

## Summary Checklist

- [ ] Install R packages: `worrms`, `rfishbase`
- [ ] Test WoRMS connection
- [ ] Test FishBase connection
- [ ] (Optional) Download BIOTIC database
- [ ] (Optional) Download MAREDAT database
- [ ] (Optional) Download PTDB database
- [ ] Place CSV files in `data/` folder
- [ ] Run verification script
- [ ] Test automated lookup with example species
- [ ] Clear cache if needed after database updates

---

**You're ready to use automated trait lookup!**

Proceed to the Automated Trait Lookup Guide for usage instructions.
