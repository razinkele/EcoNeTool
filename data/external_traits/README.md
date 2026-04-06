# External Trait Databases

This directory contains bundled CSV trait databases for marine species used by EcoNeTool.
Each file follows the naming convention: `<source>_traits.csv`.

## Databases

### 1. Black Sea Traits (`blacksea_traits.csv`)
- **Source:** Black Sea benthic and pelagic species trait compilation
- **Download URL:** https://www.emodnet-biology.eu/data-catalog?module=dataset&dasid=5756
- **Coverage:** Black Sea, Sea of Azov
- **Key traits:** feeding mode, mobility, body size, reproductive mode, temperature/salinity affinity

### 2. Arctic Traits (`arctic_traits.csv`)
- **Source:** Arctic Ocean Diversity (ArcOD) species traits
- **Download URL:** https://www.arcodiv.org/database/traits
- **Coverage:** Arctic Ocean, Barents Sea, Norwegian Sea
- **Key traits:** feeding mode, mobility, body size, reproductive mode, temperature preference

### 3. CEFAS NW Europe Benthic Traits (`cefas_benthic_traits.csv`)
- **Source:** CEFAS (Centre for Environment, Fisheries & Aquaculture Science) benthic trait database
- **Download URL:** https://www.cefas.co.uk/data-and-publications/dois/benthic-invertebrate-trait-database/
- **Coverage:** NW European shelf seas (North Sea, Irish Sea, English Channel)
- **Key traits:** feeding mode, mobility, body size, lifespan, larval development, living habit, bioturbation mode

### 4. Coral Trait Database (`coral_traits.csv`)
- **Source:** Coral Trait Database (Madin et al. 2016, Scientific Data)
- **Download URL:** https://coraltraits.org/
- **Coverage:** Global tropical/subtropical coral reefs
- **Key traits:** growth form, reproductive mode, thermal tolerance, depth range

### 5. Pelagic Trait Database (`pelagic_traits.csv`)
- **Source:** Pelagic Species Trait Database (MAREDAT / PANGAEA)
- **Download URL:** https://doi.pangaea.de/10.1594/PANGAEA.taxonomictraits
- **Coverage:** Global open ocean, shelf seas
- **Key traits:** habitat use, morphology, body length, nutritional quality, feeding mode

## File Naming Convention

`<source>_traits.csv` where `<source>` is a lowercase identifier:
- `blacksea` — Black Sea regional database
- `arctic` — Arctic Ocean regional database
- `cefas_benthic` — CEFAS NW Europe benthic database
- `coral` — Coral Trait Database
- `pelagic` — Pelagic Trait Database

## Usage

These files are loaded lazily and cached in memory by `R/functions/trait_lookup/csv_trait_databases.R`.
Populate each CSV by downloading from the URL listed above and reformatting columns to match the headers.
