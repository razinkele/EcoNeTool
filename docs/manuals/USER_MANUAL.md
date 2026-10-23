# EcoNeTool User Manual

## Table of Contents
1. [Introduction](#introduction)
2. [Getting Started](#getting-started)
3. [Dashboard Overview](#dashboard-overview)
4. [Data Import](#data-import)
5. [Trait Research Module](#trait-research-module)
6. [Network Comparison](#network-comparison)
7. [Spatial Analysis](#spatial-analysis)
8. [Network Analysis](#network-analysis)
9. [Rpath/Ecosim Integration](#rpathecosim-integration)
10. [Metaweb Manager](#metaweb-manager)
11. [Data Export](#data-export)
12. [FAQs](#faqs)
13. [Troubleshooting](#troubleshooting)

---

## Introduction

### What is EcoNeTool?

EcoNeTool is an interactive R Shiny dashboard for analyzing marine food webs. It integrates:
- **Network Analysis**: Trophic relationships, energy flows, network metrics
- **Trait Enrichment**: Automatic trait assignment from 12 integrated databases
- **Spatial Analysis**: Geographic distribution mapping via EMODnet habitats
- **Rpath/Ecosim**: Ecosystem modeling and simulation
- **Metawebs**: Regional food web templates (Baltic Sea, Lithuanian Coast)

### Key Features

| Feature | Capability |
|---------|-----------|
| **Data Import** | EwE (.ewemdb), CSV, Excel, RData formats |
| **Trait Lookup** | WoRMS, FishBase, OBIS, PolyTraits, 8+ other databases |
| **Network Metrics** | Connectance, generality, vulnerability, trophic levels |
| **Visualization** | Interactive network graphs, maps, comparison charts |
| **Export** | CSV, plots, network objects, full reports |

### System Requirements

- **Browser**: Modern browser (Chrome, Firefox, Safari, Edge)
- **Internet**: Required for live trait lookups (optional if offline database available)
- **Storage**: ~500 MB for cached data

---

## Getting Started

### Accessing EcoNeTool

#### Option 1: Live Demo (No Installation)
Visit **http://laguna.ku.lt:3838/EcoNeTool/** (hosted at Klaipėda University)

#### Option 2: Local Installation
`ash
# Clone the repository
git clone https://github.com/razinkele/EcoNeTool.git
cd EcoNeTool

# Run the app
Rscript run_app.R
# Opens at http://localhost:3838
`

#### Option 3: Docker
`ash
# (If Docker image is available)
docker run -p 3838:3838 razinkele/econetool
`

### First Steps

1. **Load a Dataset**
   - Go to "Data Import" tab
   - Upload an EwE model, CSV, or Excel file
   - Or use an example dataset (Simple_3Species.Rdata)

2. **Explore the Network**
   - View interactive network visualization
   - Check basic metrics (species count, links)
   - Examine node and link properties

3. **Assign Traits**
   - Go to "Trait Research" tab
   - Search for species by name
   - Assign ecological traits (body size, feeding type, mobility)

4. **Analyze Results**
   - View trophic levels, keystoneness, energy flows
   - Compare against reference networks
   - Export results

---

## Dashboard Overview

### Main Tabs

The app has 8 primary tabs:

#### 1. **Data Import**
- Upload network files (EwE, CSV, Excel, RData)
- View data quality reports
- Edit species lists and link matrices
- Set up network structure

#### 2. **Trait Research**
- Search and assign ecological traits
- Query 12 trait databases (WoRMS, FishBase, etc.)
- View trait confidence scores
- Track data provenance

#### 3. **Network Comparison**
- Compare two food webs side-by-side
- Analyze species differences (Jaccard similarity)
- Compare trophic links
- Examine network topology changes

#### 4. **Spatial Analysis**
- Map species distributions
- View habitat preferences
- Filter by geographic regions
- Integrate EMODnet EUSeaMap data

#### 5. **Network Visualization**
- Interactive network graph
- Trophic level layout
- Node coloring by traits
- Link filtering by strength

#### 6. **Analysis**
- Calculate network metrics
- Compute trophic levels
- Analyze energy flows
- Identify keystone species

#### 7. **Rpath/Ecosim** (if enabled)
- Import EwE models
- Edit balancing parameters
- Run ecosystem simulations
- View results

#### 8. **Metaweb Manager**
- Load regional food web templates
- Browse Baltic Sea, Lithuanian Coast, Gulf of Riga metawebs
- Export as network objects

---

## Data Import

### Supported File Formats

| Format | Extension | Use Case |
|--------|-----------|----------|
| Ecopath with Ecosim | .ewemdb, .eweaccdb | Complete ecosystem models |
| Comma-separated values | .csv | Simple food webs, trait data |
| Microsoft Excel | .xlsx, .xls | Formatted network data |
| R Data | .RData | R objects, network lists |

### Importing Data

#### From EwE (.ewemdb)
1. Click "Data Import" tab
2. Select "Upload Ecopath Database" section
3. Choose .ewemdb or .eweaccdb file
4. System extracts species, groups, diets, and biomass
5. Review imported data, edit if needed
6. Click "Apply to Network" to load

#### From CSV
1. Click "Data Import" tab
2. Upload CSV with columns: from, to, biomass (optional)
3. Preview shows the structure
4. Validate species names against WoRMS
5. Click "Load Network"

#### From Excel
1. Similar to CSV
2. Specify which sheet contains network data
3. Map column headers to network fields

#### From RData (R objects)
1. Upload pre-saved R list or igraph object
2. System auto-detects structure
3. Creates network visualization

### Example CSV Format

`
from,to,link_strength,biomass_from
Phytoplankton,Zooplankton,0.8,500
Zooplankton,Fish,0.6,50
Fish,Seabirds,0.4,5
`

### Data Validation

After import, check:
- ✓ All species names recognized (green) or unmatched (red)
- ✓ No self-loops or duplicate edges
- ✓ Biomass values reasonable
- ✓ Network is not disconnected (warning if true)

---

## Trait Research Module

### Overview

The Trait Research tab integrates 12 ecological trait databases. It answers questions like:
- What is the body size distribution of this species assemblage?
- Which species are migratory?
- What feeding strategies dominate?

### Trait Modalities

EcoNeTool standardizes traits into 5 modalities:

#### 1. **Body Size (MS)**
- Micro: < 0.1 mm (protists, copepod larvae)
- Small: 0.1 mm - 1 cm (copepods, small crustaceans)
- Medium: 1 - 10 cm (fish fry, small benthic animals)
- Large: 10 cm - 1 m (adult fish)
- Mega: > 1 m (large fish, marine mammals)

#### 2. **Feeding Strategy (FS)**
- Herbivore: Consumes primary producers
- Omnivore: Eats both plant and animal matter
- Carnivore: Eats animal matter
- Detritivore: Eats dead organic matter
- Filter-feeder: Passive/active filter feeding

#### 3. **Mobility (MB)**
- Sessile: Attached to substrate (corals, mussels)
- Sedentary: Very limited movement (sea urchins, most benthic)
- Mobile: Can move freely (small fish, mobile crustaceans)
- Highly mobile: Migratory, fast-swimming (pelagic fish, marine mammals)
- Obligate swimmer: Cannot stop swimming (tunas, jellyfish)

#### 4. **Epibenthic Status (EP)**
- Infaunal: Burrows in sediment
- Epibenthic: Lives on sediment surface
- Pelagic: Water column (planktonic, nektonic)

#### 5. **Reproduction (PR)**
- Broadcast: External fertilization, planktonic larvae
- Brooding: Internal brooding, few large offspring
- Asexual: Budding, fragmentation, fission
- Parthenogenesis: Clonal reproduction

### Searching for Traits

1. **By Species Name**
   - Type species name in search box
   - Results show data from all 12 databases
   - Color badges show data source (green=WoRMS, blue=FishBase, etc.)

2. **By Taxonomic Level**
   - Search by genus, family, class to get averages
   - Useful for filling knowledge gaps
   - Shows how trait varies across taxa

3. **By Trait Value**
   - Filter: Body size = "Large"
   - Returns all species with that trait
   - Use to identify functional groups

### Assigning Traits

1. Find species in the trait table
2. Click on trait cell to edit
3. Select from dropdown or enter custom value
4. Confidence slider: None → Low → Medium → High
5. Click "Save" to apply to current network
6. Trait appears in network visualizations and analyses

### Confidence Scoring

| Level | Meaning | Use |
|-------|---------|-----|
| **None** (0.0) | No data available | Placeholder |
| **Low** (0.33) | Single source, ambiguous | Use with caution |
| **Medium** (0.66) | Multiple consistent sources | Generally reliable |
| **High** (1.0) | Strong evidence, multiple sources + rules | Highly reliable |

Confidence affects:
- Statistical weight in analyses
- Color shading in visualizations (darker = higher confidence)
- Export recommendations

### Data Sources (12 Databases)

| Database | Coverage | Specialty |
|----------|----------|-----------|
| WoRMS | All marine species | Taxonomy, basic traits |
| FishBase | Fish species | Fish-specific ecology |
| OBIS | Marine occurrence | Distribution, MoF records |
| PolyTraits | Polychaetes | Annelid-specific |
| SeaLifeBase | Marine invertebrates | Non-fish specialty |
| AlgaeBase | Macroalgae | Seaweed ecology |
| Btrait | Soft-bottom fauna | Benthic specialization |
| SHARK | Swedish species | Regional focus |
| BVOL | Phytoplankton | Algae (3,846 spp) |
| SpeciesEnriched | Marine invertebrates | 915 species |
| freshwaterecology.info | Freshwater fauna | Trait modalities |
| EcoBase | Ecosystem models | Published parameter sets |

### Live vs. Offline Mode

**Live Mode** (Default):
- Real-time queries to databases
- Always current
- Slower (1-5 seconds per query)
- Requires internet
- No API key needed for basic queries

**Offline Mode**:
- Uses pre-built SQLite database
- Fast (< 100 ms per query)
- ~2,000 species pre-indexed
- No internet required
- Updated nightly by automated build

To switch modes:
1. Go to "Settings" (gear icon)
2. Toggle "Use Offline Trait Database"
3. Mode applies to current session

---

## Network Comparison

### Purpose

Compare two food webs to:
- Validate model against reference data
- Assess seasonal/geographic variation
- Detect assembly rule violations
- Benchmark against literature

### Getting Started

1. Load primary network (in Data Import)
2. Go to "Network Comparison" tab
3. Upload a second network (reference/alternative)
4. System auto-aligns species by name
5. View comparison results

### Comparison Views

#### 1. **Species Comparison**
- Lists shared species (green)
- Lists species only in Network A (blue)
- Lists species only in Network B (red)
- Calculates Jaccard similarity: shared / (A + B - shared)

`
Jaccard Index = Shared Species / (Network A + Network B - Shared)
Example: 80 shared, 100 A, 90 B
Jaccard = 80 / (100 + 90 - 80) = 80/110 = 0.73
`

#### 2. **Link Comparison**
- Shared links (both networks have edge A→B)
- Links only in Network A
- Links only in Network B
- Edge weight differences (if applicable)

#### 3. **Trophic Level Comparison**
- Per-species TL differences
- Highlights conflicts (e.g., species is herbivore in A, omnivore in B)
- Calculates mean TL difference

#### 4. **Topology Comparison**
- Network connectance (proportion of possible links)
- Average path length
- Clustering coefficient
- Link density

#### 5. **Node-Weighted Metrics** (if biomass available)
- Weighted connectance (biomass-weighted)
- Vulnerability metrics (predation pressure)
- Generality metrics (diet breadth)

### Exporting Comparison

1. Click "Download Comparison Report"
2. Format options: PDF, HTML, CSV
3. Includes: species diff, link diff, metrics, visualizations

---

## Spatial Analysis

### Features

- Interactive Leaflet map
- Habitat filtering (E U SeaMap)
- Hexagonal grid aggregation
- Regional bounding-box presets
- Species distribution overlay

### Map Workflow

1. Go to "Spatial Analysis" tab
2. Select region (Baltic Sea, North Sea, Mediterranean)
3. View EMODnet habitat types:
   - Soft bottom (mud, sand)
   - Hard bottom (rock, gravel)
   - Reef (coral, sponge)
   - Seagrass beds
   - Kelp forests

4. Filter network by habitat preference
5. View species distributions on map
6. Export as spatial object

### Habitat Filtering

Example: Filter for "rocky intertidal" specialists
1. Select habitat: Hard bottom, < 20 m depth
2. System filters network to species with "rocky" preference
3. Shows subset network
4. Analyze trophic structure in this habitat

### Hexagonal Grid Analysis

1. Set hex size (default: 50 km²)
2. System aggregates species occurrences into hexagons
3. Color-codes by species richness
4. Identify biodiversity hotspots
5. Export as spatial file (.shp, .geojson)

---

## Network Analysis

### Metrics

#### Basic Metrics
- **Species Count**: Total unique nodes
- **Link Count**: Total unique trophic links
- **Average TL**: Mean trophic level (1.0 for producers, increases for consumers)
- **Connectance**: Links / (Species × (Species - 1)) — proportion of realized vs. possible links
- **Omnivory**: Proportion of species eating at multiple TL

#### Advanced Metrics

**Generality**: How diverse is each predator's diet?
- Calculated as number of prey species
- Herbivores have low generality
- Omnivores have high generality

**Vulnerability**: How many predators does each species have?
- High for key forage fish
- Low for apex predators

**Path Lengths**: Average steps between any two species
- Short paths: Tightly coupled system
- Long paths: Compartmentalized system

**Keystoneness**: Species with disproportionate effects via MTI (Mixed Trophic Impact)
- Keystone if removing it causes large system changes
- Identified via sensitivity analysis

#### Trophic Levels

Computed via iterative method:
1. Primary producers (algae, plants) = TL 1.0
2. Herbivores = 1.0 + TL(prey)
3. Omnivores = weighted average TL of all prey + 1.0
4. Iterate until convergence

Example:
`
Phytoplankton (TL = 1.0)
  ↓
Zooplankton (TL = 2.0, eats phytoplankton)
  ↓
Fish (TL = 2.5, eats phytoplankton 50% + zooplankton 50%)
`

### Energy Flow Analysis

Using luxweb package:
- Calculates flow from each species to predators
- Accounts for assimilation efficiency (default 10%)
- Shows energy bottlenecks
- Identifies species supporting most energy transfer

### Keystoneness Analysis

Mixed Trophic Impact (MTI) quantifies indirect effects:
1. Define impacts between all species pairs
2. Positive: predator feeding, fertilization
3. Negative: competition, predation
4. Aggregate across pathways
5. Identify keystone species (high aggregate impact)

### Exporting Metrics

1. Click "Download Metrics"
2. Format: CSV (for R/Excel), JSON (for web), RData
3. Includes raw metrics + metadata

