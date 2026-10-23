# EcoNeTool API Manual

## Table of Contents
1. [Overview](#overview)
2. [Architecture](#architecture)
3. [Core Modules](#core-modules)
4. [Trait Lookup Functions](#trait-lookup-functions)
5. [Network Analysis Functions](#network-analysis-functions)
6. [Data Import/Export](#data-importexport)
7. [Shiny Module System](#shiny-module-system)
8. [Data Structures](#data-structures)
9. [Error Handling](#error-handling)
10. [Configuration](#configuration)

---

## Overview

### What is the EcoNeTool API?

The API is a collection of R functions organized into modules:
- **Business logic** functions for data processing, trait lookup, network analysis
- **Shiny modules** (UI + server) for interactive dashboard components
- **Configuration** system for database connections and harmonization rules

### Accessing the API

#### From Within a Shiny Session
```r
# Functions loaded automatically via app.R sourcing
result <- lookup_worms("Gadus morhua")
```

#### From R Console/Script (External)
```r
# Load function
source("R/functions/trait_lookup/orchestrator.R")
result <- lookup_offline_traits("Gadus morhua")
```

#### From External Application
```r
# Via Rserve, plumber, or direct R script execution
# (EcoNeTool does not currently expose REST API; R is native interface only)
```

---

## Architecture

### Directory Structure

```
R/
├── ui/                          # Shiny UI modules
│   ├── data_import_ui.R
│   ├── trait_research_ui.R
│   ├── network_comparison_ui.R
│   ├── spatial_analysis_ui.R
│   └── ... (18 modules total)
│
├── modules/                     # Shiny server modules
│   ├── data_import_server.R
│   ├── trait_research_server.R
│   └── ... (18 modules total)
│
├── functions/
│   ├── core/                    # Network manipulation
│   │   ├── network_import.R     # EwE parsing, CSV loading
│   │   ├── network_metrics.R    # Connectance, TL, generality
│   │   └── ...
│   │
│   ├── trait_lookup/            # Trait database integration
│   │   ├── orchestrator.R       # Dispatcher (live vs offline)
│   │   ├── lookup_worms.R
│   │   ├── lookup_fishbase.R
│   │   ├── lookup_obis.R
│   │   ├── lookup_polytraits.R
│   │   ├── harmonization.R      # Trait standardization
│   │   ├── offline_db.R
│   │   └── ...
│   │
│   ├── analysis/                # Advanced analyses
│   │   ├── fluxweb_analysis.R
│   │   ├── keystoneness.R
│   │   ├── mti.R               # Mixed Trophic Impact
│   │   └── ...
│   │
│   ├── spatial/
│   │   ├── map_integration.R    # EMODnet mapping
│   │   └── hexbin_analysis.R
│   │
│   ├── validation_utils.R       # Utility functions
│   └── ...
│
└── config/
    ├── harmonization_config.R   # Trait modality definitions
    ├── database_config.R        # API credentials, endpoints
    └── app_config.R
```

### Calling Convention

All functions follow R naming conventions:
- **snake_case** for function names: `lookup_worms()`, `calculate_connectance()`
- **Parameters** documented via roxygen2 comments
- **Return values** consistent types (list, data.frame, igraph object)

---

## Core Modules

### Network Import (R/functions/core/network_import.R)

#### `read_ewe_database(filepath)`
Reads Ecopath with Ecosim database (.ewemdb or .eweacccc).

**Parameters:**
- `filepath` (char): Path to .ewemdb file

**Returns:**
```r
list(
  species = data.frame(
    group_name = "Cod", 
    group_code = "COD",
    biomass = 5.2,
    pb_ratio = 0.35,  # Production/Biomass
    dc_ratio = 0.98   # Consumption/Biomass
  ),
  diet = data.frame(
    predator = "Seabird",
    prey = "Cod",
    fraction = 0.3
  ),
  meta = list(model_name, description, version)
)
```

**Example:**
```r
ewe_data <- read_ewe_database("models/Baltic_v2.ewemdb")
species <- ewe_data$species
diets <- ewe_data$diet
```

**Errors:**
- File not found: error
- Invalid database format: error with diagnostic message

---

#### `read_csv_network(filepath, species_col, prey_col, predator_col, ...)`
Reads food web from CSV.

**Parameters:**
- `filepath` (char): Path to CSV
- `species_col` (char): Column name with species list (optional)
- `prey_col` (char): Column name for prey
- `predator_col` (char): Column name for predator
- `weight_col` (char, optional): Link strength column

**Returns:**
```r
list(
  nodes = data.frame(name = c("A", "B", "C"), ...),
  edges = data.frame(from = "A", to = "B", weight = 0.8)
)
```

**Example:**
```r
net <- read_csv_network("data/food_web.csv",
                        predator_col = "predator",
                        prey_col = "prey")
```

---

#### `create_igraph(nodes, edges)`
Converts nodes/edges data frames to igraph object (for analysis).

**Parameters:**
- `nodes` (data.frame): Node list with columns: name, [additional attributes]
- `edges` (data.frame): Edge list with columns: from, to, [weight, ...]

**Returns:**
- igraph object with directed=TRUE

**Example:**
```r
g <- create_igraph(nodes, edges)
plot(g)  # Visualize
```

---

### Network Metrics (R/functions/core/network_metrics.R)

#### `calculate_connectance(igraph_object, weighted=FALSE)`
Computes network connectance (realized links / possible links).

**Parameters:**
- `igraph_object` (igraph): Network
- `weighted` (logical): If TRUE, uses edge weights; if FALSE, binary links

**Returns:**
- numeric: Connectance value [0, 1]

**Formula:**
```
Connectance = Links / (Species × (Species - 1))
```

**Example:**
```r
conn <- calculate_connectance(my_network)
# Returns 0.15 if 30 percent of possible links are realized
```

---

#### `calculate_trophic_levels(igraph_object, method="iterative")`
Computes trophic level for each species.

**Parameters:**
- `igraph_object` (igraph): Network
- `method` (char): "iterative" (default) or "linear_diet"

**Returns:**
```r
data.frame(
  species = c("A", "B", "C"),
  trophic_level = c(1.0, 2.0, 2.5),
  fractional_part = c(0, 0, 0.5)  # Omnivory indicator
)
```

**Algorithm:**
1. Primary producers (no incoming edges, or specified) = TL 1.0
2. For each other species: TL = 1.0 + weighted_mean(TL of prey)
3. Iterate until convergence (typically 10-20 iterations)

**Example:**
```r
tl <- calculate_trophic_levels(my_network)
apex_species <- tl[tl$trophic_level > 3.0, ]
```

---

#### `calculate_generality(igraph_object)`
Computes diet breadth for each species (number of prey types).

**Parameters:**
- `igraph_object` (igraph): Network

**Returns:**
```r
data.frame(
  species = c("A", "B", "C"),
  generality = c(1, 3, 5),  # Number of prey species
  generality_norm = c(0.2, 0.6, 1.0)  # Normalized [0, 1]
)
```

**Example:**
```r
gen <- calculate_generality(my_network)
specialists <- gen[gen$generality == 1, ]
generalists <- gen[gen$generality > 3, ]
```

---

#### `calculate_vulnerability(igraph_object)`
Computes number of predators for each species.

**Parameters:**
- `igraph_object` (igraph): Network

**Returns:**
```r
data.frame(
  species = c("A", "B", "C"),
  vulnerability = c(2, 5, 1),  # Number of predators
  vulnerability_norm = c(0.4, 1.0, 0.2)  # Normalized [0, 1]
)
```

---

## Trait Lookup Functions

### Orchestrator (R/functions/trait_lookup/orchestrator.R)

#### `lookup_species_traits(species_name, modality=NULL, use_offline=NULL)`
Main entry point for trait lookup. Automatically routes to live or offline based on settings.

**Parameters:**
- `species_name` (char): Scientific name (e.g., "Gadus morhua")
- `modality` (char, optional): Filter to specific trait ("body_size", "feeding", "mobility", "epibenthic", "reproduction")
- `use_offline` (logical, optional): Override default mode. If NULL, uses user setting or environment variable

**Returns:**
```r
list(
  success = TRUE,
  species = "Gadus morhua",
  results = data.frame(
    database = c("WoRMS", "FishBase", "OBIS"),
    modality = c("body_size", "body_size", "feeding"),
    value = c("Large", "Large", "Carnivore"),
    confidence = c(1.0, 0.9, 0.66)
  ),
  metadata = list(
    timestamp = "2026-10-23 14:15:00",
    sources_queried = 12,
    consensus = "Large, Carnivore"
  )
)
```

**Example:**
```r
# Live lookup
result <- lookup_species_traits("Gadus morhua")

# Offline lookup (faster, no internet needed)
result <- lookup_species_traits("Gadus morhua", use_offline=TRUE)

# Specific modality
size_only <- lookup_species_traits("Gadus morhua", modality="body_size")
```

**Error Handling:**
```r
result <- lookup_species_traits("InvalidSpecies123")
if (!result$success) {
  warning(sprintf("Lookup failed: %s", result$error))
  # Gracefully handle; returns empty results, not NA
}
```

---

### Live Lookups

#### `lookup_worms(species_name)`
Query World Register of Marine Species.

**Parameters:**
- `species_name` (char): Species name or LSID

**Returns:**
```r
list(
  success = TRUE,
  aphia_id = 126436,
  scientific_name = "Gadus morhua",
  authority = "Linnaeus, 1758",
  rank = "Species",
  status = "accepted",
  traits = list(...)
)
```

**API Endpoint:** https://www.marinespecies.org/rest/

**Rate Limit:** 100 requests/minute

---

#### `lookup_fishbase(species_name)`
Query FishBase.

**Parameters:**
- `species_name` (char): Species name

**Returns:**
```r
list(
  success = TRUE,
  species_id = 1234,
  common_names = c("Atlantic cod", "Dorsch"),
  max_length_cm = 200,
  max_weight_kg = 96,
  ecology = list(
    habitat = "Demersal",
    depth_range_m = c(0, 500),
    feeding_type = "Carnivore"
  ),
  traits = list(...)
)
```

**API Endpoint:** http://www.fishbase.org/

**Note:** Requires internet; fallback to offline DB if unavailable.

---

#### `lookup_obis(species_name, limit=10)`
Query Ocean Biodiversity Information System (occurrence records).

**Parameters:**
- `species_name` (char): Species name
- `limit` (int): Max number of records to return

**Returns:**
```r
data.frame(
  species_name = "Gadus morhua",
  latitude = c(54.2, 54.5, 55.1),
  longitude = c(18.2, 18.5, 19.0),
  depth = c(45, 60, 32),
  year = c(2020, 2021, 2022),
  dataset = "SHARK database"
)
```

**API Endpoint:** https://api.obis.org/v3/

---

#### `lookup_polytraits(species_name)`
Query PolyTraits (polychaete annelids).

**Parameters:**
- `species_name` (char): Species name or genus

**Returns:**
```r
list(
  success = TRUE,
  species = "Nereis virens",
  traits = data.frame(
    trait_category = c("Mobility", "Feeding", "Body size"),
    value = c("Mobile", "Carnivore/Detritivore", "Medium"),
    source = c("PolyTraits", "PolyTraits", "PolyTraits")
  )
)
```

**API Endpoint:** https://polytraits.lifewatchgreece.eu/

**Health Check:** Service monitored in CI workflow; tests skip if unavailable.

---

### Offline Database (R/functions/trait_lookup/offline_db.R)

#### `lookup_offline_traits(species_name, modality=NULL)`
Query pre-built SQLite database (~2,000 species).

**Parameters:**
- `species_name` (char): Species name
- `modality` (char, optional): Filter to specific trait

**Returns:**
Same format as `lookup_species_traits()`.

**Database Location:** `data/offline_trait_db.sqlite`

**Performance:** less than 100 ms per query

**Example:**
```r
# Fast lookup
result <- lookup_offline_traits("Gadus morhua")

# Filter to body size only
size <- lookup_offline_traits("Gadus morhua", modality="body_size")
```

**Database Maintenance:**
- Built nightly via `scripts/initialization/build_offline_trait_db.R`
- Refreshes from all 12 online sources
- Runs via GitHub Actions (`nightly-live-tests.yml`)

---

### Harmonization (R/functions/trait_lookup/harmonization.R)

#### `get_harm_config()`
Get harmonization configuration (with per-session overrides).

**Returns:**
```r
list(
  modality_map = list(
    body_size = c("Micro", "Small", "Medium", "Large", "Mega"),
    feeding = c("Herbivore", "Omnivore", "Carnivore", "Detritivore", "Filter-feeder"),
    ...
  ),
  confidence_scale = list(
    none = 0.0,
    low = 0.33,
    medium = 0.66,
    high = 1.0
  ),
  database_weights = list(  # Relative importance for consensus
    worms = 1.0,
    fishbase = 0.9,
    obis = 0.7,
    ...
  )
)
```

**Usage:**
```r
cfg <- get_harm_config()
if (is.null(cfg)) {
  cfg <- HARMONIZATION_CONFIG  # Fallback to global
}
body_sizes <- cfg$modality_map$body_size
```

**Note:** Inside Shiny sessions, returns user's harmonization_settings slider values (per-session override). Outside Shiny, returns global config.

---

#### `confidence_to_num(label)`
Convert confidence label to numeric [0, 1].

**Parameters:**
- `label` (char): "none", "low", "medium", or "high"

**Returns:**
- numeric: 0.0, 0.33, 0.66, or 1.0

**Example:**
```r
conf_num <- confidence_to_num("high")  # Returns 1.0
```

---

#### `confidence_to_label(numeric_value)`
Convert numeric confidence [0, 1] to label.

**Parameters:**
- `numeric_value` (numeric): Value in [0, 1]

**Returns:**
- char: "none", "low", "medium", or "high" (closest match)

**Example:**
```r
label <- confidence_to_label(0.7)  # Returns "high"
```

---

## Network Analysis Functions

### Energy Flow (R/functions/analysis/fluxweb_analysis.R)

#### `calculate_energy_flows(igraph_object, biomass, efficiency=0.1)`
Quantify energy transfer between trophic levels.

**Parameters:**
- `igraph_object` (igraph): Network
- `biomass` (data.frame): Columns species, biomass_kg
- `efficiency` (numeric): Assimilation efficiency (default 0.1 = 10 percent)

**Returns:**
```r
data.frame(
  from = c("Phytoplankton", "Zooplankton", "Fish"),
  to = c("Zooplankton", "Fish", "Seabird"),
  energy_flow_kcal = c(10000, 1000, 100),
  efficiency_actual = c(0.10, 0.10, 0.10)
)
```

**Example:**
```r
flows <- calculate_energy_flows(my_network, my_biomass)
# Identify bottlenecks: links with lowest flow
bottlenecks <- flows[order(flows$energy_flow_kcal), ]
```

---

### Keystoneness (R/functions/analysis/keystoneness.R)

#### `calculate_keystoneness(igraph_object, method="mti")`
Identify keystone species via Mixed Trophic Impact.

**Parameters:**
- `igraph_object` (igraph): Network
- `method` (char): "mti" (default), "betweenness", or "closeness"

**Returns:**
```r
data.frame(
  species = c("A", "B", "C"),
  keystoneness_score = c(0.8, 0.3, 0.9),
  keystoneness_rank = c(2, 3, 1),  # 1 = most keystone
  type = c("keystone", "background", "keystone")
)
```

**Algorithm (MTI):**
1. Compute direct predator-prey impacts
2. Aggregate across all indirect pathways
3. Rank by total impact magnitude
4. Species with disproportionate impact = keystone

**Example:**
```r
keystones <- calculate_keystoneness(my_network)
top_3 <- keystones[keystones$keystoneness_rank <= 3, ]
```

---

### Comparison (R/functions/network_comparison.R)

#### `compare_networks(network_a, network_b)`
Compare two food webs.

**Parameters:**
- `network_a` (igraph): First network
- `network_b` (igraph): Second network

**Returns:**
```r
list(
  species_shared = c("A", "B", "C"),  # In both networks
  species_only_a = c("D", "E"),        # Only in A
  species_only_b = c("F"),              # Only in B
  jaccard_similarity = 0.67,            # 0-1, higher = more similar
  
  links_shared = data.frame(from=..., to=...),    # Common links
  links_only_a = data.frame(...),
  links_only_b = data.frame(...),
  
  metrics = list(
    connectance_a = 0.15,
    connectance_b = 0.12,
    connectance_diff = 0.03,
    tl_diff = data.frame(species, tl_a, tl_b, diff)
  )
)
```

**Example:**
```r
comparison <- compare_networks(model, reference)
print(paste("Similarity:", comparison$jaccard_similarity))
print(comparison$links_only_a)  # Links in model but not reference
```

---

## Data Import/Export

### Import

#### `import_network_file(filepath)`
Universal import function (auto-detects format).

**Parameters:**
- `filepath` (char): Path to file

**Returns:**
```r
list(
  success = TRUE,
  network = igraph object,
  metadata = list(
    source_file = ...,
    import_time = ...,
    species_count = ...,
    link_count = ...
  )
)
```

**Supported Formats:**
- .ewemdb, .eweaccdb (Ecopath)
- .csv (comma-separated)
- .xlsx, .xls (Excel)
- .RData (R objects)
- .graphml (graph markup language)

---

### Export

#### `export_network_csv(igraph_object, filepath, include_weights=TRUE)`
Export network as CSV edge list.

**Parameters:**
- `igraph_object` (igraph): Network
- `filepath` (char): Output file path
- `include_weights` (logical): Add weight column if present

**Example:**
```r
export_network_csv(my_network, "output/network.csv")
# Creates CSV with columns: from, to, weight (if available)
```

---

#### `export_network_rdata(object_list, filepath)`
Export R objects for later session recovery.

**Parameters:**
- `object_list` (list): Named list of objects to save
- `filepath` (char): Output file path

**Example:**
```r
export_network_rdata(
  list(network = my_network, traits = trait_df),
  "session_backup.RData"
)
```

---

#### `export_metrics_report(metrics_list, filepath, format="pdf")`
Generate analysis report (PDF, HTML, or CSV).

**Parameters:**
- `metrics_list` (list): Results from analysis functions
- `filepath` (char): Output file path
- `format` (char): "pdf", "html", or "csv"

**Returns:**
- filepath on success

**Example:**
```r
report_path <- export_metrics_report(
  list(connectance=0.15, keystones=keystones_df),
  "analysis_report.pdf"
)
```

---

## Shiny Module System

### Module Pattern

All Shiny modules follow this structure:

```r
# *_ui.R - User interface
moduleName_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Module Title"),
    mainPanel(
      plotOutput(ns("plot")),
      tableOutput(ns("table"))
    )
  )
}

# *_server.R - Server logic
moduleName_server <- function(id, parent_network) {
  moduleServer(id, function(input, output, session) {
    # Reactive expressions
    processed_data <- reactive({
      # Transform parent_network based on inputs
    })
    
    # Outputs
    output$plot <- renderPlot({
      plot(processed_data())
    })
    
    output$table <- renderTable({
      # Return data.frame
    })
    
    # Return reactive for parent to use
    list(data = processed_data)
  })
}

# Usage in main app
ui <- moduleName_ui("main_id")
server <- function(input, output, session) {
  result <- moduleName_server("main_id", reactive(network_obj))
}
```

---

### Key Modules

| Module | Purpose | Inputs | Outputs |
|--------|---------|--------|---------|
| `data_import_*` | Load networks | File upload | igraph network object |
| `trait_research_*` | Trait assignment | Species name, modality | Trait data.frame |
| `network_comparison_*` | Compare two networks | Two networks | Comparison metrics |
| `spatial_analysis_*` | Map species | Network + habitat filter | Leaflet map object |
| `analysis_*` | Calculate metrics | Network, parameters | Metrics table |

---

## Data Structures

### Network Object (igraph)

Standard igraph directed graph with attributes:

```r
# Nodes (vertices)
V(network)$name          # Species name
V(network)$type          # "primary_producer", "consumer", etc.
V(network)$biomass       # Numeric, optional
V(network)$body_size     # Trait attribute, optional

# Edges
E(network)$weight        # Link strength [0, 1], optional
E(network)$type          # "feeding", "competition", etc.

# Example inspection
igraph::V(network)        # List all nodes
igraph::E(network)        # List all edges
igraph::degree(network)   # Node degrees
```

---

### Trait Data Frame

Standard format returned by all lookup functions:

```r
data.frame(
  species = "Gadus morhua",
  modality = "body_size",
  value = "Large",
  confidence = 0.9,
  source = "FishBase",
  source_url = "http://...",
  timestamp = "2026-10-23 14:15:00"
)
```

---

## Error Handling

### Convention

All functions use consistent error handling:

```r
result <- tryCatch(
  expr = {
    # Do work
    list(success = TRUE, data = output)
  },
  error = function(e) {
    warning(sprintf("[module_name] operation failed: %s",
                    conditionMessage(e)), call. = FALSE)
    list(success = FALSE, error = conditionMessage(e))
  },
  warning = function(w) {
    # Handle warnings
    list(success = TRUE, data = output, warning = w)
  }
)

# Check result
if (!result$success) {
  # Handle failure gracefully
  return(NULL)  # or provide fallback
}
```

**Key Points:**
- Return list with `success = TRUE/FALSE` (not NA or error object)
- Call `warning()` (NOT `message()`) so production logs capture it
- Caller checks `result$success` before using `result$data`

---

## Configuration

### Main Config File (R/config/app_config.R)

```r
# API credentials (do not commit)
WORMS_API_KEY <- Sys.getenv("WORMS_API_KEY", default = "")
FISHBASE_API_KEY <- Sys.getenv("FISHBASE_API_KEY", default = "")

# Database paths
OFFLINE_TRAIT_DB_PATH <- app_path("data/offline_trait_db.sqlite")
METAWEB_DATA_PATH <- app_path("data/metawebs")

# Feature flags
ENABLE_RPATH_INTEGRATION <- TRUE
ENABLE_SPATIAL_ANALYSIS <- TRUE
OFFLINE_MODE_DEFAULT <- FALSE

# Timeouts
API_TIMEOUT_SECONDS <- 10
LOOKUP_TIMEOUT_SECONDS <- 30
```

---

### Environment Variables

Set these in `.Renviron` or via `Sys.setenv()`:

```bash
# Trait database mode
ECONETOOL_USE_OFFLINE_DB=FALSE

# PolyTraits health check (set by CI workflow)
POLYTRAITS_AVAILABLE=TRUE

# Logging
ECONETOOL_LOG_LEVEL=INFO

# API keys (if using live lookups)
WORMS_API_KEY=your_key_here
FISHBASE_API_KEY=your_key_here
```

---

## Quick Reference

### Common Workflows

#### 1. Load Network and Assign Traits
```r
# Import
net <- import_network_file("model.ewemdb")$network

# Lookup trait for one species
trait <- lookup_species_traits("Gadus morhua")

# Get consensus value
consensus_size <- trait$metadata$consensus

# Calculate metrics
conn <- calculate_connectance(net)
tl <- calculate_trophic_levels(net)
```

#### 2. Compare Two Networks
```r
net_a <- import_network_file("model_a.ewemdb")$network
net_b <- import_network_file("model_b.ewemdb")$network
comparison <- compare_networks(net_a, net_b)

# Export
export_metrics_report(comparison, "comparison.pdf")
```

#### 3. Identify Keystone Species
```r
keystones <- calculate_keystoneness(net, method="mti")
top_5 <- keystones[order(keystones$keystoneness_score, decreasing=TRUE), ][1:5, ]
print(top_5)
```

---

## Support & Examples

### Example Files

- `examples/Simple_3Species.RData` - Minimal network for testing
- `examples/Baltic_Sea_Model.ewemdb` - Complex multispecies model
- `examples/README.md` - Detailed usage examples

### Test Suite

- `tests/testthat/test-layer2c-*.R` - API and function tests
- Run: `testthat::test_dir("tests/testthat")`

### Documentation

- Full roxygen2 docs: `?lookup_species_traits`
- GitHub: https://github.com/razinkele/EcoNeTool
- Issues: https://github.com/razinkele/EcoNeTool/issues
