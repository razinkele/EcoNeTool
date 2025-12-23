# 🌊 EcoNeTool - Marine Food Web Network Analysis Tool

[![License: GPL-3.0](https://img.shields.io/badge/License-GPL%203.0-blue.svg)](LICENSE)
[![GitHub](https://img.shields.io/badge/GitHub-razinkele/EcoNeTool-blue)](https://github.com/razinkele/EcoNeTool)
[![R Version](https://img.shields.io/badge/R-%E2%89%A5%204.0.0-blue.svg)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-Interactive-brightgreen.svg)](https://shiny.rstudio.com/)

**Interactive Shiny Dashboard for analyzing trophic interactions, biomass distributions, and energy fluxes in marine ecosystems**

🌐 **Live Demo**: [http://laguna.ku.lt:3838/EcoNeTool/](http://laguna.ku.lt:3838/EcoNeTool/)

---

## 📋 Table of Contents

- [Overview](#overview)
- [Key Features](#key-features)
- [Quick Start](#quick-start)
- [Installation](#installation)
- [Project Structure](#project-structure)
- [Data Format](#data-format)
- [Analysis Features](#analysis-features)
- [Deployment](#deployment)
- [Documentation](#documentation)
- [Contributing](#contributing)
- [Citation](#citation)
- [License](#license)

---

## 📋 Overview

EcoNeTool is an interactive web application built with R Shiny that provides comprehensive analysis tools for marine food web networks. The application integrates qualitative and quantitative network analysis approaches to understand food web structure and dynamics.

### Key Features

**📊 Interactive Network Visualization**: Dynamic, zoomable food web graphs with species-level details and trophic level layouts
**📁 Multiple Data Import Formats**: Support for RData, CSV, Excel, and ECOPATH exports
**📈 Topological Metrics**: Connectance, generality, vulnerability, and trophic levels
**⚖️ Biomass Analysis**: Node-weighted metrics accounting for species abundance
**⚡ Energy Flux Calculations**: Metabolic theory-based energy flow analysis using fluxweb
**🔑 Keystoneness Analysis**: Identify keystone species using Mixed Trophic Impact (MTI)
**🗺️ Spatial Analysis**: Hexagonal grid-based food web analysis with EMODnet habitat integration (MARBEFES WP3.2)
**🌊 Habitat Data Integration**: Optimized EMODnet EUSeaMap loading with regional bbox filtering
**🎨 Taxonomic Database Integration**: WoRMS, OBIS, FishBase, SeaLifeBase with geographic filtering
**🦈 Swedish Ocean Archives**: SHARK4R integration for Baltic Sea data access
**✏️ Internal Data Editor**: Edit species information and network matrices directly
**🛠️ Parameter Editors**: Edit group parameters and diet matrices before balancing
**🧮 Balancing Fixes**: Real-time validation and error diagnostics for Ecopath/Rpath balancing
**🔗 ECOPATH/ECOSIM Integration**: Import, analyze, and simulate ECOPATH models using Rpath
**📊 Export Capabilities**: Download results, plots, and data for publications
**🎯 Metaweb Export**: Export current network to RData format for reuse

### Recent Updates (v1.0.20 - 2025-12-23)

🎉 **Critical Fixes & New Features:**
- **Color Scheme Fix**: Phytoplankton now displays correctly as GREEN, Zooplankton as LIGHT BLUE
- **Default Dataset**: Changed to Lithuanian Coastal Food Web (LTCoast.Rdata) - 41 species, 244 links
- **Flux Network**: Fixed validation errors and empty edge handling
- **Network Colors**: Legend colors now match node colors precisely with visNetwork integration
- **Metaweb Export**: Export current network to RData format compatible with BalticFW structure
- **Modular Architecture**: Complete code refactoring with organized R/ directory structure
- **Performance**: Optimized spatial habitat loading with bbox filtering (10-20x faster)

See [CHANGELOG.md](CHANGELOG.md) for complete version history.

---

## 🚀 Quick Start

### 1. Running Locally

```r
# Option 1: Using helper script (recommended)
source("run_app.R")

# Option 2: Direct run
shiny::runApp()

# Option 3: With auto-reload (development)
shiny::runApp(launch.browser = TRUE)
```

### 2. Pre-flight Check

Before running, validate your installation:

```bash
cd deployment
Rscript pre-deploy-check.R
```

### 3. Online Access

Visit the deployed application at: **[http://laguna.ku.lt:3838/EcoNeTool/](http://laguna.ku.lt:3838/EcoNeTool/)**

---

## 📦 Installation

### Prerequisites

- **R**: ≥ 4.0.0
- **Operating System**: Linux, macOS, or Windows
- **Memory**: ≥ 4GB RAM recommended
- **Disk Space**: ≥ 500MB for packages and data

### Automatic Installation

```r
# Install all dependencies automatically
source("deployment/install_dependencies.R")
```

### Manual Installation

```r
# Core packages
install.packages(c(
  "shiny",          # Web framework
  "bs4Dash",        # Dashboard UI
  "igraph",         # Network analysis
  "fluxweb",        # Energy flux calculations
  "visNetwork",     # Interactive visualization
  "DT",             # Interactive tables
  "MASS",           # Matrix operations
  "leaflet",        # Spatial mapping
  "sf"              # Spatial data handling
))
```

### Verification

```bash
cd deployment
Rscript pre-deploy-check.R
```

Expected output:
```
✅ ALL CHECKS PASSED
   Application is ready for deployment!
```

---

## 📁 Project Structure

```
EcoNeTool/
├── app.R                      # Main Shiny application (UI & Server)
├── run_app.R                  # Application launcher
│
├── R/                         # Modular R code (organized by function)
│   ├── config.R              # Configuration constants (COLOR_SCHEME, etc.)
│   ├── config/               # Additional configuration
│   │   └── plugins.R         # Plugin system
│   ├── functions/            # Analysis and utility functions
│   │   ├── functional_group_utils.R
│   │   ├── validation_utils.R
│   │   ├── trophic_levels.R
│   │   ├── network_visualization.R
│   │   ├── topological_metrics.R
│   │   ├── flux_calculations.R
│   │   ├── keystoneness.R
│   │   ├── metaweb_core.R
│   │   ├── metaweb_io.R
│   │   ├── spatial_analysis.R
│   │   ├── taxonomic_api_utils.R
│   │   ├── shark_api_utils.R
│   │   ├── emodnet_habitat_utils.R
│   │   ├── ecobase_connection.R
│   │   ├── ecopath/          # ECOPATH import
│   │   └── rpath/            # Rpath integration
│   ├── ui/                   # UI components (modular)
│   │   ├── dashboard_ui.R
│   │   ├── import_ui.R
│   │   ├── network_ui.R
│   │   ├── biomass_ui.R
│   │   ├── fluxes_ui.R
│   │   ├── topological_ui.R
│   │   ├── keystoneness_ui.R
│   │   ├── metaweb_ui.R
│   │   ├── shark_ui.R
│   │   ├── ecobase_ui.R
│   │   ├── dataeditor_ui.R
│   │   └── rpath_ui.R
│   └── modules/              # Shiny modules
│
├── examples/                 # Example datasets
│   ├── LTCoast.Rdata        # Lithuanian Coastal Food Web (default)
│   ├── BalticFW.Rdata       # Gulf of Riga Food Web
│   └── ...
│
├── deployment/               # Deployment & validation scripts
│   ├── deploy.sh            # Main deployment script
│   ├── pre-deploy-check.R   # Validation script
│   ├── verify-deployment.sh # Deployment verification
│   ├── force-reload.sh      # Force app reload
│   ├── install_dependencies.R # Dependency installer
│   └── README.md            # Deployment documentation
│
├── docs/                    # Documentation
│   ├── README.md           # Documentation hub
│   ├── user-guides/        # User manuals
│   ├── development/        # Developer docs
│   ├── testing/            # Test results
│   └── deployment/         # Deployment guides
│
├── tests/                   # Test scripts and validation
├── cache/                   # Cached data (taxonomic, spatial)
├── www/                     # Web assets (img/, css/)
│
├── README.md               # This file
├── CHANGELOG.md            # Version history
└── LICENSE                 # License information
```

### Core Files

- **`app.R`**: Main application with UI definition and server logic
- **`R/config.R`**: Configuration constants (COLOR_SCHEME, DATA_FILE, etc.)
- **`R/functions/`**: Modular analysis functions organized by domain
- **`R/ui/`**: Modular UI components for each analysis tab
- **`examples/LTCoast.Rdata`**: Default dataset (Lithuanian Coastal Food Web)

---

## 📖 Data Format

### Required Data Structure

EcoNeTool requires two main components:

1. **Network Adjacency Matrix** - Who eats whom (directed graph)
2. **Species Information Table** - Attributes for each species

### Supported Formats

- **RData**: `net` (igraph object) + `info` (data.frame)
- **CSV/Excel**: Network matrix + Species info table
- **ECOPATH**: Native database (.mdb, .ewemdb, .eweaccdb) or exported CSV files

### Creating Your Dataset

```r
library(igraph)

# 1. Create network from adjacency matrix
adjacency_matrix <- matrix(...)  # Your predator-prey matrix
net <- graph_from_adjacency_matrix(adjacency_matrix, mode = 'directed')

# 2. Create species information data frame
info <- data.frame(
  species = c('Species_A', 'Species_B', 'Species_C'),
  fg = factor(c('Fish', 'Zooplankton', 'Phytoplankton')),
  meanB = c(1250.5, 850.2, 2100.0),        # Biomass (g/m²)
  bodymasses = c(50.0, 0.5, 0.001),         # Individual mass (g)
  met.types = c('ectotherm vertebrates',    # Metabolic type
                'invertebrates',
                'Other'),
  efficiencies = c(0.85, 0.75, 0.40)        # Assimilation efficiency
)

# 3. Save as RData
save(net, info, file = "MyFoodWeb.Rdata")
```

### Required Columns in `info`

| Column | Type | Description | Example |
|--------|------|-------------|---------|
| `species` | character | Species names | "Cod", "Sprat" |
| `meanB` | numeric | Mean biomass (g/m²) | 1250.5 |
| `fg` | factor | Functional group | "Fish", "Zooplankton", "Phytoplankton" |
| `bodymasses` | numeric | Individual body mass (g) | 50.0 |
| `met.types` | character | Metabolic type | "ectotherm vertebrates", "invertebrates", "Other" |
| `efficiencies` | numeric | Assimilation efficiency (0-1) | 0.85 |

### Functional Groups

Standard functional groups with color scheme:
1. **Benthos** → Light brown (burlywood)
2. **Birds** → Purple
3. **Detritus** → Brown
4. **Fish** → Blue
5. **Mammals** → Red
6. **Phytoplankton** → Green
7. **Zooplankton** → Light blue

---

## 🔬 Analysis Features

### 1. Network Visualization
- **Interactive Graphs**: Force-directed and trophic-level layouts with visNetwork
- **Color Coding**: Consistent color scheme by functional groups
- **Node Sizing**: Proportional to biomass or fixed size
- **Edge Weights**: Show interaction strength or energy flux
- **Export**: Download networks as images or RData files

### 2. Topological Metrics
- **Species Richness (S)**: Number of taxa
- **Connectance (C)**: Proportion of realized links (L / S(S-1))
- **Generality (G)**: Mean number of prey per predator
- **Vulnerability (V)**: Mean number of predators per prey
- **Trophic Levels**: Iterative calculation based on prey TL
- **Omnivory**: Standard deviation of prey trophic levels

### 3. Biomass-Weighted Analysis
- **Node-Weighted Connectance**: Accounts for species biomass
- **Node-Weighted Generality/Vulnerability**: Biomass-adjusted metrics
- **Biomass-Based Importance**: Size spectrum and distributions

### 4. Energy Flux Analysis
- **Metabolic Theory**: Based on Brown et al. (2004)
- **Allometric Scaling**: Temperature-adjusted metabolic rates
- **Flux Calculations**: Using the fluxweb package (Gauzens et al. 2019)
- **Link-Weighted Metrics**: Shannon diversity of energy flows (Bersier et al. 2002)
- **Flux Network Visualization**: Edge widths proportional to energy flow

### 5. Keystoneness Analysis
- **Mixed Trophic Impact (MTI)**: Direct and indirect species effects (ECOPATH approach)
- **Keystoneness Index**: KS = log(1 + Overall Effect) / log(1 + Relative Biomass)
- **Species Classification**: Keystone, Dominant, or Rare
- **Impact Visualization**: Heatmaps showing species interactions

### 6. Spatial Analysis
- **Hexagonal Grid**: Spatial aggregation of species occurrences
- **Habitat Integration**: EMODnet EUSeaMap habitat data
- **Regional Optimization**: Fast loading with bbox filtering
- **Interactive Maps**: Leaflet-based visualization with tooltips

### 7. Taxonomic Database Integration
- **WoRMS**: World Register of Marine Species classification
- **OBIS**: Ocean Biodiversity Information System occurrences
- **FishBase/SeaLifeBase**: Biological traits and body mass data
- **Geographic Filtering**: Bounding box filtering for multiple matches
- **Caching**: Local cache for fast repeated queries

---

## 📊 Default Dataset

The application includes the **Lithuanian Coastal Food Web** dataset:

| Property | Value |
|----------|-------|
| **Source** | LTCoastal Food Web Model |
| **Location** | Lithuanian Coast, Southeastern Baltic Sea |
| **Ecosystem** | Coastal food web |
| **Taxa** | 41 species across 6 functional groups |
| **Links** | 244 trophic interactions |
| **Functional Groups** | Phytoplankton, Zooplankton, Benthos, Fish, Birds, Detritus |

### Alternative Datasets (examples/ folder)

- **BalticFW.Rdata**: Gulf of Riga Food Web (Frelat & Kortsch, 2020) - 34 species, 207 links
- **LTgoby.eweaccdb**: ECOPATH model with Round Goby invasion scenario
- Various ECOPATH .ewemdb and .eweaccdb files for testing

---

## 🚢 Deployment

### Development Mode

```r
# Run locally with auto-reload
shiny::runApp(launch.browser = TRUE)
```

### Production Deployment to Shiny Server

```bash
cd deployment

# Step 1: Pre-deployment validation
Rscript pre-deploy-check.R

# Step 2: Deploy to Shiny Server
sudo ./deploy.sh --shiny-server

# Step 3: Verify deployment
sudo ./verify-deployment.sh

# If issues occur: Force reload
sudo ./force-reload.sh
```

### Deployment Scripts

- **`pre-deploy-check.R`**: Validates files, dependencies, syntax, and structure
- **`deploy.sh`**: Deploys to Shiny Server with cache clearing
- **`verify-deployment.sh`**: Checks what's actually deployed on server
- **`force-reload.sh`**: Nuclear option - stops server, clears all caches, redeploys

### Troubleshooting Deployment

If the server shows an old version:

1. **Clear browser cache**: Ctrl+Shift+R (or Cmd+Shift+R on Mac)
2. **Verify deployment**: `sudo ./verify-deployment.sh`
3. **Force reload**: `sudo ./force-reload.sh`
4. **Check logs**: `sudo tail -f /var/log/shiny-server.log`

See [deployment/README.md](deployment/README.md) for detailed instructions.

---

## 📚 Documentation

### Quick Start
- **[Quick Manual](docs/QUICK_MANUAL.md)** - Startup and usage guide

### User Guides
- **[Parameter Editors Guide](docs/user-guides/PARAMETER_EDITORS_GUIDE.md)** - Edit group parameters and diet matrices
- **[Balancing Guide](docs/user-guides/RPATH_BALANCING_FIX.md)** - Fix balancing issues
- **[ECOPATH/ECOSIM Integration](docs/user-guides/RPATH_INTEGRATION_GUIDE.md)** - Import and analyze ECOPATH models
- **[ECOPATH Windows Import](docs/user-guides/ECOPATH_WINDOWS_IMPORT_GUIDE.md)** - Import ECOPATH databases on Windows

### Feature Documentation
- **[Taxonomic API Integration](TAXONOMIC_API_IMPROVEMENTS.md)** - WoRMS, OBIS, FishBase integration
- **[Spatial Habitat Integration](SPATIAL_HABITAT_INTEGRATION_COMPLETE.md)** - EMODnet habitat data
- **[SHARK4R Integration](SHARK4R_INTEGRATION_COMPLETE.md)** - Swedish ocean archives

### Testing Documentation
- **[RStudio Testing Guide](docs/user-guides/RSTUDIO_TESTING_GUIDE.md)** - Test the app in RStudio
- **[Manual Testing Guide](docs/user-guides/MANUAL_TESTING_GUIDE.md)** - Manual testing procedures
- **[Test Results](docs/testing/FINAL_TEST_SUMMARY.md)** - Latest test results and validation

### Development Documentation
- **[Project Organization](docs/testing/PROJECT_ORGANIZATION_ANALYSIS.md)** - Structure and organization
- **[Modularization Guide](MODULARIZATION_GUIDE.md)** - Code organization principles
- **[Development Notes](docs/development/)** - Feature implementations and fixes

### Deployment
- **[Deployment Guide](deployment/README.md)** - Server deployment and troubleshooting
- **[Deployment Improvements](docs/deployment/DEPLOYMENT_IMPROVEMENTS.md)** - Recent enhancements

### Full Documentation Hub
- **[Documentation Hub](docs/README.md)** - Complete documentation navigation

### Scientific References

Key methodologies implemented:

- **Brown, J. H., et al. (2004).** Toward a metabolic theory of ecology. *Ecology*, 85(7), 1771-1789.
- **Bersier, L. F., et al. (2002).** Quantitative descriptors of food web matrices. *Ecology*, 83(9), 2394-2407.
- **Libralato, S., et al. (2006).** A method for identifying keystone species in food web models. *Ecological Modelling*, 195(3-4), 153-171.
- **Williams, R. J., & Martinez, N. D. (2004).** Limits to trophic levels and omnivory in complex food webs. *Proceedings of the Royal Society B*, 271(1540), 549-556.
- **Gauzens, B., et al. (2019).** fluxweb: An R package to easily estimate energy fluxes in food webs. *Methods in Ecology and Evolution*, 10(2), 270-279.

---

## 🤝 Contributing

Contributions are welcome! To contribute:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/AmazingFeature`)
3. Make your changes
4. Run validation (`Rscript deployment/pre-deploy-check.R`)
5. Commit your changes (`git commit -m 'Add AmazingFeature'`)
6. Push to the branch (`git push origin feature/AmazingFeature`)
7. Open a Pull Request

### Code Style

- Follow [tidyverse style guide](https://style.tidyverse.org/)
- Document functions using Roxygen2 comments
- Include examples and scientific references
- Test changes with pre-deploy-check.R
- Maintain modular structure in R/ directory

---

## 📖 Citation

If you use EcoNeTool in your research, please cite:

```bibtex
@software{econetool2025,
  title = {EcoNeTool: Marine Food Web Network Analysis Tool},
  author = {MARBEFES Project Team},
  year = {2025},
  version = {1.0.20},
  institution = {Klaipėda University},
  url = {https://github.com/razinkele/EcoNeTool},
  note = {Interactive R Shiny application for marine food web analysis}
}
```

### Based on the methodology from:

**Kortsch, S., Frelat, R., Pecuchet, L., Olivier, P., Putnis, I., Bonsdorff, E., Ojaveer, H., Jurgensone, I., Strāķe, S., Rubene, G., Krūze, Ē., & Nordström, M.** *Qualitative and quantitative network descriptors reveal complementary patterns of change in temporal food web dynamics.*

This work builds upon:
- **Original tutorial**: [BalticFoodWeb](https://rfrelat.github.io/BalticFoodWeb.html)
- **GitHub repository**: [BalticFoodWeb on GitHub](https://github.com/rfrelat/BalticFoodWeb)

---

## 👥 Authors & Acknowledgments

### Authors
- **MARBEFES Project Team**
- Klaipėda University, Lithuania

### Funding
- **HORIZON EUROPE** - [MARBEFES Project](https://cordis.europa.eu/project/id/101060937)
- Marine biodiversity and ecosystem functioning across scales

### Acknowledgments
- Gulf of Riga food web data: Frelat, R., & Kortsch, S. (2020)
- fluxweb package: Gauzens, B., et al. (2019)
- Original BalticFoodWeb analysis tools and methodology
- EMODnet for habitat data access
- WoRMS, OBIS, FishBase, SeaLifeBase for taxonomic data

---

## 📄 License

This project is licensed under:
- **GPL-3.0 License** - See the [LICENSE](LICENSE) file for details
- <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-sa/4.0/80x15.png" /></a> Creative Commons Attribution-ShareAlike 4.0 International License

---

## 📞 Contact & Support

- **Issues**: [GitHub Issues](https://github.com/razinkele/EcoNeTool/issues)
- **Project**: [GitHub Repository](https://github.com/razinkele/EcoNeTool)
- **MARBEFES**: [HORIZON EUROPE Project Page](https://cordis.europa.eu/project/id/101060937)

---

## 🔄 Version Information

**Current Version**: 1.0.20
**Last Updated**: 2025-12-23
**Status**: Production Ready

See [CHANGELOG.md](CHANGELOG.md) for detailed version history.

---

<div align="center">

**Built with ❤️ for marine ecology research**

[Live Demo](http://laguna.ku.lt:3838/EcoNeTool/) · [Report Bug](https://github.com/razinkele/EcoNeTool/issues) · [Request Feature](https://github.com/razinkele/EcoNeTool/issues)

</div>
