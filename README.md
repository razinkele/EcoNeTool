# EcoNeTool - Marine Food Web Network Analysis Tool

[![License: GPL-3.0](https://img.shields.io/badge/License-GPL%203.0-blue.svg)](LICENSE)
[![GitHub](https://img.shields.io/badge/GitHub-razinkele/EcoNeTool-blue)](https://github.com/razinkele/EcoNeTool)

**Interactive Shiny Dashboard for analyzing trophic interactions, biomass distributions, and energy fluxes in marine ecosystems**

## 📋 Overview

EcoNeTool is an interactive web application built with R Shiny that provides comprehensive analysis tools for marine food web networks. The application integrates qualitative and quantitative network analysis approaches to understand food web structure and dynamics.

### Key Features

- **📊 Interactive Network Visualization**: Dynamic, zoomable food web graphs with species-level details
- **📁 Multiple Data Import Formats**: Support for RData, CSV, Excel, and ECOPATH exports
- **📈 Topological Metrics**: Connectance, generality, vulnerability, and trophic levels
- **⚖️ Biomass Analysis**: Node-weighted metrics accounting for species abundance
- **⚡ Energy Flux Calculations**: Metabolic theory-based energy flow analysis
- **🔑 Keystoneness Analysis**: Identify keystone species using Mixed Trophic Impact
- **✏️ Internal Data Editor**: Edit species information and network matrices directly

## 🚀 Quick Start

### Running Locally

1. **Clone the repository**:
   ```r
   # Install required packages if needed
   install.packages(c("shiny", "bs4Dash", "igraph", "fluxweb", "visNetwork", "DT"))

   # Run the app
   shiny::runApp("path/to/EcoNeTool")
   ```

2. **Or use the helper script**:
   ```r
   source("run_app.R")
   ```

### Online Access

Visit the deployed application at: `http://laguna.ku.lt:3838/EcoNeTool/`

## 📦 Installation

### Prerequisites

- R (>= 4.0.0)
- Required R packages:
  - `shiny`
  - `bs4Dash`
  - `igraph`
  - `fluxweb`
  - `visNetwork`
  - `DT`

### Installing Dependencies

```r
# Install from CRAN
install.packages(c("shiny", "bs4Dash", "igraph", "fluxweb", "visNetwork", "DT"))
```

## 📖 Data Format

### Required Data Structure

EcoNeTool requires two main components:

1. **Network Adjacency Matrix** - Who eats whom (directed graph)
2. **Species Information Table** - Attributes for each species

### Supported Formats

- **RData**: `net` (igraph object) + `info` (data.frame)
- **CSV/Excel**: Network matrix + Species info table
- **ECOPATH**: Native database (.mdb, .ewemdb) or exported CSV files

### Example Data Structure

```r
# Network: igraph object
library(igraph)
net <- graph_from_adjacency_matrix(adj_matrix, mode='directed')

# Species info: data.frame
info <- data.frame(
  species = c('Species_A', 'Species_B', 'Species_C'),
  fg = factor(c('Fish', 'Zooplankton', 'Phytoplankton')),
  meanB = c(1250.5, 850.2, 2100.0),
  bodymasses = c(50.0, 0.5, 0.001),
  met.types = c('ectotherm vertebrates', 'invertebrates', 'Other'),
  efficiencies = c(0.85, 0.75, 0.40),
  losses = c(0.12, 0.08, 0.05)
)
```

See `examples/` directory for complete example datasets.

## 🔬 Analysis Features

### 1. Network Visualization
- Interactive force-directed network graphs
- Color-coded by functional groups
- Node size proportional to biomass
- Edge width shows interaction strength

### 2. Topological Metrics
- Species richness (S)
- Link density (L/S)
- Connectance (C)
- Mean generality and vulnerability
- Trophic levels

### 3. Biomass-Weighted Analysis
- Node-weighted connectance
- Node-weighted generality/vulnerability
- Biomass-based importance metrics

### 4. Energy Flux Analysis
- Metabolic theory-based flux calculations
- Temperature-adjusted metabolic rates
- Flux-weighted network visualization
- Shannon diversity of energy flows

### 5. Keystoneness Analysis
- Mixed Trophic Impact (MTI) calculations
- Keystoneness index (impact/biomass ratio)
- Identification of keystone species

## 📊 Default Dataset

The application includes the **Gulf of Riga Food Web** dataset:
- **Source**: Frelat, R., & Kortsch, S. (2020)
- **Period**: 1979-2016 (37 years)
- **Taxa**: 34 species across 5 functional groups
- **Links**: 207 trophic interactions

## 🛠️ Deployment

### Server Deployment

Use the included deployment script:

```bash
# Standard deployment to laguna.ku.lt
./deploy.sh

# Dry run (see what would be deployed)
./deploy.sh --dry-run

# Deploy without backup
./deploy.sh --no-backup
```

### Configuration

Edit deployment settings in `deploy.sh`:
- Server host
- Server user
- Deployment paths
- Backup settings

## 📚 Documentation

- **Data Import Guide**: See the "Data Import" tab in the application
- **Format Examples**: Check the `examples/` directory
- **Deployment Guide**: See `deployment/README.md`

## 🤝 Contributing

Contributions are welcome! Please feel free to submit issues or pull requests on GitHub.

## 👥 Authors

- **MARBEFES Project Team**
- Klaipėda University, Lithuania

## 📄 License

This project is licensed under the GPL-3.0 License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

Based on the methodology from:

**Kortsch, S., Frelat, R., Pecuchet, L., Olivier, P., Putnis, I., Bonsdorff, E., Ojaveer, H., Jurgensone, I., Strāķe, S., Rubene, G., Krūze, Ē., & Nordström, M.** *Qualitative and quantitative network descriptors reveal complementary patterns of change in temporal food web dynamics.*

This work builds upon the original BalticFoodWeb analysis tools:
- Original tutorial: [BalticFoodWeb](https://rfrelat.github.io/BalticFoodWeb.html)

## 📞 Contact

For questions or support, please open an issue on [GitHub](https://github.com/razinkele/EcoNeTool/issues).

## 🔗 Links

- **GitHub Repository**: https://github.com/razinkele/EcoNeTool
- **MARBEFES Project**: [Horizon Europe MARBEFES](https://cordis.europa.eu/project/id/101060937)
- **Original Work**: [BalticFoodWeb on GitHub](https://github.com/rfrelat/BalticFoodWeb)

---

<a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-sa/4.0/80x15.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">Creative Commons Attribution-ShareAlike 4.0 International License</a>.
