# EcoNeTool Deployment Guide

This directory contains deployment scripts and configuration files for deploying the EcoNeTool Shiny application.

**Latest Version**: v1.0.20 - Rpath Module Refactoring (2025-12-09)
- ✅ Rpath module refactored into modular architecture
- ✅ Clean separation of UI and server logic
- ✅ Enhanced import: tooltips, comments, calibration data
- ✅ All tests passing (15/15)
- See [DEPLOYMENT_CHECKLIST_v1.0.20.md](DEPLOYMENT_CHECKLIST_v1.0.20.md) for deployment guide

**Previous Version**: v1.0.19 - visNetwork Label & Topology Fix (2025-12-08)
- ✅ Network graphs now display species names (not numbers)
- ✅ Proper trophic level layout with optimal spacing
- See [DEPLOYMENT_CHECKLIST_v1.0.19.md](DEPLOYMENT_CHECKLIST_v1.0.19.md) for deployment guide

## 📋 Contents

- **`deploy.sh`** - Main deployment script (supports Docker and Shiny Server)
- **`pre-deploy-check.R`** - Pre-deployment validation script
- **`install_dependencies.R`** - R package installation script
- **`shiny-server.conf`** - Shiny Server configuration file

## 🚀 Quick Start

### Deploy to Shiny Server (Recommended)

```bash
cd deployment
sudo ./deploy.sh --shiny-server
```

This will:
1. Run pre-deployment checks
2. Install R package dependencies
3. Copy application files to `/srv/shiny-server/EcoNeTool`
4. Configure and restart Shiny Server

### Deploy with Docker

```bash
cd deployment
./deploy.sh --docker
```

## 📝 Pre-Deployment Checks

Before deploying, you can run validation checks:

```bash
cd deployment
Rscript pre-deploy-check.R
```

This validates:
- Required files exist (app.R, functions.R, BalticFW.Rdata)
- **Metaweb files** (Phase 2: Baltic, Arctic, Atlantic metawebs)
- Data file structure (net and info objects)
- R package dependencies (including Phase 1 spatial packages: sf)
- R syntax in all scripts
- App structure and functions (Phase 1, Phase 2, and core functions)
- **Critical fixes validation** (reactive scope, input validation, metaweb paths)
- **Cell size validation** and memory protection (Phase 1)

## 📦 Manual Package Installation

If you need to install R packages manually:

```bash
cd deployment
Rscript install_dependencies.R
```

Required packages:
- **Shiny**: shiny, bs4Dash, shinyjs, shinyWidgets
- **Data**: dplyr, tidyr, readr, tibble, stringr
- **Network**: igraph, visNetwork, fluxweb
- **Visualization**: ggplot2, plotly, DT
- **Utilities**: jsonlite, openxlsx, readxl
- **Phase 1 Spatial**: sf (required for Phase 1 Spatial Analysis)
- **Optional**: leaflet (interactive maps), sp, Hmisc (ECOPATH native import)

## 🔧 Configuration

### Shiny Server Configuration

The deployment script automatically configures Shiny Server. The config file (`shiny-server.conf`) includes:

- Server listens on port **3838**
- App location: `/srv/shiny-server/EcoNeTool`
- Logs: `/var/log/shiny-server`
- URL: `http://your-server:3838/EcoNeTool`

To manually edit configuration:
```bash
sudo nano /etc/shiny-server/shiny-server.conf
sudo systemctl restart shiny-server
```

## 📊 Application Structure

Files deployed to production:

```
/srv/shiny-server/EcoNeTool/
├── app.R              # Main Shiny application (sources modular components)
├── BalticFW.Rdata     # Food web data (net and info objects)
├── run_app.R          # Optional: App runner
├── R/                 # Modular application code
│   ├── config.R       # Configuration constants
│   ├── config/        # Plugin system configuration
│   ├── functions/     # Analysis and utility functions
│   │   ├── ecopath_import.R           # ECOPATH database import
│   │   ├── rpath_integration.R         # Rpath conversion & balance
│   │   ├── auxillary_parser.R          # Tooltips/comments parsing
│   │   ├── network_visualization.R     # Network graph functions
│   │   ├── topological_metrics.R       # Network metrics
│   │   ├── flux_calculations.R         # Energy flux analysis
│   │   ├── metaweb_core.R              # Metaweb functions
│   │   └── ...                         # Other analysis functions
│   ├── ui/            # UI components (modular)
│   │   ├── rpath_ui.R                  # Rpath module UI
│   │   ├── network_ui.R                # Network visualization UI
│   │   ├── topological_ui.R            # Topological analysis UI
│   │   └── ...                         # Other UI modules
│   └── modules/       # Server logic modules
│       ├── rpath_server.R              # Rpath module server (NEW)
│       └── ...                         # Other server modules
├── www/               # Web assets (images, CSS, JS)
├── examples/          # Example datasets (ECOPATH databases)
└── metawebs/          # Phase 2: Regional metaweb library
    ├── baltic/
    │   └── baltic_kortsch2021.rds
    ├── arctic/
    │   ├── kongsfjorden_farage2021.rds
    │   ├── barents_arctic_kortsch2015.rds
    │   └── barents_boreal_kortsch2015.rds
    └── atlantic/
        └── north_sea_frelat2022.rds
```

**Note**:
- The R/ directory contains all modular code components
- **NEW**: Rpath module now split into UI (R/ui/rpath_ui.R) and Server (R/modules/rpath_server.R)
- Metaweb files (.rds) are required for Phase 2 (Metaweb Manager) and Phase 1 (Spatial Analysis) features
- The pre-deployment check validates all critical files and module structure

## 🛠️ Troubleshooting

### Check Shiny Server Status

```bash
sudo systemctl status shiny-server
```

### View Application Logs

```bash
sudo tail -f /var/log/shiny-server.log
```

### Restart Shiny Server

```bash
sudo systemctl restart shiny-server
```

### Check for Errors in Pre-Deployment

If pre-deployment checks fail:

1. **Missing packages**: Run `Rscript install_dependencies.R`
2. **Syntax errors**: Check the error messages in validation output
3. **Missing data**: Ensure `BalticFW.Rdata` exists in the app directory
4. **Data structure**: Verify `net` and `info` objects with correct columns

### Seeing Old Version After Deployment?

If you deployed but still see the old version of the app, try these steps in order:

#### 1. Clear Browser Cache (Most Common Fix)
```bash
# Hard reload in browser
Ctrl+Shift+R (Linux/Windows) or Cmd+Shift+R (Mac)

# OR open in incognito/private browsing mode
```

#### 2. Verify What's Actually Deployed
```bash
cd deployment
sudo ./verify-deployment.sh
```

This will show you:
- Which files are deployed
- Whether the old or new version is on the server
- File modification times
- Recent errors

#### 3. Force Reload the App
```bash
cd deployment
sudo ./force-reload.sh
```

This script:
- Stops Shiny Server completely
- Clears all caches and bookmarks
- Re-deploys fresh files
- Starts Shiny Server
- Then **clear your browser cache** again

### Common Issues

**Issue**: "Shiny Server failed to start"
```bash
# Check detailed logs
sudo journalctl -u shiny-server -n 50

# Check port availability
sudo netstat -tulpn | grep 3838
```

**Issue**: "Package installation failed"
```bash
# Install system dependencies first
sudo apt-get install libcurl4-openssl-dev libssl-dev libxml2-dev
```

**Issue**: "Permission denied"
```bash
# Ensure correct ownership
sudo chown -R shiny:shiny /srv/shiny-server/EcoNeTool
```

## 🔄 Updating the Application

To update an already-deployed application:

```bash
cd deployment
sudo ./deploy.sh --shiny-server
```

The script will:
- Backup the current configuration
- Remove old files
- Deploy new version
- Restart the server

## 🐳 Docker Deployment

### Prerequisites

- Docker installed
- Docker Compose installed

### Build and Run

```bash
cd deployment
./deploy.sh --docker
```

### Docker Commands

```bash
# View logs
docker-compose logs -f

# Stop application
docker-compose down

# Restart
docker-compose restart
```

## 📚 System Requirements

### Minimum Requirements

- **OS**: Ubuntu 18.04+ (or compatible Linux distribution)
- **R**: Version 4.0+
- **RAM**: 2GB minimum, 4GB recommended
- **Disk**: 500MB for application + dependencies

### Shiny Server Requirements

- **Port 3838** must be available
- **User**: shiny (created automatically by Shiny Server)
- **Directory**: `/srv/shiny-server` with proper permissions

## 🔐 Security Considerations

1. **Firewall**: Ensure port 3838 is accessible
2. **SSL/HTTPS**: Consider using nginx reverse proxy for HTTPS
3. **Access Control**: Configure Shiny Server authentication if needed
4. **Updates**: Regularly update R packages and system dependencies

## 📖 Additional Resources

- [Shiny Server Documentation](https://docs.posit.co/shiny-server/)
- [EcoNeTool GitHub Repository](https://github.com/your-repo)
- [Food Web Network Analysis](https://cran.r-project.org/package=fluxweb)

## 📞 Support

For issues or questions:
1. Check the troubleshooting section above
2. Review application logs
3. Run pre-deployment checks
4. Contact the development team

## 🔄 Recent Updates

### v1.0.20 - Rpath Module Refactoring (2025-12-09) **LATEST**
- ✅ Refactored Rpath module into clean, modular architecture
- ✅ Separated UI component (R/ui/rpath_ui.R - 67 lines)
- ✅ Separated server logic (R/modules/rpath_server.R - 1688 lines)
- ✅ Enhanced ECOPATH import: tooltips, comments, calibration, pedigree data
- ✅ All tests passing (15/15): file loading, functions, data operations, mass balance
- ✅ Code consistency verified across all Rpath files
- ✅ Follows Shiny module best practices
- ✅ Backward compatible (no interface changes)

For detailed information see:
- `../docs/development/RPATH_MODULE_REFACTORING.md` - Complete refactoring documentation
- `../docs/SESSION_COMPLETE_RPATH_REFACTORING.md` - Session summary and test results
- `../docs/development/ENHANCED_IMPORT_COMPLETE.md` - Enhanced import features
- `../docs/development/COMMENTS_TOOLTIPS_COMPLETE.md` - Tooltips/comments implementation

### v1.0.19 - visNetwork Label & Topology Fix (2025-12-08)
- ✅ Network graphs display species names (not numbers)
- ✅ Proper trophic level layout with optimal spacing

### v2.3 - Phase 1: Spatial Analysis
- ✅ Complete UI and server logic for hexagonal grid-based analysis
- ✅ Integration with Phase 2 metaweb library
- ✅ Interactive species data upload and sample generation
- ✅ Local network extraction from metawebs
- ✅ Spatial metrics calculation (S, L, C, LD, meanTL, maxTL)
- ✅ Export to CSV and RDS formats

### Critical Fixes Applied
- ✅ **CRITICAL-001**: Fixed reactive value scope (current_metaweb now in global scope)
- ✅ **CRITICAL-002**: Added comprehensive input validation (step-by-step error messages)
- ✅ **CRITICAL-003**: Metaweb file paths validated at startup
- ✅ **HIGH-002**: Cell size validation (100m - 100km range, max 1M hexagons)

### Phase 2: Metaweb Manager (COMPLETE)
- ✅ Regional metaweb library (5 marine ecosystems)
- ✅ Metaweb creation, validation, and editing
- ✅ Interactive network visualization
- ✅ Export to RDS and CSV formats

---

**Version**: 1.0.20
**Last Updated**: 2025-12-09
**Maintainer**: EcoNeTool Development Team
**Production Readiness**: ✅ Ready for Deployment
