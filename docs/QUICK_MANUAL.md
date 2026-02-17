# EcoNeTool Quick Manual

## What is EcoNeTool?
EcoNeTool is an interactive R Shiny dashboard for analyzing marine food web networks, trophic interactions, biomass distributions, and energy fluxes. It supports multiple data formats and provides advanced network analysis and visualization tools.

## Getting Started

### 1. Requirements
- R (≥ 4.0.0 recommended)
- RStudio (recommended for local use)
- All required R packages (see `install_dependencies.R`)

### 2. Launching the App
- **Locally (RStudio):**
  1. Open `app.R` in RStudio
  2. Click "Run App"
- **Command Line:**
  ```bash
  Rscript run_app.R
  ```
- **Shiny Server:**
  1. Deploy using `deployment/deploy.sh`
  2. Access at `http://<server-ip>:3838/EcoNeTool`

## Main Features
- Interactive network visualization
- Import/export: RData, CSV, Excel, ECOPATH
- Topological and biomass metrics
- Parameter editors for model balancing
- Real-time error diagnostics
- Multi-tab UI for analysis, editing, and reporting

## Data Import
- Use the import tab to load food web data (RData, CSV, ECOPATH)
- For ECOPATH databases on Windows, see `docs/user-guides/ECOPATH_WINDOWS_IMPORT_GUIDE.md`

## Parameter Editing & Balancing
- Use the "Group Parameters" and "Diet Matrix" tabs to fix missing/invalid values before balancing
- See `docs/user-guides/PARAMETER_EDITORS_GUIDE.md` and `RPATH_BALANCING_FIX.md` for troubleshooting

## Deployment
- See `deployment/README.md` for full deployment instructions
- Run `deploy.sh` for Shiny Server or Docker deployment

## Documentation
- Full documentation: `docs/README.md`
- User guides: `docs/user-guides/`
- Testing guides: `docs/testing/`
- Developer docs: `docs/development/`

## Support
- For troubleshooting, see `docs/user-guides/ENVIRONMENT_FIX_GUIDE.md`
- For ECOPATH/Rpath integration, see `docs/user-guides/RPATH_INTEGRATION_GUIDE.md`

---
For more details, see the README and documentation folder.
