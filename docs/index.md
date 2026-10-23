---
layout: default
title: EcoNeTool Documentation
---

<div align="center">

# EcoNeTool Documentation Hub

## Marine Food Web Network Analysis Tool

[![CI Badge](https://github.com/razinkele/EcoNeTool/actions/workflows/ci.yml/badge.svg)](https://github.com/razinkele/EcoNeTool/actions/workflows/ci.yml)
[![License GPL-3.0](https://img.shields.io/badge/License-GPL%203.0-blue.svg)](https://github.com/razinkele/EcoNeTool/blob/master/LICENSE)
[![R 4.0.0+](https://img.shields.io/badge/R-%E2%89%A5%204.0.0-blue.svg)](https://www.r-project.org/)

</div>

---

## 📚 Getting Started

Choose your entry point based on your role:

### 👤 **For End Users & Researchers**

**[👉 USER MANUAL](manuals/USER_MANUAL.md)** — Complete guide to using EcoNeTool
- Installation options (live demo, local, Docker)
- Dashboard walkthrough (all 8 tabs)
- Data import workflows (EwE, CSV, Excel, RData)
- Trait research system (12 databases, confidence scoring)
- Network comparison and spatial analysis
- FAQs and troubleshooting

### 💻 **For R Developers & API Users**

**[👉 API MANUAL](manuals/API_MANUAL.md)** — Technical reference for developers
- Architecture overview (18 UI + 18 server modules)
- Complete function signatures with examples
- Trait lookup system (8 live APIs + offline SQLite)
- Network analysis functions
- Shiny module patterns
- Error handling conventions
- Quick reference with common workflows

### 📊 **For Training & Presentations**

**[👉 TRAINING PRESENTATION](presentations/EcoNeTool_MARBEFES_Lecce_Training.pptx)** — 18-slide deck for MARBEFES Lecce 2026
- Project overview and context
- Core features and capabilities
- Live demo walkthrough
- Q&A and support resources

---

## 🚀 Quick Links

| Resource | Purpose | Audience |
|----------|---------|----------|
| [Live Demo](http://laguna.ku.lt:3838/EcoNeTool/) | Try without installing | Everyone |
| [User Manual](manuals/USER_MANUAL.md) | Feature guide | End users |
| [API Manual](manuals/API_MANUAL.md) | Developer reference | R developers |
| [GitHub Repository](https://github.com/razinkele/EcoNeTool) | Source code | Developers |
| [Issues & Support](https://github.com/razinkele/EcoNeTool/issues) | Report bugs, request features | Everyone |
| [Changelog](../CHANGELOG.md) | Version history | Everyone |
| [Deployment Guide](deployment/README.md) | Production deployment | DevOps |
| [Contributing Guide](../CONTRIBUTING.md) | How to contribute | Developers |

---

## 📖 Documentation Index

### User Guides
- **[User Manual](manuals/USER_MANUAL.md)** — Complete feature walkthrough
- **[Quick Manual](QUICK_MANUAL.md)** — 5-minute getting started
- **[Trait Lookup Quickstart](TRAIT_LOOKUP_QUICKSTART.md)** — Trait research basics
- **[Automated Trait Lookup Guide](AUTOMATED_TRAIT_LOOKUP_GUIDE.md)** — Batch trait assignment

### Developer Documentation
- **[API Manual](manuals/API_MANUAL.md)** — Complete API reference
- **[API Reference](API_REFERENCE.md)** — Function signatures
- **[Database Setup Guide](DATABASE_SETUP_GUIDE.md)** — Offline trait DB configuration
- **[Habitat Integration Guide](HABITAT_INTEGRATION_GUIDE.md)** — EMODnet integration
- **[Regional Files Setup](REGIONAL_FILES_SETUP.md)** — Metaweb data setup

### Methodological Guides
- **[Trait/Foodweb Methodology](TRAIT_FOODWEB_METHODOLOGY.md)** — Scientific approach
- **[Trait/Foodweb Guide](TRAIT_FOODWEB_GUIDE.md)** — Practical workflow
- **[Deep Analysis Report](econetool-deep-analysis-2026-09-26.md)** — System analysis

### Project Documentation
- **[Deployment Guide](deployment/README.md)** — Production deployment
- **[Documentation Hub](README.md)** — All docs in one place
- **[Contributing](../CONTRIBUTING.md)** — How to contribute
- **[Code of Conduct](../CODE_OF_CONDUCT.md)** — Community standards

---

## 🎯 Common Tasks

### "I want to analyze a food web"
1. Read [User Manual Getting Started](manuals/USER_MANUAL.md#getting-started)
2. Visit [live demo](http://laguna.ku.lt:3838/EcoNeTool/) or install locally
3. Upload your data (EwE, CSV, Excel, or RData)
4. Follow [Network Comparison](manuals/USER_MANUAL.md#network-comparison) guide

### "I need to write R code using EcoNeTool functions"
1. Read [API Manual Overview](manuals/API_MANUAL.md#overview)
2. Review [Architecture](manuals/API_MANUAL.md#architecture) section
3. Look up your function in [Core Modules](manuals/API_MANUAL.md#core-modules) or [Trait Lookup](manuals/API_MANUAL.md#trait-lookup-functions)
4. See [Quick Reference](manuals/API_MANUAL.md#quick-reference) for code examples

### "I need to assign traits to species"
1. Read [User Manual Trait Research](manuals/USER_MANUAL.md#trait-research-module)
2. Follow [Trait Lookup Quickstart](TRAIT_LOOKUP_QUICKSTART.md)
3. For batch assignment, see [Automated Trait Lookup](AUTOMATED_TRAIT_LOOKUP_GUIDE.md)

### "I want to deploy EcoNeTool in production"
1. Read [Deployment Guide](deployment/README.md)
2. Run [pre-deploy check](../deployment/pre-deploy-check.R)
3. Execute [deploy script](../deploy.sh)
4. Verify with [verification script](../deployment/verify-deployment.sh)

### "I'm giving a presentation about EcoNeTool"
1. Download [Training Presentation](presentations/EcoNeTool_MARBEFES_Lecce_Training.pptx)
2. Customize slides 1-2 with your event details
3. Test [live demo](http://laguna.ku.lt:3838/EcoNeTool/) before presenting
4. Use slides 15-16 for hands-on demo walkthrough

---

## 📊 Documentation Statistics

| Metric | Value |
|--------|-------|
| Total Documentation Pages | 15+ |
| User Manual | 14.3 KB |
| API Manual | 22.9 KB |
| Training Presentation | 52.9 KB (18 slides) |
| Total Size | ~90 KB |
| API Functions Documented | 25+ |
| Code Examples | 20+ |
| Supported Databases | 12 |
| Trait Modalities | 5 |

---

## 🎓 Key Features Documented

### Network Analysis
- Topological metrics (connectance, generality, vulnerability)
- Iterative trophic level calculation
- Keystoneness analysis (Mixed Trophic Impact)
- Energy flux calculations with `fluxweb`
- Species comparison and validation

### Trait Research
- 12 integrated databases (WoRMS, FishBase, OBIS, PolyTraits, SeaLifeBase, AlgaeBase, SHARK, BVOL, etc.)
- 5 trait modalities (Body Size, Feeding, Mobility, Epibenthic, Reproduction)
- Confidence scoring (None/Low/Medium/High)
- Live and offline lookup modes
- ML-based trait prediction and phylogenetic imputation

### Spatial Analysis
- EMODnet EUSeaMap habitat integration
- Interactive Leaflet mapping
- Hexagonal grid aggregation
- Regional filtering (Baltic Sea, North Sea, Mediterranean)

### Data Management
- Multiple import formats (EwE, CSV, Excel, RData)
- Export to CSV, RData, PDF, HTML
- Network validation and quality checks
- Metadata tracking and provenance

### Advanced Features
- Rpath/Ecosim ecosystem modeling
- Regional metawebs (Baltic, Lithuanian Coast, Gulf of Riga)
- Shiny module system
- CI/CD pipeline with 112+ tests
- Pre-commit hooks and linting

---

## 🔬 Scientific References

The documentation and implementation build on peer-reviewed research:

- **Brown, J. H., et al. (2004).** Toward a metabolic theory of ecology. *Ecology*, 85(7), 1771–1789.
- **Bersier, L. F., et al. (2002).** Quantitative descriptors of food web matrices. *Ecology*, 83(9), 2394–2407.
- **Libralato, S., et al. (2006).** A method for identifying keystone species in food web models. *Ecological Modelling*, 195(3–4), 153–171.
- **Gauzens, B., et al. (2019).** fluxweb: An R package to easily estimate energy fluxes in food webs. *Methods Ecol. Evol.*, 10(2), 270–279.
- **Kortsch, S., et al.** Qualitative and quantitative network descriptors reveal complementary patterns of change in temporal food web dynamics.

Original tutorial: [BalticFoodWeb](https://rfrelat.github.io/BalticFoodWeb.html) · [BalticFoodWeb on GitHub](https://github.com/rfrelat/BalticFoodWeb)

---

## 📞 Support & Contact

- **Live Demo**: [http://laguna.ku.lt:3838/EcoNeTool/](http://laguna.ku.lt:3838/EcoNeTool/)
- **GitHub Issues**: [github.com/razinkele/EcoNeTool/issues](https://github.com/razinkele/EcoNeTool/issues)
- **Source Code**: [github.com/razinkele/EcoNeTool](https://github.com/razinkele/EcoNeTool)
- **Project**: [MARBEFES on CORDIS](https://cordis.europa.eu/project/id/101060937)

---

## 📄 License

- **Source Code**: [GPL-3.0](https://github.com/razinkele/EcoNeTool/blob/master/LICENSE)
- **Documentation & Data**: [Creative Commons Attribution-ShareAlike 4.0](https://creativecommons.org/licenses/by-sa/4.0/)

---

## 🏢 Attribution

**Developed by:** MARBEFES Project Team, Klaipėda University, Lithuania

**Funded by:** [HORIZON EUROPE MARBEFES](https://cordis.europa.eu/project/id/101060937)
*Marine Biodiversity and Ecosystem Functioning leading to Ecosystem Services*

**Data sources:** WoRMS, OBIS, FishBase, SeaLifeBase, AlgaeBase, SHARK, BVOL, freshwaterecology.info, PolyTraits, EMODnet

---

## 📝 Version Information

**Current Version**: 1.7.10  
**Last Updated**: 2026-10-23  
**Status**: Production Ready

See [CHANGELOG](../CHANGELOG.md) for version history.

---

<div align="center">

### Ready to get started?

**[👉 Begin with User Manual](manuals/USER_MANUAL.md)** | **[👉 View API Reference](manuals/API_MANUAL.md)** | **[👉 Try Live Demo](http://laguna.ku.lt:3838/EcoNeTool/)**

</div>
