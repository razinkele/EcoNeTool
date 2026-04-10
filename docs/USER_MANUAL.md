# EcoNeTool User Manual

**A guide for marine ecologists, food web modellers, and MARBEFES partners.**

EcoNeTool is an interactive web dashboard for building, analysing, and comparing marine
food webs. It brings together trait lookups, topological analysis, energy flux
calculations, keystoneness, spatial analysis, and Ecopath/Ecosim simulations in a
single browser-based workspace — no R knowledge required.

This manual describes how to use the tool through its graphical interface. For
developer-oriented documentation, see the `API_REFERENCE.md` and the `R/` source tree.

---

## Table of Contents

1. [Getting Started](#1-getting-started)
2. [Importing Data](#2-importing-data)
3. [Dashboard Overview](#3-dashboard-overview)
4. [Network Analysis](#4-network-analysis)
5. [Trait Research](#5-trait-research)
6. [Rpath Integration](#6-rpath-integration)
7. [Metaweb Management](#7-metaweb-management)
8. [Spatial Analysis](#8-spatial-analysis)
9. [Exporting Results](#9-exporting-results)
10. [Troubleshooting](#10-troubleshooting)
11. [Glossary](#11-glossary)

---

## 1. Getting Started

### 1.1 What EcoNeTool does

EcoNeTool helps marine ecologists turn species lists and interaction data into
quantitative food web models. It supports the full MARBEFES workflow from raw data
ingestion to publication-ready metrics:

- Import food webs from Ecopath with Ecosim (EwE) databases, CSV, or Excel.
- Look up functional traits for any marine species from 12 online and local databases.
- Build food web topologies from trait-based predation rules.
- Compute structural, biomass-weighted, and energy-flux metrics.
- Identify keystone species using the Mixed Trophic Impact (MTI) method.
- Perform spatial food web analysis on hexagonal grids with EMODnet habitat layers.
- Run dynamic Ecosim simulations through an integrated Rpath interface.

### 1.2 Who it is for

EcoNeTool is built for:

- **Marine ecologists** who want to explore food web structure without writing code.
- **Food web modellers** preparing Ecopath inputs or comparing alternative topologies.
- **MARBEFES partners** contributing to WP3.2 spatial food web deliverables.
- **Graduate students** learning quantitative network ecology.

You do not need to be an R programmer. If you can navigate a web dashboard and edit a
spreadsheet, you can use EcoNeTool.

### 1.3 MARBEFES HORIZON EUROPE context

EcoNeTool is developed within the **MARBEFES** project (*Marine Biodiversity and
Ecosystem Functioning leading to Ecosystem Services*), part of the HORIZON EUROPE
programme. It is a core tool for Work Package 3.2 (*Food web structure and functioning
across European seas*), supporting the Benthic–pelagic–Biodiversity Testbed (BBT)
sites across the Arctic, Atlantic, Baltic, Mediterranean, and North Sea.

The tool implements the workflow described in the MARBEFES WP3.2b guidance document
(see `docs/MARBEFES-WP3.2b_Guidance_Draft-v5_28.1.2026.pdf`).

### 1.4 Accessing the tool

There are two ways to use EcoNeTool:

| Mode | URL / Command | Best for |
|---|---|---|
| **Live demo** | <http://laguna.ku.lt:3838/EcoNeTool/> | Trying the tool, light analysis, teaching |
| **Local install** | `Rscript run_app.R` from the project folder | Large datasets, offline work, custom data |

> [!TIP]
> The live demo shares a single R process across users, so heavy computations
> (thousands of species, large Ecosim runs) are better done locally.

### 1.5 System requirements

**For the live demo:** any modern browser (Chrome, Edge, Firefox, Safari). No local
installation needed.

**For local installation:**

- **R >= 4.0** (tested on R 4.4.1)
- Windows, macOS, or Linux
- 8 GB RAM (16 GB recommended for large food webs)
- ~2 GB disk space for the app plus cached trait databases
- A modern web browser
- Internet connection for trait lookups (offline mode is available for local CSV databases)

> [!NOTE]
> On first launch, EcoNeTool will install required R packages automatically. This can
> take 10–20 minutes the first time. Subsequent launches take a few seconds.

---

## 2. Importing Data

EcoNeTool accepts three main data formats, each suited to a different starting point.

### 2.1 Supported formats

| Format | File extension | Description |
|---|---|---|
| **EwE database** | `.ewemdb`, `.mdb`, `.eiixml` | Native Ecopath with Ecosim model files |
| **CSV** | `.csv` | Plain text species list and/or interaction matrix |
| **Excel** | `.xlsx`, `.xls` | Multi-sheet workbook with groups and diet |
| **RData** | `.RData`, `.rds` | Pre-built EcoNeTool session |

### 2.2 Required data structure

At minimum, EcoNeTool needs two pieces of information:

1. **A species list** (or functional group list) with one row per group.
2. **A diet / interaction matrix** describing who eats whom.

#### Species list — required columns

| Column | Required | Description |
|---|---|---|
| `Group` or `Species` | yes | Unique name of the species or functional group |
| `Biomass` | recommended | Biomass density (t/km^2 or g/m^2) |
| `PB` | optional | Production/Biomass ratio (per year) |
| `QB` | optional | Consumption/Biomass ratio (per year) |
| `EE` | optional | Ecotrophic efficiency (0–1) |
| `TL` | optional | Trophic level (computed if absent) |

#### Interaction / diet matrix

A square matrix where rows are predators and columns are prey (or vice versa). Cells
contain either:

- **Binary values** (0/1) for presence/absence of a feeding link, or
- **Diet proportions** (0–1) that sum to 1 per predator row.

> [!TIP]
> Templates are provided in `metawebs/species_template.csv` and
> `metawebs/interactions_template.csv`. Copy these and fill in your data.

### 2.3 Step-by-step: importing an EwE database

1. Open the **Data Import** tab.
2. Click the **EwE Import** sub-tab.
3. Click **Browse** and select your `.ewemdb` or `.eiixml` file.
4. EcoNeTool will parse the database and show a preview of functional groups,
   biomasses, and diet.
5. Click **Load into EcoNeTool** to activate the model.
6. Navigate to the **Food Web Network** tab to visualise the imported model.

> [!NOTE]
> Older Microsoft Access `.mdb` files require a 32-bit Access driver on Windows. If
> import fails, export your model to EII-XML from within EwE first.

### 2.4 Step-by-step: importing CSV

1. Open **Data Import** > **General Import**.
2. Upload your **species list** CSV first.
3. Upload your **interaction matrix** CSV second.
4. Confirm the row/column orientation (predators as rows by convention).
5. Click **Apply** to build the food web object.

### 2.5 Step-by-step: importing Excel

1. Open **Data Import** > **General Import**.
2. Upload your `.xlsx` file.
3. Select which sheet contains the species list and which contains the diet matrix.
4. Confirm column headers and click **Apply**.

### 2.6 Common import errors

| Error message | Cause | Fix |
|---|---|---|
| *Column 'Group' not found* | Species list missing name column | Rename your first column to `Group` or `Species` |
| *Non-numeric values in diet matrix* | Text or NA mixed with numbers | Replace with 0, or clean the sheet |
| *Row sums exceed 1* | Diet proportions not normalised | Rescale each row to sum to 1 |
| *Orphan species* | Species in diet matrix not in list | Add missing species, or remove from matrix |
| *Unreadable .ewemdb* | Wrong Access driver | Export to EII-XML from EwE and re-import |

---

## 3. Dashboard Overview

The dashboard has 15 tabs in the left sidebar. Tabs are roughly grouped into:
data input, analysis, traits, spatial, external databases, and simulation.

### 3.1 Dashboard (home)

**What it does.** Landing page with value boxes showing the currently loaded model
(number of species, links, average trophic level, connectance) plus quick-start links.

**When to use it.** At the start of a session to confirm your data is loaded, or as a
snapshot to share with colleagues.

### 3.2 Data Import

**What it does.** Loads food web data from EwE, CSV, Excel, or RData sources. Also
lets you export the current metaweb.

**When to use it.** Every session begins here unless you are re-opening a saved
session.

**What you can do.** Upload files, preview contents, validate structure, and commit
data to the active session.

### 3.3 Food Web Network

**What it does.** Interactive network visualisation powered by visNetwork. Nodes are
species; edges are trophic links coloured by strength.

**What you can do.**
- Drag nodes to re-layout the graph.
- Hover for species info; click for detailed panels.
- Colour by trophic level, biomass, or keystoneness.
- Filter to show only predators or prey of a selected species.
- Export the graph as PNG or HTML.

### 3.4 Topological Metrics

**What it does.** Computes structural network metrics independent of biomass.

**Metrics reported.** Species count (S), link count (L), connectance (C = L/S^2),
linkage density, generality, vulnerability, mean shortest path, clustering coefficient,
omnivory index, and trophic level distribution.

**When to use it.** To characterise the complexity of a food web, or to compare
topologies across sites.

### 3.5 Biomass Analysis

**What it does.** Weights topological metrics by biomass — for example, biomass-
weighted connectance, biomass flow pyramids, and biomass-at-trophic-level distributions.

**When to use it.** When you have reliable biomass data and want metrics that reflect
the quantitative importance of each species.

### 3.6 Energy Fluxes

**What it does.** Calculates per-link energy fluxes using the food web energetics
framework (Barnes et al. 2018). Fluxes are derived from biomass, metabolic losses, and
assimilation efficiencies.

**What you can do.** Visualise flux magnitudes as edge widths, aggregate fluxes by
trophic level, and export a flux matrix.

### 3.7 Keystoneness Analysis

**What it does.** Identifies keystone species using the ECOPATH Mixed Trophic Impact
method (Libralato et al. 2006). Classifies species as **Keystone**, **Dominant**, or
**Rare** based on their overall ecosystem impact relative to biomass.

**What you see.** An MTI matrix heatmap, a keystoneness index bar chart, and a
scatter plot of impact vs biomass with classification labels.

### 3.8 Internal Data Editor

**What it does.** Edit the active food web inline — add or remove species, modify
biomasses, adjust diet fractions — without leaving the dashboard.

**When to use it.** For quick corrections, sensitivity checks, or building a small
model from scratch.

> [!WARNING]
> Changes made here replace the active model in memory. Export to CSV if you want to
> preserve a version before editing.

### 3.9 Metaweb Manager

**What it does.** Loads, compares, and overlays regional reference metawebs — large
expert-curated food webs covering the Arctic, Atlantic, Baltic, Mediterranean, and
North Sea.

**What you can do.** Subset a metaweb to species in your local food web, compare your
interactions against the reference, and identify missing or unexpected links.

### 3.10 Trait Research

**What it does.** Looks up functional traits (body size, feeding mode, habitat,
temperature preference, etc.) for your species list from 12 connected databases.

**What you see.** Species-by-trait table, provenance badges indicating which database
each value came from, confidence scores, and a lookup status panel.

See [Section 5](#5-trait-research) for the full workflow.

### 3.11 Food Web Construction

**What it does.** Builds a predicted food web topology from trait-based feeding rules.
Uses body size ratios, habitat overlap, and feeding modes to infer which species can
feed on which.

**When to use it.** When you have a species list but no observed diet matrix, or when
you want a "null" topology to compare against observed data.

### 3.12 Spatial Analysis

**What it does.** Implements the MARBEFES WP3.2 spatial workflow. Creates a hexagonal
grid over a study area, assigns species to cells using OBIS occurrences and EMODnet
habitat layers, and extracts local food webs per cell.

**What you can do.**
- Define a study area by polygon or bounding box.
- Overlay EMODnet Seabed Habitats and regional BBT boundaries.
- Compute network metrics per hexagon.
- Export cell-level summaries for GIS.

See [Section 8](#8-spatial-analysis).

### 3.13 EcoBase Connection

**What it does.** Connects to the **EcoBase** repository of published Ecopath models
(<http://ecobase.ecopath.org>). Browse, download, and import any public model directly
into your session.

**When to use it.** To benchmark your model against published studies or to bootstrap
a regional baseline.

### 3.14 SHARK Data

**What it does.** Queries the Swedish national marine data archive (SHARK, SMHI) for
zooplankton, phytoplankton, and benthic monitoring data. Results can be converted to
functional groups and fed into the food web builder.

### 3.15 ECOPATH/ECOSIM

**What it does.** Full Rpath (R implementation of Ecopath with Ecosim) module. Build
an Ecopath mass-balance model, run Ecosim dynamic simulations, and compare scenarios.

See [Section 6](#6-rpath-integration).

### 3.16 Navigation tips

> [!TIP]
> - The sidebar can be collapsed with the hamburger icon to give more room to plots.
> - Most plots are interactive — hover for tooltips, click legend items to toggle.
> - Tables with the download icon can be exported to CSV or Excel.
> - The gear icon in the top-right opens the Settings modal (plugins, API keys, about).

---

## 4. Network Analysis

This section explains the scientific interpretation of the metrics EcoNeTool produces.

### 4.1 Connectance

**Connectance** (C) measures how interconnected a food web is:

> C = L / S^2

where **L** is the number of realised trophic links and **S** is the number of species.
Values typically range from 0.05 to 0.3 for marine food webs. Higher connectance is
generally associated with greater structural stability but can also propagate
disturbances faster.

### 4.2 Generality and vulnerability

- **Generality** = mean number of prey per predator. High generality indicates
  generalist consumers.
- **Vulnerability** = mean number of predators per prey. High vulnerability means
  species experience strong top-down control.

Together, these describe the **directional asymmetry** of the food web.

### 4.3 Trophic level

Trophic level (TL) is calculated iteratively from diet composition:

> TL_i = 1 + sum_j (DC_ij * TL_j)

where DC_ij is the diet fraction of prey j in predator i. Primary producers and
detritus have TL = 1. EcoNeTool computes trophic levels automatically whenever a
diet matrix is available, and caches results for speed.

> [!NOTE]
> EcoNeTool uses the short-weighted trophic level by default (Pauly et al. 2000). If
> you need a different definition, you can override values in the Internal Data
> Editor.

### 4.4 Keystoneness analysis

A **keystone species** is one whose ecosystem impact is disproportionately large
relative to its biomass. EcoNeTool implements the Libralato et al. (2006) method:

1. Build the Mixed Trophic Impact (MTI) matrix from diet composition.
2. Compute each species' **overall effect** = sum of absolute MTI values.
3. Compute **keystoneness index** KS_i = log10(overall_effect_i * (1 - p_i))
   where p_i is the proportion of total biomass contributed by species i.
4. Classify species:
   - **Keystone:** high KS, low biomass (< 5% of total)
   - **Dominant:** high KS, high biomass (>= 5% of total)
   - **Rare:** low KS, low biomass

### 4.5 Energy flux visualisation

Energy fluxes are estimated using the food web energetics approach. For each predator,
the total energy needed to cover metabolic losses and biomass production is distributed
across its prey in proportion to diet. The resulting per-link fluxes (J/m^2/yr) are
shown as weighted edges in the Food Web Network tab and summarised in the Energy
Fluxes tab.

### 4.6 Mixed Trophic Impact (MTI)

MTI quantifies the direct and indirect effect of a small increase in the biomass of
one species on every other species in the web. Positive values indicate a net positive
effect; negative values indicate a net negative effect. The MTI matrix is displayed as
a red-blue heatmap in the Keystoneness Analysis tab.

**Reading the heatmap.** Rows are impacting species; columns are impacted species.
Strong red cells indicate predators whose presence benefits the column species
(typically through indirect effects like top-down control on competitors).

---

## 5. Trait Research

EcoNeTool's trait lookup engine is one of its core features. It combines online APIs
and local CSV databases to provide harmonised trait values for any marine species.

### 5.1 The 12-database pipeline

When you submit a species list, EcoNeTool queries databases in order and merges
results:

| Order | Database | Type | Main traits |
|---|---|---|---|
| 1 | WoRMS | Online | Taxonomy, valid name, authority |
| 2 | OBIS | Online | Occurrence-derived habitat and depth |
| 3 | FishBase | Online | Fish biology, diet, size, ecology |
| 4 | SeaLifeBase | Online | Invertebrate biology, size, ecology |
| 5 | EMODnet Biology | Online | European marine species traits |
| 6 | BVOL | Local CSV | Phytoplankton biovolume and morphology |
| 7 | SpeciesEnriched | Local CSV | MARBEFES-enriched trait compilation |
| 8 | Biotic | Local CSV | Benthic invertebrate traits |
| 9 | Polytraits | Local CSV | Polychaete-specific traits |
| 10 | LinkedTraits | Local CSV | Cross-referenced trait aggregator |
| 11 | TraitBase | Local CSV | General marine trait compilation |
| 12 | User overrides | Session | Manually entered values (highest priority) |

Values from higher-priority sources (later in the list) override earlier ones when
conflicts occur. The full provenance chain is recorded for every cell.

### 5.2 Provenance badges and confidence scores

Every trait value in the results table carries a small coloured badge indicating where
it came from:

| Badge colour | Meaning |
|---|---|
| Green | Directly measured / authoritative source |
| Blue | Online API (WoRMS, FishBase, OBIS) |
| Yellow | Local CSV database |
| Orange | Imputed from phylogenetic neighbours |
| Red | ML prediction (lowest confidence) |
| Purple | User override |

Hover over a badge to see the exact source, retrieval date, and confidence score
(0–1). Confidence scores combine source reliability and the number of corroborating
databases.

### 5.3 Workflow: batch lookup for a full food web

1. Open the **Trait Research** tab.
2. The species list from your loaded food web appears automatically. You can also
   paste a custom list.
3. Select which databases to query (by default all 12 are enabled).
4. Click **Start Lookup**. A progress bar shows per-species status.
5. Review the results in the **Found Traits** sub-tab.
6. Use the **Harmonize** button to convert raw values to standardised MARBEFES
   modalities (e.g. "small / medium / large" for body size).
7. Click **Export** to download a CSV of harmonised traits plus provenance.

> [!TIP]
> For large species lists (>200 species), run the lookup locally rather than on the
> live demo. Online APIs are rate-limited and the shared demo instance may take much
> longer.

### 5.4 Manual trait overrides

If you disagree with a looked-up value or need to enter unpublished data:

1. In the Found Traits table, click the cell you want to change.
2. Type or select the new value.
3. Press Enter. The badge turns purple to indicate a user override.
4. Overrides persist for the session and are included in exports.

### 5.5 Harmonisation

Trait values come in many units and vocabularies. EcoNeTool harmonises them to a
common set of **modalities** defined in `R/config/harmonization_config.R`. For
example, all body length values are binned into *small* (<10 cm), *medium* (10–50 cm),
or *large* (>50 cm). Harmonisation is required before traits can feed into the Food
Web Construction module.

---

## 6. Rpath Integration

The ECOPATH/ECOSIM tab wraps the **Rpath** package (Lucey et al. 2020) to provide
full Ecopath with Ecosim functionality inside EcoNeTool.

### 6.1 Converting an EcoNeTool model to Rpath

1. Load your food web through **Data Import**.
2. Open the **ECOPATH/ECOSIM** tab.
3. Click **Convert Current Model**. EcoNeTool generates the required Rpath parameter
   and diet files from your groups, biomasses, PB/QB ratios, and diet matrix.
4. Review the auto-generated `modelname.params.csv` and `modelname.diet.csv`
   previews.

### 6.2 Mass-balance workflow

1. Click **Run Ecopath** to attempt mass balance.
2. Check the **Ecotrophic Efficiency (EE)** column. Any value > 1 indicates that
   more biomass is being consumed than produced — the model is unbalanced.
3. Adjust biomass, PB, QB, or diet for the problematic groups using the inline
   editor.
4. Re-run until all EE values are <= 1.

> [!TIP]
> Start by reducing diet contributions from over-exploited prey rather than
> increasing prey biomass blindly. Balancing is iterative and often involves domain
> judgement.

### 6.3 Running Ecosim simulations

1. With a balanced Ecopath model loaded, switch to the **Ecosim** sub-tab.
2. Define simulation length (years) and number of time steps.
3. Apply forcing functions: fishing mortality changes, climate drivers, or species
   introductions.
4. Click **Run Ecosim**.
5. View biomass trajectories, catch time series, and summary indicators in the
   results panel.

### 6.4 Interpreting simulation results

- **Biomass trajectories.** Look for equilibrium behaviour under a no-change baseline
  before interpreting perturbation runs.
- **Relative changes.** Compare scenarios by percentage change from baseline rather
  than absolute values.
- **Vulnerability parameters.** Ecosim predictions are sensitive to the
  vulnerability parameter (default = 2). Test alternative values for sensitivity
  analysis.

---

## 7. Metaweb Management

A **metaweb** is a regional reference food web compiled from literature, expert
knowledge, and databases. It contains every species known to occur in a region and
every feeding interaction reported anywhere in that region. Your local food web is
typically a subset of the regional metaweb.

### 7.1 Available regional metawebs

| Region | Coverage | Species | Source |
|---|---|---|---|
| Arctic | Barents Sea, Greenland, Svalbard | ~500 | MARBEFES WP3.2 compilation |
| Atlantic | NE Atlantic shelf seas | ~800 | MARBEFES WP3.2 compilation |
| Baltic | Full Baltic Sea | ~300 | HELCOM / MARBEFES |
| Mediterranean | Mediterranean basin | ~900 | MARBEFES WP3.2 compilation |
| North Sea | Greater North Sea | ~600 | ICES / MARBEFES |

Files live in the `metawebs/` directory. Each region has a species list and an
interaction matrix.

### 7.2 Loading a metaweb

1. Open the **Metaweb Manager** tab.
2. Click **Load Regional Metaweb** and pick a region.
3. EcoNeTool reads the species and interaction files and displays summary stats.

### 7.3 Creating or customising a metaweb

1. Load an existing regional metaweb as a starting point.
2. Use the **Add Species** and **Add Link** buttons to extend it.
3. Click **Save As** to write your custom metaweb back to the `metawebs/` folder as
   CSV files.

### 7.4 Overlaying a metaweb on a local food web

1. Load your local food web through **Data Import**.
2. Load the corresponding regional metaweb.
3. In the **Overlay** sub-tab, click **Compare**. EcoNeTool will:
   - Match your species to metaweb entries (fuzzy matching on scientific names).
   - Flag links present in the metaweb but missing in your local web.
   - Flag links in your local web that are unknown to the metaweb.
4. Review flagged links and decide whether to add, remove, or ignore them.

---

## 8. Spatial Analysis

The Spatial Analysis tab implements the MARBEFES WP3.2 spatial workflow for producing
hexagon-level food web indicators.

### 8.1 EMODnet habitat layer integration

EcoNeTool connects to EMODnet Seabed Habitats WMS services to overlay:

- EUSeaMap broad-scale habitat types
- EUNIS habitat classifications
- Regional BBT boundaries

Layers are drawn on an interactive Leaflet map and can be toggled from the layer
control.

### 8.2 Hexagonal grid aggregation

1. In **Spatial Analysis > 0. Study Area**, draw or upload a polygon defining your
   area of interest.
2. Go to the **Grid** sub-tab and set the hexagon side length (km).
3. Click **Generate Grid**. A honeycomb of hexagons fills the study area.
4. Go to the **Species Assignment** sub-tab. EcoNeTool queries OBIS and local habitat
   layers to assign each species in your food web to the cells where it plausibly
   occurs.
5. In the **Local Networks** sub-tab, click **Extract** to compute a local food web
   for every cell by sub-setting your loaded web to the species present.
6. The **Metrics** sub-tab then calculates connectance, mean trophic level, and
   other indicators per cell and displays them as a choropleth.

### 8.3 Regional filtering and WMS layers

You can restrict the study area by selecting a pre-defined MARBEFES BBT region (e.g.
*Bothnian Bay*, *Adriatic Sea*) from the regional dropdown. This automatically loads
the correct EMODnet WMS layers and sets appropriate map bounds.

> [!NOTE]
> WMS layers require an internet connection. If you are offline, the map still works
> but without habitat overlays.

---

## 9. Exporting Results

Every analysis tab provides export options. The table below summarises what you can
download from where.

| Tab | Export types |
|---|---|
| Food Web Network | PNG, SVG, interactive HTML (visNetwork), GraphML |
| Topological Metrics | CSV (metrics table), PDF (summary report) |
| Biomass Analysis | CSV, PNG plots |
| Energy Fluxes | CSV flux matrix, PNG weighted network |
| Keystoneness Analysis | CSV (MTI matrix, keystoneness index), PNG heatmap |
| Trait Research | CSV with provenance, harmonised CSV, Excel |
| Food Web Construction | CSV interaction matrix, GraphML |
| Spatial Analysis | GeoPackage, GeoJSON, CSV (per-cell metrics) |
| ECOPATH/ECOSIM | Rpath `.params.csv` and `.diet.csv`, Ecosim time series CSV |
| Data Import | RData session save (reload any time) |

### 9.1 Saving a full session

Use **Data Import > Save Session** to write an `.RData` file containing the active
food web, trait data, and metaweb. Reload it later with **Load Session**.

### 9.2 Report generation

Many tabs include a **Generate Report** button that assembles plots and metrics into
an HTML or PDF document (via R Markdown). Reports include figure captions and
methodological notes suitable for supplementary material.

---

## 10. Troubleshooting

### 10.1 Common errors

| Symptom | Likely cause | Fix |
|---|---|---|
| "Operation not allowed without an active reactive context" | Internal app error after a data reload | Refresh the browser page |
| Network plot is empty | No diet matrix loaded | Import a diet matrix or use Food Web Construction |
| Trait lookup returns no results | Species names unmatched in WoRMS | Correct spelling; run the WoRMS name-check first |
| EE > 1 for many groups | Model not balanced | Reduce biomass of top predators or adjust diet |
| Spatial map shows grey tiles | OBIS/EMODnet services offline | Retry later; disable WMS layers temporarily |
| Ecosim run crashes | Parameter out of range | Reduce simulation length or check vulnerability parameter |
| "Cannot open connection" on import | File path contains special characters | Move the file to a plain ASCII path |

### 10.2 API rate limits

The online trait databases impose rate limits:

| API | Approximate limit | Behaviour on limit |
|---|---|---|
| WoRMS | 50 requests / minute | 429 error, EcoNeTool auto-retries with backoff |
| FishBase | ~60 requests / minute | Silent throttling |
| OBIS | 1000 records / request | Paginate automatically |
| SeaLifeBase | ~30 requests / minute | Similar to FishBase |

> [!TIP]
> EcoNeTool caches all successful lookups in a local SQLite database. Repeated
> lookups for the same species are instant. See `R/functions/cache_sqlite.R` for
> details.

### 10.3 Database connection issues

If the `.ewemdb` import fails on Windows, install the
**Microsoft Access Database Engine 2010 Redistributable** (64-bit) from Microsoft's
website. For CSV imports with non-ASCII characters, save your file as **UTF-8** from
Excel or a text editor.

### 10.4 Performance tips for large food webs

- **Close unused tabs.** Each tab holds plots in memory; closing them frees RAM.
- **Disable animations.** In the Food Web Network tab, disable "physics" after the
  layout stabilises.
- **Batch trait lookups offline.** For >500 species, use the local install and let
  the first lookup populate the cache, then reuse.
- **Use sub-setting.** In the Metaweb Manager, sub-set the reference metaweb to your
  species of interest before analysis.
- **Avoid full Ecosim runs on the live demo.** Download Rpath files and run
  simulations in a dedicated R session for reproducibility.

### 10.5 Getting help

- Check the per-tab **Help** buttons (blue circle with a question mark).
- Browse `docs/` in the project repository for topic-specific guides.
- File an issue at <https://github.com/razinkele/EcoNeTool/issues> with a reproducible
  example.

---

## 11. Glossary

| Term | Definition |
|---|---|
| **Connectance** | The fraction of possible trophic links that are actually realised: C = L / S^2, where L is link count and S is species count. A core indicator of food web complexity. |
| **Generality** | Average number of prey per predator. High values indicate generalist consumers. |
| **Vulnerability** | Average number of predators per prey. High values indicate strong top-down control. |
| **Omnivory index** | Variance of prey trophic levels for a given predator. Higher values mean a predator feeds across multiple trophic levels. |
| **Trophic level (TL)** | A species' position in the food web, with primary producers at TL = 1. Computed iteratively from diet composition. |
| **Ecotrophic efficiency (EE)** | Fraction of a species' production that is used within the ecosystem (by predation, fishing, or export). Must be ≤ 1 for a balanced Ecopath model. |
| **Production/Biomass (P/B)** | Annual production divided by average biomass. Equivalent to total mortality in steady state. Typical values range from 0.1 (long-lived predators) to >10 (microzooplankton). |
| **Consumption/Biomass (Q/B)** | Annual consumption divided by average biomass. Indicates how much food a species eats relative to its own mass. |
| **Mixed Trophic Impact (MTI)** | A matrix showing the direct and indirect effects of a small biomass increase in one species on every other species. Used for keystoneness analysis. |
| **Keystoneness** | A composite index quantifying a species' ecosystem impact relative to its biomass. High values indicate keystone species whose loss would strongly restructure the web. |
| **Functional group** | A collection of species sharing similar ecology (diet, habitat, size) that are aggregated into a single node for modelling purposes. |
| **Metaweb** | A regional reference food web containing every known species and every reported trophic interaction for a region. Local food webs are subsets of metawebs. |
| **Modality** | A discrete category used to describe a functional trait (e.g. "deposit feeder", "medium-bodied", "cold-water"). EcoNeTool uses harmonised modalities across all databases. |
| **Harmonisation** | The process of converting trait values from different databases (with different units, vocabularies, and scales) into a common set of modalities. |
| **BBT** | Benthic-pelagic-Biodiversity Testbed — a MARBEFES study site. |
| **EwE** | Ecopath with Ecosim, the de-facto standard for mass-balance marine food web modelling. |
| **Rpath** | R implementation of Ecopath with Ecosim, used internally by EcoNeTool. |
| **EMODnet** | European Marine Observation and Data Network — source of habitat and species layers. |
| **WoRMS** | World Register of Marine Species — authoritative taxonomic reference. |
| **OBIS** | Ocean Biodiversity Information System — global marine species occurrence database. |

---

*EcoNeTool is developed at Klaipeda University (KU) as part of the MARBEFES HORIZON
EUROPE project. For questions, contact the project team or file an issue on GitHub.*
