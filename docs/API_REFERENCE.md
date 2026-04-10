# EcoNeTool API Reference

> Auto-generated from source code — do not edit manually.
> Generated: 2026-04-11 | Version: 1.4.3
> Regenerate: `Rscript scripts/generate_api_reference.R`

## Table of Contents

- [Network Analysis](#network-analysis) (10 functions)
- [Data Import/Export](#data-importexport) (25 functions)
- [Trait Lookup](#trait-lookup) (67 functions)
- [Trait Imputation](#trait-imputation) (18 functions)
- [Spatial Analysis](#spatial-analysis) (22 functions)
- [Taxonomic APIs](#taxonomic-apis) (21 functions)
- [Shiny Modules](#shiny-modules) (30 functions)
- [Configuration](#configuration) (11 functions)
- [Utilities](#utilities) (61 functions)

---

## Network Analysis

### calculate_keystoneness()

Calculate Keystoneness Index

Computes the keystoneness index for each species based on their overall impact on the ecosystem and their relative biomass.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `net` | `—` | An igraph object representing the food web |
| `info` | `—` | Data frame with 'meanB' column for biomass |

**Returns:** A data frame with columns: \describe{   \item{species}{Species name}   \item{overall_effect}{Total impact on the ecosystem (sum of absolute MTI values)}   \item{relative_biomass}{Biomass relative to total ecosystem biomass}   \item{keystoneness}{Keystoneness index (high values = keystone species)}   \item{keystone_status}{Classification: "Keystone", "Dominant", or "Rare"} }

**Source:** `R/functions/keystoneness.R:97`

### calculate_losses()

Calculate metabolic losses for species

Computes species-specific metabolic losses using the allometric equation from metabolic theory of ecology (Brown et al. 2004).

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `info` | `—` | Data frame with columns: bodymasses (body mass in grams), met.types (metabolic type: "invertebrates", "ectotherm vertebrates", or "Other") |
| `temp` | `DEFAULT_TEMPERATURE` | Temperature in degrees Celsius (default = 3.5°C for Gulf of Riga spring) |

**Returns:** Numeric vector of metabolic losses (J/sec) for each species

**Source:** `R/functions/flux_calculations.R:117`

### calculate_trophic_levels()

Calculate Trophic Levels for a Food Web (Iterative Method)

Computes trophic levels using an iterative algorithm. Basal species (no prey) are assigned TL = 1. Consumer species have TL = 1 + mean(TL of prey). The algorithm iterates until convergence or maximum iterations reached.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `net` | `—` | An igraph object representing the food web (directed graph) |
| `max_iter` | `100` | Maximum number of iterations (default: 100) |
| `convergence` | `0.0001` | Convergence threshold (default: 0.0001) |

**Returns:** A numeric vector of trophic levels for each species/node

**Source:** `R/functions/trophic_levels.R:33`

### calculate_trophic_levels_shortpath()

Calculate Trophic Levels Using Shortest-Weighted Path Method

Alternative trophic level calculation using shortest path to basal species. This is the method from the original BalticFW.Rdata.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `net` | `—` | An igraph object representing the food web |

**Returns:** A numeric vector of short-weighted trophic levels (SWTL) for each species

**Source:** `R/functions/trophic_levels.R:109`

### create_foodweb_visnetwork()

Create visNetwork Visualization with Trophic Level Layout

Common function for visualizing food web networks with consistent styling, trophic level-based vertical positioning, and optimized horizontal layout.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `net` | `—` | igraph network object |
| `info` | `—` | data frame with species information (must have: fg, colfg, meanB) |
| `node_size_method` | `"biomass_sqrt"` | How to size nodes: "biomass_sqrt", "biomass_log", "flux", or "fixed" |
| `edge_network` | `NULL` | Optional: alternative igraph network for edges (e.g., flux network) |
| `edge_data` | `NULL` | Optional: data frame with edge attributes (width, value, title, color) |
| `edge_color_by` | `"prey"` | How to color edges: "prey" (source node), "predator" (target node), "fixed", or "default" |
| `fixed_edge_color` | `"#95a5a6"` | Color to use if edge_color_by = "fixed" (default: "#95a5a6") |
| `show_uncertainty` | `FALSE` | Logical. If TRUE, visualize trait prediction uncertainty (default: FALSE) |
| `uncertainty_data` | `NULL` | Data frame with confidence scores. Must have columns: species, overall_confidence |
| `trophic_levels` | `NULL` |  |

**Returns:** visNetwork object

**Source:** `R/functions/network_visualization.R:118`

### get_fluxweb_results()

Calculate energy fluxes using metabolic theory

Computes biomass fluxes between species using the fluxweb package, which applies metabolic theory of ecology. Returns both flux matrix and weighted network.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `net` | `—` | An igraph object representing the food web |
| `info` | `—` | Data frame with columns: meanB (biomass), bodymasses (body mass), met.types (metabolic type), efficiencies (assimilation efficiencies) |
| `temp` | `DEFAULT_TEMPERATURE` | Temperature in degrees Celsius (default = 3.5°C for Gulf of Riga) |
| `flux_conversion` | `FLUX_CONVERSION_FACTOR` | Conversion factor from J/sec to kJ/day (default = 86.4) |

**Returns:** A list containing: \describe{   \item{fluxes}{Matrix of energy fluxes (kJ/day/km²) between species}   \item{netLW}{Weighted igraph object with flux as edge weights}   \item{losses}{Calculated metabolic losses (J/sec) for each species} }

**Source:** `R/functions/flux_calculations.R:195`

### get_node_weighted_indicators()

Calculate node-weighted (quantitative) indicators for a food web

Computes network metrics weighted by node biomass. These metrics account for the relative importance of species based on their biomass.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `net` | `—` | An igraph object representing the food web |
| `info` | `—` | Data frame containing species information with 'meanB' column for biomass |

**Returns:** A list containing: \describe{   \item{nwC}{Node-weighted connectance}   \item{nwG}{Node-weighted generality}   \item{nwV}{Node-weighted vulnerability}   \item{nwTL}{Node-weighted mean trophic level} }

**Source:** `R/functions/topological_metrics.R:60`

### plotfw()

Plot food web with trophic level layout

Creates a visual representation of the food web with species arranged by trophic level on the y-axis.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `net` | `—` | An igraph object representing the food web |
| `col` | `NULL` | Vector of colors for each node (default: rainbow colors) |
| `lab` | `NULL` | Vector of labels for each node (default: vertex names) |
| `size` | `NULL` | Vector of node sizes (default: maxsize/2 for all) |
| `nylevel` | `5` | Number of trophic level bins for y-axis (default: 5) |
| `maxsize` | `10` | Maximum node size (default: 10) |
| `labcex` | `0.01` | Label text size (default: 0.01) |
| `ynum` | `6` | Number of y-axis tick marks (default: 6) |
| `ylab` | `"Trophic Level"` | Y-axis label (default: "Trophic Level") |
| `...` | `—` | Additional arguments passed to plot.igraph() |

**Returns:** Invisibly returns a data frame with node coordinates, sizes, and colors

**Source:** `R/functions/network_visualization.R:31`

### trophiclevels()

Calculate Trophic Levels (Deprecated)

**DEPRECATED:** Use `calculate_trophic_levels()` instead.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `...` | `—` | All parameters passed to `calculate_trophic_levels()` |

**Returns:** Trophic levels vector

**Source:** `R/functions/trophic_levels.R:175`

### trophiclevels_shortpath()

Calculate Trophic Levels Shortpath (Deprecated)

**DEPRECATED:** Use `calculate_trophic_levels_shortpath()` instead.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `...` | `—` | All parameters passed to `calculate_trophic_levels_shortpath()` |

**Returns:** Trophic levels vector

**Source:** `R/functions/trophic_levels.R:186`

---

## Data Import/Export

### add_species_to_metaweb()

Add species to metaweb

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `metaweb` | `—` | Metaweb object |
| `species_id` | `—` | Unique species identifier |
| `species_name` | `—` | Species scientific name |
| `functional_group` | `NA` | Functional group (optional) |
| `traits` | `list()` | Named list of additional traits (optional) |

**Returns:** Updated metaweb object

**Source:** `R/functions/metaweb_core.R:209`

### add_trophic_link()

Add trophic link to metaweb

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `metaweb` | `—` | Metaweb object |
| `predator_id` | `—` | Predator species identifier |
| `prey_id` | `—` | Prey species identifier |
| `quality_code` | `1` | Link quality (1-4) |
| `source` | `"user_added"` | Citation or source of link |
| `notes` | `""` | Additional notes (optional) |

**Returns:** Updated metaweb object

**Source:** `R/functions/metaweb_core.R:291`

### convert_ecobase_to_econetool()

Convert EcoBase Model to EcoNeTool Format

Converts EcoBase model data to EcoNeTool network format

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `model_id` | `—` | Numeric model ID from EcoBase |
| `use_output` | `TRUE` | Logical, if TRUE use output parameters, if FALSE use input parameters |

**Returns:** List with 'net' (igraph) and 'info' (data.frame)

**Source:** `R/functions/ecobase_connection.R:273`

### convert_ecobase_to_econetool_hybrid()

Convert EcoBase Model with Hybrid Approach

Uses Output parameters for species data (balanced) and Input parameters for diet (links) This combines the best of both: balanced parameters + complete trophic structure

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `model_id` | `—` | Numeric model ID from EcoBase |

**Returns:** List with 'net' (igraph) and 'info' (data.frame)

**Source:** `R/functions/ecobase_connection.R:473`

### export_metaweb_csv()

Export Metaweb to CSV Files

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `metaweb` | `—` | Metaweb object |
| `species_file` | `—` | Path for species CSV file |
| `interactions_file` | `—` | Path for interactions CSV file |

**Source:** `R/functions/metaweb_io.R:45`

### export_metaweb_rda()

Export Current Metaweb to RDA File

Exports the current metaweb (network and species information) to an RDA file in the same format as BalticFW.Rdata (Frelat & Kortsch format).

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `net` | `—` | igraph network object |
| `info` | `—` | data frame with species information |
| `file_path` | `—` | character, path where to save the RDA file |

**Returns:** TRUE invisibly if export succeeds

**Source:** `R/functions/metaweb_io.R:150`

### export_metaweb_rds()

Export Metaweb to RDS File

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `metaweb` | `—` | Metaweb object |
| `file` | `—` | Path for RDS file |

**Source:** `R/functions/metaweb_io.R:73`

### get_ecobase_model_input()

Get EcoBase Model Input Parameters

Downloads input parameters for a specific model from EcoBase

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `model_id` | `—` | Numeric model ID from EcoBase |

**Returns:** List of input parameters for the model

**Source:** `R/functions/ecobase_connection.R:88`

### get_ecobase_model_metadata()

Get EcoBase Model Metadata

Extracts metadata (location, time period, author, etc.) from EcoBase model

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `model_id` | `—` | Numeric model ID from EcoBase |

**Returns:** List of metadata fields

**Source:** `R/functions/ecobase_connection.R:184`

### get_ecobase_model_output()

Get EcoBase Model Output Parameters

Downloads output parameters for a specific model from EcoBase

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `model_id` | `—` | Numeric model ID from EcoBase |

**Returns:** List of output parameters for the model

**Source:** `R/functions/ecobase_connection.R:136`

### get_ecobase_models()

Get List of Available EcoBase Models

Retrieves the list of publicly available Ecopath models from EcoBase

**Returns:** data.frame with model information (model ID, name, description, etc.)

**Source:** `R/functions/ecobase_connection.R:40`

### get_link_quality_description()

Get link quality description

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `quality_code` | `—` | Integer 1-4 |

**Returns:** Character description

**Source:** `R/functions/metaweb_core.R:163`

### import_metaweb_csv()

Import Metaweb from CSV Files

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_file` | `—` | Path to species CSV file |
| `interactions_file` | `—` | Path to interactions CSV file |
| `metadata` | `list()` | Optional list of metadata |

**Returns:** Metaweb object

**Source:** `R/functions/metaweb_io.R:8`

### import_metaweb_rds()

Import Metaweb from RDS File

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `file` | `—` | Path to RDS file |

**Returns:** Metaweb object

**Source:** `R/functions/metaweb_io.R:98`

### load_baltic_foodweb()

Load and validate food web data

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `data_file` | `DATA_FILE` | Path to the .Rdata file (default: DATA_FILE from config) |

**Returns:** List with net, info, and color_scheme objects

**Source:** `R/data_loading.R:19`

### merge_metawebs()

Merge two metawebs

Combines species and interactions from two metawebs. Useful for creating regional metawebs from multiple sources.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `metaweb1` | `—` | First metaweb object |
| `metaweb2` | `—` | Second metaweb object |
| `conflict_resolution` | `"keep_higher_quality"` | How to handle duplicate links: "keep_first", "keep_second", "keep_higher_quality", "keep_both" |

**Returns:** Merged metaweb object

**Source:** `R/functions/metaweb_core.R:372`

### metaweb_to_igraph()

Convert metaweb to igraph network

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `metaweb` | `—` | Metaweb object |

**Returns:** igraph object

**Source:** `R/functions/metaweb_core.R:132`

### parse_ecopath_data()

Parse ECOPATH Data from Excel/CSV Files

Reads ECOPATH Basic Estimates and Diet Composition files exported to Excel or CSV format and converts them into network and species info objects compatible with EcoNeTool.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `basic_est_file` | `—` | Path to Basic Estimates file (Excel or CSV) |
| `diet_file` | `—` | Path to Diet Composition file (Excel or CSV) |

**Returns:** List with 'net' (igraph object) and 'info' (data.frame)

**Source:** `R/functions/ecopath/ecopath_csv.R:24`

### print.metaweb()

Print metaweb summary

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `x` | `—` | Metaweb object |
| `...` | `—` | Additional arguments (ignored) |

**Source:** `R/functions/metaweb_core.R:102`

### remove_species_from_metaweb()

Remove species from metaweb

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `metaweb` | `—` | Metaweb object |
| `species_id` | `—` | Species identifier to remove |
| `remove_links` | `TRUE` | Logical, remove associated links? (default TRUE) |

**Returns:** Updated metaweb object

**Source:** `R/functions/metaweb_core.R:261`

### remove_trophic_link()

Remove trophic link from metaweb

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `metaweb` | `—` | Metaweb object |
| `predator_id` | `—` | Predator species identifier |
| `prey_id` | `—` | Prey species identifier |

**Returns:** Updated metaweb object

**Source:** `R/functions/metaweb_core.R:346`

### require_ecobase_packages()

EcoBase Connection Module

This module provides functions to: - List available Ecopath models in EcoBase - Download model input parameters - Download model output parameters - Convert EcoBase models to EcoNeTool format

**Source:** `R/functions/ecobase_connection.R:14`

### summarize_link_quality()

Summarize link quality in metaweb

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `metaweb` | `—` | Metaweb object |

**Returns:** Data frame with quality summary

**Source:** `R/functions/metaweb_core.R:178`

### test_ecobase_connection()

Test EcoBase Connection

Tests connection to EcoBase and retrieves a sample model

**Returns:** Logical, TRUE if connection successful

**Source:** `R/functions/ecobase_connection.R:659`

### validate_metaweb()

Validate a metaweb object

Checks metaweb structure and identifies potential issues like orphan links.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `metaweb` | `—` | Metaweb object to validate |

**Returns:** TRUE if valid, otherwise throws error or warnings

**Source:** `R/functions/metaweb_core.R:62`

---

## Trait Lookup

### apply_size_adjustment()

Apply Ecosystem Profile Size Adjustment

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `size_cm` | `—` | Raw size in cm |

**Returns:** Adjusted size in cm based on active ecosystem profile

**Source:** `R/functions/trait_lookup/harmonization.R:353`

### batch_lookup_parallel()

Batch Lookup Species in Parallel

Looks up multiple species concurrently with progress tracking. Much faster than sequential batch processing.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_list` | `—` | Character vector. Species names to lookup |
| `databases` | `c("worms", "fishbase", "sealifebase")` | Character vector. Databases to query |
| `cache_dir` | `"cache/taxonomy"` | Character. Cache directory |
| `workers` | `2` | Integer. Number of parallel workers for batch (default: 2) |
| `show_progress` | `TRUE` | Logical. Show progress bar (default: TRUE) |

**Returns:** List of trait lookup results

**Source:** `R/functions/parallel_lookup.R:338`

### batch_lookup_traits()

Batch lookup traits for multiple species

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_list` | `—` | Character vector of species names |
| `...` | `—` | Additional arguments passed to lookup_species_traits |

**Returns:** Data frame with all species traits

**Source:** `R/functions/trait_lookup/orchestrator.R:1676`

### benchmark_parallel_speedup()

Compare Serial vs. Parallel Performance

Benchmarks serial and parallel lookup to measure speedup.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `"Gadus morhua"` | Character. Species to test |
| `databases` | `c("worms", "fishbase", "sealifebase")` | Character vector. Databases to query |
| `n_trials` | `3` | Integer. Number of trials to average |

**Returns:** Data frame with performance comparison

**Source:** `R/functions/parallel_lookup.R:424`

### calc_interaction_probability()

Calculate interaction probability between two species

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `consumer_traits` | `—` | Named vector of consumer traits (MS, FS, MB, EP) |
| `resource_traits` | `—` | Named vector of resource traits (MS, MB, EP, PR) |

**Returns:** Numeric probability [0, 1] or NA if interaction impossible

**Source:** `R/functions/trait_foodweb.R:163`

### construct_trait_foodweb()

Construct food web from species trait data

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_data` | `—` | Data frame with columns: species, MS, FS, MB, EP, PR |
| `threshold` | `0.05` | Minimum probability threshold for link (default 0.05) |
| `return_probs` | `FALSE` | If TRUE, return probability matrix instead of binary adjacency |

**Returns:** Adjacency matrix (rows = consumers, columns = resources)

**Source:** `R/functions/trait_foodweb.R:265`

### create_trait_template()

Create a template data frame for trait-based food web

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `n_species` | `5` | Number of species to include in template |

**Returns:** Data frame with example trait data

**Source:** `R/functions/trait_foodweb.R:453`

### extract_phytoplankton_size()

Extract Phytoplankton Size

Calculate representative size from phytoplankton morphological dimensions

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `record` | `—` | BVOL database row |

**Returns:** Numeric size in cm

**Source:** `R/functions/local_trait_databases.R:383`

### extract_primary_feeding()

Extract Primary Feeding Mode from Ontology Traits

Determines the primary (highest-scored) feeding mode from fuzzy ontology data.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `ontology_traits` | `—` | Data frame from lookup_ontology_traits() |

**Returns:** List with primary feeding mode, score, and ontology ID

**Source:** `R/functions/trait_lookup/harmonization.R:15`

### extract_size_from_range()

Extract Size from Text Range

Parse size ranges like "10-20 mm" or "up to 50 cm"

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `record` | `—` | species_enriched database row |

**Returns:** List with min, max, mean in cm

**Source:** `R/functions/local_trait_databases.R:415`

### generate_trait_help_best_practices()

Generate Best Practices HTML content

**Source:** `R/functions/trait_help_content.R:355`

### generate_trait_help_dimensions()

Generate Trait Dimensions HTML content

**Source:** `R/functions/trait_help_content.R:75`

### generate_trait_help_lookup()

Generate Automated Lookup HTML content

**Source:** `R/functions/trait_help_content.R:249`

### generate_trait_help_matrices()

Generate Probability Matrices HTML content

**Source:** `R/functions/trait_help_content.R:171`

### generate_trait_help_quickstart()

Generate Quick Start HTML content

**Source:** `R/functions/trait_help_content.R:5`

### get_config_pattern()

Get Pattern from Configuration

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `pattern_name` | `—` | String name (e.g., "MB1_sessile", "FS1_predator") |
| `pattern_type` | `"mobility"` | Type: "mobility", "foraging", "environmental", "protection" |

**Returns:** Regular expression pattern string, or NULL if not found

**Source:** `R/functions/trait_lookup/harmonization.R:377`

### get_fuzzy_profile()

Get Fuzzy Trait Profile

Extracts all trait modalities with scores for a specific trait category.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `ontology_traits` | `—` | Data frame from lookup_ontology_traits() |
| `trait_category` | `—` | Category: "feeding", "habitat", "life_history" |
| `trait_name` | `NULL` | Specific trait: "feeding_mode", "diet", "mobility", etc. |

**Returns:** Data frame with modalities and scores

**Source:** `R/functions/trait_lookup/harmonization.R:53`

### get_ptdb_data()

Get or Load PTDB Data (Cached)

Loads PTDB data from file on first call, returns cached data on subsequent calls.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `ptdb_file` | `—` | Path to PTDB CSV file |

**Returns:** data.frame with PTDB data, or NULL if file not found

**Source:** `R/functions/trait_lookup/database_lookups.R:613`

### get_trait_descriptions()

Get trait descriptions

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `trait_type` | `NULL` | Type of trait: "MS", "FS", "MB", "EP", or "PR" |

**Returns:** Named character vector of trait descriptions

**Source:** `R/functions/trait_foodweb.R:474`

### harmonize_bvol_traits()

Map BVOL Traits to Harmonized Schema

Convert BVOL raw traits to MS, FS, MB, EP, PR

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `bvol_traits` | `—` | List from parse_bvol_record() |

**Returns:** List with harmonized trait assignments

**Source:** `R/functions/local_trait_databases.R:474`

### harmonize_environmental_position()

Convert habitat/depth information to EP environmental position

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `depth_min` | `NULL` | Minimum depth (m) |
| `depth_max` | `NULL` | Maximum depth (m) |
| `habitat_info` | `NULL` | Character vector with habitat information |
| `taxonomic_info` | `NULL` | Taxonomic classification |

**Returns:** EP code (EP1-EP4)

**Source:** `R/functions/trait_lookup/harmonization.R:619`

### harmonize_foraging_strategy()

Convert feeding mode/type to FS foraging strategy

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `feeding_info` | `NULL` | Character vector with feeding information |
| `trophic_level` | `NULL` | Numeric trophic level (if available) |

**Returns:** FS code (FS0-FS6)

**Source:** `R/functions/trait_lookup/harmonization.R:449`

### harmonize_fuzzy_foraging()

Harmonize Fuzzy Foraging Traits to Categorical FS Class

Converts fuzzy-scored ontology feeding modes to categorical foraging strategy

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `ontology_traits` | `—` | Data frame from ontology traits database |

**Returns:** List with class, confidence, modalities, source

**Source:** `R/functions/trait_lookup/harmonization.R:86`

### harmonize_fuzzy_habitat()

Harmonize Fuzzy Habitat Traits to Categorical EP Class

Converts fuzzy-scored ontology habitat to environmental position class

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `ontology_traits` | `—` | Data frame from ontology traits database |

**Returns:** List with class, confidence, modalities, source

**Source:** `R/functions/trait_lookup/harmonization.R:260`

### harmonize_fuzzy_mobility()

Harmonize Fuzzy Mobility Traits to Categorical MB Class

Converts fuzzy-scored ontology mobility to categorical mobility class

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `ontology_traits` | `—` | Data frame from ontology traits database |

**Returns:** List with class, confidence, modalities, source

**Source:** `R/functions/trait_lookup/harmonization.R:179`

### harmonize_mobility()

Convert mobility information to MB class

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `mobility_info` | `NULL` | Character vector with mobility information |
| `body_shape` | `NULL` | Body shape code (for fish) |
| `taxonomic_info` | `NULL` | Taxonomic classification |

**Returns:** MB code (MB1-MB5)

**Source:** `R/functions/trait_lookup/harmonization.R:524`

### harmonize_protection()

Convert protection information to PR code

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `skeleton_info` | `NULL` | Skeleton/protection information |
| `taxonomic_info` | `NULL` | Taxonomic classification |

**Returns:** PR code (PR0, PR2, PR3, PR5-PR8)

**Source:** `R/functions/trait_lookup/harmonization.R:714`

### harmonize_reproductive_strategy()

Harmonize Reproductive Strategy

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `reproduction_text` | `—` | Character, raw reproduction description |

**Returns:** Character, RS code (RS1-RS4) or NA

**Source:** `R/functions/trait_lookup/harmonization.R:825`

### harmonize_salinity_tolerance()

Harmonize Salinity Tolerance

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `salinity_text` | `—` | Character, raw salinity/habitat description |

**Returns:** Character, ST code (ST1-ST5) or NA

**Source:** `R/functions/trait_lookup/harmonization.R:859`

### harmonize_size_class()

Convert size measurements to MS size class

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `size_cm` | `—` | Maximum body length in cm |

**Returns:** MS code (MS1-MS7)

**Source:** `R/functions/trait_lookup/harmonization.R:404`

### harmonize_species_enriched_traits()

Map Species Enriched Traits to Harmonized Schema

Convert species_enriched raw traits to MS, FS, MB, EP, PR

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_traits` | `—` | List from parse_species_enriched_record() |

**Returns:** List with harmonized trait assignments

**Source:** `R/functions/local_trait_databases.R:552`

### harmonize_temperature_tolerance()

Harmonize Temperature Tolerance

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `temperature_text` | `—` | Character, raw temperature/biogeographic description |

**Returns:** Character, TT code (TT1-TT4) or NA

**Source:** `R/functions/trait_lookup/harmonization.R:842`

### init_parallel_lookup()

Initialize Parallel Processing

Sets up parallel execution plan for database lookups.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `workers` | `4` | Integer. Number of parallel workers (default: 4) |
| `strategy` | `"multisession"` | Character. Execution strategy: "multisession", "multicore", or "sequential" (default: "multisession") |

**Returns:** NULL (invisible). Side effect: sets future plan

**Source:** `R/functions/parallel_lookup.R:53`

### is_rule_enabled()

Check if Taxonomic Rule is Enabled

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `rule_name` | `—` | String name of rule (e.g., "fish_obligate_swimmers") |

**Returns:** Boolean TRUE/FALSE

**Source:** `R/functions/trait_lookup/harmonization.R:339`

### load_bvol_database()

Load BVOL Phytoplankton Database

Loads the phytoplankton biovolume database with caching

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `force_reload` | `FALSE` | Logical. Force reload from file (ignore cache) |

**Returns:** data.frame with phytoplankton trait data

**Source:** `R/functions/local_trait_databases.R:88`

### load_species_enriched_database()

Load Species Enriched Database

Loads the marine invertebrate traits database with caching

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `force_reload` | `FALSE` | Logical. Force reload from file (ignore cache) |

**Returns:** data.frame with marine invertebrate trait data

**Source:** `R/functions/local_trait_databases.R:126`

### lookup_algaebase_traits()

Lookup AlgaeBase database traits

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Scientific name |

**Returns:** List with raw trait data

**Source:** `R/functions/trait_lookup/database_lookups.R:223`

### lookup_arctic_traits()

Lookup Arctic trait database

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Scientific name (character) |
| `csv_file` | `file.path("data", "external_traits", "arctic_traits.csv")` | Path to arctic_traits.csv (default: bundled file) |

**Returns:** list(species, source, success, traits)

**Source:** `R/functions/trait_lookup/csv_trait_databases.R:128`

### lookup_biotic_traits()

Lookup BIOTIC database traits

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Scientific name |
| `biotic_file` | `NULL` | Path to local BIOTIC CSV file (optional) |

**Returns:** List with raw trait data

**Source:** `R/functions/trait_lookup/database_lookups.R:438`

### lookup_blacksea_traits()

Lookup Black Sea trait database

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Scientific name (character) |
| `csv_file` | `file.path("data", "external_traits", "blacksea_traits.csv")` | Path to blacksea_traits.csv (default: bundled file) |

**Returns:** list(species, source, success, traits)

**Source:** `R/functions/trait_lookup/csv_trait_databases.R:88`

### lookup_bvol_traits()

Lookup Species in BVOL Database

Query phytoplankton traits by AphiaID or species name

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `aphia_id` | `NULL` | Numeric. WoRMS AphiaID (preferred) |
| `species_name` | `NULL` | Character. Species name (fallback) |

**Returns:** List with trait data or NULL if not found

**Source:** `R/functions/local_trait_databases.R:172`

### lookup_cefas_traits()

Lookup CEFAS benthic trait database

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Scientific name (character) |
| `csv_file` | `file.path("data", "external_traits", "cefas_benthic_traits.csv")` | Path to cefas_benthic_traits.csv (default: bundled file) |

**Returns:** list(species, source, success, traits)

**Source:** `R/functions/trait_lookup/csv_trait_databases.R:169`

### lookup_coral_traits()

Lookup Coral trait database

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Scientific name (character) |
| `csv_file` | `file.path("data", "external_traits", "coral_traits.csv")` | Path to coral_traits.csv (default: bundled file) |

**Returns:** list(species, source, success, traits)

**Source:** `R/functions/trait_lookup/csv_trait_databases.R:210`

### lookup_emodnet_traits()

Look up Traits from the EMODnet Btrait Package

Uses the optional \pkg{Btrait} GitHub package (karlines/Btrait) to query EMODnet trait data.  Gracefully degrades when the package is not installed.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Character. Scientific name to look up. |

**Returns:** Named list: species, source, success (logical), traits (named list).

**Source:** `R/functions/trait_lookup/api_trait_databases.R:178`

### lookup_fishbase_traits()

Lookup FishBase traits for a species

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Scientific name (character, non-empty) |
| `timeout` | `5` | Timeout in seconds (default: 5s, must be > 0) |

**Returns:** List with raw trait data

**Source:** `R/functions/trait_lookup/database_lookups.R:14`

### lookup_freshwaterecology_traits()

Lookup freshwaterecology.info database traits

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Scientific name |

**Returns:** List with raw trait data

**Source:** `R/functions/trait_lookup/database_lookups.R:360`

### lookup_maredat_traits()

Lookup plankton traits from MAREDAT

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Scientific name |
| `maredat_file` | `NULL` | Path to local MAREDAT file |

**Returns:** List with raw trait data

**Source:** `R/functions/trait_lookup/database_lookups.R:534`

### lookup_obis_traits()

Look up Traits from OBIS Occurrence Records (MoF)

Queries OBIS via \code{robis::occurrence(mof = TRUE)} and extracts body size, biomass, and depth range from Measurement-or-Fact (MoF) fields and occurrence depth columns.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Character. Scientific name to look up. |
| `timeout` | `30` | Numeric. Seconds before aborting the API call (default 30). |

**Returns:** Named list: species, source, success (logical), traits (named list).

**Source:** `R/functions/trait_lookup/api_trait_databases.R:233`

### lookup_offline_traits()

Quick lookup from offline pre-computed trait database

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Scientific name |
| `db_path` | `"cache/offline_traits.db"` | Path to offline SQLite database |

**Returns:** Data frame row with trait codes, or NULL if not found

**Source:** `R/functions/trait_lookup/orchestrator.R:46`

### lookup_ontology_traits()

Lookup Ontology-Based Traits

Retrieves fuzzy-coded trait data from the ontology traits database. Uses standardized ontology vocabularies (ECO, FOODON, ENVO, PCO).

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `NULL` | Scientific name |
| `aphia_id` | `NULL` | WoRMS AphiaID (preferred for matching) |
| `ontology_file` | `"data/ontology_traits.csv"` | Path to ontology traits CSV (default: data/ontology_traits.csv) |

**Returns:** List with ontology trait data

**Source:** `R/functions/trait_lookup/database_lookups.R:1024`

### lookup_pelagic_traits()

Lookup Pelagic trait database

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Scientific name (character) |
| `csv_file` | `file.path("data", "external_traits", "pelagic_traits.csv")` | Path to pelagic_traits.csv (default: bundled file) |

**Returns:** list(species, source, success, traits)

**Source:** `R/functions/trait_lookup/csv_trait_databases.R:248`

### lookup_polytraits()

Look up Traits from the PolyTraits Database

Performs a two-step REST lookup:   Step 1 – resolves species name to a taxonID via the /taxon/ endpoint.   Step 2 – fetches trait records for that taxonID via the /traits/ endpoint.  Parses the \code{trait} and \code{modality} fields of each returned record.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Character. Scientific name to look up. |
| `timeout` | `10` | Numeric. Seconds before aborting each HTTP call (default 10). |

**Returns:** Named list: species, source, success (logical), traits (named list).

**Source:** `R/functions/trait_lookup/api_trait_databases.R:95`

### lookup_ptdb_traits()

Lookup phytoplankton traits from PTDB

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Scientific name |
| `ptdb_file` | `NULL` | Path to local PTDB file |

**Returns:** List with raw trait data

**Source:** `R/functions/trait_lookup/database_lookups.R:638`

### lookup_sealifebase_traits()

Lookup SeaLifeBase database traits

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Scientific name |
| `timeout` | `10` | Timeout in seconds (default: 10s) |

**Returns:** List with raw trait data

**Source:** `R/functions/trait_lookup/database_lookups.R:130`

### lookup_shark_traits()

Lookup SHARK database traits (Swedish Ocean Archives)

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Scientific name |

**Returns:** List with raw trait data

**Source:** `R/functions/trait_lookup/database_lookups.R:290`

### lookup_species_enriched_traits()

Lookup Species in Species Enriched Database

Query marine invertebrate traits by AphiaID or species name

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `aphia_id` | `NULL` | Numeric. WoRMS AphiaID (preferred) |
| `species_name` | `NULL` | Character. Species name (fallback) |

**Returns:** List with trait data or NULL if not found

**Source:** `R/functions/local_trait_databases.R:216`

### lookup_species_parallel()

Lookup Species Traits in Parallel

Executes all database queries concurrently for dramatic speedup. Falls back to serial execution if parallel fails.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Character. Species name to lookup |
| `databases` | `c("worms", "fishbase", "sealifebase", "biotic", "algaebase", "shark", "freshwater", "maredat", "ptdb")` | Character vector. Databases to query (default: all) |
| `cache_dir` | `NULL` | Character. Cache directory (optional) |
| `use_rate_limiting` | `TRUE` | Logical. Use API rate limiters (default: TRUE) |
| `timeout` | `30` | Numeric. Timeout per database query in seconds (default: 30) |

**Returns:** List with results from all databases

**Source:** `R/functions/parallel_lookup.R:170`

### lookup_species_traits()

Automated trait lookup using hierarchical database workflow

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Scientific name |
| `biotic_file` | `NULL` | Path to BIOTIC CSV (optional) |
| `maredat_file` | `NULL` | Path to MAREDAT CSV (optional) |
| `ptdb_file` | `NULL` | Path to PTDB CSV (optional) |
| `cache_dir` | `NULL` | Directory for caching results (optional) |

**Returns:** Data frame with harmonized trait codes

**Source:** `R/functions/trait_lookup/orchestrator.R:104`

### lookup_traitbank()

Look up Traits from EOL TraitBank (Pages API v3)

Uses the Encyclopedia of Life (EOL) search and pages REST APIs to retrieve trait-like data objects (diet, habitat, body mass) for a species.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Character. Scientific name to look up. |
| `timeout` | `15` | Numeric. Seconds before aborting each HTTP call (default 15). |

**Returns:** Named list: species, source, success (logical), traits (named list).

**Source:** `R/functions/trait_lookup/api_trait_databases.R:311`

### lookup_worms_traits()

Use WoRMS for taxonomic information and basic traits

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Scientific name |
| `timeout` | `3` | Timeout in seconds (default: 3s) |

**Returns:** List with taxonomic and habitat information

**Source:** `R/functions/trait_lookup/database_lookups.R:768`

### lookup_worms_traits_api()

Look up Traits from WoRMS Attribute Data

Queries the WoRMS attribute/measurement data for a given AphiaID using the worrms package.  Parses measurementType / measurementValue pairs to extract functional_group, feeding_type, body_size, zone, substratum, environment, salinity and ambi_group.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `NULL` | Character. Species name (used for labelling only). |
| `aphia_id` | `NULL` | Numeric. WoRMS AphiaID (required, must be > 0). |
| `timeout` | `10` | Numeric. Seconds before aborting the API call (default 10). |

**Returns:** Named list: species, source, success (logical), traits (named list).

**Source:** `R/functions/trait_lookup/api_trait_databases.R:32`

### parallel_worker_status()

Check Parallel Worker Status

Returns information about the current parallel processing state.

**Returns:** List with worker status information

**Source:** `R/functions/parallel_lookup.R:118`

### parse_bvol_record()

Parse BVOL Record to Trait Structure

Converts a BVOL database row to standardized trait format

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `record` | `—` | data.frame row from BVOL database |

**Returns:** List with extracted traits

**Source:** `R/functions/local_trait_databases.R:260`

### parse_species_enriched_record()

Parse Species Enriched Record

Converts a species_enriched database row to standardized trait format

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `record` | `—` | data.frame row from species_enriched database |

**Returns:** List with extracted traits

**Source:** `R/functions/local_trait_databases.R:319`

### shutdown_parallel_lookup()

Shutdown Parallel Processing

Closes parallel workers and reverts to sequential execution. Safe to call multiple times.

**Returns:** NULL (invisible)

**Source:** `R/functions/parallel_lookup.R:91`

### trait_foodweb_to_igraph()

Convert trait-based food web to igraph object

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_data` | `—` | Data frame with trait information |
| `threshold` | `0.05` | Minimum probability threshold (default 0.05) |
| `include_probs` | `TRUE` | Include edge weights as probabilities |

**Returns:** igraph object

**Source:** `R/functions/trait_foodweb.R:328`

### validate_trait_data()

Validate trait codes in species data

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_data` | `—` | Data frame with trait columns |

**Returns:** List with valid (TRUE/FALSE) and messages (character vector)

**Source:** `R/functions/trait_foodweb.R:363`

---

## Trait Imputation

### apply_ml_fallback()

Apply ML predictions to harmonized traits

This is the integration point for the existing trait lookup system. Call this after database lookups to fill in missing values with ML predictions.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `harmonized_traits` | `—` | List of harmonized traits (may have NAs) |
| `raw_traits` | `—` | Raw trait data from databases (must include WoRMS taxonomy) |
| `verbose` | `FALSE` | Show prediction details (default: FALSE) |

**Returns:** Updated harmonized_traits list with ML predictions filled in

**Source:** `R/functions/ml_trait_prediction.R:452`

### apply_phylogenetic_imputation()

Apply Phylogenetic Imputation to Fill Missing Traits

Main integration function that combines relative finding and trait imputation. Called by lookup_species_traits() after database lookups and ML predictions.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Target species name |
| `current_traits` | `—` | List with current trait values (NA for missing) |
| `taxonomy` | `—` | WoRMS taxonomy for target species |
| `cache_dir` | `—` | Cache directory |
| `max_distance` | `3` | Maximum taxonomic distance (default: 3 = within order) |
| `min_relatives` | `3` | Minimum relatives needed (default: 3) |
| `min_agreement` | `0.6` | Minimum voting agreement (default: 0.6) |
| `verbose` | `FALSE` | Print detailed information (default: FALSE) |

**Returns:** Updated traits list with imputed values

**Source:** `R/functions/phylogenetic_imputation.R:409`

### batch_predict_traits()

Predict traits for multiple species

Efficiently predicts missing traits for a batch of species.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_list` | `—` | List of species data, each with taxonomic_info and current_traits |
| `progress` | `TRUE` | Show progress bar (default: TRUE) |

**Returns:** List of prediction results for each species

**Source:** `R/functions/ml_trait_prediction.R:303`

### build_taxonomy_tree()

Build Ultrametric Phylogenetic Tree from Taxonomy

Constructs a taxonomic distance matrix from the WoRMS hierarchy and builds an ultrametric phylogenetic tree via UPGMA (hclust average linkage) then converts to a phylo object using ape::as.phylo().

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `taxonomy_df` | `—` | Data frame with columns: species, phylum, class, order, family, genus |

**Returns:** An object of class "phylo", or NULL if construction fails

**Source:** `R/functions/rphylopars_imputation.R:27`

### calculate_taxonomic_distance()

Calculate Taxonomic Distance Between Two Species

Calculates phylogenetic distance based on WoRMS classification hierarchy. Distance increases at each taxonomic rank where species differ.  Rank weights (higher = more distant): - Phylum: 5 points - Class: 4 points - Order: 3 points - Family: 2 points - Genus: 1 point - Species: 0 points (same species)

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `taxonomy1` | `—` | List with: phylum, class, order, family, genus |
| `taxonomy2` | `—` | List with: phylum, class, order, family, genus |

**Returns:** Numeric distance (0 = identical, 5+ = very distant)

**Source:** `R/functions/phylogenetic_imputation.R:58`

### compute_phylo_eigenvectors()

Compute Phylogenetic Eigenvectors from Taxonomy

Constructs a taxonomic distance matrix from WoRMS hierarchy and computes principal coordinates (PCoA) using ape::pcoa().

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `taxonomy_df` | `—` | Data frame with: species, phylum, class, order, family, genus |
| `n_vectors` | `10` | Integer, eigenvectors to return (default 10) |

**Returns:** Matrix with nrow(taxonomy_df) rows and n_vectors columns

**Source:** `R/functions/ml_trait_prediction.R:30`

### find_closest_relatives()

Find Closest Taxonomic Relatives with Complete Trait Data

Searches cached species for the closest phylogenetic relatives that have complete trait information for the target traits.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `target_taxonomy` | `—` | List with WoRMS classification (phylum, class, order, family, genus) |
| `target_traits` | `—` | List with current trait values (MS, FS, MB, EP, PR) |
| `cache_dir` | `—` | Directory containing cached species data |
| `max_distance` | `3` | Maximum taxonomic distance to consider (default: 3 = within family) |
| `min_matches` | `3` | Minimum number of relatives required (default: 3) |
| `traits_needed` | `c("MS", "FS", "MB", "EP", "PR")` | Which traits to look for (default: c("MS", "FS", "MB", "EP", "PR")) |

**Returns:** Data frame with: species, distance, and trait values for each relative

**Source:** `R/functions/phylogenetic_imputation.R:124`

### format_ml_prediction()

Format ML prediction for display

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `prediction` | `—` | Prediction result from predict_trait_ml() |

**Returns:** Formatted string for display

**Source:** `R/functions/ml_trait_prediction.R:417`

### get_ml_model_diagnostics()

Get ML model performance metrics

Returns detailed performance metrics for all trained models.

**Returns:** List of performance metrics, or NULL if models not loaded

**Source:** `R/functions/ml_trait_prediction.R:355`

### get_predictable_traits()

Get list of traits that can be predicted

**Returns:** Character vector of trait names, or empty vector if models not available

**Source:** `R/functions/ml_trait_prediction.R:404`

### impute_traits_from_relatives()

Impute Missing Traits from Phylogenetic Relatives

Uses weighted majority voting to impute missing trait values based on closely related species. Closer relatives receive higher weights.  Weighting scheme: - Same genus (distance 0): weight = 5 - Same family (distance 1-2): weight = 3-4 - Same order (distance 3): weight = 2

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `target_traits` | `—` | List with current trait values (NA for missing) |
| `relatives` | `—` | Data frame from find_closest_relatives() |
| `min_agreement` | `0.6` | Minimum proportion of relatives that must agree (default: 0.6) |

**Returns:** List with imputed trait values and metadata

**Source:** `R/functions/phylogenetic_imputation.R:276`

### impute_with_bhpmf()

Impute Missing Traits Using BHPMF

Runs Bayesian Hierarchical Probabilistic Matrix Factorization gap-filling on a trait matrix with missing values.  BHPMF was removed from CRAN in 2017. Install from Archive: https://cran.r-project.org/src/contrib/Archive/BHPMF/

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `trait_matrix` | `—` | Numeric matrix with species as rows and traits as columns; NAs mark values to be imputed |
| `hierarchy` | `—` | Matrix with hierarchy information (e.g. genus, family, order columns) passed to BHPMF::gap_filling() as hierarchy.info |
| `num_samples` | `1000` | Integer, number of MCMC samples (default 1000) |
| `burn_in` | `200` | Integer, number of burn-in samples (default 200) |

**Returns:** A list with elements: filled (matrix), sd (matrix),   method ("bhpmf"), or NULL on failure / missing package

**Source:** `R/functions/bhpmf_imputation.R:34`

### impute_with_rphylopars()

Impute Missing Traits Using Rphylopars

Uses phylogenetic Brownian motion (via Rphylopars::phylopars()) to impute missing trait values. Requires the Rphylopars package.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `trait_matrix` | `—` | Data frame or matrix with species as rows and traits as columns; NAs mark values to be imputed. Row names should be species names. |
| `taxonomy_df` | `—` | Data frame with columns: species, phylum, class, order, family, genus |

**Returns:** A list with elements: imputed (matrix), variances (matrix),   method ("rphylopars"), or NULL on failure / missing package

**Source:** `R/functions/rphylopars_imputation.R:74`

### load_ml_models()

Load ML models from disk (lazy loading)

Models are loaded once and cached in memory for subsequent predictions.

**Returns:** List containing models and metadata, or NULL if models not found

**Source:** `R/functions/ml_trait_prediction.R:80`

### ml_models_available()

Check if ML models are available

**Returns:** TRUE if models are loaded/available, FALSE otherwise

**Source:** `R/functions/ml_trait_prediction.R:396`

### predict_missing_traits()

Predict all missing traits using ML models

Takes current trait data and predicts any missing values using ML models.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `current_traits` | `—` | List with current trait values (may contain NAs) |
| `taxonomic_info` | `—` | List containing phylum, class, order, family, genus |
| `traits_to_predict` | `c("MS", "FS", "MB", "EP", "PR")` | Vector of trait names to predict (default: all 5 traits) |

**Returns:** List of predictions with metadata for each missing trait

**Source:** `R/functions/ml_trait_prediction.R:261`

### predict_trait_ml()

Predict a single trait using ML model

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `trait_name` | `—` | Name of trait to predict (MS, FS, MB, EP, PR) |
| `taxonomic_info` | `—` | List containing phylum, class, order, family, genus |
| `models_package` | `NULL` | Loaded ML models (from load_ml_models()) |

**Returns:** List with prediction, probability, confidence, source, or NULL if prediction fails

**Source:** `R/functions/ml_trait_prediction.R:171`

### prepare_ml_features()

Prepare taxonomic features for ML prediction

Converts taxonomic information into the format expected by ML models.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `taxonomic_info` | `—` | List containing phylum, class, order, family, genus |

**Returns:** Data frame with feature columns as factors, or NULL if insufficient data

**Source:** `R/functions/ml_trait_prediction.R:132`

---

## Spatial Analysis

### add_habitat_to_species()

Add Habitat Data to Species Data Frame

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_df` | `—` | Data frame with species information |
| `sampling_lon` | `—` | Sampling location longitude |
| `sampling_lat` | `—` | Sampling location latitude |
| `euseamap_layer` | `—` | sf object with habitat data |

**Returns:** Enhanced species data frame with habitat columns

**Source:** `R/functions/emodnet_habitat_utils.R:355`

### aggregate_spatial_metrics()

Aggregate spatial metrics across hexagons

Calculates summary statistics (mean, SD, min, max) for spatial metrics.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `metrics_df` | `—` | Data frame from calculate_spatial_metrics() |
| `metric_cols` | `c("S", "L", "C", "LD")` | Character vector of metric column names to aggregate |

**Returns:** Data frame with summary statistics

**Source:** `R/functions/spatial_analysis.R:465`

### assign_species_to_hexagons()

Assign species occurrences to hexagonal grid

Spatially joins species occurrence data to hexagonal grid cells.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_data` | `—` | Data frame with species occurrences. Must contain: - lon, lat (or x, y) columns for coordinates - species column with species names - Optional: occurrence (1/0 or abundance), biomass |
| `hex_grid` | `—` | sf hexagonal grid object from create_hexagonal_grid() |

**Returns:** Data frame with hex_id, species, and aggregated occurrence/biomass

**Source:** `R/functions/spatial_analysis.R:136`

### calculate_spatial_metrics()

Calculate food web metrics for spatial hexagons

Calculates topological metrics for each local network in a spatial grid. Returns metrics in a format suitable for spatial visualization and analysis.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `local_networks` | `—` | Named list of igraph networks (from extract_local_networks) |
| `hex_grid` | `NULL` | Optional sf hexagonal grid object (for spatial join) |
| `metrics` | `c("S", "L", "C", "LD")` | Character vector of metrics to calculate. Options: - "S" = Species richness - "L" = Number of links - "C" = Connectance - "LD" = Link density - "meanTL" = Mean trophic level - "maxTL" = Maximum trophic level |
| `progress` | `TRUE` | Logical, show progress? (default TRUE) |

**Returns:** Data frame with hex_id and calculated metrics

**Source:** `R/functions/spatial_analysis.R:354`

### clip_habitat_to_study_area()

Clip Habitat Data to Study Area

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `euseamap_layer` | `—` | sf object with EUSeaMap habitat polygons |
| `study_area_boundary` | `—` | sf object with study area polygon(s) |

**Returns:** sf object with clipped habitat data

**Source:** `R/functions/emodnet_habitat_utils.R:493`

### create_spatial_foodweb_data()

Create spatial food web data structure

Creates an S3 object containing all spatial food web analysis components.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `hex_grid` | `—` | sf object with hexagonal grid from create_hexagonal_grid() |
| `hex_species` | `—` | Data frame with species per hexagon |
| `metaweb` | `—` | Metaweb object |
| `local_networks` | `NULL` | List of local igraph networks |
| `metrics` | `NULL` | Data frame with calculated metrics |
| `metadata` | `list()` | List with analysis parameters |

**Returns:** Object of class 'spatial_foodweb_data'

**Source:** `R/functions/spatial_analysis.R:507`

### detect_region_from_coords()

Detect Region from Coordinates

Auto-detects the marine region based on a coordinate point.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `lon` | `—` | Longitude in decimal degrees |
| `lat` | `—` | Latitude in decimal degrees |

**Returns:** Region code string

**Source:** `R/functions/euseamap_regional_config.R:294`

### extract_local_network()

Extract local food web from metaweb for a hexagon

Extracts a local food web by filtering the regional metaweb to include only species present in the specified hexagon. This implements the MARBEFES approach of deriving local networks from metawebs based on co-occurrence.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `metaweb` | `—` | Metaweb object (from Phase 2) |
| `species_list` | `—` | Character vector of species present in hexagon |
| `hexagon_id` | `NA` | Hexagon identifier (for metadata) |

**Returns:** igraph network with local species and their interactions

**Source:** `R/functions/spatial_analysis.R:222`

### extract_local_networks()

Extract local networks for all hexagons (batch processing)

Extracts local food webs for multiple hexagons in batch. Includes progress reporting for large grids.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `metaweb` | `—` | Metaweb object |
| `hex_species` | `—` | Data frame from assign_species_to_hexagons() |
| `hex_grid` | `NULL` | Optional sf hexagonal grid object (not currently used, for future enhancement) |
| `progress` | `TRUE` | Logical, show progress bar? (default TRUE) |

**Returns:** Named list of igraph networks, one per hexagon

**Source:** `R/functions/spatial_analysis.R:298`

### get_bbox_for_region()

Get Bounding Box for Region

Returns the geographic bounding box for a region.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `region` | `—` | Region code ("baltic", "north_sea", "atlantic", "arctic", "mediterranean") |

**Returns:** Numeric vector c(xmin, ymin, xmax, ymax) in WGS84

**Source:** `R/functions/euseamap_regional_config.R:131`

### get_bbt_region_mapping()

Get BBT Region Mapping

Maps each BBT (Broad Belt Transect) polygon to its geographic region for optimized habitat data loading.

**Returns:** Named list mapping BBT names to region codes

**Source:** `R/functions/euseamap_regional_config.R:20`

### get_euseamap_fields()

Get Available Field Names from EUSeaMap Layer

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `euseamap_layer` | `—` | sf object with EUSeaMap data |

**Returns:** Character vector of field names

**Source:** `R/functions/emodnet_habitat_utils.R:427`

### get_habitat_at_location()

Get Habitat at Geographic Location

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `lon` | `—` | Longitude in decimal degrees (EPSG:4326) |
| `lat` | `—` | Latitude in decimal degrees (EPSG:4326) |
| `euseamap_layer` | `—` | sf object with EUSeaMap data |

**Returns:** List with habitat attributes or NULL

**Source:** `R/functions/emodnet_habitat_utils.R:203`

### get_region_for_bbt()

Get Region for BBT

Returns the region code for a given BBT polygon name.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `bbt_name` | `—` | Name of BBT polygon (e.g., "Lithuanian", "North_Sea") |

**Returns:** Region code string ("baltic", "north_sea", "atlantic", "arctic", "mediterranean")

**Source:** `R/functions/euseamap_regional_config.R:108`

### get_regional_bboxes()

Get Regional Bounding Boxes

Returns geographic bounding boxes for each European marine region. Boxes are defined as c(xmin, ymin, xmax, ymax) in WGS84 (EPSG:4326).

**Returns:** Named list of bounding boxes (xmin, ymin, xmax, ymax)

**Source:** `R/functions/euseamap_regional_config.R:53`

### load_euseamap()

Load EUSeaMap Layer

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `path` | `"data/EUSeaMap_2025/EUSeaMap_2025.gdb"` | Path to EUSeaMap file (GeoPackage .gpkg or File Geodatabase .gdb) |
| `layer` | `NULL` | Layer name (for .gdb files, NULL for auto-detect) |
| `bbox` | `c(10, 53, 31, 66)` | Bounding box to filter data (c(xmin, ymin, xmax, ymax) in WGS84) |

**Returns:** sf object with habitat polygons

**Source:** `R/functions/emodnet_habitat_utils.R:56`

### load_regional_euseamap()

Load Regional EUSeaMap Data

Optimized loading function that automatically selects the appropriate regional subset based on BBT selection or custom study area.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `bbt_name` | `NULL` | Name of selected BBT polygon (optional) |
| `custom_bbox` | `NULL` | Custom bounding box c(xmin, ymin, xmax, ymax) (optional) |
| `study_area_sf` | `NULL` | sf object with study area boundary (optional, used to refine bbox) |
| `buffer_degrees` | `1.0` | Buffer (in degrees) to expand study area bbox (default: 1.0) |
| `path` | `"data/EUSeaMap_2025/EUSeaMap_2025.gdb"` | Path to EUSeaMap GeoDatabase (used if regional files not found) |
| `layer` | `NULL` | Layer name (NULL for auto-detect) |
| `regional_dir` | `"data/EUSeaMap_2025/regional"` | Directory containing regional .gpkg files |

**Returns:** sf object with habitat polygons for the region

**Source:** `R/functions/euseamap_regional_config.R:180`

### overlay_habitat_with_grid()

Overlay Habitat with Grid Cells

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `grid_cells` | `—` | sf object with hexagonal grid cells |
| `habitat_layer` | `—` | sf object with habitat polygons (clipped) |

**Returns:** sf object with grid cells enriched with habitat attributes

**Source:** `R/functions/emodnet_habitat_utils.R:568`

### print.spatial_foodweb_data()

Print spatial food web data summary

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `x` | `—` | spatial_foodweb_data object |
| `...` | `—` | Additional arguments (ignored) |

**Source:** `R/functions/spatial_analysis.R:535`

### print_euseamap_summary()

Print EUSeaMap Layer Summary

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `euseamap_layer` | `—` | sf object with EUSeaMap data |

**Source:** `R/functions/emodnet_habitat_utils.R:452`

### print_regional_summary()

Get Regional Summary

Prints a summary of available regions and their coverage.

**Source:** `R/functions/euseamap_regional_config.R:316`

### test_emodnet_integration()

Test EMODnet Integration

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `euseamap_path` | `"data/euseamap/EUSeaMap_Baltic.gpkg"` | Path to EUSeaMap GeoPackage file |
| `test_locations` | `NULL` | Data frame with test locations (lon, lat, expected_habitat) |

**Source:** `R/functions/emodnet_habitat_utils.R:758`

---

## Taxonomic APIs

### assign_functional_group_enhanced()

Enhanced Species Classification with API Fallback

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Character, species name |
| `use_api` | `FALSE` | Logical, whether to use API queries (default: FALSE) |
| `...` | `—` | Additional parameters passed to assign_functional_group |
| `pb` | `NA` |  |
| `indegree` | `NA` |  |
| `outdegree` | `NA` |  |
| `use_topology` | `FALSE` |  |

**Returns:** Character, functional group

**Source:** `R/functions/taxonomic_api_utils.R:1197`

### check_data_quality()

Check Data Quality

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `data_frame` | `—` | Data frame to check |

**Returns:** List with quality check results

**Source:** `R/functions/shark_api_utils.R:490`

### check_shark4r_available()

Check if SHARK4R Package is Available

**Returns:** Logical, TRUE if SHARK4R is installed

**Source:** `R/functions/shark_api_utils.R:18`

### classify_by_taxonomy()

Classify Species by Taxonomic Class

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `taxonomy` | `—` | List with taxonomic information |

**Returns:** Character, functional group

**Source:** `R/functions/taxonomic_api_utils.R:1111`

### classify_habitat_from_depth()

Classify Habitat from Depth Range

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `min_depth` | `—` | Numeric, minimum depth in meters |
| `max_depth` | `—` | Numeric, maximum depth in meters |

**Returns:** Character, habitat classification

**Source:** `R/functions/taxonomic_api_utils.R:752`

### classify_species_api()

Classify Species Using Taxonomic APIs

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Character, species name to classify |
| `functional_group_hint` | `NA` | Character, hint for functional group (default: NA) |
| `use_cache` | `TRUE` | Logical, use cached results (default: TRUE) |
| `cache_dir` | `"cache/taxonomy"` | Character, directory for cache files |
| `geographic_region` | `NULL` |  |
| `progress_callback` | `NULL` |  |

**Returns:** List with functional group and trait data

**Source:** `R/functions/taxonomic_api_utils.R:927`

### clean_species_name()

Clean Species Name for Taxonomic Query

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Character, raw species name |

**Returns:** Character, cleaned species name

**Source:** `R/functions/taxonomic_api_utils.R:882`

### format_shark_results()

Format SHARK Results for Display

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `raw_data` | `—` | Data frame from SHARK query |
| `result_type` | `"environmental"` | Character, type of result ("environmental", "occurrence", "taxonomy") |

**Returns:** Formatted data frame ready for display

**Source:** `R/functions/shark_api_utils.R:527`

### get_available_shark_parameters()

Get Available SHARK Parameters

**Returns:** Character vector of available parameter names

**Source:** `R/functions/shark_api_utils.R:276`

### get_shark_datasets()

Get SHARK Dataset List

**Returns:** Data frame with available datasets

**Source:** `R/functions/shark_api_utils.R:556`

### get_shark_environmental_data()

Get SHARK Environmental Data

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `parameters` | `—` | Character vector, parameter names to retrieve |
| `start_date` | `—` | Date or character, start date (YYYY-MM-DD) |
| `end_date` | `—` | Date or character, end date (YYYY-MM-DD) |
| `bbox` | `NULL` | Named numeric vector with north, south, east, west (optional) |
| `max_records` | `10000` | Numeric, maximum number of records (default: 10000) |

**Returns:** Data frame with environmental data or NULL if error

**Source:** `R/functions/shark_api_utils.R:310`

### get_shark_species_occurrence()

Get SHARK Species Occurrence Data

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Character, species name (scientific or Swedish) |
| `start_date` | `—` | Date or character, start date (YYYY-MM-DD) |
| `end_date` | `—` | Date or character, end date (YYYY-MM-DD) |
| `bbox` | `NULL` | Named numeric vector with north, south, east, west (optional) |
| `max_records` | `5000` | Numeric, maximum number of records (default: 5000) |

**Returns:** Data frame with occurrence data or NULL if error

**Source:** `R/functions/shark_api_utils.R:381`

### query_algaebase()

Query AlgaeBase

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Character, algae species name |
| `use_cache` | `TRUE` | Logical, use cached results (default: TRUE) |
| `cache_dir` | `"cache/shark"` | Character, cache directory (default: "cache/shark") |

**Returns:** List with AlgaeBase taxonomy information or NULL if not found

**Source:** `R/functions/shark_api_utils.R:202`

### query_dyntaxa()

Query Dyntaxa (Swedish Species Taxonomy)

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Character, species name (scientific or common Swedish name) |
| `fuzzy` | `TRUE` | Logical, use fuzzy matching (default: TRUE) |
| `use_cache` | `TRUE` | Logical, use cached results (default: TRUE) |
| `cache_dir` | `"cache/shark"` | Character, cache directory (default: "cache/shark") |

**Returns:** List with taxonomy information or NULL if not found

**Source:** `R/functions/shark_api_utils.R:44`

### query_fishbase()

Query FishBase API for Fish Species Data

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Character, species name to query |
| `geographic_region` | `NULL` | Character, study area region for filtering multiple matches (default: NULL) |
| `progress_callback` | `NULL` | Function, callback for progress updates (default: NULL) |

**Returns:** List with fish trait data or NULL if not found

**Source:** `R/functions/taxonomic_api_utils.R:305`

### query_obis()

Query OBIS for Species Ecological Data

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Character, species name (for logging) |
| `aphia_id` | `NULL` | Numeric, WoRMS AphiaID for the species (required) |

**Returns:** List with ecological data or NULL if not found

**Source:** `R/functions/taxonomic_api_utils.R:660`

### query_shark_worms()

Query WoRMS via SHARK4R

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Character, species name |
| `fuzzy` | `TRUE` | Logical, use fuzzy matching (default: TRUE) |
| `use_cache` | `TRUE` | Logical, use cached results (default: TRUE) |
| `cache_dir` | `"cache/shark"` | Character, cache directory (default: "cache/shark") |

**Returns:** List with WoRMS taxonomy information or NULL if not found

**Source:** `R/functions/shark_api_utils.R:126`

### query_worms()

Query WoRMS API for Species Classification

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Character, species name to query |
| `fuzzy` | `TRUE` | Logical, use fuzzy matching (default: TRUE) |
| `try_vernacular` | `TRUE` | Logical, try vernacular/common name search if scientific name fails (default: TRUE) |

**Returns:** List with taxonomic information or NULL if not found

**Source:** `R/functions/taxonomic_api_utils.R:190`

### query_worms_vernacular()

Query WoRMS API by Vernacular/Common Name

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `common_name` | `—` | Character, common/vernacular name to query |
| `fuzzy` | `TRUE` | Logical, use fuzzy matching with LIKE (default: TRUE) |

**Returns:** List with taxonomic information or NULL if not found

**Source:** `R/functions/taxonomic_api_utils.R:33`

### split_combined_species_names()

Split Combined Species Names

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Character, raw species name |

**Returns:** Character vector, individual species names

**Source:** `R/functions/taxonomic_api_utils.R:809`

### validate_shark_data()

Validate SHARK Format Data

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `data_frame` | `—` | Data frame to validate |

**Returns:** List with validation results

**Source:** `R/functions/shark_api_utils.R:448`

---

## Shiny Modules

### analysis_server()

Analysis Server Module (Flux + Keystoneness)

Handles energy flux analysis and keystoneness analysis including cached reactive computations, flux heatmap, flux network, flux indicators, keystoneness table/plot, MTI heatmap, and keystone summary.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `input` | `—` | Shiny input object |
| `output` | `—` | Shiny output object |
| `session` | `—` | Shiny session object |
| `net_reactive` | `—` | ReactiveVal holding the igraph network |
| `info_reactive` | `—` | ReactiveVal holding the species info data frame |
| `trophic_levels_cached` | `—` | Cached reactive for trophic levels |

**Source:** `R/modules/analysis_server.R:13`

### biomass_ui()

Biomass Analysis Tab UI

Creates the biomass analysis tab with tabbed interface.

**Returns:** A tabItem for biomass analysis

**Source:** `R/ui/biomass_ui.R:6`

### dashboard_server()

Dashboard Server Logic

Renders dynamic value boxes for species count, links, groups, location, period. Uses flat input/output pattern (no moduleServer/NS) to avoid breaking UI references.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `input` | `—` | Shiny input object |
| `output` | `—` | Shiny output object |
| `session` | `—` | Shiny session object |
| `net_reactive` | `—` | reactiveVal for igraph network |
| `info_reactive` | `—` | reactiveVal for species info |
| `metaweb_metadata` | `—` | reactiveVal for metadata |
| `dashboard_trigger` | `—` | reactiveVal for update trigger |

**Source:** `R/modules/dashboard_server.R:14`

### dashboard_ui()

Dashboard Tab UI

Creates the dashboard tab showing overview statistics and quick start guide.

**Returns:** A tabItem for the dashboard

**Source:** `R/ui/dashboard_ui.R:6`

### data_import_server()

@param input Shiny input object

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `input` | `—` | Shiny input object |
| `output` | `—` | Shiny output object |
| `session` | `—` | Shiny session object |
| `net_reactive` | `—` | ReactiveVal holding the igraph network |
| `info_reactive` | `—` | ReactiveVal holding the species info data frame |
| `dashboard_trigger` | `—` | ReactiveVal to trigger dashboard updates |
| `refresh_data_editor` | `—` | Function to refresh data editor tables |
| `diet_matrix_reactive` | `NULL` | ReactiveVal to store the original diet proportion matrix |

**Source:** `R/modules/data_import_server.R:154`

### dataeditor_inline_server()

Internal Data Editor Handlers Server Module

Manages the species info table and network adjacency matrix editors. Returns the refresh_data_editor function for use by other modules.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `input` | `—` | Shiny input object |
| `output` | `—` | Shiny output object |
| `session` | `—` | Shiny session object |
| `net_reactive` | `—` | ReactiveVal holding the igraph network |
| `info_reactive` | `—` | ReactiveVal holding the species info data frame |
| `dashboard_trigger` | `—` | ReactiveVal to trigger dashboard updates |

**Returns:** A list containing the refresh_data_editor function

**Source:** `R/modules/dataeditor_inline_server.R:13`

### dataeditor_server()

Data Editor Module Server

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `id` | `—` | Module ID |
| `net_reactive` | `—` | Reactive value for network (igraph object) |
| `info_reactive` | `—` | Reactive value for species info (data frame) |

**Returns:** List with reactive values:   - species_data: Reactive data frame of species information   - network_matrix: Reactive adjacency matrix

**Source:** `R/modules/dataeditor_module.R:20`

### dataeditor_ui()

Internal Data Editor Tab UI

Creates the data editor tab.

**Returns:** A tabItem for data editor

**Source:** `R/ui/dataeditor_ui.R:6`

### download_server()

Download Handlers and Export Server Module

Handles download handlers for example datasets and metaweb export.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `input` | `—` | Shiny input object |
| `output` | `—` | Shiny output object |
| `session` | `—` | Shiny session object |
| `net_reactive` | `—` | ReactiveVal holding the igraph network |
| `info_reactive` | `—` | ReactiveVal holding the species info data frame |

**Source:** `R/modules/download_server.R:10`

### ecobase_server()

EcoBase Connection Server Module

Handles connecting to EcoBase, browsing models, viewing model details with metadata preview, and importing models into EcoNeTool format.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `input` | `—` | Shiny input object |
| `output` | `—` | Shiny output object |
| `session` | `—` | Shiny session object |
| `net_reactive` | `—` | ReactiveVal holding the igraph network |
| `info_reactive` | `—` | ReactiveVal holding the species info data frame |
| `metaweb_metadata` | `—` | ReactiveVal holding metadata for dashboard |
| `dashboard_trigger` | `—` | ReactiveVal to trigger dashboard updates |
| `refresh_data_editor` | `—` | Function to refresh data editor tables |

**Source:** `R/modules/ecobase_server.R:14`

### ecobase_ui()

EcoBase Connection UI

Creates the EcoBase tab for downloading Ecopath models from EcoBase web service

**Returns:** A tabItem for EcoBase connection

**Source:** `R/ui/ecobase_ui.R:6`

### ecopath_import_server()

ECOPATH Import Server Logic

Handles ECOPATH file parsing (native .ewemdb/.mdb and CSV), metadata preview, and model import workflows. Merges the ECOPATH parser function and native DB import handler.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `input` | `—` | Shiny input |
| `output` | `—` | Shiny output |
| `session` | `—` | Shiny session |
| `net_reactive` | `—` | reactiveVal for network |
| `info_reactive` | `—` | reactiveVal for species info |
| `metaweb_metadata` | `—` | reactiveVal for metadata |
| `dashboard_trigger` | `—` | reactiveVal for dashboard updates |
| `ecopath_import_data` | `—` | reactiveVal for passing data to Rpath module |
| `ecopath_native_status_data` | `—` | reactiveVal for native import status display |
| `plugin_states` | `—` | reactiveVal for plugin states |
| `euseamap_data` | `—` | reactiveVal for EUSeaMap habitat data |
| `current_metaweb` | `—` | reactiveVal for current metaweb |
| `refresh_data_editor` | `—` | function to refresh data editor tables |

**Source:** `R/modules/ecopath_import_server.R:20`

### filter_disconnected()

Remove disconnected (isolated) nodes from a network and its info table.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `net` | `—` | igraph network |
| `info` | `—` | data.frame with a \code{species} column |

**Returns:** List with filtered \code{net}, \code{info}, and character vector   \code{removed} containing names of removed nodes

**Source:** `R/modules/data_import_server.R:130`

### fluxes_ui()

Energy Fluxes Tab UI

Creates the energy fluxes tab.

**Returns:** A tabItem for energy fluxes

**Source:** `R/ui/fluxes_ui.R:6`

### import_server()

Import Module Server

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `id` | `—` | Module ID |
| `net_reactive` | `—` | Reactive value for network (igraph object) |
| `info_reactive` | `—` | Reactive value for species info (data frame) |
| `refresh_data_editor` | `—` | Function to refresh data editor tables |
| `update_dashboard` | `—` | Function to update dashboard metadata |

**Returns:** List with reactive values:   - taxonomic_progress: Progress of taxonomic classification   - taxonomic_report: Results of taxonomic classification   - ecopath_import_data: ECOPATH import data for Rpath module

**Source:** `R/modules/import_module.R:26`

### import_ui()

Data Import Tab UI

Creates the data import tab with options for loading food web data.

**Returns:** A tabItem for data import

**Source:** `R/ui/import_ui.R:6`

### keystoneness_ui()

Keystoneness Analysis Tab UI

Creates the keystoneness analysis tab.

**Returns:** A tabItem for keystoneness analysis

**Source:** `R/ui/keystoneness_ui.R:6`

### metaweb_manager_server()

Metaweb Manager Server Module

Handles regional metaweb loading, custom metaweb import, species/link editing, network visualization, quality analysis, export, and conversion to active network.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `input` | `—` | Shiny input object |
| `output` | `—` | Shiny output object |
| `session` | `—` | Shiny session object |
| `current_metaweb` | `—` | ReactiveVal holding the current metaweb object |
| `net_reactive` | `—` | ReactiveVal holding the igraph network |
| `info_reactive` | `—` | ReactiveVal holding the species info data frame |
| `metaweb_metadata` | `—` | ReactiveVal holding metadata for dashboard |
| `dashboard_trigger` | `—` | ReactiveVal to trigger dashboard updates |

**Source:** `R/modules/metaweb_manager_server.R:15`

### metaweb_server()

Metaweb Module Server

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `id` | `—` | Module ID |
| `net_reactive` | `—` | Reactive value for network (igraph object) |
| `info_reactive` | `—` | Reactive value for species info (data frame) |

**Returns:** List with reactive values:   - current_metaweb: Currently loaded metaweb   - metaweb_metadata: Metadata about the metaweb

**Source:** `R/modules/metaweb_module.R:23`

### metaweb_ui()

Metaweb Manager Tab UI

Creates the metaweb manager tab (Phase 2).

**Returns:** A tabItem for metaweb manager

**Source:** `R/ui/metaweb_ui.R:6`

### network_ui()

Food Web Network Tab UI

Creates the network visualization tab.

**Returns:** A tabItem for network visualization

**Source:** `R/ui/network_ui.R:6`

### parse_adjacency_df()

Parse an adjacency or diet matrix data frame into an igraph network and info table.

Handles two layouts:   1. Square adjacency matrix with matching row/column species names (+ optional info_df)   2. Non-square diet matrix (rows = prey, columns = predators)

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `mat_df` | `—` | Data frame where the first column contains species/prey names and remaining columns are numeric interaction values |
| `info_df` | `NULL` | Optional data frame with species attributes (species, fg, meanB, etc.) |
| `threshold` | `0` | Minimum value to count as a link (default 0, meaning any non-zero) |

**Returns:** List with \code{net} (igraph), \code{info} (data.frame),   \code{weights} (named matrix of original values or NULL if binary),   and \code{matrix_type} ("binary", "proportion", or "percentage")

**Source:** `R/modules/data_import_server.R:21`

### plugin_server()

Plugin Management Server Module

Handles plugin settings UI rendering, saving, and resetting. Uses flat input/output pattern (no moduleServer/NS) to avoid breaking UI references.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `input` | `—` | Shiny input object |
| `output` | `—` | Shiny output object |
| `session` | `—` | Shiny session object |
| `plugin_states` | `—` | reactiveVal holding plugin states (plugin_id => enabled TRUE/FALSE) |

**Source:** `R/modules/plugin_server.R:11`

### rpathModuleUI()

Rpath Module UI

Creates the user interface for ECOPATH/ECOSIM functionality

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `id` | `—` | Character string. The namespace ID for this module instance. |

**Returns:** A tagList containing the module UI elements

**Source:** `R/ui/rpath_ui.R:49`

### shark_server()

SHARK4R Server Module

Swedish ocean archives integration for marine environmental data. Handles taxonomy search (Dyntaxa, WoRMS, AlgaeBase), environmental data queries, species occurrence data, and quality control checks.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `input` | `—` | Shiny input object |
| `output` | `—` | Shiny output object |
| `session` | `—` | Shiny session object |

**Source:** `R/modules/shark_server.R:10`

### shark_ui()

SHARK Data UI

Creates the SHARK4R tab for accessing Swedish marine environmental archives

**Returns:** A tabItem for SHARK data access

**Source:** `R/ui/shark_ui.R:13`

### spatial_server()

Spatial Analysis Server Module

Handles the full spatial analysis workflow: study area upload, BBT polygon selection, hexagonal grid creation, species data upload, EMODnet habitat integration, local network extraction, spatial metrics calculation, leaflet map visualization, and download handlers.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `input` | `—` | Shiny input object |
| `output` | `—` | Shiny output object |
| `session` | `—` | Shiny session object |
| `net_reactive` | `—` | ReactiveVal holding the igraph network |
| `info_reactive` | `—` | ReactiveVal holding the species info data frame |
| `current_metaweb` | `—` | ReactiveVal holding the current metaweb object |
| `euseamap_data` | `—` | ReactiveVal for cached EUSeaMap data (shared with ecopath_import_server) |

**Source:** `R/modules/spatial_server.R:15`

### spatial_ui()

Spatial Analysis Tab UI

Creates the spatial analysis tab (Phase 1).

**Returns:** A tabItem for spatial analysis

**Source:** `R/ui/spatial_ui.R:6`

### topological_ui()

Topological Indicators Tab UI

Creates the topological metrics tab.

**Returns:** A tabItem for topological indicators

**Source:** `R/ui/topological_ui.R:6`

### visualization_server()

Visualization Outputs Server Module

Renders food web visualizations, basal/top species, adjacency heatmap, topological indicators, biomass plots, and biomass network visualization.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `input` | `—` | Shiny input object |
| `output` | `—` | Shiny output object |
| `session` | `—` | Shiny session object |
| `net_reactive` | `—` | ReactiveVal holding the igraph network |
| `info_reactive` | `—` | ReactiveVal holding the species info data frame |
| `trophic_levels_cached` | `—` | Cached reactive for trophic levels |
| `topological_metrics_cached` | `—` | Cached reactive for topological metrics |

**Source:** `R/modules/visualization_server.R:13`

---

## Configuration

### check_plugin_packages()

Check if Required Packages are Available

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `plugin_id` | `—` | Plugin identifier |

**Returns:** Logical

**Source:** `R/config/plugins.R:186`

### get_all_plugins()

Get Flattened Plugin List

**Returns:** Named list of all plugins

**Source:** `R/config/plugins.R:137`

### get_api_key()

Get API key for a service

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `service` | `—` | Character, name of the service (e.g., "algaebase_username") |

**Returns:** Character, API key or empty string if not set

**Source:** `R/config.R:236`

### get_default_plugin_states()

Get Default Plugin States

**Returns:** Named list of plugin_id => enabled status

**Source:** `R/config/plugins.R:202`

### get_plugin()

Get Plugin by ID

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `plugin_id` | `—` | Plugin identifier (e.g., "topological", "spatial_analysis") |

**Returns:** Plugin configuration list or NULL

**Source:** `R/config/plugins.R:155`

### get_plugins_by_category()

Get Plugins by Category

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `category` | `—` | Category name ("core", "analysis", "data", "advanced") |

**Returns:** List of plugins in category

**Source:** `R/config/plugins.R:225`

### get_version()

Get version string

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `format` | `"short"` | Format: "short" (1.1.1), "long" (1.1.1 - Name), "full" (Version 1.1.1 - Name) |

**Returns:** Character, formatted version string

**Source:** `R/config.R:320`

### get_version_html()

Get version info as HTML

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `include_date` | `TRUE` | Include release date |

**Returns:** HTML string

**Source:** `R/config.R:339`

### has_api_key()

Check if API key is configured for a service

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `service` | `—` | Character, name of the service |

**Returns:** Logical, TRUE if key is set and non-empty

**Source:** `R/config.R:249`

### is_plugin_enabled()

Check if Plugin is Enabled

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `plugin_id` | `—` | Plugin identifier |
| `plugin_states` | `—` | Current plugin states (reactive value) |

**Returns:** Logical

**Source:** `R/config/plugins.R:165`

### load_version_info()

Load version information from VERSION file

**Returns:** List with version information

**Source:** `R/config.R:262`

---

## Utilities

### analyze_error_log()

Analyze Error Log

Generates summary statistics from error log for debugging and monitoring.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `log_file` | `"logs/trait_lookup_errors.csv"` | Character. Path to error log (default: "logs/trait_lookup_errors.csv") |
| `recent_days` | `NULL` | Integer. Only analyze errors from last N days (default: all) |

**Returns:** List with error statistics

**Source:** `R/functions/error_logging.R:230`

### analyze_success_log()

Analyze Success Log

Generates performance statistics from success log.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `log_file` | `"logs/trait_lookup_success.csv"` | Character. Path to success log |
| `recent_days` | `NULL` | Integer. Only analyze recent N days |

**Returns:** List with performance statistics

**Source:** `R/functions/error_logging.R:304`

### api_call_with_retry()

Execute API Call with Retry Logic

Wraps an API call with automatic retry and exponential backoff for rate limit errors, network errors, and temporary failures.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `api_call` | `—` | Function. The API call to execute (should return result or NULL) |
| `max_retries` | `3` | Integer. Maximum number of retries (default: 3) |
| `initial_delay` | `1` | Numeric. Initial delay in seconds (default: 1) |
| `backoff_factor` | `2` | Numeric. Multiply delay by this each retry (default: 2) |
| `rate_limiter` | `NULL` | RateLimiter. Optional rate limiter to use |
| `api_name` | `"API"` | Character. Name of API for logging |

**Returns:** Result of api_call, or NULL if all retries failed

**Source:** `R/functions/api_rate_limiter.R:296`

### assign_functional_group()

Assign Functional Group to a Species

Determines functional group based on species name and optional network properties

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `sp_name` | `—` | Character, species/group name |
| `pb` | `NA` | Numeric, production/biomass ratio (optional, for topology-based assignment) |
| `indegree` | `NA` | Numeric, network in-degree (optional, for topology-based assignment) |
| `outdegree` | `NA` | Numeric, network out-degree (optional, for topology-based assignment) |
| `use_topology` | `FALSE` | Logical, whether to use network topology for assignment (default: FALSE) |

**Returns:** Character, functional group name

**Source:** `R/functions/functional_group_utils.R:35`

### assign_functional_groups()

Assign Functional Groups to Multiple Species

Vectorized version of assign_functional_group

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_names` | `—` | Character vector of species names |
| `pb_values` | `NULL` | Numeric vector of P/B ratios (optional) |
| `indegrees` | `NULL` | Numeric vector of in-degrees (optional) |
| `outdegrees` | `NULL` | Numeric vector of out-degrees (optional) |
| `use_topology` | `FALSE` | Logical, whether to use network topology |

**Returns:** Character vector of functional groups

**Source:** `R/functions/functional_group_utils.R:159`

### calculate_all_trait_confidence()

Calculate Confidence for All Traits in a Species Record

Wrapper function to calculate confidence for all 5 traits (MS, FS, MB, EP, PR) given a complete trait record with sources and raw measurements.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `trait_record` | `—` | List. Must contain: MS, FS, MB, EP, PR (trait values), and optionally: MS_source, size_cm, MS_ml_probability, etc. |

**Returns:** List with confidence metadata for all traits.

**Source:** `R/functions/uncertainty_quantification.R:410`

### calculate_threshold_distance()

Calculate Distance to Nearest Size Threshold

Calculates how far a size measurement is from the nearest class boundary. Species close to boundaries (within 10%) have reduced confidence.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `size_cm` | `—` | Numeric. Body size in cm. |
| `size_class` | `—` | Character. Assigned size class (MS1-MS7). |

**Returns:** Numeric. Distance factor between 0 (at boundary) and 1 (far from boundary).

**Source:** `R/functions/uncertainty_quantification.R:101`

### calculate_trait_confidence()

Calculate Confidence for a Single Trait

Calculates probabilistic confidence for a trait prediction based on: - Data source authority - Distance from class boundaries (for size traits) - ML prediction probability (if ML-derived)

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `trait_value` | `—` | Character. The harmonized trait value (e.g., "MS4", "FS1"). |
| `raw_value` | `NA` | Numeric. The raw measurement (e.g., 15.0 cm for size). Optional. |
| `source` | `"Unknown"` | Character. Data source (e.g., "FishBase", "ML_prediction"). |
| `threshold_distance` | `1.0` | Numeric. Distance factor from boundaries (0-1). Default 1.0. |
| `ml_probability` | `NA` | Numeric. ML prediction probability (0-1). Optional. |

**Returns:** List with: confidence, interval_lower, interval_upper, source, notes

**Source:** `R/functions/uncertainty_quantification.R:189`

### clear_logs()

Clear Log Files

Removes all log files. Use with caution!

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `confirm` | `FALSE` | Logical. Must be TRUE to actually clear logs |

**Source:** `R/functions/error_logging.R:550`

### coalesce_values()

Coalesce Multiple Values

Returns the first non-NULL, non-NA value from the arguments. Similar to SQL COALESCE or dplyr::coalesce.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `...` | `—` | Values to check in order |

**Returns:** First valid value, or NULL if none found

**Source:** `R/functions/validation_utils.R:148`

### enable_debug_logging()

Enable Debug Mode

Turns on console logging for all errors and warnings.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `enabled` | `TRUE` | Logical. TRUE to enable, FALSE to disable |

**Source:** `R/functions/error_logging.R:534`

### estimate_body_mass_by_fg()

Estimate Body Mass by Functional Group

Returns typical body mass (in grams) for each functional group

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `fg` | `—` | Character, functional group name |

**Returns:** Numeric, estimated body mass in grams

**Source:** `R/functions/functional_group_utils.R:198`

### estimate_body_mass_enhanced()

Estimate Body Mass with Size and Stage Information

Enhanced body mass estimation that extracts size and ontogenetic stage information from species/group names

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Character, full species/group name (may contain size and stage info) |
| `fg` | `—` | Character, functional group name |

**Returns:** Numeric, estimated body mass in grams

**Source:** `R/functions/functional_group_utils.R:291`

### estimate_efficiency_by_fg()

Estimate Efficiency by Functional Group

Returns energy transfer efficiency for each functional group

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `fg` | `—` | Character, functional group name |

**Returns:** Numeric, efficiency (0-1)

**Source:** `R/functions/functional_group_utils.R:248`

### estimate_metabolic_type_by_fg()

Estimate Metabolic Type by Functional Group

Returns metabolic type for each functional group

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `fg` | `—` | Character, functional group name |

**Returns:** Character, metabolic type

**Source:** `R/functions/functional_group_utils.R:227`

### find_closest_relatives_sql()

Find Closest Relatives Using SQL (90× faster)

Uses SQL queries with indexes for blazing-fast phylogenetic relative finding.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `target_taxonomy` | `—` | List. Target species taxonomy |
| `db_path` | `"cache/taxonomy.db"` | Character. Path to SQLite database |
| `max_distance` | `3` | Integer. Maximum taxonomic distance (default: 3) |
| `min_relatives` | `3` | Integer. Minimum relatives to return (default: 3) |
| `required_traits` | `c("MS", "FS", "MB", "EP", "PR")` | Character vector. Traits that relatives must have |

**Returns:** Data frame with relatives and their traits

**Source:** `R/functions/cache_sqlite.R:433`

### generate_health_report()

Generate System Health Report

Combines error and success logs to create comprehensive health report.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `recent_days` | `7` | Integer. Analyze last N days (default: 7) |
| `output_file` | `NULL` | Character. Optional file to save report (default: console) |

**Returns:** List with health metrics

**Source:** `R/functions/error_logging.R:359`

### generate_recommendations()

Generate Recommendations

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `errors` | `—` |  |
| `successes` | `—` |  |

**Source:** `R/functions/error_logging.R:422`

### get_algaebase_limiter()

Get AlgaeBase Rate Limiter

AlgaeBase API limit: Unknown, use conservative 60/hour

**Source:** `R/functions/api_rate_limiter.R:233`

### get_all_limiter_stats()

Get All Rate Limiter Statistics

Returns statistics for all active rate limiters.

**Returns:** List of rate limiter statistics

**Source:** `R/functions/api_rate_limiter.R:385`

### get_cache_stats()

Get Cache Statistics

Returns statistics about SQLite cache.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `db_path` | `"cache/taxonomy.db"` | Character. Path to SQLite database |

**Returns:** List with cache statistics

**Source:** `R/functions/cache_sqlite.R:598`

### get_database_weight()

Get Database Authority Weight

Returns the authority weight for a given data source.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `source` | `—` | Character string. Data source name. |

**Returns:** Numeric value between 0 and 1. Returns 0.5 if source not found.

**Source:** `R/functions/uncertainty_quantification.R:56`

### get_fishbase_limiter()

Get FishBase Rate Limiter

FishBase API limit: ~100 requests per hour (unofficial, conservative estimate)

**Source:** `R/functions/api_rate_limiter.R:191`

### get_freshwater_limiter()

Get freshwaterecology.info Rate Limiter

freshwaterecology API limit: Unknown, use 100/hour

**Source:** `R/functions/api_rate_limiter.R:247`

### get_functional_group_levels()

Get Functional Group Levels

Returns the standard functional group factor levels

**Returns:** Character vector of functional group names in standard order

**Source:** `R/functions/functional_group_utils.R:187`

### get_sealifebase_limiter()

Get SeaLifeBase Rate Limiter

SeaLifeBase API limit: Same as FishBase (~100/hour)

**Source:** `R/functions/api_rate_limiter.R:205`

### get_shark_limiter()

Get SHARK Rate Limiter

SHARK API limit: Unknown, use 100/hour

**Source:** `R/functions/api_rate_limiter.R:261`

### get_worms_limiter()

Get WoRMS Rate Limiter

WoRMS API limit: ~50 requests per minute (conservative)

**Source:** `R/functions/api_rate_limiter.R:219`

### initialize_cache_db()

Initialize SQLite Cache Database

Creates SQLite database with proper schema and indexes for fast queries.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `db_path` | `"cache/taxonomy.db"` | Character. Path to SQLite database file (default: "cache/taxonomy.db") |
| `overwrite` | `FALSE` | Logical. Overwrite existing database (default: FALSE) |

**Returns:** NULL (invisible). Side effect: creates database file

**Source:** `R/functions/cache_sqlite.R:54`

### is_debug_enabled()

Check if Debug Mode is Enabled

**Returns:** Logical. TRUE if ECO_NT_DEBUG environment variable is "true"

**Source:** `R/functions/logger.R:25`

### is_valid_value()

Check if Value is Valid (not NULL and not NA)

Returns TRUE if the value is not NULL and not NA (for scalars) or has at least one non-NA value (for vectors/lists).

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `x` | `—` | Value to check |

**Returns:** Logical indicating if value is usable

**Source:** `R/functions/validation_utils.R:98`

### load_species_from_cache()

Load Species from SQLite Cache

Retrieves species data from SQLite cache.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_name` | `—` | Character. Species name |
| `db_path` | `"cache/taxonomy.db"` | Character. Path to SQLite database |
| `include_raw` | `FALSE` | Logical. Include raw data blob (default: FALSE) |

**Returns:** List with species data, or NULL if not found

**Source:** `R/functions/cache_sqlite.R:376`

### log_debug()

Log Debug Message

Outputs debug messages only when ECO_NT_DEBUG=true. Use for detailed diagnostic information during development.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `component` | `—` | Character. Component/module name (e.g., "Spatial", "Rpath") |
| `...` | `—` | Additional message parts (concatenated with spaces) |

**Source:** `R/functions/logger.R:42`

### log_debug_details()

Log Detailed Debug Information

Outputs multi-line debug information (only when debug enabled). Useful for logging object properties, CRS info, etc.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `component` | `—` | Character. Component/module name |
| `title` | `—` | Character. Title for the debug block |
| `...` | `—` | Named arguments to display as key: value pairs |

**Source:** `R/functions/logger.R:103`

### log_error()

Log an Error to File

Records errors to a CSV log file for later analysis. Creates log directory if it doesn't exist.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `source` | `—` | Character. Source of error (e.g., "FishBase", "ML_prediction") |
| `species` | `—` | Character. Species name being looked up |
| `error_message` | `—` | Character. Error message from tryCatch |
| `error_type` | `"lookup_failure"` | Character. Type of error (default: "lookup_failure") |
| `additional_info` | `NULL` | List. Additional context (optional) |

**Returns:** NULL (invisible). Side effect: writes to log file

**Source:** `R/functions/error_logging.R:50`

### log_info()

Log Info Message

Outputs informational messages. Always shown (not gated by debug flag). Use for important status updates the user should see.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `component` | `—` | Character. Component/module name |
| `...` | `—` | Additional message parts |

**Source:** `R/functions/logger.R:62`

### log_success()

Log Successful Lookup

Records successful lookups for performance tracking and usage statistics.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species` | `—` | Character. Species name |
| `sources_used` | `—` | Character vector. Data sources that provided data |
| `lookup_time_ms` | `—` | Numeric. Time taken in milliseconds |
| `traits_found` | `5` | Integer. Number of traits successfully retrieved |

**Returns:** NULL (invisible)

**Source:** `R/functions/error_logging.R:179`

### log_warn()

Log Warning Message

Outputs warning messages for non-fatal issues. Always shown (not gated by debug flag).

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `component` | `—` | Character. Component/module name |
| `...` | `—` | Additional message parts |

**Source:** `R/functions/logger.R:80`

### log_warning()

Log a Warning

Records warnings (non-fatal issues) to a separate log file.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `source` | `—` | Character. Source of warning |
| `species` | `—` | Character. Species name |
| `warning_message` | `—` | Character. Warning message |
| `warning_type` | `"data_quality"` | Character. Type of warning (default: "data_quality") |

**Returns:** NULL (invisible)

**Source:** `R/functions/error_logging.R:131`

### map_confidence_to_border()

Map Confidence to Border Width

Converts confidence score to node border width for visualization. Lower confidence = thicker border (more uncertainty).

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `confidence` | `—` | Numeric vector. Confidence scores (0-1). |
| `min_width` | `1` | Numeric. Minimum border width. Default 1. |
| `max_width` | `5` | Numeric. Maximum border width. Default 5. |

**Returns:** Numeric vector. Border widths.

**Source:** `R/functions/uncertainty_quantification.R:372`

### map_confidence_to_opacity()

Map Confidence to Edge Opacity

Converts edge confidence to opacity for visualization. Lower confidence = more transparent (uncertain interactions).

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `confidence` | `—` | Numeric vector. Confidence scores (0-1). |
| `min_opacity` | `0.3` | Numeric. Minimum opacity. Default 0.3. |
| `max_opacity` | `1.0` | Numeric. Maximum opacity. Default 1.0. |

**Returns:** Numeric vector. Opacity values.

**Source:** `R/functions/uncertainty_quantification.R:387`

### map_confidence_to_size()

Map Confidence to Node Size

Converts confidence score to node size for network visualization. Higher confidence = larger nodes.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `confidence` | `—` | Numeric vector. Confidence scores (0-1). |
| `base_size` | `10` | Numeric. Base node size. Default 10. |
| `scale_factor` | `30` | Numeric. Scaling factor. Default 30. |

**Returns:** Numeric vector. Node sizes.

**Source:** `R/functions/uncertainty_quantification.R:357`

### migrate_offline_schema()

Migrate offline trait DB schema to current version

Adds new columns for expanded trait categories without losing existing data. Safe to call multiple times.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `con` | `—` | DBI connection to the offline_traits.db |

**Returns:** invisible(NULL)

**Source:** `R/functions/cache_sqlite.R:745`

### migrate_rds_to_sqlite()

Migrate File Cache to SQLite

Converts existing .rds file cache to SQLite database.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `rds_cache_dir` | `"cache/taxonomy"` | Character. Directory with .rds files |
| `db_path` | `"cache/taxonomy.db"` | Character. Path to SQLite database |
| `batch_size` | `100` | Integer. Number of species per transaction (default: 100) |

**Returns:** NULL (invisible)

**Source:** `R/functions/cache_sqlite.R:514`

### print_all_limiter_stats()

Print All Rate Limiter Statistics

**Source:** `R/functions/api_rate_limiter.R:410`

### print_cache_stats()

Print Cache Statistics

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `db_path` | `"cache/taxonomy.db"` |  |

**Source:** `R/functions/cache_sqlite.R:667`

### print_health_report()

Print Health Report to Console

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `report` | `—` |  |

**Source:** `R/functions/error_logging.R:464`

### propagate_uncertainty()

Propagate Uncertainty Through Food Web Edges

Calculates confidence for food web interactions based on the confidence of all involved traits (size, foraging, mobility, habitat, protection). Uses geometric mean to combine trait confidences.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_traits` | `—` | Data frame. Must have columns: species, MS_confidence, FS_confidence, MB_confidence, EP_confidence, PR_confidence. |
| `adjacency_matrix` | `NULL` | Matrix or data frame. Food web adjacency matrix with predator rows and prey columns. Optional. |

**Returns:** Data frame with edge confidence scores, or species confidence summary   if adjacency_matrix not provided.

**Source:** `R/functions/uncertainty_quantification.R:265`

### reset_all_limiters()

Reset All Rate Limiters

Clears all rate limiter request histories.

**Source:** `R/functions/api_rate_limiter.R:446`

### safe_get()

Safely Get Value with Default

Extracts a value from a list/data.frame column, returning default if the value is NULL, NA, or the column doesn't exist.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `data` | `—` | List or data.frame to extract from |
| `key` | `—` | Column/element name |
| `default` | `NULL` | Default value if extraction fails (default: NULL) |

**Returns:** Extracted value or default

**Source:** `R/functions/validation_utils.R:121`

### save_species_to_cache()

Save Species to SQLite Cache

Stores species trait data in SQLite cache with proper indexing.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `species_data` | `—` | List. Species data from lookup_species_traits() |
| `db_path` | `"cache/taxonomy.db"` | Character. Path to SQLite database |
| `update_if_exists` | `TRUE` | Logical. Update if species already cached (default: TRUE) |

**Returns:** NULL (invisible)

**Source:** `R/functions/cache_sqlite.R:219`

### set_debug_mode()

Enable Debug Logging at Runtime

Convenience function to enable/disable debug logging.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `enabled` | `TRUE` | Logical. TRUE to enable, FALSE to disable |

**Source:** `R/functions/logger.R:129`

### validate_bbox()

Validate Bounding Box

Checks if a bounding box has valid coordinates.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `xmin` | `—` | Minimum x coordinate (longitude) |
| `ymin` | `—` | Minimum y coordinate (latitude) |
| `xmax` | `—` | Maximum x coordinate (longitude) |
| `ymax` | `—` | Maximum y coordinate (latitude) |

**Returns:** TRUE invisibly if bbox is valid

**Source:** `R/functions/validation_utils.R:334`

### validate_dataframe()

Validate Data Frame with Required Columns

Checks if a data frame has all required columns.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `df` | `—` | Data frame to validate |
| `required_cols` | `—` | Character vector of required column names |
| `df_name` | `"Data frame"` | Optional name of data frame for error message (default: "Data frame") |

**Returns:** TRUE invisibly if all columns are present

**Source:** `R/functions/validation_utils.R:267`

### validate_file_exists()

Validate File Exists

Checks if a file exists and is readable.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `file_path` | `—` | Character string with file path |
| `param_name` | `"file"` | Optional parameter name for error message |

**Returns:** TRUE invisibly if file exists

**Source:** `R/functions/validation_utils.R:446`

### validate_network()

Validate igraph Network Object

Checks if an object is a valid igraph network.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `net` | `—` | Network object to validate |
| `require_directed` | `TRUE` | Logical, whether network must be directed (default: TRUE) |
| `min_vertices` | `1` | Integer, minimum number of vertices required (default: 1) |

**Returns:** TRUE invisibly if network is valid

**Source:** `R/functions/validation_utils.R:298`

### validate_numeric_range()

Validate Numeric Range

Checks if a numeric value is within a specified range.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `value` | `—` | Numeric value to check |
| `param_name` | `—` | Character string with parameter name |
| `min` | `-Inf` | Minimum allowed value (inclusive) |
| `max` | `Inf` | Maximum allowed value (inclusive) |
| `allow_na` | `FALSE` | Logical, whether NA is allowed (default: FALSE) |

**Returns:** TRUE invisibly if value is valid

**Source:** `R/functions/validation_utils.R:394`

### validate_package()

Validate Required Package

Checks if a package is installed and stops with helpful error message if not.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `pkg_name` | `—` | Character string with package name |
| `purpose` | `NULL` | Optional character string describing what the package is needed for |

**Returns:** TRUE invisibly if package is available

**Source:** `R/functions/validation_utils.R:169`

### validate_packages()

Validate Multiple Required Packages

Checks if multiple packages are installed and stops with helpful error message if any are missing.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `pkg_names` | `—` | Character vector of package names |
| `purpose` | `NULL` | Optional character string describing what the packages are needed for |

**Returns:** TRUE invisibly if all packages are available

**Source:** `R/functions/validation_utils.R:198`

### validate_parameter()

Validate Required Parameter

Checks if a parameter is provided and not NULL.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `param` | `—` | The parameter value to check |
| `param_name` | `—` | Character string with parameter name (for error message) |
| `allow_na` | `FALSE` | Logical, whether NA values are allowed (default: FALSE) |

**Returns:** TRUE invisibly if parameter is valid

**Source:** `R/functions/validation_utils.R:234`

### with_timeout()

Execute Expression with Timeout

Executes an expression with a timeout limit. This is the single canonical implementation used across the entire codebase.

**Parameters:**

| Name | Default | Description |
|------|---------|-------------|
| `expr` | `—` | Expression to evaluate |
| `timeout` | `10` | Timeout in seconds (default: 10) |
| `on_timeout` | `NULL` | Value to return on timeout (default: NULL) |
| `verbose` | `FALSE` | Logical, whether to print timeout messages (default: FALSE) |

**Returns:** Result of expr, or on_timeout value if timeout occurs

**Source:** `R/functions/validation_utils.R:54`

---

