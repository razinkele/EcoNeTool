# Trait Knowledge Base Expansion — Design Specification

**Date:** 2026-04-04
**Project:** EcoNeTool (MARBEFES HORIZON EUROPE)
**Scope:** Comprehensive upgrade to the trait system across 4 layers

---

## Goal

Fix data quality issues in the existing trait system, expand from 5 to 8 trait categories with 10 new external databases, add ML gap-filling with uncertainty reporting, and expose all capabilities in the UI.

## Architecture

4 layers, implemented in order (each depends on the previous):

```
Layer 1: DATA QUALITY FIXES      — Fix broken foundations
Layer 2: TRAIT DATABASE EXPANSION — Add data sources + new trait categories
Layer 3: ML GAP-FILLING           — Fill gaps with 3 methods + uncertainty
Layer 4: UI/UX IMPROVEMENTS       — Expose everything to users
```

---

## Layer 1: Data Quality Fixes

### 1.1 EP (Environmental Position) Unification

Canonical scheme (ecological water-column-to-sediment gradient):

| Code | Meaning | Gradient Position |
|---|---|---|
| EP1 | Pelagic | Water column |
| EP2 | Benthopelagic | Near bottom, swims |
| EP3 | Epibenthic | On the seabed surface |
| EP4 | Endobenthic/Infaunal | Within sediment |

Updated in ALL 4 locations:
- `R/config/harmonization_config.R` — pattern definitions (lines 39-45)
- `R/functions/trait_lookup/harmonization.R` — `harmonize_fuzzy_habitat()` (lines 260-328)
- `R/functions/trait_lookup/harmonization.R` — `harmonize_environmental_position()` (lines 625-704)
- `R/functions/local_trait_databases.R` — `harmonize_species_enriched_traits()` (lines 609-626)

### 1.2 FS3 Conflict Resolution

- FS3 = **Omnivore** (matching config and orchestrator labels)
- Xylophagous (wood boring) gets new code **FS7**
- Update `harmonize_fuzzy_foraging()` line 130-132: map xylophagous to FS7
- Add FS7 pattern to `harmonization_config.R`

### 1.3 Fix 8 Disabled Taxonomic Rules

Add these keys to `HARMONIZATION_CONFIG$taxonomic_rules` in `harmonization_config.R`:

```r
taxonomic_rules = list(
  # existing keys...
  bivalves_sessile = TRUE,
  cnidarians_sessile = TRUE,
  phytoplankton_pelagic = TRUE,
  infaunal_bivalves = TRUE,
  bivalves_hard_shell = TRUE,
  gastropods_hard_shell = TRUE,
  crustaceans_exoskeleton = TRUE,
  echinoderms_calcium_plates = TRUE
)
```

### 1.4 Taxonomic Routing Fixes

In `R/functions/trait_lookup/orchestrator.R`, update smart routing:

| Group | Current Behavior | Fix |
|---|---|---|
| Cyanobacteria (Myzozoa/Dinophyceae) | Falls to ambiguous path | Add Myzozoa to phytoplankton phylum list |
| Macroalgae (Phaeophyceae, Rhodophyta) | Routed to BVOL/PTDB (unicellular) | Add macroalgae route → AlgaeBase + WoRMS Traits |
| Marine mammals (Mammalia) | No explicit route | Add Mammalia → SeaLifeBase + WoRMS Traits |
| Seabirds (Aves) | No explicit route | Add Aves → WoRMS Traits |
| Tunicata (Urochordata) | Falls to ambiguous path | Add Tunicata → SeaLifeBase |
| SHARK routing | Queries oceanographic data | Fix to query biological trait data or remove from trait pipeline |

### 1.5 Pre-existing Cache Bugs

**`cache_sqlite.R:465`** — SQL alias `distance` in WHERE clause:
```sql
-- Broken:
SELECT ..., CASE WHEN genus = ? THEN 0 ... END as distance
FROM species WHERE distance <= ?

-- Fix: wrap in subquery
SELECT * FROM (
  SELECT ..., CASE WHEN genus = ? THEN 0 ... END as distance
  FROM species
) sub WHERE distance <= ?
```

**`cache_sqlite.R:390-393`** — Inverted `include_raw` query logic:
- `include_raw = FALSE` currently adds `AND raw_data IS NOT NULL` (wrong)
- `include_raw = TRUE` should have that constraint
- Swap the two query branches

---

## Layer 2: Trait Database Expansion

### 2.1 Ten New Database Integrations

Each gets a new `lookup_<db>_traits()` function in `R/functions/trait_lookup/database_lookups.R`:

| # | Database | Access Method | Format |
|---|---|---|---|
| 1 | WoRMS Traits API | `worrms::wm_attr_data(AphiaID)` | R list |
| 2 | PolyTraits | REST: `GET /traits/{id}/json/` | JSON |
| 3 | Black Sea Traits DB | Local CSV in `data/external_traits/` | Fuzzy-coded CSV |
| 4 | Arctic Traits DB | Local CSV in `data/external_traits/` | Fuzzy-coded CSV |
| 5 | NW Europe Benthic (Cefas) | Local CSV in `data/external_traits/` | Standard CSV |
| 6 | EMODnet Btrait | R package `Btrait` (GitHub) | R data frames |
| 7 | OBIS MoF | `robis::occurrence(mof=TRUE)` | Darwin Core MoF |
| 8 | TraitBank (EOL) | `traits::traitbank()` | R list |
| 9 | Coral Trait DB | Local CSV in `data/external_traits/` | Standard CSV |
| 10 | Pelagic Trait DB | Local CSV in `data/external_traits/` | Standard CSV |

Bundled CSVs (databases 3, 4, 5, 9, 10) stored in `data/external_traits/`.

Orchestrator routing additions:
- Polychaeta → PolyTraits (before BIOTIC)
- Arctic species → Arctic Traits DB
- Black Sea species → Black Sea Traits DB
- Corals (Anthozoa) → Coral Trait DB
- Pelagic fish/inverts → Pelagic Trait DB
- All species → WoRMS Traits (early in pipeline, after WoRMS taxonomy)
- Benthos fallback → Cefas NW Europe + EMODnet Btrait
- Broad fallback → OBIS MoF + TraitBank

### 2.2 Extract 8 Discarded Traits

| Trait | Source | New Column |
|---|---|---|
| Trophic level (numeric) | FishBase, MAREDAT | `trophic_level` |
| Depth range | FishBase, WoRMS | `depth_min`, `depth_max` |
| HAB flag | PTDB | `is_hab` |
| Longevity | BIOTIC, Cefas | `longevity_years` |
| Growth rate | SpeciesEnriched | `growth_rate` |
| Body shape | FishBase | `body_shape` |
| Phytoplankton motility | PTDB | `phyto_motility` |
| Phytoplankton growth form | PTDB | `phyto_growth_form` |

These are added to the orchestrator result template and the offline DB schema.

### 2.3 Three New Trait Categories

**RS — Reproductive Strategy:**
| Code | Meaning |
|---|---|
| RS1 | Broadcast spawner |
| RS2 | Brooder |
| RS3 | Budding/fission |
| RS4 | Mixed/variable |

Sources: PolyTraits, Cefas, Coral Trait DB, TraitBank
New function: `harmonize_reproductive_strategy()` in harmonization.R

**TT — Temperature Tolerance:**
| Code | Meaning |
|---|---|
| TT1 | Cold stenothermal |
| TT2 | Cold eurythermal |
| TT3 | Warm eurythermal |
| TT4 | Warm stenothermal |

Sources: Coral Trait DB, Arctic Traits, WoRMS Traits, OBIS
New function: `harmonize_temperature_tolerance()` in harmonization.R

**ST — Salinity Tolerance:**
| Code | Meaning |
|---|---|
| ST1 | Freshwater (<0.5 PSU) |
| ST2 | Oligohaline (0.5-5 PSU) |
| ST3 | Mesohaline (5-18 PSU) |
| ST4 | Polyhaline (18-30 PSU) |
| ST5 | Euhaline (>30 PSU) |

Sources: BIOTIC, freshwaterecology.info, Arctic Traits
New function: `harmonize_salinity_tolerance()` in harmonization.R

All 3 get:
- Pattern definitions in `harmonization_config.R`
- Harmonization functions in `harmonization.R`
- Output columns in orchestrator result template
- Entries in offline DB schema

---

## Layer 3: ML Gap-Filling

### 3.1 Enhanced Random Forest

Modify `R/functions/ml_trait_prediction.R`:
- New function `compute_phylo_eigenvectors(taxonomy_df)`: constructs taxonomy tree from WoRMS hierarchy, computes PCoA of taxonomic distance matrix via `ape::pcoa()`, returns top 10 eigenvectors
- Update `train_trait_rf()`: add eigenvectors as features alongside existing taxonomic dummies
- Add OOB prediction intervals to output (existing `randomForest` supports this)

### 3.2 Rphylopars Integration

New file: `R/functions/rphylopars_imputation.R`
- `impute_with_rphylopars(trait_matrix, taxonomy_df)`: builds ultrametric tree from WoRMS taxonomy via `ape::chronoMPL()`, calls `Rphylopars::phylopars()` for joint estimation
- Returns imputed values with posterior variances
- Used when: single-species lookup has missing traits AND phylogenetic signal exists (Pagel's lambda > 0.3)
- Dependencies: `Rphylopars`, `ape` (both CRAN)

### 3.3 BHPMF Integration

New file: `R/functions/bhpmf_imputation.R`
- `impute_with_bhpmf(trait_matrix, taxonomy_df)`: implements hierarchical probabilistic matrix factorization (Schrodt et al. 2015)
- Gibbs sampler: 1000 iterations, 200 burn-in, taxonomy-informed priors
- Returns gap-filled matrix + 95% credible intervals per cell
- Used ONLY during offline DB rebuild (too slow for real-time)
- Dependencies: `Matrix`, `MCMCpack` or custom Gibbs implementation

### 3.4 Method Selection Logic

In orchestrator, after database lookups:
```
1. Database lookup (always first)
2. If traits missing → check phylogenetic signal (Pagel's lambda)
3. If lambda > 0.3 → Rphylopars
4. If lambda <= 0.3 or Rphylopars fails → Enhanced RF
5. BHPMF runs separately during offline DB rebuild
```

### 3.5 Unified Uncertainty Reporting

New output columns for ALL 8 trait categories:
- `{trait}_confidence` (0-1 score): `MS_confidence`, `FS_confidence`, `MB_confidence`, `EP_confidence`, `PR_confidence`, `RS_confidence`, `TT_confidence`, `ST_confidence`
- `imputation_method`: "observed", "rf_predicted", "phylopars", "bhpmf"
- `imputation_ci_lower`, `imputation_ci_upper`: per-trait confidence intervals when imputed

---

## Layer 4: UI/UX Improvements

### 4.1 Per-Trait Confidence Scores

In trait research results DT table:
- Add 8 confidence columns color-coded: green (>0.8), yellow (0.5-0.8), red (<0.5)
- Add `imputation_method` column
- Tooltip on hover showing CI for imputed values
- Files: `R/modules/trait_research_server.R`, `R/ui/trait_research_ui.R`

### 4.2 Ecosystem Profile Selector

Dropdown in Harmonization Settings tab with 7 profiles:
- Temperate (North Sea) — default
- Mediterranean
- Atlantic NE
- Arctic/Nordic
- Deep Sea
- Baltic
- Black Sea

Changes `HARMONIZATION_CONFIG$active_profile` which adjusts size thresholds.
Files: `R/ui/harmonization_settings_ui.R`, `R/modules/harmonization_settings_server.R`

### 4.3 Offline DB Management Panel

New box in Trait Research tab showing:
- Species count, build date, age (days), staleness warning
- "Rebuild Database" button with progress bar
- "View Contents" expandable DT table
Files: `R/ui/trait_research_ui.R`, `R/modules/trait_research_server.R`

### 4.4 Fuzzy Trait Profile Visualization

Radar/spider chart (plotly) showing full fuzzy profile for looked-up species:
- All modality scores visible (not just dominant class)
- Example: Mytilus edulis → filter_feeder:3, suspension_feeder:2, deposit_feeder:0
Files: `R/modules/trait_research_server.R`

### 4.5 API Key Configuration UI

Modal from Settings gear icon:
- Fields for AlgaeBase username/password, freshwaterecology.info API key
- Saves to `config/api_keys.R` (gitignored)
- Status indicators (green check / red X)
Files: `R/modules/plugin_server.R`

---

## Testing Strategy

- Each new database lookup function gets testthat tests with mocked responses
- EP unification gets before/after tests (same species, verify code consistency)
- ML methods get tests with known small datasets and expected outputs
- UI changes tested via Shiny testServer() where possible
- All existing tests must continue passing (regression check)

## Dependencies (New R Packages)

| Package | Source | Required By |
|---|---|---|
| `Rphylopars` | CRAN | Layer 3: phylogenetic imputation |
| `ape` | CRAN | Layer 3: tree construction, PCoA |
| `MCMCpack` | CRAN | Layer 3: BHPMF Gibbs sampler |
| `Btrait` | GitHub (EMODnet) | Layer 2: EMODnet traits |
| `robis` | CRAN | Layer 2: OBIS MoF |
| `traits` | CRAN | Layer 2: TraitBank |
| `plotly` | CRAN (likely already installed) | Layer 4: radar charts |
