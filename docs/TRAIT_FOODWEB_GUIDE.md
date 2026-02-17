# Trait-Based Food Web Construction Guide

## Overview

The Trait Food Web module constructs food webs using species trait probabilities based on ecological characteristics. This approach allows you to build food webs when direct interaction data is unavailable, using trait-matching rules derived from ecological theory.

## Trait System

The module uses five trait categories to predict trophic interactions:

### 1. Size Class (MS)
Determines predator-prey size relationships.

| Code | Description | Role |
|------|-------------|------|
| MS1 | Extra Small (XS) | Prey only |
| MS2 | Small (S) | Prey only |
| MS3 | Small-Medium (SM) | Predator & Prey |
| MS4 | Medium (M) | Predator & Prey |
| MS5 | Medium-Large (ML) | Predator & Prey |
| MS6 | Large (L) | Predator & Prey |
| MS7 | Extra Large (XL) | Predator only (rarely prey) |

**Key Rules:**
- Only MS3-MS6 can be active predators
- MS7 organisms are excluded as prey (too large)
- Smaller predators preferentially consume smaller prey

### 2. Foraging Strategy (FS)
How the organism acquires food.

| Code | Description | Diet Type |
|------|-------------|-----------|
| FS0 | None | Primary producer (excluded as consumer) |
| FS1 | Predator | Active hunting |
| FS2 | Scavenger | Dead/decaying matter |
| FS3 | Parasite | Living host (excluded here) |
| FS4 | Grazer | Benthic/surface feeding |
| FS5 | Deposit feeder | Sediment organic matter |
| FS6 | Filter feeder | Suspended particles |

**Key Rules:**
- FS0 (primary producers) cannot consume
- FS3 (parasites) are excluded from this framework
- Predators prefer medium-sized prey (MS3-MS4)
- Filter feeders and grazers prefer very small prey (MS1-MS2)

### 3. Mobility (MB)
Movement capability affecting encounter rates.

| Code | Description | Examples |
|------|-------------|----------|
| MB1 | Sessile | Barnacles, mussels, sponges |
| MB2 | Passive floater | Jellyfish, phytoplankton |
| MB3 | Crawler-burrower | Crabs, worms, snails |
| MB4 | Facultative swimmer | Amphipods, some fish |
| MB5 | Obligate swimmer | Most fish, marine mammals |

**Key Rules:**
- Mobile predators (MB5) hunt mobile prey effectively
- Sessile consumers (MB1) primarily capture sessile/slow prey
- Intermediate mobility allows diverse hunting strategies

### 4. Environmental Position (EP)
Vertical/spatial habitat affecting encounter probability.

| Code | Description | Habitat |
|------|-------------|---------|
| EP1 | Infaunal | Within sediment |
| EP2 | Epibenthic | On seafloor surface |
| EP3 | Benthopelagic | Near-bottom water column |
| EP4 | Pelagic | Open water column |

**Key Rules:**
- Spatial overlap increases interaction probability
- Pelagic predators (EP4) prefer pelagic prey
- Epibenthic consumers can access multiple prey types

### 5. Protection (PR)
Physical defenses affecting vulnerability.

| Code | Description | Examples |
|------|-------------|----------|
| PR0 | None | Soft-bodied organisms |
| PR2 | Tube | Polychaete tubes |
| PR3 | Burrow | Infaunal burrows |
| PR5 | Soft shell | Thin exoskeleton |
| PR6 | Hard shell | Thick shells (mussels, clams) |
| PR7 | Few spines | Sea stars, urchins |
| PR8 | Armoured | Heavy armour (crabs, lobsters) |

**Key Rules:**
- Protection reduces vulnerability
- Small prey with heavy protection have lower predation probability
- Large prey with protection are nearly immune

## Interaction Probability Matrices

The module uses five probability matrices to calculate interaction likelihood:

### 1. MS × MS (Consumer Size × Resource Size)
```
Consumer ↓ | MS1   MS2   MS3   MS4   MS5   MS6
-------------------------------------------------
MS3 (SM)   | 0.95  0.80  0.50  0.05  0.05  0.05
MS4 (M)    | 0.80  0.95  0.80  0.50  0.05  0.05
MS5 (ML)   | 0.50  0.80  0.95  0.80  0.50  0.05
MS6 (L)    | 0.20  0.50  0.80  0.95  0.80  0.50
```
**Pattern:** Predators prefer prey 1-2 size classes smaller.

### 2. FS × MS (Foraging Strategy × Resource Size)
```
Consumer ↓        | MS1   MS2   MS3   MS4   MS5   MS6
-------------------------------------------------------
FS1 (Predator)    | 0.20  0.50  0.95  0.80  0.50  0.20
FS2 (Scavenger)   | 0.05  0.20  0.50  0.95  0.80  0.50
FS4 (Grazer)      | 0.95  0.80  0.05  0.05  0.05  0.05
FS5 (Deposit)     | 0.80  0.80  0.50  0.05  0.05  0.05
FS6 (Filter)      | 0.95  0.95  0.20  0.05  0.05  0.05
```
**Pattern:** Different strategies target different size ranges.

### 3. MB × MB (Consumer Mobility × Resource Mobility)
```
Consumer ↓       | MB1   MB2   MB3   MB4   MB5
-------------------------------------------------
MB1 (Sessile)    | 0.95  0.05  0.05  0.05  0.05
MB2 (Passive)    | 0.80  0.95  0.20  0.20  0.20
MB3 (Crawler)    | 0.80  0.95  0.95  0.20  0.20
MB4 (Fac. swim)  | 0.80  0.80  0.80  0.80  0.80
MB5 (Obl. swim)  | 0.20  0.50  0.95  0.95  0.95
```
**Pattern:** Similar or compatible mobility increases interaction.

### 4. EP × MS (Environmental Position × Resource Size)
```
Consumer ↓        | MS2   MS3   MS4   MS5
-------------------------------------------
EP1 (Infaunal)    | 0.05  0.50  0.80  0.05
EP2 (Epibenthic)  | 0.50  0.80  0.95  0.80
EP3 (Benthopelag) | 0.80  0.95  0.80  0.50
EP4 (Pelagic)     | 0.95  0.80  0.50  0.05
```
**Pattern:** Position affects accessible prey sizes.

### 5. PR × MS (Resource Protection × Resource Size)
```
Resource ↓      | MS2   MS3   MS4   MS5
------------------------------------------
PR0 (None)      | 0.95  0.95  0.80  0.50
PR2 (Tube)      | 0.80  0.80  0.50  0.05
PR3 (Burrow)    | 0.80  0.80  0.50  0.05
PR6 (Hard)      | 0.50  0.50  0.80  0.50
PR7 (Spines)    | 0.20  0.20  0.50  0.20
PR8 (Armoured)  | 0.20  0.20  0.50  0.20
```
**Pattern:** Protection reduces predation, especially for small prey.

## Aggregation Rule

The final interaction probability is the **minimum** of all applicable trait probabilities:

```
P(interaction) = min(P_MS×MS, P_FS×MS, P_MB×MB, P_EP×MS, P_PR×MS)
```

**Rationale:** An interaction requires ALL conditions to be favorable. A single incompatible trait (e.g., wrong size, spatial mismatch) prevents the interaction.

## Using the Module

### Input Format

Create a CSV file with the following columns:

```csv
species,MS,FS,MB,EP,PR
Predatory_fish,MS5,FS1,MB5,EP4,PR0
Small_fish,MS3,FS1,MB5,EP4,PR0
Zooplankton,MS2,FS6,MB4,EP3,PR0
Phytoplankton,MS1,FS0,MB2,EP4,PR0
Benthic_filter_feeder,MS3,FS6,MB1,EP2,PR6
```

### Workflow

1. **Input Data**
   - Upload CSV file, use example dataset, or create manual template
   - Edit trait codes directly in the interactive table

2. **Validate**
   - Click "Validate Trait Data" to check for errors
   - Fix any invalid trait codes or duplicated species

3. **Construct Network**
   - Set probability threshold (default: 0.05)
   - Click "Construct Food Web"
   - View interactive network visualization

4. **Analyze**
   - Explore network properties (connectance, degree distribution)
   - View probability matrix heatmap
   - Examine species-specific interactions

5. **Export**
   - Download adjacency matrix as CSV
   - Download probability matrix
   - Download template for new datasets

### Example Datasets

The module includes three pre-built examples:

1. **Simple (5 species)**
   - Basic food chain with primary producer through top predator
   - Good for learning the trait system

2. **Marine Invertebrates (10 species)**
   - Diverse functional groups
   - Demonstrates various trait combinations

3. **Complex (20 species)**
   - Realistic multi-trophic network
   - Shows emergent network properties

## Interpretation

### Network Properties

- **Connectance:** Proportion of realized links out of possible links
  - Low (<0.1): Specialist network
  - Medium (0.1-0.3): Typical marine food webs
  - High (>0.3): Generalist/highly connected

- **Degree Distribution:** Number of predators/prey per species
  - Indicates trophic generalism/specialism
  - Highlights keystone species (high connectivity)

### Edge Weights

Edge width in the visualization represents interaction probability:
- Thick edges (>0.5): High probability, strong trait compatibility
- Medium edges (0.2-0.5): Moderate probability, some trait mismatches
- Thin edges (0.05-0.2): Low probability, weak trait compatibility

### Color Coding

Nodes are colored by size class (MS):
- Light blue: Small organisms (MS1-MS2)
- Medium blue: Medium organisms (MS3-MS4)
- Dark blue: Large organisms (MS5-MS7)

## Best Practices

1. **Trait Assignment**
   - Use field guides, taxonomic databases, and literature
   - Be consistent with size class definitions
   - Consider ontogenetic changes (juveniles vs adults)

2. **Threshold Selection**
   - 0.05: Include weak/rare interactions (high connectance)
   - 0.20: Moderate interactions only (medium connectance)
   - 0.50: Strong interactions only (low connectance, core network)

3. **Validation**
   - Compare with empirical food webs if available
   - Check for unrealistic patterns (e.g., no top predators)
   - Verify trophic level distributions

4. **Limitations**
   - Does not account for abundance/biomass
   - Assumes traits are accurately assigned
   - Does not include seasonal variation
   - Simplified protection categories

## Advanced Usage

### Programmatic Access

The core functions can be used outside the Shiny app:

```r
# Load functions
source("R/functions/trait_foodweb.R")

# Load species data
species_data <- read.csv("my_species_traits.csv")

# Validate
validation <- validate_trait_data(species_data)
print(validation$messages)

# Construct food web
adjacency <- construct_trait_foodweb(species_data, threshold = 0.05)

# Get probability matrix
prob_matrix <- construct_trait_foodweb(species_data, threshold = 0, return_probs = TRUE)

# Create igraph object
g <- trait_foodweb_to_igraph(species_data, threshold = 0.05)

# Analyze
library(igraph)
plot(g, vertex.size = 10, edge.arrow.size = 0.5)
```

### Customizing Probability Matrices

To modify the trait-matching rules, edit the matrices in `R/functions/trait_foodweb.R`:

```r
# Example: Increase predation on large prey
MS_MS["MS6", "MS6"] <- 0.80  # Default: 0.50
```

## References

This trait-based approach is based on:

- **Body size ratios:** Brose et al. (2006) Ecology Letters
- **Foraging modes:** Woodward & Hildrew (2002) Ecology
- **Trait matching:** Eklöf et al. (2013) Oikos
- **Aggregation rules:** Williams & Martinez (2000) Nature

## Troubleshooting

**Problem:** No interactions are created
**Solution:** Check that you have consumers (MS3-MS6 with FS1/2/4/5/6) and prey (MS1-MS6 with FS0 or as other consumers)

**Problem:** Too many/too few interactions
**Solution:** Adjust the probability threshold or review trait assignments

**Problem:** Validation errors
**Solution:** Ensure all trait codes exactly match the defined codes (case-sensitive)

**Problem:** Network looks unrealistic
**Solution:** Verify size class assignments are ecologically meaningful

## Contact

For questions or issues with the Trait Food Web module, please open an issue on the EcoNeTool GitHub repository.
