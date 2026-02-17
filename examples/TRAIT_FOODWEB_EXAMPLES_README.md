# Trait-Based Food Web Example Datasets

This folder contains example CSV files for the Trait Food Web module in EcoNeTool.

## Available Datasets

### 1. trait_foodweb_simple.csv
**Species:** 5
**Description:** Basic food chain demonstrating the trait system
**Use case:** Learning how trait codes work

Contains:
- Primary producer (phytoplankton)
- Primary consumer (zooplankton)
- Secondary consumers (small fish, benthic invertebrate)
- Top predator (predatory fish)

**Expected Network:**
- Linear food chain with some omnivory
- Connectance: ~0.25
- 4 trophic levels

---

### 2. trait_foodweb_marine_invertebrates.csv
**Species:** 15
**Description:** Diverse marine invertebrate community
**Use case:** Demonstrating various functional groups and trait combinations

Contains:
- Multiple primary producers (diatoms, dinoflagellates, kelp)
- Diverse consumers (filter feeders, grazers, predators)
- Various protection strategies (shells, spines, soft-bodied)
- Different habitats (pelagic, benthic, infaunal)

**Expected Network:**
- Complex web with multiple feeding pathways
- Connectance: ~0.15
- 3-4 trophic levels
- Clear separation between filter feeders and predators

---

### 3. trait_foodweb_coastal_ecosystem.csv
**Species:** 35
**Description:** Realistic Baltic/North Sea coastal food web
**Use case:** Full ecosystem modeling and analysis

Contains:
- Complete trophic structure (bacteria to seals/seabirds)
- Multiple functional groups at each level
- Size-structured populations (juvenile/adult fish)
- Benthic-pelagic coupling
- Top predators (seals, diving seabirds)

**Expected Network:**
- Highly complex, realistic food web
- Connectance: ~0.10
- 5+ trophic levels
- Emergent compartmentalization (benthic vs pelagic)
- Size-structured predation patterns

---

## How to Use

### In EcoNeTool App

1. Open EcoNeTool and navigate to **Trait Food Web** tab
2. Select input method: **Upload CSV file**
3. Click **Browse** and select one of these CSV files
4. Click **Validate Trait Data** to check for errors
5. Set probability threshold (recommended: 0.05 for exploration, 0.20 for core network)
6. Click **Construct Food Web**
7. Explore the interactive network visualization

### From R Console

```r
# Load functions
source("R/functions/trait_foodweb.R")

# Load example dataset
species_data <- read.csv("examples/trait_foodweb_marine_invertebrates.csv")

# Validate
validation <- validate_trait_data(species_data)
print(validation$messages)

# Construct food web
foodweb <- construct_trait_foodweb(species_data, threshold = 0.05)

# Create igraph object
library(igraph)
g <- trait_foodweb_to_igraph(species_data, threshold = 0.05)

# Analyze network properties
cat("Species:", vcount(g), "\n")
cat("Interactions:", ecount(g), "\n")
cat("Connectance:", edge_density(g), "\n")

# Plot
plot(g,
     vertex.size = 8,
     vertex.label.cex = 0.7,
     edge.arrow.size = 0.3,
     layout = layout_with_fr(g))
```

---

## Trait Code Reference

### Size Class (MS)
- **MS1:** Extra Small (bacteria, small phytoplankton)
- **MS2:** Small (copepods, small zooplankton)
- **MS3:** Small-Medium (amphipods, small fish)
- **MS4:** Medium (crabs, medium fish)
- **MS5:** Medium-Large (large fish, macroalgae)
- **MS6:** Large (large fish, top predators)
- **MS7:** Extra Large (marine mammals, large seabirds)

### Foraging Strategy (FS)
- **FS0:** None (primary producers)
- **FS1:** Predator (active hunting)
- **FS2:** Scavenger (dead/decaying matter)
- **FS4:** Grazer (benthic/surface feeding)
- **FS5:** Deposit feeder (sediment organic matter)
- **FS6:** Filter feeder (suspended particles)

### Mobility (MB)
- **MB1:** Sessile (attached, non-moving)
- **MB2:** Passive floater (drift with currents)
- **MB3:** Crawler-burrower (slow benthic movement)
- **MB4:** Facultative swimmer (can swim when needed)
- **MB5:** Obligate swimmer (continuously swimming)

### Environmental Position (EP)
- **EP1:** Infaunal (within sediment)
- **EP2:** Epibenthic (on seafloor surface)
- **EP3:** Benthopelagic (near-bottom water column)
- **EP4:** Pelagic (open water column)

### Protection (PR)
- **PR0:** None (soft-bodied, no protection)
- **PR2:** Tube (tube-dwelling)
- **PR3:** Burrow (burrow-dwelling)
- **PR5:** Soft shell (thin exoskeleton)
- **PR6:** Hard shell (thick shell)
- **PR7:** Few spines (spiny defense)
- **PR8:** Armoured (heavy armour)

---

## Creating Your Own Dataset

### Template

Download the template CSV from the app or create a file with this structure:

```csv
species,MS,FS,MB,EP,PR
My_species_1,MS3,FS1,MB5,EP4,PR0
My_species_2,MS2,FS6,MB2,EP4,PR0
```

### Guidelines

1. **Use consistent size classes**
   - Consider maximum body size, not average
   - Account for ontogenetic changes (create separate entries for juveniles/adults if needed)

2. **Match foraging strategy to diet**
   - FS0 for all autotrophs (plants, algae)
   - Be specific about feeding mode (filter vs deposit vs grazer)

3. **Consider life history**
   - Mobility can change with life stage
   - Protection may vary seasonally

4. **Validate habitat assignments**
   - Environmental position should match observed distribution
   - Consider vertical migration patterns

5. **Review protection codes**
   - Account for both chemical and physical defenses
   - Consider relative strength (PR7 vs PR8)

---

## Troubleshooting

**Q: Why are there no links in my network?**
A: Check that you have both consumers (FS1/2/4/5/6) and appropriate prey sizes. Ensure size classes are compatible (predators need smaller prey).

**Q: The network is too dense/sparse**
A: Adjust the probability threshold. Lower threshold (0.05) = more links, higher threshold (0.5) = fewer links.

**Q: Species is not consuming anything**
A: Verify the foraging strategy code. FS0 cannot consume (primary producers only).

**Q: Species is not being consumed**
A: Check if it's MS7 (excluded as prey). Also check if protection level is too high relative to size.

**Q: Validation errors**
A: Ensure trait codes are exactly as specified (case-sensitive). Use the trait reference tables.

---

## Advanced: Modifying Examples

You can modify these examples to create custom scenarios:

### Add Species
1. Open CSV in Excel/text editor
2. Add new row with trait codes
3. Save and reload in app

### Change Traits
1. Modify trait codes for existing species
2. Observe how network structure changes
3. Use for sensitivity analysis

### Size Structure Experiment
```csv
Cod_juvenile,MS4,FS1,MB5,EP3,PR0
Cod_adult,MS6,FS1,MB5,EP3,PR0
```
Compare networks with/without ontogenetic diet shifts.

---

## Citation

If you use these example datasets in publications, please cite:

> EcoNeTool Trait Food Web Module (2025). Example datasets for trait-based food web construction. https://github.com/your-repo/EcoNeTool

For the trait-matching approach, cite:
> Eklöf, A., et al. (2013). The dimensionality of ecological networks. Ecology Letters, 16(5), 577-583.

---

## Further Reading

For detailed information on the trait system and probability matrices, see:
- **docs/TRAIT_FOODWEB_GUIDE.md** - Complete user guide
- **R/functions/trait_foodweb.R** - Source code with probability matrices

For questions or contributions, please open an issue on GitHub.
