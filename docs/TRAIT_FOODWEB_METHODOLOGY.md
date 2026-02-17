# Trait-Based Food Web Construction - Complete Methodology Guide

**Version:** 1.1.2
**Last Updated:** 2025-12-23
**Authors:** EcoNeTool Development Team

---

## Table of Contents

1. [Introduction](#introduction)
2. [Theoretical Background](#theoretical-background)
3. [The Five Trait Dimensions](#the-five-trait-dimensions)
4. [Trait Codes Reference](#trait-codes-reference)
5. [Interaction Probability Matrices](#interaction-probability-matrices)
6. [Probability Calculation Method](#probability-calculation-method)
7. [Automated Trait Lookup](#automated-trait-lookup)
8. [Data Input Methods](#data-input-methods)
9. [Network Construction Workflow](#network-construction-workflow)
10. [Interpreting Results](#interpreting-results)
11. [Best Practices](#best-practices)
12. [Troubleshooting](#troubleshooting)
13. [References](#references)

---

## 1. Introduction

### What is Trait-Based Food Web Construction?

Trait-based food web construction is a methodology for predicting trophic interactions between species based on their **functional traits** rather than direct observation of feeding events. This approach uses categorical trait codes across five dimensions to calculate the probability that one species consumes another.

### Why Use Trait-Based Approaches?

**Advantages:**
- ✅ **Data efficiency:** Requires only species trait information, not exhaustive feeding observations
- ✅ **Generalizability:** Can predict interactions for unobserved species pairs
- ✅ **Mechanistic:** Based on ecological theory (gape limitation, mobility matching, habitat overlap)
- ✅ **Scalability:** Can construct large food webs (100+ species) from trait databases
- ✅ **Flexibility:** Adjustable probability thresholds for different ecosystems

**Applications:**
- Constructing food webs for poorly studied ecosystems
- Predicting novel interactions after species invasions
- Comparing food web structure across regions
- Studying effects of trait shifts on network topology
- Generating null models for network analysis

### Conceptual Framework

```
Species Traits → Trait Codes → Probability Matrices → Interaction Probabilities → Food Web Network
```

**Example:**
```
Atlantic cod (Gadus morhua)
├─ Body size: 80 cm        → MS6 (Very large)
├─ Foraging: Predator      → FS1 (Predator)
├─ Mobility: Fast swimmer  → MB5 (Obligate swimmer)
├─ Position: Pelagic       → EP4 (Pelagic)
└─ Protection: None        → PR0 (No hard protection)

Herring (Clupea harengus)
├─ Body size: 30 cm        → MS5 (Large)
├─ Foraging: Filter feeder → FS6 (Filter feeder)
├─ Mobility: Fast swimmer  → MB5 (Obligate swimmer)
├─ Position: Pelagic       → EP4 (Pelagic)
└─ Protection: None        → PR0 (No hard protection)

Interaction Probability Calculation:
P(Cod eats Herring) = min(
  P_size(MS6 → MS5) = 0.8,    # Size ratio allows
  P_forage(FS1 × MS5) = 0.9,  # Predators eat large prey
  P_mobility(MB5 → MB5) = 0.6, # Can catch fast swimmers
  P_position(EP4 × MS5) = 0.8, # Same habitat
  P_protection(PR0 × MS5) = 1.0 # No protection
) = 0.6

→ Cod-Herring interaction is predicted if threshold < 0.6
```

---

## 2. Theoretical Background

### Ecological Basis

Trait-based food web construction is grounded in **niche theory** and **metabolic theory**:

#### 2.1 Gape Limitation (Size Matching)

**Principle:** Consumers can only ingest prey within a limited size range relative to their own body size.

**Mechanism:**
- **Too small:** Prey not energetically profitable (high search time, low energy gain)
- **Too large:** Physical constraint (gape size, handling time, risk of injury)
- **Optimal range:** Typically 0.01 to 0.5 of consumer body size (varies by feeding mode)

**Size Classes (MS1-MS7):**
```
MS1: < 0.1 cm (microplankton, bacteria)
MS2: 0.1 - 1 cm (mesoplankton, small invertebrates)
MS3: 1 - 5 cm (small fish, large invertebrates)
MS4: 5 - 20 cm (medium fish, crabs)
MS5: 20 - 50 cm (large fish)
MS6: 50 - 150 cm (very large fish)
MS7: > 150 cm (marine mammals, large sharks)
```

**Patterns:**
- Small consumers (MS1-MS3) eat smaller or equal-sized prey
- Medium consumers (MS4-MS5) have broader diet breadth
- Large consumers (MS6-MS7) can eat prey from MS2-MS6

#### 2.2 Foraging Strategy (Feeding Mode)

**Principle:** Feeding mode determines which prey types can be captured/consumed.

**Categories (FS0-FS6):**
- **FS0 (Primary Producer):** No consumption (photosynthesis/chemosynthesis)
- **FS1 (Predator):** Active pursuit, high prey size selectivity
- **FS2 (Scavenger):** Dead/moribund organisms, low selectivity
- **FS3 (Omnivore):** Mixed diet, moderate selectivity
- **FS4 (Grazer):** Algae/plants, selective for producers
- **FS5 (Deposit Feeder):** Sediment organic matter, low selectivity
- **FS6 (Filter Feeder):** Suspended particles, size-selective

**Trophic Level Correspondence:**
- FS0 → TL 1 (primary producer)
- FS6 → TL 2 (primary consumer)
- FS4 → TL 2 (herbivore)
- FS1 → TL 3+ (carnivore/piscivore)

#### 2.3 Mobility Matching

**Principle:** Consumers must be able to encounter and capture prey based on relative mobility.

**Mobility Classes (MB1-MB5):**
```
MB1: Sessile (attached, no movement)
MB2: Limited (slow creeping, passive floating)
MB3: Crawling-burrowing (benthic movement)
MB4: Facultative swimmer (benthic + swimming)
MB5: Obligate swimmer (pelagic, active swimming)
```

**Capture Probability:**
- High mobility consumers can catch any prey (MB5 → MB1-MB5)
- Low mobility consumers limited to slow/sessile prey (MB1 → MB1-MB2)
- Sessile consumers rely on water flow (MB1 → MB2-MB4 via filtering)

#### 2.4 Habitat Overlap (Environmental Position)

**Principle:** Trophic interactions require spatial co-occurrence.

**Position Classes (EP1-EP4):**
```
EP1: Infaunal (buried in sediment)
EP2: Epibenthic (on sediment surface)
EP3: Benthopelagic (near bottom, some water column use)
EP4: Pelagic (water column, no bottom contact)
```

**Overlap Patterns:**
- EP1 ↔ EP1: High overlap (same burrow/sediment layer)
- EP4 ↔ EP4: High overlap (planktonic interactions)
- EP2 ↔ EP3: Moderate overlap (benthopelagic predators feed on benthos)
- EP1 ↔ EP4: Low overlap (rare cross-habitat interactions)

#### 2.5 Prey Defense (Protection)

**Principle:** Defensive structures reduce predation probability.

**Protection Classes (PR0-PR8):**
```
PR0: None (soft-bodied, no protection)
PR2: Tube (protective tube/burrow)
PR3: Burrow (sediment refuge)
PR5: Soft shell (thin calcium shell)
PR6: Hard shell (thick calcium shell, e.g., bivalves)
PR7: Few spines (some defensive spines)
PR8: Armoured (heavy exoskeleton, many spines)
```

**Predation Resistance:**
- PR0: High vulnerability (all predators)
- PR2-PR3: Moderate protection (limits some predators)
- PR6-PR8: High protection (only specialized predators)

---

## 3. The Five Trait Dimensions

### 3.1 Size Class (MS)

**Definition:** Maximum body length in centimeters (cm).

**Classification Scheme:**

| Code | Range | Examples | Ecological Group |
|------|-------|----------|------------------|
| **MS1** | < 0.1 cm | Bacteria, picoplankton, very small phytoplankton | Microorganisms |
| **MS2** | 0.1 - 1 cm | Mesoplankton, copepod nauplii, small diatom chains | Mesoplankton |
| **MS3** | 1 - 5 cm | Large copepods, mysids, juvenile fish, small gastropods | Macroplankton |
| **MS4** | 5 - 20 cm | Shrimp, small fish (sprat, goby), crabs, medium gastropods | Small nekton |
| **MS5** | 20 - 50 cm | Herring, mackerel, plaice, medium fish | Medium nekton |
| **MS6** | 50 - 150 cm | Cod, tuna, salmon, large flatfish | Large nekton |
| **MS7** | > 150 cm | Sharks, marine mammals, large rays | Megafauna |

**Data Sources:**
- FishBase: `max_length` field (cm)
- WoRMS: Species attributes (when available)
- BIOTIC: `Max_Length_mm` / 10
- MAREDAT: `ESD_um` converted to cm
- Literature: Species descriptions, field guides

**Harmonization Rules:**
1. Use **maximum** reported body length (not average)
2. Convert all units to cm
3. For colonial organisms, use colony size
4. For fish, use total length (TL), not standard length (SL)

**Ecosystem Adjustments:**
- **Arctic:** +20% (Bergmann's rule - colder = larger)
- **Tropical:** -10% (warmer = smaller)
- **Deep sea:** +10% (deep-sea gigantism)

---

### 3.2 Foraging Strategy (FS)

**Definition:** Primary feeding mode or trophic strategy.

**Classification Scheme:**

| Code | Strategy | Description | Trophic Level | Examples |
|------|----------|-------------|---------------|----------|
| **FS0** | Primary Producer | Photosynthesis or chemosynthesis | 1.0 | Phytoplankton, macroalgae, seagrass |
| **FS1** | Predator | Active pursuit of live prey | 3.0+ | Cod, tuna, squid, jellyfish |
| **FS2** | Scavenger | Dead/moribund organisms | 2.5-3.0 | Crabs, hagfish, some polychaetes |
| **FS3** | Omnivore | Mixed diet (plants + animals) | 2.5-3.0 | Some crabs, sea urchins |
| **FS4** | Grazer | Algae/plant consumption | 2.0 | Limpets, sea urchins, parrotfish |
| **FS5** | Deposit Feeder | Sediment organic matter | 2.0-2.5 | Lugworm, sea cucumbers |
| **FS6** | Filter Feeder | Suspended particles (plankton) | 2.0-2.5 | Bivalves, barnacles, salps |

**Data Sources:**
- FishBase: `FeedingMode`, `DietTroph`, `FoodTroph` fields
- BIOTIC: `Feeding_mode` field
- WoRMS: Not directly available (infer from taxonomy)
- Literature: Feeding ecology studies

**Detection Patterns (Regex):**
```regex
FS0: "photosyn|autotrop|producer|plant|algae|phytoplankton"
FS1: "predat|carnivor|pisciv|hunter"
FS2: "scaveng|detritivor|carrion"
FS3: "omnivor|mixed|opportun"
FS4: "graz|herbiv|scraper|browser"
FS5: "deposit|sediment|burrower"
FS6: "filter|suspension|planktivor"
```

**Harmonization Hierarchy:**
1. Explicit feeding mode (FishBase, BIOTIC)
2. Pattern matching on diet descriptions
3. Trophic level inference (TL < 1.5 → FS0, TL > 3.0 → FS1)
4. Taxonomic defaults (e.g., bivalves → FS6)

---

### 3.3 Mobility (MB)

**Definition:** Locomotion capacity and typical movement pattern.

**Classification Scheme:**

| Code | Mobility Type | Description | Velocity | Examples |
|------|---------------|-------------|----------|----------|
| **MB1** | Sessile | Permanently attached or fixed | 0 | Barnacles, mussels, sponges, corals |
| **MB2** | Limited | Passive floating, very slow creeping | < 1 cm/s | Jellyfish (drift), sea anemones |
| **MB3** | Crawling-Burrowing | Benthic locomotion, sediment movement | 1-10 cm/s | Crabs, gastropods, polychaetes |
| **MB4** | Facultative Swimmer | Swimming + benthic resting | 10-50 cm/s | Flatfish, rays, benthic shrimp |
| **MB5** | Obligate Swimmer | Continuous swimming, pelagic | > 50 cm/s | Most fish, squid, marine mammals |

**Data Sources:**
- FishBase: `BodyShapeI`, `DemersPelag` fields
- BIOTIC: `Mobility` field
- WoRMS: Not directly available
- Literature: Behavioral ecology

**Taxonomic Inference Rules:**
1. **Fish (Actinopterygii):** → MB5 (obligate swimmers)
2. **Cephalopods (Cephalopoda):** → MB5 (squid/octopus)
3. **Bivalves (Bivalvia):** → MB1 (sessile, except scallops)
4. **Gastropods (Gastropoda):** → MB3 (crawlers)
5. **Copepods:** → MB5 (active swimmers)
6. **Cnidarians:** → MB1 or MB2 (sessile or passive floaters)

**Escape Velocity Matters:**
- MB1-MB2: Cannot escape predators
- MB3: Can burrow/retract
- MB4: Can escape some predators
- MB5: Can outswim most predators

---

### 3.4 Environmental Position (EP)

**Definition:** Vertical position in the water column and relationship to substrate.

**Classification Scheme:**

| Code | Position | Depth Range | Substrate Contact | Examples |
|------|----------|-------------|-------------------|----------|
| **EP1** | Infaunal | 0-50 cm below surface | Buried in sediment | Burrowing bivalves, polychaetes, nematodes |
| **EP2** | Epibenthic | On sediment surface | Surface dwelling | Starfish, crabs, bottom fish, epibenthic algae |
| **EP3** | Benthopelagic | 0-5 m above bottom | Near-bottom swimming | Cod (adults), flatfish, demersal shrimp |
| **EP4** | Pelagic | > 5 m above bottom | No substrate contact | Herring, plankton, jellyfish, pelagic sharks |

**Data Sources:**
- FishBase: `DepthRangeShallow`, `DepthRangeDeep`, `DemersPelag` fields
- BIOTIC: `Living_habit`, `Substratum` fields
- WoRMS: Habitat flags (when available)
- Literature: Species accounts

**Depth-Based Inference:**
```
If depth_min and depth_max available:
  avg_depth = (depth_min + depth_max) / 2

  If avg_depth < 50 m:
    If burrowing → EP1
    Else → EP2

  If avg_depth > 200 m:
    → EP3 (deep benthopelagic)

  Else:
    Default → EP2
```

**Taxonomic Inference:**
- **Phytoplankton (Bacillariophyceae, Dinophyceae):** → EP4 (need light)
- **Zooplankton (Copepoda, Cladocera):** → EP4 (planktonic)
- **Burrowing bivalves:** → EP1 (infaunal)
- **Fish:** → EP4 default (most are pelagic or benthopelagic)

**Vertical Migration:**
- Some species migrate (e.g., zooplankton DVM)
- Assign position where they spend most time
- Or where predation risk is highest

---

### 3.5 Protection (PR)

**Definition:** Physical defensive structures reducing predation vulnerability.

**Classification Scheme:**

| Code | Protection Type | Structure | Predation Resistance | Examples |
|------|-----------------|-----------|----------------------|----------|
| **PR0** | None | Soft-bodied, no defenses | Low (all predators) | Fish, jellyfish, soft worms |
| **PR2** | Tube | Protective tube/case | Moderate (tube protects) | Tube-dwelling polychaetes |
| **PR3** | Burrow | Sediment refuge | Moderate (refuge behavior) | Burrowing shrimp, some worms |
| **PR5** | Soft Shell | Thin calcium carbonate | Moderate | Small gastropods, young bivalves |
| **PR6** | Hard Shell | Thick calcium carbonate | High (crushing resistance) | Adult bivalves, large gastropods |
| **PR7** | Few Spines | Some defensive spines | Moderate-High | Sea urchins, some fish |
| **PR8** | Armoured | Heavy exoskeleton + spines | Very High | Crabs, lobsters, armoured fish |

**Data Sources:**
- BIOTIC: `Skeleton` field
- FishBase: Body shape descriptors
- WoRMS: Not directly available
- Literature: Morphological descriptions

**Detection Patterns:**
```regex
PR0: "none|soft|naked"
PR2: "tube"
PR3: "burrow"
PR5: "thin.*shell|soft.*shell"
PR6: "shell|calcareous|calcium"
PR7: "spine|spiny|setae"
PR8: "armou?r|exoskeleton|carapace|heavily"
```

**Taxonomic Inference:**
1. **Bivalves (Bivalvia):** → PR6 (hard shell)
2. **Gastropods (Gastropoda):** → PR6 (shell)
3. **Crustaceans (Malacostraca):** → PR8 (exoskeleton)
4. **Echinoderms (Echinodermata):** → PR7 (spines) or PR5 (calcium plates)
5. **Fish (Actinopterygii):** → PR0 (no hard protection)
6. **Cephalopods:** → PR0 (soft-bodied)

**Size-Dependent Protection:**
- Juveniles often have thinner shells (PR5) than adults (PR6)
- Large size can compensate for lack of protection

---

## 4. Trait Codes Reference

### Quick Reference Table

**Size Class (MS)**
```
MS1 < 0.1 cm    MS2 0.1-1 cm   MS3 1-5 cm     MS4 5-20 cm
MS5 20-50 cm    MS6 50-150 cm  MS7 > 150 cm
```

**Foraging Strategy (FS)**
```
FS0 Primary Producer    FS1 Predator           FS2 Scavenger
FS3 Omnivore            FS4 Grazer             FS5 Deposit Feeder
FS6 Filter Feeder
```

**Mobility (MB)**
```
MB1 Sessile             MB2 Limited            MB3 Crawling-Burrowing
MB4 Facultative Swimmer MB5 Obligate Swimmer
```

**Environmental Position (EP)**
```
EP1 Infaunal            EP2 Epibenthic         EP3 Benthopelagic
EP4 Pelagic
```

**Protection (PR)**
```
PR0 None                PR2 Tube               PR3 Burrow
PR5 Soft Shell          PR6 Hard Shell         PR7 Few Spines
PR8 Armoured
```

---

## 5. Interaction Probability Matrices

### 5.1 MS × MS (Consumer Size × Resource Size)

**Principle:** Size ratio determines predation feasibility.

**Matrix:**
```
         MS1   MS2   MS3   MS4   MS5   MS6   MS7
    MS1  0.5   0.2   0.0   0.0   0.0   0.0   0.0
    MS2  0.8   0.5   0.2   0.0   0.0   0.0   0.0
    MS3  0.9   0.8   0.5   0.2   0.0   0.0   0.0
    MS4  0.9   0.9   0.8   0.5   0.2   0.0   0.0
    MS5  0.9   0.9   0.9   0.8   0.5   0.2   0.0
    MS6  0.9   0.9   0.9   0.9   0.8   0.5   0.2
    MS7  0.9   0.9   0.9   0.9   0.9   0.8   0.5
```

**Interpretation:**
- **Diagonal (0.5):** Cannibalism/same-size predation (rare)
- **Below diagonal (0.0):** Smaller cannot eat larger
- **Above diagonal:** Probability decreases with size difference
- **MS7 as resource (column):** Very low (too large for most predators)

**Ecological Rationale:**
- Optimal prey size: ~0.1-0.5 of consumer size
- Very small prey: Low energy return
- Very large prey: Handling difficulty, escape ability

---

### 5.2 FS × MS (Consumer Foraging × Resource Size)

**Principle:** Foraging strategy determines accessible prey sizes.

**Matrix:**
```
         MS1   MS2   MS3   MS4   MS5   MS6   MS7
    FS0  0.0   0.0   0.0   0.0   0.0   0.0   0.0
    FS1  0.5   0.7   0.9   0.9   0.9   0.8   0.5
    FS2  0.7   0.8   0.9   0.9   0.8   0.7   0.4
    FS3  0.6   0.8   0.8   0.7   0.6   0.4   0.1
    FS4  0.9   0.7   0.5   0.2   0.0   0.0   0.0
    FS5  0.9   0.9   0.7   0.3   0.1   0.0   0.0
    FS6  0.9   0.9   0.7   0.4   0.1   0.0   0.0
```

**Interpretation:**
- **FS0 (Producer):** Zero probability (no consumption)
- **FS1 (Predator):** High for MS2-MS6, moderate for very small/large
- **FS4-FS6:** High for small prey (MS1-MS3), low for large
- **FS2 (Scavenger):** Moderate across all sizes (opportunistic)

**Feeding Mode Constraints:**
- Filter feeders (FS6) limited by filtration mesh size
- Grazers (FS4) primarily consume producers (small size classes)
- Predators (FS1) show size selectivity

---

### 5.3 MB × MB (Consumer Mobility × Resource Mobility)

**Principle:** Predator mobility must match or exceed prey to ensure capture.

**Matrix:**
```
         MB1   MB2   MB3   MB4   MB5
    MB1  0.9   0.7   0.3   0.1   0.0
    MB2  0.9   0.8   0.5   0.2   0.0
    MB3  0.9   0.9   0.7   0.4   0.1
    MB4  0.9   0.9   0.9   0.7   0.3
    MB5  0.9   0.9   0.9   0.9   0.6
```

**Interpretation:**
- **MB1 (Sessile) consumers:** Can only catch slow prey via filtering/trapping
- **MB5 (Swimmers):** Can catch all prey types
- **Diagonal:** Moderate probability (similar speed)
- **Below diagonal:** Higher probability (faster catches slower)

**Encounter Rate:**
- Sessile consumers rely on water flow (passive encounter)
- Mobile consumers increase encounter through movement
- Very fast prey (MB5) difficult even for MB5 consumers

---

### 5.4 EP × MS (Consumer Position × Resource Size)

**Principle:** Habitat overlap required for interaction.

**Matrix:**
```
         MS1   MS2   MS3   MS4   MS5   MS6   MS7
    EP1  0.9   0.7   0.5   0.3   0.1   0.0   0.0
    EP2  0.9   0.9   0.8   0.6   0.4   0.2   0.1
    EP3  0.7   0.8   0.9   0.9   0.8   0.6   0.3
    EP4  0.9   0.9   0.8   0.8   0.9   0.9   0.7
```

**Interpretation:**
- **EP1 (Infaunal):** Limited to small prey in sediment
- **EP4 (Pelagic):** Access to all sizes in water column
- **EP2-EP3:** Intermediate (benthic + some pelagic access)

**Spatial Segregation:**
- Infaunal consumers rarely access large pelagic prey
- Pelagic consumers can vertically migrate to access benthos
- Benthopelagic consumers bridge habitats

---

### 5.5 PR × MS (Resource Protection × Resource Size)

**Principle:** Defenses reduce predation, but effectiveness depends on predator size.

**Matrix:**
```
         MS1   MS2   MS3   MS4   MS5   MS6   MS7
    PR0  1.0   1.0   1.0   1.0   1.0   1.0   1.0
    PR2  0.8   0.9   0.9   1.0   1.0   1.0   1.0
    PR3  0.7   0.8   0.9   1.0   1.0   1.0   1.0
    PR5  0.6   0.7   0.8   0.9   1.0   1.0   1.0
    PR6  0.3   0.4   0.6   0.8   0.9   1.0   1.0
    PR7  0.4   0.5   0.7   0.8   0.9   1.0   1.0
    PR8  0.2   0.3   0.5   0.7   0.9   1.0   1.0
```

**Interpretation:**
- **PR0 (No protection):** Maximum vulnerability (1.0)
- **PR6-PR8 (Hard shell/armor):** Strong protection against small predators
- **Large predators (MS6-MS7):** Can overcome most defenses (crushing force)

**Size-Dependent Defense Effectiveness:**
- Small predators cannot crack hard shells
- Large predators have crushing jaws/strength
- Protection most effective against similar-sized predators

---

## 6. Probability Calculation Method

### 6.1 Minimum Aggregation Rule

**Formula:**
```
P(Consumer i eats Resource j) = min(
  P_size(MSi, MSj),
  P_forage(FSi, MSj),
  P_mobility(MBi, MBj),
  P_position(EPi, MSj),
  P_protection(PRj, MSi)
)
```

**Rationale:**
- **Minimum rule:** Interaction limited by most constraining factor
- **Ecological realism:** All conditions must be satisfied
- **Conservative:** Avoids overestimating interactions

**Example:**
```
Consumer: Cod (MS6, FS1, MB5, EP4, PR0)
Resource: Mussel (MS3, FS6, MB1, EP2, PR6)

P_size(MS6, MS3) = 0.9  # Cod can eat MS3
P_forage(FS1, MS3) = 0.9  # Predators eat MS3
P_mobility(MB5, MB1) = 0.9  # Swimmers catch sessile
P_position(EP4, MS3) = 0.8  # Pelagic access to benthic
P_protection(PR6, MS6) = 1.0  # Large predator can crack shell

P(Cod → Mussel) = min(0.9, 0.9, 0.9, 0.8, 1.0) = 0.8

→ Strong interaction predicted
```

### 6.2 Threshold Application

**Adjacency Matrix:**
```
A[i,j] = {
  1  if P(i → j) ≥ threshold
  0  if P(i → j) < threshold
}
```

**Choosing Threshold:**
- **Low (0.01-0.1):** Permissive (many interactions, high connectance)
- **Medium (0.1-0.5):** Moderate (realistic food webs)
- **High (0.5-1.0):** Restrictive (only strong interactions)

**Typical Values by Ecosystem:**
- **Pelagic:** 0.05 (high mobility, frequent encounters)
- **Benthic:** 0.10 (lower mobility, specific predator-prey)
- **Complex:** 0.15 (many specialists)

### 6.3 Directional Interactions

**Asymmetry:**
```
P(A → B) ≠ P(B → A)

Example:
P(Cod → Herring) = 0.8  # Cod eats herring
P(Herring → Cod) = 0.0  # Herring cannot eat cod (size difference)
```

**Network Structure:**
- Directed graph (predator → prey)
- No undirected edges
- Allows cycles (e.g., cannibalism, intraguild predation)

---

## 7. Automated Trait Lookup

### 7.1 Database Hierarchy

**Workflow:**
```
1. WoRMS → Taxonomic backbone (phylum, class, order, family)
2. FishBase → Fish traits (size, trophic level, feeding, habitat)
3. BIOTIC → Marine invertebrate traits (size, feeding, mobility, skeleton)
4. MAREDAT → Zooplankton traits (size, group)
5. PTDB → Phytoplankton traits (size, class)
6. TraitBank → Gap-filling (rarely used)
```

**Priority:**
- FishBase > BIOTIC > MAREDAT > PTDB > WoRMS > Taxonomic inference

### 7.2 Harmonization Process

**Steps:**
1. **Retrieve raw data** from databases
2. **Convert continuous → categorical** (e.g., 25 cm → MS5)
3. **Pattern match text** (e.g., "predator" → FS1)
4. **Apply taxonomic rules** (e.g., fish → MB5)
5. **Assign confidence** (high = database, medium = inferred, low = default)

**Example Harmonization:**
```
Species: Calanus finmarchicus (copepod)

Raw data (MAREDAT):
  ESD_um: 1200 μm = 0.12 cm
  Group: Copepoda
  Trophic_level: 2.3

Harmonization:
  MS: 0.12 cm → MS2
  FS: TL 2.3 → FS6 (filter feeder)
  MB: Copepoda → MB5 (swimmer)
  EP: Copepoda → EP4 (pelagic)
  PR: No data → PR0 (soft-bodied, default)

Output:
  species: Calanus finmarchicus
  MS: MS2, FS: FS6, MB: MB5, EP: EP4, PR: PR0
  confidence: high
  sources: MAREDAT, WoRMS, taxonomic_inference
```

### 7.3 Caching System

**Purpose:**
- Reduce API calls
- Improve performance
- Ensure reproducibility

**Implementation:**
```
cache/taxonomy/
  ├─ Gadus_morhua.rds
  ├─ Clupea_harengus.rds
  └─ ...

TTL: 30 days
```

**Cache invalidation:**
- Manual deletion
- Database updates
- Configuration changes

---

## 8. Data Input Methods

### Method 1: Automated Lookup

**Best for:** Species with database coverage

**Steps:**
1. Enter scientific names (one per line)
2. Select databases to query
3. Click "Run Automated Lookup"
4. Review and edit results

**Advantages:**
- ✅ Fast (seconds for 10-20 species)
- ✅ Objective (reproducible)
- ✅ Includes metadata (sources, confidence)

**Limitations:**
- ⚠️ Requires internet connection
- ⚠️ Coverage varies by taxon (fish > invertebrates)
- ⚠️ May need manual correction

### Method 2: Upload CSV

**Best for:** Pre-compiled trait data, reproducible workflows

**Format:**
```csv
species,MS,FS,MB,EP,PR
Gadus morhua,MS6,FS1,MB5,EP4,PR0
Clupea harengus,MS5,FS6,MB5,EP4,PR0
Mytilus edulis,MS3,FS6,MB1,EP2,PR6
```

**Validation:**
- Checks for required columns
- Validates trait codes
- Reports errors/warnings

### Method 3: Example Datasets

**Best for:** Learning, testing, demonstrations

**Available:**
- **Simple (5 species):** Basic food chain
- **Marine invertebrates (10 species):** Benthic community
- **Complex (20 species):** Full ecosystem

### Method 4: Manual Entry

**Best for:** Small datasets, custom species, teaching

**Steps:**
1. Specify number of species
2. Click "Create Template"
3. Edit data table directly
4. Validate before constructing

---

## 9. Network Construction Workflow

### Step-by-Step Process

**1. Prepare Trait Data**
```
Option A: Automated lookup
Option B: Upload CSV
Option C: Load example
Option D: Manual entry
```

**2. Validate Trait Data**
```
Click "Validate Trait Data"
Check for:
  - Missing values
  - Invalid codes
  - Duplicate species
```

**3. Set Probability Threshold**
```
Adjust slider: 0.01 - 1.0
Recommended: 0.05 - 0.15
```

**4. Construct Network**
```
Click "Construct Food Web"
Generates:
  - Adjacency matrix
  - Probability matrix
  - igraph object
```

**5. Visualize and Analyze**
```
View:
  - Network plot
  - Network properties
  - Probability heatmap
```

**6. Export Results**
```
Download:
  - Adjacency matrix (CSV)
  - Trait data (CSV)
  - Network object (RDS)
```

---

## 10. Interpreting Results

### Network Properties

**Species (S):** Number of nodes (species)

**Links (L):** Number of directed edges (interactions)

**Connectance (C):**
```
C = L / (S²)
Range: 0 (no links) to 1 (fully connected)
Typical: 0.1 - 0.3 for real food webs
```

**Link Density:**
```
LD = L / S
Range: > 0
Typical: 2-10 links per species
```

**Modularity:**
- Measures compartmentalization
- Range: -1 to 1
- High values indicate distinct modules/compartments

**Characteristic Path Length:**
- Average shortest path between species pairs
- Indicates "degrees of separation"
- Shorter = more direct energy transfer pathways

### Probability Heatmap

**Colors:**
- **Red:** High probability (strong interaction)
- **Yellow:** Moderate probability
- **Blue:** Low probability (weak/rare interaction)
- **White:** Zero probability (impossible)

**Patterns:**
- **Diagonal line:** No cannibalism (P_size constraint)
- **Block structure:** Trophic levels
- **Sparse regions:** Habitat segregation

### Validation Checks

**Ecological Realism:**
1. **Connectance:** Should be 0.05-0.30
2. **Trophic levels:** Check for herbivores, carnivores, top predators
3. **Basal species:** Primary producers present
4. **Top predators:** Large fish/mammals with no predators
5. **Cycles:** Some loops expected (omnivory)

**Data Quality:**
1. **No isolated nodes:** All species have ≥1 link
2. **Reasonable diet breadth:** Not too generalist/specialist
3. **Size structure:** Range from MS1 to MS6-MS7
4. **Feeding diversity:** Mix of FS codes

---

## 11. Best Practices

### Trait Assignment

**DO:**
- ✅ Use maximum body size (not average)
- ✅ Assign traits based on adult life stage
- ✅ Use scientific names (not common names)
- ✅ Verify taxonomic classification (WoRMS)
- ✅ Document data sources
- ✅ Include confidence metadata

**DON'T:**
- ❌ Mix juveniles and adults
- ❌ Use outdated taxonomy
- ❌ Guess traits without justification
- ❌ Ignore local/regional variation

### Threshold Selection

**Guidelines:**
1. **Start with 0.05-0.10** (moderate)
2. **Check connectance** (should be 0.1-0.3)
3. **Adjust if needed:**
   - Too sparse? Lower threshold
   - Too dense? Raise threshold
4. **Ecosystem-specific:**
   - Pelagic: 0.05 (high encounter rates)
   - Benthic: 0.10 (lower mobility)
   - Complex: 0.15 (many specialists)

### Quality Control

**Validation Steps:**
1. **Visual inspection:** Check network plot for obvious errors
2. **Connectance:** Compare to empirical food webs
3. **Trophic levels:** Calculate and verify (should span 1-4+)
4. **Omnivory:** Check for reasonable amounts
5. **Expert review:** Have taxonomist/ecologist verify

### Regional Customization

**Harmonization Settings:**
```
Settings > Harmonization
1. Select ecosystem profile (Arctic, Temperate, Tropical, etc.)
2. Adjust size thresholds if needed
3. Toggle taxonomic rules
4. Save custom configuration
```

**Ecosystem-Specific Adjustments:**
- **Arctic:** Larger body sizes, fewer swimmers
- **Tropical:** Smaller sizes, more grazers on reefs
- **Deep sea:** Gigantism, more scavengers

---

## 12. Troubleshooting

### Problem: No interactions predicted

**Causes:**
- Threshold too high
- All species same size class
- No predators (all FS0 or FS6)

**Solutions:**
- Lower threshold to 0.01
- Check trait diversity
- Add predators (FS1)

### Problem: Unrealistic interactions

**Example:** Phytoplankton eating fish

**Causes:**
- Incorrect trait assignment (FS0 should not consume)
- Data entry error

**Solutions:**
- Validate trait data
- Check FS codes (FS0 → 0 consumption probability)
- Review automated harmonization

### Problem: Automated lookup fails

**Causes:**
- Internet connection
- Species name misspelled
- Species not in database

**Solutions:**
- Check connection
- Verify species name on WoRMS
- Use alternative spelling/synonym
- Manual entry as fallback

### Problem: Network too dense/sparse

**Metric:**
- Connectance > 0.5 (too dense)
- Connectance < 0.05 (too sparse)

**Solutions:**
- **Too dense:** Raise threshold, check for over-generalists
- **Too sparse:** Lower threshold, add intermediate species

---

## 13. References

### Primary References

**Olivier, P., et al. (2019)**
"Exploring the temporal variability of a food web using long-term biomonitoring data."
*Ecography*, 42(11), 2107-2121.
https://doi.org/10.1111/ecog.04461

- Original trait-based framework
- Probability matrices
- Harmonization methodology

**Brose, U., et al. (2006)**
"Allometric scaling enhances stability in complex food webs."
*Ecology Letters*, 9(11), 1228-1236.

- Size-structured food webs
- Allometric scaling

**Williams, R. J., & Martinez, N. D. (2000)**
"Simple rules yield complex food webs."
*Nature*, 404(6774), 180-183.

- Niche model
- Food web topology

### Database References

**WoRMS:**
WoRMS Editorial Board (2023). World Register of Marine Species.
http://www.marinespecies.org

**FishBase:**
Froese, R. and D. Pauly. Editors. (2023). FishBase.
www.fishbase.org

**BIOTIC:**
MarLIN (Marine Life Information Network).
https://www.marlin.ac.uk/biotic

**MAREDAT:**
Moriarty, R. and M. O'Brien (2013).
Distribution of mesozooplankton biomass in the global ocean.
PANGAEA.

---

## Appendix A: Full Species Example

**Species: Atlantic Cod (*Gadus morhua*)**

**Raw Database Data:**

*FishBase:*
```
max_length: 200 cm
weight: 96000 g
trophic_level: 4.4
feeding_mode: "predator"
diet: "fish, crustaceans, molluscs"
depth_range: 0-600 m
habitat: "demersal"
body_shape: "fusiform/normal"
```

*WoRMS:*
```
phylum: Chordata
class: Actinopterygii
order: Gadiformes
family: Gadidae
marine: Yes
```

**Harmonization:**

1. **Size (MS):**
   - 200 cm > 150 cm → **MS7**

2. **Foraging (FS):**
   - Pattern match: "predator" → **FS1**
   - Confirmed by TL 4.4 (high carnivore)

3. **Mobility (MB):**
   - Class: Actinopterygii → **MB5** (fish are obligate swimmers)
   - Confirmed by body shape: fusiform

4. **Position (EP):**
   - Habitat: "demersal" + depth range 0-600 m
   - Adult cod benthopelagic → **EP3**
   - (Note: juveniles more pelagic, could be EP4)

5. **Protection (PR):**
   - Fish, no hard structures → **PR0**

**Final Trait Vector:**
```
species: Gadus morhua
MS: MS7
FS: FS1
MB: MB5
EP: EP3
PR: PR0
confidence: high
sources: FishBase, WoRMS
```

**Predicted Interactions:**

*Cod as Consumer (preys on):*
- Herring (MS5, FS6, MB5, EP4, PR0): P = 0.8 (strong)
- Shrimp (MS3, FS5, MB3, EP2, PR4): P = 0.7 (moderate)
- Mussel (MS3, FS6, MB1, EP2, PR6): P = 0.6 (moderate, can crush shell)

*Cod as Resource (preyed on by):*
- Seals (MS7, FS1, MB5, EP4, PR0): P = 0.5 (adult cod vulnerable)
- Young cod vulnerable to larger fish: P varies with size

**Ecological Interpretation:**
- Top predator (FS1, MS7)
- Wide diet breadth (can consume MS2-MS6)
- Benthopelagic (access to benthic + pelagic prey)
- High mobility (can pursue fast prey)
- Large size protects from most predators (except marine mammals)

---

## Appendix B: Glossary

**Adjacency Matrix:** Square matrix A where A[i,j] = 1 if species i eats species j, else 0

**Allometry:** Size-dependent scaling of biological traits

**Basal Species:** Species with no prey (primary producers, detritus)

**Cannibalism:** Intraspecific predation (species eating itself)

**Connectance:** Proportion of realized links out of possible links (L/S²)

**Demersal:** Living on or near the seafloor

**Epibenthic:** Living on the sediment surface

**Food Chain:** Linear sequence of trophic links

**Food Web:** Network of all trophic links in a community

**Functional Group:** Species with similar ecological roles

**Gape Limitation:** Constraint on prey size imposed by predator mouth size

**Harmonization:** Converting raw trait data to standardized categorical codes

**Infaunal:** Living buried in sediment

**Intraguild Predation:** Predation among species that share a resource

**Link:** Directed edge in food web (predator → prey)

**Modularity:** Degree to which network is organized into subgroups

**Nekton:** Actively swimming organisms

**Omnivory:** Feeding at multiple trophic levels

**Pelagic:** Living in the water column

**Plankton:** Drifting organisms (phytoplankton, zooplankton)

**Top Predator:** Species with no predators

**Trophic Level:** Position in food chain (1=producer, 2=herbivore, 3+=carnivore)

---

**End of Methodology Guide**

For questions or issues, consult:
- Quick Start: `docs/TRAIT_LOOKUP_QUICKSTART.md`
- Full Guide: `docs/AUTOMATED_TRAIT_LOOKUP_GUIDE.md`
- Database Setup: `docs/DATABASE_SETUP_GUIDE.md`
- GitHub Issues: Report bugs or request features
