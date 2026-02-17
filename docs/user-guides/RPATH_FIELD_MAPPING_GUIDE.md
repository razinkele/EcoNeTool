# Rpath Field Mapping Guide - ECOPATH to Rpath Conversion

**Date:** 2025-12-06
**Purpose:** Detailed field mapping for completing Rpath integration
**Status:** Reference guide for future development

---

## Current Status

### ✅ Completed
- ECOPATH Windows import (RODBC-based) - **WORKING**
- ECOPATH database reading (group_data, diet_data) - **WORKING**
- Rpath package installed (v1.1.0) - **INSTALLED**
- Integration framework created - **COMPLETE**

### ⏳ Pending
- Detailed field mapping from ECOPATH → Rpath format
- Handling of special Rpath structures (stanzas, gears, pedigree)
- Testing with Rpath example datasets

---

## Challenge Identified

The Rpath package (v1.1.0) uses `create.rpath.params()` which requires specific data structures that are more detailed than a simple field-to-field mapping. The function expects:

```r
params <- create.rpath.params(
  group = character_vector,    # Group names
  type = numeric_vector,        # Group types (0=consumer, 1=producer, 2=detritus)
  stgroup = character_vector    # Stanza group names
)
```

However, the created params object has complex nested structure with multiple components that must be properly populated:
- `params$model` - Data frame with all biological parameters
- `params$diet` - Diet matrix (prey×predator)
- `params$stanzas` - Multi-stanza species data
- `params$pedigree` - Data quality/uncertainty
- And potentially others

---

## ECOPATH Database Structure

### What We Successfully Import

**From EcopathGroup table (51 rows × 43 columns):**

```r
data$group_data contains:
- GroupID (integer): 1, 2, 3, ...
- GroupName (character): "Detritus", "Phytoplankton", "Mesozooplankton", ...
- Type (integer): 2 (detritus), 1 (producer), 0 (consumer)
- Biomass (numeric): tons/km² (or -9999 for missing)
- ProdBiom (numeric): P/B ratio (or -9999)
- ConsBiom (numeric): Q/B ratio (or -9999)
- EcoEfficiency (numeric): Ecotrophic efficiency
- BiomAcc (numeric): Biomass accumulation
- BiomAccRate (numeric): Accumulation rate
- Unassim (numeric): Unassimilated/Consumption ratio
- DtImports (numeric): Detritus imports
- Export (numeric): Exports
- Catch (numeric): Catches/landings
- Immigration, Emigration (numeric)
- Respiration, Consumption, Production, Unassimilated
- vbK, Loo, Winf, t0 (numeric): von Bertalanffy growth parameters
- AinLW, BinLW: Length-weight parameters
- And 20+ more fields...
```

**From EcopathDietComp table (2,601 rows × 4-6 columns):**

```r
data$diet_data contains:
- PredID (integer): Predator group ID
- PreyID (integer): Prey group ID
- Diet (numeric): Diet proportion (0-1)
- DetritusFate (integer): Detritus fate code
- MTI (numeric, optional): Mixed Trophic Impact
- Electivity (numeric, optional): Electivity index
```

---

## Rpath Expected Structure

### What Rpath Needs (from documentation)

Based on Rpath package structure and example datasets, the `params` object needs:

#### 1. `params$model` - Main parameter data frame

Must include columns (from Rpath::rpath.groups, rpath.consumers, etc.):

**Basic Parameters:**
- `Group` (character): Group names
- `Type` (integer): 0=consumer, 1=producer, 2=detritus
- `TL` (numeric): Trophic level (calculated or estimated)
- `Biomass` (numeric): Biomass (tons/km²)
- `PB` (numeric): Production/Biomass ratio (1/year)
- `QB` (numeric): Consumption/Biomass ratio (1/year) - **consumers only**
- `EE` (numeric): Ecotrophic efficiency (0-1)

**Optional/Advanced:**
- `BioAcc` (numeric): Biomass accumulation
- `Unassim` (numeric): Unassimilated fraction
- `DetInput` (numeric): Detritus input
- `Detritus` (numeric): Detritus export fate
- `DetFate` (numeric): Detritus fate
- `Catch` (numeric): Catches
- `Immigration`, `Emigration` (numeric)

**Gear/Fleet Columns:**
- For each fishing gear: `Gear1`, `Gear2`, etc. (catches per gear)
- `Landing` vs `Discards` for each gear

**Stanza Parameters:**
- `Stanza` (character): Stanza group name
- `StanzaNum` (integer): Stanza number within group
- `ageStart`, `ageEnd` (numeric): Age range
- `vbK` (numeric): von Bertalanffy K
- `Wmat` (numeric): Weight at maturity

#### 2. `params$diet` - Diet matrix

**Structure:**
- Data frame: rows = prey, columns = predators
- Rownames = prey group names
- Colnames = predator group names
- Values = diet proportions (each column sums to 1)

**Example:**
```r
              Cod    Herring    Sprat
Phytoplankton 0.00   0.00       0.15
Zooplankton   0.20   0.40       0.60
Benthos       0.50   0.30       0.15
Herring       0.30   0.00       0.00
Sprat         0.00   0.30       0.10
```

#### 3. `params$stanzas` (optional) - Multi-stanza species

List of stanza groups with parameters:
```r
params$stanzas <- list(
  list(
    StanzaName = "Cod",
    nstanzas = 3,
    VBGF_Ksp = 0.15,      # von Bertalanffy K
    Wmat = 500,           # Weight at maturity (g)
    RecPower = 1,         # Recruitment power
    ...
  ),
  ...
)
```

#### 4. `params$pedigree` (optional) - Data quality

Data frame with uncertainty estimates:
```r
params$pedigree <- data.frame(
  Group = group_names,
  B = uncertainty_B,        # Biomass confidence
  PB = uncertainty_PB,      # P/B confidence
  QB = uncertainty_QB,      # Q/B confidence
  Diet = uncertainty_Diet   # Diet confidence
)
```

---

## Field Mapping: ECOPATH → Rpath

### Direct Mappings (Straightforward)

| ECOPATH Column | Rpath Column | Notes |
|----------------|--------------|-------|
| `GroupName` | `Group` | Direct copy |
| `Type` | `Type` | Direct copy (0/1/2) |
| `Biomass` | `Biomass` | Clean -9999 → NA or estimate |
| `ProdBiom` | `PB` | Clean -9999 → NA or estimate |
| `ConsBiom` | `QB` | Clean -9999 → NA or estimate (consumers only) |
| `EcoEfficiency` | `EE` | Clean -9999 → NA or calculate |
| `BiomAcc` | `BioAcc` | Direct if available |
| `BiomAccRate` | (calculated) | May need to derive |
| `Unassim` | `Unassim` | Direct if available |
| `Catch` | `Catch` | Direct if available |
| `Immigration` | `Immigration` | Direct if available |
| `Emigration` | `Emigration` | Direct if available |

### Complex Mappings (Need Processing)

#### Trophic Level (TL)
**ECOPATH:** Not directly stored, calculated from diet
**Rpath:** Expected in `params$model$TL`

**Solution:**
```r
# Calculate TL from diet matrix
# TL = 1 + weighted average of prey TL
calculate_tl <- function(diet_matrix) {
  # Iterative calculation
  # Start: producers = 1, detritus = 1, consumers = 2
  # Then refine based on diet
}
```

#### Diet Matrix Conversion
**ECOPATH:** Long format (PredID, PreyID, Diet)
**Rpath:** Wide format (prey × predator matrix)

**Solution:**
```r
convert_diet_to_matrix <- function(diet_data, group_data) {
  n_groups <- nrow(group_data)

  # Create empty matrix
  diet_matrix <- matrix(0, nrow = n_groups, ncol = n_groups)
  rownames(diet_matrix) <- group_data$GroupName
  colnames(diet_matrix) <- group_data$GroupName

  # Fill from diet_data
  for (i in 1:nrow(diet_data)) {
    pred_idx <- which(group_data$GroupID == diet_data$PredID[i])
    prey_idx <- which(group_data$GroupID == diet_data$PreyID[i])

    if (length(pred_idx) > 0 && length(prey_idx) > 0) {
      diet_matrix[prey_idx, pred_idx] <- diet_data$Diet[i]
    }
  }

  # Normalize columns to sum to 1
  for (j in 1:ncol(diet_matrix)) {
    col_sum <- sum(diet_matrix[, j])
    if (col_sum > 0) {
      diet_matrix[, j] <- diet_matrix[, j] / col_sum
    }
  }

  return(diet_matrix)
}
```

#### Missing Value Handling
**ECOPATH:** Uses -9999 or -9999.00 as sentinel for missing
**Rpath:** Expects NA or valid values

**Solution:**
```r
clean_ecopath_missing <- function(x, type = "biomass") {
  # Replace sentinel values
  x[x < -9000] <- NA

  # Apply defaults based on type
  if (type == "biomass") {
    x[is.na(x)] <- 1  # Default biomass
  } else if (type == "PB") {
    x[is.na(x)] <- 0.5  # Default P/B
  } else if (type == "QB") {
    x[is.na(x)] <- 1.5  # Default Q/B
  } else if (type == "EE") {
    x[is.na(x)] <- 0.5  # Default EE (will be calculated)
  }

  return(x)
}
```

### Special Structures

#### 1. Stanza Groups (Age-Structured Species)

**Identify stanzas:**
- Check if `StanzaID` column exists in ECOPATH
- Group species by StanzaID
- Extract growth parameters (vbK, Loo, Winf, t0)

**Build stanza list:**
```r
create_stanza_params <- function(group_data) {
  if (!"StanzaID" %in% names(group_data)) return(NULL)

  stanzas <- list()
  stanza_ids <- unique(group_data$StanzaID[group_data$StanzaID > 0])

  for (sid in stanza_ids) {
    members <- group_data[group_data$StanzaID == sid, ]

    stanzas[[length(stanzas) + 1]] <- list(
      StanzaName = members$GroupName[1],  # Base name
      nstanzas = nrow(members),
      VBGF_Ksp = mean(members$vbK, na.rm = TRUE),
      Wmat = mean(members$Winf, na.rm = TRUE),
      RecPower = 1  # Default
    )
  }

  return(stanzas)
}
```

#### 2. Fishing Gears/Fleets

**ECOPATH:** May have separate fleet/gear tables (EcopathFleet, EcopathCatch)
**Rpath:** Expects gear columns in params$model

**Approach:**
1. Check if fleet tables exist in database
2. Extract catch by fleet and species
3. Add gear columns to params$model
4. Populate with catch values

**Note:** This requires reading additional ECOPATH tables beyond EcopathGroup and EcopathDietComp.

#### 3. Pedigree (Uncertainty)

**ECOPATH:** May have pedigree table or CV columns
**Rpath:** Optional params$pedigree data frame

**If available:**
```r
params$pedigree <- data.frame(
  Group = group_data$GroupName,
  B = group_data$BiomassCV,    # If column exists
  PB = group_data$ProdBiomCV,   # If column exists
  QB = group_data$ConsBiomCV    # If column exists
)
```

---

## Implementation Strategy

### Phase 1: Basic Conversion (Minimal Rpath)

Focus on core parameters only:

```r
convert_ecopath_to_rpath_basic <- function(ecopath_data) {
  library(Rpath)

  groups <- ecopath_data$group_data
  diet <- ecopath_data$diet_data

  # Filter living groups
  living <- groups[groups$Type >= 0, ]

  # Create params
  params <- create.rpath.params(
    group = living$GroupName,
    type = living$Type,
    stgroup = living$GroupName  # Simple case: no multi-stanza
  )

  # Populate basic parameters
  params$model$Biomass <- clean_ecopath_missing(living$Biomass, "biomass")
  params$model$PB <- clean_ecopath_missing(living$ProdBiom, "PB")

  # QB only for consumers
  consumers <- living$Type == 0
  params$model$QB[consumers] <- clean_ecopath_missing(living$ConsBiom[consumers], "QB")

  # EE can be calculated or set
  params$model$EE <- clean_ecopath_missing(living$EcoEfficiency, "EE")

  # Convert diet matrix
  params$diet <- convert_diet_to_matrix(diet, living)

  return(params)
}
```

### Phase 2: Advanced Features

Add stanzas, gears, pedigree:

1. Parse stanza information
2. Read fleet/gear tables
3. Add uncertainty/pedigree data
4. Validate parameter ranges

### Phase 3: Validation

Test with Rpath's built-in checks:

```r
# Validate parameters
check.rpath.params(params)

# Run mass balance
model <- rpath(params)

# Check results
model$Group
model$Biomass
model$EE  # Should be 0-1
```

---

## Testing Approach

### Step 1: Start with Rpath Examples

Learn from Rpath's built-in datasets:

```r
library(Rpath)

# Load example dataset
data(package = "Rpath")  # List available datasets

# Study structure
# Look at how params are structured
# Understand required vs optional fields
```

### Step 2: Minimal Test Case

Create smallest possible working example:

```r
# 3 groups: producer, consumer, detritus
# Simple diet: consumer eats producer
# Check if it balances
```

### Step 3: Real ECOPATH Data

Use imported ECOPATH data with basic mapping:

```r
# Import ECOPATH
data <- parse_ecopath_native_cross_platform("coast 2011-04-10 10.00.ewemdb")

# Convert with basic mapping
params <- convert_ecopath_to_rpath_basic(data)

# Validate
check.rpath.params(params)

# Run mass balance
model <- rpath(params)
```

### Step 4: Iterative Refinement

Debug and improve field mapping based on errors.

---

## Resources

### Rpath Documentation
- Official site: https://noaa-edab.github.io/Rpath/
- GitHub: https://github.com/NOAA-EDAB/Rpath
- Vignettes: `browseVignettes("Rpath")`

### Key Vignettes to Study
```r
# After installing Rpath
vignette("Rpath", package = "Rpath")          # Introduction
vignette("RpathModels", package = "Rpath")    # Model structure
vignette("Ecosim", package = "Rpath")         # Simulations
```

### Example Code from Rpath
Look at package source code:
```r
# Find installed package location
system.file(package = "Rpath")

# Study example data structures
data(package = "Rpath")

# Examine function source
Rpath::create.rpath.params
Rpath::rpath
```

---

## Next Steps for Completion

### Immediate (2-4 hours)
1. Study Rpath vignettes and example datasets
2. Create minimal working example (3-group model)
3. Test basic conversion with one of your ECOPATH files
4. Debug field mapping based on errors

### Short-term (1-2 days)
5. Implement full field mapping
6. Handle missing values robustly
7. Add stanza support
8. Test with multiple ECOPATH databases

### Medium-term (1 week)
9. Add fleet/gear support
10. Implement pedigree/uncertainty
11. Validate with mass-balance checks
12. Create comprehensive test suite

---

## Current Implementation

The current `convert_ecopath_to_rpath()` function in `rpath_integration.R` has the right structure but needs the detailed field mappings described in this guide.

**Specific line to update:** `rpath_integration.R` line 91-95

**From:**
```r
params <- Rpath::create.rpath.params(
  group = living_groups$GroupName,
  type = living_groups$Type,
  stgroup = living_groups$GroupName
)
```

**Needs to become:**
Complete implementation following Phase 1 strategy above, with proper field mapping and cleaning.

---

## Conclusion

The Rpath integration framework is complete, but the detailed field mapping requires:

1. **Understanding Rpath's exact requirements** (study vignettes and examples)
2. **Proper data structure transformation** (ECOPATH → Rpath format)
3. **Missing value handling** (-9999 cleanup)
4. **Diet matrix conversion** (long → wide format)
5. **Validation and testing** (check.rpath.params, rpath)

**Estimated time to complete:** 4-8 hours of focused development

**Recommended approach:** Start with minimal example, test, expand iteratively.

**The ECOPATH Windows import is ready and working - this Rpath mapping is a separate enhancement that can be completed when needed.**
