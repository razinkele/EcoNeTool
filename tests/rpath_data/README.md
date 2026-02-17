# Rpath Test Data

This directory contains example datasets from the Rpath package for unit testing.

## Datasets

### 1. REco.params
- **Description:** Georges Bank ecosystem model
- **Groups:** 25 functional groups
- **Stanzas:** 4 multi-stanza groups (Roundfish1, Roundfish2, Flatfish1, Flatfish2)
- **Fleets:** 3 fishing fleets (Trawlers, Midwater, Dredgers)
- **Source:** Rpath package example

### 2. AB.params
- **Description:** Aleutian Basin ecosystem model
- **Source:** Rpath package example

### 3. Ecosense.EBS
- **Description:** Eastern Bering Sea model
- **Purpose:** Ecosense parameter uncertainty analysis
- **Source:** Rpath package example

### 4. Ecosense.ECS
- **Description:** Eastern Chukchi Sea model
- **Purpose:** Ecosense parameter uncertainty analysis
- **Source:** Rpath package example

### 5. Ecosense.GOA
- **Description:** Gulf of Alaska model
- **Purpose:** Ecosense parameter uncertainty analysis
- **Source:** Rpath package example

## CSV Files

The CSV files contain model parameters in tabular format:
- `*_base.csv` - Basic model parameters (Biomass, PB, QB, etc.)
- `*_diet.csv` - Diet composition matrix
- `*_pedigree.csv` - Data quality/pedigree indicators

## XML Files

- `Western_Bering_Sea.eiixml` - EwE interchange format for Western Bering Sea

## Usage in Tests

```r
# Load a test dataset
test_params <- readRDS('tests/rpath_data/REco.params.rds')

# Run Ecopath
test_model <- Rpath::rpath(test_params)

# Run tests
stopifnot(sum(test_model$Biomass) > 0)
```

## References

- Lucey, S.M., Gaichas, S.K., & Aydin, K.Y. (2020). Conducting reproducible
  ecosystem modeling using the open source mass balance model Rpath.
  *Ecological Modelling*, 427, 109057.

