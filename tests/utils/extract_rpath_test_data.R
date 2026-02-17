# Extract Rpath Test Data
# Saves all example datasets from Rpath package for unit testing

library(Rpath)

cat("=== Extracting Rpath Test Data ===\n\n")

# Create directories
if (!dir.exists("tests/rpath_data")) dir.create("tests/rpath_data", recursive = TRUE)

# List of available params objects
params_datasets <- c("REco.params", "AB.params", "Ecosense.EBS", "Ecosense.ECS", "Ecosense.GOA")

# Extract and save each dataset
for (dataset_name in params_datasets) {
  cat("Extracting:", dataset_name, "\n")

  tryCatch({
    # Load the dataset
    dataset <- get(dataset_name, envir = asNamespace("Rpath"))

    # Save as RDS
    rds_file <- file.path("tests/rpath_data", paste0(dataset_name, ".rds"))
    saveRDS(dataset, rds_file)
    cat("  ✓ Saved to:", rds_file, "\n")

    # Print summary
    cat("  - Groups:", nrow(dataset$model), "\n")
    cat("  - Living:", sum(dataset$model$Type < 2), "\n")
    cat("  - Detritus:", sum(dataset$model$Type == 2), "\n")
    cat("  - Fleets:", sum(dataset$model$Type == 3), "\n")

  }, error = function(e) {
    cat("  ✗ Error:", e$message, "\n")
  })

  cat("\n")
}

# Copy CSV files from extdata
cat("Copying CSV files from extdata...\n")
pkg_path <- system.file(package = "Rpath")
extdata_dir <- file.path(pkg_path, "extdata")

if (dir.exists(extdata_dir)) {
  csv_files <- list.files(extdata_dir, pattern = "\\.csv$", full.names = TRUE)

  for (csv_file in csv_files) {
    dest_file <- file.path("tests/rpath_data", basename(csv_file))
    file.copy(csv_file, dest_file, overwrite = TRUE)
    cat("  ✓ Copied:", basename(csv_file), "\n")
  }
}

# Copy XML files
xml_dir <- file.path(extdata_dir, "xml")
if (dir.exists(xml_dir)) {
  xml_files <- list.files(xml_dir, full.names = TRUE)
  xml_dest_dir <- file.path("tests/rpath_data", "xml")
  if (!dir.exists(xml_dest_dir)) dir.create(xml_dest_dir, recursive = TRUE)

  for (xml_file in xml_files) {
    dest_file <- file.path(xml_dest_dir, basename(xml_file))
    file.copy(xml_file, dest_file, overwrite = TRUE)
    cat("  ✓ Copied:", basename(xml_file), "\n")
  }
}

# Create a README for the test data
readme_content <- "# Rpath Test Data

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
"

writeLines(readme_content, "tests/rpath_data/README.md")
cat("\n✓ Created README.md\n")

# Summary
cat("\n=== Extraction Complete ===\n")
cat("Test data saved to: tests/rpath_data/\n")
cat("Files created:\n")
print(list.files("tests/rpath_data", recursive = TRUE))
