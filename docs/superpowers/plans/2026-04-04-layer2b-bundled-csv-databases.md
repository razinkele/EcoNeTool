# Layer 2b: Bundled CSV Trait Databases

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Integrate 5 bundled CSV trait databases (Black Sea, Arctic, Cefas NW Europe, Coral Trait DB, Pelagic) with lookup functions, orchestrator routing, and harmonization of RS/TT/ST traits from these sources.

**Architecture:** Each database gets a `lookup_<db>_traits()` function following the existing pattern (returns `list(species, source, success, traits)`). CSVs are stored in `data/external_traits/` and loaded on first access via in-memory caching. Fuzzy-coded databases (Black Sea, Arctic) use the same parsing pattern as BIOTIC. Orchestrator routing adds 5 new boolean flags.

**Tech Stack:** R 4.4.1, testthat. Rscript: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe"`

**Prerequisite:** Layer 2a (expanded result template, RS/TT/ST harmonization functions) must be applied.

**Data prerequisite:** CSV files must be downloaded manually before implementation. See Task 1.

---

## File Structure

| File | Action | Responsibility |
|------|--------|---------------|
| `data/external_traits/` | Create dir | Store bundled CSV files |
| `data/external_traits/README.md` | Create | Document sources, download URLs, licenses |
| `R/functions/trait_lookup/csv_trait_databases.R` | Create | 5 lookup functions for bundled CSVs |
| `R/functions/trait_lookup/load_all.R` | Modify | Source the new file |
| `R/functions/trait_lookup/orchestrator.R` | Modify | Add 5 routing flags + lookup calls |
| `tests/testthat/test-layer2b-csv-databases.R` | Create | Tests for all 5 lookup functions |

---

### Task 1: Download and prepare CSV data files

**Problem:** The 5 CSV databases must be downloaded from their sources and placed in `data/external_traits/`.

- [ ] **Step 1: Create the directory**

```bash
mkdir -p data/external_traits
```

- [ ] **Step 2: Create README.md with download instructions**

Create `data/external_traits/README.md`:

```markdown
# External Trait Databases

Bundled CSV trait databases for EcoNeTool. Each must be downloaded manually.

## Downloads

| Database | URL | Format | License |
|---|---|---|---|
| Black Sea Traits | https://blackseatraits.com/ | Fuzzy CSV (0-3) | CC-BY |
| Arctic Traits | https://arctictraits.univie.ac.at/ | Fuzzy CSV (0-3) | CC-BY |
| Cefas NW Europe Benthic | https://data.cefas.co.uk/view/21362345 | Fuzzy CSV (0-3) | OGL |
| Coral Trait DB | https://coraltraits.org/ | Standard CSV | CC-BY |
| Pelagic Trait DB | https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/0YFJED | Standard CSV | CC-BY |

## File naming convention

Place downloaded files here with these names:
- `blacksea_traits.csv`
- `arctic_traits.csv`
- `cefas_benthic_traits.csv`
- `coral_traits.csv` (pre-filter to European/Mediterranean species if >10MB)
- `pelagic_traits.csv`

## Notes
- Fuzzy-coded databases use scores 0-3 (0=no affinity, 3=high affinity)
- All taxonomy synchronized with WoRMS
- Coral Trait DB: bulk download may be large (~40MB). Pre-filter recommended.
```

- [ ] **Step 3: Download the CSV files**

This is a manual step. Download each file from the URLs above and rename per convention. If files are not yet available, create minimal placeholder CSVs for development:

```bash
# Placeholder for development (replace with real data)
echo "species,feeding_mode,mobility,body_size_mm,reproductive_mode,temperature_affinity,salinity_affinity" > data/external_traits/blacksea_traits.csv
echo "species,feeding_mode,mobility,body_size_mm,reproductive_mode,temperature_preference" > data/external_traits/arctic_traits.csv
echo "species,body_size,feeding_mode,mobility,lifespan,larval_development,living_habit" > data/external_traits/cefas_benthic_traits.csv
echo "species,growth_form,reproductive_mode,thermal_tolerance_max,depth_lower,depth_upper" > data/external_traits/coral_traits.csv
echo "species,habitat_use,morphology,body_length_cm,nutritional_quality" > data/external_traits/pelagic_traits.csv
```

- [ ] **Step 4: Add to .gitignore if files >5MB, commit directory structure**

```bash
git add data/external_traits/README.md data/external_traits/*.csv
git commit -m "$(cat <<'EOF'
feat(data): add external trait database directory with placeholder CSVs

Download instructions in README.md. Placeholder CSVs for development.
Replace with real data from: Black Sea Traits, Arctic Traits, Cefas,
Coral Trait DB, Pelagic Trait DB.

Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 2: Create csv_trait_databases.R with 5 lookup functions

**Problem:** Need 5 new `lookup_<db>_traits()` functions following the existing pattern.

**Files:**
- Create: `R/functions/trait_lookup/csv_trait_databases.R`
- Test: `tests/testthat/test-layer2b-csv-databases.R`

- [ ] **Step 1: Create test file**

```r
# Tests for Layer 2b: Bundled CSV trait databases
library(testthat)

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
source(file.path(app_root, "R/config.R"))
source(file.path(app_root, "R/functions/validation_utils.R"))
source(file.path(app_root, "R/functions/functional_group_utils.R"))
source(file.path(app_root, "R/config/harmonization_config.R"))
source(file.path(app_root, "R/functions/trait_lookup/harmonization.R"))
source(file.path(app_root, "R/functions/trait_lookup/csv_trait_databases.R"))

test_that("lookup_blacksea_traits returns correct structure", {
  csv_path <- file.path(app_root, "data/external_traits/blacksea_traits.csv")
  skip_if_not(file.exists(csv_path), "Black Sea CSV not available")
  result <- lookup_blacksea_traits("Nonexistent_species", csv_file = csv_path)
  expect_true(is.list(result))
  expect_equal(result$source, "BlackSea")
  expect_false(result$success)
  expect_true(is.list(result$traits))
})

test_that("lookup_arctic_traits returns correct structure", {
  csv_path <- file.path(app_root, "data/external_traits/arctic_traits.csv")
  skip_if_not(file.exists(csv_path), "Arctic CSV not available")
  result <- lookup_arctic_traits("Nonexistent_species", csv_file = csv_path)
  expect_true(is.list(result))
  expect_equal(result$source, "ArcticTraits")
  expect_false(result$success)
})

test_that("lookup_cefas_traits returns correct structure", {
  csv_path <- file.path(app_root, "data/external_traits/cefas_benthic_traits.csv")
  skip_if_not(file.exists(csv_path), "Cefas CSV not available")
  result <- lookup_cefas_traits("Nonexistent_species", csv_file = csv_path)
  expect_true(is.list(result))
  expect_equal(result$source, "Cefas")
  expect_false(result$success)
})

test_that("lookup_coral_traits returns correct structure", {
  csv_path <- file.path(app_root, "data/external_traits/coral_traits.csv")
  skip_if_not(file.exists(csv_path), "Coral CSV not available")
  result <- lookup_coral_traits("Nonexistent_species", csv_file = csv_path)
  expect_true(is.list(result))
  expect_equal(result$source, "CoralTraits")
  expect_false(result$success)
})

test_that("lookup_pelagic_traits returns correct structure", {
  csv_path <- file.path(app_root, "data/external_traits/pelagic_traits.csv")
  skip_if_not(file.exists(csv_path), "Pelagic CSV not available")
  result <- lookup_pelagic_traits("Nonexistent_species", csv_file = csv_path)
  expect_true(is.list(result))
  expect_equal(result$source, "PelagicTraits")
  expect_false(result$success)
})

test_that("all 5 CSV lookup functions exist", {
  expect_true(exists("lookup_blacksea_traits", mode = "function"))
  expect_true(exists("lookup_arctic_traits", mode = "function"))
  expect_true(exists("lookup_cefas_traits", mode = "function"))
  expect_true(exists("lookup_coral_traits", mode = "function"))
  expect_true(exists("lookup_pelagic_traits", mode = "function"))
})
```

- [ ] **Step 2: Create csv_trait_databases.R**

Create `R/functions/trait_lookup/csv_trait_databases.R`:

```r
# =============================================================================
# Bundled CSV Trait Database Lookups
# =============================================================================
# Lookup functions for 5 external CSV trait databases stored in
# data/external_traits/. Each follows the standard interface:
#   list(species, source, success, traits)
#
# Databases:
#   1. Black Sea Traits (fuzzy-coded, benthic invertebrates)
#   2. Arctic Traits (fuzzy-coded, Arctic benthic invertebrates)
#   3. Cefas NW Europe Benthic (fuzzy-coded, 1025 taxa, 10 traits)
#   4. Coral Trait DB (standard CSV, 5112 corals)
#   5. Pelagic Trait DB (standard CSV, 529 pelagic species)
# =============================================================================

# In-memory caches (loaded once per session)
.csv_cache <- new.env(parent = emptyenv())

#' Load a CSV file with caching
#' @param file_path Character, path to CSV file
#' @param cache_key Character, key for in-memory cache
#' @return data.frame or NULL
.load_csv_cached <- function(file_path, cache_key) {
  if (!is.null(.csv_cache[[cache_key]])) {
    return(.csv_cache[[cache_key]])
  }
  if (!file.exists(file_path)) return(NULL)
  data <- tryCatch(
    read.csv(file_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
    error = function(e) { message("Error reading ", file_path, ": ", e$message); NULL }
  )
  if (!is.null(data)) .csv_cache[[cache_key]] <- data
  data
}

#' Find a species in a data.frame (exact then genus match)
#' @param data data.frame with a species column
#' @param species_name Character, species to find
#' @param species_col Character, name of species column (default "species")
#' @return data.frame row or NULL
.find_species <- function(data, species_name, species_col = "species") {
  if (!species_col %in% names(data)) {
    # Try common alternatives
    for (alt in c("Species", "taxon", "Taxon", "scientific_name", "ScientificName")) {
      if (alt %in% names(data)) { species_col <- alt; break }
    }
  }
  if (!species_col %in% names(data)) return(NULL)

  # Exact match (case-insensitive)
  idx <- which(tolower(data[[species_col]]) == tolower(species_name))
  if (length(idx) > 0) return(data[idx[1], , drop = FALSE])

  # Genus match
  genus <- strsplit(species_name, " ")[[1]][1]
  idx <- which(grepl(paste0("^", genus), data[[species_col]], ignore.case = TRUE))
  if (length(idx) > 0) return(data[idx[1], , drop = FALSE])

  NULL
}

# =============================================================================
# 1. BLACK SEA TRAITS
# =============================================================================

#' Lookup traits from Black Sea Traits Database
#' @param species_name Character, species to look up
#' @param csv_file Character, path to CSV file
#' @return list(species, source, success, traits)
lookup_blacksea_traits <- function(species_name, csv_file = "data/external_traits/blacksea_traits.csv") {
  result <- list(species = species_name, source = "BlackSea", success = FALSE, traits = list())
  data <- .load_csv_cached(csv_file, "blacksea")
  if (is.null(data)) return(result)

  row <- .find_species(data, species_name)
  if (is.null(row)) return(result)

  traits <- list()
  # Extract available traits (column names vary by download format)
  for (col in names(row)) {
    val <- row[[col]]
    if (!is.na(val) && val != "") {
      col_lower <- tolower(col)
      if (grepl("feed|diet|troph", col_lower)) traits$feeding_mode <- val
      if (grepl("mobil|move", col_lower)) traits$mobility_info <- val
      if (grepl("size|length|body", col_lower) && is.numeric(val)) traits$size_mm <- val
      if (grepl("reproduct|spawn|brood", col_lower)) traits$reproductive_mode <- val
      if (grepl("temp", col_lower)) traits$temperature_affinity <- val
      if (grepl("salin", col_lower)) traits$salinity_affinity <- val
      if (grepl("depth", col_lower) && is.numeric(val)) traits$depth_info <- val
    }
  }

  result$traits <- traits
  result$success <- length(traits) > 0
  result
}

# =============================================================================
# 2. ARCTIC TRAITS
# =============================================================================

#' Lookup traits from Arctic Traits Database
#' @param species_name Character, species to look up
#' @param csv_file Character, path to CSV file
#' @return list(species, source, success, traits)
lookup_arctic_traits <- function(species_name, csv_file = "data/external_traits/arctic_traits.csv") {
  result <- list(species = species_name, source = "ArcticTraits", success = FALSE, traits = list())
  data <- .load_csv_cached(csv_file, "arctic")
  if (is.null(data)) return(result)

  row <- .find_species(data, species_name)
  if (is.null(row)) return(result)

  traits <- list()
  for (col in names(row)) {
    val <- row[[col]]
    if (!is.na(val) && val != "") {
      col_lower <- tolower(col)
      if (grepl("feed|diet|troph", col_lower)) traits$feeding_mode <- val
      if (grepl("mobil|move", col_lower)) traits$mobility_info <- val
      if (grepl("size|length|body", col_lower) && is.numeric(val)) traits$size_mm <- val
      if (grepl("reproduct|spawn|brood|larv", col_lower)) traits$reproductive_mode <- val
      if (grepl("temp", col_lower)) traits$temperature_preference <- val
    }
  }

  # Arctic species are cold-adapted by definition
  if (is.null(traits$temperature_preference)) {
    traits$temperature_preference <- "arctic"
  }

  result$traits <- traits
  result$success <- length(traits) > 0
  result
}

# =============================================================================
# 3. CEFAS NW EUROPE BENTHIC
# =============================================================================

#' Lookup traits from Cefas NW Europe Benthic Traits
#' @param species_name Character, species to look up
#' @param csv_file Character, path to CSV file
#' @return list(species, source, success, traits)
lookup_cefas_traits <- function(species_name, csv_file = "data/external_traits/cefas_benthic_traits.csv") {
  result <- list(species = species_name, source = "Cefas", success = FALSE, traits = list())
  data <- .load_csv_cached(csv_file, "cefas")
  if (is.null(data)) return(result)

  row <- .find_species(data, species_name)
  if (is.null(row)) return(result)

  traits <- list()
  for (col in names(row)) {
    val <- row[[col]]
    if (!is.na(val) && val != "") {
      col_lower <- tolower(col)
      if (grepl("feed|diet", col_lower)) traits$feeding_mode <- val
      if (grepl("mobil", col_lower)) traits$mobility_info <- val
      if (grepl("size|body", col_lower) && is.numeric(val)) traits$size_mm <- val
      if (grepl("lifespan|longevity", col_lower) && is.numeric(val)) traits$longevity_years <- val
      if (grepl("larv", col_lower)) traits$larval_development <- val
      if (grepl("reproduct", col_lower)) traits$reproductive_mode <- val
      if (grepl("living.habit|habit", col_lower)) traits$living_habit <- val
      if (grepl("bioturbat", col_lower)) traits$bioturbation_mode <- val
    }
  }

  result$traits <- traits
  result$success <- length(traits) > 0
  result
}

# =============================================================================
# 4. CORAL TRAIT DB
# =============================================================================

#' Lookup traits from Coral Trait Database
#' @param species_name Character, species to look up
#' @param csv_file Character, path to CSV file
#' @return list(species, source, success, traits)
lookup_coral_traits <- function(species_name, csv_file = "data/external_traits/coral_traits.csv") {
  result <- list(species = species_name, source = "CoralTraits", success = FALSE, traits = list())
  data <- .load_csv_cached(csv_file, "coral")
  if (is.null(data)) return(result)

  row <- .find_species(data, species_name)
  if (is.null(row)) return(result)

  traits <- list()
  for (col in names(row)) {
    val <- row[[col]]
    if (!is.na(val) && val != "") {
      col_lower <- tolower(col)
      if (grepl("growth.form|morphology", col_lower)) traits$growth_form <- val
      if (grepl("reproduct|spawn|brood", col_lower)) traits$reproductive_mode <- val
      if (grepl("thermal|bleach|temp.*max|temp.*tol", col_lower) && is.numeric(val)) traits$thermal_tolerance <- val
      if (grepl("depth.*low|depth.*min", col_lower) && is.numeric(val)) traits$depth_min <- val
      if (grepl("depth.*up|depth.*max", col_lower) && is.numeric(val)) traits$depth_max <- val
    }
  }

  result$traits <- traits
  result$success <- length(traits) > 0
  result
}

# =============================================================================
# 5. PELAGIC TRAIT DB
# =============================================================================

#' Lookup traits from Pelagic Species Trait Database
#' @param species_name Character, species to look up
#' @param csv_file Character, path to CSV file
#' @return list(species, source, success, traits)
lookup_pelagic_traits <- function(species_name, csv_file = "data/external_traits/pelagic_traits.csv") {
  result <- list(species = species_name, source = "PelagicTraits", success = FALSE, traits = list())
  data <- .load_csv_cached(csv_file, "pelagic")
  if (is.null(data)) return(result)

  row <- .find_species(data, species_name)
  if (is.null(row)) return(result)

  traits <- list()
  for (col in names(row)) {
    val <- row[[col]]
    if (!is.na(val) && val != "") {
      col_lower <- tolower(col)
      if (grepl("habitat|zone", col_lower)) traits$habitat_use <- val
      if (grepl("morphol|shape", col_lower)) traits$morphology <- val
      if (grepl("length|size|body", col_lower) && is.numeric(val)) traits$body_length_cm <- val
      if (grepl("nutrit|quality", col_lower)) traits$nutritional_quality <- val
      if (grepl("feed|diet|troph", col_lower)) traits$feeding_mode <- val
    }
  }

  result$traits <- traits
  result$success <- length(traits) > 0
  result
}
```

- [ ] **Step 3: Parse check and run tests**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='R/functions/trait_lookup/csv_trait_databases.R'); cat('OK\n')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer2b-csv-databases.R')"
```

- [ ] **Step 4: Commit**

```bash
git add R/functions/trait_lookup/csv_trait_databases.R tests/testthat/test-layer2b-csv-databases.R
git commit -m "$(cat <<'EOF'
feat(traits): add 5 bundled CSV trait database lookup functions

lookup_blacksea_traits(), lookup_arctic_traits(), lookup_cefas_traits(),
lookup_coral_traits(), lookup_pelagic_traits() — all follow standard
interface (species, source, success, traits). Uses in-memory caching
and flexible column name matching.

Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 3: Update load_all.R to source new file

**Files:**
- Modify: `R/functions/trait_lookup/load_all.R`

- [ ] **Step 1: Add source line**

In `R/functions/trait_lookup/load_all.R`, after the `source("R/functions/trait_lookup/database_lookups.R")` line, add:

```r
# Bundled CSV trait databases (Black Sea, Arctic, Cefas, Coral, Pelagic)
source("R/functions/trait_lookup/csv_trait_databases.R")
```

Update the message at the end from `"3 files"` to `"4 files"`.

- [ ] **Step 2: Parse check, commit**

```bash
git commit -m "feat(traits): source csv_trait_databases.R in load_all.R"
```

---

### Task 4: Add orchestrator routing for 5 CSV databases

**Problem:** The orchestrator needs flags and lookup calls for the 5 new databases.

**Files:**
- Modify: `R/functions/trait_lookup/orchestrator.R`
- Test: `tests/testthat/test-layer2b-csv-databases.R` (append)

- [ ] **Step 1: Append routing test**

```r
test_that("orchestrator has CSV database routing flags", {
  orch_text <- readLines(file.path(app_root, "R/functions/trait_lookup/orchestrator.R"))
  orch_joined <- paste(orch_text, collapse = "\n")
  expect_true(grepl("query_blacksea", orch_joined), info = "Missing query_blacksea flag")
  expect_true(grepl("query_arctic", orch_joined), info = "Missing query_arctic flag")
  expect_true(grepl("query_cefas", orch_joined), info = "Missing query_cefas flag")
  expect_true(grepl("query_coral", orch_joined), info = "Missing query_coral flag")
  expect_true(grepl("query_pelagic", orch_joined), info = "Missing query_pelagic flag")
})
```

- [ ] **Step 2: Add 5 new flags to orchestrator initialization**

In `R/functions/trait_lookup/orchestrator.R`, find the flag initialization block (around line 310-319). After the existing flags, add:

```r
  query_blacksea <- FALSE
  query_arctic <- FALSE
  query_cefas <- FALSE
  query_coral <- FALSE
  query_pelagic <- FALSE
```

- [ ] **Step 3: Add routing conditions**

In the taxonomy routing block:

**Marine invertebrates branch** (after `query_biotic <- TRUE`): add:
```r
      query_cefas <- TRUE
      query_blacksea <- TRUE
```

**Zooplankton branch**: add nothing (zooplankton not in these DBs).

**Phytoplankton branch**: add nothing.

**Corals** — add a new sub-branch inside marine invertebrates for Anthozoa:
```r
      if (class == "anthozoa") {
        query_coral <- TRUE
        message("  -> Anthozoa detected: also querying Coral Trait DB")
      }
```

**Arctic species** — add in the marine invertebrate branch:
```r
      # Arctic/Nordic species get Arctic Traits DB
      query_arctic <- TRUE  # Will return empty for non-Arctic species
```

**Pelagic fish** — add in the fish branch:
```r
      query_pelagic <- TRUE
```

**Fallback branch**: add all 5:
```r
      query_blacksea <- TRUE
      query_arctic <- TRUE
      query_cefas <- TRUE
      query_coral <- TRUE
      query_pelagic <- TRUE
```

- [ ] **Step 4: Add lookup execution blocks**

After the existing database lookup blocks (after the BIOTIC section), add 5 new lookup blocks. Each follows this pattern:

```r
  # CSV: Black Sea Traits
  if (query_blacksea) {
    message("\n[CSV] Black Sea Traits DB...")
    db_start <- Sys.time()
    blacksea_data <- lookup_blacksea_traits(species_name)
    if (blacksea_data$success) {
      raw_traits$blacksea <- blacksea_data$traits
      sources_used <- c(sources_used, "BlackSea")
      if (!is.null(blacksea_data$traits$feeding_mode)) feeding_mode <- c(feeding_mode, blacksea_data$traits$feeding_mode)
      if (!is.null(blacksea_data$traits$mobility_info)) mobility_info <- c(mobility_info, blacksea_data$traits$mobility_info)
      if (!is.null(blacksea_data$traits$reproductive_mode)) {
        result$RS <- harmonize_reproductive_strategy(blacksea_data$traits$reproductive_mode)
      }
      if (!is.null(blacksea_data$traits$temperature_affinity)) {
        result$TT <- harmonize_temperature_tolerance(blacksea_data$traits$temperature_affinity)
      }
      if (!is.null(blacksea_data$traits$salinity_affinity)) {
        result$ST <- harmonize_salinity_tolerance(blacksea_data$traits$salinity_affinity)
      }
      message("    Found: ", paste(names(blacksea_data$traits), collapse = ", "))
    }
    message("    Time: ", round(difftime(Sys.time(), db_start, units = "secs"), 2), "s")
  }
```

Repeat for Arctic, Cefas, Coral, and Pelagic with appropriate field mappings. Key differences:

- **Arctic**: map `temperature_preference` to TT, default TT1 for Arctic species
- **Cefas**: map `longevity_years` to `result$longevity_years`, `reproductive_mode` to RS
- **Coral**: map `thermal_tolerance` to TT, `reproductive_mode` to RS, `depth_min/max` to result
- **Pelagic**: map `body_length_cm` to size_cm for MS harmonization

- [ ] **Step 5: Parse check, run all tests, commit**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='R/functions/trait_lookup/orchestrator.R'); cat('OK\n')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer2b-csv-databases.R')"
```

```bash
git commit -m "$(cat <<'EOF'
feat(traits): add orchestrator routing for 5 bundled CSV databases

Added query_blacksea/arctic/cefas/coral/pelagic flags with routing:
- Marine invertebrates -> Cefas + BlackSea + Arctic
- Anthozoa -> Coral Trait DB
- Fish -> Pelagic Trait DB
- Fallback -> all 5
RS/TT/ST traits harmonized from CSV database results.

Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 5: Final integration test

- [ ] **Step 1: Run all test suites**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer2b-csv-databases.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer2a-template.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer1-quality.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-p0p1-fixes.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" tests/test_trait_expansion.R 2>&1 | tail -3
```

- [ ] **Step 2: Parse check all modified/created files**

```bash
for f in R/functions/trait_lookup/csv_trait_databases.R R/functions/trait_lookup/load_all.R R/functions/trait_lookup/orchestrator.R; do
  "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='$f'); cat('OK:', '$f', '\n')" 2>/dev/null
done
```

Expected: All pass, all OK.
