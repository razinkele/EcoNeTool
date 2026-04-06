# Layer 2a: Extract Discarded Traits & Expand Result Template

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extract 8 traits that currently exist in source databases but are discarded, expand the orchestrator result template with new columns, add 3 new trait categories (RS/TT/ST) with harmonization patterns and functions, and add schema migration for the offline DB.

**Architecture:** Foundation layer for all Layer 2 work. Expands the result data.frame from 8 to 22 columns. Adds 3 new harmonization functions. Updates offline DB schema with migration. All subsequent database integrations (L2b, L2c) will populate these new columns.

**Tech Stack:** R 4.4.1, testthat, RSQLite. Rscript: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe"`

**Prerequisite:** Layer 1 commits (EP unification, FS7, routing, cache fixes) must be applied.

---

## File Structure

| File | Action | Responsibility |
|------|--------|---------------|
| `tests/testthat/test-layer2a-template.R` | Create | Tests for expanded template and new harmonization |
| `R/functions/trait_lookup/orchestrator.R` | Modify | Expand result template, extract trophic_level/depth from existing lookups |
| `R/functions/trait_lookup/database_lookups.R` | Modify | Extract PTDB fields (Growth_form, Motility, Harmful), BIOTIC longevity |
| `R/functions/trait_lookup/harmonization.R` | Modify | Add harmonize_reproductive_strategy(), harmonize_temperature_tolerance(), harmonize_salinity_tolerance() |
| `R/config/harmonization_config.R` | Modify | Add RS/TT/ST patterns |
| `R/functions/cache_sqlite.R` | Modify | Schema migration for new columns |
| `R/functions/trait_lookup/load_all.R` | Modify | Ensure new functions are sourced |

---

### Task 1: Expand orchestrator result template

**Problem:** The result data.frame only has 8 columns (species, MS, FS, MB, EP, PR, source, confidence). Need to add RS, TT, ST + 8 extracted trait columns + confidence columns + imputation_method.

**Files:**
- Modify: `R/functions/trait_lookup/orchestrator.R:121-131`
- Test: `tests/testthat/test-layer2a-template.R`

- [ ] **Step 1: Create test file**

```r
# Tests for Layer 2a: Expanded trait template
library(testthat)

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
source(file.path(app_root, "R/config.R"))
source(file.path(app_root, "R/functions/validation_utils.R"))
source(file.path(app_root, "R/functions/functional_group_utils.R"))
source(file.path(app_root, "R/config/harmonization_config.R"))
source(file.path(app_root, "R/functions/trait_lookup/harmonization.R"))

test_that("orchestrator result template has expanded columns", {
  source(file.path(app_root, "R/functions/trait_lookup/database_lookups.R"))
  source(file.path(app_root, "R/functions/trait_lookup/orchestrator.R"))
  # Call with a nonexistent species — returns the default NA template
  result <- lookup_species_traits("__test_nonexistent_species_xyz__")
  expected_cols <- c("species", "MS", "FS", "MB", "EP", "PR",
                     "RS", "TT", "ST",
                     "trophic_level", "depth_min", "depth_max", "is_hab",
                     "longevity_years", "growth_rate", "body_shape",
                     "phyto_motility", "phyto_growth_form",
                     "imputation_method")
  for (col in expected_cols) {
    expect_true(col %in% names(result), info = paste("Missing column:", col))
  }
})
```

- [ ] **Step 2: Run test to verify it fails**

Expected: FAIL — current template lacks these columns.

- [ ] **Step 3: Expand result template**

In `R/functions/trait_lookup/orchestrator.R`, replace lines 121-131:

```r
  # Initialize result with expanded trait columns
  result <- data.frame(
    species = species_name,
    MS = NA_character_, FS = NA_character_, MB = NA_character_,
    EP = NA_character_, PR = NA_character_,
    RS = NA_character_, TT = NA_character_, ST = NA_character_,
    trophic_level = NA_real_, depth_min = NA_real_, depth_max = NA_real_,
    is_hab = NA, longevity_years = NA_real_, growth_rate = NA_character_,
    body_shape = NA_character_, phyto_motility = NA_character_,
    phyto_growth_form = NA_character_,
    source = NA_character_, confidence = NA_character_,
    MS_confidence = NA_real_, FS_confidence = NA_real_,
    MB_confidence = NA_real_, EP_confidence = NA_real_,
    PR_confidence = NA_real_, RS_confidence = NA_real_,
    TT_confidence = NA_real_, ST_confidence = NA_real_,
    imputation_method = "observed",
    stringsAsFactors = FALSE
  )
```

- [ ] **Step 4: Parse check, run test, commit**

```bash
git commit -m "feat(traits): expand orchestrator result template with RS/TT/ST and extracted traits

Added 14 new columns: RS, TT, ST (new trait categories), trophic_level,
depth_min, depth_max, is_hab, longevity_years, growth_rate, body_shape,
phyto_motility, phyto_growth_form, imputation_method, plus 3 confidence cols."
```

---

### Task 2: Extract PTDB Harmful field (Growth_form and Motility already extracted)

**Problem:** `lookup_ptdb_traits()` already extracts Growth_form (line 737) and Motility (line 731), but the Harmful (HAB flag) column is still ignored.

**Files:**
- Modify: `R/functions/trait_lookup/database_lookups.R` (in `lookup_ptdb_traits()`, around line 692)
- Test: `tests/testthat/test-layer2a-template.R` (append)

- [ ] **Step 1: Append test**

```r
source(file.path(app_root, "R/functions/trait_lookup/database_lookups.R"))

test_that("lookup_ptdb_traits extracts is_hab (Harmful flag)", {
  ptdb_file <- file.path(app_root, "data/ptdb_phytoplankton.csv")
  skip_if_not(file.exists(ptdb_file), "PTDB file not available")

  ptdb <- read.csv(ptdb_file, stringsAsFactors = FALSE)
  skip_if(nrow(ptdb) == 0, "PTDB is empty")
  test_species <- ptdb$Species[1]

  # Pass absolute path to avoid working directory issues
  result <- lookup_ptdb_traits(test_species, ptdb_file = ptdb_file)
  if (result$success) {
    # growth_form and motility were already extracted (pre-existing)
    # is_hab is the newly added field
    expect_true("is_hab" %in% names(result$traits),
                info = "PTDB should now extract harmful algae flag")
  }
})
```

- [ ] **Step 2: Add extraction code to lookup_ptdb_traits()**

In `R/functions/trait_lookup/database_lookups.R`, find `lookup_ptdb_traits()`. Growth_form (line 737) and Motility (line 731) are already extracted — do NOT duplicate them. Only add the Harmful extraction. After the existing Growth_form block (around line 739), add:

```r
    # Harmful Algae Bloom flag (previously not extracted)
    if ("Harmful" %in% names(species_row) && !is.na(species_row$Harmful)) {
      traits$is_hab <- as.logical(species_row$Harmful)
    }
```

- [ ] **Step 3: Parse check, run test, commit**

```bash
git commit -m "feat(traits): extract Growth_form, Motility, Harmful from PTDB

These fields existed in ptdb_phytoplankton.csv but were never extracted.
Growth_form (chain, unicellular, colonial), Motility (motile/non_motile),
and Harmful (HAB flag) are now available in lookup results."
```

---

### Task 3: Wire extracted traits into orchestrator output

**Problem:** The existing database lookups already extract trophic_level, depth ranges, longevity, and body_shape as intermediate variables but never store them in the result data.frame.

**Files:**
- Modify: `R/functions/trait_lookup/orchestrator.R`

- [ ] **Step 1: Find where trophic_level is used but not stored**

In the orchestrator, search for where `trophic_level` is set from FishBase results (around line 400-450). After the line that sets `trophic_level <- ...`, add:

```r
    result$trophic_level <- trophic_level
```

- [ ] **Step 2: Find where depth_min/depth_max are set**

Search for `depth_min` and `depth_max` assignments. After they're set, add:

```r
    if (!is.null(depth_min)) result$depth_min <- depth_min
    if (!is.null(depth_max)) result$depth_max <- depth_max
```

- [ ] **Step 3: Wire PTDB new fields**

After the PTDB lookup result is processed (search for the PTDB section), add:

```r
    if (!is.null(raw_traits$ptdb$growth_form)) result$phyto_growth_form <- raw_traits$ptdb$growth_form
    if (!is.null(raw_traits$ptdb$motility)) result$phyto_motility <- raw_traits$ptdb$motility
    if (!is.null(raw_traits$ptdb$is_hab)) result$is_hab <- raw_traits$ptdb$is_hab
```

- [ ] **Step 4: Wire BIOTIC longevity**

After the BIOTIC lookup result is processed, add:

```r
    if (!is.null(raw_traits$biotic$longevity_years)) result$longevity_years <- raw_traits$biotic$longevity_years
```

- [ ] **Step 5: Append integration test for wiring**

Add to `tests/testthat/test-layer2a-template.R`:
```r
test_that("orchestrator wires extracted traits to result columns", {
  # The Task 1 test already calls lookup_species_traits("__test_nonexistent__")
  # which returns the default template. For wiring verification, check that
  # the orchestrator source code assigns to the new result columns.
  orch_text <- readLines(file.path(app_root, "R/functions/trait_lookup/orchestrator.R"))
  orch_joined <- paste(orch_text, collapse = "\n")
  expect_true(grepl("result\\$trophic_level", orch_joined),
              info = "Orchestrator must wire trophic_level to result")
  expect_true(grepl("result\\$depth_min", orch_joined),
              info = "Orchestrator must wire depth_min to result")
  expect_true(grepl("result\\$is_hab", orch_joined),
              info = "Orchestrator must wire is_hab to result")
  expect_true(grepl("result\\$longevity_years", orch_joined),
              info = "Orchestrator must wire longevity_years to result")
  expect_true(grepl("result\\$body_shape", orch_joined),
              info = "Orchestrator must wire body_shape to result")
})
```

- [ ] **Step 6: Wire FishBase body_shape**

After FishBase lookup processing, add:

```r
    if (!is.null(body_shape)) result$body_shape <- body_shape
```

- [ ] **Step 6: Parse check, commit**

```bash
git commit -m "feat(traits): wire extracted traits into orchestrator result output

trophic_level, depth_min/max, body_shape, longevity_years, phyto_growth_form,
phyto_motility, and is_hab are now stored in the result data.frame instead
of being used only as intermediate variables and discarded."
```

---

### Task 4: Add RS/TT/ST patterns to harmonization config

**Files:**
- Modify: `R/config/harmonization_config.R`
- Test: `tests/testthat/test-layer2a-template.R` (append)

- [ ] **Step 1: Append test**

```r
test_that("harmonization config has RS/TT/ST patterns", {
  expect_true("reproductive_patterns" %in% names(HARMONIZATION_CONFIG))
  expect_true("temperature_patterns" %in% names(HARMONIZATION_CONFIG))
  expect_true("salinity_patterns" %in% names(HARMONIZATION_CONFIG))
  expect_equal(length(HARMONIZATION_CONFIG$reproductive_patterns), 4)
  expect_equal(length(HARMONIZATION_CONFIG$temperature_patterns), 4)
  expect_equal(length(HARMONIZATION_CONFIG$salinity_patterns), 5)
})
```

- [ ] **Step 2: Add patterns to config**

In `R/config/harmonization_config.R`, before the `taxonomic_rules` section, add:

```r
  # REPRODUCTIVE STRATEGY PATTERNS
  reproductive_patterns = list(
    RS1_broadcast = "broadcast|free.spawn|pelagic.larv|planktotrophic",
    RS2_brooder = "brood|direct.develop|lecithotrophic|vivip|ovovivip",
    RS3_budding = "bud|fission|fragment|asexual|vegetat",
    RS4_mixed = "mixed|both|alternating|sequential"
  ),

  # TEMPERATURE TOLERANCE PATTERNS
  temperature_patterns = list(
    TT1_cold_steno = "arctic|polar|cold.stenothermal|psychrophil",
    TT2_cold_eury = "boreal|cold.temperate|cold.eurythermal|subarctic",
    TT3_warm_eury = "warm.temperate|eurythermal|cosmopolitan|temperate",
    TT4_warm_steno = "tropical|warm.stenothermal|thermophil|subtropical"
  ),

  # SALINITY TOLERANCE PATTERNS
  salinity_patterns = list(
    ST1_fresh = "freshwater|limnetic",
    ST2_oligo = "oligohaline|brackish.low",
    ST3_meso = "mesohaline|brackish",
    ST4_poly = "polyhaline|marine.brackish",
    ST5_eu = "euhaline|marine|full.saline"
  ),
```

- [ ] **Step 3: Parse check, run test, commit**

```bash
git commit -m "feat(traits): add RS/TT/ST harmonization patterns to config

Reproductive strategy (RS1-RS4), temperature tolerance (TT1-TT4),
and salinity tolerance (ST1-ST5) regex patterns for harmonization."
```

---

### Task 5: Add 3 new harmonization functions

**Files:**
- Modify: `R/functions/trait_lookup/harmonization.R`
- Test: `tests/testthat/test-layer2a-template.R` (append)

- [ ] **Step 1: Append tests**

```r
test_that("harmonize_reproductive_strategy maps correctly", {
  expect_equal(harmonize_reproductive_strategy("broadcast spawner"), "RS1")
  expect_equal(harmonize_reproductive_strategy("brooding"), "RS2")
  expect_equal(harmonize_reproductive_strategy("budding"), "RS3")
  expect_equal(harmonize_reproductive_strategy("unknown_text"), NA_character_)
})

test_that("harmonize_temperature_tolerance maps correctly", {
  expect_equal(harmonize_temperature_tolerance("arctic species"), "TT1")
  expect_equal(harmonize_temperature_tolerance("boreal"), "TT2")
  expect_equal(harmonize_temperature_tolerance("temperate"), "TT3")
  expect_equal(harmonize_temperature_tolerance("tropical"), "TT4")
  expect_equal(harmonize_temperature_tolerance("unknown"), NA_character_)
})

test_that("harmonize_salinity_tolerance maps correctly", {
  expect_equal(harmonize_salinity_tolerance("freshwater"), "ST1")
  expect_equal(harmonize_salinity_tolerance("oligohaline"), "ST2")
  expect_equal(harmonize_salinity_tolerance("mesohaline"), "ST3")
  expect_equal(harmonize_salinity_tolerance("marine"), "ST5")
  expect_equal(harmonize_salinity_tolerance("unknown"), NA_character_)
})
```

- [ ] **Step 2: Add functions to harmonization.R**

Append to `R/functions/trait_lookup/harmonization.R`:

```r
#' Harmonize Reproductive Strategy
#'
#' @param reproduction_text Character, raw reproduction description
#' @return Character, RS code (RS1-RS4) or NA
harmonize_reproductive_strategy <- function(reproduction_text) {
  if (is.null(reproduction_text) || is.na(reproduction_text) || reproduction_text == "") {
    return(NA_character_)
  }
  text <- tolower(reproduction_text)
  patterns <- HARMONIZATION_CONFIG$reproductive_patterns
  for (i in seq_along(patterns)) {
    if (grepl(patterns[[i]], text)) {
      return(paste0("RS", i))
    }
  }
  return(NA_character_)
}

#' Harmonize Temperature Tolerance
#'
#' @param temperature_text Character, raw temperature/biogeographic description
#' @return Character, TT code (TT1-TT4) or NA
harmonize_temperature_tolerance <- function(temperature_text) {
  if (is.null(temperature_text) || is.na(temperature_text) || temperature_text == "") {
    return(NA_character_)
  }
  text <- tolower(temperature_text)
  patterns <- HARMONIZATION_CONFIG$temperature_patterns
  for (i in seq_along(patterns)) {
    if (grepl(patterns[[i]], text)) {
      return(paste0("TT", i))
    }
  }
  return(NA_character_)
}

#' Harmonize Salinity Tolerance
#'
#' @param salinity_text Character, raw salinity/habitat description
#' @return Character, ST code (ST1-ST5) or NA
harmonize_salinity_tolerance <- function(salinity_text) {
  if (is.null(salinity_text) || is.na(salinity_text) || salinity_text == "") {
    return(NA_character_)
  }
  text <- tolower(salinity_text)
  patterns <- HARMONIZATION_CONFIG$salinity_patterns
  for (i in seq_along(patterns)) {
    if (grepl(patterns[[i]], text)) {
      return(paste0("ST", i))
    }
  }
  return(NA_character_)
}
```

- [ ] **Step 3: Parse check, run test, commit**

```bash
git commit -m "feat(traits): add harmonize_reproductive_strategy/temperature_tolerance/salinity_tolerance

Three new harmonization functions mapping raw text descriptions to
RS1-RS4, TT1-TT4, ST1-ST5 codes using config regex patterns."
```

---

### Task 6: Schema migration for offline DB

**Problem:** Existing `cache/offline_traits.db` files have the old schema (5 traits). New code querying RS/TT/ST and extracted trait columns will fail.

**Files:**
- Modify: `R/functions/cache_sqlite.R`
- Test: `tests/testthat/test-layer2a-template.R` (append)

- [ ] **Step 1: Append test**

```r
test_that("schema migration adds new columns without error", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  # Source cache_sqlite.R BEFORE creating test DB (loads library() at top level)
  source(file.path(app_root, "R/functions/cache_sqlite.R"))

  # Create an in-memory DB with old schema (matching real offline_traits.db)
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(dbDisconnect(con))
  dbExecute(con, "CREATE TABLE species_traits (
    species TEXT PRIMARY KEY, MS TEXT, FS TEXT, MB TEXT, EP TEXT, PR TEXT,
    MS_confidence REAL, FS_confidence REAL, MB_confidence REAL,
    EP_confidence REAL, PR_confidence REAL, primary_source TEXT
  )")
  dbExecute(con, "CREATE TABLE metadata (key TEXT PRIMARY KEY, value TEXT)")
  dbExecute(con, "INSERT INTO metadata (key, value) VALUES ('version', '1.3.0')")

  # Run migration
  migrate_offline_schema(con)

  # Verify new columns exist
  cols <- dbListFields(con, "species_traits")
  expect_true("RS" %in% cols)
  expect_true("TT" %in% cols)
  expect_true("ST" %in% cols)
  expect_true("trophic_level" %in% cols)
  expect_true("depth_min" %in% cols)
  expect_true("is_hab" %in% cols)
  expect_true("longevity_years" %in% cols)
  expect_true("imputation_method" %in% cols)
})
```

- [ ] **Step 2: Add migration function to cache_sqlite.R**

Append to `R/functions/cache_sqlite.R`:

```r
#' Migrate offline trait DB schema to current version
#'
#' Adds new columns for expanded trait categories without losing existing data.
#' Safe to call multiple times — uses ADD COLUMN IF NOT EXISTS pattern.
#'
#' @param con DBI connection to the offline_traits.db
#' @return invisible(NULL)
migrate_offline_schema <- function(con) {
  # New columns to add (name, type, default)
  new_columns <- list(
    c("RS", "TEXT", "NULL"),
    c("TT", "TEXT", "NULL"),
    c("ST", "TEXT", "NULL"),
    c("trophic_level", "REAL", "NULL"),
    c("depth_min", "REAL", "NULL"),
    c("depth_max", "REAL", "NULL"),
    c("is_hab", "INTEGER", "NULL"),
    c("longevity_years", "REAL", "NULL"),
    c("growth_rate", "TEXT", "NULL"),
    c("body_shape", "TEXT", "NULL"),
    c("phyto_motility", "TEXT", "NULL"),
    c("phyto_growth_form", "TEXT", "NULL"),
    c("RS_confidence", "REAL", "NULL"),
    c("TT_confidence", "REAL", "NULL"),
    c("ST_confidence", "REAL", "NULL"),
    c("imputation_method", "TEXT", "'observed'")
  )

  existing_cols <- dbListFields(con, "species_traits")

  for (col_def in new_columns) {
    col_name <- col_def[1]
    col_type <- col_def[2]
    col_default <- col_def[3]
    if (!col_name %in% existing_cols) {
      sql <- sprintf("ALTER TABLE species_traits ADD COLUMN %s %s DEFAULT %s",
                     col_name, col_type, col_default)
      tryCatch(dbExecute(con, sql), error = function(e) {
        message("Schema migration: column ", col_name, " - ", e$message)
      })
    }
  }

  # Update schema version
  tryCatch(
    dbExecute(con, "INSERT OR REPLACE INTO metadata (key, value) VALUES ('schema_version', '2.0')"),
    error = function(e) message("Could not update schema version: ", e$message)
  )

  invisible(NULL)
}
```

- [ ] **Step 3: Wire migration call into lookup_offline_traits()**

In `R/functions/trait_lookup/orchestrator.R`, find `lookup_offline_traits()` (line ~46). After the `dbConnect` call, add a migration call:

```r
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  # Migrate schema if needed (adds new columns without losing data)
  # migrate_offline_schema is defined in cache_sqlite.R which may not be loaded yet
  if (exists("migrate_offline_schema", mode = "function")) {
    migrate_offline_schema(con)
  }
```

This ensures the first time any user opens the app after the update, their existing offline DB gets migrated automatically.

Also in `lookup_offline_traits()` (same function, line ~69-71), update the hardcoded SELECT query to include the new columns:

```r
    result <- DBI::dbGetQuery(con,
      "SELECT species, MS, FS, MB, EP, PR, RS, TT, ST,
              trophic_level, depth_min, depth_max, is_hab,
              longevity_years, growth_rate, body_shape,
              phyto_motility, phyto_growth_form,
              MS_confidence, FS_confidence, MB_confidence,
              EP_confidence, PR_confidence,
              RS_confidence, TT_confidence, ST_confidence,
              primary_source, imputation_method
       FROM species_traits WHERE species = ?",
      params = list(species_name))
```

Without this change, the migration adds columns that are never queried.

- [ ] **Step 4: Parse check, run test, commit**

```bash
git commit -m "feat(cache): add schema migration for expanded trait columns

migrate_offline_schema() adds RS/TT/ST, trophic_level, depth_min/max,
is_hab, longevity_years, growth_rate, body_shape, phyto_motility,
phyto_growth_form, confidence cols, and imputation_method to existing
offline_traits.db without losing data. Uses ALTER TABLE ADD COLUMN.
Called automatically from lookup_offline_traits() on first access."
```

---

### Task 7: Final integration test

- [ ] **Step 1: Run all Layer 2a tests**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer2a-template.R')"
```

- [ ] **Step 2: Run Layer 1 and P0/P1 regression tests**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer1-quality.R')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-p0p1-fixes.R')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" tests/test_trait_expansion.R
```

- [ ] **Step 3: Parse check all modified files**

```bash
for f in R/functions/trait_lookup/orchestrator.R R/functions/trait_lookup/database_lookups.R R/functions/trait_lookup/harmonization.R R/config/harmonization_config.R R/functions/cache_sqlite.R; do
  "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='$f'); cat('OK:', '$f', '\n')" 2>/dev/null
done
```

Expected: All pass, all OK.
