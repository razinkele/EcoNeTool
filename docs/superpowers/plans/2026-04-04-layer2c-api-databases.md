# Layer 2c: API Trait Database Integrations

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Integrate 5 API-based trait databases (WoRMS Traits, PolyTraits, EMODnet Btrait, OBIS MoF, TraitBank) with lookup functions and orchestrator routing.

**Architecture:** Each database gets a lookup function in a new file `api_trait_databases.R`. WoRMS Traits uses the existing `worrms` package (already installed). PolyTraits uses its REST API via `httr`. OBIS uses `robis`. TraitBank uses `traits` package (archived but installed). EMODnet Btrait is GitHub-only and not installed — made optional with `requireNamespace` guard. All functions return the standard `list(species, source, success, traits)` interface.

**Tech Stack:** R 4.4.1, testthat, worrms, httr, jsonlite, robis, traits. Rscript: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe"`

**Prerequisite:** Layer 2a (expanded template) and Layer 2b (CSV databases) must be applied.

**Package availability:** worrms (installed), robis (installed), httr (installed), jsonlite (installed), Btrait (NOT installed — made optional). TraitBank now uses EOL Pages API via httr (no `traits` package needed — it was archived from CRAN and requires Cypher queries + API keys).

---

## File Structure

| File | Action | Responsibility |
|------|--------|---------------|
| `R/functions/trait_lookup/api_trait_databases.R` | Create | 5 API lookup functions |
| `R/functions/trait_lookup/load_all.R` | Modify | Source new file |
| `R/functions/trait_lookup/orchestrator.R` | Modify | Add routing + lookup blocks |
| `tests/testthat/test-layer2c-api-databases.R` | Create | Tests for API functions |

---

### Task 1: Create api_trait_databases.R with WoRMS Traits lookup

**Problem:** WoRMS has a traits API endpoint (`wm_attr_data(AphiaID)`) that returns trait attributes. The existing `lookup_worms_traits()` only does taxonomy — it doesn't query traits.

**Files:**
- Create: `R/functions/trait_lookup/api_trait_databases.R`
- Test: `tests/testthat/test-layer2c-api-databases.R`

- [ ] **Step 1: Create test file**

```r
# Tests for Layer 2c: API trait databases
library(testthat)

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
source(file.path(app_root, "R/config.R"))
source(file.path(app_root, "R/functions/validation_utils.R"))
source(file.path(app_root, "R/functions/functional_group_utils.R"))
source(file.path(app_root, "R/config/harmonization_config.R"))
source(file.path(app_root, "R/functions/trait_lookup/harmonization.R"))
source(file.path(app_root, "R/functions/trait_lookup/api_trait_databases.R"))

test_that("lookup_worms_traits_api returns correct structure", {
  skip_if_not_installed("worrms")
  skip_if_offline()
  # Use a known AphiaID (126436 = Gadus morhua)
  result <- lookup_worms_traits_api(aphia_id = 126436)
  expect_true(is.list(result))
  expect_equal(result$source, "WoRMS_Traits")
  expect_true(is.list(result$traits))
  # May or may not succeed depending on API availability
  if (result$success) {
    expect_true(length(result$traits) > 0)
  }
})

test_that("lookup_worms_traits_api handles invalid AphiaID gracefully", {
  skip_if_not_installed("worrms")
  skip_if_offline()
  result <- lookup_worms_traits_api(aphia_id = -999)
  expect_false(result$success)
})
```

- [ ] **Step 2: Create api_trait_databases.R with WoRMS Traits function**

```r
# =============================================================================
# API-Based Trait Database Lookups
# =============================================================================
# Lookup functions for 5 API-based trait databases.
# Each returns: list(species, source, success, traits)
#
# Databases:
#   1. WoRMS Traits API (worrms::wm_attr_data)
#   2. PolyTraits REST API (httr)
#   3. EMODnet Btrait (R package, optional)
#   4. OBIS MoF (robis::occurrence with mof=TRUE)
#   5. TraitBank/EOL (traits::traitbank)
# =============================================================================

# =============================================================================
# 1. WoRMS TRAITS API
# =============================================================================

#' Lookup trait attributes from WoRMS Traits API
#'
#' Queries WoRMS for functional trait attributes via AphiaID.
#' Returns: feeding type, body size, habitat, AMBI group, etc.
#'
#' @param species_name Character (optional, for result labeling)
#' @param aphia_id Numeric, WoRMS AphiaID (required)
#' @param timeout Numeric, timeout in seconds (default 10)
#' @return list(species, source, success, traits)
lookup_worms_traits_api <- function(species_name = NULL, aphia_id = NULL, timeout = 10) {
  result <- list(
    species = species_name %||% paste0("AphiaID:", aphia_id),
    source = "WoRMS_Traits",
    success = FALSE,
    traits = list()
  )

  if (is.null(aphia_id) || !is.numeric(aphia_id) || aphia_id <= 0) {
    return(result)
  }

  if (!requireNamespace("worrms", quietly = TRUE)) {
    return(result)
  }

  tryCatch({
    attrs <- with_timeout(
      worrms::wm_attr_data(aphia_id),
      timeout = timeout,
      on_timeout = NULL
    )

    if (is.null(attrs) || nrow(attrs) == 0) {
      return(result)
    }

    traits <- list()

    # Parse measurementType -> trait values
    for (i in seq_len(nrow(attrs))) {
      mt <- tolower(attrs$measurementType[i])
      mv <- attrs$measurementValue[i]
      if (is.na(mv) || mv == "") next

      if (grepl("functional group|functional.group", mt)) traits$functional_group <- mv
      if (grepl("body size", mt)) traits$body_size <- mv
      if (grepl("feeding type|feeding.type|diet", mt)) traits$feeding_type <- mv
      if (grepl("substratum|substrate", mt)) traits$substratum <- mv
      if (grepl("zone|zonation", mt)) traits$zone <- mv
      if (grepl("ambi|ambi.group", mt)) traits$ambi_group <- mv
      if (grepl("environment|habitat", mt)) traits$environment <- mv
      if (grepl("salinity", mt)) traits$salinity <- mv
    }

    result$traits <- traits
    result$success <- length(traits) > 0
    result

  }, error = function(e) {
    message("  [WoRMS Traits] Error: ", e$message)
    result
  })
}
```

- [ ] **Step 3: Parse check and run tests**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='R/functions/trait_lookup/api_trait_databases.R'); cat('OK\n')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer2c-api-databases.R')"
```

- [ ] **Step 4: Commit**

```bash
git add R/functions/trait_lookup/api_trait_databases.R tests/testthat/test-layer2c-api-databases.R
git commit -m "$(cat <<'EOF'
feat(traits): add WoRMS Traits API lookup function

lookup_worms_traits_api() queries worrms::wm_attr_data(AphiaID) for
trait attributes: functional group, body size, feeding type, substratum,
zonation, AMBI group, environment, salinity.

Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 2: Add PolyTraits, EMODnet, OBIS, TraitBank lookup functions

**Files:**
- Modify: `R/functions/trait_lookup/api_trait_databases.R`
- Test: `tests/testthat/test-layer2c-api-databases.R` (append)

- [ ] **Step 1: Append tests**

```r
test_that("lookup_polytraits returns correct structure", {
  skip_if_not_installed("httr")
  skip_if_offline()
  result <- lookup_polytraits("Nonexistent_polychaete_xyz")
  expect_true(is.list(result))
  expect_equal(result$source, "PolyTraits")
  expect_false(result$success)
})

test_that("lookup_emodnet_traits returns correct structure", {
  # Btrait is optional (GitHub-only)
  result <- lookup_emodnet_traits("Nonexistent_species_xyz")
  expect_true(is.list(result))
  expect_equal(result$source, "EMODnet")
  expect_false(result$success)
})

test_that("lookup_obis_traits returns correct structure", {
  skip_if_not_installed("robis")
  skip_if_offline()
  result <- lookup_obis_traits("Nonexistent_species_xyz")
  expect_true(is.list(result))
  expect_equal(result$source, "OBIS")
  expect_false(result$success)
})

test_that("lookup_traitbank returns correct structure", {
  skip_if_offline()
  result <- lookup_traitbank("Nonexistent_species_xyz")
  expect_true(is.list(result))
  expect_equal(result$source, "TraitBank")
  expect_false(result$success)
})

test_that("all 5 API lookup functions exist", {
  expect_true(exists("lookup_worms_traits_api", mode = "function"))
  expect_true(exists("lookup_polytraits", mode = "function"))
  expect_true(exists("lookup_emodnet_traits", mode = "function"))
  expect_true(exists("lookup_obis_traits", mode = "function"))
  expect_true(exists("lookup_traitbank", mode = "function"))
})
```

- [ ] **Step 2: Add 4 lookup functions to api_trait_databases.R**

Append to the file:

```r
# =============================================================================
# 2. POLYTRAITS REST API
# =============================================================================

#' Lookup traits from PolyTraits (polychaete trait database)
#'
#' @param species_name Character, species to look up
#' @param timeout Numeric, timeout in seconds (default 10)
#' @return list(species, source, success, traits)
lookup_polytraits <- function(species_name, timeout = 10) {
  result <- list(species = species_name, source = "PolyTraits", success = FALSE, traits = list())

  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    return(result)
  }

  tryCatch({
    # Step 1: Get taxonID from the taxon endpoint
    taxon_url <- paste0("http://polytraits.lifewatchgreece.eu/taxon/",
                        utils::URLencode(species_name), "/json/")
    taxon_resp <- httr::GET(taxon_url, httr::timeout(timeout))
    if (httr::http_error(taxon_resp)) return(result)

    taxon_data <- jsonlite::fromJSON(httr::content(taxon_resp, as = "text", encoding = "UTF-8"))
    if (is.null(taxon_data) || length(taxon_data) == 0) return(result)

    # taxon_data is a data.frame with columns: taxonID, taxon
    taxon_id <- taxon_data$taxonID[1]
    if (is.null(taxon_id) || taxon_id == "") return(result)

    # Step 2: Get traits using the taxonID
    traits_url <- paste0("http://polytraits.lifewatchgreece.eu/traits/",
                         taxon_id, "/json/")
    traits_resp <- httr::GET(traits_url, httr::timeout(timeout))
    if (httr::http_error(traits_resp)) return(result)

    traits_json <- jsonlite::fromJSON(httr::content(traits_resp, as = "text", encoding = "UTF-8"))
    if (is.null(traits_json) || length(traits_json) == 0) return(result)

    # traits_json is a named list: {"taxonID": [array of trait records]}
    # Each record has: trait, modality, traitvalue, reference, etc.
    trait_records <- traits_json[[1]]  # First (and usually only) element
    if (!is.data.frame(trait_records) || nrow(trait_records) == 0) return(result)

    traits <- list()
    for (i in seq_len(nrow(trait_records))) {
      tn <- tolower(trait_records$trait[i] %||% "")
      mv <- trait_records$modality[i] %||% ""
      if (mv == "") next
      if (grepl("body size|size", tn)) traits$body_size <- mv
      if (grepl("feeding|diet", tn)) traits$feeding_mode <- mv
      if (grepl("mobility|movement", tn)) traits$mobility_info <- mv
      if (grepl("reproduction|reproductive|sexual", tn)) traits$reproductive_mode <- mv
      if (grepl("larval", tn)) traits$larval_development <- mv
      if (grepl("depth|zonation", tn)) traits$habitat_info <- mv
    }

    result$traits <- traits
    result$success <- length(traits) > 0
    result

  }, error = function(e) {
    message("  [PolyTraits] Error: ", e$message)
    result
  })
}

# =============================================================================
# 3. EMODNET BTRAIT (optional — requires GitHub package)
# =============================================================================

#' Lookup traits from EMODnet Btrait R package
#'
#' @param species_name Character, species to look up
#' @return list(species, source, success, traits)
lookup_emodnet_traits <- function(species_name) {
  result <- list(species = species_name, source = "EMODnet", success = FALSE, traits = list())

  if (!requireNamespace("Btrait", quietly = TRUE)) {
    # Btrait is a GitHub-only package — graceful degradation
    return(result)
  }

  tryCatch({
    # Btrait::getTrait returns trait composition for species
    trait_data <- Btrait::getTrait(species_name)

    if (is.null(trait_data) || nrow(trait_data) == 0) return(result)

    traits <- list()
    row <- trait_data[1, ]
    for (col in names(row)) {
      val <- row[[col]]
      if (!is.na(val) && val != "") {
        col_lower <- tolower(col)
        if (grepl("^feeding", col_lower)) traits$feeding_mode <- val
        if (grepl("^mobility", col_lower)) traits$mobility_info <- val
        if (grepl("^size", col_lower)) traits$body_size <- val
        if (grepl("^bioturbation", col_lower)) traits$bioturbation <- val
        if (grepl("^living", col_lower)) traits$living_habit <- val
      }
    }

    result$traits <- traits
    result$success <- length(traits) > 0
    result

  }, error = function(e) {
    message("  [EMODnet] Error: ", e$message)
    result
  })
}

# =============================================================================
# 4. OBIS MoF (Measurement or Fact)
# =============================================================================

#' Lookup traits from OBIS via MeasurementOrFact records
#'
#' Queries OBIS for occurrence records with trait measurements.
#' Extracts body size and biomass from MoF extension.
#'
#' @param species_name Character, species to look up
#' @param limit Numeric, max occurrence records to fetch (default 50)
#' @param timeout Numeric, timeout in seconds (default 15)
#' @return list(species, source, success, traits)
lookup_obis_traits <- function(species_name, timeout = 30) {
  result <- list(species = species_name, source = "OBIS", success = FALSE, traits = list())

  if (!requireNamespace("robis", quietly = TRUE)) {
    return(result)
  }

  tryCatch({
    # Query OBIS with MoF extension
    # Note: robis::occurrence has no 'limit' parameter — fetches all records
    # Use with_timeout to prevent hanging on large queries
    occ <- with_timeout(
      robis::occurrence(scientificname = species_name, mof = TRUE,
                        fields = c("scientificName", "minimumDepthInMeters",
                                   "maximumDepthInMeters")),
      timeout = timeout,
      on_timeout = NULL
    )

    if (is.null(occ) || nrow(occ) == 0) return(result)

    traits <- list()

    # Extract MoF records if present (robis attaches as attribute or column)
    if ("mof" %in% names(attributes(occ)) || "measurementType" %in% names(occ)) {
      mof_data <- if ("mof" %in% names(occ)) occ$mof else occ

      if (is.data.frame(mof_data) && "measurementType" %in% names(mof_data)) {
        for (i in seq_len(min(nrow(mof_data), 100))) {
          mt <- tolower(mof_data$measurementType[i] %||% "")
          mv <- mof_data$measurementValue[i] %||% ""
          if (mv == "") next
          if (grepl("body size|length|size", mt) && is.null(traits$body_size)) {
            traits$body_size <- mv
          }
          if (grepl("biomass|weight", mt) && is.null(traits$biomass)) {
            traits$biomass <- mv
          }
        }
      }
    }

    # Extract depth from occurrences (median across records)
    if ("minimumDepthInMeters" %in% names(occ)) {
      depths_min <- occ$minimumDepthInMeters
      if (any(!is.na(depths_min))) traits$depth_min <- median(depths_min, na.rm = TRUE)
    }
    if ("maximumDepthInMeters" %in% names(occ)) {
      depths_max <- occ$maximumDepthInMeters
      if (any(!is.na(depths_max))) traits$depth_max <- median(depths_max, na.rm = TRUE)
    }

    result$traits <- traits
    result$success <- length(traits) > 0
    result

  }, error = function(e) {
    message("  [OBIS] Error: ", e$message)
    result
  })
}

# =============================================================================
# 5. TRAITBANK (Encyclopedia of Life)
# =============================================================================

#' Lookup traits from TraitBank/EOL
#'
#' Uses the traits R package if available, otherwise returns empty.
#'
#' @param species_name Character, species to look up
#' @param timeout Numeric, timeout in seconds (default 15)
#' @return list(species, source, success, traits)
lookup_traitbank <- function(species_name, timeout = 15) {
  result <- list(species = species_name, source = "TraitBank", success = FALSE, traits = list())

  # TraitBank/EOL requires a Cypher query + API key via the traits package.
  # The traits::traitbank() function does NOT accept a species name directly.
  # Instead, use the EOL Pages API v3 which is open and returns trait data.
  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    return(result)
  }

  tryCatch({
    # Step 1: Search EOL for the species to get a page ID
    search_url <- paste0("https://eol.org/api/search/1.0.json?q=",
                         utils::URLencode(species_name), "&page=1&exact=true")
    search_resp <- httr::GET(search_url, httr::timeout(timeout))
    if (httr::http_error(search_resp)) return(result)

    search_data <- jsonlite::fromJSON(httr::content(search_resp, as = "text", encoding = "UTF-8"))
    if (is.null(search_data$results) || length(search_data$results) == 0) return(result)

    page_id <- search_data$results$id[1]
    if (is.null(page_id)) return(result)

    # Step 2: Get trait data from the EOL pages API
    pages_url <- paste0("https://eol.org/api/pages/1.0/", page_id,
                        ".json?details=true&common_names=false&images_per_page=0")
    pages_resp <- httr::GET(pages_url, httr::timeout(timeout))
    if (httr::http_error(pages_resp)) return(result)

    pages_data <- jsonlite::fromJSON(httr::content(pages_resp, as = "text", encoding = "UTF-8"))

    traits <- list()

    # Extract from dataObjects if present
    if (!is.null(pages_data$dataObjects) && is.data.frame(pages_data$dataObjects)) {
      for (i in seq_len(min(nrow(pages_data$dataObjects), 20))) {
        desc <- tolower(pages_data$dataObjects$description[i] %||% "")
        if (grepl("body mass|mass|weight", desc) && is.null(traits$body_mass)) {
          traits$body_mass <- desc
        }
        if (grepl("habitat|environment", desc) && is.null(traits$habitat)) {
          traits$habitat <- desc
        }
        if (grepl("diet|food|feeds on", desc) && is.null(traits$diet)) {
          traits$diet <- desc
        }
      }
    }

    result$traits <- traits
    result$success <- length(traits) > 0
    result

  }, error = function(e) {
    message("  [TraitBank] Error: ", e$message)
    result
  })
}
```

- [ ] **Step 3: Parse check and run tests**

- [ ] **Step 4: Commit**

```bash
git commit -m "$(cat <<'EOF'
feat(traits): add PolyTraits, EMODnet, OBIS, TraitBank lookup functions

PolyTraits: REST API via httr for polychaete traits
EMODnet Btrait: optional (GitHub package), graceful degradation
OBIS: MoF records for body size/biomass/depth via robis
TraitBank: traits package (archived but functional)

Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 3: Update load_all.R and add orchestrator routing

**Files:**
- Modify: `R/functions/trait_lookup/load_all.R`
- Modify: `R/functions/trait_lookup/orchestrator.R`
- Test: `tests/testthat/test-layer2c-api-databases.R` (append)

- [ ] **Step 1: Update load_all.R**

After the `csv_trait_databases.R` source line, add:

```r
# API-based trait databases (WoRMS Traits, PolyTraits, EMODnet, OBIS, TraitBank)
source("R/functions/trait_lookup/api_trait_databases.R")
```

Update file count message.

- [ ] **Step 2: Add 5 new flags to orchestrator**

In the flag initialization block, after the CSV flags, add:

```r
  query_worms_attrs <- FALSE
  query_polytraits <- FALSE
  query_emodnet <- FALSE
  query_obis <- FALSE
  query_traitbank <- FALSE
```

- [ ] **Step 3: Add routing conditions**

**All species** (after WoRMS taxonomy succeeds, around line 325): add `query_worms_attrs <- TRUE` (traits for every species with an AphiaID).

**Polychaeta** (in marine invertebrates branch, when `class == "polychaeta"`): add `query_polytraits <- TRUE`.

**Marine invertebrates** (the existing branch): add `query_emodnet <- TRUE`.

**Fallback branch**: add `query_worms_attrs <- TRUE` and `query_obis <- TRUE` and `query_traitbank <- TRUE`.

- [ ] **Step 4: Add 5 lookup execution blocks**

After the CSV database blocks, before HARMONIZATION PHASE:

```r
  # ═══════════════════════════════════════════════════════════════════════
  # API-BASED TRAIT DATABASES
  # ═══════════════════════════════════════════════════════════════════════

  # API: WoRMS Traits (uses AphiaID from WoRMS taxonomy lookup)
  if (query_worms_attrs && !is.null(raw_traits$worms$aphia_id)) {
    message("\n[API] WoRMS Traits API...")
    db_start <- Sys.time()
    worms_attr_data <- lookup_worms_traits_api(
      species_name = species_name,
      aphia_id = raw_traits$worms$aphia_id
    )
    if (worms_attr_data$success) {
      raw_traits$worms_attrs <- worms_attr_data$traits
      sources_used <- c(sources_used, "WoRMS_Traits")
      if (!is.null(worms_attr_data$traits$feeding_type)) feeding_mode <- c(feeding_mode, worms_attr_data$traits$feeding_type)
      if (!is.null(worms_attr_data$traits$zone)) habitat_info <- c(habitat_info, worms_attr_data$traits$zone)
      if (!is.null(worms_attr_data$traits$salinity)) {
        result$ST <- harmonize_salinity_tolerance(worms_attr_data$traits$salinity)
      }
      message("    Found: ", paste(names(worms_attr_data$traits), collapse = ", "))
    }
    message("    Time: ", round(difftime(Sys.time(), db_start, units = "secs"), 2), "s")
  }

  # API: PolyTraits (polychaete specialist)
  if (query_polytraits) {
    message("\n[API] PolyTraits...")
    db_start <- Sys.time()
    poly_data <- lookup_polytraits(species_name)
    if (poly_data$success) {
      raw_traits$polytraits <- poly_data$traits
      sources_used <- c(sources_used, "PolyTraits")
      if (!is.null(poly_data$traits$feeding_mode)) feeding_mode <- c(feeding_mode, poly_data$traits$feeding_mode)
      if (!is.null(poly_data$traits$mobility_info)) mobility_info <- c(mobility_info, poly_data$traits$mobility_info)
      if (!is.null(poly_data$traits$reproductive_mode)) {
        result$RS <- harmonize_reproductive_strategy(poly_data$traits$reproductive_mode)
      }
      message("    Found: ", paste(names(poly_data$traits), collapse = ", "))
    }
    message("    Time: ", round(difftime(Sys.time(), db_start, units = "secs"), 2), "s")
  }

  # API: EMODnet Btrait (optional — package may not be installed)
  if (query_emodnet) {
    message("\n[API] EMODnet Btrait...")
    db_start <- Sys.time()
    emodnet_data <- lookup_emodnet_traits(species_name)
    if (emodnet_data$success) {
      raw_traits$emodnet <- emodnet_data$traits
      sources_used <- c(sources_used, "EMODnet")
      if (!is.null(emodnet_data$traits$feeding_mode)) feeding_mode <- c(feeding_mode, emodnet_data$traits$feeding_mode)
      if (!is.null(emodnet_data$traits$mobility_info)) mobility_info <- c(mobility_info, emodnet_data$traits$mobility_info)
      message("    Found: ", paste(names(emodnet_data$traits), collapse = ", "))
    }
    message("    Time: ", round(difftime(Sys.time(), db_start, units = "secs"), 2), "s")
  }

  # API: OBIS MoF (body size, biomass, depth from occurrences)
  if (query_obis) {
    message("\n[API] OBIS MoF...")
    db_start <- Sys.time()
    obis_data <- lookup_obis_traits(species_name)
    if (obis_data$success) {
      raw_traits$obis <- obis_data$traits
      sources_used <- c(sources_used, "OBIS")
      if (!is.null(obis_data$traits$depth_min) && is.null(depth_min)) {
        depth_min <- obis_data$traits$depth_min
      }
      if (!is.null(obis_data$traits$depth_max) && is.null(depth_max)) {
        depth_max <- obis_data$traits$depth_max
      }
      message("    Found: ", paste(names(obis_data$traits), collapse = ", "))
    }
    message("    Time: ", round(difftime(Sys.time(), db_start, units = "secs"), 2), "s")
  }

  # API: TraitBank/EOL (broad coverage fallback)
  if (query_traitbank) {
    message("\n[API] TraitBank/EOL...")
    db_start <- Sys.time()
    tb_data <- lookup_traitbank(species_name)
    if (tb_data$success) {
      raw_traits$traitbank <- tb_data$traits
      sources_used <- c(sources_used, "TraitBank")
      if (!is.null(tb_data$traits$diet)) feeding_mode <- c(feeding_mode, tb_data$traits$diet)
      if (!is.null(tb_data$traits$trophic_level) && is.null(trophic_level)) {
        trophic_level <- as.numeric(tb_data$traits$trophic_level)
      }
      message("    Found: ", paste(names(tb_data$traits), collapse = ", "))
    }
    message("    Time: ", round(difftime(Sys.time(), db_start, units = "secs"), 2), "s")
  }
```

- [ ] **Step 5: Append routing test**

```r
test_that("orchestrator has API database routing flags", {
  orch_text <- readLines(file.path(app_root, "R/functions/trait_lookup/orchestrator.R"))
  orch_joined <- paste(orch_text, collapse = "\n")
  expect_true(grepl("query_worms_attrs", orch_joined), info = "Missing query_worms_attrs flag")
  expect_true(grepl("query_polytraits", orch_joined), info = "Missing query_polytraits flag")
  expect_true(grepl("query_emodnet", orch_joined), info = "Missing query_emodnet flag")
  expect_true(grepl("query_obis", orch_joined), info = "Missing query_obis flag")
  expect_true(grepl("query_traitbank", orch_joined), info = "Missing query_traitbank flag")
})
```

- [ ] **Step 6: Parse check, run tests, commit**

```bash
git commit -m "$(cat <<'EOF'
feat(traits): add orchestrator routing for 5 API trait databases

WoRMS Traits: queried for all species with AphiaID
PolyTraits: queried for Polychaeta class
EMODnet: queried for marine invertebrates (optional package)
OBIS: queried in fallback mode for depth/biomass
TraitBank: queried in fallback mode for broad coverage

Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 4: Final integration test

- [ ] **Step 1: Run ALL test suites**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer2c-api-databases.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer2b-csv-databases.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer2a-template.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer1-quality.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-p0p1-fixes.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" tests/test_trait_expansion.R 2>&1 | tail -3
```

- [ ] **Step 2: Parse check all files**

```bash
for f in R/functions/trait_lookup/api_trait_databases.R R/functions/trait_lookup/load_all.R R/functions/trait_lookup/orchestrator.R; do
  "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='$f'); cat('OK:', '$f', '\n')" 2>/dev/null
done
```

Expected: All pass, all OK.
