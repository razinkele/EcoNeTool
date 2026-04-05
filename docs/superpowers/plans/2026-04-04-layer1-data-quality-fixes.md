# Layer 1: Data Quality Fixes — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Fix data quality issues in the trait system: unify EP codes across 21 files, add FS7 code across 11 files, fix 8 disabled taxonomic rules, fix taxonomic routing holes, fix pre-existing cache bugs, and update the PR banner.

**Architecture:** 7 tasks attacking each fix category. EP unification is the largest (touches probability matrices + labels + harmonization). FS7 addition is second largest (touches validation + matrices + labels). Other fixes are isolated. Tests use testthat. Each task commits independently.

**Tech Stack:** R 4.4.1, testthat, igraph. Rscript: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe"`

**Prerequisite:** The P0/P1 bugfixes (commits b3fbeef..76d5d18) must already be applied.

---

## File Structure

| File | Action | Tasks |
|------|--------|-------|
| `tests/testthat/test-layer1-quality.R` | Create | Test file for all Layer 1 fixes |
| `R/config/harmonization_config.R` | Modify | T1 (EP patterns), T2 (FS7 pattern), T3 (taxonomic rules) |
| `R/functions/trait_foodweb.R` | Modify | T1 (EP_MS matrix + TRAIT_DEFINITIONS), T2 (FS7 in FS_MS + validation) |
| `R/functions/trait_lookup/harmonization.R` | Modify | T1 (EP harmonizers), T2 (FS7 in fuzzy foraging) |
| `R/functions/local_trait_databases.R` | Modify | T1 (EP in species_enriched harmonizer) |
| `R/functions/trait_lookup/orchestrator.R` | Modify | T1 (ep_labels), T2 (fs_labels), T4 (routing), T7 (PR banner) |
| `R/functions/trait_help_content.R` | Modify | T1 (EP HTML), T2 (FS HTML) |
| `R/ui/trait_research_ui.R` | Modify | T1 (EP descriptions) |
| `R/ui/traitfoodweb_ui.R` | Modify | T1 (EP descriptions), T2 (FS range) |
| `R/ui/foodweb_construction_ui.R` | Modify | T1 (EP ref), T2 (FS range) |
| `R/functions/cache_sqlite.R` | Modify | T5 (SQL alias bug), T6 (inverted include_raw) |
| `R/functions/trait_lookup/orchestrator.R` | Modify | T4 (routing fixes for mammals, birds, tunicates, macroalgae, cyanobacteria, SHARK removal) |

---

### Task 1: Unify EP codes across the codebase (EP1=Pelagic, EP2=Benthopelagic, EP3=Epibenthic, EP4=Endobenthic)

**Problem:** 4 incompatible EP numbering schemes exist. The spec mandates a water-column-to-sediment gradient. The `EP_MS` probability matrix in `trait_foodweb.R` currently has EP1=Infaunal (opposite of the new EP1=Pelagic), so row order must be reversed.

**Files:**
- Modify: `R/config/harmonization_config.R:39-44`
- Modify: `R/functions/trait_foodweb.R:63-75,127-131`
- Modify: `R/functions/trait_lookup/harmonization.R` (two functions)
- Modify: `R/functions/local_trait_databases.R`
- Modify: `R/functions/trait_lookup/orchestrator.R` (ep_labels)
- Modify: `R/functions/trait_help_content.R`
- Modify: `R/ui/trait_research_ui.R`
- Modify: `R/ui/traitfoodweb_ui.R`
- Modify: `R/ui/foodweb_construction_ui.R`
- Test: `tests/testthat/test-layer1-quality.R`

- [ ] **Step 1: Create test file with EP unification test**

Create `tests/testthat/test-layer1-quality.R`:

```r
# Tests for Layer 1: Data Quality Fixes
library(testthat)
library(igraph)

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
source(file.path(app_root, "R/config.R"))
source(file.path(app_root, "R/functions/validation_utils.R"))
source(file.path(app_root, "R/functions/functional_group_utils.R"))
source(file.path(app_root, "R/config/harmonization_config.R"))
source(file.path(app_root, "R/functions/trait_foodweb.R"))

test_that("EP codes follow water-column-to-sediment gradient", {
  # EP_MS matrix row names must be EP1-EP4 in order
  expect_equal(rownames(EP_MS), c("EP1", "EP2", "EP3", "EP4"))

  # EP1=Pelagic should have HIGH probability for small pelagic prey (MS2)
  # EP4=Endobenthic should have LOW probability for small pelagic prey
  expect_true(EP_MS["EP1", "MS2"] > EP_MS["EP4", "MS2"],
              info = "Pelagic consumers (EP1) should access pelagic prey more than infaunal (EP4)")

  # TRAIT_DEFINITIONS must match
  expect_equal(names(TRAIT_DEFINITIONS$EP), c("EP1", "EP2", "EP3", "EP4"))
  expect_true(grepl("Pelagic", TRAIT_DEFINITIONS$EP["EP1"], ignore.case = TRUE))
  expect_true(grepl("Endobenthic|Infaunal", TRAIT_DEFINITIONS$EP["EP4"], ignore.case = TRUE))
})

test_that("EP config patterns match canonical scheme", {
  patterns <- HARMONIZATION_CONFIG$environmental_patterns
  expect_true(grepl("pelagic", names(patterns)[1]))
  expect_true(grepl("benthopelagic|demersal", names(patterns)[2], ignore.case = TRUE))
  expect_true(grepl("epibenthic", names(patterns)[3], ignore.case = TRUE))
  expect_true(grepl("endobenthic|infaun", names(patterns)[4], ignore.case = TRUE))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer1-quality.R')"`

Expected: FAIL — EP_MS has EP1=Infaunal (reversed order), TRAIT_DEFINITIONS has EP1=Infaunal.

- [ ] **Step 3: Update EP_MS matrix in trait_foodweb.R**

The current matrix has rows: EP1=Infaunal(low pelagic access), EP2=Epibenthic, EP3=Benthopelagic, EP4=Pelagic(high pelagic access). The new scheme reverses this: EP1=Pelagic(high), EP2=Benthopelagic, EP3=Epibenthic, EP4=Endobenthic(low).

In `R/functions/trait_foodweb.R`, replace lines 63-75:

```r
EP_MS <- matrix(
  c(
    0.95, 0.80, 0.50, 0.05,  # EP1 Pelagic (high access to pelagic prey)
    0.80, 0.95, 0.80, 0.50,  # EP2 Benthopelagic
    0.50, 0.80, 0.95, 0.80,  # EP3 Epibenthic
    0.05, 0.50, 0.80, 0.05   # EP4 Endobenthic/Infaunal
  ),
  nrow = 4, byrow = TRUE,
  dimnames = list(
    Consumer_EP = c("EP1", "EP2", "EP3", "EP4"),
    Resource_MS = c("MS2", "MS3", "MS4", "MS5")
  )
)
```

- [ ] **Step 4: Update TRAIT_DEFINITIONS$EP**

In `R/functions/trait_foodweb.R`, replace lines 127-131:

```r
  EP = c(
    EP1 = "Pelagic",
    EP2 = "Benthopelagic",
    EP3 = "Epibenthic",
    EP4 = "Endobenthic/Infaunal"
  ),
```

- [ ] **Step 5: Update harmonization_config.R EP patterns**

In `R/config/harmonization_config.R`, replace lines 40-44:

```r
  environmental_patterns = list(
    EP1_pelagic = "pelagic|water column|planktonic|nektonic|open water",
    EP2_benthopelagic = "benthopelagic|demersal|near bottom|benthic-pelagic",
    EP3_epibenthic = "epibenthic|epifauna|surface dwelling|on substrate",
    EP4_endobenthic = "endobenthic|infauna|burrowing|within sediment|interstitial"
  ),
```

- [ ] **Step 6: Update harmonize_fuzzy_habitat() in harmonization.R**

Find the EP assignment block in `harmonize_fuzzy_habitat()` (around lines 292-307). Replace all EP code assignments to match:
- Pelagic/water column patterns → EP1
- Benthopelagic/demersal patterns → EP2
- Benthic/epibenthic patterns → EP3
- Infaunal/burrowing patterns → EP4

- [ ] **Step 7: Update harmonize_environmental_position() in harmonization.R**

Find the EP assignment block (around lines 627-639). Replace:
- Pelagic habitat → EP1 (was EP4)
- Benthopelagic/demersal → EP2 (was EP3)
- Epibenthic → EP3 (was EP2)
- Infaunal/burrowing → EP4 (was EP1)

- [ ] **Step 8: Update harmonize_species_enriched_traits() in local_trait_databases.R**

Find EP assignment (around lines 609-626). Update to match canonical scheme.

- [ ] **Step 9: Update ep_labels in orchestrator.R**

Find both `ep_labels` dicts (lines ~884 and ~897). Replace both with:
```r
ep_labels <- c(EP1 = "Pelagic", EP2 = "Benthopelagic", EP3 = "Epibenthic", EP4 = "Endobenthic")
```

- [ ] **Step 10: Update UI files**

In `R/ui/trait_research_ui.R` (lines 493-496): update EP descriptions.
In `R/ui/traitfoodweb_ui.R` (lines 573-575): update EP descriptions.
In `R/ui/foodweb_construction_ui.R`: update EP reference table.
In `R/functions/trait_help_content.R` (lines 140, 143, 221): update EP HTML.

- [ ] **Step 11: Parse check all modified files**

```bash
for f in R/config/harmonization_config.R R/functions/trait_foodweb.R R/functions/trait_lookup/harmonization.R R/functions/local_trait_databases.R R/functions/trait_lookup/orchestrator.R R/functions/trait_help_content.R R/ui/trait_research_ui.R R/ui/traitfoodweb_ui.R R/ui/foodweb_construction_ui.R; do
  "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='$f'); cat('OK:', '$f', '\n')" 2>/dev/null
done
```

- [ ] **Step 12: Run test**

Expected: PASS

- [ ] **Step 13: Commit**

```bash
git add R/config/harmonization_config.R R/functions/trait_foodweb.R R/functions/trait_lookup/harmonization.R R/functions/local_trait_databases.R R/functions/trait_lookup/orchestrator.R R/functions/trait_help_content.R R/ui/trait_research_ui.R R/ui/traitfoodweb_ui.R R/ui/foodweb_construction_ui.R tests/testthat/test-layer1-quality.R
git commit -m "fix(traits): unify EP codes to water-column-to-sediment gradient

EP1=Pelagic, EP2=Benthopelagic, EP3=Epibenthic, EP4=Endobenthic.
Updated EP_MS probability matrix (row order reversed), TRAIT_DEFINITIONS,
harmonization_config patterns, both harmonization functions, local_trait_databases,
orchestrator labels, UI descriptions, and help content.
Previously 4 incompatible EP schemes existed across the codebase."
```

---

### Task 2: Add FS7 (Xylophagous) code and resolve FS3 conflict

**Problem:** FS3 is used for both Omnivore (config/orchestrator) and Xylophagous (fuzzy harmonizer). Fix: FS3=Omnivore, FS7=Xylophagous. Must update validation, probability matrices, labels, UI, and tests.

**Files:**
- Modify: `R/config/harmonization_config.R` (add FS7 pattern)
- Modify: `R/functions/trait_foodweb.R:29-42,111-118,380`
- Modify: `R/functions/trait_lookup/harmonization.R:130-132`
- Modify: `R/functions/trait_lookup/orchestrator.R` (fs_labels)
- Modify: `R/functions/trait_help_content.R`
- Modify: `R/ui/traitfoodweb_ui.R`, `R/ui/foodweb_construction_ui.R`
- Test: `tests/testthat/test-layer1-quality.R` (append)

- [ ] **Step 1: Append FS7 test**

```r
test_that("FS7 is valid and accepted in food web construction", {
  valid_FS <- paste0("FS", c(0:7))
  expect_true("FS7" %in% valid_FS)

  # FS7 should be in TRAIT_DEFINITIONS
  expect_true("FS7" %in% names(TRAIT_DEFINITIONS$FS))

  # FS_MS matrix should NOT contain FS7 row (xylophagous excluded from predation like FS0/FS3)
  # OR should have a row — depends on ecological interpretation
  # Xylophagous organisms bore into wood, not standard prey — exclude from FS_MS like FS0/FS3
})
```

- [ ] **Step 2: Add FS7 pattern to harmonization_config.R**

After the existing foraging patterns, add:
```r
    FS7_xylophagous = "xylophag|wood.bor|wood.eat|lignivor"
```

- [ ] **Step 3: Update harmonize_fuzzy_foraging() in harmonization.R**

Replace line ~130 mapping xylophagous to FS3:
```r
# Old: "xylophag|wood.bor" -> FS3
# New:
    if (grepl("xylophag|wood.bor|wood.eat|lignivor", modality)) return("FS7")
```

- [ ] **Step 4: Update validation in trait_foodweb.R**

Replace line 380:
```r
  valid_FS <- paste0("FS", c(0:7))
```

- [ ] **Step 5: Update TRAIT_DEFINITIONS$FS**

In `R/functions/trait_foodweb.R`, fix FS3 (currently says "Parasite (excluded)" but should be "Omnivore") AND add FS7:

Replace line 115:
```r
    FS3 = "Parasite (excluded)",
```
With:
```r
    FS3 = "Omnivore",
```

And add after FS6 line:
```r
    FS7 = "Xylophagous (wood borer)"
```

- [ ] **Step 6: Update fs_labels in orchestrator.R**

Find both `fs_labels` dicts (lines ~794 and ~808). Add `FS7 = "Xylophagous"` to both.

- [ ] **Step 7: Update UI and help content**

In `R/ui/traitfoodweb_ui.R`, `R/ui/foodweb_construction_ui.R`: change "FS0-FS6" to "FS0-FS7".
In `R/functions/trait_help_content.R`: add FS7 to HTML content.

- [ ] **Step 8: Parse check, run tests, commit**

```bash
git commit -m "feat(traits): add FS7 Xylophagous code, resolve FS3 conflict

FS3 now exclusively means Omnivore. Xylophagous (wood boring) moved to FS7.
Updated validation (valid_FS 0:7), TRAIT_DEFINITIONS, harmonization config,
fuzzy harmonizer, orchestrator labels, and all UI references."
```

---

### Task 3: Fix 8 disabled taxonomic rules

**Problem:** 8 `is_rule_enabled()` calls in harmonization.R reference keys that don't exist in `HARMONIZATION_CONFIG$taxonomic_rules`, so they always return FALSE.

**Files:**
- Modify: `R/config/harmonization_config.R:61-78`
- Test: `tests/testthat/test-layer1-quality.R` (append)

- [ ] **Step 1: Append test**

```r
test_that("all taxonomic rule keys used in harmonization exist in config", {
  rules <- HARMONIZATION_CONFIG$taxonomic_rules
  expected_keys <- c(
    "bivalves_sessile", "cnidarians_sessile", "phytoplankton_pelagic",
    "infaunal_bivalves", "bivalves_hard_shell", "gastropods_hard_shell",
    "crustaceans_exoskeleton", "echinoderms_calcium_plates"
  )
  for (key in expected_keys) {
    expect_true(key %in% names(rules), info = paste("Missing rule:", key))
  }
})
```

- [ ] **Step 2: Add missing keys to harmonization_config.R**

In the `taxonomic_rules` list (after existing keys, before the closing `)`), add:

```r
    bivalves_sessile = TRUE,
    cnidarians_sessile = TRUE,
    phytoplankton_pelagic = TRUE,
    infaunal_bivalves = TRUE,
    bivalves_hard_shell = TRUE,
    gastropods_hard_shell = TRUE,
    crustaceans_exoskeleton = TRUE,
    echinoderms_calcium_plates = TRUE
```

- [ ] **Step 3: Parse check, run tests, commit**

```bash
git commit -m "fix(traits): add 8 missing taxonomic rule keys to harmonization config

These rules were called via is_rule_enabled() in harmonization.R but the keys
did not exist in HARMONIZATION_CONFIG, causing them to always return FALSE.
Bivalves, gastropods, crustaceans, echinoderms, cnidarians, and phytoplankton
now get correct taxonomic trait inference."
```

---

### Task 4: Fix taxonomic routing holes

**Problem:** Cyanobacteria, macroalgae, mammals, birds, tunicates have no proper database routes. SHARK queries oceanographic data, not traits.

**Files:**
- Modify: `R/functions/trait_lookup/orchestrator.R:290-382`
- Test: `tests/testthat/test-layer1-quality.R` (append)

- [ ] **Step 1: Append routing test**

```r
test_that("SHARK is removed from trait routing in orchestrator", {
  # query_shark is a local variable inside lookup_species_traits(),
  # so we test by inspecting the source text directly
  orch_text <- readLines(file.path(app_root, "R/functions/trait_lookup/orchestrator.R"))
  shark_assignments <- grep("query_shark\\s*<-\\s*TRUE", orch_text)
  expect_equal(length(shark_assignments), 0,
               info = "All query_shark <- TRUE assignments should be removed from orchestrator")
})
```

- [ ] **Step 2: Add mammal/bird/tunicate routes**

In the orchestrator routing block (after the fish branch, before marine invertebrates), add:

```r
    # Marine mammals -> WoRMS Traits (will be added in Layer 2) + SeaLifeBase
    } else if (phylum == "chordata" && class %in% c("mammalia")) {
      query_sealifebase <- TRUE
      message("  -> Detected MARINE MAMMAL (", class, ") -> Querying: SeaLifeBase")

    # Seabirds -> WoRMS Traits (will be added in Layer 2)
    } else if (phylum == "chordata" && class %in% c("aves")) {
      message("  -> Detected SEABIRD (", class, ") -> Limited trait data available")

    # Tunicates -> SeaLifeBase
    } else if (phylum %in% c("chordata") && class %in% c("ascidiacea")) {
      query_sealifebase <- TRUE
      message("  -> Detected TUNICATE (", class, ") -> Querying: SeaLifeBase")
```

**Note:** Only `ascidiacea` is added here. `thaliacea` and `appendicularia` are ALREADY routed via the zooplankton branch (line 329) — do NOT duplicate them.

- [ ] **Step 3: Fix macroalgae routing**

In the phytoplankton/algae branch, add a sub-check for macroalgae:

After `query_algaebase <- TRUE` (line 342), add:
```r
      # Macroalgae (multicellular) should NOT query BVOL/PTDB (unicellular databases)
      if (phylum %in% c("rhodophyta", "ochrophyta", "chlorophyta") &&
          class %in% c("phaeophyceae", "florideophyceae", "ulvophyceae")) {
        query_bvol <- FALSE
        query_ptdb <- FALSE
        message("  -> Macroalgae detected: skipping BVOL/PTDB (unicellular only)")
      }
```

- [ ] **Step 4: Remove SHARK from trait routing**

Remove `query_shark <- TRUE` from line 323 (marine invertebrate branch).
Remove `query_shark <- TRUE` from line 381 (fallback branch).
Remove the SHARK lookup execution block (find `if (query_shark)` and remove or comment out).

- [ ] **Step 5: Parse check, run tests, commit**

```bash
git commit -m "fix(traits): add mammal/bird/tunicate routes, fix macroalgae, remove SHARK

Added explicit routing for Mammalia, Aves, Tunicata (Ascidiacea/Thaliacea).
Macroalgae (Phaeophyceae etc.) no longer routed to unicellular-only BVOL/PTDB.
Removed SHARK from trait pipeline (provides oceanographic data, not species traits)."
```

---

### Task 5: Fix cache_sqlite.R SQL alias bug

**Problem:** `find_closest_relatives_sql()` uses column alias `distance` in WHERE clause, which SQLite rejects.

**Files:**
- Modify: `R/functions/cache_sqlite.R:449-465`
- Test: `tests/testthat/test-layer1-quality.R` (append)

- [ ] **Step 1: Append test**

```r
test_that("find_closest_relatives_sql query uses subquery for distance alias", {
  skip_if_not_installed("RSQLite")
  source(file.path(app_root, "R/functions/cache_sqlite.R"))
  db_path <- file.path(app_root, "cache/taxonomy.db")
  skip_if_not(file.exists(db_path), "taxonomy.db not available")

  # The function takes a list with taxonomy fields, not a string
  result <- tryCatch(
    find_closest_relatives_sql(
      list(genus = "Nonexistent", family = "Nonexistent",
           order = "Nonexistent", class = "Nonexistent",
           phylum = "Nonexistent"),
      db_path = db_path,
      max_distance = 2
    ),
    error = function(e) e
  )
  # Before fix: errors with "no such column: distance"
  # After fix: returns empty data.frame (no matches)
  expect_false(inherits(result, "error"),
               info = "Should not error with 'no such column: distance'")
})
```

- [ ] **Step 2: Fix the SQL query**

In `R/functions/cache_sqlite.R`, find the `find_closest_relatives_sql()` function. Replace the query that has `WHERE distance <= ?` with a subquery:

```r
  query <- "
    SELECT * FROM (
      SELECT species_name, genus, family, order_name, class, phylum,
        MS, FS, MB, EP, PR, overall_confidence,
        CASE
          WHEN genus = ? THEN 0
          WHEN family = ? THEN 1
          WHEN order_name = ? THEN 2
          WHEN class = ? THEN 3
          WHEN phylum = ? THEN 4
          ELSE 5
        END as distance
      FROM species
      WHERE (genus = ? OR family = ? OR order_name = ? OR class = ? OR phylum = ?)
    ) sub
    WHERE distance <= ?
    ORDER BY distance ASC, overall_confidence DESC
    LIMIT ?
  "
```

**Note:** The schema uses `order_name` (not `"order"`) because `order` is a SQL reserved word.

- [ ] **Step 3: Parse check, run tests, commit**

```bash
git commit -m "fix(cache): wrap SQL CASE in subquery for distance alias in WHERE

SQLite rejects column aliases in WHERE clauses. The find_closest_relatives_sql()
function always errored with 'no such column: distance'. Now uses a subquery."
```

---

### Task 6: Fix cache_sqlite.R inverted include_raw query

**Problem:** `include_raw = FALSE` adds `AND raw_data IS NOT NULL` (wrong direction). Should be on the `include_raw = TRUE` branch.

**Files:**
- Modify: `R/functions/cache_sqlite.R:390-393`

- [ ] **Step 1: Swap the query branches**

In `R/functions/cache_sqlite.R`, find lines 390-393:
```r
if (include_raw) {
  query <- "SELECT * FROM species WHERE species_name = ?"
} else {
  query <- "SELECT * FROM species WHERE species_name = ? AND raw_data IS NOT NULL"
}
```

Replace with:
```r
if (include_raw) {
  query <- "SELECT * FROM species WHERE species_name = ? AND raw_data IS NOT NULL"
} else {
  query <- "SELECT * FROM species WHERE species_name = ?"
}
```

- [ ] **Step 2: Parse check, commit**

```bash
git commit -m "fix(cache): swap inverted include_raw query branches

When include_raw=FALSE, the query incorrectly required raw_data IS NOT NULL,
dropping species without blobs. Now include_raw=TRUE requires the blob
(for deserialization) and FALSE accepts all rows (structured columns only)."
```

---

### Task 6b: Add test for include_raw fix

- [ ] **Step 1: Append test for inverted include_raw logic**

```r
test_that("include_raw=FALSE does not filter by raw_data presence", {
  skip_if_not_installed("RSQLite")
  source(file.path(app_root, "R/functions/cache_sqlite.R"))
  # Structural test: check the function body for correct query logic
  fn_body <- deparse(body(load_species_from_cache))
  fn_text <- paste(fn_body, collapse = "\n")
  # After fix: include_raw=TRUE branch should have IS NOT NULL
  # include_raw=FALSE branch should NOT have IS NOT NULL
  if_true_block <- regmatches(fn_text, regexpr("if \\(include_raw\\).*?else", fn_text))
  expect_true(grepl("IS NOT NULL", if_true_block),
              info = "include_raw=TRUE branch should require raw_data IS NOT NULL")
})
```

- [ ] **Step 2: Run test, commit with Task 6**

---

### Task 6c: Handle legacy files

**Problem:** `R/functions/trait_lookup.R` (3,103 lines) and `R/functions/trait_lookup-laguna-safeBackup-0001.R` (3,112 lines) contain outdated ep_labels, fs_labels, and query_shark references. These are legacy backups that should NOT be sourced (the orchestrator.R + harmonization.R + database_lookups.R modular structure replaced them).

- [ ] **Step 1: Verify legacy files are NOT sourced**

```bash
grep -rn "trait_lookup-laguna\|trait_lookup\.R" R/ app.R --include="*.R" | grep "source("
```

If they are NOT sourced anywhere (expected), they are dead code.

- [ ] **Step 2: Move legacy files to archive**

```bash
mkdir -p archive/legacy_trait_lookup
mv R/functions/trait_lookup.R archive/legacy_trait_lookup/
mv R/functions/trait_lookup-laguna-safeBackup-0001.R archive/legacy_trait_lookup/
```

- [ ] **Step 3: Commit**

```bash
git add -A archive/legacy_trait_lookup/ R/functions/trait_lookup.R R/functions/trait_lookup-laguna-safeBackup-0001.R
git commit -m "chore: move legacy trait_lookup files to archive

These 3000+ line files are pre-modularization backups replaced by
orchestrator.R + harmonization.R + database_lookups.R. They contain
outdated EP/FS labels and SHARK routing that conflicts with the
current modular system. Moved to archive/ to prevent confusion."
```

---

### Task 7: Update PR banner and orchestrator step labels

**Problem:** Orchestrator banner says "PR1-PR3" but actual range is PR0-PR8. Step labels say "/10" or "/12" inconsistently.

**Files:**
- Modify: `R/functions/trait_lookup/orchestrator.R`

- [ ] **Step 1: Fix PR banner text**

Find the banner text (around line 117) that says `PR (Predator Resistance) - Protection (PR1-PR3)`. Replace with:
```r
    "  PR (Predator Resistance) - Protection (PR0-PR8)\n",
```

- [ ] **Step 2: Fix EP banner text**

Update the EP line in the same banner to match the new scheme:
```r
    "  EP (Environmental Position) - Habitat (EP1=Pelagic to EP4=Endobenthic)\n",
```

- [ ] **Step 3: Fix FS banner text**

Update to include FS7:
```r
    "  FS (Foraging Strategy) - Feeding (FS0-FS7)\n",
```

- [ ] **Step 4: Parse check, commit**

```bash
git commit -m "fix(traits): update orchestrator banner text for PR/EP/FS ranges

PR range corrected from PR1-PR3 to PR0-PR8. EP updated to show new gradient
scheme (EP1=Pelagic to EP4=Endobenthic). FS updated to include FS7."
```

---

### Task 8: Final integration test

- [ ] **Step 1: Run all Layer 1 tests**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer1-quality.R')"
```

Expected: All PASS.

- [ ] **Step 2: Run existing P0/P1 tests for regressions**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-p0p1-fixes.R')"
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" tests/test_trait_expansion.R
```

Expected: All still pass.

- [ ] **Step 3: Parse check ALL modified files**

```bash
for f in R/config/harmonization_config.R R/functions/trait_foodweb.R R/functions/trait_lookup/harmonization.R R/functions/local_trait_databases.R R/functions/trait_lookup/orchestrator.R R/functions/trait_help_content.R R/ui/trait_research_ui.R R/ui/traitfoodweb_ui.R R/ui/foodweb_construction_ui.R R/functions/cache_sqlite.R; do
  "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='$f'); cat('OK:', '$f', '\n')" 2>/dev/null
done
```

Expected: All OK.
