# Hotfix: Critical Issues from Deep Review

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Fix 1 critical security vulnerability (R code injection), 1 critical data correctness bug (EP inversion), 1 high-severity silent failure (CSV path resolution), and 4 additional high-priority issues discovered in the deep codebase review.

**Architecture:** 7 targeted fixes, each independently committable. Fixes are ordered: security first (C1), then data correctness (H1), then silent failures (H2-H6).

**Tech Stack:** R 4.4.1, testthat. Rscript: `"/c/Program Files/R/R-4.4.1/bin/Rscript.exe"`

---

## File Structure

| File | Action | Fix |
|------|--------|-----|
| `R/modules/plugin_server.R` | Modify | C1: Sanitize API key input (R code injection) |
| `R/functions/trait_lookup/harmonization.R` | Modify | H1: Fix EP inversion in taxonomic inference + depth mapping |
| `R/functions/trait_lookup/csv_trait_databases.R` | Modify | H2: Replace sys.frame(1)$ofile with simple relative paths |
| `R/functions/trait_lookup/api_trait_databases.R` | Modify | H3: Add message() to requireNamespace guards |
| `R/functions/cache_sqlite.R` | Modify | H4: Only suppress "duplicate column" errors in migration |
| `R/functions/ml_trait_prediction.R` | Modify | H6: Return NULL instead of zero matrix when ape missing |
| `tests/testthat/test-hotfix.R` | Create | Tests for all fixes |

---

### Task 1: Fix C1 — R code injection in API key modal

**Problem:** User input is interpolated directly into `paste0()` and written to `config/api_keys.R` which is later `source()`'d. A username containing `"; system("whoami"); "` would execute arbitrary R code.

**Files:**
- Modify: `R/modules/plugin_server.R:174-185`
- Test: `tests/testthat/test-hotfix.R`

- [ ] **Step 1: Create test file**

```r
# Tests for hotfix: critical issues from deep review
library(testthat)

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")

test_that("API key saving uses JSON format, not source()'d R code", {
  server_text <- readLines(file.path(app_root, "R/modules/plugin_server.R"))
  server_joined <- paste(server_text, collapse = "\n")
  # Should use write_json or deparse, NOT raw paste0 interpolation
  expect_true(
    grepl("write_json|toJSON|deparse|sanitize", server_joined, ignore.case = TRUE),
    info = "API key saving must sanitize input or use non-executable format (JSON)"
  )
  expect_false(
    grepl('paste0.*API_KEYS.*\\$.*<-.*\\".*input\\$', server_joined),
    info = "Must NOT use raw paste0 interpolation of user input into R source code"
  )
})
```

- [ ] **Step 2: Replace paste0 interpolation with JSON format**

In `R/modules/plugin_server.R`, replace lines 174-185 (the `save_api_keys` observer):

```r
  observeEvent(input$save_api_keys, {
    dir.create("config", showWarnings = FALSE)
    # Use JSON format to avoid R code injection via source()
    keys_list <- list(
      algaebase_username = input$api_key_algaebase_user,
      algaebase_password = input$api_key_algaebase_pass,
      freshwaterecology_key = input$api_key_freshwater
    )
    jsonlite::write_json(keys_list, "config/api_keys.json", auto_unbox = TRUE, pretty = TRUE)

    # Also update in-memory API_KEYS if it exists
    if (exists("API_KEYS", envir = .GlobalEnv)) {
      API_KEYS$algaebase_username <<- input$api_key_algaebase_user
      API_KEYS$algaebase_password <<- input$api_key_algaebase_pass
      API_KEYS$freshwaterecology_key <<- input$api_key_freshwater
    }

    removeModal()
    showNotification("API keys saved successfully!", type = "message")
  })
```

**Note:** This changes the storage format from `.R` (source'd) to `.json` (read). The config loading in `R/config.R` must also be updated to read JSON instead of sourcing R. Add after the existing `source(API_KEYS_FILE)` block:

```r
# Also try JSON format (safer)
json_keys_file <- "config/api_keys.json"
if (file.exists(json_keys_file)) {
  tryCatch({
    json_keys <- jsonlite::fromJSON(json_keys_file)
    for (key in names(json_keys)) {
      API_KEYS[[key]] <- json_keys[[key]]
    }
  }, error = function(e) message("Could not load API keys from JSON: ", e$message))
}
```

- [ ] **Step 3: Parse check, run test, commit**

```bash
git commit -m "$(cat <<'EOF'
security(api): fix R code injection in API key modal

CRITICAL: User input was interpolated directly into paste0() and written
to config/api_keys.R which was source()'d — allowing arbitrary code
execution. Now uses JSON format (config/api_keys.json) which is parsed,
not executed. Also loads JSON keys in config.R alongside the legacy .R format.

Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 2: Fix H1 — EP code inversion in harmonize_environmental_position()

**Problem:** The taxonomic inference block (lines 668-700) returns EP4 for pelagic species and EP1 for infaunal species — the OPPOSITE of the canonical scheme (EP1=Pelagic, EP4=Endobenthic). Also the depth mapping (lines 646-660) returns EP1 for shallow burrowing species (should be EP4).

**Files:**
- Modify: `R/functions/trait_lookup/harmonization.R:646-700`
- Test: `tests/testthat/test-hotfix.R` (append)

- [ ] **Step 1: Append test**

```r
source(file.path(app_root, "R/config.R"))
source(file.path(app_root, "R/functions/validation_utils.R"))
source(file.path(app_root, "R/config/harmonization_config.R"))
source(file.path(app_root, "R/functions/trait_lookup/harmonization.R"))

test_that("harmonize_environmental_position returns EP1 for pelagic species", {
  # Phytoplankton (photosynthesizers) should be EP1 (Pelagic)
  result <- harmonize_environmental_position(
    habitat_info = NULL, depth_min = NULL, depth_max = NULL,
    taxonomic_info = list(phylum = "Ochrophyta", class = "Bacillariophyceae",
                          feeding_mode = "photosynthesis")
  )
  expect_equal(result, "EP1", info = "Phytoplankton should be EP1 (Pelagic)")
})

test_that("harmonize_environmental_position returns EP1 for fish", {
  result <- harmonize_environmental_position(
    habitat_info = NULL, depth_min = NULL, depth_max = NULL,
    taxonomic_info = list(phylum = "Chordata", class = "Actinopteri")
  )
  expect_equal(result, "EP1", info = "Fish should default to EP1 (Pelagic)")
})

test_that("harmonize_environmental_position returns EP4 for infaunal bivalves", {
  result <- harmonize_environmental_position(
    habitat_info = NULL, depth_min = NULL, depth_max = NULL,
    taxonomic_info = list(phylum = "Mollusca", class = "Bivalvia")
  )
  expect_equal(result, "EP4", info = "Infaunal bivalves should be EP4 (Endobenthic)")
})

test_that("depth-based EP returns EP4 for shallow burrowers", {
  result <- harmonize_environmental_position(
    habitat_info = c("burrowing"), depth_min = 5, depth_max = 20,
    taxonomic_info = NULL
  )
  expect_equal(result, "EP4", info = "Shallow burrowers should be EP4 (Endobenthic)")
})
```

- [ ] **Step 2: Fix taxonomic inference — swap EP codes**

In `R/functions/trait_lookup/harmonization.R`, fix lines 668-700:

**Line 672:** Change `return("EP4")` to `return("EP1")` (phytoplankton → Pelagic)
**Line 676:** Change `return("EP4")` to `return("EP1")` (diatoms/dinos → Pelagic)
**Line 683:** Change `return("EP4")` to `return("EP1")` (copepods/cladocerans → Pelagic)
**Line 692:** Change `return("EP1")` to `return("EP4")` (infaunal bivalves → Endobenthic)
**Line 699:** Change `return("EP4")` to `return("EP1")` (fish → Pelagic)

Also fix depth mapping (lines 646-660):
**Line 652:** Change `return("EP1")` to `return("EP4")` (shallow burrowing → Endobenthic)
**Line 654:** Keep `return("EP2")` (shallow non-burrowing → Epibenthic... wait, EP2=Benthopelagic in canonical scheme). Actually EP3=Epibenthic. Change to `return("EP3")`.
**Line 659:** Keep `return("EP3")` (deep → Epibenthic... wait, deep species benthopelagic = EP2). Change to `return("EP2")`.

Summary of depth mapping fixes:
- Line 652: `EP1` → `EP4` (burrowing = Endobenthic)
- Line 654: `EP2` → `EP3` (shallow non-burrowing = Epibenthic)
- Line 659: `EP3` → `EP2` (deep = Benthopelagic)

- [ ] **Step 3: Parse check, run test, commit**

```bash
git commit -m "$(cat <<'EOF'
fix(traits): correct EP code inversion in harmonize_environmental_position

CRITICAL: Taxonomic inference returned EP4 for pelagic species and EP1
for infaunal — the opposite of the canonical scheme (EP1=Pelagic,
EP4=Endobenthic). Fixed: phytoplankton/zooplankton/fish → EP1,
infaunal bivalves → EP4. Also fixed depth mapping: shallow burrowing
→ EP4 (was EP1), deep → EP2 (was EP3).

Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 3: Fix H2 — CSV database path resolution

**Problem:** All 5 CSV lookup functions use `sys.frame(1)$ofile` for default paths — fails at runtime in Shiny (returns NULL), silently disabling all 5 databases.

**Files:**
- Modify: `R/functions/trait_lookup/csv_trait_databases.R` (5 function signatures)
- Test: `tests/testthat/test-hotfix.R` (append)

- [ ] **Step 1: Append test**

```r
test_that("CSV lookup functions use simple relative paths, not sys.frame", {
  csv_text <- readLines(file.path(app_root, "R/functions/trait_lookup/csv_trait_databases.R"))
  csv_joined <- paste(csv_text, collapse = "\n")
  expect_false(grepl("sys.frame", csv_joined),
               info = "CSV functions must not use sys.frame(1)$ofile (fails in Shiny)")
  expect_true(grepl('data/external_traits|data.external_traits', csv_joined),
              info = "CSV functions should use simple relative paths")
})
```

- [ ] **Step 2: Replace all 5 default csv_file parameters**

In `R/functions/trait_lookup/csv_trait_databases.R`, replace each function's default parameter. There are 5 occurrences of the `sys.frame` pattern:

**lookup_blacksea_traits (line ~90-93):**
```r
    csv_file = file.path("data", "external_traits", "blacksea_traits.csv")
```

**lookup_arctic_traits (line ~133-135):**
```r
    csv_file = file.path("data", "external_traits", "arctic_traits.csv")
```

**lookup_cefas_traits (line ~177-179):**
```r
    csv_file = file.path("data", "external_traits", "cefas_benthic_traits.csv")
```

**lookup_coral_traits (line ~221-224):**
```r
    csv_file = file.path("data", "external_traits", "coral_traits.csv")
```

**lookup_pelagic_traits (line ~262-264):**
```r
    csv_file = file.path("data", "external_traits", "pelagic_traits.csv")
```

- [ ] **Step 3: Parse check, run test, commit**

```bash
git commit -m "fix(traits): replace sys.frame(1) with relative paths in CSV lookups

sys.frame(1)\$ofile returns NULL at runtime in Shiny, silently disabling
all 5 CSV trait databases. Now uses simple file.path('data', 'external_traits', ...)
which resolves correctly from Shiny's working directory."
```

---

### Task 4: Fix H3 — Add messages to silent requireNamespace guards

**Problem:** 7 `requireNamespace(..., quietly=TRUE)` calls in api_trait_databases.R silently return empty results when packages are missing.

**Files:**
- Modify: `R/functions/trait_lookup/api_trait_databases.R`
- Test: `tests/testthat/test-hotfix.R` (append)

- [ ] **Step 1: Append test**

```r
test_that("API lookup functions emit message when package is missing", {
  api_text <- readLines(file.path(app_root, "R/functions/trait_lookup/api_trait_databases.R"))
  api_joined <- paste(api_text, collapse = "\n")
  # Every requireNamespace guard should have a message() before return
  # Count requireNamespace lines vs message lines near them
  req_lines <- grep("requireNamespace.*quietly.*TRUE", api_text)
  for (line_num in req_lines) {
    # Check next 3 lines for a message()
    nearby <- paste(api_text[line_num:min(line_num + 3, length(api_text))], collapse = " ")
    expect_true(grepl("message\\(", nearby),
                info = paste("Missing message() near requireNamespace at line", line_num))
  }
})
```

- [ ] **Step 2: Add message() to each guard**

Replace each silent return pattern. For example, line 41:

Before:
```r
  if (!requireNamespace("worrms", quietly = TRUE)) return(result)
```

After:
```r
  if (!requireNamespace("worrms", quietly = TRUE)) {
    message("  [WoRMS Traits] Package 'worrms' not installed. Install with: install.packages('worrms')")
    return(result)
  }
```

Apply to ALL 7 occurrences (lines 41, 100, 101, 181, 233, 308, 309). For compound guards (lines 100-101, 308-309 where httr AND jsonlite are checked), combine into one block:

```r
  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    message("  [PolyTraits] Packages 'httr' and 'jsonlite' required. Install with: install.packages(c('httr', 'jsonlite'))")
    return(result)
  }
```

- [ ] **Step 3: Parse check, run test, commit**

```bash
git commit -m "fix(traits): add diagnostic messages to API lookup package guards

Silent requireNamespace returns were indistinguishable from 'no data found'.
Now each guard emits a message with the package name and install command."
```

---

### Task 5: Fix H4 — Schema migration error specificity

**Problem:** `migrate_offline_schema()` catches ALL ALTER TABLE errors including corruption/disk-full. Should only suppress "duplicate column" errors.

**Files:**
- Modify: `R/functions/cache_sqlite.R` (in `migrate_offline_schema`)
- Test: `tests/testthat/test-hotfix.R` (append)

- [ ] **Step 1: Append test**

```r
test_that("schema migration only suppresses duplicate column errors", {
  migration_text <- readLines(file.path(app_root, "R/functions/cache_sqlite.R"))
  migration_joined <- paste(migration_text, collapse = "\n")
  expect_true(grepl("duplicate column", migration_joined, ignore.case = TRUE),
              info = "Migration should check for 'duplicate column' error specifically")
})
```

- [ ] **Step 2: Fix the error handler**

In `R/functions/cache_sqlite.R`, find the `tryCatch` inside `migrate_offline_schema()`. Replace:

```r
      tryCatch(dbExecute(con, sql), error = function(e) {
        message("Schema migration: column ", col_name, " - ", e$message)
      })
```

With:

```r
      tryCatch(dbExecute(con, sql), error = function(e) {
        if (grepl("duplicate column", e$message, ignore.case = TRUE)) {
          # Expected: column already exists (idempotent migration)
        } else {
          warning("Schema migration FAILED for column '", col_name, "': ", e$message,
                  "\n  The offline trait database may be corrupt or read-only.", call. = FALSE)
        }
      })
```

- [ ] **Step 3: Parse check, run test, commit**

```bash
git commit -m "fix(cache): only suppress duplicate column errors in schema migration

Previously, ALL ALTER TABLE errors were swallowed (including disk corruption,
read-only FS, table-not-found). Now only 'duplicate column' is suppressed
(expected for idempotent migration). Other errors emit a warning."
```

---

### Task 6: Fix H6 — compute_phylo_eigenvectors returns NULL when ape missing

**Problem:** Returns zero matrix (not NULL) when `ape` is missing — downstream RF uses all-zero features producing garbage predictions silently.

**Files:**
- Modify: `R/functions/ml_trait_prediction.R`
- Test: `tests/testthat/test-hotfix.R` (append)

- [ ] **Step 1: Append test**

```r
source(file.path(app_root, "R/functions/ml_trait_prediction.R"))

test_that("compute_phylo_eigenvectors returns NULL when ape unavailable", {
  # We can't unload ape, but we can verify the function's guard clause
  fn_text <- deparse(body(compute_phylo_eigenvectors))
  fn_joined <- paste(fn_text, collapse = "\n")
  expect_true(grepl("return\\(NULL\\)", fn_joined),
              info = "Should return NULL (not zero matrix) when ape is missing")
})
```

- [ ] **Step 2: Fix the guard clause**

In `R/functions/ml_trait_prediction.R`, find `compute_phylo_eigenvectors()`. Replace the `ape` guard:

Before:
```r
  if (!requireNamespace("ape", quietly = TRUE)) {
    warning("Package 'ape' required for phylogenetic eigenvectors")
    return(matrix(0, nrow = nrow(taxonomy_df), ncol = n_vectors))
  }
```

After:
```r
  if (!requireNamespace("ape", quietly = TRUE)) {
    warning("Package 'ape' required for phylogenetic eigenvectors. Install with: install.packages('ape')")
    return(NULL)
  }
```

Also fix the small-data guard (n_species < 3):

Before:
```r
  if (n_species < 3) return(matrix(0, nrow = n_species, ncol = n_vectors))
```

After:
```r
  if (n_species < 3) return(NULL)
```

And the PCoA failure fallback:

Before:
```r
  if (is.null(pcoa_result) || is.null(pcoa_result$vectors)) {
    return(matrix(0, nrow = n_species, ncol = n_vectors))
  }
```

After:
```r
  if (is.null(pcoa_result) || is.null(pcoa_result$vectors)) {
    return(NULL)
  }
```

**Note:** The callers of `compute_phylo_eigenvectors()` must now handle NULL returns. The existing `prepare_ml_features()` function should check for NULL and proceed without eigenvectors if unavailable.

- [ ] **Step 3: Parse check, run test, commit**

```bash
git commit -m "fix(ml): return NULL instead of zero matrix when ape unavailable

Zero matrix was silently consumed by RF as valid features, producing
garbage predictions. NULL return forces callers to handle the missing
eigenvectors case explicitly."
```

---

### Task 7: Final integration test

- [ ] **Step 1: Run all test suites**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-hotfix.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer4-ui.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer3-ml.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer1-quality.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-p0p1-fixes.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" tests/test_trait_expansion.R 2>&1 | tail -3
```

- [ ] **Step 2: Parse check all modified files**

```bash
for f in R/modules/plugin_server.R R/functions/trait_lookup/harmonization.R R/functions/trait_lookup/csv_trait_databases.R R/functions/trait_lookup/api_trait_databases.R R/functions/cache_sqlite.R R/functions/ml_trait_prediction.R R/config.R; do
  "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='$f'); cat('OK:', '$f', '\n')" 2>/dev/null
done
```

Expected: All pass, all OK.
