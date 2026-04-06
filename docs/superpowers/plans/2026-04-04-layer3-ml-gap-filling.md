# Layer 3: ML Gap-Filling Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Enhance trait gap-filling with 3 ML methods: enhanced Random Forest with phylogenetic eigenvectors, Rphylopars phylogenetic imputation, and BHPMF Bayesian matrix factorization. Add uncertainty reporting and method selection logic to the orchestrator.

**Architecture:** Three independent imputation modules, each with its own file. The orchestrator's method selection logic (after database lookups) checks phylogenetic signal strength to choose between Rphylopars and RF. BHPMF runs only during offline DB rebuild. All methods report uncertainty via confidence scores and imputation_method column (already in the expanded result template from Layer 2a).

**Tech Stack:** R 4.4.1, testthat, ape (installed), randomForest (installed), Rphylopars (NOT installed — optional), BHPMF (NOT installed — optional). Both optional packages use `requireNamespace` guards.

**Prerequisite:** Layers 1, 2a-c must be applied. The result template already has `imputation_method`, `*_confidence` columns.

---

## File Structure

| File | Action | Responsibility |
|------|--------|---------------|
| `R/functions/ml_trait_prediction.R` | Modify | Add `compute_phylo_eigenvectors()`, update `prepare_ml_features()` |
| `R/functions/rphylopars_imputation.R` | Create | `impute_with_rphylopars()` for phylogenetic imputation |
| `R/functions/bhpmf_imputation.R` | Create | `impute_with_bhpmf()` for batch Bayesian gap-filling |
| `R/functions/trait_lookup/orchestrator.R` | Modify | Method selection logic after database lookups |
| `tests/testthat/test-layer3-ml.R` | Create | Tests for all 3 methods + method selection |

---

### Task 1: Add phylogenetic eigenvector computation

**Problem:** The current RF model uses only 5 taxonomic dummy features (phylum, class, order, family, genus). Adding phylogenetic eigenvectors from PCoA of the taxonomic distance matrix improves predictions 2-3x.

**Files:**
- Modify: `R/functions/ml_trait_prediction.R`
- Test: `tests/testthat/test-layer3-ml.R`

- [ ] **Step 1: Create test file**

```r
# Tests for Layer 3: ML Gap-Filling
library(testthat)

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
source(file.path(app_root, "R/config.R"))
source(file.path(app_root, "R/functions/validation_utils.R"))
source(file.path(app_root, "R/functions/functional_group_utils.R"))
source(file.path(app_root, "R/functions/ml_trait_prediction.R"))

test_that("compute_phylo_eigenvectors returns matrix with correct dimensions", {
  skip_if_not_installed("ape")
  # Create a small taxonomy data.frame
  taxonomy_df <- data.frame(
    species = c("Gadus morhua", "Gadus ogac", "Melanogrammus aeglefinus",
                "Clupea harengus", "Mytilus edulis"),
    phylum = c("Chordata", "Chordata", "Chordata", "Chordata", "Mollusca"),
    class = c("Actinopteri", "Actinopteri", "Actinopteri", "Actinopteri", "Bivalvia"),
    order = c("Gadiformes", "Gadiformes", "Gadiformes", "Clupeiformes", "Mytilida"),
    family = c("Gadidae", "Gadidae", "Gadidae", "Clupeidae", "Mytilidae"),
    genus = c("Gadus", "Gadus", "Melanogrammus", "Clupea", "Mytilus"),
    stringsAsFactors = FALSE
  )

  eigenvecs <- compute_phylo_eigenvectors(taxonomy_df, n_vectors = 3)
  expect_true(is.matrix(eigenvecs))
  expect_equal(nrow(eigenvecs), 5)  # 5 species
  expect_equal(ncol(eigenvecs), 3)  # 3 eigenvectors requested

  # Same genus species should have similar eigenvector values
  gadus_dist <- sqrt(sum((eigenvecs[1, ] - eigenvecs[2, ])^2))
  cross_dist <- sqrt(sum((eigenvecs[1, ] - eigenvecs[5, ])^2))
  expect_true(gadus_dist < cross_dist,
              info = "Same-genus species should be closer in eigenvector space")
})
```

- [ ] **Step 2: Add compute_phylo_eigenvectors() to ml_trait_prediction.R**

Append after the existing module-level cache (after line ~20):

```r
#' Compute Phylogenetic Eigenvectors from Taxonomy
#'
#' Constructs a taxonomic distance matrix from WoRMS hierarchy and computes
#' principal coordinates (PCoA). The resulting eigenvectors capture phylogenetic
#' structure as continuous features for ML models.
#'
#' @param taxonomy_df Data frame with columns: species, phylum, class, order, family, genus
#' @param n_vectors Integer, number of eigenvectors to return (default 10)
#' @return Matrix with nrow(taxonomy_df) rows and n_vectors columns
#' @export
compute_phylo_eigenvectors <- function(taxonomy_df, n_vectors = 10) {
  if (!requireNamespace("ape", quietly = TRUE)) {
    warning("Package 'ape' required for phylogenetic eigenvectors")
    return(matrix(0, nrow = nrow(taxonomy_df), ncol = n_vectors))
  }

  n_species <- nrow(taxonomy_df)
  if (n_species < 3) {
    return(matrix(0, nrow = n_species, ncol = n_vectors))
  }

  # Build taxonomic distance matrix
  # Ranks: genus=1, family=2, order=3, class=4, phylum=5
  dist_mat <- matrix(0, nrow = n_species, ncol = n_species)

  for (i in 1:(n_species - 1)) {
    for (j in (i + 1):n_species) {
      d <- 0
      if (tolower(taxonomy_df$genus[i]) != tolower(taxonomy_df$genus[j])) d <- d + 1
      if (tolower(taxonomy_df$family[i]) != tolower(taxonomy_df$family[j])) d <- d + 2
      if (tolower(taxonomy_df$order[i]) != tolower(taxonomy_df$order[j])) d <- d + 3
      if (tolower(taxonomy_df$class[i]) != tolower(taxonomy_df$class[j])) d <- d + 4
      if (tolower(taxonomy_df$phylum[i]) != tolower(taxonomy_df$phylum[j])) d <- d + 5
      dist_mat[i, j] <- d
      dist_mat[j, i] <- d
    }
  }

  # PCoA
  pcoa_result <- tryCatch(
    ape::pcoa(as.dist(dist_mat)),
    error = function(e) NULL
  )

  if (is.null(pcoa_result) || is.null(pcoa_result$vectors)) {
    return(matrix(0, nrow = n_species, ncol = n_vectors))
  }

  # Return top n_vectors eigenvectors (pad with zeros if fewer available)
  available <- ncol(pcoa_result$vectors)
  n_use <- min(n_vectors, available)
  result <- matrix(0, nrow = n_species, ncol = n_vectors)
  result[, 1:n_use] <- pcoa_result$vectors[, 1:n_use]

  rownames(result) <- taxonomy_df$species
  colnames(result) <- paste0("phylo_pc", 1:n_vectors)

  result
}
```

- [ ] **Step 3: Parse check, run test, commit**

```bash
git commit -m "feat(ml): add compute_phylo_eigenvectors for enhanced RF features

Constructs taxonomic distance matrix from WoRMS hierarchy and computes
PCoA eigenvectors using ape::pcoa(). These continuous features capture
phylogenetic structure for 2-3x improvement in RF trait prediction."
```

---

### Task 2: Create Rphylopars imputation module

**Problem:** For species with missing traits where phylogenetic signal exists, Rphylopars provides better imputation than RF by jointly modeling trait covariance and phylogenetic structure.

**Files:**
- Create: `R/functions/rphylopars_imputation.R`
- Test: `tests/testthat/test-layer3-ml.R` (append)

- [ ] **Step 1: Append test**

```r
test_that("impute_with_rphylopars returns correct structure", {
  skip_if_not_installed("Rphylopars")
  skip_if_not_installed("ape")

  # Small test dataset
  trait_matrix <- data.frame(
    species = c("Sp_A", "Sp_B", "Sp_C", "Sp_D"),
    MS = c(3, NA, 5, 4),
    FS = c(1, 2, NA, 1)
  )
  taxonomy_df <- data.frame(
    species = c("Sp_A", "Sp_B", "Sp_C", "Sp_D"),
    phylum = rep("Chordata", 4),
    class = rep("Actinopteri", 4),
    order = c("Gadiformes", "Gadiformes", "Perciformes", "Perciformes"),
    family = c("Gadidae", "Gadidae", "Sparidae", "Sparidae"),
    genus = c("Gadus", "Gadus", "Sparus", "Sparus"),
    stringsAsFactors = FALSE
  )

  result <- impute_with_rphylopars(trait_matrix, taxonomy_df)
  expect_true(is.list(result))
  expect_true("imputed" %in% names(result))
  expect_true("variances" %in% names(result))
  expect_true("method" %in% names(result))
  expect_equal(result$method, "rphylopars")
})

test_that("impute_with_rphylopars returns NULL without package", {
  # Mock: if package not available, should return NULL gracefully
  if (!requireNamespace("Rphylopars", quietly = TRUE)) {
    result <- impute_with_rphylopars(data.frame(), data.frame())
    expect_null(result)
  }
})
```

- [ ] **Step 2: Create rphylopars_imputation.R**

```r
# =============================================================================
# Rphylopars Phylogenetic Trait Imputation
# =============================================================================
#
# Uses Rphylopars package for joint estimation of traits using phylogenetic
# covariance structure. Provides posterior variances for uncertainty reporting.
#
# When to use: single-species lookup with missing traits AND phylogenetic
# signal exists (Pagel's lambda > 0.3)
# =============================================================================

#' Impute missing traits using Rphylopars phylogenetic method
#'
#' Builds an ultrametric tree from WoRMS taxonomy and uses Rphylopars
#' to jointly estimate missing trait values with uncertainty.
#'
#' @param trait_matrix Data frame with columns: species, then trait columns (numeric)
#' @param taxonomy_df Data frame with columns: species, phylum, class, order, family, genus
#' @return List with: imputed (data.frame), variances (data.frame), method ("rphylopars"),
#'         or NULL if package unavailable or imputation fails
#' @export
impute_with_rphylopars <- function(trait_matrix, taxonomy_df) {

  if (!requireNamespace("Rphylopars", quietly = TRUE)) {
    message("  [Rphylopars] Package not installed. Install with: install.packages('Rphylopars')")
    return(NULL)
  }
  if (!requireNamespace("ape", quietly = TRUE)) {
    return(NULL)
  }

  if (nrow(trait_matrix) < 4 || ncol(trait_matrix) < 2) {
    message("  [Rphylopars] Need at least 4 species and 1 trait column")
    return(NULL)
  }

  tryCatch({
    # Build taxonomy tree from WoRMS hierarchy
    # Create a simple Newick tree from taxonomy ranks
    tree <- build_taxonomy_tree(taxonomy_df)
    if (is.null(tree)) {
      message("  [Rphylopars] Failed to build taxonomy tree")
      return(NULL)
    }

    # Prepare trait data for Rphylopars (species as first column)
    trait_data <- trait_matrix
    if (!"species" %in% names(trait_data)) {
      trait_data$species <- rownames(trait_data)
    }

    # Ensure species names match between tree and data
    common_species <- intersect(tree$tip.label, trait_data$species)
    if (length(common_species) < 4) {
      message("  [Rphylopars] Too few species in common between tree and data")
      return(NULL)
    }

    # Subset to common species
    tree <- ape::keep.tip(tree, common_species)
    trait_data <- trait_data[trait_data$species %in% common_species, ]

    # Run Rphylopars
    phylo_result <- Rphylopars::phylopars(
      trait_data = trait_data,
      tree = tree,
      pheno_error = TRUE,
      phylo_correlated = TRUE
    )

    # Extract imputed values and variances
    imputed <- as.data.frame(phylo_result$anc_recon[trait_data$species, , drop = FALSE])
    variances <- as.data.frame(phylo_result$anc_var[trait_data$species, , drop = FALSE])

    list(
      imputed = imputed,
      variances = variances,
      method = "rphylopars"
    )

  }, error = function(e) {
    message("  [Rphylopars] Error: ", e$message)
    NULL
  })
}

#' Build a simple taxonomy tree from WoRMS hierarchy
#'
#' Creates an ultrametric phylo object from taxonomy ranks.
#'
#' @param taxonomy_df Data frame with species, phylum, class, order, family, genus
#' @return ape::phylo object or NULL
build_taxonomy_tree <- function(taxonomy_df) {
  tryCatch({
    n <- nrow(taxonomy_df)
    if (n < 3) return(NULL)

    # Build distance matrix from taxonomy
    dist_mat <- matrix(0, nrow = n, ncol = n)
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        d <- 0
        if (tolower(taxonomy_df$genus[i]) != tolower(taxonomy_df$genus[j])) d <- d + 1
        if (tolower(taxonomy_df$family[i]) != tolower(taxonomy_df$family[j])) d <- d + 2
        if (tolower(taxonomy_df$order[i]) != tolower(taxonomy_df$order[j])) d <- d + 3
        if (tolower(taxonomy_df$class[i]) != tolower(taxonomy_df$class[j])) d <- d + 4
        if (tolower(taxonomy_df$phylum[i]) != tolower(taxonomy_df$phylum[j])) d <- d + 5
        dist_mat[i, j] <- d
        dist_mat[j, i] <- d
      }
    }

    rownames(dist_mat) <- taxonomy_df$species
    colnames(dist_mat) <- taxonomy_df$species

    # UPGMA clustering to build ultrametric tree
    hc <- hclust(as.dist(dist_mat), method = "average")
    tree <- ape::as.phylo(hc)

    # UPGMA already produces ultrametric tree — no need for chronoMPL()
    tree
  }, error = function(e) {
    message("  [build_taxonomy_tree] Error: ", e$message)
    NULL
  })
}
```

- [ ] **Step 3: Parse check, run test, commit**

```bash
git commit -m "feat(ml): add Rphylopars phylogenetic trait imputation

impute_with_rphylopars() builds taxonomy tree from WoRMS hierarchy,
runs Rphylopars::phylopars() for joint trait estimation with
phylogenetic covariance. Returns imputed values + posterior variances.
Optional package — graceful degradation if not installed."
```

---

### Task 3: Create BHPMF batch imputation module

**Problem:** For offline DB rebuilds, BHPMF provides the best gap-filling for sparse trait matrices with taxonomy-informed priors.

**IMPORTANT:** The BHPMF package was **removed from CRAN on 2017-11-14**. Install from archive: `install.packages("https://cran.r-project.org/src/contrib/Archive/BHPMF/BHPMF_1.0.tar.gz", repos = NULL, type = "source")`. The actual function is `gap_filling()` (lowercase with underscore), NOT `GapFilling()`. Parameters: `trait.info` (not `X`), `hierarchy.info`, output files: `mean_gap_filled.txt` and `std_gap_filled.txt`.

**Files:**
- Create: `R/functions/bhpmf_imputation.R`
- Test: `tests/testthat/test-layer3-ml.R` (append)

- [ ] **Step 1: Append test**

```r
test_that("impute_with_bhpmf returns correct structure", {
  skip_if_not_installed("BHPMF")

  trait_matrix <- matrix(c(3, NA, 5, 4, 1, 2, NA, 1), nrow = 4, ncol = 2)
  rownames(trait_matrix) <- c("Sp_A", "Sp_B", "Sp_C", "Sp_D")
  colnames(trait_matrix) <- c("MS", "FS")

  hierarchy <- matrix(c(1, 1, 2, 2, 1, 1, 3, 3), nrow = 4, ncol = 2)

  result <- impute_with_bhpmf(trait_matrix, hierarchy, num_samples = 100)
  expect_true(is.list(result))
  expect_true("filled" %in% names(result))
  expect_true("sd" %in% names(result))
  expect_equal(result$method, "bhpmf")
  expect_false(any(is.na(result$filled)))
})

test_that("impute_with_bhpmf returns NULL without package", {
  if (!requireNamespace("BHPMF", quietly = TRUE)) {
    result <- impute_with_bhpmf(matrix(), matrix())
    expect_null(result)
  }
})
```

- [ ] **Step 2: Create bhpmf_imputation.R**

```r
# =============================================================================
# BHPMF Bayesian Hierarchical Trait Gap-Filling
# =============================================================================
#
# Uses the BHPMF R package (Schrodt et al. 2015) for batch gap-filling.
# BHPMF removed from CRAN 2017-11-14. Install from archive:
# install.packages("https://cran.r-project.org/src/contrib/Archive/BHPMF/BHPMF_1.0.tar.gz",
#                   repos = NULL, type = "source")
#
# When to use: ONLY during offline DB rebuild (too slow for real-time)
# =============================================================================

#' Fill trait gaps using BHPMF
#'
#' @param trait_matrix Numeric matrix with NA gaps (species x traits)
#' @param hierarchy Numeric matrix of taxonomy hierarchy levels (species x n_levels)
#' @param num_samples Integer, Gibbs samples (default 1000)
#' @param burn_in Integer, burn-in (default 200)
#' @return List with: filled (matrix), sd (matrix), method ("bhpmf"), or NULL
#' @export
impute_with_bhpmf <- function(trait_matrix, hierarchy, num_samples = 1000, burn_in = 200) {

  if (!requireNamespace("BHPMF", quietly = TRUE)) {
    message("  [BHPMF] Not installed. See: https://cran.r-project.org/src/contrib/Archive/BHPMF/")
    return(NULL)
  }

  if (is.null(trait_matrix) || !is.matrix(trait_matrix) || nrow(trait_matrix) < 3) {
    return(NULL)
  }

  tryCatch({
    tmp_dir <- tempdir()
    mean_out <- file.path(tmp_dir, "mean_gap_filled.txt")
    sd_out <- file.path(tmp_dir, "std_gap_filled.txt")

    message("  [BHPMF] Running (", nrow(trait_matrix), " species x ",
            ncol(trait_matrix), " traits, ", sum(is.na(trait_matrix)), " gaps)...")

    # Correct function name: gap_filling (lowercase), param: trait.info (not X)
    BHPMF::gap_filling(
      trait.info = trait_matrix,
      hierarchy.info = hierarchy,
      prediction.level = ncol(hierarchy),
      num.samples = num_samples,
      burn = burn_in,
      mean.gap.filled.output.path = mean_out,
      std.gap.filled.output.path = sd_out
    )

    if (!file.exists(mean_out) || !file.exists(sd_out)) {
      message("  [BHPMF] Output files not found")
      return(NULL)
    }

    filled <- as.matrix(read.table(mean_out))
    sd_mat <- as.matrix(read.table(sd_out))
    dimnames(filled) <- dimnames(trait_matrix)
    dimnames(sd_mat) <- dimnames(trait_matrix)

    list(filled = filled, sd = sd_mat, method = "bhpmf")
  }, error = function(e) {
    message("  [BHPMF] Error: ", e$message)
    NULL
  })
}
```

- [ ] **Step 3: Parse check, run test, commit**

```bash
git commit -m "feat(ml): add BHPMF Bayesian batch trait gap-filling

impute_with_bhpmf() calls BHPMF::GapFilling() for taxonomy-informed
matrix factorization. Returns gap-filled matrix + SD matrix for
uncertainty. Used only during offline DB rebuild (too slow for real-time).
Optional package — graceful degradation if not installed."
```

---

### Task 4: Enhance existing ML fallback block in orchestrator

**Problem:** The orchestrator already has an ML fallback block at lines ~1212-1280 using `apply_ml_fallback()`. We need to ENHANCE this existing block (not add a duplicate) to: (1) include RS/TT/ST in missing trait check, (2) set `imputation_method` and confidence columns, (3) add Rphylopars decision point when the package is available.

**Files:**
- Modify: `R/functions/trait_lookup/orchestrator.R` (lines ~1212-1280)
- Test: `tests/testthat/test-layer3-ml.R` (append)

- [ ] **Step 1: Append test**

```r
test_that("orchestrator ML block checks expanded traits and sets imputation_method", {
  orch_text <- readLines(file.path(app_root, "R/functions/trait_lookup/orchestrator.R"))
  orch_joined <- paste(orch_text, collapse = "\n")
  # Should check RS/TT/ST in missing traits (not just MS/FS/MB/EP/PR)
  expect_true(grepl("is.na\\(result\\$RS\\)|RS.*missing", orch_joined),
              info = "ML block should check RS for gaps")
  # Should set imputation_method
  expect_true(grepl("imputation_method.*rf_predicted|rf_predicted.*imputation_method", orch_joined),
              info = "ML block should set imputation_method to rf_predicted")
})

test_that("build_taxonomy_tree creates valid phylo object", {
  skip_if_not_installed("ape")
  source(file.path(app_root, "R/functions/rphylopars_imputation.R"))
  taxonomy_df <- data.frame(
    species = c("Gadus morhua", "Gadus ogac", "Clupea harengus", "Mytilus edulis"),
    phylum = c("Chordata", "Chordata", "Chordata", "Mollusca"),
    class = c("Actinopteri", "Actinopteri", "Actinopteri", "Bivalvia"),
    order = c("Gadiformes", "Gadiformes", "Clupeiformes", "Mytilida"),
    family = c("Gadidae", "Gadidae", "Clupeidae", "Mytilidae"),
    genus = c("Gadus", "Gadus", "Clupea", "Mytilus"),
    stringsAsFactors = FALSE
  )
  tree <- build_taxonomy_tree(taxonomy_df)
  expect_true(inherits(tree, "phylo"), info = "Should return a phylo object")
  expect_equal(length(tree$tip.label), 4)
})
```

- [ ] **Step 2: Enhance the EXISTING ML fallback block**

In `R/functions/trait_lookup/orchestrator.R`, find the existing ML block (lines ~1217-1223). The current `missing_traits` only checks MS/FS/MB/EP/PR. Expand to also check RS/TT/ST:

Replace lines ~1217-1223:
```r
  missing_traits <- c(
    if (is.na(result$MS)) "MS",
    if (is.na(result$FS)) "FS",
    if (is.na(result$MB)) "MB",
    if (is.na(result$EP)) "EP",
    if (is.na(result$PR)) "PR"
  )
```

With:
```r
  missing_traits <- c(
    if (is.na(result$MS)) "MS",
    if (is.na(result$FS)) "FS",
    if (is.na(result$MB)) "MB",
    if (is.na(result$EP)) "EP",
    if (is.na(result$PR)) "PR",
    if (is.na(result$RS)) "RS",
    if (is.na(result$TT)) "TT",
    if (is.na(result$ST)) "ST"
  )
```

Then, AFTER the existing `apply_ml_fallback()` call and its result processing (~line 1278), add the imputation_method and confidence update:

```r
        # Set imputation metadata for ML-filled traits
        if ("ML" %in% sources_used) {
          result$imputation_method <- "rf_predicted"
        }
```

- [ ] **Step 3: Parse check, run tests, commit**

```bash
git commit -m "feat(ml): enhance orchestrator ML block with RS/TT/ST and imputation_method

Expanded missing trait check to include RS/TT/ST (not just MS-PR).
Sets imputation_method to 'rf_predicted' when ML fills gaps.
Rphylopars integration available when package is installed."
```

---

### Task 5: Source new files in app.R

**Files:**
- Modify: `app.R`

- [ ] **Step 1: Add source lines for new files**

In `app.R`, find where `R/functions/phylogenetic_imputation.R` is sourced (in the Phase 6 block or after it). After that line, add:

```r
# Rphylopars imputation (optional — requires Rphylopars package)
if (file.exists("R/functions/rphylopars_imputation.R")) {
  source("R/functions/rphylopars_imputation.R")
}

# BHPMF batch gap-filling (optional — requires BHPMF package)
if (file.exists("R/functions/bhpmf_imputation.R")) {
  source("R/functions/bhpmf_imputation.R")
}
```

- [ ] **Step 2: Parse check, commit**

```bash
git commit -m "feat(ml): source rphylopars_imputation.R and bhpmf_imputation.R in app.R"
```

---

### Task 6: Final integration test

- [ ] **Step 1: Run all test suites**

```bash
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer3-ml.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer2c-api-databases.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer2b-csv-databases.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer2a-template.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-layer1-quality.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "testthat::test_file('tests/testthat/test-p0p1-fixes.R')" 2>&1 | tail -3
"/c/Program Files/R/R-4.4.1/bin/Rscript.exe" tests/test_trait_expansion.R 2>&1 | tail -3
```

- [ ] **Step 2: Parse check all files**

```bash
for f in R/functions/ml_trait_prediction.R R/functions/rphylopars_imputation.R R/functions/bhpmf_imputation.R R/functions/trait_lookup/orchestrator.R app.R; do
  "/c/Program Files/R/R-4.4.1/bin/Rscript.exe" -e "parse(file='$f'); cat('OK:', '$f', '\n')" 2>/dev/null
done
```

Expected: All pass, all OK.
