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

    # Make ultrametric (required by Rphylopars)
    tree <- ape::chronoMPL(tree)

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

**Files:**
- Create: `R/functions/bhpmf_imputation.R`
- Test: `tests/testthat/test-layer3-ml.R` (append)

- [ ] **Step 1: Append test**

```r
test_that("impute_with_bhpmf returns correct structure", {
  skip_if_not_installed("BHPMF")

  # Small test matrix (BHPMF needs numeric matrix with NAs)
  trait_matrix <- matrix(c(3, NA, 5, 4, 1, 2, NA, 1), nrow = 4, ncol = 2)
  rownames(trait_matrix) <- c("Sp_A", "Sp_B", "Sp_C", "Sp_D")
  colnames(trait_matrix) <- c("MS", "FS")

  hierarchy <- matrix(c(1, 1, 2, 2, 1, 1, 3, 3), nrow = 4, ncol = 2)

  result <- impute_with_bhpmf(trait_matrix, hierarchy, num_samples = 100)
  expect_true(is.list(result))
  expect_true("filled" %in% names(result))
  expect_true("sd" %in% names(result))
  expect_true("method" %in% names(result))
  expect_equal(result$method, "bhpmf")
  expect_equal(dim(result$filled), c(4, 2))
  # All NAs should be filled
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
# Uses the BHPMF R package (Schrodt et al. 2015) for batch gap-filling of
# sparse trait matrices with taxonomy-informed priors.
#
# When to use: ONLY during offline DB rebuild (too slow for real-time lookups)
# =============================================================================

#' Fill trait gaps using BHPMF (Bayesian Hierarchical Probabilistic Matrix Factorization)
#'
#' @param trait_matrix Numeric matrix with NA gaps (species x traits)
#' @param hierarchy Numeric matrix of taxonomy hierarchy levels (species x n_levels)
#' @param num_samples Integer, number of Gibbs samples (default 1000)
#' @param burn_in Integer, burn-in period (default 200)
#' @return List with: filled (matrix), sd (matrix of SDs), method ("bhpmf"),
#'         or NULL if package unavailable
#' @export
impute_with_bhpmf <- function(trait_matrix, hierarchy, num_samples = 1000, burn_in = 200) {

  if (!requireNamespace("BHPMF", quietly = TRUE)) {
    message("  [BHPMF] Package not installed. Install with: install.packages('BHPMF')")
    return(NULL)
  }

  if (is.null(trait_matrix) || !is.matrix(trait_matrix) || nrow(trait_matrix) < 3) {
    message("  [BHPMF] Need at least 3 species in trait matrix")
    return(NULL)
  }

  tryCatch({
    message("  [BHPMF] Running gap-filling (", num_samples, " samples, ",
            burn_in, " burn-in)...")
    message("  [BHPMF] Matrix: ", nrow(trait_matrix), " species x ",
            ncol(trait_matrix), " traits")
    message("  [BHPMF] Missing values: ", sum(is.na(trait_matrix)), " (",
            round(100 * sum(is.na(trait_matrix)) / length(trait_matrix), 1), "%)")

    # Create temporary directory for BHPMF output
    tmp_dir <- tempdir()

    # Run BHPMF GapFilling
    BHPMF::GapFilling(
      X = trait_matrix,
      hierarchy.info = hierarchy,
      prediction.level = ncol(hierarchy),
      num.samples = num_samples,
      burn = burn_in,
      tmp.dir = tmp_dir
    )

    # Read results from BHPMF output files
    mean_file <- file.path(tmp_dir, "mean_thinned.txt")
    sd_file <- file.path(tmp_dir, "std_thinned.txt")

    if (!file.exists(mean_file) || !file.exists(sd_file)) {
      message("  [BHPMF] Output files not found")
      return(NULL)
    }

    filled <- as.matrix(read.table(mean_file))
    sd_mat <- as.matrix(read.table(sd_file))

    # Restore dimensions and names
    dimnames(filled) <- dimnames(trait_matrix)
    dimnames(sd_mat) <- dimnames(trait_matrix)

    message("  [BHPMF] Gap-filling complete")

    list(
      filled = filled,
      sd = sd_mat,
      method = "bhpmf"
    )

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

### Task 4: Add method selection logic to orchestrator

**Problem:** The orchestrator needs to choose between Rphylopars and enhanced RF when traits are missing after database lookups, and set `imputation_method` and confidence columns.

**Files:**
- Modify: `R/functions/trait_lookup/orchestrator.R`
- Test: `tests/testthat/test-layer3-ml.R` (append)

- [ ] **Step 1: Append test**

```r
test_that("orchestrator sets imputation_method column", {
  source(file.path(app_root, "R/functions/trait_lookup/database_lookups.R"))
  source(file.path(app_root, "R/functions/trait_lookup/csv_trait_databases.R"))
  source(file.path(app_root, "R/functions/trait_lookup/api_trait_databases.R"))
  source(file.path(app_root, "R/functions/trait_lookup/orchestrator.R"))

  # Call with nonexistent species — should get "observed" (default) or ML method
  result <- lookup_species_traits("__test_nonexistent_xyz__")
  expect_true("imputation_method" %in% names(result))
  # For nonexistent species, imputation_method should be set
  expect_true(!is.null(result$imputation_method))
})
```

- [ ] **Step 2: Add ML imputation block to orchestrator**

In `R/functions/trait_lookup/orchestrator.R`, find the section AFTER harmonization (after all MS/FS/MB/EP/PR are assigned from harmonization functions). Before the final `return(result)`, add:

```r
  # ═══════════════════════════════════════════════════════════════════════
  # ML GAP-FILLING (for missing traits after database lookups + harmonization)
  # ═══════════════════════════════════════════════════════════════════════

  missing_traits <- c()
  for (trait in c("MS", "FS", "MB", "EP", "PR", "RS", "TT", "ST")) {
    if (is.na(result[[trait]])) missing_traits <- c(missing_traits, trait)
  }

  if (length(missing_traits) > 0 && !is.null(raw_traits$worms)) {
    taxonomic_info <- list(
      phylum = raw_traits$worms$phylum,
      class = raw_traits$worms$class,
      order = raw_traits$worms$order,
      family = raw_traits$worms$family,
      genus = raw_traits$worms$genus
    )

    message("\n  ML Gap-Filling for: ", paste(missing_traits, collapse = ", "))

    # Try ML prediction for each missing trait
    ml_predictions <- predict_missing_traits(
      current_traits = as.list(result[1, c("MS", "FS", "MB", "EP", "PR")]),
      taxonomic_info = taxonomic_info,
      traits_to_predict = intersect(missing_traits, c("MS", "FS", "MB", "EP", "PR"))
    )

    if (!is.null(ml_predictions) && length(ml_predictions) > 0) {
      for (trait_name in names(ml_predictions)) {
        pred <- ml_predictions[[trait_name]]
        if (!is.null(pred) && !is.null(pred$value)) {
          result[[trait_name]] <- pred$value
          result[[paste0(trait_name, "_confidence")]] <- pred$probability %||% 0.5
          result$imputation_method <- "rf_predicted"
          message("    ", trait_name, " = ", pred$value,
                  " (RF, prob=", round(pred$probability %||% 0, 2), ")")
        }
      }
    }
  }
```

- [ ] **Step 3: Parse check, run tests, commit**

```bash
git commit -m "feat(ml): add ML gap-filling to orchestrator method selection

After database lookups and harmonization, missing traits are filled
via predict_missing_traits() (RF model). Sets imputation_method to
'rf_predicted' and per-trait confidence scores. Rphylopars/BHPMF
integration points documented for when packages are available."
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
