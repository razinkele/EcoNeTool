# Tests for Layer 3: ML Gap-Filling
library(testthat)

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
source(file.path(app_root, "R/config.R"))
source(file.path(app_root, "R/functions/validation_utils.R"))
source(file.path(app_root, "R/functions/functional_group_utils.R"))
source(file.path(app_root, "R/functions/ml_trait_prediction.R"))

test_that("compute_phylo_eigenvectors returns correct dimensions", {
  skip_if_not_installed("ape")
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
  expect_equal(nrow(eigenvecs), 5)
  expect_equal(ncol(eigenvecs), 3)
  gadus_dist <- sqrt(sum((eigenvecs[1, ] - eigenvecs[2, ])^2))
  cross_dist <- sqrt(sum((eigenvecs[1, ] - eigenvecs[5, ])^2))
  expect_true(gadus_dist < cross_dist,
              info = "Same-genus species should be closer in eigenvector space")
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
  expect_true(inherits(tree, "phylo"))
  expect_equal(length(tree$tip.label), 4)
})

test_that("impute_with_rphylopars returns NULL without package", {
  if (!requireNamespace("Rphylopars", quietly = TRUE)) {
    source(file.path(app_root, "R/functions/rphylopars_imputation.R"))
    result <- impute_with_rphylopars(data.frame(), data.frame())
    expect_null(result)
  }
})

test_that("impute_with_bhpmf returns NULL without package", {
  source(file.path(app_root, "R/functions/bhpmf_imputation.R"))
  if (!requireNamespace("BHPMF", quietly = TRUE)) {
    result <- impute_with_bhpmf(matrix(), matrix())
    expect_null(result)
  }
})

test_that("orchestrator ML block checks expanded traits and sets imputation_method", {
  orch_text <- readLines(file.path(app_root, "R/functions/trait_lookup/orchestrator.R"))
  orch_joined <- paste(orch_text, collapse = "\n")
  expect_true(grepl("is.na\\(result\\$RS\\)", orch_joined),
              info = "ML block should check RS for gaps")
  expect_true(grepl("is.na\\(result\\$TT\\)", orch_joined),
              info = "ML block should check TT for gaps")
  expect_true(grepl("is.na\\(result\\$ST\\)", orch_joined),
              info = "ML block should check ST for gaps")
  expect_true(grepl("rf_predicted", orch_joined),
              info = "ML block should set imputation_method to rf_predicted")
})
