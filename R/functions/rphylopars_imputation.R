# =============================================================================
# Rphylopars Phylogenetic Trait Imputation
# =============================================================================
#
# Provides phylogenetic imputation of missing trait values using the
# Rphylopars package (Goolsby et al. 2017).
#
# Functions:
#   build_taxonomy_tree()     - builds ultrametric phylo from taxonomy
#   impute_with_rphylopars()  - runs phylogenetic imputation
#
# Date: 2026-04-04
# Version: 1.0
#
# =============================================================================

#' Build Ultrametric Phylogenetic Tree from Taxonomy
#'
#' Constructs a taxonomic distance matrix from the WoRMS hierarchy and builds
#' an ultrametric phylogenetic tree via UPGMA (hclust average linkage) then
#' converts to a phylo object using ape::as.phylo().
#'
#' @param taxonomy_df Data frame with columns: species, phylum, class, order,
#'   family, genus
#' @return An object of class "phylo", or NULL if construction fails
#' @export
build_taxonomy_tree <- function(taxonomy_df) {
  if (!requireNamespace("ape", quietly = TRUE)) {
    warning("Package 'ape' required for build_taxonomy_tree")
    return(NULL)
  }
  if (is.null(taxonomy_df) || nrow(taxonomy_df) < 3) return(NULL)

  tryCatch({
    n_species <- nrow(taxonomy_df)
    dist_mat <- matrix(0, nrow = n_species, ncol = n_species)
    rownames(dist_mat) <- taxonomy_df$species
    colnames(dist_mat) <- taxonomy_df$species

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

    hc <- hclust(as.dist(dist_mat), method = "average")
    tree <- ape::as.phylo(hc)
    tree
  }, error = function(e) {
    message("  [rphylopars] build_taxonomy_tree error: ", e$message)
    NULL
  })
}

#' Impute Missing Traits Using Rphylopars
#'
#' Uses phylogenetic Brownian motion (via Rphylopars::phylopars()) to impute
#' missing trait values. Requires the Rphylopars package.
#'
#' @param trait_matrix Data frame or matrix with species as rows and traits as
#'   columns; NAs mark values to be imputed. Row names should be species names.
#' @param taxonomy_df Data frame with columns: species, phylum, class, order,
#'   family, genus
#' @return A list with elements: imputed (matrix), variances (matrix),
#'   method ("rphylopars"), or NULL on failure / missing package
#' @export
impute_with_rphylopars <- function(trait_matrix, taxonomy_df) {
  if (!requireNamespace("Rphylopars", quietly = TRUE)) {
    message("  [Rphylopars] Not installed. Install with: install.packages('Rphylopars')")
    return(NULL)
  }
  if (is.null(trait_matrix) || is.null(taxonomy_df)) return(NULL)
  if (!is.data.frame(trait_matrix) && !is.matrix(trait_matrix)) return(NULL)
  if (nrow(taxonomy_df) < 3) return(NULL)

  tryCatch({
    tree <- build_taxonomy_tree(taxonomy_df)
    if (is.null(tree)) return(NULL)

    # Rphylopars expects a data frame with a 'species' column
    if (is.matrix(trait_matrix)) {
      trait_df <- as.data.frame(trait_matrix)
      trait_df$species <- rownames(trait_matrix)
    } else {
      trait_df <- trait_matrix
      if (!"species" %in% colnames(trait_df)) {
        trait_df$species <- rownames(trait_df)
      }
    }

    # Move species column to front
    trait_df <- trait_df[, c("species", setdiff(colnames(trait_df), "species"))]

    result <- Rphylopars::phylopars(trait_data = trait_df, tree = tree)

    imputed_mat <- as.matrix(result$anc_recon[seq_len(nrow(trait_df)), , drop = FALSE])
    var_mat <- if (!is.null(result$anc_var)) {
      as.matrix(result$anc_var[seq_len(nrow(trait_df)), , drop = FALSE])
    } else {
      matrix(NA, nrow = nrow(imputed_mat), ncol = ncol(imputed_mat),
             dimnames = dimnames(imputed_mat))
    }

    list(imputed = imputed_mat, variances = var_mat, method = "rphylopars")
  }, error = function(e) {
    message("  [Rphylopars] impute_with_rphylopars error: ", e$message)
    NULL
  })
}
