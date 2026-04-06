# =============================================================================
# BHPMF Bayesian Hierarchical Probabilistic Matrix Factorization
# =============================================================================
#
# Provides trait gap-filling via the BHPMF package (Fazayeli et al. 2014).
# NOTE: BHPMF was removed from CRAN in 2017. Install from Archive:
#   https://cran.r-project.org/src/contrib/Archive/BHPMF/
#
# Function:
#   impute_with_bhpmf() - runs BHPMF gap-filling on a trait matrix
#
# Date: 2026-04-04
# Version: 1.0
#
# =============================================================================

#' Impute Missing Traits Using BHPMF
#'
#' Runs Bayesian Hierarchical Probabilistic Matrix Factorization gap-filling
#' on a trait matrix with missing values.
#'
#' BHPMF was removed from CRAN in 2017. Install from Archive:
#' https://cran.r-project.org/src/contrib/Archive/BHPMF/
#'
#' @param trait_matrix Numeric matrix with species as rows and traits as
#'   columns; NAs mark values to be imputed
#' @param hierarchy Matrix with hierarchy information (e.g. genus, family,
#'   order columns) passed to BHPMF::gap_filling() as hierarchy.info
#' @param num_samples Integer, number of MCMC samples (default 1000)
#' @param burn_in Integer, number of burn-in samples (default 200)
#' @return A list with elements: filled (matrix), sd (matrix),
#'   method ("bhpmf"), or NULL on failure / missing package
#' @export
impute_with_bhpmf <- function(trait_matrix, hierarchy, num_samples = 1000, burn_in = 200) {
  if (!requireNamespace("BHPMF", quietly = TRUE)) {
    message("  [BHPMF] Not installed. See: https://cran.r-project.org/src/contrib/Archive/BHPMF/")
    return(NULL)
  }
  if (is.null(trait_matrix) || !is.matrix(trait_matrix) || nrow(trait_matrix) < 3) return(NULL)

  tryCatch({
    tmp_dir <- tempdir()
    mean_out <- file.path(tmp_dir, "mean_gap_filled.txt")
    sd_out <- file.path(tmp_dir, "std_gap_filled.txt")

    BHPMF::gap_filling(
      trait.info = trait_matrix,
      hierarchy.info = hierarchy,
      prediction.level = ncol(hierarchy),
      num.samples = num_samples,
      burn = burn_in,
      mean.gap.filled.output.path = mean_out,
      std.gap.filled.output.path = sd_out
    )

    if (!file.exists(mean_out) || !file.exists(sd_out)) return(NULL)

    filled <- as.matrix(read.table(mean_out))
    sd_mat <- as.matrix(read.table(sd_out))
    dimnames(filled) <- dimnames(trait_matrix)
    dimnames(sd_mat) <- dimnames(trait_matrix)

    list(filled = filled, sd = sd_mat, method = "bhpmf")
  }, error = function(e) { message("  [BHPMF] Error: ", e$message); NULL })
}
