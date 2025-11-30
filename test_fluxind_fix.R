# ==============================================================================
# Test Script for fluxind Environment Fix
# ==============================================================================
# This script tests that fluxind can now find trophiclevels() after the fix
#
# Test Date: 2025-11-27
# Issue: "could not find function 'trophiclevels'" in flux network plot
# Fix: Reassign fluxind environment to .GlobalEnv after loading
# ==============================================================================

cat("Testing fluxind environment fix...\n\n")

# Load required libraries
suppressMessages({
  library(igraph)
  library(fluxweb)
})

# Load configuration and data (same as app.R startup)
source("plotfw.R")

# Configuration constants
COLOR_SCHEME <- c("orange", "darkgrey", "blue", "green", "cyan")
TROPHIC_LEVEL_MAX_ITER <- 100
TROPHIC_LEVEL_CONVERGENCE <- 0.0001
FLUX_CONVERSION_FACTOR <- 86.4
DATA_FILE <- "BalticFW.Rdata"

# Define trophiclevels function (same as in app.R)
trophiclevels <- function(net) {
  if (!igraph::is_igraph(net)) {
    stop("Input 'net' must be an igraph object")
  }
  n <- vcount(net)
  if (n == 0) {
    stop("Network contains no vertices")
  }
  tl <- rep(1, n)
  adj <- as_adjacency_matrix(net, sparse = FALSE)
  converged <- FALSE
  for (iter in 1:TROPHIC_LEVEL_MAX_ITER) {
    tl_old <- tl
    for (i in 1:n) {
      prey_indices <- which(adj[i, ] > 0)
      if (length(prey_indices) > 0) {
        tl[i] <- 1 + mean(tl[prey_indices])
      } else {
        tl[i] <- 1
      }
    }
    if (max(abs(tl - tl_old)) < TROPHIC_LEVEL_CONVERGENCE) {
      converged <- TRUE
      break
    }
  }
  return(tl)
}

cat("1. Loading data from", DATA_FILE, "...\n")
suppressMessages({
  load(DATA_FILE)
  net <- igraph::upgrade_graph(net)
})
cat("   ✓ Data loaded:", vcount(net), "species,", ecount(net), "links\n\n")

cat("2. Checking if fluxind function exists...\n")
if (!exists("fluxind")) {
  stop("ERROR: fluxind function not found in data file")
}
cat("   ✓ fluxind function found\n\n")

cat("3. Testing fluxind BEFORE environment fix...\n")
cat("   Current environment of fluxind:\n")
print(environment(fluxind))
cat("\n")

# Try calling fluxind without the fix (should fail)
test_before <- tryCatch({
  netmatrix <- as_adjacency_matrix(net, sparse=F)
  biomass <- info$meanB
  fluxes <- fluxing(netmatrix, biomass, info$losses, info$efficiencies, ef.level="prey")
  fluxes <- fluxes * FLUX_CONVERSION_FACTOR
  result <- fluxind(fluxes)
  "SUCCESS"
}, error = function(e) {
  paste("ERROR:", e$message)
})

cat("   Result:", test_before, "\n\n")

cat("4. Applying environment fix...\n")
environment(fluxind) <- .GlobalEnv
cat("   ✓ fluxind environment reassigned to .GlobalEnv\n\n")

cat("5. Testing fluxind AFTER environment fix...\n")
cat("   New environment of fluxind:\n")
print(environment(fluxind))
cat("\n")

# Try calling fluxind with the fix (should succeed)
test_after <- tryCatch({
  netmatrix <- as_adjacency_matrix(net, sparse=F)
  biomass <- info$meanB
  fluxes <- fluxing(netmatrix, biomass, info$losses, info$efficiencies, ef.level="prey")
  fluxes <- fluxes * FLUX_CONVERSION_FACTOR
  result <- fluxind(fluxes)
  cat("   ✓ fluxind executed successfully!\n")
  cat("   Flux indicators calculated:\n")
  print(result)
  "SUCCESS"
}, error = function(e) {
  paste("ERROR:", e$message)
})

cat("\n")
cat("6. Final Result:", test_after, "\n\n")

if (test_after == "SUCCESS") {
  cat("================================================================\n")
  cat("  ✓✓✓ TEST PASSED ✓✓✓\n")
  cat("  The fluxind environment fix works correctly!\n")
  cat("  The flux-weighted network plot should now work in the app.\n")
  cat("================================================================\n")
} else {
  cat("================================================================\n")
  cat("  ✗✗✗ TEST FAILED ✗✗✗\n")
  cat("  Issue:", test_after, "\n")
  cat("================================================================\n")
}
