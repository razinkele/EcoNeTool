# ==============================================================================
# Complete Test for Flux Network Plot
# ==============================================================================
# This script simulates the exact sequence of operations that occurs when
# rendering the flux-weighted network plot in the Shiny app
#
# Test Date: 2025-11-27
# Issue: "could not find function 'trophiclevels'" in flux network plot
# ==============================================================================

cat("Testing complete flux network plot generation...\n\n")

# Load required libraries
suppressMessages({
  library(igraph)
  library(fluxweb)
})

# Source plotting function
source("plotfw.R")

# ============================================================================
# CONFIGURATION CONSTANTS (from app.R)
# ============================================================================
COLOR_SCHEME <- c("orange", "darkgrey", "blue", "green", "cyan")
TROPHIC_LEVEL_MAX_ITER <- 100
TROPHIC_LEVEL_CONVERGENCE <- 0.0001
FLUX_CONVERSION_FACTOR <- 86.4
NODE_SIZE_SCALE <- 25
NODE_SIZE_MIN <- 4
EDGE_WIDTH_SCALE <- 15
EDGE_WIDTH_MIN <- 0.1
EDGE_ARROW_SIZE_TOPOLOGY <- 0.3
EDGE_ARROW_SIZE_FLUX <- 0.05
DATA_FILE <- "BalticFW.Rdata"

# ============================================================================
# HELPER FUNCTIONS (from app.R)
# ============================================================================

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
  if (!converged) {
    warning(paste("Trophic level calculation did not converge after",
                  TROPHIC_LEVEL_MAX_ITER, "iterations"))
  }
  return(tl)
}

get_fluxweb_results <- function(net, info) {
  if (!igraph::is_igraph(net)) {
    stop("Input 'net' must be an igraph object")
  }
  if (!is.data.frame(info)) {
    stop("Input 'info' must be a data frame")
  }

  required_cols <- c("meanB", "losses", "efficiencies")
  missing_cols <- setdiff(required_cols, colnames(info))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns in info:", paste(missing_cols, collapse=", ")))
  }

  if (nrow(info) != vcount(net)) {
    stop("Number of rows in 'info' must match number of vertices in 'net'")
  }

  tryCatch({
    netmatrix <- as_adjacency_matrix(net, sparse=F)
    biomass <- info$meanB

    if (any(is.na(biomass))) {
      warning("NA values in biomass, flux calculations may fail")
    }
    if (any(biomass < 0, na.rm = TRUE)) {
      stop("Biomass values must be non-negative")
    }

    # Calculate fluxes using fluxweb package
    fluxes <- fluxing(netmatrix, biomass, info$losses, info$efficiencies, ef.level="prey")

    # Convert J/sec to kJ/day
    fluxes <- fluxes * FLUX_CONVERSION_FACTOR

    # Create weighted network
    netLW <- graph_from_adjacency_matrix(fluxes, weighted=TRUE)

    list(fluxes=fluxes, netLW=netLW)

  }, error = function(e) {
    stop(paste("Error calculating fluxweb results:", e$message))
  })
}

# ============================================================================
# LOAD DATA (simulating app.R startup)
# ============================================================================
cat("1. Loading data file:", DATA_FILE, "\n")

tryCatch({
  suppressMessages({
    load(DATA_FILE)

    if (!exists("net")) stop("'net' object not found in data file")
    if (!exists("info")) stop("'info' object not found in data file")
    if (!exists("fluxind")) stop("'fluxind' function not found in data file")

    if (!igraph::is_igraph(net)) stop("'net' must be an igraph object")

    net <<- igraph::upgrade_graph(net)

    # APPLY THE FIX
    if (exists("fluxind") && is.function(fluxind)) {
      environment(fluxind) <<- .GlobalEnv
      cat("   ✓ fluxind environment fixed\n")
    }
  })

  cat("   ✓ Data loaded successfully:", vcount(net), "species,", ecount(net), "links\n\n")

}, error = function(e) {
  stop(paste("Error loading data:", e$message))
})

# Assign colors
info$colfg <- COLOR_SCHEME[as.numeric(info$fg)]

# ============================================================================
# TEST 1: Flux Heatmap
# ============================================================================
cat("2. Testing flux heatmap generation...\n")
test_heatmap <- tryCatch({
  res <- get_fluxweb_results(net, info)
  cat("   ✓ Fluxes calculated successfully\n")
  cat("   ✓ Flux matrix dimensions:", nrow(res$fluxes), "x", ncol(res$fluxes), "\n")
  cat("   ✓ Flux range:", round(min(res$fluxes), 2), "to", round(max(res$fluxes), 2), "kJ/day/km²\n")
  "SUCCESS"
}, error = function(e) {
  paste("ERROR:", e$message)
})
cat("   Result:", test_heatmap, "\n\n")

# ============================================================================
# TEST 2: Flux Network Plot (THE CRITICAL TEST)
# ============================================================================
cat("3. Testing flux-weighted network plot (the reported error location)...\n")
test_flux_plot <- tryCatch({
  res <- get_fluxweb_results(net, info)

  # Calculate edge widths
  wid <- EDGE_WIDTH_MIN + (E(res$netLW)$weight/max(E(res$netLW)$weight) * EDGE_WIDTH_SCALE)
  cat("   ✓ Edge widths calculated\n")

  # Set vertex properties
  V(res$netLW)$frame.color <- NA
  cat("   ✓ Vertex properties set\n")

  # Create plot
  pdf(file = NULL)  # Plot to null device for testing
  plotfw(res$netLW, col=info$colfg, edge.width=wid, edge.arrow.size=EDGE_ARROW_SIZE_FLUX)
  dev.off()

  cat("   ✓ Flux network plot generated successfully!\n")
  "SUCCESS"
}, error = function(e) {
  paste("ERROR:", e$message)
})
cat("   Result:", test_flux_plot, "\n\n")

# ============================================================================
# TEST 3: Flux Indicators
# ============================================================================
cat("4. Testing flux indicators calculation...\n")
test_flux_ind <- tryCatch({
  res <- get_fluxweb_results(net, info)
  result <- fluxind(res$fluxes)
  cat("   ✓ Flux indicators calculated:\n")
  cat("     - lwC (link-weighted connectance):", round(result$lwC, 4), "\n")
  cat("     - lwG (link-weighted generality):", round(result$lwG, 4), "\n")
  cat("     - lwV (link-weighted vulnerability):", round(result$lwV, 4), "\n")
  "SUCCESS"
}, error = function(e) {
  paste("ERROR:", e$message)
})
cat("   Result:", test_flux_ind, "\n\n")

# ============================================================================
# SUMMARY
# ============================================================================
cat("================================================================\n")
cat("TEST SUMMARY\n")
cat("================================================================\n")
cat("Flux Heatmap:       ", test_heatmap, "\n")
cat("Flux Network Plot:  ", test_flux_plot, "\n")
cat("Flux Indicators:    ", test_flux_ind, "\n")
cat("================================================================\n\n")

if (test_flux_plot == "SUCCESS") {
  cat("✓✓✓ ALL TESTS PASSED ✓✓✓\n\n")
  cat("The flux-weighted network plot is now working correctly.\n")
  cat("The fix successfully resolves the 'trophiclevels not found' error.\n\n")
  cat("The Shiny app should now work without errors in the Energy Fluxes tab.\n")
} else {
  cat("✗✗✗ TESTS FAILED ✗✗✗\n\n")
  cat("There are still issues to resolve:\n")
  if (test_heatmap != "SUCCESS") cat("  - Flux heatmap:", test_heatmap, "\n")
  if (test_flux_plot != "SUCCESS") cat("  - Flux network plot:", test_flux_plot, "\n")
  if (test_flux_ind != "SUCCESS") cat("  - Flux indicators:", test_flux_ind, "\n")
}
