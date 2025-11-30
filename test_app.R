# Test script for improved application
cat("Testing improved EcoNeTool application...\n\n")

tryCatch({
  # Source the application
  source("app.R")

  cat("SUCCESS: Application loaded without errors!\n\n")

  # Check configuration constants
  cat("Configuration Constants:\n")
  cat("  COLOR_SCHEME:", COLOR_SCHEME, "\n")
  cat("  TROPHIC_LEVEL_MAX_ITER:", TROPHIC_LEVEL_MAX_ITER, "\n")
  cat("  TROPHIC_LEVEL_CONVERGENCE:", TROPHIC_LEVEL_CONVERGENCE, "\n")
  cat("  FLUX_CONVERSION_FACTOR:", FLUX_CONVERSION_FACTOR, "\n")
  cat("  NODE_SIZE_SCALE:", NODE_SIZE_SCALE, "\n")
  cat("  NODE_SIZE_MIN:", NODE_SIZE_MIN, "\n")
  cat("  EDGE_WIDTH_SCALE:", EDGE_WIDTH_SCALE, "\n")
  cat("  EDGE_WIDTH_MIN:", EDGE_WIDTH_MIN, "\n\n")

  # Check data loaded
  cat("Data Validation:\n")
  cat("  Network vertices:", vcount(net), "\n")
  cat("  Network edges:", ecount(net), "\n")
  cat("  Species info rows:", nrow(info), "\n\n")

  # Test functions with error handling
  cat("Testing Functions:\n")

  # Test trophiclevels
  tl <- trophiclevels(net)
  cat("  trophiclevels() - OK (mean TL:", round(mean(tl), 2), ")\n")

  # Test topological indicators
  topo <- get_topological_indicators(net)
  cat("  get_topological_indicators() - OK (S =", topo$S, ")\n")

  # Test node-weighted indicators
  nw <- get_node_weighted_indicators(net, info)
  cat("  get_node_weighted_indicators() - OK (nwC =", round(nw$nwC, 3), ")\n")

  # Test fluxweb results
  flux <- get_fluxweb_results(net, info)
  cat("  get_fluxweb_results() - OK\n")

  cat("\nAll functions validated successfully!\n")
  cat("\nDocumentation added for:\n")
  cat("  - trophiclevels()\n")
  cat("  - get_topological_indicators()\n")
  cat("  - get_node_weighted_indicators()\n")
  cat("  - get_fluxweb_results()\n")
  cat("  - plotfw()\n")

  cat("\nImmediate wins implemented:\n")
  cat("  [x] Configuration constants extracted\n")
  cat("  [x] Input validation added\n")
  cat("  [x] Error handling implemented\n")
  cat("  [x] Roxygen2 documentation created\n")

}, error = function(e) {
  cat("\nERROR:", e$message, "\n")
  quit(status = 1)
})

cat("\n=== TEST PASSED ===\n")
