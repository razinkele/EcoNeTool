# Debug test for bs4Dash
cat("Testing bs4Dash - Debug Mode\n\n")

# Step 1: Load libraries
cat("Step 1: Loading libraries...\n")
library(shiny)
library(bs4Dash)
library(igraph)
library(fluxweb)
library(visNetwork)
cat("  Libraries loaded OK\n\n")

# Step 2: Load constants and configuration
cat("Step 2: Loading configuration...\n")
source("plotfw.R")

COLOR_SCHEME <- c("orange", "darkgrey", "blue", "green", "cyan")
TROPHIC_LEVEL_MAX_ITER <- 100
TROPHIC_LEVEL_CONVERGENCE <- 0.0001
FLUX_CONVERSION_FACTOR <- 86.4
FLUX_LOG_EPSILON <- 0.00001
NODE_SIZE_SCALE <- 25
NODE_SIZE_MIN <- 4
EDGE_WIDTH_SCALE <- 15
EDGE_WIDTH_MIN <- 0.1
EDGE_ARROW_SIZE_TOPOLOGY <- 0.3
EDGE_ARROW_SIZE_FLUX <- 0.05
DATA_FILE <- "BalticFW.Rdata"
cat("  Configuration loaded OK\n\n")

# Step 3: Load data
cat("Step 3: Loading data...\n")
load(DATA_FILE)
cat("  Data file loaded\n")
cat("  Network vertices:", vcount(net), "\n")
cat("  Network edges:", ecount(net), "\n")
cat("  Info dimensions:", nrow(info), "x", ncol(info), "\n\n")

# Step 4: Check data structure
cat("Step 4: Checking data structure...\n")
cat("  class(info):", class(info), "\n")
cat("  names(info):", paste(names(info), collapse=", "), "\n")
cat("  class(info$fg):", class(info$fg), "\n\n")

# Step 5: Try to add color column
cat("Step 5: Adding color column...\n")
tryCatch({
  info$colfg <- COLOR_SCHEME[as.numeric(info$fg)]
  cat("  Color column added successfully\n")
  cat("  First few colors:", head(info$colfg), "\n\n")
}, error = function(e) {
  cat("  ERROR adding color column:", e$message, "\n\n")
})

cat("=== Debug test completed ===\n")
