# Test UI construction only
cat("Testing UI construction...\n\n")

# Load everything up to the UI
library(shiny)
library(bs4Dash)
library(igraph)
library(fluxweb)
library(visNetwork)

source("plotfw.R")

# Configuration
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

# Load data
load(DATA_FILE)

cat("Data loaded. Now testing UI construction...\n")

# Try to create a simple bs4Dash page
tryCatch({
  test_ui <- dashboardPage(
    header = dashboardHeader(title = "Test"),
    sidebar = dashboardSidebar(),
    body = dashboardBody(
      h1("Test Dashboard")
    )
  )
  cat("Simple bs4Dash UI: OK\n")
}, error = function(e) {
  cat("ERROR creating simple UI:", e$message, "\n")
})

cat("\nTest completed\n")
