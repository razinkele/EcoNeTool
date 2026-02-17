#!/usr/bin/env Rscript
# ============================================================================
# EcoNeTool - R Package Dependencies Installation Script
# ============================================================================
#
# This script installs all required R packages for the EcoNeTool application
#
# Usage:
#   Rscript install_dependencies.R
#
# ============================================================================

cat("================================================================================\n")
cat("EcoNeTool - Installing R Package Dependencies\n")
cat("================================================================================\n\n")

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Function to install package if not already installed
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("Installing %s...\n", pkg))
    install.packages(pkg, dependencies = TRUE)
    if (requireNamespace(pkg, quietly = TRUE)) {
      cat(sprintf("✓ %s installed successfully\n", pkg))
      return(TRUE)
    } else {
      cat(sprintf("✗ Failed to install %s\n", pkg))
      return(FALSE)
    }
  } else {
    cat(sprintf("✓ %s already installed\n", pkg))
    return(TRUE)
  }
}

# Core Shiny packages
cat("\n[1/6] Installing Core Shiny Packages...\n")
cat("----------------------------------------\n")
shiny_packages <- c(
  "shiny",
  "bs4Dash",
  "shinyjs",
  "shinyWidgets",
  "shinyBS"
)

success_shiny <- sapply(shiny_packages, install_if_missing)

# Data manipulation packages
cat("\n[2/6] Installing Data Manipulation Packages...\n")
cat("-----------------------------------------------\n")
data_packages <- c(
  "dplyr",
  "tidyr",
  "readr",
  "tibble",
  "stringr"
)

success_data <- sapply(data_packages, install_if_missing)

# Network analysis packages
cat("\n[3/6] Installing Network Analysis Packages...\n")
cat("----------------------------------------------\n")
network_packages <- c(
  "igraph",
  "visNetwork",
  "fluxweb"
)

success_network <- sapply(network_packages, install_if_missing)

# Visualization packages
cat("\n[4/6] Installing Visualization Packages...\n")
cat("-------------------------------------------\n")
viz_packages <- c(
  "ggplot2",
  "plotly",
  "DT"
)

success_viz <- sapply(viz_packages, install_if_missing)

# Utility packages
cat("\n[5/6] Installing Utility Packages...\n")
cat("-------------------------------------\n")
util_packages <- c(
  "jsonlite",
  "openxlsx",
  "readxl",
  "MASS"
)

success_util <- sapply(util_packages, install_if_missing)

# Spatial analysis packages (Phase 1)
cat("\n[6/7] Installing Spatial Analysis Packages (Phase 1)...\n")
cat("--------------------------------------------------------\n")
spatial_packages <- c(
  "sf",
  "leaflet"
)

success_spatial <- sapply(spatial_packages, install_if_missing)

# EcoBase connection packages
cat("\n[7/7] Installing EcoBase Connection Packages...\n")
cat("------------------------------------------------\n")
ecobase_packages <- c(
  "RCurl",
  "XML",
  "plyr"
)

success_ecobase <- sapply(ecobase_packages, install_if_missing)

# Summary
cat("\n================================================================================\n")
cat("Installation Summary\n")
cat("================================================================================\n\n")

all_packages <- c(shiny_packages, data_packages, network_packages, viz_packages, util_packages, spatial_packages, ecobase_packages)
all_success <- c(success_shiny, success_data, success_network, success_viz, success_util, success_spatial, success_ecobase)

total <- length(all_packages)
installed <- sum(all_success)
failed <- total - installed

cat(sprintf("Total packages: %d\n", total))
cat(sprintf("✓ Successfully installed/verified: %d\n", installed))
if (failed > 0) {
  cat(sprintf("✗ Failed: %d\n", failed))
  failed_packages <- all_packages[!all_success]
  cat(sprintf("\nFailed packages: %s\n", paste(failed_packages, collapse = ", ")))
}

cat("\n")

if (failed > 0) {
  cat("❌ INSTALLATION INCOMPLETE\n")
  cat("   Some packages failed to install. Please install them manually.\n\n")
  quit(status = 1)
} else {
  cat("✅ ALL PACKAGES INSTALLED SUCCESSFULLY\n")
  cat("   Ready to deploy EcoNeTool!\n\n")
  quit(status = 0)
}
