# Install required packages for plugin system
cat("Installing required packages for plugin system...\n\n")

# Check and install shinyBS
if (!requireNamespace("shinyBS", quietly = TRUE)) {
  cat("Installing shinyBS...\n")
  install.packages("shinyBS", repos = "https://cloud.r-project.org")
} else {
  cat("✓ shinyBS already installed\n")
}

# Check and install shinyWidgets
if (!requireNamespace("shinyWidgets", quietly = TRUE)) {
  cat("Installing shinyWidgets...\n")
  install.packages("shinyWidgets", repos = "https://cloud.r-project.org")
} else {
  cat("✓ shinyWidgets already installed\n")
}

cat("\n✓ All required packages ready!\n")
cat("\nYou can now run the app with: shiny::runApp()\n")
