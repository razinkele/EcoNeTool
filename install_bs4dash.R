# Install bs4Dash if needed
if (!requireNamespace("bs4Dash", quietly = TRUE)) {
  cat("Installing bs4Dash...\n")
  install.packages("bs4Dash", repos="https://cloud.r-project.org")
  cat("bs4Dash installed successfully!\n")
} else {
  cat("bs4Dash already installed\n")
  library(bs4Dash)
  cat("Version:", as.character(packageVersion("bs4Dash")), "\n")
}
