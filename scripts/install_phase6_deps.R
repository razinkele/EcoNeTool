# Install Phase 6 dependencies

options(repos = c(CRAN = "https://cloud.r-project.org/"))

packages <- c("RSQLite", "DBI", "R6", "future", "future.apply", "digest")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing ", pkg, "...")
    install.packages(pkg)
  } else {
    message(pkg, " already installed")
  }
}

message("\n✓ All Phase 6 dependencies installed")
