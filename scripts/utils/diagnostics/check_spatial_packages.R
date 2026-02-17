# Check spatial packages
installed_packages <- installed.packages()[, 'Package']
spatial_pkgs <- c('sf', 'dggridR', 'leaflet', 'raster', 'sp', 'rgdal', 'spdep')

available <- spatial_pkgs[spatial_pkgs %in% installed_packages]
missing <- setdiff(spatial_pkgs, installed_packages)

cat("SPATIAL PACKAGES STATUS\n")
cat(paste(rep("=", 50), collapse=""), "\n\n")

cat("AVAILABLE:\n")
if(length(available) > 0) {
  for(pkg in available) cat("  ✓", pkg, "\n")
} else {
  cat("  (none)\n")
}

cat("\nMISSING:\n")
if(length(missing) > 0) {
  for(pkg in missing) cat("  ✗", pkg, "\n")
} else {
  cat("  (none)\n")
}

cat("\n")
