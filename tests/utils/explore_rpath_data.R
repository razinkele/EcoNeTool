# Explore Rpath package data and structure

library(Rpath)

cat("=== Rpath Package Information ===\n\n")

# List all objects
cat("1. All exported objects:\n")
exports <- ls("package:Rpath")
print(exports)

cat("\n2. Looking for data objects:\n")
data_objects <- c()
for (obj in exports) {
  if (!grepl("^[a-z]", obj)) {  # Capital letter start often indicates data
    tryCatch({
      val <- get(obj, envir = asNamespace("Rpath"))
      if (is.data.frame(val) || is.list(val)) {
        cat("  -", obj, ":", class(val), "\n")
        data_objects <- c(data_objects, obj)
      }
    }, error = function(e) {})
  }
}

cat("\n3. Checking for .params objects:\n")
param_objects <- exports[grepl("params", exports, ignore.case = TRUE)]
print(param_objects)

cat("\n4. Trying to load known example data:\n")
tryCatch({
  data("REco.params", package = "Rpath")
  cat("  ✓ REco.params loaded\n")
  cat("    Structure:\n")
  str(REco.params, max.level = 1)
}, error = function(e) {
  cat("  ✗ REco.params not found\n")
})

tryCatch({
  data("Ecosense.EBS", package = "Rpath")
  cat("\n  ✓ Ecosense.EBS loaded\n")
}, error = function(e) {
  cat("\n  ✗ Ecosense.EBS not found\n")
})

cat("\n5. Checking package directory:\n")
pkg_path <- system.file(package = "Rpath")
cat("Package path:", pkg_path, "\n")

# List data directory
data_dir <- file.path(pkg_path, "data")
if (dir.exists(data_dir)) {
  cat("Data directory contents:\n")
  print(list.files(data_dir))
} else {
  cat("No data directory found\n")
}

# List extdata directory
extdata_dir <- file.path(pkg_path, "extdata")
if (dir.exists(extdata_dir)) {
  cat("\nExtdata directory contents:\n")
  print(list.files(extdata_dir, recursive = TRUE))
} else {
  cat("\nNo extdata directory found\n")
}

# Check for tests
test_dir <- file.path(pkg_path, "tests")
if (dir.exists(test_dir)) {
  cat("\nTests directory contents:\n")
  print(list.files(test_dir, recursive = TRUE))
} else {
  cat("\nNo tests directory found\n")
}
