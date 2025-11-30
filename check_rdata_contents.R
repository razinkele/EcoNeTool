# Check what's actually in BalticFW.Rdata
cat("Checking contents of BalticFW.Rdata...\n\n")

# Load the file
load("BalticFW.Rdata")

# List all objects loaded
cat("Objects loaded from BalticFW.Rdata:\n")
objects_loaded <- ls()
print(objects_loaded)

cat("\n\nDetails of each object:\n")
cat("================================================\n")

for (obj_name in objects_loaded) {
  obj <- get(obj_name)
  cat("\nObject:", obj_name, "\n")
  cat("  Class:", class(obj), "\n")

  if (is.function(obj)) {
    cat("  Type: Function\n")
    cat("  Arguments:", paste(names(formals(obj)), collapse=", "), "\n")
  } else if (is.data.frame(obj)) {
    cat("  Type: Data frame\n")
    cat("  Dimensions:", nrow(obj), "rows x", ncol(obj), "columns\n")
    cat("  Columns:", paste(colnames(obj), collapse=", "), "\n")
  } else if (class(obj)[1] == "igraph") {
    cat("  Type: igraph network\n")
    cat("  Vertices:", igraph::vcount(obj), "\n")
    cat("  Edges:", igraph::ecount(obj), "\n")
  } else {
    cat("  Type:", typeof(obj), "\n")
    cat("  Length/Size:", length(obj), "\n")
  }
}

cat("\n================================================\n")
cat("\nChecking for fluxind function:\n")
if (exists("fluxind")) {
  cat("✓ fluxind EXISTS\n")
  cat("  Class:", class(fluxind), "\n")
} else {
  cat("✗ fluxind DOES NOT EXIST in BalticFW.Rdata\n")
  cat("\nThis means fluxind must come from the fluxweb package!\n")
}
