# Check if fluxind is available from fluxweb package
library(fluxweb)

cat("Checking fluxweb package for fluxind function...\n\n")

# Check if fluxind is exported from fluxweb package
if (exists("fluxind", where = "package:fluxweb")) {
  cat("✓ fluxind IS exported from fluxweb package\n")
  cat("  Function signature:\n")
  print(args(fluxind))
} else {
  cat("✗ fluxind is NOT exported from fluxweb package\n")
  cat("  We need to implement it ourselves\n\n")

  cat("Available functions in fluxweb package:\n")
  fluxweb_functions <- ls("package:fluxweb")
  print(fluxweb_functions)
}

# Also load BalticFW.Rdata to check the fluxind function there
cat("\n\nChecking fluxind from BalticFW.Rdata:\n")
load("BalticFW.Rdata")
if (exists("fluxind")) {
  cat("✓ fluxind found in BalticFW.Rdata\n")
  cat("  Function signature:\n")
  print(args(fluxind))
  cat("\n  Function body:\n")
  print(fluxind)
}
