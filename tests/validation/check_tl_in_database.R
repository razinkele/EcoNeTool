# Check if ECOPATH database has TL values

library(RODBC)
source("R/config.R")
source("R/functions/ecopath_import.R")

d <- parse_ecopath_native_windows("examples/LTgoby.eweaccdb")

cat("\n=== CHECKING FOR TL IN DATABASE ===\n\n")

cat("Group data columns:\n")
print(names(d$group_data))

cat("\n\nChecking for TrophicLevel column...\n")
if ("TrophicLevel" %in% names(d$group_data)) {
  cat("Found TrophicLevel column!\n\n")

  cat("Trophic Levels from ECOPATH database:\n")
  tl_data <- d$group_data[, c("GroupName", "TrophicLevel", "Type")]
  print(tl_data)
} else {
  cat("No TrophicLevel column found in database\n")
  cat("(ECOPATH stores TL only after balance, not in raw database)\n")
}
