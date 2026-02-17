# Demonstration of detailed trait lookup logging
# Testing with a single Baltic Sea species

# Load required functions
source("R/config.R")
source("R/functions/trait_lookup.R")

# Create cache directory
cache_dir <- file.path("cache", "taxonomy")
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

cat("\n════════════════════════════════════════════════════════════════\n")
cat("DETAILED TRAIT LOOKUP DEMONSTRATION\n")
cat("Species: Mya arenaria (Soft-shell clam)\n")
cat("════════════════════════════════════════════════════════════════\n\n")

# Run lookup with full detailed logging
result <- lookup_species_traits(
  species_name = "Mya arenaria",
  biotic_file = if (file.exists("data/biotic_traits.csv")) "data/biotic_traits.csv" else NULL,
  cache_dir = cache_dir
)

cat("\n════════════════════════════════════════════════════════════════\n")
cat("RESULT:\n")
cat("════════════════════════════════════════════════════════════════\n")
print(result)

cat("\n\nExpected traits from manual table:\n")
cat("  Size: L (Large)\n")
cat("  Foraging: Filter feeder / Scavenger\n")
cat("  Mobility: Sessile / Crawler burrower\n")
cat("  Position: Infauna / Epibenthic\n")
cat("  Protection: Hard shell / Burrow\n")
