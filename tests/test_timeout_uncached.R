# Test Database Timeout with Uncached Species
# Tests timeout functionality with species not in cache

# Load required functions
source("R/config.R")
source("R/functions/trait_lookup.R")

# Use a DIFFERENT cache directory to avoid cached results
cache_dir <- file.path("cache", "timeout_test")
if (dir.exists(cache_dir)) {
  unlink(cache_dir, recursive = TRUE)
}
dir.create(cache_dir, recursive = TRUE)

cat("\n╔═══════════════════════════════════════════════════════════════╗\n")
cat("║ UNCACHED DATABASE TIMEOUT TEST                                 ║\n")
cat("║ Testing timeout on fresh, uncached species                    ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

# Test with a species that's likely NOT in SeaLifeBase
# This will force the timeout to trigger
test_species <- "Cerastoderma glaucum"

cat("Testing species: ", test_species, " (Lagoon cockle)\n")
cat("Expected behavior:\n")
cat("  1. WoRMS lookup: ~1-3s (successful)\n")
cat("  2. SeaLifeBase query: TIMEOUT at 10s (not found)\n")
cat("  3. Total time: <15s (vs ~30-40s without timeout)\n\n")

cat("Starting lookup...\n\n")

start_time <- Sys.time()

result <- lookup_species_traits(
  species_name = test_species,
  biotic_file = if (file.exists("data/biotic_traits.csv")) "data/biotic_traits.csv" else NULL,
  cache_dir = cache_dir
)

total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

cat("\n╔═══════════════════════════════════════════════════════════════╗\n")
cat("║ RESULT                                                         ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

cat("Total time: ", round(total_time, 2), "s\n", sep = "")
cat("Species: ", result$species, "\n", sep = "")
cat("Source: ", result$source, "\n", sep = "")
cat("Confidence: ", result$confidence, "\n\n", sep = "")

cat("Traits found:\n")
cat("  MS (Size): ", ifelse(is.na(result$MS), "❌", paste0("✓ ", result$MS)), "\n", sep = "")
cat("  FS (Foraging): ", ifelse(is.na(result$FS), "❌", paste0("✓ ", result$FS)), "\n", sep = "")
cat("  MB (Mobility): ", ifelse(is.na(result$MB), "❌", paste0("✓ ", result$MB)), "\n", sep = "")
cat("  EP (Environment): ", ifelse(is.na(result$EP), "❌", paste0("✓ ", result$EP)), "\n", sep = "")
cat("  PR (Protection): ", ifelse(is.na(result$PR), "❌", paste0("✓ ", result$PR)), "\n\n", sep = "")

cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║ TIMEOUT VALIDATION                                             ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

if (total_time < 15) {
  cat("✅ SUCCESS: Timeout working correctly!\n")
  cat("   Total time: ", round(total_time, 2), "s (expected <15s)\n", sep = "")
  cat("   → SeaLifeBase timeout prevented long wait\n")
  cat("   → Expected speedup vs no timeout: ~70-85%\n\n")
} else if (total_time < 25) {
  cat("⚠️  PARTIAL: Timeout may be working but slower than expected\n")
  cat("   Total time: ", round(total_time, 2), "s (expected <15s)\n", sep = "")
  cat("   → Check if SeaLifeBase timeout triggered\n\n")
} else {
  cat("❌ TIMEOUT NOT WORKING: Time exceeded expected limit\n")
  cat("   Total time: ", round(total_time, 2), "s (expected <15s)\n", sep = "")
  cat("   → SeaLifeBase likely did NOT timeout\n\n")
}

# Clean up test cache
cat("Cleaning up test cache directory...\n")
unlink(cache_dir, recursive = TRUE)

cat("\n════════════════════════════════════════════════════════════════\n")
cat("UNCACHED TIMEOUT TEST COMPLETE\n")
cat("════════════════════════════════════════════════════════════════\n\n")
