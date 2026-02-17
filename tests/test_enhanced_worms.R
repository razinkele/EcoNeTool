# Test Enhanced WoRMS Trait Extraction
# Validates that body size and other traits are extracted from WoRMS Traits Portal

# Load required functions
source("R/config.R")
source("R/functions/trait_lookup.R")

# Create cache directory
cache_dir <- file.path("cache", "worms_enhanced_test")
if (dir.exists(cache_dir)) {
  unlink(cache_dir, recursive = TRUE)
}
dir.create(cache_dir, recursive = TRUE)

cat("\n╔═══════════════════════════════════════════════════════════════╗\n")
cat("║ ENHANCED WORMS TRAIT EXTRACTION TEST                           ║\n")
cat("║ Testing body size extraction from WoRMS Traits Portal          ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

# Test species with known body sizes from WoRMS Traits
test_species <- list(
  list(name = "Mya arenaria", expected_size_mm = 15, expected_habitat = "benthic"),
  list(name = "Macoma balthica", expected_size_mm = 5, expected_habitat = "benthic"),
  list(name = "Gammarus oceanicus", expected_size_mm = 38, expected_habitat = "benthic"),
  list(name = "Hediste diversicolor", expected_size_mm = 120, expected_habitat = "benthic")
)

results <- list()

for (i in seq_along(test_species)) {
  test_case <- test_species[[i]]

  cat("════════════════════════════════════════════════════════════════\n")
  cat("TEST ", i, "/", length(test_species), ": ", test_case$name, "\n", sep = "")
  cat("════════════════════════════════════════════════════════════════\n")
  cat("Expected max size: ", test_case$expected_size_mm, " mm (", test_case$expected_size_mm/10, " cm)\n", sep = "")
  cat("Expected habitat: ", test_case$expected_habitat, "\n\n", sep = "")

  # Lookup traits
  result <- lookup_species_traits(
    species_name = test_case$name,
    cache_dir = cache_dir
  )

  cat("\n")
  cat("─────────────────────────────────────────────────────────────────\n")
  cat("RESULT\n")
  cat("─────────────────────────────────────────────────────────────────\n")
  cat("Species: ", result$species, "\n", sep = "")
  cat("Source: ", result$source, "\n", sep = "")

  cat("\nTraits found:\n")
  cat("  MS (Size): ", ifelse(is.na(result$MS), "❌", paste0("✓ ", result$MS)), "\n", sep = "")
  cat("  FS (Foraging): ", ifelse(is.na(result$FS), "❌", paste0("✓ ", result$FS)), "\n", sep = "")
  cat("  MB (Mobility): ", ifelse(is.na(result$MB), "❌", paste0("✓ ", result$MB)), "\n", sep = "")
  cat("  EP (Environment): ", ifelse(is.na(result$EP), "❌", paste0("✓ ", result$EP)), "\n", sep = "")
  cat("  PR (Protection): ", ifelse(is.na(result$PR), "❌", paste0("✓ ", result$PR)), "\n\n", sep = "")

  # Check if body size was extracted
  size_extracted <- !is.na(result$MS)

  if (size_extracted) {
    cat("✅ SUCCESS: Body size extracted from WoRMS Traits Portal\n")
    cat("   Size class: ", result$MS, "\n", sep = "")
  } else {
    cat("❌ FAILED: Body size NOT extracted\n")
  }

  cat("\n")

  results[[i]] <- list(
    species = test_case$name,
    size_extracted = size_extracted,
    ms_class = result$MS,
    expected_size = test_case$expected_size_mm
  )
}

cat("\n╔═══════════════════════════════════════════════════════════════╗\n")
cat("║ TEST SUMMARY                                                   ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

success_count <- sum(sapply(results, function(x) x$size_extracted))
total_count <- length(results)
success_rate <- round(100 * success_count / total_count, 1)

cat("Species tested: ", total_count, "\n")
cat("Body size extracted: ", success_count, " (", success_rate, "%)\n", sep = "")
cat("Body size NOT extracted: ", total_count - success_count, "\n\n", sep = "")

cat("INDIVIDUAL RESULTS:\n")
cat("─────────────────────────────────────────────────────────────────\n")
for (i in seq_along(results)) {
  r <- results[[i]]
  status <- if (r$size_extracted) "✅" else "❌"
  cat(status, " ", r$species, " → ", ifelse(r$size_extracted, r$ms_class, "No size"), "\n", sep = "")
}

cat("\n╔═══════════════════════════════════════════════════════════════╗\n")
cat("║ VALIDATION                                                     ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

if (success_rate >= 75) {
  cat("🎉 EXCELLENT: ", success_rate, "% of species have body size from WoRMS!\n", sep = "")
  cat("   → Enhanced WoRMS extraction working correctly\n")
  cat("   → Trait completeness improved\n\n")
} else if (success_rate >= 50) {
  cat("✓ GOOD: ", success_rate, "% of species have body size\n", sep = "")
  cat("   → Some improvement but may need refinement\n\n")
} else {
  cat("⚠️  NEEDS WORK: Only ", success_rate, "% have body size\n", sep = "")
  cat("   → Check unit conversion logic\n\n")
}

# Clean up test cache
cat("Cleaning up test cache directory...\n")
unlink(cache_dir, recursive = TRUE)

cat("\n════════════════════════════════════════════════════════════════\n")
cat("ENHANCED WORMS TEST COMPLETE\n")
cat("════════════════════════════════════════════════════════════════\n\n")

invisible(results)
