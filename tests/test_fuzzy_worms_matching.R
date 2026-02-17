# Test Improved WoRMS Fuzzy Name Matching
# Validates that the 4-strategy fuzzy matching improves WoRMS success rate

# Load required functions
source("R/config.R")
source("R/functions/trait_lookup.R")

# Create cache directory
cache_dir <- file.path("cache", "taxonomy")
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

cat("\n╔═══════════════════════════════════════════════════════════════╗\n")
cat("║ WORMS FUZZY MATCHING VALIDATION TEST                          ║\n")
cat("║ Testing 4-strategy fuzzy matching on challenging Baltic spp.  ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

# Test cases that previously failed WoRMS lookup
test_species <- list(
  list(name = "Macoma balthica",
       reason = "Common Baltic clam - may be synonym issue",
       expected = "Should find via exact or fuzzy match"),

  list(name = "Mytilus edulis trossulus",
       reason = "Subspecies name (trinomial)",
       expected = "Should find via binomial strategy ('Mytilus edulis')"),

  list(name = "Gammarus spp.",
       reason = "Genus-level identification",
       expected = "Should find via genus-only strategy ('Gammarus')"),

  list(name = "Mya arenaria",
       reason = "Common soft-shell clam - previous failure",
       expected = "Should find via exact or fuzzy match"),

  list(name = "Idotea balthica",
       reason = "Baltic isopod - geographic subspecies",
       expected = "Should find via exact or fuzzy match"),

  list(name = "Marenzelleria spp.",
       reason = "Genus-level identification",
       expected = "Should find via genus-only strategy ('Marenzelleria')"),

  list(name = "Hediste diversicolor",
       reason = "Ragworm - may be in WoRMS",
       expected = "Should find via exact match"),

  list(name = "Saduria entomon",
       reason = "Baltic isopod - previous failure likely",
       expected = "Should find via exact or fuzzy match")
)

# Track success metrics
results <- data.frame(
  species = character(),
  worms_found = logical(),
  strategy_used = character(),
  phylum = character(),
  class = character(),
  time_seconds = numeric(),
  stringsAsFactors = FALSE
)

cat("════════════════════════════════════════════════════════════════\n")
cat("TESTING ", length(test_species), " CHALLENGING BALTIC SPECIES\n")
cat("════════════════════════════════════════════════════════════════\n\n")

for (i in seq_along(test_species)) {
  test_case <- test_species[[i]]
  species <- test_case$name

  cat("─────────────────────────────────────────────────────────────────\n")
  cat("TEST ", i, "/", length(test_species), ": ", species, "\n")
  cat("─────────────────────────────────────────────────────────────────\n")
  cat("Reason: ", test_case$reason, "\n")
  cat("Expected: ", test_case$expected, "\n\n")

  # Test WoRMS lookup
  start_time <- Sys.time()
  worms_result <- lookup_worms_traits(species)
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  # Record results
  if (worms_result$success) {
    cat("✅ SUCCESS (", round(elapsed, 2), "s)\n", sep = "")
    cat("   Phylum: ", worms_result$traits$phylum, "\n", sep = "")
    cat("   Class: ", worms_result$traits$class, "\n", sep = "")
    if (!is.na(worms_result$traits$order) && worms_result$traits$order != "") {
      cat("   Order: ", worms_result$traits$order, "\n", sep = "")
    }

    # Try to extract which strategy was used from the result
    strategy_used <- "unknown"
    # The strategy message is printed during lookup, capture it if possible

    results <- rbind(results, data.frame(
      species = species,
      worms_found = TRUE,
      strategy_used = strategy_used,
      phylum = worms_result$traits$phylum,
      class = worms_result$traits$class,
      time_seconds = elapsed,
      stringsAsFactors = FALSE
    ))
  } else {
    cat("❌ FAILED (", round(elapsed, 2), "s)\n", sep = "")
    cat("   Reason: ", worms_result$note, "\n", sep = "")

    results <- rbind(results, data.frame(
      species = species,
      worms_found = FALSE,
      strategy_used = "none",
      phylum = NA,
      class = NA,
      time_seconds = elapsed,
      stringsAsFactors = FALSE
    ))
  }

  cat("\n")
}

cat("\n╔═══════════════════════════════════════════════════════════════╗\n")
cat("║ TEST RESULTS SUMMARY                                           ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

success_count <- sum(results$worms_found)
total_count <- nrow(results)
success_rate <- round(100 * success_count / total_count, 1)

cat("Species tested: ", total_count, "\n")
cat("WoRMS found: ", success_count, " (", success_rate, "%)\n", sep = "")
cat("WoRMS failed: ", total_count - success_count, " (", round(100 - success_rate, 1), "%)\n\n", sep = "")

cat("Average lookup time: ", round(mean(results$time_seconds), 2), "s\n\n", sep = "")

cat("SUCCESS BREAKDOWN BY SPECIES:\n")
cat("─────────────────────────────────────────────────────────────────\n")
for (i in 1:nrow(results)) {
  status <- if (results$worms_found[i]) "✅" else "❌"
  cat(status, " ", results$species[i], "\n", sep = "")
  if (results$worms_found[i]) {
    cat("     → ", results$phylum[i], " / ", results$class[i], "\n", sep = "")
  }
}

cat("\n╔═══════════════════════════════════════════════════════════════╗\n")
cat("║ VALIDATION AGAINST EXPECTED SUCCESS RATE                      ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

if (success_rate >= 75) {
  cat("🎉 EXCELLENT: ", success_rate, "% success rate meets optimization goal!\n", sep = "")
  cat("   → Fuzzy matching is working as expected\n")
  cat("   → Taxonomic pre-filtering should now be effective\n")
  cat("   → Expected overall speedup: 70-90%\n\n")
} else if (success_rate >= 50) {
  cat("⚠️  GOOD: ", success_rate, "% success rate is improved\n", sep = "")
  cat("   → Some fuzzy matching strategies are working\n")
  cat("   → May need additional refinement for edge cases\n")
  cat("   → Expected overall speedup: 40-60%\n\n")
} else {
  cat("❌ NEEDS WORK: ", success_rate, "% success rate is too low\n", sep = "")
  cat("   → Fuzzy matching strategies may need adjustment\n")
  cat("   → Consider additional fallback strategies\n")
  cat("   → Expected overall speedup: <40%\n\n")
}

cat("════════════════════════════════════════════════════════════════\n")
cat("TEST COMPLETE\n")
cat("════════════════════════════════════════════════════════════════\n\n")

# Return results for further analysis
invisible(results)
