# Test Database Timeout Functionality
# Validates that slow database queries are stopped after timeout

# Load required functions
source("R/config.R")
source("R/functions/trait_lookup.R")

# Create cache directory
cache_dir <- file.path("cache", "taxonomy")
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

cat("\n╔═══════════════════════════════════════════════════════════════╗\n")
cat("║ DATABASE TIMEOUT TEST                                          ║\n")
cat("║ Testing that slow databases are stopped after timeout         ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

# Test with Baltic invertebrates that are NOT in SeaLifeBase
# These should timeout quickly instead of taking 12-39 seconds
test_species <- list(
  list(
    name = "Mya arenaria",
    type = "Mollusk (not in SeaLifeBase)",
    expected_slb_time = "<10s (timeout)",
    previous_time = "39.23s"
  ),
  list(
    name = "Gammarus spp.",
    type = "Amphipod (not in SeaLifeBase)",
    expected_slb_time = "<10s (timeout)",
    previous_time = "12.51s"
  ),
  list(
    name = "Hediste diversicolor",
    type = "Polychaete (not in SeaLifeBase)",
    expected_slb_time = "<10s (timeout)",
    previous_time = "27.90s"
  )
)

results <- list()

for (i in seq_along(test_species)) {
  test_case <- test_species[[i]]

  cat("════════════════════════════════════════════════════════════════\n")
  cat("TEST ", i, "/", length(test_species), ": ", test_case$name, "\n", sep = "")
  cat("════════════════════════════════════════════════════════════════\n")
  cat("Type: ", test_case$type, "\n")
  cat("Expected SeaLifeBase time: ", test_case$expected_slb_time, "\n")
  cat("Previous time (no timeout): ", test_case$previous_time, "\n\n")

  # Run full trait lookup with timeouts
  start_time <- Sys.time()

  result <- lookup_species_traits(
    species_name = test_case$name,
    biotic_file = if (file.exists("data/biotic_traits.csv")) "data/biotic_traits.csv" else NULL,
    cache_dir = cache_dir
  )

  total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  cat("\n")
  cat("─────────────────────────────────────────────────────────────────\n")
  cat("RESULT\n")
  cat("─────────────────────────────────────────────────────────────────\n")
  cat("Total time: ", round(total_time, 2), "s\n", sep = "")
  cat("Previous time: ", test_case$previous_time, "\n", sep = "")

  time_saved <- as.numeric(gsub("s", "", test_case$previous_time)) - total_time
  speedup_pct <- round(100 * time_saved / as.numeric(gsub("s", "", test_case$previous_time)), 1)

  if (speedup_pct > 0) {
    cat("Time saved: ", round(time_saved, 2), "s (", speedup_pct, "% faster) ✅\n", sep = "")
  } else {
    cat("Time saved: ", round(time_saved, 2), "s (", speedup_pct, "% slower) ⚠️\n", sep = "")
  }

  cat("\n")

  results[[i]] <- list(
    species = test_case$name,
    total_time = total_time,
    previous_time = as.numeric(gsub("s", "", test_case$previous_time)),
    time_saved = time_saved,
    speedup_pct = speedup_pct
  )
}

cat("\n╔═══════════════════════════════════════════════════════════════╗\n")
cat("║ TIMEOUT PERFORMANCE SUMMARY                                   ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

avg_current <- mean(sapply(results, function(x) x$total_time))
avg_previous <- mean(sapply(results, function(x) x$previous_time))
avg_saved <- mean(sapply(results, function(x) x$time_saved))
avg_speedup <- round(100 * avg_saved / avg_previous, 1)

cat("Average time WITH timeout:    ", round(avg_current, 2), "s\n", sep = "")
cat("Average time WITHOUT timeout: ", round(avg_previous, 2), "s\n", sep = "")
cat("Average time saved:           ", round(avg_saved, 2), "s\n", sep = "")
cat("Average speedup:              ", avg_speedup, "%\n\n", sep = "")

cat("INDIVIDUAL RESULTS:\n")
cat("─────────────────────────────────────────────────────────────────\n")
for (i in seq_along(results)) {
  r <- results[[i]]
  cat(sprintf("%-25s  %5.2fs → %5.2fs  [%+5.1f%%]\n",
              r$species,
              r$previous_time,
              r$total_time,
              r$speedup_pct))
}

cat("\n╔═══════════════════════════════════════════════════════════════╗\n")
cat("║ VALIDATION                                                     ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

if (avg_speedup >= 60) {
  cat("🎉 EXCELLENT: ", avg_speedup, "% speedup achieved!\n", sep = "")
  cat("   → Database timeouts working as expected\n")
  cat("   → Combined with taxonomic pre-filtering:\n")
  cat("     • Before all optimizations: ~55s per invertebrate\n")
  cat("     • After timeouts + pre-filtering: ~", round(avg_current, 1), "s per invertebrate\n", sep = "")
  cat("     • Total speedup: ", round(100 * (55 - avg_current) / 55, 1), "%\n\n", sep = "")
} else if (avg_speedup >= 40) {
  cat("✓ GOOD: ", avg_speedup, "% speedup\n", sep = "")
  cat("   → Timeouts are helping but may need fine-tuning\n\n")
} else {
  cat("⚠️  LIMITED IMPROVEMENT: ", avg_speedup, "% speedup\n", sep = "")
  cat("   → Timeouts may not be triggering as expected\n\n")
}

cat("════════════════════════════════════════════════════════════════\n")
cat("TIMEOUT TEST COMPLETE\n")
cat("════════════════════════════════════════════════════════════════\n\n")

invisible(results)
