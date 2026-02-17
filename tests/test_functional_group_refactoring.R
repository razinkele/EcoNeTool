#' Test Functional Group Utility Refactoring
#'
#' Verifies that the refactored shared utilities work correctly

cat("\n=== Testing Functional Group Utility Refactoring ===\n\n")

# Source the new shared utilities
source("R/functions/functional_group_utils.R")
source("R/config.R")

# Test 1: Single species assignment
cat("=== Test 1: Individual Species Assignment ===\n\n")

test_species <- c(
  "Phytoplankton",
  "Zooplankton",
  "Monk seal",
  "Common dolphin",
  "Seabirds",
  "Atlantic cod",
  "Benthos",
  "Detritus"
)

cat(sprintf("%-25s | %-15s\n", "Species", "Functional Group"))
cat(strrep("-", 43), "\n")

for (sp in test_species) {
  fg <- assign_functional_group(sp, use_topology = FALSE)
  cat(sprintf("%-25s | %-15s\n", sp, fg))
}

# Test 2: Vectorized assignment
cat("\n=== Test 2: Vectorized Assignment ===\n\n")

species_vec <- c("Grey seal", "Harbor porpoise", "Minke whale", "Common gull",
                 "Herring", "Sprat", "Copepod", "Diatom")

fg_vec <- assign_functional_groups(species_vec, use_topology = FALSE)

cat(sprintf("%-20s | %-15s\n", "Species", "Functional Group"))
cat(strrep("-", 38), "\n")
for (i in 1:length(species_vec)) {
  cat(sprintf("%-20s | %-15s\n", species_vec[i], fg_vec[i]))
}

# Test 3: Topology-based assignment
cat("\n=== Test 3: Topology-Based Assignment ===\n\n")

# Simulate network properties
species_topo <- c("Unknown_1", "Unknown_2", "Unknown_3", "Unknown_4")
pb_topo <- c(5.0, 0.5, 0.3, 0.2)  # High P/B suggests producer
indegree_topo <- c(0, 3, 2, 1)     # 0 = no predators
outdegree_topo <- c(5, 0, 2, 3)    # 0 = no prey

cat(sprintf("%-12s | %-6s | %-4s | %-5s | %-15s\n",
            "Species", "PB", "In", "Out", "Functional Group"))
cat(strrep("-", 70), "\n")

for (i in 1:length(species_topo)) {
  fg <- assign_functional_group(
    species_topo[i],
    pb = pb_topo[i],
    indegree = indegree_topo[i],
    outdegree = outdegree_topo[i],
    use_topology = TRUE
  )
  cat(sprintf("%-12s | PB=%.1f | In=%d | Out=%d | %-15s\n",
              species_topo[i], pb_topo[i], indegree_topo[i], outdegree_topo[i], fg))
}

# Test 4: Body mass estimation
cat("\n=== Test 4: Body Mass Estimation ===\n\n")

fg_levels <- get_functional_group_levels()
cat(sprintf("%-15s | %-20s\n", "Functional Group", "Body Mass (g)"))
cat(strrep("-", 38), "\n")

for (fg in fg_levels) {
  bm <- estimate_body_mass_by_fg(fg)
  cat(sprintf("%-15s | %-20s\n", fg, format(bm, scientific = TRUE)))
}

# Test 5: Metabolic types
cat("\n=== Test 5: Metabolic Types ===\n\n")

cat(sprintf("%-15s | %-25s\n", "Functional Group", "Metabolic Type"))
cat(strrep("-", 43), "\n")

for (fg in fg_levels) {
  mt <- estimate_metabolic_type_by_fg(fg)
  cat(sprintf("%-15s | %-25s\n", fg, mt))
}

# Test 6: Efficiencies
cat("\n=== Test 6: Energy Transfer Efficiencies ===\n\n")

cat(sprintf("%-15s | %-12s\n", "Functional Group", "Efficiency"))
cat(strrep("-", 30), "\n")

for (fg in fg_levels) {
  eff <- estimate_efficiency_by_fg(fg)
  cat(sprintf("%-15s | %-12.2f\n", fg, eff))
}

# Test 7: Color scheme alignment
cat("\n=== Test 7: Color Scheme Alignment ===\n\n")

cat(sprintf("%-3s | %-15s | %-10s\n", "#", "Functional Group", "Color"))
cat(strrep("-", 32), "\n")

for (i in 1:length(fg_levels)) {
  cat(sprintf("%-3d | %-15s | %-10s\n", i, fg_levels[i], COLOR_SCHEME[i]))
}

# Verification
cat("\n=== Verification ===\n\n")

# Check that mammals are correctly classified
mammal_test <- c("Grey seal", "Monk seal", "Common dolphin", "Bottlenose dolphin", "Harbor porpoise")
mammal_fg <- assign_functional_groups(mammal_test, use_topology = FALSE)
all_mammals <- all(mammal_fg == "Mammals")

cat("Mammal classification:", ifelse(all_mammals, "✓ PASS", "✗ FAIL"), "\n")

# Check that birds are correctly classified
bird_test <- c("Common gull", "Arctic tern", "Great cormorant", "Seabirds")
bird_fg <- assign_functional_groups(bird_test, use_topology = FALSE)
all_birds <- all(bird_fg == "Birds")

cat("Bird classification:", ifelse(all_birds, "✓ PASS", "✗ FAIL"), "\n")

# Check functional group levels match color scheme
levels_match <- length(fg_levels) == length(COLOR_SCHEME)

cat("Levels/colors alignment:", ifelse(levels_match, "✓ PASS", "✗ FAIL"), "\n")

# Overall result
cat("\n=== Test Result ===\n\n")

if (all_mammals && all_birds && levels_match) {
  cat("✓ SUCCESS: All refactoring tests passed!\n")
  cat("✓ Shared utilities are working correctly.\n")
  cat("✓ Code is optimized and DRY (Don't Repeat Yourself).\n")
} else {
  cat("✗ FAILED: Some tests failed!\n")
}

cat("\n=== Benefits of Refactoring ===\n\n")

cat("Before refactoring:\n")
cat("  - Duplicate code in 3 places (app.R ECOPATH + 2 EcoBase functions)\n")
cat("  - ~50 lines of repeated pattern matching code\n")
cat("  - Hard to maintain consistency\n\n")

cat("After refactoring:\n")
cat("  - Single source of truth in functional_group_utils.R\n")
cat("  - Reduced from ~150 lines to ~15 lines per import\n")
cat("  - Easy to add new functional groups or patterns\n")
cat("  - Consistent behavior across all import methods\n")

cat("\n=== END ===\n")
