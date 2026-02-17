# Test WoRMS Traits API
# Demonstrates what trait data is available from WoRMS Traits Portal

library(worrms)

cat("\n╔═══════════════════════════════════════════════════════════════╗\n")
cat("║ WoRMS TRAITS API TEST                                          ║\n")
cat("║ Testing what trait data is available from WoRMS Traits Portal  ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

# Test species list (Baltic invertebrates)
test_species <- c(
  "Mya arenaria",
  "Macoma balthica",
  "Gammarus oceanicus",
  "Hediste diversicolor"
)

for (species_name in test_species) {
  cat("════════════════════════════════════════════════════════════════\n")
  cat("Species:", species_name, "\n")
  cat("════════════════════════════════════════════════════════════════\n")

  # Get WoRMS record
  spp <- wm_records_name(species_name, marine_only = FALSE)

  if (is.null(spp) || nrow(spp) == 0) {
    cat("✗ Species not found in WoRMS\n\n")
    next
  }

  aphia_id <- as.numeric(spp$AphiaID[1])
  cat("AphiaID:", aphia_id, "\n")
  cat("Accepted name:", as.character(spp$scientificname[1]), "\n\n")

  # Get trait/attribute data
  cat("Querying trait data...\n")
  traits <- tryCatch({
    wm_attr_data(aphia_id)
  }, error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    NULL
  })

  if (is.null(traits) || nrow(traits) == 0) {
    cat("✗ No trait data available for this species\n\n")
  } else {
    cat("✓ Found", nrow(traits), "trait records\n\n")
    cat("Trait types available:\n")
    cat("─────────────────────────────────────────────────────────────────\n")
    for (i in 1:nrow(traits)) {
      cat(sprintf("  [%d] %s: %s\n",
                  i,
                  traits$measurementType[i],
                  traits$measurementValue[i]))
    }
    cat("\n")
  }
}

cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║ TEST COMPLETE                                                  ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

cat("Summary:\n")
cat("  The wm_attr_data() function queries the WoRMS Traits Portal\n")
cat("  for biological/ecological trait data.\n\n")

cat("  Available trait types may include:\n")
cat("    - Body size / length\n")
cat("    - Feeding mode / diet\n")
cat("    - Mobility / motility\n")
cat("    - Habitat / environment\n")
cat("    - Reproduction\n")
cat("    - Morphology\n\n")

cat("  Integration approach:\n")
cat("    1. Extract measurementType and measurementValue\n")
cat("    2. Map to categorical traits (MS, FS, MB, EP, PR)\n")
cat("    3. Add to trait hierarchy in lookup_species_traits()\n\n")
