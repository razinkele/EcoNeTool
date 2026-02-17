# Test Trait Lookup System with Baltic Sea Invertebrates
# Comparing automated lookup vs manual trait table
#
# Reference table from user with 18 invertebrate species

# Load required functions
source("R/config.R")
source("R/functions/trait_lookup.R")

# Define test species (from the table - ALL 18 species)
test_species <- data.frame(
  abbreviation = c("Ai", "Gsp", "Hdi", "Mba", "Msp", "Mar",
                   "Met", "Sen", "Bpi", "Cgl", "Csp", "Hsp", "Iba", "Jal",
                   "Oli", "Osp", "Pel", "Tfl"),
  scientific_name = c(
    "Amphibalanus improvisus",    # Ai - Barnacle
    "Gammarus",                   # Gsp - Gammarus spp. (genus level)
    "Hediste diversicolor",       # Hdi - Ragworm (polychaete)
    "Macoma balthica",            # Mba - Baltic clam
    "Marenzelleria",              # Msp - Marenzelleria spp. (genus level)
    "Mya arenaria",               # Mar - Soft-shell clam
    "Mytilus edulis trossulus",   # Met - Baltic mussel
    "Saduria entomon",            # Sen - Isopod
    "Bathyporeia pilosa",         # Bpi - Amphipod
    "Cerastoderma glaucum",       # Cgl - Lagoon cockle
    "Corophium",                  # Csp - Corophium spp. (genus level)
    "Hydrobia",                   # Hsp - Hydrobia spp. (genus level)
    "Idotea balthica",            # Iba - Baltic isopod
    "Jaera albifrons",            # Jal - Isopod
    "Oligochaeta",                # Oli - Oligochaeta undet. (class level)
    "Ostracoda",                  # Osp - Ostracoda undet. (class level)
    "Pygospio elegans",           # Pel - Polychaete worm
    "Theodoxus fluviatilis"       # Tfl - River nerite snail
  ),
  stringsAsFactors = FALSE
)

# Manual trait assignments from the table
manual_traits <- list(
  "Amphibalanus improvisus" = list(
    size = "ML", foraging = "Filter feeder", consumer = "Detritivore",
    mobility = "Sessile", position = "Epibenthic", protection = "Hard shell"
  ),
  "Gammarus" = list(
    size = "SM", foraging = "Grazer/Deposit feeder", consumer = "Detritivore/Herbivore/Omnivore",
    mobility = "Passive floater/Crawler burrower/Facultative swimmer", position = "Epibenthic",
    protection = "Soft shell"
  ),
  "Hediste diversicolor" = list(
    size = "ML", foraging = "Predator/Deposit feeder", consumer = "Detritivore/Carnivore/Omnivore",
    mobility = "Crawler burrower", position = "Infauna/Epibenthic",
    protection = "Burrow/Tube"
  ),
  "Macoma balthica" = list(
    size = "SM", foraging = "Deposit feeder", consumer = "Detritivore",
    mobility = "Crawler burrower", position = "Infauna",
    protection = "Hard shell/Burrow"
  ),
  "Marenzelleria" = list(
    size = "ML", foraging = "Grazer/Deposit feeder", consumer = "Detritivore/Herbivore",
    mobility = "Crawler burrower", position = "Infauna",
    protection = "Tube/Burrow"
  ),
  "Mya arenaria" = list(
    size = "L", foraging = "Filter feeder/Scavenger", consumer = "Detritivore/Omnivore",
    mobility = "Sessile/Crawler burrower", position = "Infauna/Epibenthic",
    protection = "Hard shell/Burrow"
  ),
  "Mytilus edulis trossulus" = list(
    size = "ML", foraging = "Filter feeder", consumer = "Detritivore/Omnivore",
    mobility = "Sessile", position = "Epibenthic", protection = "Hard shell"
  ),
  "Saduria entomon" = list(
    size = "ML", foraging = "Predator", consumer = "Carnivore",
    mobility = "Crawler burrower/Facultative swimmer", position = "Epibenthic/Benthopelagic",
    protection = "Soft shell"
  ),
  "Bathyporeia pilosa" = list(
    size = "SM", foraging = "Grazer/Deposit feeder", consumer = "Detritivore/Omnivore",
    mobility = "Crawler burrower/Facultative swimmer", position = "Infauna/Epibenthic",
    protection = "Burrow"
  ),
  "Cerastoderma glaucum" = list(
    size = "ML", foraging = "Filter feeder", consumer = "Detritivore",
    mobility = "Crawler burrower", position = "Infauna", protection = "Hard shell/Burrow"
  ),
  "Corophium" = list(
    size = "SM", foraging = "Deposit feeder", consumer = "Detritivore/Omnivore",
    mobility = "Crawler burrower/Facultative swimmer", position = "Infauna",
    protection = "Tube/Burrow"
  ),
  "Hydrobia" = list(
    size = "SM", foraging = "Grazer", consumer = "Herbivore/Detritivore",
    mobility = "Crawler burrower", position = "Epibenthic", protection = "Hard shell"
  ),
  "Idotea balthica" = list(
    size = "ML", foraging = "Grazer/Deposit feeder", consumer = "Herbivore/Detritivore",
    mobility = "Crawler burrower/Facultative swimmer", position = "Epibenthic",
    protection = "Soft shell"
  ),
  "Jaera albifrons" = list(
    size = "SM", foraging = "Grazer/Deposit feeder", consumer = "Herbivore/Omnivore",
    mobility = "Crawler burrower", position = "Epibenthic", protection = "Soft shell"
  ),
  "Oligochaeta" = list(
    size = "SM", foraging = "Deposit feeder", consumer = "Detritivore",
    mobility = "Crawler burrower", position = "Infauna", protection = "Burrow/No protection"
  ),
  "Ostracoda" = list(
    size = "SM", foraging = "Deposit feeder", consumer = "Detritivore/Omnivore",
    mobility = "Passive floater/Crawler burrower", position = "Infauna/Epibenthic",
    protection = "Soft shell"
  ),
  "Pygospio elegans" = list(
    size = "SM", foraging = "Filter feeder/Deposit feeder", consumer = "Detritivore",
    mobility = "Crawler burrower", position = "Infauna", protection = "Tube/Burrow"
  ),
  "Theodoxus fluviatilis" = list(
    size = "ML", foraging = "Grazer", consumer = "Herbivore/Detritivore",
    mobility = "Crawler burrower", position = "Epibenthic", protection = "Hard shell"
  )
)

cat("\n╔═══════════════════════════════════════════════════════════════╗\n")
cat("║ BALTIC SEA INVERTEBRATE TRAIT LOOKUP TEST                     ║\n")
cat("║ Testing automated lookup vs manual trait table                ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n")

cat("\nTest species: ", nrow(test_species), "\n")
cat("Database coverage: WoRMS, FishBase, SeaLifeBase, BIOTIC (if available)\n")
cat("\n════════════════════════════════════════════════════════════════\n")

# Create cache directory
cache_dir <- file.path("cache", "taxonomy")
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

# Run lookups
results <- list()
timings <- numeric()
success_count <- 0

for (i in seq_len(nrow(test_species))) {
  sp_abbrev <- test_species$abbreviation[i]
  sp_name <- test_species$scientific_name[i]

  cat("\n\n")
  cat("════════════════════════════════════════════════════════════════\n")
  cat("TEST ", i, "/", nrow(test_species), ": ", sp_abbrev, " - ", sp_name, "\n")
  cat("════════════════════════════════════════════════════════════════\n")

  start_time <- Sys.time()

  result <- tryCatch({
    lookup_species_traits(
      species_name = sp_name,
      biotic_file = if (file.exists("data/biotic_traits.csv")) "data/biotic_traits.csv" else NULL,
      cache_dir = cache_dir
    )
  }, error = function(e) {
    message("\n❌ ERROR: ", e$message)
    NULL
  })

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  timings[i] <- elapsed

  if (!is.null(result)) {
    results[[sp_name]] <- result

    # Count success
    n_traits <- sum(!is.na(c(result$MS, result$FS, result$MB, result$EP, result$PR)))
    if (n_traits >= 3) success_count <- success_count + 1

    # Compare with manual traits
    if (sp_name %in% names(manual_traits)) {
      cat("\n┌────────────────────────────────────────────────────────────┐\n")
      cat("│ COMPARISON WITH MANUAL TRAIT TABLE                        │\n")
      cat("├────────────────────────────────────────────────────────────┤\n")
      manual <- manual_traits[[sp_name]]
      cat("│ Size:      Manual = ", sprintf("%-20s", manual$size), "            │\n", sep="")
      cat("│            Auto   = ", sprintf("%-20s", ifelse(is.na(result$MS), "NA", result$MS)), "            │\n", sep="")
      cat("│ Foraging:  Manual = ", sprintf("%-30s", substr(manual$foraging, 1, 30)), "│\n", sep="")
      cat("│            Auto   = ", sprintf("%-30s", ifelse(is.na(result$FS), "NA", result$FS)), "│\n", sep="")
      cat("│ Mobility:  Manual = ", sprintf("%-30s", substr(manual$mobility, 1, 30)), "│\n", sep="")
      cat("│            Auto   = ", sprintf("%-30s", ifelse(is.na(result$MB), "NA", result$MB)), "│\n", sep="")
      cat("│ Position:  Manual = ", sprintf("%-30s", substr(manual$position, 1, 30)), "│\n", sep="")
      cat("│            Auto   = ", sprintf("%-30s", ifelse(is.na(result$EP), "NA", result$EP)), "│\n", sep="")
      cat("│ Protectn:  Manual = ", sprintf("%-30s", substr(manual$protection, 1, 30)), "│\n", sep="")
      cat("│            Auto   = ", sprintf("%-30s", ifelse(is.na(result$PR), "NA", result$PR)), "│\n", sep="")
      cat("└────────────────────────────────────────────────────────────┘\n")
    }
  }

  # Rate limiting
  if (i < nrow(test_species)) {
    Sys.sleep(0.5)
  }
}

# Summary statistics
cat("\n\n")
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║ TEST SUMMARY                                                   ║\n")
cat("╠═══════════════════════════════════════════════════════════════╣\n")
cat("║ Total species tested:  ", sprintf("%-36s", nrow(test_species)), "║\n", sep="")
cat("║ Successful lookups:    ", sprintf("%-36s", success_count), "║\n", sep="")
cat("║ Success rate:          ", sprintf("%-36s", paste0(round(100*success_count/nrow(test_species)), "%")), "║\n", sep="")
cat("║                                                                ║\n")
cat("║ Average lookup time:   ", sprintf("%-36s", paste0(round(mean(timings, na.rm=TRUE), 2), "s")), "║\n", sep="")
cat("║ Total time:            ", sprintf("%-36s", paste0(round(sum(timings, na.rm=TRUE), 2), "s")), "║\n", sep="")
cat("╚═══════════════════════════════════════════════════════════════╝\n")

# Trait coverage summary
if (length(results) > 0) {
  cat("\n")
  cat("╔═══════════════════════════════════════════════════════════════╗\n")
  cat("║ TRAIT COVERAGE                                                 ║\n")
  cat("╠═══════════════════════════════════════════════════════════════╣\n")

  ms_count <- sum(sapply(results, function(x) !is.na(x$MS)))
  fs_count <- sum(sapply(results, function(x) !is.na(x$FS)))
  mb_count <- sum(sapply(results, function(x) !is.na(x$MB)))
  ep_count <- sum(sapply(results, function(x) !is.na(x$EP)))
  pr_count <- sum(sapply(results, function(x) !is.na(x$PR)))

  cat("║ MS (Max Size):          ", sprintf("%-5s", ms_count), " / ", nrow(test_species), " (",
      sprintf("%3d", round(100*ms_count/nrow(test_species))), "%)                ║\n", sep="")
  cat("║ FS (Foraging):          ", sprintf("%-5s", fs_count), " / ", nrow(test_species), " (",
      sprintf("%3d", round(100*fs_count/nrow(test_species))), "%)                ║\n", sep="")
  cat("║ MB (Mobility):          ", sprintf("%-5s", mb_count), " / ", nrow(test_species), " (",
      sprintf("%3d", round(100*mb_count/nrow(test_species))), "%)                ║\n", sep="")
  cat("║ EP (Env. Position):     ", sprintf("%-5s", ep_count), " / ", nrow(test_species), " (",
      sprintf("%3d", round(100*ep_count/nrow(test_species))), "%)                ║\n", sep="")
  cat("║ PR (Protection):        ", sprintf("%-5s", pr_count), " / ", nrow(test_species), " (",
      sprintf("%3d", round(100*pr_count/nrow(test_species))), "%)                ║\n", sep="")
  cat("╚═══════════════════════════════════════════════════════════════╝\n")
}

# Create results data frame
if (length(results) > 0) {
  results_df <- do.call(rbind, results)
  rownames(results_df) <- NULL

  cat("\n\nFinal results table:\n")
  print(results_df[, c("species", "MS", "FS", "MB", "EP", "PR", "source", "confidence")])

  # Save results
  output_file <- "tests/test_results/baltic_invertebrate_traits_automated.csv"
  if (!dir.exists("tests/test_results")) {
    dir.create("tests/test_results", recursive = TRUE)
  }
  write.csv(results_df, output_file, row.names = FALSE)
  cat("\n✓ Results saved to:", output_file, "\n")
}

cat("\n════════════════════════════════════════════════════════════════\n")
cat("TEST COMPLETE\n")
cat("════════════════════════════════════════════════════════════════\n\n")
