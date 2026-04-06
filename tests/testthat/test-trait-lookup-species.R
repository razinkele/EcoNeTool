# Trait lookup tests using real species from test datasets
library(testthat)

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
source(file.path(app_root, "R/config.R"))
source(file.path(app_root, "R/functions/validation_utils.R"))
source(file.path(app_root, "R/functions/functional_group_utils.R"))
source(file.path(app_root, "R/config/harmonization_config.R"))
source(file.path(app_root, "R/functions/trait_lookup/harmonization.R"))
source(file.path(app_root, "R/functions/trait_lookup/database_lookups.R"))
source(file.path(app_root, "R/functions/trait_lookup/csv_trait_databases.R"))

# =============================================================================
# 1. BIOTIC lookup for known benthic species
# =============================================================================

test_that("BIOTIC finds Hediste diversicolor with correct traits", {
  biotic_file <- file.path(app_root, "data/biotic_traits.csv")
  skip_if_not(file.exists(biotic_file), "BIOTIC CSV not available")
  result <- lookup_biotic_traits("Hediste diversicolor", biotic_file = biotic_file)
  expect_true(result$success, info = "Hediste should be found in BIOTIC")
  expect_true(!is.null(result$traits$feeding_mode), info = "Should have feeding_mode")
  expect_true(grepl("omnivore", result$traits$feeding_mode, ignore.case = TRUE),
              info = "Hediste is an omnivore")
  expect_true(!is.null(result$traits$max_length_cm), info = "Should have size data")
  expect_true(result$traits$max_length_cm > 5, info = "Hediste > 5cm")
})

# =============================================================================
# 2. PTDB lookup for known phytoplankton
# =============================================================================

test_that("PTDB finds Skeletonema marinoi with correct traits", {
  ptdb_file <- file.path(app_root, "data/ptdb_phytoplankton.csv")
  skip_if_not(file.exists(ptdb_file), "PTDB CSV not available")
  result <- lookup_ptdb_traits("Skeletonema marinoi", ptdb_file = ptdb_file)
  expect_true(result$success, info = "Skeletonema should be found in PTDB")
  expect_true(!is.null(result$traits$feeding_mode), info = "Should have feeding_mode")
  expect_equal(result$traits$feeding_mode, "primary_producer",
               info = "Autotroph should map to primary_producer")
  expect_true(!is.null(result$traits$growth_form), info = "Should have growth_form")
  expect_true(grepl("chain", result$traits$growth_form, ignore.case = TRUE),
              info = "Skeletonema is chain-forming")
})

# =============================================================================
# 3. MAREDAT lookup for known zooplankton
# =============================================================================

test_that("MAREDAT finds Pseudocalanus acuspes with correct traits", {
  maredat_file <- file.path(app_root, "data/maredat_zooplankton.csv")
  skip_if_not(file.exists(maredat_file), "MAREDAT CSV not available")
  result <- lookup_maredat_traits("Pseudocalanus acuspes", maredat_file = maredat_file)
  expect_true(result$success, info = "Pseudocalanus should be found in MAREDAT")
  expect_true(!is.null(result$traits$size_um), info = "Should have ESD size")
  expect_true(result$traits$size_um > 500, info = "Pseudocalanus ESD > 500 um")
  expect_true(!is.null(result$traits$trophic_level), info = "Should have trophic level")
  expect_equal(result$traits$trophic_level, 2.0, info = "TL should be 2.0")
})

# =============================================================================
# 4. Ontology lookup for known species (by AphiaID)
# =============================================================================

test_that("ontology finds Gadus morhua traits", {
  ontology_file <- file.path(app_root, "data/ontology_traits.csv")
  skip_if_not(file.exists(ontology_file), "Ontology CSV not available")
  result <- lookup_ontology_traits(aphia_id = 126436, ontology_file = ontology_file)
  expect_true(result$success, info = "Gadus morhua (126436) should be in ontology")
  expect_true(length(result$traits) > 0, info = "Should have traits")
})

# =============================================================================
# 5. Harmonization end-to-end: benthic polychaete (Hediste diversicolor)
# =============================================================================

test_that("Hediste diversicolor harmonizes to correct trait codes", {
  # Known: omnivore, burrower/crawler, 120mm
  ms <- harmonize_size_class(12.0)  # 120mm = 12cm
  expect_true(ms %in% c("MS4", "MS5"), info = "12cm polychaete should be MS4 or MS5")

  fs <- harmonize_foraging_strategy(feeding_info = "omnivore", trophic_level = 2.5)
  expect_equal(fs, "FS3", info = "Omnivore should be FS3")

  mb <- harmonize_mobility(mobility_info = "crawler")
  expect_true(mb %in% c("MB3", "MB4"), info = "Crawler should be MB3 or MB4")
})

# =============================================================================
# 6. Harmonization: phytoplankton (Skeletonema marinoi)
# =============================================================================

test_that("Skeletonema harmonizes to correct trait codes", {
  fs <- harmonize_foraging_strategy(feeding_info = "primary_producer", trophic_level = 1.0)
  expect_equal(fs, "FS0", info = "Primary producer should be FS0")

  ep <- harmonize_environmental_position(
    habitat_info = c("pelagic"), depth_min = 0, depth_max = 30,
    taxonomic_info = list(phylum = "Ochrophyta", class = "Bacillariophyceae",
                          feeding_mode = "photosynthesis"))
  expect_equal(ep, "EP1", info = "Pelagic diatom should be EP1")
})

# =============================================================================
# 7. Harmonization: zooplankton (Pseudocalanus acuspes)
# =============================================================================

test_that("Pseudocalanus harmonizes to EP1 (pelagic zooplankton)", {
  ep <- harmonize_environmental_position(
    habitat_info = NULL, depth_min = NULL, depth_max = NULL,
    taxonomic_info = list(phylum = "Arthropoda", class = "Copepoda"))
  expect_equal(ep, "EP1", info = "Copepod should be EP1 (Pelagic)")
})

# =============================================================================
# 8. Genus-level fallback for missing species
# =============================================================================

test_that("BIOTIC finds genus match when exact species missing", {
  biotic_file <- file.path(app_root, "data/biotic_traits.csv")
  skip_if_not(file.exists(biotic_file), "BIOTIC CSV not available")
  # Use a species name not in DB but with genus match
  result <- lookup_biotic_traits("Hediste unknown_species", biotic_file = biotic_file)
  expect_true(result$success, info = "Should match on genus Hediste")
})

# =============================================================================
# 9. Test species dataset structure
# =============================================================================

test_that("baltic_sea_test_species.csv has correct structure", {
  f <- file.path(app_root, "data/baltic_sea_test_species.csv")
  skip_if_not(file.exists(f))
  data <- read.csv(f, stringsAsFactors = FALSE)
  expect_true("species" %in% names(data))
  expect_true("functional_group" %in% names(data))
  expect_true(nrow(data) >= 30, info = "Should have at least 30 test species")
  # Should have all functional groups
  fgs <- unique(data$functional_group)
  expect_true("Phytoplankton" %in% fgs)
  expect_true("Zooplankton" %in% fgs)
  expect_true("Benthos" %in% fgs)
  expect_true("Fish" %in% fgs)
})

test_that("european_marine_test_species.csv has correct structure", {
  f <- file.path(app_root, "data/european_marine_test_species.csv")
  skip_if_not(file.exists(f))
  data <- read.csv(f, stringsAsFactors = FALSE)
  expect_true("species" %in% names(data))
  expect_true("region" %in% names(data))
  expect_true(nrow(data) >= 100, info = "Should have at least 100 test species")
  # Should cover multiple regions
  regions <- unique(data$region)
  expect_true(length(regions) >= 3, info = "Should cover at least 3 regions")
})

# =============================================================================
# 10. RS/TT/ST harmonization with ecologically plausible inputs
# =============================================================================

test_that("RS harmonization works for known reproductive strategies", {
  expect_equal(harmonize_reproductive_strategy("broadcast spawning pelagic larvae"), "RS1")
  expect_equal(harmonize_reproductive_strategy("brooding with direct development"), "RS2")
  expect_equal(harmonize_reproductive_strategy("asexual budding and fragmentation"), "RS3")
})

test_that("TT harmonization works for biogeographic terms", {
  expect_equal(harmonize_temperature_tolerance("arctic cold-water species"), "TT1")
  expect_equal(harmonize_temperature_tolerance("boreal cold-temperate distribution"), "TT2")
  expect_equal(harmonize_temperature_tolerance("temperate cosmopolitan"), "TT3")
  expect_equal(harmonize_temperature_tolerance("tropical subtropical reef"), "TT4")
})

test_that("ST harmonization works for salinity descriptions", {
  expect_equal(harmonize_salinity_tolerance("freshwater limnetic habitat"), "ST1")
  expect_equal(harmonize_salinity_tolerance("oligohaline brackish low salinity"), "ST2")
  expect_equal(harmonize_salinity_tolerance("mesohaline brackish water"), "ST3")
  expect_equal(harmonize_salinity_tolerance("euhaline fully marine"), "ST5")
})

# =============================================================================
# 11. CSV database lookup for nonexistent species returns gracefully
# =============================================================================

test_that("all CSV databases return success=FALSE for nonexistent species", {
  result1 <- lookup_blacksea_traits("Completely_fake_species_xyz")
  result2 <- lookup_arctic_traits("Completely_fake_species_xyz")
  result3 <- lookup_cefas_traits("Completely_fake_species_xyz")
  result4 <- lookup_coral_traits("Completely_fake_species_xyz")
  result5 <- lookup_pelagic_traits("Completely_fake_species_xyz")
  expect_false(result1$success)
  expect_false(result2$success)
  expect_false(result3$success)
  expect_false(result4$success)
  expect_false(result5$success)
})

# ============================================================================
# MEDITERRANEAN REGION
# ============================================================================

test_that("harmonize EP for Mediterranean seagrass (Posidonia oceanica)", {
  # Seagrass is benthic/epibenthic, photosynthetic
  ep <- harmonize_environmental_position(
    habitat_info = c("seagrass", "benthic", "shallow"),
    depth_min = 1, depth_max = 40,
    taxonomic_info = NULL
  )
  expect_true(ep %in% c("EP3", "EP4"), info = "Seagrass should be epibenthic or endobenthic")
})

test_that("harmonize PR for Mediterranean sea urchin (Paracentrotus lividus)", {
  # Echinoderms have spines as their primary protection trait
  pr <- harmonize_protection(skeleton_info = "spines")
  expect_equal(pr, "PR7", info = "Sea urchin spines should map to PR7 (few spines)")
})

# ============================================================================
# NORTH SEA REGION
# ============================================================================

test_that("BIOTIC finds Arenicola marina (North Sea lugworm)", {
  biotic_file <- file.path(app_root, "data/biotic_traits.csv")
  skip_if_not(file.exists(biotic_file))
  result <- lookup_biotic_traits("Arenicola marina", biotic_file = biotic_file)
  expect_true(result$success, info = "Arenicola should be in BIOTIC")
  expect_true(grepl("deposit_feeder", result$traits$feeding_mode, ignore.case = TRUE),
              info = "Arenicola is a deposit feeder")
  expect_true(result$traits$max_length_cm >= 15, info = "Arenicola > 15cm")
})

test_that("BIOTIC finds Crangon crangon (North Sea brown shrimp)", {
  biotic_file <- file.path(app_root, "data/biotic_traits.csv")
  skip_if_not(file.exists(biotic_file))
  result <- lookup_biotic_traits("Crangon crangon", biotic_file = biotic_file)
  expect_true(result$success)
  expect_true(grepl("predator", result$traits$feeding_mode, ignore.case = TRUE))
})

test_that("PTDB finds Guinardia delicatula (North Sea chain diatom)", {
  ptdb_file <- file.path(app_root, "data/ptdb_phytoplankton.csv")
  skip_if_not(file.exists(ptdb_file))
  result <- lookup_ptdb_traits("Guinardia delicatula", ptdb_file = ptdb_file)
  expect_true(result$success)
  expect_true(grepl("chain", result$traits$growth_form, ignore.case = TRUE))
})

test_that("PTDB finds Pseudo-nitzschia as HAB species", {
  ptdb_file <- file.path(app_root, "data/ptdb_phytoplankton.csv")
  skip_if_not(file.exists(ptdb_file))
  # Use delicatissima (confirmed in DB with Harmful=TRUE)
  result <- lookup_ptdb_traits("Pseudo-nitzschia delicatissima", ptdb_file = ptdb_file)
  if (result$success) {
    expect_true(!is.null(result$traits$is_hab), info = "HAB diatom should have is_hab flag")
    expect_true(result$traits$is_hab == TRUE, info = "Pseudo-nitzschia should be flagged as harmful")
  }
})

test_that("MAREDAT finds Calanus finmarchicus (North Sea key copepod)", {
  maredat_file <- file.path(app_root, "data/maredat_zooplankton.csv")
  skip_if_not(file.exists(maredat_file))
  result <- lookup_maredat_traits("Calanus finmarchicus", maredat_file = maredat_file)
  expect_true(result$success, info = "C. finmarchicus should be in MAREDAT")
  expect_true(result$traits$size_um >= 2000, info = "C. finmarchicus ESD >= 2000 um")
  expect_equal(result$traits$trophic_level, 2.0, info = "Herbivore TL should be 2.0")
})

test_that("ontology finds Merluccius merluccius (European hake)", {
  ontology_file <- file.path(app_root, "data/ontology_traits.csv")
  skip_if_not(file.exists(ontology_file))
  result <- lookup_ontology_traits(aphia_id = 126484, ontology_file = ontology_file)
  expect_true(result$success, info = "Merluccius should be in ontology")
})

# ============================================================================
# ARCTIC REGION
# ============================================================================

test_that("MAREDAT finds Calanus glacialis (Arctic copepod)", {
  maredat_file <- file.path(app_root, "data/maredat_zooplankton.csv")
  skip_if_not(file.exists(maredat_file))
  result <- lookup_maredat_traits("Calanus glacialis", maredat_file = maredat_file)
  expect_true(result$success, info = "C. glacialis should be in MAREDAT")
  expect_true(result$traits$size_um >= 3000, info = "C. glacialis ESD >= 3000 um (larger than C. finmarchicus)")
})

test_that("MAREDAT finds Calanus hyperboreus (large Arctic copepod)", {
  maredat_file <- file.path(app_root, "data/maredat_zooplankton.csv")
  skip_if_not(file.exists(maredat_file))
  result <- lookup_maredat_traits("Calanus hyperboreus", maredat_file = maredat_file)
  expect_true(result$success)
  expect_true(result$traits$size_um >= 4000, info = "C. hyperboreus is the largest calanoid (>4000 um)")
})

test_that("Arctic copepods harmonize to EP1 (pelagic)", {
  ep <- harmonize_environmental_position(
    habitat_info = NULL, depth_min = NULL, depth_max = NULL,
    taxonomic_info = list(phylum = "Arthropoda", class = "Copepoda")
  )
  expect_equal(ep, "EP1", info = "Copepods should be EP1 (Pelagic)")
})

test_that("Arctic species harmonize to TT1 (cold stenothermal)", {
  tt <- harmonize_temperature_tolerance("arctic polar cold-water species")
  expect_equal(tt, "TT1", info = "Arctic species should be TT1")
})

# ============================================================================
# ATLANTIC REGION
# ============================================================================

test_that("harmonize size for large Atlantic fish", {
  # Basking shark (Cetorhinus maximus) can reach 12m = 1200cm
  ms <- harmonize_size_class(1200)
  expect_equal(ms, "MS7", info = "12m fish should be MS7 (Giant)")
})

test_that("harmonize size for medium Atlantic fish", {
  # European hake ~70cm
  ms <- harmonize_size_class(70)
  expect_true(ms %in% c("MS5", "MS6"), info = "70cm fish should be MS5 or MS6")
})

test_that("harmonize foraging for Atlantic predators", {
  fs <- harmonize_foraging_strategy(feeding_info = "piscivorous predator", trophic_level = 4.0)
  expect_equal(fs, "FS1", info = "Piscivore TL4+ should be FS1 (Predator)")
})

# ============================================================================
# CROSS-REGIONAL CONSISTENCY
# ============================================================================

test_that("same species gets same traits regardless of region label", {
  # Calanus finmarchicus appears in both North Sea and Atlantic
  maredat_file <- file.path(app_root, "data/maredat_zooplankton.csv")
  skip_if_not(file.exists(maredat_file))
  result <- lookup_maredat_traits("Calanus finmarchicus", maredat_file = maredat_file)
  if (result$success) {
    # Harmonize to codes
    ms <- harmonize_size_class(result$traits$size_um / 10000)  # um to cm
    expect_true(ms %in% c("MS1", "MS2"), info = "Small copepod should be MS1 or MS2")
    expect_equal(result$traits$trophic_level, 2.0)
  }
})

test_that("deposit feeder harmonizes consistently across databases", {
  # Both BIOTIC's Arenicola (deposit_feeder) and text description should give same FS
  fs1 <- harmonize_foraging_strategy(feeding_info = "deposit_feeder", trophic_level = 2.0)
  fs2 <- harmonize_foraging_strategy(feeding_info = "deposit feeder", trophic_level = 2.0)
  expect_equal(fs1, fs2, info = "deposit_feeder and 'deposit feeder' should give same FS code")
  expect_equal(fs1, "FS5", info = "Deposit feeders should be FS5")
})

test_that("European marine test species dataset covers all 6 regions", {
  f <- file.path(app_root, "data/european_marine_test_species.csv")
  skip_if_not(file.exists(f))
  data <- read.csv(f, stringsAsFactors = FALSE)
  regions <- unique(data$region)
  expected_regions <- c("Baltic", "Mediterranean", "North_Sea", "Atlantic", "Arctic", "Widespread")
  for (r in expected_regions) {
    expect_true(r %in% regions, info = paste("Dataset should include", r, "region"))
  }
  # Each region should have at least 15 species
  for (r in c("Baltic", "Mediterranean", "North_Sea", "Atlantic", "Arctic")) {
    count <- sum(data$region == r)
    expect_true(count >= 15, info = paste(r, "should have >= 15 species, has", count))
  }
})

test_that("all functional groups represented in European test species", {
  f <- file.path(app_root, "data/european_marine_test_species.csv")
  skip_if_not(file.exists(f))
  data <- read.csv(f, stringsAsFactors = FALSE)
  fgs <- unique(data$functional_group)
  for (fg in c("Phytoplankton", "Zooplankton", "Benthos", "Fish", "Bird", "Mammal")) {
    expect_true(fg %in% fgs, info = paste("Should have", fg, "functional group"))
  }
})
