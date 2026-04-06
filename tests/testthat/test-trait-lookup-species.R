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
