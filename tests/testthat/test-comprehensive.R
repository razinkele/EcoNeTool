library(testthat)
library(igraph)

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
source(file.path(app_root, "R/config.R"))
source(file.path(app_root, "R/functions/validation_utils.R"))
source(file.path(app_root, "R/functions/functional_group_utils.R"))
source(file.path(app_root, "R/config/harmonization_config.R"))
source(file.path(app_root, "R/functions/trait_foodweb.R"))
source(file.path(app_root, "R/functions/trait_lookup/harmonization.R"))
source(file.path(app_root, "R/functions/trait_lookup/database_lookups.R"))
source(file.path(app_root, "R/functions/trait_lookup/csv_trait_databases.R"))

# =============================================================================
# 1. FS3 Omnivore participates in food web
# =============================================================================
test_that("FS3 Omnivore has nonzero interaction probability", {
  consumer <- c(MS = "MS4", FS = "FS3", MB = "MB5", EP = "EP1")
  resource <- c(MS = "MS3", MB = "MB3", EP = "EP3", PR = "PR0")
  prob <- calc_interaction_probability(consumer, resource)
  expect_true(!is.na(prob) && prob > 0,
              info = "FS3 Omnivore must participate in food web (not excluded)")
})

# =============================================================================
# 2. FS7 Xylophagous has nonzero probability
# =============================================================================
test_that("FS7 Xylophagous has nonzero interaction probability", {
  consumer <- c(MS = "MS4", FS = "FS7", MB = "MB3", EP = "EP4")
  resource <- c(MS = "MS4", MB = "MB1", EP = "EP4", PR = "PR0")
  prob <- calc_interaction_probability(consumer, resource)
  expect_true(!is.na(prob) && prob > 0,
              info = "FS7 Xylophagous must have nonzero probability")
})

# =============================================================================
# 3. PR4 and PR5 have proper probabilities
# =============================================================================
test_that("PR4 Exoskeleton has matrix-based probability", {
  consumer <- c(MS = "MS4", FS = "FS1", MB = "MB5", EP = "EP1")
  resource <- c(MS = "MS3", MB = "MB3", EP = "EP3", PR = "PR4")
  prob <- calc_interaction_probability(consumer, resource)
  expect_true(!is.na(prob) && prob > 0 && prob < 1)
})

test_that("PR5 Soft shell has matrix-based probability", {
  consumer <- c(MS = "MS4", FS = "FS1", MB = "MB5", EP = "EP1")
  resource <- c(MS = "MS3", MB = "MB3", EP = "EP3", PR = "PR5")
  prob <- calc_interaction_probability(consumer, resource)
  expect_true(!is.na(prob) && prob > 0 && prob < 1)
})

# =============================================================================
# 4. EP scheme end-to-end
# =============================================================================
test_that("EP1 Pelagic consumer accesses MS2 prey better than EP4 Endobenthic", {
  pelagic <- c(MS = "MS4", FS = "FS1", MB = "MB5", EP = "EP1")
  endobenthic <- c(MS = "MS4", FS = "FS1", MB = "MB3", EP = "EP4")
  resource <- c(MS = "MS2", MB = "MB4", EP = "EP1", PR = "PR0")
  prob_pelagic <- calc_interaction_probability(pelagic, resource)
  prob_endo <- calc_interaction_probability(endobenthic, resource)
  expect_true(prob_pelagic > prob_endo,
              info = "Pelagic consumer should access small pelagic prey better than endobenthic")
})

# =============================================================================
# 5. Harmonization RS/TT/ST edge cases
# =============================================================================
test_that("harmonize_reproductive_strategy handles edge cases", {
  expect_equal(harmonize_reproductive_strategy(NULL), NA_character_)
  expect_equal(harmonize_reproductive_strategy(NA), NA_character_)
  expect_equal(harmonize_reproductive_strategy(""), NA_character_)
  expect_equal(harmonize_reproductive_strategy("BROADCAST SPAWNER"), "RS1")  # uppercase
  expect_equal(harmonize_reproductive_strategy("Direct Development"), "RS2")  # mixed case
})

test_that("harmonize_temperature_tolerance handles edge cases", {
  expect_equal(harmonize_temperature_tolerance(NULL), NA_character_)
  expect_equal(harmonize_temperature_tolerance(""), NA_character_)
  expect_equal(harmonize_temperature_tolerance("ARCTIC"), "TT1")
  expect_equal(harmonize_temperature_tolerance("Tropical reef"), "TT4")
})

test_that("harmonize_salinity_tolerance handles edge cases", {
  expect_equal(harmonize_salinity_tolerance(NULL), NA_character_)
  expect_equal(harmonize_salinity_tolerance(""), NA_character_)
  expect_equal(harmonize_salinity_tolerance("EUHALINE"), "ST5")
  expect_equal(harmonize_salinity_tolerance("polyhaline"), "ST4")
})

# =============================================================================
# 6. CSV .extract helpers edge cases
# =============================================================================
test_that(".extract handles NA and empty string correctly", {
  row <- data.frame(a = "value", b = NA, c = "", d = 42, stringsAsFactors = FALSE)
  expect_equal(.extract(row, "a"), "value")
  expect_null(.extract(row, "b"))  # NA -> NULL
  expect_null(.extract(row, "c"))  # empty string -> NULL
  expect_null(.extract(row, "nonexistent"))  # missing column -> NULL
})

test_that(".extract_num returns NULL for non-numeric", {
  row <- data.frame(a = "text", b = 42, c = NA, stringsAsFactors = FALSE)
  expect_null(.extract_num(row, "a"))  # non-numeric -> NULL
  expect_equal(.extract_num(row, "b"), 42)
  expect_null(.extract_num(row, "c"))  # NA -> NULL
})

# =============================================================================
# 7. Schema migration idempotency
# =============================================================================
test_that("schema migration is idempotent (safe to call twice)", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  source(file.path(app_root, "R/functions/cache_sqlite.R"))

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE species_traits (species TEXT PRIMARY KEY, MS TEXT)")
  DBI::dbExecute(con, "CREATE TABLE metadata (key TEXT PRIMARY KEY, value TEXT)")

  # Call twice — should not error
  expect_silent(migrate_offline_schema(con))
  expect_silent(migrate_offline_schema(con))

  # All columns should exist
  cols <- DBI::dbListFields(con, "species_traits")
  expect_true("RS" %in% cols)
  expect_true("ST" %in% cols)
  expect_true("imputation_method" %in% cols)
})

# =============================================================================
# 8. API key JSON round-trip
# =============================================================================
test_that("API keys saved as JSON can be loaded back", {
  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))

  keys <- list(algaebase_username = "testuser", algaebase_password = "testpass",
               freshwaterecology_key = "abc123")
  jsonlite::write_json(keys, tmp, auto_unbox = TRUE)

  loaded <- jsonlite::fromJSON(tmp)
  expect_equal(loaded$algaebase_username, "testuser")
  expect_equal(loaded$freshwaterecology_key, "abc123")
})

# =============================================================================
# 9. FS0 is excluded (primary producer)
# =============================================================================
test_that("FS0 primary producer has zero interaction probability as consumer", {
  consumer <- c(MS = "MS1", FS = "FS0", MB = "MB1", EP = "EP1")
  resource <- c(MS = "MS1", MB = "MB1", EP = "EP1", PR = "PR0")
  prob <- calc_interaction_probability(consumer, resource)
  expect_equal(prob, 0, info = "FS0 primary producers should not consume")
})

# =============================================================================
# 10. Trait codes in TRAIT_DEFINITIONS match matrix rows
# =============================================================================
test_that("all FS codes in FS_MS matrix match TRAIT_DEFINITIONS", {
  fs_in_matrix <- rownames(FS_MS)
  fs_in_defs <- names(TRAIT_DEFINITIONS$FS)
  # FS0 is excluded from matrix (primary producer)
  fs_expected <- setdiff(fs_in_defs, "FS0")
  expect_equal(sort(fs_in_matrix), sort(fs_expected),
               info = "FS_MS matrix rows should match TRAIT_DEFINITIONS (minus FS0)")
})

test_that("all PR codes in PR_MS matrix match TRAIT_DEFINITIONS", {
  pr_in_matrix <- rownames(PR_MS)
  pr_in_defs <- names(TRAIT_DEFINITIONS$PR)
  expect_equal(sort(pr_in_matrix), sort(pr_in_defs),
               info = "PR_MS matrix rows should match TRAIT_DEFINITIONS")
})
