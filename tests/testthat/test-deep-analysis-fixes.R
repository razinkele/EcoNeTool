# =============================================================================
# Regression tests for the 2026-07-17 deep-analysis critical fixes
# =============================================================================
# Covers:
#   #7  FS0 primary-producer regex must not code consumers as autotrophs
#   #3  batch trait combine must union path-dependent columns (bind_rows)
# =============================================================================

source_app_dependencies()

# parse_ecopath_data (#6) needs the DEFAULT_* constants from R/config.R and the
# ecopath CSV parser, neither of which source_app_dependencies() loads.
local({
  root <- get_app_root()
  tryCatch(
    source(file.path(root, "R/config.R"), local = FALSE),
    error = function(e) message("Note: R/config.R side effects skipped: ", conditionMessage(e))
  )
  source(file.path(root, "R/functions/ecopath/ecopath_csv.R"), local = FALSE)
})

# -----------------------------------------------------------------------------
# #7 - Foraging strategy: diet nouns must not force FS0 (primary producer)
# -----------------------------------------------------------------------------

test_that("a predator whose diet mentions algae is not coded as a primary producer", {
  fs <- harmonize_foraging_strategy(
    feeding_info = "predator, feeds on algae and diatoms",
    trophic_level = 3.5
  )
  expect_false(fs == "FS0",
               info = "A TL 3.5 predator must never be FS0 just because its diet lists algae")
  expect_equal(fs, "FS1", info = "'predator' text should resolve to FS1")
})

test_that("a herbivore grazing on algae is a grazer, not a primary producer", {
  fs <- harmonize_foraging_strategy(
    feeding_info = "herbivore feeding on algae",
    trophic_level = 2.0
  )
  expect_false(fs == "FS0", info = "A herbivore is a consumer, not an autotroph")
  expect_equal(fs, "FS4", info = "'herbivore' should resolve to FS4 grazer")
})

test_that("a true primary producer is still coded FS0", {
  # Producer-specific vocabulary must continue to map to FS0.
  expect_equal(
    harmonize_foraging_strategy(feeding_info = "photosynthesis", trophic_level = 1.0),
    "FS0"
  )
  expect_equal(
    harmonize_foraging_strategy(feeding_info = "autotroph", trophic_level = 1.0),
    "FS0"
  )
  expect_equal(
    harmonize_foraging_strategy(feeding_info = "primary_producer", trophic_level = 1.0),
    "FS0"
  )
})

# -----------------------------------------------------------------------------
# #3 - Combining per-species results with divergent column sets
# -----------------------------------------------------------------------------

test_that("combine_trait_results unions divergent columns with NA fill", {
  # Mimics the real bug: an offline-complete species returns a narrow frame,
  # a full-pipeline species returns extra confidence/imputation columns.
  offline_only <- data.frame(
    species = "Species A", MS = "MS3", FS = "FS1",
    stringsAsFactors = FALSE
  )
  full_path <- data.frame(
    species = "Species B", MS = "MS2", FS = "FS4",
    overall_confidence = 0.82, imputation_method = "rf_predicted",
    stringsAsFactors = FALSE
  )

  combined <- combine_trait_results(list(offline_only, full_path))

  expect_equal(nrow(combined), 2)
  expect_true(all(c("species", "MS", "FS", "overall_confidence") %in% names(combined)))
  # Row from the narrow frame gets NA for the column it never had.
  expect_true(is.na(combined$overall_confidence[combined$species == "Species A"]))
  expect_equal(combined$overall_confidence[combined$species == "Species B"], 0.82)
})

test_that("combine_trait_results drops NULL entries from failed lookups", {
  a <- data.frame(species = "Species A", MS = "MS3", stringsAsFactors = FALSE)
  combined <- combine_trait_results(list(a, NULL))
  expect_equal(nrow(combined), 1)
  expect_equal(combined$species, "Species A")
})

# -----------------------------------------------------------------------------
# #6 - ecopath_csv: a blank group name must not misalign biomass/PB/QB
# -----------------------------------------------------------------------------

test_that("parse_ecopath_data keeps biomass aligned when a group name is blank", {
  basic <- tempfile(fileext = ".csv")
  diet <- tempfile(fileext = ".csv")
  on.exit(unlink(c(basic, diet)), add = TRUE)

  # A blank group-name row between Cod and Herring - routine in hand-edited
  # ECOPATH exports. Pre-fix, dropping it shrank species_names but the summary
  # mask (built from the shrunk vector) was recycled against the full frame,
  # so biomass zipped to the wrong species.
  writeLines(c(
    "Group,Biomass,PB,QB",
    "Cod,10,0.5,3",
    ",999,0.1,1",
    "Herring,20,1.0,5",
    "Sprat,30,1.2,6"
  ), basic)

  writeLines(c(
    "Prey,Cod,Herring,Sprat",
    "Cod,0,0,0",
    "Herring,0.5,0,0",
    "Sprat,0.3,0.6,0"
  ), diet)

  res <- parse_ecopath_data(basic, diet)

  expect_equal(res$info["Cod", "meanB"], 10)
  expect_equal(res$info["Herring", "meanB"], 20)
  expect_equal(res$info["Sprat", "meanB"], 30)
  # The blank row's biomass must never leak into a real species.
  expect_false(999 %in% res$info$meanB)
})

# -----------------------------------------------------------------------------
# #5 - Rpath diet-matrix cell edit must write to the edited prey row (no +1)
# -----------------------------------------------------------------------------

local({
  root <- get_app_root()
  source(file.path(root, "R/functions/rpath/rpath_conversion.R"), local = FALSE)
})

make_diet <- function() {
  data.table::data.table(
    Group = c("Phytoplankton", "Zooplankton", "Cod"),
    Cod = c(0.0, 0.0, 0.0),
    Zooplankton = c(0.0, 0.0, 0.0)
  )
}

test_that("apply_diet_cell_edit writes to the edited prey row, not the next", {
  # DT _cell_edit row is already 1-based; col is 0-based (rownames = FALSE).
  # Edit prey row 2 (Zooplankton), predator column 'Cod' (col index 1).
  res <- apply_diet_cell_edit(make_diet(), list(row = 2L, col = 1L, value = "0.7"))

  expect_equal(res$status, "ok")
  expect_equal(res$diet[["Cod"]][2], 0.7)  # the row the user edited
  expect_equal(res$diet[["Cod"]][1], 0.0)  # untouched
  expect_equal(res$diet[["Cod"]][3], 0.0)  # NOT shifted onto the next prey
})

test_that("apply_diet_cell_edit on the last prey row does not grow the column", {
  res <- apply_diet_cell_edit(make_diet(), list(row = 3L, col = 1L, value = "0.5"))

  expect_equal(res$status, "ok")
  expect_equal(nrow(res$diet), 3)          # no ragged data.table growth
  expect_equal(res$diet[["Cod"]][3], 0.5)
})

test_that("apply_diet_cell_edit leaves the Group (prey-name) column unedited", {
  res <- apply_diet_cell_edit(make_diet(), list(row = 1L, col = 0L, value = "Renamed"))
  expect_equal(res$status, "skip")
  expect_equal(res$diet[["Group"]][1], "Phytoplankton")
})

test_that("apply_diet_cell_edit rejects out-of-range diet proportions", {
  res <- apply_diet_cell_edit(make_diet(), list(row = 1L, col = 1L, value = "1.5"))
  expect_equal(res$status, "invalid")
  expect_equal(res$diet[["Cod"]][1], 0.0)  # unchanged
})

# -----------------------------------------------------------------------------
# #4 - cache envelope shape validation (traits vs data collision)
# -----------------------------------------------------------------------------

write_cache <- function(envelope) {
  f <- tempfile(fileext = ".rds")
  saveRDS(envelope, f)
  f
}

test_that("read_cache_field returns the field from a fresh matching envelope", {
  f <- write_cache(list(traits = data.frame(MS = "MS3"), timestamp = Sys.time()))
  on.exit(unlink(f), add = TRUE)
  got <- read_cache_field(f, "traits")
  expect_true(is.data.frame(got))
  expect_equal(got$MS, "MS3")
})

test_that("read_cache_field returns NULL for a foreign envelope (the #4 collision)", {
  # classify_species_api writes {data,...}; the orchestrator asks for 'traits'.
  # Pre-fix it dereferenced cached$traits = NULL and corrupted the row.
  f <- write_cache(list(data = list(source = "api"), timestamp = Sys.time()))
  on.exit(unlink(f), add = TRUE)
  expect_null(read_cache_field(f, "traits"))
})

test_that("read_cache_field returns NULL for a stale envelope", {
  f <- write_cache(list(traits = data.frame(MS = "MS3"),
                        timestamp = Sys.time() - as.difftime(40, units = "days")))
  on.exit(unlink(f), add = TRUE)
  expect_null(read_cache_field(f, "traits"))
})

test_that("read_cache_field returns NULL for a missing file", {
  expect_null(read_cache_field(tempfile(fileext = ".rds"), "traits"))
})

test_that("read_cache_field returns NULL for a file with no timestamp", {
  f <- write_cache(list(traits = data.frame(MS = "MS3")))
  on.exit(unlink(f), add = TRUE)
  expect_null(read_cache_field(f, "traits"))
})
