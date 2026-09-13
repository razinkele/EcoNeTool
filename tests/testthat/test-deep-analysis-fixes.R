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

# -----------------------------------------------------------------------------
# #9 - habitat load bbox must cover the whole study area, not a 1-degree box
# -----------------------------------------------------------------------------

test_that("habitat_load_bbox returns the full study extent, not a centred 1-deg box", {
  # A 5x4 degree study area (Baltic proper). Pre-fix it was truncated to a
  # center +/-0.5 deg (~1 deg) box, laundering ~90% of the seabed into 'No data'.
  study <- c(18, 54, 23, 58)  # xmin, ymin, xmax, ymax
  got <- habitat_load_bbox(study, buffer_deg = 0)
  expect_equal(unname(got), c(18, 54, 23, 58))
})

test_that("habitat_load_bbox applies the edge buffer", {
  got <- habitat_load_bbox(c(18, 54, 23, 58), buffer_deg = 0.1)
  expect_equal(unname(got), c(17.9, 53.9, 23.1, 58.1))
})

test_that("habitat_load_bbox warns and caps only for absurdly large areas", {
  # A 50-degree span exceeds the sanity cap; must warn (not silently truncate).
  expect_warning(
    got <- habitat_load_bbox(c(-20, 20, 30, 70), buffer_deg = 0, max_span_deg = 30),
    "exceed"
  )
  # capped span is at most max_span_deg
  expect_lte(got[3] - got[1], 30 + 1e-9)
  expect_lte(got[4] - got[2], 30 + 1e-9)
})

test_that("habitat_load_bbox does not warn for a normal multi-degree area", {
  expect_silent(habitat_load_bbox(c(18, 54, 23, 58), buffer_deg = 0.05))
})

# =============================================================================
# app_path(): resolve repo-relative paths independently of the working directory
# =============================================================================
# Regression: orchestrator.R and network_visualization.R sourced
# "R/functions/uncertainty_quantification.R" with a wd-relative path, so
# uncertainty quantification was silently skipped whenever wd != repo root
# (e.g. under testthat, where wd is tests/testthat/).

test_that("app_path resolves repo files when wd is not the repo root", {
  # Precondition: these tests run with wd = tests/testthat, not the repo root.
  skip_if(file.exists("R/functions/uncertainty_quantification.R"),
          "wd is already the repo root; this test needs a non-root wd to be meaningful")

  expect_true(file.exists(app_path("R/functions/uncertainty_quantification.R")))
})

test_that("app_path returns the repo root when called with no arguments", {
  expect_true(file.exists(file.path(app_path(), "app.R")))
  expect_true(file.exists(file.path(app_path(), "VERSION")))
})

test_that("uncertainty quantification functions load from a non-root wd", {
  skip_if(file.exists("R/functions/uncertainty_quantification.R"),
          "wd is already the repo root; this test needs a non-root wd to be meaningful")

  env <- new.env(parent = globalenv())
  source(app_path("R/functions/uncertainty_quantification.R"), local = env)

  expect_true(exists("calculate_all_trait_confidence", envir = env, inherits = FALSE))
  expect_true(exists("map_confidence_to_size", envir = env, inherits = FALSE))
})

test_that("runtime source() of uncertainty_quantification.R is wd-independent", {
  # Guards the regression directly: a bare relative path here resolves against
  # the caller's wd, so the source() fails and the feature degrades silently.
  needle <- 'source("R/functions/uncertainty_quantification.R"'
  offenders <- character(0)

  for (rel in c("R/functions/trait_lookup/orchestrator.R",
                "R/functions/network_visualization.R")) {
    lines <- readLines(app_path(rel), warn = FALSE)
    hits <- which(grepl(needle, lines, fixed = TRUE))
    if (length(hits) > 0) {
      offenders <- c(offenders, sprintf("%s:%s", rel, paste(hits, collapse = ",")))
    }
  }

  expect_equal(offenders, character(0),
               label = "files using a wd-relative source() for uncertainty_quantification.R")
})

test_that("no runtime source() in R/ uses a wd-relative literal path", {
  # load_all.R files are startup-only: app.R sources them from the repo root.
  # Everything else may run at any wd, so a bare relative path is a silent
  # failure waiting to happen. config.R is no longer excluded - its API key
  # paths now resolve through app_path(); see test-api-key-paths.R.
  files <- list.files(app_path("R"), pattern = "[.]R$",
                      recursive = TRUE, full.names = TRUE)
  files <- files[!grepl("load_all[.]R$", files)]

  offenders <- character(0)
  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    code <- lines[!startsWith(trimws(lines), "#")]
    idx <- which(grepl('source("', code, fixed = TRUE))
    for (i in idx) {
      if (!grepl("app_path(", code[i], fixed = TRUE)) {
        offenders <- c(offenders, sprintf("%s: %s",
                                          basename(f), trimws(code[i])))
      }
    }
  }

  expect_equal(offenders, character(0),
               label = "bare wd-relative source() calls in R/")
})

test_that("parallel_lookup resolves its worker function files via app_path", {
  lines <- readLines(app_path("R/functions/parallel_lookup.R"), warn = FALSE)
  expect_true(any(grepl("app_path(func_file)", lines, fixed = TRUE)),
              info = "parallel_lookup must resolve required_functions via app_path()")
})

test_that("trait_lookup/load_all.R is self-sufficient in a fresh session", {
  # load_all.R is the documented entry point. Because orchestrator.R calls
  # app_path() in top-level code, load_all.R must pull in validation_utils.R
  # itself rather than relying on the caller having done so.
  rscript <- file.path(R.home("bin"), "Rscript.exe")
  if (!file.exists(rscript)) rscript <- file.path(R.home("bin"), "Rscript")
  skip_if(!file.exists(rscript), "Rscript not found; cannot spawn a fresh session")

  root <- app_path()
  script <- tempfile(fileext = ".R")
  on.exit(unlink(script), add = TRUE)
  writeLines(c(
    'setwd(commandArgs(trailingOnly = TRUE)[1])',
    'suppressMessages(suppressWarnings(source("R/functions/trait_lookup/load_all.R")))',
    'cat("LOADED_OK
")'
  ), script)

  out <- suppressWarnings(
    system2(rscript, c(shQuote(script), shQuote(root)),
            stdout = TRUE, stderr = TRUE)
  )

  expect_true(any(grepl("LOADED_OK", out, fixed = TRUE)),
              info = paste(utils::tail(out, 6), collapse = " | "))
})

# -----------------------------------------------------------------------------
# #29 - ecopath_csv: the fg factor must carry all seven functional groups
# -----------------------------------------------------------------------------
# The factor levels were hardcoded to five, so a model containing seabirds or
# marine mammals - routine in Baltic and Arctic food webs - silently coerced
# those rows' fg to NA while every other column stayed populated.

test_that("parse_ecopath_data keeps Birds and Mammals in the fg factor", {
  basic <- tempfile(fileext = ".csv")
  diet <- tempfile(fileext = ".csv")
  on.exit(unlink(c(basic, diet)), add = TRUE)

  writeLines(c(
    "Group,Biomass,PB,QB",
    "Cod,10,0.5,3",
    "Grey seal,0.5,0.1,10",
    "Common gull,0.2,0.2,80",
    "Detritus,100,0,0"
  ), basic)

  writeLines(c(
    "Prey,Cod,Grey seal,Common gull,Detritus",
    "Cod,0,0.6,0.3,0",
    "Grey seal,0,0,0,0",
    "Common gull,0,0,0,0",
    "Detritus,0,0,0,0"
  ), diet)

  res <- parse_ecopath_data(basic, diet)

  expect_false(any(is.na(res$info$fg)),
               info = paste("NA fg for:",
                            paste(rownames(res$info)[is.na(res$info$fg)],
                                  collapse = ", ")))
  expect_equal(as.character(res$info["Grey seal", "fg"]), "Mammals")
  expect_equal(as.character(res$info["Common gull", "fg"]), "Birds")
})

test_that("the fg factor uses the canonical seven-level set", {
  basic <- tempfile(fileext = ".csv")
  diet <- tempfile(fileext = ".csv")
  on.exit(unlink(c(basic, diet)), add = TRUE)

  writeLines(c(
    "Group,Biomass,PB,QB",
    "Cod,10,0.5,3",
    "Herring,20,1.0,5",
    "Detritus,100,0,0"
  ), basic)
  writeLines(c(
    "Prey,Cod,Herring,Detritus",
    "Cod,0,0,0",
    "Herring,0.5,0,0",
    "Detritus,0,0.4,0"
  ), diet)

  res <- parse_ecopath_data(basic, diet)

  expect_equal(levels(res$info$fg), get_functional_group_levels())
})

# -----------------------------------------------------------------------------
# #20 - ecopath_windows: the RODBC channel must close on every path
# -----------------------------------------------------------------------------
# The handle was closed only on the success path. Any stop() between connect
# and close - an unreadable table, a missing column - leaked the ODBC channel
# for the life of the R process, and shiny-server reuses that process.

test_that("the RODBC connection is released by on.exit, not only on success", {
  src <- readLines(app_path("R/functions/ecopath/ecopath_windows.R"), warn = FALSE)
  code <- src[!startsWith(trimws(src), "#")]

  connect_at <- which(grepl("odbcConnectAccess2007(", code, fixed = TRUE))
  skip_if(length(connect_at) == 0, "no RODBC connect call found")

  # An on.exit registration must follow the connect, before any other work,
  # so an error between the two cannot strand the handle.
  window <- code[connect_at[1]:min(connect_at[1] + 20L, length(code))]
  expect_true(any(grepl("on.exit(", window, fixed = TRUE)),
              info = "no on.exit() registered within 20 lines of odbcConnectAccess2007()")
  expect_true(any(grepl("odbcClose", window, fixed = TRUE)),
              info = "the on.exit near the connect does not close the channel")
})

# -----------------------------------------------------------------------------
# #15 - RS/TT/ST: an unmappable later source must not erase a resolved code
# -----------------------------------------------------------------------------
# The extended modalities were assigned whenever the RAW field was non-NULL,
# with no check that the harmoniser actually resolved it. Sources run in a
# fixed order (BlackSea -> Arctic -> Cefas -> Coral -> WoRMS -> PolyTraits),
# so a later source carrying free text the harmoniser cannot map returns NA
# and overwrites a good code from an earlier source - while *_source is
# rewritten to the later source, so the result claims provenance it lacks.

test_that("assign_trait_if_resolved keeps the existing value when the new one is NA", {
  result <- list(RS = "RS1", RS_source = "BlackSea")
  out <- assign_trait_if_resolved(result, "RS", NA_character_, "PolyTraits")

  expect_equal(out$RS, "RS1")
  expect_equal(out$RS_source, "BlackSea",
               info = "provenance must not be rewritten by a source that resolved nothing")
})

test_that("assign_trait_if_resolved writes a resolved value and its source", {
  result <- list(RS = NA_character_, RS_source = NA_character_)
  out <- assign_trait_if_resolved(result, "RS", "RS2", "Cefas")

  expect_equal(out$RS, "RS2")
  expect_equal(out$RS_source, "Cefas")
})

test_that("assign_trait_if_resolved lets a later resolved value win", {
  # Last-writer-wins among sources that actually resolve is the existing
  # semantic for these traits; only NA writes are suppressed.
  result <- list(TT = "TT2", TT_source = "BlackSea")
  out <- assign_trait_if_resolved(result, "TT", "TT4", "ArcticTraits")

  expect_equal(out$TT, "TT4")
  expect_equal(out$TT_source, "ArcticTraits")
})

test_that("assign_trait_if_resolved treats empty and zero-length values as unresolved", {
  result <- list(ST = "ST3", ST_source = "BlackSea")

  expect_equal(assign_trait_if_resolved(result, "ST", NULL, "X")$ST, "ST3")
  expect_equal(assign_trait_if_resolved(result, "ST", character(0), "X")$ST, "ST3")
  expect_equal(assign_trait_if_resolved(result, "ST", NA, "X")$ST_source, "BlackSea")
})

test_that("no RS/TT/ST assignment in orchestrator.R bypasses the guard", {
  code <- readLines(app_path("R/functions/trait_lookup/orchestrator.R"), warn = FALSE)
  code <- code[!startsWith(trimws(code), "#")]

  # A bare `result$RS <- harmonize_...` writes whatever the harmoniser returned,
  # NA included. Every such site must go through assign_trait_if_resolved().
  offenders <- grep("result[$](RS|TT|ST) *<- *harmonize_", code, value = TRUE)

  expect_equal(length(offenders), 0L,
               label = paste("unguarded RS/TT/ST assignments:",
                             paste(trimws(offenders), collapse = " | ")))
})

# -----------------------------------------------------------------------------
# #14 - MS must not be wiped when it came from the offline DB
# -----------------------------------------------------------------------------
# The inner branch (size_cm present) is guarded by offline_prefilled; the outer
# else (no size data) was not, so it set MS to NA while MS_source stayed
# "OfflineDB" - a row claiming an offline provenance for a missing value.

test_that("the no-size branch does not clear an offline-prefilled MS", {
  code <- readLines(app_path("R/functions/trait_lookup/orchestrator.R"), warn = FALSE)
  hit <- grep("result[$]MS <- NA_character_", code)
  skip_if(length(hit) == 0, "MS reset line not found; orchestrator restructured")

  # Walk back to the nearest enclosing guard and require it to consult
  # offline_prefilled.
  window <- code[max(1L, hit[1] - 6L):hit[1]]
  expect_true(any(grepl("offline_prefilled", window, fixed = TRUE)),
              info = paste("unguarded MS reset at line", hit[1]))
})
