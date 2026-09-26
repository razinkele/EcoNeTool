#!/usr/bin/env Rscript
# =============================================================================
# fix_metaweb_orientation.R - one-off data migration for PR A1
# =============================================================================
# Four bundled metawebs were produced out of tree with their interaction
# columns swapped: the value under `predator_id` is really the prey. Until A1,
# metaweb_to_igraph() built predator -> prey edges, which swapped them back, so
# the two errors cancelled. A1 makes metaweb_to_igraph() follow the edge
# contract (prey_id -> predator_id), so these files must now say what their
# column names mean.
#
# For each listed metaweb this script:
#   1. checks orientation from the columns alone: basal-named species
#      (detritus / producers) must appear only as prey_id;
#   2. if the CSV is inverted, swaps predator_id <-> prey_id and rewrites it;
#   3. rebuilds the .rds from the CSVs with import_metaweb_csv(), keeping the
#      old metadata and adding metadata$orientation_fixed.
# A file that already passes the check is never swapped again, so a second run
# is a no-op. Kongsfjorden (arctic/kongsfjorden_farage2021) is already correct
# and is deliberately not listed.
#
# Usage (from anywhere):  Rscript scripts/initialization/fix_metaweb_orientation.R
# =============================================================================

.find_app_root <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  start <- if (length(file_arg) > 0) dirname(sub("^--file=", "", file_arg[1])) else getwd()
  dir <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(dir, "app.R")) && dir.exists(file.path(dir, "R", "functions"))) {
      return(dir)
    }
    parent <- dirname(dir)
    if (identical(parent, dir)) stop("Cannot locate the EcoNeTool root (app.R + R/functions)")
    dir <- parent
  }
}

options(econetool.app_root = .find_app_root())
source(file.path(getOption("econetool.app_root"), "R", "functions", "validation_utils.R"))
source(app_path("R/functions/metaweb_core.R"))
source(app_path("R/functions/metaweb_io.R"))

SWAPPED_METAWEBS <- c(
  "baltic/baltic_kortsch2021",
  "arctic/barents_boreal_kortsch2015",
  "arctic/barents_arctic_kortsch2015",
  "atlantic/north_sea_frelat2022"
)
BASAL_PATTERN <- "detrit|phyto|diatom|autotroph|macroalgae|microalgae"
ORIENTATION_TAG <- "2026-09 (A1)"

#' TRUE when basal-named species occur only as prey (never as predator_id)
orientation_ok <- function(species, interactions) {
  basal_ids <- species$species_id[grepl(BASAL_PATTERN, species$species_name, ignore.case = TRUE)]
  if (length(basal_ids) == 0) {
    stop("no detritus/producer-named species; orientation cannot be checked")
  }
  !any(interactions$predator_id %in% basal_ids) && any(interactions$prey_id %in% basal_ids)
}

for (stem in SWAPPED_METAWEBS) {
  species_file <- app_path("metawebs", paste0(stem, "_species.csv"))
  interactions_file <- app_path("metawebs", paste0(stem, "_interactions.csv"))
  rds_file <- app_path("metawebs", paste0(stem, ".rds"))

  species <- read.csv(species_file, stringsAsFactors = FALSE)
  interactions <- read.csv(interactions_file, stringsAsFactors = FALSE)
  old <- readRDS(rds_file)

  csv_ok <- orientation_ok(species, interactions)
  rds_ok <- orientation_ok(old$species, old$interactions)
  if (csv_ok && rds_ok) {
    cat(sprintf("SKIP  %s: already prey/predator-correct, not touched\n", stem))
    next
  }

  if (!csv_ok) {
    fixed <- interactions
    fixed$predator_id <- interactions$prey_id
    fixed$prey_id <- interactions$predator_id
    if (!orientation_ok(species, fixed)) {
      stop(sprintf("%s: swapping the columns does not fix the orientation; inspect by hand", stem))
    }
    # Binary connection: keep LF line endings on Windows (repo is eol=lf).
    con <- file(interactions_file, open = "wb")
    write.csv(fixed, con, row.names = FALSE)
    close(con)
  }

  metadata <- old$metadata
  metadata$orientation_fixed <- ORIENTATION_TAG
  rebuilt <- import_metaweb_csv(species_file, interactions_file, metadata = metadata)
  saveRDS(rebuilt, rds_file)
  cat(sprintf("FIXED %s: %d interactions, csv %s, rds rebuilt\n",
              stem, nrow(rebuilt$interactions), if (csv_ok) "unchanged" else "swapped"))
}
