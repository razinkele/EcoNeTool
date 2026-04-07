#!/usr/bin/env Rscript
# =============================================================================
# prefetch_fishbase_traits.R
# Pre-downloads FishBase traits for common European marine fish species
# and stores them in the offline trait database for instant lookups.
#
# Usage:  Rscript scripts/initialization/prefetch_fishbase_traits.R
#
# After running: trait lookups for these species will be instant (0ms)
# instead of ~30s per species via FishBase API.
# =============================================================================

cat("=== EcoNeTool FishBase Trait Pre-Fetcher ===\n")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ---------------------------------------------------------------------------
# 0. Resolve project root
# ---------------------------------------------------------------------------
script_dir <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile)),
  error = function(e) getwd()
)
project_root <- normalizePath(file.path(script_dir, "..", ".."), mustWork = FALSE)
if (!file.exists(file.path(project_root, "R", "config.R"))) {
  project_root <- getwd()
}
cat("Project root:", project_root, "\n")
setwd(project_root)

# ---------------------------------------------------------------------------
# 1. Source required files
# ---------------------------------------------------------------------------
source("R/config.R")
source("R/functions/validation_utils.R")
source("R/functions/functional_group_utils.R")
source("R/config/harmonization_config.R")
source("R/functions/trait_lookup/harmonization.R")
source("R/functions/trait_lookup/database_lookups.R")
source("R/functions/trait_lookup/csv_trait_databases.R")
source("R/functions/trait_lookup/api_trait_databases.R")
source("R/functions/trait_lookup/orchestrator.R")
source("R/functions/ml_trait_prediction.R")

cat("All modules loaded.\n\n")

# ---------------------------------------------------------------------------
# 2. Define common European marine fish species
# ---------------------------------------------------------------------------

# Read from test datasets
baltic_species <- tryCatch({
  df <- read.csv("data/baltic_sea_test_species.csv", stringsAsFactors = FALSE)
  df$species[df$functional_group == "Fish"]
}, error = function(e) character(0))

european_species <- tryCatch({
  df <- read.csv("data/european_marine_test_species.csv", stringsAsFactors = FALSE)
  df$species[df$functional_group == "Fish"]
}, error = function(e) character(0))

# Additional common European marine fish not in test datasets
additional_fish <- c(
  # North Sea / Atlantic commercial fish
  "Pollachius virens",         # Saithe
  "Pollachius pollachius",     # Pollack
  "Molva molva",               # Ling
  "Lophius piscatorius",       # Anglerfish/Monkfish
  "Scophthalmus maximus",      # Turbot
  "Scophthalmus rhombus",      # Brill
  "Microstomus kitt",          # Lemon sole
  "Hippoglossus hippoglossus", # Atlantic halibut
  "Dicentrarchus labrax",      # European sea bass
  "Mullus barbatus",           # Red mullet
  "Mullus surmuletus",         # Striped red mullet
  "Trisopterus luscus",        # Bib/Pouting
  "Trisopterus esmarkii",      # Norway pout

  # Mediterranean fish
  "Sparus aurata",             # Gilt-head seabream
  "Diplodus sargus",           # White seabream
  "Diplodus vulgaris",         # Common two-banded seabream
  "Boops boops",               # Bogue
  "Oblada melanura",           # Saddled seabream
  "Pagellus erythrinus",       # Common pandora
  "Pagrus pagrus",             # Red porgy
  "Dentex dentex",             # Common dentex
  "Epinephelus marginatus",    # Dusky grouper
  "Serranus scriba",           # Painted comber
  "Scorpaena porcus",          # Black scorpionfish
  "Coris julis",               # Mediterranean rainbow wrasse
  "Symphodus tinca",           # East Atlantic peacock wrasse
  "Gobius niger",              # Black goby

  # Baltic additional
  "Zoarces viviparus",         # Eelpout
  "Cyclopterus lumpus",        # Lumpfish
  "Myoxocephalus quadricornis", # Fourhorn sculpin

  # Deep-sea / Atlantic
  "Sebastes mentella",         # Deepwater redfish
  "Reinhardtius hippoglossoides", # Greenland halibut
  "Argentina silus",           # Greater argentine
  "Micromesistius poutassou",  # Blue whiting
  "Sardina pilchardus",        # European pilchard (may already be in list)
  "Engraulis encrasicolus",    # European anchovy
  "Belone belone",             # Garfish
  "Atherina presbyter",        # Sand smelt
  "Trachinus draco",           # Greater weever

  # Common forage fish
  "Ammodytes tobianus",        # Small sandeel
  "Hyperoplus lanceolatus",    # Great sandeel
  "Sprattus sprattus",         # Sprat (may already be in list)
  "Osmerus eperlanus",         # Smelt (may already be in list)

  # Sharks and rays (European waters)
  "Squalus acanthias",         # Spurdog
  "Scyliorhinus canicula",     # Smallspotted catshark
  "Mustelus mustelus",         # Smooth-hound
  "Raja clavata",              # Thornback ray
  "Raja montagui"              # Spotted ray
)

# Combine and deduplicate
all_fish <- unique(c(baltic_species, european_species, additional_fish))

# Clean names (remove subspecies markers, trim whitespace)
all_fish <- trimws(all_fish)
all_fish <- all_fish[nchar(all_fish) > 3]

cat("Total fish species to pre-fetch:", length(all_fish), "\n\n")

# ---------------------------------------------------------------------------
# 3. Setup offline database
# ---------------------------------------------------------------------------
cache_dir <- file.path(project_root, "cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

db_path <- file.path(cache_dir, "offline_traits.db")

# Check if DB exists; if not, run build_offline_trait_db.R first
if (!file.exists(db_path)) {
  cat("Offline DB does not exist. Creating fresh database...\n")

  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("RSQLite required. Install: install.packages('RSQLite')")
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS species_traits (
      id              INTEGER PRIMARY KEY AUTOINCREMENT,
      species         TEXT UNIQUE NOT NULL,
      aphia_id        INTEGER,
      functional_group TEXT,
      MS TEXT, FS TEXT, MB TEXT, EP TEXT, PR TEXT,
      RS TEXT, TT TEXT, ST TEXT,
      trophic_level REAL, depth_min REAL, depth_max REAL,
      is_hab INTEGER, longevity_years REAL, growth_rate TEXT,
      body_shape TEXT, phyto_motility TEXT, phyto_growth_form TEXT,
      MS_confidence REAL, FS_confidence REAL, MB_confidence REAL,
      EP_confidence REAL, PR_confidence REAL,
      RS_confidence REAL, TT_confidence REAL, ST_confidence REAL,
      primary_source TEXT, imputation_method TEXT DEFAULT 'observed',
      region TEXT, notes TEXT
    )
  ")

  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS metadata (key TEXT PRIMARY KEY, value TEXT)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_species ON species_traits(species)")
  DBI::dbExecute(con, "INSERT OR REPLACE INTO metadata VALUES ('build_timestamp', ?)",
                 params = list(format(Sys.time(), "%Y-%m-%dT%H:%M:%S")))
  DBI::dbExecute(con, "INSERT OR REPLACE INTO metadata VALUES ('version', '2.0')")

  DBI::dbDisconnect(con)
  cat("Fresh database created.\n\n")
}

# ---------------------------------------------------------------------------
# 4. Pre-fetch traits for each species via orchestrator
# ---------------------------------------------------------------------------

cat("Starting pre-fetch...\n")
cat("Each species takes ~30-40s (FishBase API). Total estimated: ",
    round(length(all_fish) * 35 / 60), " minutes.\n\n")

success_count <- 0
fail_count <- 0
skip_count <- 0

# Check which species are already in the DB
con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
existing <- DBI::dbGetQuery(con, "SELECT species FROM species_traits")$species
DBI::dbDisconnect(con)

already_cached <- all_fish[all_fish %in% existing]
to_fetch <- all_fish[!all_fish %in% existing]

cat("Already in offline DB:", length(already_cached), "\n")
cat("Need to fetch:", length(to_fetch), "\n\n")

if (length(to_fetch) == 0) {
  cat("All species already in offline DB. Nothing to do.\n")
  quit(save = "no", status = 0)
}

for (i in seq_along(to_fetch)) {
  species <- to_fetch[i]
  cat(sprintf("[%d/%d] %s...", i, length(to_fetch), species))

  tryCatch({
    # Use the full orchestrator (WoRMS + FishBase + harmonization)
    result <- lookup_species_traits(species, cache_dir = file.path(cache_dir, "taxonomy"))

    if (!is.null(result) && !all(is.na(result[, c("MS", "FS", "MB", "EP", "PR")]))) {
      # Insert into offline DB
      con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

      DBI::dbExecute(con, "
        INSERT OR REPLACE INTO species_traits
          (species, MS, FS, MB, EP, PR, RS, TT, ST,
           trophic_level, depth_min, depth_max, body_shape,
           MS_confidence, FS_confidence, MB_confidence, EP_confidence, PR_confidence,
           primary_source, imputation_method)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      ", params = list(
        species,
        result$MS, result$FS, result$MB, result$EP, result$PR,
        result$RS %||% NA, result$TT %||% NA, result$ST %||% NA,
        result$trophic_level %||% NA, result$depth_min %||% NA,
        result$depth_max %||% NA, result$body_shape %||% NA,
        result$MS_confidence %||% NA, result$FS_confidence %||% NA,
        result$MB_confidence %||% NA, result$EP_confidence %||% NA,
        result$PR_confidence %||% NA,
        result$source %||% "FishBase", result$imputation_method %||% "observed"
      ))

      DBI::dbDisconnect(con)
      success_count <- success_count + 1
      cat(" OK (", result$MS, "/", result$FS, "/", result$MB, "/", result$EP, "/", result$PR, ")\n")
    } else {
      fail_count <- fail_count + 1
      cat(" NO DATA\n")
    }

  }, error = function(e) {
    fail_count <<- fail_count + 1
    cat(" ERROR:", e$message, "\n")
  })

  # Small delay between API calls
  Sys.sleep(0.2)
}

# Update build timestamp
con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
DBI::dbExecute(con, "INSERT OR REPLACE INTO metadata VALUES ('build_timestamp', ?)",
               params = list(format(Sys.time(), "%Y-%m-%dT%H:%M:%S")))
DBI::dbExecute(con, "INSERT OR REPLACE INTO metadata VALUES ('prefetch_fish_count', ?)",
               params = list(as.character(success_count)))
DBI::dbDisconnect(con)

cat("\n=== PRE-FETCH COMPLETE ===\n")
cat("Success:", success_count, "\n")
cat("Failed:", fail_count, "\n")
cat("Skipped (already cached):", length(already_cached), "\n")
cat("Total in offline DB:", length(already_cached) + success_count, "\n")
cat("Finished:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
