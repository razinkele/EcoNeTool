#!/usr/bin/env Rscript
# =============================================================================
# build_offline_trait_db.R
# Pre-computes harmonized trait codes (MS, FS, MB, EP, PR) for all species
# from local data sources and saves to cache/offline_traits.db (SQLite).
#
# Usage:  Rscript scripts/initialization/build_offline_trait_db.R
# =============================================================================

cat("=== EcoNeTool Offline Trait Database Builder ===\n")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ---------------------------------------------------------------------------
# 0. Resolve project root (works from any working directory)
# ---------------------------------------------------------------------------
script_dir <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile)),
  error = function(e) getwd()
)
project_root <- normalizePath(file.path(script_dir, "..", ".."), mustWork = FALSE)
if (!file.exists(file.path(project_root, "R", "config", "harmonization_config.R"))) {
  # Fallback: try current working directory as project root

  project_root <- getwd()
}
cat("Project root:", project_root, "\n")

# ---------------------------------------------------------------------------
# 1. Source configuration and utilities
# ---------------------------------------------------------------------------
source(file.path(project_root, "R", "config", "harmonization_config.R"))
source(file.path(project_root, "R", "functions", "validation_utils.R"))

# Verify %||% is available
stopifnot("operator %||% not available" = exists("%||%", mode = "function"))
cat("Sourced harmonization_config.R and validation_utils.R\n")

# ---------------------------------------------------------------------------
# 2. Load required packages
# ---------------------------------------------------------------------------
required_pkgs <- c("DBI", "RSQLite")
optional_pkgs <- c("readxl")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Required package '", pkg, "' is not installed. ",
         "Install with: install.packages('", pkg, "')", call. = FALSE)
  }
}

has_readxl <- requireNamespace("readxl", quietly = TRUE)
if (!has_readxl) {
  warning("Package 'readxl' not installed -- Excel sources (BVOL, SpeciesEnriched) will be skipped.")
}

library(DBI)
library(RSQLite)

# ---------------------------------------------------------------------------
# 3. Helper functions
# ---------------------------------------------------------------------------

size_to_ms <- function(size_cm) {
  if (is.null(size_cm) || is.na(size_cm) || size_cm <= 0) return(NA_character_)
  thresholds <- HARMONIZATION_CONFIG$size_thresholds
  if (size_cm < thresholds$MS1_MS2) return("MS1")
  if (size_cm < thresholds$MS2_MS3) return("MS2")
  if (size_cm < thresholds$MS3_MS4) return("MS3")
  if (size_cm < thresholds$MS4_MS5) return("MS4")
  if (size_cm < thresholds$MS5_MS6) return("MS5")
  if (size_cm < thresholds$MS6_MS7) return("MS6")
  return("MS7")
}

safe_grep_col <- function(pattern, col_names) {
  matches <- grep(pattern, col_names, ignore.case = TRUE, value = TRUE)
  if (length(matches) > 0) matches[1] else NULL
}

safe_insert <- function(con, sql, params) {
  tryCatch(dbExecute(con, sql, params = params), error = function(e) {
    warning("DB insert failed for '", params[[1]], "': ", e$message)
    0
  })
}

safe_max_trait <- function(scores) {
  if (length(scores) == 0 || all(is.na(scores))) return(NA_real_)
  max(scores, na.rm = TRUE)
}

feeding_to_fs <- function(mode) {
  if (is.na(mode) || is.null(mode) || nchar(trimws(mode)) == 0) return(NA_character_)
  mode_lower <- tolower(trimws(mode))
  for (fs_name in names(HARMONIZATION_CONFIG$foraging_patterns)) {
    if (grepl(HARMONIZATION_CONFIG$foraging_patterns[[fs_name]], mode_lower))
      return(sub("_.*", "", fs_name))
  }
  warning("Unrecognized feeding mode: '", mode, "'")
  NA_character_
}

mobility_to_mb <- function(mode) {
  if (is.na(mode) || is.null(mode) || nchar(trimws(mode)) == 0) return(NA_character_)
  mode_lower <- tolower(trimws(mode))
  for (mb_name in names(HARMONIZATION_CONFIG$mobility_patterns)) {
    if (grepl(HARMONIZATION_CONFIG$mobility_patterns[[mb_name]], mode_lower))
      return(sub("_.*", "", mb_name))
  }
  warning("Unrecognized mobility mode: '", mode, "'")
  NA_character_
}

protection_to_pr <- function(mode) {
  if (is.na(mode) || is.null(mode) || nchar(trimws(mode)) == 0) return(NA_character_)
  mode_lower <- tolower(trimws(mode))
  for (pr_name in names(HARMONIZATION_CONFIG$protection_patterns)) {
    if (grepl(HARMONIZATION_CONFIG$protection_patterns[[pr_name]], mode_lower))
      return(sub("_.*", "", pr_name))
  }
  warning("Unrecognized protection mode: '", mode, "'")
  NA_character_
}

# ---------------------------------------------------------------------------
# 4. Create cache/ directory and database
# ---------------------------------------------------------------------------
cache_dir <- file.path(project_root, "cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

db_path <- file.path(cache_dir, "offline_traits.db")
cat("Database path:", db_path, "\n")

# Remove old database if it exists (fresh build)
if (file.exists(db_path)) {
  file.remove(db_path)
  cat("Removed existing database for fresh build.\n")
}

con <- dbConnect(RSQLite::SQLite(), db_path)
on.exit(dbDisconnect(con), add = TRUE)

# ---------------------------------------------------------------------------
# 5. Create schema
# ---------------------------------------------------------------------------
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS species_traits (
    id              INTEGER PRIMARY KEY AUTOINCREMENT,
    species         TEXT UNIQUE NOT NULL,
    aphia_id        INTEGER,
    functional_group TEXT,
    MS              TEXT,
    FS              TEXT,
    MB              TEXT,
    EP              TEXT,
    PR              TEXT,
    MS_confidence   REAL DEFAULT 0.0,
    FS_confidence   REAL DEFAULT 0.0,
    MB_confidence   REAL DEFAULT 0.0,
    EP_confidence   REAL DEFAULT 0.0,
    PR_confidence   REAL DEFAULT 0.0,
    primary_source  TEXT,
    region          TEXT,
    notes           TEXT
  )
")

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS metadata (
    key   TEXT PRIMARY KEY,
    value TEXT
  )
")

dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_species ON species_traits(species)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_aphia   ON species_traits(aphia_id)")

# Write metadata
dbExecute(con, "INSERT INTO metadata (key, value) VALUES ('build_timestamp', ?)",
          params = list(format(Sys.time(), "%Y-%m-%dT%H:%M:%S")))
dbExecute(con, "INSERT INTO metadata (key, value) VALUES ('version', ?)",
          params = list(HARMONIZATION_CONFIG$version %||% "1.0.0"))

cat("Schema created.\n\n")

# ---------------------------------------------------------------------------
# SQL template for inserts
# ---------------------------------------------------------------------------
INSERT_SQL <- "
  INSERT OR IGNORE INTO species_traits
    (species, aphia_id, functional_group,
     MS, FS, MB, EP, PR,
     MS_confidence, FS_confidence, MB_confidence, EP_confidence, PR_confidence,
     primary_source, region, notes)
  VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
"

REPLACE_SQL <- "
  INSERT OR REPLACE INTO species_traits
    (species, aphia_id, functional_group,
     MS, FS, MB, EP, PR,
     MS_confidence, FS_confidence, MB_confidence, EP_confidence, PR_confidence,
     primary_source, region, notes)
  VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
"

# Counters
source_counts <- list()

# ===========================================================================
# SOURCE 1: Ontology (highest priority -- INSERT OR REPLACE)
# ===========================================================================
cat("--- Source 1/6: Ontology ---\n")
ontology_path <- file.path(project_root, "data", "ontology_traits.csv")
if (file.exists(ontology_path)) {
  ont <- tryCatch(read.csv(ontology_path, stringsAsFactors = FALSE),
                  error = function(e) { warning("Failed to read ontology: ", e$message); NULL })
  if (!is.null(ont) && nrow(ont) > 0) {
    required_cols <- c("species", "trait_type", "trait_value")
    if (all(required_cols %in% tolower(names(ont)))) {
      names(ont) <- tolower(names(ont))
      score_col <- safe_grep_col("score|confidence", names(ont))
      species_list <- unique(ont$species)
      n_inserted <- 0
      for (sp in species_list) {
        sp_data <- ont[ont$species == sp, , drop = FALSE]
        if (nrow(sp_data) == 0) next

        # Extract traits by type
        get_best <- function(type_pattern) {
          rows <- sp_data[grepl(type_pattern, sp_data$trait_type, ignore.case = TRUE), , drop = FALSE]
          if (nrow(rows) == 0) return(list(value = NA_character_, score = 0.0))
          if (!is.null(score_col) && score_col %in% names(rows)) {
            scores <- suppressWarnings(as.numeric(rows[[score_col]]))
            best_score <- safe_max_trait(scores)
            if (!is.na(best_score)) {
              best_row <- rows[which.max(scores), , drop = FALSE]
              return(list(value = best_row$trait_value[1], score = best_score))
            }
          }
          return(list(value = rows$trait_value[1], score = 0.5))
        }

        feeding  <- get_best("feed|forag|trophic")
        mobility <- get_best("mobil|locomot|movement")
        zone     <- get_best("zone|position|habitat|environment")
        armor    <- get_best("protect|armor|shell|skeleton")
        size_rec <- get_best("size|length|morpholog")

        ms_val <- NA_character_; ms_conf <- 0.0
        if (!is.na(size_rec$value)) {
          size_num <- suppressWarnings(as.numeric(size_rec$value))
          if (!is.na(size_num) && size_num > 0) {
            ms_val  <- size_to_ms(size_num)
            ms_conf <- size_rec$score
          }
        }

        fs_val  <- feeding_to_fs(feeding$value %||% NA_character_)
        mb_val  <- mobility_to_mb(mobility$value %||% NA_character_)
        ep_val  <- if (!is.na(zone$value)) {
          mode_lower <- tolower(trimws(zone$value))
          for (ep_name in names(HARMONIZATION_CONFIG$environmental_patterns)) {
            if (grepl(HARMONIZATION_CONFIG$environmental_patterns[[ep_name]], mode_lower)) {
              break
            }
            ep_name <- NA_character_
          }
          if (!is.na(ep_name) && nchar(ep_name) > 0) sub("_.*", "", ep_name) else NA_character_
        } else NA_character_
        pr_val  <- protection_to_pr(armor$value %||% NA_character_)

        aphia_col <- safe_grep_col("aphia", names(sp_data))
        aphia_id  <- if (!is.null(aphia_col)) suppressWarnings(as.integer(sp_data[[aphia_col]][1])) else NA_integer_
        fg_col    <- safe_grep_col("functional.?group|group", names(sp_data))
        fg        <- if (!is.null(fg_col)) sp_data[[fg_col]][1] else NA_character_

        params <- list(sp, aphia_id, fg,
                       ms_val, fs_val, mb_val, ep_val, pr_val,
                       ms_conf, feeding$score, mobility$score, zone$score, armor$score,
                       "ontology", NA_character_, NA_character_)
        safe_insert(con, REPLACE_SQL, params)
        n_inserted <- n_inserted + 1
      }
      source_counts[["ontology"]] <- n_inserted
      cat("  Inserted:", n_inserted, "species\n")
    } else {
      cat("  Skipped: missing required columns (need: species, trait_type, trait_value)\n")
    }
  } else {
    cat("  Skipped: empty or unreadable file\n")
  }
} else {
  cat("  Skipped: file not found\n")
}

# ===========================================================================
# SOURCE 2: BIOTIC
# ===========================================================================
cat("--- Source 2/6: BIOTIC ---\n")
biotic_path <- file.path(project_root, "data", "biotic_traits.csv")
if (file.exists(biotic_path)) {
  biotic <- tryCatch(read.csv(biotic_path, stringsAsFactors = FALSE),
                     error = function(e) { warning("Failed to read BIOTIC: ", e$message); NULL })
  if (!is.null(biotic) && nrow(biotic) > 0) {
    required_cols <- c("Species", "Max_Length_mm", "Feeding_mode", "Mobility", "Skeleton")
    col_check <- sapply(required_cols, function(rc) {
      !is.null(safe_grep_col(paste0("^", rc, "$"), names(biotic)))
    })
    # Case-insensitive column matching
    names(biotic) <- tolower(names(biotic))
    required_lower <- tolower(required_cols)
    missing_cols <- required_lower[!required_lower %in% names(biotic)]

    if (length(missing_cols) == 0) {
      n_inserted <- 0
      for (i in seq_len(nrow(biotic))) {
        sp <- trimws(biotic$species[i])
        if (is.na(sp) || nchar(sp) == 0) next

        size_mm <- suppressWarnings(as.numeric(biotic$max_length_mm[i]))
        size_cm <- if (!is.na(size_mm)) size_mm / 10 else NA_real_
        ms_val  <- size_to_ms(size_cm)
        fs_val  <- feeding_to_fs(biotic$feeding_mode[i] %||% NA_character_)
        mb_val  <- mobility_to_mb(biotic$mobility[i] %||% NA_character_)
        pr_val  <- protection_to_pr(biotic$skeleton[i] %||% NA_character_)

        # EP from Living_habit if available
        ep_val <- NA_character_
        habit_col <- safe_grep_col("living.?habit", names(biotic))
        if (!is.null(habit_col)) {
          habit <- tolower(trimws(biotic[[habit_col]][i] %||% ""))
          if (grepl("burrow", habit)) {
            ep_val <- "EP3"
          } else if (grepl("tube|attached|free", habit)) {
            ep_val <- "EP2"
          }
        }

        conf <- if (!is.na(ms_val)) 0.7 else 0.0
        params <- list(sp, NA_integer_, NA_character_,
                       ms_val, fs_val, mb_val, ep_val, pr_val,
                       conf, 0.7, 0.7, 0.5, 0.7,
                       "biotic", NA_character_, NA_character_)
        res <- safe_insert(con, INSERT_SQL, params)
        if (res > 0) n_inserted <- n_inserted + 1
      }
      source_counts[["biotic"]] <- n_inserted
      cat("  Inserted:", n_inserted, "species (skipped existing)\n")
    } else {
      cat("  Skipped: missing columns:", paste(missing_cols, collapse = ", "), "\n")
    }
  } else {
    cat("  Skipped: empty or unreadable file\n")
  }
} else {
  cat("  Skipped: file not found\n")
}

# ===========================================================================
# SOURCE 3: MAREDAT
# ===========================================================================
cat("--- Source 3/6: MAREDAT ---\n")
maredat_path <- file.path(project_root, "data", "maredat_zooplankton.csv")
if (file.exists(maredat_path)) {
  maredat <- tryCatch(read.csv(maredat_path, stringsAsFactors = FALSE),
                      error = function(e) { warning("Failed to read MAREDAT: ", e$message); NULL })
  if (!is.null(maredat) && nrow(maredat) > 0) {
    names(maredat) <- tolower(names(maredat))
    required_lower <- c("species", "esd_um", "group", "trophic_category")
    missing_cols <- required_lower[!required_lower %in% names(maredat)]

    if (length(missing_cols) == 0) {
      n_inserted <- 0
      for (i in seq_len(nrow(maredat))) {
        sp <- trimws(maredat$species[i])
        if (is.na(sp) || nchar(sp) == 0) next

        esd_um  <- suppressWarnings(as.numeric(maredat$esd_um[i]))
        size_cm <- if (!is.na(esd_um)) esd_um / 10000 else NA_real_
        ms_val  <- size_to_ms(size_cm)

        # FS from Trophic_category
        trophic <- tolower(trimws(maredat$trophic_category[i] %||% ""))
        fs_val <- if (grepl("herbivor", trophic)) "FS4"
                  else if (grepl("carnivor", trophic)) "FS1"
                  else if (grepl("omnivor", trophic)) "FS3"
                  else if (grepl("detritivor", trophic)) "FS5"
                  else "FS6"

        # MB from Group
        grp <- tolower(trimws(maredat$group[i] %||% ""))
        mb_val <- if (grepl("copepod|euphausiac|chaetognath", grp)) "MB5"
                  else if (grepl("cladocer|appendicular", grp)) "MB4"
                  else if (grepl("cnidari|ctenophor", grp)) "MB2"
                  else NA_character_

        ep_val <- "EP1"  # pelagic

        # PR
        pr_val <- if (grepl("euphausiac|ostracod", grp)) "PR5" else "PR0"

        conf <- if (!is.na(ms_val)) 0.6 else 0.0
        params <- list(sp, NA_integer_, grp,
                       ms_val, fs_val, mb_val, ep_val, pr_val,
                       conf, 0.6, 0.5, 0.8, 0.4,
                       "maredat", NA_character_, NA_character_)
        res <- safe_insert(con, INSERT_SQL, params)
        if (res > 0) n_inserted <- n_inserted + 1
      }
      source_counts[["maredat"]] <- n_inserted
      cat("  Inserted:", n_inserted, "species (skipped existing)\n")
    } else {
      cat("  Skipped: missing columns:", paste(missing_cols, collapse = ", "), "\n")
    }
  } else {
    cat("  Skipped: empty or unreadable file\n")
  }
} else {
  cat("  Skipped: file not found\n")
}

# ===========================================================================
# SOURCE 4: PTDB (phytoplankton)
# ===========================================================================
cat("--- Source 4/6: PTDB ---\n")
ptdb_path <- file.path(project_root, "data", "ptdb_phytoplankton.csv")
if (file.exists(ptdb_path)) {
  ptdb <- tryCatch(read.csv(ptdb_path, stringsAsFactors = FALSE),
                   error = function(e) { warning("Failed to read PTDB: ", e$message); NULL })
  if (!is.null(ptdb) && nrow(ptdb) > 0) {
    names(ptdb) <- tolower(names(ptdb))
    required_lower <- c("species", "cell_volume_um3", "trophic_strategy")
    missing_cols <- required_lower[!required_lower %in% names(ptdb)]

    if (length(missing_cols) == 0) {
      n_inserted <- 0
      for (i in seq_len(nrow(ptdb))) {
        sp <- trimws(ptdb$species[i])
        if (is.na(sp) || nchar(sp) == 0) next

        # MS: from cell volume (cube root, then um to cm)
        vol <- suppressWarnings(as.numeric(ptdb$cell_volume_um3[i]))
        size_cm <- NA_real_
        if (!is.na(vol) && vol > 0) {
          size_cm <- (vol^(1/3)) / 10000
        } else {
          # Fallback to cell_length_um
          len_col <- safe_grep_col("cell_length_um", names(ptdb))
          if (!is.null(len_col)) {
            len_um <- suppressWarnings(as.numeric(ptdb[[len_col]][i]))
            if (!is.na(len_um) && len_um > 0) {
              size_cm <- len_um / 10000
            }
          }
        }
        ms_val <- size_to_ms(size_cm)  # NA if size_cm still NA

        # FS from trophic_strategy
        strategy <- tolower(trimws(ptdb$trophic_strategy[i] %||% ""))
        fs_val <- if (grepl("autotroph", strategy)) "FS0"
                  else if (grepl("mixotroph", strategy)) "FS0"
                  else if (grepl("heterotroph", strategy)) "FS1"
                  else NA_character_

        # MB from motility
        motile_col <- safe_grep_col("motil", names(ptdb))
        mb_val <- NA_character_
        if (!is.null(motile_col)) {
          motility <- tolower(trimws(ptdb[[motile_col]][i] %||% ""))
          mb_val <- if (grepl("motile|yes|true", motility) && !grepl("non", motility)) "MB4"
                    else "MB2"
        } else {
          mb_val <- "MB2"
        }

        ep_val <- "EP1"
        pr_val <- "PR0"

        conf <- if (!is.na(ms_val)) 0.5 else 0.0
        params <- list(sp, NA_integer_, "phytoplankton",
                       ms_val, fs_val, mb_val, ep_val, pr_val,
                       conf, 0.6, 0.4, 0.9, 0.3,
                       "ptdb", NA_character_, NA_character_)
        res <- safe_insert(con, INSERT_SQL, params)
        if (res > 0) n_inserted <- n_inserted + 1
      }
      source_counts[["ptdb"]] <- n_inserted
      cat("  Inserted:", n_inserted, "species (skipped existing)\n")
    } else {
      cat("  Skipped: missing columns:", paste(missing_cols, collapse = ", "), "\n")
    }
  } else {
    cat("  Skipped: empty or unreadable file\n")
  }
} else {
  cat("  Skipped: file not found\n")
}

# ===========================================================================
# SOURCE 5: BVOL (Excel)
# ===========================================================================
cat("--- Source 5/6: BVOL ---\n")
bvol_path <- file.path(project_root, "data", "bvol_nomp_version_2024.xlsx")
if (file.exists(bvol_path) && has_readxl) {
  bvol <- tryCatch(
    readxl::read_excel(bvol_path),
    error = function(e) {
      warning("Failed to read BVOL Excel file: ", e$message)
      NULL
    }
  )
  if (!is.null(bvol) && nrow(bvol) > 0) {
    col_names <- names(bvol)
    sp_col  <- safe_grep_col("species|taxon|name", col_names)
    vol_col <- safe_grep_col("volume|biovolume|vol", col_names)
    tro_col <- safe_grep_col("troph|trophy", col_names)

    if (!is.null(sp_col) && !is.null(vol_col)) {
      # Validate volume column is numeric
      vol_numeric <- suppressWarnings(as.numeric(bvol[[vol_col]]))
      if (all(is.na(vol_numeric))) {
        cat("  Skipped: volume column '", vol_col, "' is not numeric\n")
      } else {
        n_inserted <- 0
        # Aggregate by species: median volume
        sp_list <- unique(bvol[[sp_col]])
        for (sp in sp_list) {
          if (is.na(sp) || nchar(trimws(sp)) == 0) next
          sp_clean <- trimws(sp)
          sp_rows <- which(bvol[[sp_col]] == sp)
          vols <- suppressWarnings(as.numeric(bvol[[vol_col]][sp_rows]))
          med_vol <- median(vols, na.rm = TRUE)

          size_cm <- if (!is.na(med_vol) && med_vol > 0) (med_vol^(1/3)) / 10000 else NA_real_
          ms_val <- size_to_ms(size_cm)

          fs_val <- NA_character_
          if (!is.null(tro_col)) {
            tro_val <- bvol[[tro_col]][sp_rows[1]]
            if (!is.na(tro_val) && nchar(trimws(tro_val)) > 0) {
              fs_val <- feeding_to_fs(tro_val)
            }
          }

          mb_val <- "MB2"
          ep_val <- "EP1"
          pr_val <- "PR0"

          conf <- if (!is.na(ms_val)) 0.4 else 0.0
          params <- list(sp_clean, NA_integer_, "phytoplankton",
                         ms_val, fs_val, mb_val, ep_val, pr_val,
                         conf, 0.3, 0.3, 0.8, 0.2,
                         "bvol", NA_character_, NA_character_)
          res <- safe_insert(con, INSERT_SQL, params)
          if (res > 0) n_inserted <- n_inserted + 1
        }
        source_counts[["bvol"]] <- n_inserted
        cat("  Inserted:", n_inserted, "species (skipped existing)\n")
      }
    } else {
      cat("  Skipped: could not find species or volume columns\n")
    }
  } else {
    cat("  Skipped: empty or unreadable file\n")
  }
} else if (!file.exists(bvol_path)) {
  cat("  Skipped: file not found\n")
} else {
  cat("  Skipped: readxl not installed\n")
}

# ===========================================================================
# SOURCE 6: SpeciesEnriched (Excel)
# ===========================================================================
cat("--- Source 6/6: SpeciesEnriched ---\n")
enriched_path <- file.path(project_root, "data", "species_enriched.xlsx")
if (file.exists(enriched_path) && has_readxl) {
  enriched <- tryCatch(
    readxl::read_excel(enriched_path),
    error = function(e) {
      warning("Failed to read SpeciesEnriched Excel file: ", e$message)
      NULL
    }
  )
  if (!is.null(enriched) && nrow(enriched) > 0) {
    col_names <- names(enriched)
    sp_col <- safe_grep_col("species|taxon|scientific.?name", col_names)

    if (is.null(sp_col)) {
      cat("  Skipped: could not find species/taxonomy column (no fallback)\n")
    } else {
      mob_col  <- safe_grep_col("mobil", col_names)
      feed_col <- safe_grep_col("feed|forag|trophic", col_names)
      ep_col   <- safe_grep_col("environment|position|zone|habitat", col_names)
      size_col <- safe_grep_col("size|length|mass", col_names)

      n_inserted <- 0
      for (i in seq_len(nrow(enriched))) {
        sp <- trimws(enriched[[sp_col]][i])
        if (is.na(sp) || nchar(sp) == 0) next

        ms_val <- NA_character_; ms_conf <- 0.0
        if (!is.null(size_col)) {
          sz <- suppressWarnings(as.numeric(enriched[[size_col]][i]))
          if (!is.na(sz) && sz > 0) {
            ms_val  <- size_to_ms(sz)
            ms_conf <- 0.3
          }
        }

        fs_val <- NA_character_
        if (!is.null(feed_col)) {
          fs_val <- feeding_to_fs(enriched[[feed_col]][i] %||% NA_character_)
        }

        mb_val <- NA_character_
        if (!is.null(mob_col)) {
          mb_val <- mobility_to_mb(enriched[[mob_col]][i] %||% NA_character_)
        }

        ep_val <- NA_character_
        if (!is.null(ep_col)) {
          ep_raw <- enriched[[ep_col]][i] %||% NA_character_
          if (!is.na(ep_raw) && nchar(trimws(ep_raw)) > 0) {
            ep_lower <- tolower(trimws(ep_raw))
            ep_val <- NA_character_
            for (ep_name in names(HARMONIZATION_CONFIG$environmental_patterns)) {
              if (grepl(HARMONIZATION_CONFIG$environmental_patterns[[ep_name]], ep_lower)) {
                ep_val <- sub("_.*", "", ep_name)
                break
              }
            }
          }
        }

        pr_val <- NA_character_

        params <- list(sp, NA_integer_, NA_character_,
                       ms_val, fs_val, mb_val, ep_val, pr_val,
                       ms_conf, 0.3, 0.3, 0.3, 0.0,
                       "species_enriched", NA_character_, NA_character_)
        res <- safe_insert(con, INSERT_SQL, params)
        if (res > 0) n_inserted <- n_inserted + 1
      }
      source_counts[["species_enriched"]] <- n_inserted
      cat("  Inserted:", n_inserted, "species (skipped existing)\n")
    }
  } else {
    cat("  Skipped: empty or unreadable file\n")
  }
} else if (!file.exists(enriched_path)) {
  cat("  Skipped: file not found\n")
} else {
  cat("  Skipped: readxl not installed\n")
}

# ===========================================================================
# Summary
# ===========================================================================
cat("\n=== Build Summary ===\n")
total <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM species_traits")$n
cat("Total species in database:", total, "\n")
cat("Species per source:\n")
for (src in names(source_counts)) {
  cat(sprintf("  %-20s %d\n", src, source_counts[[src]]))
}

# Trait coverage
coverage <- dbGetQuery(con, "
  SELECT
    SUM(CASE WHEN MS IS NOT NULL THEN 1 ELSE 0 END) AS has_MS,
    SUM(CASE WHEN FS IS NOT NULL THEN 1 ELSE 0 END) AS has_FS,
    SUM(CASE WHEN MB IS NOT NULL THEN 1 ELSE 0 END) AS has_MB,
    SUM(CASE WHEN EP IS NOT NULL THEN 1 ELSE 0 END) AS has_EP,
    SUM(CASE WHEN PR IS NOT NULL THEN 1 ELSE 0 END) AS has_PR,
    COUNT(*) AS total
  FROM species_traits
")
if (coverage$total > 0) {
  cat("\nTrait coverage:\n")
  for (trait in c("MS", "FS", "MB", "EP", "PR")) {
    pct <- round(100 * coverage[[paste0("has_", trait)]] / coverage$total, 1)
    cat(sprintf("  %s: %d/%d (%.1f%%)\n", trait,
                coverage[[paste0("has_", trait)]], coverage$total, pct))
  }
}

cat("\nDatabase saved to:", db_path, "\n")
cat("Finished:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
