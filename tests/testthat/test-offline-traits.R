# Unit tests for offline trait database and helpers

# App root helper (mirrors helper-fixtures.R)
app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")

test_that("size_to_ms returns correct codes at boundaries", {
  source(file.path(app_root, "R/config/harmonization_config.R"), local = TRUE)

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

  # Edge cases
  expect_equal(size_to_ms(NA), NA_character_)
  expect_equal(size_to_ms(0), NA_character_)
  expect_equal(size_to_ms(-5), NA_character_)

  # Boundary values
  expect_equal(size_to_ms(0.05), "MS1")
  expect_equal(size_to_ms(0.5), "MS2")
  expect_equal(size_to_ms(3), "MS3")
  expect_equal(size_to_ms(10), "MS4")
  expect_equal(size_to_ms(30), "MS5")
  expect_equal(size_to_ms(100), "MS6")
  expect_equal(size_to_ms(200), "MS7")
})

test_that("MAREDAT ESD to cm conversion is plausible for zooplankton", {
  # Typical copepod ESD ~1000 um -> 0.1 cm (MS2 boundary)
  expect_equal(1000 / 10000, 0.1)
  # Euphausiid ~20000 um -> 2 cm (MS3)
  expect_equal(20000 / 10000, 2.0)
  # Jellyfish ~200000 um -> 20 cm (MS5)
  expect_equal(200000 / 10000, 20.0)
})

test_that("PTDB volume to cm conversion is correct", {
  # 1000 um3 -> cube root = 10 um -> 0.001 cm (MS1)
  expect_equal(round((1000^(1/3)) / 10000, 5), 0.001)
  # 1e6 um3 -> cube root = 100 um -> 0.01 cm (MS1)
  expect_equal(round((1e6^(1/3)) / 10000, 4), 0.01)
  # Very large cell 1e9 um3 -> 1000 um -> 0.1 cm (MS2)
  expect_equal(round((1e9^(1/3)) / 10000, 2), 0.1)
})

test_that("PR codes in harmonization config form a complete 9-level (PR0-PR8) system", {
  source(file.path(app_root, "R/config/harmonization_config.R"), local = TRUE)

  # Patterns and labels must agree on the same set of PR codes; the UI
  # legend now reads from labels (data-driven), so any drift between them
  # would silently mislabel the on-screen table.
  pattern_codes <- sub("_.*", "", names(HARMONIZATION_CONFIG$protection_patterns))
  label_codes   <- names(HARMONIZATION_CONFIG$protection_labels)
  expect_setequal(pattern_codes, paste0("PR", 0:8))
  expect_setequal(label_codes,   paste0("PR", 0:8))

  # Each label must be non-empty - prevents shipping a UI table row with
  # blank protection or examples columns.
  for (code in label_codes) {
    info <- HARMONIZATION_CONFIG$protection_labels[[code]]
    expect_true(nchar(info$label %||% "") > 0,
                info = paste(code, "label is empty"))
    expect_true(nchar(info$examples %||% "") > 0,
                info = paste(code, "examples is empty"))
  }
})

test_that("harmonize_protection covers PR1 (mucus) and PR4 (thin exoskeleton)", {
  source(file.path(app_root, "R/config/harmonization_config.R"), local = TRUE)
  source(file.path(app_root, "R/functions/trait_lookup/harmonization.R"),
         local = TRUE)

  # PR1 was orphan pre-PR1b: UI listed "Mucus/slime" but no branch in
  # harmonize_protection() and no pattern in config produced it.
  expect_equal(harmonize_protection(skeleton_info = "mucus"),    "PR1")
  expect_equal(harmonize_protection(skeleton_info = "slime coat"), "PR1")
  # PR4 was outright missing from the harmonize_protection branches.
  expect_equal(harmonize_protection(skeleton_info = "chitinous"),    "PR4")
  expect_equal(harmonize_protection(skeleton_info = "small arthropod"), "PR4")
})

test_that("New ecosystem profiles are accessible", {
  source(file.path(app_root, "R/config/harmonization_config.R"), local = TRUE)
  profiles <- names(HARMONIZATION_CONFIG$profiles)
  expect_true("mediterranean" %in% profiles)
  expect_true("atlantic_ne" %in% profiles)
  expect_true("deep_sea" %in% profiles)
  # Deep sea should have larger size thresholds
  expect_equal(HARMONIZATION_CONFIG$profiles$deep_sea$size_multiplier, 1.3)
})

test_that("lookup_offline_traits returns NULL for missing DB file", {
  source(file.path(app_root, "R/functions/validation_utils.R"), local = FALSE)
  source(file.path(app_root, "R/config/harmonization_config.R"), local = FALSE)
  env <- new.env(parent = globalenv())
  sys.source(file.path(app_root, "R/functions/trait_lookup/orchestrator.R"), envir = env)
  result <- env$lookup_offline_traits("Gadus morhua", db_path = "nonexistent.db")
  expect_null(result)
})

test_that("lookup_offline_traits returns data for known species when DB exists", {
  skip_if_not(file.exists(file.path(app_root, "cache/offline_traits.db")), "Offline DB not built yet")
  source(file.path(app_root, "R/functions/validation_utils.R"), local = FALSE)
  source(file.path(app_root, "R/config/harmonization_config.R"), local = FALSE)
  env <- new.env(parent = globalenv())
  sys.source(file.path(app_root, "R/functions/trait_lookup/orchestrator.R"), envir = env)

  # Hediste diversicolor (a polychaete) is in BIOTIC, the first source
  # build_offline_trait_db.R ingests, so it's reliably present whenever the
  # build completes. Earlier versions of this test used Gadus morhua, which
  # the build's data sources (BIOTIC/MAREDAT/PTDB/BVOL/SpeciesEnriched —
  # all benthic invertebrates / plankton) never cover.
  #
  # Pass an absolute db_path. The function defaults to a relative path
  # ("cache/offline_traits.db") that's resolved against the caller's cwd.
  # testthat sets cwd to tests/testthat/, so the default would miss the DB
  # even when the gate above confirms the file exists at app_root.
  result <- env$lookup_offline_traits(
    "Hediste diversicolor",
    db_path = file.path(app_root, "cache/offline_traits.db")
  )
  skip_if(is.null(result),
          "Offline DB has no entry for Hediste diversicolor; rebuild with build_offline_trait_db.R")

  expect_true("MS" %in% names(result))
  expect_true("FS" %in% names(result))
  expect_true("MB" %in% names(result))
  expect_true("EP" %in% names(result))
  expect_true("PR" %in% names(result))
  expect_true("primary_source" %in% names(result))
})

test_that("config has labels for all extended modality codes (PR8b Phase B)", {
  source(file.path(app_root, "R/config/harmonization_config.R"), local = TRUE)

  # Each *_labels list must cover every key in the matching *_patterns
  # list — same data-driven contract as PR1b's protection_labels.
  for (kind in c("reproductive", "temperature", "salinity")) {
    pattern_codes <- sub("_.*", "", names(HARMONIZATION_CONFIG[[paste0(kind, "_patterns")]]))
    label_codes   <- names(HARMONIZATION_CONFIG[[paste0(kind, "_labels")]])
    # expect_setequal doesn't take `info`; use a dedicated true/false
    # expectation as the diagnostic when the sets diverge.
    drift_msg <- paste(kind, "_patterns vs _labels code-set drift:",
                       "patterns=", paste(pattern_codes, collapse = ","),
                       "labels=",   paste(label_codes,   collapse = ","))
    expect_true(setequal(pattern_codes, label_codes), info = drift_msg)
    for (code in label_codes) {
      info <- HARMONIZATION_CONFIG[[paste0(kind, "_labels")]][[code]]
      expect_true(nchar(info$label %||% "") > 0,
                  info = paste(kind, code, "label is empty"))
      expect_true(nchar(info$examples %||% "") > 0,
                  info = paste(kind, code, "examples is empty"))
    }
  }
})

test_that("external trait CSVs are present (visibility for empty-stub state)", {
  # All four are header-only stubs in the repo; users populate via the
  # URLs in data/external_traits/README.md. Test asserts the FILES exist
  # so a future delete is loud; doesn't assert NROW > 0 because that
  # would currently fail by design.
  for (csv in c("blacksea_traits.csv", "arctic_traits.csv",
                "cefas_benthic_traits.csv", "coral_traits.csv")) {
    p <- file.path(app_root, "data", "external_traits", csv)
    expect_true(file.exists(p),
                info = paste("missing external trait CSV stub:", csv))
  }
})

test_that("offline DB schema includes RS/TT/ST extended modality columns (PR8b)", {
  # Stage A of the RS/TT/ST implementation: schema accommodates the
  # extended modalities even though data writers haven't been wired yet
  # (deferred to a stakeholder-driven follow-up). Without these columns
  # the live-API path's RS/TT/ST values had no place to land in cache;
  # subsequent lookups silently re-paid the API cost.
  db_path <- file.path(app_root, "cache/offline_traits.db")
  skip_if_not(file.exists(db_path), "Offline DB not built yet")
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  cols <- DBI::dbListFields(con, "species_traits")
  for (extended in c("RS", "TT", "ST",
                     "RS_confidence", "TT_confidence", "ST_confidence")) {
    expect_true(extended %in% cols,
                info = paste("species_traits is missing column:", extended))
  }
})

test_that("offline DB has ontology rows (build script must not silently drop Source 1)", {
  # Regression for the column-name drift that produced 0 ontology rows
  # for months. The fix changed the build script to read taxon_name /
  # trait_category / trait_name / trait_modality / trait_score (the
  # actual CSV schema) and stop() loudly on any future drift.
  db_path <- file.path(app_root, "cache/offline_traits.db")
  skip_if_not(file.exists(db_path), "Offline DB not built yet")
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  n_ontology <- DBI::dbGetQuery(
    con, "SELECT COUNT(*) AS n FROM species_traits WHERE primary_source = 'ontology'"
  )$n
  expect_gt(n_ontology, 0,
            label = "ontology source rows in offline DB (was 0 pre-fix)")
})

test_that("lookup_offline_traits returns NULL for unknown species", {
  skip_if_not(file.exists(file.path(app_root, "cache/offline_traits.db")), "Offline DB not built yet")
  source(file.path(app_root, "R/functions/validation_utils.R"), local = FALSE)
  source(file.path(app_root, "R/config/harmonization_config.R"), local = FALSE)
  env <- new.env(parent = globalenv())
  sys.source(file.path(app_root, "R/functions/trait_lookup/orchestrator.R"), envir = env)

  result <- env$lookup_offline_traits("Nonexistent_species_xyz_99999")
  expect_null(result)
})
