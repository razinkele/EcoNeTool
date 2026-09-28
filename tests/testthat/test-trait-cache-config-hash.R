# Spec B section 4.1 (F72): harmonized trait codes are cached per species in
# cache/taxonomy/<species>.rds and shared by every session, but they depend on
# the session's harmonization config. The envelope now carries the effective
# config hash, and a reader that passes its own hash treats a different (or
# missing) hash as a cache miss.

source_app_dependencies()

write_envelope <- function(path, hash) {
  envelope <- list(traits = data.frame(species = "Gadus morhua", MS = "MS6", stringsAsFactors = FALSE),
                   timestamp = Sys.time())
  if (!is.null(hash)) envelope$config_hash <- hash
  saveRDS(envelope, path)
}

test_that("harm_config_hash ignores last_modified and version", {
  b <- HARMONIZATION_CONFIG
  b$last_modified <- as.Date("2001-01-01")
  b$version <- "0.0.1"
  expect_identical(harm_config_hash(b), harm_config_hash(HARMONIZATION_CONFIG))
})

test_that("harm_config_hash changes when a threshold changes", {
  b <- HARMONIZATION_CONFIG
  b$size_thresholds$MS3_MS4 <- 6
  expect_false(identical(harm_config_hash(b), harm_config_hash(HARMONIZATION_CONFIG)))
})

test_that("harm_config_hash survives a JSON round-trip (integer/double drift)", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  export_config_json(tmp, HARMONIZATION_CONFIG)
  expect_identical(harm_config_hash(import_config_json(tmp)), harm_config_hash(HARMONIZATION_CONFIG))
})

test_that("outside Shiny the default hash is the process default's hash", {
  expect_identical(harm_config_hash(), harm_config_hash(HARMONIZATION_CONFIG))
  expect_identical(harm_default_config_hash(), harm_config_hash(HARMONIZATION_CONFIG))
  expect_type(harm_config_hash(), "character")
  expect_length(harm_config_hash(), 1L)
})

test_that("read_cache_field treats a different or missing config hash as a miss", {
  f <- tempfile(fileext = ".rds")
  on.exit(unlink(f), add = TRUE)

  write_envelope(f, "abc")
  expect_null(read_cache_field(f, "traits", config_hash = "x"))
  expect_equal(read_cache_field(f, "traits", config_hash = "abc")$MS, "MS6")
  expect_equal(read_cache_field(f, "traits")$MS, "MS6") # no hash asked: unchanged behaviour

  write_envelope(f, NULL) # envelope written before B1
  expect_null(read_cache_field(f, "traits", config_hash = "abc"))
})

test_that("two sessions with different MS3_MS4 do not share cached harmonized codes", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("withr")
  root <- get_app_root()
  source(file.path(root, "R/functions/admin_auth.R"), local = FALSE)
  source(file.path(root, "R/ui/harmonization_settings_ui.R"), local = FALSE)
  source(file.path(root, "R/modules/harmonization_settings_server.R"), local = FALSE)

  old_file <- get("HARMONIZATION_CONFIG_FILE", envir = globalenv())
  old_worms <- get("lookup_worms_traits", envir = globalenv())
  assign("HARMONIZATION_CONFIG_FILE", file.path(tempdir(), "no_such_harm_cfg.json"), envir = globalenv())
  # A cache miss falls through to the live WoRMS lookup first; make it loud.
  assign("lookup_worms_traits", function(...) stop("CACHE MISS: live lookup reached"), envir = globalenv())
  withr::defer({
    assign("HARMONIZATION_CONFIG_FILE", old_file, envir = globalenv())
    assign("lookup_worms_traits", old_worms, envir = globalenv())
  })

  cache_dir <- tempfile("taxonomy_")
  dir.create(cache_dir)
  withr::defer(unlink(cache_dir, recursive = TRUE))
  species <- "Gadus morhua"

  # Session A (default settings) owns the cached row.
  shiny::testServer(harmonization_settings_server, {
    write_envelope(file.path(cache_dir, "Gadus_morhua.rds"), harm_config_hash())
    got <- suppressMessages(lookup_species_traits(species, cache_dir = cache_dir))
    expect_equal(got$MS, "MS6")
  })

  # Session B moved MS3/MS4: its codes can differ, so A's row must not be served.
  shiny::testServer(harmonization_settings_server, {
    session$setInputs(
      harm_thresh_MS1_MS2 = 0.1, harm_thresh_MS2_MS3 = 1.0,
      harm_thresh_MS3_MS4 = 9, harm_thresh_MS4_MS5 = 20.0,
      harm_thresh_MS5_MS6 = 50.0, harm_thresh_MS6_MS7 = 150.0
    )
    expect_error(suppressMessages(lookup_species_traits(species, cache_dir = cache_dir)),
                 "CACHE MISS")
  })
})

test_that("every trait-cache writer stamps the hash and every reader passes one", {
  orch <- readLines(app_path("R/functions/trait_lookup/orchestrator.R"), warn = FALSE)
  orch <- paste(orch[!startsWith(trimws(orch), "#")], collapse = "\n")
  expect_true(grepl('read_cache_field(cache_file, "traits", config_hash = harm_config_hash())', orch, fixed = TRUE))
  expect_true(grepl("config_hash = harm_default_config_hash()", orch, fixed = TRUE))
  # One reader plus the full-pipeline writer (cache_data <- list(...)).
  n_session_hash <- lengths(regmatches(orch, gregexpr("config_hash = harm_config_hash()", orch, fixed = TRUE)))
  expect_gte(n_session_hash, 2L)
  # apply_phylogenetic_imputation() reads every envelope in cache_dir too (F72
  # fix round 1): the orchestrator's call must pass along the session hash.
  expect_true(grepl("apply_phylogenetic_imputation(", orch, fixed = TRUE))
  phylo_call_start <- regexpr("apply_phylogenetic_imputation(", orch, fixed = TRUE)
  phylo_call_tail <- substr(orch, phylo_call_start, phylo_call_start + 600)
  expect_true(grepl("config_hash = harm_config_hash()", phylo_call_tail, fixed = TRUE))

  srv <- readLines(app_path("R/modules/trait_research_server.R"), warn = FALSE)
  srv <- srv[!startsWith(trimws(srv), "#")]
  expect_false(any(grepl("readRDS(cache_file)", srv, fixed = TRUE)))
  expect_true(any(grepl('read_cache_field(cache_file, "traits", config_hash = cfg_hash)', srv, fixed = TRUE)))

  par <- readLines(app_path("R/functions/parallel_lookup.R"), warn = FALSE)
  expect_true(any(grepl('read_cache_field(cache_file, "traits", config_hash = cfg_hash)', par, fixed = TRUE)))

  phylo <- readLines(app_path("R/functions/phylogenetic_imputation.R"), warn = FALSE)
  phylo <- paste(phylo[!startsWith(trimws(phylo), "#")], collapse = "\n")
  expect_true(grepl("find_closest_relatives <- function(", phylo, fixed = TRUE))
  expect_true(grepl("config_hash = NULL", phylo, fixed = TRUE))
})

test_that("find_closest_relatives skips envelopes from a different config_hash", {
  source(file.path(get_app_root(), "R/functions/phylogenetic_imputation.R"), local = FALSE)
  cache_dir <- tempfile("phylo_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  target_taxonomy <- list(phylum = "Chordata", class = "Actinopterygii",
                           order = "Gadiformes", family = "Gadidae", genus = "Gadus")
  target_traits <- list(MS = NA, FS = "FS1", MB = "MB1", EP = "EP1", PR = "PR1")

  write_relative <- function(file, species, ms, hash) {
    saveRDS(list(
      species = species,
      traits = list(MS = ms, FS = NA, MB = NA, EP = NA, PR = NA),
      worms_taxonomy = target_taxonomy, # same genus -> distance 0
      timestamp = Sys.time(),
      config_hash = hash
    ), file.path(cache_dir, file))
  }
  write_relative("Relative_A.rds", "Relative A", "MS3", "A")
  write_relative("Relative_B.rds", "Relative B", "MS6", "B")

  with_hash <- find_closest_relatives(target_taxonomy, target_traits, cache_dir, config_hash = "A")
  expect_equal(nrow(with_hash), 1L)
  expect_equal(with_hash$MS, "MS3")

  without_hash <- find_closest_relatives(target_taxonomy, target_traits, cache_dir, config_hash = NULL)
  expect_equal(nrow(without_hash), 2L) # old behaviour: unkeyed, both considered
})

test_that("apply_phylogenetic_imputation only imputes from the matching config_hash", {
  source(file.path(get_app_root(), "R/functions/phylogenetic_imputation.R"), local = FALSE)
  cache_dir <- tempfile("phylo_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  target_taxonomy <- list(phylum = "Chordata", class = "Actinopterygii",
                           order = "Gadiformes", family = "Gadidae", genus = "Gadus")
  current_traits <- list(MS = NA, FS = "FS1", MB = "MB1", EP = "EP1", PR = "PR1")

  saveRDS(list(species = "Relative A", traits = list(MS = "MS3", FS = NA, MB = NA, EP = NA, PR = NA),
               worms_taxonomy = target_taxonomy, timestamp = Sys.time(), config_hash = "A"),
          file.path(cache_dir, "Relative_A.rds"))

  imputed <- apply_phylogenetic_imputation(
    species_name = "Gadus morhua", current_traits = current_traits, taxonomy = target_taxonomy,
    cache_dir = cache_dir, config_hash = "A"
  )
  expect_equal(imputed$MS, "MS3")

  imputed_wrong_hash <- apply_phylogenetic_imputation(
    species_name = "Gadus morhua", current_traits = current_traits, taxonomy = target_taxonomy,
    cache_dir = cache_dir, config_hash = "B"
  )
  expect_true(is.na(imputed_wrong_hash$MS)) # relative filtered out: no relatives, MS stays NA
})
