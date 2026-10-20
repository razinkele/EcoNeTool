# Spatial tab session state (deep-analysis fix D1: F57, F58, F60, F61).
# Pure helpers in R/functions/spatial_state.R; no Shiny, no EUSeaMap GDB.

skip_if_not_installed("sf")
app_root <- get_app_root()
source(file.path(app_root, "R/functions/validation_utils.R"), local = FALSE)
source(file.path(app_root, "R/functions/euseamap_regional_config.R"), local = FALSE)
source(file.path(app_root, "R/functions/spatial_state.R"), local = FALSE)

square_sf <- function(xmin, ymin, xmax, ymax, ...) {
  poly <- sf::st_polygon(list(matrix(c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin),
                                     ncol = 2, byrow = TRUE)))
  sf::st_sf(data.frame(...), geometry = sf::st_sfc(poly, crs = 4326))
}

# ---------------------------------------------------------------------------
# D1.1 invalidation DAG (F58)
# ---------------------------------------------------------------------------

test_that("spatial_invalidation_targets returns the downstream node set for each trigger", {
  expect_setequal(spatial_invalidation_targets("study_area"),
                  c("hex_grid", "local_networks", "metrics_data", "habitat_clipped", "grid_with_habitat"))
  expect_setequal(spatial_invalidation_targets("grid"), c("local_networks", "metrics_data", "grid_with_habitat"))
  expect_setequal(spatial_invalidation_targets("networks"), "metrics_data")
  expect_setequal(spatial_invalidation_targets("habitat"), "grid_with_habitat")
  for (from in c("study_area", "grid", "networks", "habitat")) {
    expect_false("species_data" %in% spatial_invalidation_targets(from), info = from)
  }
  expect_error(spatial_invalidation_targets("nope"))
})

# ---------------------------------------------------------------------------
# D1.2 habitat cache coverage (F57)
# ---------------------------------------------------------------------------

test_that("euseamap_covers is TRUE only when the loaded layer's box contains the target", {
  layer <- square_sf(18, 54, 22, 57, EUNIScomb = "A5.2")
  attr(layer, "bbox_filter") <- c(18, 54, 22, 57)
  expect_true(euseamap_covers(layer, c(19, 55, 20, 56)))
  expect_true(euseamap_covers(layer, c(18, 54, 22, 57)))   # equal box
  expect_false(euseamap_covers(layer, c(21, 56, 23, 58)))  # sticks out
  expect_false(euseamap_covers(layer, c(30, 60, 31, 61)))  # disjoint
  expect_false(euseamap_covers(NULL, c(19, 55, 20, 56)))
  expect_false(euseamap_covers(layer, NULL))
})

test_that("habitat_target_bbox has no default box: NULL without an area or grid", {
  expect_null(habitat_target_bbox(NULL, NULL))
  grid <- square_sf(20, 55, 21, 56, hex_id = "HEX_0001")
  expect_equal(unname(habitat_target_bbox(NULL, grid)), c(20, 55, 21, 56))
  area <- square_sf(19, 54, 20, 55, id = 1)
  # A study area wins over the grid and is buffered by habitat_load_bbox().
  got <- unname(habitat_target_bbox(area, grid))
  expect_true(got[1] < 19 && got[2] < 54 && got[3] > 20 && got[4] > 55)
  expect_true(got[3] < 21)
})

# ---------------------------------------------------------------------------
# D1.3 s2 save/restore (F60)
# ---------------------------------------------------------------------------

test_that("with_s2_disabled restores the previous s2 setting, even on error", {
  old <- suppressMessages(sf::sf_use_s2())
  withr::defer(suppressMessages(sf::sf_use_s2(old)))
  for (start in c(TRUE, FALSE)) {
    suppressMessages(sf::sf_use_s2(start))
    expect_false(with_s2_disabled(sf::sf_use_s2()))
    expect_identical(with_s2_disabled(1), 1)
    expect_error(with_s2_disabled(stop("boom")), "boom")
    expect_identical(suppressMessages(sf::sf_use_s2()), start, info = start)
  }
})

test_that("read_bbt_polygon returns the named area and errors cleanly on an unknown name", {
  old <- suppressMessages(sf::sf_use_s2())
  withr::defer(suppressMessages(sf::sf_use_s2(old)))
  f <- withr::local_tempfile(fileext = ".geojson")
  bbt <- rbind(square_sf(20, 55, 21, 56, Name = "Bay_of_Gdansk"), square_sf(10, 54, 11, 55, Name = "Kiel"))
  sf::st_write(bbt, f, quiet = TRUE)
  got <- read_bbt_polygon("Bay_of_Gdansk", path = f)
  expect_s3_class(got, "sf")
  expect_identical(nrow(got), 1L)
  expect_identical(got$Name, "Bay_of_Gdansk")
  expect_error(read_bbt_polygon("nope", path = f), "nope")
  expect_identical(suppressMessages(sf::sf_use_s2()), old)
  expect_error(read_bbt_polygon("Kiel", path = tempfile(fileext = ".geojson")), "not found")
})

# ---------------------------------------------------------------------------
# D1.4 species upload validation (F61)
# ---------------------------------------------------------------------------

test_that("validate_species_occurrences coerces comma decimals and drops bad rows with a count", {
  df <- data.frame(species = c("A", "B", "", "C", "D"),
                   lon = c("20,5", "20.6", "20.7", "abc", "200"),
                   lat = c("55,1", "55.2", "55.3", "55.4", "55.5"),
                   biomass = c("1", "2,5", "3", "4", "5"), stringsAsFactors = FALSE)
  res <- validate_species_occurrences(df)
  expect_identical(res$data$species, c("A", "B"))
  expect_equal(res$data$lon, c(20.5, 20.6))
  expect_equal(res$data$lat, c(55.1, 55.2))
  expect_equal(res$data$biomass, c(1, 2.5))
  expect_identical(res$dropped, 3L)
})

test_that("validate_species_occurrences reads ';'-separated files and flags swapped lon/lat", {
  semi <- data.frame(`species;lon;lat` = c("A;20,5;55,1", "B;20,6;55,2"), check.names = FALSE)
  res <- validate_species_occurrences(semi)
  expect_identical(res$data$species, c("A", "B"))
  expect_equal(res$data$lon, c(20.5, 20.6))
  swapped <- data.frame(species = c("A", "B", "C"), lon = c(55.1, 55.2, 55.3), lat = c(20.1, 20.2, 20.3))
  res <- validate_species_occurrences(swapped, bbox = c(19, 54, 22, 57))
  expect_true(any(grepl("swapped", res$messages)))
})

test_that("validate_species_occurrences checks the required columns after a ';' re-read", {
  expect_error(validate_species_occurrences(data.frame(species = "A", x = 1, y = 2)),
               "Missing required columns: lon, lat")
  semi <- data.frame(`species;lon;lat;biomass` = "A;20,5;55,1;3", check.names = FALSE)
  expect_identical(validate_species_occurrences(semi)$data$biomass, 3)
})

test_that("validate_species_occurrences errors when nothing usable is left", {
  df <- data.frame(species = c("A", "B"), lon = c("x", "y"), lat = c("1", "2"))
  expect_error(validate_species_occurrences(df), "No valid")
})

# ---------------------------------------------------------------------------
# spatial_server wiring (testServer). Assert reactive state and outputs only;
# map_proxy() returns NULL under MockShinySession.
# ---------------------------------------------------------------------------

skip_if_not_installed("leaflet")
skip_if_not_installed("DT")
suppressWarnings(suppressPackageStartupMessages({
  library(shiny)
  library(leaflet)
  library(DT)
  library(sf)
  library(igraph)
}))
withr::with_dir(app_root, suppressMessages(source("R/config.R", local = FALSE)))  # MIN_CELL_SIZE_METERS
source(file.path(app_root, "R/functions/spatial_analysis.R"), local = FALSE)
source(file.path(app_root, "R/functions/emodnet_habitat_utils.R"), local = FALSE)
source(file.path(app_root, "R/modules/spatial_server.R"), local = FALSE)

spatial_app <- function(euseamap = reactiveVal(NULL)) {
  force(euseamap)
  function(input, output, session) {
    euseamap_data <- euseamap
    st <- spatial_server(input, output, session, reactiveVal(), reactiveVal(), reactiveVal(), euseamap_data)
  }
}

one_node_net <- function() igraph::make_graph(c("a", "b"), directed = TRUE)

test_that("a new grid clears the previous run's networks and metrics", {
  testServer(spatial_app(), {
    session$setInputs(spatial_xmin = 20, spatial_ymin = 55, spatial_xmax = 20.2, spatial_ymax = 55.1,
                      spatial_cell_size = 5000, spatial_create_grid = 1)
    expect_false(is.null(st$hex_grid()))
    st$local_networks(list(HEX_0001 = one_node_net()))
    st$metrics_data(data.frame(hex_id = "HEX_0001", S = 2))
    session$setInputs(spatial_cell_size = 4000, spatial_create_grid = 2)
    expect_null(st$local_networks())
    expect_null(st$metrics_data())
    expect_false(is.null(st$hex_grid()))
  })
})

test_that("a study-area change clears the grid; species data survives", {
  testServer(spatial_app(), {
    st$hex_grid(sf::st_sf(hex_id = "HEX_0001", geometry = sf::st_sfc(sf::st_point(c(20, 55)), crs = 4326)))
    st$species_data(data.frame(species = "A", lon = 20, lat = 55))
    session$setInputs(spatial_clear_study_area = 1)
    expect_null(st$hex_grid())
    expect_false(is.null(st$species_data()))
  })
})

test_that("the metrics table keeps non-empty hexagons even when S is not a selected metric", {
  nets <- list(HEX_0001 = one_node_net(), HEX_0002 = igraph::make_empty_graph())
  metrics <- data.frame(hex_id = c("HEX_0001", "HEX_0002"), C = c(0.5, NA))
  expect_identical(spatial_metrics_display(metrics, nets)$hex_id, "HEX_0001")
  # The old filter metrics[metrics$S > 0, ] gave 0 rows here.
  expect_identical(nrow(metrics[metrics$S > 0, ]), 0L)
  testServer(spatial_app(), {
    st$local_networks(nets)
    st$metrics_data(metrics)
    session$flushReact()
    # DT renders server-side, so check the table renders with the C column.
    expect_match(as.character(output$spatial_metrics_table), "<th>C<\\\\/th>")
  })
})

test_that("enabling habitat with no study area or grid loads nothing (no default box)", {
  eu <- reactiveVal(NULL)
  testServer(spatial_app(eu), {
    session$setInputs(spatial_enable_habitat = TRUE)
    expect_null(eu())
  })
})

test_that("a failing BBT load leaves the process-wide s2 setting unchanged", {
  old <- suppressMessages(sf::sf_use_s2())
  withr::defer(suppressMessages(sf::sf_use_s2(old)))
  real <- read_bbt_polygon
  withr::defer(assign("read_bbt_polygon", real, envir = globalenv()))
  for (start in c(TRUE, FALSE)) {
    suppressMessages(sf::sf_use_s2(start))
    assign("read_bbt_polygon", function(...) with_s2_disabled(stop("boom")), envir = globalenv())
    testServer(spatial_app(), {
      expect_warning(session$setInputs(spatial_bbt_selector = "Bay_of_Gdansk"), "boom")
      expect_null(st$study_area())
    })
    expect_identical(suppressMessages(sf::sf_use_s2()), start, info = start)
  }
})

test_that("a ';'-separated upload is read, not rejected for missing columns", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("species;lon;lat", "Gadus morhua;20,5;55,1", "Clupea harengus;20,6;55,2"), f)
  testServer(spatial_app(), {
    session$setInputs(spatial_species_file = list(datapath = f, name = basename(f)))
    expect_identical(st$species_data()$species, c("Gadus morhua", "Clupea harengus"))
  })
})

test_that("an upload with bad coordinates keeps the valid rows instead of killing the session", {
  testServer(spatial_app(), {
    f <- test_path("fixtures", "spatial_bad_coords.csv")
    expect_warning(session$setInputs(spatial_species_file = list(datapath = f, name = basename(f))), "dropped")
    got <- st$species_data()
    expect_identical(got$species, c("Gadus morhua", "Platichthys flesus"))
    expect_equal(got$lon, c(20.5, 20.6))
    expect_equal(got$biomass, c(10, 2.5))
  })
})
