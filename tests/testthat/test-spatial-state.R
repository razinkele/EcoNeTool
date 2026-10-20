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

test_that("validate_species_occurrences errors when nothing usable is left", {
  df <- data.frame(species = c("A", "B"), lon = c("x", "y"), lat = c("1", "2"))
  expect_error(validate_species_occurrences(df), "No valid")
})
