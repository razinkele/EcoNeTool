# Deep-analysis fix D1: F62 (NA biomass drops species) and F63 (repeated
# habitat overlay gives .x/.y columns). Tiny synthetic sf fixtures, no GDB.

skip_if_not_installed("sf")
app_root <- get_app_root()
source(file.path(app_root, "R/functions/spatial_analysis.R"), local = FALSE)
suppressWarnings(suppressPackageStartupMessages(library(sf)))
source(file.path(app_root, "R/functions/emodnet_habitat_utils.R"), local = FALSE)

square <- function(xmin, ymin, xmax, ymax, crs) {
  sf::st_sfc(sf::st_polygon(list(matrix(c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin),
                                        ncol = 2, byrow = TRUE))), crs = crs)
}

test_that("assign_species_to_hexagons keeps species with NA biomass (presence never needs biomass)", {
  grid <- sf::st_sf(hex_id = "HEX_0001", geometry = square(20, 55, 21, 56, 4326))
  occ <- data.frame(lon = c(20.2, 20.4), lat = c(55.2, 55.4), species = c("x", "y"), biomass = c(1, NA))
  expect_warning(res <- assign_species_to_hexagons(occ, grid), "NA biomass")
  expect_setequal(res$species, c("x", "y"))
  expect_equal(res$biomass[res$species == "x"], 1)
  expect_true(is.na(res$biomass[res$species == "y"]))
  expect_true(all(res$hex_id == "HEX_0001"))
})

test_that("overlaying habitat twice gives the same columns and values as once", {
  grid <- sf::st_sf(cell_id = 1:2, geometry = c(square(0, 0, 1000, 1000, 3857), square(1000, 0, 2000, 1000, 3857)))
  hab <- sf::st_sf(EUNIScomb = c("A5.13", "A5.23"), EUNIScombD = c("Sand", "Mud"), Substrate = c("Sand", "Mud"),
                   Biozone = c("Infralittoral", "Circalittoral"), Energy = c("Low", "Low"),
                   geometry = c(square(0, 0, 1000, 1000, 3857), square(1000, 0, 2000, 1000, 3857)))
  once <- suppressMessages(overlay_habitat_with_grid(grid, hab))
  twice <- suppressMessages(overlay_habitat_with_grid(once, hab))
  expect_false(any(grepl("\\.(x|y)$", names(twice))))
  expect_setequal(names(twice), names(once))
  expect_identical(sf::st_drop_geometry(twice)[order(twice$cell_id), names(sf::st_drop_geometry(once))],
                   sf::st_drop_geometry(once)[order(once$cell_id), ])
})
