# =============================================================================
# A3: import attribute layer (F50, F49, F52, F68, F74, F51)
# =============================================================================

source_app_dependencies()

local({
  root <- get_app_root()
  source(file.path(root, "R/config.R"), local = FALSE)
  source(file.path(root, "R/functions/network_finalize.R"), local = FALSE)
  source(file.path(root, "R/functions/ecopath/ecopath_group_biomass.R"), local = FALSE)
})

# ---------------------------------------------------------------------------
# F52 - one biomass definition for the network and Rpath paths
# ---------------------------------------------------------------------------

test_that("ewe_group_biomass multiplies habitat-area biomass by Area (F52)", {
  groups <- data.frame(
    GroupName = c("Filtrators", "Polychaetes", "Fish", "Unknown B", "No area"),
    Biomass = c(14.26, 4.85, 0.5, -9999, 2),
    Area = c(0.2, 0.1, 1, 1, -9999),
    stringsAsFactors = FALSE
  )
  expect_equal(ewe_group_biomass(groups), c(2.852, 0.485, 0.5, NA, 2))
})

test_that("ewe_group_biomass treats an absent Area column as 1", {
  groups <- data.frame(GroupName = "A", Biomass = 3)
  expect_equal(ewe_group_biomass(groups), 3)
})

test_that("ewe_group_biomass warns on a non-positive habitat area", {
  groups <- data.frame(GroupName = "Zero", Biomass = 3, Area = 0)
  expect_warning(b <- ewe_group_biomass(groups), "Zero")
  expect_equal(b, 3)
})

read_example_groups <- function(file) {
  path <- app_path("examples", file)
  skip_if_not(file.exists(path), paste("example database not found:", file))
  root <- get_app_root()
  for (f in c("ecopath_windows.R", "ecopath_unix.R", "ecopath_import.R")) {
    source(file.path(root, "R/functions/ecopath", f), local = FALSE)
  }
  result <- tryCatch(
    suppressMessages(parse_ecopath_native_cross_platform(path)),
    error = function(e) NULL
  )
  skip_if(is.null(result),
          "no Access reader available (RODBC + Access driver on Windows, mdbtools on Linux)")
  result$group_data
}

test_that("ewe_group_biomass known answers on the Coastal model example (F52)", {
  g <- read_example_groups("Coastal model EE 1.ewemdb")
  b <- setNames(ewe_group_biomass(g), trimws(g$GroupName))

  expect_equal(unname(b["Macrozoobenthos filtrators"]), 14.26 * 0.2, tolerance = 1e-6)
  expect_equal(unname(b["Polychaetes"]), 4.85 * 0.1, tolerance = 1e-6)
  expect_equal(unname(b["Phytoplankton"]), 5.199, tolerance = 1e-6)
  expect_true(is.na(b["Greater sand-eel (adult)"]))  # -9999: left for Ecopath to estimate
})

test_that("ewe_group_biomass known answers on the LT2022 example (F52)", {
  g <- read_example_groups("LT2022_0.5ST_final7.eweaccdb")
  b <- setNames(ewe_group_biomass(g), trimws(g$GroupName))

  expect_equal(unname(b["Herring"]), 6.13, tolerance = 1e-6)  # Area = 1 throughout
  expect_equal(unname(b["Phytoplankton"]), 26.7, tolerance = 1e-6)
})

test_that("both biomass paths call ewe_group_biomass (F52)", {
  server <- readLines(app_path("R/modules/ecopath_import_server.R"), warn = FALSE)
  rpath <- readLines(app_path("R/functions/rpath/rpath_conversion.R"), warn = FALSE)
  expect_true(any(grepl("ewe_group_biomass(", server, fixed = TRUE)))
  expect_true(any(grepl("ewe_group_biomass(living_groups)", rpath, fixed = TRUE)))
  expect_false(any(grepl("biomass_values * area_proportions", server, fixed = TRUE)))
})
