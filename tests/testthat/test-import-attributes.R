# =============================================================================
# A3: import attribute layer (F50, F49, F52, F68, F74, F51)
# =============================================================================

source_app_dependencies()

local({
  root <- get_app_root()
  source(file.path(root, "R/config.R"), local = FALSE)
  source(file.path(root, "R/functions/network_finalize.R"), local = FALSE)
  source(file.path(root, "R/functions/ecopath/ecopath_group_biomass.R"), local = FALSE)
  source(file.path(root, "R/functions/data_editor_utils.R"), local = FALSE)
  source(file.path(root, "R/functions/euseamap_regional_config.R"), local = FALSE)
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

# ---------------------------------------------------------------------------
# F50 - EcoBase: no biomass*100 body mass, met.types filled by finalize
# ---------------------------------------------------------------------------

convert_403_offline <- function() {
  input <- load_fixture("ecobase_model_403_input")
  output <- load_fixture("ecobase_model_403_output")
  meta <- load_fixture("ecobase_model_403_metadata")
  with_mocked_function(globalenv(), "get_ecobase_model_input", function(model_id) input, {
    with_mocked_function(globalenv(), "get_ecobase_model_output", function(model_id) output, {
      with_mocked_function(globalenv(), "get_ecobase_model_metadata", function(model_id) meta, {
        suppressWarnings(suppressMessages(convert_ecobase_to_econetool_hybrid(403)))
      })
    })
  })
}

test_that("EcoBase converter no longer emits proxy body mass / efficiency / losses (F50)", {
  result <- convert_403_offline()
  expect_false(any(c("bodymasses", "efficiencies", "losses") %in% names(result$info)))
})

test_that("EcoBase import finalised carries met.types and a real body-mass estimate (F50)", {
  result <- convert_403_offline()
  out <- finalize_network(result$net, result$info)

  expect_false(anyNA(out$info$met.types))
  expect_false(isTRUE(all.equal(out$info$bodymasses, out$info$meanB * 100)))
  expect_false(all(out$info$efficiencies == 0.8))
})

test_that("ecobase_server.R finalises the EcoBase import (F50)", {
  code <- readLines(app_path("R/modules/ecobase_server.R"), warn = FALSE)
  code <- code[!startsWith(trimws(code), "#")]
  expect_true(any(grepl("finalize_network(ecobase_net, ecobase_info)", code, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# F49 - EwE Type drives Detritus / producer assignment
# ---------------------------------------------------------------------------

test_that("EwE Type 1/2/0 gives Phytoplankton / Detritus / Fish (F49)", {
  fg <- assign_ewe_functional_groups(c("Phyto", "Rdetrit", "Cod"), ewe_type = c(1, 2, 0))
  expect_equal(fg, c("Phytoplankton", "Detritus", "Fish"))
})

test_that("Type 2 is Detritus even when the name says nothing about detritus", {
  fg <- assign_ewe_functional_groups("Dead organic matter", ewe_type = 2)
  expect_equal(fg, "Detritus")
})

test_that("Type 1 benthic macrophytes stay Benthos", {
  fg <- assign_ewe_functional_groups("Benthic macrophytes", ewe_type = 1)
  expect_equal(fg, "Benthos")
})

test_that("a Type 0 consumer is never made a producer or detritus", {
  # Topology alone (no prey, P/B > 1) would call this Phytoplankton.
  fg <- assign_ewe_functional_groups("Group X", ewe_type = 0, pb_values = 50,
                                     indegrees = 0, outdegrees = 2)
  expect_false(fg %in% c("Phytoplankton", "Detritus"))
  # The name classifier calls this Detritus; EwE says it is a consumer.
  fg2 <- assign_ewe_functional_groups("Detritus feeders", ewe_type = 0,
                                      indegrees = 2, outdegrees = 1)
  expect_equal(fg2, "Benthos")
})

test_that("without a Type column the classifier behaves as before", {
  names_in <- c("Phytoplankton", "Herring", "Group X")
  expect_equal(
    assign_ewe_functional_groups(names_in, ewe_type = NULL, pb_values = c(100, 1, 50),
                                 indegrees = c(0, 1, 0), outdegrees = c(1, 0, 1)),
    unname(assign_functional_groups(names_in, c(100, 1, 50), c(0, 1, 0), c(1, 0, 1),
                                    use_topology = TRUE))
  )
})

test_that("the widened detritus regex catches EwE spellings", {
  expect_equal(assign_functional_group("Detritu"), "Detritus")
  expect_equal(assign_functional_group("Rdetrit"), "Detritus")
  expect_equal(assign_functional_group("Pelagic det."), "Detritus")
  expect_equal(assign_functional_group("Debris"), "Detritus")
  # Detritivores are consumers, not detritus - also for EcoBase, which has no Type.
  expect_equal(assign_functional_group("Detritivorous fish"), "Fish")
  expect_false(assign_functional_group("Benthic detritivores") == "Detritus")
})

test_that("ecopath_import_server.R reads the EwE Type column (F49)", {
  code <- readLines(app_path("R/modules/ecopath_import_server.R"), warn = FALSE)
  code <- code[!startsWith(trimws(code), "#")]
  expect_true(any(grepl("assign_ewe_functional_groups(", code, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# F68 - data editor keeps numeric columns numeric
# ---------------------------------------------------------------------------

editor_frame <- function() {
  data.frame(
    meanB = c(1, 2),
    fg = factor(c("Fish", "Benthos"), levels = get_functional_group_levels()),
    met.types = c("invertebrates", "invertebrates"),
    stringsAsFactors = FALSE,
    row.names = c("Cod", "Mussel")
  )
}

test_that("editing a meanB cell with '3.5' keeps meanB numeric (F68)", {
  df <- apply_cell_edit(editor_frame(), row = 1, col = 1, value = "3.5")
  expect_true(is.numeric(df$meanB))
  expect_equal(df$meanB, c(3.5, 2))
})

test_that("editing fg to a valid level keeps the factor", {
  df <- apply_cell_edit(editor_frame(), row = 2, col = 2, value = "Fish")
  expect_true(is.factor(df$fg))
  expect_equal(as.character(df$fg), c("Fish", "Fish"))
})

test_that("an edit on the row-name column is ignored with a warning", {
  expect_warning(df <- apply_cell_edit(editor_frame(), row = 1, col = 0, value = "X"),
                 "outside the data")
  expect_identical(df, editor_frame())
})

test_that("text that is not a number is rejected, not written as NA", {
  expect_warning(df <- apply_cell_edit(editor_frame(), row = 1, col = 1, value = "abc"),
                 "not a valid value for column 'meanB'")
  expect_identical(df, editor_frame())
})

test_that("an fg outside the canonical levels is rejected", {
  expect_warning(df <- apply_cell_edit(editor_frame(), row = 1, col = 2, value = "Krill"),
                 "not a valid value for column 'fg'")
  expect_identical(df, editor_frame())
})

test_that("non_numeric_info_columns names character-typed numeric columns", {
  df <- editor_frame()
  df$bodymasses <- c("1", "2")
  df$efficiencies <- c(0.7, 0.8)
  expect_equal(non_numeric_info_columns(df), "bodymasses")
})

test_that("dataeditor_inline_server.R routes edits through apply_cell_edit (F68)", {
  code <- readLines(app_path("R/modules/dataeditor_inline_server.R"), warn = FALSE)
  code <- code[!startsWith(trimws(code), "#")]
  expect_true(any(grepl("apply_cell_edit(", code, fixed = TRUE)))
  expect_true(any(grepl("non_numeric_info_columns(", code, fixed = TRUE)))
  expect_false(any(grepl("<- info_edit$value", code, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# F51 - EMODnet enrichment wired to the sampling location
# ---------------------------------------------------------------------------

test_that("resolve_emodnet_bbox prefers the sampling point (F51)", {
  expect_equal(resolve_emodnet_bbox(21, 55.5, NULL), c(19, 53.5, 23, 57.5))
})

test_that("resolve_emodnet_bbox falls back to the model metadata box", {
  meta <- list(min_lon = 20.7, max_lon = 21.1, min_lat = 55.3, max_lat = 56.1)
  expect_equal(resolve_emodnet_bbox(NA, NA, meta), c(20.7, 55.3, 21.1, 56.1))
  expect_equal(resolve_emodnet_bbox(NULL, NULL, meta), c(20.7, 55.3, 21.1, 56.1))
})

test_that("resolve_emodnet_bbox returns NULL with no location at all", {
  expect_null(resolve_emodnet_bbox(NA, NA, NULL))
  expect_null(resolve_emodnet_bbox(NA, NA, list(min_lon = 0, max_lon = 0, min_lat = 0, max_lat = 0)))
  expect_null(resolve_emodnet_bbox(NA, NA, list(min_lon = -9999, max_lon = 1, min_lat = 1, max_lat = 2)))
})

test_that("EMODnet observer has no current_network() and an app_path() GDB path (F51)", {
  code <- readLines(app_path("R/modules/ecopath_import_server.R"), warn = FALSE)
  code <- code[!startsWith(trimws(code), "#")]
  expect_false(any(grepl("current_network(", code, fixed = TRUE)))
  expect_true(any(grepl('app_path("data/EUSeaMap_2025/EUSeaMap_2025.gdb")', code, fixed = TRUE)))
  expect_false(any(grepl("custom_bbox <- c(20, 55, 21, 56)", code, fixed = TRUE)))
  expect_true(any(grepl("resolve_emodnet_bbox(", code, fixed = TRUE)))
})
