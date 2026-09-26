# =============================================================================
# Edge contract: A -> B means "A is eaten by B" (prey -> predator) everywhere
# =============================================================================
# The contract is documented in the header of R/functions/network_finalize.R.
# Before A1 several ingest boundaries built predator -> prey edges (F64, F56,
# F48, N1, N3), and four bundled metaweb CSVs stored their columns swapped so
# that two of those bugs cancelled. Every test here pins one boundary.

source_app_dependencies()

local({
  root <- get_app_root()
  source(file.path(root, "R/config.R"), local = FALSE)
  source(file.path(root, "R/functions/functional_group_utils.R"), local = FALSE)
  source(file.path(root, "R/functions/network_finalize.R"), local = FALSE)
  source(file.path(root, "R/functions/trophic_levels.R"), local = FALSE)
  source(file.path(root, "R/functions/metaweb_core.R"), local = FALSE)
  source(file.path(root, "R/functions/spatial_analysis.R"), local = FALSE)
  source(file.path(root, "R/functions/ecopath/ecopath_csv.R"), local = FALSE)
  source(file.path(root, "R/functions/trait_foodweb.R"), local = FALSE)
})
suppressPackageStartupMessages(library(igraph))

# ---------------------------------------------------------------------------
# F48 / F55 - parse_ecopath_data (CSV/Excel EwE import)
# ---------------------------------------------------------------------------

write_ewe_csv <- function() {
  basic <- tempfile(fileext = ".csv")
  diet <- tempfile(fileext = ".csv")
  writeLines(c(
    "Group,Biomass,PB,QB",
    "Phyto,20,100,0",
    "Detritus,50,0,0",
    "Zoo,5,30,100",
    "Cod,1,0.5,3",
    "Apex,0.2,1.5,4"
  ), basic)
  # EwE layout: rows = prey, columns = predators, columns sum to 1.
  writeLines(c(
    "Prey,Phyto,Detritus,Zoo,Cod,Apex",
    "Phyto,0,0,0.6,0,0",
    "Detritus,0,0,0.4,0,0",
    "Zoo,0,0,0,1,0",
    "Cod,0,0,0,0,1",
    "Apex,0,0,0,0,0"
  ), diet)
  c(basic = basic, diet = diet)
}

test_that("parse_ecopath_data keeps EwE's prey x predator orientation (F48)", {
  files <- write_ewe_csv()
  on.exit(unlink(files), add = TRUE)
  res <- parse_ecopath_data(files[["basic"]], files[["diet"]])

  assert_prey_to_predator(res$net, "Phyto", "Zoo")
  assert_prey_to_predator(res$net, "Zoo", "Cod")
  tl <- calculate_trophic_levels(res$net)
  expect_equal(unname(tl[c("Phyto", "Detritus", "Zoo", "Cod", "Apex")]), c(1, 1, 2, 3, 4))
})

test_that("parse_ecopath_data retains the Detritus group (F55)", {
  files <- write_ewe_csv()
  on.exit(unlink(files), add = TRUE)
  res <- parse_ecopath_data(files[["basic"]], files[["diet"]])

  expect_true("Detritus" %in% V(res$net)$name)
  assert_prey_to_predator(res$net, "Detritus", "Zoo")
})

test_that("the topology heuristic sees a top predator, not a producer (F48)", {
  files <- write_ewe_csv()
  on.exit(unlink(files), add = TRUE)
  res <- parse_ecopath_data(files[["basic"]], files[["diet"]])

  # "Apex" matches no name pattern, so topology decides. With inverted edges
  # it had in-degree 0 and P/B > 1 and was classified Phytoplankton.
  expect_equal(as.character(res$info["Apex", "fg"]), "Fish")
  expect_false(as.character(res$info["Cod", "fg"]) == "Phytoplankton")
})

test_that("blank cells in an EwE diet CSV are read as zero", {
  basic <- tempfile(fileext = ".csv")
  diet <- tempfile(fileext = ".csv")
  on.exit(unlink(c(basic, diet)), add = TRUE)
  writeLines(c("Group,Biomass,PB,QB", "Phyto,20,100,0", "Zoo,5,30,100", "Cod,1,0.5,3"), basic)
  writeLines(c("Prey,Phyto,Zoo,Cod", "Phyto,,1,", "Zoo,,,1", "Cod,,,"), diet)

  expect_no_error(res <- parse_ecopath_data(basic, diet))
  expect_equal(ecount(res$net), 2)
  assert_prey_to_predator(res$net, "Phyto", "Zoo")
})

# ---------------------------------------------------------------------------
# E(net)$diet_prop on the EwE importers
# ---------------------------------------------------------------------------

test_that("CSV-imported edges carry their diet proportion", {
  files <- write_ewe_csv()
  on.exit(unlink(files), add = TRUE)
  res <- parse_ecopath_data(files[["basic"]], files[["diet"]])

  expect_true("diet_prop" %in% edge_attr_names(res$net))
  expect_equal(E(res$net)[.from("Phyto") & .to("Zoo")]$diet_prop, 0.6)
  expect_equal(E(res$net)[.from("Detritus") & .to("Zoo")]$diet_prop, 0.4)
})

test_that("EcoBase-imported edges carry their diet proportion", {
  out <- load_fixture("ecobase_model_403_output")
  inp <- load_fixture("ecobase_model_403_input")
  meta <- load_fixture("ecobase_model_403_metadata")
  res <- with_mocked_function(globalenv(), "get_ecobase_model_output", function(id) out,
    with_mocked_function(globalenv(), "get_ecobase_model_input", function(id) inp,
      with_mocked_function(globalenv(), "get_ecobase_model_metadata", function(id) meta,
        suppressWarnings(suppressMessages(convert_ecobase_to_econetool_hybrid(403)))
      )
    )
  )

  expect_gt(ecount(res$net), 0)
  expect_true("diet_prop" %in% edge_attr_names(res$net))
  expect_true(all(E(res$net)$diet_prop > 0 & E(res$net)$diet_prop <= 1))
})

test_that("the native ECOPATH import keeps diet_prop on its edges", {
  # The .ewemdb parser is a closure inside the Shiny module; guard the line.
  code <- readLines(app_path("R/modules/ecopath_import_server.R"), warn = FALSE)
  code <- code[!startsWith(trimws(code), "#")]
  expect_true(any(grepl("E(net)$diet_prop <-", code, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# N3 - trait_foodweb_to_igraph
# ---------------------------------------------------------------------------

test_that("trait food webs are exported prey -> predator (N3)", {
  # The "simple" example dataset from foodweb_construction_server.R
  simple <- data.frame(
    species = c("Predatory_fish", "Small_fish", "Zooplankton", "Phytoplankton", "Benthic_filter_feeder"),
    MS = c("MS5", "MS3", "MS2", "MS1", "MS3"),
    FS = c("FS1", "FS1", "FS6", "FS0", "FS6"),
    MB = c("MB5", "MB5", "MB4", "MB2", "MB1"),
    EP = c("EP4", "EP4", "EP3", "EP4", "EP2"),
    PR = c("PR0", "PR0", "PR0", "PR0", "PR6"),
    stringsAsFactors = FALSE
  )
  g <- trait_foodweb_to_igraph(simple)
  expect_gt(ecount(g), 0)

  # The primary producer (FS0) eats nothing.
  expect_equal(unname(degree(g, "Phytoplankton", mode = "in")), 0)
  # Every edge ends at a consumer: its head is never the FS0 producer.
  heads <- as_edgelist(g)[, 2]
  expect_false(any(simple$FS[match(heads, simple$species)] == "FS0"))
  assert_prey_to_predator(g, "Phytoplankton", "Benthic_filter_feeder")
})
