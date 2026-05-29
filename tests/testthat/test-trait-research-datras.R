# Unit tests for the DATRAS panel plumbing in the Trait Research module
# (R3 PR2). The pure AphiaID-selection helper is testable without a Shiny
# session; the reactive/observer wiring around it is not unit-tested here.

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")

test_that(".datras_aphia_for_species reads a valid AphiaID from the results frame", {
  source(file.path(app_root, "R/modules/trait_research_server.R"), local = TRUE)

  df <- data.frame(
    species  = c("Gadus morhua", "Clupea harengus"),
    aphia_id = c(126436, 126417),
    stringsAsFactors = FALSE
  )
  expect_equal(.datras_aphia_for_species(df, "Gadus morhua"), 126436)
  expect_equal(.datras_aphia_for_species(df, "Clupea harengus"), 126417)
})

test_that(".datras_aphia_for_species returns NA when AphiaID is unusable or absent", {
  source(file.path(app_root, "R/modules/trait_research_server.R"), local = TRUE)

  # column entirely absent (the common API-path case pre Part-C fix)
  no_col <- data.frame(species = "Gadus morhua", stringsAsFactors = FALSE)
  expect_true(is.na(.datras_aphia_for_species(no_col, "Gadus morhua")))

  # column present but NA / non-positive / non-numeric
  bad <- data.frame(
    species  = c("A", "B", "C"),
    aphia_id = c(NA_real_, 0, -1),
    stringsAsFactors = FALSE
  )
  expect_true(is.na(.datras_aphia_for_species(bad, "A")))
  expect_true(is.na(.datras_aphia_for_species(bad, "B")))
  expect_true(is.na(.datras_aphia_for_species(bad, "C")))

  # species not in the frame, and a NULL frame
  expect_true(is.na(.datras_aphia_for_species(df = bad, sp = "Z")))
  expect_true(is.na(.datras_aphia_for_species(df = NULL, sp = "A")))
})
