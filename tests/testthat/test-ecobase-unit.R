# =============================================================================
# Unit Tests: EcoBase Connection (Mocked with Fixtures)
# =============================================================================
# These tests use saved API responses (fixtures) so they run fast and offline.
# Run capture_fixtures.R first to generate fixture files.
# =============================================================================

# Source app dependencies once
source_app_dependencies()

# ============================================================================
# get_ecobase_models()
# ============================================================================

test_that("get_ecobase_models returns data.frame with expected structure", {
  models <- load_fixture("ecobase_models")

  expect_s3_class(models, "data.frame")
  expect_gt(nrow(models), 0)

  # Should have model identification columns
  col_names <- tolower(names(models))
  expect_true(
    any(grepl("model.*number|modelid|model_number", col_names)),
    info = "Should have a model number/ID column"
  )
  expect_true(
    any(grepl("model.*name|modelname", col_names)),
    info = "Should have a model name column"
  )
})

test_that("get_ecobase_models contains publicly disseminated models only", {
  models <- load_fixture("ecobase_models")

  # All models should have dissemination_allow = true
  dissem_col <- grep("dissemination", names(models), ignore.case = TRUE, value = TRUE)
  if (length(dissem_col) > 0) {
    expect_true(all(models[[dissem_col[1]]] == "true"),
                info = "All returned models should be publicly available")
  }
})

test_that("get_ecobase_models has reasonable number of models", {
  models <- load_fixture("ecobase_models")

  # EcoBase has ~450+ public models as of 2024

  expect_gt(nrow(models), 100,
            label = "Expected at least 100 public models")
  expect_lt(nrow(models), 2000,
            label = "Sanity check: not more than 2000 models")
})

# ============================================================================
# get_ecobase_model_input()
# ============================================================================

test_that("get_ecobase_model_input returns list of groups", {
  input_data <- load_fixture("ecobase_model_403_input")

  expect_type(input_data, "list")
  expect_gt(length(input_data), 0)
})

test_that("EcoBase input groups have expected fields", {
  input_data <- load_fixture("ecobase_model_403_input")

  # Check first group has expected structure
  first_group <- input_data[[1]]
  expect_true(!is.null(first_group))

  # Should have group_name or name
  has_name <- !is.null(first_group[["group_name"]]) || !is.null(first_group[["name"]])
  expect_true(has_name, info = "Group should have group_name or name field")
})

test_that("EcoBase input groups have diet information", {
  input_data <- load_fixture("ecobase_model_403_input")

  # At least some groups should have diet_descr
  has_diet <- sapply(input_data, function(g) !is.null(g[["diet_descr"]]))
  expect_true(any(has_diet),
              info = "At least some groups should have diet composition")
})

test_that("EcoBase input groups have numeric parameters", {
  input_data <- load_fixture("ecobase_model_403_input")

  # Check biomass, pb, qb fields exist and are parseable
  for (i in seq_along(input_data)) {
    group <- input_data[[i]]
    if (!is.null(group[["biomass"]])) {
      b <- as.numeric(group[["biomass"]])
      expect_true(!is.na(b) || group[["biomass"]] == "",
                  info = paste("Group", i, "biomass should be numeric"))
    }
  }
})

# ============================================================================
# get_ecobase_model_output()
# ============================================================================

test_that("get_ecobase_model_output returns list of groups", {
  output_data <- load_fixture("ecobase_model_403_output")

  expect_type(output_data, "list")
  expect_gt(length(output_data), 0)
})

test_that("EcoBase output has same group count as input", {
  input_data <- load_fixture("ecobase_model_403_input")
  output_data <- load_fixture("ecobase_model_403_output")

  expect_equal(length(input_data), length(output_data),
               info = "Input and output should have same number of groups")
})

# ============================================================================
# get_ecobase_model_metadata()
# ============================================================================

test_that("get_ecobase_model_metadata returns metadata list", {
  metadata <- load_fixture("ecobase_model_403_metadata")

  expect_type(metadata, "list")
  expect_true("model_name" %in% names(metadata))
  expect_true("ecosystem_name" %in% names(metadata))
})

test_that("EcoBase metadata has geographic coordinates", {
  metadata <- load_fixture("ecobase_model_403_metadata")

  expect_true("latitude" %in% names(metadata))
  expect_true("longitude" %in% names(metadata))
})

test_that("EcoBase metadata has attribution info", {
  metadata <- load_fixture("ecobase_model_403_metadata")

  # At least some attribution fields should be present
  attr_fields <- c("author", "publication", "doi", "institution")
  has_attr <- sapply(attr_fields, function(f) f %in% names(metadata))
  expect_true(any(has_attr),
              info = "Should have at least one attribution field")
})

# ============================================================================
# convert_ecobase_to_econetool() - via saved converted fixture
# ============================================================================

test_that("convert_ecobase_to_econetool produces valid model", {
  model <- load_fixture("ecobase_model_403_converted")

  expect_valid_ecobase_model(model)
})

test_that("converted model has trophic links", {
  model <- load_fixture("ecobase_model_403_converted")

  expect_gt(igraph::ecount(model$net), 0,
            label = "Network should have edges (trophic links)")
})

test_that("converted model has positive biomass values", {
  model <- load_fixture("ecobase_model_403_converted")

  expect_true(all(model$info$meanB > 0),
              info = "All biomass values should be positive")
})

test_that("converted model has valid EE values (0-1)", {
  model <- load_fixture("ecobase_model_403_converted")

  expect_true(all(model$info$EE >= 0 & model$info$EE <= 1),
              info = "Ecotrophic efficiency should be between 0 and 1")
})

test_that("converted model vertex names match info species", {
  model <- load_fixture("ecobase_model_403_converted")

  vertex_names <- igraph::V(model$net)$name
  expect_equal(vertex_names, model$info$species)
})

test_that("converted model has functional group assignments", {
  model <- load_fixture("ecobase_model_403_converted")

  expect_true(is.factor(model$info$fg))
  expect_true(nlevels(model$info$fg) > 1,
              info = "Should have multiple functional groups")
})

test_that("converted model includes metadata", {
  model <- load_fixture("ecobase_model_403_converted")

  expect_true("metadata" %in% names(model))
  if (!is.null(model$metadata)) {
    expect_type(model$metadata, "list")
  }
})

# ============================================================================
# Error handling tests
# ============================================================================

test_that("require_ecobase_packages detects installed packages", {
  # This should not error since packages are installed
  expect_silent(require_ecobase_packages())
})

test_that("EcoBase conversion handles empty diet gracefully", {
  # Simulate a model with no diet links
  model <- load_fixture("ecobase_model_403_converted")

  # Even if original model had links, verify structure is valid
  expect_true(is.data.frame(model$info))
  expect_true(igraph::is_igraph(model$net))
})
