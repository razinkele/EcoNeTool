# =============================================================================
# Live Integration Tests: EcoBase Connection
# =============================================================================
# These tests hit the REAL EcoBase API. Skipped by default.
# Enable with: Sys.setenv(RUN_LIVE_TESTS = "true")
# =============================================================================

source_app_dependencies()

# ============================================================================
# Connection smoke tests
# ============================================================================

test_that("EcoBase API is reachable", {
  skip_if_no_ecobase()

  result <- test_ecobase_connection()
  expect_true(result)
})

test_that("get_ecobase_models returns current model list", {
  skip_if_no_ecobase()

  models <- get_ecobase_models()

  expect_s3_class(models, "data.frame")
  expect_gt(nrow(models), 100)

  # Verify known model exists (model 403 = North Sea)
  id_col <- grep("model.*number|model_number", names(models),
                 ignore.case = TRUE, value = TRUE)[1]
  if (!is.na(id_col)) {
    model_ids <- as.numeric(models[[id_col]])
    expect_true(403 %in% model_ids,
                info = "Known model 403 (North Sea) should be in the list")
  }
})

# ============================================================================
# Model download tests
# ============================================================================

test_that("get_ecobase_model_input downloads model 403", {
  skip_if_no_ecobase()

  input_data <- get_ecobase_model_input(403)

  expect_type(input_data, "list")
  expect_gt(length(input_data), 5,
            label = "Model 403 should have more than 5 groups")
})

test_that("get_ecobase_model_output downloads model 403", {
  skip_if_no_ecobase()

  output_data <- get_ecobase_model_output(403)

  expect_type(output_data, "list")
  expect_gt(length(output_data), 5)
})

test_that("get_ecobase_model_metadata returns valid metadata", {
  skip_if_no_ecobase()

  metadata <- get_ecobase_model_metadata(403)

  expect_type(metadata, "list")
  expect_true(!is.na(metadata$model_name))
})

# ============================================================================
# End-to-end conversion
# ============================================================================

test_that("convert_ecobase_to_econetool produces valid network", {
  skip_if_no_ecobase()

  model <- convert_ecobase_to_econetool(403, use_output = TRUE)

  expect_valid_ecobase_model(model)
  expect_gt(igraph::ecount(model$net), 0)
  expect_gt(nrow(model$info), 5)
})

test_that("convert_ecobase_to_econetool works with input parameters", {
  skip_if_no_ecobase()

  model <- convert_ecobase_to_econetool(403, use_output = FALSE)

  expect_valid_ecobase_model(model)
})

test_that("convert_ecobase_to_econetool_hybrid produces valid network", {
  skip_if_no_ecobase()

  model <- convert_ecobase_to_econetool_hybrid(403)

  expect_valid_ecobase_model(model)
  expect_gt(igraph::ecount(model$net), 0)
})

# ============================================================================
# Response format validation (detect API changes)
# ============================================================================

test_that("EcoBase input XML structure is unchanged", {
  skip_if_no_ecobase()

  input_data <- get_ecobase_model_input(403)
  first_group <- input_data[[1]]

  # Verify expected field names are present
  field_names <- names(first_group)
  expect_true("group_name" %in% field_names || "name" %in% field_names,
              info = "API should return group_name or name field")
  expect_true("group_seq" %in% field_names || "seq" %in% field_names,
              info = "API should return group sequence field")
})

test_that("EcoBase model list columns are stable", {
  skip_if_no_ecobase()

  models <- get_ecobase_models()

  # These columns should always exist (may be prefixed with "model.")
  col_names <- names(models)
  expect_true(
    any(grepl("model_number|model\\.model_number", col_names)),
    info = "model_number column should exist"
  )
  expect_true(
    any(grepl("model_name|model\\.model_name", col_names)),
    info = "model_name column should exist"
  )
  expect_true(
    any(grepl("dissemination_allow|model\\.dissemination_allow", col_names)),
    info = "dissemination_allow column should exist"
  )
})

# ============================================================================
# Error handling with bad input
# ============================================================================

test_that("get_ecobase_model_input handles non-existent model", {
  skip_if_no_ecobase()

  # Model ID 999999 should not exist
  expect_error(
    get_ecobase_model_input(999999),
    info = "Should error for non-existent model"
  ) |> tryCatch(error = function(e) {
    # Some versions return empty list instead of error
    result <- tryCatch(get_ecobase_model_input(999999), error = function(e) NULL)
    if (!is.null(result)) {
      expect_equal(length(result), 0)
    }
  })
})
