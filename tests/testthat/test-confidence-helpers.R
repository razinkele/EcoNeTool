# Unit tests for the confidence_to_num / confidence_to_label helper pair
# introduced in PR10-S. These centralize the categorical-to-numeric
# mapping that previously lived inline in build_offline_trait_db.R's
# ontology block.

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")

test_that("confidence_to_num maps the four canonical labels", {
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/trait_lookup/harmonization.R"),
         local = TRUE)

  expect_equal(confidence_to_num("none"),   0.0)
  expect_equal(confidence_to_num("low"),    0.33)
  expect_equal(confidence_to_num("medium"), 0.66)
  expect_equal(confidence_to_num("high"),   1.0)

  # Case-insensitive
  expect_equal(confidence_to_num("HIGH"), 1.0)
  expect_equal(confidence_to_num("Medium"), 0.66)

  # Vectorized
  expect_equal(confidence_to_num(c("none", "low", "medium", "high")),
               c(0.0, 0.33, 0.66, 1.0))

  # Unknown labels and NULL/NA
  expect_true(is.na(confidence_to_num("garbage")))
  expect_true(is.na(confidence_to_num(NA)))
  expect_true(is.na(confidence_to_num(NULL)))
})

test_that("confidence_to_label round-trips with confidence_to_num", {
  source(file.path(app_root, "R/functions/validation_utils.R"), local = TRUE)
  source(file.path(app_root, "R/functions/trait_lookup/harmonization.R"),
         local = TRUE)

  for (lbl in c("none", "low", "medium", "high")) {
    expect_equal(confidence_to_label(confidence_to_num(lbl)), lbl,
                 info = paste("round-trip failed for", lbl))
  }

  # Banding behavior at boundaries
  expect_equal(confidence_to_label(0),    "none")
  expect_equal(confidence_to_label(0.01), "low")
  expect_equal(confidence_to_label(0.5),  "medium")
  expect_equal(confidence_to_label(0.7),  "high")
  expect_equal(confidence_to_label(1.0),  "high")

  # NA / vector / non-numeric
  expect_true(is.na(confidence_to_label(NA)))
  expect_true(is.na(confidence_to_label("not-a-number")))
  expect_equal(confidence_to_label(c(0, 0.5, 1)),
               c("none", "medium", "high"))
})
