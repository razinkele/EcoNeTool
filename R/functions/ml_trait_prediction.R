# =============================================================================
# Machine Learning Trait Prediction
# =============================================================================
#
# This module provides ML-based prediction of missing traits using Random
# Forest models trained on taxonomic features.
#
# Models trained: MS, FS, MB, EP, PR (5 total)
# Features: phylum, class, order, family, genus (taxonomic hierarchy)
# Algorithm: Random Forest (handles categorical data well)
#
# Date: 2025-12-25
# Version: 1.0
#
# =============================================================================

# Cache environment for lazy loading (avoids <<-)
.ml_cache <- new.env(parent = emptyenv())
.ml_cache$models <- NULL

# =============================================================================
# LOAD MODELS
# =============================================================================

#' Load ML models from disk (lazy loading)
#'
#' Models are loaded once and cached in memory for subsequent predictions.
#'
#' @return List containing models and metadata, or NULL if models not found
#' @export
load_ml_models <- function() {

  # Return cached models if already loaded
  if (!is.null(.ml_cache$models)) {
    return(.ml_cache$models)
  }

  models_file <- "models/trait_ml_models.rds"

  if (!file.exists(models_file)) {
    message("⚠️  ML models not found at: ", models_file)
    message("    Run scripts/train_trait_models.R to generate models")
    return(NULL)
  }

  tryCatch({
    models <- readRDS(models_file)

    # Validate model structure
    required_fields <- c("models", "performance", "feature_cols", "training_date")
    if (!all(required_fields %in% names(models))) {
      message("⚠️  Invalid model file structure")
      return(NULL)
    }

    # Cache in global variable
    .ml_cache$models <- models

    message("✓ ML models loaded successfully")
    message("  Training date: ", models$training_date)
    message("  Training samples: ", models$training_samples)
    message("  Available models: ", paste(names(models$models), collapse = ", "))

    return(models)

  }, error = function(e) {
    message("⚠️  Error loading ML models: ", e$message)
    return(NULL)
  })
}

# =============================================================================
# PREPARE FEATURES
# =============================================================================

#' Prepare taxonomic features for ML prediction
#'
#' Converts taxonomic information into the format expected by ML models.
#'
#' @param taxonomic_info List containing phylum, class, order, family, genus
#' @return Data frame with feature columns as factors, or NULL if insufficient data
#' @export
prepare_ml_features <- function(taxonomic_info) {

  # Extract taxonomic ranks
  phylum <- tolower(taxonomic_info$phylum %||% "")
  class <- tolower(taxonomic_info$class %||% "")
  order <- tolower(taxonomic_info$order %||% "")
  family <- tolower(taxonomic_info$family %||% "")
  genus <- tolower(taxonomic_info$genus %||% "")

  # Check if we have minimum required data (at least phylum and class)
  if (phylum == "" || class == "") {
    message("    ⚠️  Insufficient taxonomic data for ML prediction (need at least phylum and class)")
    return(NULL)
  }

  # Create feature data frame
  features <- data.frame(
    phylum = factor(phylum),
    class = factor(class),
    order = factor(order),
    family = factor(family),
    genus = factor(genus),
    stringsAsFactors = FALSE
  )

  return(features)
}

# =============================================================================
# PREDICT SINGLE TRAIT
# =============================================================================

#' Predict a single trait using ML model
#'
#' @param trait_name Name of trait to predict (MS, FS, MB, EP, PR)
#' @param taxonomic_info List containing phylum, class, order, family, genus
#' @param models_package Loaded ML models (from load_ml_models())
#' @return List with prediction, probability, confidence, source, or NULL if prediction fails
#' @export
predict_trait_ml <- function(trait_name, taxonomic_info, models_package = NULL) {

  # Load models if not provided
  if (is.null(models_package)) {
    models_package <- load_ml_models()
    if (is.null(models_package)) {
      return(NULL)
    }
  }

  # Check if model exists for this trait
  if (!trait_name %in% names(models_package$models)) {
    message("    ⚠️  No ML model available for trait: ", trait_name)
    return(NULL)
  }

  # Prepare features
  features <- prepare_ml_features(taxonomic_info)
  if (is.null(features)) {
    return(NULL)
  }

  # Get model
  model <- models_package$models[[trait_name]]

  # Make prediction
  tryCatch({
    # Predict class
    prediction <- predict(model, features, type = "response")

    # Get class probabilities
    probabilities <- predict(model, features, type = "prob")

    # Get probability of predicted class
    predicted_class <- as.character(prediction[1])
    if (predicted_class %in% colnames(probabilities)) {
      probability <- probabilities[1, predicted_class]
    } else {
      probability <- NA
    }

    # Calculate confidence based on probability
    # High confidence: >0.7, Medium: 0.5-0.7, Low: <0.5
    confidence <- if (!is.na(probability)) {
      if (probability > 0.7) "high"
      else if (probability > 0.5) "medium"
      else "low"
    } else {
      "low"
    }

    # Get model performance metrics
    perf <- models_package$performance[[trait_name]]
    model_accuracy <- if (!is.null(perf)) perf$accuracy else NA

    # Return prediction with metadata
    return(list(
      value = predicted_class,
      probability = probability,
      confidence = confidence,
      source = "ML",
      model_accuracy = model_accuracy,
      taxonomic_input = list(
        phylum = taxonomic_info$phylum,
        class = taxonomic_info$class,
        order = taxonomic_info$order,
        family = taxonomic_info$family,
        genus = taxonomic_info$genus
      )
    ))

  }, error = function(e) {
    message("    ⚠️  ML prediction failed for ", trait_name, ": ", e$message)
    return(NULL)
  })
}

# =============================================================================
# PREDICT MULTIPLE TRAITS
# =============================================================================

#' Predict all missing traits using ML models
#'
#' Takes current trait data and predicts any missing values using ML models.
#'
#' @param current_traits List with current trait values (may contain NAs)
#' @param taxonomic_info List containing phylum, class, order, family, genus
#' @param traits_to_predict Vector of trait names to predict (default: all 5 traits)
#' @return List of predictions with metadata for each missing trait
#' @export
predict_missing_traits <- function(current_traits, taxonomic_info,
                                   traits_to_predict = c("MS", "FS", "MB", "EP", "PR")) {

  # Load models (lazy loading)
  models_package <- load_ml_models()
  if (is.null(models_package)) {
    return(list())
  }

  predictions <- list()

  # Predict each missing trait
  for (trait in traits_to_predict) {

    # Skip if trait already has a value
    if (!is.null(current_traits[[trait]]) && !is.na(current_traits[[trait]])) {
      next
    }

    # Predict using ML
    pred <- predict_trait_ml(trait, taxonomic_info, models_package)

    if (!is.null(pred)) {
      predictions[[trait]] <- pred
    }
  }

  return(predictions)
}

# =============================================================================
# BATCH PREDICTION
# =============================================================================

#' Predict traits for multiple species
#'
#' Efficiently predicts missing traits for a batch of species.
#'
#' @param species_list List of species data, each with taxonomic_info and current_traits
#' @param progress Show progress bar (default: TRUE)
#' @return List of prediction results for each species
#' @export
batch_predict_traits <- function(species_list, progress = TRUE) {

  # Load models once for all predictions
  models_package <- load_ml_models()
  if (is.null(models_package)) {
    message("⚠️  Cannot perform batch prediction: ML models not available")
    return(list())
  }

  n_species <- length(species_list)
  results <- list()

  if (progress && n_species > 0) {
    pb <- txtProgressBar(min = 0, max = n_species, style = 3)
  }

  for (i in seq_along(species_list)) {
    species_data <- species_list[[i]]

    predictions <- predict_missing_traits(
      current_traits = species_data$current_traits,
      taxonomic_info = species_data$taxonomic_info
    )

    results[[i]] <- list(
      species = species_data$species %||% paste0("Species_", i),
      predictions = predictions,
      n_predicted = length(predictions)
    )

    if (progress && n_species > 0) {
      setTxtProgressBar(pb, i)
    }
  }

  if (progress && n_species > 0) {
    close(pb)
  }

  return(results)
}

# =============================================================================
# MODEL DIAGNOSTICS
# =============================================================================

#' Get ML model performance metrics
#'
#' Returns detailed performance metrics for all trained models.
#'
#' @return List of performance metrics, or NULL if models not loaded
#' @export
get_ml_model_diagnostics <- function() {

  models_package <- load_ml_models()
  if (is.null(models_package)) {
    return(NULL)
  }

  diagnostics <- list(
    training_date = models_package$training_date,
    training_samples = models_package$training_samples,
    r_version = models_package$r_version,
    performance = list()
  )

  for (trait in names(models_package$models)) {
    model <- models_package$models[[trait]]
    perf <- models_package$performance[[trait]]

    diagnostics$performance[[trait]] <- list(
      accuracy = perf$accuracy,
      oob_error_rate = model$err.rate[model$ntree, "OOB"],
      n_classes = length(model$classes),
      classes = model$classes,
      ntree = model$ntree,
      mtry = model$mtry,
      top_features = names(sort(perf$variable_importance[, "MeanDecreaseAccuracy"],
                                decreasing = TRUE))[1:5]
    )
  }

  return(diagnostics)
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Check if ML models are available
#'
#' @return TRUE if models are loaded/available, FALSE otherwise
#' @export
ml_models_available <- function() {
  return(file.exists("models/trait_ml_models.rds"))
}

#' Get list of traits that can be predicted
#'
#' @return Character vector of trait names, or empty vector if models not available
#' @export
get_predictable_traits <- function() {
  models_package <- load_ml_models()
  if (is.null(models_package)) {
    return(character(0))
  }
  return(names(models_package$models))
}

#' Format ML prediction for display
#'
#' @param prediction Prediction result from predict_trait_ml()
#' @return Formatted string for display
#' @export
format_ml_prediction <- function(prediction) {
  if (is.null(prediction)) {
    return("No prediction available")
  }

  prob_str <- if (!is.na(prediction$probability)) {
    sprintf("%.1f%%", prediction$probability * 100)
  } else {
    "N/A"
  }

  acc_str <- if (!is.na(prediction$model_accuracy)) {
    sprintf("%.1f%%", prediction$model_accuracy * 100)
  } else {
    "N/A"
  }

  sprintf("%s (prob: %s, confidence: %s, model accuracy: %s)",
          prediction$value, prob_str, prediction$confidence, acc_str)
}

# =============================================================================
# INTEGRATION WITH EXISTING SYSTEM
# =============================================================================

#' Apply ML predictions to harmonized traits
#'
#' This is the integration point for the existing trait lookup system.
#' Call this after database lookups to fill in missing values with ML predictions.
#'
#' @param harmonized_traits List of harmonized traits (may have NAs)
#' @param raw_traits Raw trait data from databases (must include WoRMS taxonomy)
#' @param verbose Show prediction details (default: FALSE)
#' @return Updated harmonized_traits list with ML predictions filled in
#' @export
apply_ml_fallback <- function(harmonized_traits, raw_traits, verbose = FALSE) {

  # Check if we have taxonomic data for ML prediction
  if (is.null(raw_traits$worms)) {
    if (verbose) {
      message("    ⚠️  No taxonomic data available for ML prediction")
    }
    return(harmonized_traits)
  }

  # Check if ML models are available
  if (!ml_models_available()) {
    if (verbose) {
      message("    ⚠️  ML models not available")
    }
    return(harmonized_traits)
  }

  # Extract taxonomic info
  taxonomic_info <- list(
    phylum = raw_traits$worms$phylum,
    class = raw_traits$worms$class,
    order = raw_traits$worms$order,
    family = raw_traits$worms$family,
    genus = raw_traits$worms$genus
  )

  # Predict missing traits
  ml_predictions <- predict_missing_traits(harmonized_traits, taxonomic_info)

  if (length(ml_predictions) > 0) {
    if (verbose) {
      message("    🤖 ML predictions:")
    }

    # Apply predictions
    for (trait in names(ml_predictions)) {
      pred <- ml_predictions[[trait]]
      harmonized_traits[[trait]] <- pred$value

      # Add metadata about ML prediction
      harmonized_traits[[paste0(trait, "_ml_confidence")]] <- pred$confidence
      harmonized_traits[[paste0(trait, "_ml_probability")]] <- pred$probability

      if (verbose) {
        message(sprintf("       %s: %s", trait, format_ml_prediction(pred)))
      }
    }
  }

  return(harmonized_traits)
}
