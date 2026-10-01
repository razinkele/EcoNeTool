# =============================================================================
# Train Trait Prediction Models
# =============================================================================
#
# This script trains Random Forest models to predict missing traits based on
# taxonomic classification. Models are saved for use in production.
#
# Models trained: MS, FS, MB, EP, PR (5 total)
# Features: phylum, class, order, family, genus (taxonomic hierarchy)
# Algorithm: Random Forest (handles categorical data well)
#
# Date: 2025-12-25
# Version: 1.0
#
# =============================================================================

cat("=============================================================================\n")
cat("TRAIT PREDICTION MODEL TRAINING\n")
cat("=============================================================================\n\n")

# Required packages
required_packages <- c("randomForest", "caret")

cat("Checking required packages...\n")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("  Installing", pkg, "...\n")
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(randomForest)
library(caret)

# =============================================================================
# STEP 1: COLLECT TRAINING DATA
# =============================================================================

cat("\n=============================================================================\n")
cat("STEP 1: Collecting Training Data\n")
cat("=============================================================================\n\n")

cache_dir <- "cache/taxonomy"

if (!dir.exists(cache_dir)) {
  stop("Cache directory not found: ", cache_dir, "\n",
       "Please run some trait lookups first to populate the cache.")
}

# Load ontology data as additional training source
ontology_file <- "data/ontology_traits.csv"
ontology_raw <- NULL

if (file.exists(ontology_file)) {
  cat("Loading ontology data...\n")
  ontology_raw <- read.csv(ontology_file, stringsAsFactors = FALSE)
  cat("  ✓ Loaded", nrow(ontology_raw), "trait records from",
      length(unique(ontology_raw$taxon_name)), "species\n")
}

# Load functions for harmonization and WoRMS lookup. trait_lookup.R was split
# into R/functions/trait_lookup/ long ago; load_all.R is the entry point (run
# this script from the repo root, like load_all.R itself).
source("R/functions/validation_utils.R")
source("R/config/harmonization_config.R")
source("R/functions/trait_lookup/load_all.R")

# Training labels: only codes a database or a rule produced (T_method
# "observed" / "rule") - never ML, phylogenetic or default codes, which would
# train the models on their own output (spec C3.7, F34). Cache files written
# before C-8 carry no per-trait method and are skipped with one warning.
cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
cat("Found", length(cache_files), "cached species\n")

cat("Extracting training data...\n")
cache_rows <- training_rows_from_cache(cache_files)
training_data <- if (nrow(cache_rows) > 0) split(cache_rows, seq_len(nrow(cache_rows))) else list()

cat("\n")
cat("Cache data extracted:", length(training_data), "species\n")

# =============================================================================
# SUPPLEMENT WITH ONTOLOGY DATA (if no cache data found)
# =============================================================================

if (!is.null(ontology_raw) && length(training_data) < 10) {
  cat("\nInsufficient cache data. Supplementing with ontology species...\n")

  # Get unique species from ontology
  unique_species <- unique(ontology_raw[, c("taxon_name", "aphia_id")])
  cat("Found", nrow(unique_species), "unique species in ontology\n")
  cat("Querying WoRMS for taxonomy...\n")

  pb2 <- txtProgressBar(min = 0, max = nrow(unique_species), style = 3)

  for (i in 1:nrow(unique_species)) {
    species_name <- unique_species$taxon_name[i]
    aphia_id <- unique_species$aphia_id[i]

    tryCatch({
      # Get WoRMS taxonomy
      worms_result <- lookup_worms_traits(species_name)

      if (worms_result$success && !is.null(worms_result$traits)) {
        worms <- worms_result$traits

        # Check minimum taxonomy
        has_taxonomy <- !is.null(worms$phylum) && !is.null(worms$class)

        if (has_taxonomy) {
          # Get traits from ontology for this species
          species_ontology <- ontology_raw[ontology_raw$taxon_name == species_name, ]

          # Harmonize traits from ontology
          traits_harmonized <- list(MS = NA, FS = NA, MB = NA, EP = NA, PR = NA)

          # Try fuzzy harmonization for feeding
          fuzzy_fs <- harmonize_fuzzy_foraging(species_ontology)
          if (!is.na(fuzzy_fs$class)) {
            traits_harmonized$FS <- fuzzy_fs$class
          }

          # Try fuzzy harmonization for mobility
          fuzzy_mb <- harmonize_fuzzy_mobility(species_ontology)
          if (!is.na(fuzzy_mb$class)) {
            traits_harmonized$MB <- fuzzy_mb$class
          }

          # Try fuzzy harmonization for habitat
          fuzzy_ep <- harmonize_fuzzy_habitat(species_ontology)
          if (!is.na(fuzzy_ep$class)) {
            traits_harmonized$EP <- fuzzy_ep$class
          }

          # Check if we have at least one trait
          has_traits <- any(!is.na(unlist(traits_harmonized)))

          if (has_traits) {
            training_data[[length(training_data) + 1]] <- data.frame(
              species = species_name,
              phylum = tolower(worms$phylum %||% ""),
              class = tolower(worms$class %||% ""),
              order = tolower(worms$order %||% ""),
              family = tolower(worms$family %||% ""),
              genus = tolower(worms$genus %||% ""),
              MS = traits_harmonized$MS,
              FS = traits_harmonized$FS,
              MB = traits_harmonized$MB,
              EP = traits_harmonized$EP,
              PR = traits_harmonized$PR,
              stringsAsFactors = FALSE
            )
          }
        }
      }

      # Rate limiting
      Sys.sleep(0.5)

    }, error = function(e) {
      # Skip species with errors
    })

    setTxtProgressBar(pb2, i)
  }
  close(pb2)

  cat("\n✓ Added", length(training_data), "total species from cache + ontology\n")
}

if (length(training_data) == 0) {
  stop("No training data found. Please run trait lookups to populate cache, or ensure ontology data has valid species.")
}

# Combine into single data frame
training_df <- do.call(rbind, training_data)

cat("\n✓ Collected training data:\n")
cat("  Total species:", nrow(training_df), "\n")
cat("  Unique phyla:", length(unique(training_df$phylum)), "\n")
cat("  Unique classes:", length(unique(training_df$class)), "\n")
cat("  Unique orders:", length(unique(training_df$order)), "\n")
cat("  Unique families:", length(unique(training_df$family)), "\n")

# Data completeness
cat("\nTrait coverage:\n")
for (trait in c("MS", "FS", "MB", "EP", "PR")) {
  complete <- sum(!is.na(training_df[[trait]]))
  pct <- round(complete / nrow(training_df) * 100, 1)
  cat(sprintf("  %s: %d/%d (%0.1f%%)\n", trait, complete, nrow(training_df), pct))
}

# =============================================================================
# STEP 2: PREPARE FEATURES
# =============================================================================

cat("\n=============================================================================\n")
cat("STEP 2: Feature Engineering\n")
cat("=============================================================================\n\n")

# Convert taxonomy to factors (required for randomForest)
prepare_features <- function(df) {
  df$phylum <- factor(df$phylum)
  df$class <- factor(df$class)
  df$order <- factor(df$order)
  df$family <- factor(df$family)
  df$genus <- factor(df$genus)
  return(df)
}

training_df <- prepare_features(training_df)

cat("Feature summary:\n")
cat("  Phylum levels:", nlevels(training_df$phylum), "\n")
cat("  Class levels:", nlevels(training_df$class), "\n")
cat("  Order levels:", nlevels(training_df$order), "\n")
cat("  Family levels:", nlevels(training_df$family), "\n")
cat("  Genus levels:", nlevels(training_df$genus), "\n")

# =============================================================================
# STEP 3: TRAIN MODELS
# =============================================================================

cat("\n=============================================================================\n")
cat("STEP 3: Training Random Forest Models\n")
cat("=============================================================================\n\n")

models <- list()
performance <- list()

# Features for prediction. Family + genus are dropped because randomForest
# can't handle factor predictors with > 53 levels (we have ~56 families,
# ~76 genera in the cache), and with only ~80 training samples splitting on
# genus would overfit to single-species leaves anyway. phylum/class/order
# generalise better and cover most variance in trait values.
feature_cols <- c("phylum", "class", "order")

# Train model for each trait
for (trait in c("MS", "FS", "MB", "EP", "PR")) {

  cat("\n--- Training", trait, "model ---\n")

  # Filter to complete cases for this trait
  train_data <- training_df[!is.na(training_df[[trait]]), ]

  if (nrow(train_data) < 10) {
    cat("  ⚠️  Insufficient data (", nrow(train_data), " samples) - skipping\n")
    next
  }

  # Convert target to factor
  train_data[[trait]] <- factor(train_data[[trait]])

  cat("  Training samples:", nrow(train_data), "\n")
  cat("  Classes:", nlevels(train_data[[trait]]), "-",
      paste(levels(train_data[[trait]]), collapse = ", "), "\n")

  # Split into train/test (80/20)
  set.seed(42)
  train_idx <- createDataPartition(train_data[[trait]], p = 0.8, list = FALSE)

  train_set <- train_data[train_idx, ]
  test_set <- train_data[-train_idx, ]

  cat("  Train set:", nrow(train_set), "| Test set:", nrow(test_set), "\n")

  # Train Random Forest
  cat("  Training Random Forest...\n")

  formula <- as.formula(paste(trait, "~", paste(feature_cols, collapse = " + ")))

  rf_model <- randomForest(
    formula,
    data = train_set,
    ntree = 500,
    mtry = 3,
    importance = TRUE,
    na.action = na.omit
  )

  # Evaluate on test set
  predictions <- predict(rf_model, test_set)
  confusion <- confusionMatrix(predictions, test_set[[trait]])

  accuracy <- confusion$overall["Accuracy"]
  cat("  ✓ Test accuracy:", round(accuracy * 100, 1), "%\n")

  # Store model and performance
  models[[trait]] <- rf_model
  performance[[trait]] <- list(
    accuracy = accuracy,
    confusion_matrix = confusion$table,
    class_error = rf_model$confusion[, "class.error"],
    variable_importance = importance(rf_model)
  )
}

cat("\n=============================================================================\n")
cat("Model Training Summary\n")
cat("=============================================================================\n\n")

for (trait in names(models)) {
  acc <- performance[[trait]]$accuracy
  cat(sprintf("%-3s: %0.1f%% accuracy\n", trait, acc * 100))
}

# =============================================================================
# STEP 4: SAVE MODELS
# =============================================================================

cat("\n=============================================================================\n")
cat("STEP 4: Saving Models\n")
cat("=============================================================================\n\n")

# Create models directory
models_dir <- "models"
dir.create(models_dir, showWarnings = FALSE, recursive = TRUE)

# Save models
models_file <- file.path(models_dir, "trait_ml_models.rds")

model_package <- list(
  models = models,
  performance = performance,
  feature_cols = feature_cols,
  training_date = Sys.Date(),
  training_samples = nrow(training_df),
  r_version = R.version.string,
  randomForest_version = packageVersion("randomForest"),
  # predict_trait_ml() serves MB only from a model trained on the current
  # trait vocabulary (C-5 gate).
  trait_vocab_version = current_trait_vocab_version()
)

saveRDS(model_package, models_file)

cat("✓ Models saved to:", models_file, "\n")
cat("  File size:", round(file.size(models_file) / 1024, 1), "KB\n")

# =============================================================================
# STEP 5: VALIDATION AND DIAGNOSTICS
# =============================================================================

cat("\n=============================================================================\n")
cat("STEP 5: Model Diagnostics\n")
cat("=============================================================================\n\n")

for (trait in names(models)) {
  cat("\n--- ", trait, " Model ---\n", sep = "")

  model <- models[[trait]]
  perf <- performance[[trait]]

  cat("OOB Error Rate:", round(model$err.rate[model$ntree, "OOB"] * 100, 1), "%\n")

  cat("\nClass-specific error rates:\n")
  for (class_name in rownames(model$confusion)) {
    if (class_name == "class.error") next
    err <- model$confusion[class_name, "class.error"]
    cat(sprintf("  %s: %0.1f%%\n", class_name, err * 100))
  }

  cat("\nTop 5 most important features:\n")
  imp <- perf$variable_importance[order(-perf$variable_importance[, "MeanDecreaseAccuracy"]), ]
  for (i in 1:min(5, nrow(imp))) {
    cat(sprintf("  %d. %s (importance: %0.3f)\n",
                i, rownames(imp)[i], imp[i, "MeanDecreaseAccuracy"]))
  }
}

cat("\n=============================================================================\n")
cat("TRAINING COMPLETE\n")
cat("=============================================================================\n\n")

cat("✓ Successfully trained", length(models), "models\n")
cat("✓ Models saved to:", models_file, "\n")
cat("✓ Ready for production use\n\n")

cat("Next steps:\n")
cat("  1. Review model performance metrics above\n")
cat("  2. Test predictions with: source('R/functions/ml_trait_prediction.R')\n")
cat("  3. Models will be automatically loaded in production\n\n")

cat("=============================================================================\n")
