# Test ECOPATH Network Import and Conversion
# This script tests the full pipeline from ECOPATH import to network creation

library(igraph)

#' Test ECOPATH Network Import
#'
#' @param db_file Path to ECOPATH database file
#' @return List with test results and diagnostics
test_ecopath_network <- function(db_file = "coast 2011-04-10 10.00.ewemdb") {

  cat("================================================================================\n")
  cat("ECOPATH NETWORK IMPORT TEST\n")
  cat("================================================================================\n\n")

  # Step 1: Import ECOPATH data
  cat("STEP 1: Importing ECOPATH database\n")
  cat("File:", db_file, "\n")
  cat(rep("-", 80), "\n", sep = "")

  source("ecopath_windows_import.R")

  tryCatch({
    import_result <- parse_ecopath_native_cross_platform(db_file)
    cat("✓ Import successful\n")
    cat("  Method:", import_result$method, "\n")
    cat("  Groups:", nrow(import_result$group_data), "\n")
    cat("  Diet links:", nrow(import_result$diet_data), "\n\n")
  }, error = function(e) {
    cat("✗ Import failed:", e$message, "\n\n")
    return(list(success = FALSE, error = e$message))
  })

  # Step 2: Check group data structure
  cat("STEP 2: Checking group data structure\n")
  cat(rep("-", 80), "\n", sep = "")

  group_data <- import_result$group_data
  cat("  Rows:", nrow(group_data), "\n")
  cat("  Columns:", ncol(group_data), "\n")
  cat("  Column names:\n")
  print(colnames(group_data))
  cat("\n")

  # Check for required columns
  required_cols <- c("GroupID", "GroupName", "Type", "Biomass")
  missing <- setdiff(required_cols, colnames(group_data))
  if (length(missing) > 0) {
    cat("✗ Missing required columns:", paste(missing, collapse = ", "), "\n\n")
  } else {
    cat("✓ All required columns present\n\n")
  }

  # Step 3: Check diet data structure
  cat("STEP 3: Checking diet data structure\n")
  cat(rep("-", 80), "\n", sep = "")

  diet_data <- import_result$diet_data
  cat("  Rows:", nrow(diet_data), "\n")
  cat("  Columns:", ncol(diet_data), "\n")
  cat("  Column names:\n")
  print(colnames(diet_data))
  cat("\n")

  # Check diet data integrity
  cat("  Sample diet entries:\n")
  print(head(diet_data[, c("PredID", "PreyID", "Diet")], 10))
  cat("\n")

  # Step 4: Parse to network format
  cat("STEP 4: Converting to network format\n")
  cat(rep("-", 80), "\n", sep = "")

  source("app.R", local = TRUE)  # Load parse_ecopath_native function

  tryCatch({
    result <- parse_ecopath_native(db_file)
    cat("✓ Network conversion successful\n")
    cat("  Network class:", class(result$net), "\n")
    cat("  Info class:", class(result$info), "\n\n")
  }, error = function(e) {
    cat("✗ Network conversion failed:", e$message, "\n\n")
    return(list(success = FALSE, error = e$message))
  })

  # Step 5: Check network object
  cat("STEP 5: Checking network object\n")
  cat(rep("-", 80), "\n", sep = "")

  net <- result$net
  info <- result$info

  cat("  Is igraph:", igraph::is_igraph(net), "\n")
  cat("  Vertices:", vcount(net), "\n")
  cat("  Edges:", ecount(net), "\n")
  cat("  Directed:", is_directed(net), "\n\n")

  # Check vertex names
  cat("  Vertex names (first 10):\n")
  print(head(V(net)$name, 10))
  cat("\n")

  # Step 6: Check adjacency matrix
  cat("STEP 6: Checking adjacency matrix\n")
  cat(rep("-", 80), "\n", sep = "")

  adj_matrix <- as_adjacency_matrix(net, sparse = FALSE)
  cat("  Adjacency matrix dimensions:", dim(adj_matrix), "\n")
  cat("  Row names present:", !is.null(rownames(adj_matrix)), "\n")
  cat("  Column names present:", !is.null(colnames(adj_matrix)), "\n")
  cat("  Sum of matrix (total links):", sum(adj_matrix), "\n")
  cat("  Non-zero entries:", sum(adj_matrix > 0), "\n\n")

  # Show sample of adjacency matrix
  cat("  Sample adjacency matrix (first 5x5):\n")
  print(adj_matrix[1:min(5, nrow(adj_matrix)), 1:min(5, ncol(adj_matrix))])
  cat("\n")

  # Step 7: Check info data frame
  cat("STEP 7: Checking info data frame\n")
  cat(rep("-", 80), "\n", sep = "")

  cat("  Rows:", nrow(info), "\n")
  cat("  Columns:", ncol(info), "\n")
  cat("  Column names:\n")
  print(colnames(info))
  cat("\n")

  cat("  Sample info data:\n")
  print(head(info[, c("species", "fg", "meanB")], 10))
  cat("\n")

  # Check for NAs
  cat("  NA counts by column:\n")
  na_counts <- sapply(info, function(x) sum(is.na(x)))
  print(na_counts[na_counts > 0])
  cat("\n")

  # Step 8: Check functional groups
  cat("STEP 8: Checking functional groups\n")
  cat(rep("-", 80), "\n", sep = "")

  cat("  Number of functional groups:", nlevels(info$fg), "\n")
  cat("  Functional group levels:", levels(info$fg), "\n")
  cat("  Distribution:\n")
  print(table(info$fg))
  cat("\n")

  # Step 9: Test metaweb conversion
  cat("STEP 9: Testing metaweb conversion\n")
  cat(rep("-", 80), "\n", sep = "")

  tryCatch({
    # Create species data frame for metaweb
    species_data <- data.frame(
      species_id = V(net)$name,
      species_name = V(net)$name,
      functional_group = as.character(info$fg),
      biomass = info$meanB,
      stringsAsFactors = FALSE
    )

    # Create interactions data frame
    edges <- as_edgelist(net)
    interactions_data <- data.frame(
      predator_id = edges[,1],
      prey_id = edges[,2],
      quality_code = 3,
      source = paste0("ECOPATH: ", basename(db_file)),
      stringsAsFactors = FALSE
    )

    cat("✓ Metaweb data frames created\n")
    cat("  Species data rows:", nrow(species_data), "\n")
    cat("  Interactions rows:", nrow(interactions_data), "\n\n")

    cat("  Sample species data:\n")
    print(head(species_data, 5))
    cat("\n")

    cat("  Sample interactions:\n")
    print(head(interactions_data, 10))
    cat("\n")

  }, error = function(e) {
    cat("✗ Metaweb conversion failed:", e$message, "\n\n")
  })

  # Step 10: Summary
  cat("STEP 10: Summary\n")
  cat(rep("=", 80), "\n", sep = "")

  tests <- list(
    "ECOPATH import" = TRUE,
    "Network conversion" = igraph::is_igraph(net),
    "Adjacency matrix formed" = sum(adj_matrix) > 0,
    "Info data frame valid" = nrow(info) > 0,
    "Functional groups assigned" = nlevels(info$fg) > 0,
    "Metaweb conversion" = exists("species_data") && exists("interactions_data")
  )

  cat("\nTest Results:\n")
  for (test_name in names(tests)) {
    result <- tests[[test_name]]
    symbol <- if(result) "✓" else "✗"
    cat("  ", symbol, test_name, "\n")
  }
  cat("\n")

  # Return results
  return(invisible(list(
    success = all(unlist(tests)),
    net = net,
    info = info,
    adj_matrix = adj_matrix,
    species_data = if(exists("species_data")) species_data else NULL,
    interactions_data = if(exists("interactions_data")) interactions_data else NULL,
    tests = tests
  )))
}

# Run test if called directly
if (!interactive()) {
  test_ecopath_network()
}
