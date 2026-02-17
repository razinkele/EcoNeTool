# Simple ECOPATH Network Test
# Tests import, adjacency matrix, and basic network structure

cat("================================================================================\n")
cat("SIMPLE ECOPATH NETWORK TEST\n")
cat("================================================================================\n\n")

# Load required libraries
library(igraph)
source("ecopath_windows_import.R")

# Test file
db_file <- "coast 2011-04-10 10.00.ewemdb"

cat("Testing:", db_file, "\n\n")

# Step 1: Import
cat("STEP 1: Import ECOPATH database\n")
cat(rep("-", 80), "\n", sep = "")

import_result <- parse_ecopath_native_cross_platform(db_file)

cat("✓ Import successful\n")
cat("  Groups:", nrow(import_result$group_data), "\n")
cat("  Diet links:", nrow(import_result$diet_data), "\n\n")

# Step 2: Build adjacency matrix manually
cat("STEP 2: Build adjacency matrix from diet data\n")
cat(rep("-", 80), "\n", sep = "")

group_data <- import_result$group_data
diet_data <- import_result$diet_data

# Get species names
species_names <- group_data$GroupName
n_species <- length(species_names)

cat("  Number of species:", n_species, "\n")
cat("  Species names (first 10):\n")
print(head(species_names, 10))
cat("\n")

# Create adjacency matrix
adj_matrix <- matrix(0, nrow = n_species, ncol = n_species)
rownames(adj_matrix) <- species_names
colnames(adj_matrix) <- species_names

cat("  Building adjacency matrix from diet data...\n")

# Fill matrix from diet data
links_added <- 0
for (i in 1:nrow(diet_data)) {
  pred_id <- diet_data$PredID[i]
  prey_id <- diet_data$PreyID[i]
  diet_value <- diet_data$Diet[i]

  # Skip if diet value is 0 or NA
  if (is.na(diet_value) || diet_value == 0) next

  # Find indices
  pred_idx <- which(group_data$GroupID == pred_id)
  prey_idx <- which(group_data$GroupID == prey_id)

  if (length(pred_idx) == 1 && length(prey_idx) == 1) {
    # Adjacency matrix: 1 if predator eats prey
    adj_matrix[prey_idx, pred_idx] <- 1
    links_added <- links_added + 1
  }
}

cat("  Links added to matrix:", links_added, "\n")
cat("  Matrix dimensions:", dim(adj_matrix), "\n")
cat("  Sum of matrix:", sum(adj_matrix), "\n\n")

# Show sample of matrix
cat("  Sample adjacency matrix (first 8x8):\n")
print(adj_matrix[1:min(8, nrow(adj_matrix)), 1:min(8, ncol(adj_matrix))])
cat("\n")

# Step 3: Create igraph network
cat("STEP 3: Create igraph network from adjacency matrix\n")
cat(rep("-", 80), "\n", sep = "")

net <- graph_from_adjacency_matrix(adj_matrix, mode = "directed")

cat("  Network created!\n")
cat("  Is igraph:", is_igraph(net), "\n")
cat("  Vertices:", vcount(net), "\n")
cat("  Edges:", ecount(net), "\n")
cat("  Directed:", is_directed(net), "\n")
cat("  Has vertex names:", !is.null(V(net)$name), "\n\n")

# Show vertex names
cat("  Vertex names (first 10):\n")
print(head(V(net)$name, 10))
cat("\n")

# Step 4: Verify adjacency matrix from network
cat("STEP 4: Verify adjacency matrix can be extracted from network\n")
cat(rep("-", 80), "\n", sep = "")

adj_from_net <- as_adjacency_matrix(net, sparse = FALSE)

cat("  Extracted adjacency matrix dimensions:", dim(adj_from_net), "\n")
cat("  Sum of extracted matrix:", sum(adj_from_net), "\n")
cat("  Matches original matrix:", identical(adj_matrix, adj_from_net), "\n\n")

# Step 5: Check edge list
cat("STEP 5: Check edge list\n")
cat(rep("-", 80), "\n", sep = "")

edge_list <- as_edgelist(net)

cat("  Edge list dimensions:", dim(edge_list), "\n")
cat("  Sample edges (first 10):\n")
print(head(edge_list, 10))
cat("\n")

# Step 6: Summary
cat("STEP 6: Summary\n")
cat(rep("=", 80), "\n", sep = "")

tests <- list(
  "Import successful" = TRUE,
  "Adjacency matrix built" = sum(adj_matrix) > 0,
  "Network created" = is_igraph(net),
  "Vertices match groups" = vcount(net) == n_species,
  "Edges created" = ecount(net) > 0,
  "Vertex names assigned" = !is.null(V(net)$name),
  "Adjacency extractable" = sum(adj_from_net) > 0
)

cat("\nTest Results:\n")
for (test_name in names(tests)) {
  result <- tests[[test_name]]
  symbol <- if(result) "✓" else "✗"
  cat("  ", symbol, test_name, "\n")
}

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("Overall Status:", if(all(unlist(tests))) "✓ ALL TESTS PASSED" else "✗ SOME TESTS FAILED", "\n")
cat(rep("=", 80), "\n", sep = "")
