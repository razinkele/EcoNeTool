#' Test Model 608 Conversion Step-by-Step

library(RCurl)
library(XML)
library(igraph)

cat("\n=== Step-by-Step Conversion of Model 608 ===\n\n")

model_id <- 608

# Download
cat("Step 1: Downloading...\n")
h <- basicTextGatherer()
curlPerform(
  url = paste0('http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client.php?no_model=', model_id),
  writefunction = h$update,
  verbose = FALSE
)

cat("Step 2: Parsing XML...\n")
data <- xmlTreeParse(h$value(), useInternalNodes = TRUE)
group_nodes <- getNodeSet(data, "//group")
model_data <- lapply(group_nodes, xmlToList)
cat("  Groups:", length(model_data), "\n")

# Extract parameters
cat("\nStep 3: Extracting parameters...\n")
n_groups <- length(model_data)

species_names <- character(n_groups)
group_seqs <- numeric(n_groups)

for (i in 1:n_groups) {
  group <- model_data[[i]]

  species_names[i] <- if (!is.null(group[["group_name"]])) {
    as.character(group[["group_name"]])
  } else {
    paste0("Group_", i)
  }

  group_seqs[i] <- if (!is.null(group[["group_seq"]])) {
    as.numeric(group[["group_seq"]])
  } else {
    i
  }
}

cat("  Species names:", paste(head(species_names, 5), collapse=", "), "...\n")
cat("  Group seqs:", paste(head(group_seqs, 10), collapse=", "), "...\n")

# Create mapping
cat("\nStep 4: Creating seq-to-index mapping...\n")
seq_to_idx <- setNames(1:n_groups, group_seqs)
cat("  Mapping created with", length(seq_to_idx), "entries\n")
cat("  Sample: seq 1 -> idx", seq_to_idx["1"], "\n")
cat("  Sample: seq 6 -> idx", seq_to_idx["6"], "\n")

# Build diet matrix
cat("\nStep 5: Building diet matrix...\n")
diet_matrix <- matrix(0, nrow = n_groups, ncol = n_groups)
links_added <- 0

for (i in 1:n_groups) {
  group <- model_data[[i]]

  if (!is.null(group[["diet_descr"]])) {
    diet_list <- group[["diet_descr"]]
    if (is.list(diet_list)) {
      for (diet_item in diet_list) {
        if (is.list(diet_item)) {
          prey_seq <- as.numeric(diet_item[["prey_seq"]])
          proportion <- as.numeric(diet_item[["proportion"]])

          if (!is.na(prey_seq) && !is.na(proportion) && proportion > 0) {
            prey_idx <- seq_to_idx[as.character(prey_seq)]

            if (!is.na(prey_idx) && prey_idx > 0 && prey_idx <= n_groups) {
              diet_matrix[prey_idx, i] <- proportion
              links_added <- links_added + 1
            }
          }
        }
      }
    }
  }
}

cat("  Links added:", links_added, "\n")
cat("  Diet matrix non-zero entries:", sum(diet_matrix > 0), "\n")

# Create adjacency matrix
cat("\nStep 6: Creating adjacency matrix...\n")
adjacency_matrix <- (diet_matrix > 0) * 1
rownames(adjacency_matrix) <- colnames(adjacency_matrix) <- species_names
cat("  Adjacency matrix created\n")
cat("  Non-zero entries:", sum(adjacency_matrix), "\n")

# Create network
cat("\nStep 7: Creating igraph network...\n")
tryCatch({
  net <- graph_from_adjacency_matrix(adjacency_matrix, mode = "directed")
  cat("  ✓ Network created!\n")
  cat("  Vertices:", vcount(net), "\n")
  cat("  Edges:", ecount(net), "\n")
}, error = function(e) {
  cat("  ✗ Network creation failed:", e$message, "\n")
})

cat("\n=== END ===\n")
