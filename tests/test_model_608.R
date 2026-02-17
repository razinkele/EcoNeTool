#' Test Model 608 - Central Baltic Sea
#' Investigate why it has no trophic links

library(RCurl)
library(XML)

cat("\n=== Testing Model 608 (Central Baltic Sea) ===\n\n")

model_id <- 608

# Download input data
cat("Downloading input parameters...\n")
h <- basicTextGatherer()
curlPerform(
  url = paste0('http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client.php?no_model=', model_id),
  writefunction = h$update,
  verbose = FALSE
)

cat("Parsing XML...\n")
data <- xmlTreeParse(h$value(), useInternalNodes = TRUE)

# Extract groups
cat("Extracting groups...\n")
group_nodes <- getNodeSet(data, "//group")
input_data <- lapply(group_nodes, xmlToList)

cat("✓ Got", length(input_data), "groups\n\n")

# Check diet data
cat("Checking diet data in groups:\n\n")

groups_with_diet <- 0
total_diet_items <- 0

for (i in 1:min(length(input_data), 20)) {
  group <- input_data[[i]]

  if (is.list(group)) {
    group_name <- group[["group_name"]]
    if (is.null(group_name)) group_name <- paste0("Group_", i)

    # Check for diet
    if (!is.null(group[["diet_descr"]])) {
      diet_list <- group[["diet_descr"]]
      if (is.list(diet_list) && length(diet_list) > 0) {
        groups_with_diet <- groups_with_diet + 1
        n_items <- length(diet_list)
        total_diet_items <- total_diet_items + n_items

        cat(i, ".", group_name, "- Diet items:", n_items, "\n")

        # Show first diet item
        if (n_items > 0) {
          first_item <- diet_list[[1]]
          if (is.list(first_item)) {
            cat("     First item: prey_seq=", first_item[["prey_seq"]],
                ", proportion=", first_item[["proportion"]], "\n")
          }
        }
      }
    }
  }
}

cat("\n=== Summary ===\n")
cat("Total groups:", length(input_data), "\n")
cat("Groups with diet data:", groups_with_diet, "\n")
cat("Total diet items:", total_diet_items, "\n")

# Check group_seq values
cat("\n=== Checking group_seq values ===\n")
group_seqs <- sapply(input_data, function(g) {
  if (is.list(g) && !is.null(g[["group_seq"]])) {
    return(as.numeric(g[["group_seq"]]))
  } else {
    return(NA)
  }
})

cat("group_seq values:", paste(head(group_seqs, 20), collapse=", "), "...\n")
cat("Min:", min(group_seqs, na.rm=TRUE), "\n")
cat("Max:", max(group_seqs, na.rm=TRUE), "\n")
cat("Number of groups:", length(input_data), "\n")

cat("\n=== END ===\n")
