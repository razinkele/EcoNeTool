#' Examine Raw EcoBase XML Structure
#'
#' This script looks at the actual XML to understand the structure

cat("\n=== EcoBase XML Structure Examination ===\n\n")

library(RCurl)
library(XML)

model_id <- 403
cat("Downloading model", model_id, "...\n")

h <- basicTextGatherer()
curlPerform(
  url = paste0('http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client.php?no_model=', model_id),
  writefunction = h$update,
  verbose = FALSE
)

# Get raw XML
raw_xml <- h$value()

# Show first 3000 characters
cat("\n=== First 3000 characters of XML ===\n\n")
cat(substr(raw_xml, 1, 3000))
cat("\n\n")

# Parse and show structure
cat("=== Parsing XML ===\n\n")
data <- xmlTreeParse(raw_xml, useInternalNodes = TRUE)

# Get root node
root <- xmlRoot(data)
cat("Root node name:", xmlName(root), "\n")
cat("Root node children:", length(xmlChildren(root)), "\n\n")

# Get all group nodes
group_nodes <- getNodeSet(data, "//group")
cat("Number of <group> nodes:", length(group_nodes), "\n\n")

if (length(group_nodes) > 0) {
  cat("=== First group node ===\n")
  first_group_node <- group_nodes[[1]]

  cat("Node name:", xmlName(first_group_node), "\n")
  cat("Number of children:", length(xmlChildren(first_group_node)), "\n")

  # Show all child element names
  cat("\nChild elements:\n")
  children <- xmlChildren(first_group_node)
  for (i in 1:min(20, length(children))) {
    child <- children[[i]]
    child_name <- xmlName(child)
    child_value <- xmlValue(child)
    cat("  ", i, ".", child_name, "=", substr(child_value, 1, 50), "\n")
  }

  cat("\n=== Converting first group with xmlToList ===\n")
  first_group_list <- xmlToList(first_group_node)
  cat("Class:", class(first_group_list), "\n")
  cat("Length:", length(first_group_list), "\n")
  cat("Names:", paste(head(names(first_group_list), 20), collapse = ", "), "\n\n")

  # Show some values
  cat("Sample values:\n")
  for (field in head(names(first_group_list), 15)) {
    value <- first_group_list[[field]]
    if (is.list(value)) {
      cat("  ", field, ": [LIST]\n")
    } else {
      cat("  ", field, ":", value, "\n")
    }
  }
}

cat("\n=== END ===\n")
