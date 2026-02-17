# ==============================================================================
# ECOPATH CSV/EXCEL IMPORT
# ==============================================================================
# Parse ECOPATH data exported to Excel or CSV format
#
# Features:
#   - Read Basic Estimates and Diet Composition files
#   - Support for Excel (.xlsx, .xls) and CSV formats
#   - Flexible column name matching
#   - Automatic functional group assignment
#   - Body mass estimation
#
# ==============================================================================

#' Parse ECOPATH Data from Excel/CSV Files
#'
#' Reads ECOPATH Basic Estimates and Diet Composition files exported to Excel or CSV format
#' and converts them into network and species info objects compatible with EcoNeTool.
#'
#' @param basic_est_file Path to Basic Estimates file (Excel or CSV)
#' @param diet_file Path to Diet Composition file (Excel or CSV)
#' @return List with 'net' (igraph object) and 'info' (data.frame)
#' @export
parse_ecopath_data <- function(basic_est_file, diet_file) {
  # Read Basic Estimates file
  basic_ext <- tools::file_ext(basic_est_file)
  if (basic_ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' required for Excel files. Install with: install.packages('readxl')")
    }
    basic_data <- readxl::read_excel(basic_est_file)
  } else if (basic_ext == "csv") {
    basic_data <- read.csv(basic_est_file, stringsAsFactors = FALSE)
  } else {
    stop("Unsupported file format for Basic Estimates")
  }

  # Read Diet Composition file
  diet_ext <- tools::file_ext(diet_file)
  if (diet_ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' required for Excel files. Install with: install.packages('readxl')")
    }
    diet_data <- readxl::read_excel(diet_file)
  } else if (diet_ext == "csv") {
    diet_data <- read.csv(diet_file, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    stop("Unsupported file format for Diet Composition")
  }

  # Clean column names (handle variations in ECOPATH export formats)
  basic_colnames <- tolower(colnames(basic_data))
  colnames(basic_data) <- basic_colnames

  # Find key columns (flexible matching)
  group_col <- which(grepl("group.*name|^name$|^group$", basic_colnames))[1]
  biomass_col <- which(grepl("biomass|^b$", basic_colnames))[1]
  pb_col <- which(grepl("p/b|pb|production", basic_colnames))[1]
  qb_col <- which(grepl("q/b|qb|consumption", basic_colnames))[1]

  if (is.na(group_col) || is.na(biomass_col)) {
    stop("Could not find required columns in Basic Estimates file. Need: Group name, Biomass")
  }

  # Extract species names and biomass
  species_names <- as.character(basic_data[[group_col]])
  species_names <- species_names[!is.na(species_names) & species_names != ""]

  # Remove any summary rows (like "Sum", "Total", etc.)
  valid_rows <- !grepl("^sum$|^total$|^import$|^export$|^detritus$",
                       tolower(species_names))
  species_names <- species_names[valid_rows]
  basic_data <- basic_data[valid_rows, ]

  biomass_values <- as.numeric(basic_data[[biomass_col]])

  # Get P/B and Q/B if available (use defaults from config if missing)
  pb_values <- if (!is.na(pb_col)) as.numeric(basic_data[[pb_col]]) else rep(DEFAULT_PB_RATIO, length(species_names))
  qb_values <- if (!is.na(qb_col)) as.numeric(basic_data[[qb_col]]) else rep(DEFAULT_QB_RATIO, length(species_names))

  # Process Diet Composition matrix
  # First column is prey names, rest are predators
  diet_matrix <- as.matrix(diet_data[, -1])
  rownames(diet_matrix) <- as.character(diet_data[[1]])

  # Match species names between basic and diet files
  common_species <- intersect(species_names, rownames(diet_matrix))
  if (length(common_species) < 2) {
    stop("Species names in Basic Estimates and Diet Composition do not match sufficiently")
  }

  # Filter to common species only
  species_idx <- match(common_species, species_names)
  basic_data <- basic_data[species_idx, ]
  species_names <- common_species
  biomass_values <- biomass_values[species_idx]
  pb_values <- pb_values[species_idx]
  qb_values <- qb_values[species_idx]

  # Subset and reorder diet matrix
  diet_matrix <- diet_matrix[common_species, common_species, drop = FALSE]

  # Convert diet proportions to binary adjacency matrix
  # In ECOPATH: columns are predators, rows are prey
  # In our format: rows are predators, columns are prey
  # So we need to transpose
  adjacency_matrix <- t(diet_matrix > 0) * 1

  # Ensure rownames and colnames are preserved after transpose
  rownames(adjacency_matrix) <- colnames(diet_matrix)
  colnames(adjacency_matrix) <- rownames(diet_matrix)

  # Create igraph network
  net <- igraph::graph_from_adjacency_matrix(adjacency_matrix, mode = "directed")

  # Explicitly set vertex names to ensure they're preserved
  igraph::V(net)$name <- species_names

  # Assign functional groups based on species characteristics
  # Uses the comprehensive assign_functional_group() from functional_group_utils.R
  # which includes patterns for Mammals, Birds, and more detailed classification

  # Calculate degrees for functional group assignment
  indegrees <- igraph::degree(net, mode = "in")
  outdegrees <- igraph::degree(net, mode = "out")

  functional_groups <- sapply(1:length(species_names), function(i) {
    assign_functional_group(
      sp_name = species_names[i],
      pb = pb_values[i],
      indegree = indegrees[i],
      outdegree = outdegrees[i],
      use_topology = TRUE
    )
  })

  # Estimate body masses (rough heuristic based on functional group)
  estimate_body_mass <- function(fg) {
    # Values from config.R - estimates in grams
    if (fg == "Phytoplankton") return(DEFAULT_BODYMASS_PHYTOPLANKTON)
    if (fg == "Zooplankton") return(DEFAULT_BODYMASS_ZOOPLANKTON)
    if (fg == "Benthos") return(DEFAULT_BODYMASS_BENTHOS)
    if (fg == "Fish") return(DEFAULT_BODYMASS_FISH)
    if (fg == "Detritus") return(DEFAULT_BODYMASS_DETRITUS)
    return(DEFAULT_BODYMASS_DEFAULT)
  }

  body_masses <- sapply(functional_groups, estimate_body_mass)

  # Assign metabolic types
  met_types <- sapply(functional_groups, function(fg) {
    if (fg %in% c("Fish")) return("ectotherm vertebrates")
    return("invertebrates")
  })

  # Calculate efficiencies (based on functional group, using config values)
  efficiencies <- sapply(functional_groups, function(fg) {
    if (fg == "Phytoplankton") return(DEFAULT_EFFICIENCY_PHYTOPLANKTON)
    if (fg == "Zooplankton") return(DEFAULT_EFFICIENCY_ZOOPLANKTON)
    if (fg == "Benthos") return(DEFAULT_EFFICIENCY_BENTHOS)
    if (fg == "Fish") return(DEFAULT_EFFICIENCY_FISH)
    if (fg == "Detritus") return(DEFAULT_EFFICIENCY_DETRITUS)
    return(DEFAULT_EFFICIENCY_DEFAULT)
  })

  # Create info data frame
  info <- data.frame(
    meanB = biomass_values,
    fg = factor(functional_groups, levels = c("Benthos", "Detritus", "Fish", "Phytoplankton", "Zooplankton")),
    bodymasses = body_masses,
    met.types = met_types,
    efficiencies = efficiencies,
    PB = pb_values,
    QB = qb_values,
    row.names = species_names,
    stringsAsFactors = FALSE
  )

  return(list(net = net, info = info))
}
