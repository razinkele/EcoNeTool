#' EcoBase Connection Module
#'
#' Functions for connecting to EcoBase web service and retrieving Ecopath models
#' EcoBase: http://sirs.agrocampus-ouest.fr/EcoBase/
#'
#' @description
#' This module provides functions to:
#' - List available Ecopath models in EcoBase
#' - Download model input parameters
#' - Download model output parameters
#' - Convert EcoBase models to EcoNeTool format

# Required packages
require_ecobase_packages <- function() {
  packages <- c("RCurl", "XML", "plyr", "dplyr")
  missing <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]

  if (length(missing) > 0) {
    stop(
      "EcoBase connection requires additional packages.\n",
      "Please install: ", paste(missing, collapse = ", "), "\n\n",
      "Install with: install.packages(c('", paste(missing, collapse = "', '"), "'))"
    )
  }

  invisible(TRUE)
}

#' Get List of Available EcoBase Models
#'
#' Retrieves the list of publicly available Ecopath models from EcoBase
#'
#' @return data.frame with model information (model ID, name, description, etc.)
#' @export
#' @examples
#' \dontrun{
#' models <- get_ecobase_models()
#' head(models)
#' }
get_ecobase_models <- function() {
  # Check packages
  require_ecobase_packages()

  library(RCurl)
  library(XML)
  library(plyr)
  library(dplyr)

  message("Connecting to EcoBase...")

  tryCatch({
    # Fetch model list from EcoBase
    h <- basicTextGatherer()
    curlPerform(
      url = 'http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client_3.php',
      writefunction = h$update
    )

    # Parse XML response
    data <- xmlTreeParse(h$value(), useInternalNodes = TRUE)

    # Convert to data frame
    liste_mod <- ldply(xmlToList(data), data.frame) %>%
      filter(model.dissemination_allow == 'true')

    message("✓ Retrieved ", nrow(liste_mod), " available models from EcoBase")

    return(liste_mod)

  }, error = function(e) {
    stop("Failed to retrieve EcoBase models: ", e$message, "\n",
         "Check internet connection and EcoBase availability.")
  })
}

#' Get EcoBase Model Input Parameters
#'
#' Downloads input parameters for a specific model from EcoBase
#'
#' @param model_id Numeric model ID from EcoBase
#' @return List of input parameters for the model
#' @export
#' @examples
#' \dontrun{
#' input_data <- get_ecobase_model_input(403)
#' }
get_ecobase_model_input <- function(model_id) {
  # Check packages
  require_ecobase_packages()

  library(RCurl)
  library(XML)
  library(plyr)

  message("Downloading input parameters for model ", model_id, "...")

  tryCatch({
    # Fetch model input
    h <- basicTextGatherer()
    curlPerform(
      url = paste0('http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client.php?no_model=', model_id),
      writefunction = h$update,
      verbose = FALSE
    )

    # Parse XML response
    data <- xmlTreeParse(h$value(), useInternalNodes = TRUE)

    # Extract group nodes and convert to list
    group_nodes <- getNodeSet(data, "//group")
    input_data <- lapply(group_nodes, xmlToList)

    message("✓ Retrieved input parameters (", length(input_data), " groups)")

    return(input_data)

  }, error = function(e) {
    stop("Failed to retrieve model input: ", e$message)
  })
}

#' Get EcoBase Model Output Parameters
#'
#' Downloads output parameters for a specific model from EcoBase
#'
#' @param model_id Numeric model ID from EcoBase
#' @return List of output parameters for the model
#' @export
#' @examples
#' \dontrun{
#' output_data <- get_ecobase_model_output(403)
#' }
get_ecobase_model_output <- function(model_id) {
  # Check packages
  require_ecobase_packages()

  library(RCurl)
  library(XML)
  library(plyr)

  message("Downloading output parameters for model ", model_id, "...")

  tryCatch({
    # Fetch model output
    h <- basicTextGatherer()
    curlPerform(
      url = paste0('http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client_output.php?no_model=', model_id),
      writefunction = h$update,
      verbose = FALSE
    )

    # Parse XML response
    data <- xmlTreeParse(h$value(), useInternalNodes = TRUE)

    # Extract group nodes and convert to list
    group_nodes <- getNodeSet(data, "//group")
    output_data <- lapply(group_nodes, xmlToList)

    message("✓ Retrieved output parameters (", length(output_data), " groups)")

    return(output_data)

  }, error = function(e) {
    stop("Failed to retrieve model output: ", e$message)
  })
}

#' Get EcoBase Model Metadata
#'
#' Extracts metadata (location, time period, author, etc.) from EcoBase model
#'
#' @param model_id Numeric model ID from EcoBase
#' @return List of metadata fields
#' @export
#' @examples
#' \dontrun{
#' metadata <- get_ecobase_model_metadata(608)
#' }
get_ecobase_model_metadata <- function(model_id) {
  # Check packages
  require_ecobase_packages()

  library(RCurl)
  library(XML)

  message("Downloading metadata for model ", model_id, "...")

  tryCatch({
    # Fetch model data (use INPUT endpoint as it has model_descr)
    h <- basicTextGatherer()
    curlPerform(
      url = paste0('http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client.php?no_model=', model_id),
      writefunction = h$update,
      verbose = FALSE
    )

    # Parse XML response
    data <- xmlTreeParse(h$value(), useInternalNodes = TRUE)

    # Extract model_descr section
    model_descr_nodes <- getNodeSet(data, "//model_descr")

    if (length(model_descr_nodes) > 0) {
      model_descr <- xmlToList(model_descr_nodes[[1]])

      # Extract relevant metadata fields
      metadata <- list(
        # Basic identification
        model_name = if (!is.null(model_descr$model_name)) model_descr$model_name else NA,
        ecosystem_name = if (!is.null(model_descr$ecosystem_name)) model_descr$ecosystem_name else NA,
        description = if (!is.null(model_descr$description)) model_descr$description else NA,

        # Geographic info
        country = if (!is.null(model_descr$country)) model_descr$country else NA,
        region = if (!is.null(model_descr$region)) model_descr$region else NA,
        ecosystem_type = if (!is.null(model_descr$ecosystem_type)) model_descr$ecosystem_type else NA,
        area = if (!is.null(model_descr$area)) model_descr$area else NA,
        latitude = if (!is.null(model_descr$latitude)) model_descr$latitude else NA,
        longitude = if (!is.null(model_descr$longitude)) model_descr$longitude else NA,

        # Temporal info
        model_year = if (!is.null(model_descr$model_year)) model_descr$model_year else NA,
        model_period = if (!is.null(model_descr$model_period)) model_descr$model_period else NA,

        # Attribution
        author = if (!is.null(model_descr$author)) model_descr$author else NA,
        contact = if (!is.null(model_descr$contact)) model_descr$contact else NA,
        institution = if (!is.null(model_descr$institution)) model_descr$institution else NA,

        # Publication references
        publication = if (!is.null(model_descr$publication)) model_descr$publication else NA,
        doi = if (!is.null(model_descr$doi)) model_descr$doi else NA,

        # Technical
        model_number = if (!is.null(model_descr$model_number)) model_descr$model_number else NA,
        last_modified = if (!is.null(model_descr$last_modified)) model_descr$last_modified else NA
      )

      message("  → Extracted metadata: ", metadata$model_name)

      return(metadata)
    } else {
      message("  → No metadata found")
      return(NULL)
    }

  }, error = function(e) {
    message("  → Could not read metadata: ", e$message)
    return(NULL)
  })
}

#' Convert EcoBase Model to EcoNeTool Format
#'
#' Converts EcoBase model data to EcoNeTool network format
#'
#' @param model_id Numeric model ID from EcoBase
#' @param use_output Logical, if TRUE use output parameters, if FALSE use input parameters
#' @return List with 'net' (igraph) and 'info' (data.frame)
#' @export
#' @examples
#' \dontrun{
#' model <- convert_ecobase_to_econetool(403)
#' }
convert_ecobase_to_econetool <- function(model_id, use_output = TRUE) {
  library(igraph)

  message("Converting EcoBase model ", model_id, " to EcoNeTool format...")

  # Get model data
  if (use_output) {
    model_data <- get_ecobase_model_output(model_id)
  } else {
    model_data <- get_ecobase_model_input(model_id)
  }

  # Get metadata
  metadata <- get_ecobase_model_metadata(model_id)

  # Extract species names and parameters
  n_groups <- length(model_data)

  species_names <- character(n_groups)
  group_seqs <- numeric(n_groups)  # To map prey_seq to species index
  biomass_values <- numeric(n_groups)
  pb_values <- numeric(n_groups)
  qb_values <- numeric(n_groups)
  ee_values <- numeric(n_groups)

  # Diet matrix to store trophic links
  diet_matrix <- matrix(0, nrow = n_groups, ncol = n_groups)

  # First pass: Extract group info and build seq-to-index mapping
  for (i in 1:n_groups) {
    group <- model_data[[i]]

    # Extract group name (using [[ ]] to work with atomic vectors)
    species_names[i] <- if (!is.null(group[["group_name"]])) {
      as.character(group[["group_name"]])
    } else if (!is.null(group[["name"]])) {
      as.character(group[["name"]])
    } else {
      paste0("Group_", i)
    }

    # Extract group_seq for diet mapping
    group_seqs[i] <- if (!is.null(group[["group_seq"]])) {
      as.numeric(group[["group_seq"]])
    } else {
      i
    }

    # Extract biomass
    biomass_values[i] <- if (!is.null(group[["biomass"]])) {
      as.numeric(group[["biomass"]])
    } else if (!is.null(group[["B"]])) {
      as.numeric(group[["B"]])
    } else {
      1
    }

    # Extract P/B
    pb_values[i] <- if (!is.null(group[["pb"]])) {
      as.numeric(group[["pb"]])
    } else if (!is.null(group[["PB"]])) {
      as.numeric(group[["PB"]])
    } else {
      0.5
    }

    # Extract Q/B
    qb_values[i] <- if (!is.null(group[["qb"]])) {
      as.numeric(group[["qb"]])
    } else if (!is.null(group[["QB"]])) {
      as.numeric(group[["QB"]])
    } else {
      1.5
    }

    # Extract EE
    ee_values[i] <- if (!is.null(group[["ee"]])) {
      as.numeric(group[["ee"]])
    } else if (!is.null(group[["EE"]])) {
      as.numeric(group[["EE"]])
    } else {
      0.5
    }
  }

  # Create mapping from group_seq to species index
  seq_to_idx <- setNames(1:n_groups, group_seqs)

  # Second pass: Extract diet composition
  links_added <- 0
  for (i in 1:n_groups) {
    group <- model_data[[i]]

    # Extract diet composition from diet_descr
    if (!is.null(group[["diet_descr"]])) {
      diet_list <- group[["diet_descr"]]
      if (is.list(diet_list)) {
        for (diet_item in diet_list) {
          if (is.list(diet_item)) {
            prey_seq <- as.numeric(diet_item[["prey_seq"]])
            proportion <- as.numeric(diet_item[["proportion"]])

            if (!is.na(prey_seq) && !is.na(proportion) && proportion > 0) {
              # Map prey_seq to species index
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

  message("  Added ", links_added, " diet links from diet_descr")

  # Clean negative or missing values
  biomass_values[is.na(biomass_values) | biomass_values < 0] <- 1
  pb_values[is.na(pb_values) | pb_values < 0] <- 0.5
  qb_values[is.na(qb_values) | qb_values < 0] <- 1.5
  ee_values[is.na(ee_values) | ee_values < 0 | ee_values > 1] <- 0.5

  # Create adjacency matrix (binary: 1 if diet > 0)
  adjacency_matrix <- (diet_matrix > 0) * 1
  rownames(adjacency_matrix) <- colnames(adjacency_matrix) <- species_names

  # Create network
  net <- graph_from_adjacency_matrix(adjacency_matrix, mode = "directed")

  # Explicitly set vertex names to ensure they're preserved
  V(net)$name <- species_names

  # Assign functional groups using shared utility (name-based only for EcoBase)
  functional_groups <- assign_functional_groups(
    species_names,
    pb_values = NULL,
    indegrees = NULL,
    outdegrees = NULL,
    use_topology = FALSE  # EcoBase uses name-based only
  )

  # Create info data frame
  info <- data.frame(
    species = species_names,
    fg = factor(functional_groups),
    meanB = biomass_values,
    PB = pb_values,
    QB = qb_values,
    EE = ee_values,
    bodymasses = biomass_values * 100,  # Rough estimate
    losses = 0.1,  # Default
    efficiencies = 0.8,  # Default
    stringsAsFactors = FALSE
  )

  message("✓ Conversion complete:")
  message("  - Species/groups: ", vcount(net))
  message("  - Trophic links: ", ecount(net))
  message("  - Functional groups: ", nlevels(info$fg))

  return(list(
    net = net,
    info = info,
    metadata = metadata
  ))
}

#' Convert EcoBase Model with Hybrid Approach
#'
#' Uses Output parameters for species data (balanced) and Input parameters for diet (links)
#' This combines the best of both: balanced parameters + complete trophic structure
#'
#' @param model_id Numeric model ID from EcoBase
#' @return List with 'net' (igraph) and 'info' (data.frame)
#' @export
convert_ecobase_to_econetool_hybrid <- function(model_id) {
  library(igraph)

  message("Converting EcoBase model ", model_id, " with HYBRID approach...")
  message("  - Species parameters from OUTPUT (balanced)")
  message("  - Diet composition from INPUT (complete)")

  # Get OUTPUT data for species parameters
  output_data <- get_ecobase_model_output(model_id)
  n_groups <- length(output_data)

  # Get metadata
  metadata <- get_ecobase_model_metadata(model_id)

  # Extract species info from OUTPUT (balanced parameters)
  species_names <- character(n_groups)
  group_seqs <- numeric(n_groups)
  biomass_values <- numeric(n_groups)
  pb_values <- numeric(n_groups)
  qb_values <- numeric(n_groups)
  ee_values <- numeric(n_groups)

  for (i in 1:n_groups) {
    group <- output_data[[i]]

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

    biomass_values[i] <- if (!is.null(group[["biomass"]])) {
      as.numeric(group[["biomass"]])
    } else if (!is.null(group[["B"]])) {
      as.numeric(group[["B"]])
    } else {
      1
    }

    pb_values[i] <- if (!is.null(group[["pb"]])) {
      as.numeric(group[["pb"]])
    } else if (!is.null(group[["PB"]])) {
      as.numeric(group[["PB"]])
    } else {
      0.5
    }

    qb_values[i] <- if (!is.null(group[["qb"]])) {
      as.numeric(group[["qb"]])
    } else if (!is.null(group[["QB"]])) {
      as.numeric(group[["QB"]])
    } else {
      1.5
    }

    ee_values[i] <- if (!is.null(group[["ee"]])) {
      as.numeric(group[["ee"]])
    } else if (!is.null(group[["EE"]])) {
      as.numeric(group[["EE"]])
    } else {
      0.5
    }
  }

  # Clean negative or missing values
  biomass_values[is.na(biomass_values) | biomass_values < 0] <- 1
  pb_values[is.na(pb_values) | pb_values < 0] <- 0.5
  qb_values[is.na(qb_values) | qb_values < 0] <- 1.5
  ee_values[is.na(ee_values) | ee_values < 0 | ee_values > 1] <- 0.5

  # Get INPUT data for diet composition
  input_data <- get_ecobase_model_input(model_id)

  # Build diet matrix from INPUT
  diet_matrix <- matrix(0, nrow = n_groups, ncol = n_groups)
  seq_to_idx <- setNames(1:n_groups, group_seqs)

  links_added <- 0
  for (i in 1:length(input_data)) {
    group <- input_data[[i]]

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

  message("  Added ", links_added, " diet links from INPUT parameters")

  # Create adjacency matrix
  adjacency_matrix <- (diet_matrix > 0) * 1
  rownames(adjacency_matrix) <- colnames(adjacency_matrix) <- species_names

  # Create network
  net <- graph_from_adjacency_matrix(adjacency_matrix, mode = "directed")

  # Explicitly set vertex names to ensure they're preserved
  V(net)$name <- species_names

  # Assign functional groups using shared utility (name-based only for EcoBase)
  functional_groups <- assign_functional_groups(
    species_names,
    pb_values = NULL,
    indegrees = NULL,
    outdegrees = NULL,
    use_topology = FALSE  # EcoBase uses name-based only
  )

  # Create info data frame
  info <- data.frame(
    species = species_names,
    fg = factor(functional_groups),
    meanB = biomass_values,
    PB = pb_values,
    QB = qb_values,
    EE = ee_values,
    bodymasses = biomass_values * 100,
    losses = 0.1,
    efficiencies = 0.8,
    stringsAsFactors = FALSE
  )

  message("✓ HYBRID conversion complete:")
  message("  - Species/groups: ", vcount(net))
  message("  - Trophic links: ", ecount(net))
  message("  - Functional groups: ", nlevels(info$fg))
  message("  - Parameters from: OUTPUT (balanced)")
  message("  - Diet links from: INPUT (complete)")

  return(list(
    net = net,
    info = info,
    metadata = metadata
  ))
}

#' Test EcoBase Connection
#'
#' Tests connection to EcoBase and retrieves a sample model
#'
#' @return Logical, TRUE if connection successful
#' @export
test_ecobase_connection <- function() {
  message("Testing EcoBase connection...")

  tryCatch({
    # Check packages
    require_ecobase_packages()
    message("✓ Required packages available")

    # Try to get model list
    models <- get_ecobase_models()
    message("✓ Successfully retrieved ", nrow(models), " models")

    # Show sample models
    message("\nSample models:")
    sample_models <- head(models[, c("model.model_number", "model.model_name", "model.ecosystem_name")], 5)
    print(sample_models)

    return(TRUE)

  }, error = function(e) {
    message("✗ Connection test failed: ", e$message)
    return(FALSE)
  })
}
