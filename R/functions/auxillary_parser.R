# ==============================================================================
# AUXILLARY DATA PARSER
# ==============================================================================
# Functions to parse and organize tooltips/comments from Auxillary table
#
# The Auxillary table stores tooltips/comments/remarks for parameter values
# ValueID format examples:
#   - "EcoPathGroupInput:6:PBInput" - P/B for group 6
#   - "EcoPathGroupInput:11:DietComp:EcoPathGroupInput:6" - Diet comp for group 11 eating group 6
#   - "EcoPathFleetInput:1:FixedCost" - Fixed cost for fleet 1
# ==============================================================================

parse_auxillary_valueid <- function(value_id) {
  #' Parse ValueID from Auxillary table
  #'
  #' Extracts entity type, entity ID, and parameter name from ValueID string
  #'
  #' @param value_id String in format "EntityType:ID:Parameter" or similar
  #' @return List with entity_type, entity_id, parameter, and full_info
  #' @export

  if (is.na(value_id) || value_id == "") {
    return(list(
      entity_type = NA,
      entity_id = NA,
      parameter = NA,
      full_info = NA
    ))
  }

  # Split by colon
  parts <- strsplit(value_id, ":", fixed = TRUE)[[1]]

  if (length(parts) < 2) {
    return(list(
      entity_type = NA,
      entity_id = NA,
      parameter = NA,
      full_info = value_id
    ))
  }

  # Parse based on pattern
  entity_type <- parts[1]
  entity_id <- if (length(parts) >= 2) parts[2] else NA
  parameter <- if (length(parts) >= 3) parts[3] else NA

  # Simplify entity type names
  entity_type_simple <- gsub("EcoPath", "", entity_type)
  entity_type_simple <- gsub("Input", "", entity_type_simple)

  # Simplify parameter names
  param_names <- list(
    "BiomassInput" = "Biomass",
    "PBInput" = "P/B",
    "QBInput" = "Q/B",
    "EEInput" = "EE",
    "DietComp" = "Diet",
    "TCatchInput" = "Catch",
    "BiomassAreaInput" = "Biomass",
    "FixedCost" = "Fixed Cost",
    "VariableCost" = "Variable Cost"
  )

  parameter_simple <- if (!is.na(parameter) && parameter %in% names(param_names)) {
    param_names[[parameter]]
  } else {
    parameter
  }

  list(
    entity_type = entity_type_simple,
    entity_id = entity_id,
    parameter = parameter_simple,
    full_info = value_id
  )
}

organize_auxillary_data <- function(auxillary_data, group_data = NULL, fleet_data = NULL) {
  #' Organize Auxillary data into readable format
  #'
  #' Parses ValueID, merges with group/fleet names, and creates display table
  #'
  #' @param auxillary_data Data frame from Auxillary table
  #' @param group_data Optional: EcopathGroup data for merging names
  #' @param fleet_data Optional: EcopathFleet data for merging names
  #' @return Data frame with organized comments/tooltips
  #' @export

  if (is.null(auxillary_data) || nrow(auxillary_data) == 0) {
    return(data.frame(
      Entity_Type = character(0),
      Entity_ID = character(0),
      Entity_Name = character(0),
      Parameter = character(0),
      Remark = character(0),
      stringsAsFactors = FALSE
    ))
  }

  # Parse all ValueIDs
  parsed <- lapply(auxillary_data$ValueID, parse_auxillary_valueid)

  # Create organized data frame
  df <- data.frame(
    DBID = auxillary_data$DBID,
    Entity_Type = sapply(parsed, function(x) x$entity_type),
    Entity_ID = sapply(parsed, function(x) x$entity_id),
    Parameter = sapply(parsed, function(x) x$parameter),
    Remark = if ("Remark" %in% colnames(auxillary_data)) auxillary_data$Remark else NA,
    ValueID_Full = auxillary_data$ValueID,
    stringsAsFactors = FALSE
  )

  # Filter to only rows with remarks
  if ("Remark" %in% colnames(df)) {
    df <- df[!is.na(df$Remark) & nchar(as.character(df$Remark)) > 0, ]
  }

  # Add entity names if group/fleet data provided
  if (!is.null(group_data) && "GroupID" %in% colnames(group_data) && "GroupName" %in% colnames(group_data)) {
    # Create lookup for group names
    group_lookup <- setNames(group_data$GroupName, as.character(group_data$GroupID))

    # Match entity IDs to group names
    df$Entity_Name <- ifelse(
      df$Entity_Type == "Group" & !is.na(df$Entity_ID),
      group_lookup[as.character(df$Entity_ID)],
      NA
    )
  } else {
    df$Entity_Name <- NA
  }

  if (!is.null(fleet_data) && "FleetID" %in% colnames(fleet_data) && "FleetName" %in% colnames(fleet_data)) {
    # Create lookup for fleet names
    fleet_lookup <- setNames(fleet_data$FleetName, as.character(fleet_data$FleetID))

    # Match entity IDs to fleet names
    df$Entity_Name <- ifelse(
      df$Entity_Type == "Fleet" & !is.na(df$Entity_ID) & is.na(df$Entity_Name),
      fleet_lookup[as.character(df$Entity_ID)],
      df$Entity_Name
    )
  }

  # Use Entity_ID if name not found
  df$Entity_Name <- ifelse(is.na(df$Entity_Name), df$Entity_ID, df$Entity_Name)

  # Reorder columns
  df <- df[, c("Entity_Type", "Entity_ID", "Entity_Name", "Parameter", "Remark", "ValueID_Full")]

  # Sort by entity type and ID
  df <- df[order(df$Entity_Type, as.numeric(df$Entity_ID)), ]

  return(df)
}

extract_citations_from_remarks <- function(remarks) {
  #' Extract citations/sources from remark text
  #'
  #' Identifies citation-like text in remarks
  #'
  #' @param remarks Vector of remark strings
  #' @return Data frame with remark and extracted citation
  #' @export

  if (length(remarks) == 0) {
    return(data.frame(
      remark = character(0),
      citation = character(0),
      stringsAsFactors = FALSE
    ))
  }

  # Simple pattern: look for text with year in format "Name, Year" or "Name et al., Year"
  # Also look for journal names in ALL CAPS

  citations <- sapply(remarks, function(remark) {
    if (is.na(remark) || nchar(remark) == 0) {
      return(NA)
    }

    # Look for patterns like "Author, Year" or "Author et al., Year"
    pattern <- "([A-Z][a-z]+\\s+(et\\s+al\\.)?\\s*,?\\s*[12][0-9]{3}[a-z]?)"
    matches <- regmatches(remark, gregexpr(pattern, remark, perl = TRUE))[[1]]

    if (length(matches) > 0) {
      return(paste(matches, collapse = "; "))
    }

    # If no match, return first part if it looks like a citation
    parts <- strsplit(remark, "\t")[[1]]
    if (length(parts) > 1 && grepl("[12][0-9]{3}", parts[2])) {
      return(parts[2])
    }

    return(NA)
  })

  data.frame(
    remark = remarks,
    citation = citations,
    stringsAsFactors = FALSE
  )
}
