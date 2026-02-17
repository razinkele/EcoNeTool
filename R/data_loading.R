# ============================================================================
# EcoNeTool - Data Loading and Validation
# ============================================================================
#
# This file handles loading and validating food web data.
#
# Returns a list containing:
#   - net: igraph network object
#   - info: data frame with species information
#   - color_scheme: color scheme for functional groups
#
# ============================================================================

#' Load and validate food web data
#'
#' @param data_file Path to the .Rdata file (default: DATA_FILE from config)
#' @return List with net, info, and color_scheme objects
#' @export
load_baltic_foodweb <- function(data_file = DATA_FILE) {

  # Check file exists
  if (!file.exists(data_file)) {
    stop(paste("Data file not found:", data_file,
               "\nPlease ensure the data file is in the correct location."))
  }

  tryCatch({
    # Load data into a separate environment to avoid overwriting functions
    # Some .Rdata files may contain functions (trophiclevels, plotfw, fluxind).
    # We extract only data objects and use our own analysis functions
    suppressMessages({
      data_env <- new.env()
      load(data_file, envir = data_env)

      # Validate required objects exist
      if (!exists("net", envir = data_env)) {
        stop("'net' object not found in data file")
      }
      if (!exists("info", envir = data_env)) {
        stop("'info' object not found in data file")
      }

      # Extract only the data objects from the loaded environment
      # (Functions are defined in functions.R)
      net <- data_env$net
      info <- data_env$info

      # Validate network structure
      if (!igraph::is_igraph(net)) {
        stop("'net' must be an igraph object")
      }

      # Upgrade igraph object if needed
      net <- igraph::upgrade_graph(net)

      # Ensure vertex names are properly set
      # If vertex names are missing or numeric, try to get them from info$species or rownames
      if (is.null(igraph::V(net)$name) || all(grepl("^[0-9]+$", igraph::V(net)$name))) {
        if ("species" %in% colnames(info)) {
          igraph::V(net)$name <- as.character(info$species)
        } else if (!is.null(rownames(info))) {
          igraph::V(net)$name <- rownames(info)
        }
      }
    })

    # Validate network has vertices and edges
    if (igraph::vcount(net) == 0) {
      stop("Network contains no vertices")
    }
    if (igraph::ecount(net) == 0) {
      warning("Network contains no edges")
    }

    # Validate info data frame has required columns
    required_cols <- c("meanB", "fg", "bodymasses", "met.types", "efficiencies")
    missing_cols <- setdiff(required_cols, colnames(info))
    if (length(missing_cols) > 0) {
      stop(paste("Missing required columns in info:",
                 paste(missing_cols, collapse = ", ")))
    }

    # Validate biomass values
    if (any(info$meanB < 0, na.rm = TRUE)) {
      stop("Biomass values must be non-negative")
    }
    if (all(is.na(info$meanB))) {
      stop("All biomass values are NA")
    }

    # Keep full COLOR_SCHEME - colors are matched by functional group NAME, not index
    # Do NOT adjust based on number of groups in data
    color_scheme <- COLOR_SCHEME

    message("✓ Data loaded successfully: ",
            igraph::vcount(net), " species, ",
            igraph::ecount(net), " trophic links")

    # Return list with all required objects (color_scheme will be full 7-color scheme)
    list(net = net, info = info, color_scheme = color_scheme)

  }, error = function(e) {
    stop(paste("Error loading data:", e$message))
  })
}

# ============================================================================
# AUTO-LOAD DATA
# ============================================================================

# Automatically load data when this file is sourced
# (unless DATA_FILE doesn't exist or we're in testing mode)
if (exists("DATA_FILE") && !isTRUE(getOption("econetool.skip_autoload"))) {
  foodweb_data <- load_baltic_foodweb(DATA_FILE)
  net <- foodweb_data$net
  info <- foodweb_data$info
  # Update COLOR_SCHEME if adjusted
  if (!is.null(foodweb_data$color_scheme)) {
    COLOR_SCHEME <- foodweb_data$color_scheme
  }
}
