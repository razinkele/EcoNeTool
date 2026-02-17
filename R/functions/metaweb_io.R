#' Import Metaweb from CSV Files
#'
#' @param species_file Path to species CSV file
#' @param interactions_file Path to interactions CSV file
#' @param metadata Optional list of metadata
#' @return Metaweb object
#' @export
import_metaweb_csv <- function(species_file, interactions_file, metadata = list()) {
  # Validate input files
  validate_file_exists(species_file, "species_file")
  validate_file_exists(interactions_file, "interactions_file")

  tryCatch({
    # Read CSV files
    species <- read.csv(species_file, stringsAsFactors = FALSE)
    interactions <- read.csv(interactions_file, stringsAsFactors = FALSE)

    # Validate data structure
    if (nrow(species) == 0) {
      stop("Species file is empty")
    }
    if (nrow(interactions) == 0) {
      stop("Interactions file is empty")
    }

    # Create metaweb object
    metaweb <- create_metaweb(species, interactions, metadata)

    message(sprintf("✓ Imported metaweb: %d species, %d interactions",
                    nrow(species), nrow(interactions)))

    return(metaweb)

  }, error = function(e) {
    stop(sprintf("Failed to import metaweb from CSV: %s", e$message), call. = FALSE)
  })
}

#' Export Metaweb to CSV Files
#'
#' @param metaweb Metaweb object
#' @param species_file Path for species CSV file
#' @param interactions_file Path for interactions CSV file
#' @export
export_metaweb_csv <- function(metaweb, species_file, interactions_file) {
  # Validate metaweb
  if (!inherits(metaweb, "metaweb")) {
    stop("Input must be a metaweb object", call. = FALSE)
  }

  validate_metaweb(metaweb)

  tryCatch({
    # Export species data
    write.csv(metaweb$species, species_file, row.names = FALSE)

    # Export interactions data
    write.csv(metaweb$interactions, interactions_file, row.names = FALSE)

    message(sprintf("✓ Metaweb exported to:\n  Species: %s\n  Interactions: %s",
                    species_file, interactions_file))

  }, error = function(e) {
    stop(sprintf("Failed to export metaweb: %s", e$message), call. = FALSE)
  })
}

#' Export Metaweb to RDS File
#'
#' @param metaweb Metaweb object
#' @param file Path for RDS file
#' @export
export_metaweb_rds <- function(metaweb, file) {
  # Validate metaweb
  if (!inherits(metaweb, "metaweb")) {
    stop("Input must be a metaweb object", call. = FALSE)
  }

  validate_metaweb(metaweb)

  tryCatch({
    # Save RDS file
    saveRDS(metaweb, file)

    file_size_kb <- round(file.info(file)$size / 1024, 2)
    message(sprintf("✓ Metaweb exported to: %s (%.2f KB)", file, file_size_kb))

  }, error = function(e) {
    stop(sprintf("Failed to export metaweb to RDS: %s", e$message), call. = FALSE)
  })
}

#' Import Metaweb from RDS File
#'
#' @param file Path to RDS file
#' @return Metaweb object
#' @export
import_metaweb_rds <- function(file) {
  # Validate file exists
  validate_file_exists(file, "file")

  tryCatch({
    # Load RDS file
    metaweb <- readRDS(file)

    # Validate it's a metaweb object
    if (!inherits(metaweb, "metaweb")) {
      stop("File does not contain a valid metaweb object")
    }

    # Validate metaweb structure
    validate_metaweb(metaweb)

    message(sprintf("✓ Imported metaweb: %d species, %d interactions",
                    nrow(metaweb$species), nrow(metaweb$interactions)))

    return(metaweb)

  }, error = function(e) {
    stop(sprintf("Failed to import metaweb from RDS: %s", e$message), call. = FALSE)
  })
}

#' Export Current Metaweb to RDA File
#'
#' Exports the current metaweb (network and species information) to an RDA file
#' in the same format as BalticFW.Rdata (Frelat & Kortsch format).
#'
#' @param net igraph network object
#' @param info data frame with species information
#' @param file_path character, path where to save the RDA file
#'
#' @return TRUE invisibly if export succeeds
#' @export
#'
#' @details
#' The exported RDA file contains two objects:
#' - \code{net}: igraph network object with directed food web
#' - \code{info}: data frame with species attributes
#'
#' The info data frame structure matches BalticFW.Rdata format with columns:
#' species, fg, nbY, meanB, taxon, met.types, org.type, bodymasses, losses, efficiencies
#'
#' Note: colfg column is removed before export (it's generated dynamically from COLOR_SCHEME)
#'
#' @examples
#' \dontrun{
#' export_metaweb_rda(net, info, "my_foodweb.Rdata")
#' }
export_metaweb_rda <- function(net, info, file_path) {
  tryCatch({
    # Validate inputs
    if (!igraph::is_igraph(net)) {
      stop("net must be an igraph object", call. = FALSE)
    }

    if (!is.data.frame(info)) {
      stop("info must be a data frame", call. = FALSE)
    }

    if (nrow(info) != igraph::vcount(net)) {
      stop(sprintf(
        "Number of rows in info (%d) must match number of vertices in net (%d)",
        nrow(info), igraph::vcount(net)
      ), call. = FALSE)
    }

    # Prepare info dataframe - remove colfg (generated dynamically)
    export_info <- info[, !names(info) %in% c("colfg"), drop = FALSE]

    # Ensure required columns exist, add defaults if missing
    if (!"species" %in% names(export_info)) {
      export_info$species <- factor(igraph::V(net)$name)
    }
    if (!"nbY" %in% names(export_info)) {
      export_info$nbY <- rep(1L, nrow(export_info))
    }
    if (!"taxon" %in% names(export_info)) {
      export_info$taxon <- rep("Unknown", nrow(export_info))
    }
    if (!"org.type" %in% names(export_info)) {
      # Infer organism type from functional group
      export_info$org.type <- sapply(as.character(export_info$fg), function(fg) {
        if (fg %in% c("Phytoplankton", "Phytobenthos")) "plant"
        else if (fg == "Detritus") "detritus"
        else "animal"
      })
    }

    # Reorder columns to match BalticFW.Rdata format
    standard_cols <- c("species", "fg", "nbY", "meanB", "taxon", "met.types",
                      "org.type", "bodymasses", "losses", "efficiencies")
    present_cols <- intersect(standard_cols, names(export_info))
    other_cols <- setdiff(names(export_info), standard_cols)
    export_info <- export_info[, c(present_cols, other_cols)]

    # Convert species and fg to factors if not already
    if (!is.factor(export_info$species)) {
      export_info$species <- factor(export_info$species)
    }
    if (!is.factor(export_info$fg)) {
      export_info$fg <- factor(export_info$fg, levels = get_functional_group_levels())
    }

    # Ensure vertex names match species in info
    igraph::V(net)$name <- as.character(export_info$species)

    # Save to RDA file (rename export_info to info for compatibility)
    info <- export_info
    save(net, info, file = file_path, compress = TRUE)

    message(sprintf(
      "\u2713 Metaweb exported to: %s\n  Species: %d\n  Links: %d\n  Functional groups: %s",
      file_path,
      igraph::vcount(net),
      igraph::ecount(net),
      paste(unique(as.character(info$fg)), collapse = ", ")
    ))

    invisible(TRUE)

  }, error = function(e) {
    stop(sprintf("Failed to export metaweb: %s", e$message), call. = FALSE)
  })
}

# ============================================================================
# PHASE 1: SPATIAL ANALYSIS FUNCTIONS (MARBEFES WP3.2 BBT Support)
# ============================================================================

#' Create hexagonal spatial grid
#'
#' Creates a hexagonal grid for spatial food web analysis. Supports MARBEFES
#' BBT (Broad Belt Transect) workflows with 1-3km hexagons as per guidance.
#'
#' This function handles the unit mismatch between WGS84 (degrees) and metric
#' cell sizes by projecting to an appropriate metric CRS, creating the grid,
#' and projecting back to the original CRS.
#'
#' @param bbox Bounding box - can be:
#'   - sf object with geometry
#'   - Numeric vector c(xmin, ymin, xmax, ymax)
#'   - Named list with xmin, ymin, xmax, ymax
#' @param cell_size Cell size in meters (default 2000 = 2km)
#' @param crs Coordinate reference system (default 4326 = WGS84)
#' @return sf object with hexagonal polygons and hex_id
#' @export
#' @examples
#' # Create 2km hexagons for Baltic Sea region
#' bbox <- c(xmin = 20, ymin = 58, xmax = 24, ymax = 60)
#' hex_grid <- create_hexagonal_grid(bbox, cell_size = 2000)
#'
#' @references
#' MARBEFES WP3.2 Guidelines Section 3.1: Hexagonal spatial units (1-3km)
