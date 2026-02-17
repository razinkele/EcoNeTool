create_hexagonal_grid <- function(bbox, cell_size = 2000, crs = 4326) {
  # Validate required package
  validate_package("sf", "spatial analysis")

  # Extract bbox values directly
  if (is.numeric(bbox) && length(bbox) == 4) {
    # Numeric vector: c(xmin, ymin, xmax, ymax)
    xmin <- bbox[1]
    ymin <- bbox[2]
    xmax <- bbox[3]
    ymax <- bbox[4]
  } else if (inherits(bbox, "sf")) {
    # sf object - extract bbox
    bbox_sf <- sf::st_bbox(bbox)
    xmin <- as.numeric(bbox_sf["xmin"])
    ymin <- as.numeric(bbox_sf["ymin"])
    xmax <- as.numeric(bbox_sf["xmax"])
    ymax <- as.numeric(bbox_sf["ymax"])
  } else if (is.list(bbox)) {
    # Named list
    xmin <- bbox$xmin
    ymin <- bbox$ymin
    xmax <- bbox$xmax
    ymax <- bbox$ymax
  } else {
    stop("bbox must be numeric vector, sf object, or named list")
  }

  # Validate bbox values using shared utility
  validate_bbox(xmin, ymin, xmax, ymax)

  # Validate cell_size using shared utility with config constants
  validate_numeric_range(cell_size, "cell_size", min = MIN_CELL_SIZE_METERS, max = MAX_CELL_SIZE_METERS)

  # Check cell_size vs bbox extent to prevent creating too many hexagons
  bbox_width_deg <- xmax - xmin
  bbox_height_deg <- ymax - ymin
  bbox_width_m <- bbox_width_deg * DEGREES_TO_METERS  # Convert degrees to meters
  bbox_height_m <- bbox_height_deg * DEGREES_TO_METERS

  if (cell_size > min(bbox_width_m, bbox_height_m) / 2) {
    warning(sprintf("cell_size (%d m) is large relative to bbox extent. May produce very few hexagons.", cell_size))
  }

  # Estimate number of hexagons to prevent memory issues
  hexagon_packing_factor <- 2.6  # Hexagon packing efficiency
  estimated_hexagons <- (bbox_width_m * bbox_height_m) / (cell_size^2 * hexagon_packing_factor)
  if (estimated_hexagons > MAX_HEXAGONS) {
    stop(sprintf("cell_size too small: would create ~%.0f million hexagons (max %.0f million). Increase cell_size to at least %d m.",
                 estimated_hexagons / 1e6,
                 MAX_HEXAGONS / 1e6,
                 ceiling(sqrt((bbox_width_m * bbox_height_m) / (MAX_HEXAGONS * hexagon_packing_factor)))))
  }

  # Determine appropriate UTM zone based on bbox center longitude
  center_lon <- (xmin + xmax) / 2
  center_lat <- (ymin + ymax) / 2

  # Calculate UTM zone
  utm_zone <- floor((center_lon + 180) / 6) + 1

  # Determine hemisphere
  hemisphere <- ifelse(center_lat >= 0, "north", "south")

  # Create EPSG code for UTM zone
  # Northern hemisphere: 32600 + zone, Southern: 32700 + zone
  epsg_code <- ifelse(hemisphere == "north", 32600 + utm_zone, 32700 + utm_zone)

  # Create a proper polygon from bbox coordinates
  bbox_coords <- matrix(c(
    xmin, ymin,
    xmax, ymin,
    xmax, ymax,
    xmin, ymax,
    xmin, ymin
  ), ncol = 2, byrow = TRUE)

  bbox_polygon <- sf::st_polygon(list(bbox_coords))
  bbox_sfc <- sf::st_sfc(bbox_polygon, crs = crs)

  # Transform bbox to UTM projection
  bbox_proj <- sf::st_transform(bbox_sfc, crs = epsg_code)
  bbox_proj_sf <- sf::st_bbox(bbox_proj)

  # Create hexagonal grid in metric coordinates
  hex_grid_proj <- sf::st_make_grid(
    bbox_proj_sf,
    cellsize = cell_size,
    square = FALSE,  # Hexagons, not squares
    what = "polygons"
  )

  # hex_grid_proj is an sfc object (geometry collection)
  # Transform grid back to original CRS first
  hex_grid_geom <- sf::st_transform(hex_grid_proj, crs = sf::st_crs(crs))

  # Now create data frame with hex IDs
  n_hexagons <- length(hex_grid_geom)
  hex_df <- data.frame(
    hex_id = paste0("HEX_", sprintf("%04d", seq_len(n_hexagons)))
  )

  # Convert to sf object
  hex_df <- sf::st_sf(hex_df, geometry = hex_grid_geom, crs = crs)

  # Add centroid coordinates for reference
  suppressWarnings({
    centroids <- sf::st_coordinates(sf::st_centroid(hex_df))
  })
  hex_df$center_lon <- centroids[, 1]
  hex_df$center_lat <- centroids[, 2]

  return(hex_df)
}

#' Assign species occurrences to hexagonal grid
#'
#' Spatially joins species occurrence data to hexagonal grid cells.
#'
#' @param species_data Data frame with species occurrences. Must contain:
#'   - lon, lat (or x, y) columns for coordinates
#'   - species column with species names
#'   - Optional: occurrence (1/0 or abundance), biomass
#' @param hex_grid sf hexagonal grid object from create_hexagonal_grid()
#' @return Data frame with hex_id, species, and aggregated occurrence/biomass
#' @export
#' @examples
#' # Assign species to hexagons
#' species_data <- data.frame(
#'   lon = c(22.1, 22.3, 22.5),
#'   lat = c(59.0, 59.1, 59.0),
#'   species = c("Gadus morhua", "Clupea harengus", "Gadus morhua"),
#'   biomass = c(12.5, 8.3, 10.2)
#' )
#' hex_species <- assign_species_to_hexagons(species_data, hex_grid)
assign_species_to_hexagons <- function(species_data, hex_grid) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required")
  }

  # Check required columns
  coord_cols <- c("lon", "lat")
  if (!all(coord_cols %in% colnames(species_data))) {
    # Try x, y
    coord_cols <- c("x", "y")
    if (!all(coord_cols %in% colnames(species_data))) {
      stop("species_data must have lon/lat or x/y columns")
    }
  }

  if (!"species" %in% colnames(species_data)) {
    stop("species_data must have 'species' column")
  }

  # Convert species data to sf points
  species_sf <- sf::st_as_sf(
    species_data,
    coords = coord_cols,
    crs = sf::st_crs(hex_grid)
  )

  # Spatial join: assign each point to hexagon
  joined <- sf::st_join(species_sf, hex_grid, join = sf::st_within)

  # Remove geometry for easier manipulation
  joined_df <- sf::st_drop_geometry(joined)

  # Remove rows where hex_id is NA (points outside grid)
  joined_df <- joined_df[!is.na(joined_df$hex_id), ]

  # Check if any points were assigned
  if (nrow(joined_df) == 0) {
    warning("No species occurrences fell within the grid boundaries")
    return(data.frame(
      hex_id = character(0),
      species = character(0),
      occurrence = numeric(0)
    ))
  }

  # Aggregate by hexagon and species
  if ("biomass" %in% colnames(joined_df)) {
    # Sum biomass per species per hexagon
    result <- aggregate(
      biomass ~ hex_id + species,
      data = joined_df,
      FUN = sum
    )
    result$occurrence <- 1
  } else {
    # Count occurrences - create occurrence column first
    joined_df$occ_count <- 1
    result <- aggregate(
      occ_count ~ hex_id + species,
      data = joined_df,
      FUN = sum
    )
    colnames(result)[3] <- "occurrence"
  }

  return(result)
}

#' Extract local food web from metaweb for a hexagon
#'
#' Extracts a local food web by filtering the regional metaweb to include only
#' species present in the specified hexagon. This implements the MARBEFES
#' approach of deriving local networks from metawebs based on co-occurrence.
#'
#' @param metaweb Metaweb object (from Phase 2)
#' @param species_list Character vector of species present in hexagon
#' @param hexagon_id Hexagon identifier (for metadata)
#' @return igraph network with local species and their interactions
#' @export
#' @examples
#' # Extract local network for one hexagon
#' local_species <- c("Gadus morhua", "Clupea harengus", "Mytilus edulis")
#' local_net <- extract_local_network(metaweb, local_species, "HEX_001")
#'
#' @references
#' MARBEFES WP3.2 Guidelines Section 4: Local network extraction from metaweb
extract_local_network <- function(metaweb, species_list, hexagon_id = NA) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required")
  }

  # Validate inputs
  if (!inherits(metaweb, "metaweb")) {
    stop("metaweb must be a metaweb object")
  }

  if (length(species_list) == 0) {
    warning("No species in hexagon ", hexagon_id, " - returning empty network")
    empty_net <- igraph::make_empty_graph(n = 0, directed = TRUE)
    return(empty_net)
  }

  # Filter species present in hexagon
  local_species_df <- metaweb$species[metaweb$species$species_id %in% species_list |
                                       metaweb$species$species_name %in% species_list, ]

  if (nrow(local_species_df) == 0) {
    warning("No matching species found in metaweb for hexagon ", hexagon_id)
    empty_net <- igraph::make_empty_graph(n = 0, directed = TRUE)
    return(empty_net)
  }

  # Filter interactions where both predator and prey are present
  local_interactions <- metaweb$interactions[
    (metaweb$interactions$predator_id %in% local_species_df$species_id |
     metaweb$interactions$predator_id %in% local_species_df$species_name) &
    (metaweb$interactions$prey_id %in% local_species_df$species_id |
     metaweb$interactions$prey_id %in% local_species_df$species_name),
  ]

  # Create igraph network
  if (nrow(local_interactions) > 0) {
    local_net <- igraph::graph_from_data_frame(
      d = local_interactions[, c("predator_id", "prey_id")],
      directed = TRUE,
      vertices = local_species_df
    )

    # Add edge attributes
    if ("quality_code" %in% colnames(local_interactions)) {
      igraph::E(local_net)$quality_code <- local_interactions$quality_code
    }
    if ("source" %in% colnames(local_interactions)) {
      igraph::E(local_net)$source <- local_interactions$source
    }
  } else {
    # Network with species but no interactions
    local_net <- igraph::graph_from_data_frame(
      d = data.frame(from = character(0), to = character(0)),
      directed = TRUE,
      vertices = local_species_df
    )
  }

  # Add hexagon metadata
  local_net$hexagon_id <- hexagon_id
  local_net$extracted_from <- metaweb$metadata$region %||% "Unknown metaweb"

  return(local_net)
}

#' Extract local networks for all hexagons (batch processing)
#'
#' Extracts local food webs for multiple hexagons in batch. Includes
#' progress reporting for large grids.
#'
#' @param metaweb Metaweb object
#' @param hex_species Data frame from assign_species_to_hexagons()
#' @param hex_grid Optional sf hexagonal grid object (not currently used, for future enhancement)
#' @param progress Logical, show progress bar? (default TRUE)
#' @return Named list of igraph networks, one per hexagon
#' @export
extract_local_networks <- function(metaweb, hex_species, hex_grid = NULL, progress = TRUE) {
  # Get unique hexagons
  hex_ids <- unique(hex_species$hex_id)
  n_hexagons <- length(hex_ids)

  if (progress) {
    cat("Extracting local networks for", n_hexagons, "hexagons...\n")
  }

  # Extract network for each hexagon
  local_networks <- list()

  for (i in seq_along(hex_ids)) {
    hex_id <- hex_ids[i]

    # Get species list for this hexagon
    species_in_hex <- hex_species$species[hex_species$hex_id == hex_id]

    # Extract local network
    local_networks[[hex_id]] <- extract_local_network(metaweb, species_in_hex, hex_id)

    # Progress reporting
    if (progress && (i %% 10 == 0 || i == n_hexagons)) {
      cat(sprintf("  Progress: %d/%d hexagons (%.1f%%)\n",
                  i, n_hexagons, 100 * i / n_hexagons))
    }
  }

  if (progress) {
    cat("✓ Extraction complete!\n")
  }

  return(local_networks)
}

#' Calculate food web metrics for spatial hexagons
#'
#' Calculates topological metrics for each local network in a spatial grid.
#' Returns metrics in a format suitable for spatial visualization and analysis.
#'
#' @param local_networks Named list of igraph networks (from extract_local_networks)
#' @param hex_grid Optional sf hexagonal grid object (for spatial join)
#' @param metrics Character vector of metrics to calculate. Options:
#'   - "S" = Species richness
#'   - "L" = Number of links
#'   - "C" = Connectance
#'   - "LD" = Link density
#'   - "meanTL" = Mean trophic level
#'   - "maxTL" = Maximum trophic level
#' @param progress Logical, show progress? (default TRUE)
#' @return Data frame with hex_id and calculated metrics
#' @export
#' @examples
#' # Calculate metrics for spatial networks
#' metrics_df <- calculate_spatial_metrics(local_networks,
#'   metrics = c("S", "L", "C", "meanTL"))
calculate_spatial_metrics <- function(local_networks,
                                      hex_grid = NULL,
                                      metrics = c("S", "L", "C", "LD"),
                                      progress = TRUE) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required")
  }

  n_hexagons <- length(local_networks)
  hex_ids <- names(local_networks)

  if (progress) {
    cat("Calculating metrics for", n_hexagons, "hexagons...\n")
  }

  # Initialize results data frame
  results <- data.frame(
    hex_id = hex_ids,
    stringsAsFactors = FALSE
  )

  # Calculate each metric
  for (i in seq_along(hex_ids)) {
    hex_id <- hex_ids[i]
    net <- local_networks[[hex_id]]

    # Species richness (S)
    if ("S" %in% metrics) {
      results$S[i] <- igraph::vcount(net)
    }

    # Number of links (L)
    if ("L" %in% metrics) {
      results$L[i] <- igraph::ecount(net)
    }

    # Connectance (C = L / (S * (S-1)))
    if ("C" %in% metrics) {
      S <- igraph::vcount(net)
      L <- igraph::ecount(net)
      results$C[i] <- if (S > 1) L / (S * (S - 1)) else 0
    }

    # Link density (LD = L / S)
    if ("LD" %in% metrics) {
      S <- igraph::vcount(net)
      L <- igraph::ecount(net)
      results$LD[i] <- if (S > 0) L / S else 0
    }

    # Trophic levels
    if ("meanTL" %in% metrics || "maxTL" %in% metrics) {
      if (igraph::vcount(net) > 0 && igraph::ecount(net) > 0) {
        # Simple trophic level: 1 + mean trophic level of prey
        # (This is a simplified version - full version needs basal species detection)
        tl <- rep(1, igraph::vcount(net))  # Default = basal

        # Iteratively calculate trophic levels
        for (iter in 1:10) {  # Max 10 iterations
          for (v in igraph::V(net)) {
            prey <- igraph::neighbors(net, v, mode = "in")
            if (length(prey) > 0) {
              tl[v] <- 1 + mean(tl[prey])
            }
          }
        }

        if ("meanTL" %in% metrics) {
          results$meanTL[i] <- mean(tl)
        }
        if ("maxTL" %in% metrics) {
          results$maxTL[i] <- max(tl)
        }
      } else {
        if ("meanTL" %in% metrics) results$meanTL[i] <- NA
        if ("maxTL" %in% metrics) results$maxTL[i] <- NA
      }
    }

    # Progress reporting
    if (progress && (i %% 10 == 0 || i == n_hexagons)) {
      cat(sprintf("  Progress: %d/%d hexagons (%.1f%%)\n",
                  i, n_hexagons, 100 * i / n_hexagons))
    }
  }

  # Add spatial coordinates if hex_grid provided
  if (!is.null(hex_grid)) {
    # Match hex_ids to grid
    hex_match <- match(results$hex_id, hex_grid$hex_id)
    if (any(!is.na(hex_match))) {
      results$center_lon <- hex_grid$center_lon[hex_match]
      results$center_lat <- hex_grid$center_lat[hex_match]
    }
  }

  if (progress) {
    cat("✓ Metric calculation complete!\n")
  }

  return(results)
}

#' Aggregate spatial metrics across hexagons
#'
#' Calculates summary statistics (mean, SD, min, max) for spatial metrics.
#'
#' @param metrics_df Data frame from calculate_spatial_metrics()
#' @param metric_cols Character vector of metric column names to aggregate
#' @return Data frame with summary statistics
#' @export
aggregate_spatial_metrics <- function(metrics_df,
                                      metric_cols = c("S", "L", "C", "LD")) {
  results <- data.frame(
    metric = metric_cols,
    mean = NA,
    sd = NA,
    min = NA,
    max = NA,
    n_hexagons = nrow(metrics_df),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(metric_cols)) {
    metric <- metric_cols[i]
    if (metric %in% colnames(metrics_df)) {
      values <- metrics_df[[metric]]
      values <- values[!is.na(values)]  # Remove NAs

      if (length(values) > 0) {
        results$mean[i] <- mean(values)
        results$sd[i] <- sd(values)
        results$min[i] <- min(values)
        results$max[i] <- max(values)
      }
    }
  }

  return(results)
}

#' Create spatial food web data structure
#'
#' Creates an S3 object containing all spatial food web analysis components.
#'
#' @param hex_grid sf object with hexagonal grid from create_hexagonal_grid()
#' @param hex_species Data frame with species per hexagon
#' @param metaweb Metaweb object
#' @param local_networks List of local igraph networks
#' @param metrics Data frame with calculated metrics
#' @param metadata List with analysis parameters
#' @return Object of class 'spatial_foodweb_data'
#' @export
create_spatial_foodweb_data <- function(hex_grid,
                                        hex_species,
                                        metaweb,
                                        local_networks = NULL,
                                        metrics = NULL,
                                        metadata = list()) {
  spatial_data <- list(
    hexagons = hex_grid,
    species_occurrence = hex_species,
    metaweb = metaweb,
    local_networks = local_networks,
    metrics = metrics,
    metadata = c(metadata, list(
      created = Sys.time(),
      n_hexagons = nrow(hex_grid),
      metaweb_region = metaweb$metadata$region %||% "Unknown"
    ))
  )

  class(spatial_data) <- "spatial_foodweb_data"
  return(spatial_data)
}

#' Print spatial food web data summary
#'
#' @param x spatial_foodweb_data object
#' @param ... Additional arguments (ignored)
#' @export
print.spatial_foodweb_data <- function(x, ...) {
  cat("Spatial Food Web Data\n")
  cat("=====================\n")
  cat("Metaweb region:", x$metadata$metaweb_region, "\n")
  cat("Hexagons:", x$metadata$n_hexagons, "\n")
  cat("Total species occurrences:", nrow(x$species_occurrence), "\n")
  cat("Unique species:", length(unique(x$species_occurrence$species)), "\n")

  if (!is.null(x$local_networks)) {
    cat("Local networks extracted:", length(x$local_networks), "\n")
  }

  if (!is.null(x$metrics)) {
    cat("Metrics calculated:", ncol(x$metrics) - 1, "metrics\n")
    if (nrow(x$metrics) > 0) {
      cat("  Metric ranges:\n")
      metric_cols <- setdiff(colnames(x$metrics), c("hex_id", "center_lon", "center_lat"))
      for (metric in metric_cols[1:min(5, length(metric_cols))]) {
        if (metric %in% colnames(x$metrics)) {
          vals <- x$metrics[[metric]]
          vals <- vals[!is.na(vals)]
          if (length(vals) > 0) {
            cat(sprintf("    %s: %.2f - %.2f (mean: %.2f)\n",
                        metric, min(vals), max(vals), mean(vals)))
          }
        }
      }
    }
  }

  cat("\nCreated:", format(x$metadata$created, "%Y-%m-%d %H:%M:%S"), "\n")
  invisible(x)
}

