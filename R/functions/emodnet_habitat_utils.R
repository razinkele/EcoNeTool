#' EMODnet EUSeaMap Habitat Integration
#'
#' Functions to query habitat data from EMODnet EUSeaMap layers
#' for marine food web species classification.
#'
#' @description
#' This module integrates EMODnet EUSeaMap broad-scale habitat maps
#' to provide EUNIS habitat classification for marine species based
#' on sampling location.
#'
#' Key functions:
#' - load_euseamap(): Load EUSeaMap GeoPackage layer
#' - get_habitat_at_location(): Query habitat at coordinates
#' - add_habitat_to_species(): Add habitat data to species data frame
#'
#' @details
#' EMODnet EUSeaMap provides:
#' - EUNIS habitat codes
#' - Substrate types
#' - Depth zones
#' - Biological zones
#'
#' Coverage: Baltic Sea, North Sea, Mediterranean, Black Sea, NE Atlantic
#' Resolution: ~250m
#' Data source: https://emodnet.ec.europa.eu/en/seabed-habitats
#'
#' @author Claude (Anthropic)
#' @date 2025-12-17

library(sf)

#' Load EUSeaMap Layer
#'
#' @param path Path to EUSeaMap file (GeoPackage .gpkg or File Geodatabase .gdb)
#' @param layer Layer name (for .gdb files, NULL for auto-detect)
#' @param bbox Bounding box to filter data (c(xmin, ymin, xmax, ymax) in WGS84)
#' @return sf object with habitat polygons
#'
#' @details
#' Loads the EMODnet EUSeaMap layer from a local file.
#' Supports both GeoPackage (.gpkg) and File Geodatabase (.gdb) formats.
#' The layer should be downloaded from EMODnet Seabed Habitats portal.
#'
#' For large datasets, a bounding box filter is recommended to reduce memory usage.
#' Default bbox covers the Baltic Sea region (10-31°E, 53-66°N).
#'
#' @examples
#' \dontrun{
#' euseamap <- load_euseamap("data/EUSeaMap_2025.gdb")
#' euseamap <- load_euseamap("data/euseamap/EUSeaMap_Baltic.gpkg")
#' # Custom bounding box for North Sea
#' euseamap <- load_euseamap(bbox = c(-5, 50, 10, 60))
#' }
#'
#' @export
load_euseamap <- function(path = "data/EUSeaMap_2025/EUSeaMap_2025.gdb",
                          layer = NULL,
                          bbox = c(10, 53, 31, 66)) {
  # Check if path exists (file for .gpkg, directory for .gdb)
  if (!file.exists(path) && !dir.exists(path)) {
    stop(sprintf(
      "EUSeaMap file not found at: %s\n
Please download from: https://emodnet.ec.europa.eu/en/seabed-habitats\n
Or check if EUSeaMap_2025.zip is extracted to: data/",
      path
    ))
  }

  message(sprintf("Loading EUSeaMap from: %s", path))

  # Create WKT filter from bounding box if provided
  wkt_filter <- NULL
  if (!is.null(bbox) && length(bbox) == 4) {
    message(sprintf("  Filtering to bbox: %.1f-%.1f°E, %.1f-%.1f°N",
                    bbox[1], bbox[3], bbox[2], bbox[4]))
    wkt_filter <- st_as_text(st_as_sfc(st_bbox(c(
      xmin = bbox[1], ymin = bbox[2],
      xmax = bbox[3], ymax = bbox[4]
    ), crs = 4326)))
  }

  # Determine file type
  is_gdb <- grepl("\\.gdb$", path)

  # Read the data
  euseamap <- tryCatch({
    # Disable s2 spherical geometry (can cause issues with complex polygons)
    old_s2 <- sf::sf_use_s2()
    sf::sf_use_s2(FALSE)
    on.exit(sf::sf_use_s2(old_s2))

    # Set GDAL config options to handle invalid geometries
    # These tell GDAL to skip/accept problematic features instead of failing
    old_accept_ring <- Sys.getenv("OGR_GEOMETRY_ACCEPT_UNCLOSED_RING", unset = NA)
    old_skip_invalid <- Sys.getenv("OGR_SKIP_INVALID_GEOMETRIES", unset = NA)

    Sys.setenv(OGR_GEOMETRY_ACCEPT_UNCLOSED_RING = "YES")
    Sys.setenv(OGR_SKIP_INVALID_GEOMETRIES = "YES")

    on.exit({
      if (is.na(old_accept_ring)) {
        Sys.unsetenv("OGR_GEOMETRY_ACCEPT_UNCLOSED_RING")
      } else {
        Sys.setenv(OGR_GEOMETRY_ACCEPT_UNCLOSED_RING = old_accept_ring)
      }
      if (is.na(old_skip_invalid)) {
        Sys.unsetenv("OGR_SKIP_INVALID_GEOMETRIES")
      } else {
        Sys.setenv(OGR_SKIP_INVALID_GEOMETRIES = old_skip_invalid)
      }
    }, add = TRUE)

    if (is_gdb) {
      # File Geodatabase - need to specify layer
      available_layers <- st_layers(path)

      if (is.null(layer)) {
        # Auto-detect: use first layer
        layer <- available_layers$name[1]
        message(sprintf("  Using layer: %s", layer))
      }

      # Show available layers
      if (length(available_layers$name) > 1) {
        message(sprintf("  Available layers: %s",
                       paste(available_layers$name, collapse = ", ")))
      }

      # Try to read with WKT filter, check_ring_dir=FALSE to be more permissive
      if (!is.null(wkt_filter)) {
        suppressWarnings({
          st_read(path, layer = layer, wkt_filter = wkt_filter, quiet = TRUE,
                  check_ring_dir = FALSE)
        })
      } else {
        suppressWarnings({
          st_read(path, layer = layer, quiet = TRUE, check_ring_dir = FALSE)
        })
      }
    } else {
      # GeoPackage or shapefile
      if (!is.null(wkt_filter)) {
        suppressWarnings({
          st_read(path, wkt_filter = wkt_filter, quiet = TRUE, check_ring_dir = FALSE)
        })
      } else {
        suppressWarnings({
          st_read(path, quiet = TRUE, check_ring_dir = FALSE)
        })
      }
    }
  }, error = function(e) {
    stop(sprintf("Error reading EUSeaMap file: %s", conditionMessage(e)))
  })

  # Validate CRS
  if (is.na(st_crs(euseamap))) {
    warning("No CRS found in EUSeaMap layer, assuming EPSG:4326")
    st_crs(euseamap) <- 4326
  }

  message(sprintf("✓ Loaded %d habitat polygons", nrow(euseamap)))
  message(sprintf("✓ CRS: %s", st_crs(euseamap)$input))

  # Validate geometries and remove invalid ones
  original_count <- nrow(euseamap)
  valid_geoms <- sf::st_is_valid(euseamap)

  if (any(!valid_geoms, na.rm = TRUE)) {
    invalid_count <- sum(!valid_geoms, na.rm = TRUE)
    message(sprintf("⚠ Removing %d invalid geometries (%.1f%% of data)",
                    invalid_count,
                    100 * invalid_count / original_count))
    euseamap <- euseamap[valid_geoms & !is.na(valid_geoms), ]
    message(sprintf("✓ Retained %d valid polygons", nrow(euseamap)))
  }

  return(euseamap)
}

#' Get Habitat at Geographic Location
#'
#' @param lon Longitude in decimal degrees (EPSG:4326)
#' @param lat Latitude in decimal degrees (EPSG:4326)
#' @param euseamap_layer sf object with EUSeaMap data
#' @return List with habitat attributes or NULL
#'
#' @details
#' Performs spatial intersection to find habitat at specified coordinates.
#' Returns EUNIS code, habitat description, substrate, and depth zone.
#'
#' Field names may vary by EUSeaMap version. This function tries multiple
#' common field names to extract habitat attributes.
#'
#' @examples
#' \dontrun{
#' euseamap <- load_euseamap()
#' habitat <- get_habitat_at_location(18.65, 54.52, euseamap)
#' print(habitat$eunis_code)
#' }
#'
#' @export
get_habitat_at_location <- function(lon, lat, euseamap_layer) {
  # Validate inputs
  if (is.na(lon) || is.na(lat)) {
    warning("Invalid coordinates provided (NA values)")
    return(NULL)
  }

  if (lon < -180 || lon > 180 || lat < -90 || lat > 90) {
    warning(sprintf("Coordinates out of range: lon=%f, lat=%f", lon, lat))
    return(NULL)
  }

  # Create point geometry
  point <- st_sfc(st_point(c(lon, lat)), crs = 4326)

  # Transform to match euseamap CRS if needed
  if (st_crs(euseamap_layer) != st_crs(point)) {
    point <- st_transform(point, st_crs(euseamap_layer))
  }

  # Spatial intersection
  # Disable S2 temporarily to avoid geometry validation issues
  old_s2 <- sf_use_s2()
  sf_use_s2(FALSE)
  on.exit(sf_use_s2(old_s2))

  habitat_data <- tryCatch({
    # Use st_filter which is much faster for point-in-polygon queries
    suppressMessages({
      suppressWarnings({
        st_filter(euseamap_layer, point, .predicate = st_intersects)
      })
    })
  }, error = function(e) {
    warning(sprintf("Spatial query failed: %s", conditionMessage(e)))
    return(NULL)
  })

  if (is.null(habitat_data) || nrow(habitat_data) == 0) {
    message(sprintf("⚠ No habitat data at location: %.4f°E, %.4f°N", lon, lat))
    message("  Location may be outside coverage area or on land")
    return(NULL)
  }

  # Initialize result
  result <- list(
    lon = lon,
    lat = lat,
    source = "EMODnet EUSeaMap"
  )

  # Try to extract EUNIS code (multiple possible field names)
  # EUSeaMap_2025 uses: EUNIScomb
  eunis_fields <- c("EUNIScomb", "eunis_code", "eunis", "hab_code", "EUNIS_code", "Code",
                    "EUNIS", "EunisCode", "code", "habitat_code")
  for (field in eunis_fields) {
    if (field %in% names(habitat_data)) {
      result$eunis_code <- as.character(habitat_data[[field]][1])
      break
    }
  }

  # Try to extract description
  # EUSeaMap_2025 uses: EUNIScombD
  desc_fields <- c("EUNIScombD", "AllcombD", "description", "habitat_desc", "hab_desc", "Description",
                   "Habitat", "habitat_name", "name", "full_name")
  for (field in desc_fields) {
    if (field %in% names(habitat_data)) {
      result$eunis_description <- as.character(habitat_data[[field]][1])
      break
    }
  }

  # Try to extract substrate
  # EUSeaMap_2025 uses: Substrate
  substrate_fields <- c("Substrate", "substrate", "substrate_type", "sediment",
                        "substratum", "substrat", "sed_type")
  for (field in substrate_fields) {
    if (field %in% names(habitat_data)) {
      result$substrate_type <- as.character(habitat_data[[field]][1])
      break
    }
  }

  # Try to extract depth zone
  depth_fields <- c("depth_zone", "zone", "Zone", "depth", "depth_class",
                    "bathyal_zone", "bathy_zone")
  for (field in depth_fields) {
    if (field %in% names(habitat_data)) {
      result$depth_zone <- as.character(habitat_data[[field]][1])
      break
    }
  }

  # Try to extract biological zone
  # EUSeaMap_2025 uses: Biozone
  bio_fields <- c("Biozone", "bio_zone", "biological_zone", "biozone", "biol_zone",
                  "light_zone", "photic_zone")
  for (field in bio_fields) {
    if (field %in% names(habitat_data)) {
      result$biological_zone <- as.character(habitat_data[[field]][1])
      break
    }
  }

  # Try to extract energy level
  # EUSeaMap_2025 uses: Energy
  energy_fields <- c("Energy", "energy", "energy_level", "wave_exposure", "exposure")
  for (field in energy_fields) {
    if (field %in% names(habitat_data)) {
      result$energy_level <- as.character(habitat_data[[field]][1])
      break
    }
  }

  # Log success
  eunis_str <- if (!is.null(result$eunis_code)) result$eunis_code else "unknown"
  message(sprintf("✓ Habitat found: %s at %.4f°E, %.4f°N", eunis_str, lon, lat))

  return(result)
}

#' Add Habitat Data to Species Data Frame
#'
#' @param species_df Data frame with species information
#' @param sampling_lon Sampling location longitude
#' @param sampling_lat Sampling location latitude
#' @param euseamap_layer sf object with habitat data
#' @return Enhanced species data frame with habitat columns
#'
#' @details
#' Queries habitat at the sampling location once, then applies the same
#' habitat to all species in the data frame. This is ecologically correct
#' since all species in a food web sample share the same habitat location.
#'
#' Adds columns:
#' - eunis_code: EUNIS habitat code
#' - eunis_habitat: Habitat description
#' - substrate: Substrate type
#' - depth_zone: Depth classification
#' - biological_zone: Biological zone
#' - habitat_source: Data source
#' - sampling_lon: Sampling longitude
#' - sampling_lat: Sampling latitude
#'
#' @examples
#' \dontrun{
#' euseamap <- load_euseamap()
#' species_data <- add_habitat_to_species(species_data, 18.65, 54.52, euseamap)
#' }
#'
#' @export
add_habitat_to_species <- function(species_df, sampling_lon, sampling_lat, euseamap_layer) {
  # Validate inputs
  if (!is.data.frame(species_df)) {
    stop("species_df must be a data frame")
  }

  if (nrow(species_df) == 0) {
    warning("species_df is empty")
    return(species_df)
  }

  # Get habitat at sampling location (once for all species)
  message(sprintf("Querying habitat at: %.4f°E, %.4f°N", sampling_lon, sampling_lat))
  habitat <- get_habitat_at_location(sampling_lon, sampling_lat, euseamap_layer)

  if (is.null(habitat)) {
    warning("Could not retrieve habitat data for the sampling location")

    # Add NA columns
    species_df$eunis_code <- NA_character_
    species_df$eunis_habitat <- NA_character_
    species_df$substrate <- NA_character_
    species_df$depth_zone <- NA_character_
    species_df$biological_zone <- NA_character_
    species_df$energy_level <- NA_character_
    species_df$habitat_source <- "None (location outside coverage)"
    species_df$sampling_lon <- sampling_lon
    species_df$sampling_lat <- sampling_lat

    return(species_df)
  }

  # Add habitat columns to all species
  species_df$eunis_code <- habitat$eunis_code %||% NA_character_
  species_df$eunis_habitat <- habitat$eunis_description %||% NA_character_
  species_df$substrate <- habitat$substrate_type %||% NA_character_
  species_df$depth_zone <- habitat$depth_zone %||% NA_character_
  species_df$biological_zone <- habitat$biological_zone %||% NA_character_
  species_df$energy_level <- habitat$energy_level %||% NA_character_
  species_df$habitat_source <- "EMODnet EUSeaMap"
  species_df$sampling_lon <- sampling_lon
  species_df$sampling_lat <- sampling_lat

  message(sprintf("✓ Added habitat data to %d species", nrow(species_df)))

  # Summary of habitat assignment
  if (!is.na(species_df$eunis_code[1])) {
    message(sprintf("  EUNIS: %s - %s",
                    species_df$eunis_code[1],
                    species_df$eunis_habitat[1]))
  }

  return(species_df)
}

#' Get Available Field Names from EUSeaMap Layer
#'
#' @param euseamap_layer sf object with EUSeaMap data
#' @return Character vector of field names
#'
#' @details
#' Helper function to inspect available attribute fields in the EUSeaMap layer.
#' Useful for debugging when field names don't match expected values.
#'
#' @examples
#' \dontrun{
#' euseamap <- load_euseamap()
#' fields <- get_euseamap_fields(euseamap)
#' print(fields)
#' }
#'
#' @export
get_euseamap_fields <- function(euseamap_layer) {
  fields <- names(euseamap_layer)
  # Remove geometry column
  fields <- fields[fields != attr(euseamap_layer, "sf_column")]
  return(fields)
}

#' Print EUSeaMap Layer Summary
#'
#' @param euseamap_layer sf object with EUSeaMap data
#'
#' @details
#' Prints a summary of the EUSeaMap layer including:
#' - Number of polygons
#' - CRS information
#' - Available attribute fields
#' - Spatial extent
#'
#' @examples
#' \dontrun{
#' euseamap <- load_euseamap()
#' print_euseamap_summary(euseamap)
#' }
#'
#' @export
print_euseamap_summary <- function(euseamap_layer) {
  cat("EMODnet EUSeaMap Layer Summary\n")
  cat("==============================\n\n")

  cat(sprintf("Polygons: %d\n", nrow(euseamap_layer)))
  cat(sprintf("CRS: %s\n", st_crs(euseamap_layer)$input))

  bbox <- st_bbox(euseamap_layer)
  cat(sprintf("Extent: %.2f°E to %.2f°E, %.2f°N to %.2f°N\n",
              bbox["xmin"], bbox["xmax"], bbox["ymin"], bbox["ymax"]))

  cat("\nAvailable fields:\n")
  fields <- get_euseamap_fields(euseamap_layer)
  for (field in fields) {
    cat(sprintf("  - %s\n", field))
  }

  cat("\nSample data (first polygon):\n")
  print(st_drop_geometry(euseamap_layer[1, ]))
}

# NOTE: %||% operator is now defined in validation_utils.R

#' Clip Habitat Data to Study Area
#'
#' @param euseamap_layer sf object with EUSeaMap habitat polygons
#' @param study_area_boundary sf object with study area polygon(s)
#' @return sf object with clipped habitat data
#'
#' @details
#' Clips the EUSeaMap habitat layer to the study area boundary.
#' Useful for reducing data size and focusing on area of interest.
#'
#' @examples
#' \dontrun{
#' euseamap <- load_euseamap()
#' study_area <- st_read("study_area.gpkg")
#' clipped <- clip_habitat_to_study_area(euseamap, study_area)
#' }
#'
#' @export
clip_habitat_to_study_area <- function(euseamap_layer, study_area_boundary) {

  # Validate inputs
  if (!inherits(euseamap_layer, "sf")) {
    stop("euseamap_layer must be an sf object")
  }

  if (!inherits(study_area_boundary, "sf")) {
    stop("study_area_boundary must be an sf object")
  }

  message("Clipping habitat data to study area...")
  message(sprintf("  Input habitats: %d polygons", nrow(euseamap_layer)))

  # Ensure same CRS
  if (st_crs(euseamap_layer) != st_crs(study_area_boundary)) {
    message("  Transforming study area to habitat CRS...")
    study_area_boundary <- st_transform(study_area_boundary, st_crs(euseamap_layer))
  }

  # Disable S2 for clipping (avoids geometry issues)
  old_s2 <- sf_use_s2()
  sf_use_s2(FALSE)
  on.exit(sf_use_s2(old_s2))

  # Union study area if multiple polygons
  if (nrow(study_area_boundary) > 1) {
    message("  Unioning multiple study area polygons...")
    study_area_boundary <- st_union(study_area_boundary)
    study_area_boundary <- st_sf(geometry = study_area_boundary)
  }

  # Clip using st_intersection
  clipped <- tryCatch({
    suppressMessages({
      suppressWarnings({
        st_intersection(euseamap_layer, study_area_boundary)
      })
    })
  }, error = function(e) {
    stop("Failed to clip habitat data: ", e$message)
  })

  message(sprintf("✓ Clipped to %d habitat polygons", nrow(clipped)))
  message(sprintf("✓ Memory reduced: %.1f MB → %.1f MB",
                  object.size(euseamap_layer) / 1024^2,
                  object.size(clipped) / 1024^2))

  return(clipped)
}

#' Overlay Habitat with Grid Cells
#'
#' @param grid_cells sf object with hexagonal grid cells
#' @param habitat_layer sf object with habitat polygons (clipped)
#' @return sf object with grid cells enriched with habitat attributes
#'
#' @details
#' Performs spatial overlay between grid cells and habitat polygons.
#' Calculates habitat statistics for each grid cell:
#' - Dominant EUNIS code (most common)
#' - Dominant habitat description
#' - Dominant substrate type
#' - Habitat diversity (Shannon index)
#' - Number of different habitats
#' - Substrate composition (% of each type)
#'
#' @examples
#' \dontrun{
#' grid <- create_hexagonal_grid(bbox, cell_size = 2000)
#' habitat <- clip_habitat_to_study_area(euseamap, study_area)
#' grid_with_habitat <- overlay_habitat_with_grid(grid, habitat)
#' }
#'
#' @export
overlay_habitat_with_grid <- function(grid_cells, habitat_layer) {

  # Validate inputs
  if (!inherits(grid_cells, "sf")) {
    stop("grid_cells must be an sf object")
  }

  if (!inherits(habitat_layer, "sf")) {
    stop("habitat_layer must be an sf object")
  }

  message("Overlaying habitat with grid cells...")
  message(sprintf("  Grid cells: %d", nrow(grid_cells)))
  message(sprintf("  Habitat polygons: %d", nrow(habitat_layer)))

  # Ensure same CRS
  if (st_crs(grid_cells) != st_crs(habitat_layer)) {
    message("  Transforming habitat to grid CRS...")
    habitat_layer <- st_transform(habitat_layer, st_crs(grid_cells))
  }

  # Ensure grid has cell_id
  if (!"cell_id" %in% names(grid_cells)) {
    grid_cells$cell_id <- seq_len(nrow(grid_cells))
  }

  # Disable S2 for overlay
  old_s2 <- sf_use_s2()
  sf_use_s2(FALSE)
  on.exit(sf_use_s2(old_s2))

  # Perform intersection to get habitat-grid overlaps
  message("  Computing spatial intersection...")
  intersections <- tryCatch({
    suppressMessages({
      suppressWarnings({
        st_intersection(
          grid_cells[, "cell_id"],
          habitat_layer[, c("EUNIScomb", "EUNIScombD", "Substrate", "Biozone", "Energy")]
        )
      })
    })
  }, error = function(e) {
    stop("Failed to overlay habitat with grid: ", e$message)
  })

  # Calculate area of each intersection
  message("  Calculating intersection areas...")
  intersections$overlap_area <- as.numeric(st_area(intersections))

  # Calculate statistics per grid cell
  message("  Computing habitat statistics per cell...")

  # Function to calculate dominant value by area
  get_dominant <- function(values, areas) {
    if (all(is.na(values)) || length(values) == 0) return(NA_character_)
    agg <- tapply(areas, values, sum, na.rm = TRUE)
    names(agg)[which.max(agg)]
  }

  # Function to calculate Shannon diversity
  shannon_diversity <- function(values, areas) {
    if (all(is.na(values)) || length(values) == 0) return(0)
    props <- tapply(areas, values, sum, na.rm = TRUE)
    props <- props[!is.na(props) & props > 0]
    props <- props / sum(props)
    -sum(props * log(props))
  }

  # Function to calculate substrate percentages
  calc_substrate_pct <- function(substrate, areas, cell_total_area) {
    if (all(is.na(substrate)) || length(substrate) == 0) {
      return(list(Sand = 0, Mud = 0, Rock = 0, Mixed = 0, Other = 0))
    }

    # Standardize substrate names
    substrate_std <- sapply(substrate, function(x) {
      if (is.na(x)) return("Other")
      x <- tolower(as.character(x))
      if (grepl("sand", x)) return("Sand")
      if (grepl("mud", x)) return("Mud")
      if (grepl("rock|stone|boulder", x)) return("Rock")
      if (grepl("mixed|heterogeneous", x)) return("Mixed")
      return("Other")
    })

    # Sum areas by substrate type
    substrate_areas <- tapply(areas, substrate_std, sum, na.rm = TRUE)

    # Convert to percentages
    pct <- (substrate_areas / cell_total_area) * 100

    # Ensure all types are present
    all_types <- c("Sand", "Mud", "Rock", "Mixed", "Other")
    result <- setNames(rep(0, length(all_types)), all_types)
    result[names(pct)] <- pct

    as.list(result)
  }

  # Aggregate by cell_id
  habitat_stats <- do.call(rbind, lapply(split(intersections, intersections$cell_id), function(cell_data) {
    cell_id <- cell_data$cell_id[1]
    total_area <- sum(cell_data$overlap_area, na.rm = TRUE)

    # Get dominant values
    dominant_eunis <- get_dominant(cell_data$EUNIScomb, cell_data$overlap_area)
    dominant_habitat <- get_dominant(cell_data$EUNIScombD, cell_data$overlap_area)
    dominant_substrate <- get_dominant(cell_data$Substrate, cell_data$overlap_area)
    dominant_biozone <- get_dominant(cell_data$Biozone, cell_data$overlap_area)
    dominant_energy <- get_dominant(cell_data$Energy, cell_data$overlap_area)

    # Calculate diversity
    n_habitats <- length(unique(cell_data$EUNIScomb[!is.na(cell_data$EUNIScomb)]))
    habitat_diversity <- shannon_diversity(cell_data$EUNIScomb, cell_data$overlap_area)

    # Calculate substrate composition
    substrate_pct <- calc_substrate_pct(cell_data$Substrate, cell_data$overlap_area, total_area)

    # Return as data frame row
    data.frame(
      cell_id = cell_id,
      dominant_eunis = dominant_eunis,
      dominant_habitat = dominant_habitat,
      dominant_substrate = dominant_substrate,
      dominant_biozone = dominant_biozone,
      dominant_energy = dominant_energy,
      n_habitats = n_habitats,
      habitat_diversity = round(habitat_diversity, 3),
      habitat_area_km2 = round(total_area / 1e6, 3),
      substrate_sand_pct = round(substrate_pct$Sand, 1),
      substrate_mud_pct = round(substrate_pct$Mud, 1),
      substrate_rock_pct = round(substrate_pct$Rock, 1),
      substrate_mixed_pct = round(substrate_pct$Mixed, 1),
      substrate_other_pct = round(substrate_pct$Other, 1),
      stringsAsFactors = FALSE
    )
  }))

  # Merge with grid cells
  grid_with_habitat <- merge(
    grid_cells,
    habitat_stats,
    by = "cell_id",
    all.x = TRUE
  )

  # Fill NA values for cells with no habitat data
  habitat_cols <- c("dominant_eunis", "dominant_habitat", "dominant_substrate",
                    "dominant_biozone", "dominant_energy")
  for (col in habitat_cols) {
    grid_with_habitat[[col]][is.na(grid_with_habitat[[col]])] <- "No data"
  }

  numeric_cols <- c("n_habitats", "habitat_diversity", "habitat_area_km2",
                    "substrate_sand_pct", "substrate_mud_pct", "substrate_rock_pct",
                    "substrate_mixed_pct", "substrate_other_pct")
  for (col in numeric_cols) {
    grid_with_habitat[[col]][is.na(grid_with_habitat[[col]])] <- 0
  }

  message(sprintf("✓ Enriched %d grid cells with habitat data", nrow(grid_with_habitat)))
  message(sprintf("✓ Cells with habitat: %d (%.1f%%)",
                  sum(habitat_stats$n_habitats > 0),
                  100 * sum(habitat_stats$n_habitats > 0) / nrow(grid_cells)))

  return(grid_with_habitat)
}

#' Test EMODnet Integration
#'
#' @param euseamap_path Path to EUSeaMap GeoPackage file
#' @param test_locations Data frame with test locations (lon, lat, expected_habitat)
#'
#' @details
#' Runs integration tests for EMODnet habitat queries.
#' Useful for verifying the setup is working correctly.
#'
#' @examples
#' \dontrun{
#' test_locations <- data.frame(
#'   lon = c(18.65, 18.3),
#'   lat = c(54.52, 59.3),
#'   name = c("Gdansk Bay", "Stockholm"),
#'   expected_eunis = c("A5.3", "A3")
#' )
#' test_emodnet_integration(test_locations = test_locations)
#' }
#'
#' @export
test_emodnet_integration <- function(euseamap_path = "data/euseamap/EUSeaMap_Baltic.gpkg",
                                     test_locations = NULL) {
  cat("Testing EMODnet Integration\n")
  cat("===========================\n\n")

  # Default test locations
  if (is.null(test_locations)) {
    test_locations <- data.frame(
      lon = c(18.6466, 18.3, 11.5, 21.0),
      lat = c(54.5189, 59.3, 55.5, 58.0),
      name = c("Gdansk Bay", "Stockholm Archipelago", "Danish Straits", "Gotland"),
      stringsAsFactors = FALSE
    )
  }

  # Test 1: Load layer
  cat("Test 1: Loading EUSeaMap layer\n")
  cat("-------------------------------\n")
  euseamap <- tryCatch({
    load_euseamap(euseamap_path)
  }, error = function(e) {
    cat("✗ FAILED:", conditionMessage(e), "\n")
    return(NULL)
  })

  if (is.null(euseamap)) {
    return(invisible(FALSE))
  }

  cat("✓ PASSED\n\n")

  # Test 2: Query habitats
  cat("Test 2: Querying habitat at test locations\n")
  cat("-------------------------------------------\n")

  results <- list()
  for (i in 1:nrow(test_locations)) {
    loc <- test_locations[i, ]
    cat(sprintf("\n[%d/%d] %s (%.4f°E, %.4f°N)\n",
                i, nrow(test_locations), loc$name, loc$lon, loc$lat))

    habitat <- get_habitat_at_location(loc$lon, loc$lat, euseamap)

    if (!is.null(habitat)) {
      cat(sprintf("  EUNIS: %s\n", habitat$eunis_code %||% "Not available"))
      cat(sprintf("  Habitat: %s\n", habitat$eunis_description %||% "Not available"))
      cat(sprintf("  Substrate: %s\n", habitat$substrate_type %||% "Not available"))
      results[[i]] <- "✓ PASSED"
    } else {
      results[[i]] <- "✗ FAILED (no habitat found)"
    }
  }

  cat("\n\nTest Summary\n")
  cat("------------\n")
  for (i in 1:length(results)) {
    cat(sprintf("[%d] %s: %s\n", i, test_locations$name[i], results[[i]]))
  }

  cat("\n✓ Integration test complete\n")
  return(invisible(TRUE))
}
