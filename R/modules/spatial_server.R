#' Spatial Analysis Server Module
#'
#' Handles the full spatial analysis workflow: study area upload, BBT polygon
#' selection, hexagonal grid creation, species data upload, EMODnet habitat
#' integration, local network extraction, spatial metrics calculation,
#' leaflet map visualization, and download handlers.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param net_reactive ReactiveVal holding the igraph network
#' @param info_reactive ReactiveVal holding the species info data frame
#' @param current_metaweb ReactiveVal holding the current metaweb object
#' @param euseamap_data ReactiveVal for cached EUSeaMap data (shared with ecopath_import_server)
spatial_server <- function(input, output, session, net_reactive, info_reactive,
                            current_metaweb, euseamap_data) {

  # Reactive values for spatial analysis (local to this module)
  spatial_hex_grid <- reactiveVal(NULL)
  spatial_species_data <- reactiveVal(NULL)
  spatial_local_networks <- reactiveVal(NULL)
  spatial_metrics_data <- reactiveVal(NULL)
  spatial_metaweb <- reactiveVal(NULL)
  spatial_study_area <- reactiveVal(NULL)
  spatial_habitat_clipped <- reactiveVal(NULL)  # Clipped habitat data
  spatial_grid_with_habitat <- reactiveVal(NULL)  # Grid enriched with habitat

  # 0. Study Area Upload Handler (NEW)
  observeEvent(input$spatial_study_area, {
    req(input$spatial_study_area)

    tryCatch({
      uploaded_files <- input$spatial_study_area
      file_names <- uploaded_files$name
      file_paths <- uploaded_files$datapath

      # Determine file type
      if (any(grepl("\\.gpkg$", file_names, ignore.case = TRUE))) {
        # GeoPackage: single file
        gpkg_file <- file_paths[grepl("\\.gpkg$", file_names, ignore.case = TRUE)][1]
        study_area <- sf::st_read(gpkg_file, quiet = TRUE)

      } else if (any(grepl("\\.shp$", file_names, ignore.case = TRUE))) {
        # Shapefile: multiple components

        # Validate required components
        required_exts <- c(".shp", ".shx", ".dbf")
        has_required <- sapply(required_exts, function(ext) {
          any(grepl(paste0("\\", ext, "$"), file_names, ignore.case = TRUE))
        })

        if (!all(has_required)) {
          missing <- required_exts[!has_required]

          error_msg <- paste0(
            "Missing required shapefile components: ",
            paste(missing, collapse = ", "),
            "\n\n",
            "HOW TO FIX:\n",
            "1. Click 'Browse' button\n",
            "2. Navigate to your shapefile folder\n",
            "3. Hold Ctrl (Windows) or Cmd (Mac)\n",
            "4. Select ALL these files:\n",
            "   - ", basename(file_names[1]), " (you selected this)\n",
            if (!has_required[2]) "   - [missing] .shx file\n" else "",
            if (!has_required[3]) "   - [missing] .dbf file\n" else "",
            "   - .prj file (optional)\n",
            "5. Click 'Open'\n\n",
            "All files must have the same base name (e.g., boundary.shp, boundary.shx, boundary.dbf)"
          )

          stop(error_msg)
        }

        # Check for .prj (projection file)
        if (!any(grepl("\\.prj$", file_names, ignore.case = TRUE))) {
          showNotification(
            "Warning: No .prj file found. CRS may not be correctly defined.",
            type = "warning",
            duration = 8
          )
        }

        # Create temporary directory for shapefile components
        temp_dir <- file.path(tempdir(), "shapefile_upload")
        dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

        # Copy all files to temp directory with consistent naming
        base_name <- "study_area"
        for (i in seq_along(file_names)) {
          ext <- tools::file_ext(file_names[i])
          dest_file <- file.path(temp_dir, paste0(base_name, ".", ext))
          file.copy(file_paths[i], dest_file, overwrite = TRUE)
        }

        # Read shapefile
        shp_path <- file.path(temp_dir, paste0(base_name, ".shp"))
        study_area <- sf::st_read(shp_path, quiet = TRUE)

      } else {
        stop("Please upload either a .gpkg file or complete shapefile (.shp, .shx, .dbf, .prj)")
      }

      # Validate CRS
      if (is.na(sf::st_crs(study_area))) {
        showNotification(
          "Warning: Study area CRS is not defined. Assuming EPSG:4326 (WGS84).",
          type = "warning",
          duration = 8
        )
        sf::st_crs(study_area) <- 4326
      }

      # Transform to WGS84 if needed
      crs_current <- sf::st_crs(study_area)
      is_wgs84 <- FALSE

      # Check if already WGS84
      if (!is.na(crs_current$epsg) && crs_current$epsg == 4326) {
        is_wgs84 <- TRUE
      } else if (!is.na(crs_current$input)) {
        # Check if it's a longlat system
        crs_text <- tolower(crs_current$input)
        if (grepl("\\+proj=longlat", crs_text) && grepl("wgs.*84", crs_text)) {
          is_wgs84 <- TRUE
        }
      }

      # Transform if not WGS84
      if (!is_wgs84) {
        original_crs <- crs_current$input
        study_area <- sf::st_transform(study_area, crs = 4326)
        showNotification(
          sprintf("Study area transformed from %s to WGS84",
                  substr(original_crs, 1, 50)),
          type = "message",
          duration = 5
        )
      }

      # Validate geometry
      if (!all(sf::st_is_valid(study_area))) {
        study_area <- sf::st_make_valid(study_area)
        showNotification("Invalid geometry detected and repaired",
                        type = "warning", duration = 5)
      }

      # Dissolve multi-polygon to single boundary if needed
      if (nrow(study_area) > 1) {
        study_area <- sf::st_union(study_area)
        study_area <- sf::st_sf(geometry = study_area)
      }

      # Store in reactive value (this will trigger reactive outputs to update)
      spatial_study_area(study_area)

      # Reset BBT selector since custom file is being used
      updateSelectInput(session, "spatial_bbt_selector", selected = "")

      # Auto-fill bounding box inputs
      bbox <- sf::st_bbox(study_area)
      updateNumericInput(session, "spatial_xmin", value = round(as.numeric(bbox["xmin"]), 3))
      updateNumericInput(session, "spatial_ymin", value = round(as.numeric(bbox["ymin"]), 3))
      updateNumericInput(session, "spatial_xmax", value = round(as.numeric(bbox["xmax"]), 3))
      updateNumericInput(session, "spatial_ymax", value = round(as.numeric(bbox["ymax"]), 3))

      # Update main spatial map with study area (Visualization tab)
      suppressWarnings({
        # Prepare geometry for main map
        study_area_map <- study_area

        # Convert MULTIPOLYGON to POLYGON for better display
        geom_type_upload <- as.character(sf::st_geometry_type(study_area_map, by_geometry = FALSE))
        if (geom_type_upload == "MULTIPOLYGON") {
          study_area_map <- sf::st_cast(study_area_map, "POLYGON")
          if (nrow(study_area_map) > 1) {
            areas <- sf::st_area(study_area_map)
            study_area_map <- study_area_map[which.max(areas), ]
          }
        }

        # Get fresh bbox after geometry conversion
        bbox_upload <- sf::st_bbox(study_area_map)
        xmin_upload <- as.numeric(bbox_upload["xmin"])
        ymin_upload <- as.numeric(bbox_upload["ymin"])
        xmax_upload <- as.numeric(bbox_upload["xmax"])
        ymax_upload <- as.numeric(bbox_upload["ymax"])

        leafletProxy("spatial_map") %>%
          clearGroup("Study Area") %>%
          addPolygons(
            data = study_area_map,
            color = "#0033cc",
            weight = 2,
            opacity = 0.6,
            fillOpacity = 0.15,
            fillColor = "#3366ff",
            group = "Study Area",
            label = "Custom Study Area",
            labelOptions = labelOptions(
              style = list("font-weight" = "bold", padding = "3px 8px"),
              textsize = "13px",
              direction = "auto"
            ),
            highlightOptions = highlightOptions(
              weight = 4,
              color = "#FF0000",
              opacity = 0.8,
              fillOpacity = 0.4,
              bringToFront = TRUE
            )
          ) %>%
          showGroup("Study Area") %>%
          fitBounds(lng1 = xmin_upload, lat1 = ymin_upload,
                    lng2 = xmax_upload, lat2 = ymax_upload)
      })

      showNotification(
        "Study area loaded. Grid will be clipped to this boundary.",
        type = "message",
        duration = 5
      )

    }, error = function(e) {
      showNotification(
        HTML(paste0(
          "<strong>Error Loading Study Area</strong><br><br>",
          gsub("\n", "<br>", e$message),
          "<br><br><em>Check the console for details.</em>"
        )),
        type = "error",
        duration = 15
      )

      cat("\nERROR loading study area:\n")
      cat(e$message, "\n\n")
    })
  })

  # BBT Polygon Selection Handler
  observeEvent(input$spatial_bbt_selector, {
    req(input$spatial_bbt_selector)

    # If empty selection, do nothing (user wants to upload custom boundary)
    if (input$spatial_bbt_selector == "") {
      return()
    }

    tryCatch({
      # Load BBT.geojson file
      bbt_file <- file.path("data", "BBT.geojson")

      if (!file.exists(bbt_file)) {
        showNotification(
          "BBT.geojson file not found in data directory",
          type = "error",
          duration = 5
        )
        return()
      }

      # Read the entire BBT geojson
      # Disable s2 to avoid geometry issues with 3D coordinates
      sf::sf_use_s2(FALSE)
      bbt_data <- sf::st_read(bbt_file, quiet = TRUE)

      # Filter by selected BBT name
      selected_bbt <- bbt_data[bbt_data$Name == input$spatial_bbt_selector, ]

      if (nrow(selected_bbt) == 0) {
        showNotification(
          paste("BBT polygon not found:", input$spatial_bbt_selector),
          type = "error",
          duration = 5
        )
        sf::sf_use_s2(TRUE)
        return()
      }

      # Drop Z/M coordinates if present
      selected_bbt <- sf::st_zm(selected_bbt, drop = TRUE, what = "ZM")

      # Check for invalid geometries
      if (any(!sf::st_is_valid(selected_bbt))) {
        selected_bbt <- sf::st_make_valid(selected_bbt)
      }

      # Ensure CRS is WGS84
      crs_info <- sf::st_crs(selected_bbt)
      if (is.na(crs_info$input)) {
        sf::st_crs(selected_bbt) <- 4326
      } else if (is.null(crs_info$epsg) || crs_info$epsg != 4326) {
        selected_bbt <- sf::st_transform(selected_bbt, crs = 4326)
      }

      # Keep BBT name for display
      bbt_name <- input$spatial_bbt_selector

      # Dissolve to single boundary if needed
      if (nrow(selected_bbt) > 1) {
        selected_bbt <- sf::st_union(selected_bbt)
        selected_bbt <- sf::st_sf(
          Name = bbt_name,
          geometry = selected_bbt
        )
      } else {
        if (!"Name" %in% names(selected_bbt)) {
          selected_bbt$Name <- bbt_name
        }
      }

      # Simplify geometry if too complex (for leaflet rendering)
      n_coords <- nrow(sf::st_coordinates(selected_bbt))
      if (n_coords > 10000) {
        cat("\nSimplifying geometry (", n_coords, " coordinates) for map display...\n")
        selected_bbt <- sf::st_simplify(selected_bbt, dTolerance = 0.001, preserveTopology = TRUE)
        n_coords_new <- nrow(sf::st_coordinates(selected_bbt))
        cat("  Simplified to", n_coords_new, "coordinates\n")
      }

      # Re-enable s2
      sf::sf_use_s2(TRUE)

      # Store in reactive value
      spatial_study_area(selected_bbt)

      # Auto-fill bounding box inputs
      bbox <- sf::st_bbox(selected_bbt)
      updateNumericInput(session, "spatial_xmin", value = round(as.numeric(bbox["xmin"]), 3))
      updateNumericInput(session, "spatial_ymin", value = round(as.numeric(bbox["ymin"]), 3))
      updateNumericInput(session, "spatial_xmax", value = round(as.numeric(bbox["xmax"]), 3))
      updateNumericInput(session, "spatial_ymax", value = round(as.numeric(bbox["ymax"]), 3))

      # Debug output (only shown when ECO_NT_DEBUG=true)
      if (exists("log_debug_details")) {
        log_debug_details("Spatial", paste("BBT polygon loaded:", bbt_name),
          CRS = sf::st_crs(selected_bbt)$input,
          EPSG = sf::st_crs(selected_bbt)$epsg,
          Bbox = paste0("[", round(bbox["xmin"], 2), ",", round(bbox["ymin"], 2), "] to [",
                        round(bbox["xmax"], 2), ",", round(bbox["ymax"], 2), "]"),
          Area_km2 = round(as.numeric(sf::st_area(selected_bbt)) / 1e6, 2),
          Geometry_type = as.character(sf::st_geometry_type(selected_bbt, by_geometry = FALSE)),
          Geometry_valid = all(sf::st_is_valid(selected_bbt)),
          Coordinates = nrow(sf::st_coordinates(selected_bbt))
        )
      }

      # Update main spatial map with study area (Visualization tab)
      cat("\nAdding BBT to Visualization map...\n")
      suppressWarnings({
        selected_bbt_map <- selected_bbt

        crs_epsg <- sf::st_crs(selected_bbt_map)$epsg
        cat("  BBT CRS before:", sf::st_crs(selected_bbt_map)$input, "\n")
        if (is.null(crs_epsg) || is.na(crs_epsg) || crs_epsg != 4326) {
          if (is.null(crs_epsg) || is.na(crs_epsg)) {
            sf::st_crs(selected_bbt_map) <- 4326
          } else {
            selected_bbt_map <- sf::st_transform(selected_bbt_map, crs = 4326)
          }
          cat("  Transformed to WGS84\n")
        }

        geom_type_main <- as.character(sf::st_geometry_type(selected_bbt_map, by_geometry = FALSE))
        cat("  Geometry type:", geom_type_main, "\n")
        if (geom_type_main == "MULTIPOLYGON") {
          selected_bbt_map <- sf::st_cast(selected_bbt_map, "POLYGON")
          if (nrow(selected_bbt_map) > 1) {
            areas <- sf::st_area(selected_bbt_map)
            selected_bbt_map <- selected_bbt_map[which.max(areas), ]
            cat("  Selected largest polygon from", length(areas), "parts\n")
          }
        }

        bbox_main <- sf::st_bbox(selected_bbt_map)
        xmin_main <- as.numeric(bbox_main["xmin"])
        ymin_main <- as.numeric(bbox_main["ymin"])
        xmax_main <- as.numeric(bbox_main["xmax"])
        ymax_main <- as.numeric(bbox_main["ymax"])
        cat("  Bbox:", xmin_main, ymin_main, "to", xmax_main, ymax_main, "\n")
        cat("  Rows:", nrow(selected_bbt_map), "\n")

        leafletProxy("spatial_map") %>%
          clearGroup("Study Area") %>%
          addPolygons(
            data = selected_bbt_map,
            color = "#0033cc",
            weight = 2,
            opacity = 0.6,
            fillOpacity = 0.15,
            fillColor = "#3366ff",
            group = "Study Area",
            label = ~paste0("Study Area: ", Name),
            labelOptions = labelOptions(
              style = list("font-weight" = "bold", padding = "3px 8px"),
              textsize = "13px",
              direction = "auto"
            ),
            highlightOptions = highlightOptions(
              weight = 4,
              color = "#FF0000",
              opacity = 0.8,
              fillOpacity = 0.4,
              bringToFront = TRUE
            )
          ) %>%
          showGroup("Study Area") %>%
          fitBounds(lng1 = xmin_main, lat1 = ymin_main,
                    lng2 = xmax_main, lat2 = ymax_main)

        cat("  BBT added to Visualization map\n\n")
      })

      showNotification(
        HTML(paste0(
          "<strong>BBT Polygon Loaded: ", bbt_name, "</strong><br>",
          "Area: ", round(as.numeric(sf::st_area(selected_bbt)) / 1e6, 2), " km2<br>",
          "Check the Study Area Preview map and bounding box values"
        )),
        type = "message",
        duration = 8
      )

    }, error = function(e) {
      sf::sf_use_s2(TRUE)

      showNotification(
        HTML(paste0(
          "<strong>Error Loading BBT Polygon</strong><br><br>",
          gsub("\n", "<br>", e$message)
        )),
        type = "error",
        duration = 10
      )

      cat("\nERROR loading BBT polygon:\n")
      cat(e$message, "\n\n")
    })
  })

  # Reactive study area preview map (updates when study area changes)
  output$spatial_study_area_map <- renderLeaflet({
    study_area <- spatial_study_area()

    cat("\nRendering spatial_study_area_map...\n")

    if (is.null(study_area)) {
      cat("  No study area - showing empty map\n")
      leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
        addTiles(
          urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
          attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
        ) %>%
        setView(lng = 18.5, lat = 57, zoom = 5) %>%
        addControl(
          html = "<div style='padding: 10px; background: white; border-radius: 5px;'>
                  <i class='fa fa-info-circle'></i> Upload a study area to see preview
                  </div>",
          position = "topright"
        )
    } else {
      cat("  Study area present, rendering polygon...\n")

      tryCatch({
        crs_epsg <- sf::st_crs(study_area)$epsg
        if (is.null(crs_epsg) || is.na(crs_epsg) || crs_epsg != 4326) {
          if (is.null(crs_epsg) || is.na(crs_epsg)) {
            sf::st_crs(study_area) <- 4326
          } else {
            study_area <- sf::st_transform(study_area, crs = 4326)
          }
        }

        cat("  Converting geometry for leaflet...\n")
        geom_type <- as.character(sf::st_geometry_type(study_area, by_geometry = FALSE))

        if (geom_type == "MULTIPOLYGON") {
          cat("  Casting MULTIPOLYGON to POLYGON...\n")
          study_area <- sf::st_cast(study_area, "POLYGON")
          if (nrow(study_area) > 1) {
            areas <- sf::st_area(study_area)
            study_area <- study_area[which.max(areas), ]
            cat("  Selected largest polygon from", length(areas), "parts\n")
          }
        }

        bbox <- sf::st_bbox(study_area)
        xmin <- as.numeric(bbox["xmin"])
        ymin <- as.numeric(bbox["ymin"])
        xmax <- as.numeric(bbox["xmax"])
        ymax <- as.numeric(bbox["ymax"])
        cat("  Bbox for map:", xmin, ",", ymin, "to", xmax, ",", ymax, "\n")

        popup_text <- paste0("Area: ", round(as.numeric(sf::st_area(study_area)) / 1e6, 2), " km2")
        if ("Name" %in% names(study_area)) {
          popup_text <- paste0("<strong>", study_area$Name[1], "</strong><br>", popup_text)
        }

        cat("  Creating leaflet map...\n")
        m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
          addTiles(
            urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
            attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
          ) %>%
          setView(lng = mean(c(xmin, xmax)),
                  lat = mean(c(ymin, ymax)),
                  zoom = 8)

        cat("  Adding polygon to map (", nrow(study_area), "feature(s))...\n")
        m <- m %>%
          addPolygons(
            data = study_area,
            color = "#0033cc",
            weight = 2,
            opacity = 0.6,
            fillOpacity = 0.15,
            fillColor = "#3366ff",
            popup = popup_text,
            highlightOptions = highlightOptions(
              weight = 4,
              color = "#FF0000",
              opacity = 0.8,
              fillOpacity = 0.4,
              bringToFront = TRUE
            )
          )

        cat("  Fitting map bounds...\n")
        result <- m %>% fitBounds(
          lng1 = xmin, lat1 = ymin,
          lng2 = xmax, lat2 = ymax
        )

        cat("  Map rendered successfully\n\n")
        result

      }, error = function(e) {
        cat("  ERROR rendering map:", e$message, "\n\n")
        leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
          addTiles(
            urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
            attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
          ) %>%
          setView(lng = 18.5, lat = 57, zoom = 5) %>%
          addControl(
            html = paste0("<div style='padding: 10px; background: white; border-radius: 5px; color: red;'>
                    <i class='fa fa-exclamation-triangle'></i> Error: ", e$message, "
                    </div>"),
            position = "topright"
          )
      })
    }
  })

  # Reactive study area info (updates when study area changes)
  output$spatial_study_area_info <- renderPrint({
    study_area <- spatial_study_area()

    if (is.null(study_area)) {
      cat("No study area loaded.\n\n")
      cat("Upload a shapefile or GeoPackage to define your study area.\n")
    } else {
      bbox <- sf::st_bbox(study_area)
      cat("Study area loaded!\n\n")

      if ("Name" %in% names(study_area)) {
        cat("BBT Region:", study_area$Name[1], "\n")
      }

      cat("Geometry type:", as.character(sf::st_geometry_type(study_area, by_geometry = FALSE)), "\n")
      cat("CRS:", sf::st_crs(study_area)$input, "\n")
      cat("Features:", nrow(study_area), "\n")
      cat("Bounding box:\n")
      cat("  X: [", round(bbox["xmin"], 3), ",", round(bbox["xmax"], 3), "]\n")
      cat("  Y: [", round(bbox["ymin"], 3), ",", round(bbox["ymax"], 3), "]\n")
      cat("\nArea:", round(as.numeric(sf::st_area(study_area)) / 1e6, 2), "km2\n")
    }
  })

  # Clear Study Area Handler
  observeEvent(input$spatial_clear_study_area, {
    spatial_study_area(NULL)
    spatial_habitat_clipped(NULL)
    spatial_grid_with_habitat(NULL)

    updateSelectInput(session, "spatial_bbt_selector", selected = "")

    leafletProxy("spatial_map") %>%
      clearGroup("Study Area") %>%
      clearGroup("Habitat")

    showNotification("Study area cleared", type = "message", duration = 3)
  })

  # Helper output for conditionalPanel
  output$spatial_study_area_loaded <- reactive({
    !is.null(spatial_study_area())
  })
  outputOptions(output, "spatial_study_area_loaded", suspendWhenHidden = FALSE)

  # 1. Grid Creation
  observeEvent(input$spatial_create_grid, {
    tryCatch({
      bbox <- c(
        xmin = input$spatial_xmin,
        ymin = input$spatial_ymin,
        xmax = input$spatial_xmax,
        ymax = input$spatial_ymax
      )

      if (any(is.na(bbox))) {
        showNotification("Please fill all bounding box coordinates", type = "warning")
        return()
      }

      if (bbox["xmin"] >= bbox["xmax"] || bbox["ymin"] >= bbox["ymax"]) {
        showNotification("Invalid bounding box: min values must be less than max values", type = "error")
        return()
      }

      cell_size <- input$spatial_cell_size
      hex_grid <- create_hexagonal_grid(bbox, cell_size = cell_size, crs = 4326)

      original_count <- nrow(hex_grid)
      clipped <- FALSE

      if (!is.null(spatial_study_area())) {
        study_area <- spatial_study_area()
        hex_grid <- sf::st_intersection(hex_grid, study_area)

        suppressWarnings({
          centroids <- sf::st_coordinates(sf::st_centroid(hex_grid))
        })
        hex_grid$center_lon <- centroids[, 1]
        hex_grid$center_lat <- centroids[, 2]

        clipped <- TRUE
      }

      spatial_hex_grid(hex_grid)

      if (exists("log_debug_details")) {
        log_debug_details("Spatial", "Adding grid to map",
          CRS = sf::st_crs(hex_grid)$input,
          EPSG = sf::st_crs(hex_grid)$epsg,
          Rows = nrow(hex_grid)
        )
      }

      if (is.null(sf::st_crs(hex_grid)$epsg) || sf::st_crs(hex_grid)$epsg != 4326) {
        cat("  Transforming grid to WGS84...\n")
        hex_grid <- sf::st_transform(hex_grid, crs = 4326)
      }

      leafletProxy("spatial_map") %>%
        clearGroup("Grid") %>%
        addPolygons(
          data = hex_grid,
          group = "Grid",
          fillColor = "transparent",
          fillOpacity = 0.2,
          color = "#555555",
          weight = 1,
          popup = ~paste0("Hex ID: ", hex_id),
          label = ~paste0("Grid Cell ", hex_id),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "12px",
            direction = "auto"
          )
        ) %>%
        showGroup("Grid")

      cat("  Grid added to map\n\n")

      output$spatial_grid_info <- renderPrint({
        cat("Grid created successfully!\n\n")
        cat("Parameters:\n")
        cat("  Cell size:", cell_size, "m (", cell_size/1000, "km)\n")
        cat("  Bounding box: [", bbox["xmin"], ",", bbox["ymin"], "] to [",
            bbox["xmax"], ",", bbox["ymax"], "]\n\n")

        if (clipped) {
          cat("  Clipped to study area: YES\n")
          cat("  Hexagons before clipping:", original_count, "\n")
          cat("  Hexagons after clipping:", nrow(hex_grid), "\n")
          cat("  Hexagons removed:", original_count - nrow(hex_grid), "\n\n")
        }

        cat("Grid summary:\n")
        cat("  Total hexagons:", nrow(hex_grid), "\n")
        cat("  Coverage area:",
            round((bbox["xmax"] - bbox["xmin"]) * (bbox["ymax"] - bbox["ymin"]), 2),
            "degrees2\n")
      })

      notification_msg <- if (clipped) {
        paste0("Grid created & clipped: ", nrow(hex_grid), " hexagons (",
               original_count - nrow(hex_grid), " removed)")
      } else {
        paste("Grid created:", nrow(hex_grid), "hexagons")
      }

      showNotification(notification_msg, type = "message", duration = 5)
    }, error = function(e) {
      output$spatial_grid_info <- renderPrint({
        cat("ERROR creating grid:\n")
        cat(e$message, "\n")
      })
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # 2. Generate Sample Species Data
  observeEvent(input$spatial_generate_sample, {
    req(spatial_hex_grid())

    tryCatch({
      if (is.null(current_metaweb())) {
        showNotification("Please load a metaweb first (Metaweb Manager tab)", type = "warning")
        return()
      }

      metaweb <- current_metaweb()
      bbox <- c(
        xmin = input$spatial_xmin,
        ymin = input$spatial_ymin,
        xmax = input$spatial_xmax,
        ymax = input$spatial_ymax
      )

      n_species <- min(5, nrow(metaweb$species))
      sample_species <- sample(metaweb$species$species_name, n_species)

      set.seed(as.numeric(Sys.time()))
      n_occurrences <- 30

      species_data <- data.frame(
        lon = runif(n_occurrences, min = bbox["xmin"], max = bbox["xmax"]),
        lat = runif(n_occurrences, min = bbox["ymin"], max = bbox["ymax"]),
        species = sample(sample_species, n_occurrences, replace = TRUE),
        biomass = round(runif(n_occurrences, min = 5, max = 50), 1)
      )

      spatial_species_data(species_data)

      output$spatial_species_preview <- renderDT({
        datatable(
          species_data,
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            dom = 'tip'
          ),
          rownames = FALSE
        )
      })

      showNotification(
        paste("Generated", n_occurrences, "sample occurrences for", n_species, "species"),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # ============================================================================
  # 2. EMODnet Habitat Integration
  # ============================================================================

  observeEvent(input$spatial_enable_habitat, {
    if (input$spatial_enable_habitat && is.null(euseamap_data())) {
      showNotification("Loading EUSeaMap habitat data (optimized regional loading)...",
                       type = "message", duration = NULL, id = "spatial_emodnet_loading")

      tryCatch({
        bbt_name <- NULL
        custom_bbox <- NULL
        study_area_sf <- NULL

        if (!is.null(input$spatial_bbt_selector) && input$spatial_bbt_selector != "") {
          bbt_name <- input$spatial_bbt_selector
          study_area_sf <- spatial_study_area()

          if (is.null(study_area_sf)) {
            cat("\nStudy area not loaded yet, loading BBT polygon directly...\n")
            tryCatch({
              sf::sf_use_s2(FALSE)
              bbt_data <- sf::st_read("data/BBT.geojson", quiet = TRUE)
              study_area_sf <- bbt_data[bbt_data$Name == bbt_name, ]
              study_area_sf <- sf::st_zm(study_area_sf, drop = TRUE, what = "ZM")
              sf::sf_use_s2(TRUE)
              cat("BBT polygon loaded directly for habitat extraction\n")
            }, error = function(e) {
              cat("Failed to load BBT polygon:", conditionMessage(e), "\n")
              study_area_sf <- NULL
            })
          }

          cat("\nLoading habitat for BBT:", bbt_name, "\n")
        } else if (!is.null(spatial_study_area())) {
          bbox <- sf::st_bbox(spatial_study_area())
          custom_bbox <- c(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])
          study_area_sf <- spatial_study_area()
          cat("\nLoading habitat for custom study area\n")
        } else if (!is.null(spatial_hex_grid())) {
          bbox <- sf::st_bbox(spatial_hex_grid())
          custom_bbox <- c(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])
          cat("\nLoading habitat for grid area\n")
        }

        if (is.null(custom_bbox) && is.null(study_area_sf)) {
          cat("\nNo study area/grid defined yet, using default Baltic test area (1x1 deg)\n")
          custom_bbox <- c(20, 55, 21, 56)
        }

        euseamap <- NULL

        cat("  Loading habitat with safe minimal bbox strategy...\n")
        cat("  This loads a small test area. Use 'Clip Habitat' button to get full coverage.\n")

        minimal_bbox <- NULL

        if (!is.null(study_area_sf)) {
          bbox_full <- sf::st_bbox(study_area_sf)
          center_lon <- mean(c(bbox_full["xmin"], bbox_full["xmax"]))
          center_lat <- mean(c(bbox_full["ymin"], bbox_full["ymax"]))

          minimal_bbox <- c(center_lon - 0.5, center_lat - 0.5,
                           center_lon + 0.5, center_lat + 0.5)

          cat(sprintf("  Centered on study area: [%.2f, %.2f] to [%.2f, %.2f]\n",
                      minimal_bbox[1], minimal_bbox[2], minimal_bbox[3], minimal_bbox[4]))

        } else if (!is.null(custom_bbox)) {
          center_lon <- mean(c(custom_bbox[1], custom_bbox[3]))
          center_lat <- mean(c(custom_bbox[2], custom_bbox[4]))

          minimal_bbox <- c(center_lon - 0.5, center_lat - 0.5,
                           center_lon + 0.5, center_lat + 0.5)

          cat(sprintf("  Centered on custom bbox: [%.2f, %.2f] to [%.2f, %.2f]\n",
                      minimal_bbox[1], minimal_bbox[2], minimal_bbox[3], minimal_bbox[4]))

        } else {
          region_test_areas <- list(
            baltic = c(20, 55, 21, 56),
            mediterranean = c(25, 35, 26, 36),
            arctic = c(15, 78, 16, 79),
            north_sea = c(3, 54, 4, 55),
            atlantic = c(-10, 48, -9, 49)
          )

          test_region <- "baltic"
          if (!is.null(bbt_name)) {
            test_region <- tryCatch(get_region_for_bbt(bbt_name), error = function(e) "baltic")
          }

          minimal_bbox <- region_test_areas[[test_region]]
          cat(sprintf("  Using %s test area: [%.1f, %.1f] to [%.1f, %.1f]\n",
                      toupper(test_region), minimal_bbox[1], minimal_bbox[2],
                      minimal_bbox[3], minimal_bbox[4]))
        }

        euseamap <- load_regional_euseamap(
          custom_bbox = minimal_bbox,
          path = "data/EUSeaMap_2025/EUSeaMap_2025.gdb"
        )
        euseamap_data(euseamap)

        region <- attr(euseamap, "region") %||% "unknown"

        removeNotification("spatial_emodnet_loading")
        showNotification(
          sprintf("EUSeaMap loaded: %d polygons (%s region)", nrow(euseamap), toupper(region)),
          type = "message",
          duration = 5
        )
      }, error = function(e) {
        removeNotification("spatial_emodnet_loading")
        showNotification(
          paste("Failed to load EUSeaMap:", e$message,
                "\nPlease ensure EUSeaMap_2025.gdb exists in data/ directory"),
          type = "error",
          duration = 10
        )
        updateCheckboxInput(session, "spatial_enable_habitat", value = FALSE)
      })
    }
  })

  # Habitat status output
  output$spatial_habitat_status <- renderPrint({
    if (!isTRUE(input$spatial_enable_habitat)) {
      cat("Habitat analysis disabled\n\n")
      cat("Enable checkbox above to activate habitat integration")
      return()
    }

    if (is.null(euseamap_data())) {
      cat("Loading EUSeaMap data...\n")
      return()
    }

    cat("EUSeaMap loaded:", nrow(euseamap_data()), "polygons\n\n")

    if (!is.null(spatial_habitat_clipped())) {
      cat("Habitat clipped to study area\n")
      cat("  Clipped polygons:", nrow(spatial_habitat_clipped()), "\n\n")
    } else {
      cat("Step 1: Click 'Clip Habitat to Study Area'\n\n")
    }

    if (!is.null(spatial_grid_with_habitat())) {
      cat("Habitat overlaid with grid\n")
      grid_data <- spatial_grid_with_habitat()
      n_with_habitat <- sum(grid_data$n_habitats > 0, na.rm = TRUE)
      cat("  Enriched cells:", nrow(grid_data), "\n")
      cat("  Cells with habitat:", n_with_habitat,
          sprintf("(%.1f%%)\n", 100 * n_with_habitat / nrow(grid_data)))
      cat("\nReady! Grid cells now have habitat attributes")
    } else if (!is.null(spatial_habitat_clipped())) {
      cat("Step 2: Click 'Overlay with Grid Cells'\n")
    }
  })

  # Clip habitat to study area
  observeEvent(input$spatial_clip_habitat, {
    req(euseamap_data())

    if (is.null(spatial_study_area())) {
      showNotification(
        "Please upload a study area boundary first (Tab 0: Study Area)",
        type = "warning",
        duration = 8
      )
      return()
    }

    showNotification("Clipping habitat to study area...", type = "message",
                     duration = NULL, id = "habitat_clip_progress")

    tryCatch({
      clipped <- clip_habitat_to_study_area(
        euseamap_data(),
        spatial_study_area()
      )

      spatial_habitat_clipped(clipped)

      if (exists("log_debug_details")) {
        log_debug_details("Spatial", "Adding habitat to map",
          CRS = sf::st_crs(clipped)$input,
          EPSG = sf::st_crs(clipped)$epsg,
          Rows = nrow(clipped)
        )
      }

      if (is.null(sf::st_crs(clipped)$epsg) || sf::st_crs(clipped)$epsg != 4326) {
        cat("  Transforming habitat to WGS84...\n")
        clipped <- sf::st_transform(clipped, crs = 4326)
      }

      suppressWarnings({
        leafletProxy("spatial_map") %>%
          clearGroup("Habitat") %>%
          addPolygons(
            data = clipped,
            group = "Habitat",
            fillColor = ~colorFactor(
              palette = "Spectral",
              domain = EUNIScomb,
              reverse = TRUE
            )(EUNIScomb),
            fillOpacity = 0.5,
            color = "#666666",
            weight = 0.5,
            popup = ~paste0(
              "<strong>EUNIS:</strong> ", EUNIScomb, "<br>",
              "<strong>Habitat:</strong> ", substr(EUNIScombD, 1, 60), "<br>",
              "<strong>Substrate:</strong> ", Substrate, "<br>",
              "<strong>Biozone:</strong> ", Biozone
            ),
            label = ~paste0(
              EUNIScomb, ": ", substr(EUNIScombD, 1, 40),
              ifelse(nchar(EUNIScombD) > 40, "...", "")
            ),
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "12px",
              direction = "auto"
            )
          ) %>%
          showGroup("Habitat")
      })

      cat("  Habitat added to map\n\n")

      removeNotification("habitat_clip_progress")
      showNotification(
        sprintf("Clipped habitat: %d polygons (visible on map)", nrow(clipped)),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      removeNotification("habitat_clip_progress")
      showNotification(
        paste("Error clipping habitat:", e$message),
        type = "error",
        duration = 10
      )
    })
  })

  # Overlay habitat with grid
  observeEvent(input$spatial_overlay_habitat, {
    req(spatial_habitat_clipped())
    req(spatial_hex_grid())

    showNotification("Overlaying habitat with grid cells... This may take a minute.",
                     type = "message", duration = NULL, id = "habitat_overlay_progress")

    tryCatch({
      grid_with_habitat <- overlay_habitat_with_grid(
        spatial_hex_grid(),
        spatial_habitat_clipped()
      )

      spatial_hex_grid(grid_with_habitat)
      spatial_grid_with_habitat(grid_with_habitat)

      removeNotification("habitat_overlay_progress")
      showNotification(
        sprintf("Grid enriched with habitat data: %d cells", nrow(grid_with_habitat)),
        type = "message",
        duration = 5
      )

      suppressWarnings({
        leafletProxy("spatial_map") %>%
          clearGroup("Grid") %>%
          addPolygons(
            data = grid_with_habitat,
            group = "Grid",
            fillColor = ~colorFactor(
              palette = "Set3",
              domain = dominant_eunis
            )(dominant_eunis),
            fillOpacity = 0.6,
            color = "#555555",
            weight = 1,
            popup = ~paste0(
              "<strong>Cell ID:</strong> ", cell_id, "<br>",
              "<strong>Dominant EUNIS:</strong> ", dominant_eunis, "<br>",
              "<strong>Habitat:</strong> ", substr(dominant_habitat, 1, 50), "<br>",
              "<strong>Substrate:</strong> ", dominant_substrate, "<br>",
              "<strong>Habitat Diversity:</strong> ", habitat_diversity, "<br>",
              "<strong>N Habitats:</strong> ", n_habitats
            ),
            label = ~paste0(
              "Cell ", cell_id, " | ",
              dominant_eunis, " | ",
              "Diversity: ", round(habitat_diversity, 2), " | ",
              n_habitats, " habitat", ifelse(n_habitats > 1, "s", "")
            ),
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "12px",
              direction = "auto"
            )
          ) %>%
          showGroup("Grid")
      })
    }, error = function(e) {
      removeNotification("habitat_overlay_progress")
      showNotification(
        paste("Error overlaying habitat:", e$message),
        type = "error",
        duration = 10
      )
    })
  })

  # 3. Handle species file upload
  observeEvent(input$spatial_species_file, {
    req(input$spatial_species_file)

    tryCatch({
      species_data <- read.csv(input$spatial_species_file$datapath, stringsAsFactors = FALSE)

      required_cols <- c("lon", "lat", "species")
      missing_cols <- setdiff(required_cols, colnames(species_data))

      if (length(missing_cols) > 0) {
        stop(paste("Missing required columns:", paste(missing_cols, collapse = ", "),
                   "\nRequired: lon, lat, species\nOptional: biomass"))
      }

      spatial_species_data(species_data)

      output$spatial_species_preview <- renderDT({
        datatable(
          species_data,
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            dom = 'tip'
          ),
          rownames = FALSE
        )
      })

      showNotification(
        paste("Loaded", nrow(species_data), "species occurrences"),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      showNotification(paste("Error loading species data:", e$message), type = "error")
    })
  })

  # Reactive map update for species layer
  observe({
    req(spatial_species_data())
    species_data <- spatial_species_data()

    unique_species <- unique(species_data$species)
    species_colors <- colorFactor(
      palette = "Set3",
      domain = unique_species
    )

    leafletProxy("spatial_map") %>%
      clearGroup("Species") %>%
      addCircleMarkers(
        data = species_data,
        lng = ~lon,
        lat = ~lat,
        color = ~species_colors(species),
        fillColor = ~species_colors(species),
        fillOpacity = 0.7,
        opacity = 0.9,
        radius = 5,
        weight = 1,
        group = "Species",
        popup = ~paste0(
          "<strong>", species, "</strong><br>",
          "Lon: ", round(lon, 4), "<br>",
          "Lat: ", round(lat, 4),
          if("biomass" %in% colnames(species_data)) paste0("<br>Biomass: ", biomass) else ""
        )
      ) %>%
      showGroup("Species")
  })

  # 3. Extract Local Networks
  observeEvent(input$spatial_extract_networks, {
    req(spatial_hex_grid(), spatial_species_data())

    tryCatch({
      hex_grid <- spatial_hex_grid()
      species_data <- spatial_species_data()

      if (is.null(input$spatial_metaweb) || input$spatial_metaweb == "") {
        showNotification("Please select a metaweb", type = "error")
        return()
      }

      metaweb_file <- METAWEB_PATHS[[input$spatial_metaweb]]

      if (is.null(metaweb_file) || !file.exists(metaweb_file)) {
        showNotification(
          paste("Metaweb file not available:", input$spatial_metaweb),
          type = "error",
          duration = 10
        )
        return()
      }

      metaweb <- readRDS(metaweb_file)
      spatial_metaweb(metaweb)

      output$spatial_extraction_info <- renderPrint({
        cat("Assigning species to hexagons...\n")
      })

      hex_species <- assign_species_to_hexagons(species_data, hex_grid)

      output$spatial_extraction_info <- renderPrint({
        cat("Extracting local food webs...\n")
      })

      local_networks <- extract_local_networks(
        metaweb,
        hex_species,
        hex_grid,
        progress = FALSE
      )

      spatial_local_networks(local_networks)

      n_networks <- length(local_networks)
      n_nonempty <- sum(sapply(local_networks, igraph::vcount) > 0)

      output$spatial_extraction_info <- renderPrint({
        cat("Network extraction complete!\n\n")
        cat("Summary:\n")
        cat("  Total hexagons:", n_networks, "\n")
        cat("  With species:", n_nonempty, "\n")
        cat("  Empty:", n_networks - n_nonempty, "\n\n")
        cat("Metaweb used:", metaweb$metadata$region %||% input$spatial_metaweb, "\n")
      })

      showNotification(
        paste("Extracted", n_nonempty, "local food webs"),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      output$spatial_extraction_info <- renderPrint({
        cat("ERROR extracting networks:\n")
        cat(e$message, "\n")
      })
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # 4. Calculate Spatial Metrics
  observeEvent(input$spatial_calculate_metrics, {
    if (is.null(spatial_hex_grid())) {
      showNotification("Please create a hexagonal grid first (Step 1)", type = "error", duration = 8)
      return()
    }

    if (is.null(spatial_species_data())) {
      showNotification("Please upload or generate species data first (Step 2)", type = "error", duration = 8)
      return()
    }

    if (is.null(spatial_local_networks())) {
      showNotification("Please extract local networks first (Step 3)", type = "error", duration = 8)
      return()
    }

    if (length(spatial_local_networks()) == 0) {
      showNotification("No local networks available. Extract networks first.", type = "error", duration = 8)
      return()
    }

    req(spatial_local_networks(), spatial_hex_grid())

    tryCatch({
      local_networks <- spatial_local_networks()
      hex_grid <- spatial_hex_grid()
      selected_metrics <- input$spatial_metrics

      if (length(selected_metrics) == 0) {
        showNotification("Please select at least one metric", type = "warning")
        return()
      }

      metrics <- calculate_spatial_metrics(
        local_networks,
        hex_grid,
        metrics = selected_metrics,
        progress = FALSE
      )

      spatial_metrics_data(metrics)

      output$spatial_metrics_table <- renderDT({
        metrics_display <- metrics[metrics$S > 0, ]

        datatable(
          metrics_display,
          options = list(
            pageLength = 15,
            scrollX = TRUE,
            dom = 'tip'
          ),
          rownames = FALSE
        ) %>%
          formatRound(columns = selected_metrics, digits = 3)
      })

      summary_stats <- aggregate_spatial_metrics(metrics, metric_cols = selected_metrics)

      showNotification(
        paste("Calculated", length(selected_metrics), "metrics for", nrow(metrics), "hexagons"),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      showNotification(paste("Error calculating metrics:", e$message), type = "error")
    })
  })

  # Reactive map update for metrics layer
  observe({
    req(spatial_hex_grid(), input$spatial_map_metric)

    hex_grid <- spatial_hex_grid()
    selected_metric <- input$spatial_map_metric

    food_web_metrics <- c("S", "L", "C", "LD", "meanTL", "maxTL")
    habitat_metrics <- c("dominant_eunis", "dominant_substrate", "habitat_diversity",
                         "n_habitats", "habitat_area_km2")

    is_food_web <- selected_metric %in% food_web_metrics
    is_habitat <- selected_metric %in% habitat_metrics

    if (is_food_web) {
      req(spatial_metrics_data())
      metrics_data <- spatial_metrics_data()

      hex_grid_with_metrics <- merge(
        hex_grid,
        metrics_data,
        by = "hex_id",
        all.x = TRUE
      )
    } else if (is_habitat) {
      hex_grid_with_metrics <- hex_grid

      if (!selected_metric %in% names(hex_grid_with_metrics)) {
        showNotification(
          "Please overlay habitat data in Tab 2 first",
          type = "warning",
          duration = 5
        )
        leafletProxy("spatial_map") %>%
          clearGroup("Metrics") %>%
          clearControls()
        return()
      }
    } else {
      return()
    }

    hex_grid_with_metrics <- hex_grid_with_metrics[!is.na(hex_grid_with_metrics[[selected_metric]]), ]

    if (nrow(hex_grid_with_metrics) == 0) {
      leafletProxy("spatial_map") %>%
        clearGroup("Metrics") %>%
        clearControls()
      return()
    }

    metric_values <- hex_grid_with_metrics[[selected_metric]]

    is_categorical <- selected_metric %in% c("dominant_eunis", "dominant_substrate")
    is_count <- selected_metric %in% c("S", "L", "n_habitats")

    if (is_categorical) {
      pal <- colorFactor(
        palette = "Set3",
        domain = metric_values
      )
    } else if (is_count) {
      pal <- colorBin(
        palette = "YlOrRd",
        domain = metric_values,
        bins = 5,
        pretty = TRUE
      )
    } else {
      pal <- colorNumeric(
        palette = "Spectral",
        domain = metric_values,
        reverse = TRUE
      )
    }

    metric_labels <- c(
      "S" = "Species Richness",
      "L" = "Number of Links",
      "C" = "Connectance",
      "LD" = "Link Density",
      "meanTL" = "Mean Trophic Level",
      "maxTL" = "Max Trophic Level",
      "dominant_eunis" = "Dominant EUNIS Code",
      "dominant_substrate" = "Dominant Substrate",
      "habitat_diversity" = "Habitat Diversity (Shannon)",
      "n_habitats" = "Number of Habitats",
      "habitat_area_km2" = "Habitat Area (km2)"
    )
    metric_label <- metric_labels[[selected_metric]]

    format_value <- function(val) {
      if (is_categorical) {
        as.character(val)
      } else {
        round(as.numeric(val), 3)
      }
    }

    leafletProxy("spatial_map") %>%
      clearGroup("Metrics") %>%
      clearControls() %>%
      addPolygons(
        data = hex_grid_with_metrics,
        group = "Metrics",
        fillColor = ~pal(get(selected_metric)),
        fillOpacity = 0.7,
        color = "#444444",
        weight = 1,
        popup = ~paste0(
          "<strong>Hex ID: ", hex_id, "</strong><br>",
          metric_label, ": ",
          if (is_categorical) as.character(get(selected_metric)) else round(get(selected_metric), 3)
        ),
        label = ~paste0(
          "Cell ", hex_id, " | ",
          metric_label, ": ",
          if (is_categorical) as.character(get(selected_metric)) else round(get(selected_metric), 3)
        ),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "12px",
          direction = "auto"
        )
      ) %>%
      showGroup("Metrics") %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = metric_values,
        title = metric_label,
        opacity = 0.7,
        layerId = "metrics_legend"
      )
  })

  # 5. Leaflet Map Initialization
  output$spatial_map <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean Basemap") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
      setView(lng = 18, lat = 57, zoom = 6) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Ocean Basemap", "Light"),
        overlayGroups = c("Study Area", "Habitat", "Grid", "Species", "Metrics"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addScaleBar(position = "bottomleft") %>%
      addMiniMap(
        toggleDisplay = TRUE,
        position = "bottomright",
        tiles = providers$Esri.WorldStreetMap
      ) %>%
      addControl(
        html = "<div style='padding: 5px; background: white; border-radius: 3px; font-size: 11px;'>
                <strong>Habitat Layer:</strong> Clip habitat in Tab 2 to see EMODnet data
                </div>",
        position = "topleft"
      )
  })

  # 6. Download Handlers
  output$spatial_download_metrics <- downloadHandler(
    filename = function() {
      paste0("spatial_metrics_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(spatial_metrics_data())
      write.csv(spatial_metrics_data(), file, row.names = FALSE)
    }
  )

  output$spatial_download_rds <- downloadHandler(
    filename = function() {
      paste0("spatial_analysis_", format(Sys.Date(), "%Y%m%d"), ".rds")
    },
    content = function(file) {
      req(spatial_hex_grid(), spatial_species_data(), spatial_local_networks(),
          spatial_metrics_data(), spatial_metaweb())

      spatial_data <- create_spatial_foodweb_data(
        hex_grid = spatial_hex_grid(),
        hex_species = spatial_species_data(),
        metaweb = spatial_metaweb(),
        local_networks = spatial_local_networks(),
        metrics = spatial_metrics_data(),
        metadata = list(
          created = Sys.time(),
          bbox = c(
            xmin = input$spatial_xmin,
            ymin = input$spatial_ymin,
            xmax = input$spatial_xmax,
            ymax = input$spatial_ymax
          ),
          cell_size = input$spatial_cell_size,
          metaweb_source = input$spatial_metaweb
        )
      )

      saveRDS(spatial_data, file)
    }
  )

  # Ensure spatial analysis outputs are not suspended when hidden (load by default)
  outputOptions(output, "spatial_study_area_info", suspendWhenHidden = FALSE)
  outputOptions(output, "spatial_study_area_map", suspendWhenHidden = FALSE)
  outputOptions(output, "spatial_habitat_status", suspendWhenHidden = FALSE)
  outputOptions(output, "spatial_map", suspendWhenHidden = FALSE)
}
