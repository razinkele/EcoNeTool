#' Taxonomic Database API Integration
#'
#' Functions to query FishBase, OBIS, and WoRMS for species classification
#' and trait data. Provides authoritative taxonomic information.
#'
#' @description
#' This module integrates with three major biodiversity databases:
#' - FishBase: Comprehensive fish species database
#' - OBIS: Ocean Biodiversity Information System
#' - WoRMS: World Register of Marine Species
#'
#' These APIs provide:
#' - Taxonomic classification
#' - Functional group assignment
#' - Body mass/size data
#' - Habitat information
#' - Trophic level data

#' Query WoRMS API by Vernacular/Common Name
#'
#' @param common_name Character, common/vernacular name to query
#' @param fuzzy Logical, use fuzzy matching with LIKE (default: TRUE)
#' @return List with taxonomic information or NULL if not found
#'
#' @details
#' Searches WoRMS by vernacular (common) names using the AphiaRecordsByVernacular endpoint.
#' This endpoint is specifically designed for common name lookups.
#'
#' API Endpoint: https://www.marinespecies.org/rest/AphiaRecordsByVernacular/{vernacular}
#' Documentation: https://www.marinespecies.org/aphia.php?p=webservice
#'
#' @export
query_worms_vernacular <- function(common_name, fuzzy = TRUE) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    message("      ✗ httr package not installed")
    return(NULL)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    message("      ✗ jsonlite package not installed")
    return(NULL)
  }

  tryCatch({
    # Try plural/singular variants
    search_names <- c(common_name)

    # If name ends with common plural patterns, add singular form
    if (grepl("ies$", common_name)) {
      singular <- sub("ies$", "y", common_name)
      search_names <- c(search_names, singular)
    } else if (grepl("es$", common_name)) {
      singular <- sub("es$", "", common_name)
      search_names <- c(search_names, singular)
    } else if (grepl("s$", common_name)) {
      singular <- sub("s$", "", common_name)
      search_names <- c(search_names, singular)
    }

    # Try each variant
    for (search_name in search_names) {
      if (search_name != common_name) {
        message(sprintf("      → WoRMS (vernacular): Trying singular form '%s'", search_name))
      } else {
        message(sprintf("      → WoRMS (vernacular): Searching for '%s'", common_name))
      }

      result <- query_worms_vernacular_single(search_name, fuzzy)
      if (!is.null(result)) {
        return(result)
      }
    }

    message(sprintf("      ✗ WoRMS (vernacular): No matches found for '%s' or variants", common_name))
    return(NULL)

  }, error = function(e) {
    message(sprintf("      ✗ WoRMS (vernacular) query error: %s", conditionMessage(e)))
    return(NULL)
  })
}

# Internal function to search single name variant
query_worms_vernacular_single <- function(common_name, fuzzy = TRUE) {
  tryCatch({

    # WoRMS API endpoint for vernacular/common name search
    # Format: /AphiaRecordsByVernacular/{vernacular}?like={true|false}&offset=1
    base_url <- "https://www.marinespecies.org/rest/AphiaRecordsByVernacular"

    # URL encode the common name for the path parameter
    encoded_name <- utils::URLencode(common_name, reserved = TRUE)

    # Build complete URL
    url <- paste0(base_url, "/", encoded_name)

    # Query parameters
    params <- list(
      like = tolower(as.character(fuzzy)),  # Use fuzzy matching (SQL LIKE '%name%')
      offset = 1  # Starting record number
    )

    message(sprintf("      → WoRMS (vernacular): GET %s?like=%s&offset=1", url, params$like))

    # Make API request
    response <- httr::GET(url, query = params, httr::timeout(10))

    message(sprintf("      → WoRMS (vernacular): Response status %d", httr::status_code(response)))

    # Handle different status codes
    if (httr::status_code(response) == 204) {
      message(sprintf("      ✗ WoRMS (vernacular): No content (HTTP 204) - '%s' not found as common name", common_name))
      return(NULL)
    }

    if (httr::status_code(response) != 200) {
      message(sprintf("      ✗ WoRMS (vernacular): HTTP error %d", httr::status_code(response)))
      return(NULL)
    }

    # Parse JSON response
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    result <- jsonlite::fromJSON(content, flatten = TRUE)

    # WoRMS returns a data.frame
    if (is.null(result) || !is.data.frame(result) || nrow(result) == 0) {
      message(sprintf("      ✗ WoRMS (vernacular): '%s' not found (empty result)", common_name))
      return(NULL)
    }

    # Multiple matches possible - use first
    if (nrow(result) > 1) {
      message(sprintf("      ⚠ WoRMS (vernacular): Multiple matches found (%d), using first", nrow(result)))
    }

    # Extract first row
    match <- result[1, ]

    # Return taxonomic information
    worms_data <- list(
      aphia_id = match$AphiaID,
      scientific_name = match$scientificname,
      authority = match$authority,
      status = match$status,
      rank = match$rank,
      phylum = match$phylum,
      class = match$class,
      order = match$order,
      family = match$family,
      genus = match$genus,
      is_marine = match$isMarine,
      is_brackish = match$isBrackish,
      is_freshwater = match$isFreshwater,
      is_terrestrial = match$isTerrestrial,
      is_extinct = match$isExtinct,
      query_type = "vernacular"  # Mark as vernacular query
    )

    message(sprintf("      ✓ WoRMS (vernacular): Found '%s' → %s (AphiaID: %s, Class: %s)",
                    common_name,
                    worms_data$scientific_name,
                    worms_data$aphia_id,
                    ifelse(is.null(worms_data$class), "NA", worms_data$class)))

    return(worms_data)
  }, error = function(e) {
    message(sprintf("      ✗ WoRMS (vernacular) query error: %s", conditionMessage(e)))
    return(NULL)
  })
}

#' Query WoRMS API for Species Classification
#'
#' @param species_name Character, species name to query
#' @param fuzzy Logical, use fuzzy matching (default: TRUE)
#' @param try_vernacular Logical, try vernacular/common name search if scientific name fails (default: TRUE)
#' @return List with taxonomic information or NULL if not found
#'
#' @details
#' WoRMS (World Register of Marine Species) provides authoritative
#' taxonomic information for marine species.
#'
#' Search strategy:
#' 1. Try scientific name search (AphiaRecordsByName)
#' 2. If that fails and try_vernacular=TRUE, try common name search (AphiaRecordsByVernacular)
#'
#' API Documentation: https://www.marinespecies.org/rest/
#' Vernacular docs: https://www.marinespecies.org/aphia.php?p=webservice
#'
#' @export
query_worms <- function(species_name, fuzzy = TRUE, try_vernacular = TRUE) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    message("      ✗ httr package not installed")
    return(NULL)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    message("      ✗ jsonlite package not installed")
    return(NULL)
  }

  tryCatch({
    message(sprintf("      → WoRMS: Searching for '%s'", species_name))

    # STRATEGY 1: Try scientific name search
    # WoRMS API endpoint for single species name matching
    # Format: /AphiaRecordsByName/{scientificname}?like={true|false}&marine_only={true|false}
    base_url <- "https://www.marinespecies.org/rest/AphiaRecordsByName"

    # URL encode the species name for the path parameter
    encoded_name <- utils::URLencode(species_name, reserved = TRUE)

    # Build complete URL with species name as path parameter
    url <- paste0(base_url, "/", encoded_name)

    # Query parameters
    params <- list(
      like = tolower(as.character(fuzzy)),  # Use fuzzy matching
      marine_only = "false"  # Include brackish/freshwater species
    )

    message(sprintf("      → WoRMS: GET %s?like=%s&marine_only=false", url, params$like))

    # Make API request
    response <- httr::GET(url, query = params, httr::timeout(10))

    message(sprintf("      → WoRMS: Response status %d", httr::status_code(response)))

    # Check if scientific name search succeeded
    if (httr::status_code(response) == 200) {
      # Parse JSON response
      content <- httr::content(response, as = "text", encoding = "UTF-8")
      result <- jsonlite::fromJSON(content, flatten = TRUE)

      # WoRMS returns a data.frame, not a list
      if (!is.null(result) && is.data.frame(result) && nrow(result) > 0) {
        # Extract first row (first match)
        match <- result[1, ]

        # Check if match is valid
        if (!is.null(match) && !all(is.na(match))) {
          # Return taxonomic information
          worms_data <- list(
            aphia_id = match$AphiaID,
            scientific_name = match$scientificname,
            authority = match$authority,
            status = match$status,
            rank = match$rank,
            phylum = match$phylum,
            class = match$class,
            order = match$order,
            family = match$family,
            genus = match$genus,
            is_marine = match$isMarine,
            is_brackish = match$isBrackish,
            is_freshwater = match$isFreshwater,
            is_terrestrial = match$isTerrestrial,
            is_extinct = match$isExtinct,
            query_type = "scientific"  # Mark as scientific query
          )

          message(sprintf("      ✓ WoRMS: Found (AphiaID: %s, Class: %s, Family: %s)",
                          worms_data$aphia_id,
                          ifelse(is.null(worms_data$class), "NA", worms_data$class),
                          ifelse(is.null(worms_data$family), "NA", worms_data$family)))

          return(worms_data)
        }
      }
    }

    # STRATEGY 2: Try vernacular/common name search if scientific name failed
    if (try_vernacular) {
      message(sprintf("      → WoRMS: Scientific name not found, trying vernacular search..."))
      return(query_worms_vernacular(species_name, fuzzy = fuzzy))
    } else {
      message(sprintf("      ✗ WoRMS: '%s' not found (scientific name search only)", species_name))
      return(NULL)
    }

  }, error = function(e) {
    message(sprintf("      ✗ WoRMS query error: %s", conditionMessage(e)))
    return(NULL)
  })
}

#' Query FishBase API for Fish Species Data
#'
#' @param species_name Character, species name to query
#' @param geographic_region Character, study area region for filtering multiple matches (default: NULL)
#' @param progress_callback Function, callback for progress updates (default: NULL)
#' @return List with fish trait data or NULL if not found
#'
#' @details
#' FishBase provides comprehensive data on fish species including:
#' - Body size (length, weight)
#' - Trophic level
#' - Habitat preferences
#' - Life history traits
#'
#' Geographic filtering: When multiple species match the same common name,
#' prefer species found in the specified geographic region.
#'
#' API Documentation: https://fishbase.ropensci.org/
#'
#' @export
query_fishbase <- function(species_name, geographic_region = NULL, progress_callback = NULL) {
  # Helper to update progress
  update_progress <- function(msg) {
    message(msg)
    if (!is.null(progress_callback)) {
      tryCatch(progress_callback(msg), error = function(e) {})
    }
  }

  if (!requireNamespace("rfishbase", quietly = TRUE)) {
    update_progress("      ✗ rfishbase package not installed")
    return(NULL)
  }

  tryCatch({
    # Singularize common plural forms (FishBase uses singular common names)
    singular_name <- species_name

    # Handle common fish plural patterns
    if (grepl("gobies$", species_name, ignore.case = TRUE)) {
      singular_name <- sub("gobies$", "goby", species_name, ignore.case = TRUE)
    } else if (grepl("([^aeiouy])ies$", species_name, ignore.case = TRUE)) {
      # herries → herry, guppies → guppy
      singular_name <- sub("([^aeiouy])ies$", "\\1y", species_name, ignore.case = TRUE)
    } else if (grepl("(ss|sh|ch|x|z)es$", species_name, ignore.case = TRUE)) {
      # basses → bass, fishes → fish
      singular_name <- sub("(ss|sh|ch|x|z)es$", "\\1", species_name, ignore.case = TRUE)
    } else if (grepl("([aeiou]y)s$", species_name, ignore.case = TRUE)) {
      # rays → ray (keep the 'y')
      singular_name <- sub("s$", "", species_name)
    } else if (grepl("s$", species_name, ignore.case = TRUE) &&
               !grepl("(us|is|ss)$", species_name, ignore.case = TRUE)) {
      # eels → eel, cods → cod, but NOT (nautilus, analysis, bass)
      singular_name <- sub("s$", "", species_name)
    }

    if (singular_name != species_name) {
      update_progress(sprintf("      → FishBase: Singularized '%s' → '%s'", species_name, singular_name))
    }

    update_progress(sprintf("      → FishBase: Searching for '%s'...", singular_name))

    validated <- NULL

    # STRATEGY 1: Try as COMMON NAME first (most ECOPATH models use common names)
    update_progress("      → FishBase: Querying common name database...")
    common_result <- tryCatch({
      rfishbase::common_to_sci(singular_name, Language = "English")
    }, error = function(e) {
      update_progress(sprintf("      ⚠ FishBase: common_to_sci error: %s", conditionMessage(e)))
      NULL
    })

    if (!is.null(common_result) && nrow(common_result) > 0) {
      # If multiple matches, try geographic filtering
      if (nrow(common_result) > 1) {
        update_progress(sprintf("      → FishBase: Found %d matches", nrow(common_result)))

        if (!is.null(geographic_region) && geographic_region != "") {
          update_progress(sprintf("      → FishBase: Filtering by region '%s'...", geographic_region))

          # Try to filter by geographic distribution
          geographic_match <- NULL
          region_lower <- tolower(geographic_region)

          for (i in 1:nrow(common_result)) {
            species_to_check <- common_result$Species[i]

            # Query distribution data
            dist_data <- tryCatch({
              rfishbase::distribution(species_to_check)
            }, error = function(e) NULL)

            if (!is.null(dist_data) && nrow(dist_data) > 0) {
              # Check if species found in the specified region
              # FishBase distribution has: C_Code (country), FAOAreas, Ecosystem
              countries <- if ("C_Code" %in% names(dist_data)) {
                paste(tolower(dist_data$C_Code), collapse = " ")
              } else ""

              fao_areas <- if ("FAOAreas" %in% names(dist_data)) {
                paste(tolower(dist_data$FAOAreas), collapse = " ")
              } else ""

              ecosystems <- if ("Ecosystem" %in% names(dist_data)) {
                paste(tolower(dist_data$Ecosystem), collapse = " ")
              } else ""

              # Check if region matches any of the distribution data
              # Common region keywords: Baltic, Mediterranean, Atlantic, Pacific, North Sea, etc.
              if (grepl(region_lower, countries) ||
                  grepl(region_lower, fao_areas) ||
                  grepl(region_lower, ecosystems)) {
                geographic_match <- i
                update_progress(sprintf("      ✓ FishBase: Geographic match found - '%s' occurs in %s",
                                        species_to_check, geographic_region))
                break
              }
            }
          }

          if (!is.null(geographic_match)) {
            validated <- common_result$Species[geographic_match]
            update_progress(sprintf("      → FishBase: Using geographically-filtered match: '%s' (SpecCode: %s)",
                                    validated, common_result$SpecCode[geographic_match]))
          } else {
            validated <- common_result$Species[1]
            update_progress(sprintf("      ⚠ FishBase: No geographic match found, using first: '%s' (SpecCode: %s)",
                                    validated, common_result$SpecCode[1]))
          }
        } else {
          # No geographic region specified - use first match
          validated <- common_result$Species[1]
          update_progress(sprintf("      ⚠ FishBase: Multiple matches (%d), using first: '%s' (SpecCode: %s)",
                                  nrow(common_result), validated, common_result$SpecCode[1]))
        }
      } else {
        # Single match
        validated <- common_result$Species[1]
        update_progress(sprintf("      → FishBase: Common name '%s' → '%s' (SpecCode: %s)",
                                singular_name, validated, common_result$SpecCode[1]))
      }
    }

    # STRATEGY 2: If common name failed, try as SCIENTIFIC NAME
    if (is.null(validated)) {
      update_progress("      → FishBase: Trying scientific name validation...")
      validated <- rfishbase::validate_names(singular_name)

      if (!is.null(validated) && length(validated) > 0 && !is.na(validated[1])) {
        validated <- validated[1]
        update_progress(sprintf("      → FishBase: Validated as scientific name '%s'", validated))
      } else {
        update_progress(sprintf("      ✗ FishBase: '%s' not found", singular_name))
        return(NULL)
      }
    }

    # Use validated name for queries
    valid_name <- validated

    # Get taxonomic hierarchy from load_taxa()
    update_progress("      → FishBase: Loading taxonomic data...")
    taxa_info <- rfishbase::load_taxa()
    species_taxa <- taxa_info[taxa_info$Species == valid_name, ]

    if (is.null(species_taxa) || nrow(species_taxa) == 0) {
      update_progress(sprintf("      ✗ FishBase: No taxonomy data for '%s'", valid_name))
      return(NULL)
    }

    # Get species info
    update_progress("      → FishBase: Loading species data...")
    species_info <- rfishbase::species(valid_name)

    if (is.null(species_info) || nrow(species_info) == 0) {
      update_progress(sprintf("      ✗ FishBase: No species data for '%s'", valid_name))
      return(NULL)
    }

    update_progress(sprintf("      → FishBase: Found '%s' (SpecCode: %s)", valid_name, species_info$SpecCode[1]))

    # Get ecology data (trophic level, habitat)
    update_progress("      → FishBase: Loading ecology & habitat data...")
    ecology_info <- tryCatch({
      rfishbase::ecology(valid_name)
    }, error = function(e) {
      update_progress(sprintf("      ⚠ FishBase: Ecology data unavailable"))
      NULL
    })

    # Extract trophic level safely
    trophic_level <- NA
    if (!is.null(ecology_info) && is.data.frame(ecology_info) && nrow(ecology_info) > 0) {
      if ("FoodTroph" %in% names(ecology_info)) {
        ft_val <- ecology_info$FoodTroph[1]
        if (length(ft_val) > 0 && !is.na(ft_val)) {
          trophic_level <- ft_val
        }
      }
    }

    # Extract depth range from species info
    min_depth <- NA
    max_depth <- NA
    if ("DepthRangeShallow" %in% names(species_info)) {
      depth_val <- species_info$DepthRangeShallow[1]
      if (length(depth_val) > 0 && !is.na(depth_val)) {
        min_depth <- depth_val
      }
    }
    if ("DepthRangeDeep" %in% names(species_info)) {
      depth_val <- species_info$DepthRangeDeep[1]
      if (length(depth_val) > 0 && !is.na(depth_val)) {
        max_depth <- depth_val
      }
    }

    # Extract habitat classification from ecology data
    habitat <- NA
    if (!is.null(ecology_info) && is.data.frame(ecology_info) && nrow(ecology_info) > 0) {
      # FishBase has habitat flags: Pelagic, Demersal, Benthic
      is_pelagic <- if ("Pelagic" %in% names(ecology_info)) ecology_info$Pelagic[1] == 1 else FALSE
      is_demersal <- if ("Demersal" %in% names(ecology_info)) ecology_info$Demersal[1] == 1 else FALSE
      is_benthic <- if ("Benthic" %in% names(ecology_info)) ecology_info$Benthic[1] == 1 else FALSE

      # Classify based on flags (prefer specific over general)
      if (is_demersal) {
        habitat <- "Demersal"
      } else if (is_benthic) {
        habitat <- "Benthic"
      } else if (is_pelagic) {
        habitat <- "Pelagic"
      } else if (!is.na(min_depth) || !is.na(max_depth)) {
        # Fallback: classify from depth if no habitat flags
        habitat <- classify_habitat_from_depth(min_depth, max_depth)
      }
    } else if (!is.na(min_depth) || !is.na(max_depth)) {
      # No ecology data but have depth - classify from depth
      habitat <- classify_habitat_from_depth(min_depth, max_depth)
    }

    # Extract weight information
    max_weight_g <- NA
    avg_weight_g <- NA

    # Get maximum weight
    if ("Weight" %in% names(species_info)) {
      weight_val <- species_info$Weight[1]
      if (length(weight_val) > 0 && !is.na(weight_val)) {
        max_weight_g <- weight_val
      }
    }

    # Try to get average/common weight from popchar table
    avg_weight_from_popchar <- tryCatch({
      popchar_data <- rfishbase::popchar(valid_name)
      if (!is.null(popchar_data) && nrow(popchar_data) > 0) {
        # Look for weight fields in popchar (population characteristics)
        if ("Weight" %in% names(popchar_data)) {
          weights <- popchar_data$Weight[!is.na(popchar_data$Weight)]
          if (length(weights) > 0) {
            mean(weights)  # Average of reported weights
          } else {
            NA
          }
        } else {
          NA
        }
      } else {
        NA
      }
    }, error = function(e) {
      NA
    })

    # Calculate average weight estimate
    update_progress("      → FishBase: Calculating body mass...")
    if (!is.na(avg_weight_from_popchar)) {
      # Use population data average
      avg_weight_g <- avg_weight_from_popchar
      update_progress(sprintf("      ✓ FishBase: Body mass from population data: %.2f g", avg_weight_g))
    } else if (!is.na(max_weight_g)) {
      # Estimate average as ~30% of maximum weight (typical for fish)
      # This is a rough heuristic: adult fish typically weigh 20-40% of max recorded
      avg_weight_g <- max_weight_g * 0.3
      update_progress(sprintf("      ✓ FishBase: Body mass estimated (30%% of max): %.2f g", avg_weight_g))
    } else {
      # Try to estimate from common length using length-weight relationship
      if ("CommonLength" %in% names(species_info)) {
        common_length <- species_info$CommonLength[1]
        if (!is.na(common_length) && common_length > 0) {
          # Get length-weight parameters if available
          lw_params <- tryCatch({
            rfishbase::length_weight(valid_name)
          }, error = function(e) NULL)

          if (!is.null(lw_params) && nrow(lw_params) > 0) {
            # Use first available length-weight relationship
            # W = a * L^b (where W is weight in g, L is length in cm)
            a <- lw_params$a[1]
            b <- lw_params$b[1]

            if (!is.na(a) && !is.na(b) && a > 0 && b > 0) {
              avg_weight_g <- a * (common_length ^ b)
              update_progress(sprintf("      ✓ FishBase: Body mass from length-weight formula: %.2f g", avg_weight_g))
            }
          } else {
            # Fallback: use generic fish length-weight relationship
            # W ≈ 0.01 * L^3 (rough approximation for fusiform fish)
            avg_weight_g <- 0.01 * (common_length ^ 3)
            update_progress(sprintf("      ✓ FishBase: Body mass from length (generic): %.2f g", avg_weight_g))
          }
        }
      }
    }

    # Return combined information
    result <- list(
      species_code = species_info$SpecCode[1],
      scientific_name = species_info$Species[1],
      common_name = species_info$FBname[1],
      family = species_taxa$Family[1],
      order = species_taxa$Order[1],
      class = species_taxa$Class[1],
      max_length_cm = species_info$Length[1],
      common_length_cm = species_info$CommonLength[1],
      max_weight_g = max_weight_g,
      avg_weight_g = avg_weight_g,  # NEW: Average individual weight
      trophic_level = trophic_level,
      habitat = habitat,
      min_depth_m = min_depth,
      max_depth_m = max_depth,
      functional_group = "Fish"
    )

    update_progress(sprintf("      ✓ FishBase: SUCCESS - %s (%s, %s)",
                            result$scientific_name,
                            result$family,
                            ifelse(is.na(avg_weight_g), "no weight data", sprintf("%.1fg", avg_weight_g))))

    return(result)
  }, error = function(e) {
    update_progress(sprintf("      ✗ FishBase query error: %s", conditionMessage(e)))
    return(NULL)
  })
}

#' Query OBIS for Species Ecological Data
#'
#' @param species_name Character, species name (for logging)
#' @param aphia_id Numeric, WoRMS AphiaID for the species (required)
#' @return List with ecological data or NULL if not found
#'
#' @details
#' OBIS (Ocean Biodiversity Information System) provides ecological context:
#' - Depth ranges (mindepth, maxdepth) → Habitat classification
#' - Occurrence record counts → Data quality indicator
#' - Geographic distribution → Species range
#'
#' **IMPORTANT:** OBIS requires WoRMS AphiaID (not scientific name).
#' This function should be called AFTER query_worms() succeeds.
#'
#' Habitat Classification:
#' - Pelagic: depth range 0-200m (surface/epipelagic)
#' - Demersal: depth range overlaps 50-500m (near-bottom)
#' - Benthic: min depth > 50m or max depth < 50m (bottom-dwelling)
#' - Deep-sea: min depth > 200m (deep waters)
#'
#' API Documentation: https://api.obis.org/
#' Endpoint: GET /taxon/{aphiaid}
#'
#' @export
query_obis <- function(species_name, aphia_id = NULL) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    message("      ✗ httr package not installed")
    return(NULL)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    message("      ✗ jsonlite package not installed")
    return(NULL)
  }

  # Check if AphiaID provided
  if (is.null(aphia_id) || is.na(aphia_id)) {
    message(sprintf("      ⊗ OBIS: Skipped (no WoRMS AphiaID available for '%s')", species_name))
    return(NULL)
  }

  tryCatch({
    message(sprintf("      → OBIS: Searching for AphiaID %s ('%s')", aphia_id, species_name))

    # OBIS API endpoint for taxon by AphiaID
    base_url <- paste0("https://api.obis.org/taxon/", aphia_id)

    message(sprintf("      → OBIS: GET %s", base_url))

    # Make API request
    response <- httr::GET(base_url, httr::timeout(10))

    message(sprintf("      → OBIS: Response status %d", httr::status_code(response)))

    if (httr::status_code(response) != 200) {
      message(sprintf("      ✗ OBIS: HTTP error %d", httr::status_code(response)))
      return(NULL)
    }

    # Parse JSON response
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    result <- jsonlite::fromJSON(content, flatten = TRUE)

    # Check if results exist
    if (is.null(result) || length(result$results) == 0) {
      message(sprintf("      ✗ OBIS: No data for AphiaID %s", aphia_id))
      return(NULL)
    }

    # Extract first result
    match <- result$results[[1]]

    # Extract depth information
    min_depth <- if (!is.null(match$mindepth) && !is.na(match$mindepth)) match$mindepth else NA
    max_depth <- if (!is.null(match$maxdepth) && !is.na(match$maxdepth)) match$maxdepth else NA

    # Classify habitat based on depth
    habitat <- classify_habitat_from_depth(min_depth, max_depth)

    # Return ecological information
    obis_data <- list(
      aphia_id = aphia_id,
      records = if (!is.null(match$records)) match$records else 0,
      min_depth_m = min_depth,
      max_depth_m = max_depth,
      habitat = habitat,
      source = "OBIS"
    )

    message(sprintf("      ✓ OBIS: Found (Records: %d, Depth: %s-%sm, Habitat: %s)",
                    obis_data$records,
                    ifelse(is.na(min_depth), "NA", round(min_depth, 0)),
                    ifelse(is.na(max_depth), "NA", round(max_depth, 0)),
                    habitat))

    return(obis_data)
  }, error = function(e) {
    message(sprintf("      ✗ OBIS query error: %s", conditionMessage(e)))
    return(NULL)
  })
}

#' Classify Habitat from Depth Range
#'
#' @param min_depth Numeric, minimum depth in meters
#' @param max_depth Numeric, maximum depth in meters
#' @return Character, habitat classification
#'
#' @details
#' Habitat classification for marine food web modeling:
#' - Pelagic: Surface to mid-water (0-200m typical)
#' - Demersal: Near-bottom swimmers (50-500m range)
#' - Benthic: Bottom-dwelling (>50m min or shallow max)
#' - Deep-sea: Deep waters (>200m minimum)
#' - Unknown: No depth data available
#'
#' @export
classify_habitat_from_depth <- function(min_depth, max_depth) {
  # No depth data
  if (is.na(min_depth) && is.na(max_depth)) {
    return("Unknown")
  }

  # Use available depth values
  min_d <- ifelse(is.na(min_depth), 0, min_depth)
  max_d <- ifelse(is.na(max_depth), min_d, max_depth)

  # Classification logic for food web ecology
  if (max_d <= 200) {
    # Shallow to mid-depth
    if (min_d < 50) {
      return("Pelagic")  # Surface/epipelagic zone
    } else {
      return("Benthic")  # Shallow benthic
    }
  } else if (min_d <= 200 && max_d > 200) {
    # Spans multiple zones
    if (min_d < 50) {
      return("Pelagic-Demersal")  # Wide vertical range
    } else {
      return("Demersal")  # Near-bottom, mid-depth
    }
  } else {
    # Deep waters
    if (min_d > 500) {
      return("Deep-sea Benthic")  # Abyssal/bathyal
    } else {
      return("Deep-sea Demersal")  # Deep mesopelagic
    }
  }
}

#' Split Combined Species Names
#'
#' @param species_name Character, raw species name
#' @return Character vector, individual species names
#'
#' @details
#' Detects and splits combined species names separated by:
#' - " and " (case insensitive)
#' - " & "
#' - " / "
#' - ", "
#'
#' Examples:
#' - "Lesser sand-eel and Greater sand-eel" → ["Lesser sand-eel", "Greater sand-eel"]
#' - "Common and Sand gobies" → ["Common gobies", "Sand gobies"]
#' - "Sprat / Herring" → ["Sprat", "Herring"]
#'
#' Special handling for partial names:
#' - "Common and Sand gobies" → extracts "gobies" and applies to both parts
#' - "Lesser and Greater sand-eel" → extracts "sand-eel" and applies to both parts
#'
#' @export
split_combined_species_names <- function(species_name) {
  # Trim whitespace first
  clean_name <- trimws(species_name)

  # Check for combination patterns
  has_and <- grepl("\\s+and\\s+", clean_name, ignore.case = TRUE)
  has_ampersand <- grepl("\\s*&\\s*", clean_name)
  has_slash <- grepl("\\s*/\\s*", clean_name)
  has_comma <- grepl(",\\s+", clean_name)

  # If no combination pattern found, return as single-element vector
  if (!has_and && !has_ampersand && !has_slash && !has_comma) {
    return(clean_name)
  }

  # Determine separator and split
  if (has_and) {
    # Case-insensitive split for "and"
    parts <- strsplit(clean_name, "\\s+[Aa][Nn][Dd]\\s+")[[1]]
  } else if (has_ampersand) {
    parts <- strsplit(clean_name, "\\s*&\\s*")[[1]]
  } else if (has_slash) {
    parts <- strsplit(clean_name, "\\s*/\\s*")[[1]]
  } else {
    parts <- strsplit(clean_name, ",\\s+")[[1]]
  }

  parts <- trimws(parts)

  # Special handling: Check if last word is shared suffix
  # E.g., "Common and Sand gobies" → suffix is "gobies"
  # E.g., "Lesser and Greater sand-eel" → suffix is "sand-eel"

  # Check if first part is missing a noun (single word or short)
  first_words <- strsplit(parts[1], "\\s+")[[1]]
  last_words <- strsplit(parts[length(parts)], "\\s+")[[1]]

  # If first part has only 1 word and last part has 2+ words, extract suffix
  if (length(first_words) == 1 && length(last_words) >= 2) {
    # Extract suffix from last part (all words except first)
    suffix <- paste(last_words[-1], collapse = " ")

    # Apply suffix to all parts except the last
    for (i in 1:(length(parts) - 1)) {
      part_words <- strsplit(parts[i], "\\s+")[[1]]
      if (length(part_words) == 1) {
        # Single word - add suffix
        parts[i] <- paste(parts[i], suffix)
      }
    }
  }

  return(parts)
}

#' Clean Species Name for Taxonomic Query
#'
#' @param species_name Character, raw species name
#' @return Character, cleaned species name
#'
#' @details
#' Removes life stage information and cleans formatting:
#' - Prefixes: "adult", "juvenile", "young", "old", "immature", "larvae", "larval"
#' - Parenthetical info: "(juvenile)", "(juvenile/adult)", "(larvae)"
#' - Extra whitespace and special characters
#'
#' Examples:
#' - "adult sprat" → "sprat"
#' - "juvenile cod" → "cod"
#' - "Three-spined stickleback (juvenile/adult)" → "Three-spined stickleback"
#' - "Detritus" → "Detritus" (unchanged)
#'
#' @export
clean_species_name <- function(species_name) {
  # Trim whitespace first
  clean_name <- trimws(species_name)

  # Collapse multiple spaces to single space
  clean_name <- gsub("\\s+", " ", clean_name)

  # Remove life stage prefixes at start (case insensitive)
  clean_name <- gsub("^(adult|juvenile|young|old|immature|larvae|larval)\\s+", "",
                     clean_name, ignore.case = TRUE)

  # Remove parenthetical life stage info (e.g., "(juvenile/adult)", "(larvae)")
  clean_name <- gsub("\\s*\\((juvenile|adult|young|old|immature|larvae|larval|juv|ad)[/\\s]*(juvenile|adult|young|old|immature|larvae|larval|juv|ad)*\\)", "",
                     clean_name, ignore.case = TRUE)

  # Remove other common parenthetical notes
  clean_name <- gsub("\\s*\\(.*?\\)\\s*$", "", clean_name)

  # Final trim
  clean_name <- trimws(clean_name)

  return(clean_name)
}

#' Classify Species Using Taxonomic APIs
#'
#' @param species_name Character, species name to classify
#' @param functional_group_hint Character, hint for functional group (default: NA)
#' @param use_cache Logical, use cached results (default: TRUE)
#' @param cache_dir Character, directory for cache files
#'
#' @return List with functional group and trait data
#'
#' @details
#' This function queries multiple taxonomic databases in priority order:
#' 1. FishBase (for fish species) - SKIPPED for non-fish groups
#' 2. WoRMS (for marine species)
#' 3. OBIS (for occurrence data)
#'
#' Species names are cleaned before querying (e.g., "adult sprat" → "sprat").
#' FishBase is skipped for known non-fish groups (Detritus, Phytoplankton, etc.).
#'
#' Results are cached to avoid repeated API calls.
#'
#' @export
classify_species_api <- function(species_name, functional_group_hint = NA, geographic_region = NULL, progress_callback = NULL, use_cache = TRUE, cache_dir = "cache/taxonomy") {
  # Create cache directory if needed
  if (use_cache && !dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  message(sprintf("    ┌─ classify_species_api('%s')", species_name))
  if (!is.na(functional_group_hint)) {
    message(sprintf("    │  Pattern hint: %s", functional_group_hint))
  }
  if (!is.null(geographic_region) && geographic_region != "") {
    message(sprintf("    │  Geographic region: %s", geographic_region))
  }

  # STEP 1: Clean species name FIRST (remove life stage prefixes, parentheticals)
  clean_name <- clean_species_name(species_name)
  if (clean_name != species_name) {
    message(sprintf("    │  [cleaned] '%s' → '%s'", species_name, clean_name))
  }

  # STEP 2: Check for combined species names (after cleaning)
  split_names <- split_combined_species_names(clean_name)
  if (length(split_names) > 1) {
    message(sprintf("    │  [combined] Detected %d species names:", length(split_names)))
    for (i in 1:length(split_names)) {
      message(sprintf("    │    %d. '%s'", i, split_names[i]))
    }
    message(sprintf("    │  → Querying first species only: '%s'", split_names[1]))
    message(sprintf("    │  ⚠ Note: Combined species should ideally be split in source data"))

    # Use first species for classification
    clean_name <- split_names[1]
  }

  # Check cache first (use cleaned name for cache key)
  cache_file <- file.path(cache_dir, paste0(gsub("[^a-zA-Z0-9]", "_", clean_name), ".rds"))
  if (use_cache && file.exists(cache_file)) {
    cached <- readRDS(cache_file)
    # Check if cache is less than 30 days old
    if (difftime(Sys.time(), cached$timestamp, units = "days") < 30) {
      age_days <- round(difftime(Sys.time(), cached$timestamp, units = "days"), 1)
      message(sprintf("    │  ✓ CACHE HIT: '%s' (age: %s days, source: %s)",
                      clean_name, age_days, cached$data$source))
      message(sprintf("    └─ Result: %s (from cache)", cached$data$functional_group))
      return(cached$data)
    } else {
      message(sprintf("    │  ⚠ Cache expired (age: %s days), re-querying",
                      round(difftime(Sys.time(), cached$timestamp, units = "days"), 1)))
    }
  }

  result <- list(
    species_name = species_name,
    functional_group = NA,
    body_mass_g = NA,
    trophic_level = NA,
    habitat = NA,
    min_depth_m = NA,
    max_depth_m = NA,
    source = NA,
    confidence = "low"
  )

  # Determine if we should skip FishBase based on functional group hint
  non_fish_groups <- c("Detritus", "Phytoplankton", "Zooplankton", "Benthos",
                       "Macroalgae", "Bacteria", "Microbes", "Sediment", "Birds", "Mammals")
  skip_fishbase <- !is.na(functional_group_hint) && functional_group_hint %in% non_fish_groups

  if (skip_fishbase) {
    message(sprintf("    │  ⊗ Skipping FishBase (hint: %s = non-fish)", functional_group_hint))
  }

  # Try FishBase first (most detailed fish data) - only for potential fish
  if (!skip_fishbase) {
    fishbase_result <- query_fishbase(clean_name, geographic_region = geographic_region, progress_callback = progress_callback)
    if (!is.null(fishbase_result)) {
      result$functional_group <- "Fish"
      # Use average weight if available, otherwise fall back to max weight
      result$body_mass_g <- if (!is.na(fishbase_result$avg_weight_g)) {
        fishbase_result$avg_weight_g
      } else {
        fishbase_result$max_weight_g
      }
      result$trophic_level <- fishbase_result$trophic_level
      result$habitat <- fishbase_result$habitat
      result$min_depth_m <- fishbase_result$min_depth_m
      result$max_depth_m <- fishbase_result$max_depth_m
      result$source <- "FishBase"
      result$confidence <- "high"
      result$taxonomy <- fishbase_result

      # Cache result
      if (use_cache) {
        saveRDS(list(data = result, timestamp = Sys.time()), cache_file)
        message(sprintf("      → Cached result for '%s'", clean_name))
      }

      message(sprintf("    └─ ✓ SUCCESS: FishBase → %s (habitat: %s, confidence: high)",
                      result$functional_group,
                      ifelse(is.na(result$habitat), "NA", result$habitat)))
      return(result)
    }
  }

  # Try WoRMS (comprehensive marine taxonomy)
  worms_result <- query_worms(clean_name)
  if (!is.null(worms_result)) {
    # Classify based on WoRMS taxonomic class
    worms_classification <- classify_by_taxonomy(worms_result)

    # CRITICAL: Sanity check against common name to catch mismatches
    # WoRMS may return wrong species if name is ambiguous
    name_lower <- tolower(species_name)

    # Override if common name strongly indicates different group
    if (grepl("\\bcod\\b|\\bherring\\b|\\bsprat\\b|\\bflounder\\b|\\bsalmon\\b|\\bperch\\b|\\bpike\\b", name_lower) &&
        worms_classification != "Fish") {
      message(sprintf("      ⚠ OVERRIDE: WoRMS said '%s' but name contains fish term → forcing Fish", worms_classification))
      worms_classification <- "Fish"
      result$confidence <- "low"  # Mark as low confidence due to override
    }

    if (grepl("\\bworm\\b|\\bpolychaete\\b", name_lower) && worms_classification == "Fish") {
      message(sprintf("      ⚠ OVERRIDE: WoRMS said 'Fish' but name contains 'worm' → forcing Benthos"))
      worms_classification <- "Benthos"
      result$confidence <- "low"
    }

    if (grepl("\\bmussel\\b|\\bclam\\b|\\bcrab\\b|\\bshrimp\\b", name_lower) && worms_classification == "Fish") {
      message(sprintf("      ⚠ OVERRIDE: WoRMS said 'Fish' but name contains benthos term → forcing Benthos"))
      worms_classification <- "Benthos"
      result$confidence <- "low"
    }

    if (grepl("\\bbird\\b|\\bcormorant\\b|\\bduck\\b|\\bgull\\b", name_lower) && worms_classification != "Birds") {
      message(sprintf("      ⚠ OVERRIDE: WoRMS said '%s' but name contains bird term → forcing Birds", worms_classification))
      worms_classification <- "Birds"
      result$confidence <- "low"
    }

    result$functional_group <- worms_classification
    result$source <- "WoRMS"
    if (is.null(result$confidence)) result$confidence <- "medium"
    result$taxonomy <- worms_result

    # Note: OBIS doesn't provide aggregated depth/habitat data
    # Only FishBase has depth ranges and habitat classifications

    # Cache result
    if (use_cache) {
      saveRDS(list(data = result, timestamp = Sys.time()), cache_file)
      message(sprintf("      → Cached result for '%s'", clean_name))
    }

    message(sprintf("    └─ ✓ SUCCESS: WoRMS → %s (class: %s, confidence: %s)",
                    result$functional_group,
                    ifelse(is.null(worms_result$class), "NA", worms_result$class),
                    result$confidence))
    return(result)
  }

  # OBIS standalone query not attempted (requires WoRMS AphiaID)
  # If WoRMS failed, OBIS will also fail since it needs AphiaID

  # No API results - return NA
  message(sprintf("    └─ ✗ NOT FOUND: No database matches for '%s' (will use pattern matching)", clean_name))
  result$confidence <- "none"
  return(result)
}

#' Classify Species by Taxonomic Class
#'
#' @param taxonomy List with taxonomic information
#' @return Character, functional group
#'
#' @details
#' Maps taxonomic class to functional groups:
#' - Actinopterygii, Chondrichthyes, Myxini → Fish
#' - Aves → Birds
#' - Mammalia → Mammals
#' - Malacostraca, Polychaeta, Bivalvia, Gastropoda → Benthos
#' - Copepoda, Branchiopoda → Zooplankton
#' - Phaeophyceae, Chlorophyta → Phytoplankton
#'
classify_by_taxonomy <- function(taxonomy) {
  if (is.null(taxonomy) || is.null(taxonomy$class)) {
    # Show more taxonomy info to understand what WoRMS returned
    if (!is.null(taxonomy)) {
      message(sprintf("      → classify_by_taxonomy: No class info"))
      message(sprintf("         Phylum: %s, Order: %s, Family: %s",
                      ifelse(is.null(taxonomy$phylum), "NA", taxonomy$phylum),
                      ifelse(is.null(taxonomy$order), "NA", taxonomy$order),
                      ifelse(is.null(taxonomy$family), "NA", taxonomy$family)))
    }
    message(sprintf("      → classify_by_taxonomy: Defaulting to Fish (no class)"))
    return("Fish")  # Default
  }

  class_lower <- tolower(taxonomy$class)
  message(sprintf("      → classify_by_taxonomy: Class = '%s' | Phylum: %s | Order: %s",
                  taxonomy$class,
                  ifelse(is.null(taxonomy$phylum), "NA", taxonomy$phylum),
                  ifelse(is.null(taxonomy$order), "NA", taxonomy$order)))

  # Fish classes
  if (grepl("actinopterygii|chondrichthyes|myxini|agnatha|osteichthyes|elasmobranchii", class_lower)) {
    message(sprintf("      → classify_by_taxonomy: Matched Fish pattern"))
    return("Fish")
  }

  # Birds
  if (grepl("aves", class_lower)) {
    message(sprintf("      → classify_by_taxonomy: Matched Birds pattern"))
    return("Birds")
  }

  # Mammals
  if (grepl("mammalia", class_lower)) {
    message(sprintf("      → classify_by_taxonomy: Matched Mammals pattern"))
    return("Mammals")
  }

  # Benthos (invertebrates) - check phylum too for better classification
  if (grepl("malacostraca|polychaeta|bivalvia|gastropoda|anthozoa|echinoidea|asteroidea|ophiuroidea|clitellata|oligochaeta|hirudinea", class_lower)) {
    message(sprintf("      → classify_by_taxonomy: Matched Benthos pattern (class)"))
    return("Benthos")
  }

  # Check phylum for annelids (worms)
  if (!is.null(taxonomy$phylum)) {
    phylum_lower <- tolower(taxonomy$phylum)
    if (grepl("annelida", phylum_lower)) {
      message(sprintf("      → classify_by_taxonomy: Matched Benthos pattern (phylum: Annelida)"))
      return("Benthos")
    }
  }

  # Zooplankton
  if (grepl("copepoda|branchiopoda|appendicularia|thaliacea", class_lower)) {
    message(sprintf("      → classify_by_taxonomy: Matched Zooplankton pattern"))
    return("Zooplankton")
  }

  # Phytoplankton
  if (grepl("phaeophyceae|chlorophyta|bacillariophyceae|dinophyceae|cyanophyceae", class_lower)) {
    message(sprintf("      → classify_by_taxonomy: Matched Phytoplankton pattern"))
    return("Phytoplankton")
  }

  # Default to Fish (but warn - this might be wrong!)
  message(sprintf("      ⚠ WARNING: No pattern matched for class '%s' (phylum: %s) - defaulting to Fish",
                  taxonomy$class,
                  ifelse(is.null(taxonomy$phylum), "NA", taxonomy$phylum)))
  return("Fish")
}

#' Enhanced Species Classification with API Fallback
#'
#' @param species_name Character, species name
#' @param use_api Logical, whether to use API queries (default: FALSE)
#' @param ... Additional parameters passed to assign_functional_group
#'
#' @return Character, functional group
#'
#' @details
#' This function enhances the pattern-based classification with API lookups:
#' 1. First tries pattern matching (fast, offline)
#' 2. If uncertain, queries taxonomic APIs (slow, authoritative)
#'
#' @export
assign_functional_group_enhanced <- function(species_name, use_api = FALSE, pb = NA, indegree = NA, outdegree = NA, use_topology = FALSE) {
  # First try pattern matching
  pattern_result <- assign_functional_group(species_name, pb, indegree, outdegree, use_topology)

  # If not using API or got a good result from patterns, return it
  if (!use_api || pattern_result != "Fish") {
    # "Fish" is the default fallback, so it might be uncertain
    message(sprintf("    ✓ Pattern match: %s → %s", species_name, pattern_result))
    return(pattern_result)
  }

  # Try API classification for uncertain cases
  message(sprintf("    → Checking taxonomic databases for: %s", species_name))
  api_result <- classify_species_api(species_name)

  if (!is.na(api_result$functional_group) && api_result$confidence %in% c("high", "medium")) {
    message(sprintf("    ✓ API classification: %s → %s (source: %s, confidence: %s)",
                    species_name, api_result$functional_group, api_result$source, api_result$confidence))
    return(api_result$functional_group)
  }

  # Fall back to pattern result
  message(sprintf("    ✓ Using pattern match: %s → %s (API uncertain)", species_name, pattern_result))
  return(pattern_result)
}
