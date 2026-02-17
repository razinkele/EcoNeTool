#' Functional Group Utility Functions
#'
#' Shared functions for assigning functional groups to species
#' Used by ECOPATH, EcoBase, and other import methods

#' Assign Functional Group to a Species
#'
#' Determines functional group based on species name and optional network properties
#'
#' @param sp_name Character, species/group name
#' @param pb Numeric, production/biomass ratio (optional, for topology-based assignment)
#' @param indegree Numeric, network in-degree (optional, for topology-based assignment)
#' @param outdegree Numeric, network out-degree (optional, for topology-based assignment)
#' @param use_topology Logical, whether to use network topology for assignment (default: FALSE)
#'
#' @return Character, functional group name
#'
#' @details
#' Assignment priority (in order):
#' 1. Name-based patterns (highest priority)
#'    - Phytoplankton: phyto, algae, plant, diatom, cyano
#'    - Zooplankton: zoo, plankton, copepod, acartia, pseudo, mysid, cladocer, rotifer
#'    - Mammals: seal, whale, dolphin, porpoise, otter, walrus, sea lion, manatee, dugong
#'    - Birds: bird, gull, tern, cormorant, duck, goose, albatross, petrel, penguin, etc.
#'    - Fish: fish, cod, herring, sprat, flounder, shark, ray
#'    - Benthos: benthos, benthic, mussel, clam, worm, shrimp, crab, bottom, macrobent, meiobent
#'    - Detritus: detritus, det., debris
#' 2. Network topology (if use_topology = TRUE)
#'    - No predators + high P/B → Phytoplankton
#'    - Has predators, no prey → Top predator (Fish)
#'    - Has both → Intermediate (Benthos)
#' 3. Default: Fish
#'
#' @export
assign_functional_group <- function(sp_name, pb = NA, indegree = NA, outdegree = NA, use_topology = FALSE) {
  sp_lower <- tolower(sp_name)

  # Priority 1: Name-based pattern matching
  # Check detritus first (most specific)
  if (grepl("detritus|det\\.|debris", sp_lower)) {
    return("Detritus")
  }

  # Check phytoplankton (primary producers)
  if (grepl("phyto|algae|plant|diatom|cyano", sp_lower)) {
    return("Phytoplankton")
  }

  # Check benthos BEFORE zooplankton (critical fix!)
  # This prevents "Macrozoobenthos" from matching "zoo" first
  if (grepl("benthos|benthic|^bent\\b|mussel|clam|worm|shrimp|crab|bottom|macro.*bent|meio.*bent|amphipod|isopod|gastropod|bivalve|polychaete", sp_lower)) {
    return("Benthos")
  }

  # Check zooplankton (after benthos check)
  # Use ^zoo or \\bzoo to match word boundaries
  if (grepl("^zoo|\\bzooplankton|mesozoo|copepod|acartia|pseudo|mysid|cladocer|rotifer|calanus|oithona", sp_lower)) {
    return("Zooplankton")
  }

  # Check mammals BEFORE fish (critical - prevents misclassification!)
  if (grepl("seal|whale|dolphin|porpoise|otter|walrus|sea lion|fur seal|manatee|dugong|monk seal", sp_lower)) {
    return("Mammals")
  }

  # Check birds BEFORE fish (critical - prevents misclassification!)
  if (grepl("bird|gull|tern|cormoran|duck|goose|albatross|petrel|penguin|gannet|puffin|murre|auk|eider|merganser", sp_lower)) {
    return("Birds")
  }

  # Check fish (expanded patterns for freshwater and marine species)
  # Include fish orders (ending in 'iformes'), singular/plural fish names, and specific species
  if (grepl("fish|iformes|cod|herring|sprat|flounder|shark|ray|perch|pike|vimba|ruffe|stickleback|roach|bream|carp|cyprin|zander|salmon|trout|bass|tuna|mackerel|sardine|anchovy|haddock|pollock|whiting|goby|gobies|blenny|sculpin|smelt|eel|burbot|dab|plaice|sole|turbot|shad|percidae|cyprinidae", sp_lower)) {
    return("Fish")
  }

  # Priority 2: Network topology heuristics (optional)
  if (use_topology && !is.na(pb) && !is.na(indegree) && !is.na(outdegree)) {
    # No predators (indegree = 0) + high production rate → Primary producer
    if (indegree == 0 && pb > 1) {
      return("Phytoplankton")
    }

    # Has predators but no prey (top predator) → Fish
    if (indegree > 0 && outdegree == 0) {
      return("Fish")
    }

    # Has both predators and prey (intermediate consumer) → Benthos
    if (indegree > 0 && outdegree > 0) {
      return("Benthos")
    }
  }

  # Default fallback
  return("Fish")
}

#' Assign Functional Groups to Multiple Species
#'
#' Vectorized version of assign_functional_group
#'
#' @param species_names Character vector of species names
#' @param pb_values Numeric vector of P/B ratios (optional)
#' @param indegrees Numeric vector of in-degrees (optional)
#' @param outdegrees Numeric vector of out-degrees (optional)
#' @param use_topology Logical, whether to use network topology
#'
#' @return Character vector of functional groups
#' @export
assign_functional_groups <- function(species_names, pb_values = NULL, indegrees = NULL, outdegrees = NULL, use_topology = FALSE) {
  n <- length(species_names)

  # Set defaults if not provided
  if (is.null(pb_values)) pb_values <- rep(NA, n)
  if (is.null(indegrees)) indegrees <- rep(NA, n)
  if (is.null(outdegrees)) outdegrees <- rep(NA, n)

  # Assign functional groups
  functional_groups <- sapply(1:n, function(i) {
    assign_functional_group(
      species_names[i],
      pb_values[i],
      indegrees[i],
      outdegrees[i],
      use_topology
    )
  })

  return(functional_groups)
}

#' Get Functional Group Levels
#'
#' Returns the standard functional group factor levels
#'
#' @return Character vector of functional group names in standard order
#' @export
get_functional_group_levels <- function() {
  c("Benthos", "Birds", "Detritus", "Fish", "Mammals", "Phytoplankton", "Zooplankton")
}

#' Estimate Body Mass by Functional Group
#'
#' Returns typical body mass (in grams) for each functional group
#'
#' @param fg Character, functional group name
#' @return Numeric, estimated body mass in grams
#' @export
estimate_body_mass_by_fg <- function(fg) {
  # Input validation
  if (missing(fg) || is.null(fg)) {
    stop("Parameter 'fg' is required and cannot be NULL")
  }
  if (!is.character(fg) && !is.factor(fg)) {
    stop("Parameter 'fg' must be a character or factor")
  }

  body_masses <- c(
    "Phytoplankton" = 0.00001,    # 0.01 mg
    "Zooplankton" = 0.001,         # 1 mg
    "Benthos" = 1.0,               # 1 g
    "Fish" = 100.0,                # 100 g
    "Mammals" = 50000.0,           # 50 kg
    "Birds" = 2000.0,              # 2 kg
    "Detritus" = 0.0001            # 0.1 mg
  )

  ifelse(fg %in% names(body_masses), body_masses[fg], 1.0)
}

#' Estimate Metabolic Type by Functional Group
#'
#' Returns metabolic type for each functional group
#'
#' @param fg Character, functional group name
#' @return Character, metabolic type
#' @export
estimate_metabolic_type_by_fg <- function(fg) {
  # Input validation
  if (missing(fg) || is.null(fg)) {
    stop("Parameter 'fg' is required and cannot be NULL")
  }
  if (!is.character(fg) && !is.factor(fg)) {
    stop("Parameter 'fg' must be a character or factor")
  }

  if (fg == "Fish") return("ectotherm vertebrates")
  if (fg %in% c("Mammals", "Birds")) return("endotherm vertebrates")
  return("invertebrates")
}

#' Estimate Efficiency by Functional Group
#'
#' Returns energy transfer efficiency for each functional group
#'
#' @param fg Character, functional group name
#' @return Numeric, efficiency (0-1)
#' @export
estimate_efficiency_by_fg <- function(fg) {
  efficiencies <- c(
    "Phytoplankton" = 0.4,
    "Zooplankton" = 0.75,
    "Benthos" = 0.7,
    "Fish" = 0.85,
    "Mammals" = 0.9,
    "Birds" = 0.88,
    "Detritus" = 0.2
  )

  ifelse(fg %in% names(efficiencies), efficiencies[fg], 0.7)
}

#' Estimate Body Mass with Size and Stage Information
#'
#' Enhanced body mass estimation that extracts size and ontogenetic stage
#' information from species/group names
#'
#' @param species_name Character, full species/group name (may contain size and stage info)
#' @param fg Character, functional group name
#' @return Numeric, estimated body mass in grams
#'
#' @details
#' Extracts information from species names:
#'
#' Size patterns:
#' - "<X mm", "<X cm", "<X g" - smaller than X
#' - "X-Y mm", "X-Y cm" - range from X to Y
#' - ">X mm", ">X cm", ">X g" - larger than X
#'
#' Ontogenetic stages:
#' - "juvenile" - 20-40% of adult mass
#' - "sub-adult" / "subadult" - 60-80% of adult mass
#' - "adult" - 100% (baseline)
#' - "larvae" / "larval" - 5-15% of adult mass
#'
#' Examples:
#' - "Macrozoobenthos (<9.9mm)" → small benthos, ~0.01 g
#' - "Baltic herring (juvenile)" → juvenile fish, ~20 g (20% of 100g)
#' - "Cod (adult)" → adult fish, ~100 g
#'
#' @export
estimate_body_mass_enhanced <- function(species_name, fg) {
  # Get baseline mass for functional group
  baseline_mass <- estimate_body_mass_by_fg(fg)

  # Convert to lowercase for pattern matching
  name_lower <- tolower(species_name)

  # =========================================================================
  # EXTRACT SIZE INFORMATION
  # =========================================================================

  size_multiplier <- 1.0
  size_extracted <- FALSE

  # Pattern: <X mm, <X,X mm (handles both . and , as decimal separator)
  if (grepl("<\\s*([0-9]+[.,]?[0-9]*)\\s*mm", name_lower)) {
    size_match <- regmatches(name_lower, regexpr("<\\s*([0-9]+[.,]?[0-9]*)\\s*mm", name_lower))
    size_value <- as.numeric(gsub(",", ".", gsub("[^0-9.,]", "", size_match)))

    if (!is.na(size_value)) {
      # Convert mm to appropriate size multiplier
      if (fg == "Benthos") {
        # For benthos: <10mm = very small
        if (size_value < 10) {
          size_multiplier <- 0.01  # 1% of baseline
        } else if (size_value < 20) {
          size_multiplier <- 0.1   # 10% of baseline
        } else {
          size_multiplier <- 0.5   # 50% of baseline
        }
      } else if (fg == "Fish") {
        # For fish: size in mm indicates larvae/juveniles
        size_multiplier <- 0.05  # 5% of baseline
      }
      size_extracted <- TRUE
    }
  }

  # Pattern: X-Y mm (range)
  if (!size_extracted && grepl("([0-9]+[.,]?[0-9]*)-([0-9]+[.,]?[0-9]*)\\s*mm", name_lower)) {
    size_match <- regmatches(name_lower, regexpr("([0-9]+[.,]?[0-9]*)-([0-9]+[.,]?[0-9]*)\\s*mm", name_lower))
    # Extract both numbers
    numbers <- as.numeric(gsub(",", ".", unlist(regmatches(size_match, gregexpr("[0-9]+[.,]?[0-9]*", size_match)))))

    if (length(numbers) == 2 && !any(is.na(numbers))) {
      avg_size <- mean(numbers)

      if (fg == "Benthos") {
        if (avg_size < 10) {
          size_multiplier <- 0.01  # Very small
        } else if (avg_size < 15) {
          size_multiplier <- 0.05  # Small
        } else if (avg_size < 25) {
          size_multiplier <- 0.2   # Medium
        } else {
          size_multiplier <- 0.8   # Large
        }
      } else if (fg == "Zooplankton") {
        # Zooplankton size scaling
        if (avg_size < 1) {
          size_multiplier <- 0.1
        } else if (avg_size < 5) {
          size_multiplier <- 0.5
        } else {
          size_multiplier <- 2.0   # Large zooplankton
        }
      }
      size_extracted <- TRUE
    }
  }

  # Pattern: >X mm (larger than)
  if (!size_extracted && grepl(">\\s*([0-9]+[.,]?[0-9]*)\\s*mm", name_lower)) {
    size_match <- regmatches(name_lower, regexpr(">\\s*([0-9]+[.,]?[0-9]*)\\s*mm", name_lower))
    size_value <- as.numeric(gsub(",", ".", gsub("[^0-9.,]", "", size_match)))

    if (!is.na(size_value)) {
      if (fg == "Benthos") {
        if (size_value > 20) {
          size_multiplier <- 2.0   # Double baseline for large benthos
        } else {
          size_multiplier <- 1.5   # 50% larger
        }
      }
      size_extracted <- TRUE
    }
  }

  # Pattern: X-Y cm (larger organisms, centimeters)
  if (!size_extracted && grepl("([0-9]+[.,]?[0-9]*)-([0-9]+[.,]?[0-9]*)\\s*cm", name_lower)) {
    size_match <- regmatches(name_lower, regexpr("([0-9]+[.,]?[0-9]*)-([0-9]+[.,]?[0-9]*)\\s*cm", name_lower))
    numbers <- as.numeric(gsub(",", ".", unlist(regmatches(size_match, gregexpr("[0-9]+[.,]?[0-9]*", size_match)))))

    if (length(numbers) == 2 && !any(is.na(numbers))) {
      avg_size_cm <- mean(numbers)

      if (fg == "Fish") {
        # Fish length-weight relationship (approximate)
        # W = a * L^b, where b ~ 3 (isometric growth)
        # Relative mass = (L / L_baseline)^3
        baseline_length_cm <- 20  # Assume baseline is ~20cm fish
        size_multiplier <- (avg_size_cm / baseline_length_cm)^3
        size_multiplier <- max(0.01, min(size_multiplier, 100))  # Cap at reasonable range
      }
      size_extracted <- TRUE
    }
  }

  # Pattern: X-Y g (mass directly specified)
  if (!size_extracted && grepl("([0-9]+[.,]?[0-9]*)-([0-9]+[.,]?[0-9]*)\\s*g", name_lower)) {
    size_match <- regmatches(name_lower, regexpr("([0-9]+[.,]?[0-9]*)-([0-9]+[.,]?[0-9]*)\\s*g", name_lower))
    numbers <- as.numeric(gsub(",", ".", unlist(regmatches(size_match, gregexpr("[0-9]+[.,]?[0-9]*", size_match)))))

    if (length(numbers) == 2 && !any(is.na(numbers))) {
      avg_mass_g <- mean(numbers)
      # Use actual mass value directly (overrides baseline)
      return(avg_mass_g)
    }
  }

  # =========================================================================
  # EXTRACT ONTOGENETIC STAGE INFORMATION
  # =========================================================================

  stage_multiplier <- 1.0

  # Larvae / Larval - smallest stage (5-15% of adult)
  if (grepl("\\blarva\\b|\\blarval\\b|\\blarvae\\b", name_lower)) {
    stage_multiplier <- 0.10  # 10% of adult mass
  }
  # Juvenile - young stage (20-40% of adult)
  else if (grepl("\\bjuvenile\\b|\\bjuv\\b", name_lower)) {
    stage_multiplier <- 0.30  # 30% of adult mass
  }
  # Sub-adult / Subadult - pre-adult stage (60-80% of adult)
  else if (grepl("\\bsub-adult\\b|\\bsubadult\\b|\\bsub\\b", name_lower)) {
    stage_multiplier <- 0.70  # 70% of adult mass
  }
  # Adult - full size (100%, baseline)
  else if (grepl("\\badult\\b", name_lower)) {
    stage_multiplier <- 1.0   # No adjustment needed
  }
  # If stage not mentioned but size is specified, assume size dominates
  # (don't apply default stage multiplier)

  # =========================================================================
  # COMBINE SIZE AND STAGE MULTIPLIERS
  # =========================================================================

  # If both size and stage are extracted, size takes precedence for benthos/zooplankton
  # For fish, combine both (size is relative to stage)
  if (size_extracted && stage_multiplier != 1.0) {
    if (fg == "Fish" || fg == "Birds" || fg == "Mammals") {
      # For vertebrates: stage modifies the size-adjusted mass
      final_multiplier <- size_multiplier * stage_multiplier
    } else {
      # For invertebrates: size is more reliable, use stage only if size not found
      final_multiplier <- size_multiplier
    }
  } else if (size_extracted) {
    final_multiplier <- size_multiplier
  } else {
    final_multiplier <- stage_multiplier
  }

  # Apply multiplier to baseline mass
  estimated_mass <- baseline_mass * final_multiplier

  # Ensure reasonable range (0.00001 g to 100 kg)
  estimated_mass <- max(0.00001, min(estimated_mass, 100000))

  return(estimated_mass)
}
