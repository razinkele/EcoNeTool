# app.R
# Shiny app for Baltic Food Web analysis
library(shiny)
library(igraph)
library(fluxweb)
library(visNetwork)

source("plotfw.R")

# ============================================================================
# CONFIGURATION CONSTANTS
# ============================================================================

# Color scheme for functional groups (Benthos, Detritus, Fish, Phytoplankton, Zooplankton)
COLOR_SCHEME <- c("orange", "darkgrey", "blue", "green", "cyan")

# Trophic level calculation parameters
TROPHIC_LEVEL_MAX_ITER <- 100      # Maximum iterations for convergence
TROPHIC_LEVEL_CONVERGENCE <- 0.0001  # Convergence threshold

# Flux calculation parameters
FLUX_CONVERSION_FACTOR <- 86.4     # Convert J/sec to kJ/day
FLUX_LOG_EPSILON <- 0.00001        # Small value to avoid log(0)

# Visualization parameters
NODE_SIZE_SCALE <- 25              # Scaling factor for node size by biomass
NODE_SIZE_MIN <- 4                 # Minimum node size
EDGE_WIDTH_SCALE <- 15             # Scaling factor for edge width by flux
EDGE_WIDTH_MIN <- 0.1              # Minimum edge width
EDGE_ARROW_SIZE_TOPOLOGY <- 0.3    # Arrow size for topology networks
EDGE_ARROW_SIZE_FLUX <- 0.05       # Arrow size for flux networks

# Data file path
DATA_FILE <- "BalticFW.Rdata"

# ============================================================================
# DATA LOADING WITH VALIDATION
# ============================================================================

# Load data and prepare variables
if (!file.exists(DATA_FILE)) {
  stop(paste("Data file not found:", DATA_FILE,
             "\nPlease ensure BalticFW.Rdata is in the working directory."))
}

tryCatch({
  load(DATA_FILE)

  # Validate required objects exist
  if (!exists("net")) stop("'net' object not found in data file")
  if (!exists("info")) stop("'info' object not found in data file")
  if (!exists("fluxind")) stop("'fluxind' function not found in data file")

  # Validate network structure
  if (!igraph::is_igraph(net)) stop("'net' must be an igraph object")
  if (vcount(net) == 0) stop("Network contains no vertices")
  if (ecount(net) == 0) warning("Network contains no edges")

  # Validate info data frame
  required_cols <- c("meanB", "fg", "losses", "efficiencies")
  missing_cols <- setdiff(required_cols, colnames(info))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns in info:", paste(missing_cols, collapse=", ")))
  }

  # Validate biomass values
  if (any(info$meanB < 0, na.rm = TRUE)) {
    stop("Biomass values must be non-negative")
  }
  if (all(is.na(info$meanB))) {
    stop("All biomass values are NA")
  }

  # Validate functional groups
  if (nlevels(info$fg) != length(COLOR_SCHEME)) {
    warning(paste("Number of functional groups (", nlevels(info$fg),
                  ") does not match color scheme length (", length(COLOR_SCHEME), ")",
                  sep=""))
  }

}, error = function(e) {
  stop(paste("Error loading data:", e$message))
})

# ============================================================================
# HELPER FUNCTIONS WITH DOCUMENTATION
# ============================================================================

#' Calculate trophic levels for a food web
#'
#' Computes trophic levels using an iterative algorithm. Basal species
#' (no prey) are assigned TL = 1. Consumer species have TL = 1 + mean(TL of prey).
#' The algorithm iterates until convergence or maximum iterations reached.
#'
#' @param net An igraph object representing the food web (directed graph)
#'
#' @return A numeric vector of trophic levels for each species/node
#'
#' @details
#' The algorithm uses fixed-point iteration:
#' - Initialize all species to TL = 1
#' - Iterate: for each consumer, TL = 1 + mean(prey TL)
#' - Stop when max change < TROPHIC_LEVEL_CONVERGENCE or max iterations reached
#'
#' @examples
#' tl <- trophiclevels(net)
#' mean(tl)  # Mean trophic level of the food web
#'
#' @references
#' Williams, R. J., & Martinez, N. D. (2004). Limits to trophic levels and
#' omnivory in complex food webs. Proceedings of the Royal Society B, 271(1540), 549-556.
trophiclevels <- function(net) {
  # Input validation
  if (!igraph::is_igraph(net)) {
    stop("Input 'net' must be an igraph object")
  }

  n <- vcount(net)
  if (n == 0) {
    stop("Network contains no vertices")
  }

  tl <- rep(1, n)  # Initialize all to 1
  adj <- as_adjacency_matrix(net, sparse = FALSE)

  # Iterate until convergence
  converged <- FALSE
  for (iter in 1:TROPHIC_LEVEL_MAX_ITER) {
    tl_old <- tl
    for (i in 1:n) {
      # Find prey of species i (incoming edges)
      prey_indices <- which(adj[i, ] > 0)
      if (length(prey_indices) > 0) {
        # TL = 1 + mean TL of prey
        tl[i] <- 1 + mean(tl[prey_indices])
      } else {
        # Basal species
        tl[i] <- 1
      }
    }
    # Check for convergence
    if (max(abs(tl - tl_old)) < TROPHIC_LEVEL_CONVERGENCE) {
      converged <- TRUE
      break
    }
  }

  if (!converged) {
    warning(paste("Trophic level calculation did not converge after",
                  TROPHIC_LEVEL_MAX_ITER, "iterations"))
  }

  return(tl)
}

#' Calculate topological (qualitative) indicators for a food web
#'
#' Computes structural properties of the food web network without considering
#' node weights (biomass). These are purely topological metrics.
#'
#' @param net An igraph object representing the food web
#'
#' @return A list containing:
#' \describe{
#'   \item{S}{Species richness (number of taxa)}
#'   \item{C}{Connectance (proportion of realized links)}
#'   \item{G}{Generality (mean number of prey per predator)}
#'   \item{V}{Vulnerability (mean number of predators per prey)}
#'   \item{ShortPath}{Mean shortest path length}
#'   \item{TL}{Mean trophic level}
#'   \item{Omni}{Omnivory index (mean SD of prey trophic levels)}
#' }
#'
#' @details
#' Formulas:
#' - C = L / (S * (S-1)) where L is number of links
#' - G = sum(in-degree for predators) / number of predators
#' - V = sum(out-degree for prey) / number of prey
#' - Omnivory = mean(SD of prey TL for each consumer)
#'
#' @references
#' Williams, R. J., & Martinez, N. D. (2000). Simple rules yield complex food webs.
#' Nature, 404(6774), 180-183.
get_topological_indicators <- function(net) {
  # Input validation
  if (!igraph::is_igraph(net)) {
    stop("Input 'net' must be an igraph object")
  }

  tryCatch({
    S <- vcount(net)
    if (S <= 1) {
      warning("Network has only one or zero species. Metrics may be undefined.")
    }

    C <- ecount(net)/(S*(S-1))
    pred <- degree(net, mode="in")>0
    G <- sum(degree(net, mode="in")[pred])/sum(pred)
    prey <- degree(net, mode="out")>0
    V <- sum(degree(net, mode="out")[prey])/sum(prey)
    sp <- distances(net)
    ShortPath <- mean(sp[upper.tri(sp)])
    tlnodes <- trophiclevels(net)
    TL <- mean(tlnodes)
    netmatrix <- as_adjacency_matrix(net, sparse=F)
    webtl <- netmatrix*tlnodes
    webtl[webtl==0] <- NA
    omninodes <- apply(webtl,2,sd, na.rm=TRUE)
    Omni <- mean(omninodes, na.rm=TRUE)

    list(S=S, C=C, G=G, V=V, ShortPath=ShortPath, TL=TL, Omni=Omni)

  }, error = function(e) {
    stop(paste("Error calculating topological indicators:", e$message))
  })
}

#' Calculate node-weighted (quantitative) indicators for a food web
#'
#' Computes network metrics weighted by node biomass. These metrics account
#' for the relative importance of species based on their biomass.
#'
#' @param net An igraph object representing the food web
#' @param info Data frame containing species information with 'meanB' column for biomass
#'
#' @return A list containing:
#' \describe{
#'   \item{nwC}{Node-weighted connectance}
#'   \item{nwG}{Node-weighted generality}
#'   \item{nwV}{Node-weighted vulnerability}
#'   \item{nwTL}{Node-weighted mean trophic level}
#' }
#'
#' @details
#' Node-weighted metrics give more importance to high-biomass species.
#' - nwC = sum(degree * biomass) / (2 * sum(biomass) * (S-1))
#' - nwG = sum(in-degree * biomass for predators) / sum(predator biomass)
#' - nwV = sum(out-degree * biomass for prey) / sum(prey biomass)
#' - nwTL = sum(TL * biomass) / sum(biomass)
#'
#' @references
#' Olivier, P., et al. (2019). Exploring the temporal variability of a food web
#' using long-term biomonitoring data. Ecography, 42(11), 2107-2121.
get_node_weighted_indicators <- function(net, info) {
  # Input validation
  if (!igraph::is_igraph(net)) {
    stop("Input 'net' must be an igraph object")
  }
  if (!is.data.frame(info)) {
    stop("Input 'info' must be a data frame")
  }
  if (!"meanB" %in% colnames(info)) {
    stop("'info' must contain 'meanB' column for biomass")
  }
  if (nrow(info) != vcount(net)) {
    stop("Number of rows in 'info' must match number of vertices in 'net'")
  }

  tryCatch({
    biomass <- info$meanB

    # Check for NA or negative biomass
    if (any(is.na(biomass))) {
      warning("NA values found in biomass, results may be unreliable")
    }
    if (any(biomass < 0, na.rm = TRUE)) {
      stop("Biomass values must be non-negative")
    }

    tlnodes <- trophiclevels(net)
    nwC <- sum(degree(net)*biomass)/(2*sum(biomass)*(vcount(net)-1))
    pred <- degree(net, mode="in")>0
    nwG <- sum((degree(net, mode="in")*biomass)[pred])/(sum(biomass[pred]))
    prey <- degree(net, mode="out")>0
    nwV <- sum((degree(net, mode="out")*biomass)[prey])/(sum(biomass[prey]))
    nwTL <- sum(tlnodes*biomass)/sum(biomass)

    list(nwC=nwC, nwG=nwG, nwV=nwV, nwTL=nwTL)

  }, error = function(e) {
    stop(paste("Error calculating node-weighted indicators:", e$message))
  })
}

#' Calculate energy fluxes using metabolic theory
#'
#' Computes biomass fluxes between species using the fluxweb package,
#' which applies metabolic theory of ecology. Returns both flux matrix
#' and weighted network.
#'
#' @param net An igraph object representing the food web
#' @param info Data frame with columns: meanB (biomass), losses (metabolic losses),
#'        efficiencies (assimilation efficiencies)
#'
#' @return A list containing:
#' \describe{
#'   \item{fluxes}{Matrix of energy fluxes (kJ/day/km²) between species}
#'   \item{netLW}{Weighted igraph object with flux as edge weights}
#' }
#'
#' @details
#' Uses allometric scaling based on metabolic theory:
#' X_i = (x_0 * M_i^0.71 * exp(-E/BT)) * b_i
#'
#' Fluxes are calculated using prey-level assimilation efficiencies
#' and converted from J/sec to kJ/day (multiply by FLUX_CONVERSION_FACTOR).
#'
#' @references
#' Brown, J. H., et al. (2004). Toward a metabolic theory of ecology.
#' Ecology, 85(7), 1771-1789.
#'
#' Gauzens, B., et al. (2019). fluxweb: An R package to easily estimate energy
#' fluxes in food webs. Methods in Ecology and Evolution, 10(2), 270-279.
get_fluxweb_results <- function(net, info) {
  # Input validation
  if (!igraph::is_igraph(net)) {
    stop("Input 'net' must be an igraph object")
  }
  if (!is.data.frame(info)) {
    stop("Input 'info' must be a data frame")
  }

  required_cols <- c("meanB", "losses", "efficiencies")
  missing_cols <- setdiff(required_cols, colnames(info))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns in info:", paste(missing_cols, collapse=", ")))
  }

  if (nrow(info) != vcount(net)) {
    stop("Number of rows in 'info' must match number of vertices in 'net'")
  }

  tryCatch({
    netmatrix <- as_adjacency_matrix(net, sparse=F)
    biomass <- info$meanB

    # Validate inputs
    if (any(is.na(biomass))) {
      warning("NA values in biomass, flux calculations may fail")
    }
    if (any(biomass < 0, na.rm = TRUE)) {
      stop("Biomass values must be non-negative")
    }

    # Calculate fluxes using fluxweb package
    fluxes <- fluxing(netmatrix, biomass, info$losses, info$efficiencies, ef.level="prey")

    # Convert J/sec to kJ/day
    fluxes <- fluxes * FLUX_CONVERSION_FACTOR

    # Create weighted network
    netLW <- graph_from_adjacency_matrix(fluxes, weighted=TRUE)

    list(fluxes=fluxes, netLW=netLW)

  }, error = function(e) {
    stop(paste("Error calculating fluxweb results:", e$message))
  })
}

 # UI
ui <- fluidPage(
  titlePanel("Baltic Food Web Explorer"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      helpText("Explore the Gulf of Riga food web. Data: Frelat & Kortsch, 2020."),
      hr(),
      helpText("Use the tabs on the right to navigate through different analysis sections.")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        type = "pills",

        tabPanel(
          "Food Web Visualization",
          icon = icon("sitemap"),
          br(),
          visNetworkOutput("foodweb_visnet", height = "600px"),
          hr(),
          verbatimTextOutput("basal_species"),
          verbatimTextOutput("top_predators"),
          plotOutput("adjacency_heatmap")
        ),

        tabPanel(
          "Topological Indicators",
          icon = icon("chart-bar"),
          br(),
          verbatimTextOutput("topo_indicators")
        ),

        tabPanel(
          "Node-weighted Indicators",
          icon = icon("balance-scale"),
          br(),
          plotOutput("biomass_boxplot"),
          hr(),
          plotOutput("biomass_barplot"),
          hr(),
          plotOutput("foodweb_biomass_plot"),
          hr(),
          verbatimTextOutput("node_weighted_indicators")
        ),

        tabPanel(
          "Fluxweb Analysis",
          icon = icon("exchange"),
          br(),
          plotOutput("flux_heatmap"),
          hr(),
          plotOutput("flux_network_plot"),
          hr(),
          verbatimTextOutput("flux_indicators")
        )
      )
    )
  )
)

# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {
  # Assign colors to functional groups
  info$colfg <- COLOR_SCHEME[as.numeric(info$fg)]

  # Food Web Visualization (visNetwork)
  output$foodweb_visnet <- renderVisNetwork({
    tryCatch({
      # Prepare nodes data frame
      nodes <- data.frame(
        id = 1:vcount(net),
        label = V(net)$name,
        group = info$fg,
        color = info$colfg,
        value = info$meanB,
        title = paste0("<b>", V(net)$name, "</b><br>FG: ", info$fg, "<br>Biomass: ", round(info$meanB,2)),
        stringsAsFactors = FALSE
      )
      # Map node names to IDs for edges
      name_to_id <- setNames(nodes$id, nodes$label)
      edgelist <- as.data.frame(as_edgelist(net))
      colnames(edgelist) <- c("from", "to")
      edgelist$from <- name_to_id[edgelist$from]
      edgelist$to <- name_to_id[edgelist$to]
      # Create network visualization
      visNetwork(nodes, edgelist, width = "100%", height = "600px") %>%
        visEdges(arrows = "to") %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visLegend(useGroups = TRUE, width = 0.15)
    }, error = function(e) {
      # Return empty network on error
      visNetwork(data.frame(id=1, label="Error", title=e$message),
                 data.frame(from=integer(0), to=integer(0)))
    })
  })

  output$basal_species <- renderPrint({
    tryCatch({
      basal <- V(net)$name[degree(net, mode="in")==0]
      cat("Basal species:\n", paste(basal, collapse=", "))
    }, error = function(e) {
      cat("Error identifying basal species:", e$message)
    })
  })

  output$top_predators <- renderPrint({
    tryCatch({
      top_pred <- V(net)$name[degree(net, mode="out")==0]
      cat("Top predators:\n", paste(top_pred, collapse=", "))
    }, error = function(e) {
      cat("Error identifying top predators:", e$message)
    })
  })

  output$adjacency_heatmap <- renderPlot({
    tryCatch({
      netmatrix <- as_adjacency_matrix(net, sparse=F)
      heatmap(netmatrix, Rowv=NA, Colv=NA, scale="none")
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating adjacency heatmap:", e$message))
    })
  })

  # Topological Indicators
  output$topo_indicators <- renderPrint({
    tryCatch({
      ind <- get_topological_indicators(net)
      print(ind)
    }, error = function(e) {
      cat("Error calculating topological indicators:", e$message)
    })
  })

  output$node_weighted_indicators <- renderPrint({
    tryCatch({
      ind <- get_node_weighted_indicators(net, info)
      print(ind)
    }, error = function(e) {
      cat("Error calculating node-weighted indicators:", e$message)
    })
  })

  # Node-weighted Indicators
  output$biomass_boxplot <- renderPlot({
    tryCatch({
      boxplot(info$meanB~info$fg, las=2, col=COLOR_SCHEME,
              ylab="Biomass (g/day/km2)", xlab="")
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating biomass boxplot:", e$message))
    })
  })

  output$biomass_barplot <- renderPlot({
    tryCatch({
      percB <- tapply(info$meanB, info$fg, sum)/sum(info$meanB)*100
      barplot(as.matrix(percB), col=COLOR_SCHEME, ylab="%")
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating biomass barplot:", e$message))
    })
  })

  output$foodweb_biomass_plot <- renderPlot({
    tryCatch({
      nodmax <- max(info$meanB)
      sizeB <- (info$meanB/nodmax)*NODE_SIZE_SCALE + NODE_SIZE_MIN
      plotfw(net, col=info$colfg, size=sizeB,
             edge.width=EDGE_ARROW_SIZE_TOPOLOGY,
             edge.arrow.size=EDGE_ARROW_SIZE_TOPOLOGY)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating biomass plot:", e$message))
    })
  })

  # Fluxweb Analysis
  output$flux_heatmap <- renderPlot({
    tryCatch({
      res <- get_fluxweb_results(net, info)
      heatmap(log(res$fluxes + FLUX_LOG_EPSILON), Rowv=NA, Colv=NA, scale="none")
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating flux heatmap:", e$message))
    })
  })

  output$flux_network_plot <- renderPlot({
    tryCatch({
      res <- get_fluxweb_results(net, info)
      wid <- EDGE_WIDTH_MIN + (E(res$netLW)$weight/max(E(res$netLW)$weight) * EDGE_WIDTH_SCALE)
      V(res$netLW)$frame.color <- NA
      plotfw(res$netLW, col=info$colfg, edge.width=wid, edge.arrow.size=EDGE_ARROW_SIZE_FLUX)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating flux network plot:", e$message))
    })
  })

  output$flux_indicators <- renderPrint({
    tryCatch({
      res <- get_fluxweb_results(net, info)
      print(fluxind(res$fluxes))
    }, error = function(e) {
      cat("Error calculating flux indicators:", e$message)
    })
  })
}
shinyApp(ui, server)
