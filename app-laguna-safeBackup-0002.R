# app.R
# Shiny app for Baltic Food Web analysis
library(shiny)
library(bs4Dash)
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
  # Load data into a separate environment to avoid overwriting functions
  # BalticFW.Rdata contains functions (trophiclevels, plotfw, fluxind) that
  # would overwrite our own function definitions if loaded into GlobalEnv
  suppressMessages({
    data_env <- new.env()
    load(DATA_FILE, envir = data_env)

    # Validate required objects exist
    if (!exists("net", envir = data_env)) stop("'net' object not found in data file")
    if (!exists("info", envir = data_env)) stop("'info' object not found in data file")

    # Extract only the data objects (not functions) from the loaded environment
    net <<- data_env$net
    info <<- data_env$info

    # Validate network structure
    if (!igraph::is_igraph(net)) stop("'net' must be an igraph object")

    # Upgrade igraph object if needed
    net <<- igraph::upgrade_graph(net)
  })

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

#' Calculate link-weighted flux indicators
#'
#' Computes Shannon diversity-based indicators from an energy flux matrix.
#' These metrics account for the distribution of energy flows across trophic links.
#'
#' @param fluxes Numeric matrix of energy fluxes between species (from fluxing())
#' @param loop Logical, whether to include self-loops in connectance calculation
#'
#' @return A list containing:
#' \describe{
#'   \item{lwC}{Link-weighted connectance}
#'   \item{lwG}{Link-weighted generality (effective number of prey)}
#'   \item{lwV}{Link-weighted vulnerability (effective number of predators)}
#' }
#'
#' @details
#' Uses Shannon diversity indices to calculate effective numbers of trophic
#' interactions. Higher values indicate more evenly distributed energy flows.
#'
#' @references
#' Bersier, L. F., et al. (2002). Quantitative descriptors of food web matrices.
#' Ecology, 83(9), 2394-2407.
fluxind <- function(fluxes, loop = FALSE) {
  res <- list()

  # The flux matrix
  W.net <- as.matrix(fluxes)

  ### Taxon-specific Shannon indices of inflows
  # sum of k species inflows --> colsums
  sum.in <- apply(W.net, 2, sum)

  # Diversity of k species inflows
  # columns divided by the total col sum
  H.in.mat <- t(t(W.net)/sum.in)*t(log(t(W.net)/sum.in))
  H.in.mat[!is.finite(H.in.mat)] <- 0  # converts NaN to 0's
  H.in <- apply(H.in.mat, 2, sum)*-1

  # Effective number of prey or resources = N(R,k)
  # The reciprocal of H(R,k) --> N (R,k) is the equivalent number of prey for species k
  N.res <- ifelse(sum.in==0, H.in, exp(H.in))

  ### Taxon-specific Shannon indices of outflows
  # sum of k species outflows --> rowsums
  sum.out <- apply(W.net, 1, sum)

  # Diversity of k species outflows
  # rows divided by the total row sum
  H.out.mat <- (W.net/sum.out)*log(W.net/sum.out)
  H.out.mat[!is.finite(H.out.mat)] <- 0  # converts NaN to 0's
  H.out <- apply(H.out.mat, 1, sum)*-1

  # Effective number of predators or consumers = N(C,k)
  # The reciprocal of H(C,k) --> N (C,k) is the equivalent number of predators for species k
  N.con <- ifelse(sum.out==0, H.out, exp(H.out))

  ### Quantitative Weighted connectance
  no.species <- ncol(W.net)

  # The weighted link density (LDw) is:
  # In the weighted version the effective number of predators for species i is weighted by i's
  # contribution to the total outflow the same is the case for the inflows
  tot.mat <- sum(W.net)
  # LD.w <- (sum((sum.in/tot.mat)*N.res) + sum((sum.out/tot.mat)*N.con))/2
  # equivalent to next formula, but next one is closer to manuscript
  LD <- 1/(2*tot.mat)*(sum(sum.in*N.res) + sum(sum.out*N.con))

  # Weighted connectance
  res$lwC <- LD/ifelse(loop, no.species, no.species-1)

  # positional.index
  pos.ind <- sum.in*N.res/(sum.in*N.res+sum.out*N.con)  # positional index
  basal.sp <- pos.ind[pos.ind==0]  # basal species = 0
  top.sp <- pos.ind[pos.ind==1]  # definition according to Bersier et al. 2002 top species = [0.99, 1]

  con.sp <- length(pos.ind)-length(basal.sp)  # all consumer taxa except basal
  # weighted quantitative Generality
  res$lwG <- sum(sum.in*N.res/sum(W.net))

  res.sp <- length(pos.ind)-length(top.sp)
  # weighted quantitative Vulnerability
  res$lwV <- sum(sum.out*N.con/sum(W.net))

  return(res)
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

# ============================================================================
# UI - BS4DASH DASHBOARD
# ============================================================================

ui <- dashboardPage(
  # ============================================================================
  # HEADER
  # ============================================================================
  header = dashboardHeader(
    title = dashboardBrand(
      title = "EcoNeTool",
      color = "primary",
      href = "https://github.com",
      image = NULL
    ),
    skin = "light",
    status = "white",
    border = TRUE,
    sidebarIcon = icon("bars"),
    controlbarIcon = icon("info-circle"),
    fixed = FALSE,
    leftUI = tagList(
      h4("Food Web Explorer", style = "margin: 10px; color: #007bff;")
    ),
    rightUI = tagList(
      dropdownMenu(
        type = "messages",
        badgeStatus = "info",
        icon = icon("question-circle"),
        messageItem(
          from = "About",
          message = "Gulf of Riga Food Web (1979-2016)",
          icon = icon("fish"),
          time = "34 taxa"
        ),
        messageItem(
          from = "Data Source",
          message = "Frelat & Kortsch, 2020",
          icon = icon("database"),
          time = "207 links"
        )
      )
    )
  ),

  # ============================================================================
  # SIDEBAR
  # ============================================================================
  sidebar = dashboardSidebar(
    skin = "light",
    status = "primary",
    elevation = 3,
    sidebarMenu(
      id = "sidebar_menu",

      sidebarHeader("Navigation"),

      menuItem(
        text = "Dashboard",
        tabName = "dashboard",
        icon = icon("home")
      ),

      menuItem(
        text = "Data Import",
        tabName = "import",
        icon = icon("upload")
      ),

      menuItem(
        text = "Food Web Network",
        tabName = "network",
        icon = icon("project-diagram")
      ),

      menuItem(
        text = "Topological Metrics",
        tabName = "topological",
        icon = icon("chart-line")
      ),

      menuItem(
        text = "Biomass Analysis",
        tabName = "biomass",
        icon = icon("weight")
      ),

      menuItem(
        text = "Energy Fluxes",
        tabName = "fluxes",
        icon = icon("bolt")
      ),

      sidebarHeader("Information"),

      tags$div(
        style = "padding: 15px; font-size: 12px; color: #6c757d;",
        tags$p(tags$strong("EcoNeTool")),
        tags$p("Interactive analysis of marine food web networks."),
        tags$p(
          tags$i(class = "fas fa-fish"), " 34 species", tags$br(),
          tags$i(class = "fas fa-link"), " 207 trophic links", tags$br(),
          tags$i(class = "fas fa-layer-group"), " 5 functional groups"
        ),
        tags$p(style = "margin-top: 10px;", tags$small("Data: Frelat & Kortsch, 2020"))
      )
    )
  ),

  # ============================================================================
  # BODY
  # ============================================================================
  body = dashboardBody(
    tabItems(

      # ========================================================================
      # DASHBOARD TAB
      # ========================================================================
      tabItem(
        tabName = "dashboard",

        fluidRow(
          box(
            title = "Welcome to EcoNeTool",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            HTML("
              <h4>Food Web Explorer</h4>
              <p>This interactive dashboard allows you to explore and analyze marine food web networks.
              The tool integrates qualitative and quantitative network analysis approaches to understand
              food web structure and dynamics.</p>

              <h5>Current Dataset:</h5>
              <ul>
                <li><strong>Source:</strong> Gulf of Riga food web (Frelat & Kortsch, 2020)</li>
                <li><strong>Period:</strong> 1979-2016 (37 years)</li>
                <li><strong>Taxa:</strong> 34 species across 5 functional groups</li>
                <li><strong>Links:</strong> 207 trophic interactions</li>
              </ul>

              <h5>Features:</h5>
              <ul>
                <li><strong>Data Import:</strong> Upload your own food web data (Excel, CSV, RData)</li>
                <li><strong>Food Web Network:</strong> Interactive visualization of species interactions</li>
                <li><strong>Topological Metrics:</strong> Qualitative indicators (Connectance, Generality, Vulnerability, etc.)</li>
                <li><strong>Biomass Analysis:</strong> Node-weighted metrics accounting for species biomass</li>
                <li><strong>Energy Fluxes:</strong> Metabolic theory-based energy flow calculations</li>
              </ul>

              <h5>Navigation:</h5>
              <p>Use the sidebar menu to navigate through different analysis sections. Start with <strong>Data Import</strong>
              to learn about supported formats or use the default Gulf of Riga dataset.</p>
            ")
          )
        ),

        fluidRow(
          valueBox(
            value = 34,
            subtitle = "Taxa / Species",
            icon = icon("fish"),
            color = "primary",
            width = 3
          ),
          valueBox(
            value = 207,
            subtitle = "Trophic Links",
            icon = icon("link"),
            color = "success",
            width = 3
          ),
          valueBox(
            value = 5,
            subtitle = "Functional Groups",
            icon = icon("layer-group"),
            color = "info",
            width = 3
          ),
          valueBox(
            value = "1979-2016",
            subtitle = "Time Period",
            icon = icon("calendar"),
            color = "warning",
            width = 3
          )
        ),

        fluidRow(
          box(
            title = "Functional Groups",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            HTML("
              <div style='padding: 10px;'>
                <p><span style='color: orange; font-size: 20px;'>●</span> <strong>Benthos</strong> - Bottom-dwelling organisms</p>
                <p><span style='color: darkgrey; font-size: 20px;'>●</span> <strong>Detritus</strong> - Organic matter</p>
                <p><span style='color: blue; font-size: 20px;'>●</span> <strong>Fish</strong> - Fish species</p>
                <p><span style='color: green; font-size: 20px;'>●</span> <strong>Phytoplankton</strong> - Primary producers</p>
                <p><span style='color: cyan; font-size: 20px;'>●</span> <strong>Zooplankton</strong> - Small drifting organisms</p>
              </div>
            ")
          ),
          box(
            title = "Quick Start",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            HTML("
              <div style='padding: 10px;'>
                <ol>
                  <li><strong>Food Web Network:</strong> Explore the interactive network visualization</li>
                  <li><strong>Topological Metrics:</strong> View structural properties of the food web</li>
                  <li><strong>Biomass Analysis:</strong> Examine biomass-weighted metrics</li>
                  <li><strong>Energy Fluxes:</strong> Analyze energy flow patterns</li>
                </ol>
                <p style='margin-top: 15px;'><em>Click on the sidebar menu items to navigate!</em></p>
              </div>
            ")
          )
        )
      ),

      # ========================================================================
      # DATA IMPORT TAB
      # ========================================================================
      tabItem(
        tabName = "import",

        fluidRow(
          box(
            title = "Data Import & Format Guide",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            HTML("
              <h4>Import Your Own Food Web Data</h4>
              <p>EcoNeTool supports multiple data formats for food web analysis. You can upload your own data
              or use the default Gulf of Riga dataset (pre-loaded).</p>
            ")
          )
        ),

        fluidRow(
          box(
            title = "Supported File Formats",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            HTML("
              <h5>1. Excel Format (.xlsx, .xls)</h5>
              <p>Excel files should contain the following sheets:</p>

              <h6><strong>Sheet 1: Network (Adjacency Matrix)</strong></h6>
              <p>A square matrix where rows and columns represent species:</p>
              <table class='table table-sm table-bordered' style='width: auto; margin: 10px 0;'>
                <thead><tr><th></th><th>Species_A</th><th>Species_B</th><th>Species_C</th></tr></thead>
                <tbody>
                  <tr><td><strong>Species_A</strong></td><td>0</td><td>1</td><td>0</td></tr>
                  <tr><td><strong>Species_B</strong></td><td>0</td><td>0</td><td>1</td></tr>
                  <tr><td><strong>Species_C</strong></td><td>0</td><td>0</td><td>0</td></tr>
                </tbody>
              </table>
              <p><em>Value = 1 means Species A eats Species B (row → column)</em></p>

              <h6><strong>Sheet 2: Species_Info</strong></h6>
              <p>Species attributes (one row per species):</p>
              <table class='table table-sm table-bordered' style='width: auto; margin: 10px 0;'>
                <thead><tr><th>species</th><th>fg</th><th>meanB</th><th>losses</th><th>efficiencies</th></tr></thead>
                <tbody>
                  <tr><td>Species_A</td><td>Fish</td><td>1250.5</td><td>0.12</td><td>0.85</td></tr>
                  <tr><td>Species_B</td><td>Zooplankton</td><td>850.2</td><td>0.08</td><td>0.75</td></tr>
                  <tr><td>Species_C</td><td>Phytoplankton</td><td>2100.0</td><td>0.05</td><td>0.40</td></tr>
                </tbody>
              </table>

              <h5>Required Columns:</h5>
              <ul>
                <li><strong>species:</strong> Species name (must match network row/column names)</li>
                <li><strong>fg:</strong> Functional group (e.g., Fish, Benthos, Phytoplankton, Zooplankton, Detritus)</li>
                <li><strong>meanB:</strong> Mean biomass (g/km² or your preferred unit)</li>
                <li><strong>losses:</strong> Metabolic losses (J/sec) for flux calculations</li>
                <li><strong>efficiencies:</strong> Assimilation efficiencies (0-1) for flux calculations</li>
              </ul>

              <h5>Optional Columns:</h5>
              <ul>
                <li><strong>bodymasses:</strong> Average body mass (g)</li>
                <li><strong>taxon:</strong> Taxonomic classification</li>
                <li><strong>nbY:</strong> Number of years recorded</li>
              </ul>

              <hr>

              <h5>2. CSV Format (.csv)</h5>
              <p>Two CSV files required:</p>

              <h6><strong>File 1: network.csv (Adjacency Matrix)</strong></h6>
              <pre style='background: #f8f9fa; padding: 10px; border-radius: 5px;'>
species,Species_A,Species_B,Species_C
Species_A,0,1,0
Species_B,0,0,1
Species_C,0,0,0</pre>

              <h6><strong>File 2: species_info.csv</strong></h6>
              <pre style='background: #f8f9fa; padding: 10px; border-radius: 5px;'>
species,fg,meanB,losses,efficiencies
Species_A,Fish,1250.5,0.12,0.85
Species_B,Zooplankton,850.2,0.08,0.75
Species_C,Phytoplankton,2100.0,0.05,0.40</pre>

              <hr>

              <h5>3. RData Format (.Rdata, .rda)</h5>
              <p>R workspace containing two objects:</p>
              <ul>
                <li><strong>net:</strong> igraph object with food web network</li>
                <li><strong>info:</strong> data.frame with species information (columns as above)</li>
              </ul>

              <p><strong>Example R code to create:</strong></p>
              <pre style='background: #f8f9fa; padding: 10px; border-radius: 5px;'>
library(igraph)

# Create adjacency matrix
adj_matrix <- matrix(c(0,1,0, 0,0,1, 0,0,0), nrow=3, byrow=TRUE)
rownames(adj_matrix) <- colnames(adj_matrix) <- c('Species_A', 'Species_B', 'Species_C')

# Create network
net <- graph_from_adjacency_matrix(adj_matrix, mode='directed')

# Create species info
info <- data.frame(
  species = c('Species_A', 'Species_B', 'Species_C'),
  fg = factor(c('Fish', 'Zooplankton', 'Phytoplankton')),
  meanB = c(1250.5, 850.2, 2100.0),
  losses = c(0.12, 0.08, 0.05),
  efficiencies = c(0.85, 0.75, 0.40)
)

# Save
save(net, info, file='my_foodweb.Rdata')</pre>
            ")
          )
        ),

        fluidRow(
          box(
            title = "Upload Your Data",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            fileInput(
              "data_file",
              "Choose File (Excel, CSV, or RData)",
              accept = c(
                ".xlsx", ".xls",
                ".csv",
                ".Rdata", ".rda"
              ),
              multiple = FALSE
            ),
            helpText("Maximum file size: 10 MB"),
            br(),
            actionButton("load_data", "Load Data", icon = icon("upload"), class = "btn-primary"),
            hr(),
            verbatimTextOutput("data_upload_status")
          ),
          box(
            title = "Data Validation",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            HTML("
              <h5>Data Requirements:</h5>
              <ul>
                <li>✓ Species names must match between network and info</li>
                <li>✓ Network must be square (same row/column names)</li>
                <li>✓ Biomass values must be positive</li>
                <li>✓ Functional groups should be consistent</li>
                <li>✓ Losses and efficiencies: 0-1 range</li>
                <li>✓ At least 3 species recommended</li>
              </ul>

              <h5>After Upload:</h5>
              <p>Once your data is loaded, all analysis tabs (Network, Metrics, Fluxes) will
              automatically use your uploaded data instead of the default dataset.</p>

              <h5>Reset to Default:</h5>
              <p>Refresh the page to reload the default Gulf of Riga dataset.</p>
            ")
          )
        ),

        fluidRow(
          box(
            title = "Example Datasets",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            HTML("
              <h5>Download Example Templates</h5>
              <p>Use these templates as starting points for your own data:</p>
              <ul>
                <li><strong>Excel Template:</strong> Includes both sheets pre-formatted</li>
                <li><strong>CSV Template:</strong> Two files (network.csv + species_info.csv)</li>
                <li><strong>RData Template:</strong> R workspace with sample data</li>
              </ul>

              <h5>Sample Food Webs Available:</h5>
              <ol>
                <li><strong>Gulf of Riga (default):</strong> 34 species, 207 links, 1979-2016</li>
                <li><strong>Simple 3-Species:</strong> Basic predator-prey chain for testing</li>
                <li><strong>Caribbean Reef:</strong> Example tropical marine food web</li>
              </ol>

              <p><em>Contact the development team for access to example files.</em></p>
            ")
          )
        )
      ),

      # ========================================================================
      # FOOD WEB NETWORK TAB
      # ========================================================================
      tabItem(
        tabName = "network",

        fluidRow(
          box(
            title = "Interactive Food Web Network",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            maximizable = TRUE,
            visNetworkOutput("foodweb_visnet", height = "600px")
          )
        ),

        fluidRow(
          box(
            title = "Basal Species",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            icon = icon("seedling"),
            verbatimTextOutput("basal_species")
          ),
          box(
            title = "Top Predators",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            icon = icon("crown"),
            verbatimTextOutput("top_predators")
          )
        ),

        fluidRow(
          box(
            title = "Adjacency Matrix Heatmap",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            plotOutput("adjacency_heatmap", height = "600px")
          )
        )
      ),

      # ========================================================================
      # TOPOLOGICAL INDICATORS TAB
      # ========================================================================
      tabItem(
        tabName = "topological",

        fluidRow(
          box(
            title = "Topological Indicators (Qualitative Metrics)",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            HTML("
              <p>These metrics describe the structural properties of the food web network without
              considering node weights (biomass).</p>
              <ul>
                <li><strong>S:</strong> Species richness (number of taxa)</li>
                <li><strong>C:</strong> Connectance (proportion of realized links)</li>
                <li><strong>G:</strong> Generality (mean number of prey per predator)</li>
                <li><strong>V:</strong> Vulnerability (mean number of predators per prey)</li>
                <li><strong>ShortPath:</strong> Mean shortest path length</li>
                <li><strong>TL:</strong> Mean trophic level</li>
                <li><strong>Omni:</strong> Omnivory index (mean SD of prey trophic levels)</li>
              </ul>
            ")
          )
        ),

        fluidRow(
          box(
            title = "Calculated Metrics",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("topo_indicators")
          )
        )
      ),

      # ========================================================================
      # BIOMASS ANALYSIS TAB
      # ========================================================================
      tabItem(
        tabName = "biomass",

        fluidRow(
          box(
            title = "Biomass Distribution by Functional Group",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            plotOutput("biomass_boxplot", height = "400px")
          ),
          box(
            title = "Biomass Percentage by Functional Group",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            plotOutput("biomass_barplot", height = "400px")
          )
        ),

        fluidRow(
          box(
            title = "Food Web with Biomass-Scaled Nodes",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            maximizable = TRUE,
            plotOutput("foodweb_biomass_plot", height = "600px")
          )
        ),

        fluidRow(
          box(
            title = "Node-weighted Indicators (Quantitative Metrics)",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            HTML("
              <p>These metrics account for the relative importance of species based on their biomass.</p>
              <ul>
                <li><strong>nwC:</strong> Node-weighted connectance</li>
                <li><strong>nwG:</strong> Node-weighted generality</li>
                <li><strong>nwV:</strong> Node-weighted vulnerability</li>
                <li><strong>nwTL:</strong> Node-weighted mean trophic level</li>
              </ul>
            "),
            verbatimTextOutput("node_weighted_indicators")
          )
        )
      ),

      # ========================================================================
      # ENERGY FLUXES TAB
      # ========================================================================
      tabItem(
        tabName = "fluxes",

        fluidRow(
          box(
            title = "Energy Flux Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            HTML("
              <p>Energy fluxes are calculated using metabolic theory of ecology. Fluxes represent
              biomass flow between species based on allometric scaling and temperature-adjusted
              metabolic rates (T=3.5°C, Gulf of Riga spring conditions).</p>
              <p><strong>Units:</strong> kJ/day/km²</p>
            ")
          )
        ),

        fluidRow(
          box(
            title = "Flux Matrix Heatmap (Log-transformed)",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            maximizable = TRUE,
            plotOutput("flux_heatmap", height = "500px")
          ),
          box(
            title = "Flux-weighted Network",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            maximizable = TRUE,
            plotOutput("flux_network_plot", height = "500px")
          )
        ),

        fluidRow(
          box(
            title = "Link-weighted Flux Indicators",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            HTML("
              <p>Shannon diversity indices calculated from energy flux distributions.</p>
            "),
            verbatimTextOutput("flux_indicators")
          )
        )
      )
    )
  ),

  # ============================================================================
  # CONTROLBAR (optional - for additional info/settings)
  # ============================================================================
  controlbar = dashboardControlbar(
    skin = "light",
    pinned = FALSE,
    overlay = TRUE,
    controlbarMenu(
      id = "controlbar_menu",
      controlbarItem(
        title = "Information",
        HTML("
          <div style='padding: 15px;'>
            <h5>EcoNeTool</h5>
            <p><strong>Version:</strong> 2.1</p>
            <p><strong>License:</strong> GPL-3.0</p>

            <h5>About</h5>
            <p>Generic food web analysis tool supporting custom data import in multiple formats (Excel, CSV, RData).</p>

            <h5>Default Dataset</h5>
            <p><strong>Gulf of Riga Food Web</strong><br>
            Frelat, R., & Kortsch, S. (2020).<br>
            34 species, 207 links<br>
            Period: 1979-2016</p>

            <h5>Data Import</h5>
            <p>Upload your own food web data using the <strong>Data Import</strong> tab. Supported formats:</p>
            <ul style='font-size: 12px; margin-left: -15px;'>
              <li>Excel (.xlsx, .xls)</li>
              <li>CSV files</li>
              <li>RData (.Rdata, .rda)</li>
            </ul>

            <h5>Color Scheme</h5>
            <p><strong>Default Functional Groups:</strong><br>
            <span style='color: orange;'>●</span> Benthos<br>
            <span style='color: darkgrey;'>●</span> Detritus<br>
            <span style='color: blue;'>●</span> Fish<br>
            <span style='color: green;'>●</span> Phytoplankton<br>
            <span style='color: cyan;'>●</span> Zooplankton</p>

            <h5>References</h5>
            <p style='font-size: 11px;'>
            Williams & Martinez (2004). Limits to trophic levels. Proc. R. Soc. B.<br><br>
            Olivier et al. (2019). Temporal variability. Ecography.<br><br>
            Brown et al. (2004). Metabolic theory of ecology. Ecology.
            </p>
          </div>
        ")
      )
    )
  ),

  # Dashboard footer
  footer = dashboardFooter(
    left = tagList(
      "EcoNeTool - Food Web Explorer | ",
      tags$a(href = "https://github.com", "GitHub", target = "_blank")
    ),
    right = "Powered by bs4Dash & Shiny"
  ),

  # Dashboard options
  title = "EcoNeTool - Food Web Explorer",
  skin = "light",
  freshTheme = NULL,
  help = NULL,
  dark = NULL,
  scrollToTop = TRUE
)

# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {
  # Assign colors to functional groups
  info$colfg <- COLOR_SCHEME[as.numeric(info$fg)]

  # ============================================================================
  # DATA IMPORT HANDLER
  # ============================================================================

  # Output status message for data upload
  output$data_upload_status <- renderPrint({
    if (is.null(input$data_file)) {
      cat("No file uploaded yet.\n\n")
      cat("Current dataset: Gulf of Riga (default)\n")
      cat("  - 34 species\n")
      cat("  - 207 trophic links\n")
      cat("  - 5 functional groups\n")
    } else {
      cat("File selected:", input$data_file$name, "\n")
      cat("File size:", round(input$data_file$size / 1024, 2), "KB\n\n")
      cat("Click 'Load Data' button to import.\n")
    }
  })

  # Handle file upload when button clicked
  observeEvent(input$load_data, {
    req(input$data_file)

    tryCatch({
      file_path <- input$data_file$datapath
      file_ext <- tools::file_ext(input$data_file$name)

      # Update status
      output$data_upload_status <- renderPrint({
        cat("Processing file:", input$data_file$name, "\n")
        cat("Format:", toupper(file_ext), "\n\n")
        cat("Loading...")
      })

      # Load based on file type
      if (file_ext %in% c("Rdata", "rda")) {
        # Load RData file into separate environment
        # (to avoid overwriting app functions if RData contains them)
        env <- new.env()
        load(file_path, envir = env)

        # Validate required objects
        if (!exists("net", envir = env)) {
          stop("RData file must contain 'net' object (igraph network)")
        }
        if (!exists("info", envir = env)) {
          stop("RData file must contain 'info' data frame")
        }

        # Extract only data objects (not functions) from loaded environment
        # This ensures we use the app's function definitions, not ones from the RData file
        net <<- env$net
        info <<- env$info

        # Upgrade igraph if needed
        net <<- igraph::upgrade_graph(net)

        # Assign colors
        info$colfg <<- COLOR_SCHEME[as.numeric(info$fg)]

        output$data_upload_status <- renderPrint({
          cat("✓ SUCCESS: Data loaded!\n\n")
          cat("Network: ", vcount(net), "species,", ecount(net), "links\n")
          cat("Species info:", nrow(info), "rows\n")
          cat("\nAll analysis tabs now use your uploaded data.\n")
          cat("Navigate to other tabs to explore.\n")
        })

      } else if (file_ext %in% c("xlsx", "xls")) {
        # Excel file - require readxl package
        if (!requireNamespace("readxl", quietly = TRUE)) {
          stop("Package 'readxl' required for Excel files.\nInstall with: install.packages('readxl')")
        }

        output$data_upload_status <- renderPrint({
          cat("✗ ERROR: Excel import not yet implemented.\n\n")
          cat("For now, please use:\n")
          cat("  - RData format (.Rdata), or\n")
          cat("  - Convert your Excel file to CSV\n\n")
          cat("Excel import coming in next version!\n")
        })

      } else if (file_ext == "csv") {
        output$data_upload_status <- renderPrint({
          cat("✗ ERROR: CSV import not yet implemented.\n\n")
          cat("For now, please use RData format (.Rdata)\n\n")
          cat("CSV import coming in next version!\n")
        })

      } else {
        stop("Unsupported file format")
      }

    }, error = function(e) {
      output$data_upload_status <- renderPrint({
        cat("✗ ERROR loading data:\n\n")
        cat(e$message, "\n\n")
        cat("Please check:\n")
        cat("  - File format is correct\n")
        cat("  - Required objects/sheets are present\n")
        cat("  - Data matches expected structure\n")
      })
    })
  })

  # ============================================================================
  # VISUALIZATION OUTPUTS
  # ============================================================================

  # Food Web Visualization (visNetwork)
  output$foodweb_visnet <- renderVisNetwork({
    tryCatch({
      # Calculate trophic levels for hierarchical layout
      tl <- trophiclevels(net)

      # Prepare nodes data frame
      # Set Y positions based on trophic level (higher TL = higher Y position)
      # This guides the initial layout but physics will still apply
      y_pos <- (max(tl) - tl) * -200  # Negative Y values put higher TL at top

      nodes <- data.frame(
        id = 1:vcount(net),
        label = V(net)$name,
        group = as.character(info$fg),
        color = info$colfg,
        value = info$meanB,
        y = y_pos,  # Initial Y position based on trophic level
        physics = TRUE,  # Allow physics to move nodes
        shape = "dot",  # Circular nodes
        title = paste0("<b>", V(net)$name, "</b><br>",
                      "Functional Group: ", info$fg, "<br>",
                      "Trophic Level: ", round(tl, 2), "<br>",
                      "Biomass: ", round(info$meanB, 2)),
        stringsAsFactors = FALSE
      )

      # Map node names to IDs for edges
      name_to_id <- setNames(nodes$id, nodes$label)
      edgelist <- as.data.frame(as_edgelist(net))
      colnames(edgelist) <- c("from", "to")
      edgelist$from <- name_to_id[edgelist$from]
      edgelist$to <- name_to_id[edgelist$to]

      # Create network visualization with gravity-based physics
      vis <- visNetwork(nodes, edgelist, width = "100%", height = "600px") %>%
        visEdges(arrows = "to", smooth = list(type = "curvedCW", roundness = 0.2)) %>%
        visNodes(
          shape = "dot",
          size = 15,
          font = list(size = 12)
        ) %>%
        visOptions(
          highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
          nodesIdSelection = TRUE
        ) %>%
        visInteraction(
          navigationButtons = TRUE,
          keyboard = TRUE
        )

      # Configure each functional group with dot shape for legend
      fg_levels <- levels(info$fg)
      for (i in seq_along(fg_levels)) {
        vis <- vis %>%
          visGroups(groupname = fg_levels[i],
                   shape = "dot",
                   color = COLOR_SCHEME[i])
      }

      # Add gravity-based physics layout organized by trophic level
      vis <- vis %>%
        visPhysics(
          enabled = TRUE,
          solver = "barnesHut",
          barnesHut = list(
            gravitationalConstant = -2000,
            centralGravity = 0.1,
            springLength = 200,
            springConstant = 0.05,
            damping = 0.2,
            avoidOverlap = 0.5
          ),
          stabilization = list(
            enabled = TRUE,
            iterations = 2000,
            fit = TRUE
          )
        ) %>%
        visLegend(
          useGroups = TRUE,
          width = 0.2,
          position = "right",
          main = list(text = "Functional Groups", style = "font-size:14px;font-weight:bold;")
        )

      vis
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
