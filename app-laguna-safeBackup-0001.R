# Custom function: trophiclevels
trophiclevels <- function(net) {
  # Basal species: nodes with no incoming edges
  basal <- which(degree(net, mode = "in") == 0)
  S <- vcount(net)
  tl <- rep(NA, S)
  for (i in 1:S) {
    if (i %in% basal) {
      tl[i] <- 1
    } else {
      # Find all shortest paths from i to all basal nodes
      sp <- shortest.paths(net, v = i, to = basal)
      # Remove infinite paths (no path to basal)
      sp <- sp[is.finite(sp)]
      if (length(sp) > 0) {
        tl[i] <- mean(sp) + 1
      } else {
        tl[i] <- NA
      }
    }
  }
  names(tl) <- V(net)$name
  return(tl)
}

# Custom function: plotfw
plotfw <- function(net, col = "lightblue", size = 10, edge.width = 1, edge.arrow.size = 0.5, ...) {
  plot(
    net,
    vertex.color = col,
    vertex.size = size,
    edge.width = edge.width,
    edge.arrow.size = edge.arrow.size,
    vertex.label.cex = 0.7,
    vertex.label.color = "black",
    vertex.frame.color = NA,
    layout = layout_with_fr,
    ...
  )
}

# Custom function: fluxind
fluxind <- function(fluxes) {
  # Link-weighted connectance, generality, vulnerability
  S <- nrow(fluxes)
  L <- sum(fluxes > 0)
  Cw <- L / (S * (S - 1))
  # Generality: mean number of prey per consumer, weighted by inflow
  inflow <- colSums(fluxes)
  prey_counts <- colSums(fluxes > 0)
  Gw <- sum(prey_counts * inflow) / sum(inflow)
  # Vulnerability: mean number of consumers per prey, weighted by outflow
  outflow <- rowSums(fluxes)
  cons_counts <- rowSums(fluxes > 0)
  Vw <- sum(cons_counts * outflow) / sum(outflow)
  return(list(Cw = Cw, Gw = Gw, Vw = Vw))
}
# app.R
# Shiny app for Baltic Food Web analysis
library(shiny)
library(igraph)
library(fluxweb)

# Load data and prepare variables (modularize Script.R logic)
load("BalticFW.Rdata")

# Helper functions for indicators (from Script.R)
get_topological_indicators <- function(net) {
  S <- vcount(net)
  C <- ecount(net)/(S*(S-1))
  pred <- degree(net, mode="in")>0
  G <- sum(degree(net, mode="in")[pred])/sum(pred)
  prey <- degree(net, mode="out")>0
  V <- sum(degree(net, mode="out")[prey])/sum(prey)
  sp <- shortest.paths(net)
  ShortPath <- mean(sp[upper.tri(sp)])
  tlnodes <- trophiclevels(net)
  TL <- mean(tlnodes)
  netmatrix <- get.adjacency(net, sparse=F)
  webtl <- netmatrix*tlnodes
  webtl[webtl==0] <- NA
  omninodes <- apply(webtl,2,sd, na.rm=TRUE)
  Omni <- mean(omninodes, na.rm=TRUE)
  list(S=S, C=C, G=G, V=V, ShortPath=ShortPath, TL=TL, Omni=Omni)
}

get_node_weighted_indicators <- function(net, info) {
  biomass <- info$meanB
  tlnodes <- trophiclevels(net)
  nwC <- sum(degree(net)*biomass)/(2*sum(biomass)*(vcount(net)-1))
  pred <- degree(net, mode="in")>0
  nwG <- sum((degree(net, mode="in")*biomass)[pred])/(sum(biomass[pred]))
  prey <- degree(net, mode="out")>0
  nwV <- sum((degree(net, mode="out")*biomass)[prey])/(sum(biomass[prey]))
  nwTL <- sum(tlnodes*biomass)/sum(biomass)
  list(nwC=nwC, nwG=nwG, nwV=nwV, nwTL=nwTL)
}

get_fluxweb_results <- function(net, info) {
  netmatrix <- get.adjacency(net, sparse=F)
  biomass <- info$meanB
  fluxes <- fluxing(netmatrix, biomass, info$losses, info$efficiencies, ef.level="prey")
  fluxes <- fluxes*86.4
  netLW <- graph_from_adjacency_matrix(fluxes, weighted=TRUE)
  list(fluxes=fluxes, netLW=netLW)
}

# UI
ui <- fluidPage(
  titlePanel("Baltic Food Web Explorer"),
  sidebarLayout(
    sidebarPanel(
      helpText("Explore the Gulf of Riga food web. Data: Frelat & Kortsch, 2020."),
      selectInput("tab", "Section:",
                  choices=c("Food Web Visualization", "Topological Indicators", "Node-weighted Indicators", "Fluxweb Analysis"),
                  selected="Food Web Visualization")
    ),
    mainPanel(
      uiOutput("main_ui")
    )
  )
)

# Server
server <- function(input, output, session) {
  colFG <- c("orange", "darkgrey", "blue", "green", "cyan")
  info$colfg <- colFG[as.numeric(info$fg)]

  output$main_ui <- renderUI({
    if (input$tab == "Food Web Visualization") {
      tagList(
        plotOutput("foodweb_plot"),
        verbatimTextOutput("basal_species"),
        verbatimTextOutput("top_predators"),
        plotOutput("adjacency_heatmap")
      )
    } else if (input$tab == "Topological Indicators") {
      verbatimTextOutput("topo_indicators")
    } else if (input$tab == "Node-weighted Indicators") {
      tagList(
        plotOutput("biomass_boxplot"),
        plotOutput("biomass_barplot"),
        plotOutput("foodweb_biomass_plot"),
        verbatimTextOutput("node_weighted_indicators")
      )
    } else if (input$tab == "Fluxweb Analysis") {
      tagList(
        plotOutput("flux_heatmap"),
        plotOutput("flux_network_plot"),
        verbatimTextOutput("flux_indicators")
      )
    }
  })

  # Food Web Visualization
  output$foodweb_plot <- renderPlot({
    plotfw(net, col=info$colfg, edge.width=0.3, edge.arrow.size=0.3)
  })
  output$basal_species <- renderPrint({
    cat("Basal species:\n", paste(V(net)$name[degree(net, mode="in")==0], collapse=", "))
  })
  output$top_predators <- renderPrint({
    cat("Top predators:\n", paste(V(net)$name[degree(net, mode="out")==0], collapse=", "))
  })
  output$adjacency_heatmap <- renderPlot({
    netmatrix <- get.adjacency(net, sparse=F)
    heatmap(netmatrix, Rowv=NA, Colv=NA, scale="none")
  })

  # Topological Indicators
  output$topo_indicators <- renderPrint({
    ind <- get_topological_indicators(net)
    print(ind)
  })

  # Node-weighted Indicators
  output$biomass_boxplot <- renderPlot({
    colFG <- c("orange", "darkgrey", "blue", "green", "cyan")
    boxplot(info$meanB~info$fg, las=2, col=colFG,
            ylab="Biomass (g/day/km2)", xlab="")
  })
  output$biomass_barplot <- renderPlot({
    colFG <- c("orange", "darkgrey", "blue", "green", "cyan")
    percB <- tapply(info$meanB, info$fg, sum)/sum(info$meanB)*100
    barplot(as.matrix(percB), col=colFG, ylab="%")
  })
  output$foodweb_biomass_plot <- renderPlot({
    Vscale <- 25; Vmin <- 4
    nodmax <- max(info$meanB)
    sizeB <- (info$meanB/nodmax)*Vscale + Vmin
    plotfw(net, col=info$colfg, size=sizeB, edge.width=0.3, edge.arrow.size=0.3)
  })
  output$node_weighted_indicators <- renderPrint({
    ind <- get_node_weighted_indicators(net, info)
    print(ind)
  })

  # Fluxweb Analysis
  output$flux_heatmap <- renderPlot({
    res <- get_fluxweb_results(net, info)
    heatmap(log(res$fluxes+0.00001), Rowv=NA, Colv=NA, scale="none")
  })
  output$flux_network_plot <- renderPlot({
    res <- get_fluxweb_results(net, info)
    colFG <- c("orange", "darkgrey", "blue", "green", "cyan")
    info$colfg <- colFG[as.numeric(info$fg)]
    Escale <- 15; Emin <- 0.1
    wid <- Emin+(E(res$netLW)$weight/max(E(res$netLW)$weight)*Escale)
    V(res$netLW)$frame.color=NA
    plotfw(res$netLW, col=info$colfg, edge.width=wid, edge.arrow.size=0.05)
  })
  output$flux_indicators <- renderPrint({
    res <- get_fluxweb_results(net, info)
    print(fluxind(res$fluxes))
  })
}

shinyApp(ui, server)
