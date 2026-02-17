#' Food Web Network Tab UI
#'
#' Creates the network visualization tab.
#'
#' @return A tabItem for network visualization
network_ui <- function() {
      # FOOD WEB NETWORK TAB
      # ========================================================================
      tabItem(
        tabName = "network",

        fluidRow(
          tabBox(
            width = 12,
            id = "network_tabs",

            # TAB 1: Interactive Network
            tabPanel(
              title = "Interactive Network",
              icon = icon("project-diagram"),

              br(),
              box(
                title = "Interactive Food Web Network",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                maximizable = TRUE,
                visNetworkOutput("foodweb_visnet", height = "600px")
              ),

              fluidRow(
                column(6,
                  box(
                    title = "Basal Species",
                    status = "success",
                    solidHeader = TRUE,
                    width = 12,
                    icon = icon("seedling"),
                    verbatimTextOutput("basal_species")
                  )
                ),
                column(6,
                  box(
                    title = "Top Predators",
                    status = "danger",
                    solidHeader = TRUE,
                    width = 12,
                    icon = icon("crown"),
                    verbatimTextOutput("top_predators")
                  )
                )
              )
            ),

            # TAB 2: Adjacency Matrix
            tabPanel(
              title = "Adjacency Matrix",
              icon = icon("th"),

              br(),
              box(
                title = "Adjacency Matrix Heatmap",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                maximizable = TRUE,
                plotOutput("adjacency_heatmap", height = "600px")
              )
            )
          )
        )
      )

      # ========================================================================
}
