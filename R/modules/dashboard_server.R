#' Dashboard Server Logic
#'
#' Renders dynamic value boxes for species count, links, groups, location, period.
#' Uses flat input/output pattern (no moduleServer/NS) to avoid breaking UI references.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param net_reactive reactiveVal for igraph network
#' @param info_reactive reactiveVal for species info
#' @param metaweb_metadata reactiveVal for metadata
#' @param dashboard_trigger reactiveVal for update trigger

dashboard_server <- function(input, output, session, net_reactive, info_reactive,
                             metaweb_metadata, dashboard_trigger) {

  output$box_species <- renderValueBox({
    # Trigger update
    dashboard_trigger()

    current_net <- net_reactive()
    valueBox(
      value = vcount(current_net),
      subtitle = "Taxa / Species",
      icon = icon("fish"),
      color = "primary"
    )
  })

  output$box_links <- renderValueBox({
    # Trigger update
    dashboard_trigger()

    current_net <- net_reactive()
    valueBox(
      value = ecount(current_net),
      subtitle = "Trophic Links",
      icon = icon("link"),
      color = "success"
    )
  })

  output$box_groups <- renderValueBox({
    # Trigger update
    dashboard_trigger()

    current_info <- info_reactive()
    valueBox(
      value = nlevels(current_info$fg),
      subtitle = "Functional Groups",
      icon = icon("layer-group"),
      color = "info"
    )
  })

  output$box_period <- renderValueBox({
    # Trigger update
    dashboard_trigger()

    metadata <- metaweb_metadata()
    valueBox(
      value = metadata$time_period,
      subtitle = "Time Period",
      icon = icon("calendar"),
      color = "warning"
    )
  })

  output$box_location <- renderValueBox({
    # Trigger update
    dashboard_trigger()

    metadata <- metaweb_metadata()
    valueBox(
      value = metadata$location,
      subtitle = "Location",
      icon = icon("map-marker-alt"),
      color = "danger"
    )
  })
}
