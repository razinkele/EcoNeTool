# plotfw.R
# Custom plotfw function for food web visualization

#' Plot a food web network
#'
#' A custom plotting function for food web visualization using igraph's plotting
#' capabilities. Allows customization of node colors, sizes, and edge properties.
#'
#' @param net An igraph object representing the food web network
#' @param col Character vector of node colors. If NULL, defaults to "lightblue".
#'        Should have length equal to number of nodes.
#' @param size Numeric vector or scalar for node sizes. If scalar, all nodes
#'        have the same size. Default is 10.
#' @param edge.width Numeric scalar or vector for edge widths. Default is 1.
#' @param edge.arrow.size Numeric scalar for arrow size. Default is 0.5.
#' @param ... Additional arguments passed to igraph::plot.igraph
#'
#' @return NULL (creates a plot)
#'
#' @details
#' This function is a wrapper around igraph's plot function with sensible
#' defaults for food web visualization. Vertex label size is fixed at 0.7
#' for readability.
#'
#' @examples
#' # Plot with default settings
#' plotfw(net)
#'
#' # Plot with custom colors and sizes
#' plotfw(net, col=c("red", "blue", "green"), size=c(10, 15, 20))
#'
#' @seealso \code{\link[igraph]{plot.igraph}}
plotfw <- function(net, col = NULL, size = 10, edge.width = 1, edge.arrow.size = 0.5, ...) {
  # Input validation
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for plotfw function")
  }

  if (!igraph::is_igraph(net)) {
    stop("Input 'net' must be an igraph object")
  }

  n <- igraph::vcount(net)
  if (n == 0) {
    warning("Network contains no vertices. Nothing to plot.")
    return(invisible(NULL))
  }

  # Set default color
  if (is.null(col)) {
    col <- "lightblue"
  }

  # Validate color vector length
  if (length(col) != 1 && length(col) != n) {
    warning(paste("Color vector length (", length(col),
                  ") does not match number of vertices (", n,
                  "). Using first color for all nodes.", sep=""))
    col <- rep(col[1], n)
  }

  # Expand size to vector if scalar
  if (length(size) == 1) {
    size <- rep(size, n)
  } else if (length(size) != n) {
    warning(paste("Size vector length (", length(size),
                  ") does not match number of vertices (", n,
                  "). Using first size for all nodes.", sep=""))
    size <- rep(size[1], n)
  }

  # Validate numeric parameters
  if (!is.numeric(size) || any(size < 0, na.rm = TRUE)) {
    stop("Node sizes must be non-negative numeric values")
  }
  if (!is.numeric(edge.width) || any(edge.width < 0, na.rm = TRUE)) {
    stop("Edge widths must be non-negative numeric values")
  }
  if (!is.numeric(edge.arrow.size) || edge.arrow.size < 0) {
    stop("Edge arrow size must be a non-negative numeric value")
  }

  # Create the plot
  tryCatch({
    plot(
      net,
      vertex.color = col,
      vertex.size = size,
      edge.width = edge.width,
      edge.arrow.size = edge.arrow.size,
      vertex.label.cex = 0.7,
      ...
    )
  }, error = function(e) {
    stop(paste("Error creating plot:", e$message))
  })

  invisible(NULL)
}
