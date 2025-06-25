#' Set Up a Multi-Frame Plot Layout
#'
#' This function sets up a multi-frame plotting layout using `par(mfrow = c(x, y))`,
#' allowing multiple plots to be displayed in a grid format.
#'
#' @param x An integer specifying the number of rows in the plot layout.
#' @param y An integer specifying the number of columns in the plot layout.
#'
#' @return No return value. This function modifies the graphical parameters temporarily.
#'
#' @details
#' This function changes the `mfrow` graphical parameter using `par()`, enabling multiple plots
#' to be displayed in a grid layout within the same plotting window. The original plotting
#' parameters are automatically restored when the function exits.
#'
#' @seealso [im.ggplot()], [im.import()]
#'
#' @examples
#' # Set up a 2x2 plotting layout
#' im.multiframe(2, 2)
#'
#' # Example plots
#' plot(1:10, rnorm(10))
#' plot(1:10, runif(10))
#' plot(1:10, rpois(10, lambda = 5))
#' plot(1:10, rbeta(10, shape1 = 2, shape2 = 5))
#'
#' # Layout is automatically restored after im.multiframe() exits
#'
#' @export
im.multiframe <- function(x, y) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow = c(x, y))
}
