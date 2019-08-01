#' plot_morphogram
#'
#' This function is a convenience plotting command using a sensible sequence of converge, distribute.
#'
#' @param x An sf object, ready to be converged and distributed; REQUIRED.
#' @param ... Any arguments to be passed to sf::plot.sf
#' @keywords cartogram, sf, infographic
#' @seealso \code{\link{converge}}, \code{\link{align}}, \code{\link{normalize}},  \code{\link{distribute}}
#' @return A sf-based plot
#' @example
#' sf_layer <- sf::st_read(system.file("shape/nc.shp", package="sf"),quiet=T)
#' plot_morphogram(sf_layer)
#' @export
plot_morphogram <- function(x, ...) {
  sf::plot_sf(distribute(converge(x)), ...)
}
