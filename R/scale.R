#' scale
#'
#' NOT YET IMPLEMENTED
#' This function scales the size of sf features, by either linear or areal amounts.
#'
#' Features should have first been converged into a single comparable scale, using
#' the `converge` function, and usually normalized in area or linear dimensions using
#' `normalize`.
#'
#' @param x An sf object, already processed with `converge`, to be transformed in scale; REQUIRED.
#' @param scale Scaling factor to be used, either a single numeric or a vector of numeric scales of equal length to number of features in x; default=1.
#' @param method Method of scaling: "area", "width", "height", "maxlinear"; default="area".
#' @keywords cartogram, sf, infographic
#' @seealso \code{\link{converge}}, \code{\link{align}}, \code{\link{normalize}},  \code{\link{distribute}}
#' @return An sf object containing one or more features (with no defined CRS)
#' @example
#' sf_layer <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' sf_layer <- normalize(sf_layer, method="top")
#' @export
scale <- function(x, scale=1, method="area") {
  warning("Not yet implemented!")
  return(x)
}
