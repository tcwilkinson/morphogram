#' normalize
#'
#' NOT YET IMPLEMENTED
#' This function normalizes the scale of sf features, normally polygons. By default the scaled parameter
#' is the polygon *area*, so that each feature has an area of `scale` m2, whatever the actual shape of the polygon.
#'
#' Features should have first been converged into a single comparable scale, using the `converge` function.
#'
#' @param x An sf object, already processed with `converge`, to be transformed to normalized scale; REQUIRED.
#' @param target Target value to be normalized to, usually a single numeric, but can also be a vector of numeric target values of equal length to number of features in x; value meaning depends on method, e.g. square metres for areal, or metres for linear methods; default=1.
#' @param method Method of scaling: "area", "width", "height", "maxlinear"; default="area".
#' @keywords cartogram, sf, infographic
#' @seealso \code{\link{converge}}, \code{\link{align}}, \code{\link{normalize}}, \code{\link{scale}}, \code{\link{distribute}}
#' @return An sf object containing one or more features (with no defined CRS)
#' @example
#' sf_layer <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' sf_layer <- normalize(sf_layer, method="top")
#' @export
normalize <- function(x, target=1, method="area") {
  stop("normalize() is not yet implemented!")
  return(x)
}
