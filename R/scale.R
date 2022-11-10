#' st_scale
#'
#' NOT YET IMPLEMENTED
#' This function scales the size of sf features around coordinate origin or centroid, by either linear or areal amounts.
#'
#' @param x An sf object, to be transformed in scale; REQUIRED.
#' @param scale Scaling factor to be used, either a single numeric or a vector of numeric scales of equal length to number of features in x; superceded by xscale or yscale; default=1.
#' @param xscale X-axis scaling factor to be used, either a single numeric or a vector of numeric scales of equal length to number of features in x; default=scale.
#' @param yscale Y-axis scaling factor to be used, either a single numeric or a vector of numeric scales of equal length to number of features in x; default=scale.
#' @param method Method of scaling: "linear", "area"; default="linear".
#' @keywords cartogram, sf, infographic
#' @seealso \code{\link{converge}}, \code{\link{align}}, \code{\link{normalize}},  \code{\link{distribute}}
#' @return An sf object containing one or more features
#' @example
#' sf_layer <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' sf_layer <- normalize(sf_layer, method="top")
#' @export
st_scale <- function(x, scale=1, xscale=scale, yscale=scale, method="linear", origin="centroid") {
  warning("Not yet implemented!")
  return(x)
}
