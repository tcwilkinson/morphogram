#' align
#'
#' NOT YET IMPLEMENTED
#' This function aligns simple features spatially by bounding box or centroid values.
#' Bounding box alignment or justification types include: top, bottom, left, right, vcentre, hcentre.
#' Centroid alignment include: vcentroid, hcentroid, centroid
#'
#' To visually distribute the resulting features, use the `distribute` function.
#'
#' @param x An sf object, already processed with `converge`, to be transformed in alignment; REQUIRED.
#' @param method Method used to align "top", "bottom", "left", "right", "vcentre", "hcentre";
#' "vcentroid", "hcentroid", "centroid"; default="centroid".
#' @keywords cartogram, sf, infographic
#' @seealso \code{\link{converge}}, \code{\link{normalize}}, \code{\link{scale}}, \code{\link{distribute}}
#' @return An sf object containing one or more features (with no defined CRS)
#' @example
#' sf_layer <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' sf_layer <- align(sf_layer, method="top")
#' @export
align <- function(x, method="centroid") {
  warning("Not yet implemented!")
  return(x)
}
