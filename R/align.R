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
#' @param method Method used to align "top", "bottom", "left", "right";
#' "vcentroid", "hcentroid", "centroid"; default="centroid".
#' @keywords cartogram, sf, infographic
#' @seealso \code{\link{converge}}, \code{\link{normalize}}, \code{\link{scale}}, \code{\link{distribute}}
#' @return An sf object containing one or more features (with no defined CRS)
#' @example
#' sf_layer <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' sf_layer <- align(sf_layer, method="top")
#' @export
align <- function(x, method="centroid") {
  if (nrow(x)>0) {
    # find maximum bounding box
    #bbox <- st_bbox(st_union(x))
    d <- list()
    for (i in seq(1:nrow(x))) {
      s1 <- sf::st_geometry(x[i,])
      dt <- sf::st_drop_geometry(x[i,])
      cn = sf::st_coordinates(sf::st_geometry(sf::st_centroid(s1)))
      bb = sf::st_bbox(s1)
      if(method=="centroid") { affine_transform <- -cn }
      if(method=="vcentroid") { affine_transform <- c(0,-cn[2]) }
      if(method=="hcentroid") { affine_transform <- c(-cn[1],0) }
      if(method=="left") { affine_transform <- c(-as.numeric(bb$xmin),0) }
      if(method=="right") { affine_transform <- c(-as.numeric(bb$xmax),0) }
      if(method=="top") { affine_transform <- c(0,-as.numeric(bb$ymax)) }
      if(method=="bottom") { affine_transform <- c(0,-as.numeric(bb$ymin)) }
      #message(affine_transform)
      d[[i]] <- sf::st_sf(data.frame( dt, geom=sf::st_sf( s1 + affine_transform )) )
    }
    d <- sf::st_sf(do.call(rbind,d))
    return(d)
  } else {
    message("Warning: no spatial features supplied, nothing to align")
    return(x)
  }

}
