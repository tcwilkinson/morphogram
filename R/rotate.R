#' rotate
#'
#' This function rotates simple features spatially by centroid or origin.
#'
#' @param x An sf object, to be transformed in alignment; REQUIRED.
#' @param angle Angle in degrees; default=0.
#' @param origin Whether to use centroid or coordinates origin; default="centroid".
#' @keywords sf, rotation
#' @seealso \code{\link{converge}}, \code{\link{align}}, \code{\link{normalize}}, \code{\link{scale}}, \code{\link{distribute}}
#' @return An sf object containing one or more features
#' @example
#' sf_layer <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' sf_layer <- rotate(sf_layer, angle=45)
#' @export
rotate <- function(x, angle=c(0), origin="centroid") {
  if (!(origin=="origin" | origin=="centroid")) {
    stop("Warning: unknown origin value '",origin,"'")
  }

  if (nrow(x)>0) {
    if (length(angle)==1 | length(angle)==nrow(x)) {
      # Angle
      if (length(angle)==1) { angle <- rep(angle,nrow(x)) }

      d <- list()
      for (i in seq(1:nrow(x))) {
        # Rotation should be done before other translation to ensure it is done around origin
        rotate_fun = function(a){
          r = a * pi / 180 #degrees to radians
          matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
        }
        s1 <- sf::st_geometry(x[i,])
        dt <- sf::st_drop_geometry(x[i,])
        if (origin=="centroid") {
          cn = sf::st_centroid(s1)
          s2 = (s1-cn) * rotate_fun(angle[i]) + cn
        } else if(origin=="origin") {
          s2 = s1 * rotate_fun(angle[i])
        } else {
          s2 = s1
        }

        # Add to list
        d[[i]] <- sf::st_sf(data.frame(dt,geom=sf::st_sf(s2)))

      }
      # Combine all items into a single sf frame again
      d <- sf::st_sf(do.call(rbind,d))

      return(d)

    } else {
      stop("angle must be either a single numerical value in degrees to be applied to all features, or a vector of numerical values the same length as the number of rows in x")
    }
  } else {
    # Warning
    warning("No features provided to rotate, returning the identical empty object")
    return(x)
  }

}
