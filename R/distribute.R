#' distribute
#'
#' This function spatially distributes a set of sf features based on their dimensions, and orders them
#' according to defined features.
#'
#' The spatial features should be centred around 0,0. Use `normalizeAroundCentroid` to group different
#' sets of spatial features around 0,0.
#'
#' @param x An sf-compatible feature layer, often containing polygons whose size is to be visually compared; REQUIRED.
#' @param preserve.parameters Whether to preserve non-geometry parameters; default T.
#' @param type Method used to distribute features; default and only functioning method is "regulargrid".
#' @param dir Direction of overall diagram, "v" impiies filling each row first, "h" implies filling each column first; default "v".
#' @param max.features Maximum features to compare; default=200.
#' @param cols Number of columns of features to plot before moving onto next line if dir="v".
#' @param rows Number of rows of features to plot before moving onto next line if dir="h".
#' @param margin Scalar coefficient of spacing between features; default 1.2.
#' @param x.mar Scalar coefficient of spacing between x values; default 1.
#' @param y.mar Scalar coefficient of spacing between x values; default 1.
#' @param scale Affine scaling of feature dimensions; default 1.
#' @seealso \code{\link{normalizeAroundCentroid}}
#' @return An sf object containing one or more features (with no defined CRS)
#' @example
#' sf_layer <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' sf_layer <- normalizeAroundCentroid(sf_layer)
#' distribute(sf_layer)
#' distribute(sf_layer,margin=1.5)
#' @export
distribute <- function(x, preserve.parameters=T,
                       type="regulargrid", cols=NULL, rows=NULL,
                       dir="v", max.features=200,
                       margin=1.2, x.mar=1, y.mar=1,
                       scale=1
                       ) {
  type <- "regulargrid" # FORCE regular.grid as this is the only supported method type

  if (nrow(x)<1) {
    warning("No features found in supplied sf object, returning empty sf object")
    return(x)
  }
  if (max.features>200) {
    warning("Friendly advice: distributing too many features may make it difficult to perceive size differences")
  }
  if (max.features<nrow(x)) {
    warning(paste0("There are more features than the value of max.features; only the first ",
                   max.features," features will be distributed"))
    x <- x[1:max.features,]
  }
  if (max.features>nrow(x)) { max.features = nrow(x) }
  # First find sizes of objects and maximal bounding boxes PER FEATURE
  bbox <- list()
  for (i in 1:max.features) bbox[[i]] <- sf::st_bbox(x[i,])
  bbox <- as.data.frame(do.call(rbind,bbox))
  bbox$diff_x <- bbox$xmax - bbox$xmin
  bbox$diff_y <- bbox$ymax - bbox$ymin
  max_x <- max(bbox$diff_x)
  max_y <- max(bbox$diff_y)
  min_x <- min(bbox$diff_x)
  min_y <- min(bbox$diff_y)
  ratio_x <- (max_x/min_x)
  ratio_y <- (max_y/min_y)
  if ((ratio_x > 100) || (ratio_y > 100)) {
    warning("The ratio of largest to smallest objects is very high, the resulting visualisation may be difficult to comprehend")
  }

  ## regulargrid - Calculate centre point to translate objects to, based on a regular grid using maximal extent of bounding boxes
  if(type=="regulargrid") {
    d <- NULL
    if (is.null(cols)) { cols = ceiling(sqrt(max.features)) }
    cols <- as.numeric(cols)
    if (cols>max.features) { cols = max.features }
    if (is.null(rows)) { rows = ceiling(sqrt(max.features)) }
    rows <- as.numeric(rows)
    if (rows>max.features) { rows = max.features }

    x_multiplier <- (max_x/2) * (x.mar * margin) # left to right
    y_multiplier <- (max_y/2) * -(y.mar * margin) # top to bottom
    d <- list()
    for (i in seq(1:max.features)) {
      if (dir=="v") {
        pos_y <- ceiling(i/cols)
        pos_x <- i %% cols
      } else if(dir=="h") {
        pos_x <- ceiling(i/rows)
        pos_y <- i %% rows
      }
      affine_transform <- c(x_multiplier*pos_x , y_multiplier*pos_y)
      if(isTRUE(preserve.parameters)) {
        d[[i]] <- sf::st_sf(
          data.frame( sf::st_drop_geometry(x[i,]),
                      geom=sf::st_sf( (sf::st_geometry(x[i,]) * scale) + affine_transform) )
          )
      } else {
        d[[i]] <- sf::st_sf((sf::st_geometry(x[i,]) * scale) + affine_transform)
      }
    }
    d <- sf::st_sf(do.call(rbind,d))
  }

  return(d)
}
