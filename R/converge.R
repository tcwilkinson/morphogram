#' converge
#'
#' This function finds the centroid of a set of sf-based features and reprojects them so that each feature's
#' centroid lies at the centre (0,0) of an arbitrary metre-based Mercator projection. The resulting features
#' are thus projected overlapping at the same scale,. Note that the resulting sf object has no CRS.
#'
#' To visually distribute the resulting features, use the `distribute` function.
#'
#' @param x An sf-compatible feature layer, often containing polygons whose size is to be visually compared; REQUIRED.
#' @param z An sf-compatible feature layer, the target polygons by which x will be converged
#' @param by.feature Whether to reproject by single features by individual feature centroids, T,
#' or reproject all features by a single centroid of the union of all features, F; default=TRUE.
#' @param combine Combine multiple geometries into one, using st_combine; default=F.
#' @keywords cartogram, sf, infographic
#' @seealso \code{\link{distribute}}
#' @return An sf object containing one or more features (with no defined CRS)
#' @example
#' sf_layer <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' sf_layer <- converge(sf_layer)
#' converge(sf_layer)
#' converge(sf_layer, by.feature=F)
#' @export
converge <- function(x, z=NULL, by.feature=T, combine=F) {
  if(!is(x, "sf"))
    stop("x has to be a simple features/sf object for morphogram::converge")
  if(!is.null(z)) {
    if(!is(z, "sf"))
      stop("z has to be a simple features/sf object for morphogram::converge")
  }
  by.id = by.feature

  if (length(x)<1) {
    warning("No features found in supplied sf object, returning empty sf object")
    return(sf::st_set_crs(x, NA))
  }

  # if lat/long is used, convert to a global projected CRS?
  if (isTRUE(sf::st_is_longlat(x))) {
    x1 <- sf::st_transform(x, 3395) # EPSG for World Mercator to enable better centroids with metres
    warning("Supplied sf object uses lat/long values, hence converted to Mercator; centroids may not be satisfactory")
  } else {
    x1 = sf::st_transform(x, 3395)
  }

  # use centroid of each polygon
  # if interested in centroid of entire layer then temporarily find the union of x
  if(isFALSE(by.id) && length(x)>1) { x1 <- sf::st_union(x1) }

  # find centroid (note: does not work with lat/long)
  centroid <- sf::st_centroid(sf::st_geometry(x1))
  # create a mock projection based around the centroid
  centroid <- sf::st_transform(centroid, 4326) # convert centroid to lat/long WGS84 geographic projection
  lat <- sf::st_coordinates(centroid)[,2]   # y
  long <- sf::st_coordinates(centroid)[,1]  # x
  local_crs <- paste0("+proj=etmerc +lat_0=",lat," +lon_0=",long,
                      " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

  # transform original sf layer to mock local projection
  if(isFALSE(by.id) | nrow(x)<2) {
    # if a single row provided OR if not interested in individual items, simples
    y <- sf::st_transform(x, local_crs)
    # remove CRS
    y <- sf::st_set_crs(y, NA)
  } else {
    # effectively split each original polygon, reproject, then recombine into single layer
    y <- list()
    for (i in 1:nrow(x)) y[[i]] <- sf::st_transform(x[i,],local_crs[i])   #seq_along(x[[1]])
    for (i in 1:nrow(x)) y[[i]] <- sf::st_set_crs(y[[i]],NA) #seq_along(x[[1]])
    y <- do.call(rbind,y)

    ######(not yet functional!!)
    #x$new_crs <- local_crs
    # temp_transform <- function(spatialfeatures) {
    #   #sl <- st_sfc(sl)
    #   sf::st_transform(spatialfeatures, sl$new_crs)
    #   }
    # y <- purrr::map(x, temp_transform)
    #y$new_crs <- NULL
  }
  if (isTRUE(combine)) { y <- sf::st_combine(y) }
  return(y)
}
