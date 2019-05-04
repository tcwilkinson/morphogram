#' normalizeAroundCentroid
#'
#' This function finds the centroid of a set of sf-based features and reprojects them so that each feature's
#' centroid lies at the centre (0,0) of an arbitrary metre-based mercator projection. The resulting featuresh
#' are thus projected overlapping at the same scale, which can be visually compared. Note that the resulting
#' sf object has no CRS.
#' To visually distribute the resulting features, use a `distribute` function.
#' @param x An sf-compatible feature layer, often containing polygons whose size is to be visually compared.
#' @param by.feature Whether to reproject by single features by individual feature centroids
#' or reproject all features by a single centroid of the union of all features (FALSE). Default `TRUE`
#' @keywords cartogram, sf, infographic
#' @export y An sf object containing one or more features (with no defined CRS)
#' @examples
#'
#' normalizeAroundCentroid(nc)

normalizeAroundCentroid <- function(x, by.feature=T) {
  by.id = by.feature

  if (length(x)<1) {
    warning("No features found in supplied sf object, returning empty sf object")
    return(sf::st_set_crs(x, NA))
  }

  # if lat/long is used, convert to a global projected CRS?
  if (isTRUE(sf::st_is_longlat(x))) {
    x1 <- sf::st_transform(x, 3395) # EPSG for World Mercator to enable better centroids with metres
    warning("Supplied sf object uses lat/long values, hence converted to Mercator; centroids may not be satisfactory")
  } else { x1 = x }

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
  if(isFALSE(by.id) || nrow(x)<2) {
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
  return(y)
}
