rotate_fun = function(a) {
  r = a * pi / 180 #degrees to radians
  matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
}

#' st_bbox_and_dims
#'
#' This helper function returns the bounding boxes and width/height dimensions of each feature row in an sf object.
#'
#' @param x An sf-compatible feature layer REQUIRED.
#' @return A data.frame with columns xmin, ymin, xmax, ymax, width, height
st_bbox_and_dims = function(x) {
  x = st_geometry(x)
  f <- function(y) st_bbox(y)
  df <- as.data.frame(do.call("rbind", lapply(x, f)))
  #colnames(df) <- c("xmin","ymin","xmax","ymax")
  df$width <- df$xmax-df$xmin
  df$height <- df$ymax-df$ymin
  df
}

#' get_table_layout
#'
#' This helper function converts a single-dimensional vector into a two-dimensional
#' tabular matrix.
#'
#' In morphogram, it is used to help structure the tabular distribution layouts.
#' Optional are the origin corner, direction of filling, number columns and/or rows.
#'
#' @param n The number of items, if vector is to start from 1 and end at n. Default=1
#' @param i A vector of numbers to be ordered. Default=1:n
#' @param originalcorner The corner from which the matrix should be filled: topleft, bottomleft, topright, bottomright. Default="bottomleft"
#' @param direction The direction by which the table should be filled: row, column. Default="row"
#' @param cols Number of columns to define the matrix
#' @param rows Number of rows to define the matrix
#' @return A matrix containing the vector.
#'
#' If only cols is present, rows are calculated automatically to make a matrix.
#' If neither cols or rows are present, cols is set to sqrt of length of vector.
#' The vector is also padded with NA to create empty cells where
#' the cols*rows is greater than the length of the initial vector.
#'
#' @example
#' tl <- get_table_layout(10,origincorner="topleft")
#' print(tl)
#' @export
get_table_layout <- function(n=1,i=1:n,origincorner="topleft",direction="row",
                       cols=NULL,rows=NULL) {
  if(!origincorner %in% c("topleft","bottomright","topright","bottomleft"))
    stop("origincorner must be one of topleft,bottomright,topright or bottomleft")
  if(direction %in% c("row","horizontal")) {
    byrow=T
  } else if(direction %in% c("col","column","vertical")) {
    byrow=F
  } else {
    stop("direction must be either row or col")
  }

  if (is.null(cols) & is.null(rows)) cols = ceiling(sqrt(length(i)))
  if (is.null(rows)) rows <- ceiling(length(i) / cols) else if (is.null(cols)) cols <- ceiling(length(i) / rows)

  suppressWarnings({
    length(i) <- prod(dim(matrix(i, ncol = cols,nrow = rows)))
  })

  if(origincorner %in% c("bottomright","bottomleft")) {
    i <- rev(i)
  }

  m <- matrix(i, ncol = cols, byrow = byrow)

  if(origincorner %in% c("topright","bottomleft")) {
    m <- m[,ncol(m):1]
  }
  m
}

#' get_table_layout_index
#'
#' An internal convenience function to transform layout matrix into a data.frame, listing row, col
#' coordinates according to the unique numerical vector i, default being 1:ncells in layout
#'
get_table_layout_index <- function(layout,i=1:(nrow(layout)*ncol(layout))) {
  df <- data.frame(do.call("rbind",lapply(i,
                                    FUN=function(x,layout) { which(layout == x, arr.ind = TRUE)},
                                    layout=layout)))
  df
}

#' get_transform
#'
#' An internal function which calculates a set of transformations needed and
#' returns them in a data.frame with columns xshift, yshift for affine transforms,
#' rotate for rotation transformations.
#' Exact parameters used depend on the transformation method used.
#'
#' @param direction Direction of overall diagram, "row" fills each row first, "column" fills each column first; default "row".
#' @param cols Number of columns of features to plot before moving onto next line if dir="v".
#' @param rows Number of rows of features to plot before moving onto next line if dir="h".
#' @param margin Scalar coefficient of spacing between features; default 1.1.
#' @param x.mar Scalar coefficient of cell spacing on x axis, note that this is compounded on margin; default 1.
#' @param y.mar Scalar coefficient of cell spacing on y axis, note that this is compounded on margin; default 1.
#' @param x.nudge Additional spacing to add to x margins; default 0.
#' @param y.nudge Additional spacing to add to y margins; default 0.
get_transform <- function(x, method="regulargrid", ...) {

  # set default values for args, and check for unexpected parameters
  defaultArgs = list(
    #For all
    margin=1.1,x.mar=1,y.mar=1,
    #For regulargrid or table
    bb=NULL,origincorner="topleft",direction="row",cols=NULL,rows=NULL,
    transform_target="bbox_centre",
    x.nudge=0.0,y.nudge=0.0
  )
  elli = names(list(...))
  check = elli %in% names(defaultArgs)
  if (any(!check)) warning(sprintf("%s is not an expected parameter. Did you mistype it?",
                                   paste(elli[!check], collapse = ", ")))
  methodArgs = modifyList(defaultArgs,list(...))
  # #restrict only to defined default args
  # methodArgs <- methodArgs[check]

  #there may be a better way to do this than loop, perhaps tailor per method?:
  # for(i in 1:length(methodArgs)) {
  #   assign(x = names(methodArgs)[i], value = methodArgs[i])
  # }
  margin=methodArgs$margin
  x.mar=methodArgs$x.mar
  y.mar=methodArgs$y.mar
  # check if provided margin values are valid
  if(is.null(margin) || margin==0 || is.infinite(margin)) { stop("Invalid value for margin: ",margin)}
  if(is.null(y.mar) || y.mar==0 || is.infinite(y.mar)) { stop("Invalid value for y.mar: ", y.mar)}
  if(is.null(x.mar) || x.mar==0 || is.infinite(x.mar)) { stop("Invalid value for x.mar: ", x.mar)}

  if(method %in% c("regulargrid","table")) {
    #better way to assign?
    origincorner=methodArgs$origincorner
    cols=methodArgs$cols
    rows=methodArgs$rows
    direction=methodArgs$direction
    bb=methodArgs$bb
    x.nudge=methodArgs$x.nudge
    y.nudge=methodArgs$y.nudge
    tf_target=methodArgs$transform_target

    if(origincorner %in% c("topleft","topright")) {
      shiftydir <- -1 #1 for up or -1 for down
    } else if(origincorner %in% c("bottomleft","bottomright")) {
      shiftydir <- 1 #1 for up or -1 for down
    } else {
      stop("Parameter origincorner must be one of: topleft, topright, bottomleft, bottomright")
    }
    if(origincorner %in% c("topleft","bottomleft")) {
      shiftxdir <- 1 #1 for right or -1 left
    } else if(origincorner %in% c("topright","bottomright")) {
      shiftxdir <- -1 #1 for right or -1 left
    }

    # check for bbox df and derive if not already provided
    if (is.null(bb)) {
      bb = st_bbox_and_dims(x)
    }
    # check if rows and cols is going to work
    if (is.null(cols) & is.null(rows)) {
      cols = ceiling(sqrt(nrow(x)))
      rows = ceiling(nrow(x)/cols)
    } else if (is.null(cols)) {
      cols = ceiling(nrow(x)/rows)
    } else {
      rows = ceiling(nrow(x)/cols)
    }
    if(cols*rows < nrow(x)) {
      warning(paste0("Column or row numbers (",cols,",",rows,") too few for number of features ",nrow(x),". Expect truncated or strange output."))
    } #else {
      #message(paste0("Column or row numbers (",cols,",",rows,")"))
    #}

    ly = get_table_layout(nrow(x),origincorner=origincorner,cols=cols,direction=direction)
    lyi = get_table_layout_index(ly)

    #create inverse col/row directions where needed
    if(origincorner %in% c("topleft","bottomleft")) { bb$col = lyi$col } else { bb$col = max(lyi$col,na.rm=T) +1 - lyi$col }
    if(origincorner %in% c("topleft","topright")) { bb$row = lyi$row } else { bb$row = max(lyi$row,na.rm=T) +1 - lyi$row }

    #adjust widths and heights for margin settings
    if(!margin==1) { bb$width = bb$width * margin; bb$height = bb$height * margin }
    if(!x.mar==1) { bb$width = bb$width * x.mar }
    if(!y.mar==1) { bb$height = bb$height * y.mar }

    if (method=="table") {
      # shift must be sensitive to own col/row and to previous, so loops/apply(s) required
      xcolmaxes = as.vector(
        sapply(bb$col, function(x)
          max(bb$width[bb$col == x],na.rm=T)
        )
      )
      bb$xcolmax = rep(xcolmaxes,max(bb$row))[1:nrow(bb)]
      uxcolmax = unique(bb[c("col","xcolmax")])

      yrowmaxes = as.vector(
        sapply(bb$row, function(y)
          max(bb$height[bb$row == y],na.rm=T)
        )
      )
      bb$yrowmax = rep(yrowmaxes,max(bb$col))[1:nrow(bb)]
      uyrowmax = unique(bb[c("row","yrowmax")])

      bb$xshift = ((as.vector(
        sapply(bb$col, function(x)
          sum(uxcolmax$xcolmax[uxcolmax$col < x],na.rm=T)
        )
      ) + x.nudge) * shiftxdir) + (bb$xcolmax/2 * shiftxdir)

      bb$yshift = ((as.vector(
        sapply(bb$row, function(y)
          sum(uyrowmax$yrowmax[uyrowmax$row < y],na.rm=T)
        )
      ) + y.nudge) * shiftydir) + (bb$yrowmax/2 * shiftydir)

    } else if (method=="regulargrid") {
      # relatively simple as each shift is regular based on bbox of largest object
      bb$xcolmax = max(bb$width,na.rm=T)
      bb$yrowmax = max(bb$height,na.rm=T)

      bb$xshift = (bb$xcolmax/2 * shiftxdir) + ((bb$xcolmax + x.nudge) * (bb$col-1) * shiftxdir)
      bb$yshift = (bb$yrowmax/2 * shiftydir) + ((bb$yrowmax + y.nudge) * (bb$row-1) * shiftydir)
    }

    target_x <- bb$xshift
    target_y <- bb$yshift
    if(tf_target=="centre") {
      #keep as is
    } else if (tf_target=="top") {
      target_y <- bb$yshift + (bb$yrowmax/2)
    } else if (tf_target=="bottom") {
      target_y <- bb$yshift - (bb$yrowmax/2)
    } else if (tf_target=="right") {
      target_x <- bb$xshift + (bb$xcolmax/2)
    } else if (tf_target=="left") {
      target_x <- bb$xshift - (bb$xcolmax/2)
    }

    #bb$target_x <-
    #bb$target_y <- target_y
    bb$xshift <- target_x #bb$target_x
    bb$yshift <- target_y #bb$target_y

    # There is no rotation in default grid-based method, but we pass it back as 0
    bb$rotate <- 0

    #return(bb)
    return(bb[,c("xshift","yshift","rotate")])
  } else {
    stop("Unknown transformation method: ",method)
  }

}

#' distribute
#'
#' This function spatially distributes a set of sf features.
#'
#' The spatial features should be aligned around 0,0 for meaningful results.
#' First use `converge` to bring together diversely distributed polygons around a single origin.
#' Use `align` to align different sets of simple spatial features around the origin.
#'
#' Note that to rotate or scale features individually, you can use the `rotate` and `scale`
#' methods with a vector parameters.
#'
#' @param x An sf-compatible feature layer, often containing polygons whose size is to be visually compared; REQUIRED.
#' @param preserve.parameters Whether to preserve non-geometry parameters; default TRUE.
#' @param method Method used to distribute features; default and only functioning method is "regulargrid".
#' @param max.features Maximum features to compare; default=200.
#' @param scale Affine linear scaling of ALL feature dimensions, this is not areal scaling; default 1.
#' @param angle Affine rotation of ALL feature in degrees; default 0.
#' @param label.points If TRUE, will add two columns label_x and label_y to resulting sf data.frame based on label.pos; default FALSE.
#' @param label.pos Position value for point at edge of regular grid squares, 0=centre, 1=top, 2=right, 3=bottom, 4=left; default=0.
#' @seealso \code{\link{converge}}
#' @return An sf object containing one or more features (with no defined CRS)
#'
#' ##
#'
#' @example
#' sf_layer <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' sf_layer <- align(sf_layer)
#' distribute(sf_layer)
#' distribute(sf_layer,margin=1.5)
#' @export
distribute <- function(x, preserve.parameters=T, max.features=200,
                       method="regulargrid",
                       #scale=1, angle=0,
                       label.points=F, label.pos=1,
                       ...
) {
  sf = x # copy for code readability
  #sf2 <- sf # make copy to preserve original attributes

  known_methods <- c("regulargrid","table")
  if (!method %in% known_methods) {
    stop(paste0("Distribution method ",method," not recognised, it must be one of: ",paste(known_methods,collapse=", ")))
  }
  if (!is(x,"sf") || nrow(x)<1) {
    stop("Parameter x must be an sf object with at least one feature")
    return(x)
  }
  if (max.features>200) {
    warning("Friendly advice: distributing too many features may make it difficult to perceive size differences")
  }
  if (max.features<nrow(x)) {
    warning(paste0("There are more features (",nrow(x),") than the value of max.features; only the first ",
                   max.features," features will be distributed"))
    x <- x[1:max.features,]
  }
  if (max.features>nrow(x)) { max.features = nrow(x) }

  bb = st_bbox_and_dims(x)

  # test to see ratios of sizes are likely to be visually difficult to depict
  ratio_x <- (max(bb$width)/min(bb$width))
  ratio_y <- (max(bb$height)/min(bb$height))
  if ((ratio_x > 100) || (ratio_y > 100)) {
    warning("Friendly advice: the ratio of largest to smallest bounding boxes is very high (width ratio=",ratio_x,", height=",ratio_y,"), depending on the scale of the plot, the visualisation may be difficult to comprehend")
  }

  # get data on how to transform each feature according to method
  tt = "centre"
  if(isTRUE(label.points)) {
    if(label.pos==1 || label.pos=="top") tt = "top"
    if(label.pos==2 || label.pos=="right") tt = "right"
    if(label.pos==3 || label.pos=="bottom") tt = "bottom"
    if(label.pos==4 || label.pos=="left") tt = "left"
  }
  at = get_transform(x, method=method, bb=bb, transform_target=tt, ...)

  # map according to individual features translation shift
  sfg <- sf::st_sfc( cbind(mapply(FUN=function(geom,xshift,yshift,rotate) {
    if(rotate!=0) {
      geom * rotate_fun(rotate) + c(xshift,yshift)
    } else {
      geom + c(xshift,yshift)
    }
  }, sf::st_geometry(sf), at[["xshift"]], at[["yshift"]], at[["rotate"]],  SIMPLIFY = F)) )
  sf::st_geometry(sf) <- sfg

  if(isTRUE(label.points)) {
    # convert sf to a set of centroid points
    sf = sf::st_centroid(sf)
    # add coords for targetted points for labels
    sf$label_x = st_coordinates(sf)[,1]
    sf$label_y = st_coordinates(sf)[,2]
  }

  sf
}

# old_distribute <- function(x, preserve.parameters=T,
#                        method="regulargrid", cols=NULL, rows=NULL,
#                        dir="v", max.features=200,
#                        margin=1.1, x.mar=1, y.mar=1, x.nudge=0, y.nudge=0,
#                        scale=1, angle=0,
#                        label.points=F, label.pos=1
#                        ) {
#   method <- "regulargrid" # FORCE regular.grid as this is the only currently supported method type
#
#   if (nrow(x)<1) {
#     warning("No features found in supplied sf object, returning empty sf object")
#     return(x)
#   }
#   if (max.features>200) {
#     warning("Friendly advice: distributing too many features may make it difficult to perceive size differences")
#   }
#   if (max.features<nrow(x)) {
#     warning(paste0("There are more features (",nrow(x),") than the value of max.features; only the first ",
#                    max.features," features will be distributed"))
#     x <- x[1:max.features,]
#   }
#   if (max.features>nrow(x)) { max.features = nrow(x) }
#
#   # Find sizes of objects and maximal bounding boxes PER FEATURE
#   bbox <- list()
#   for (i in 1:max.features) bbox[[i]] <- sf::st_bbox(x[i,])
#   bbox <- as.data.frame(do.call(rbind,bbox))
#   bbox$diff_x <- bbox$xmax - bbox$xmin
#   bbox$diff_y <- bbox$ymax - bbox$ymin
#
#   # test to see ratios of sizes are likely to be visually difficult to depict
#   widest_bb   <- max(bbox$diff_x)
#   tallest_bb  <- max(bbox$diff_y)
#   thinnest_bb <- min(bbox$diff_x)
#   shortest_bb <- min(bbox$diff_y)
#   ratio_x <- (widest_bb/thinnest_bb)
#   ratio_y <- (tallest_bb/shortest_bb)
#   if ((ratio_x > 100) || (ratio_y > 100)) {
#     warning("Friendly advice: the ratio of largest to smallest objects is very high, the resulting visualisation may be difficult to comprehend")
#   }
#
#   ## regulargrid - Calculate centre point to translate objects to, based on a regular grid using maximal extent of bounding boxes
#   if(method=="regulargrid") {
#     # Find bbox of all features
#     bbox_a <- sf::st_bbox(x)
#     gridsq_x <- abs(bbox_a$xmax-bbox_a$xmin)
#     gridsq_y <- abs(bbox_a$ymax-bbox_a$ymin)
#     d <- NULL
#     if (is.null(cols)) { cols = ceiling(sqrt(max.features)) }
#     cols <- as.numeric(cols)
#     if (cols>max.features) { cols = max.features }
#     if (is.null(rows)) { rows = ceiling(sqrt(max.features)) }
#     rows <- as.numeric(rows)
#     if (rows>max.features) { rows = max.features }
#
#     x_multiplier <- ceiling( (gridsq_x) * (x.mar * margin)) + x.nudge
#     y_multiplier <- ceiling( (gridsq_y) * (y.mar * margin)) + y.nudge
#     #message(x_multiplier) # for testing only
#     #message(y_multiplier)
#
#     d <- list()
#     for (i in seq(1:max.features)) {
#       # Note that y values are negative to go from top to bottom; x are positive
#       if (dir=="v") {
#         pos_y <- 0 - ceiling(i/cols)        # division to find row
#         pos_x <- i %% cols                  # modulus value to find column
#         if (pos_x==0) { pos_x <- cols }     # if remainder is 0 then it must be the final column
#       } else if(dir=="h") {
#         pos_x <- ceiling(i/rows)
#         pos_y <- 0 - (i %% rows)
#         if (pos_y==0) { pos_y <- 0 - (rows) }
#       }
#       #message(paste0(pos_x,",",pos_y)) # for testing only
#       # Rotation should be done before other translation to ensure it is done around origin
#       rotate_fun = function(a){
#         r = a * pi / 180 #degrees to radians
#         matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
#       }
#       affine_transform <- c(x_multiplier*pos_x , y_multiplier*pos_y)
#       if(isTRUE(preserve.parameters)) {
#
#         d[[i]] <- sf::st_sf(
#           data.frame( sf::st_drop_geometry(x[i,]),
#                       geom=sf::st_sf( (sf::st_geometry(x[i,]) * scale * rotate_fun(angle)) + affine_transform) )
#           )
#       } else {
#         d[[i]] <- sf::st_sf((sf::st_geometry(x[i,]) * scale * rotate_fun(angle)) + affine_transform)
#       }
#
#       if(isTRUE(label.points)) { # find points for labels, other or 0=centre, 1=top etc.
#         #centroid <- st_coordinates(st_centroid(d[[i]]))
#         label_x <- affine_transform[1] #centroid[1]
#         label_y <- affine_transform[2] - (y_multiplier/2) #centroid[2]
#         message(paste0(label_x,",",label_y)) # for testing only
#         if(label.pos==0 || label.pos=="centre") {
#           # keep as is
#         } else if (label.pos==1 || label.pos=="top") {
#           label_y <- label_y + (y_multiplier/2)
#         } else if (label.pos==3 || label.pos=="bottom") {
#           label_y <- label_y - (y_multiplier/2)
#         } else if (label.pos==2 || label.pos=="right") {
#           label_x <- label_x + (x_multiplier/2)
#         } else if (label.pos==4 || label.pos=="left") {
#           label_x <- label_x - (x_multiplier/2)
#         }
#         d[[i]]$label_x <- label_x
#         d[[i]]$label_y <- label_y
#       }
#
#     }
#     d <- sf::st_sf(do.call(rbind,d))
#   }
#
#   return(st_sf(st_drop_geometry(d),geometry=st_geometry(d)))
# }
