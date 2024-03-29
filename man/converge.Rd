% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/converge.R
\name{converge}
\alias{converge}
\title{converge}
\usage{
converge(x, z = NULL, by.feature = T, combine = F)
}
\arguments{
\item{x}{An sf-compatible feature layer, often containing polygons whose size is to be visually compared; REQUIRED.}

\item{z}{An sf-compatible feature layer, the target polygons by which x will be converged}

\item{by.feature}{Whether to reproject by single features by individual feature centroids, T,
or reproject all features by a single centroid of the union of all features, F; default=TRUE.}

\item{combine}{Combine multiple geometries into one, using st_combine; default=F.}
}
\value{
An sf object containing one or more features (with no defined CRS)
}
\description{
This function finds the centroid of a set of sf-based features and reprojects them so that each feature's
centroid lies at the centre (0,0) of an arbitrary metre-based Mercator projection. The resulting features
are thus projected overlapping at the same scale,. Note that the resulting sf object has no CRS.
}
\details{
To visually distribute the resulting features, use the `distribute` function.
}
\seealso{
\code{\link{distribute}}
}
\keyword{cartogram,}
\keyword{infographic}
\keyword{sf,}
