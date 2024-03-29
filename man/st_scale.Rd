% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale.R
\name{st_scale}
\alias{st_scale}
\title{st_scale}
\usage{
st_scale(
  x,
  scale = 1,
  xscale = scale,
  yscale = scale,
  method = "linear",
  origin = "centroid"
)
}
\arguments{
\item{x}{An sf object, to be transformed in scale; REQUIRED.}

\item{scale}{Scaling factor to be used, either a single numeric or a vector of numeric scales of equal length to number of features in x; superceded by xscale or yscale; default=1.}

\item{xscale}{X-axis scaling factor to be used, either a single numeric or a vector of numeric scales of equal length to number of features in x; default=scale.}

\item{yscale}{Y-axis scaling factor to be used, either a single numeric or a vector of numeric scales of equal length to number of features in x; default=scale.}

\item{method}{Method of scaling: "linear", "area"; default="linear".}
}
\value{
An sf object containing one or more features
}
\description{
NOT YET IMPLEMENTED
This function scales the size of sf features around coordinate origin or centroid, by either linear or areal amounts.
}
\seealso{
\code{\link{converge}}, \code{\link{align}}, \code{\link{normalize}},  \code{\link{distribute}}
}
\keyword{cartogram,}
\keyword{infographic}
\keyword{sf,}
