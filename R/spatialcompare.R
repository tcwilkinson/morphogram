#' spatialcompare: A package to construct spatially-sensitive tableaux of spatial features.
#'
#' The spatialcompare package provides two main functions:
#' `converge`, and `distribute`, with additional support from `align`, `normalize` and `scale`.
#'
#' @section spatialcompare functions:
#'
#' Constructing a spatialcompare graph requires two or more steps:
#' 1. transforming the spatial features so that they are centred around 0,0 of a
#' notional or mock projection using metres - `converge`;
#' 2. (optionally) transforming the converged features to particular alignment,
#' or scale - `align`, `normalize` and `scale`
#' 3. re-distributing features across the mock projection so that they can be
#' plotted discretely and compared visually - `distribute`.
#'
#' Please read the Getting Started vignette for more information.
#'
#' @docType package
#' @name spatialcompare
NULL
#> NULL
