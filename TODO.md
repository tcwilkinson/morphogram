## Potential future improvements (TO DO LIST)

### Converge improvements

- Offer alternative centre points for `converge`, such as st_point_in_polygon (`method=c("centroid","pip")`).
- Improve documentation, especially around the transformation of source spatial features which are in latlong projection in `converge`.
- Provide alternative transformation of source features in latlong projection? (e.g. alternative target `crs`?)

### Manipulation improvements

- Offer to `normalize` dimensions of features, with st_normalize, so that full features fit within space of 0,1. (In this case, dimensions are lost, but shape is preserved.)
- Implement functions: `normalize`, `scale`.
- Consider renaming `scale` to avoid clash with `base::scale`, e.g. `scaleByArea`

### Distribute improvements

- Additional `distribute` algorithms which translate from converged to distributed tableaus in different ways:-
    - `regulargrid` - the current algorithm, uses just the largest bounding box to grid a grid square
    - `circlepack` - using circle packing algorithm to pack differently sized polygons together in a less regular way
    - `regularstacks` - rather than filling rows or columns, this method would stack according to certain classificatory criteria, e.g. classifying parameters such as polygon area. (`group.by="AREA"`, `margin=1`). Could be used as basis for spatial-icon-based "bar charts", with features optionally normalized.
    - `nofitpoly` - using methods used to enable efficient cutting out irregular polygons from materials such as cloth or metal, this method would use a user-defined buffer around polygons to find heuristic-optimal translation matrices to fit all (buffered) polygons together, approximating a tesselation. (`buffer=1.5`).

### Generalise transitions so that different sf objects can be matched based on a single feature.

- Add a `z` parameter to all functions so that objects can be transformed in a way dictated by another sf layer.
    - #' @param z An sf object, already processed with `converge`, from which transformation will be calculated. default=NULL.

### Convenience functions

- Convenience `textLabel` function (with position options of centroid, within, above, below, left, right)
- Convenience `scaleKey` function (with option of linear vs. areal scales: value specification of linear and or density, as appropriate)
- Convenience functions for processing and plotting as one-liner (e.g. `plot.sc.regulargrid`, `plot.sc.spatialbarplot`)
- Implement a convenience function `sc` which performs the most likely chain with defaults, or a few minor options: `converge` -> `distribute`, so that it is possible to just call plot(sc(sf_layer)).

### Other

- Integration with `cartogram` package?
- Optimisation of code (e.g. replacement of for loops on sf objects with faster alternatives?); low priority as it's actually fast enough?
