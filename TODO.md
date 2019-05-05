## Potential future improvements (TO DO LIST)

- Offer alternative centre points for `align`, such as st_point_in_polygon (`method=c("centroid","pip")`).
- Offer to normalize dimensions of features, with st_normalize, so that full features fit within space of 0,1. (In this case, dimensions are lost, but shape is preserved.)
- Improve documentation, especially around the transformation of source spatial features which are in latlong projection in `align`.
- Provide alternative transformation of source features in latlong projection? (e.g. alternative target `crs`?)
- Additional `distribute` algorithms (e.g. no-fit polygon) which translate from normalized to distributed tableaus in different ways
    - `regularstacks` - rather than filling rows or columns, this method would stack according to certain classificatory criteria, e.g. classifying parameters such as polygon area. (`group.by="AREA"`, `margin=1`). Could be used as basis for spatial-icon-based "bar charts", with features optionally normalized.
    - `nofitpoly` - using methods used to enable efficient cutting out irregular polygons from materials such as cloth or metal, this method would use a user-defined buffer around polygons to find optimal translation matrices to fit all (buffered) polygons together, approximating a tesselation. (`buffer=1.5`).
- Integration with `cartogram` package.
- Convenience functions for processing and plotting as one-liner (e.g. `plot.sc.regulargrid`, `plot.sc.spatialbarplot`)


- Optimisation of code (e.g. replacement of for loops on sf objects with faster alternatives?); low priority as it's actually fast enough.
