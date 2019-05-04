# `spatialcompare`: Create pretty comparative maps of spatial features using `sf`

- Author: Toby C. Wilkinson

## Introduction

This package designed to facilitate the construction of infographics which place spatial features side-by-side, for example to compare visually the dimensions of cities, study regions or building sizes in two dimensional space. The package aims to preserve the accurate spatial _dimensions_ of the features but translate their spatial _position_ so that they are distributed visually. Because spatial _dimensions_ are preserved, further spatial analysis (e.g. area calculations) or the additional of scale bars can be accurately plotted.

Functions rely on `sf` objects as inputs and outputs, hence the primary dependency of this package is `sf`. Use of `sf` objects provides maximum flexibility for the graphics output by leveraging base graphics or packages such as `ggplot` according to need or preference.

![](images/sample_nc.png)



## Install

You can install this directly from the github repository:-

```r
library(devtools)
install_github("tcwilkinson/spatialcompare")
```

**Read the introduction vignette for guidance on how to use this package**:

```r
browseVignettes("spatialcompare")
```

## Contributions

I'm happy to accept pull requests on this package.
