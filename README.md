
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rbff

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/rdinnager/rbff/workflows/R-CMD-check/badge.svg)](https://github.com/rdinnager/rbff/actions)
<!-- badges: end -->

The goal of rbff is to provide an R wrapper around the awesome
[boundary-first-flattening](https://geometrycollective.github.io/boundary-first-flattening/)
software, which allows you to flatten and / or map a 3d object to a 2d
shape (or a sphere).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rdinnager/rbff")
```

## Example

You can flatten a mesh with the `bff_flatten()` function. Using default
arguments it tries to find a 2d shape to flatten a 3d mesh into that
minimizes angle and area distortion.

``` r
library(rbff)

face_flat <- bff_flatten(face)
#> Loading required namespace: rgl
```

Visualise the area distortion in this flattening:

``` r
bff_vis_metrics(flat_face, metric = "area distortion")
```

![An plot showing the original 3d mesh and the flattened version, colour
coded by amount of area
distortion](man/figures/rbff_area_distortion.png)

This package is still in the (very) early days of development.
