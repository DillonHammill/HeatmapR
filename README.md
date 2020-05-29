
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HeatmapR <img src="man/figures/logo.png" align="right" alt="" width="240"/>

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Travis build
status](https://travis-ci.com/DillonHammill/HeatmapR.svg?branch=master)](https://travis-ci.com/DillonHammill/HeatmapR)
[![Codecov test
coverage](https://codecov.io/gh/DillonHammill/HeatmapR/branch/master/graph/badge.svg)](https://codecov.io/gh/DillonHammill/HeatmapR?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Last-changedate](https://img.shields.io/badge/last%20change-2020--05--29-yellowgreen.svg)](/commits/master)
<!-- badges: end -->

**HeatmapR** it a lightweight R package that makes it easy to generate
high quality heatmaps with minimal data preprocessing or manual
customization. Visit the **HeatmapR** website to get started
<https://dillonhammill.github.io/HeatmapR/>.

## Overview

As the dimensionality of datasets continues to increase there is a need
for visualisation tools such as heatmaps to present data in an easily
interpretable way. The construction of complex heatmaps poses a number
of challenges as they are composed multiple graphical elements, such as
a coloured matrix, dendrograms, cluster sub-divisions, axes, titles and
legends. The first base graphics implementation of heatmaps included
`heatmap()` in the `stats` package and `heatmap.2()` in the
[gplots](https://cran.r-project.org/web/packages/gplots/index.html). The
packages attempt to solve these graphical challenges by treating each
graphical component as a separate plot element and arranging them using
`layout()`. This approach can generate complex heatmaps but it rendered
users unable to arrange the heatmap with additional plot elements.
**HeatmapR** aims to address these layout issues using solely a base
graphics approach.

## HeatmapR

**HeatmapR** has a number of benefits over other heatmap packages:

  - **HeatmapR** is built using base graphics, so there is no required
    `ggplot2` or `plotly` knowledge in order to use this package.
  - **HeatmapR** is extremely lighweight and therefore there are no
    external dependencies that need to be installed.
  - **HeatmapR** is designed under ROpenSci naming guidelines for a
    consistent and intuitive user experience.
  - Unlike other heatmap packages that use base graphics, **HeatmapR**
    actually returns the plot object instead of an image. This makes it
    easy to arrange multiple plots in complex layouts without
    sacrificing resolution.
  - **HeatmapR** is fully customizable and comes with useful saving API
    (`heat_map_save()`) to export high resolution images.
  - **HeatmapR** handles datasets that contain non-numeric or missing
    data, which means you don’t have to spend ages formatting and
    pre-processing the data beforehand.
  - **HeatmapR** generates publication ready images with minimal manual
    customization, making it ideal for users that have limited coding
    experience.

## Installation

**HeatmapR** can be installed directly from GitHub:

``` r
devtools::install_github("DillonHammill/HeatmapR")
```

## Usage

Creating heatmaps is as easy as loading **HeatmapR** and supplying your
dataset to the `heat_map()` function. For details on customising your
heatmaps, refer to the package vignette.

``` r
library(HeatmapR)
heat_map(mtcars,
         scale = "range",
         dendrogram = "both")
```

<img src="man/figures/README-HeatmapR-1.png" width="95%" style="display: block; margin: auto;" />

## Acknowledgements

**HeatmapR** relies on statistical methods in the `stats` package to
compute distance matrices and perform hiearchical clustering.
**HeatmapR** also uses some modified `stats` code from the
[ggdendro](https://github.com/andrie/ggdendro) package to get the
co-ordinates for the dendrogram line segments.

## Code of Conduct

Please note that the HeatmapR project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## Citation

A **HeatmapR** publication is on the way, but in the meantime if you use
**HeatmapR** for your work please cite the package as follows:

``` r
citation("HeatmapR")
#> 
#> To cite package 'HeatmapR' in publications use:
#> 
#>   Dillon Hammill (2020). HeatmapR: Create Heatmaps using Base Graphics.
#>   R package version 0.0.1. https://github.com/DillonHammill/HeatmapR
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {HeatmapR: Create Heatmaps using Base Graphics},
#>     author = {Dillon Hammill},
#>     year = {2020},
#>     note = {R package version 0.0.1},
#>     url = {https://github.com/DillonHammill/HeatmapR},
#>   }
```
