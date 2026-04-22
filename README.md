
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggscribe <a href="https://davidhodge931.github.io/ggscribe/"><img src="man/figures/logo.png" align="right" height="139" alt="ggscribe website" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ggscribe)](https://CRAN.R-project.org/package=ggscribe)
<!-- badges: end -->

The objective of ggscribe is to provide helpers to annotate ‘ggplot2’
Visualisation.

Note:

- `sec_axis_annotate` adjusts space in the plot, whereas `annotate_*`
  functions do not.
- `annotate_axis_ticks`, `annotate_axis_text` and
  `annotate_axis_bracket` require (1) a globally set theme with explicit
  panel dimensions and (2) `coord_cartesian(clip = "off")`
- `annotate_panel_shade` must be before geoms.
- `annotate_reference_line` should be before geoms.
- Where you require annotation text along a axis with different angles
  etc, use a combination of `sec_axis_annotate` and `annotate_*`
  functions. The `sec_axis_annotate` function should include the
  annotation that requires the maximum space that you want the plot to
  adjust to.

## Installation

Install from CRAN, or the development version from
[GitHub](https://github.com/davidhodge931/ggscribe).

``` r
install.packages("ggscribe")
pak::pak("davidhodge931/ggscribe")
```

## Example

<img src="man/figures/README-unnamed-chunk-2-1.png" alt="" width="100%" />
