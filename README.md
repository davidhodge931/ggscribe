
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

``` r
library(ggplot2)
library(dplyr)

set_theme(
  ggrefine::theme_grey(
    panel_heights = rep(unit(50, "mm"), 100),
    panel_widths = rep(unit(75, "mm"), 100),
  )
)

mtcars |>
  ggplot(aes(x = wt, y = mpg, colour = as.factor(gear), fill = as.factor(gear))) +
  coord_cartesian(clip = "off") +
  ggscribe::annotate_reference_line(xintercept = 2.4) +
  ggscribe::annotate_reference_line(yintercept = 16)  +
  geom_point() +
  scale_x_continuous(
    sec.axis = ggscribe::sec_axis_annotate(
      breaks = c(mean(c(4, 5))),
      labels = c("Threshold"),
      guide = ggscribe::guide_axis_annotate(
        angle = 90,
      )
    )
  ) +
  ggscribe::annotate_axis_text(
    position = "top",
    breaks = c(2.4),
    labels = c("A"),
  ) +
  ggscribe::annotate_axis_text(
      position = "right",
      breaks = 16,
      labels = "C",
  ) +
  ggscribe::annotate_axis_bracket(
    position = "top",
    breaks = c(4, 5),
  ) +
  ggscribe::annotate_panel_shade(
    xmin = 4,
    xmax = 5,
  ) +
  ggscribe::annotate_axis_text(
    position = "bottom",
    breaks = 4.25,
    labels = "D",
  ) +
  ggscribe::annotate_axis_ticks(
    position = "bottom",
    breaks = 4.25,
  ) +
  theme(plot.background = element_rect(colour = "grey92")) 
```

<img src="man/figures/README-unnamed-chunk-2-1.png" alt="" width="100%" />
