# ggscribe

The objective of ggscribe is to provide helpers to annotate ‘ggplot2’
Visualisation

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
#> Warning: package 'ggplot2' was built under R version 4.5.3
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.5.3
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

set_theme(
  ggrefine::theme_grey(
    panel_heights = rep(unit(50, "mm"), 100),
    panel_widths = rep(unit(75, "mm"), 100),
  )
)

ggplot2::mpg |>
  dplyr::mutate(drv = dplyr::case_when(
    drv == "4" ~ "4-wheel",
    drv == "f" ~ "Front",
    drv == "r" ~ "Rear",
  )
  ) |>
  ggplot(aes(x = displ, y = hwy, fill = drv, colour = drv)) +
  geom_point() +
  scale_fill_discrete(palette = jumble::jumble) +
  scale_colour_discrete(palette = blends::multiply(jumble::jumble)) +
  ggrefine::refine_modern() +
  #required for ggscribe
  coord_cartesian(clip = "off") +
  #top labels of low, normal, high
  ggscribe::annotate_axis_text(
    y = c(20, 30, 40),
    element_to = "blank",
    tick_length = rel(-1),
    hjust = 0,
    vjust = -0.5,
  ) +
  ggscribe::annotate_axis_text(
    position = "top",
    x = I(0),
    label = "Low",
    tick_length = rel(0),
    hjust = 0,
  ) +
  ggscribe::annotate_axis_text(
    position = "top",
    x = I(0.5),
    label = "Normal",
    tick_length = rel(0),
    hjust = 0.5,
  ) +
  ggscribe::annotate_axis_text(
    position = "top",
    x = I(1),
    label = "High",
    tick_length = rel(0),
    hjust = 1,
  ) +
  #hack to create space between legend and right labels
  scale_y_continuous(
    position = "right",
    labels = \(x) paste0(x, rep("              ")),
    name = NULL,
  ) +
  #inefficient shade and labels
  ggscribe::annotate_panel_shade(ymax = 20, , fill = flexoki::flexoki$red["red200"]) +
  ggscribe::annotate_axis_text(
    position = "right",
    y = c(20),
  ) +
  ggscribe::annotate_axis_ticks(
    position = "right",
    y = c(20),
  ) +
  ggscribe::annotate_axis_text(
    position = "right",
    y = mean(c(min(ggplot2::mpg$hwy), 20)),
    label = "Inefficient",
    element_to = "transparent",
  ) +
  #efficient shade and labels
  ggscribe::annotate_panel_shade(ymin = 30, fill = flexoki::flexoki$green["green200"]) +
  ggscribe::annotate_axis_text(
    position = "right",
    y = c(30),
  ) +
  ggscribe::annotate_axis_ticks(
    position = "right",
    y = c(30),
  ) +
  ggscribe::annotate_axis_text(
    position = "right",
    y = mean(c(30, max(ggplot2::mpg$hwy))),
    label = "Efficient",
  ) +
  #titles
  labs(
    y = NULL,
    title = "Highway fuel economy",
    subtitle = "By displacement and drive train\n\n",
  )
```

![](reference/figures/README-unnamed-chunk-2-1.png)
