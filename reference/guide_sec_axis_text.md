# Guide optimised for secondary axis text annotations

A wrapper around
[`ggplot2::guide_axis()`](https://ggplot2.tidyverse.org/reference/guide_axis.html)
that defaults to using
[`theme_sec_axis_text()`](https://davidhodge931.github.io/ggscribe/reference/theme_sec_axis_text.md).
This guide is designed to strip away standard axis furniture (like lines
and ticks) while preserving text, making it ideal for secondary axes
used as margin labels.

## Usage

``` r
guide_sec_axis_text(..., theme = theme_sec_axis_text())
```

## Arguments

- ...:

  Additional arguments passed to
  [`ggplot2::guide_axis()`](https://ggplot2.tidyverse.org/reference/guide_axis.html),
  such as `title`, `check.overlap`, or `angle`.

- theme:

  A `theme` object to style the guide. Defaults to
  [`theme_sec_axis_text()`](https://davidhodge931.github.io/ggscribe/reference/theme_sec_axis_text.md),
  which suppresses ticks and lines.

## Value

A `guide` object to be used in a scale's `guide` argument or within
[`sec_axis_text()`](https://davidhodge931.github.io/ggscribe/reference/sec_axis_text.md).

## See also

[`sec_axis_text()`](https://davidhodge931.github.io/ggscribe/reference/sec_axis_text.md),
[`theme_sec_axis_text()`](https://davidhodge931.github.io/ggscribe/reference/theme_sec_axis_text.md)

## Examples

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
  scale_colour_discrete(palette = blends::multiply(get_theme()$palette.colour.discrete)) +
  #clip = "off" is required for axis_text, axis_ticks and axis_bracket
  coord_cartesian(clip = "off") +
  #reference lines and shade
  ggscribe::reference_line(xintercept = 2.4) +
  ggscribe::reference_line(yintercept = 12)  +
  ggscribe::panel_shade(
    xmin = 4,
    xmax = 5,
  ) +
  #top axis
  scale_x_continuous(
    sec.axis = ggscribe::sec_axis_text(
      breaks = c(mean(c(4, 5))),
      labels = c("Range"),
      guide = ggscribe::guide_sec_axis_text(
        angle = 90,
      )
    )
  ) +
  ggscribe::axis_bracket(
    position = "top",
    breaks = c(4, 5),
  ) +
  ggscribe::axis_text(
    position = "top",
    breaks = c(2.4),
    labels = c("Threshold"),
  ) +
  #right axis
  ggscribe::axis_text(
    position = "right",
    breaks = 12,
    labels = "Threshold",
  ) +
  #'geom
  geom_point() +
  #annotations fit plot
  theme(plot.background = element_rect(colour = "grey92"))

```
