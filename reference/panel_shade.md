# Annotate a shaded panel region

Draws a filled rectangle over the panel with colour defaults taken from
the set theme. Defaults to a subtle overlay across the full panel, with
the fill automatically adapting to light or dark panel backgrounds.
Should be placed before geom layers.

## Usage

``` r
panel_shade(
  ...,
  xmin = -Inf,
  xmax = Inf,
  ymin = -Inf,
  ymax = Inf,
  fill = "#878580",
  alpha = 0.25,
  colour = "transparent",
  linewidth = NULL,
  linetype = NULL
)
```

## Arguments

- ...:

  Not used. Allows trailing commas and named-argument style calls.

- xmin, xmax:

  Left and right edges of the rectangle. Defaults to `-Inf` and `Inf`.
  Use [`I()`](https://rdrr.io/r/base/AsIs.html) for normalized
  coordinates (0-1).

- ymin, ymax:

  Bottom and top edges of the rectangle. Defaults to `-Inf` and `Inf`.
  Use [`I()`](https://rdrr.io/r/base/AsIs.html) for normalized
  coordinates (0-1).

- fill:

  Fill colour. Defaults to a neutral grey.

- alpha:

  Opacity of the rectangle. Defaults to `0.25`.

- colour:

  Border colour. Defaults to `"transparent"`.

- linewidth:

  Inherits from `panel.border` in the set theme. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html).

- linetype:

  Border linetype. Defaults to `1`.

## Value

A list containing an annotation layer.

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
