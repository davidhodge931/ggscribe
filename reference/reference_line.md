# Annotate a reference line

Draws a reference line within the panel, with style defaults taken from
the `axis.line` element of the set theme.

## Usage

``` r
reference_line(
  ...,
  xintercept = NULL,
  yintercept = NULL,
  colour = NULL,
  linewidth = NULL,
  linetype = "dashed",
  arrow = NULL,
  layout = NULL
)
```

## Arguments

- ...:

  Not used. Forces named arguments.

- xintercept:

  Draw vertical reference lines at these x positions in data
  coordinates, or wrapped in [`I()`](https://rdrr.io/r/base/AsIs.html)
  for normalised panel coordinates (npc), where `I(0)` is the left edge
  and `I(1)` is the right edge of the panel. May be a vector for
  multiple lines.

- yintercept:

  Draw horizontal reference lines at these y positions in data
  coordinates, or wrapped in [`I()`](https://rdrr.io/r/base/AsIs.html)
  for normalised panel coordinates (npc), where `I(0)` is the bottom
  edge and `I(1)` is the top edge of the panel. May be a vector for
  multiple lines.

- colour:

  Inherits from `axis.line` in the set theme. May be a vector the same
  length as `xintercept`/`yintercept` to style each line individually.

- linewidth:

  Inherits from `axis.line` in the set theme. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html). May
  be a vector the same length as `xintercept`/`yintercept`.

- linetype:

  Defaults to `"dashed"`. May be a vector the same length as
  `xintercept`/`yintercept`.

- arrow:

  A [`grid::arrow()`](https://rdrr.io/r/grid/arrow.html) specification,
  or a list of such specifications the same length as
  `xintercept`/`yintercept`. The arrowhead points in the positive axis
  direction (right for vertical lines, up for horizontal lines). Must
  use [`list()`](https://rdrr.io/r/base/list.html) not
  [`c()`](https://rdrr.io/r/base/c.html) when supplying multiple values.
  E.g.
  `grid::arrow(angle = 15, length = unit(1.5, "mm"), type = "closed")`.

- layout:

  Controls which panels the annotation appears in. `NULL` (default)
  repeats in all panels. An integer targets a specific panel. `"fixed"`
  repeats in all panels ignoring faceting variables. See
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)
  for full details.

## Value

A list of ggplot2 annotation layers.

## See also

[`axis_line()`](https://davidhodge931.github.io/ggscribe/reference/axis_line.md),
[`axis_ticks()`](https://davidhodge931.github.io/ggscribe/reference/axis_ticks.md),
[`axis_text()`](https://davidhodge931.github.io/ggscribe/reference/axis_text.md),
[`axis_bracket()`](https://davidhodge931.github.io/ggscribe/reference/axis_bracket.md),
[`panel_shade()`](https://davidhodge931.github.io/ggscribe/reference/panel_shade.md),
[`sec_axis_text()`](https://davidhodge931.github.io/ggscribe/reference/sec_axis_text.md)

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
