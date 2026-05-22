# Annotate an axis bracket

Draws one or more brackets along an axis edge or at a floating data
position. Each bracket spans `min(breaks)` to `max(breaks)` with caps at
every break value. The bar uses the same rendering path as
[`axis_line()`](https://davidhodge931.github.io/ggscribe/reference/axis_line.md);
the caps use the same path as
[`axis_ticks()`](https://davidhodge931.github.io/ggscribe/reference/axis_ticks.md).
Requires `coord_cartesian(clip = "off")`.

## Usage

``` r
axis_bracket(
  ...,
  position = NULL,
  breaks,
  colour = NULL,
  linewidth = NULL,
  linetype = NULL,
  length = ggplot2::rel(1),
  layout = NULL,
  xintercept = NULL,
  yintercept = NULL
)
```

## Arguments

- ...:

  Not used. Forces named arguments.

- position:

  One of `"top"`, `"bottom"`, `"left"`, or `"right"`. Inferred from
  `xintercept` or `yintercept` if not provided.

- breaks:

  A numeric vector of length \>= 2 in data coordinates, or wrapped in
  [`I()`](https://rdrr.io/r/base/AsIs.html) for normalised panel
  coordinates (npc), where `I(0)` is the left/bottom edge and `I(1)` is
  the right/top edge of the panel. The bar spans `min(breaks)` to
  `max(breaks)`; caps are drawn at every break value. Pass a list of
  such vectors to draw multiple brackets in one call — e.g.
  `breaks = list(c(2, 4), c(6, 8))` draws two brackets. Style args and
  intercepts are recycled to the number of brackets.

- colour:

  Inherits from `axis.ticks` in the set theme. May be a vector the same
  length as the number of brackets.

- linewidth:

  Inherits from `axis.ticks` in the set theme. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html). May
  be a vector the same length as the number of brackets.

- linetype:

  Inherits from `axis.ticks` in the set theme. May be a vector the same
  length as the number of brackets.

- length:

  Length of the bracket caps. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html).
  Negative values flip the cap direction. Defaults to `rel(1)`. May be a
  vector the same length as the number of brackets.

- layout:

  Controls which panels the annotation appears in. `NULL` (default)
  repeats in all panels. An integer targets a specific panel. `"fixed"`
  repeats in all panels ignoring faceting variables. See
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)
  for full details.

- xintercept:

  For `"left"`/`"right"` axes: float the bracket to these x positions in
  data coordinates. Paired 1:1 with brackets — each bracket gets its own
  intercept, recycling applies.

- yintercept:

  For `"top"`/`"bottom"` axes: float the bracket to these y positions in
  data coordinates. Paired 1:1 with brackets — each bracket gets its own
  intercept, recycling applies.

## Value

A list of ggplot2 annotation layers.

## See also

[`axis_line()`](https://davidhodge931.github.io/ggscribe/reference/axis_line.md),
[`axis_ticks()`](https://davidhodge931.github.io/ggscribe/reference/axis_ticks.md),
[`axis_text()`](https://davidhodge931.github.io/ggscribe/reference/axis_text.md),
[`reference_line()`](https://davidhodge931.github.io/ggscribe/reference/reference_line.md),
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
  #geom
  geom_point() +
  #annotations fit plot
  theme(plot.background = element_rect(colour = "grey92"))

```
