# Annotate axis ticks

Draws axis ticks at specified break positions, with style defaults taken
from the `axis.ticks` element of the set theme. Requires
`coord_cartesian(clip = "off")`.

## Usage

``` r
axis_ticks(
  ...,
  position = NULL,
  breaks,
  colour = NULL,
  linewidth = NULL,
  linetype = NULL,
  length = ggplot2::rel(1),
  arrow = NULL,
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

  A numeric vector of break positions in data coordinates, or wrapped in
  [`I()`](https://rdrr.io/r/base/AsIs.html) for normalised panel
  coordinates (npc), where `I(0)` is the left/bottom edge and `I(1)` is
  the right/top edge of the panel.

- colour:

  Inherits from `axis.ticks` in the set theme. May be a vector the same
  length as `breaks` to style each tick individually.

- linewidth:

  Inherits from `axis.ticks` in the set theme. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html). May
  be a vector the same length as `breaks`.

- linetype:

  Inherits from `axis.ticks` in the set theme. May be a vector the same
  length as `breaks`.

- length:

  Total tick length as a grid unit. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html).
  Negative values flip the tick direction (inward). Defaults to `rel(1)`
  (outward at theme tick length). May be a vector the same length as
  `breaks`.

- arrow:

  A [`grid::arrow()`](https://rdrr.io/r/grid/arrow.html) specification,
  or a list of such specifications the same length as `breaks` to mix
  arrowed and plain ticks. Use `NULL` as a list element for no arrow on
  a specific tick. The arrowhead points toward the axis line. Must use
  [`list()`](https://rdrr.io/r/base/list.html) not
  [`c()`](https://rdrr.io/r/base/c.html) when supplying multiple values.
  E.g.
  `grid::arrow(angle = 15, length = unit(1.5, "mm"), type = "closed")`.

- layout:

  Controls which panels the annotation appears in. `NULL` (default)
  repeats in all panels. An integer targets a specific panel. `"fixed"`
  repeats in all panels ignoring faceting variables. See
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)
  for full details.

- xintercept:

  For `"left"`/`"right"` axes: float the axis to this x position in data
  coordinates instead of the panel edge.

- yintercept:

  For `"top"`/`"bottom"` axes: float the axis to this y position in data
  coordinates instead of the panel edge.

## Value

A list of ggplot2 annotation layers.

## See also

[`axis_line()`](https://davidhodge931.github.io/ggscribe/reference/axis_line.md),
[`axis_text()`](https://davidhodge931.github.io/ggscribe/reference/axis_text.md),
[`axis_bracket()`](https://davidhodge931.github.io/ggscribe/reference/axis_bracket.md),
[`reference_line()`](https://davidhodge931.github.io/ggscribe/reference/reference_line.md),
[`panel_shade()`](https://davidhodge931.github.io/ggscribe/reference/panel_shade.md),
[`sec_axis_text()`](https://davidhodge931.github.io/ggscribe/reference/sec_axis_text.md)
