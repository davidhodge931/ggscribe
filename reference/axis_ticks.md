# Annotate axis ticks

Draws axis ticks at specified break positions along a floating axis
line. Requires `coord_cartesian(clip = "off")`.

## Usage

``` r
axis_ticks(
  xintercept = NULL,
  yintercept = NULL,
  breaks,
  length = ggplot2::rel(1),
  colour = NULL,
  linewidth = NULL,
  linetype = NULL,
  arrow = NULL,
  layout = NULL
)
```

## Arguments

- xintercept:

  One or more x positions for vertical axis lines, in data coordinates
  or wrapped in [`I()`](https://rdrr.io/r/base/AsIs.html) for normalised
  panel coordinates (npc). May be a vector; each value produces a
  separate axis.

- yintercept:

  One or more y positions for horizontal axis lines, in data coordinates
  or wrapped in [`I()`](https://rdrr.io/r/base/AsIs.html) for normalised
  panel coordinates (npc). May be a vector; each value produces a
  separate axis.

- breaks:

  A numeric vector of break positions in data coordinates, or wrapped in
  [`I()`](https://rdrr.io/r/base/AsIs.html) for npc. Pass a list the
  same length as the total number of axes to use different breaks per
  axis.

- length:

  Total tick length. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html).
  Negative values flip the tick direction. Defaults to `rel(1)`. May be
  a vector recycled across all breaks in order.

- colour:

  Inherits from `axis.ticks` in the set theme. May be a vector recycled
  across all breaks in order.

- linewidth:

  Inherits from `axis.ticks` in the set theme. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html). May
  be a vector recycled across all breaks in order.

- linetype:

  Inherits from `axis.ticks` in the set theme. May be a vector recycled
  across all breaks in order.

- arrow:

  A [`grid::arrow()`](https://rdrr.io/r/grid/arrow.html) specification,
  or a list recycled across all breaks. The arrowhead points toward the
  axis line. Must use [`list()`](https://rdrr.io/r/base/list.html) not
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

## Details

Ticks always point in the positive direction by default (right for
`xintercept`, up for `yintercept`). Use a negative `length` to flip them
(e.g. `length = -rel(1)`).

## See also

[`axis_line()`](https://davidhodge931.github.io/ggscribe/reference/axis_line.md),
[`axis_text()`](https://davidhodge931.github.io/ggscribe/reference/axis_text.md),
[`axis_bracket()`](https://davidhodge931.github.io/ggscribe/reference/axis_bracket.md),
[`reference_line()`](https://davidhodge931.github.io/ggscribe/reference/reference_line.md),
[`panel_background()`](https://davidhodge931.github.io/ggscribe/reference/panel_background.md)
