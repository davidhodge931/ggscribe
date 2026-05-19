# Annotate an axis line

Draws a line along an axis edge, with style defaults taken from the
`axis.line` element of the set theme. Requires
`coord_cartesian(clip = "off")`.

## Usage

``` r
axis_line(
  ...,
  position = NULL,
  colour = NULL,
  linewidth = NULL,
  linetype = NULL,
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

- colour:

  Inherits from `axis.line` in the set theme. May be a vector the same
  length as `xintercept`/`yintercept` to style each line individually.

- linewidth:

  Inherits from `axis.line` in the set theme. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html). May
  be a vector the same length as `xintercept`/`yintercept`.

- linetype:

  Inherits from `axis.line` in the set theme. May be a vector the same
  length as `xintercept`/`yintercept`.

- arrow:

  A [`grid::arrow()`](https://rdrr.io/r/grid/arrow.html) specification,
  or a list of such specifications the same length as
  `xintercept`/`yintercept`. The arrowhead points in the positive axis
  direction (right for x, up for y). Must use
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

  For `"left"`/`"right"` axes: draw lines at these x positions in data
  coordinates, or wrapped in [`I()`](https://rdrr.io/r/base/AsIs.html)
  for normalised panel coordinates (npc). May be a vector for multiple
  lines.

- yintercept:

  For `"top"`/`"bottom"` axes: draw lines at these y positions in data
  coordinates, or wrapped in [`I()`](https://rdrr.io/r/base/AsIs.html)
  for normalised panel coordinates (npc). May be a vector for multiple
  lines.

## Value

A list of ggplot2 annotation layers.

## See also

[`axis_ticks()`](https://davidhodge931.github.io/ggscribe/reference/axis_ticks.md),
[`axis_text()`](https://davidhodge931.github.io/ggscribe/reference/axis_text.md),
[`axis_bracket()`](https://davidhodge931.github.io/ggscribe/reference/axis_bracket.md),
[`reference_line()`](https://davidhodge931.github.io/ggscribe/reference/reference_line.md),
[`panel_shade()`](https://davidhodge931.github.io/ggscribe/reference/panel_shade.md),
[`sec_axis_text()`](https://davidhodge931.github.io/ggscribe/reference/sec_axis_text.md)
