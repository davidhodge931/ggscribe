# Annotate an axis bracket

Draws one or more brackets along a floating axis line. Each bracket
spans `min(breaks)` to `max(breaks)` with caps at every break value.
Requires `coord_cartesian(clip = "off")`.

## Usage

``` r
axis_bracket(
  xintercept = NULL,
  yintercept = NULL,
  breaks,
  length = ggplot2::rel(1),
  colour = NULL,
  linewidth = NULL,
  linetype = NULL,
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

  A numeric vector of length \>= 2 in data coordinates, or wrapped in
  [`I()`](https://rdrr.io/r/base/AsIs.html) for npc. The bar spans
  `min(breaks)` to `max(breaks)`; caps are drawn at every break value.
  Pass a list the same length as the total number of axes to use
  different breaks per axis.

- length:

  Length of the bracket caps. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html).
  Negative values flip the cap direction. Defaults to `rel(1)`. May be a
  vector the same length as the number of axes.

- colour:

  Inherits from `axis.ticks` in the set theme (falling back through
  `axis.line` and `line`). May be a vector the same length as the number
  of axes.

- linewidth:

  Inherits from `axis.ticks` in the set theme. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html). May
  be a vector the same length as the number of axes.

- linetype:

  Inherits from `axis.ticks` in the set theme. May be a vector the same
  length as the number of axes.

- layout:

  Controls which panels the annotation appears in. `NULL` (default)
  repeats in all panels. An integer targets a specific panel. `"fixed"`
  repeats in all panels ignoring faceting variables. See
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)
  for full details.

## Value

A list of ggplot2 annotation layers.

## Details

Caps always point in the positive direction by default (right for
`xintercept`, up for `yintercept`). Use a negative `length` to flip them
(e.g. `length = -rel(1)`).
