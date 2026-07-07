# Annotate a reference line

Draws one or more reference lines within the panel, with style defaults
taken from the `axis.line` element of the set theme.

## Usage

``` r
reference_line(
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

- xintercept:

  One or more x positions for vertical reference lines, in data
  coordinates or wrapped in [`I()`](https://rdrr.io/r/base/AsIs.html)
  for normalised panel coordinates (npc). May be a vector; each value
  produces a separate line.

- yintercept:

  One or more y positions for horizontal reference lines, in data
  coordinates or wrapped in [`I()`](https://rdrr.io/r/base/AsIs.html)
  for normalised panel coordinates (npc). May be a vector; each value
  produces a separate line.

- colour:

  Inherits from `axis.line` in the set theme. May be a vector the same
  length as the total number of lines.

- linewidth:

  Inherits from `axis.line` in the set theme. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html). May
  be a vector the same length as the total number of lines.

- linetype:

  Defaults to `"dashed"`. May be a vector the same length as the total
  number of lines.

- arrow:

  A [`grid::arrow()`](https://rdrr.io/r/grid/arrow.html) specification,
  or a list the same length as the total number of lines. Must use
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

## Value

A list of ggplot2 annotation layers.

## Details

The arrow (if any) points in the positive direction by default —
rightward for `xintercept` lines, upward for `yintercept` lines.
