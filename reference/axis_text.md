# Annotate axis text

Draws text labels at specified break positions along a floating axis
line, with style defaults taken from the `axis.text` element of the set
theme. Requires `coord_cartesian(clip = "off")`.

## Usage

``` r
axis_text(
  xintercept = NULL,
  yintercept = NULL,
  breaks,
  labels = NULL,
  length = ggplot2::rel(1),
  angle = 0,
  hjust = NULL,
  vjust = NULL,
  colour = NULL,
  size = NULL,
  family = NULL,
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

- labels:

  One of:

  - `NULL` (default) to use break values as labels

  - A character vector recycled across all breaks in order

  - A function taking break values and returning labels

  - A list the same length as the number of axes, each element being one
    of the above

- length:

  Offset from the axis line including tick length and margin. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html).
  Negative values place text on the opposite side. Defaults to `rel(1)`.
  May be a vector recycled across all breaks in order.

- angle:

  Text rotation angle. Defaults to `0`. May be a vector recycled across
  all breaks in order.

- hjust, vjust:

  Justification. Auto-calculated from axis direction and `angle` if
  `NULL`. May be a vector recycled across all breaks in order.

- colour:

  Inherits from `axis.text` in the set theme. May be a vector recycled
  across all breaks in order.

- size:

  Inherits from `axis.text` in the set theme. May be a vector recycled
  across all breaks in order.

- family:

  Inherits from `axis.text` in the set theme. May be a vector recycled
  across all breaks in order.

- layout:

  Controls which panels the annotation appears in. `NULL` (default)
  repeats in all panels. An integer targets a specific panel. `"fixed"`
  repeats in all panels ignoring faceting variables. See
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)
  for full details.

## Value

A list of ggplot2 annotation layers.

## Details

Text always sits on the positive side of the axis by default (right of
`xintercept` lines, above `yintercept` lines). Use a negative `length`
to place text on the opposite side (e.g. `length = -rel(1)`).
