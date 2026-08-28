# Annotate panel gridlines

Draws gridlines at specified positions, with style defaults taken from
the `panel.grid.major` element of the set theme. Crop bounds (`xmin`,
`xmax`, `ymin`, `ymax`) both filter which lines are drawn and control
how far they run across the panel.

## Usage

``` r
annotate_panel_grid(
  xintercept = NULL,
  yintercept = NULL,
  xmin = -Inf,
  xmax = Inf,
  ymin = -Inf,
  ymax = Inf,
  colour = NULL,
  linewidth = NULL,
  linetype = NULL,
  layout = NULL
)
```

## Arguments

- xintercept:

  One or more x positions for vertical gridlines, in data coordinates or
  wrapped in [`I()`](https://rdrr.io/r/base/AsIs.html) for normalised
  panel coordinates (npc). May be a vector.

- yintercept:

  One or more y positions for horizontal gridlines, in data coordinates
  or wrapped in [`I()`](https://rdrr.io/r/base/AsIs.html) for normalised
  panel coordinates (npc). May be a vector.

- xmin, xmax:

  Left and right crop bounds. Vertical gridlines outside `[xmin, xmax]`
  are not drawn; horizontal gridlines run only from `xmin` to `xmax`.
  Defaults to `-Inf` and `Inf` (full panel). Use
  [`I()`](https://rdrr.io/r/base/AsIs.html) for normalised coordinates
  (npc). Note: filtering (removing lines outside the range) only works
  when both the crop bound and the intercept are in data coordinates.
  npc bounds affect extent only.

- ymin, ymax:

  Bottom and top crop bounds. Horizontal gridlines outside
  `[ymin, ymax]` are not drawn; vertical gridlines run only from `ymin`
  to `ymax`. Defaults to `-Inf` and `Inf` (full panel). Use
  [`I()`](https://rdrr.io/r/base/AsIs.html) for normalised coordinates
  (npc). Note: filtering only works when both the crop bound and the
  intercept are in data coordinates. npc bounds affect extent only.

- colour:

  Inherits from `panel.grid.major` in the set theme. May be a vector the
  same length as the total number of lines.

- linewidth:

  Inherits from `panel.grid.major` in the set theme. Supports `rel()`.
  May be a vector the same length as the total number of lines.

- linetype:

  Inherits from `panel.grid.major` in the set theme. May be a vector the
  same length as the total number of lines.

- layout:

  Controls which panels the annotation appears in. `NULL` (default)
  repeats in all panels. An integer targets a specific panel. `"fixed"`
  repeats in all panels ignoring faceting variables. See
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)
  for full details.

## Value

A list of ggplot2 annotation layers.
