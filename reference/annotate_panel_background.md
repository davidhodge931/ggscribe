# Annotate a panel background region

Draws filled rectangles over the panel. Defaults to the
`panel.background` fill from the set theme at full opacity, making it
useful for layering a solid background over existing content. Should be
placed before geom layers.

## Usage

``` r
annotate_panel_background(
  xmin = -Inf,
  xmax = Inf,
  ymin = -Inf,
  ymax = Inf,
  fill = NULL,
  alpha = 1,
  colour = NULL,
  linewidth = NULL,
  linetype = 0,
  layout = NULL
)
```

## Arguments

- xmin, xmax:

  Left and right edges of the rectangle in data coordinates. Defaults to
  `-Inf` and `Inf`. Use [`I()`](https://rdrr.io/r/base/AsIs.html) for
  normalised coordinates (0-1). May be a vector for multiple rectangles.
  Bounds may be mixed freely, e.g. `xmin = I(0.5), xmax = Inf` fills
  from 50% to the right panel edge.

- ymin, ymax:

  Bottom and top edges of the rectangle in data coordinates. Defaults to
  `-Inf` and `Inf`. Use [`I()`](https://rdrr.io/r/base/AsIs.html) for
  normalised coordinates (0-1). May be a vector for multiple rectangles.

- fill:

  Fill colour. Defaults to the `panel.background` fill from the set
  theme, falling back to `"white"`. May be a vector the same length as
  the bounds to style each rectangle individually.

- alpha:

  Opacity. Defaults to `1` (fully opaque). May be a vector.

- colour:

  Border colour. Defaults to the resolved `fill` value, giving a
  seamless border. May be a vector.

- linewidth:

  Inherits from `panel.border` in the set theme. Supports `rel()`. May
  be a vector.

- linetype:

  Border linetype. Defaults to `0` (none). May be a vector.

- layout:

  Controls which panels the annotation appears in. `NULL` (default)
  repeats in all panels. An integer targets a specific panel. `"fixed"`
  repeats in all panels ignoring faceting variables. See
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)
  for full details.

## Value

A list containing annotation layers.
