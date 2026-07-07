# Annotate a shaded panel region

A convenience wrapper around
[`panel_background()`](https://davidhodge931.github.io/ggscribe/reference/panel_background.md)
with defaults suited to subtle overlays: a blue shade at 25% opacity
with no border. Should be placed before geom layers.

## Usage

``` r
panel_shade(
  xmin = -Inf,
  xmax = Inf,
  ymin = -Inf,
  ymax = Inf,
  fill = flexoki::flexoki$blue["blue200"],
  alpha = 0.25,
  colour = "transparent",
  linewidth = NULL,
  linetype = 1,
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

  Fill colour. Defaults to a neutral grey. May be a vector the same
  length as the bounds.

- alpha:

  Opacity. Defaults to `0.25`. May be a vector.

- colour:

  Border colour. Defaults to `"transparent"`. May be a vector.

- linewidth:

  Inherits from `panel.border` in the set theme. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html). May
  be a vector.

- linetype:

  Border linetype. Defaults to `1`. May be a vector.

- layout:

  Controls which panels the annotation appears in. `NULL` (default)
  repeats in all panels. An integer targets a specific panel. `"fixed"`
  repeats in all panels ignoring faceting variables. See
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)
  for full details.

## Value

A list containing annotation layers.
