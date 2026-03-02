# Annotate curve

Create annotated curved line segments with defaults from the axis line
in the set theme.

## Usage

``` r
scribe_curve(
  ...,
  x = NULL,
  y = NULL,
  xend = NULL,
  yend = NULL,
  curvature = 0.5,
  angle = 90,
  ncp = 5,
  colour = NULL,
  linewidth = NULL,
  linetype = NULL
)
```

## Arguments

- ...:

  Arguments passed to `ggplot2::annotate("curve", ....)`. Require named
  arguments (and support trailing commas).

- x, y, xend, yend:

  Start and end positions of the curve. Use
  [`I()`](https://rdrr.io/r/base/AsIs.html) for normalized coordinates
  (0-1).

- curvature:

  A numeric value giving the amount of curvature. Defaults to `0.5`.

- angle:

  A numeric value between 0 and 180, giving an amount to skew the
  control points of the curve. Defaults to `90`.

- ncp:

  The number of control points used to draw the curve. Defaults to `5`.

- colour:

  The colour of the curve. Inherits from the current theme `axis.line`
  etc.

- linewidth:

  The linewidth of the curve. Inherits from the current theme
  `axis.line` etc.

- linetype:

  The linetype of the curve. Inherits from the current theme `axis.line`
  etc.

## Value

A list containing annotation layers.
