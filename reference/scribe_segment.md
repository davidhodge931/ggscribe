# Annotate segment

Create annotated straight line segments with defaults from the axis line
in the set theme.

## Usage

``` r
scribe_segment(
  ...,
  x = NULL,
  y = NULL,
  xend = NULL,
  yend = NULL,
  colour = NULL,
  linewidth = NULL,
  linetype = NULL
)
```

## Arguments

- ...:

  Arguments passed to `ggplot2::annotate("segment", ....)`. Require
  named arguments (and support trailing commas).

- x, y, xend, yend:

  Start and end positions of the segment. Use
  [`I()`](https://rdrr.io/r/base/AsIs.html) for normalized coordinates
  (0-1).

- colour:

  The colour of the segment. Inherits from the current theme `axis.line`
  etc.

- linewidth:

  The linewidth of the segment. Inherits from the current theme
  `axis.line` etc.

- linetype:

  The linetype of the segment. Inherits from the current theme
  `axis.line` etc.

## Value

A list containing annotation layers.
