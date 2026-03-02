# Annotate axis line segment

Create an annotated segment of the axis line.

This function is designed to work with a theme that is globally set.

It should be used with a `coord` of
`ggplot2::coord_cartesian(clip = "off")`.

Note that this function does not support plots where either positional
scale is of date or datetime class. Use
[ggplot2::geom_segment](https://ggplot2.tidyverse.org/reference/geom_segment.html),
[ggplot2::geom_hline](https://ggplot2.tidyverse.org/reference/geom_abline.html)
or
[ggplot2::geom_vline](https://ggplot2.tidyverse.org/reference/geom_abline.html)
instead.

## Usage

``` r
scribe_axis_line(
  ...,
  position = NULL,
  x = NULL,
  y = NULL,
  xmin = NULL,
  xmax = NULL,
  ymin = NULL,
  ymax = NULL,
  colour = NULL,
  linewidth = NULL,
  linetype = NULL,
  theme = "keep"
)
```

## Arguments

- ...:

  Arguments passed to `ggplot2::annotate("segment", ....)` (if
  normalised coordinates not used). Require named arguments (and support
  trailing commas).

- position:

  The position of the axis line. One of `"top"`, `"bottom"`, `"left"`,
  or `"right"`. Ignored if `x` or `y` is provided.

- x:

  A single x-axis value for a vertical line. Cannot be used together
  with `y` or `xmin`/`xmax`. Use
  [`I()`](https://rdrr.io/r/base/AsIs.html) for normalized coordinates
  (0-1).

- y:

  A single y-axis value for a horizontal line. Cannot be used together
  with `x` or `ymin`/`ymax`. Use
  [`I()`](https://rdrr.io/r/base/AsIs.html) for normalized coordinates
  (0-1).

- xmin:

  The starting x position for a horizontal line segment. Use
  [`I()`](https://rdrr.io/r/base/AsIs.html) for normalized coordinates
  (0-1).

- xmax:

  The ending x position for a horizontal line segment. Use
  [`I()`](https://rdrr.io/r/base/AsIs.html) for normalized coordinates
  (0-1).

- ymin:

  The starting y position for a vertical line segment. Use
  [`I()`](https://rdrr.io/r/base/AsIs.html) for normalized coordinates
  (0-1).

- ymax:

  The ending y position for a vertical line segment. Use
  [`I()`](https://rdrr.io/r/base/AsIs.html) for normalized coordinates
  (0-1).

- colour:

  The colour of the annotated segment. Inherits from the current theme
  axis.line etc.

- linewidth:

  A number. Inherits from the current theme axis.line etc.

- linetype:

  An integer. Inherits from the current theme axis.line etc.

- theme:

  How to modify the corresponding theme element. One of `"keep"`,
  `"transparent"`, or `"blank"`. Defaults to `"keep"`.

## Value

A list of annotation annotates and theme elements.
