# Annotate axis ticks segments

Create annotated segments of the axis ticks.

This function is designed to work with a theme that is globally set.

It should be used with a `coord` of
`ggplot2::coord_cartesian(clip = "off")`.

## Usage

``` r
scribe_axis_ticks(
  ...,
  position = NULL,
  x = NULL,
  y = NULL,
  minor = FALSE,
  colour = NULL,
  linewidth = NULL,
  length = NULL,
  theme = "keep"
)
```

## Arguments

- ...:

  Require named arguments (and support trailing commas).

- position:

  The position of the axis ticks. One of `"top"`, `"bottom"`, `"left"`,
  or `"right"`.

- x:

  A vector of x-axis breaks for ticks positioning. Use
  [`I()`](https://rdrr.io/r/base/AsIs.html) to specify normalized
  coordinates (0-1).

- y:

  A vector of y-axis breaks for ticks positioning. Use
  [`I()`](https://rdrr.io/r/base/AsIs.html) to specify normalized
  coordinates (0-1).

- minor:

  `TRUE` or `FALSE` whether to relate to minor ticks. Defaults `FALSE`.

- colour:

  The colour of the ticks. Inherits from the current theme `axis.ticks`
  etc.

- linewidth:

  The linewidth of the ticks. Inherits from the current theme
  `axis.ticks` etc.

- length:

  The total distance from the axis line to the ticks as a grid unit. Use
  `rel()` to scale relative to default length. Negative values or
  `rel()` with negative multiplier flip direction.

- theme:

  What to do with the equivalent theme elements. Either `"keep"`,
  `"transparent"`, or `"blank"`. Defaults `"keep"`.

## Value

A list of annotation annotates and theme elements.
