# Annotate axis text

Create annotated text labels for axis breaks.

This function is designed to work with a theme that is globally set.

It should be used with a `coord` of
`ggplot2::coord_cartesian(clip = "off")`.

## Usage

``` r
scribe_axis_text(
  ...,
  position = NULL,
  x = NULL,
  y = NULL,
  label = NULL,
  colour = NULL,
  size = NULL,
  family = NULL,
  length = NULL,
  hjust = NULL,
  vjust = NULL,
  angle = 0,
  theme = "keep"
)
```

## Arguments

- ...:

  Require named arguments (and support trailing commas).

- position:

  The position of the axis text. One of `"top"`, `"bottom"`, `"left"`,
  or `"right"`. Ignored if both `x` and `y` are provided.

- x:

  A vector of x-axis breaks for text positioning. Use
  [`I()`](https://rdrr.io/r/base/AsIs.html) to specify normalized
  coordinates (0-1).

- y:

  A vector of y-axis breaks for text positioning. Use
  [`I()`](https://rdrr.io/r/base/AsIs.html) to specify normalized
  coordinates (0-1).

- label:

  A vector of text labels or a function that takes breaks and returns
  labels. If `NULL`, uses appropriate formatting based on data type.

- colour:

  The colour of the text. Inherits from the current theme `axis.text`
  etc.

- size:

  The size of the text. Inherits from the current theme `axis.text` etc.

- family:

  The font family of the text. Inherits from the current theme
  `axis.text` etc.

- length:

  The tick length as a grid unit. Use `rel()` to scale relative to
  default length. Negative values or `rel()` with negative multiplier
  place text on the opposite side of the axis (inside the panel).
  Inherits from the current theme `axis.ticks.length` etc.

- hjust, vjust:

  Horizontal and vertical justification. Auto-calculated based on
  position if `NULL`. When `length` is negative, justification
  automatically adjusts for the flipped position.

- angle:

  Text rotation angle. Defaults to `0`.

- theme:

  What to do with the equivalent theme elements. Either `"keep"`,
  `"transparent"`, or `"blank"`. Defaults to `"keep"`.

## Value

A list of annotation annotates and theme elements.
