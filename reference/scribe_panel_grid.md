# Annotate panel grid segments

Create annotated segments of the panel grid.

This function is designed to work with a theme that is globally set.

## Usage

``` r
scribe_panel_grid(
  ...,
  x = NULL,
  y = NULL,
  xmin = NULL,
  xmax = NULL,
  ymin = NULL,
  ymax = NULL,
  minor = FALSE,
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

- x:

  A vector of x-axis breaks for vertical grid lines. Cannot be used
  together with `y`. Use [`I()`](https://rdrr.io/r/base/AsIs.html) to
  specify normalized coordinates (0-1).

- y:

  A vector of y-axis breaks for horizontal grid lines. Cannot be used
  together with `x`. Use [`I()`](https://rdrr.io/r/base/AsIs.html) to
  specify normalized coordinates (0-1).

- xmin, xmax:

  The starting and ending x positions for horizontal grid lines. Use
  [`I()`](https://rdrr.io/r/base/AsIs.html) for normalized coordinates
  (0-1). Defaults to `-Inf` and `Inf`.

- ymin, ymax:

  The starting and ending y positions for vertical grid lines. Use
  [`I()`](https://rdrr.io/r/base/AsIs.html) for normalized coordinates
  (0-1). Defaults to `-Inf` and `Inf`.

- minor:

  Logical. If `FALSE` (default), creates major grid lines. If `TRUE`,
  creates minor grid lines.

- colour:

  The colour of grid lines. Inherits from current theme
  `panel.grid.major` or `panel.grid.minor` etc.

- linewidth:

  A number. Inherits from current theme `panel.grid.major` or
  `panel.grid.minor` etc.

- linetype:

  An integer. Inherits from current theme `panel.grid.major` or
  `panel.grid.minor` etc.

- theme:

  What to do with the equivalent theme elements. Either `"keep"`,
  `"transparent"`, or `"blank"`. Defaults `"keep"`.

## Value

A list of annotate annotates and theme elements.
