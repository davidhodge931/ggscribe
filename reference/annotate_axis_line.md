# Annotate an axis line

Draws a line along an axis edge or between two arbitrary points, with
style defaults taken from the `axis.line` element of the set theme.
Requires `coord_cartesian(clip = "off")`.

## Usage

``` r
annotate_axis_line(
  ...,
  position = NULL,
  x = NULL,
  y = NULL,
  xend = NULL,
  yend = NULL,
  xmin = NULL,
  xmax = NULL,
  ymin = NULL,
  ymax = NULL,
  curvature = NULL,
  angle = 90,
  ncp = 5,
  colour = NULL,
  linewidth = NULL,
  linetype = NULL,
  element_to = "keep"
)
```

## Arguments

- ...:

  Not used. Allows trailing commas and named-argument style calls.

- position:

  One of `"top"`, `"bottom"`, `"left"`, or `"right"`. Axis line mode
  only.

- x:

  In axis line mode, a single x value for a vertical line. In
  segment/curve mode, the x start position. Use
  [`I()`](https://rdrr.io/r/base/AsIs.html) for normalized coordinates
  (0-1).

- y:

  In axis line mode, a single y value for a horizontal line. In
  segment/curve mode, the y start position. Use
  [`I()`](https://rdrr.io/r/base/AsIs.html) for normalized coordinates
  (0-1).

- xend, yend:

  End position of the segment or curve. Providing all of `x`, `y`,
  `xend`, `yend` triggers segment/curve mode.

- xmin, xmax:

  Start and end x positions for a horizontal axis line. Use
  [`I()`](https://rdrr.io/r/base/AsIs.html) for normalized coordinates
  (0-1). Axis line mode only.

- ymin, ymax:

  Start and end y positions for a vertical axis line. Use
  [`I()`](https://rdrr.io/r/base/AsIs.html) for normalized coordinates
  (0-1). Axis line mode only.

- curvature:

  Amount of curvature. Negative curves left, positive curves right, zero
  is straight. `NULL` (default) draws a straight segment.

- angle:

  Skew angle of curve control points (0-180). Used only when `curvature`
  is non-`NULL`. Defaults to `90`.

- ncp:

  Number of curve control points. Higher values give smoother curves.
  Used only when `curvature` is non-`NULL`. Defaults to `5`.

- colour:

  Inherits from `axis.line` in the set theme.

- linewidth:

  Inherits from `axis.line` in the set theme. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html).

- linetype:

  Inherits from `axis.line` in the set theme.

- element_to:

  One of `"keep"`, `"transparent"`, or `"blank"`. Controls whether the
  native theme axis line is suppressed. Defaults to `"keep"`. Axis line
  mode only.

## Value

A list of ggplot2 annotation layers and theme elements.

## Details

Operates in two modes:

- **Axis line mode**: triggered by `position`, `x`, or `y` alone.

- **Segment/curve mode**: triggered when `x`, `y`, `xend`, and `yend`
  are all provided. Pass `curvature` to draw a curve instead of a
  straight line.

## Examples

``` r
library(ggplot2)

set_theme(theme_classic())

p <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  coord_cartesian(clip = "off")

# Replace the bottom axis line
p + annotate_axis_line(position = "bottom", element_to = "transparent")


# Partial bottom axis between x = 2 and x = 4
p + annotate_axis_line(position = "bottom", xmin = 2, xmax = 4, element_to = "transparent")


# Vertical rule at x = 3.5
p + annotate_axis_line(x = 3.5)


# Straight line between two data points
p + annotate_axis_line(x = 2, y = 15, xend = 5, yend = 30)


# Curved line between two data points
p + annotate_axis_line(x = 2, y = 15, xend = 5, yend = 30, curvature = 0.3)
```
