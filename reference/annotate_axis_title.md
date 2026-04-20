# Annotate an axis title

Draws a title label along a panel edge, with style defaults taken from
the `axis.title` element of the set theme. Requires
`coord_cartesian(clip = "off")` to render outside the panel boundary.

## Usage

``` r
annotate_axis_title(
  ...,
  position,
  label,
  colour = NULL,
  size = NULL,
  family = NULL,
  angle = NULL,
  hjust = NULL,
  vjust = NULL,
  offset = NULL,
  elements_to = "keep"
)
```

## Arguments

- ...:

  Not used. Allows trailing commas and named-argument style calls.

- position:

  One of `"top"`, `"bottom"`, `"left"`, or `"right"`.

- label:

  A single string for the title.

- colour:

  Inherits from `axis.title` in the set theme.

- size:

  Inherits from `axis.title` in the set theme.

- family:

  Inherits from `axis.title` in the set theme.

- angle:

  Text rotation. Auto-inferred from `position` if `NULL` (0 for
  top/bottom, 90 for left, -90 for right).

- hjust, vjust:

  Justification. Auto-calculated from `position` if `NULL`.

- offset:

  Distance from the panel edge as a
  [`grid::unit()`](https://rdrr.io/r/grid/unit.html). Defaults to
  `unit(30, "pt")`. Increase if the title overlaps tick labels.

- elements_to:

  One of `"keep"`, `"transparent"`, or `"blank"`. Controls whether the
  native theme axis title is suppressed. Defaults to `"keep"`.

## Value

A list of ggplot2 annotation layers and theme elements.

## See also

[`annotate_axis_text()`](https://davidhodge931.github.io/ggscribe/reference/annotate_axis_text.md),
[`annotate_axis_ticks()`](https://davidhodge931.github.io/ggscribe/reference/annotate_axis_ticks.md),
[`annotate_axis_line()`](https://davidhodge931.github.io/ggscribe/reference/annotate_axis_line.md)

## Examples

``` r
library(ggplot2)

set_theme(
  ggrefine::theme_grey(
    panel_heights = rep(unit(50, "mm"), 100),
    panel_widths  = rep(unit(75, "mm"), 100),
  )
)

p <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  coord_cartesian(clip = "off")

# Replace the bottom axis title
p + labs(x = NULL) +
  annotate_axis_title(position = "bottom", label = "Weight (1000 lbs)")


# Suppress the native title and annotate instead
p + annotate_axis_title(
  position = "left",
  label    = "Miles per gallon",
  elements_to = "blank"
)


# Custom offset when tick labels are large
p + annotate_axis_title(
  position = "bottom",
  label    = "Weight (1000 lbs)",
  offset   = grid::unit(45, "pt")
)
```
