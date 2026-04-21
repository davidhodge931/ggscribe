# Annotate axis ticks

Draws axis ticks at specified break positions, with style defaults taken
from the `axis.ticks` element of the set theme. Requires
`coord_cartesian(clip = "off")`.

## Usage

``` r
annotate_axis_ticks(
  ...,
  position = NULL,
  xintercept = NULL,
  yintercept = NULL,
  breaks,
  minor = FALSE,
  colour = NULL,
  linewidth = NULL,
  tick_length = NULL,
  elements_to = "transparent"
)
```

## Arguments

- ...:

  Not used. Forces named arguments.

- position:

  One of `"top"`, `"bottom"`, `"left"`, or `"right"`. Inferred from
  `xintercept` or `yintercept` if not provided.

- xintercept:

  For `"left"`/`"right"` axes: float the axis to this x position in data
  coordinates instead of the panel edge.

- yintercept:

  For `"top"`/`"bottom"` axes: float the axis to this y position in data
  coordinates instead of the panel edge.

- breaks:

  A numeric vector of break positions.

- minor:

  Logical. If `TRUE`, uses minor tick theme defaults. Defaults to
  `FALSE`.

- colour:

  Inherits from `axis.ticks` in the set theme.

- linewidth:

  Inherits from `axis.ticks` in the set theme. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html).

- tick_length:

  Total tick length as a grid unit. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html) to
  scale relative to the theme default. Negative values flip the tick
  direction.

- elements_to:

  One of `"keep"`, `"transparent"`, or `"blank"`. Controls whether
  native theme ticks are suppressed. Defaults to `"transparent"`.

## Value

A list of ggplot2 annotation layers and theme elements.

## See also

[`annotate_axis_line()`](https://davidhodge931.github.io/ggscribe/reference/annotate_axis_line.md),
[`annotate_axis_text()`](https://davidhodge931.github.io/ggscribe/reference/annotate_axis_text.md),
[`annotate_axis_bracket()`](https://davidhodge931.github.io/ggscribe/reference/annotate_axis_bracket.md),
[`annotate_reference_line()`](https://davidhodge931.github.io/ggscribe/reference/annotate_reference_line.md)
