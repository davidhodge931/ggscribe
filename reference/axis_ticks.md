# Annotate axis ticks

Draws axis ticks at specified break positions, with style defaults taken
from the `axis.ticks` element of the set theme. Requires
`coord_cartesian(clip = "off")`.

## Usage

``` r
axis_ticks(
  ...,
  position = NULL,
  xintercept = NULL,
  yintercept = NULL,
  breaks,
  minor = FALSE,
  colour = NULL,
  linewidth = NULL,
  ticks_length = ggplot2::rel(1)
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

- ticks_length:

  Total tick length as a grid unit. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html).
  Negative values flip the tick direction (inward). Defaults to `rel(1)`
  (outward at theme tick length).

## Value

A list of ggplot2 annotation layers.

## See also

[`axis_line()`](https://davidhodge931.github.io/ggscribe/reference/axis_line.md),
[`axis_text()`](https://davidhodge931.github.io/ggscribe/reference/axis_text.md),
[`axis_bracket()`](https://davidhodge931.github.io/ggscribe/reference/axis_bracket.md),
[`reference_line()`](https://davidhodge931.github.io/ggscribe/reference/reference_line.md),
[`panel_shade()`](https://davidhodge931.github.io/ggscribe/reference/panel_shade.md),
[`sec_axis()`](https://davidhodge931.github.io/ggscribe/reference/sec_axis.md)
