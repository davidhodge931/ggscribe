# Annotate axis text

Draws text labels at specified break positions along an axis, with style
defaults taken from the `axis.text` element of the set theme. Requires
`coord_cartesian(clip = "off")`.

## Usage

``` r
annotate_axis_text(
  ...,
  position = NULL,
  xintercept = NULL,
  yintercept = NULL,
  breaks,
  labels = NULL,
  colour = NULL,
  size = NULL,
  family = NULL,
  hjust = NULL,
  vjust = NULL,
  angle = 0,
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

- labels:

  One of:

  - `NULL` (default) to auto-format break values

  - A character vector the same length as `breaks`

  - A function taking break values and returning labels

- colour:

  Inherits from `axis.text` in the set theme.

- size:

  Inherits from `axis.text` in the set theme.

- family:

  Inherits from `axis.text` in the set theme.

- hjust, vjust:

  Justification. Auto-calculated from `position` if `NULL`.

- angle:

  Text rotation angle. Defaults to `0`.

- tick_length:

  Offset from the axis edge including tick length and margin. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html).
  Negative values place labels inside the panel.

- elements_to:

  One of `"keep"`, `"transparent"`, or `"blank"`. Controls whether
  native theme axis text is suppressed. Defaults to `"transparent"`.

## Value

A list of ggplot2 annotation layers and theme elements.

## See also

[`annotate_axis_line()`](https://davidhodge931.github.io/ggscribe/reference/annotate_axis_line.md),
[`annotate_axis_ticks()`](https://davidhodge931.github.io/ggscribe/reference/annotate_axis_ticks.md),
[`annotate_axis_bracket()`](https://davidhodge931.github.io/ggscribe/reference/annotate_axis_bracket.md),
[`annotate_reference_line()`](https://davidhodge931.github.io/ggscribe/reference/annotate_reference_line.md)
