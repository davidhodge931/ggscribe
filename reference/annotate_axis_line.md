# Annotate an axis line

Draws a line along an axis edge, with style defaults taken from the
`axis.line` element of the set theme. Requires
`coord_cartesian(clip = "off")`.

## Usage

``` r
annotate_axis_line(
  ...,
  position = NULL,
  xintercept = NULL,
  yintercept = NULL,
  colour = NULL,
  linewidth = NULL,
  linetype = NULL,
  elements_to = "keep"
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

- colour:

  Inherits from `axis.line` in the set theme.

- linewidth:

  Inherits from `axis.line` in the set theme. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html).

- linetype:

  Inherits from `axis.line` in the set theme.

- elements_to:

  One of `"keep"`, `"transparent"`, or `"blank"`. Controls whether the
  native theme axis line is suppressed. Defaults to `"keep"`.

## Value

A list of ggplot2 annotation layers and theme elements.

## See also

[`annotate_axis_ticks()`](https://davidhodge931.github.io/ggscribe/reference/annotate_axis_ticks.md),
[`annotate_axis_text()`](https://davidhodge931.github.io/ggscribe/reference/annotate_axis_text.md),
[`annotate_axis_bracket()`](https://davidhodge931.github.io/ggscribe/reference/annotate_axis_bracket.md),
[`annotate_reference_line()`](https://davidhodge931.github.io/ggscribe/reference/annotate_reference_line.md)
