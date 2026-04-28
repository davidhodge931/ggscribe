# Annotate a reference line

Draws a reference line within the panel, with style defaults taken from
the `axis.line` element of the set theme. Requires
`coord_cartesian(clip = "off")`.

## Usage

``` r
reference_line(
  ...,
  xintercept = NULL,
  yintercept = NULL,
  colour = NULL,
  linewidth = NULL,
  linetype = "dashed"
)
```

## Arguments

- ...:

  Not used. Forces named arguments.

- xintercept:

  Draw a vertical reference line at this x position.

- yintercept:

  Draw a horizontal reference line at this y position.

- colour:

  Inherits from `axis.line` in the set theme.

- linewidth:

  Inherits from `axis.line` in the set theme. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html).

- linetype:

  Defaults to `"dashed"`.

## Value

A list of ggplot2 annotation layers.

## See also

[`axis_line()`](https://davidhodge931.github.io/ggscribe/reference/axis_line.md),
[`axis_ticks()`](https://davidhodge931.github.io/ggscribe/reference/axis_ticks.md),
[`axis_text()`](https://davidhodge931.github.io/ggscribe/reference/axis_text.md),
[`axis_bracket()`](https://davidhodge931.github.io/ggscribe/reference/axis_bracket.md),
[`panel_shade()`](https://davidhodge931.github.io/ggscribe/reference/panel_shade.md),
[`sec_axis()`](https://davidhodge931.github.io/ggscribe/reference/sec_axis.md)
