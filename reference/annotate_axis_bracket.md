# Annotate an axis bracket

Draws a bracket spanning `min(breaks)` to `max(breaks)` along an axis
edge or at a floating data position. The bar uses the same rendering
path as
[`annotate_axis_line()`](https://davidhodge931.github.io/ggscribe/reference/annotate_axis_line.md);
the caps use the same path as
[`annotate_axis_ticks()`](https://davidhodge931.github.io/ggscribe/reference/annotate_axis_ticks.md).
Requires `coord_cartesian(clip = "off")`.

## Usage

``` r
annotate_axis_bracket(
  ...,
  position = NULL,
  xintercept = NULL,
  yintercept = NULL,
  breaks,
  colour = NULL,
  linewidth = NULL,
  linetype = NULL,
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

  For `"left"`/`"right"` axes: float the bracket to this x position in
  data coordinates instead of the panel edge.

- yintercept:

  For `"top"`/`"bottom"` axes: float the bracket to this y position in
  data coordinates instead of the panel edge.

- breaks:

  A numeric vector of length \>= 2. The bar spans `min(breaks)` to
  `max(breaks)`; caps are drawn at every break value.

- colour:

  Inherits from `axis.ticks` in the set theme (falling back through
  `axis.line` and `line`).

- linewidth:

  Inherits from `axis.ticks` in the set theme. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html).

- linetype:

  Inherits from `axis.ticks` in the set theme.

- ticks_length:

  Length of the bracket caps as a grid unit. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html).
  Negative values flip the cap direction. Defaults to `rel(1)` (outward
  at theme tick length).

## Value

A list of ggplot2 annotation layers.

## See also

[`annotate_axis_line()`](https://davidhodge931.github.io/ggscribe/reference/annotate_axis_line.md),
[`annotate_axis_ticks()`](https://davidhodge931.github.io/ggscribe/reference/annotate_axis_ticks.md),
[`annotate_axis_text()`](https://davidhodge931.github.io/ggscribe/reference/annotate_axis_text.md),
[`annotate_reference_line()`](https://davidhodge931.github.io/ggscribe/reference/annotate_reference_line.md),
[`annotate_panel_shade()`](https://davidhodge931.github.io/ggscribe/reference/annotate_panel_shade.md),
[`sec_axis_annotate()`](https://davidhodge931.github.io/ggscribe/reference/sec_axis_annotate.md)
