# Annotate an axis bracket

Draws a bracket spanning `min(breaks)` to `max(breaks)` along an axis
edge, with an optional centered label. Style defaults are taken from the
`axis.line` and `axis.text` elements of the set theme. Requires
`coord_cartesian(clip = "off")`.

## Usage

``` r
annotate_axis_bracket(
  ...,
  position = NULL,
  breaks,
  labels = NULL,
  colour = NULL,
  size = NULL,
  family = NULL,
  linewidth = NULL,
  linetype = NULL,
  tick_length = NULL,
  xintercept = NULL,
  yintercept = NULL,
  elements_to = "transparent"
)
```

## Arguments

- ...:

  Not used. Allows trailing commas and named-argument style calls.

- position:

  One of `"top"`, `"bottom"`, `"left"`, or `"right"`.

- breaks:

  A numeric vector of length \>= 2. The bracket spans `min(breaks)` to
  `max(breaks)`.

- labels:

  An optional label string (or function returning one) centered over the
  bracket. Defaults to `NULL` (no label).

- colour:

  Inherits from `axis.line` in the set theme.

- size:

  Label text size. Inherits from `axis.text` in the set theme.

- family:

  Label font family. Inherits from `axis.text` in the set theme.

- linewidth:

  Inherits from `axis.line` in the set theme. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html).

- linetype:

  Inherits from `axis.line` in the set theme.

- tick_length:

  Length of the bracket end caps as a grid unit. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html).
  Defaults to the theme tick length.

- xintercept:

  For `"left"`/`"right"` axes: float the bracket to this x position in
  data coordinates instead of the panel edge.

- yintercept:

  For `"top"`/`"bottom"` axes: float the bracket to this y position in
  data coordinates instead of the panel edge.

- elements_to:

  One of `"keep"`, `"transparent"`, or `"blank"`. Controls whether
  native theme ticks are suppressed. Defaults to `"transparent"`.

## Value

A list of ggplot2 annotation layers and theme elements.

## See also

[`annotate_axis_line()`](https://davidhodge931.github.io/ggscribe/reference/annotate_axis_line.md),
[`annotate_axis_ticks()`](https://davidhodge931.github.io/ggscribe/reference/annotate_axis_ticks.md),
[`annotate_axis_text()`](https://davidhodge931.github.io/ggscribe/reference/annotate_axis_text.md),
[`annotate_reference_line()`](https://davidhodge931.github.io/ggscribe/reference/annotate_reference_line.md)
