# Secondary axis for providing space for text annotations

A convenience wrapper around
[`sec_axis_text()`](https://davidhodge931.github.io/ggscribe/reference/sec_axis_text.md)
that reserves vertical (or horizontal) space above (or beside) an axis
without drawing visible text. Useful for pushing axis titles away from
the panel to make room for annotations added with
[`axis_text()`](https://davidhodge931.github.io/ggscribe/reference/axis_text.md),
[`axis_ticks()`](https://davidhodge931.github.io/ggscribe/reference/axis_ticks.md),
or
[`axis_bracket()`](https://davidhodge931.github.io/ggscribe/reference/axis_bracket.md).

## Usage

``` r
sec_axis_spacer(
  breaks = function(x) mean(x),
  labels = "",
  name = NULL,
  guide = guide_sec_axis_spacer(),
  ...
)
```

## Arguments

- breaks:

  A function or numeric vector giving the break position(s) used to
  anchor the text. Defaults to `\(x) mean(x)`, which places a single
  label at the midpoint of the scale limits for continuous scales.

- labels:

  A character string, character vector, or labelling function used as
  the spacer. Defaults to `""`. The spacer works by drawing text that is
  hidden by the guide theme while still taking up layout space. Use
  repeated newlines (e.g. `"\n"`) or words to create the desired amount
  of space.

- name:

  The name of the secondary axis. Use
  [`ggplot2::waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
  to derive the name from the primary axis, or `NULL` (default) for no
  name.

- guide:

  A guide object used to render the axis. Defaults to
  [`guide_sec_axis_spacer()`](https://davidhodge931.github.io/ggscribe/reference/guide_sec_axis_spacer.md),
  which makes transparent ticks and lines.

- ...:

  Additional arguments passed to
  [`ggplot2::dup_axis()`](https://ggplot2.tidyverse.org/reference/sec_axis.html).

## Value

An `AxisSecondary` object for use in the `sec.axis` argument of
[`scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
or
[`scale_y_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html).

## See also

[`guide_sec_axis_spacer()`](https://davidhodge931.github.io/ggscribe/reference/guide_sec_axis_spacer.md),
[`axis_text()`](https://davidhodge931.github.io/ggscribe/reference/axis_text.md),
[`axis_ticks()`](https://davidhodge931.github.io/ggscribe/reference/axis_ticks.md),
[`axis_bracket()`](https://davidhodge931.github.io/ggscribe/reference/axis_bracket.md)
