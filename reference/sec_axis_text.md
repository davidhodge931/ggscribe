# Secondary axis for text annotations

Secondary axis for text annotations

## Usage

``` r
sec_axis_text(
  breaks = function(x) mean(x),
  labels = ggplot2::waiver(),
  name = NULL,
  guide = guide_sec_axis_text(),
  ...
)
```

## Arguments

- breaks:

  A function or numeric vector giving the break position(s) used to
  anchor the text. Defaults to `\(x) mean(x)`, which places a single
  label at the midpoint of the scale limits for continuous scales.

- labels:

  One of:

  - A character vector of labels, the same length as `breaks`

  - A function that takes break positions as input and returns labels If
    left as
    [`ggplot2::waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html),
    labels are derived from the break positions and may be numeric.

- name:

  The name of the secondary axis. Use
  [`ggplot2::waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
  to derive the name from the primary axis, or `NULL` (default) for no
  name.

- guide:

  A guide object used to render the axis. Defaults to
  [`guide_sec_axis_text()`](https://davidhodge931.github.io/ggscribe/reference/guide_sec_axis_text.md),
  which makes transparent ticks and lines.

- ...:

  Additional arguments passed to
  [`ggplot2::dup_axis()`](https://ggplot2.tidyverse.org/reference/sec_axis.html).

## Value

A `AxisSecondary` object for use in the `sec.axis` argument of
[`scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
or
[`scale_y_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html).

## See also

[`guide_sec_axis_text()`](https://davidhodge931.github.io/ggscribe/reference/guide_sec_axis_text.md),
[`axis_text()`](https://davidhodge931.github.io/ggscribe/reference/axis_text.md),
[`axis_ticks()`](https://davidhodge931.github.io/ggscribe/reference/axis_ticks.md),
[`axis_bracket()`](https://davidhodge931.github.io/ggscribe/reference/axis_bracket.md)
