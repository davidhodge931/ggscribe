# Duplicate axis with axis text only

A wrapper around
[`ggplot2::dup_axis()`](https://ggplot2.tidyverse.org/reference/sec_axis.html)
that creates a secondary axis displaying only axis text — axis lines and
ticks are hidden, making it useful for placing annotation labels (e.g.
region labels alongside a shaded panel).

## Usage

``` r
sec_axis_annotate(
  breaks = ggplot2::waiver(),
  labels = ggplot2::derive(),
  name = NULL,
  guide = ggplot2::guide_axis(theme = theme_axis_annotate()),
  ...
)
```

## Arguments

- breaks:

  One of:

  - `NULL` for no breaks

  - [`ggplot2::waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
    (default) to inherit breaks from the primary axis

  - A numeric vector of break positions

  - A function that takes the scale limits as input and returns break
    positions (e.g. `\(x) mean(c(x[2], 32))`)

- labels:

  One of:

  - [`ggplot2::derive()`](https://ggplot2.tidyverse.org/reference/sec_axis.html)
    (default) to derive labels from `breaks`

  - A character vector of labels, the same length as `breaks`

  - A function that takes break positions as input and returns labels

- name:

  The name of the secondary axis. Use
  [`ggplot2::waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
  to derive the name from the primary axis, or `NULL` (default) for no
  name.

- guide:

  A guide object used to render the axis. Defaults to
  [`guide_axis_annotate()`](https://davidhodge931.github.io/ggscribe/reference/guide_axis_annotate.md),
  which uses
  [`theme_axis_annotate()`](https://davidhodge931.github.io/ggscribe/reference/theme_axis_annotate.md)
  to make transparent ticks and lines by default.

- ...:

  Additional arguments passed to
  [`ggplot2::dup_axis()`](https://ggplot2.tidyverse.org/reference/sec_axis.html).

## Value

A `AxisSecondary` object for use in the `sec.axis` argument of
[`scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
or
[`scale_y_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html).

## See also

[`ggplot2::dup_axis()`](https://ggplot2.tidyverse.org/reference/sec_axis.html),
[`annotate_panel_shade()`](https://davidhodge931.github.io/ggscribe/reference/annotate_panel_shade.md)

## Examples

``` r
library(ggplot2)

ggplot(mpg, aes(x = displ, y = hwy)) +
   geom_point() +
   annotate_panel_shade(ymin = 32) +
   scale_y_continuous(
     sec.axis = sec_axis_annotate(
       breaks = \(x) mean(c(x[2], 32)),
       labels = "Inefficient",
     )
   )
```
