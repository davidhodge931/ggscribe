# Guide optimised for secondary axis text annotations

A wrapper around
[`ggplot2::guide_axis()`](https://ggplot2.tidyverse.org/reference/guide_axis.html)
that defaults to making transparent ticks and lines while preserving
text, making it ideal for annotation labels.

## Usage

``` r
guide_sec_axis_text(..., theme = NULL)
```

## Arguments

- ...:

  Additional arguments passed to
  [`ggplot2::guide_axis()`](https://ggplot2.tidyverse.org/reference/guide_axis.html),
  such as `title`, `check.overlap`, or `angle`.

- theme:

  A theme object to style the secondary axis.

## Value

A `guide` object to be used in a scale's `guide` argument or within
[`sec_axis_text()`](https://davidhodge931.github.io/ggscribe/reference/sec_axis_text.md).

## See also

[`sec_axis_text()`](https://davidhodge931.github.io/ggscribe/reference/sec_axis_text.md),
[`axis_text()`](https://davidhodge931.github.io/ggscribe/reference/axis_text.md),
[`axis_ticks()`](https://davidhodge931.github.io/ggscribe/reference/axis_ticks.md),
[`axis_bracket()`](https://davidhodge931.github.io/ggscribe/reference/axis_bracket.md)
