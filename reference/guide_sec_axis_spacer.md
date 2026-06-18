# Guide optimised for secondary axis space adjustments

A wrapper around
[`ggplot2::guide_axis()`](https://ggplot2.tidyverse.org/reference/guide_axis.html)
that defaults to making transparent ticks and lines and hiding text
while preserving its layout space, making it useful for reserving room
for annotations.

## Usage

``` r
guide_sec_axis_spacer(...)
```

## Arguments

- ...:

  Additional arguments passed to
  [`ggplot2::guide_axis()`](https://ggplot2.tidyverse.org/reference/guide_axis.html),
  such as `title`, `check.overlap`, or `angle`.

## Value

A `guide` object to be used in a scale's `guide` argument or within
[`sec_axis_spacer()`](https://davidhodge931.github.io/ggscribe/reference/sec_axis_spacer.md).

## See also

[`sec_axis_spacer()`](https://davidhodge931.github.io/ggscribe/reference/sec_axis_spacer.md),
[`axis_text()`](https://davidhodge931.github.io/ggscribe/reference/axis_text.md),
[`axis_ticks()`](https://davidhodge931.github.io/ggscribe/reference/axis_ticks.md),
[`axis_bracket()`](https://davidhodge931.github.io/ggscribe/reference/axis_bracket.md)
