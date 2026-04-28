# Guide for secondary axis annotation

A wrapper around
[`ggplot2::guide_axis()`](https://ggplot2.tidyverse.org/reference/guide_axis.html)
that defaults to using
[`theme_sec_axis()`](https://davidhodge931.github.io/ggscribe/reference/theme_sec_axis.md).
This guide is designed to strip away standard axis furniture (like lines
and ticks) while preserving text, making it ideal for secondary axes
used as margin labels.

## Usage

``` r
guide_sec_axis(..., theme = theme_sec_axis())
```

## Arguments

- ...:

  Additional arguments passed to
  [`ggplot2::guide_axis()`](https://ggplot2.tidyverse.org/reference/guide_axis.html),
  such as `title`, `check.overlap`, or `angle`.

- theme:

  A `theme` object to style the guide. Defaults to
  [`theme_sec_axis()`](https://davidhodge931.github.io/ggscribe/reference/theme_sec_axis.md),
  which suppresses ticks and lines.

## Value

A `guide` object to be used in a scale's `guide` argument or within
[`sec_axis()`](https://davidhodge931.github.io/ggscribe/reference/sec_axis.md).

## See also

[`sec_axis()`](https://davidhodge931.github.io/ggscribe/reference/sec_axis.md),
[`theme_sec_axis()`](https://davidhodge931.github.io/ggscribe/reference/theme_sec_axis.md)

## Examples

``` r
library(ggplot2)

ggplot(mpg, aes(displ, hwy)) +
  ggscribe::reference_line(yintercept = 20) +
  geom_point() +
  scale_y_continuous(
    sec.axis = ggscribe::sec_axis(
      breaks = 20,
      labels = "Reference",
      guide = ggscribe::guide_sec_axis(angle = 270)
    )
  )

```
