# Axis guide with annotation-friendly defaults

A wrapper around
[`ggplot2::guide_axis()`](https://ggplot2.tidyverse.org/reference/guide_axis.html)
that defaults to using
[`theme_axis()`](https://davidhodge931.github.io/ggscribe/reference/theme_axis.md).
This guide is designed to strip away standard axis furniture (like lines
and ticks) while preserving text, making it ideal for secondary axes
used as margin labels.

## Usage

``` r
guide_axis(..., theme = theme_axis())
```

## Arguments

- ...:

  Additional arguments passed to
  [`ggplot2::guide_axis()`](https://ggplot2.tidyverse.org/reference/guide_axis.html),
  such as `title`, `check.overlap`, or `angle`.

- theme:

  A `theme` object to style the guide. Defaults to
  [`theme_axis()`](https://davidhodge931.github.io/ggscribe/reference/theme_axis.md),
  which suppresses ticks and lines.

## Value

A `guide` object to be used in a scale's `guide` argument or within
[`sec_axis()`](https://davidhodge931.github.io/ggscribe/reference/sec_axis.md).

## See also

[`sec_axis()`](https://davidhodge931.github.io/ggscribe/reference/sec_axis.md),
[`theme_axis()`](https://davidhodge931.github.io/ggscribe/reference/theme_axis.md)

## Examples

``` r
library(ggplot2)

# Using the guide directly in a scale
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_x_continuous(
    guide = ggscribe::guide_axis(title = "Displacement Label Only")
  )


# The guide is also used internally by sec_axis()
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_y_continuous(
    sec.axis = ggscribe::sec_axis(
      breaks = 20,
      labels = "Reference point",
      guide = ggscribe::guide_axis(angle = 90)
    )
  )
```
