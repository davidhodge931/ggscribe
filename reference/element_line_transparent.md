# Element line that is transparent

A convenience wrapper around
[`ggplot2::element_line()`](https://ggplot2.tidyverse.org/reference/element.html)
that sets the line colour to `"transparent"`. This is particularly
useful in theme modifications where you want the element to maintain its
logical space without being visible.

## Usage

``` r
element_line_transparent(...)
```

## Arguments

- ...:

  Additional arguments passed to
  [`ggplot2::element_line()`](https://ggplot2.tidyverse.org/reference/element.html),
  such as `linewidth`, `linetype`, or `lineend`.

## Value

An `element_line` object.

## See also

[`theme_axis()`](https://davidhodge931.github.io/ggscribe/reference/theme_axis.md),
[`guide_axis()`](https://davidhodge931.github.io/ggscribe/reference/guide_axis.md)

## Examples

``` r
library(ggplot2)

# Using it to hide panel grid lines without using element_blank()
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  theme(panel.grid.major = element_line_transparent())
```
