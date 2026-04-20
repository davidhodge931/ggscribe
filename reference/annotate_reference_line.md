# Annotate a reference line

Draws a reference line within the inside of the panel, with style
defaults taken from the `axis.line` element of the set theme (apart from
linetype, which defaults to "dashed"). Requires
`coord_cartesian(clip = "off")`.

## Usage

``` r
annotate_reference_line(
  ...,
  xintercept = NULL,
  yintercept = NULL,
  colour = NULL,
  linewidth = NULL,
  linetype = "dashed"
)
```

## Arguments

- ...:

  Not used. Allows trailing commas and named-argument style calls.

- xintercept:

  Draw a vertical reference line at this x position.

- yintercept:

  Draw a horizontal reference line at this y position.

- colour:

  Inherits from `axis.line` in the set theme.

- linewidth:

  Inherits from `axis.line` in the set theme. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html).

- linetype:

  Inherits from `axis.line` in the set theme.

## Value

A list of ggplot2 annotation layers.
