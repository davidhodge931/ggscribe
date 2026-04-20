# Annotate a reference line

Draws a reference line within the inside of the panel, with style
defaults taken from the `axis.line` element of the set theme (apart from
linetype, which defaults to "dashed"). Requires
`coord_cartesian(clip = "off")`.

## Usage

``` r
annotate_reference_line(
  ...,
  breaks = NULL,
  colour = NULL,
  linewidth = NULL,
  linetype = "dashed",
  xintercept = NULL,
  yintercept = NULL
)
```

## Arguments

- ...:

  Not used. Allows trailing commas and named-argument style calls.

- breaks:

  Optional numeric vector of length 2 specifying `c(from, to)` to draw a
  partial line. Defaults to the full axis extent.

- colour:

  Inherits from `axis.line` in the set theme.

- linewidth:

  Inherits from `axis.line` in the set theme. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html).

- linetype:

  Inherits from `axis.line` in the set theme.

- xintercept:

  Draw a vertical reference line at this x position.

- yintercept:

  Draw a horizontal reference line at this y position.

## Value

A list of ggplot2 annotation layers.
