# Annotate the panel background

Annotate a filled rectangle on the panel background. It is designed to
work with a theme that is globally set.

## Usage

``` r
scribe_panel_shade(
  ...,
  xmin = -Inf,
  xmax = Inf,
  ymin = -Inf,
  ymax = Inf,
  fill = NULL,
  alpha = 0.25,
  colour = "transparent",
  linewidth = NULL,
  linetype = NULL
)
```

## Arguments

- ...:

  Arguments passed to `ggplot2::annotate("rect", ....)` (if normalised
  coordinates not used). Require named arguments (and support trailing
  commas).

- xmin:

  A value of length 1. Defaults to `-Inf`. Use
  [`I()`](https://rdrr.io/r/base/AsIs.html) to specify normalized
  coordinates (0-1).

- xmax:

  A value of length 1. Defaults to `Inf`. Use
  [`I()`](https://rdrr.io/r/base/AsIs.html) to specify normalized
  coordinates (0-1).

- ymin:

  A value of length 1. Defaults to `-Inf`. Use
  [`I()`](https://rdrr.io/r/base/AsIs.html) to specify normalized
  coordinates (0-1).

- ymax:

  A value of length 1. Defaults to `Inf`. Use
  [`I()`](https://rdrr.io/r/base/AsIs.html) to specify normalized
  coordinates (0-1).

- fill:

  The fill color to blend with the panel background. Defaults to
  `"#8991A1FF"`. The final rectangle color is created by blending this
  fill with the current panel background: screen blend for dark
  backgrounds, multiply blend for light backgrounds.

- alpha:

  The transparency of the rectangle. Defaults to `0.2` (subtle overlay).

- colour:

  The border colour of the rectangle. Defaults to `"transparent"`.

- linewidth:

  A number. Inherits from the current theme `panel.border` linewidth.
  Supports `rel()` for relative sizing.

- linetype:

  An integer. Defaults to `1`.

## Value

A list containing an annotation annotate.
