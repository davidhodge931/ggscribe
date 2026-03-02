# Annotate text

Create annotated text labels with defaults from the axis text in the set
theme.

## Usage

``` r
scribe_text(
  ...,
  x = NULL,
  y = NULL,
  label = NULL,
  colour = NULL,
  size = NULL,
  family = NULL,
  hjust = 0.5,
  vjust = 0.5,
  angle = 0
)
```

## Arguments

- ...:

  Arguments passed to `ggplot2::annotate("text", ....)`. Require named
  arguments (and support trailing commas).

- x, y:

  Position of the text. Use [`I()`](https://rdrr.io/r/base/AsIs.html)
  for normalized coordinates (0-1).

- label:

  The text to display.

- colour:

  The colour of the text. Inherits from the current theme `axis.text`
  etc.

- size:

  The size of the text. Inherits from the current theme `axis.text` etc.

- family:

  The font family of the text. Inherits from the current theme
  `axis.text` etc.

- hjust, vjust:

  Horizontal and vertical justification. Defaults to `0.5`.

- angle:

  Text rotation angle. Defaults to `0`.

## Value

A list containing annotation layers.
