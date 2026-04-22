# Theme axis annotate

Theme axis annotate

## Usage

``` r
theme_axis_annotate(
  axis = NULL,
  elements_to_ticks = "transparent",
  elements_to_line = "transparent",
  elements_to_text = "keep",
  elements_to_title = "keep"
)
```

## Arguments

- axis:

  Character. "x", "y", or NULL (defaults to both).

- elements_to_ticks:

  Action for ticks: "transparent", "blank", or "keep".

- elements_to_line:

  Action for lines: "transparent", "blank", or "keep".

- elements_to_text:

  Action for text: "transparent", "blank", or "keep".

- elements_to_title:

  Action for titles: "transparent", "blank", or "keep".

## Value

A ggplot2 theme object.
