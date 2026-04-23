# Theme axis annotate

Theme axis annotate

## Usage

``` r
theme_axis_annotate(
  axis = NULL,
  axis_ticks_to = "transparent",
  axis_line_to = "transparent",
  axis_text_to = "keep",
  axis_title_to = "keep"
)
```

## Arguments

- axis:

  Character. "x", "y", or NULL (defaults to both).

- axis_ticks_to:

  Action for ticks: "transparent", "blank", or "keep".

- axis_line_to:

  Action for lines: "transparent", "blank", or "keep".

- axis_text_to:

  Action for text: "transparent", "blank", or "keep".

- axis_title_to:

  Action for titles: "transparent", "blank", or "keep".

## Value

A ggplot2 theme object.

## See also

[`sec_axis_annotate()`](https://davidhodge931.github.io/ggscribe/reference/sec_axis_annotate.md),
[`guide_axis_annotate()`](https://davidhodge931.github.io/ggscribe/reference/guide_axis_annotate.md)

[`annotate_axis_ticks()`](https://davidhodge931.github.io/ggscribe/reference/annotate_axis_ticks.md),
[`annotate_axis_line()`](https://davidhodge931.github.io/ggscribe/reference/annotate_axis_line.md),
[`annotate_axis_text()`](https://davidhodge931.github.io/ggscribe/reference/annotate_axis_text.md),
[`annotate_reference_line()`](https://davidhodge931.github.io/ggscribe/reference/annotate_reference_line.md)
