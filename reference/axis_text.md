# Annotate axis text

Draws text labels at specified break positions along an axis, with style
defaults taken from the `axis.text` element of the set theme. Requires
`coord_cartesian(clip = "off")`.

## Usage

``` r
axis_text(
  ...,
  position = NULL,
  xintercept = NULL,
  yintercept = NULL,
  breaks,
  labels = NULL,
  colour = NULL,
  size = NULL,
  family = NULL,
  hjust = NULL,
  vjust = NULL,
  angle = 0,
  length = ggplot2::rel(1)
)
```

## Arguments

- ...:

  Not used. Forces named arguments.

- position:

  One of `"top"`, `"bottom"`, `"left"`, or `"right"`. Inferred from
  `xintercept` or `yintercept` if not provided.

- xintercept:

  For `"left"`/`"right"` axes: float the axis to this x position in data
  coordinates instead of the panel edge.

- yintercept:

  For `"top"`/`"bottom"` axes: float the axis to this y position in data
  coordinates instead of the panel edge.

- breaks:

  A numeric vector of break positions.

- labels:

  One of:

  - `NULL` (default) to use break values as labels

  - A character vector the same length as `breaks`

  - A function taking break values and returning labels

- colour:

  Inherits from `axis.text` in the set theme.

- size:

  Inherits from `axis.text` in the set theme.

- family:

  Inherits from `axis.text` in the set theme.

- hjust, vjust:

  Justification. Auto-calculated from `position` if `NULL`.

- angle:

  Text rotation angle. Defaults to `0`.

- length:

  Offset from the axis edge including tick length and margin. Supports
  [`rel()`](https://ggplot2.tidyverse.org/reference/element.html).
  Negative values place labels inside the panel. Defaults to `rel(1)`
  (theme tick length + text margin).

## Value

A list of ggplot2 annotation layers.

## See also

[`axis_line()`](https://davidhodge931.github.io/ggscribe/reference/axis_line.md),
[`axis_ticks()`](https://davidhodge931.github.io/ggscribe/reference/axis_ticks.md),
[`axis_bracket()`](https://davidhodge931.github.io/ggscribe/reference/axis_bracket.md),
[`reference_line()`](https://davidhodge931.github.io/ggscribe/reference/reference_line.md),
[`panel_shade()`](https://davidhodge931.github.io/ggscribe/reference/panel_shade.md),
[`sec_axis_text()`](https://davidhodge931.github.io/ggscribe/reference/sec_axis_text.md)

## Examples

``` r
library(ggplot2)
library(dplyr)

set_theme(
  ggrefine::theme_grey(
    panel_heights = rep(unit(50, "mm"), 100),
    panel_widths = rep(unit(75, "mm"), 100),
  )
)

mtcars |>
  ggplot(aes(x = wt, y = mpg, colour = as.factor(gear), fill = as.factor(gear))) +
  scale_colour_discrete(palette = blends::multiply(get_theme()$palette.colour.discrete)) +
  #clip = "off" is required for axis_text, axis_ticks and axis_bracket
  coord_cartesian(clip = "off") +
  #reference lines and shade
  ggscribe::reference_line(xintercept = 2.4) +
  ggscribe::reference_line(yintercept = 12)  +
  ggscribe::panel_shade(
    xmin = 4,
    xmax = 5,
  ) +
  #top axis
  scale_x_continuous(
    sec.axis = ggscribe::sec_axis_text(
      breaks = c(mean(c(4, 5))),
      labels = c("Range"),
      guide = ggscribe::guide_sec_axis_text(
        angle = 90,
      )
    )
  ) +
  ggscribe::axis_bracket(
    position = "top",
    breaks = c(4, 5),
  ) +
  ggscribe::axis_text(
    position = "top",
    breaks = c(2.4),
    labels = c("Threshold"),
  ) +
  #right axis
  ggscribe::axis_text(
    position = "right",
    breaks = 12,
    labels = "Threshold",
  ) +
  #'geom
  geom_point() +
  #annotations fit plot
  theme(plot.background = element_rect(colour = "grey92"))

```
