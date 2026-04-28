# A mapped aesthetic for text colour on fill

Modifies a mapped colour (or fill) aesthetic for contrast against the
fill (or colour) aesthetic.

Function can be spliced into
[ggplot2::aes](https://ggplot2.tidyverse.org/reference/aes.html) with
[rlang::!!!](https://rlang.r-lib.org/reference/splice-operator.html).

## Usage

``` r
aes_contrast(..., dark = NULL, light = NULL, aesthetic = "colour")
```

## Arguments

- ...:

  Require named arguments (and support trailing commas).

- dark:

  A dark colour. If NULL, derived from theme text or panel background.

- light:

  A light colour. If NULL, derived from theme text or panel background.

- aesthetic:

  The aesthetic to be modified for contrast. Either "colour" (default)
  or "fill".

## Value

A ggplot2 aesthetic in
[ggplot2::aes](https://ggplot2.tidyverse.org/reference/aes.html).

## See also

[`splice`](https://rlang.r-lib.org/reference/splice.html)

## Examples

``` r
library(ggplot2)
#> 
#> Attaching package: ‘ggplot2’
#> The following object is masked from ‘package:ggscribe’:
#> 
#>     sec_axis
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(stringr)

set_theme(
 ggrefine::theme_light(
    panel_heights = rep(unit(50, "mm"), 100),
    panel_widths = rep(unit(75, "mm"), 100),
 )
)

ggwidth::set_equiwidth(equiwidth = 1.75)

mtcars |>
  count(cyl, am) |>
  mutate(
    am = if_else(am == 0, "Automatic", "Manual"),
    cyl = as.factor(cyl)
  ) |>
  ggplot(aes(x = am, y = n, colour = cyl, fill = cyl, label = n)) +
  geom_col(
    position = position_dodge2(preserve = "single", padding = 0.05),
    width = ggwidth::get_width(n = 2, n_dodge = 3),
  ) +
  scale_fill_discrete(palette = jumble::jumble) +
  scale_colour_discrete(palette = blends::multiply(jumble::jumble)) +
  geom_text(
    mapping = ggscribe::aes_contrast(), # or aes(!!!ggscribe::aes_contrast()),
    position = position_dodge2(
      width = ggwidth::get_width(n = 2, n_dodge = 3),
      padding = 0.05,
      preserve = "single"),
    vjust = 1.33,
    show.legend = FALSE,
  ) +
  scale_y_continuous(expand = expansion(c(0, 0.05))) +
  ggrefine::modern(x_type = "discrete")
#> Error: 'modern' is not an exported object from 'namespace:ggrefine'

mtcars |>
  count(cyl, am) |>
  mutate(
    am = if_else(am == 0, "automatic", "manual"),
    am = stringr::str_to_sentence(am),
    cyl = as.factor(cyl)
  ) |>
  ggplot(aes(y = am, x = n, colour = cyl, fill = cyl, label = n)) +
  geom_col(
    position = position_dodge2(preserve = "single", padding = 0.05),
    width = ggwidth::get_width(n = 2, n_dodge = 3, orientation = "y"),
  ) +
  scale_fill_discrete(palette = jumble::jumble) +
  scale_colour_discrete(palette = blends::multiply(jumble::jumble)) +
  geom_text(
    mapping = ggscribe::aes_contrast(), # or aes(!!!ggscribe::aes_contrast()),
    position = position_dodge2(
      width = ggwidth::get_width(n = 2, n_dodge = 3, orientation = "y"),
      preserve = "single",
      padding = 0.05,
    ),
    hjust = 1.25,
    show.legend = FALSE,
  ) +
  scale_x_continuous(expand = expansion(c(0, 0.05))) +
  ggrefine::modern(y_type = "discrete")
#> Error: 'modern' is not an exported object from 'namespace:ggrefine'
```
