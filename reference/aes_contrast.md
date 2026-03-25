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

mtcars |>
  count(cyl, am) |>
  mutate(
    am = if_else(am == 0, "automatic", "manual"),
    am = str_to_sentence(am),
    cyl = as.factor(cyl)
  ) |>
  ggplot(aes(x = am, y = n, fill = cyl, label = n)) +
  geom_col(
    position = position_dodge(preserve = "single"),
    width = 0.75,
  ) +
  scale_fill_manual(values = c("4" = "navy", "6" = "orange", "8" = "pink")) +
  geom_text(
    mapping = aes_contrast(),
    position = position_dodge(width = 0.75, preserve = "single"),
    vjust = 1.33,
    show.legend = FALSE,
  ) +
  scale_y_continuous(expand = expansion(c(0, 0.05)))


mtcars |>
  count(cyl, am) |>
  mutate(
    am = if_else(am == 0, "automatic", "manual"),
    am = stringr::str_to_sentence(am),
    cyl = as.factor(cyl)
  ) |>
  ggplot(aes(y = am, x = n, fill = cyl, label = n)) +
  geom_col(
    position = position_dodge(preserve = "single"),
    width = 0.75,
  ) +
  scale_fill_manual(values = c("4" = "navy", "6" = "orange", "8" = "pink")) +
  geom_text(
    mapping = aes_contrast(),
    position = position_dodge(width = 0.75, preserve = "single"),
    hjust = 1.25,
    show.legend = FALSE,
  ) +
  scale_x_continuous(expand = expansion(c(0, 0.05)))

```
