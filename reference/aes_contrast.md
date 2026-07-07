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

  Unused. Included to support a trailing comma.

- dark:

  A dark colour. If NULL, derived from theme text or panel background.

- light:

  A light colour. If NULL, derived from theme text or panel background.

- aesthetic:

  The aesthetic to be modified for contrast. Either `"colour"` (default)
  or `"fill"`.

## Value

A ggplot2 mapping object suitable for use in
[`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html) or
as a `mapping =` argument in a layer.
