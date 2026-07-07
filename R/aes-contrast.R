# aes_contrast -------------------------------------------------------------

#' A mapped aesthetic for text colour on fill
#'
#' @description Modifies a mapped colour (or fill) aesthetic for contrast against
#' the fill (or colour) aesthetic.
#'
#' Function can be spliced into [ggplot2::aes] with [rlang::!!!].
#'
#' @param ... Unused. Included to support a trailing comma.
#' @param dark A dark colour. If NULL, derived from theme text or panel background.
#' @param light A light colour. If NULL, derived from theme text or panel background.
#' @param aesthetic The aesthetic to be modified for contrast. Either `"colour"`
#'   (default) or `"fill"`.
#'
#' @return A ggplot2 mapping object suitable for use in `ggplot2::aes()` or as a
#'   `mapping =` argument in a layer.
#'
#' @export
aes_contrast <- function(..., dark = NULL, light = NULL, aesthetic = "colour") {
  aesthetic <- rlang::arg_match(aesthetic, c("colour", "fill"))

  defaults <- .contrast_defaults(dark = dark, light = light)
  dark <- defaults$dark
  light <- defaults$light

  if (identical(aesthetic, "colour")) {
    ggplot2::aes(
      colour = ggplot2::after_scale(
        .get_contrast(col = .data$fill, dark = dark, light = light)
      )
    )
  } else {
    ggplot2::aes(
      fill = ggplot2::after_scale(
        .get_contrast(col = .data$colour, dark = dark, light = light)
      )
    )
  }
}

# internal helpers ---------------------------------------------------------

#' Get a dark/light colour for contrast
#'
#' @description Get a dark/light colour based on contrast.
#'
#' @param ... Unused. Included to support a trailing comma.
#' @param col A vector of colours from which to determine a contrast vector of
#'   light/dark colours.
#' @param dark A dark colour. If NULL, derived from theme text or panel background.
#' @param light A light colour. If NULL, derived from theme text or panel background.
#'
#' @return A character vector of colours, the same length as the `col` vector,
#'   containing either the dark or light colour determined for contrast.
#'
#' @noRd
.get_contrast <- function(..., col, dark = NULL, light = NULL) {
  defaults <- .contrast_defaults(dark = dark, light = light)
  dark <- defaults$dark
  light <- defaults$light

  is_dark <- .is_col_dark(col)

  out <- rep_len(dark, length(col))
  light_vals <- rep_len(light, length(col))
  out[is_dark] <- light_vals[is_dark]

  out
}

.contrast_defaults <- function(dark = NULL, light = NULL) {
  if (!rlang::is_null(dark) && !rlang::is_null(light)) {
    return(list(dark = dark, light = light))
  }

  current_theme <- ggplot2::get_theme()

  theme_text <- .first_theme_colour(
    current_theme,
    c(
      "axis.text.x.bottom",
      "axis.text.x.top",
      "axis.text.y.left",
      "axis.text.y.right",
      "axis.text.x",
      "axis.text.y",
      "axis.text",
      "text"
    )
  )

  if (is.null(theme_text)) {
    theme_text <- "black"
  }

  theme_panel <- .first_theme_fill(
    current_theme,
    c(
      "panel.background",
      "plot.background"
    )
  )

  if (is.null(theme_panel)) {
    theme_panel <- "white"
  }

  if (.is_col_dark(theme_text)) {
    list(
      dark = if (is.null(dark)) theme_text else dark,
      light = if (is.null(light)) theme_panel else light
    )
  } else {
    list(
      dark = if (is.null(dark)) theme_panel else dark,
      light = if (is.null(light)) theme_text else light
    )
  }
}

# utils -------------------------------------------------------------------

.first_theme_colour <- function(theme, elements) {
  for (element in elements) {
    value <- .theme_colour(theme, element)
    if (!is.null(value)) {
      return(value)
    }
  }

  NULL
}

.first_theme_fill <- function(theme, elements) {
  for (element in elements) {
    value <- .theme_fill(theme, element)
    if (!is.null(value)) {
      return(value)
    }
  }

  NULL
}

.theme_colour <- function(theme, element) {
  el <- tryCatch(
    ggplot2::calc_element(element, theme),
    error = function(...) NULL
  )

  if (is.null(el)) {
    return(NULL)
  }

  value <- el$colour

  if (is.null(value)) {
    return(NULL)
  }

  value
}

.theme_fill <- function(theme, element) {
  el <- tryCatch(
    ggplot2::calc_element(element, theme),
    error = function(...) NULL
  )

  if (is.null(el)) {
    return(NULL)
  }

  value <- el$fill

  if (is.null(value)) {
    return(NULL)
  }

  value
}

#' Check if a colour is dark
#'
#' @description
#' Determines whether a colour is dark by examining its luminance value.
#'
#' @param col A colour value. Can be a hex code, colour name, or any format
#'   accepted by farver. If NULL, returns FALSE.
#'
#' @return TRUE if dark (luminance <= 50) and FALSE otherwise.
#'
#' @noRd
.is_col_dark <- function(col) {
  if (rlang::is_null(col) || length(col) == 0) {
    return(FALSE)
  }

  col_luminance <- farver::get_channel(
    colour = col,
    channel = "l",
    space = "hcl"
  )

  col_luminance <= 50
}
