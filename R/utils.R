# Internal helpers ------------------------------------------------------------

.validate_intercept <- function(axis, position, xintercept, yintercept) {
  if (!is.null(xintercept) && axis == "x") {
    rlang::abort(glue::glue(
      "`xintercept` is not applicable for position = \"{position}\". ",
      "Did you mean `yintercept`?"
    ))
  }
  if (!is.null(yintercept) && axis == "y") {
    rlang::abort(glue::glue(
      "`yintercept` is not applicable for position = \"{position}\". ",
      "Did you mean `xintercept`?"
    ))
  }
  invisible(NULL)
}

.resolve_intercept <- function(axis, position, xintercept, yintercept) {
  if (axis == "x") {
    yintercept %||% if (position == "bottom") -Inf else Inf
  } else {
    xintercept %||% if (position == "left") -Inf else Inf
  }
}

.infer_position <- function(position, xintercept, yintercept) {
  if (!is.null(position)) {
    return(rlang::arg_match(position, c("top", "bottom", "left", "right")))
  }
  if (!is.null(yintercept)) return("bottom")
  if (!is.null(xintercept)) return("left")
  rlang::abort(
    "Must specify `position`, or supply `xintercept` (implies left/right) or `yintercept` (implies top/bottom)."
  )
}

#' Get a dark/light colour for contrast
#'
#' @description Get a dark/light colour based on contrast.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param col A vector of colours from which to determine a contrast vector of light/dark colours.
#' @param dark A dark colour. If NULL, derived from theme text or panel background.
#' @param light A light colour. If NULL, derived from theme text or panel background.
#'
#' @return A character vector of colours, the same length as the `col` vector, containing either
#'         the dark or light colour determined for contrast.
#'
#' @noRd
#'
#' @examples
#' get_contrast(col = c("#000000", "#FFFFFF", "#808080"))  # Uses theme colours
#' get_contrast(col = c("navy", "yellow", "orange"), dark = "navy", light = "lightblue")
#'
get_contrast <- function(..., col, dark = NULL, light = NULL) {
  # Only get theme if we need it
  if (rlang::is_null(dark) || rlang::is_null(light)) {
    # Get current theme
    current_theme <- ggplot2::get_theme()

    # Get text colour from theme
    theme_text <- current_theme$axis.text.x@colour %||%
      current_theme$axis.text.y@colour %||%
      current_theme$axis.text@colour %||%
      current_theme$text@colour %||%
      "black"

    # Get panel background from theme
    theme_panel <- current_theme$panel.background@fill %||%
      "white"

    # Determine which is dark and which is light using is_col_dark
    if (is_col_dark(theme_text)) {
      # Dark text theme (light mode)
      dark <- dark %||% theme_text
      light <- light %||% theme_panel
    } else {
      # Light text theme (dark mode)
      dark <- dark %||% theme_panel
      light <- light %||% theme_text
    }
  }

  # Use is_col_dark to determine which colour to return
  ifelse(!is_col_dark(col), dark, light)
}

#' Check if a colour is dark
#'
#' @description
#' Determines whether a colour is dark by examining its luminance value.
#'
#' @param col A colour value. Can be a hex code, colour name, or any format
#'        accepted by farver. If NULL, returns FALSE.
#'
#' @return TRUE if dark (luminance <= 50) and FALSE otherwise.
#'
#' @noRd
#'
is_col_dark <- function(col) {
  # Handle NULL or missing input
  if (rlang::is_null(col) || length(col) == 0) {
    return(FALSE)
  }

  # Calculate luminance of the colour
  col_luminance <- farver::get_channel(
    colour = col,
    channel = "l",
    space = "hcl"
  )

  # Return TRUE if low luminance
  col_luminance <= 50
}



