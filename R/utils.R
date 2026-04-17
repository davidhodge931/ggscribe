# .resolve_axis_line_element --------------------------------------------------

#' @keywords internal
.resolve_axis_line_element <- function(colour, linewidth, linetype) {
  current_theme <- ggplot2::theme_get()

  theme_element_blank <- NULL
  for (nm in c("axis.line.x", "axis.line")) {
    el <- ggplot2::calc_element(nm, current_theme, skip_blank = FALSE)
    if (!is.null(el)) {
      theme_element_blank <- el
      break
    }
  }
  axis_line_intentionally_blank <- is.null(theme_element_blank) ||
    inherits(theme_element_blank, "element_blank")

  resolved_element <- NULL
  if (!axis_line_intentionally_blank) {
    for (nm in c("axis.line.x", "axis.line")) {
      el <- ggplot2::calc_element(nm, current_theme, skip_blank = TRUE)
      if (!is.null(el) && !inherits(el, "element_blank")) {
        resolved_element <- el
        break
      }
    }
  }

  if (
    is.null(colour) &&
    (axis_line_intentionally_blank || is.null(resolved_element$colour))
  ) {
    rlang::warn(
      "The set theme does not define an `axis.line` colour. Defaulting to \"black\"."
    )
  }
  if (
    is.null(linewidth) &&
    (axis_line_intentionally_blank || is.null(resolved_element$linewidth))
  ) {
    rlang::warn(
      "The set theme does not define an `axis.line` linewidth. Defaulting to `0.5`."
    )
  }

  if (is.null(resolved_element)) {
    resolved_element <- list(colour = "black", linewidth = 0.5, linetype = 1)
  }

  list(
    colour = colour %||% resolved_element$colour %||% "black",
    linewidth = if (is.null(linewidth)) {
      resolved_element$linewidth %||% 0.5
    } else if (inherits(linewidth, "rel")) {
      as.numeric(linewidth) * (resolved_element$linewidth %||% 0.5)
    } else {
      linewidth
    },
    linetype = linetype %||% resolved_element$linetype %||% 1
  )
}
