# reference_line --------------------------------------------------------------

#' Annotate a reference line
#'
#' Draws a reference line within the panel, with style defaults taken from the
#' `axis.line` element of the set theme. Requires `coord_cartesian(clip = "off")`.
#'
#' @param ... Not used. Forces named arguments.
#' @param xintercept Draw a vertical reference line at this x position.
#' @param yintercept Draw a horizontal reference line at this y position.
#' @param colour Inherits from `axis.line` in the set theme.
#' @param linewidth Inherits from `axis.line` in the set theme. Supports `rel()`.
#' @param linetype Defaults to `"dashed"`.
#'
#' @return A list of ggplot2 annotation layers.
#' @seealso [axis_line()], [axis_ticks()],
#'   [axis_text()], [axis_bracket()], [panel_shade()],
#'   [sec_axis_text()]
#' @export
#'
#' @inherit sec_axis_text examples
#'
reference_line <- function(
    ...,
    xintercept = NULL,
    yintercept = NULL,
    colour     = NULL,
    linewidth  = NULL,
    linetype   = "dashed"
) {
  rlang::check_dots_empty()

  if (!is.null(xintercept)) {
    position <- "left"
    axis     <- "y"
  } else if (!is.null(yintercept)) {
    position <- "bottom"
    axis     <- "x"
  } else {
    rlang::abort("Must supply either `xintercept` or `yintercept`.")
  }

  intercept     <- .resolve_intercept(axis, position, xintercept, yintercept)
  current_theme <- ggplot2::theme_get()

  element_hierarchy <- c(
    paste0("axis.line.", axis, ".", position),
    paste0("axis.line.", axis),
    "axis.line"
  )

  theme_element_blank <- NULL
  for (nm in element_hierarchy) {
    el <- ggplot2::calc_element(nm, current_theme, skip_blank = FALSE)
    if (!is.null(el)) { theme_element_blank <- el; break }
  }
  axis_line_intentionally_blank <- is.null(theme_element_blank) ||
    inherits(theme_element_blank, "element_blank")

  resolved_element <- NULL
  if (!axis_line_intentionally_blank) {
    for (nm in element_hierarchy) {
      el <- ggplot2::calc_element(nm, current_theme, skip_blank = TRUE)
      if (!is.null(el) && !inherits(el, "element_blank")) { resolved_element <- el; break }
    }
  }
  if (is.null(resolved_element)) {
    resolved_element <- list(colour = "black", linewidth = 0.5, linetype = 1)
  }

  line_colour    <- colour %||% resolved_element$colour %||% "black"
  line_linewidth <- if (is.null(linewidth)) {
    resolved_element$linewidth %||% 0.5
  } else if (inherits(linewidth, "rel")) {
    as.numeric(linewidth) * (resolved_element$linewidth %||% 0.5)
  } else {
    linewidth
  }
  line_linetype <- linetype %||% resolved_element$linetype %||% 1

  if (axis == "x") {
    list(ggplot2::annotate(
      "segment", x = -Inf, xend = Inf, y = intercept, yend = intercept,
      colour = line_colour, linewidth = line_linewidth, linetype = line_linetype
    ))
  } else {
    list(ggplot2::annotate(
      "segment", x = intercept, xend = intercept, y = -Inf, yend = Inf,
      colour = line_colour, linewidth = line_linewidth, linetype = line_linetype
    ))
  }
}
