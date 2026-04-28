# axis_line -------------------------------------------------------------------

#' Annotate an axis line
#'
#' Draws a line along an axis edge, with style defaults taken from the
#' `axis.line` element of the set theme. Requires `coord_cartesian(clip = "off")`.
#'
#' @param ... Not used. Forces named arguments.
#' @param position One of `"top"`, `"bottom"`, `"left"`, or `"right"`. Inferred
#'   from `xintercept` or `yintercept` if not provided.
#' @param xintercept For `"left"`/`"right"` axes: float the axis to this x
#'   position in data coordinates instead of the panel edge.
#' @param yintercept For `"top"`/`"bottom"` axes: float the axis to this y
#'   position in data coordinates instead of the panel edge.
#' @param colour Inherits from `axis.line` in the set theme.
#' @param linewidth Inherits from `axis.line` in the set theme. Supports `rel()`.
#' @param linetype Inherits from `axis.line` in the set theme.
#'
#' @return A list of ggplot2 annotation layers.
#' @seealso [axis_ticks()], [axis_text()],
#'   [axis_bracket()], [reference_line()],
#'   [panel_shade()], [sec_axis()]
#' @export
axis_line <- function(
    ...,
    position   = NULL,
    xintercept = NULL,
    yintercept = NULL,
    colour     = NULL,
    linewidth  = NULL,
    linetype   = NULL
) {
  rlang::check_dots_empty()

  position <- .infer_position(position, xintercept, yintercept)
  axis     <- if (position %in% c("top", "bottom")) "x" else "y"

  .validate_intercept(axis, position, xintercept, yintercept)

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

  if (is.null(colour) && (axis_line_intentionally_blank || is.null(resolved_element$colour))) {
    rlang::warn("The set theme does not define an `axis.line` colour. Defaulting to \"black\".")
  }
  if (is.null(linewidth) && (axis_line_intentionally_blank || is.null(resolved_element$linewidth))) {
    rlang::warn("The set theme does not define an `axis.line` linewidth. Defaulting to `0.5`.")
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
