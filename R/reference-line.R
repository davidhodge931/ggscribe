# reference_line --------------------------------------------------------------

#' Annotate a reference line
#'
#' Draws a reference line within the panel, with style defaults taken from the
#' `axis.line` element of the set theme.
#'
#' @param ... Not used. Forces named arguments.
#' @param xintercept Draw a vertical reference line at this x position in data
#'   coordinates, or wrapped in [I()] for a normalised panel coordinate (npc),
#'   where `I(0)` is the left edge and `I(1)` is the right edge of the panel.
#' @param yintercept Draw a horizontal reference line at this y position in
#'   data coordinates, or wrapped in [I()] for a normalised panel coordinate
#'   (npc), where `I(0)` is the bottom edge and `I(1)` is the top edge of the
#'   panel.
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
    npc_intercept <- inherits(xintercept, "AsIs")
    xintercept    <- as.numeric(xintercept)
    position      <- "left"
    axis          <- "y"
  } else if (!is.null(yintercept)) {
    npc_intercept <- inherits(yintercept, "AsIs")
    yintercept    <- as.numeric(yintercept)
    position      <- "bottom"
    axis          <- "x"
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

  gp <- grid::gpar(
    col     = line_colour,
    lwd     = line_linewidth * ggplot2::.pt,
    lty     = line_linetype,
    lineend = "butt"
  )

  # For npc intercepts, the intercept value goes directly into the grob as
  # unit(val, "npc") and the annotation spans the full panel.
  # For data intercepts, the grob spans the full panel in npc and
  # annotation_custom pins it at the intercept in data coordinates —
  # consistent with the rendering path used by axis_ticks, axis_text, and
  # axis_bracket.
  if (axis == "x") {
    line_grob <- if (npc_intercept) {
      grid::segmentsGrob(
        x0 = grid::unit(0, "npc"),   x1 = grid::unit(1, "npc"),
        y0 = grid::unit(intercept, "npc"), y1 = grid::unit(intercept, "npc"),
        gp = gp
      )
    } else {
      grid::segmentsGrob(
        x0 = grid::unit(0, "npc"), x1 = grid::unit(1, "npc"),
        y0 = grid::unit(0.5, "npc"), y1 = grid::unit(0.5, "npc"),
        gp = gp
      )
    }
    anno_pos <- if (npc_intercept) {
      list(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
    } else {
      list(xmin = -Inf, xmax = Inf, ymin = intercept, ymax = intercept)
    }
  } else {
    line_grob <- if (npc_intercept) {
      grid::segmentsGrob(
        x0 = grid::unit(intercept, "npc"), x1 = grid::unit(intercept, "npc"),
        y0 = grid::unit(0, "npc"),   y1 = grid::unit(1, "npc"),
        gp = gp
      )
    } else {
      grid::segmentsGrob(
        x0 = grid::unit(0.5, "npc"), x1 = grid::unit(0.5, "npc"),
        y0 = grid::unit(0, "npc"),   y1 = grid::unit(1, "npc"),
        gp = gp
      )
    }
    anno_pos <- if (npc_intercept) {
      list(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
    } else {
      list(xmin = intercept, xmax = intercept, ymin = -Inf, ymax = Inf)
    }
  }

  list(rlang::exec(ggplot2::annotation_custom, grob = line_grob, !!!anno_pos))
}
