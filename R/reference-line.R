# reference_line --------------------------------------------------------------

#' Annotate a reference line
#'
#' Draws a reference line within the panel, with style defaults taken from the
#' `axis.line` element of the set theme.
#'
#' @param ... Not used. Forces named arguments.
#' @param xintercept Draw vertical reference lines at these x positions in data
#'   coordinates, or wrapped in [I()] for normalised panel coordinates (npc),
#'   where `I(0)` is the left edge and `I(1)` is the right edge of the panel.
#'   May be a vector for multiple lines.
#' @param yintercept Draw horizontal reference lines at these y positions in
#'   data coordinates, or wrapped in [I()] for normalised panel coordinates
#'   (npc), where `I(0)` is the bottom edge and `I(1)` is the top edge of the
#'   panel. May be a vector for multiple lines.
#' @param colour Inherits from `axis.line` in the set theme. May be a vector
#'   the same length as `xintercept`/`yintercept` to style each line
#'   individually.
#' @param linewidth Inherits from `axis.line` in the set theme. Supports
#'   `rel()`. May be a vector the same length as `xintercept`/`yintercept`.
#' @param linetype Defaults to `"dashed"`. May be a vector the same length as
#'   `xintercept`/`yintercept`.
#' @param arrow A [grid::arrow()] specification, or a list of such
#'   specifications the same length as `xintercept`/`yintercept`. The
#'   arrowhead points in the positive axis direction (right for vertical lines,
#'   up for horizontal lines). Must use `list()` not `c()` when supplying
#'   multiple values.
#'   E.g. `grid::arrow(angle = 15, length = unit(1.5, "mm"), type = "closed")`.
#' @param layout Controls which panels the annotation appears in. `NULL`
#'   (default) repeats in all panels. An integer targets a specific panel.
#'   `"fixed"` repeats in all panels ignoring faceting variables. See
#'   [ggplot2::layer()] for full details.
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
    linetype   = "dashed",
    arrow      = NULL,
    layout     = NULL
) {
  rlang::check_dots_empty()

  if (!is.null(xintercept)) {
    npc_intercept <- inherits(xintercept, "AsIs")
    intercepts    <- as.numeric(xintercept)
    axis          <- "y"
  } else if (!is.null(yintercept)) {
    npc_intercept <- inherits(yintercept, "AsIs")
    intercepts    <- as.numeric(yintercept)
    axis          <- "x"
  } else {
    rlang::abort("Must supply either `xintercept` or `yintercept`.")
  }

  n             <- length(intercepts)
  current_theme <- ggplot2::theme_get()

  position <- if (axis == "x") "bottom" else "left"

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

  theme_colour    <- resolved_element$colour   %||% "black"
  theme_linewidth <- resolved_element$linewidth %||% 0.5

  colour_vec <- if (is.null(colour)) rep_len(theme_colour, n) else rep_len(colour, n)

  linewidth_vec <- if (is.null(linewidth)) {
    rep_len(theme_linewidth, n)
  } else if (inherits(linewidth, "rel")) {
    rep_len(as.numeric(linewidth), n) * theme_linewidth
  } else {
    rep_len(linewidth, n)
  }

  linetype_vec <- rep_len(linetype, n)

  arrow_list <- if (inherits(arrow, "arrow")) {
    rep_len(list(arrow), n)
  } else if (is.list(arrow)) {
    rep_len(arrow, n)
  } else {
    rep_len(list(NULL), n)
  }

  lapply(seq_len(n), \(i) {
    intercept <- intercepts[[i]]

    gp <- grid::gpar(
      col     = colour_vec[[i]],
      fill    = colour_vec[[i]],
      lwd     = linewidth_vec[[i]] * ggplot2::.pt,
      lty     = linetype_vec[[i]],
      lineend = "butt"
    )

    if (axis == "x") {
      line_grob <- if (npc_intercept) {
        grid::segmentsGrob(
          x0 = grid::unit(0, "npc"), x1 = grid::unit(1, "npc"),
          y0 = grid::unit(intercept, "npc"), y1 = grid::unit(intercept, "npc"),
          gp = gp, arrow = arrow_list[[i]]
        )
      } else {
        grid::segmentsGrob(
          x0 = grid::unit(0, "npc"), x1 = grid::unit(1, "npc"),
          y0 = grid::unit(0.5, "npc"), y1 = grid::unit(0.5, "npc"),
          gp = gp, arrow = arrow_list[[i]]
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
          y0 = grid::unit(0, "npc"), y1 = grid::unit(1, "npc"),
          gp = gp, arrow = arrow_list[[i]]
        )
      } else {
        grid::segmentsGrob(
          x0 = grid::unit(0.5, "npc"), x1 = grid::unit(0.5, "npc"),
          y0 = grid::unit(0, "npc"),   y1 = grid::unit(1, "npc"),
          gp = gp, arrow = arrow_list[[i]]
        )
      }
      anno_pos <- if (npc_intercept) {
        list(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
      } else {
        list(xmin = intercept, xmax = intercept, ymin = -Inf, ymax = Inf)
      }
    }

    ggplot2::layer(
      geom     = ggplot2::GeomCustomAnn,
      stat     = "identity",
      data     = NULL,
      mapping  = ggplot2::aes(),
      position = "identity",
      params   = c(list(grob = line_grob, na.rm = FALSE), anno_pos),
      layout   = layout
    )
  })
}
