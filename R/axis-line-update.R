# annotate_axis_line ----------------------------------------------------------

#' Annotate an axis line
#'
#' Draws a line along an axis edge or as an interior reference line, with style
#' defaults taken from the `axis.line` element of the set theme. Use
#' `xmin`/`xmax` or `ymin`/`ymax` to draw a partial line. Lines along or
#' outside the panel boundary require `coord_cartesian(clip = "off")`.
#'
#' To draw a straight or curved line between two arbitrary data points, see
#' [annotate_axis_segment()] and [annotate_axis_curve()].
#'
#' @param ... Not used. Allows trailing commas and named-argument style calls.
#' @param position One of `"top"`, `"bottom"`, `"left"`, or `"right"`. Inferred
#'   from `x` or `y` if not provided.
#' @param x A single x value for a vertical reference line. Use `I()` for
#'   normalized coordinates (0-1).
#' @param y A single y value for a horizontal reference line. Use `I()` for
#'   normalized coordinates (0-1).
#' @param xmin,xmax Start and end x positions for a horizontal axis line. Use
#'   `I()` for normalized coordinates (0-1).
#' @param ymin,ymax Start and end y positions for a vertical axis line. Use
#'   `I()` for normalized coordinates (0-1).
#' @param colour Inherits from `axis.line` in the set theme.
#' @param linewidth Inherits from `axis.line` in the set theme. Supports
#'   `rel()`.
#' @param linetype Inherits from `axis.line` in the set theme.
#' @param element_to One of `"keep"`, `"transparent"`, or `"blank"`. Controls
#'   whether the native theme axis line is suppressed. Defaults to `"keep"`.
#'
#' @return A list of ggplot2 annotation layers and theme elements.
#' @seealso [annotate_axis_segment()], [annotate_axis_curve()]
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' set_theme(
#'   ggrefine::theme_grey(
#'     panel_heights = rep(unit(50, "mm"), 100),
#'     panel_widths = rep(unit(75, "mm"), 100),
#'   )
#' )
#'
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   coord_cartesian(clip = "off")
#'
#' # Replace the bottom axis line
#' p + annotate_axis_line(position = "bottom", element_to = "transparent")
#'
#' # Partial bottom axis between x = 2 and x = 4
#' p + annotate_axis_line(position = "bottom", xmin = 2, xmax = 4, element_to = "transparent")
#'
#' # Vertical reference line at x = 3.5
#' p + annotate_axis_line(x = 3.5)
annotate_axis_line <- function(
    ...,
    position = NULL,
    x = NULL,
    y = NULL,
    xmin = NULL,
    xmax = NULL,
    ymin = NULL,
    ymax = NULL,
    colour = NULL,
    linewidth = NULL,
    linetype = NULL,
    element_to = "keep"
) {
  rlang::check_dots_empty()

  if (!is.null(x) && !is.null(y)) {
    rlang::abort(
      "Cannot specify both `x` and `y`. For a segment between two points, use `annotate_axis_segment()`."
    )
  }
  if (!is.null(x) && (!is.null(xmin) || !is.null(xmax))) {
    rlang::abort("Cannot specify both `x` and `xmin`/`xmax`.")
  }
  if (!is.null(y) && (!is.null(ymin) || !is.null(ymax))) {
    rlang::abort("Cannot specify both `y` and `ymin`/`ymax`.")
  }

  use_xy_positioning <- !is.null(x) || !is.null(y)

  if (use_xy_positioning && !is.null(position)) {
    rlang::warn("`position` is ignored when `x` or `y` is provided.")
  }

  if (use_xy_positioning) {
    x_is_normalized <- !is.null(x) && inherits(x, "AsIs")
    y_is_normalized <- !is.null(y) && inherits(y, "AsIs")
    xmin_is_normalized <- !is.null(xmin) && inherits(xmin, "AsIs")
    xmax_is_normalized <- !is.null(xmax) && inherits(xmax, "AsIs")
    ymin_is_normalized <- !is.null(ymin) && inherits(ymin, "AsIs")
    ymax_is_normalized <- !is.null(ymax) && inherits(ymax, "AsIs")

    if (x_is_normalized) {
      x <- unclass(x)
      if (length(x) != 1 || x < 0 || x > 1) {
        rlang::abort("Normalized `x` must be a single value between 0 and 1.")
      }
    } else if (!is.null(x) && length(x) != 1) {
      rlang::abort("`x` must be a single value.")
    }

    if (y_is_normalized) {
      y <- unclass(y)
      if (length(y) != 1 || y < 0 || y > 1) {
        rlang::abort("Normalized `y` must be a single value between 0 and 1.")
      }
    } else if (!is.null(y) && length(y) != 1) {
      rlang::abort("`y` must be a single value.")
    }

    if (xmin_is_normalized) {
      xmin <- unclass(xmin)
      if (length(xmin) != 1 || xmin < 0 || xmin > 1) {
        rlang::abort("Normalized `xmin` must be between 0 and 1.")
      }
    }
    if (xmax_is_normalized) {
      xmax <- unclass(xmax)
      if (length(xmax) != 1 || xmax < 0 || xmax > 1) {
        rlang::abort("Normalized `xmax` must be between 0 and 1.")
      }
    }
    if (ymin_is_normalized) {
      ymin <- unclass(ymin)
      if (length(ymin) != 1 || ymin < 0 || ymin > 1) {
        rlang::abort("Normalized `ymin` must be between 0 and 1.")
      }
    }
    if (ymax_is_normalized) {
      ymax <- unclass(ymax)
      if (length(ymax) != 1 || ymax < 0 || ymax > 1) {
        rlang::abort("Normalized `ymax` must be between 0 and 1.")
      }
    }

    # axis refers to the axis.line theme element, not line direction:
    # a vertical line (x provided) maps to axis.line.y; horizontal to axis.line.x
    axis <- if (!is.null(x)) "y" else "x"

    use_normalized <- x_is_normalized ||
      y_is_normalized ||
      xmin_is_normalized ||
      xmax_is_normalized ||
      ymin_is_normalized ||
      ymax_is_normalized
  } else {
    if (is.null(position)) {
      rlang::abort("Must specify `position`, `x`, or `y`.")
    }
    position <- rlang::arg_match(position, c("top", "bottom", "left", "right"))
    axis <- if (position %in% c("top", "bottom")) "x" else "y"
    use_normalized <- FALSE
  }

  element_to <- rlang::arg_match(element_to, c("keep", "transparent", "blank"))

  # ---- Resolve theme properties ---------------------------------------------

  current_theme <- ggplot2::theme_get()

  element_hierarchy <- if (use_xy_positioning) {
    c(paste0("axis.line.", axis), "axis.line")
  } else {
    c(
      paste0("axis.line.", axis, ".", position),
      paste0("axis.line.", axis),
      "axis.line"
    )
  }

  theme_element_blank <- NULL
  for (nm in element_hierarchy) {
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
    for (nm in element_hierarchy) {
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

  line_colour <- colour %||% resolved_element$colour %||% "black"
  line_linewidth <- if (is.null(linewidth)) {
    resolved_element$linewidth %||% 0.5
  } else if (inherits(linewidth, "rel")) {
    as.numeric(linewidth) * (resolved_element$linewidth %||% 0.5)
  } else {
    linewidth
  }
  line_linetype <- linetype %||% resolved_element$linetype %||% 1

  stamp <- list()

  # ---- Build annotation -----------------------------------------------------

  if (use_xy_positioning) {
    if (!is.null(x)) {
      if (is.null(ymin)) ymin <- if (use_normalized) 0 else -Inf
      if (is.null(ymax)) ymax <- if (use_normalized) 1 else Inf

      if (use_normalized) {
        line_grob <- grid::linesGrob(
          x = grid::unit(c(x, x), "npc"),
          y = grid::unit(c(ymin, ymax), "npc"),
          gp = ggplot2::gg_par(
            col = line_colour,
            stroke = line_linewidth,
            lty = line_linetype,
            lineend = "butt"
          )
        )
        stamp <- c(stamp, list(ggplot2::annotation_custom(
          line_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
        )))
      } else {
        stamp <- c(stamp, list(ggplot2::annotate(
          "segment",
          x = x, xend = x, y = ymin, yend = ymax,
          colour = line_colour, linewidth = line_linewidth, linetype = line_linetype
        )))
      }
    } else {
      if (is.null(xmin)) xmin <- if (use_normalized) 0 else -Inf
      if (is.null(xmax)) xmax <- if (use_normalized) 1 else Inf

      if (use_normalized) {
        line_grob <- grid::linesGrob(
          x = grid::unit(c(xmin, xmax), "npc"),
          y = grid::unit(c(y, y), "npc"),
          gp = ggplot2::gg_par(
            col = line_colour,
            stroke = line_linewidth,
            lty = line_linetype,
            lineend = "butt"
          )
        )
        stamp <- c(stamp, list(ggplot2::annotation_custom(
          line_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
        )))
      } else {
        stamp <- c(stamp, list(ggplot2::annotate(
          "segment",
          x = xmin, xend = xmax, y = y, yend = y,
          colour = line_colour, linewidth = line_linewidth, linetype = line_linetype
        )))
      }
    }
  } else {
    seg_args <- list(
      colour = line_colour,
      linewidth = line_linewidth,
      linetype = line_linetype
    )
    stamp <- c(stamp, list(rlang::exec(
      ggplot2::annotate,
      "segment",
      x    = if (position %in% c("top", "bottom")) { if (!is.null(xmin)) xmin else -Inf } else { if (position == "left") -Inf else Inf },
      xend = if (position %in% c("top", "bottom")) { if (!is.null(xmax)) xmax else Inf  } else { if (position == "left") -Inf else Inf },
      y    = if (position %in% c("left", "right")) { if (!is.null(ymin)) ymin else -Inf } else { if (position == "bottom") -Inf else Inf },
      yend = if (position %in% c("left", "right")) { if (!is.null(ymax)) ymax else Inf  } else { if (position == "bottom") -Inf else Inf },
      !!!seg_args
    )))
  }

  # ---- Theme modification ---------------------------------------------------

  if (element_to != "keep") {
    theme_name <- NULL

    if (use_xy_positioning) {
      if (!is.null(x)) {
        if (x_is_normalized) {
          if (x == 0) theme_name <- "axis.line.y.left"
          else if (x == 1) theme_name <- "axis.line.y.right"
        } else if (is.infinite(x)) {
          theme_name <- if (x < 0) "axis.line.y.left" else "axis.line.y.right"
        }
      } else if (!is.null(y)) {
        if (y_is_normalized) {
          if (y == 0) theme_name <- "axis.line.x.bottom"
          else if (y == 1) theme_name <- "axis.line.x.top"
        } else if (is.infinite(y)) {
          theme_name <- if (y < 0) "axis.line.x.bottom" else "axis.line.x.top"
        }
      }
    } else {
      theme_name <- paste0("axis.line.", axis, ".", position)
    }

    if (!is.null(theme_name)) {
      theme_mod <- list()
      theme_mod[[theme_name]] <- if (element_to == "transparent") {
        ggplot2::element_line(colour = "transparent")
      } else {
        ggplot2::element_blank()
      }
      stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_mod)))
    }
  }

  return(stamp)
}


# annotate_axis_segment -------------------------------------------------------

#' Annotate a straight segment between two data points
#'
#' Draws a straight line segment between `(x, y)` and `(xend, yend)`, with
#' style defaults taken from the `axis.line` element of the set theme. For a
#' curved line, see [annotate_axis_curve()]. For axis-edge lines, see
#' [annotate_axis_line()].
#'
#' @param ... Not used. Allows trailing commas and named-argument style calls.
#' @param x,y Start coordinates of the segment.
#' @param xend,yend End coordinates of the segment.
#' @param colour Inherits from `axis.line` in the set theme.
#' @param linewidth Inherits from `axis.line` in the set theme. Supports
#'   `rel()`.
#' @param linetype Inherits from `axis.line` in the set theme.
#'
#' @return A list containing a single ggplot2 annotation layer.
#' @seealso [annotate_axis_line()], [annotate_axis_curve()]
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' set_theme(
#'   ggrefine::theme_grey(
#'     panel_heights = rep(unit(50, "mm"), 100),
#'     panel_widths = rep(unit(75, "mm"), 100),
#'   )
#' )
#'
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   coord_cartesian(clip = "off")
#'
#' # Segment between two data points
#' p + annotate_axis_segment(x = 2, y = 15, xend = 5, yend = 30)
#'
#' # Thicker segment
#' p + annotate_axis_segment(x = 2, y = 15, xend = 5, yend = 30, linewidth = rel(2))
annotate_axis_segment <- function(
    ...,
    x,
    y,
    xend,
    yend,
    colour = NULL,
    linewidth = NULL,
    linetype = NULL
) {
  rlang::check_dots_empty()

  props <- .resolve_axis_line_element(colour, linewidth, linetype)

  list(ggplot2::annotate(
    "segment",
    x = x, y = y, xend = xend, yend = yend,
    colour = props$colour,
    linewidth = props$linewidth,
    linetype = props$linetype
  ))
}


# annotate_axis_curve ---------------------------------------------------------

#' Annotate a curved line between two data points
#'
#' Draws a curved line between `(x, y)` and `(xend, yend)`, with style
#' defaults taken from the `axis.line` element of the set theme. For a straight
#' segment, see [annotate_axis_segment()]. For axis-edge lines, see
#' [annotate_axis_line()].
#'
#' @param ... Not used. Allows trailing commas and named-argument style calls.
#' @param x,y Start coordinates of the curve.
#' @param xend,yend End coordinates of the curve.
#' @param curvature Amount of curvature. Negative values curve left, positive
#'   curve right, zero is straight. Defaults to `0.3`.
#' @param angle Skew angle of the curve control points (0–180). Defaults to
#'   `90`.
#' @param ncp Number of control points. Higher values produce smoother curves.
#'   Defaults to `5`.
#' @param colour Inherits from `axis.line` in the set theme.
#' @param linewidth Inherits from `axis.line` in the set theme. Supports
#'   `rel()`.
#' @param linetype Inherits from `axis.line` in the set theme.
#'
#' @return A list containing a single ggplot2 annotation layer.
#' @seealso [annotate_axis_line()], [annotate_axis_segment()]
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' set_theme(
#'   ggrefine::theme_grey(
#'     panel_heights = rep(unit(50, "mm"), 100),
#'     panel_widths = rep(unit(75, "mm"), 100),
#'   )
#' )
#'
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   coord_cartesian(clip = "off")
#'
#' # Gentle curve between two data points
#' p + annotate_axis_curve(x = 2, y = 15, xend = 5, yend = 30, curvature = 0.3)
#'
#' # Sharp curve in the opposite direction
#' p + annotate_axis_curve(x = 2, y = 15, xend = 5, yend = 30, curvature = -0.6)
annotate_axis_curve <- function(
    ...,
    x,
    y,
    xend,
    yend,
    curvature = 0.3,
    angle = 90,
    ncp = 5,
    colour = NULL,
    linewidth = NULL,
    linetype = NULL
) {
  rlang::check_dots_empty()

  props <- .resolve_axis_line_element(colour, linewidth, linetype)

  list(ggplot2::annotate(
    "curve",
    x = x, y = y, xend = xend, yend = yend,
    curvature = curvature,
    angle = angle,
    ncp = ncp,
    colour = props$colour,
    linewidth = props$linewidth,
    linetype = props$linetype
  ))
}
