# annotate_axis_line ----------------------------------------------------------

#' Annotate an axis line
#'
#' Draws a line along an axis edge or between two arbitrary points, with style
#' defaults taken from the `axis.line` element of the set theme. Requires
#' `coord_cartesian(clip = "off")`.
#'
#' Operates in two modes:
#' - **Axis line mode**: triggered by `position`, `x`, or `y` alone.
#' - **Segment/curve mode**: triggered when `x`, `y`, `xend`, and `yend` are
#'   all provided. Pass `curvature` to draw a curve instead of a straight line.
#'
#' @param ... Not used. Allows trailing commas and named-argument style calls.
#' @param position One of `"top"`, `"bottom"`, `"left"`, or `"right"`. Axis
#'   line mode only.
#' @param x In axis line mode, a single x value for a vertical line. In
#'   segment/curve mode, the x start position. Use `I()` for normalized
#'   coordinates (0-1).
#' @param y In axis line mode, a single y value for a horizontal line. In
#'   segment/curve mode, the y start position. Use `I()` for normalized
#'   coordinates (0-1).
#' @param xend,yend End position of the segment or curve. Providing all of
#'   `x`, `y`, `xend`, `yend` triggers segment/curve mode.
#' @param xmin,xmax Start and end x positions for a horizontal axis line. Use
#'   `I()` for normalized coordinates (0-1). Axis line mode only.
#' @param ymin,ymax Start and end y positions for a vertical axis line. Use
#'   `I()` for normalized coordinates (0-1). Axis line mode only.
#' @param curvature Amount of curvature. Negative curves left, positive curves
#'   right, zero is straight. `NULL` (default) draws a straight segment.
#' @param angle Skew angle of curve control points (0-180). Used only when
#'   `curvature` is non-`NULL`. Defaults to `90`.
#' @param ncp Number of curve control points. Higher values give smoother
#'   curves. Used only when `curvature` is non-`NULL`. Defaults to `5`.
#' @param colour Inherits from `axis.line` in the set theme.
#' @param linewidth Inherits from `axis.line` in the set theme. Supports
#'   `rel()`.
#' @param linetype Inherits from `axis.line` in the set theme.
#' @param element_to One of `"keep"`, `"transparent"`, or `"blank"`. Controls
#'   whether the native theme axis line is suppressed. Defaults to `"keep"`.
#'   Axis line mode only.
#'
#' @return A list of ggplot2 annotation layers and theme elements.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' set_theme(theme_classic())
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
#' # Vertical rule at x = 3.5
#' p + annotate_axis_line(x = 3.5)
#'
#' # Straight line between two data points
#' p + annotate_axis_line(x = 2, y = 15, xend = 5, yend = 30)
#'
#' # Curved line between two data points
#' p + annotate_axis_line(x = 2, y = 15, xend = 5, yend = 30, curvature = 0.3)
annotate_axis_line <- function(
    ...,
    position  = NULL,
    x         = NULL,
    y         = NULL,
    xend      = NULL,
    yend      = NULL,
    xmin      = NULL,
    xmax      = NULL,
    ymin      = NULL,
    ymax      = NULL,
    curvature = NULL,
    angle     = 90,
    ncp       = 5,
    colour    = NULL,
    linewidth = NULL,
    linetype  = NULL,
    element_to = "keep"
) {
  rlang::check_dots_empty()

  # ---- Mode detection -------------------------------------------------------

  segment_mode <- !is.null(x) && !is.null(y) && !is.null(xend) && !is.null(yend)

  # ---- Segment / curve mode -------------------------------------------------

  if (segment_mode) {
    current_theme <- ggplot2::theme_get()

    theme_element_blank <- c("axis.line.x", "axis.line") |>
      purrr::map(\(nm) ggplot2::calc_element(nm, current_theme, skip_blank = FALSE)) |>
      purrr::detect(\(el) !is.null(el))
    axis_line_intentionally_blank <- is.null(theme_element_blank) || inherits(theme_element_blank, "element_blank")

    resolved_element <- if (axis_line_intentionally_blank) NULL else {
      c("axis.line.x", "axis.line") |>
        purrr::map(\(nm) ggplot2::calc_element(nm, current_theme, skip_blank = TRUE)) |>
        purrr::detect(\(el) !is.null(el) && !inherits(el, "element_blank"))
    }

    if (is.null(colour)    && (axis_line_intentionally_blank || is.null(resolved_element$colour)))    rlang::warn("The set theme does not define an `axis.line` colour. Defaulting to \"black\".")
    if (is.null(linewidth) && (axis_line_intentionally_blank || is.null(resolved_element$linewidth))) rlang::warn("The set theme does not define an `axis.line` linewidth. Defaulting to `0.5`.")

    if (is.null(resolved_element)) resolved_element <- list(colour = "black", linewidth = 0.5, linetype = 1)
    line_colour <- colour %||% resolved_element$colour %||% "black"

    line_linewidth <- if (is.null(linewidth)) {
      resolved_element$linewidth %||% 0.5
    } else if (inherits(linewidth, "rel")) {
      as.numeric(linewidth) * (resolved_element$linewidth %||% 0.5)
    } else {
      linewidth
    }

    line_linetype <- linetype %||% resolved_element$linetype %||% 1

    geom_type  <- if (!is.null(curvature)) "curve" else "segment"
    extra_args <- if (!is.null(curvature)) list(curvature = curvature, angle = angle, ncp = ncp) else list()

    return(list(
      rlang::exec(
        ggplot2::annotate,
        geom_type,
        x         = x,
        y         = y,
        xend      = xend,
        yend      = yend,
        colour    = line_colour,
        linewidth = line_linewidth,
        linetype  = line_linetype,
        !!!extra_args
      )
    ))
  }

  # ---- Axis line mode -------------------------------------------------------

  if (!is.null(x) && !is.null(y)) {
    rlang::abort("Cannot specify both x and y. To draw a segment, also provide xend and yend.")
  }
  if (!is.null(x) && (!is.null(xmin) || !is.null(xmax))) {
    rlang::abort("Cannot specify both x and xmin/xmax.")
  }
  if (!is.null(y) && (!is.null(ymin) || !is.null(ymax))) {
    rlang::abort("Cannot specify both y and ymin/ymax.")
  }

  use_xy_positioning <- !is.null(x) || !is.null(y)

  if (use_xy_positioning && !is.null(position)) {
    rlang::warn("`position` is ignored when `x` or `y` is provided.")
  }

  if (use_xy_positioning) {
    x_is_normalized    <- !is.null(x)    && inherits(x,    "AsIs")
    y_is_normalized    <- !is.null(y)    && inherits(y,    "AsIs")
    xmin_is_normalized <- !is.null(xmin) && inherits(xmin, "AsIs")
    xmax_is_normalized <- !is.null(xmax) && inherits(xmax, "AsIs")
    ymin_is_normalized <- !is.null(ymin) && inherits(ymin, "AsIs")
    ymax_is_normalized <- !is.null(ymax) && inherits(ymax, "AsIs")

    if (x_is_normalized) {
      x <- unclass(x)
      if (length(x) != 1 || x < 0 || x > 1) rlang::abort("Normalized x must be a single value between 0 and 1.")
    } else if (!is.null(x) && length(x) != 1) {
      rlang::abort("x must be a single value.")
    }

    if (y_is_normalized) {
      y <- unclass(y)
      if (length(y) != 1 || y < 0 || y > 1) rlang::abort("Normalized y must be a single value between 0 and 1.")
    } else if (!is.null(y) && length(y) != 1) {
      rlang::abort("y must be a single value.")
    }

    if (xmin_is_normalized) { xmin <- unclass(xmin); if (length(xmin) != 1 || xmin < 0 || xmin > 1) rlang::abort("Normalized xmin must be between 0 and 1.") }
    if (xmax_is_normalized) { xmax <- unclass(xmax); if (length(xmax) != 1 || xmax < 0 || xmax > 1) rlang::abort("Normalized xmax must be between 0 and 1.") }
    if (ymin_is_normalized) { ymin <- unclass(ymin); if (length(ymin) != 1 || ymin < 0 || ymin > 1) rlang::abort("Normalized ymin must be between 0 and 1.") }
    if (ymax_is_normalized) { ymax <- unclass(ymax); if (length(ymax) != 1 || ymax < 0 || ymax > 1) rlang::abort("Normalized ymax must be between 0 and 1.") }

    # axis refers to the axis.line theme element, not line direction:
    # a vertical line (x provided) maps to axis.line.y; horizontal to axis.line.x
    axis <- if (!is.null(x)) "y" else "x"

    use_normalized <- x_is_normalized || y_is_normalized ||
      xmin_is_normalized || xmax_is_normalized ||
      ymin_is_normalized || ymax_is_normalized

  } else {
    if (is.null(position)) rlang::abort("Must specify either position, x, or y.")
    position       <- rlang::arg_match(position, c("top", "bottom", "left", "right"))
    axis           <- if (position %in% c("top", "bottom")) "x" else "y"
    use_normalized <- FALSE
  }

  element_to <- rlang::arg_match(element_to, c("keep", "transparent", "blank"))

  # ---- Resolve theme properties ---------------------------------------------

  current_theme <- ggplot2::theme_get()

  element_hierarchy <- if (use_xy_positioning) {
    c(paste0("axis.line.", axis), "axis.line")
  } else {
    c(paste0("axis.line.", axis, ".", position), paste0("axis.line.", axis), "axis.line")
  }

  theme_element_blank <- element_hierarchy |>
    purrr::map(\(x) ggplot2::calc_element(x, current_theme, skip_blank = FALSE)) |>
    purrr::detect(\(x) !is.null(x))
  axis_line_intentionally_blank <- is.null(theme_element_blank) || inherits(theme_element_blank, "element_blank")

  resolved_element <- if (axis_line_intentionally_blank) NULL else {
    element_hierarchy |>
      purrr::map(\(x) ggplot2::calc_element(x, current_theme, skip_blank = TRUE)) |>
      purrr::detect(\(x) !is.null(x) && !inherits(x, "element_blank"))
  }

  if (is.null(colour)    && (axis_line_intentionally_blank || is.null(resolved_element$colour)))    rlang::warn("The set theme does not define an `axis.line` colour. Defaulting to \"black\".")
  if (is.null(linewidth) && (axis_line_intentionally_blank || is.null(resolved_element$linewidth))) rlang::warn("The set theme does not define an `axis.line` linewidth. Defaulting to `0.5`.")

  if (is.null(resolved_element)) resolved_element <- list(colour = "black", linewidth = 0.5, linetype = 1)
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
          x  = grid::unit(c(x, x),       "npc"),
          y  = grid::unit(c(ymin, ymax), "npc"),
          gp = grid::gpar(col = line_colour, lwd = line_linewidth * 72 / 25.4, lty = line_linetype, lineend = "butt")
        )
        stamp <- c(stamp, list(ggplot2::annotation_custom(line_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)))
      } else {
        stamp <- c(stamp, list(rlang::exec(ggplot2::annotate, "segment", x = x, xend = x, y = ymin, yend = ymax, colour = line_colour, linewidth = line_linewidth, linetype = line_linetype)))
      }

    } else {
      if (is.null(xmin)) xmin <- if (use_normalized) 0 else -Inf
      if (is.null(xmax)) xmax <- if (use_normalized) 1 else Inf

      if (use_normalized) {
        line_grob <- grid::linesGrob(
          x  = grid::unit(c(xmin, xmax), "npc"),
          y  = grid::unit(c(y, y),       "npc"),
          gp = grid::gpar(col = line_colour, lwd = line_linewidth * 72 / 25.4, lty = line_linetype, lineend = "butt")
        )
        stamp <- c(stamp, list(ggplot2::annotation_custom(line_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)))
      } else {
        stamp <- c(stamp, list(rlang::exec(ggplot2::annotate, "segment", x = xmin, xend = xmax, y = y, yend = y, colour = line_colour, linewidth = line_linewidth, linetype = line_linetype)))
      }
    }

  } else {
    if (position == "bottom") {
      stamp <- c(stamp, list(rlang::exec(ggplot2::annotate, "segment", x = if (!is.null(xmin)) xmin else -Inf, xend = if (!is.null(xmax)) xmax else Inf, y = -Inf, yend = -Inf, colour = line_colour, linewidth = line_linewidth, linetype = line_linetype)))
    } else if (position == "top") {
      stamp <- c(stamp, list(rlang::exec(ggplot2::annotate, "segment", x = if (!is.null(xmin)) xmin else -Inf, xend = if (!is.null(xmax)) xmax else Inf, y = Inf, yend = Inf, colour = line_colour, linewidth = line_linewidth, linetype = line_linetype)))
    } else if (position == "left") {
      stamp <- c(stamp, list(rlang::exec(ggplot2::annotate, "segment", x = -Inf, xend = -Inf, y = if (!is.null(ymin)) ymin else -Inf, yend = if (!is.null(ymax)) ymax else Inf, colour = line_colour, linewidth = line_linewidth, linetype = line_linetype)))
    } else {
      stamp <- c(stamp, list(rlang::exec(ggplot2::annotate, "segment", x = Inf, xend = Inf, y = if (!is.null(ymin)) ymin else -Inf, yend = if (!is.null(ymax)) ymax else Inf, colour = line_colour, linewidth = line_linewidth, linetype = line_linetype)))
    }
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
      theme_mod[[theme_name]] <- if (element_to == "transparent") ggplot2::element_line(colour = "transparent") else ggplot2::element_blank()
      stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_mod)))
    }
  }

  return(stamp)
}


# annotate_axis_ticks ---------------------------------------------------------

#' Annotate axis ticks
#'
#' Draws axis ticks at specified break positions, with style defaults taken
#' from the `axis.ticks` element of the set theme. Requires
#' `coord_cartesian(clip = "off")`.
#'
#' @param ... Not used. Allows trailing commas and named-argument style calls.
#' @param position One of `"top"`, `"bottom"`, `"left"`, or `"right"`.
#' @param x A vector of x-axis break positions for top/bottom ticks. Use `I()`
#'   for normalized coordinates (0-1).
#' @param y A vector of y-axis break positions for left/right ticks. Use `I()`
#'   for normalized coordinates (0-1).
#' @param minor Logical. If `TRUE`, uses minor tick theme defaults. Defaults to
#'   `FALSE`.
#' @param colour Inherits from `axis.ticks` in the set theme.
#' @param linewidth Inherits from `axis.ticks` in the set theme. Supports
#'   `rel()`.
#' @param tick_length Total tick length as a grid unit. Supports `rel()` to scale
#'   relative to the theme default. Negative values flip the tick direction.
#' @param element_to One of `"keep"`, `"transparent"`, or `"blank"`. Controls
#'   whether native theme ticks are suppressed. Defaults to `"keep"`.
#'
#' @return A list of ggplot2 annotation layers and theme elements.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' set_theme(theme_classic())
#'
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   coord_cartesian(clip = "off")
#'
#' # Bottom ticks at specific breaks
#' p + annotate_axis_ticks(position = "bottom", x = c(2, 3, 4, 5))
#'
#' # Left ticks with native ticks suppressed
#' p + annotate_axis_ticks(position = "left", y = c(10, 20, 30), element_to = "transparent")
#'
#' # Inward ticks using a negative length
#' p + annotate_axis_ticks(position = "bottom", x = c(2, 3, 4, 5), tick_length = grid::unit(-5, "pt"))
#'
#' # Minor ticks
#' p + annotate_axis_ticks(position = "bottom", x = seq(2, 5, by = 0.5), minor = TRUE)
annotate_axis_ticks <- function(
    ...,
    position  = NULL,
    x         = NULL,
    y         = NULL,
    minor     = FALSE,
    colour    = NULL,
    linewidth = NULL,
    tick_length = NULL,
    element_to = "keep"
) {
  rlang::check_dots_empty()

  if (is.null(position)) {
    if (!is.null(x) && !is.null(y)) {
      rlang::abort("Cannot specify both x and y.")
    }
    if (!is.null(x)) {
      position <- "bottom"
    } else if (!is.null(y)) {
      position <- "left"
    } else {
      rlang::abort("Must specify either position, x, or y.")
    }
  }

  position <- rlang::arg_match(position, c("top", "bottom", "left", "right"))

  x_is_normalized <- !is.null(x) && inherits(x, "AsIs")
  y_is_normalized <- !is.null(y) && inherits(y, "AsIs")

  if (x_is_normalized) {
    x <- unclass(x)
    if (any(x < 0 | x > 1)) rlang::abort("Normalized x coordinates must be between 0 and 1.")
  }
  if (y_is_normalized) {
    y <- unclass(y)
    if (any(y < 0 | y > 1)) rlang::abort("Normalized y coordinates must be between 0 and 1.")
  }

  if (position %in% c("top", "bottom")) {
    if (!is.null(y)) rlang::abort("position = 'top' or 'bottom' expects `x`, not `y`.")
    if (is.null(x))  rlang::abort("position = 'top' or 'bottom' requires `x` to be specified.")
    use_normalized <- x_is_normalized
  } else {
    if (!is.null(x)) rlang::abort("position = 'left' or 'right' expects `y`, not `x`.")
    if (is.null(y))  rlang::abort("position = 'left' or 'right' requires `y` to be specified.")
    use_normalized <- y_is_normalized
  }

  element_to <- rlang::arg_match(element_to, c("keep", "transparent", "blank"))

  axis   <- if (position %in% c("top", "bottom")) "x" else "y"
  breaks <- if (!is.null(x)) x else y

  if (base::length(breaks) == 0) return(list())

  current_theme <- ggplot2::theme_get()

  # ---- Resolve tick element -------------------------------------------------

  tick_hierarchy <- if (minor) {
    c(
      paste0("axis.minor.ticks.", axis, ".", position),
      paste0("axis.ticks.", axis, ".", position),
      paste0("axis.ticks.", axis),
      "axis.ticks"
    )
  } else {
    c(
      paste0("axis.ticks.", axis, ".", position),
      paste0("axis.ticks.", axis),
      "axis.ticks"
    )
  }

  resolved_tick_element <- NULL
  tick_intentionally_blank <- FALSE
  for (nm in tick_hierarchy) {
    el_raw <- ggplot2::calc_element(nm, current_theme, skip_blank = FALSE)
    if (!is.null(el_raw)) {
      if (inherits(el_raw, "element_blank")) { tick_intentionally_blank <- TRUE; break }
      el <- ggplot2::calc_element(nm, current_theme, skip_blank = TRUE)
      if (!is.null(el) && !inherits(el, "element_blank")) { resolved_tick_element <- el; break }
    }
  }

  if (is.null(colour)    && (tick_intentionally_blank || is.null(resolved_tick_element$colour)))    rlang::warn("The set theme does not define an `axis.ticks` colour. Defaulting to \"black\".")
  if (is.null(linewidth) && (tick_intentionally_blank || is.null(resolved_tick_element$linewidth))) rlang::warn("The set theme does not define an `axis.ticks` linewidth. Defaulting to `0.5`.")

  if (is.null(resolved_tick_element)) resolved_tick_element <- list(colour = "black", linewidth = 0.5)

  length_hierarchy <- if (minor) {
    c(
      paste0("axis.minor.ticks.length.", axis, ".", position),
      paste0("axis.minor.ticks.length.", axis),
      "axis.minor.ticks.length",
      paste0("axis.ticks.length.", axis, ".", position),
      paste0("axis.ticks.length.", axis),
      "axis.ticks.length"
    )
  } else {
    c(
      paste0("axis.ticks.length.", axis, ".", position),
      paste0("axis.ticks.length.", axis),
      "axis.ticks.length"
    )
  }

  resolved_length_element <- NULL
  for (nm in length_hierarchy) {
    el <- ggplot2::calc_element(nm, current_theme, skip_blank = TRUE)
    if (!is.null(el) && !inherits(el, "element_blank")) { resolved_length_element <- el; break }
  }

  # ---- Extract properties ---------------------------------------------------

  tick_colour <- colour %||% resolved_tick_element$colour %||% "black"

  tick_linewidth <- if (is.null(linewidth)) {
    resolved_tick_element$linewidth %||% 0.5
  } else if (inherits(linewidth, "rel")) {
    as.numeric(linewidth) * (resolved_tick_element$linewidth %||% 0.5)
  } else {
    linewidth
  }

  calculate_default_length <- function() {
    tl <- resolved_length_element
    if (is.null(tl)) {
      return(grid::unit((if (minor) 0.375 else 0.5) * (current_theme$text$size %||% 11), "pt"))
    } else if (inherits(tl, "rel")) {
      spacing <- current_theme$spacing %||% grid::unit(5.5, "pt")
      spacing_pts <- as.numeric(grid::convertUnit(if (inherits(spacing, "unit")) spacing else grid::unit(5.5, "pt"), "pt"))
      return(grid::unit(as.numeric(tl) * spacing_pts, "pt"))
    } else if (!inherits(tl, "unit")) {
      return(grid::unit(if (is.numeric(tl)) tl else (if (minor) 0.375 else 0.5) * (current_theme$text$size %||% 11), "pt"))
    } else {
      return(tl)
    }
  }

  flip_direction <- FALSE

  if (is.null(tick_length)) {
    tick_length <- calculate_default_length()
  } else if (inherits(tick_length, "rel")) {
    rel_value   <- as.numeric(tick_length)
    default_pts <- as.numeric(grid::convertUnit(calculate_default_length(), "pt"))
    tick_length <- grid::unit(abs(rel_value) * default_pts, "pt")
    flip_direction <- rel_value < 0
  } else if (inherits(tick_length, "unit")) {
    tick_pts    <- as.numeric(grid::convertUnit(tick_length, "pt"))
    tick_length <- grid::unit(abs(tick_pts), "pt")
    flip_direction <- tick_pts < 0
  } else if (is.numeric(tick_length)) {
    flip_direction <- tick_length < 0
    tick_length    <- grid::unit(abs(tick_length), "pt")
  } else {
    tick_length <- calculate_default_length()
  }

  stamp <- list()

  # ---- Theme modification ---------------------------------------------------

  if (element_to != "keep") {
    theme_name <- if (minor) paste0("axis.minor.ticks.", axis, ".", position) else paste0("axis.ticks.", axis, ".", position)
    theme_mod  <- list()
    theme_mod[[theme_name]] <- if (element_to == "transparent") ggplot2::element_line(colour = "transparent") else ggplot2::element_blank()
    stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_mod)))
  }

  # ---- Build tick annotations -----------------------------------------------

  tick_annotations <- breaks |>
    purrr::map(\(break_val) {
      gp <- grid::gpar(col = tick_colour, lwd = tick_linewidth * 72 / 25.4, lineend = "butt")

      tick_grob <- if (use_normalized) {
        if (position == "bottom") {
          grid::segmentsGrob(x0 = grid::unit(break_val, "npc"), x1 = grid::unit(break_val, "npc"), y0 = grid::unit(0, "npc"), y1 = if (flip_direction) grid::unit(0, "npc") + tick_length else grid::unit(0, "npc") - tick_length, gp = gp)
        } else if (position == "top") {
          grid::segmentsGrob(x0 = grid::unit(break_val, "npc"), x1 = grid::unit(break_val, "npc"), y0 = grid::unit(1, "npc"), y1 = if (flip_direction) grid::unit(1, "npc") - tick_length else grid::unit(1, "npc") + tick_length, gp = gp)
        } else if (position == "left") {
          grid::segmentsGrob(x0 = grid::unit(0, "npc"), x1 = if (flip_direction) grid::unit(0, "npc") + tick_length else grid::unit(0, "npc") - tick_length, y0 = grid::unit(break_val, "npc"), y1 = grid::unit(break_val, "npc"), gp = gp)
        } else {
          grid::segmentsGrob(x0 = grid::unit(1, "npc"), x1 = if (flip_direction) grid::unit(1, "npc") - tick_length else grid::unit(1, "npc") + tick_length, y0 = grid::unit(break_val, "npc"), y1 = grid::unit(break_val, "npc"), gp = gp)
        }
      } else {
        if (position == "bottom") {
          grid::segmentsGrob(x0 = grid::unit(0.5, "npc"), x1 = grid::unit(0.5, "npc"), y0 = grid::unit(0, "npc"), y1 = if (flip_direction) grid::unit(0, "npc") + tick_length else grid::unit(0, "npc") - tick_length, gp = gp)
        } else if (position == "top") {
          grid::segmentsGrob(x0 = grid::unit(0.5, "npc"), x1 = grid::unit(0.5, "npc"), y0 = grid::unit(1, "npc"), y1 = if (flip_direction) grid::unit(1, "npc") - tick_length else grid::unit(1, "npc") + tick_length, gp = gp)
        } else if (position == "left") {
          grid::segmentsGrob(x0 = grid::unit(0, "npc"), x1 = if (flip_direction) grid::unit(0, "npc") + tick_length else grid::unit(0, "npc") - tick_length, y0 = grid::unit(0.5, "npc"), y1 = grid::unit(0.5, "npc"), gp = gp)
        } else {
          grid::segmentsGrob(x0 = grid::unit(1, "npc"), x1 = if (flip_direction) grid::unit(1, "npc") - tick_length else grid::unit(1, "npc") + tick_length, y0 = grid::unit(0.5, "npc"), y1 = grid::unit(0.5, "npc"), gp = gp)
        }
      }

      annotation_position <- if (use_normalized) {
        list(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
      } else if (axis == "x") {
        if (position == "bottom") list(xmin = break_val, xmax = break_val, ymin = -Inf, ymax = -Inf)
        else                      list(xmin = break_val, xmax = break_val, ymin =  Inf, ymax =  Inf)
      } else {
        if (position == "left")   list(xmin = -Inf, xmax = -Inf, ymin = break_val, ymax = break_val)
        else                      list(xmin =  Inf, xmax =  Inf, ymin = break_val, ymax = break_val)
      }

      rlang::exec(ggplot2::annotation_custom, grob = tick_grob, !!!annotation_position)
    })

  c(stamp, tick_annotations)
}


# annotate_axis_text ----------------------------------------------------------

#' Annotate axis text
#'
#' Draws text labels at specified break positions along an axis, or at
#' arbitrary (x, y) coordinates. Style defaults are taken from the `axis.text`
#' element of the set theme. Requires `coord_cartesian(clip = "off")`.
#'
#' When only `x` or only `y` is provided, the function operates in axis mode
#' and labels are placed relative to the relevant axis edge. When both `x` and
#' `y` are provided, labels are placed at those exact coordinates.
#'
#' @param ... Not used. Allows trailing commas and named-argument style calls.
#' @param position One of `"top"`, `"bottom"`, `"left"`, or `"right"`. Inferred
#'   from `x`/`y` if not provided.
#' @param x A vector of x positions. Use `I()` for normalized coordinates (0-1).
#'   When combined with `y`, triggers arbitrary positioning mode.
#' @param y A vector of y positions. Use `I()` for normalized coordinates (0-1).
#'   When combined with `x`, triggers arbitrary positioning mode.
#' @param label A vector of labels or a function that takes breaks and returns
#'   labels. Defaults to formatted break values.
#' @param colour Inherits from `axis.text` in the set theme.
#' @param size Inherits from `axis.text` in the set theme.
#' @param family Inherits from `axis.text` in the set theme.
#' @param tick_length Offset from the axis edge as a grid unit, including tick length
#'   and margin. Supports `rel()`. Negative values place labels on the inside of
#'   the panel. Axis mode only.
#' @param hjust,vjust Justification. Auto-calculated from position if `NULL`.
#' @param angle Text rotation angle. Defaults to `0`.
#' @param element_to One of `"keep"`, `"transparent"`, or `"blank"`. Controls
#'   whether native theme axis text is suppressed. Defaults to `"keep"`. Axis
#'   mode only.
#'
#' @return A list of ggplot2 annotation layers and theme elements.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' set_theme(theme_classic())
#'
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   coord_cartesian(clip = "off")
#'
#' # Bottom axis labels at specific breaks
#' p + annotate_axis_text(position = "bottom", x = c(2, 3, 4, 5))
#'
#' # Custom labels
#' p + annotate_axis_text(position = "bottom", x = c(2, 3, 4, 5),
#'                        label = c("two", "three", "four", "five"))
#'
#' # Inward labels using negative length
#' p + annotate_axis_text(position = "bottom", x = c(2, 3, 4, 5),
#'                        tick_length = grid::unit(-15, "pt"))
#'
#' # Arbitrary positioning — label a specific point on the plot
#' p + annotate_axis_text(x = 3.215, y = 21.4, label = "this one")
annotate_axis_text <- function(
    ...,
    position = NULL,
    x        = NULL,
    y        = NULL,
    label    = NULL,
    colour   = NULL,
    size     = NULL,
    family   = NULL,
    tick_length = NULL,
    hjust    = NULL,
    vjust    = NULL,
    angle    = 0,
    element_to = "keep"
) {
  rlang::check_dots_empty()

  arbitrary_position <- !is.null(x) && !is.null(y)

  if (!arbitrary_position) {
    if (is.null(position)) {
      if (!is.null(x))      position <- "bottom"
      else if (!is.null(y)) position <- "left"
      else                  rlang::abort("Must specify either position, x, y, or both x and y.")
    }
    position <- rlang::arg_match(position, c("top", "bottom", "left", "right"))
  }

  x_is_normalized <- !is.null(x) && inherits(x, "AsIs")
  y_is_normalized <- !is.null(y) && inherits(y, "AsIs")

  if (x_is_normalized) {
    x <- unclass(x)
    if (any(x < 0 | x > 1)) rlang::abort("Normalized x coordinates must be between 0 and 1.")
  }
  if (y_is_normalized) {
    y <- unclass(y)
    if (any(y < 0 | y > 1)) rlang::abort("Normalized y coordinates must be between 0 and 1.")
  }

  if (arbitrary_position) {
    if (base::length(x) != base::length(y)) rlang::abort("x and y must have the same length when both are specified.")
    use_normalized <- x_is_normalized || y_is_normalized
    axis           <- "x"
    breaks         <- list(x = x, y = y)
  } else {
    if (position %in% c("top", "bottom")) {
      if (!is.null(y)) rlang::abort("position = 'top' or 'bottom' expects `x`, not `y`.")
      if (is.null(x))  rlang::abort("position = 'top' or 'bottom' requires `x` to be specified.")
      use_normalized <- x_is_normalized
    } else {
      if (!is.null(x)) rlang::abort("position = 'left' or 'right' expects `y`, not `x`.")
      if (is.null(y))  rlang::abort("position = 'left' or 'right' requires `y` to be specified.")
      use_normalized <- y_is_normalized
    }
    axis   <- if (position %in% c("top", "bottom")) "x" else "y"
    breaks <- if (!is.null(x)) x else y
  }

  element_to <- rlang::arg_match(element_to, c("keep", "transparent", "blank"))

  current_theme <- ggplot2::theme_get()

  n_breaks <- if (arbitrary_position) base::length(breaks$x) else base::length(breaks)
  if (n_breaks == 0) return(list())

  # ---- Resolve text element -------------------------------------------------

  text_hierarchy <- if (arbitrary_position) {
    c("axis.text.x", "axis.text")
  } else {
    c(paste0("axis.text.", axis, ".", position), paste0("axis.text.", axis), "axis.text")
  }

  resolved_text_element <- NULL
  for (nm in text_hierarchy) {
    el <- ggplot2::calc_element(nm, current_theme, skip_blank = TRUE)
    if (!is.null(el) && !inherits(el, "element_blank")) { resolved_text_element <- el; break }
  }
  if (is.null(resolved_text_element)) {
    resolved_text_element <- ggplot2::element_text(colour = "black", size = 11, family = "")
  }

  text_colour <- colour %||% resolved_text_element$colour %||% "black"
  text_size   <- size   %||% resolved_text_element$size   %||% 11
  text_family <- family %||% resolved_text_element$family %||% ""

  # ---- Resolve labels -------------------------------------------------------

  if (is.null(label)) {
    labels <- if (arbitrary_position) {
      paste0("(", breaks$x, ", ", breaks$y, ")")
    } else if (use_normalized) {
      as.character(breaks)
    } else if (inherits(breaks, "Date")) {
      format(breaks, "%d-%m-%Y")
    } else if (inherits(breaks, c("POSIXct", "POSIXlt"))) {
      format(breaks, "%d-%m-%Y %H:%M:%S")
    } else if (inherits(breaks, c("hms", "difftime"))) {
      as.character(breaks)
    } else if (is.numeric(breaks)) {
      scales::comma(breaks)
    } else {
      as.character(breaks)
    }
  } else if (is.function(label)) {
    labels <- label(breaks)
  } else {
    labels <- label
  }

  if (base::length(labels) != n_breaks) rlang::abort("Length of labels must match length of breaks.")

  # ---- Resolve tick length offset (axis mode only) --------------------------

  flip_direction <- FALSE

  if (!arbitrary_position) {
    length_hierarchy <- c(
      paste0("axis.ticks.length.", axis, ".", position),
      paste0("axis.ticks.length.", axis),
      "axis.ticks.length"
    )

    resolved_length <- NULL
    for (nm in length_hierarchy) {
      el <- ggplot2::calc_element(nm, current_theme, skip_blank = TRUE)
      if (!is.null(el) && !inherits(el, "element_blank")) { resolved_length <- el; break }
    }

    calculate_default_tick_length <- function() {
      tl <- resolved_length
      if (is.null(tl)) {
        return(grid::unit(0.5 * (current_theme$text$size %||% 11), "pt"))
      } else if (inherits(tl, "rel")) {
        spacing     <- current_theme$spacing %||% grid::unit(5.5, "pt")
        spacing_pts <- as.numeric(grid::convertUnit(if (inherits(spacing, "unit")) spacing else grid::unit(5.5, "pt"), "pt"))
        return(grid::unit(as.numeric(tl) * spacing_pts, "pt"))
      } else if (!inherits(tl, "unit")) {
        return(grid::unit(if (is.numeric(tl)) tl else 0.5 * (current_theme$text$size %||% 11), "pt"))
      } else {
        return(tl)
      }
    }

    if (is.null(tick_length)) {
      tick_length <- calculate_default_tick_length()
    } else if (inherits(tick_length, "rel")) {
      rel_value   <- as.numeric(tick_length)
      default_pts <- as.numeric(grid::convertUnit(calculate_default_tick_length(), "pt"))
      tick_length    <- grid::unit(abs(rel_value) * default_pts, "pt")
      flip_direction <- rel_value < 0
    } else if (inherits(tick_length, "unit")) {
      tick_pts       <- as.numeric(grid::convertUnit(tick_length, "pt"))
      tick_length    <- grid::unit(abs(tick_pts), "pt")
      flip_direction <- tick_pts < 0
    } else if (is.numeric(tick_length)) {
      flip_direction <- tick_length < 0
      tick_length    <- grid::unit(abs(tick_length), "pt")
    } else {
      tick_length <- calculate_default_tick_length()
    }

    text_margin <- resolved_text_element$margin
    margin_unit <- grid::unit(2, "pt")

    if (!is.null(text_margin)) {
      margin_index <- switch(position, bottom = 1L, top = 3L, left = 2L, right = 4L)
      if (inherits(text_margin, c("margin", "unit"))) {
        if (base::length(text_margin) >= margin_index) margin_unit <- text_margin[margin_index]
        else if (base::length(text_margin) == 1)        margin_unit <- text_margin
      }
    }

    total_length <- tick_length + margin_unit
  }

  # ---- Justification --------------------------------------------------------

  if (arbitrary_position) {
    if (is.null(hjust)) hjust <- 0.5
    if (is.null(vjust)) vjust <- 0.5
  } else {
    if (is.null(hjust)) {
      hjust <- if (position %in% c("top", "bottom")) 0.5
      else if (position == "left")          if (flip_direction) 0 else 1
      else                                  if (flip_direction) 1 else 0
    }
    if (is.null(vjust)) {
      vjust <- if (position == "bottom")      if (flip_direction) 0 else 1
      else if (position == "top")    if (flip_direction) 1 else 0
      else                           0.5
    }
  }

  stamp <- list()

  # ---- Theme modification ---------------------------------------------------

  if (!arbitrary_position && element_to != "keep") {
    theme_name <- paste0("axis.text.", axis, ".", position)
    theme_mod  <- list()
    theme_mod[[theme_name]] <- if (element_to == "transparent") ggplot2::element_text(colour = "transparent") else ggplot2::element_blank()
    stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_mod)))
  }

  # ---- Build text annotations -----------------------------------------------

  make_gpar <- function() {
    grid::gpar(col = text_colour, fontsize = text_size, fontfamily = text_family)
  }

  text_annotations <- if (arbitrary_position) {
    seq_len(n_breaks) |>
      purrr::map(\(i) {
        if (use_normalized) {
          text_grob <- grid::textGrob(labels[i], x = grid::unit(breaks$x[i], "npc"), y = grid::unit(breaks$y[i], "npc"), just = c(hjust, vjust), rot = angle, gp = make_gpar())
          ggplot2::annotation_custom(text_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
        } else {
          ggplot2::annotate("text", x = breaks$x[i], y = breaks$y[i], label = labels[i], colour = text_colour, size = text_size / ggplot2::.pt, family = text_family, hjust = hjust, vjust = vjust, angle = angle)
        }
      })
  } else {
    breaks |>
      purrr::imap(\(break_val, i) {
        y_offset <- function(base, sign) if (sign > 0) grid::unit(base, "npc") + total_length else grid::unit(base, "npc") - total_length
        x_offset <- function(base, sign) if (sign > 0) grid::unit(base, "npc") + total_length else grid::unit(base, "npc") - total_length

        if (use_normalized) {
          text_grob <- if (position == "bottom") {
            grid::textGrob(labels[i], x = grid::unit(break_val, "npc"), y = y_offset(0, if (flip_direction) 1 else -1), just = c(hjust, vjust), rot = angle, gp = make_gpar())
          } else if (position == "top") {
            grid::textGrob(labels[i], x = grid::unit(break_val, "npc"), y = y_offset(1, if (flip_direction) -1 else 1), just = c(hjust, vjust), rot = angle, gp = make_gpar())
          } else if (position == "left") {
            grid::textGrob(labels[i], x = x_offset(0, if (flip_direction) 1 else -1), y = grid::unit(break_val, "npc"), just = c(hjust, vjust), rot = angle, gp = make_gpar())
          } else {
            grid::textGrob(labels[i], x = x_offset(1, if (flip_direction) -1 else 1), y = grid::unit(break_val, "npc"), just = c(hjust, vjust), rot = angle, gp = make_gpar())
          }
          ggplot2::annotation_custom(text_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
        } else {
          text_grob <- if (position == "bottom") {
            grid::textGrob(labels[i], x = grid::unit(0.5, "npc"), y = y_offset(0, if (flip_direction) 1 else -1), just = c(hjust, vjust), rot = angle, gp = make_gpar())
          } else if (position == "top") {
            grid::textGrob(labels[i], x = grid::unit(0.5, "npc"), y = y_offset(1, if (flip_direction) -1 else 1), just = c(hjust, vjust), rot = angle, gp = make_gpar())
          } else if (position == "left") {
            grid::textGrob(labels[i], x = x_offset(0, if (flip_direction) 1 else -1), y = grid::unit(0.5, "npc"), just = c(hjust, vjust), rot = angle, gp = make_gpar())
          } else {
            grid::textGrob(labels[i], x = x_offset(1, if (flip_direction) -1 else 1), y = grid::unit(0.5, "npc"), just = c(hjust, vjust), rot = angle, gp = make_gpar())
          }

          annotation_position <- if (axis == "x") {
            if (position == "bottom") list(xmin = break_val, xmax = break_val, ymin = -Inf, ymax = -Inf)
            else                      list(xmin = break_val, xmax = break_val, ymin =  Inf, ymax =  Inf)
          } else {
            if (position == "left")   list(xmin = -Inf, xmax = -Inf, ymin = break_val, ymax = break_val)
            else                      list(xmin =  Inf, xmax =  Inf, ymin = break_val, ymax = break_val)
          }

          rlang::exec(ggplot2::annotation_custom, grob = text_grob, !!!annotation_position)
        }
      })
  }

  c(stamp, text_annotations)
}
