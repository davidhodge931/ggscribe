# annotate_panel_grid ---------------------------------------------------------

#' Annotate panel grid lines
#'
#' Draws grid lines at specified break positions with style defaults taken from
#' the `panel.grid` element of the set theme. Specify `x` for vertical lines
#' or `y` for horizontal lines.
#'
#' @param ... Named arguments passed to `ggplot2::annotate()`. Support trailing
#'   commas.
#' @param x A vector of x-axis breaks for vertical grid lines. Cannot be used
#'   together with `y`. Use `I()` for normalized coordinates (0-1).
#' @param y A vector of y-axis breaks for horizontal grid lines. Cannot be used
#'   together with `x`. Use `I()` for normalized coordinates (0-1).
#' @param xmin,xmax Start and end x positions for horizontal grid lines. Use
#'   `I()` for normalized coordinates (0-1). Defaults to `-Inf` and `Inf`.
#' @param ymin,ymax Start and end y positions for vertical grid lines. Use
#'   `I()` for normalized coordinates (0-1). Defaults to `-Inf` and `Inf`.
#' @param minor Logical. If `TRUE`, uses minor grid theme defaults. Defaults to
#'   `FALSE`.
#' @param colour Inherits from `panel.grid.major` or `panel.grid.minor` in the
#'   set theme.
#' @param linewidth Inherits from `panel.grid.major` or `panel.grid.minor` in
#'   the set theme. Supports `rel()`.
#' @param linetype Inherits from `panel.grid.major` or `panel.grid.minor` in
#'   the set theme.
#' @param element_to One of `"keep"`, `"transparent"`, or `"blank"`. Controls
#'   whether native theme grid lines are suppressed. Defaults to `"keep"`.
#'
#' @return A list of ggplot2 annotation layers and theme elements.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' set_theme(theme_minimal())
#'
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point()
#'
#' # Vertical grid lines at specific x breaks
#' p + annotate_panel_grid(x = c(2, 3, 4, 5))
#'
#' # Horizontal grid lines at specific y breaks, native lines suppressed
#' p + annotate_panel_grid(y = c(10, 20, 30), element_to = "blank")
#'
#' # Minor vertical grid lines
#' p + annotate_panel_grid(x = seq(2, 5, by = 0.5), minor = TRUE)
#'
#' # Partial horizontal lines that don't span the full panel width
#' p + annotate_panel_grid(y = c(15, 25), xmax = I(0.5), element_to = "blank")
annotate_panel_grid <- function(
    ...,
    x         = NULL,
    y         = NULL,
    xmin      = NULL,
    xmax      = NULL,
    ymin      = NULL,
    ymax      = NULL,
    minor     = FALSE,
    colour    = NULL,
    linewidth = NULL,
    linetype  = NULL,
    element_to = "keep"
) {
  if (is.null(x) && is.null(y)) {
    rlang::abort("Either x or y must be specified")
  }

  if (!is.null(x) && !is.null(y)) {
    rlang::abort("Only one of x or y can be specified")
  }

  element_to <- rlang::arg_match(element_to, c("keep", "transparent", "blank"))

  x_is_normalized    <- !is.null(x)    && inherits(x,    "AsIs")
  y_is_normalized    <- !is.null(y)    && inherits(y,    "AsIs")
  xmin_is_normalized <- !is.null(xmin) && inherits(xmin, "AsIs")
  xmax_is_normalized <- !is.null(xmax) && inherits(xmax, "AsIs")
  ymin_is_normalized <- !is.null(ymin) && inherits(ymin, "AsIs")
  ymax_is_normalized <- !is.null(ymax) && inherits(ymax, "AsIs")

  if (x_is_normalized) {
    x <- unclass(x)
    if (any(x < 0 | x > 1)) rlang::abort("Normalized x coordinates must be between 0 and 1.")
  }
  if (y_is_normalized) {
    y <- unclass(y)
    if (any(y < 0 | y > 1)) rlang::abort("Normalized y coordinates must be between 0 and 1.")
  }
  if (xmin_is_normalized) { xmin <- unclass(xmin); if (xmin < 0 || xmin > 1) rlang::abort("Normalized xmin must be between 0 and 1.") }
  if (xmax_is_normalized) { xmax <- unclass(xmax); if (xmax < 0 || xmax > 1) rlang::abort("Normalized xmax must be between 0 and 1.") }
  if (ymin_is_normalized) { ymin <- unclass(ymin); if (ymin < 0 || ymin > 1) rlang::abort("Normalized ymin must be between 0 and 1.") }
  if (ymax_is_normalized) { ymax <- unclass(ymax); if (ymax < 0 || ymax > 1) rlang::abort("Normalized ymax must be between 0 and 1.") }

  axis             <- if (!is.null(x)) "x" else "y"
  breaks_normalized <- if (axis == "x") x_is_normalized else y_is_normalized
  limits_normalized <- if (axis == "x") ymin_is_normalized || ymax_is_normalized else xmin_is_normalized || xmax_is_normalized
  breaks           <- if (!is.null(x)) x else y

  if (length(breaks) == 0) return(list())

  current_theme <- ggplot2::theme_get()

  # ---- Resolve grid element -------------------------------------------------

  grid_hierarchy <- if (minor) {
    c(paste0("panel.grid.minor.", axis), "panel.grid.minor", "panel.grid")
  } else {
    c(paste0("panel.grid.major.", axis), "panel.grid.major", "panel.grid")
  }

  resolved_grid_element <- grid_hierarchy |>
    purrr::map(\(x) ggplot2::calc_element(x, current_theme, skip_blank = TRUE)) |>
    purrr::detect(\(x) !is.null(x) && !inherits(x, "element_blank"))

  if (is.null(resolved_grid_element)) {
    resolved_grid_element <- list(
      colour    = if (minor) "grey95" else "grey90",
      linewidth = if (minor) 0.25 else 0.5,
      linetype  = 1
    )
  }

  # ---- Extract properties ---------------------------------------------------

  grid_colour <- colour %||% resolved_grid_element$colour %||% if (minor) "grey95" else "grey90"

  grid_linewidth <- if (is.null(linewidth)) {
    resolved_grid_element$linewidth %||% if (minor) 0.25 else 0.5
  } else if (inherits(linewidth, "rel")) {
    as.numeric(linewidth) * (resolved_grid_element$linewidth %||% if (minor) 0.25 else 0.5)
  } else {
    linewidth
  }

  grid_linetype <- linetype %||% resolved_grid_element$linetype %||% 1

  stamp <- list()

  # ---- Theme modification ---------------------------------------------------

  if (element_to != "keep") {
    element_name <- if (minor) paste0("panel.grid.minor.", axis) else paste0("panel.grid.major.", axis)
    stamp <- c(stamp, list(ggplot2::theme(
      !!element_name := if (element_to == "transparent") ggplot2::element_line(colour = "transparent") else ggplot2::element_blank()
    )))
  }

  # ---- Build grid annotations -----------------------------------------------

  make_gpar <- function() {
    grid::gpar(col = grid_colour, lwd = grid_linewidth * 72 / 25.4, lty = grid_linetype, lineend = "butt")
  }

  if (breaks_normalized && limits_normalized) {
    grid_annotations <- breaks |>
      purrr::map(\(break_val) {
        grid_grob <- if (axis == "x") {
          grid::linesGrob(x = grid::unit(c(break_val, break_val), "npc"), y = grid::unit(c(if (!is.null(ymin)) ymin else 0, if (!is.null(ymax)) ymax else 1), "npc"), gp = make_gpar())
        } else {
          grid::linesGrob(x = grid::unit(c(if (!is.null(xmin)) xmin else 0, if (!is.null(xmax)) xmax else 1), "npc"), y = grid::unit(c(break_val, break_val), "npc"), gp = make_gpar())
        }
        ggplot2::annotation_custom(grid_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
      })
    stamp <- c(stamp, grid_annotations)

  } else if (!breaks_normalized && limits_normalized) {
    grid_annotations <- breaks |>
      purrr::map(\(break_val) {
        if (axis == "x") {
          grid_grob <- grid::linesGrob(x = grid::unit(c(0.5, 0.5), "npc"), y = grid::unit(c(if (!is.null(ymin)) ymin else 0, if (!is.null(ymax)) ymax else 1), "npc"), gp = make_gpar())
          ggplot2::annotation_custom(grid_grob, xmin = break_val, xmax = break_val, ymin = -Inf, ymax = Inf)
        } else {
          grid_grob <- grid::linesGrob(x = grid::unit(c(if (!is.null(xmin)) xmin else 0, if (!is.null(xmax)) xmax else 1), "npc"), y = grid::unit(c(0.5, 0.5), "npc"), gp = make_gpar())
          ggplot2::annotation_custom(grid_grob, xmin = -Inf, xmax = Inf, ymin = break_val, ymax = break_val)
        }
      })
    stamp <- c(stamp, grid_annotations)

  } else if (breaks_normalized && !limits_normalized) {
    grid_annotations <- breaks |>
      purrr::map(\(break_val) {
        grid_grob <- if (axis == "x") {
          grid::linesGrob(x = grid::unit(c(break_val, break_val), "npc"), y = grid::unit(c(0, 1), "npc"), gp = make_gpar())
        } else {
          grid::linesGrob(x = grid::unit(c(0, 1), "npc"), y = grid::unit(c(break_val, break_val), "npc"), gp = make_gpar())
        }
        ggplot2::annotation_custom(grid_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
      })
    stamp <- c(stamp, grid_annotations)

  } else {
    if (axis == "x") {
      stamp <- c(stamp, list(ggplot2::annotate("segment", x = breaks, xend = breaks, y = if (!is.null(ymin)) ymin else -Inf, yend = if (!is.null(ymax)) ymax else Inf, colour = grid_colour, linewidth = grid_linewidth, linetype = grid_linetype, ...)))
    } else {
      stamp <- c(stamp, list(ggplot2::annotate("segment", x = if (!is.null(xmin)) xmin else -Inf, xend = if (!is.null(xmax)) xmax else Inf, y = breaks, yend = breaks, colour = grid_colour, linewidth = grid_linewidth, linetype = grid_linetype, ...)))
    }
  }

  return(stamp)
}


# annotate_panel_shade --------------------------------------------------------

#' Annotate a shaded panel region
#'
#' Draws a filled rectangle over the panel with colour defaults taken from the
#' set theme. Defaults to a subtle overlay across the full panel, with the fill
#' automatically adapting to light or dark panel backgrounds.
#'
#' @param ... Named arguments passed to `ggplot2::annotate()`. Support trailing
#'   commas.
#' @param xmin,xmax Left and right edges of the rectangle. Defaults to `-Inf`
#'   and `Inf`. Use `I()` for normalized coordinates (0-1).
#' @param ymin,ymax Bottom and top edges of the rectangle. Defaults to `-Inf`
#'   and `Inf`. Use `I()` for normalized coordinates (0-1).
#' @param fill Fill colour. Defaults to a neutral grey that adapts to the panel
#'   background luminance.
#' @param alpha Opacity of the rectangle. Defaults to `0.25`.
#' @param colour Border colour. Defaults to `"transparent"`.
#' @param linewidth Inherits from `panel.border` in the set theme. Supports
#'   `rel()`.
#' @param linetype Border linetype. Defaults to `1`.
#'
#' @return A list containing an annotation layer.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' set_theme(theme_classic())
#'
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point()
#'
#' # Shade the full panel
#' p + annotate_panel_shade()
#'
#' # Shade a specific data range
#' p + annotate_panel_shade(xmin = 3, xmax = 4)
#'
#' # Shade using normalized coordinates
#' p + annotate_panel_shade(xmin = I(0.25), xmax = I(0.75))
#'
#' # Custom fill and opacity
#' p + annotate_panel_shade(ymin = 20, ymax = 30, fill = "steelblue", alpha = 0.15)
annotate_panel_shade <- function(
    ...,
    xmin      = -Inf,
    xmax      = Inf,
    ymin      = -Inf,
    ymax      = Inf,
    fill      = NULL,
    alpha     = 0.25,
    colour    = "transparent",
    linewidth = NULL,
    linetype  = NULL
) {
  if (rlang::is_null(fill)) {
    fill <- if (is_panel_dark()) {
      flexoki::flexoki$base["base600"]
    } else {
      flexoki::flexoki$base["base400"]
    }
  }

  xmin_is_normalized <- inherits(xmin, "AsIs")
  xmax_is_normalized <- inherits(xmax, "AsIs")
  ymin_is_normalized <- inherits(ymin, "AsIs")
  ymax_is_normalized <- inherits(ymax, "AsIs")

  x_uses_normalized <- xmin_is_normalized || xmax_is_normalized
  y_uses_normalized <- ymin_is_normalized || ymax_is_normalized

  if (x_uses_normalized) {
    if (!((xmin_is_normalized || is.infinite(xmin)) && (xmax_is_normalized || is.infinite(xmax)))) {
      rlang::abort("Cannot mix normalized (I()) and data coordinates for x. Use I() for both xmin and xmax, or neither.")
    }
  }
  if (y_uses_normalized) {
    if (!((ymin_is_normalized || is.infinite(ymin)) && (ymax_is_normalized || is.infinite(ymax)))) {
      rlang::abort("Cannot mix normalized (I()) and data coordinates for y. Use I() for both ymin and ymax, or neither.")
    }
  }

  if (xmin_is_normalized) { xmin <- unclass(xmin); if (length(xmin) != 1 || xmin < 0 || xmin > 1) rlang::abort("Normalized xmin must be a single value between 0 and 1.") }
  if (xmax_is_normalized) { xmax <- unclass(xmax); if (length(xmax) != 1 || xmax < 0 || xmax > 1) rlang::abort("Normalized xmax must be a single value between 0 and 1.") }
  if (ymin_is_normalized) { ymin <- unclass(ymin); if (length(ymin) != 1 || ymin < 0 || ymin > 1) rlang::abort("Normalized ymin must be a single value between 0 and 1.") }
  if (ymax_is_normalized) { ymax <- unclass(ymax); if (length(ymax) != 1 || ymax < 0 || ymax > 1) rlang::abort("Normalized ymax must be a single value between 0 and 1.") }

  use_grob <- x_uses_normalized || y_uses_normalized

  current_theme  <- ggplot2::theme_get()
  panel_border   <- ggplot2::calc_element("panel.border", current_theme, skip_blank = TRUE)
  base_linewidth <- if (!rlang::is_null(panel_border) && !inherits(panel_border, "element_blank")) panel_border$linewidth %||% 0.5 else 0.5

  alpha    <- alpha %||% 1
  linewidth <- if (rlang::is_null(linewidth)) base_linewidth else if (inherits(linewidth, "rel")) as.numeric(linewidth) * base_linewidth else linewidth
  linetype <- linetype %||% 1

  if (use_grob) {
    x_left   <- if (xmin_is_normalized) grid::unit(xmin, "npc") else grid::unit(0, "npc")
    x_right  <- if (xmax_is_normalized) grid::unit(xmax, "npc") else grid::unit(1, "npc")
    y_bottom <- if (ymin_is_normalized) grid::unit(ymin, "npc") else grid::unit(0, "npc")
    y_top    <- if (ymax_is_normalized) grid::unit(ymax, "npc") else grid::unit(1, "npc")

    rect_grob <- grid::rectGrob(
      x      = x_left,
      y      = y_bottom,
      width  = x_right - x_left,
      height = y_top - y_bottom,
      just   = c("left", "bottom"),
      gp     = grid::gpar(fill = scales::alpha(fill, alpha), col = colour, lwd = linewidth * 72 / 25.4, lty = linetype)
    )

    list(ggplot2::annotation_custom(rect_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf))
  } else {
    list(ggplot2::annotate("rect", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill, colour = colour, linewidth = linewidth, linetype = linetype, alpha = alpha, ...))
  }
}
