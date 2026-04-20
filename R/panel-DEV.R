

# annotate_panel_shade --------------------------------------------------------

#' Annotate a shaded panel region
#'
#' Draws a filled rectangle over the panel with colour defaults taken from the
#' set theme. Defaults to a subtle overlay across the full panel, with the fill
#' automatically adapting to light or dark panel backgrounds.
#'
#' @param ... Not used. Allows trailing commas and named-argument style calls.
#' @param xmin,xmax Left and right edges of the rectangle. Defaults to `-Inf`
#'   and `Inf`. Use `I()` for normalized coordinates (0-1).
#' @param ymin,ymax Bottom and top edges of the rectangle. Defaults to `-Inf`
#'   and `Inf`. Use `I()` for normalized coordinates (0-1).
#' @param fill Fill colour. Defaults to a neutral grey.
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
#' set_theme(
#'   ggrefine::theme_grey(
#'     panel_heights = rep(unit(50, "mm"), 100),
#'     panel_widths = rep(unit(75, "mm"), 100),
#'    )
#'  )
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
  xmin = -Inf,
  xmax = Inf,
  ymin = -Inf,
  ymax = Inf,
  fill = "#878580",
  alpha = 0.25,
  colour = "transparent",
  linewidth = NULL,
  linetype = NULL
) {
  rlang::check_dots_empty()

  xmin_is_normalized <- inherits(xmin, "AsIs")
  xmax_is_normalized <- inherits(xmax, "AsIs")
  ymin_is_normalized <- inherits(ymin, "AsIs")
  ymax_is_normalized <- inherits(ymax, "AsIs")

  x_uses_normalized <- xmin_is_normalized || xmax_is_normalized
  y_uses_normalized <- ymin_is_normalized || ymax_is_normalized

  if (x_uses_normalized) {
    if (
      !((xmin_is_normalized || is.infinite(xmin)) &&
        (xmax_is_normalized || is.infinite(xmax)))
    ) {
      rlang::abort(
        "Cannot mix normalized (I()) and data coordinates for x. Use I() for both xmin and xmax, or neither."
      )
    }
  }
  if (y_uses_normalized) {
    if (
      !((ymin_is_normalized || is.infinite(ymin)) &&
        (ymax_is_normalized || is.infinite(ymax)))
    ) {
      rlang::abort(
        "Cannot mix normalized (I()) and data coordinates for y. Use I() for both ymin and ymax, or neither."
      )
    }
  }

  if (xmin_is_normalized) {
    xmin <- unclass(xmin)
    if (length(xmin) != 1 || xmin < 0 || xmin > 1) {
      rlang::abort("Normalized xmin must be a single value between 0 and 1.")
    }
  }
  if (xmax_is_normalized) {
    xmax <- unclass(xmax)
    if (length(xmax) != 1 || xmax < 0 || xmax > 1) {
      rlang::abort("Normalized xmax must be a single value between 0 and 1.")
    }
  }
  if (ymin_is_normalized) {
    ymin <- unclass(ymin)
    if (length(ymin) != 1 || ymin < 0 || ymin > 1) {
      rlang::abort("Normalized ymin must be a single value between 0 and 1.")
    }
  }
  if (ymax_is_normalized) {
    ymax <- unclass(ymax)
    if (length(ymax) != 1 || ymax < 0 || ymax > 1) {
      rlang::abort("Normalized ymax must be a single value between 0 and 1.")
    }
  }

  use_grob <- x_uses_normalized || y_uses_normalized

  current_theme <- ggplot2::theme_get()
  panel_border <- ggplot2::calc_element(
    "panel.border",
    current_theme,
    skip_blank = TRUE
  )
  base_linewidth <- if (
    !is.null(panel_border) && !inherits(panel_border, "element_blank")
  ) {
    panel_border$linewidth %||% 0.5
  } else {
    0.5
  }

  alpha <- alpha %||% 1
  linewidth <- if (is.null(linewidth)) {
    base_linewidth
  } else if (inherits(linewidth, "rel")) {
    as.numeric(linewidth) * base_linewidth
  } else {
    linewidth
  }
  linetype <- linetype %||% 1

  if (use_grob) {
    x_left <- if (xmin_is_normalized) {
      grid::unit(xmin, "npc")
    } else {
      grid::unit(0, "npc")
    }
    x_right <- if (xmax_is_normalized) {
      grid::unit(xmax, "npc")
    } else {
      grid::unit(1, "npc")
    }
    y_bottom <- if (ymin_is_normalized) {
      grid::unit(ymin, "npc")
    } else {
      grid::unit(0, "npc")
    }
    y_top <- if (ymax_is_normalized) {
      grid::unit(ymax, "npc")
    } else {
      grid::unit(1, "npc")
    }

    rect_grob <- grid::rectGrob(
      x = x_left,
      y = y_bottom,
      width = x_right - x_left,
      height = y_top - y_bottom,
      just = c("left", "bottom"),
      gp = ggplot2::gg_par(
        fill = scales::alpha(fill, alpha),
        col = colour,
        stroke = linewidth,
        lty = linetype
      )
    )

    list(ggplot2::annotation_custom(
      rect_grob,
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf
    ))
  } else {
    list(ggplot2::annotate(
      "rect",
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = fill,
      colour = colour,
      linewidth = linewidth,
      linetype = linetype,
      alpha = alpha
    ))
  }
}
