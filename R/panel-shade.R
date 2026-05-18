# panel_shade -----------------------------------------------------------------

#' Annotate a shaded panel region
#'
#' Draws filled rectangles over the panel with colour defaults taken from the
#' set theme. Defaults to a subtle overlay across the full panel, with the fill
#' automatically adapting to light or dark panel backgrounds. Should be placed
#' before geom layers.
#'
#' @param ... Not used. Allows trailing commas and named-argument style calls.
#' @param xmin,xmax Left and right edges of the rectangle in data coordinates.
#'   Defaults to `-Inf` and `Inf`. Use `I()` for normalised coordinates (0-1).
#'   May be a vector for multiple rectangles.
#' @param ymin,ymax Bottom and top edges of the rectangle in data coordinates.
#'   Defaults to `-Inf` and `Inf`. Use `I()` for normalised coordinates (0-1).
#'   May be a vector for multiple rectangles.
#' @param fill Fill colour. Defaults to a neutral grey. May be a vector the
#'   same length as the bounds to style each rectangle individually.
#' @param alpha Opacity of the rectangle. Defaults to `0.25`. May be a vector.
#' @param colour Border colour. Defaults to `"transparent"`. May be a vector.
#' @param linewidth Inherits from `panel.border` in the set theme. Supports
#'   `rel()`. May be a vector.
#' @param linetype Border linetype. Defaults to `1`. May be a vector.
#'
#' @return A list containing annotation layers.
#' @export
#'
#' @inherit sec_axis_text examples
#'
panel_shade <- function(
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
      !((xmin_is_normalized || all(is.infinite(xmin))) &&
        (xmax_is_normalized || all(is.infinite(xmax))))
    ) {
      rlang::abort(
        "Cannot mix normalized (I()) and data coordinates for x. Use I() for both xmin and xmax, or neither."
      )
    }
  }
  if (y_uses_normalized) {
    if (
      !((ymin_is_normalized || all(is.infinite(ymin))) &&
        (ymax_is_normalized || all(is.infinite(ymax))))
    ) {
      rlang::abort(
        "Cannot mix normalized (I()) and data coordinates for y. Use I() for both ymin and ymax, or neither."
      )
    }
  }

  xmin <- as.numeric(xmin)
  xmax <- as.numeric(xmax)
  ymin <- as.numeric(ymin)
  ymax <- as.numeric(ymax)

  # n driven by the longest bounds vector; all recycled to match
  n <- max(length(xmin), length(xmax), length(ymin), length(ymax))

  xmin <- rep_len(xmin, n)
  xmax <- rep_len(xmax, n)
  ymin <- rep_len(ymin, n)
  ymax <- rep_len(ymax, n)

  current_theme <- ggplot2::theme_get()
  panel_border  <- ggplot2::calc_element("panel.border", current_theme, skip_blank = TRUE)
  base_linewidth <- if (
    !is.null(panel_border) && !inherits(panel_border, "element_blank")
  ) {
    panel_border$linewidth %||% 0.5
  } else {
    0.5
  }

  # ---- Vectorise and recycle style args to n --------------------------------

  fill_vec   <- rep_len(fill,  n)
  alpha_vec  <- rep_len(alpha %||% 1, n)
  colour_vec <- rep_len(colour, n)

  linewidth_vec <- if (is.null(linewidth)) {
    rep_len(base_linewidth, n)
  } else if (inherits(linewidth, "rel")) {
    rep_len(as.numeric(linewidth), n) * base_linewidth
  } else {
    rep_len(linewidth, n)
  }

  linetype_vec <- rep_len(linetype %||% 1, n)

  # ---- Draw one grob per rectangle -----------------------------------------
  # Always uses annotation_custom + rectGrob for consistency with the other
  # functions. npc bounds go directly into the grob; data bounds are pinned
  # via annotation_custom's xmin/xmax/ymin/ymax.

  lapply(seq_len(n), \(i) {
    x_left <- if (x_uses_normalized) {
      grid::unit(xmin[[i]], "npc")
    } else {
      grid::unit(0, "npc")
    }
    x_right <- if (x_uses_normalized) {
      grid::unit(xmax[[i]], "npc")
    } else {
      grid::unit(1, "npc")
    }
    y_bottom <- if (y_uses_normalized) {
      grid::unit(ymin[[i]], "npc")
    } else {
      grid::unit(0, "npc")
    }
    y_top <- if (y_uses_normalized) {
      grid::unit(ymax[[i]], "npc")
    } else {
      grid::unit(1, "npc")
    }

    rect_grob <- grid::rectGrob(
      x      = x_left,
      y      = y_bottom,
      width  = x_right - x_left,
      height = y_top - y_bottom,
      just   = c("left", "bottom"),
      gp     = grid::gpar(
        fill = scales::alpha(fill_vec[[i]], alpha_vec[[i]]),
        col  = colour_vec[[i]],
        lwd  = linewidth_vec[[i]] * ggplot2::.pt,
        lty  = linetype_vec[[i]]
      )
    )

    # For npc bounds, span the full panel — the grob's npc units handle
    # positioning. For data bounds, pin via annotation_custom coordinates.
    anno_pos <- list(
      xmin = if (x_uses_normalized) -Inf else xmin[[i]],
      xmax = if (x_uses_normalized) Inf  else xmax[[i]],
      ymin = if (y_uses_normalized) -Inf else ymin[[i]],
      ymax = if (y_uses_normalized) Inf  else ymax[[i]]
    )

    rlang::exec(ggplot2::annotation_custom, grob = rect_grob, !!!anno_pos)
  })
}
