# panel_background ------------------------------------------------------------

#' Annotate a panel background region
#'
#' Draws filled rectangles over the panel. Defaults to the `panel.background`
#' fill from the set theme at full opacity, making it useful for layering a
#' solid background over existing content. Should be placed before geom layers.
#'
#' @param xmin,xmax Left and right edges of the rectangle in data coordinates.
#'   Defaults to `-Inf` and `Inf`. Use [I()] for normalised coordinates (0-1).
#'   May be a vector for multiple rectangles. Bounds may be mixed freely,
#'   e.g. `xmin = I(0.5), xmax = Inf` fills from 50% to the right panel edge.
#' @param ymin,ymax Bottom and top edges of the rectangle in data coordinates.
#'   Defaults to `-Inf` and `Inf`. Use [I()] for normalised coordinates (0-1).
#'   May be a vector for multiple rectangles.
#' @param fill Fill colour. Defaults to the `panel.background` fill from the
#'   set theme, falling back to `"white"`. May be a vector the same length as
#'   the bounds to style each rectangle individually.
#' @param alpha Opacity. Defaults to `1` (fully opaque). May be a vector.
#' @param colour Border colour. Defaults to the resolved `fill` value, giving
#'   a seamless border. May be a vector.
#' @param linewidth Inherits from `panel.border` in the set theme. Supports
#'   `rel()`. May be a vector.
#' @param linetype Border linetype. Defaults to `0` (none). May be a vector.
#' @param layout Controls which panels the annotation appears in. `NULL`
#'   (default) repeats in all panels. An integer targets a specific panel.
#'   `"fixed"` repeats in all panels ignoring faceting variables. See
#'   [ggplot2::layer()] for full details.
#'
#' @return A list containing annotation layers.
#' @seealso [panel_shade()], [axis_line()], [reference_line()]
#' @export
panel_background <- function(
    xmin      = -Inf,
    xmax      = Inf,
    ymin      = -Inf,
    ymax      = Inf,
    fill      = NULL,
    alpha     = 1,
    colour    = NULL,
    linewidth = NULL,
    linetype  = 0,
    layout    = NULL
) {
  if (is.null(fill)) {
    fill <- ggplot2::get_theme()$panel.background@fill %||% "white"
  }
  if (is.null(colour)) {
    colour <- fill
  }

  # Detect npc per bound before stripping AsIs
  xmin_npc <- inherits(xmin, "AsIs")
  xmax_npc <- inherits(xmax, "AsIs")
  ymin_npc <- inherits(ymin, "AsIs")
  ymax_npc <- inherits(ymax, "AsIs")

  xmin <- as.numeric(xmin)
  xmax <- as.numeric(xmax)
  ymin <- as.numeric(ymin)
  ymax <- as.numeric(ymax)

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

  fill_vec      <- rep_len(fill,         n)
  alpha_vec     <- rep_len(alpha %||% 1, n)
  colour_vec    <- rep_len(colour,       n)
  linewidth_vec <- if (is.null(linewidth)) {
    rep_len(base_linewidth, n)
  } else if (inherits(linewidth, "rel")) {
    rep_len(as.numeric(linewidth), n) * base_linewidth
  } else {
    rep_len(linewidth, n)
  }
  linetype_vec  <- rep_len(linetype, n)

  # Resolve one bound to a grob unit and annotation_custom value.
  # npc (I()) → unit(val, "npc") in grob, ±Inf in anno
  # ±Inf/data → unit(0/1, "npc") in grob, value in anno
  .bound <- function(val, is_npc, lo) {
    if (is_npc) {
      list(grob = grid::unit(val, "npc"), anno = if (lo) -Inf else Inf)
    } else {
      list(grob = grid::unit(if (lo) 0 else 1, "npc"), anno = val)
    }
  }

  lapply(seq_len(n), \(i) {
    x1 <- .bound(xmin[[i]], xmin_npc, lo = TRUE)
    x2 <- .bound(xmax[[i]], xmax_npc, lo = FALSE)
    y1 <- .bound(ymin[[i]], ymin_npc, lo = TRUE)
    y2 <- .bound(ymax[[i]], ymax_npc, lo = FALSE)

    rect_grob <- grid::rectGrob(
      x      = x1$grob,
      y      = y1$grob,
      width  = x2$grob - x1$grob,
      height = y2$grob - y1$grob,
      just   = c("left", "bottom"),
      gp     = grid::gpar(
        fill = scales::alpha(fill_vec[[i]], alpha_vec[[i]]),
        col  = colour_vec[[i]],
        lwd  = linewidth_vec[[i]] * ggplot2::.pt,
        lty  = linetype_vec[[i]]
      )
    )

    anno_pos <- list(
      xmin = x1$anno, xmax = x2$anno,
      ymin = y1$anno, ymax = y2$anno
    )

    .make_ann_layer(rect_grob, anno_pos, layout)
  })
}
