# panel_background ------------------------------------------------------------

#' Annotate a panel background region
#'
#' Draws filled rectangles over the panel using the `panel.background` fill
#' from the set theme as the default colour. Unlike [panel_shade()], opacity
#' defaults to `1` (fully opaque), making it useful for layering a solid
#' background over existing content. Should be placed before geom layers.
#'
#' @param xmin,xmax Left and right edges of the rectangle in data coordinates.
#'   Defaults to `-Inf` and `Inf`. Use [I()] for normalised coordinates (0-1).
#'   May be a vector for multiple rectangles.
#' @param ymin,ymax Bottom and top edges of the rectangle in data coordinates.
#'   Defaults to `-Inf` and `Inf`. Use [I()] for normalised coordinates (0-1).
#'   May be a vector for multiple rectangles.
#' @param fill Fill colour. Defaults to the `panel.background` fill from the
#'   set theme. May be a vector the same length as the bounds.
#' @param alpha Opacity. Defaults to `1` (fully opaque). May be a vector.
#' @param colour Border colour. Defaults to the resolved `fill` value, giving
#'   a seamless border. May be a vector.
#' @param linewidth Inherits from `panel.border` in the set theme. Supports
#'   `rel()`. May be a vector.
#' @param linetype Border linetype. Defaults to `1`. May be a vector.
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
    linetype  = NULL,
    layout    = NULL
) {
  if (is.null(fill)) {
    fill <- ggplot2::get_theme()$panel.background@fill %||% "white"
  }
  if (is.null(colour)) {
    colour <- fill
  }

  panel_shade(
    xmin      = xmin,
    xmax      = xmax,
    ymin      = ymin,
    ymax      = ymax,
    fill      = fill,
    alpha     = alpha,
    colour    = colour,
    linewidth = linewidth,
    linetype  = linetype,
    layout    = layout
  )
}
