# panel_shade -----------------------------------------------------------------

#' Annotate a shaded panel region
#'
#' A convenience wrapper around [panel_background()] with defaults suited to
#' subtle overlays: a neutral grey fill at 25% opacity with no border. Should
#' be placed before geom layers.
#'
#' @inheritParams panel_background
#' @param fill Fill colour. Defaults to a neutral grey. May be a vector the
#'   same length as the bounds.
#' @param alpha Opacity. Defaults to `0.25`. May be a vector.
#' @param colour Border colour. Defaults to `"transparent"`. May be a vector.
#' @param linetype Border linetype. Defaults to `1`. May be a vector.
#'
#' @return A list containing annotation layers.
#' @seealso [panel_background()], [axis_line()], [reference_line()]
#' @export
panel_shade <- function(
    xmin      = -Inf,
    xmax      = Inf,
    ymin      = -Inf,
    ymax      = Inf,
    fill      = "#878580",
    alpha     = 0.25,
    colour    = "transparent",
    linewidth = NULL,
    linetype  = 1,
    layout    = NULL
) {
  panel_background(
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
