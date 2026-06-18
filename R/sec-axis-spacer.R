# sec_axis_spacer -------------------------------------------------------------

#' Secondary axis for providing space for text annotations
#'
#' A convenience wrapper around [sec_axis_text()] that reserves vertical (or
#' horizontal) space above (or beside) an axis without drawing visible text.
#' Useful for pushing axis titles away from the panel to make room for
#' annotations added with [axis_text()], [axis_ticks()], or [axis_bracket()].
#'
#' @param breaks A function or numeric vector giving the break position(s) used
#'   to anchor the text. Defaults to `NULL`, which places a single label at the
#'   midpoint of the scale — the mean of the limits for continuous scales.
#' @param labels A character string used as the spacer. Defaults to `""`. Use
#'   repeated newlines (e.g. `"\n"`) or a word to fit.
#' @param name The name of the secondary axis. Use [ggplot2::waiver()] to
#'   derive the name from the primary axis, or `NULL` (default) for no name.
#' @param guide A guide object used to render the axis. Defaults to
#'   [guide_sec_axis_spacer()], which makes transparent ticks and lines.
#' @param ... Additional arguments passed to [ggplot2::dup_axis()].
#'
#' @return A [ggplot2::sec_axis()] object.
#' @seealso [guide_sec_axis_spacer()], [axis_text()], [axis_ticks()], [axis_bracket()]
#' @export
sec_axis_spacer <- function(
    breaks = \(x) mean(x),
    labels = "",
    name   = NULL,
    guide  = guide_sec_axis_spacer(),
    ...
) {

  ggplot2::dup_axis(
    breaks = breaks,
    labels = labels,
    name   = name,
    guide  = guide,
    ...
  )
}

# guide_sec_axis_spacer ---------------------------------------------------------------

#' Guide optimised for secondary axis space adjustments
#'
#' A wrapper around [ggplot2::guide_axis()] that defaults to making transparent
#' ticks and lines and making the text the same colour as the plot background fill
#' from the set theme.
#'
#' @param ... Additional arguments passed to [ggplot2::guide_axis()], such as
#'   `title`, `check.overlap`, or `angle`.
#'
#' @returns A `guide` object to be used in a scale's `guide` argument or within
#'   [sec_axis_text()].
#'
#' @seealso [sec_axis_spacer()], [axis_text()], [axis_ticks()], [axis_bracket()]
#'
#' @export
#'
#' @inherit sec_axis_text examples
#'
guide_sec_axis_spacer <- function(...) {
  ggplot2::guide_axis(
    theme = theme_sec_axis_spacer(),
    ...
  )
}

# theme_sec_axis_spacer ------------------------------------------------------------------

#' Theme adjustment for secondary axis space adjustments.
#'
#' @returns A ggplot2 theme object.
#' @noRd
#'
#' @seealso [sec_axis_text()], [guide_sec_axis_text()]
#'
#' @seealso [axis_ticks()], [axis_line()],
#' [axis_text()], [reference_line()]
#'
theme_sec_axis_spacer <- function() {
  plot_background_fill <- scales::alpha(ggplot2::get_theme()$plot.background@fill %||% "white", alpha = 0)

  ggplot2::theme(
    axis.line.x.top = ggplot2::element_line(linetype = 0),
    axis.line.x.bottom = ggplot2::element_line(linetype = 0),
    axis.ticks.x.top = ggplot2::element_line(linetype = 0),
    axis.ticks.x.bottom = ggplot2::element_line(linetype = 0),
    axis.text.x.top = ggplot2::element_text(colour = plot_background_fill),
    axis.text.x.bottom = ggplot2::element_text(colour = plot_background_fill),

    axis.line.y.right = ggplot2::element_line(linetype = 0),
    axis.line.y.left = ggplot2::element_line(linetype = 0),
    axis.ticks.y.right = ggplot2::element_line(linetype = 0),
    axis.ticks.y.left = ggplot2::element_line(linetype = 0),
    axis.text.y.right = ggplot2::element_text(colour = plot_background_fill),
    axis.text.y.left = ggplot2::element_text(colour = plot_background_fill)
  )
}
