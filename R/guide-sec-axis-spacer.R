# guide_sec_axis_spacer ---------------------------------------------------------------

#' Guide optimised for secondary axis space adjustments
#'
#' A wrapper around [ggplot2::guide_axis()] that defaults to making transparent
#' ticks and lines and making the text the same colour as the plot background fill
#' from the set theme.
#'
#' @param ... Additional arguments passed to [ggplot2::guide_axis()], such as
#'   `title`, `check.overlap`, or `angle`.
#' @param theme A `theme` object to adjust the style of the guide.
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
guide_sec_axis_spacer <- function(..., theme = NULL) {
  ggplot2::guide_axis(
    theme = theme_sec_axis_spacer() + theme,
    ...
  )
}
