# guide_sec_axis_text ---------------------------------------------------------------

#' Guide optimised for secondary axis text annotations
#'
#' A wrapper around [ggplot2::guide_axis()] that defaults to making transparent
#' ticks and lines while preserving text, making it ideal for annotation labels.
#'
#' @param theme A theme object to style the secondary axis.
#' @param ... Additional arguments passed to [ggplot2::guide_axis()], such as
#'   `title`, `check.overlap`, or `angle`.
#'
#' @returns A `guide` object to be used in a scale's `guide` argument or within
#'   [sec_axis_text()].
#'
#' @seealso [sec_axis_text()], [axis_text()], [axis_ticks()], [axis_bracket()]
#'
#' @export
#'
#' @inherit sec_axis_text examples
#'
guide_sec_axis_text <- function(..., theme = NULL) {
  base_theme <- theme_sec_axis_text()

  if (!is.null(theme)) {
    theme <- base_theme + theme
  } else {
    theme <- base_theme
  }

  ggplot2::guide_axis(
    theme = theme,
    ...
  )
}
