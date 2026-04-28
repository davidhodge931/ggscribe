# guide_sec_axis_text ---------------------------------------------------------------

#' Guide optimised for secondary axis text annotations
#'
#' A wrapper around [ggplot2::guide_axis()] that defaults to using
#' [theme_sec_axis_text()]. This guide is designed to strip away standard axis
#' furniture (like lines and ticks) while preserving text, making it ideal for
#' secondary axes used as margin labels.
#'
#' @param ... Additional arguments passed to [ggplot2::guide_axis()], such as
#'   `title`, `check.overlap`, or `angle`.
#' @param theme A `theme` object to style the guide. Defaults to
#'   [theme_sec_axis_text()], which suppresses ticks and lines.
#'
#' @returns A `guide` object to be used in a scale's `guide` argument or within
#'   [sec_axis_text()].
#'
#' @seealso [sec_axis_text()], [theme_sec_axis_text()]
#'
#' @export
#'
#' @inherit sec_axis_text examples
#'
guide_sec_axis_text <- function(..., theme = theme_sec_axis_text()) {
  ggplot2::guide_axis(
    theme = theme,
    ...
  )
}
