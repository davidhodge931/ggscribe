# guide_sec_axis_text ---------------------------------------------------------------

#' Guide optimised for secondary axis text annotations
#'
#' A wrapper around [ggplot2::guide_axis()] that defaults to making transparent
#' ticks and lines while preserving text, making it ideal for annotation labels.
#'
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
guide_sec_axis_text <- function(...) {
  ggplot2::guide_axis(
    theme = theme_sec_axis_text(),
    ...
  )
}
