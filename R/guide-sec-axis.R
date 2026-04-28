# guide_sec_axis ---------------------------------------------------------------

#' Guide for secondary axis annotation
#'
#' A wrapper around [ggplot2::guide_axis()] that defaults to using
#' [theme_sec_axis()]. This guide is designed to strip away standard axis
#' furniture (like lines and ticks) while preserving text, making it ideal for
#' secondary axes used as margin labels.
#'
#' @param ... Additional arguments passed to [ggplot2::guide_axis()], such as
#'   `title`, `check.overlap`, or `angle`.
#' @param theme A `theme` object to style the guide. Defaults to
#'   [theme_sec_axis()], which suppresses ticks and lines.
#'
#' @returns A `guide` object to be used in a scale's `guide` argument or within
#'   [sec_axis()].
#'
#' @seealso [sec_axis()], [theme_sec_axis()]
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   ggscribe::reference_line(yintercept = 20) +
#'   geom_point() +
#'   scale_y_continuous(
#'     sec.axis = ggscribe::sec_axis(
#'       breaks = 20,
#'       labels = "Reference",
#'       guide = ggscribe::guide_sec_axis(angle = 270)
#'     )
#'   )
#'
guide_sec_axis <- function(..., theme = theme_sec_axis()) {
  ggplot2::guide_axis(
    theme = theme,
    ...
  )
}
