# guide_axis ------------------------------------------------------------------

#' Axis guide with annotation-friendly defaults
#'
#' A wrapper around [ggplot2::guide_axis()] that defaults to using
#' [theme_axis()]. This guide is designed to strip away standard axis
#' furniture (like lines and ticks) while preserving text, making it ideal for
#' secondary axes used as margin labels.
#'
#' @param ... Additional arguments passed to [ggplot2::guide_axis()], such as
#'   `title`, `check.overlap`, or `angle`.
#' @param theme A `theme` object to style the guide. Defaults to
#'   [theme_axis()], which suppresses ticks and lines.
#'
#' @returns A `guide` object to be used in a scale's `guide` argument or within
#'   [sec_axis()].
#'
#' @seealso [sec_axis()], [theme_axis()]
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Using the guide directly in a scale
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   scale_x_continuous(
#'     guide = ggscribe::guide_axis(title = "Displacement Label Only")
#'   )
#'
#' # The guide is also used internally by sec_axis()
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   scale_y_continuous(
#'     sec.axis = ggscribe::sec_axis(
#'       breaks = 20,
#'       labels = "Reference point",
#'       guide = ggscribe::guide_axis(angle = 90)
#'     )
#'   )
guide_axis <- function(..., theme = theme_axis()) {
  ggplot2::guide_axis(
    theme = theme,
    ...
  )
}
