#' Element line that is transparent
#'
#' A convenience wrapper around [ggplot2::element_line()] that sets the line
#' colour to `"transparent"`. This is particularly useful in theme
#' modifications where you want the element to maintain its logical space
#' without being visible.
#'
#' @param ... Additional arguments passed to [ggplot2::element_line()],
#'   such as `linewidth`, `linetype`, or `lineend`.
#'
#' @returns An `element_line` object.
#'
#' @seealso [theme_axis_annotate()], [guide_axis_annotate()]
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Using it to hide panel grid lines without using element_blank()
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   theme(panel.grid.major = element_line_transparent())
element_line_transparent <- function(...) {
  ggplot2::element_line(colour = "transparent", ...)
}
