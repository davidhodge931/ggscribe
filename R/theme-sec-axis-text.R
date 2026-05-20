# theme_sec_axis_text ------------------------------------------------------------------

#' Theme adjustment for secondary axis text annotations.
#'
#' @returns A ggplot2 theme object.
#' @noRd
#'
#' @seealso [sec_axis_text()], [guide_sec_axis_text()]
#'
#' @seealso [axis_ticks()], [axis_line()],
#' [axis_text()], [reference_line()]
#'
theme_sec_axis_text <- function() {
  ggplot2::theme(
    axis.line.x.top = ggplot2::element_line(linetype = 0),
    axis.line.x.bottom = ggplot2::element_line(linetype = 0),
    axis.ticks.x.top = ggplot2::element_line(linetype = 0),
    axis.ticks.x.bottom = ggplot2::element_line(linetype = 0),

    axis.line.y.right = ggplot2::element_line(linetype = 0),
    axis.line.y.left = ggplot2::element_line(linetype = 0),
    axis.ticks.y.right = ggplot2::element_line(linetype = 0),
    axis.ticks.y.left = ggplot2::element_line(linetype = 0),
  )
}
