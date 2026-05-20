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
      axis.text.y.left = ggplot2::element_text(colour = plot_background_fill),
    )
}
