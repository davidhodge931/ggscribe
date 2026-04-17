#' Duplicate axis with axis text only
#'
#' A wrapper around [ggplot2::dup_axis()] that creates a secondary axis
#' displaying only axis text — axis lines and ticks are hidden, making it useful
#' for placing annotation labels (e.g. region labels alongside a shaded panel).
#'
#' @param breaks One of:
#'   - `NULL` for no breaks
#'   - [ggplot2::waiver()] (default) to inherit breaks from the primary axis
#'   - A numeric vector of break positions
#'   - A function that takes the scale limits as input and returns break
#'     positions (e.g. `\(x) mean(c(x[2], 32))`)
#' @param labels One of:
#'   - [ggplot2::derive()] (default) to derive labels from `breaks`
#'   - A character vector of labels, the same length as `breaks`
#'   - A function that takes break positions as input and returns labels
#' @param ... Additional arguments passed to [ggplot2::dup_axis()].
#'
#' @returns A `AxisSecondary` object for use in the `sec.axis` argument of
#'   `scale_x_continuous()` or `scale_y_continuous()`.
#'
#' @seealso [ggplot2::dup_axis()], [annotate_panel_shade()]
#'
#' @export
#'
#' @examples
#' ggplot(mpg, aes(x = displ, y = hwy)) +
#'   geom_point() +
#'   annotate_panel_shade(ymin = 32) +
#'   scale_y_continuous(
#'     sec.axis = dup_axis_text(
#'       breaks = \(x) mean(c(x[2], 32)),
#'       labels = "Inefficient",
#'     )
#'   )
dup_axis_text <- function(
    breaks = ggplot2::waiver(),
    labels = ggplot2::derive(),
    ...
  ) {

  ggplot2::dup_axis(
    breaks = breaks,
    labels = labels,
    name = NULL,
    guide = ggplot2::guide_axis(
      theme = ggplot2::theme(
        axis.line.x = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_line(colour = "transparent"),
        axis.ticks.x = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_line(colour = "transparent"),
      )
    )
  )
}
