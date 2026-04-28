# sec_axis --------------------------------------------------------------------

#' Duplicate axis with axis text only
#'
#' A wrapper around [ggplot2::dup_axis()] that creates a secondary axis
#' displaying only axis text — axis lines and ticks are hidden, making it useful
#' for placing annotation labels (e.g. region labels alongside a shaded panel).
#'
#' @param breaks One of:
#'    - `NULL` for no breaks
#'    - [ggplot2::waiver()] (default) to inherit breaks from the primary axis
#'    - A numeric vector of break positions
#'    - A function that takes the scale limits as input and returns break
#'      positions (e.g. `\(x) mean(c(x[2], 32))`)
#' @param name The name of the secondary axis. Use [ggplot2::waiver()] to
#'    derive the name from the primary axis, or `NULL` (default) for no name.
#' @param guide A guide object used to render the axis. Defaults to
#'    [guide_axis()], which uses [theme_axis()] to
#'    make transparent ticks and lines by default.
#' @param labels One of:
#'    - [ggplot2::derive()] (default) to derive labels from `breaks`
#'    - A character vector of labels, the same length as `breaks`
#'    - A function that takes break positions as input and returns labels
#' @param ... Additional arguments passed to [ggplot2::dup_axis()].
#'
#' @returns A `AxisSecondary` object for use in the `sec.axis` argument of
#'    `scale_x_continuous()` or `scale_y_continuous()`.
#'
#' @seealso [guide_axis()], [theme_axis()], [axis_text()]
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mpg, aes(x = displ, y = hwy)) +
#'    geom_point() +
#'    panel_shade(ymin = 32) +
#'    scale_y_continuous(
#'      sec.axis = ggscribe::sec_axis(
#'        breaks = \(x) mean(c(x[2], 32)),
#'        labels = "Inefficient",
#'      )
#'    )
sec_axis <- function(
    breaks = ggplot2::waiver(),
    labels = ggplot2::derive(),
    name = NULL,
    guide = ggplot2::guide_axis(theme = theme_axis()),
    ...
) {

  ggplot2::sec_axis(
    transform = "identity",
    breaks = breaks,
    labels = labels,
    name = name,
    guide = guide
  )
}
