# sec_axis_text --------------------------------------------------------------------

#' Secondary axis for text annotations
#'
#' @param breaks A function or numeric vector giving the break position(s) used
#'   to anchor the text. Defaults to `NULL`, which places a single label at the
#'   midpoint of the scale — the mean of the limits for continuous scales.
#' @param labels One of:
#'   - A character vector of labels, the same length as `breaks`
#'   - A function that takes break positions as input and returns labels
#' @param name The name of the secondary axis. Use [ggplot2::waiver()] to
#'   derive the name from the primary axis, or `NULL` (default) for no name.
#' @param guide A guide object used to render the axis. Defaults to
#'   [guide_sec_axis_text()], which makes transparent ticks and lines.
#' @param ... Additional arguments passed to [ggplot2::dup_axis()].
#'
#' @returns A `AxisSecondary` object for use in the `sec.axis` argument of
#'   `scale_x_continuous()` or `scale_y_continuous()`.
#'
#' @seealso [guide_sec_axis_text()], [axis_text()], [axis_ticks()], [axis_bracket()]
#' @export
#'
sec_axis_text <- function(
    breaks = \(x) mean(x),
    labels = ggplot2::waiver(),
    name   = NULL,
    guide  = guide_sec_axis_text(),
    ...
) {
  ggplot2::dup_axis(
    breaks = breaks,
    labels = labels,
    name   = name,
    guide  = guide,
    ...
  )
}

