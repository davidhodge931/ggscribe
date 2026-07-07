# sec_axis_text --------------------------------------------------------------------

#' Secondary axis for text annotations
#'
#' @param breaks A function or numeric vector giving the break position(s) used
#'   to anchor the text. Defaults to `\(x) mean(x)`, which places a single label
#'   at the midpoint of the scale limits for continuous scales.
#' @param labels One of:
#'   - A character vector of labels, the same length as `breaks`
#'   - A function that takes break positions as input and returns labels
#'   If left as [ggplot2::waiver()], labels are derived from the break positions
#'   and may be numeric.
#' @param name The name of the secondary axis. Use [ggplot2::waiver()] to
#'   derive the name from the primary axis, or `NULL` (default) for no name.
#' @param guide A guide object used to render the axis. Defaults to
#'   `guide_sec_axis_text()`, which makes transparent ticks and lines.
#' @param ... Additional arguments passed to [ggplot2::dup_axis()].
#'
#' @returns A `AxisSecondary` object for use in the `sec.axis` argument of
#'   `scale_x_continuous()` or `scale_y_continuous()`.
#'
#' @export
sec_axis_text <- function(
  breaks = \(x) mean(x),
  labels = ggplot2::waiver(),
  name = NULL,
  guide = guide_sec_axis_text(),
  ...
) {
  ggplot2::dup_axis(
    breaks = breaks,
    labels = labels,
    name = name,
    guide = guide,
    ...
  )
}

# guide_sec_axis_text ---------------------------------------------------------------

#' Guide optimised for secondary axis text annotations
#'
#' A wrapper around [ggplot2::guide_axis()] that defaults to making transparent
#' ticks and lines while preserving text, making it ideal for annotation labels.
#'
#' @param theme A theme object to style the secondary axis.
#' @param ... Additional arguments passed to [ggplot2::guide_axis()], such as
#'   `title`, `check.overlap`, or `angle`.
#'
#' @returns A `guide` object to be used in a scale's `guide` argument or within
#'   `sec_axis_text()`.
#'
#' @export
#'
#' @inherit sec_axis_text examples
#'
guide_sec_axis_text <- function(..., theme = NULL) {
  base_theme <- theme_sec_axis_text()

  if (!is.null(theme)) {
    theme <- base_theme + theme
  } else {
    theme <- base_theme
  }

  ggplot2::guide_axis(
    theme = theme,
    ...
  )
}

# theme_sec_axis_text ------------------------------------------------------------------

#' Theme adjustment for secondary axis text annotations
#'
#' @returns A ggplot2 theme object.
#' @noRd
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
    axis.ticks.y.left = ggplot2::element_line(linetype = 0)
  )
}
