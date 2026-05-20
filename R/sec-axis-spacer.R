# sec_axis_spacer -------------------------------------------------------------

#' Add space above an axis
#'
#' A convenience wrapper around [sec_axis_text()] that reserves vertical (or
#' horizontal) space above (or beside) an axis without drawing visible text.
#' Useful for pushing axis titles away from the panel to make room for
#' annotations added with [axis_text()], [axis_ticks()], or [axis_bracket()].
#'
#' @param breaks A function or numeric vector giving the break position(s) used
#'   to anchor the spacer. Defaults to `\(x) mean(x)`, which places a single
#'   invisible label at the midpoint of the scale.
#' @param labels A character string used as the spacer. Defaults to `""`. Use
#' repeated newlines (e.g. `"\n"`) or a word to fit (e.g. `"Threshold"`).
#' @param name The name of the secondary axis. Use [ggplot2::waiver()] to
#'    derive the name from the primary axis, or `NULL` (default) for no name.
#' @param guide A guide object used to render the axis. Defaults to
#'    [guide_sec_axis_spacer()], which makes transparent ticks and lines.
#' @param ... Additional arguments passed to [ggplot2::dup_axis()].
#'
#' @return A [ggplot2::sec_axis()] object.
#' @seealso [guide_sec_axis_spacer()], [axis_text()], [axis_ticks()], [axis_bracket()]
#' @export
#'
sec_axis_spacer <- function(
    breaks = \(x) mean(x),
    labels = "",
    name = NULL,
    guide = guide_sec_axis_spacer(),
    ...
) {

  # if (is.null(labels)) {
  #   labels <- as.character(breaks)
  # } else if (is.function(labels)) {
  #   labels <- labels(breaks)
  # }

  ggplot2::sec_axis(
    transform = "identity",
    breaks = breaks,
    labels = labels,
    name = name,
    guide = guide
  )
}

