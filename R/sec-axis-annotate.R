# sec_axis_annotate ---------------------------------------------------------------

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
#'    [guide_axis_annotate()], which uses [theme_axis_annotate()] to
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
#' @seealso [ggplot2::dup_axis()], [annotate_panel_shade()]
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mpg, aes(x = displ, y = hwy)) +
#'    geom_point() +
#'    annotate_panel_shade(ymin = 32) +
#'    scale_y_continuous(
#'      sec.axis = sec_axis_annotate(
#'        breaks = \(x) mean(c(x[2], 32)),
#'        labels = "Inefficient",
#'      )
#'    )
sec_axis_annotate <- function(
    breaks = ggplot2::waiver(),
    labels = ggplot2::derive(),
    name = NULL,
    guide = ggplot2::guide_axis(theme = theme_axis_annotate()),
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

#' Axis guide with annotation-friendly defaults
#'
#' A wrapper around [ggplot2::guide_axis()] that defaults to using
#' [theme_axis_annotate()]. This guide is designed to strip away standard axis
#' furniture (like lines and ticks) while preserving text, making it ideal for
#' secondary axes used as margin labels.
#'
#' @param ... Additional arguments passed to [ggplot2::guide_axis()], such as
#'   `title`, `check.overlap`, or `angle`.
#' @param theme A `theme` object to style the guide. Defaults to
#'   `theme_axis_annotate()`, which suppresses ticks and lines.
#'
#' @returns A `guide` object to be used in a scale's `guide` argument or within
#'   [sec_axis_annotate()].
#'
#' @seealso [theme_axis_annotate()], [sec_axis_annotate()]
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
#'     guide = guide_axis_annotate(title = "Displacement Label Only")
#'   )
#'
#' # The guide is also used internally by sec_axis_annotate()
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   scale_y_continuous(
#'     sec.axis = sec_axis_annotate(
#'       breaks = 20,
#'       labels = "Reference point",
#'       guide = guide_axis_annotate(angle = 90)
#'     )
#'   )
guide_axis_annotate <- function(..., theme = theme_axis_annotate()) {
  ggplot2::guide_axis(
    theme = theme,
    ...
  )
}

#' Theme axis annotate
#'
#' @param axis Character. "x", "y", or NULL (defaults to both).
#' @param axis_ticks_to Action for ticks: "transparent", "blank", or "keep".
#' @param axis_line_to Action for lines: "transparent", "blank", or "keep".
#' @param axis_text_to Action for text: "transparent", "blank", or "keep".
#' @param axis_title_to Action for titles: "transparent", "blank", or "keep".
#'
#' @returns A ggplot2 theme object.
#' @export
theme_axis_annotate <- function(
    axis = NULL,
    axis_ticks_to = "transparent",
    axis_line_to = "transparent",
    axis_text_to = "keep",
    axis_title_to = "keep"
) {

  # Define which sides of the plot to target based on the axis argument
  if (is.null(axis)) {
    targets <- c("x.top", "x.bottom", "y.left", "y.right")
  } else if (axis == "x") {
    targets <- c("x.top", "x.bottom")
  } else if (axis == "y") {
    targets <- c("y.left", "y.right")
  } else {
    stop("Invalid axis argument. Use 'x', 'y', or NULL.")
  }

  # Helper function to return the correct ggplot2 element
  get_element <- function(action) {
    if (action == "keep") return(NULL)
    if (action == "blank") return(ggplot2::element_blank())
    if (action == "transparent") return(element_line_transparent())
    return(NULL)
  }

  # Initialize an empty list to store theme arguments
  theme_args <- list()

  # Loop through targets and assign the specified elements
  for (side in targets) {

    # Ticks logic
    tick_val <- get_element(axis_ticks_to)
    if (!is.null(tick_val)) theme_args[[paste0("axis.ticks.", side)]] <- tick_val

    # Line logic
    line_val <- get_element(axis_line_to)
    if (!is.null(line_val)) theme_args[[paste0("axis.line.", side)]] <- line_val

    # Text logic
    text_val <- get_element(axis_text_to)
    if (!is.null(text_val)) theme_args[[paste0("axis.text.", side)]] <- text_val

    # Title logic
    title_val <- get_element(axis_title_to)
    if (!is.null(title_val)) theme_args[[paste0("axis.title.", side)]] <- title_val
  }

  # Return the combined theme
  do.call(ggplot2::theme, theme_args)
}

