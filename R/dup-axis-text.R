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
#' @param labels One of:
#'    - [ggplot2::derive()] (default) to derive labels from `breaks`
#'    - A character vector of labels, the same length as `breaks`
#'    - A function that takes break positions as input and returns labels
#' @param ... Additional arguments passed to [ggplot2::dup_axis()].
#' @param elements_to One of `"keep"`, `"transparent"`, or `"blank"`. Controls
#'    whether native theme ticks are suppressed. Defaults to `"keep"`.
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

#' #' Title
#' #'
#' #' @param ticks_to
#' #'
#' #' @returns
#' #' @export
#' #'
#' #' @examples
#' theme_axis_annotate <- function(
#'     axis = NULL, #defaults to both
#'     ticks_to = "transparent",
#'     line_to = "transparent",
#'     text_to = "keep",
#'     title_to = "keep",
#'   ) {
#'
#'   if (is.null(axis)) {
#'     if (ticks_to == "transparent") {
#'       theme <- ggplot2::theme(
#'         axis.ticks.x.top    = element_line_transparent(),
#'         axis.ticks.x.bottom = element_line_transparent(),
#'         axis.ticks.y.left   = element_line_transparent(),
#'         axis.ticks.y.right  = element_line_transparent(),
#'       )
#'     }
#'     else if (ticks_to == "blank") {
#'       theme <- ggplot2::theme(
#'         axis.ticks.x.top    = element_blank(),
#'         axis.ticks.x.bottom = element_blank(),
#'         axis.ticks.y.left   = element_blank(),
#'         axis.ticks.y.right  = element_blank(),
#'       )
#'     }
#'     else if (ticks_to == "keep") {
#'       theme <- ggplot2::theme()
#'     }
#'
#'     if (line_to == "transparent") {
#'       theme <- ggplot2::theme(
#'         axis.line.x.top    = element_line_transparent(),
#'         axis.line.x.bottom = element_line_transparent(),
#'         axis.line.y.left   = element_line_transparent(),
#'         axis.line.y.right  = element_line_transparent(),
#'       )
#'     }
#'     else if (line_to == "blank") {
#'       theme <- ggplot2::theme(
#'         axis.line.x.top    = element_blank(),
#'         axis.line.x.bottom = element_blank(),
#'         axis.line.y.left   = element_blank(),
#'         axis.line.y.right  = element_blank(),
#'       )
#'     }
#'     else if (line_to == "keep") {
#'       theme <- ggplot2::theme()
#'     }
#'
#'     if (text_to == "transparent") {
#'       theme <- ggplot2::theme(
#'         axis.text.x.top    = element_line_transparent(),
#'         axis.text.x.bottom = element_line_transparent(),
#'         axis.text.y.left   = element_line_transparent(),
#'         axis.text.y.right  = element_line_transparent(),
#'       )
#'     }
#'     else if (text_to == "blank") {
#'       theme <- ggplot2::theme(
#'         axis.text.x.top    = element_blank(),
#'         axis.text.x.bottom = element_blank(),
#'         axis.text.y.left   = element_blank(),
#'         axis.text.y.right  = element_blank(),
#'       )
#'     }
#'     else if (text_to == "keep") {
#'       theme <- ggplot2::theme()
#'     }
#'
#'     if (title_to == "transparent") {
#'       theme <- ggplot2::theme(
#'         axis.title.x.top    = element_line_transparent(),
#'         axis.title.x.bottom = element_line_transparent(),
#'         axis.title.y.left   = element_line_transparent(),
#'         axis.title.y.right  = element_line_transparent(),
#'       )
#'     }
#'     else if (title_to == "blank") {
#'       theme <- ggplot2::theme(
#'         axis.title.x.top    = element_blank(),
#'         axis.title.x.bottom = element_blank(),
#'         axis.title.y.left   = element_blank(),
#'         axis.title.y.right  = element_blank(),
#'       )
#'     }
#'     else if (title_to == "keep") {
#'       theme <- ggplot2::theme()
#'     }
#'
#'
#'   }
#'   else if (axis == "x") {
#'   }
#'   else if (axis == "y") {
#'   }
#'
#'   theme +
#'     ggplot2::theme(
#'       axis.line.x.top    = ggplot2::element_blank(),
#'       axis.line.x.bottom = ggplot2::element_blank(),
#'       axis.line.y.left   = ggplot2::element_blank(),
#'       axis.line.y.right  = ggplot2::element_blank(),
#'     )
#' }

#' Theme axis annotate
#'
#' @param axis Character. "x", "y", or NULL (defaults to both).
#' @param elements_to_ticks Action for ticks: "transparent", "blank", or "keep".
#' @param elements_to_line Action for lines: "transparent", "blank", or "keep".
#' @param elements_to_text Action for text: "transparent", "blank", or "keep".
#' @param elements_to_title Action for titles: "transparent", "blank", or "keep".
#'
#' @returns A ggplot2 theme object.
#' @export
theme_axis_annotate <- function(
    axis = NULL,
    elements_to_ticks = "transparent",
    elements_to_line = "transparent",
    elements_to_text = "keep",
    elements_to_title = "keep"
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
    tick_val <- get_element(elements_to_ticks)
    if (!is.null(tick_val)) theme_args[[paste0("axis.ticks.", side)]] <- tick_val

    # Line logic
    line_val <- get_element(elements_to_line)
    if (!is.null(line_val)) theme_args[[paste0("axis.line.", side)]] <- line_val

    # Text logic
    text_val <- get_element(elements_to_text)
    if (!is.null(text_val)) theme_args[[paste0("axis.text.", side)]] <- text_val

    # Title logic
    title_val <- get_element(elements_to_title)
    if (!is.null(title_val)) theme_args[[paste0("axis.title.", side)]] <- title_val
  }

  # Return the combined theme
  do.call(ggplot2::theme, theme_args)
}

#' Title
#'
#' @param ...
#' @param theme
#'
#' @returns
#' @export
#'
#' @examples
guide_axis_annotate <- function(..., theme = theme_axis_annotate()) {
  ggplot2::guide_axis(
    theme = theme,
    ...
  )
}
