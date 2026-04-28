# theme_sec_axis_text ------------------------------------------------------------------

#' Theme adjustments optimised for secondary axis text annotations
#'
#' @param axis Character. "x", "y", or NULL (defaults to both).
#' @param axis_ticks_to Action for ticks: "transparent", "blank", or "keep".
#' @param axis_line_to Action for lines: "transparent", "blank", or "keep".
#' @param axis_text_to Action for text: "transparent", "blank", or "keep".
#' @param axis_title_to Action for titles: "transparent", "blank", or "keep".
#'
#' @returns A ggplot2 theme object.
#' @export
#'
#' @seealso [sec_axis_text()], [guide_sec_axis_text()]
#'
#' @seealso [axis_ticks()], [axis_line()],
#' [axis_text()], [reference_line()]
#'
theme_sec_axis_text <- function(
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
    if (action == "transparent") return(ggplot2::element_line(colour = "transparent"))
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
