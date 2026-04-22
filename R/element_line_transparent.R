#' Element line that is transparent
#'
#' @param ... Passed to `ggplot2::element_line`.
#'
#' @returns
#' @export
#'
#' @examples
element_line_transparent <- function(...) {
  ggplot2::element_line(colour = "transparent", ...)
}
