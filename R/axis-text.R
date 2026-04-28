# axis_text -------------------------------------------------------------------

#' Annotate axis text
#'
#' Draws text labels at specified break positions along an axis, with style
#' defaults taken from the `axis.text` element of the set theme. Requires
#' `coord_cartesian(clip = "off")`.
#'
#' @param ... Not used. Forces named arguments.
#' @param position One of `"top"`, `"bottom"`, `"left"`, or `"right"`. Inferred
#'   from `xintercept` or `yintercept` if not provided.
#' @param xintercept For `"left"`/`"right"` axes: float the axis to this x
#'   position in data coordinates instead of the panel edge.
#' @param yintercept For `"top"`/`"bottom"` axes: float the axis to this y
#'   position in data coordinates instead of the panel edge.
#' @param breaks A numeric vector of break positions.
#' @param labels One of:
#'   - `NULL` (default) to use break values as labels
#'   - A character vector the same length as `breaks`
#'   - A function taking break values and returning labels
#' @param colour Inherits from `axis.text` in the set theme.
#' @param size Inherits from `axis.text` in the set theme.
#' @param family Inherits from `axis.text` in the set theme.
#' @param hjust,vjust Justification. Auto-calculated from `position` if `NULL`.
#' @param angle Text rotation angle. Defaults to `0`.
#' @param ticks_length Offset from the axis edge including tick length and margin.
#'   Supports `rel()`. Negative values place labels inside the panel. Defaults
#'   to `rel(1)` (theme tick length + text margin).
#'
#' @return A list of ggplot2 annotation layers.
#' @seealso [axis_line()], [axis_ticks()],
#'   [axis_bracket()], [reference_line()],
#'   [panel_shade()], [sec_axis()]
#' @export
axis_text <- function(
    ...,
    position     = NULL,
    xintercept   = NULL,
    yintercept   = NULL,
    breaks,
    labels       = NULL,
    colour       = NULL,
    size         = NULL,
    family       = NULL,
    hjust        = NULL,
    vjust        = NULL,
    angle        = 0,
    ticks_length = ggplot2::rel(1)
) {
  rlang::check_dots_empty()

  position <- .infer_position(position, xintercept, yintercept)
  axis     <- if (position %in% c("top", "bottom")) "x" else "y"

  .validate_intercept(axis, position, xintercept, yintercept)

  if (length(breaks) == 0) return(list())

  intercept     <- .resolve_intercept(axis, position, xintercept, yintercept)
  current_theme <- ggplot2::theme_get()

  text_hierarchy <- c(
    paste0("axis.text.", axis, ".", position),
    paste0("axis.text.", axis),
    "axis.text"
  )

  resolved_text_element <- NULL
  for (nm in text_hierarchy) {
    el <- ggplot2::calc_element(nm, current_theme, skip_blank = TRUE)
    if (!is.null(el) && !inherits(el, "element_blank")) { resolved_text_element <- el; break }
  }
  if (is.null(resolved_text_element)) {
    resolved_text_element <- ggplot2::element_text(colour = "black", size = 11, family = "")
  }

  text_colour <- colour %||% resolved_text_element$colour %||% "black"
  text_size   <- size   %||% resolved_text_element$size   %||% 11
  text_family <- family %||% resolved_text_element$family %||% ""

  if (is.null(labels)) {
    labels <- as.character(breaks)
  } else if (is.function(labels)) {
    labels <- labels(breaks)
  }

  if (length(labels) != length(breaks)) {
    rlang::abort("Length of `labels` must match length of `breaks`.")
  }

  length_hierarchy <- c(
    paste0("axis.ticks.length.", axis, ".", position),
    paste0("axis.ticks.length.", axis),
    "axis.ticks.length"
  )
  resolved_length <- NULL
  for (nm in length_hierarchy) {
    el <- ggplot2::calc_element(nm, current_theme, skip_blank = TRUE)
    if (!is.null(el) && !inherits(el, "element_blank")) { resolved_length <- el; break }
  }

  calculate_default_ticks_length <- function() {
    tl <- resolved_length
    if (is.null(tl)) {
      return(grid::unit(0.5 * (current_theme$text$size %||% 11), "pt"))
    } else if (inherits(tl, "rel")) {
      spacing_pts <- as.numeric(grid::convertUnit(current_theme$spacing %||% grid::unit(5.5, "pt"), "pt"))
      return(grid::unit(as.numeric(tl) * spacing_pts, "pt"))
    } else if (!inherits(tl, "unit")) {
      return(grid::unit(if (is.numeric(tl)) tl else 0.5 * (current_theme$text$size %||% 11), "pt"))
    } else {
      return(tl)
    }
  }

  flip_direction <- FALSE
  if (inherits(ticks_length, "rel")) {
    rel_value      <- as.numeric(ticks_length)
    default_pts    <- as.numeric(grid::convertUnit(calculate_default_ticks_length(), "pt"))
    ticks_length   <- grid::unit(abs(rel_value) * default_pts, "pt")
    flip_direction <- rel_value < 0
  } else if (inherits(ticks_length, "unit")) {
    tick_pts       <- as.numeric(grid::convertUnit(ticks_length, "pt"))
    ticks_length   <- grid::unit(abs(tick_pts), "pt")
    flip_direction <- tick_pts < 0
  } else if (is.numeric(ticks_length)) {
    flip_direction <- ticks_length < 0
    ticks_length   <- grid::unit(abs(ticks_length), "pt")
  }

  text_margin  <- resolved_text_element$margin
  margin_unit  <- grid::unit(2, "pt")
  if (!is.null(text_margin)) {
    margin_index <- switch(position, bottom = 1L, top = 3L, left = 2L, right = 4L)
    if (inherits(text_margin, c("margin", "unit")) && length(text_margin) >= margin_index) {
      margin_unit <- text_margin[margin_index]
    }
  }
  total_length <- ticks_length + margin_unit

  if (is.null(hjust)) {
    hjust <- if (position %in% c("top", "bottom")) 0.5
    else if (position == "left") { if (flip_direction) 0 else 1 }
    else { if (flip_direction) 1 else 0 }
  }
  if (is.null(vjust)) {
    vjust <- if (position == "bottom") { if (flip_direction) 0 else 1 }
    else if (position == "top") { if (flip_direction) 1 else 0 }
    else 0.5
  }

  make_gpar <- function() {
    grid::gpar(col = text_colour, fontsize = text_size, fontfamily = text_family)
  }

  lapply(seq_along(breaks), \(i) {
    break_val <- breaks[[i]]

    text_grob <- if (position == "bottom") {
      grid::textGrob(
        labels[i],
        x    = grid::unit(0.5, "npc"),
        y    = if (flip_direction) grid::unit(0, "npc") + total_length
        else                grid::unit(0, "npc") - total_length,
        just = c(hjust, vjust), rot = angle, gp = make_gpar()
      )
    } else if (position == "top") {
      grid::textGrob(
        labels[i],
        x    = grid::unit(0.5, "npc"),
        y    = if (flip_direction) grid::unit(1, "npc") - total_length
        else                grid::unit(1, "npc") + total_length,
        just = c(hjust, vjust), rot = angle, gp = make_gpar()
      )
    } else if (position == "left") {
      grid::textGrob(
        labels[i],
        x    = if (flip_direction) grid::unit(0, "npc") + total_length
        else                grid::unit(0, "npc") - total_length,
        y    = grid::unit(0.5, "npc"),
        just = c(hjust, vjust), rot = angle, gp = make_gpar()
      )
    } else {
      grid::textGrob(
        labels[i],
        x    = if (flip_direction) grid::unit(1, "npc") - total_length
        else                grid::unit(1, "npc") + total_length,
        y    = grid::unit(0.5, "npc"),
        just = c(hjust, vjust), rot = angle, gp = make_gpar()
      )
    }

    annotation_position <- if (axis == "x") {
      list(xmin = break_val, xmax = break_val, ymin = intercept, ymax = intercept)
    } else {
      list(xmin = intercept, xmax = intercept, ymin = break_val, ymax = break_val)
    }

    rlang::exec(ggplot2::annotation_custom, grob = text_grob, !!!annotation_position)
  })
}
