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
#' @param breaks A numeric vector of break positions in data coordinates, or
#'   wrapped in [I()] for normalised panel coordinates (npc), where `I(0)`
#'   is the left/bottom edge and `I(1)` is the right/top edge of the panel.
#' @param labels One of:
#'   - `NULL` (default) to use break values as labels
#'   - A character vector the same length as `breaks`
#'   - A function taking break values and returning labels
#' @param colour Inherits from `axis.text` in the set theme. May be a vector
#'   the same length as `breaks` to style each label individually.
#' @param size Inherits from `axis.text` in the set theme. May be a vector
#'   the same length as `breaks`.
#' @param family Inherits from `axis.text` in the set theme. May be a vector
#'   the same length as `breaks`.
#' @param hjust,vjust Justification. Auto-calculated from `position` and
#'   `angle` if `NULL`. Text always anchors to the tick end — the label edge
#'   facing the tick aligns to it, rotating naturally with `angle`. Negative
#'   `length` flips the anchor to the opposite edge. May be a vector the same
#'   length as `breaks`.
#' @param angle Text rotation angle. Defaults to `0`. May be a vector the same
#'   length as `breaks`.
#' @param length Offset from the axis edge including tick length and margin.
#'   Supports `rel()`. Negative values flip the tick direction. Defaults to
#'   `rel(1)`. May be a vector the same length as `breaks`.
#' @param layout Controls which panels the annotation appears in. `NULL`
#'   (default) repeats in all panels. An integer targets a specific panel.
#'   `"fixed"` repeats in all panels ignoring faceting variables. See
#'   [ggplot2::layer()] for full details.
#' @param xintercept For `"left"`/`"right"` axes: float the axis to these x
#'   positions in data coordinates instead of the panel edge. Paired 1:1 with
#'   `breaks` — each break gets its own intercept, recycling applies.
#' @param yintercept For `"top"`/`"bottom"` axes: float the axis to these y
#'   positions in data coordinates instead of the panel edge. Paired 1:1 with
#'   `breaks` — each break gets its own intercept, recycling applies.
#'
#' @return A list of ggplot2 annotation layers.
#' @seealso [axis_line()], [axis_ticks()],
#'   [axis_bracket()], [reference_line()],
#'   [panel_shade()], [sec_axis_text()]
#' @export
#'
#' @inherit sec_axis_text examples
#'
axis_text <- function(
    ...,
    position     = NULL,
    breaks,
    labels       = NULL,
    colour       = NULL,
    size         = NULL,
    family       = NULL,
    hjust        = NULL,
    vjust        = NULL,
    angle        = 0,
    length       = ggplot2::rel(1),
    layout       = NULL,
    xintercept   = NULL,
    yintercept   = NULL
) {
  

  position <- .infer_position(position, xintercept, yintercept)
  axis     <- if (position %in% c("top", "bottom")) "x" else "y"

  .validate_intercept(axis, position, xintercept, yintercept)

  if (length(breaks) == 0) return(list())

  if (is.list(breaks)) breaks <- unlist(breaks)

  npc_breaks <- inherits(breaks, "AsIs")
  breaks     <- as.numeric(breaks)

  raw_intercepts <- if (axis == "x") {
    if (is.null(yintercept)) if (position == "bottom") -Inf else Inf
    else as.numeric(yintercept)
  } else {
    if (is.null(xintercept)) if (position == "left") -Inf else Inf
    else as.numeric(xintercept)
  }

  # 1:1 pairing — recycle both to the longer
  n          <- max(length(breaks), length(raw_intercepts))
  breaks     <- rep_len(breaks, n)
  intercepts <- rep_len(raw_intercepts, n)

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

  theme_colour <- resolved_text_element$colour %||% "black"
  theme_size   <- resolved_text_element$size   %||% 11
  theme_family <- resolved_text_element$family %||% ""

  theme_length_pts <- {
    tl <- resolved_length
    if (is.null(tl)) {
      0.5 * (current_theme$text$size %||% 11)
    } else if (inherits(tl, "rel")) {
      spacing_pts <- as.numeric(grid::convertUnit(current_theme$spacing %||% grid::unit(5.5, "pt"), "pt"))
      as.numeric(tl) * spacing_pts
    } else if (!inherits(tl, "unit")) {
      if (is.numeric(tl)) tl else 0.5 * (current_theme$text$size %||% 11)
    } else {
      as.numeric(grid::convertUnit(tl, "pt"))
    }
  }

  text_margin <- resolved_text_element$margin
  margin_pts  <- 2
  if (!is.null(text_margin)) {
    margin_index <- switch(position, bottom = 1L, top = 3L, left = 2L, right = 4L)
    if (inherits(text_margin, c("margin", "unit")) && length(text_margin) >= margin_index) {
      margin_pts <- as.numeric(grid::convertUnit(text_margin[margin_index], "pt"))
    }
  }

  if (is.null(labels)) {
    labels <- as.character(breaks)
  } else if (is.function(labels)) {
    labels <- labels(breaks)
  }
  if (length(labels) != n) {
    rlang::abort("Length of `labels` must match length of `breaks`.")
  }

  # Style args recycle to n
  colour_vec <- if (is.null(colour)) rep_len(theme_colour, n) else rep_len(colour, n)
  size_vec   <- if (is.null(size))   rep_len(theme_size,   n) else rep_len(size,   n)
  family_vec <- if (is.null(family)) rep_len(theme_family, n) else rep_len(family, n)
  angle_vec  <- rep_len(angle, n)

  length_pts_vec <- if (inherits(length, "rel")) {
    abs(rep_len(as.numeric(length), n)) * theme_length_pts
  } else if (inherits(length, "unit")) {
    rep_len(abs(as.numeric(grid::convertUnit(length, "pt"))), n)
  } else {
    rep_len(abs(as.numeric(length)), n)
  }

  flip_vec <- if (inherits(length, "rel")) {
    rep_len(as.numeric(length) < 0, n)
  } else if (inherits(length, "unit")) {
    rep_len(as.numeric(grid::convertUnit(length, "pt")) < 0, n)
  } else {
    rep_len(as.numeric(length) < 0, n)
  }

  .deg2rad <- function(deg) deg * pi / 180

  .get_hjust <- function(pos, ang, flip) {
    rad    <- .deg2rad(ang)
    cosine <- sign(round(cos(rad), 3)) / 2 + 0.5
    sine   <- sign(round(sin(rad), 3)) / 2 + 0.5
    h <- switch(pos, left = cosine, right = 1 - cosine, top = 1 - sine, bottom = sine)
    if (flip) 1 - h else h
  }

  .get_vjust <- function(pos, ang, flip) {
    rad    <- .deg2rad(ang)
    cosine <- sign(round(cos(rad), 3)) / 2 + 0.5
    sine   <- sign(round(sin(rad), 3)) / 2 + 0.5
    v <- switch(pos, left = 1 - sine, right = sine, top = 1 - cosine, bottom = cosine)
    if (flip) 1 - v else v
  }

  hjust_provided <- !is.null(hjust)
  vjust_provided <- !is.null(vjust)
  hjust_vec_raw  <- if (hjust_provided) rep_len(hjust, n) else NULL
  vjust_vec_raw  <- if (vjust_provided) rep_len(vjust, n) else NULL

  # Single loop over paired (break, intercept) values
  lapply(seq_len(n), \(i) {
    break_val      <- breaks[[i]]
    intercept      <- intercepts[[i]]
    flip_direction <- flip_vec[[i]]
    ang            <- angle_vec[[i]]
    total_length   <- grid::unit(length_pts_vec[[i]] + margin_pts, "pt")

    gp <- grid::gpar(
      col        = colour_vec[[i]],
      fontsize   = size_vec[[i]],
      fontfamily = family_vec[[i]]
    )

    just <- c(
      hjust = if (hjust_provided) hjust_vec_raw[[i]] else .get_hjust(position, ang, flip_direction),
      vjust = if (vjust_provided) vjust_vec_raw[[i]] else .get_vjust(position, ang, flip_direction)
    )

    grob_along <- if (npc_breaks) grid::unit(break_val, "npc") else grid::unit(0.5, "npc")

    text_grob <- if (position == "bottom") {
      grid::textGrob(labels[i], x = grob_along,
                     y    = if (flip_direction) grid::unit(0, "npc") + total_length
                     else                grid::unit(0, "npc") - total_length,
                     just = just, rot = ang, gp = gp)
    } else if (position == "top") {
      grid::textGrob(labels[i], x = grob_along,
                     y    = if (flip_direction) grid::unit(1, "npc") - total_length
                     else                grid::unit(1, "npc") + total_length,
                     just = just, rot = ang, gp = gp)
    } else if (position == "left") {
      grid::textGrob(labels[i],
                     x    = if (flip_direction) grid::unit(0, "npc") + total_length
                     else                grid::unit(0, "npc") - total_length,
                     y = grob_along, just = just, rot = ang, gp = gp)
    } else {
      grid::textGrob(labels[i],
                     x    = if (flip_direction) grid::unit(1, "npc") - total_length
                     else                grid::unit(1, "npc") + total_length,
                     y = grob_along, just = just, rot = ang, gp = gp)
    }

    annotation_position <- if (npc_breaks && axis == "x") {
      list(xmin = -Inf, xmax = Inf, ymin = intercept, ymax = intercept)
    } else if (npc_breaks && axis == "y") {
      list(xmin = intercept, xmax = intercept, ymin = -Inf, ymax = Inf)
    } else if (axis == "x") {
      list(xmin = break_val, xmax = break_val, ymin = intercept, ymax = intercept)
    } else {
      list(xmin = intercept, xmax = intercept, ymin = break_val, ymax = break_val)
    }

    ggplot2::layer(
      geom     = ggplot2::GeomCustomAnn,
      stat     = "identity",
      data     = NULL,
      mapping  = ggplot2::aes(),
      position = "identity",
      params   = c(list(grob = text_grob, na.rm = FALSE), annotation_position),
      layout   = layout
    )
  })
}
