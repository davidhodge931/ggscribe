# axis_ticks ------------------------------------------------------------------

#' Annotate axis ticks
#'
#' Draws axis ticks at specified break positions, with style defaults taken from
#' the `axis.ticks` element of the set theme. Requires
#' `coord_cartesian(clip = "off")`.
#'
#' @param ... Not used. Forces named arguments.
#' @param position One of `"top"`, `"bottom"`, `"left"`, or `"right"`. Inferred
#'   from `xintercept` or `yintercept` if not provided.
#' @param breaks A numeric vector of break positions in data coordinates, or
#'   wrapped in [I()] for normalised panel coordinates (npc), where `I(0)`
#'   is the left/bottom edge and `I(1)` is the right/top edge of the panel.
#' @param colour Inherits from `axis.ticks` in the set theme. May be a vector
#'   the same length as `breaks` to style each tick individually.
#' @param linewidth Inherits from `axis.ticks` in the set theme. Supports
#'   `rel()`. May be a vector the same length as `breaks`.
#' @param linetype Inherits from `axis.ticks` in the set theme. May be a vector
#'   the same length as `breaks`.
#' @param length Total tick length as a grid unit. Supports `rel()`. Negative
#'   values flip the tick direction (inward). Defaults to `rel(1)` (outward at
#'   theme tick length). May be a vector the same length as `breaks`.
#' @param arrow A [grid::arrow()] specification, or a list of such
#'   specifications the same length as `breaks` to mix arrowed and plain ticks.
#'   Use `NULL` as a list element for no arrow on a specific tick. The
#'   arrowhead points toward the axis line. Must use `list()` not `c()` when
#'   supplying multiple values.
#'   E.g. `grid::arrow(angle = 15, length = unit(1.5, "mm"), type = "closed")`.
#' @param xintercept For `"left"`/`"right"` axes: float the axis to this x
#'   position in data coordinates instead of the panel edge.
#' @param yintercept For `"top"`/`"bottom"` axes: float the axis to this y
#'   position in data coordinates instead of the panel edge.
#'
#' @return A list of ggplot2 annotation layers.
#' @seealso [axis_line()], [axis_text()],
#'   [axis_bracket()], [reference_line()],
#'   [panel_shade()], [sec_axis_text()]
#' @export
axis_ticks <- function(
    ...,
    position     = NULL,
    breaks,
    colour       = NULL,
    linewidth    = NULL,
    linetype     = NULL,
    length       = ggplot2::rel(1),
    arrow        = NULL,
    xintercept   = NULL,
    yintercept   = NULL
) {
  rlang::check_dots_empty()

  position <- .infer_position(position, xintercept, yintercept)
  axis     <- if (position %in% c("top", "bottom")) "x" else "y"

  .validate_intercept(axis, position, xintercept, yintercept)

  if (length(breaks) == 0) return(list())

  # Check once whether breaks are in npc (I()) or data coordinates
  npc_breaks <- inherits(breaks, "AsIs")
  breaks     <- as.numeric(breaks)
  n          <- length(breaks)

  intercept     <- .resolve_intercept(axis, position, xintercept, yintercept)
  current_theme <- ggplot2::theme_get()

  # ---- Resolve theme element ------------------------------------------------

  tick_hierarchy <- c(
    paste0("axis.ticks.", axis, ".", position),
    paste0("axis.ticks.", axis),
    "axis.ticks"
  )

  resolved_tick_element    <- NULL
  tick_intentionally_blank <- FALSE
  for (nm in tick_hierarchy) {
    el_raw <- ggplot2::calc_element(nm, current_theme, skip_blank = FALSE)
    if (!is.null(el_raw)) {
      if (inherits(el_raw, "element_blank")) { tick_intentionally_blank <- TRUE; break }
      el <- ggplot2::calc_element(nm, current_theme, skip_blank = TRUE)
      if (!is.null(el) && !inherits(el, "element_blank")) { resolved_tick_element <- el; break }
    }
  }

  if (is.null(colour) && (tick_intentionally_blank || is.null(resolved_tick_element$colour))) {
    rlang::warn("The set theme does not define an `axis.ticks` colour. Defaulting to \"black\".")
  }
  if (is.null(linewidth) && (tick_intentionally_blank || is.null(resolved_tick_element$linewidth))) {
    rlang::warn("The set theme does not define an `axis.ticks` linewidth. Defaulting to `0.5`.")
  }
  if (is.null(resolved_tick_element)) {
    resolved_tick_element <- list(colour = "black", linewidth = 0.5, linetype = 1)
  }

  length_hierarchy <- c(
    paste0("axis.ticks.length.", axis, ".", position),
    paste0("axis.ticks.length.", axis),
    "axis.ticks.length"
  )

  resolved_length_element <- NULL
  for (nm in length_hierarchy) {
    el <- ggplot2::calc_element(nm, current_theme, skip_blank = TRUE)
    if (!is.null(el) && !inherits(el, "element_blank")) { resolved_length_element <- el; break }
  }

  # ---- Resolve scalar theme defaults ----------------------------------------

  theme_colour    <- resolved_tick_element$colour   %||% "black"
  theme_linewidth <- resolved_tick_element$linewidth %||% 0.5
  theme_linetype  <- resolved_tick_element$linetype  %||% 1

  theme_length_pts <- {
    tl <- resolved_length_element
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

  # ---- Vectorise and recycle all args to length(breaks) --------------------

  colour_vec <- if (is.null(colour)) {
    rep_len(theme_colour, n)
  } else {
    rep_len(colour, n)
  }

  linewidth_vec <- if (is.null(linewidth)) {
    rep_len(theme_linewidth, n)
  } else if (inherits(linewidth, "rel")) {
    rep_len(as.numeric(linewidth), n) * theme_linewidth
  } else {
    rep_len(linewidth, n)
  }

  linetype_vec <- if (is.null(linetype)) {
    rep_len(theme_linetype, n)
  } else {
    rep_len(linetype, n)
  }

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

  arrow_list <- if (inherits(arrow, "arrow")) {
    rep_len(list(arrow), n)
  } else if (is.list(arrow)) {
    rep_len(arrow, n)
  } else {
    rep_len(list(NULL), n)
  }

  # ---- Draw one grob per break ---------------------------------------------

  lapply(seq_along(breaks), \(i) {
    break_val      <- breaks[[i]]
    tick_colour    <- colour_vec[[i]]
    tick_linewidth <- linewidth_vec[[i]]
    tick_linetype  <- linetype_vec[[i]]
    tick_length    <- grid::unit(length_pts_vec[[i]], "pt")
    flip_direction <- flip_vec[[i]]
    tick_arrow     <- arrow_list[[i]]

    gp <- grid::gpar(
      col     = tick_colour,
      fill    = tick_colour,
      lwd     = tick_linewidth * ggplot2::.pt,
      lty     = tick_linetype,
      lineend = "butt"
    )

    grob_along <- if (npc_breaks) {
      grid::unit(break_val, "npc")
    } else {
      grid::unit(0.5, "npc")
    }

    # Segments are drawn from tip → panel edge so that the arrowhead (placed
    # at the end of the segment) points toward the axis line.
    tick_grob <- if (position == "bottom") {
      grid::segmentsGrob(
        x0 = grob_along, x1 = grob_along,
        y0 = if (flip_direction) grid::unit(0, "npc") + tick_length
        else                grid::unit(0, "npc") - tick_length,
        y1 = grid::unit(0, "npc"),
        gp = gp, arrow = tick_arrow
      )
    } else if (position == "top") {
      grid::segmentsGrob(
        x0 = grob_along, x1 = grob_along,
        y0 = if (flip_direction) grid::unit(1, "npc") - tick_length
        else                grid::unit(1, "npc") + tick_length,
        y1 = grid::unit(1, "npc"),
        gp = gp, arrow = tick_arrow
      )
    } else if (position == "left") {
      grid::segmentsGrob(
        x0 = if (flip_direction) grid::unit(0, "npc") + tick_length
        else                grid::unit(0, "npc") - tick_length,
        x1 = grid::unit(0, "npc"),
        y0 = grob_along, y1 = grob_along,
        gp = gp, arrow = tick_arrow
      )
    } else {
      grid::segmentsGrob(
        x0 = if (flip_direction) grid::unit(1, "npc") - tick_length
        else                grid::unit(1, "npc") + tick_length,
        x1 = grid::unit(1, "npc"),
        y0 = grob_along, y1 = grob_along,
        gp = gp, arrow = tick_arrow
      )
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

    rlang::exec(ggplot2::annotation_custom, grob = tick_grob, !!!annotation_position)
  })
}
