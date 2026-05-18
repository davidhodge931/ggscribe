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
#' @param minor Logical. If `TRUE`, uses minor tick theme defaults. Defaults to
#'   `FALSE`.
#' @param colour Inherits from `axis.ticks` in the set theme.
#' @param linewidth Inherits from `axis.ticks` in the set theme. Supports `rel()`.
#' @param linetype Inherits from `axis.ticks` in the set theme.
#' @param length Total tick length as a grid unit. Supports `rel()`.
#'   Negative values flip the tick direction (inward). Defaults to `rel(1)`
#'   (outward at theme tick length).
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
    minor        = FALSE,
    colour       = NULL,
    linewidth    = NULL,
    linetype     = NULL,
    length       = ggplot2::rel(1),
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

  intercept     <- .resolve_intercept(axis, position, xintercept, yintercept)
  current_theme <- ggplot2::theme_get()

  tick_hierarchy <- if (minor) {
    c(paste0("axis.minor.ticks.", axis, ".", position), paste0("axis.ticks.", axis, ".", position),
      paste0("axis.ticks.", axis), "axis.ticks")
  } else {
    c(paste0("axis.ticks.", axis, ".", position), paste0("axis.ticks.", axis), "axis.ticks")
  }

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

  length_hierarchy <- if (minor) {
    c(paste0("axis.minor.ticks.length.", axis, ".", position), paste0("axis.minor.ticks.length.", axis),
      "axis.minor.ticks.length", paste0("axis.ticks.length.", axis, ".", position),
      paste0("axis.ticks.length.", axis), "axis.ticks.length")
  } else {
    c(paste0("axis.ticks.length.", axis, ".", position), paste0("axis.ticks.length.", axis), "axis.ticks.length")
  }

  resolved_length_element <- NULL
  for (nm in length_hierarchy) {
    el <- ggplot2::calc_element(nm, current_theme, skip_blank = TRUE)
    if (!is.null(el) && !inherits(el, "element_blank")) { resolved_length_element <- el; break }
  }

  tick_colour    <- colour   %||% resolved_tick_element$colour   %||% "black"
  tick_linewidth <- if (is.null(linewidth)) {
    resolved_tick_element$linewidth %||% 0.5
  } else if (inherits(linewidth, "rel")) {
    as.numeric(linewidth) * (resolved_tick_element$linewidth %||% 0.5)
  } else {
    linewidth
  }
  tick_linetype  <- linetype %||% resolved_tick_element$linetype %||% 1

  calculate_theme_length <- function() {
    tl <- resolved_length_element
    if (is.null(tl)) {
      return(grid::unit((if (minor) 0.375 else 0.5) * (current_theme$text$size %||% 11), "pt"))
    } else if (inherits(tl, "rel")) {
      spacing_pts <- as.numeric(grid::convertUnit(current_theme$spacing %||% grid::unit(5.5, "pt"), "pt"))
      return(grid::unit(as.numeric(tl) * spacing_pts, "pt"))
    } else if (!inherits(tl, "unit")) {
      return(grid::unit(
        if (is.numeric(tl)) tl else (if (minor) 0.375 else 0.5) * (current_theme$text$size %||% 11), "pt"
      ))
    } else {
      return(tl)
    }
  }

  flip_direction <- FALSE
  if (inherits(length, "rel")) {
    rel_value      <- as.numeric(length)
    default_pts    <- as.numeric(grid::convertUnit(calculate_theme_length(), "pt"))
    length         <- grid::unit(abs(rel_value) * default_pts, "pt")
    flip_direction <- rel_value < 0
  } else if (inherits(length, "unit")) {
    tick_pts       <- as.numeric(grid::convertUnit(length, "pt"))
    length         <- grid::unit(abs(tick_pts), "pt")
    flip_direction <- tick_pts < 0
  } else if (is.numeric(length)) {
    flip_direction <- length < 0
    length         <- grid::unit(abs(length), "pt")
  }

  gp <- grid::gpar(
    col     = tick_colour,
    lwd     = tick_linewidth * ggplot2::.pt,
    lty     = tick_linetype,
    lineend = "butt"
  )

  lapply(breaks, \(break_val) {

    # Resolve the along-axis coordinate of the grob:
    # - data coords: pinned to 0.5 npc (annotation_custom handles the mapping)
    # - npc coords:  directly placed via unit(break_val, "npc")
    grob_along <- if (npc_breaks) {
      grid::unit(break_val, "npc")
    } else {
      grid::unit(0.5, "npc")
    }

    tick_grob <- if (position == "bottom") {
      grid::segmentsGrob(
        x0 = grob_along, x1 = grob_along,
        y0 = grid::unit(0, "npc"),
        y1 = if (flip_direction) grid::unit(0, "npc") + length
        else                grid::unit(0, "npc") - length,
        gp = gp
      )
    } else if (position == "top") {
      grid::segmentsGrob(
        x0 = grob_along, x1 = grob_along,
        y0 = grid::unit(1, "npc"),
        y1 = if (flip_direction) grid::unit(1, "npc") - length
        else                grid::unit(1, "npc") + length,
        gp = gp
      )
    } else if (position == "left") {
      grid::segmentsGrob(
        x0 = grid::unit(0, "npc"),
        x1 = if (flip_direction) grid::unit(0, "npc") + length
        else                grid::unit(0, "npc") - length,
        y0 = grob_along, y1 = grob_along,
        gp = gp
      )
    } else {
      grid::segmentsGrob(
        x0 = grid::unit(1, "npc"),
        x1 = if (flip_direction) grid::unit(1, "npc") - length
        else                grid::unit(1, "npc") + length,
        y0 = grob_along, y1 = grob_along,
        gp = gp
      )
    }

    # For npc breaks, span the full panel in the along-axis direction so the
    # grob's internal npc coordinate handles positioning. The intercept is
    # preserved in the perpendicular direction so yintercept/xintercept
    # floating still works correctly.
    # For data breaks, pin to the break value as before.
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
